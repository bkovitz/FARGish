# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable, Sequence
from dataclasses import dataclass, field
from inspect import isclass
from copy import copy
from operator import attrgetter, itemgetter
from random import choice
from itertools import product

from Primitives import Hop, Hops, PortGraphPrimitives, ActiveGraphPrimitives, \
    ActivationPrimitives, ActivationPolicy, SupportPrimitives, SupportPolicy, \
    SlipnetPolicy
from Node import Node, NodeId, MaybeNodeId, PortLabel, PortLabels, is_nodeid, \
    NRef, NRefs, CRef, CRefs, MaybeNRef, MaybeCRef, \
    as_nodeid, as_node, as_nodeids, as_nodes
from PortMates import PortMates
from Action import Action, Actions
from ActiveNode import ActiveNode, ActionNode
from Propagator import Propagator
from util import as_iter, as_list, as_set, is_iter, repr_str, first, reseed, \
    intersection, empty_set, sample_without_replacement, PushAttr, always_true
from exc import NodeLacksMethod, NoSuchNodeclass, NeedArg, FargDone, \
    ActionBlocked, ActionFailure
from log import *
from criteria import Criterion, OfClass, NodeEq, NoMate


#TODO rm
class Support(ActiveGraphPrimitives):

    def support_for(self, node: NRef) -> float:
        #TODO
        return 1.0

class Touches:
    pass

class Members(ActiveGraphPrimitives):
    pass

class ActiveGraph(
    Touches, Members, ActivationPolicy, SupportPolicy, SlipnetPolicy,
    ActivationPrimitives, SupportPrimitives, ActiveGraphPrimitives
):
    std_port_mates = PortMates([
        ('members', 'member_of'), ('tags', 'taggees'), ('built_by', 'built'),
        ('next', 'prev'), ('copy_of', 'copies')
    ])

    def __init__(
        self,
        seed: Union[int, None]=None,
        t: Union[int, None]=None,
        num_timesteps: Union[int, None]=None,
        port_mates: Union[PortMates, None]=None,
        *args,
        **kwargs
    ):
        if t is None:
            t = 0
        self.t = t
        self.seed = reseed(seed)

        super().__init__(*args, **kwargs)

        self.num_timesteps = num_timesteps
        self.final_result: Union[FargDone, None] = None

        self.port_mates: PortMates = copy(self.std_port_mates)
        if port_mates:
            self.port_mates += port_mates
        self.nodeclasses: Dict[str, Type[Node]] = {}

        self.max_active_nodes: Union[int, None] = None
        self.max_actions: int = 1

        self.builder: MaybeNRef = None  # Node that is currently "building"
                                        # other nodes
        self.new_nodes: Set[NodeId] = set()        # Nodes built this timestep
        self.prev_new_nodes: Set[NodeId] = set()   # Nodes built last timestep
        self.touched_nodes: Set[NodeId] = set()
        self.prev_touched_nodes: Set[NodeId] = set()
        self.after_touch_nodes: Set[NodeId] = set()
            # Nodes that need their .after_touch_update method called at the
            # end of each timestep.
        self.during_touch = False
        self.prev_actions: List[Action] = []

        self.ws: MaybeNRef = None
        self.slipnet: MaybeNRef = None

    def add_nodeclasses(self, *clss: Type[Node]):
        self.nodeclasses.update((cls.__name__, cls) for cls in clss)

    # Overrides for ActiveGraphPrimitives

    def add_node(
        self,
        node: Union[Type[Node], Node, str, Type[Action]],
        *args,
        **kwargs
    ) -> Node:
        '''Creates a new node and returns its Node object. The Node object's
        NodeParams are filled in from 'args' and 'kwargs', producing
        FilledParams and attributes within the node and/or links to neighbors.
        The filling in of NodeParams happens whether a new Node object is
        created or if an existing Node is supplied, as explained below.

        If 'node' is a Node class, a new Node object of that class is created.

        If 'node' is a string, it must be the name of a NodeClass registered
        with the graph. The new Node is created as if 'node' were that
        NodeClass.

        If 'node' is itself a Node object, the new node is given that object
        for its datum and its .id field is filled in.

        If 'node' is an Action object, the new node is an ActionNode with
        .action set to that Action object.

        If 'node' is a nodeid (an int), it must refer to an existing node in
        the graph. A new Node is created as if 'node' were the class of the
        referenced node.

        #TODO Explain touches, implicit membership, builder, Workspace and
        Slipnet, and .on_build().
        '''
        #print('ADD_NODE', repr(node), args, kwargs)
        if isclass(node) and issubclass(node, Action):
            action = node(*args)
            kwargs['action'] = action
            return self.add_node(ActionNode, **kwargs)
        elif isinstance(node, Action):
            return self.add_node(ActionNode, action=node, **kwargs)
        builder = kwargs.pop('builder', self.builder)
        if isinstance(node, Node):
            kwargs = {**node.regen_kwargs(), **kwargs}
            already = self.already_built(node, *args, **kwargs)
            if already:
                return already
            self._add_node(node)
            if ShowPrimitives:
                print('built', self.long_nodestr(node))
            # TODO Apply link FilledParams?  DONE?
            filled_params = node.make_filled_params(self, *args, **kwargs)
            filled_params.apply_to_node(self, node.id)
        else:
            if isinstance(node, str) or is_nodeid(node):
                node = self.as_nodeclass(node)
                # TODO Catch/throw exception if it doesn't exist?
            already = self.already_built(node, *args, **kwargs)
            if already:
                return already  # TODO Don't return yet; updates later
            assert issubclass(node, Node), f'{node} is not a subclass of Node'
            filled_params = node.make_filled_params(self, *args, **kwargs)
            node: Node = node()  # Create the Node object
            self._add_node(node)
            if ShowPrimitives:
                print('built', self.long_nodestr(node))
            filled_params.apply_to_node(self, node.id)

        self.add_implicit_membership(node)
        self.mark_builder(node, builder)

        classname = node.__class__.__name__
        if not self.ws and classname == 'Workspace':
            self.ws = node
        if not self.slipnet and classname == 'Slipnet':
            self.slipnet = node
        if classname not in self.nodeclasses:
            self.nodeclasses[classname] = node.__class__

        node.on_build()
        self.new_nodes.add(self.as_nodeid(node))
        if self.callable(node, 'after_touch_update'):
            self.after_touch_nodes.add(node.id)
        return node

    def _add_node(self, node: Node) -> NodeId:
        '''Makes the node, sets its .id and .g members, its .tob ("time of
        birth"), its initial activation, and returns its id.'''
        id = super()._add_node(node)
        node.tob = self.t
        self.set_activation(node, node.initial_activation)
        self.set_support_for(node, node.initial_support_for)
        return id

    #TODO UT
    def remove_node(self, node: NRefs):
        '''Touches all of node's neighbors and all the tags of node's
        neighbors, and then removes node.'''
        self.touch(self.neighbors(node))
        self.touch(self.tags_of(self.neighbors(node)))
        for n in as_iter(node):
            if self.has_node(n):
                if ShowPrimitives:
                    print('removed node', self.long_nodestr(n))
                self._remove_node(self.as_nodeid(n))

    def add_edge(
        self,
        nodes1: NRefs,
        port_label1: PortLabels,
        nodes2: NRefs,
        port_label2: PortLabels,
        **attr
    ):
        for fromid in as_nodeids(nodes1):
            for fromlabel in as_iter(port_label1):
                for toid in as_nodeids(nodes2):
                    for tolabel in as_iter(port_label2):
                        self._add_edge(fromid, fromlabel, toid, tolabel, **attr)
                        self.touch(fromid)
                        self.touch(toid)
                        if ShowPrimitives:
                            print(
                                'added edge ',
                                self.hopstr(fromid, fromlabel, toid, tolabel),
                                attr
                            )

    def edge_weight(
        self,
        node1: NRef,
        port_label1: PortLabel,
        node2: NRef,
        port_label2: PortLabel,
    ):
        return self._edge_weight(
            self.as_nodeid(node1), port_label1,
            self.as_nodeid(node2), port_label2
        )

    def has_edge(
        self,
        nodes1: NRefs,
        port_label1: PortLabels,
        nodes2: NRefs,
        port_label2: PortLabels
    ) -> bool:
        '''Returns True iff all the nodes specified have edges between all
        the port labels specified.'''
        for fromid in as_nodeids(nodes1):
            for fromlabel in as_iter(port_label1):
                for toid in as_nodeids(nodes2):
                    for tolabel in as_iter(port_label2):
                        if not self.find_hop(fromid, fromlabel, toid, tolabel):
                            return False
        return True

    def has_hop(
        self,
        from_node: NRefs,
        from_port_label: PortLabels,
        to_node: NRefs,
        to_port_label: PortLabels
    ):
        return all(
            self.find_hop(fromid, fromlabel, toid, tolabel)
                for fromid in as_nodeids(from_node)
                    for fromlabel in as_iter(from_port_label)
                        for toid in as_nodeids(to_node)
                            for tolabel in as_iter(to_port_label)
        )

    def remove_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
    ):
        '''It is not an error to remove an edge that does not exist. Regardless,
        we "touch" both nodes.'''
        for fromid in as_nodeids(node1):
            for fromlabel in as_iter(port_label1):
                for toid in as_nodeids(node2):
                    for tolabel in as_iter(port_label2):
                        self._remove_edge(fromid, fromlabel, toid, tolabel)
                        self.touch(fromid)
                        self.touch(toid)
                        if ShowPrimitives:
                            print('removed edge', self.nodestr(fromid), fromlabel, self.nodestr(toid), tolabel)

    # TODO UT
    def neighbors(
        self,
        nodes: NRefs,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None,
        exclude_port_label: PortLabels = frozenset(['copy_of', 'copies'])
    ) -> Set[NodeId]:
        hops = set()
        for node in as_iter(nodes):
            if port_label is None:
                hops |= self.hops_from_node(node)
            else:
                for pl in as_iter(port_label):
                    hops |= self.hops_from_port(node, pl)
        if exclude_port_label:
            exclude_port_label = as_set(exclude_port_label)
            for hop in list(hops):
                if hop.from_port_label in exclude_port_label:
                    hops.discard(hop)
        if neighbor_class is not None:
            for hop in list(hops):
                if not self.is_of_class(hop.to_node, neighbor_class):
                    hops.discard(hop)

        if neighbor_label is None:
            result = set(hop.to_node for hop in hops)
        else:
            pls = as_set(neighbor_label)
            result = set(
                hop.to_node
                    for hop in hops
                        if hop.to_port_label in pls
            )
        return result

    #TODO UT
    def neighbor(
        self,
        node: NRef,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None
    ) -> MaybeNRef:
        '''Returns the 'first' neighbor. TODO Define 'first' better.'''
        return first(self.neighbors(
            node, port_label, neighbor_class, neighbor_label
        ))

    def walk(
        self,
        nodes: NRefs,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None,
        include_start = True,
        max_hops: Union[int, None] = None
    ) -> Iterable[NodeId]:
        seen: Set[NodeId] = set()
        to_visit: List[Tuple(NodeId, int)] = []  # int is # of hops
        for node in as_iter(nodes):
            nodeid = as_nodeid(node)
            if nodeid not in seen:
                if include_start:
                    yield nodeid
                seen.add(nodeid)
                to_visit.append((nodeid, 0))
        while to_visit:
            nodeid, num_hops = to_visit.pop(0)
            for n in self.neighbors(
                nodeid, port_label, neighbor_class, neighbor_label
            ):
                if n not in seen:
                    yield n
                    seen.add(n)
                    if max_hops is None or num_hops + 1 < max_hops:
                        to_visit.append((n, num_hops + 1))

    # TODO UT
    def link_sequence(self, nodes: Iterable[NRef]):
        nodes = list(nodes)
        for prev, next in zip(nodes[:-1], nodes[1:]):
            self.add_edge(prev, 'next', next, 'prev')

    def inhibit_all_next(self, node: NRefs):
        for n in as_iter(node):
            self.as_node(n).inhibit_all_next()

    # as_ functions

    def as_nodeid(self, nref: NRef) -> Union[NodeId, None]:
        return as_nodeid(nref)

    def as_nodeids(self, nrefs: NRefs) -> Set[NodeId]:
        return as_nodeids(nrefs)

    def as_node(self, nref: NRef) -> Union[Node, None]:
        return as_node(self, nref)

    def as_nodes(self, nrefs: NRefs) -> Iterable[Node]:
        return as_nodes(self, nrefs)

    def as_nodeclasses(self, crefs: Union[CRefs, None]) -> List[Type[Node]]:
        return [self.as_nodeclass(cref) for cref in as_iter(crefs)]

    def as_nodeclass(self, cref: CRef) -> Type[Node]:
        if isclass(cref):
            assert issubclass(cref, Node)
            return cref
        elif is_nodeid(cref):
            node = self.as_node(cref)
            if not node:
                raise NoSuchNode(node)
            return node.__class__
        else:
            assert isinstance(cref, str)
            try:
                return self.nodeclasses[cref]
            except KeyError:
                raise NoSuchNodeclass(cref)

    def is_nref(self, x: Any) -> bool:
        '''Is x a reference to a Node, i.e. a nodeid or a Node object? Returns
        True even if x is not a node in the graph (e.g. if x is a deleted node
        or a Node that hasn't yet been given an id).'''
        return is_nodeid(x) or isinstance(x, Node)

    def is_nrefs(self, x: Any) -> bool:
        '''Is x a reference to zero or more Nodes, as defined by .is_nref()?
        Returns True if x is itself a Node reference or if x is an iterable
        containing only Node references. Hence returns True if x is an empty
        list or tuple.'''
        if self.is_nref(x):
            return True
        if is_iter(x):
            return all(self.is_nref(elem) for elem in x)
        return False
        
    # Node-building

    def already_built(
        self,
        nodeclas: Union[Type[Node], MaybeNRef],
        *args,     # ignored if nodeclas is a Node
        **kwargs   # ignored if nodeclas is a Node
    ) -> Union[Node, None]:
        '''Returns None if the specified node is not already built, or the Node
        if it is.  If kwargs provides a non-false value for 'copy_of', we
        automatically return None. A nodeclass with .is_duplicable == True is
        never deemed already built. If nodeclas is a Node object, we look for
        existing nodes with the same class and parameters, getting the Node
        object's parameters by calling its .regen_kwargs() method.'''
        if kwargs.get('copy_of', None):
            return None
        if not nodeclas:
            return None
        if is_nodeid(nodeclas):
            nodeclas = self.as_node(nodeclas)
        if isinstance(nodeclas, Node):
            args = ()
            kwargs = nodeclas.regen_kwargs()
            nodeclas = nodeclas.__class__
        if nodeclas.is_duplicable:
            return None

        filled_params = nodeclas.make_filled_params(self, *args, **kwargs)
        if filled_params.specifies_mates():
            candidates = self.neighbors(filled_params.potential_neighbors())
        else:
            candidates = self.nodes()
        return self.as_node(first(
            candidate
                for candidate in candidates
                    if (
                        self.class_of(candidate) is nodeclas
                        and
                        filled_params.is_match(self, nodeclas, candidate)
                    )
        ))

    def auto_link(self, from_node: NRef, port_label: PortLabel, to_node: NRef):
        '''Links from_node.port_label to to_node at appropriate port_label
        for to_node, or throws an exception.'''
        self.port_mates.auto_link(self, from_node, port_label, to_node)

    def add_implicit_membership(self, node: Node):
        '''If nodeid has no member_of link, add link to the "lowest common
        denominator" member_of all its neighbors.'''
        if self.has_neighbor_at(node, 'member_of'):
            return
        container_sets: Set[Set[NRef]] = set()
        for n1 in self.neighbors(node):
            n1_containers = self.containers_of_recursive(n1)
            if n1_containers:
                container_sets.add(frozenset(n1_containers))
        containers = intersection(*container_sets)
        containers.discard(as_nodeid(node))
        for c in containers:
            self.put_in_container(node, c)

    def containers_of_recursive(self, node: NRef) -> Set[NodeId]:
        result = set()
        for container in self.containers_of(node):
            result.add(container)
            result |= self.containers_of_recursive(container)
        return result

    def containers_of(self, node: NRef) -> Set[NodeId]:
        return self.neighbors(node, 'member_of')

    # TODO Specify tag_port_label and/or infer it
    def add_tag(self, tag: MaybeCRef, node: NRefs) \
    -> MaybeNRef:
        if isclass(tag) or isinstance(tag, str):
            #assert tag.is_tag
            return self.add_node(tag, taggees=node)
        elif tag is None:
            return
        else:
            assert is_nodeid(tag) or isinstance(tag, Node)
            #TODO UT
            if not self.has_node(tag):
                return self.add_node(tag, taggees=node)
            else:
                self.add_edge(tag, 'taggees', node, 'tags')
                return tag

    def copy_node(self, old_node: NRef, **kwargs):
        # HACK Should be deepcopy, but getting an error: NoneType is not
        # callable.
        old_node = self.as_node(old_node)
        datum = copy(old_node)
        return self.add_node(datum, copy_of=old_node.id, **kwargs)
        
    def copy_group(
        self,
        original_group_node: NRef,
        destination_group_node: NRef
    ) -> Union[NodeId, None]:
        # TODO Handle case where nothing gets copied, or disallow it.
        '''Returns nodeid of new group node.'''
        d = {}  # Maps source nodes to new nodes
        original_group_node = self.as_nodeid(original_group_node)
        destination_group_node = self.as_nodeid(destination_group_node)

        # Copy the nodes
        old_nodeids = (
            self.members_recursive(original_group_node)
            |
            {original_group_node}
        )
        for old_nodeid in old_nodeids:
            d[old_nodeid] = self.copy_node(old_nodeid)

        # Link to destination_group_node
        self.add_edge(
            d[original_group_node], 'member_of',
            destination_group_node, 'members'
        )

        # Copy the edges
        for old_nodeid, new_node in d.items():
            for hop in self.hops_from_node(old_nodeid):
                try:
                    new_mate = d[hop.to_node]
                except KeyError:
                    continue
                self.add_edge(
                    new_node, hop.from_port_label, new_mate, hop.to_port_label,
                    weight=self._hop_weight(hop)
                )

        return d[original_group_node]

    def add_next_member(
        self,
        group: NRef, 
        nspec: Union[Type[Node], Node, str],
        *args,
        **kwargs
    ) -> Node:
        lm = self.last_member(group)
        node = self.add_node(nspec, *args, **kwargs, member_of=group)
        self.add_edge(lm, 'next', node, 'prev')
        return node

    # TODO UT
    def last_member(self, group: MaybeNRef) -> MaybeNRef:
        return self.look_for(NoMate('next'), subset=self.members_of(group))

    def mark_builder(self, built_node: MaybeNRef, builder: MaybeNRef):
        self.add_edge(built_node, 'built_by', builder, 'built')

    # Interrogating nodes

    def class_of(self, node: NRef) -> Union[Type[Node], None]:
        '''Returns None if node does not exist.'''
        datum = self.datum(node)
        if datum is None:
            return None
        else:
            return datum.__class__
            
    #TODO UT
    def is_of_class(self, nrefs: NRefs, nodeclasses: CRefs) -> bool:
        '''Returns True iff all nodes referenced are instances of any the
        nodeclasses referenced. Returns False if there are no node references
        or no nodeclasses.'''
        nodes = as_list(self.as_nodes(nrefs))
        if not nodes:
            return False
        nodeclasses = self.as_nodeclasses(nodeclasses)
        if not nodeclasses:
            return False
        return all(
            any(isinstance(node, cl) for cl in nodeclasses)
                for node in nodes
        )

    def is_member(self, node: NRefs, container_node: NRefs):
        return self.has_hop(container_node, 'members', node, 'member_of')

    def value_of(self, nref: NRef, attr_name: str='value') -> Any:
        try:
            return getattr(self.as_node(nref), attr_name)
        except AttributeError:
            return None

    def has_tag(
        self,
        node: NRefs,
        tagclass: Union[Type[Node], NodeId, Node, str]='Tag',
        taggee_port_label: PortLabel='tags'
    ) -> bool:
        '''Returns True iff all the nodes have the given tag.'''
        if isinstance(tagclass, str):
            try:
                tagclass = self.as_nodeclass(tagclass)
            except NoSuchNodeclass:
                if tagclass == 'Failed' or tagclass == 'Blocked':
                    return False
                else:
                    raise
        if isclass(tagclass):
            return all(
                self.has_neighbor_at(
                    n, port_label=taggee_port_label, neighbor_class=tagclass
                )
                    for n in as_iter(node)
            )
        else: #tagclass is actually a node, not a class
            if isinstance(tagclass, Node) and tagclass.id is None:
                # tagclass is just a Node object to compare against, not
                # an actual node in the graph.
                return all(
                    self.at_least_one_eq_node(
                        tagclass,
                        self.neighbors(n, port_label=taggee_port_label)
                    ) for n in as_iter(node)
                )
            else:
                tagid = self.as_nodeid(tagclass)
                return all(
                    tagid in self.neighbors(n, port_label=taggee_port_label)
                        for n in as_iter(node)
                )

    def at_least_one_eq_node(self, nobject: Node, candidates: NRefs) -> bool:
        '''Returns True if at least one member of candidates == nobject.
        It's OK if nobject does not exist in the graph or does not have an id.
        We are only comparing against a Node object, not necessarily an
        actual node in the graph.'''
        for c in as_iter(candidates):
            #print('ATL', nobject, c, self.as_node(c), nobject == self.as_node(c))
            if nobject == self.as_node(c):
                return True
        else:
            return False

    def is_tag(self, node: NRefs) -> bool:
        return all(n and n.is_tag for n in self.as_nodes(node))

    def builder_of(self, node: MaybeNRef):
        return self.neighbor(node, port_label='built_by')

    def is_built_by(self, node: NRefs, builder_node: MaybeNRef) -> bool:
        '''Was node built by builder_node, as shown in the graph?'''
        return all(
            self.has_edge(n, 'built_by', builder_node, 'built')
                for n in self.as_nodes(node)
        )

    #TODO UT
    def has_neighbor_at(
        self,
        node: NRefs,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None
    ) -> bool:
        '''Returns True if at least one node given has a neighbor with the
        specified characteristics.'''
        # INEFFICIENT Instead of calling .neighbors(), should stop searching
        # at first match.
        return bool(self.neighbors(
            node, port_label, neighbor_class, neighbor_label
        ))

    def add_override_node(
        self,
        node: NRef,
        port_label: PortLabel,
        overriding_node: NRef
    ):
        '''Adds an edge from node.port_label to overriding_node.overriding.
        This signifies that overriding_node should be the value of the
        argument named port_label when running any Action inside node.'''
        self.add_edge(node, port_label, overriding_node, 'overriding')

    def get_overrides(
        self,
        node: NRef,
        names: Union[str, Iterable[str], None]
    ) -> Dict[str, Any]:
        result = {}
        for name in as_iter(names):
            if self.is_plural_port_label(name):
                result[name] = self.neighbors(node, port_label=name)
            else:
                n = self.neighbor(node, port_label=name)
                if n:
                    if name == 'value':  # HACK
                        result[name] = self.value_of(n)
                    else:
                        result[name] = n
        return result

    def is_plural_port_label(self, port_label: PortLabel) -> bool:
        # Numbo-specific HACK
        return port_label == 'consume_operands'

    # Querying the graph

    # TODO Better: Pass OfClass to .nodes()
    def nodes_of_class(
        self,
        cls: Iterable[Union[Type[Node], Type[Action]]],
        nodes: NRefs=None
    ) -> List[NRef]:
        result = []
        if nodes is None:
            nodes = self.nodes()
        for node in nodes:
            for cl in as_iter(cls):
                if issubclass(cl, Node):
                    if isinstance(self.as_node(node), cl):
                        result.append(node)
                else:
                    assert issubclass(cl, Action)
                    if (
                        isinstance(self.as_node(node), ActionNode)
                        and
                        isinstance(self.getattr(node, 'action'), cl)
                    ):
                        result.append(node)
        return result

    def node_of_class(self, cl: Type[Node], nodes=None) -> MaybeNRef:
        # TODO Choose in a more principled way. And maybe call look_for()
        # to do the search.
        if nodes is None:
            nodes = self.nodes()
        for node in as_iter(nodes):
            if self.is_of_class(node, cl):
                return node
        return None

    def look_for(
        self,
        criterion: Union[Criterion, Sequence[Criterion]],
        within: MaybeNRef=None,
        subset: Union[Set[NodeId], None]=None,
        tupcond: Callable[[Tuple[NRef], PortLabel], bool]=always_true
    ) -> Union[NodeId, None, Tuple[NodeId]]:
        '''Returns one node that meets criteria, or None if not found.
        criteria are functions that take two arguments: g, nodeid, and
        return a true value if nodeid matches the criterion.'''
        #TODO Document Cartesian product
        nodes_or_tups = self.find_all(
            criterion, within=within, subset=subset, tupcond=tupcond
        )
        try:
            return choice(nodes_or_tups) # TODO choose by salience?
        except IndexError:
            return None

    def find_all(
        self,
        criterion: Union[Criterion, Sequence[Criterion]],
        within: MaybeNRef=None,
        subset: Union[Set[NodeId], None]=None,
        tupcond: Callable[[Tuple[NRef], PortLabel], bool]=always_true
    ) -> Union[List[NodeId], List[Tuple[NodeId]]]:
        '''Returns list of all nodes that meet criteria. criteria are functions
        that take two arguments: g and nodeid. A criterion function returns
        a true value if nodeid matches the criteria, false if not.'''
        #TODO Document Cartesian product
        if within is None:
            nodes = self.nodeids()  # Start with all nodes (INEFFICIENT)
        else:
            nodes = self.members_recursive(within)
        if subset is not None:
            nodes = as_set(subset).intersection(nodes)

#        for c in criteria:
#            c = self.as_criterion(c)
#            nodes = [n for n in nodes if c(self, n)]
#            if not nodes:
#                return []
#        return as_list(nodes)
        if is_iter(criterion):
            return list(
                tup for tup in product(
                    *(self.find_all(c, within=within, subset=subset)
                        for c in criterion
                    )
                ) if len(set(tup)) == len(tup) and tupcond(self, tup)
            )
        else:
            criterion = self.as_criterion(criterion)
            return [n for n in nodes if criterion(self, n)]

    def as_criterion(self, x: Union[Node, CRef, Criterion]) -> Criterion:
        if isinstance(x, Criterion):
            return x
        elif isinstance(x, Node):
            return NodeEq(x)
        elif isclass(x) and issubclass(x, Node):
            return OfClass(x)
        assert False, f"Can't convert {x} to Criterion"

    def partition_nodes(self, nodes: NRefs, gpred: Callable[['G', NRef], bool]):
        '''Returns a tuple of lists: (those satisfying gpred, those not).
        gpred takes two arguments: g, nref.'''
        yes = []
        no = []
        for node in as_iter(nodes):
            if gpred(self, node):
                yes.append(node)
            else:
                no.append(node)
        return yes, no

    def members_of(self, container_node: NRefs) -> List[NodeId]:
        return list(self.neighbors(container_node, 'members'))

    #TODO UT
    def members_recursive(self, container_node: MaybeNRef) -> Set[NodeId]:
        result = set()
        if not container_node:
            return result
        container_node = as_nodeid(container_node)
        visited = set()
        to_visit = set([container_node])
        while to_visit:
            members = set()
            for node in to_visit:
                for m in self.members_of(node):
                    members.add(m)
            result |= members
            visited |= to_visit
            to_visit = members - visited
        return result

    def list(self, *nodes, **kwargs) -> List[NodeId]:
        '''For debugging. Called by pg() to get ordered list of nodes to
        print.'''
        if not nodes:
            return sorted(self.nodeids())

        result: List[NodeId] = []
        def add(nref: MaybeNRef):
            nodeid = self.as_nodeid(nref)
            if nodeid and nodeid not in result:
                result.append(nodeid)

        def iterate(nodes: Iterable[MaybeNRef]):
            for node in nodes:
                if is_nodeid(node) or isinstance(node, Node):
                    add(node)
                elif is_iter(node):
                    iterate(node)
                elif isclass(node):
                    for n in self.nodes_of_class(node):
                        add(n)

        iterate(nodes)
        return result

    # Port labels

    def is_port_label(self, name: str) -> bool:
        return self.port_mates.is_port_label(name)

    def port_labels_of(self, nref: NRef) -> Set[PortLabel]:
        return set(hop.from_port_label for hop in self.hops_from_node(nref))

    # Doing things

    def do_action(self, action: Union['Action', None], actor: MaybeNRef=None):
        if not action:
            return
        if actor:  # TODO rm
            action.actor = actor  # TODO rm
        with PushAttr(self, 'builder'):
            self.builder = action.actor
            self.prev_actions.append(action)
            try:
                action.go(self, actor)
            except ActionBlocked as exc:
                if ShowActionsPerformed or ShowPrimitives:
                    print('blocked:', action, exc)
                try:
                    self.call_method(exc.actor, 'action_blocked', exc)
                except Exception as exc2:
                    print(f'\nEXCEPTION in do_action at t={self.t} WHILE RECOVERING FROM {exc}:')
                    #TODO OAOO
                    try:
                        print(f'ACTOR: {self.nodestr(action.actor)}')
                    except AttributeError:
                        pass
                    print(f'ACTION: {action}')
                    print(f'EXC2: {exc2}')
                    raise
            except ActionFailure as exc:
                if ShowActionsPerformed:
                    print('failed:', action, exc)
                try:
                    self.call_method(exc.actor, 'action_failed', exc)
                except Exception as exc2:
                    print(f'\nEXCEPTION in do_action at t={self.t} WHILE RECOVERING FROM FAILURE {exc}:')
                    #TODO OAOO
                    try:
                        print(f'ACTOR: {self.nodestr(action.actor)}')
                    except AttributeError:
                        pass
                    print(f'ACTION: {action}')
                    print(f'EXC2: {exc2}')
                    raise
            except FargDone:
                raise
            except:
                print(f'\nEXCEPTION in do_action at t={self.t}')
                try:
                    #print(f'ACTOR: {self.nodestr(action.actor)}  ON BEHALF OF: {self.nodestr(action.on_behalf_of)}')
                    print(f'ACTOR: {self.nodestr(action.actor)}')
                except AttributeError:
                    pass
                print(f'ACTION: {action}')
                raise

    do = do_action

    def set_attr(self, nrefs: NRefs, name: str, v: Any):
        for nref in as_iter(nrefs):
            if nref:
                setattr(self.as_node(nref), name, v)

    def put_in_container(self, node: NRefs, container: NRefs):
        #TODO Don't allow a node to contain itself. Or should that get caught
        # by some "proofreading" process in the model?
        self.add_edge(node, 'member_of', container, 'members')

    def is_dormant(self, nref: NRef):
        '''Is node in a dormant state, i.e. not capable of generating any
        Actions right now?'''
        return self.call_method(nref, 'is_dormant')

    def is_failed(self, node):
        return self.has_tag(node, 'Failed')  # TODO not if Failed is canceled

    def is_blocked(self, node):
        return self.has_tag(node, 'Blocked')

    def call_method(self, nref: MaybeNRef, method_name: str, *args, **kwargs):
        '''Returns result of calling method method_name(self, nodeid) on nodeid
        if nodeid exists and its datum has a callable attr named method_name.
        Otherwise returns None.'''
        '''EXPERIMENT 9-Sep-2020: raise NodeLacksMethod if the method does not
        exist.'''
        d = self.datum(nref)
        if d is None:
            return None
        m = getattr(d, method_name)
        if callable(m):
            return m(*args, **kwargs)
        else:
            raise NodeLacksMethod(nref, method_name, args, kwargs)

    def callable(self, nref: MaybeNRef, method_name: str) -> bool:
        return callable(self.getattr(nref, method_name))

    def getattr(self, nref: MaybeNRef, attrname: str) -> Union[Any, None]:
        node = self.as_node(nref)
        return getattr(node, attrname)

    def behalf_of(self, nref: MaybeNRef) -> MaybeNRef:
        result = self.neighbor(nref, port_label='behalf_of')
        if result:
            return result
        else:
            return self.neighbor(nref, port_label='built_by')

    def new_state(self, node: NRef, state: 'ActiveNodeState'):
        node = self.as_node(node)
        if node:
            node.state = state
            if state.is_completed:
                node.on_completion()

    #TODO UT
    def move_tag(self, tagclass, fromids, toids):
        '''Moves all tags of class tagclass from fromids to toids. fromids and
        toids can be either a single integer or a list of integer node ids.
        If none of the fromids have such a tag, does nothing.'''
        if self.remove_tag(fromids, tagclass):
            for toid in as_iter(toids):
                self.add_tag(tagclass, toid)

    def tags_of(
        self, nodes: NRefs, tagclass: CRefs='Tag', taggee_port_label='tags'
    ) -> Set[NodeId]:
        '''Returns a generator. nodes can be a single node id or an iterable
        of node ids.'''
        #TODO Should be able to specify tagclass more narrowly.
        tag_sets = (
            set(tag
                for node in as_iter(nodes)
                    for tag in self.neighbors(
                            node, port_label=taggee_port_label
                        )
                        if self.is_of_class(tag, tagclass)
            )
        )
        return set.intersection(tag_sets)

    def tag_of(self, node, tagclass='Tag', taggee_port_label='tags'):
        #TODO Handle multiple taggees, different port labels
        try:
            return next(iter(self.tags_of(
                node, tagclass=tagclass, taggee_port_label=taggee_port_label
            )))
        except StopIteration:
            return None
        
    def tags_without_agent(self, node: NRefs, tagclass: CRefs) -> NRefs:
        result = set()
        agents = self.neighbors(node, 'agents')
        for tag in self.tags_of(node, tagclass):
            for agent in agents:
                if tag in self.neighbors(agent, 'problem'):
                    break
            else:
                result.add(tag)
        return result

    #TODO Consistent argument order: put tag_or_tagclass first?
    def remove_tag(
        self,
        node_or_nodes: NRefs,
        tag_or_tagclass: Union[NRef, CRef]
    ):
        '''Removes all tags of node that match tagclass. Returns True iff at
        last one of node_or_nodes was actually so tagged.'''
        #TODO Should only remove the edge if the tag tags other nodes, too.
        #TODO Should be able to specify the tagclass more narrowly (e.g.
        # by value, by other taggees).
        result = False
        if isclass(tag_or_tagclass):
            tagids = list(self.tags_of(node_or_nodes, tagclass=tag_or_tagclass))
            if tagids:
                self.remove_node(tagids)
            result = True  # TODO indent
        else:
            # tag_or_tagclass is an NRefs
            self.remove_node(tag_or_tagclass)
            result = True # HACK BUG We should check that node_or_nodes really
                          # has the given tag node.
        return result

    # Timestepping (TODO: make this section into a mix-in)

    def do_timestep(
        self,
        num=1,
        action: Union[Action, List[Action], None]=None,
        actor: MaybeNRef=None
    ) -> None:
        try:
            for i in range(num):
                self.t += 1
                self.prev_new_nodes = set(self.new_nodes)
                self.new_nodes.clear()
                self.prev_touched_nodes = set(self.touched_nodes)
                self.touched_nodes.clear()
                self.prev_actions.clear()

                if any(
                    l for l in (ShowActiveNodes, ShowActiveNodesCollected, ShowActionList, ShowActionsChosen, ShowActionsPerformed, ShowPrimitives)
                ):
                    #print(f'{chr(10)}t={self.t}')
                    pt(self)

                self.propagate_support()
                self.log_support()

                self.propagate_activation()
                self.log_activation()

                #self.update_coarse_views()

                if action is not None:
                    actions_to_do = as_list(action)
                elif actor is not None:
                    actions_to_do = self.collect_actions([actor])
                else:
                    actions_to_do = self.collect_actions_from_graph()

                if ShowActionsPerformed.is_logging():
                    print('ACTIONS PERFORMED')
                    self.print_actions_header(actions_to_do)
                for a in actions_to_do:
                    if ShowActionsPerformed.is_logging():
#                                     a))
                        self.print_action(a)
                    self.do_action(a, a.actor)

                self.do_touches()
                self.update_all_asup()

                d = self.done()
                if d:
                    ShowResults(d)
                    #ShowResults(f"t={self.graph['t']}\n")
                    break
        except FargDone as exc:
            self.final_result = exc
            ShowResults(str(exc))

    def update_all_asup(self):
        '''Calls .update_asup() on all allowable_active_nodes.'''
        for node in self.allowable_active_nodes():
            self.call_method(node, 'update_asup')

    def do_actions1(self, actions: Actions):
        '''Force a sequence of actions, one per timestep. If a single Action
        is an iterable, then all the Actions it contains will be performed
        on its timestep.'''
        for a in as_iter(actions):
            self.do_timestep(action=a)

    def collect_actions_from_graph(self):
        active_nodes = self.collect_active_nodes()
        if ShowActiveNodesCollected.is_logging():
            print('ACTIVE NODES COLLECTED')
            for node in sorted(active_nodes, key=self.display_name):
                print(self.long_nodestr(node))

        actions = self.collect_actions(active_nodes)

        actions = [a for a in actions if self.urgency(a) > 0.0]

        # TODO Boost activation or lower standards if too many timesteps
        # have gone by with no action.

        chosen_actions = self.choose_actions(actions)
        if ShowActionsChosen.is_logging():
            print('ACTIONS CHOSEN')
            self.print_actions(chosen_actions)
        return chosen_actions

    def collect_active_nodes(self) -> List[NRef]:
        active_nodes = self.active_nodes()
        if ShowActiveNodes.is_logging():
            print('ACTIVE NODES')
            for node in sorted(active_nodes, key=self.display_name):
                print(self.long_nodestr(node))
        return self.choose_active_nodes(active_nodes)

    def active_nodes(self) -> List[NRef]:
        '''Returns list of all non-dormant allowable ActiveNodes.'''
        return [
            node for node in self.nodes_of_class(
                    ActiveNode,
                    nodes=self.allowable_active_nodes()
                )
                    #if not self.is_dormant(node)  #TODO rm
                    if self.as_node(node).can_go()
        ]

    def is_active(self, node: NRef) -> bool:
        try:
            return self.call_method(node, 'can_go')
        except NodeLacksMethod:
            return False

    def choose_active_nodes(
        self,
        active_nodes: List[NRef],
        k: Union[int, None]=None
    ) -> List[NRef]:
        if k is None:
            k = self.max_active_nodes
        if (self.max_active_nodes is None
            or
            len(active_nodes) <= self.max_active_nodes
        ):
            return active_nodes
        return list(sample_without_replacement(
            active_nodes,
            k=k,
            #weights=[self.support_for(node) for node in active_nodes]
            weights=[self.activation(node) for node in active_nodes]
        ))

    def actions(self, nref: NRef) -> Actions:
        # TODO Refactor: just call .datum() once; never call .has_node().
        if not self.has_node(nref):
            return None
        if self.datum(nref).needs_update:
            result = self.datum(nref).update()
            if result:
                return result
        return self.datum(nref).actions()

    def collect_actions(self, active_nodes: NRefs) -> List[Action]:
        actions = []
        for node in as_iter(active_nodes):
            try:
                got = self.actions(node)
            except:
                print('EXCEPTION in .actions()')
                print(f'NODE: {self.nodestr(node)}')
                raise

            for action in as_iter(got):
                if action:
                    action.actor = node
                    actions.append(action)
        if ShowActionList.is_logging():
            print('ACTIONS COLLECTED')
            self.print_actions(actions)
        return actions

    def choose_actions(self, actions: Actions, k: Union[int, None]=None):
        '''Randomly chooses up to k Actions, weighted by .urgency.
        Returns a collection. k defaults to self.max_actions.'''
        if k is None:
            k = self.max_actions
        return list(sample_without_replacement(
            actions,
            k=k,
            weights=[self.urgency(a) for a in actions]
        ))
        
    def allowable_active_nodes(self) -> Iterable[NRef]:
        '''Returns generator containing all ActiveNodes in the 'ws' (the
        workspace), if a 'ws' has been set. Otherwise generator includes all
        ActiveNodes. The nodes are not necessarily in an active state.'''
        if not self.ws:
            result = self.nodes()
        else:
            result = self.members_recursive(self.ws)
            if self.slipnet:
                result.add(self.slipnet.id)
        return (
            node for node in result if self.is_of_class(node, ActiveNode)
        )

    def urgency(self, action: Action) -> float:
        support = self.support_for(action.actor)
        if support < action.support_threshold:
            return action.min_urgency
        activation = self.activation(action.actor)
        if activation < action.threshold:
            return action.min_urgency
        return max(
            activation - action.threshold,
            action.min_urgency
        )

    def min_activation(self, node: NRef) -> float:
        #TODO Return 0.0 if node does not exist?
        return as_node(self, node).min_activation

    def min_support_for(self, node: NRef) -> float:
        #TODO Return 0.0 if node does not exist?
        return as_node(self, node).min_support_for

    def touch(self, nrefs: NRefs):
        if not self.during_touch:
            self.touched_nodes |= self.as_nodeids(nrefs)

    def do_touch(self, node: NRef):
        if not self.during_touch and self.has_node(node):
            with PushAttr(self, 'during_touch'):
                self.during_touch = True
                self.boost_activation(node)
                self.as_node(node).on_touch()

    def do_touches(self):
        for nodeid in self.touched_nodes:
            self.do_touch(nodeid)
        for nodeid in self.after_touch_nodes:
            if self.has_node(nodeid):
                self.call_method(
                    nodeid,
                    'after_touch_update',
                    self.touched_nodes,
                    self.new_nodes
                )
            else:
                self.after_touch_nodes.discard(nodeid)

    def done(self) -> Union[FargDone, None]:
        return self.final_result

    def succeeded(self) -> bool:
        d = self.done()
        return isinstance(d, FargDone) and d.succeeded

    def action_sorting_key(self, action: Action) -> Tuple:
        return (
            self.urgency(action),
            self.activation(action.actor),
            self.support_for(action.actor)
        )

    def print_actions_header(self, actions: Sequence):
        if not actions:
            print('  (none)')
        else:
            headingfmt = '  %5s %5s %7s %5s %7s %-20s %s'
            headings = ('u', 'a', '(a.t)', 's', '(s.t)', 'actor', 'action')
            print(headingfmt % headings)

    def print_actions(self, actions: List[Action], header: bool=True):
#        if not len(actions):
#            print('  (none)')
#            return
        if header:
            self.print_actions_header(actions)
#        fmt =        '  %.3f %.3f (%.3f) %.3f (%.3f) %4d %s'
#        for action in sorted(actions, key=self.action_sorting_key):
#            print(fmt % (self.urgency(action),
#                         self.activation(action.actor),
#                         action.threshold,
#                         self.support_for(action.actor),
#                         action.support_threshold,
#                         self.as_nodeid(action.actor),
#                         action))
        for action in sorted(actions, key=self.action_sorting_key):
            self.print_action(action)

    def print_action(self, a: Action):
        fmt =        '  %.3f %.3f (%.3f) %.3f (%.3f) %-20s %s'
        print(fmt % (self.urgency(a),
                     self.activation(a.actor),
                     a.threshold,
                     self.support_for(a.actor),
                     a.support_threshold,
                     self.nodestr(a.actor).strip()[:20],
                     a))

    # Printing

    def display_name(self, node: MaybeNRef) -> str:
        node = self.as_node(node)
        if node is None:
            return 'None'
        else:
            return node.display_name()

    def nodestr(self, nref: MaybeNRef) -> str:
        if nref is None:
            return 'None'
        node = self.as_node(nref)
        if not node:
            return f'{nref}: (does not exist)'
        else:
            return node.nodestr()

    def long_nodestr(self, node: NRef) -> str:
        return '%-25s  a=%.3f s=%.3f   tob=%d    %s' % (
            self.nodestr(node),
            self.activation(node),
            self.support_for(node),
            self.tob(node),
            self.statestr(node)
        )

    def tob(self, nref: NRef) -> int:
        return self.as_node(nref).tob

    def statestr(self, node: NRef) -> str:
        node = self.as_node(node)
        if not node:
            return ''
        else:
            return node.statestr()

    def dict_str(self, nref: NRef) -> Union[str, None]:
        try:
            return self.as_node(nref).dict_str()
        except AttributeError:
            return None

    def hopstr(self, *args, prefix=''):
        if len(args) == 1:
            hop = args[0]
        elif len(args) == 4:
            hop = self.find_hop(
                as_nodeid(args[0]), args[1], as_nodeid(args[2]), args[3]
            )
        else:
            raise ValueError(f'print_hop() takes either a Hop or (node, port_label, node, port_label); got: {args}')
        #TODO append weight=%.3f
        return hop.hopstr(self)

    def long_hopstr(self, hop: Hop, prefix=''):
        return '%s%-15s --> %s  %s (%.3f)' % (
            prefix,
            hop.from_port_label,
            self.nodestr(hop.to_node),
            hop.to_port_label,
            self._hop_weight(hop)
        )

    def print_hops(self, hops: Iterable[Hop], prefix=''):
        for line in sorted(self.long_hopstr(hop) for hop in hops):
            print(line)
        
    def print_edges(self, node, from_port_label=None, prefix=''):
        if from_port_label:
            hops = self.hops_from_port(node, from_port_label)
        else:
            hops = self.hops_from_node(node)
        self.print_hops(
            sorted(hops, key=attrgetter('from_port_label')),
            prefix=prefix
        )

    def pg(self, *args, **kwargs):
        # TODO Move the implementation code inside ActiveGraph.
        pg(self, *args, **kwargs)

    # Unimplemented  # TODO

    def add_support(self, from_node, to_node, weight=None):
        return

G = ActiveGraph

@dataclass
class Context(ABC):

    @classmethod
    @abstractmethod
    def within(cls, g: G, actor: NRef) -> MaybeNRef:
        '''Should return the group node that is the relevant context
        for 'actor'.'''
        pass

    @staticmethod
    def is_context(x: Any) -> bool:
        '''Is x a Context?'''
        return isclass(x) and issubclass(x, Context)

@dataclass
class MyContext(Context):

    @classmethod
    def within(cls, g: G, actor: NRef) -> MaybeNRef:
        result = g.neighbor(actor, 'member_of')
        if g.is_of_class(result, 'ActionSeqNode'):  # HACK
            return cls.within(g, result)
        else:
            return result

@dataclass
class InWorkspace(Context):

    @classmethod
    def within(cls, g: G, actor: NRef) -> MaybeNRef:
        return g.ws

def pt(g: G):
    '''Prints title with t= and other info about the graph.'''
    sum_a = sum(g.activation(n) for n in g.nodes())
    sum_s = sum(g.support_for(n) for n in g.nodes())
    print(f'\nt={g.t}    sum_a={sum_a:.3f}  sum_s={sum_s:.3f}')

def pg(g: G, *nodes, **kwargs):
    '''Prints graph g in simple text form.'''
    pt(g)
#    if nodes is None:
#        nodes = g.nodes()
#    elif isclass(nodes) and issubclass(nodes, Node):
#        nodes = g.nodes_of_class(nodes)
#    for node in g.as_nodes(nodes):
    for node in g.list(*nodes, **kwargs):
        print(g.long_nodestr(node))
        g.print_edges(node, prefix='      ')

def pa(g: G, *nodes, **kwargs):
    '''Prints activations of nodes.'''
    pt(g)
    for node in sorted(g.list(*nodes, **kwargs), key=g.activation):
        print(g.long_nodestr(node))

def ps(g: G, *nodes, **kwargs):
    pt(g)
    for node in sorted(g.list(*nodes, **kwargs), key=g.support_for):
        print('supp=%.3f  %s' % (
            g.support_for(node),
            g.nodestr(node)
        ))
        g.print_hops(g.support_hops_from(node), prefix='      ')
