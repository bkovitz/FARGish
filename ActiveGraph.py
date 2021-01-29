# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable, Sequence
from dataclasses import dataclass, field
from inspect import isclass
from copy import copy, deepcopy
from operator import attrgetter, itemgetter
from random import choice, choices
from itertools import product, chain
from collections import defaultdict

from Primitives import Hop, Hops, PortGraphPrimitives, ActiveGraphPrimitives, \
    ActivationPrimitives, ActivationPolicy, SupportPrimitives, SupportPolicy, \
    SlipnetPolicy
from Node import Node, NodeId, MaybeNodeId, PortLabel, PortLabels, is_nodeid, \
    NRef, NRefs, CRef, CRefs, MaybeNRef, MaybeCRef, \
    as_nodeid, as_node, as_nodeids, as_nodes, is_abstract_cref
from PortMates import PortMates
from Action import Action, Actions
from ActiveNode import ActiveNode, ActionNode, Sleeping
from Propagator import Propagator
from util import as_iter, as_list, as_set, is_iter, repr_str, first, reseed, \
    intersection, empty_set, sample_without_replacement, PushAttr, \
    always_true, filter_none, clip, reweight
from exc import NodeLacksMethod, NoSuchNode, NoSuchNodeclass, NeedArg, \
    FargDone, FizzleWithTag, Fizzle
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
        ('next', 'prev'), ('copy_of', 'copies'), ('problem', 'problem_solver'),
        ('activation_from', 'activation_to'), ('support_from', 'support_to'),
        ('completion', 'completion_of')
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

        self.port_mates: PortMates = deepcopy(self.std_port_mates)
        if port_mates:
            self.port_mates += port_mates
        self.nodeclasses: Dict[str, Type[Node]] = {}

        # Automatically link activation from new nodes of given type to
        # given node(s).
        self.activation_autolinks: List[Tuple[Type[Node], NRefs]] = []

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

    def add_activation_autolinks(self, *pairs: Tuple[Type[Node], NRefs]):
        self.activation_autolinks += pairs
        self.make_activation_links(self.members_recursive(self.ws), pairs)

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
        activation = kwargs.pop('activation', None)
        if isinstance(node, Node):  # We were given a Node object
            kwargs = {**node.regen_kwargs(), **kwargs}
            already = self.already_built(node, *args, **kwargs)
            if already:
                # TODO boost its activation
                return already
            self._add_node(node)
            if ShowPrimitives:
                print('built', self.long_nodestr(node))
            # TODO Apply link FilledParams?  DONE?
            filled_params = node.make_filled_params(self, *args, **kwargs)
            filled_params.apply_to_node(self, node.id)
        else:  # We were given some indication of the new node's class
            if isinstance(node, str) or is_nodeid(node):
                node = self.as_nodeclass(node)
                # TODO Catch/throw exception if it doesn't exist?
            already = self.already_built(node, *args, **kwargs)
            if already:
                # TODO boost its activation
                return already  # TODO Don't return yet; updates later
            assert issubclass(node, Node), f'{node} is not a subclass of Node'
            filled_params = node.make_filled_params(self, *args, **kwargs)
            node: Node = node()  # Create the Node object
            self._add_node(node, activation)
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

    def _add_node(
        self,
        node: Node,
        activation: Union[float, None]=None
    ) -> NodeId:
        '''Makes the node, sets its .id and .g members, its .tob ("time of
        birth"), its initial activation, and returns its id.'''
        id = super()._add_node(node)
        node.tob = self.t
        if activation is None:
            activation = node.initial_activation
        self.set_activation(node, activation)
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

    # TODO Allow either port_label1 or port_label2 to be None; fill in
    # default from port_mates.
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
                        tl = self.best_fitting_port_label(toid, tolabel)
                        self._add_edge(fromid, fromlabel, toid, tl, **attr)
                        self.touch(fromid)
                        self.touch(toid)
                        if ShowPrimitives:
                            print(
                                'added edge ',
                                self.hopstr(fromid, fromlabel, toid, tl),
                                attr
                            )

    def move_edge(
        self,
        nodes1: NRefs,
        port_label1: PortLabels,
        nodes2: NRefs,
        port_label2: PortLabels,
        **attr
    ):
        self.remove_hops_from_port(nodes1, port_label1)
        self.add_edge(nodes1, port_label1, nodes2, port_label2, **attr)

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
#        for fromid in as_nodeids(nodes1):
#            for fromlabel in as_iter(port_label1):
#                for toid in as_nodeids(nodes2):
#                    for tolabel in as_iter(port_label2):
#                        if not self.find_hop(fromid, fromlabel, toid, tolabel):
#                            return False
        fromlabel = self.port_mates.expand_port_label(port_label1)
        tolabel = self.port_mates.expand_port_label(port_label2)
        for fromid in as_nodeids(nodes1):
            for toid in as_nodeids(nodes2):
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
        '''Returns True iff all the specified hops exist. Unlike .has_edge(),
        .has_hop() does not expand port labels. The port labels much match
        exactly; port inheritance does not make a match.'''
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
        port_label = self.expand_port_label(port_label)
        exclude_port_label = self.expand_port_label(exclude_port_label)
        neighbor_label = self.expand_port_label(neighbor_label)
        for node in as_iter(nodes):
            if not port_label:
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

    # TODO UT
    def incidence_outgoing(self, node: MaybeNRef, port_label: PortLabels) \
    -> Dict[PortLabel, NRefs]:
#        node = self.as_node(node)
#        if not node:
#            return {}
        result: Dict[PortLabel, Set[NodeId]] = defaultdict(set)
        for hop in self.hops_from_node(node):
            for port_label in as_iter(port_label):
                if self.port_mates.isa(hop.from_port_label, port_label):
                    result[hop.from_port_label].add(hop.to_node)
        return result

    # TODO UT
    @staticmethod
    def prepend_port_label_prefix(prefix: str, d: Dict[PortLabel, Any]) \
    -> Dict[PortLabel, Any]:
        '''Returns d after prepending prefix to each key. Does not change d,
        and does not prepend the prefix if it's already there.'''
        return dict(
            (k if k.startswith(prefix) else prefix + k, v)
                for k, v in d.items()
        )

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

    def walkd(
        self,
        nodes: NRefs,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None,
        include_start = True,
        max_hops: Union[int, None] = None
    ) -> Iterable[Tuple[NodeId, int]]:
        '''Same as .walk() but generates tuples of
        (nodeid, distance-from-start-node[s]).'''
        seen: Set[NodeId] = set()
        to_visit: List[Tuple(NodeId, int)] = []  # int is # of hops
        for node in as_iter(nodes):
            nodeid = as_nodeid(node)
            if nodeid not in seen:
                if include_start:
                    yield (nodeid, 0)
                seen.add(nodeid)
                to_visit.append((nodeid, 0))
        while to_visit:
            nodeid, num_hops = to_visit.pop(0)
            for n in self.neighbors(
                nodeid, port_label, neighbor_class, neighbor_label
            ):
                if n not in seen:
                    yield (n, num_hops + 1)
                    seen.add(n)
                    if max_hops is None or num_hops + 1 < max_hops:
                        to_visit.append((n, num_hops + 1))

    # TODO UT
    # TODO rename chain_together
    def link_sequence(
        self,
        nodes: Iterable[NRef],
        prev_label: PortLabel='prev',
        next_label: PortLabel='next'
    ):
        nodes = list(nodes)
        for prev_node, next_node in zip(nodes[:-1], nodes[1:]):
            self.add_edge(prev_node, next_label, next_node, prev_label)

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
        elif isinstance(cref, str):
            try:
                return self.nodeclasses[cref]
            except KeyError:
                raise NoSuchNodeclass(cref)

        elif isinstance(cref, Node):
            return cref.__class__
        raise ValueError(
            f'{cref} is not a CRef (i.e. does not specify a nodeclass)'
        )

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
        filled_params.remove_ignored_for_dupcheck(nodeclas)
        if filled_params.specifies_mates():
            candidates = self.neighbors(filled_params.potential_neighbors())
        else:
            candidates = self.nodes()
        # TODO Refactor so that the (possible) generator in 'candidates'
        # never gets assigned to a named variable. (Good practice?)

        result = self.as_node(first(
            candidate
                for candidate in candidates
                    if (
                        self.class_of(candidate) is nodeclas
                        and
                        filled_params.is_match(self, nodeclas, candidate)
                    )
        ))
        #print('ALREADY', nodeclas, result)
        return result

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
        try:
            nodeclasses = self.as_nodeclasses(nodeclasses)
        except NoSuchNodeclass:
            return False
        if not nodeclasses:
            return False
        return all(
            any(isinstance(node, cl) for cl in nodeclasses)
                for node in nodes
        )

    # TODO UT
    def node_isa(
        self,
        nrefs: NRefs,
        specs: Union[CRefs, Node]
    ) -> bool:
        '''Does every node in 'nrefs' match 'specs'? False if nrefs is
        empty or contains a non-existent node.
        
        If specs is a Node object without an id, we match against class,
        allowing inheritance, and against attributes; we ignore mates.
        Otherwise we match against *any* nodeclass indicated by 'specs'.
        '''
        nodes = as_list(self.as_nodes(nrefs))
        if not nodes:
            return False
        nodeclasses = self.as_nodeclasses(specs)
        if not nodeclasses:
            return False

        for node in as_iter(nodes):
            if not any(isinstance(node, cl) for cl in nodeclasses):
                return False
            if isinstance(node, Node) and node.id is None:
                raise NotImplementedError
        return True

    def is_member(self, node: NRefs, container_node: NRefs):
        return self.has_hop(container_node, 'members', node, 'member_of')

    def value_of(self, nref: NRef, attr_name: str='value') -> Any:
        try:
            return getattr(self.as_node(nref), attr_name)
        except AttributeError:
            return None

    def has_tag(
        self,
        nodes: NRefs,
        tagclass: Union[Type[Node], NodeId, Node, str]='Tag',
        #taggee_port_label: PortLabel='tags'
        **kwargs
    ) -> bool:
        '''Returns True iff all the nodes have the given tag.'''
        # TODO Document kwargs
        return all(
            self._has_tag(node, tagclass, **kwargs)
                for node in as_iter(nodes)
        )
        # OLD
#        if isinstance(tagclass, str):
#            try:
#                tagclass = self.as_nodeclass(tagclass)
#            except NoSuchNodeclass:
#                if tagclass == 'Failed' or tagclass == 'Blocked':
#                    return False
#                else:
#                    raise
#
#        if isclass(tagclass):
#            return all(
#                self.has_neighbor_at(
#                    n, port_label=taggee_port_label, neighbor_class=tagclass
#                )
#                    for n in as_iter(node)
#            )
#        else: #tagclass is actually a node, not a class
#            if isinstance(tagclass, Node) and tagclass.id is None:
#                # tagclass is just a Node object to compare against, not
#                # an actual node in the graph.
#                return all(
#                    self.at_least_one_eq_node(
#                        tagclass,
#                        self.neighbors(n, port_label=taggee_port_label)
#                    ) for n in as_iter(node)
#                )
#            else:
#                tagid = self.as_nodeid(tagclass)
#                return all(
#                    tagid in self.neighbors(n, port_label=taggee_port_label)
#                        for n in as_iter(node)
#                )

    def _has_tag(
        self,
        node: MaybeNRef,
        tagclass: Union[Type[Node], NodeId, Node, str]='Tag',
        **kwargs
    ) -> bool:
        '''Like .has_tag() but looks only at a single node.'''
        if not kwargs:
            kwargs = {'taggees': self.as_nodeid(node)}
        return any(self.nb_match(tag, **kwargs)
            for tag in self.tags_of(node, tagclass)
        )

    def nb_match(self, node: MaybeNRef, **kwargs) -> bool:
        '''Neighborhood match.''' #TODO Document better.
        node = self.as_node(node)
        if not node:
            return False
        for port_label, neighbor in kwargs.items():
            if neighbor:
                if not as_nodeids(neighbor).intersection(
                    self.neighbors(node, port_label=port_label)
                ):
                    return False
            else: # if neighbor is unspecified, match any neighbor
                if not self.neighbor(node, port_label=port_label):
                    return False
        return True

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

    def choose_by_activation(
        self,
        nodes_or_tups: Union[List[NodeId], List[Tuple[NodeId]]]
    ) -> MaybeNRef:
#        if not nodes_or_tups:
#            return None
#        return choices(nodes_or_tups, self.activations_of(nodes_or_tups))[0]
        d = dict(
            (nodeid, self.activation(nodeid))
                for nodeid in self._uniq_in_tups(nodes_or_tups)
        )
        return self.choose_by_dict_weight(nodes_or_tups, d)

    def _uniq_in_tups(
        self,
        nodes_or_tups: Union[List[NodeId], List[Tuple[NodeId]]]
    ) -> Iterable[NodeId]:
        if isinstance(nodes_or_tups[0], tuple):
            return set(
                x
                    for tup in nodes_or_tups
                        for x in tup
            )
        else:
            return nodes_or_tups

    def choose_by_dict_weight(
        self,
        nodes_or_tups: Union[List[NodeId], List[Tuple[NodeId]]],
        d: Dict[NodeId, float]
    ) -> MaybeNRef:
        if not nodes_or_tups:
            return None
        weights = list(reweight(self._weighted(nodes_or_tups, d), s=0.78))
#        print('CBDW', nodes_or_tups, weights)
#        for nt, w in zip(nodes_or_tups, weights):
#            print(f'  {nt}  {w:.3f}')
        return choices(nodes_or_tups, weights)[0]

    def _weighted(
        self,
        nodes_or_tups: Union[List[NodeId], List[Tuple[NodeId]]],
        d: Dict[NodeId, float]
    ) -> List[float]:
        if not nodes_or_tups:
            return []
        if isinstance(nodes_or_tups[0], tuple):
            return [
                sum(d.get(node, 0.0) for node in tup)
                    for tup in nodes_or_tups
            ]
        else:
            return [d.get(node, 0.0) for node in nodes_or_tups]

    def _asum(self, nnn: Union[NRef, List[NRef], Tuple[NRef]]) \
    -> float:
        '''Returns the sum of the activations of all the NRefs passed in.'''
        if not nnn:
            return 0.0
        elif self.is_nref(nnn):
            return self.activation(nnn)
        elif (
            isinstance(nnn[0], tuple)
            or
            isinstance(nnn[0], list)
        ):
            return [
                sum(self.activation(node) for node in tup)
                    for tup in nnn
            ]
        else:
            return sum(self.activation(node) for node in nnn)

    def look_for(
        self,
        criterion: Union[Criterion, Sequence[Criterion]],
        focal_point: MaybeNRef=None,
        subset: Union[Set[NodeId], None]=None,
        tupcond: Callable[[Tuple[NRef], PortLabel], bool]=always_true
    ) -> Union[NodeId, None, Tuple[NodeId]]:
        '''Returns one node that meets 'criterion', or None if not found.'''
        #TODO Document Cartesian product, weighting, walking near.
        # If 'focal_point' is a node other than a Group node, we treat it as a
        # focal point, and only search near it.
        weight_by_distance = False
        if focal_point:
            if self.is_of_class(focal_point, 'Group'):
                nodes = self.members_recursive(focal_point)
            else:
                weight_by_distance = True
                nodesd = dict(self.walkd(
                    focal_point,
                    max_hops=5,
                    neighbor_label={
                        'taggees', 'tags', 'built', 'built_by',
                        'source', 'consumer',   # are these two HACKs?
                        'next', 'prev',
                        'problem', 'problem_solver',
                        'member_of', 'members' # TODO longer distance?
                    }
                    # TODO Make it easy to override neighbor_label.
                ))
                nodes = nodesd.keys()
            if subset is None:
                subset = as_set(nodes)
            else:
                subset = subset.intersection(nodes)
        else:
            nodes = subset

        nodes_or_tups = self.find_all(
            criterion, subset=nodes, tupcond=tupcond
        )
        if weight_by_distance:
            nodesw = dict(
                #(nodeid, self.activation(nodeid) / abs(dist + 1) ** 10)
                (nodeid, self.activation(nodeid) / max(dist + 1, 1))
                    for nodeid, dist in nodesd.items()
            )
#            nodesw = dict(zip(
#                nodesw.keys(), reweight(nodesw.values(), 0.8)
#            ))
#            print('LOOK_FOR1', self.nodestr(focal_point))
#            print('LOOK_FOR2')
#            for nodeid in [10, 12, 14]:  #DEBUG
#                dist = nodesd[nodeid]
#                m = 1 / (dist + 1)**1
#                a = self.activation(nodeid)
#                print(f'  {nodeid} {dist} {m:.3f}  a={a:.3f}  {a*m:.3f}  {nodesw[nodeid]}')
#            for nt, w in zip(nodes_or_tups, self._weighted(nodes_or_tups, nodesw)): #DEBUG
#                print(f'  {nt} {self._asum(nt):.3f} {w}')
            return self.choose_by_dict_weight(nodes_or_tups, nodesw)
        else:
            try:
                return self.choose_by_activation(nodes_or_tups)
            except IndexError:
                return None

    def find_all(
        self,
        criterion: Union[Criterion, Sequence[Criterion]],
        focal_point: MaybeNRef=None,
        subset: Union[Set[NodeId], None]=None,
        tupcond: Callable[[Tuple[NRef], PortLabel], bool]=always_true
    ) -> Union[List[NodeId], List[Tuple[NodeId]]]:
        '''Returns list of all nodes that meet 'criterion'.'''
        #TODO Document Cartesian product
        if focal_point:
            if self.is_of_class(focal_point, 'Group'):
                nodes = self.members_recursive(focal_point)
            else:
                # TODO Somehow need to make it more likely that .look_for()
                # finds nearer nodes.
                nodes = self.walk(focal_point, max_hops=4)
            if subset is not None:
                nodes = subset.intersection(nodes)
        else:
            if subset is not None:
                nodes = subset
            else:
                nodes = self.nodeids()

        if is_iter(criterion):  # If sequence of criteria, return tuples
            # TODO Shouldn't c be found by .as_criterion()?
            return list(
                tup for tup in product(
                    #*(self.find_all(c, focal_point=focal_point, subset=subset)
                    *(self.find_all(c, subset=nodes)
                        for c in criterion
                    )
                ) if len(set(tup)) == len(tup) and tupcond(self, tup)
            )
        else:
            criterion = self.as_criterion(criterion)
            return [n for n in nodes if criterion(self, n)]

    def get_nodes(
        self,
        *nodespecs: Union[Node, Type[Node]],
        within: MaybeNRef=None
    ) -> Sequence[Node]:
        '''Convenience function for extracting nodes whose type and
        attributes are known, but not their nodeids. Raises an exception
        if any nodes are not found. 'within' defaults to self.ws.'''
        if not within:
            within = self.ws
        result = []
        for nodespec in nodespecs:
            node = self.as_node(self.look_for(nodespec, focal_point=within))
            if not node:
                raise NoSuchNode(nodespec)
            result.append(node)
        return result

    # TODO OAOO
    def as_criterion(self, x: Union[Node, CRef, Criterion]) -> Criterion:
        if isinstance(x, Criterion):
            return x
        elif isclass(x) and issubclass(x, Node):
            return OfClass(x)
        elif isinstance(x, Node):
            return NodeEq(x)
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

    def initial_member_of(self, group: NRef) -> NRefs:
        '''Returns None if no initial member.'''
        # TODO Exclude irrelevant types of nodes--that lack both a 'prev' and
        # a 'next' because they're not in a chain.
        for node in self.members_of(group):
            if not self.neighbor(node, 'prev'):
                return node

    def list(self, *nodes, **kwargs) -> List[NodeId]:
        '''For debugging. Called by pg() to get ordered list of nodes to
        print. Filters out nodes that do not exist. (That might not be a good
        idea, if pg() needs to show that the node does not exist.)'''
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
                    if self.has_node(node):
                        add(node)
                elif is_iter(node):
                    iterate(node)
                elif isclass(node):
                    for n in self.nodes_of_class(node):
                        add(n)

        iterate(nodes)
        return result

    # Port labels

    def declare_portlabel_parent(self, parent: PortLabel, *children: PortLabel):
        self.port_mates.declare_parent(parent, *children)

    def expand_port_label(self, port_label: PortLabels) \
    -> Union[Set[PortLabel], None]:
        return self.port_mates.expand_port_label(port_label)

    def is_port_label(self, name: str) -> bool:
        return self.port_mates.is_port_label(name)

    def port_labels_of(self, nref: NRef) -> Set[PortLabel]:
        return set(
            hop.from_port_label for hop in self.hops_from_node(nref)
        ).union(self.defined_port_labels(nref))

    def defined_port_labels(self, nref: NRef) -> Iterable[PortLabel]:
        return self.as_node(nref).defined_port_labels()

    def best_fitting_port_label(self, nref: NRef, given_port_label: PortLabel) \
    -> Union[PortLabel, None]:
        #assert nref, f'best_fitting_port_label: {nref}'
        #assert given_port_label
        if not nref or not given_port_label:
            return None
        existing_port_labels = self.port_labels_of(nref)
        if given_port_label in existing_port_labels:
            return given_port_label
        # TODO HACK There is surely a better criterion for
        # best_fitting_port_label than 'the first one we find that
        # matches'.
        result = first(
            pl for pl in existing_port_labels
                if self.port_mates.isa(pl, given_port_label)
        )
        if result:
            return result
        return given_port_label

    # Doing things

    def do_action(self, action: Union['Action', None], actor: MaybeNRef=None):
        if not action:
            return
        if actor:  # TODO rm
            action.actor = actor  # TODO rm
        if ShowActionsPerformed.is_logging():
            self.print_action(action)
        with PushAttr(self, 'builder'):
            self.builder = action.actor
            self.setattr(actor, 'tola', self.t)
            self.prev_actions.append(action)
            try:
                action.go(self, actor)
                if ShowActionsPerformed:
                    print('succeeded')
            except FizzleWithTag as exc:
                if ShowActionsPerformed or ShowPrimitives:
                    print(exc.action_msg(self, actor))
                try:
                    exc.place_tag(self, actor)
                except Exception as exc2:
                    print(f'\nEXCEPTION in do_action at t={self.t} WHILE RECOVERING FROM {exc}:')
                    try:
                        print(f'ACTOR: {self.nodestr(action.actor)}')
                    except AttributeError:
                        pass
                    print(f'ACTION: {action}')
                    print(f'EXC2: {exc2}')
                    raise
            except Fizzle:
                if ShowActionsPerformed or ShowPrimitives:
                    print('fizzled')
                action.on_fizzle(self, actor)
#            except ActionBlocked as exc:
#                if ShowActionsPerformed or ShowPrimitives:
#                    print('blocked:', action, exc)
#                try:
#                    self.call_method(exc.actor, 'action_blocked', exc)
#                except Exception as exc2:
#                    print(f'\nEXCEPTION in do_action at t={self.t} WHILE RECOVERING FROM {exc}:')
#                    #TODO OAOO
#                    try:
#                        print(f'ACTOR: {self.nodestr(action.actor)}')
#                    except AttributeError:
#                        pass
#                    print(f'ACTION: {action}')
#                    print(f'EXC2: {exc2}')
#                    raise
#            except ActionFailure as exc:
#                if ShowActionsPerformed:
#                    print('failed:', action, exc)
#                try:
#                    self.call_method(exc.actor, 'action_failed', exc)
#                except Exception as exc2:
#                    print(f'\nEXCEPTION in do_action at t={self.t} WHILE RECOVERING FROM FAILURE {exc}:')
#                    #TODO OAOO
#                    try:
#                        print(f'ACTOR: {self.nodestr(action.actor)}')
#                    except AttributeError:
#                        pass
#                    print(f'ACTION: {action}')
#                    print(f'EXC2: {exc2}')
#                    raise
            # TODO catch Fizzle
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

    def can_go(self, nref: NRef):
        return self.call_method(nref, 'can_go')

    def is_sleeping(self, node: NRef) -> bool:
        '''Is node in a Sleeping state?'''
        state = self.getattr(node, 'state')
        if state:
            return state.is_sleeping(self, node)

    def is_failed(self, node):
        return self.has_tag(node, 'Failed')  # TODO not if Failed is canceled

    def is_blocked(self, node):
        return self.has_tag(node, 'Blocked')

    def boost_activation_from_to(
        self,
        fromnodes: NRefs,
        tonodes: NRefs,
        by: float=3.0
    ):
        for fromnode in as_iter(fromnodes):
            for tonode in as_iter(tonodes):
                self.boost_activation(
                    tonode,
                    boost_amount=clip(1.0, 10.0, by * self.activation(fromnode))
                )

    excite = boost_activation_from_to
    # TODO rm boost_activation_from_to; all callers should call excite().
    # Or maybe excite_from().
        
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
        if node:
            return getattr(node, attrname)
        else:
            return None

    def setattr(self, nref: MaybeNRef, attrname: str, value: Any):
        node = self.as_node(nref)
        if node:
            setattr(node, attrname, value)

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
            if state.is_completed(self, node):
                node.on_completion()

    # TODO UT
    def sleep(self, node: NRef, sleep_duration: Union[int, None]=3):
        node = self.as_node(node)
        if sleep_duration is not None:
            until = self.t + sleep_duration
        else:
            until=None
        self.new_state(
            node,
            Sleeping(self.getattr(node, 'state'), until=until)
        )
        #self.calm(node)

    # TODO UT
    def calm(self, nodes: NRefs):
        '''Greatly reduce activation--especially, so that an ActiveNode
        becomes much less likely to be chosen on the next timestep.'''
        for node in self.as_nodes(nodes):
            new_a = node.initial_activation / 20.0
            if ShowPrimitives:
                print(f'calmed  {node.nodestr()}  a={new_a:.3f}')
            self.set_activation(node, new_a)

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
        #TODO Update type hint for tagclass: it can also be a Node object
        # without id.
        tag_sets = (
            set(tag
                for node in as_iter(nodes)
                    for tag in self.neighbors(
                            node, port_label=taggee_port_label
                        )
                        #if self.is_of_class(tag, tagclass)
                        if self.node_isa(tag, tagclass)
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
        actor: Union[NRef, CRef, None]=None
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
                    if is_abstract_cref(actor):
                        actor = self.look_for(actor)
                    #actions_to_do = self.collect_actions([actor])
                    actions_to_do = self.collect_actions(
                        as_set(self.as_nodeids(actor))
                    )
                else:
                    actions_to_do = self.collect_actions_from_graph()

                if ShowActionsPerformed.is_logging():
                    print('ACTIONS PERFORMED')
                    self.print_actions_header(actions_to_do)
                if actions_to_do:
                    for a in actions_to_do:
                        self.do_action(a, a.actor)
                else:
                    if ShowActionsPerformed:
                        print('no actions')

                self.do_activation_autolinks()
                self.do_touches()
                self.wake_done_sleeping()
                self.update_all_asup()

                self.end_of_timestep()

                d = self.done()
                if d:
                    ShowResults(d)
                    #ShowResults(f"t={self.graph['t']}\n")
                    break
        except FargDone as exc:
            self.final_result = exc
            ShowResults(str(exc))

    def wake_done_sleeping(self):
        '''Wake up all sleeping nodes that should wake up on the next
        timestep.'''
        for node in self.allowable_active_nodes():
            self.call_method(node, 'awaken_if_done_sleeping_next_t')

    def update_all_asup(self):
        '''Calls .update_asup() on all allowable_active_nodes.'''
        for node in self.allowable_active_nodes():
            self.call_method(node, 'update_asup')

    def end_of_timestep(self):
        '''Called by do_timestep() at the end of each timestep.'''
        pass

    def link_activation_to_archetypes(self, nrefs: NRefs):
        for slipnode in self.members_recursive(self.slipnet):
            for node in as_iter(nrefs):
                if self.is_of_class(slipnode, node):
                    self.set_mutual_activation(node, slipnode)

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

    def collect_all_actions(self) -> List[Action]:
        '''Returns all Actions from all active ActiveNodes, regardless of
        urgency; no random choice. This is helpful to see what's ready to
        happen in the graph; it's called automatically by .print_actions()
        with no arguments.'''
        return self.collect_actions(self.active_nodes())

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

    def make_activation_links(
        self,
        candidate_nodes: NRefs,
        pairs: Iterable[Tuple[Type[Node], NRefs]]
    ):
        for nodeclass, nrefs in pairs:
            for candidate_node in candidate_nodes:
                if self.node_isa(candidate_node, nodeclass):
                    for nref in as_iter(nrefs):
                        self.set_activation_from_to(candidate_node, nref)
        
    def do_activation_autolinks(self):
        '''Add an activation_to/from edge from each new node that matches
        a tuple in self.activation_autolinks.'''
        self.make_activation_links(self.new_nodes, self.activation_autolinks)

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

    def unexpected_abort(self, actor: NRef, msg: str):
        '''Destroy 'actor' and log a error message 'msg' if ShowPrimitives
        or ShowActionsPerformed.'''
        if ShowPrimitives or ShowActionsPerformed:
            print(f'unexpected abort  {self.nodestr(actor)}: {msg}')
        self.remove_node(actor)

    def print_actions_header(self, actions: Sequence):
        if not actions:
            print('  (none)')
        else:
            headingfmt = '  %5s %5s %7s %5s %7s %-20s %s'
            headings = ('u', 'a', '(a.t)', 's', '(s.t)', 'actor', 'action')
            print(headingfmt % headings)

    def print_actions(
        self,
        actions: Union[List[Action], None]=None,
        header: bool=True
    ):
        with SuppressLogging():
            if actions is None:
                actions = self.collect_all_actions()
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
        if self.is_of_class(node, ActiveNode):
            tolastr = 'tola=%s' % self.tola(node)
        else:
            tolastr = ''
        return '%-25s  a=%.3f s=%.3f   tob=%s %s    %s' % (
            self.nodestr(node),
            self.activation(node),
            self.support_for(node),
            self.tob(node),
            tolastr,
            self.statestr(node)
        )

    def tob(self, nref: NRef) -> int:
        '''Time of birth.'''
        return self.getattr(nref, 'tob')

    def tola(self, nref: NRef) -> int:
        '''Time of last (most recent) action.'''
        return self.getattr(nref, 'tola')

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
            print(f'{prefix}{line}')
        
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

    def add_support(self, from_node, to_node, weight=1.0):
        # TODO This function ought to adjust the support levels of other
        # support_to edges coming from from_node.
        current = self.support_from_to(from_node, to_node)
        self.set_support_from_to(
            from_node,
            to_node,
            current + weight
        )

    def cut_off_support(self, from_node, to_node):
        self.set_support_from_to(from_node, to_node, 0.0)
        self.set_activation_from_to(from_node, to_node, 0.0)

    def oppose(self, from_node, to_node, weight=0.2):
        '''Sets support from from_node to to_node to a negative number.
        If 'weight' is positive, we will set the support to -weight.
        Sets activation from from_node to to_node to 0.0.'''
        self.set_support_from_to(from_node, to_node, -abs(weight))
        self.set_activation_from_to(from_node, to_node, 0.0)

G = ActiveGraph

@dataclass
class Context(ABC):

    @classmethod
    @abstractmethod
    def focal_point(cls, g: G, actor: NRef) -> MaybeNRef:
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
    def focal_point(cls, g: G, actor: NRef) -> MaybeNRef:
        result = g.neighbor(actor, 'member_of')
        if g.is_of_class(result, 'ActionSeqNode'):  # HACK
            return cls.focal_point(g, result)
        else:
            return result

@dataclass
class InWorkspace(Context):

    @classmethod
    def focal_point(cls, g: G, actor: NRef) -> MaybeNRef:
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
        g.print_hops(g.activation_hops_from(node), prefix='      ')

def pai(g: G, *nodes, **kwargs):
    '''Prints activations of nodes but shows incoming links.'''
    pt(g)
    for node in sorted(g.list(*nodes, **kwargs), key=g.activation):
        print(g.long_nodestr(node))
        g.print_hops(g.activation_hops_to(node), prefix='      ')

def paa(g: G, *nodes, **kwargs):
    '''Prints activations of nodes, showing both incoming and outgoing links.'''
    pt(g)
    for node in sorted(g.list(*nodes, **kwargs), key=g.activation):
        print(g.long_nodestr(node))
        g.print_hops(g.activation_hops_from(node), prefix='      ')
        g.print_hops(g.activation_hops_to(node), prefix='      ')

def ps(g: G, *nodes, **kwargs):
    pt(g)
    for node in sorted(g.list(*nodes, **kwargs), key=g.support_for):
#        print('supp=%.3f  %s' % (
#            g.support_for(node),
#            g.nodestr(node)
#        ))
        print(g.long_nodestr(node))
        g.print_hops(g.support_hops_from(node), prefix='      ')
