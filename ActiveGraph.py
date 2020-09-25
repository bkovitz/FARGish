# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from dataclasses import dataclass, field
from inspect import isclass
from copy import copy
from operator import attrgetter, itemgetter

from Primitives import Hop, Hops, PortGraphPrimitives, ActiveGraphPrimitives, \
    ActivationPrimitives, ActivationPolicy
from Node import Node, NodeId, MaybeNodeId, PortLabel, PortLabels, is_nodeid, \
    NRef, NRefs, CRef, CRefs, MaybeNRef, \
    as_nodeid, as_node, as_nodeids, as_nodes
from PortMates import PortMates
from Action import Action, Actions
from ActiveNode import ActiveNode
from WithActivation import WithActivation, Propagator as ActivationPropagator
from util import as_iter, as_list, repr_str, first, intersection, reseed, \
    empty_set, sample_without_replacement, PushAttr
from exc import NodeLacksMethod, NoSuchNodeclass
from log import *


class Support(ActiveGraphPrimitives):

    def support_for(self, node: NRef) -> float:
        #TODO
        return 1.0

class Touches:
    pass

class Members(ActiveGraphPrimitives):
    pass

class ActiveGraph(
    Support, Touches, Members, ActivationPolicy, ActivationPrimitives,
    ActiveGraphPrimitives 
):
    std_port_mates = PortMates([
        ('members', 'member_of'), ('tags', 'taggees'), ('built_by', 'built'),
        ('next', 'prev')
    ])

    def __init__(
        self,
        seed: Union[int, None]=None,
        t: Union[int, None]=None,
        num_timesteps: Union[int, None]=None,
        *args,
        **kwargs
    ):
        super().__init__(*args, **kwargs)
        self.seed = reseed(seed)
        self.num_timesteps = num_timesteps

        self.port_mates: PortMates = copy(self.std_port_mates)
        self.nodeclasses: Dict[str, Type[Node]] = {}

        self.max_active_nodes: Union[int, None] = None
        self.max_actions: int = 1

        self.builder: MaybeNRef = None  # Node that is currently "building"
                                        # other nodes
        self.new_nodes: Set[NodeId] = set()        # Nodes built this timestep
        self.prev_new_nodes: Set[NodeId] = set()   # Nodes built last timestep

        if t is None:
            t = 0
        self.t = t

        self.ws: MaybeNRef = None
        self.slipnet: MaybeNRef = None

        #TODO Allow this as a class variable
        self.activation_propagator = ActivationPropagator(
            max_total_activation=20,
            sigmoid_p=0.5,
            alpha=0.98,
            # TODO noise=0.0
        )

    # Overrides for ActiveGraphPrimitives

    def add_node(
        self,
        node: Union[Type[Node], Node, str],
        *args,
        **kwargs
    ) -> Node:
        if isinstance(node, Node):
            already = self.already_built(node, *args, **kwargs)
            if already:
                return already
            self._add_node(node)
            # TODO Apply link FilledParams?
        else:
            if isinstance(node, str):
                # TODO If node is a str, get_nodeclass
                raise NotImplementedError
            already = self.already_built(node, *args, **kwargs)
            if already:
                return already
            assert issubclass(node, Node), f'{node} is not a subclass of Node'
            filled_params = node.make_filled_params(self, *args, **kwargs)
            node: Node = node()  # Create the Node object
            self._add_node(node)
            filled_params.apply_to_node(self, node.id)

        classname = node.__class__.__name__
        if not self.ws and classname == 'Workspace':
            self.ws = node
        if not self.slipnet and classname == 'Slipnet':
            self.slipnet = slipnet
        if classname not in self.nodeclasses:
            self.nodeclasses[classname] = node.__class__

        self.set_activation(node, node.initial_activation)
        self.add_implicit_membership(node)
        self.mark_builder(node, self.builder)
        node.on_build()
        # logging
        self.new_nodes.add(self.as_nodeid(node))
        # touches
        return node

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
                        self._add_edge(fromid, fromlabel, toid, tolabel)

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
        '''It is not an error to remove an edge that does not exist. If the
        edge did exist, we remove it and "touch" both nodes.'''
        raise NotImplementedError

    # TODO UT
    def neighbors(
        self,
        nodes: NRefs,
        port_label: PortLabels = None,
        neighbor_class: Union[Type[Node], NRef] = None,
        neighbor_label: PortLabels = None
    ) -> Set[NodeId]:
        result = set()
        if port_label is None:
            for node in as_iter(nodes):
                result.update(self._neighbors(as_nodeid(node)))
        else:
            for node in as_iter(nodes):
                for pl in as_iter(port_label):
                    result.update(
                        hop.to_node
                            for hop in self.hops_from_port(node, pl)
                    )

        # Now pare down the neighbors to just those specified
        if neighbor_class:
            for nodeid in result:
                if self.is_of_class(nodeid, neighbor_class):
                    result.discard(nodeid)
        if neighbor_label:
            #TODO
            raise NotImplementedError

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
        
    # Node-building

    def already_built(
        self,
        nodeclass: Union[Type[Node], Node],
        *args,     # ignored if nodeclass is a Node
        **kwargs   # ignored if nodeclass is a Node
    ) -> Union[Node, None]:
        '''Returns None if the specified node is not already built, or the
        Node if it is. A nodeclass with .is_duplicable == True is never
        deemed already built. If nodeclass is a Node object, we look for
        existing nodes with the same class and parameters, getting the
        Node object's parameters by calling its .regen_kwargs() method.'''
        if isinstance(nodeclass, Node):
            args = ()
            kwargs = nodeclass.regen_kwargs()
            nodeclass = nodeclass.__class__
        if nodeclass.is_duplicable:
            return None

        filled_params = nodeclass.make_filled_params(self, *args, **kwargs)
        if filled_params.specifies_attrs():
            candidates = self.nodes()
        else:
            candidates = self.neighbors(filled_params.potential_neighbors())
        return self.as_node(first(
            candidate
                for candidate in candidates
                    if filled_params.is_match(self, nodeclass, candidate)
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
        containers = intersection(*(
            self.neighbors(n1, 'member_of') for n1 in self.neighbors(node)
        ))
        containers.discard(as_nodeid(node))
        for c in containers:
            self.put_in_container(node, c)

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
            
    def is_of_class(self, nrefs: NRefs, nodeclasses: CRefs) -> bool:
        '''Returns True iff all nodes referenced are instances of all the
        nodeclasses referenced. Returns False if there are no node references
        or no nodeclasses.'''
        nodes = as_list(self.as_nodes(nrefs))
        if not nodes:
            return False
        nodeclasses = self.as_nodeclasses(nodeclasses)
        if not nodeclasses:
            return False
        return all(
            isinstance(node, cl)
                for node in nodes
                    for cl in nodeclasses
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
                if tagclass == 'Failed':
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
            return all(
                tagclass in self.neighbors(n, port_label=taggee_port_label)
                    for n in as_iter(node)
            )

    def builder_of(self, node: MaybeNRef):
        return self.neighbor(node, port_label='built_by')

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

    def get_overrides(self, node: NRef, names: Set[str]) -> Dict[str, Any]:
        result = {}
        for name in names:
            n = self.neighbor(node, name)
            if n:
                result[name] = n
        return result

    # Querying the graph

    # TODO Better: Pass OfClass to .nodes()
    def nodes_of_class(self, cl: Type[Node], nodes: NRefs=None) -> List[NRef]:
        result = []
        if nodes is None:
            nodes = self.nodes()
        for node in nodes:
            if isinstance(self.as_node(node), cl):
                result.append(node)
        return result

    def members_of(self, container_node: NRefs) -> List[NodeId]:
        return list(self.neighbors(container_node, 'members'))

    #TODO UT
    def members_recursive(self, container_node: MaybeNRef) -> Set[NodeId]:
        result = set()
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

    # Port labels

    def is_port_label(self, name: str) -> bool:
        return self.port_mates.is_port_label(name)

    # Doing things

    def do_action(self, action: 'Action', actor: MaybeNRef=None):
        if actor:
            action.actor = actor
        with PushAttr(self, 'builder'):
            self.builder = actor
            action.go(self)

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
            return m(self, *args, **kwargs)
        else:
            raise NodeLacksMethod(nref, method_name, args, kwargs)

    def new_state(self, node: NRef, state: 'ActiveNodeState'):
        node = self.as_node(node)
        if node:
            node.state = state
            if state.is_completed:
                node.on_completion()

    # Timestepping (TODO: make this section into a mix-in)

    def do_timestep(
        self,
        num=1,
        action: Union[Action, List[Action], None]=None,
        actor: MaybeNRef=None
    ) -> None:
        for i in range(num):
            self.t += 1
            self.prev_new_nodes = set(self.new_nodes)
            self.new_nodes.clear()

            if any(
                l for l in (ShowActiveNodes, ShowActionList, ShowActionsChosen)
            ):
                print(f'{chr(10)}t={self.t}')

            #self.clear_touched_and_new()

            #self.propagate_support()
            #support.log_support(self)

            self.propagate_activation()
            #log_activation(self)

            #self.update_coarse_views()

            if action is not None:
                actions_to_do = as_iter(action)
            elif actor is not None:
                actions_to_do = self.collect_actions([actor])
            else:
                actions_to_do = self.collect_actions_from_graph()

            if ShowActionsPerformed.is_logging():
                print('ACTIONS PERFORMED')
                if not actions_to_do:
                    print('  (none)')
            for a in actions_to_do:
                if ShowActionsPerformed.is_logging():
                    print(f'  {a.actor}: {a}')
                self.do_action(a)

            #self.do_touches()
            #self.update_all_support()

            d = self.done()
            if d:
                ShowResults(d)
                ShowResults(f"t={self.graph['t']}\n")
                break

    def do_actions1(self, actions: Actions):
        '''Force a sequence of actions, one per timestep. If a single Action
        is an iterable, then all the Actions it contains will be performed
        on its timestep.'''
        for a in as_iter(actions):
            self.do_timestep(action=a)

    def collect_actions_from_graph(self):
        active_nodes = self.collect_active_nodes()
        if ShowActiveNodes.is_logging():
            print('ACTIVE NODES')
            raise NotImplementedError
            #pg(self, active_nodes)

        actions = self.collect_actions(active_nodes)
        if ShowActionList.is_logging():
            print('ACTIONS COLLECTED')
            self.print_actions(actions)

        actions = [a for a in actions if self.urgency(a) > 0.0]

        # TODO Boost activation or lower standards if too many timesteps
        # have gone by with no action.

        chosen_actions = self.choose_actions(actions)
        if ShowActionsChosen.is_logging():
            print('ACTIONS CHOSEN')
            self.print_actions(chosen_actions)
        return chosen_actions

    def collect_active_nodes(self) -> List[NRef]:
        return self.choose_active_nodes(list(
            node for node in self.nodes_of_class(
                    ActiveNode,
                    nodes=self.allowable_active_nodes()
                )
                    if not self.is_dormant(node)
        ))

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

    def collect_actions(self, active_nodes: NRefs) -> List[Action]:
        actions = []
        for node in as_iter(active_nodes):
            got = self.datum(node).actions(self)
            for action in as_iter(got):
                if action:
                    action.actor = node
                    actions.append(action)
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
        
    def allowable_active_nodes(self):
        '''Returns all nodes in the 'ws' (the workspace), if a 'ws' has been
        set. Otherwise returns all nodes.'''
        if not self.ws:
            return self.nodes()
        result = self.members_recursive(self.ws)
        if self.slipnet:
            result.add(slipnet)
        return result

    def urgency(self, action: Action) -> float:
        return 1.0
        #TODO
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

        
    def done(self) -> Any:
        # TODO Make this return a meaningful result. Maybe self.final_result?
        return False

    def action_sorting_key(self, action: Action) -> Tuple:
        return (
            self.urgency(action),
            self.activation(action.actor),
            self.support_for(action.actor)
        )

    def print_actions(self, actions: List[Action]):
        if not len(actions):
            print('  (none)')
            return
        headingfmt = '  %5s %5s %7s %5s %7s %4s %s'
        fmt =        '  %.3f %.3f (%.3f) %.3f (%.3f) %4d %s'
        headings = ('u', 'a', '(a-t)', 's', '(s-t)', 'node', 'action')
        print(headingfmt % headings)
        for action in sorted(actions, key=self.action_sorting_key):
            print(fmt % (self.urgency(action),
                         self.activation(action.actor),
                         action.threshold,
                         self.support_for(action.actor),
                         action.support_threshold,
                         self.as_nodeid(action.actor),
                         action))

    # Printing

    def nodestr(self, node: MaybeNRef) -> str:
        node = self.as_node(node)
        if node is None:
            return 'None'
        else:
            return f'{node.id:4d}: {repr(node)}'

    def long_nodestr(self, node):
        return '%s  a=%.3f supp=%.3f' % (
            self.nodestr(node),
            self.activation(node),
            self.support_for(node),
        )

    def dict_str(self, nref: NRef) -> Union[str, None]:
        try:
            return self.as_node(nref).dict_str()
        except AttributeError:
            return None

    def print_edges(self, node, prefix=''):
        for hop in sorted(
            self.hops_from_node(node), key=attrgetter('from_port_label')
        ):
            print('%s%-20s --> %s %s (%.3f)' % (
                prefix,
                hop.from_port_label,
                self.nodestr(hop.to_node),
                hop.to_port_label,
                self._hop_weight(hop)
            ))

G = ActiveGraph
