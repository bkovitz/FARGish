# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Set, FrozenSet, Iterable, Any, NewType, Type, \
    ClassVar
from dataclasses import dataclass, field
from inspect import isclass
from copy import copy

from Node import Node, NodeId, MaybeNodeId, PortLabel, PortLabels, is_nodeid, \
    NRef, NRefs, CRef, CRefs, MaybeNRef, \
    as_nodeid, as_node, as_nodeids, as_nodes
from PortMates import PortMates
from util import as_iter, as_list, repr_str, first, intersection


@dataclass(frozen=True)
class Hop:
    from_node: NodeId
    from_port_label: PortLabel
    to_node: NodeId
    to_port_label: PortLabel
    key: int

    def reverse(self):
        return Hop(
            self.to_node,
            self.to_port_label,
            self.from_node,
            self.from_port_label,
            self.key
        )

Hops = Union[Hop, Iterable[Hop], None]

class PortGraphPrimitives(ABC):
    nextid: NodeId = 1  # We number nodes starting from 1

    def _bump_nextid(self) -> NodeId:
        result = self.nextid
        self.nextid += 1
        return result

    @abstractmethod
    def datum(self, nodeid: NRef) -> Union[Node, None]:
        pass

    @abstractmethod
    def _add_node(self, datum: Node) -> NodeId:
        '''Should assign the node a unique nodeid, set datum.id to that id,
        set datum.g to the graph, and return the nodeid.'''
        pass

    @abstractmethod
    def _add_edge(
        self,
        node1: NodeId,
        port_label1: PortLabel,
        node2: NodeId,
        port_label2: PortLabel,
        **attr
    ) -> int:
        pass

    @abstractmethod
    def _remove_node(self, nodeid: NodeId):
        pass

    @abstractmethod
    def _remove_edge(
        self,
        node1: NodeId,
        port_label1: PortLabel,
        node2: NodeId,
        port_label2: PortLabel
    ):
        pass

    @abstractmethod
    def _neighbors(self, nodeid: NodeId) -> Iterable[NodeId]:
        pass

    @abstractmethod
    def remove_hop(self, hop: Hops):
        pass

    def find_hop(
        self,
        from_node: NodeId,
        from_port_label: PortLabel,
        to_node: NodeId,
        to_port_label: PortLabel
    ) -> Union[Hop, None]:
        '''Returns the Hop if it exists, else None.'''
        return first(hop for hop in self._hops_to_neighbor(from_node, to_node)
                            if hop.from_port_label == from_port_label
                                and
                                hop.to_port_label == to_port_label)

    @abstractmethod
    def _hops_from_node(self, nodeid: MaybeNodeId) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def _hops_from_port(
        self, nodeid: MaybeNodeId, port_label: PortLabel
    ) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def _hops_to_neighbor(
        self, nodeid: NodeId, neighborid: NodeId
    ) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def _port_labels(self, nodeid: MaybeNodeId) -> PortLabels:
        '''Should return only port labels on nodeid that actually have
        Hops connected to them.'''
        pass

    @abstractmethod
    def _nodeids(self) -> Iterable[NodeId]:
        '''Should return all NodeIds.'''
        pass

    @abstractmethod
    def _nodes(self) -> Iterable[Node]:
        '''Should return all Nodes (datum, not nodeid).'''
        pass

    @abstractmethod
    def num_nodes(self) -> int:
        pass

    @abstractmethod
    def num_edges(self) -> int:
        pass

    def __len__(self):
        return self.num_nodes()

class ActiveGraphPrimitives(PortGraphPrimitives):
    @abstractmethod
    def add_node(
        self,
        node: Union[Type[Node], Node],
        *args,
        **kwargs
    ) -> Node:
        '''If node is a Node object, creates the node and fills in its .id.
        If node is a Node class, creates the Node object from args and
        kwargs. Either way, returns the Node object.'''
        pass

    @abstractmethod
    def add_edge(
        self,
        nodes1: NRefs,
        port_label1: PortLabels,
        nodes2: NRefs,
        port_label2: PortLabels,
        **attr
    ):
        pass

    @abstractmethod
    def has_edge(self, u, v, w=None, y=None) -> bool:
        pass

    @abstractmethod
    def remove_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
        port_label2: PortLabels,
    ):
        pass

    @abstractmethod
    def neighbors(
        self,
        node: NRefs,
        port_label: PortLabels,
        neighbor_class: Union[Type[Node], NodeId, None],
        neighbor_label: PortLabels
    ) -> Set[NodeId]:
        pass

    def nodes(self) -> Iterable[Node]:
        return self._nodes()

    def nodeids(self) -> Set[NodeId]:
        return set(self._nodeids())

    def hops_from_node(self, node: NRef):
        return self._hops_from_node(as_nodeid(node))

    def hops_from_port(self, node: NRef, port_label: PortLabel):
        return self._hops_from_port(as_nodeid(node), port_label)

    def hops_to_neighbor(self, node: NRef, neighbor: NRef):
        return self._hops_to_neighbor(as_nodeid(node), as_nodeid(neighbor))

class Building(ActiveGraphPrimitives):
    pass

class Activation(ActiveGraphPrimitives):
    pass

class Support(ActiveGraphPrimitives):
    pass

class Touches:
    pass

class Members(ActiveGraphPrimitives):
    pass

class ActiveGraph(
    Building, Activation, Support, Touches, Members, ActiveGraphPrimitives 
):
    std_port_mates = PortMates([
        ('members', 'member_of'), ('tags', 'taggees')
    ])

    def __init__(self):
        self.port_mates = copy(self.std_port_mates)

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

        self.add_implicit_membership(node)
        # add_edge_to_default_container
        # built_by
        # on_build
        # logging
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
            #TODO
            raise NotImplementedError
        
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

    # Interrogating nodes

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

    def is_member(self, node: NRefs, group_node: NRefs):
        return self.has_hop(group_node, 'members', node, 'member_of')

    def value_of(self, nref: NRef, attr_name: str='value') -> Any:
        try:
            return getattr(self.as_node(nref), attr_name)
        except AttributeError:
            return None

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

    # Port labels

    def is_port_label(self, name: str) -> bool:
        return self.port_mates.is_port_label(name)

    # Doing things

    def do_action(self, action: 'Action'):
        raise NotImplementedError

    def put_in_container(self, node: NRefs, container: NRefs):
        #TODO Don't allow a node to contain itself. Or should that get caught
        # by some "proofreading" process in the model?
        self.add_edge(node, 'member_of', container, 'members')

    # Printing

    def dict_str(self, nref: NRef) -> Union[str, None]:
        try:
            return self.as_node(nref).dict_str()
        except AttributeError:
            return None

G = ActiveGraph
