# Primitives.py -- Abstract classes for defining ActiveGraph classes and mix-ins

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from dataclasses import dataclass

from Node import Node, NodeId, MaybeNodeId, PortLabel, PortLabels, is_nodeid, \
    NRef, NRefs, CRef, CRefs, MaybeNRef, \
    as_nodeid, as_node, as_nodeids, as_nodes
from util import first, as_iter


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

    def hopstr(self, g):
        # TODO Catch nonexistent node
        f = g.as_node(self.from_node).nodestr().strip()
        fp = self.from_port_label
        t = g.as_node(self.to_node).nodestr().strip()
        tp = self.to_port_label
        return f'{f} {fp}  -->  {tp} {t}'

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
    def _edge_weight(
        node1: NodeId,
        port_label1: PortLabel,
        node2: NodeId,
        port_label2: PortLabel,
    ) -> float:
        '''A non-existent edge has weight 0.0.'''
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

    #TODO _hop_weight

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

    def has_node(self, nrefs: NRefs) -> bool:
        return all(self.datum(self.as_nodeid(nref)) for nref in as_iter(nrefs))

    @abstractmethod
    def remove_node(self, node: NRefs):
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

    def remove_hops_from_port(self, node: NRef, port_label: PortLabel):
        self.remove_hop(self.hops_from_port(node, port_label))

class ActivationPrimitives(ABC):

    @abstractmethod
    def activation(self, node: NRef) -> float:
        '''A non-existent node must have activation 0.0.'''
        pass

    @abstractmethod
    def min_activation(self, node: NRef) -> float:
        pass

    @abstractmethod
    def set_activation(self, node: NRef, a: float):
        pass

    @abstractmethod
    def set_activation_from_to(
        self, from_node: NRef, to_node: NRef, weight: float
    ):
        pass

    @abstractmethod
    def activation_from_to(self, from_node: NRef, to_node: NRef):
        pass

    @abstractmethod
    def incoming_activation_neighbors(self, node: NRef) -> Iterable[NodeId]:
        pass

    @abstractmethod
    def remove_outgoing_activation_edges(self, node: NRef):
        pass

    @abstractmethod
    def remove_incoming_activation_edges(self, node: NRef):
        pass

    @abstractmethod
    def activation_dict(
        self, nodes=Union[Iterable[NRef], None]
    ) -> Dict[NodeId, float]:
        pass

class ActivationPolicy(ActivationPrimitives):

    @abstractmethod
    def boost_activation(self, node: NRef, boost_amount: float):
        pass

    @abstractmethod
    def propagate_activation(self):
        pass

    def set_mutual_activation(
        self, node1: NRefs, node2: NRefs, weight: float=1.0
    ):
        for n1 in as_iter(node1):
            for n2 in as_iter(node2):
                self.set_activation_from_to(n1, n2, weight=weight)
                self.set_activation_from_to(n2, n1, weight=weight)

    def deactivate(self, node: NRefs):
        for n in as_iter(node):
            self.set_activation(n, 0.0)

class SlipnetPolicy(ABC):
    @abstractmethod
    def slipnet_search(self, nodes: NRefs, slipnodes: Set[NodeId]) \
    -> Set[NodeId]:
        '''Returns set of activated slipnodes. Leaves dict of slipnet
        activations in self.slipnet_d.'''
        pass
