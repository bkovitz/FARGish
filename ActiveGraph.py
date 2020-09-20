# ActiveGraph.py -- The Graph class needed for defining FARG models, and
#                   supporting classes

from abc import ABC, abstractmethod
from typing import Union, List, Set, FrozenSet, Iterable, Any, NewType, Type
from dataclasses import dataclass


NodeId = NewType('NodeId', int)
MaybeNodeId = Union[NodeId, None]

PortLabel = NewType('PortLabel', str)
PortLabels = Union[PortLabel, None, Iterable[PortLabel]]


@dataclass
class Node:
    id: NodeId

    is_tag = False


NRef = Union[NodeId, Node, None]   # A Node reference
NRefs = Union[NRef, Iterable[NRef]]

@dataclass
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
    def _add_node(self, nodeid: NodeId, datum: Node):
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
    def _neighbors(self, nodeid: NodeId) -> List[NodeId]:
        pass

    @abstractmethod
    def remove_hop(self, hop: Hops):
        pass

    @abstractmethod
    def hops_from_node(self, nodeid: MaybeNodeId) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def hops_from_port(
        self, nodeid: MaybeNodeId, port_label: PortLabel
    ) -> FrozenSet[Hop]:
        pass

    @abstractmethod
    def port_labels(self, nodeid: MaybeNodeId) -> PortLabels:
        pass

    @property
    @abstractmethod
    def nodes(self) -> Iterable[Node]:
        pass

class ActiveGraphPrimitives(PortGraphPrimitives):
    @abstractmethod
    def add_edge(
        self,
        node1: NRefs,
        port_label1: PortLabels,
        node2: NRefs,
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

    def do_action(self, action: 'Action'):
        pass

if __name__ == '__main__':
    g = ActiveGraph()
    
