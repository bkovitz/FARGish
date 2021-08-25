# Graph2.py

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from dataclasses import dataclass, field, InitVar
import math
from itertools import chain
from collections import defaultdict

from util import as_iter, empty_set, first_non_none, unique_everseen


Node = Hashable

@dataclass(frozen=True)
class Hop:
    '''A directed edge.'''
    from_node: Node
    to_node: Node
    weight: float

class Hops:
    '''Just a scope for factory methods that make iterables to pass to
    EnumEdges.'''

    @classmethod
    def from_dict(cls, weight=1.0, **kwargs) -> Iterable[Hop]:
        for from_node, to_nodes in kwargs.items():
            for to_node in as_iter(to_nodes):
                yield Hop(from_node, to_node, weight)

    @classmethod
    def from_pairs(cls, *pairs: Tuple[Node, Node], weight=1.0) -> Iterable[Hop]:
        for from_node, to_node in pairs:
            yield Hop(from_node, to_node, weight)
    
class Nodes(ABC):
    @abstractmethod
    def has_node(self, x) -> bool:
        pass

    #TODO
    # all_nodes()
    # nodes(query)

@dataclass
class EnumNodes(Nodes):
    nodeset: Set[Node]  # TODO Container?

    def has_node(self, x):
        return x in self.nodeset

@dataclass
class NodesSeries(Nodes):
    nodess: Sequence[Nodes]

    def has_node(self, x):
        return any(nodes.has_node(x) for nodes in self.nodess)

class Edges(ABC):
    @abstractmethod
    def hops_from_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        pass

    @abstractmethod
    def hops_to_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        pass

    @abstractmethod
    def find_hop(self, nodes: Nodes, from_node: Any, to_node: Any) \
    -> Union[Hop, None]:
        pass

@dataclass
class EnumEdges(Edges):
    hops_from: Dict[Node, Dict[Node, Hop]]
    hops_to: Dict[Node, Dict[Node, Hop]]

    def __init__(self, *hopss: Iterable[Hop]):
        self.hops_from = defaultdict(dict)
        self.hops_to = defaultdict(dict)
        for hops in hopss:
            for hop in as_iter(hops):
                self.hops_from[hop.from_node][hop.to_node] = hop
                self.hops_to[hop.to_node][hop.from_node] = hop

    def hops_from_node(self, nodes, x):
        if not nodes.has_node(x):
            return
        try:
            hd = self.hops_from[x]
        except KeyError:
            return
        for hop in hd.values():
            if nodes.has_node(hop.to_node):
                yield hop
    
    def hops_to_node(self, nodes, x):
        if not nodes.has_node(x):
            return
        try:
            hd = self.hops_to[x]
        except KeyError:
            return
        for hop in hd.values():
            if nodes.has_node(hop.from_node):
                yield hop

    def find_hop(self, nodes, from_node, to_node):
        if not nodes.has_node(from_node) or not nodes.has_node(to_node):
            return None
        try:
            return self.hops_from[from_node][to_node]
        except KeyError:
            return None

@dataclass
class EdgesSeries(Edges):
    edgess: Sequence[Edges]

    def hops_from_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        return unique_everseen(chain.from_iterable(
            edges.hops_from_node(nodes, x) for edges in self.edgess
        ))

    def hops_to_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        return unique_everseen(chain.from_iterable(
            edges.hops_to_node(nodes, x) for edges in self.edgess
        ))

    def find_hop(self, nodes: Nodes, from_node: Any, to_node: Any) \
    -> Union[Hop, None]:
        for edges in self.edgess:
            hop = edges.find_hop(nodes, from_node, to_node)
            if hop is not None:
                return hop

@dataclass
class Graph:
    nodes: Nodes
    edges: Edges

    def has_node(self, x: Any) -> bool:
        return self.nodes.has_node(x)

    def hops_from_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_from_node(self, x)

    def hops_to_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_to_node(self, x)

    def find_hop(self, from_node: Any, to_node: Any) -> Union[Hop, None]:
        return self.edges.find_hop(self.nodes, from_node, to_node)

    def successors_of(self, x: Any) -> Iterable[Node]:
        return (hop.to_node for hop in self.edges.hops_from_node(self.nodes, x))

    def predecessors_of(self, x: Any) -> Iterable[Node]:
        return (hop.from_node for hop in self.edges.hops_to_node(self.nodes, x))

    #TODO hop_weight()

    @classmethod
    def augment(cls, *graphs: 'Graph') -> 'Graph':
        return Graph(
            nodes=NodesSeries([g.nodes for g in graphs]),
            edges=EdgesSeries([g.edges for g in graphs])
        )
