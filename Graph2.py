# Graph2.py

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from dataclasses import dataclass, field, InitVar
import math
from itertools import chain
from collections import defaultdict
from inspect import isclass

from Propagator import Propagator, Delta
from FMTypes import epsilon
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
    @abstractmethod
    #TODO type for q
    def query(self, q) -> Iterable[Node]:
        pass

@dataclass(frozen=True)
class OfClass:
    cl: Type

@dataclass
class EnumNodes(Nodes):
    nodeset: Set[Node]  # TODO Container?
    nodeclasses: Dict[Type, Set[Node]] = field(
        init=False, default_factory=lambda: defaultdict(set)
    )

    def __post_init__(self):
        for node in self.nodeset:
            self.nodeclasses[node.__class__].add(node)

    def has_node(self, x):
        return x in self.nodeset

    def query(self, q):
        if isinstance(q, OfClass):
            try:
                yield from self.nodeclasses[q.cl]
            except KeyError:
                return
        elif q is None:
            yield from self.nodeset
        else:
            #TODO allow multiple nodes in q
            if q in self.nodeset:
                yield q

    def __iter__(self):
        return iter(self.nodeset)

    def __len__(self):
        return len(self.nodeset)

@dataclass
class NodesSeries(Nodes):
    nodess: Sequence[Nodes]

    def has_node(self, x):
        return any(nodes.has_node(x) for nodes in self.nodess)

    def query(self, q):
        for nodes in self.nodess:
            yield from nodes.query(q)

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

    def __iter__(self):
        for d in self.hops_from.values():
            for hop in d.values():
                yield hop

@dataclass
class MutualInhibition(Edges):
    #TODO docstring
    superclass: Union[Type, Tuple[Type]]
    weight: float = -0.2

    def hops_from_node(self, nodes, from_node):
        if (
            nodes.has_node(from_node)
            and
            isinstance(from_node, self.superclass)
        ):
            for to_node in nodes.query(OfClass(from_node.__class__)):
                if to_node != from_node:
                    yield Hop(from_node, to_node, self.weight)

    def hops_to_node(self, nodes, to_node):
        if (
            nodes.has_node(to_node)
            and
            isinstance(to_node, self.superclass)
        ):
            for from_node in nodes.query(OfClass(to_node.__class__)):
                if from_node != to_node:
                    yield Hop(from_node, to_node, self.weight)
        
    def find_hop(self, nodes, from_node, to_node):
        if (
            nodes.has_node(from_node)
            and
            nodes.has_node(to_node)
            and
            isinstance(from_node, self.superclass)
            and
            from_node.__class__ == to_node.__class__
            and
            from_node != to_node
        ):
            return Hop(from_node, to_node, self.weight)

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

    #TODO type for q
    def query(self, q) -> Iterable[Node]:
        return self.nodes.query(q)

    def hops_from_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_from_node(self, x)

    def hops_to_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_to_node(self, x)

    # TODO UT
    def degree_out(self, x: Any) -> int:
        return len(list(self.hops_from_node(x)))

    # TODO UT
    def degree_in(self, x: Any) -> int:
        return len(list(self.hops_to_node(x)))

    def find_hop(self, from_node: Any, to_node: Any) -> Union[Hop, None]:
        return self.edges.find_hop(self.nodes, from_node, to_node)

    def successors_of(self, x: Any) -> Iterable[Node]:
        return (hop.to_node for hop in self.edges.hops_from_node(self.nodes, x))

    def predecessors_of(self, x: Any) -> Iterable[Node]:
        return (hop.from_node for hop in self.edges.hops_to_node(self.nodes, x))

    def hop_weight(self, from_node: Node, to_node: Node) -> float:
        '''If either node does not exist, or there is no hop from from_node to
        to_node, returns 0.0.'''
        try:
            return self.find_hop(from_node, to_node).weight
        except AttributeError:
            return 0.0

    @classmethod
    def augment(cls, *graphs: 'Graph') -> 'Graph':
        return Graph(
            nodes=NodesSeries([g.nodes for g in graphs]),
            edges=EdgesSeries([g.edges for g in graphs])
        )

    @classmethod
    def OLDwith_features(cls, *base_nodess: Iterable[Node]) -> 'Graph':
        '''Makes and returns a Graph containing all nodes in base_nodess,
        as well as a node for each feature returned by features_of, and
        a positive edge connecting each base node to its feature nodes.'''
        nodeset = set()
        hopset = set()
        for base_node in chain.from_iterable(base_nodess):
            nodeset.add(base_node)
            for feature_node in features_of(base_node):
                nodeset.add(feature_node)
                hopset.add(Hop(base_node, feature_node, 1.0))
                hopset.add(Hop(feature_node, base_node, 1.0))
                # TODO Special nodes for feature classes?
                
        return Graph(
            nodes=EnumNodes(nodeset),
            edges=EnumEdges(hopset)
        )

    @classmethod
    def with_features(cls, *base_nodess: Iterable[Node]) -> 'Graph':
        nodeset = set()
        hopset = set()

        nodes_to_do = set(chain.from_iterable(base_nodess))
        nodes_done = set()
        
        while nodes_to_do:
            base_node = nodes_to_do.pop()
            nodes_done.add(base_node)
            new_features = (
                set(add_features(base_node, nodeset, hopset))
                -
                nodes_done
            )
            nodes_to_do |= new_features

        return Graph(
            nodes=EnumNodes(nodeset),
            edges=EnumEdges(hopset)
        )

    def add_edges(self, edges: Edges) -> 'Graph':
        return Graph(nodes=self.nodes, edges=EdgesSeries([edges, self.edges]))

def add_features(base_node: Node, nodeset: Set[Node], hopset: Set[Hop]):
    '''Update nodeset and hopset.'''
    new_features = set()
    nodeset.add(base_node)
    for feature_node in features_of(base_node):
        new_features.add(feature_node)
        nodeset.add(feature_node)
        hopset.add(Hop(base_node, feature_node, 1.0))
        hopset.add(Hop(feature_node, base_node, 1.0))
    return new_features

@dataclass(frozen=True)
class Feature:
    pass

def features_of(x: Any) -> Iterable[Node]:
    if not isclass(x):
        if hasattr(x, 'features_of'):
            yield from x.features_of()
        yield type(x)

@dataclass(frozen=True)
class FeatureWrapper(Feature):
    feature: Union[Hashable, None] = None

### Propagator classes that work with Graph

@dataclass
class GraphPropagatorIncoming(Propagator):
    '''A Propagator that follows hops that lead to nodes whose keys are in the
    activations dictionary. Each timestep, g.hops_to_node() supplies all the
    edges. Because of this, only nodes explicitly included in old_d can
    *ever* receive any activation when .propagate() is called.'''

    def make_deltas(self, g: Graph, old_d):
        return chain.from_iterable(
            self.deltas_to(g, old_d, node) for node in old_d
        )

    def deltas_to(self, g, old_d, node):
        for hop in g.hops_to_node(node):
            if abs(hop.weight) >= epsilon:
                neighbor_a = old_d.get(hop.from_node, 0.0)
                if abs(neighbor_a) >= epsilon:
                    yield Delta(node, hop.weight * neighbor_a, hop.from_node)

@dataclass
class GraphPropagatorOutgoing(Propagator):
    '''A Propagator that follows hops that lead to nodes whose keys are in the
    activations dictionary. Each timestep, g.hops_to_node() supplies all the
    edges. Because of this, only nodes explicitly included in old_d can
    *ever* receive any activation when .propagate() is called.'''

    def make_deltas(self, g: Graph, old_d):
        return chain.from_iterable(
            self.deltas_from(g, old_d, node) for node in old_d
        )

    def deltas_from(self, g, old_d, node):
        node_a = old_d.get(node, 0.0)
        if abs(node_a) >= epsilon:
            for hop in g.hops_from_node(node):
                neighbor_a = old_d.get(hop.to_node, 0.0)
                if abs(hop.weight) >= epsilon:
                    yield Delta(hop.to_node, hop.weight * node_a, node)
