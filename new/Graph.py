# Graph.py

from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from dataclasses import dataclass, field, InitVar
import math
from itertools import chain
from collections import defaultdict
from inspect import isclass

from FMTypes import epsilon, Activation, ADict
from Log import trace
from Features import Feature, features_of, Before, After
from util import as_iter, as_set, empty_set, first_non_none, unique_everseen, \
    clip, union, pr, pts, short


Node = Hashable

@dataclass(frozen=True)
class Hop:
    '''A directed edge.'''
    from_node: Node
    to_node: Node
    weight: float = 1.0

    def add_prefix(self, prefix: Hashable) -> Hop:
        '''Returns a Hop identical to self but with the nodes replaced by
        PrefixNode of .from_node and .to_node, with the given prefix.'''
        return Hop(
            PrefixedNode(prefix, self.from_node),
            PrefixedNode(prefix, self.to_node),
            self.weight
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.from_node)} -> {short(self.to_node)} {self.weight:1.1f})'

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
    
class Query:
    '''A query specification to pass to Nodes.query().'''

    def unprefixed(self, prefix: Hashable) -> Query:
        return self

class Nodes(ABC):
    @abstractmethod
    def has_node(self, x: Any) -> bool:
        pass

    def add_node(self, node: Node) -> None:
        raise CantAddNode

    def remove_node(self, node: Node) -> None:
        raise CantRemoveNode

    @abstractmethod
    def __iter__(self) -> Iterator[Node]:
        '''Iterates through all the nodes.'''
        # TODO Note that we might raise an exception; some Nodes objects
        # might not support iterating through all nodes.
        pass

    @abstractmethod
    def __len__(self) -> int:
        pass

    #TODO
    # all_nodes()
    # nodes(query)
    @abstractmethod
    #TODO type for q
    def query(self, q) -> Iterable[Node]:
        pass

    def unprefixed(self, prefix: Hashable) -> Nodes:
        '''Returns a version of this Nodes without a prefix. The default
        implementation simply returns an empty EnumNodes. PrefixedNodes
        overrides this method to return the base Nodes object.'''
        return EnumNodes(set())

@dataclass(frozen=True)
class OfClass(Query):
    cl: Type

@dataclass(frozen=True)
class WithPrefix(Query):
    prefix: Hashable
    q: Query

    def unprefixed(self, prefix: Hashable) -> Query:
        if prefix != self.prefix:
            raise WrongPrefix
        else:
            return self.q

class WrongPrefix(Exception):
    pass

class CantAddNode(Exception):
    pass

class CantAddHop(Exception):
    pass

class CantRemoveNode(Exception):
    pass

class CantRemoveHop(Exception):
    pass

@dataclass
class EnumNodes(Nodes):
    Q = TypeVar('Q', bound='EnumNodes')

    nodeset: Set[Node]
    nodeclasses: Dict[Type, Set[Node]]

    def __init__(self, nodeset: Iterable[Node]):
        self.nodeset = as_set(nodeset)
        self.nodeclasses = defaultdict(set)
        for node in self.nodeset:
            self.nodeclasses[node.__class__].add(node)

    def add_node(self, node: Node) -> None:
        self.nodeset.add(node)

    def remove_node(self, node: Node) -> None:
        self.nodeset.discard(node)

    def has_node(self, x) -> bool:
        return x in self.nodeset

    def query(self, q) -> Iterable[Node]:
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

    def __iter__(self) -> Iterator[Node]:
        return iter(self.nodeset)

    def __len__(self) -> int:
        return len(self.nodeset)

    @classmethod
    def empty(cls: Type[Q]) -> Q:
        return cls(set())

@dataclass
class NodesSeries(Nodes):
    nodess: Sequence[Nodes]

    def has_node(self, x) -> bool:
        return any(nodes.has_node(x) for nodes in self.nodess)

    def query(self, q) -> Iterable[Node]:
        for nodes in self.nodess:
            yield from nodes.query(q)

    def unprefixed(self, prefix):
        return NodesSeries([nodes.unprefixed(prefix) for nodes in self.nodess])

    def __iter__(self) -> Iterator[Node]:
        yield from chain.from_iterable(self.nodess)

    def __len__(self) -> int:
        return sum(1 for _ in self)

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

    @abstractmethod
    def __len__(self) -> int:
        pass

    def add_hop(self, hop: Hop) -> None:
        raise CantAddHop

    def remove_hop(self, hop: Hop) -> None:
        raise CantRemoveHop

    def remove_node(self, node: Node) -> None:
        raise CantRemoveNode

@dataclass
class EnumEdges(Edges):
    hops_from: Dict[Node, Dict[Node, Hop]]  # from_node -> to_node -> Hop
    hops_to: Dict[Node, Dict[Node, Hop]]    # to_node -> from_node -> Hop

    def __init__(self, *hopss: Iterable[Hop]):
        self.hops_from = defaultdict(dict)
        self.hops_to = defaultdict(dict)
        for hops in hopss:
            for hop in as_iter(hops):
                self.add_hop(hop)

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

    def add_hop(self, hop: Hop):
        self.hops_from[hop.from_node][hop.to_node] = hop
        self.hops_to[hop.to_node][hop.from_node] = hop

    # TODO UT
    def remove_hop(self, hop: Hop):
        try:
            del self.hops_from[hop.from_node][hop.to_node]
        except KeyError:
            pass
        try:
            del self.hops_to[hop.to_node][hop.from_node]
        except KeyError:
            pass

    def remove_node(self, node: Node):
        '''Removes hops that refer to node even if node does not exist.'''
        to_neighbors: Iterable[Node] = []
        from_neighbors: Iterable[Node] = []
        if node in self.hops_from:
            to_neighbors = self.hops_from[node].keys()
            del self.hops_from[node]
        if node in self.hops_to:
            from_neighbors = self.hops_to[node].keys()
            del self.hops_to[node]
        for to_neighbor in to_neighbors:
            del self.hops_to[to_neighbor][node]
        for from_neighbor in from_neighbors:
            del self.hops_from[from_neighbor][node]

    def __iter__(self) -> Iterator[Hop]:
        for d in self.hops_from.values():
            for hop in d.values():
                yield hop

    def __len__(self) -> int:
        return sum(1 for _ in self)

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

    def __len__(self) -> int:
        raise NotImplementedError

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
        return None

    def __len__(self) -> int:
        raise NotImplementedError

@dataclass
class Graph:
    nodes: Nodes
    edges: Edges

    Q = TypeVar('Q', bound='Graph')

    def has_node(self, x: Any) -> bool:
        return self.nodes.has_node(x)

    def add_node(self, node: Node) -> None:  # TODO return the node
        '''Adds node to this Graph. Not all Nodes objects allow nodes to be
        added. May raise CantAddNode.'''
        self.nodes.add_node(node)

    def remove_node(self, node: Node):
        '''Removes node from this Graph if it exists, and removes all hops to
        which it is incident. It is not an error to remove a non-existent node.
        Not all Nodes and Edges objects allow nodes to be removed. May raise
        CantRemoveNode.'''
        self.nodes.remove_node(node)
        self.edges.remove_node(node)

    #TODO type for q
    def query(self, q) -> Iterable[Node]:
        return self.nodes.query(q)

    def hops_from_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_from_node(self.nodes, x)

    def hops_to_node(self, x: Any) -> Iterable[Hop]:
        return self.edges.hops_to_node(self.nodes, x)

    # TODO UT
    def neighbors(self, node: Node) -> Set[Node]:
        return union(
            [hop.to_node for hop in self.hops_from_node(node)],
            [hop.from_node for hop in self.hops_to_node(node)]
        )

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
        hop = self.find_hop(from_node, to_node)
        if hop:
            return hop.weight
        else:
            return 0.0
        '''
        try:
            return self.find_hop(from_node, to_node).weight
        except AttributeError:
            return 0.0
        '''

    def add_hop(self, hop: Hop):
        '''Adds hop to this Graph. Not all Edges objects allow hops to be
        added. May raise CantAddHop.'''
        self.edges.add_hop(hop)
        
    def add_edges(self, edges: Edges) -> 'Graph':
        # TODO Modify the Graph. You should call .augment to introduce the
        # edges as an EdgesSeries.
        '''Returns a new Graph, containing the edges.'''
        return Graph(nodes=self.nodes, edges=EdgesSeries([edges, self.edges]))

    def num_nodes(self) -> int:
        return len(self.nodes)

    def num_edges(self) -> int:
        return len(self.edges)

    def __len__(self) -> int:
        return self.num_nodes()

    @classmethod
    def empty(cls: Type[Q]) -> Q:
        return cls(nodes=EnumNodes(set()), edges=EnumEdges())

    @classmethod
    def augment(cls, *graphs: 'Graph') -> 'Graph':
        return Graph(
            nodes=NodesSeries([g.nodes for g in graphs]),
            edges=EdgesSeries([g.edges for g in graphs])
        )

    @classmethod
    def with_features(cls, *base_nodess: Iterable[Node]) -> 'Graph':
        '''Makes and returns a Graph containing all nodes in base_nodess,
        as well as a node for each feature returned by features_of, and
        a positive edge connecting each base node to its feature nodes.'''
        nodeset: Set[Node] = set()
        hopset: Set[Hop] = set()

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

@dataclass
class WithActivations(Graph):
    '''Mix-in to track an activation level for each node in a Graph.'''
    activations: ADict = field(
        default_factory=lambda: defaultdict(Activation),
        init=False
    )
    propagator: PropagatorModule.Propagator
    default_a: float = 0.0  # Default activation for a node that is in
                            # the graph but has not been explicitly given
                            # an activation.

    def a(self, node: Node) -> Activation:
        '''Returns the activation of node. A non-existent node has 0.0
        activation.'''
        if self.has_node(node):
            return self.activations.get(node, self.default_a)
        else:
            return 0.0

    def boost(self, node: Node, amt: Optional[Activation]=None):
        '''Boosts the activation of 'node' by 'amt'. If 'node' does not exist,
        has no effect.'''
        if not self.has_node(node):
            return
        if amt is None:
            amt = clip(0.5, 1.0, self.a(node))
        self.activations[node] += amt

    def set_a(self, node: Node, a: Activation):
        '''Sets the activation of 'node' to 'a'. If 'node' does not exist,
        has no effect.'''
        if self.has_node(node):
            self.activations[node] = a

    # TODO Override .remove_node()?

    def propagate(self) -> None:
        self.activations = \
            self.propagator.propagate(self, self.activations_in())
        
    def activations_in(self) -> ADict:
        return dict(
            (node, self.a(node)) for node in self.nodes
        )

    def pr_flows(self) -> None:
        pr(self.propagator.flows)

### Prefixes

@dataclass(frozen=True)
class PrefixedNode:
    prefix: Hashable
    node: Node

    def unprefixed(self, prefix: Any=None) -> Node:
        return unprefixed(self, prefix)

    def with_prefix(self, new_prefix) -> PrefixedNode:
        return PrefixedNode(new_prefix, self.node)

    def __repr__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.prefix}, {self.node})'

def unprefixed(x: Node, prefix: Any=None) -> Node:
    if isinstance(x, PrefixedNode):
        if prefix is not None and x.prefix != prefix:
            return None
        else:
            return x.node
    else:
        return None

@dataclass
class PrefixedNodes(Nodes):
    prefix: Hashable
    base_nodes: Nodes

    def has_node(self, x) -> bool:
        return self.base_nodes.has_node(unprefixed(x, self.prefix))

    def query(self, q):
        try:
            q = q.unprefixed(self.prefix)
        except WrongPrefix:
            return []
        return self.prefix_all(self.base_nodes.query(q))

    def prefix_all(self, nodes: Iterable[Node]) -> Iterable[PrefixedNode]:
        '''Returns a generator in which all of 'nodes' are wrapped in
        a PrefixedNode containing self.prefix.'''
        return (
            PrefixedNode(self.prefix, node)
                for node in nodes
        )

    def __iter__(self) -> Iterator[Node]:
        yield from self.prefix_all(self.base_nodes)

    def unprefixed(self, prefix: Hashable):
        if prefix == self.prefix:
            return self.base_nodes
        else:
            return EnumNodes.empty()

    def __len__(self) -> int:
        return len(self.base_nodes)

@dataclass
class PrefixedEdges(Edges):
    prefix: Hashable
    base_edges: Edges

    def hops_from_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        return (
            hop.add_prefix(self.prefix)
                for hop in self.base_edges.hops_from_node(
                    nodes.unprefixed(self.prefix),
                    unprefixed(x, self.prefix)
                )
        )

    def hops_to_node(self, nodes: Nodes, x: Any) -> Iterable[Hop]:
        return (
            hop.add_prefix(self.prefix)
                for hop in self.base_edges.hops_to_node(
                    nodes.unprefixed(self.prefix),
                    unprefixed(x, self.prefix)
                )
        )

    def find_hop(self, nodes: Nodes, from_node: Any, to_node: Any) \
    -> Union[Hop, None]:
        hop = self.base_edges.find_hop(
            nodes.unprefixed(self.prefix),
            unprefixed(from_node, self.prefix),
            unprefixed(to_node, self.prefix)
        )
        if hop is not None:
            return hop.add_prefix(self.prefix)
        else:
            return None

    def __len__(self) -> int:
        return len(self.base_edges)

@dataclass
class PrefixedGraph(Graph):
    prefix: Hashable

    def __init__(self, prefix: Hashable, basegraph: Graph):
        self.prefix = prefix
        self.nodes = PrefixedNodes(self.prefix, basegraph.nodes)
        self.edges = PrefixedEdges(self.prefix, basegraph.edges)

### Features

def add_features(base_node: Node, nodeset: Set[Node], hopset: Set[Hop]):
    '''Update nodeset and hopset.'''
    new_features = set()
    nodeset.add(base_node)
    for feature_node in features_of(base_node):
        new_features.add(feature_node)
        nodeset.add(feature_node)
        '''
        in_weight = 0.01 if isclass(feature_node) else 1.0
        out_weight = 0.01 if isclass(base_node) else 1.0
        '''
        '''
        in_weight = 0.01 if isclass(base_node) or isclass(feature_node) else 1.0
        out_weight = in_weight
        '''
        in_weight = out_weight = 1.0
        #print(f'ADD {base_node} {feature_node} {in_weight} {out_weight}')
        hopset.add(Hop(base_node, feature_node, out_weight))
        hopset.add(Hop(feature_node, base_node, in_weight))
    return new_features

import Propagator as PropagatorModule
