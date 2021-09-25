# VirtualGraphPrimitives.py -- A lazy/virtual graph

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from dataclasses import dataclass
import math
from itertools import chain

from util import empty_set, first_non_none, unique_everseen


Node = Hashable

@dataclass(frozen=True)
class Hop:
    '''A directed edge.'''
    from_node: Node
    to_node: Node
    weight: float
    
#@dataclass
class Graph(ABC):
    '''Abstract base class for graphs.'''

    #TODO (probably)
    #def neighbors

    @abstractmethod
    def __len__(self) -> Union[int, Literal[math.inf]]: # type: ignore
        '''The number of nodes in the graph, or math.inf.'''
        pass

    @abstractmethod
    def all_nodes(self) -> Iterable[Node]:
        '''An iterable containing all the nodes in this graph.'''
        pass

    def nodes(self, ns: Iterable[Any]) -> Iterable[Node]:
        '''Returns an iterable containing all and only the nodes in ns
        that exist in this graph. Default implementation simply queries
        .has_node().'''
        #TODO uniq
        #TODO Query by user-specified criteria like IsAlphabetic.
        for n in ns:
            if self.has_node(n):
                yield n
        
    @abstractmethod
    def has_node(self, x: Any) -> bool:
        '''Does this graph contain x?'''
        pass

    def hops_from_node(self, x: Any) -> Iterable[Hop]:
        '''An iterable of all the hops leading out of x. Default implementation
        calls .successors_of(x) and gives each hop a weight of 1.0'''
        for neighbor in self.successors_of(x):
            yield Hop(x, neighbor, 1.0)

    def hops_to_node(self, x: Any) -> Iterable[Hop]:
        '''An iterable of all the hops leading into x. Default implementation
        calls .predecessors_of(x) and gives each hop a weight of 1.0'''
        for neighbor in self.predecessors_of(x):
            yield Hop(neighbor, x, 1.0)

    def hop_weight(self, from_node: Any, to_node: Any) -> float:
        '''Weight of edge from from_node to to_node, or 0.0 if no such edge
        exists.'''
        hop = self.find_hop(from_node, to_node)
        if hop is None:
            return 0.0
        else:
            return hop.weight

    @abstractmethod
    def find_hop(self, from_node: Any, to_node: Any) -> Union[Hop, None]:
        '''Returns the Hop from from_node to to_node, or None if it does
        not exist.'''
        pass

    #has_hop = find_hop
    #has_edge = find_hop

    @abstractmethod
    def successors_of(self, x: Any)-> Iterable[Node]:
        '''An iterable of all the successor nodes of x.'''
        pass

    @abstractmethod
    def predecessors_of(self, x: Any)-> Iterable[Node]:
        '''An iterable of all the predecessor nodes of x.'''
        pass

    @classmethod
    def augment(cls, *graphs: 'Graph') -> 'Graph':
        #TODO docstring
        return GraphSeries(graphs)

#@dataclass
class NoEdges(Graph):
    '''Has methods that override those in Graph to return no edges, with
    no computation.'''

    def find_hop(self, from_node: Any, to_node: Any) -> Union[Hop, None]:
        return None

    def successors_of(self, x: Any)-> Iterable[Node]:
        return []

    def predecessors_of(self, x: Any)-> Iterable[Node]:
        return []

@dataclass
class LiteralGraph(NoEdges):
    '''A graph consisting of a set of nodes specified literally when the
    graph is constructed, and no edges.'''
    literals: Sequence[Node]
    
    def __init__(self, literals, **kwargs):
        self.literals = literals
        super().__init__(**kwargs)

    def all_nodes(self):
        return self.literals

    def has_node(self, x):
        return x in self.literals

    def __len__(self):
        return len(self.literals)

"""
#@dataclass  commented out due to mypy bug https://stackoverflow.com/q/69330256/1393162
@dataclass(frozen=True)
class WantEdges(Graph):
    #TODO docstring
    want_edges: Dict[Node, FrozenSet[Node]]

    def __init__(self, want_edges, **kwargs):
        self.want_edges = want_edges
        super().__init__(**kwargs)

    def find_hop(self, from_node, to_node):
        print('WAE', from_node, to_node,
            self.has_node(from_node),
            to_node in self.want_edges.get(from_node, empty_set),
            self.has_node(to_node),
            self.__class__,
            self.all_nodes(),
        )
        if (
            self.has_node(from_node)
            and
            to_node in self.want_edges.get(from_node, empty_set)
            and
            self.has_node(to_node)
        ):
            return Hop(from_node, to_node, 1.0)

    def successors_of(self, from_node):
        for to_node in self.nodes(self.want_edges.get(from_node, empty_set)):
            yield to_node

    def predecessors_of(self, to_node):
        raise NotImplementedError
"""
        
@dataclass
class GraphSeries(Graph):
    graphs: Sequence[Graph]
    
    def __len__(self):
        raise NotImplementedError

    def all_nodes(self):
        return unique_everseen(chain.from_iterable(
            g.all_nodes() for g in self.graphs)
        )

    def has_node(self, x):
        return any(g.has_node(x) for g in self.graphs)

    def hops_from_node(self, x):
        raise NotImplementedError

    def hops_to_node(self, x):
        raise NotImplementedError

    # BUG g.find_hop() calls its own has_node(), not our override.
    # Consequently WantEdges.find_hop() doesn't see augmented nodes.
    def find_hop(self, from_node, to_node):
        for g in self.graphs:
            method = g.__class__.find_hop
            hop = method(self, from_node, to_node)
            if hop is not None:
                return hop
        '''
        return first_non_none(
            g.find_hop(from_node, to_node) for g in self.graphs
        )
        '''

    def successors_of(self, x):
        return unique_everseen(chain.from_iterable(
            g.successors_of(x) for g in self.graphs)
        )

    def predecessors_of(self, x):
        return unique_everseen(chain.from_iterable(
            g.predecessors_of(x) for g in self.graphs)
        )
