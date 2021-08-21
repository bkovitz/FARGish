# VirtualGraphPrimitives.py -- A lazy/virtual graph

from abc import ABC, abstractmethod
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from dataclasses import dataclass

Node = Hashable

@dataclass(frozen=True)
class Hop:
    '''A directed edge.'''
    from_node: Node
    to_node: Node
    weight: float
    
class VirtualGraphPrimitives(ABC):
    '''
    nodes  (a query)
    has_node
    hops_from_node
    hops_to_node
    '''

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

    @abstractmethod
    def successors_of(self, x: Any)-> Iterable[Node]:
        '''An iterable of all the successor nodes of x.'''
        pass

    @abstractmethod
    def predecessors_of(self, x: Any)-> Iterable[Node]:
        '''An iterable of all the predecessor nodes of x.'''
        pass

