# FARGish.py -- The generic classes and functions for making a FARG model

# Consume: provide arguments; get them from a source


from pprint import pprint as pp
import inspect
from time import process_time

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
from abc import ABC, abstractmethod
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, pl, csep, ssep, as_hashable, \
    backslash, singleton

# Global atoms (constants)

@singleton
@dataclass(frozen=True)
class Atom:
    name: str

    def __str__(self):
        return self.name

Top = Atom(name='Top')
All = Atom(name='All')

# Classes

Elem = NewType('Elem', Hashable)
Addr = NewType('Addr', Hashable)
Value = NewType('Value', Hashable)
CAddr = Union[Tuple['Canvas', Addr], 'Canvas', None]

def caddr_of(x: Any) -> Tuple['Canvas', Addr]:
    return (getattr(x, 'canvas', None), getattr(x, 'addr', None))

class ElemSet(set):
    '''A set of Elems that share the same Addr but do not compete with each
    other.'''
    def coalesce_with(self, other: Elem) -> 'ElemSet':
        if isinstance(other, set):
            self |= other
        else:
            self.add(other)
        return self

@dataclass
class ValueNotAvail(Exception):
    container: Any
    value: Any

@dataclass(frozen=True)
class SeqState:  # TODO Inherit from Value?
    avails: Union[Tuple[int], None] = None
    last_move: Union[str, None] = None

    #TODO In __iter__, make a tuple out of avails if it's not Hashable
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValueNotAvail.'''
        remaining_avails = [] if self.avails is None else list(self.avails)
        taken_avails = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                raise ValueNotAvail(self, v)
            taken_avails.append(v)
        return (taken_avails, remaining_avails)
    
@dataclass
class Canvas(ABC):
    '''A Canvas changes when it's painted to; a Value is what is painted;
    painting always occurs at an Addr.'''
    @abstractmethod
    def paint(self, fm: 'FARGModel', addr: Addr, value, **kwargs) -> Value:
        pass

    @abstractmethod
    def all_at(self, addr: Addr, **kwargs) -> Iterable[Value]:
        pass

    @abstractmethod
    def get(self, addr, **kwargs) -> Value:
        pass

    @abstractmethod
    def find_one(self, criterion, **kwargs) -> Value:
        pass

    @abstractmethod
    def find_all(self, criterion, **kwargs) -> Iterable[Value]:
        pass

@dataclass
class Cell:
    #NEXT
    pass

@dataclass
class SeqCanvas(Canvas):
    car: Cell = field(default_factory=Cell)  # Cell[SeqState]
    cdr: Cell = field(default_factory=Cell)  # Cell[SeqCanvas]

    #TODO __init__: accept a value for car

    # TODO Make 'value' the 2nd arg so addr can have a default
    #def paint(self, fm: 'FARGModel', addr: Addr='cdr', value, **kwargs) \
    def paint(self, fm: 'FARGModel', addr: Addr, value, **kwargs) \
    -> Value:
        # stash value in addr Cell
        # update support_g or leave that to Cell?
        pass

    @abstractmethod
    def all_at(self, addr: Addr, **kwargs) -> Iterable[Value]:
        pass

    @abstractmethod
    def get(self, addr, **kwargs) -> Value:
        pass

    @abstractmethod
    def find_one(self, criterion, **kwargs) -> Value:
        pass

    @abstractmethod
    def find_all(self, criterion, **kwargs) -> Iterable[Value]:
        pass

@dataclass
class Workspace(Canvas):
    '''In a Workspace, all Values with addr==Top coexist with no mutual
    antipathy.'''
    top_level: Set[Elem] = field(default_factory=set)

    def paint(self, fm: 'FARGModel', addr: Addr, value, **kwargs) -> Value:
        '''Returns value updated with .canvas=self and .addr=Top.'''
        if addr is Top or addr is None:
            value = fm.with_caddr(value, (self, Top))
            self.top_level.add(value)
            # TODO Initialize activation level?
            return value
        else:
            raise ValueError(
                f'The addr for a Value in a Workspace must be Top; was {repr(addr)}'
            )

    def all_at(self, fm: 'FARGModel', addr: Addr, **kwargs) -> Iterable[Value]:
        if addr is Top or addr is None:
            return self.top_level
        else:
            raise ValueError(
                f'The addr for a Value in a Workspace must be Top; was {repr(addr)}'
            )

    def get(self, addr, **kwargs) -> Value:
        raise NotImplementedError

    def find_one(self, criterion, **kwargs) -> Value:
        raise NotImplementedError

    def find_all(self, criterion, **kwargs) -> Iterable[Value]:
        raise NotImplementedError

@dataclass
class FARGModel:
    ws: Workspace = field(default_factory=Workspace)

    def paint(self, caddr: Union[CAddr, None], value: Value, **kwargs):
        canvas, addr = self.unpack_caddr(caddr)
        return canvas.paint(self, addr, value, **kwargs)

    def all_at(self, caddr: Addr, **kwargs) -> Iterable[Value]:
        canvas, addr = self.unpack_caddr(caddr)
        return canvas.all_at(self, addr, **kwargs)

    def unpack_caddr(self, caddr: CAddr) -> Tuple[Canvas, Addr]:
        if caddr is None:
            return (self.ws, Top)
        elif isinstance(caddr, tuple):
            assert len(caddr) == 2
            return caddr
        elif isinstance(caddr, Canvas):
            return (caddr, None)
        else:
            raise ValueError(
                f'caddr must be (Canvas, Addr), Canvas, or None; was {repr(caddr)}'
            )

    def with_caddr(self, value: Hashable, caddr: CAddr) -> Hashable:
        canvas, addr = self.unpack_caddr(caddr)
        oldcanvas = getattr(value, 'canvas', None)
        oldaddr = getattr(value, 'addr', None)
        if oldaddr == addr and oldcanvas is canvas:
            return value
        new_value = copy(value)
        object.__setattr__(new_value, 'canvas', canvas)  # TODO document this
        object.__setattr__(new_value, 'addr', addr)  # TODO document this
        return new_value



@dataclass
class Cell:
    '''A set of Elems that share the same Addr and compete with each other,
    such as mutually contrary values for one cell of a canvas.'''
    #canvas: 'Canvas'
    ws: 'Workspace'
    addr: Addr
    values: Set[Value] = field(default_factory=set)

    def add(self, value: Value):
        value = with_addr(as_hashable(value), self.addr)
        if value not in self.values:
            for v in self.values:
                self.ws.add_mut_antipathy(v, value)
        self.values.add(value)

    def coalesce_with(self, other: Elem) -> 'Cell':
        for v in as_values(other):
            self.add(v)
        return self
            
    def as_values(self) -> Iterable[Value]:
        return self.values       

    def __iter__(self):
        return iter(self.values)

    def __len__(self):
        return len(self.values)

    def __str__(self):
        if not self.values:
            return '{}'
        elif len(self.values) == 1:
            return str(list(self.values)[0])
        else:
            return f'{{{ssep(self.values)}}}'

class SupportGraph(nx.Graph):

    def ns(self, node) -> List[str]:
        '''Returns list of neighbors represented as strings.'''
        return [gstr(neighbor) for neighbor in self.neighbors(node)]

    def draw(self):
        global pos, node_labels, plot_instance
        pos = nx.layout.spring_layout(self)
        node_labels = dict((node, gstr(node)) for node in self.nodes)
#        nx.draw(self, pos, with_labels=True, labels=node_labels)
#        nx.draw_networkx_edge_labels(
#            self, pos, edge_labels=nx.get_edge_attributes(self, 'weight')
#        )
        plot_instance = netgraph.InteractiveGraph(
            self,
            node_positions=pos,
            node_labels=node_labels,
            node_label_font_size=6
        )
        #plot_instance = netgraph.InteractiveGraph(self)

@dataclass
class OLDWorkspace:  # TODO Make some of this into FARGModel
    d: Dict[Hashable, Any] = field(default_factory=dict)

    support_g: SupportGraph = field(default_factory=SupportGraph, init=False)

    mutual_support_weight: ClassVar[float] = 1.0
    mutual_antipathy_weight: ClassVar[float] = -0.2
    
    def __post_init__(self):
        self.add(Top, ElemSet())

    def add(self, addr: Addr, elem: Elem, src: Union[Elem, None]=None) \
    -> Elem:
        if addr is None:
            raise ValueError(f'addr must not be None')
        elem = with_addr(elem, addr)
        existing = self.d.get(addr, None)
        self.d[addr] = self.coalesce(existing, elem)
        if src is not None:
            self.add_mut_support(src, elem)
        return elem

    def coalesce(self, old_elem: Elem, new_elem: Elem) -> Elem:
        if old_elem is None:
            return new_elem
        elif hasattr(old_elem, 'coalesce_with'):
            return(old_elem.coalesce_with(new_elem))
        else:
            cell = Cell(ws=self, addr=addr_of(old_elem))
            cell.coalesce_with(old_elem)
            cell.coalesce_with(new_elem)
            return cell
        
    def add_mut_support(self, a: Hashable, b: Hashable):
        self.support_g.add_edge(
            a, b, weight=self.mutual_support_weight
        )
        
    def add_mut_antipathy(self, a: Hashable, b: Hashable):
        self.support_g.add_edge(
            a, b, weight=self.mutual_antipathy_weight
        )
        
        
# Functions for querying and constructing Elems and Addrs

#def with_addr(value: Hashable, canvas: Canvas, addr: Addr) -> Hashable:
