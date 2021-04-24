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
    backslash, singleton, first

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

def needs_init(x: Any) -> Union[bool, Callable]:
    return getattr(x, 'needs_make', False)

def as_seqstate(x: Any) -> 'SeqState':
    # TODO Catch exception if x has no .as_seqstate()?
    return x.as_seqstate()

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

    def as_seqstate(self):
        return self
    
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

    def __hash__(self):
        return hash(id(self))

    def __eq__(self, other):
        return self is other

@dataclass
class Cell:
    '''A set of Values that share the same Addr and compete with each other,
    such as mutually contrary values for one element of a canvas.

    No two Cells may have the same CAddr.

    A Cell is not a Value. It cannot be put into another Cell.
    '''
    canvas: Union[Canvas, None] = None
    addr: Addr = None
    values: Set[Value] = field(default_factory=set)

    def add_value(self, fm: 'FARGModel', value: Value, painter: Value=None):
        '''Returns 'value' with this Cell's CAddr added.'''
        value = fm.with_caddr(as_hashable(value), (self.canvas, self.addr))
        for oldv in self.values:
            fm.add_mut_antipathy(oldv, value)
        self.values.add(value)
        fm.add_mut_support(value, painter)
        return value

@dataclass
class Maker:
    o: Any

@dataclass
class SeqCanvas(Canvas):
    car: Cell = field(default_factory=Cell)  # Cell[SeqState]
    cdr: Cell = field(default_factory=Cell)  # Cell[SeqCanvas]

#    needs_init: Union[bool, Callable] = field(
#        default=False, init=False, repr=False
#    )
#    _args: Tuple = field(default=(), init=False, repr=False)
#    _kwargs: Dict = field(default_factory=dict, init=False, repr=False)

    def __init__(self, car=None, cdr=None):
        self.car = Cell()
        self.cdr = Cell()
        def f(
            fm: 'FARGModel',
            painter: Value
        ):
            self.car.canvas = self
            self.car.addr = 'car'
            if car is not None:
                self.car.add_value(fm, car, painter=painter)
            self.cdr.canvas = self
            self.cdr.addr = 'cdr'
            if cdr is not None:
                self.cdr.add_value(fm, cdr, painter=painter)
            self.needs_init = False
        self.needs_init = f

    # TODO Make 'value' the 2nd arg so addr can have a default
    #def paint(self, fm: 'FARGModel', addr: Addr='cdr', value, **kwargs) \
    def paint(
        self,
        fm: 'FARGModel',
        addr: Addr,
        value,
        painter: Value=None,
        **kwargs
    ) -> Value:
        cell = getattr(self, addr)  # TODO Catch invalid addr?
        value = cell.add_value(fm, value, painter)
        return value

    def all_at(self, addr: Addr, **kwargs) -> Iterable[Value]:
        addr = self.normalize_addr(addr, **kwargs)
        cell = getattr(self, addr)  # TODO Catch invalid addr?
        return cell.values

    def normalize_addr(self, addr: Addr, **kwargs) -> Addr:
        if addr is None or addr is Top:
            return 'car'
        else:
            return addr

    def get(self, addr, **kwargs) -> Value:
        # TODO Better way of choosing which Cell value
        # TODO Throw exception if nothing is at 'addr'
        # TODO Translate numerical addr into advancing that many steps forward
        return first(self.all_at(addr, **kwargs))

    def find_one(self, criterion, **kwargs) -> Value:
        raise NotImplementedError

    def find_all(self, criterion, **kwargs) -> Iterable[Value]:
        raise NotImplementedError

    def __hash__(self):
        return hash(id(self))

    def __eq__(self, other):
        return self is other

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

    def all_at(self, addr: Addr, **kwargs) -> Iterable[Value]:
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

    def __hash__(self):
        return hash(id(self))

    def __eq__(self, other):
        return self is other

@dataclass(frozen=True)
class NodeWrapper:
    node: Hashable
    caddr: CAddr

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
class FARGModel:
    ws: Workspace = field(default_factory=Workspace)

    support_g: SupportGraph = field(default_factory=SupportGraph, init=False)
    mutual_support_weight: ClassVar[float] = 1.0
    mutual_antipathy_weight: ClassVar[float] = -0.2

    def paint(
        self,
        caddr: Union[CAddr, None],
        value: Value,
        painter: Value=None,
        **kwargs
    ):
        needs_init = getattr(value, 'needs_init', False)
        if needs_init:
            needs_init(self, painter)
        canvas, addr = self.unpack_caddr(caddr)
        return canvas.paint(self, addr, value, painter=painter, **kwargs)

    def add_mut_support(self, a: Hashable, b: Hashable):
        a = self.as_node(a)
        b = self.as_node(b)
        self.support_g.add_edge(
            a, b, weight=self.mutual_support_weight
        )
        
    def add_mut_antipathy(self, a: Hashable, b: Hashable):
        a = self.as_node(a)
        b = self.as_node(b)
        self.support_g.add_edge(
            a, b, weight=self.mutual_antipathy_weight
        )

    def support_weight(self, a: Hashable, b: Hashable) -> float:
        '''Returns the mutual support between nodes 'a' and 'b'. If neither
        node exists, or there is no edge between them in self.support_g,
        then the weight is 0.0.'''
        a = self.as_node(a)
        b = self.as_node(b)
        try:
            return self.support_g.edges[a, b]['weight']
        except KeyError:
            return 0.0

    def as_node(self, a: Hashable) -> Hashable:
        if hasattr(a, 'canvas') or hasattr(a, 'addr'):
            return NodeWrapper(a, caddr_of(a))
        else:
            return a

    def get(self, caddr: Addr, **kwargs) -> Value:
        # TODO Document exception thrown if nothing is at caddr
        # Reject if canvas no longer exists (even if the object still exists)
        canvas, addr = self.unpack_caddr(caddr)
        return canvas.get(addr, **kwargs)

    def all_at(self, caddr: Addr, **kwargs) -> Iterable[Value]:
        canvas, addr = self.unpack_caddr(caddr)
        return canvas.all_at(addr, **kwargs)

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
class Cell0:
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

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def call(self, *operands: int) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class PainterWithArgs:
    painter: Hashable
    arg: Hashable
    # TODO more flexibility for arg/args

    def __str__(self):
        return "PARG"

@dataclass(frozen=True)
class Consume:
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None

    def paint(self, fm: FARGModel, caddr: CAddr) -> SeqCanvas:
        '''Tries to consume avails from caddr, paint a new value to caddr,
        and return the SeqCanvas whose car is the result of applying
        'self.operation' to the avails.
        TODO Document exception thrown if avails are lacking.
        '''
        start_canvas, start_addr = fm.unpack_caddr(caddr)
        start_state = as_seqstate(fm.get(caddr))
        taken_avails, remaining_avails = start_state.take_avails(self.operands)
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        expr = f' {self.operator} '.join(str(n) for n in taken_avails)
        move_str = f'{expr} = {result}'
        pargs = PainterWithArgs(self, start_canvas)
        new_canvas = fm.paint(
            (start_canvas, 'cdr'),
            SeqCanvas(SeqState(new_avails, move_str)),
            painter=pargs
        )
        fm.add_mut_support(new_canvas, pargs)
        fm.add_mut_support(self, pargs)
        return new_canvas

    def all_at(self, addr: Addr, **kwargs) -> Iterable[Value]:
        raise NotImplementedError

    def get(self, addr, **kwargs) -> Value:
        raise NotImplementedError

    def find_one(self, criterion, **kwargs) -> Value:
        raise NotImplementedError

    def find_all(self, criterion, **kwargs) -> Iterable[Value]:
        raise NotImplementedError
# Functions for querying and constructing Elems and Addrs

#def with_addr(value: Hashable, canvas: Canvas, addr: Addr) -> Hashable:
