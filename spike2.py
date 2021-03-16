# spike2.py -- Architectural spike for FARGish/Numbo without a port graph

from pprint import pprint as pp
import inspect
from time import process_time

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter

import networkx as nx
import matplotlib.pyplot as plt

from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, csep, ssep, as_hashable


NodeId = NewType('NodeId', int)

Value = NewType('Value', Hashable)
Painter = Any  # TODO narrow this down

# Classes

class SupportGraph(nx.Graph):

    def ns(self, node) -> List[str]:
        '''Returns list of neighbors represented as strings.'''
        return [str(neighbor) for neighbor in self.neighbors(node)]


@dataclass
class Workspace:
    elems: Set['WorkspaceElement'] = field(default_factory=set)
    d: Dict[Hashable, Any] = field(default_factory=dict)

    support_g: SupportGraph = field(default_factory=SupportGraph, init=False)

    mutual_antipathy_weight: ClassVar[float] = -0.2
    
    def add(self, elem: 'WorkspaceElement'):
        self.elems.add(elem)

    def get(self, addr: Hashable) -> Hashable:
        return self.d.get(addr, None)

    def paint(self, addr: Hashable, elem: 'WorkspaceElement'):
        '''
        anything at that addr yet?
        no: add it
        yes, but it's already elem: do nothing
        yes, and it's a different elem: split into Cell with competing Values
        yes, and it's a Cell with competing Values: add elem to that Cell
        '''
        #elems.add(elem)
        existing = self.d.get(addr, None)
        if existing is None:
            self.d[addr] = elem
            return
        elif existing == elem:
            return
        if isinstance(existing, Cell):
            cell = existing
        else:  # else it's a competing elem
            cell = Cell()
            existing = ValueInCell(existing, addr)
            cell.add(existing)
        new_vic = ValueInCell(elem, addr)
        for old_elem in cell:
            self.support_g.add_edge(
                new_vic, old_elem, weight=self.mutual_antipathy_weight
            )
        cell.add(new_vic)
        self.d[addr] = cell

    def __str__(self):
        return 'Workspace\n' + self.dstr(prefix='  ')

    def dstr(self, prefix: str='  ') -> str:
        lines = []
        for addr, elem in self.d.items():
            lines.append(f'{prefix}{addr} -> {elem}')
        return '\n'.join(lines)

@dataclass
class Cell:
    #canvas: 'Canvas'
    values: Set[Value] = field(default_factory=set)

    def add_value(self, source: Painter, value: Value):
        self.values.add(as_hashable(value))
        #NEXT Mutual support to painter
        #NEXT Mutual opposition between values

    def add(self, value: Value):
        self.values.add(as_hashable(value))

    def __iter__(self):
        return iter(self.values)

    def __str__(self):
        if not self.values:
            return '{}'
        elif len(self.values) == 1:
            return str(list(self.values)[0])
        else:
            return f'{{{ssep(self.values)}}}'

@dataclass(frozen=True)
class ValueInCell:
    value: Hashable
    addr: Hashable

    def __str__(self):
        return f'ValueInCell({self.value}, {self.addr})'

@dataclass(frozen=True)
class Canvas:
    pass

@dataclass(frozen=True)
class SeqCanvas(Canvas):
    car: Cell = None
    cdr: Cell = None

    def add_value(self, source: Painter, cell_name: str, value: Value):
        cell = getattr(self, cell_name, None)
        if cell is None:
            cell = Cell()
            setattr(self, cell_name, cell)
        cell.add_value(source, value)

    def __str__(self):
        return f'{self.car}; {self.cdr}'

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
class Numble:
    bricks: Tuple[int]
    target: int
    operators: Set[Operator] = frozenset([plus, times, minus])

    @property
    def avails(self) -> List[int]:
        return self.bricks

    def __str__(self):
        bricks_str = ', '.join(str(b) for b in self.bricks)
        return f'from {bricks_str} make {self.target}'

@dataclass(frozen=True)
class SolnState:
    avails: Union[Tuple[int], None] = None
    last_move: Union[str, None] = None

    def __str__(self):
        if self.last_move is None:
            if avails is None:
                return '[ ]'
            else:
                return f'[{self.last_move}; ]'
        else:
            return f'[{self.last_move}; {ssep(self.avails)}]'

def avails(o) -> List[int]:
    #TODO try-except
    return as_list(o.avails)
    
@dataclass(frozen=True)
class Consume:  # TODO inherit from Painter
    # TODO Cell[Operator], etc.
    operator: Union[Operator, None] = None
    operands: Union[List[int], None] = None
    
    def paint(self, canvas: SeqCanvas):
        new_avails = self.without_operands(canvas)
        result = self.operator.call(*self.operands)
        expr = f' {self.operator} '.join(str(n) for n in self.operands)
        move_str = f'{expr} = {result}'
        canvas.add_value(self, 'cdr', SolnState(new_avails, move_str))
        # TODO catch exc from without_operands

    def without_operands(self, canvas: SeqCanvas) -> Tuple[int]:
        new_avails = list(avails(canvas.car))
        # TODO something when .remove fails
        for operand in self.operands:
            new_avails.remove(operand)
        return tuple(new_avails)

    def __str__(self):
        return f'C({self.operator}, {ssep(self.operands)})'

# Global variable

ws = Workspace()

# 'main' test code

plt.ion()

soln_canvas = SeqCanvas(Numble((4, 5, 6), 15))
#c1 = Consume(plus, [4, 5])
#c1.paint(soln_canvas)
#c2 = Consume(times, [4, 5])
#c2.paint(soln_canvas)
#print(soln_canvas)

ss2a = SolnState((6, 9), '4+5=9')
ss2b = SolnState((6, 9), '4x5=20')
ss2c = SolnState((5, 24), '4x6=24')
ss2d = SolnState((4, 30), '5x6=30')
#print(soln_canvas)
print(ws.get(('SolnCanvas', 'cdr')))
#ws.paint(('SolnCanvas', 'cdr'), ss2a)
#ws.paint(('SolnCanvas', 'cdr'), ss2a)
#ws.paint(('SolnCanvas', 'cdr'), ss2b)
#ws.paint(('SolnCanvas', 'cdr'), ss2c)
for ss in [ss2a, ss2a, ss2b, ss2c, ss2d]:
    ws.paint(('SolnCanvas', 'cdr'), ss)
    print(ws.get(('SolnCanvas', 'cdr')))
    print()
    
print(ws)
pos = nx.spring_layout(ws.support_g)
nx.draw(ws.support_g, pos, with_labels=True)
nx.draw_networkx_edge_labels(ws.support_g, pos, edge_labels=nx.get_edge_attributes(ws.support_g, 'weight'))
