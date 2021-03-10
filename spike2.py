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

from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, csep, ssep, as_hashable


NodeId = NewType('NodeId', int)

Value = NewType('Value', Hashable)
Painter = Any  # TODO narrow this down

@dataclass
class Cell:
    values: Set[Value] = field(default_factory=set)

    def add_value(self, source: Painter, value: Value):
        self.values.add(as_hashable(value))
        #NEXT Mutual support to painter
        #NEXT Mutual opposition between values

    def __str__(self):
        if not self.values:
            return '{}'
        elif len(self.values) == 1:
            return str(list(self.values)[0])
        else:
            return f'{{{ssep(self.values)}}}'

@dataclass
class SeqCanvas:
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

soln_canvas = SeqCanvas(Numble((4, 5, 6), 15))
c1 = Consume(plus, [4, 5])
c1.paint(soln_canvas)
c2 = Consume(times, [4, 5])
c2.paint(soln_canvas)
print(soln_canvas)
