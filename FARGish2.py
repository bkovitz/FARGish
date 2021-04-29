# FARGish2.py -- The generic classes and functions for making a FARG model

# Unlike in FARGish.py, this version does not represent sequences with a Cell
# for each step (containing multiple, competing Values). Instead, every
# distinct temporal sequence is represented by a separate object.


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
from io import StringIO

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, pl, csep, ssep, as_hashable, \
    backslash, singleton, first


# Classes

Value = NewType('Value', Hashable)
Addr = NewType('Addr', Hashable)


@dataclass
class FARGModel:
    # TODO support_g and associated methods

    def paint_value(self, canvas: 'Canvas', addr: Addr, v: Value):
        canvas.paint_value(self, addr, v)


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


@dataclass(eq=False)
class Canvas(ABC):

    @abstractmethod
    def paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        '''Put Value v at address addr. The lowest-level canvas-writing
        operation.'''
        pass

    # TODO Make this into __getitem__?
    @abstractmethod
    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        '''Returns the value at addr, or None if the cell at addr is blank.'''
        pass

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

@dataclass(eq=False)
class SeqCanvas:
    states: List[SeqState]

    def paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        while len(self.states) <= addr:
            self.states.append(None)
        self.states[addr] = v

    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        # TODO Handle addr that can't be found or is not an index
        return self.states[addr]

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        return self.states[addr]


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
class Painter(ABC):

    @abstractmethod
    # TODO Do we really need to pass 'fm'?
    def paint(self, fm: FARGModel):
        pass

@dataclass(frozen=True)
class Consume(Painter):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None
    canvas: Union[SeqCanvas, None] = None  # what to paint on
    addr: Hashable = None                  # where to paint result

    def paint(self, fm: FARGModel):
        # TODO Throw exception if any members (args) are missing
        s0: SeqState = self.canvas[self.addr - 1]
        taken_avails, remaining_avails = s0.take_avails(self.operands)
        result = self.operator.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        s1 = SeqState(new_avails, self)
        fm.paint_value(self.canvas, self.addr, s1)

    #def __str__(self):
    # TODO

@dataclass
class Parg(Painter):
    '''A complex of painter objects and objects specifying arguments for
    them, suitable for support relationships between PargComplexes, painters,
    arguments, and the sources thereof.'''
    painter: Painter
    kwargs: Dict[str, Any] = field(default_factory=dict)

    def add_arg(self, **kwargs):
        '''Alter .painter and .kwargs to match kwargs.'''
        # TODO Update support_g?
        # TODO If an arg already has a non-None value defined, do we create
        # a new Parg with mutual antipathy with the old Parg?
        self.kwargs.update(kwargs)
        self.painter = self.painter.__class__(**self.kwargs)

    def paint(self, fm: FARGModel):
        # TODO Throw exception of .painter doesn't have enough args
        self.painter.paint(fm)

if __name__ == '__main__':
    if False:
        # TODO Make a UT out of this
        fm = FARGModel()
        s0 = SeqState((4, 5, 6), None)
        c = SeqCanvas([s0])
        cu = Consume(plus, (4, 5), c, 1)
        cu.paint(fm)
        print(c)

    fm = FARGModel()
    s0 = SeqState((4, 5, 6), None)
    c = SeqCanvas([s0])
    p = Parg(Consume())
    p.add_arg(canvas=c)
    p.add_arg(addr=1, operator=plus, operands=(5, 4))
    p.paint(fm)
    print(c)


    #s0 = fm.add(Top, SeqState((2, 3, 4, 5, 11), None))
    #g0 = fm.make_seqglom(s0)
    #s1 = fm.consume(s0, Times, (5, 11))
    #g1 = fm.append(g0, s1)
    #s2 = fm.consume(s0, Times, (4, 11))
    #g2 = fm.append(g0, s2)
    #? g2 = fm.consume(g0, Times, (4, 11))
    # Now see the support graph
