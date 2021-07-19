# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import operator
from operator import itemgetter, attrgetter

from FARGModel import FARGModel, Canvas, SeqCanvas, SeqState, StateDelta, \
    CellRef
from FMTypes import Value, Addr


@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str

    def call(self, *operands) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class ArithDelta(StateDelta):
    '''A completed arithmetic operation.'''
    before: Sequence
    after: Union[Value, Collection]
    how: Operator

    def seq_str(self):
        expr = f' {self.how} '.join(str(n) for n in self.before)
        return f'{expr} = {self.after}'

class testFARGModel(unittest.TestCase):

    def test_basics(self):
        fm = FARGModel()

        # Build something
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        # Is it there?
        eiws = fm.get_eiws(ca)
        self.assertEqual(eiws.elem, ca)
        # Can we find it by exact match?
        self.assertEqual(fm.the(ca), ca)
        # Can we find it by searching for its class?
        self.assertEqual(fm.the(Canvas), ca)

        # TODO Check builder and behalf_of

        # NEXT Paint the next state? Log the BUILT?
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas
        ad = ArithDelta((4, 5), 9, plus)
        state1 = SeqState((9, 6), ad)
        fm.paint(cr, state1)
        self.assertEqual(ca[1], state1)

