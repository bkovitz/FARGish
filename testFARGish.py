# testFARGish.py -- Unit tests for FARGish.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable
import operator

import matplotlib.pyplot as plt
#import netgraph

from FARGish import FARGModel, Workspace, Cell, SeqState, SeqCanvas, Top, \
    caddr_of, ValueNotAvail


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
class Wrapper:
    '''For wrapping primitives types like int and str.'''
    x: Any

@dataclass(frozen=True)
class TestPainter:
    '''For testing whether support networks are set up correctly.'''
    name: str


class TestFARGish(unittest.TestCase):

    def test_ws(self):
        fm = FARGModel()
        numble = fm.paint(None, Numble((4, 5, 6), 15))
        self.assertEqual(caddr_of(numble), (fm.ws, Top))
        self.assertEqual(numble, Numble((4, 5, 6), 15))
        self.assertCountEqual(fm.all_at(None), [Numble((4, 5, 6), 15)])
        # TODO add something else to Top
        # TODO test for support betw painter and value

    def test_seqstate(self):
        ss = SeqState((4, 5, 6))
        self.assertEqual(ss.avails, (4, 5, 6))

        taken, remaining = ss.take_avails([5, 4])
        self.assertEqual(taken, [5, 4])
        self.assertCountEqual(remaining, [6])

        with self.assertRaises(ValueNotAvail) as cm:
            taken, remaining = ss.take_avails([5, 400])
        self.assertEqual(cm.exception, ValueNotAvail(ss, 400))

    def test_cell(self):
        fm = FARGModel()
        cell = Cell(canvas='CV', addr=100)
        self.assertEqual(cell.values, set())

        painter1a = TestPainter('1a')
        painter1b = TestPainter('1b')
        self.assertEqual(fm.support_weight(Wrapper(1), painter1a), 0.0)

        # Add a value to the Cell. There should be mutual support between
        # the painter and the value.
        w1a = cell.add_value(fm, Wrapper(1), painter=painter1a)
        self.assertEqual(cell.values, set([w1a]))
        self.assertEqual(caddr_of(w1a), ('CV', 100))
        self.assertEqual(fm.support_weight(Wrapper(1), painter1a), 0.0)
        self.assertEqual(fm.support_weight(w1a, painter1a), 1.0)

        w1b = cell.add_value(fm, Wrapper(1), painter=painter1b)
        # Identical value, so no new value is added, but support from
        # painter1b is added.
        self.assertEqual(cell.values, set([w1a]))
        self.assertEqual(caddr_of(w1a), caddr_of(w1b))
        self.assertEqual(fm.support_weight(w1a, painter1b), 1.0)

        painter2 = TestPainter('2')
        # Different value, so there should be mutual antipathy between the
        # values.
        w2 = cell.add_value(fm, Wrapper(2), painter=painter2)
        self.assertEqual(cell.values, set([w1a, w2]))
        self.assertEqual(caddr_of(w1a), caddr_of(w2))
        self.assertEqual(fm.support_weight(w1a, w2), -0.2)

        # Now let's make a different Cell containing the same value. There
        # should be no support or antipathy between the values of different
        # Cells, even if the values are equal.
        cell2 = Cell(canvas='CV', addr=101)
        painter1c = TestPainter('1c')
        w1c = cell2.add_value(fm, Wrapper(1), painter=painter1c)
        self.assertEqual(w1a, w1c) # The values are equal...
        self.assertNotEqual(caddr_of(w1a), caddr_of(w1c))
            # ...but have different CAddrs.
        self.assertEqual(fm.support_weight(w1a, w1c), 0.0)
        self.assertEqual(fm.support_weight(w2, w1c), 0.0)

    def test_seqcanvas(self):
        fm = FARGModel()
        canvas1 = fm.paint(None, SeqCanvas(Numble((4, 5, 6), 15)))
        self.assertTrue(isinstance(canvas1, SeqCanvas))
        # TODO Test for same Cell contents
        state1 = fm.paint((canvas1, 'cdr'), SeqCanvas(SeqState((6, 9), '4+5=9')))
        #self.assertEqual(state1, SeqCanvas(SeqState((6, 9), '4+5=9')))
        self.assertTrue(isinstance(state1, SeqCanvas))
        ca = caddr_of(state1)
        #print(state1)
        #self.assertEqual(ca[0], canvas1)
        self.assertEqual(ca[1], 'cdr')
        print(fm.all_at((canvas1, 'cdr')))
        #self.assertEqual(caddr_of(state1), (canvas1, 'cdr'))
        #TODO Verify that the SeqCanvas in 'cdr' is correct
        #print(state1)

    @unittest.skip('not implemented yet')
    def test_consume(self):
        ws = Workspace()
        canvas1 = ws.add(Top, SeqCanvas(Numble((4, 5, 6), 15)))
        consume1 = ws.add(Top, Consume(plus, (4, 5)))
        canvas2 = consume1.paint(ws, canvas1)
        self.assertEqual(ws.get((canvas1, 'cdr')), canvas2)
