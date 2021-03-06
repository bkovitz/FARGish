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
    caddr_of, ValueNotAvail, Operator, plus, times, minus, Consume
from util import first


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

    def as_seqstate(self):
        return SeqState(self.bricks, None)

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
        canvas2 = fm.paint((canvas1, 'cdr'), SeqCanvas(SeqState((6, 9), '4+5=9')))
        #self.assertEqual(state1, SeqCanvas(SeqState((6, 9), '4+5=9')))
        self.assertTrue(isinstance(canvas2, SeqCanvas))
        ca = caddr_of(canvas2)
        #print(state1)
        self.assertEqual(ca[0], canvas1)
        self.assertEqual(ca[1], 'cdr')
        #print(fm.all_at((canvas1, 'cdr')))
        #self.assertEqual(caddr_of(state1), (canvas1, 'cdr'))
        #TODO Verify that the SeqCanvas in 'cdr' is correct
        #print(state1)

    def test_cells_get_new_caddr(self):
        fm = FARGModel()
        canvas1 = SeqCanvas(Numble((4, 5, 6), 15))
        got = fm.paint(None, canvas1)
        self.assertEqual(caddr_of(canvas1), (fm.ws, Top))
        self.assertEqual(id(canvas1), id(got))

        canvas2 = SeqCanvas(SeqState('bogus_avails', 'bogus_lastop'))
        got = fm.paint((canvas1, 'cdr'), canvas2)
        self.assertEqual(caddr_of(canvas2), (canvas1, 'cdr'))

        # Put in a second instance of canvas2: this must create a new,
        # identical Canvas, with a different caddr.
        canvas3 = fm.paint((canvas2, 'cdr'), canvas2)
        self.assertNotEqual(canvas2, canvas3)  # Different canvases
        self.assertEqual(canvas2.car, canvas3.car)  # Identical 'car' states
        self.assertEqual(canvas2.canvas, canvas1)
        self.assertEqual(canvas3.canvas, canvas2)
        self.assertEqual(fm.get((canvas2, 'car')), fm.get((canvas3, 'car')))

        # The caddrs of the Values and Cells must be different
        self.assertEqual(canvas2.car.canvas, canvas2)
        self.assertEqual(canvas3.car.canvas, canvas3)
        self.assertEqual(canvas2.cdr.canvas, canvas2)
        self.assertEqual(canvas2.cdr.values, {canvas3})
        self.assertEqual(first(canvas2.cdr.values).canvas, canvas2)
        self.assertEqual(canvas3.cdr.canvas, canvas3)
        self.assertEqual(canvas3.cdr.values, set())

        # TODO Get the rest of this to pass

        # Do it again, putting canvas2 into the cdr of yet another Canvas.
        # This is needed to test that canvas2.cdr (which contains canvas3)
        # gets a new addr.

        #canvas4 = fm.paint(None, SeqCanvas(SeqState('new_avails', 'new_op')))
        #canvas5 = fm.paint((canvas4, 'cdr'), canvas2)
        #(self.assertEqual(canvas2
        #print('C4', canvas4)
        #print('CADDRS', id(canvas2.car.canvas), id(canvas3.car.canvas))

    #@unittest.skip('not implemented yet')
    def test_consume(self):
        #ws = Workspace()
        #canvas1 = ws.add(Top, SeqCanvas(Numble((4, 5, 6), 15)))
        #consume1 = ws.add(Top, Consume(plus, (4, 5)))
        #canvas2 = consume1.paint(ws, canvas1)
        #self.assertEqual(ws.get((canvas1, 'cdr')), canvas2)

        fm = FARGModel()
        canvas1 = fm.paint(None, SeqCanvas(Numble((4, 5, 6), 15)))
        consume1 = fm.paint(None, Consume(plus, (4, 5)))
        canvas2 = consume1.paint(fm, canvas1)
        self.assertEqual(
            canvas2.get('car'),
            SeqState(avails=(6, 9), last_move='4 + 5 = 9')
        )
        #print('\nCCC', list(canvas1.all_seqs()))
        #print('CANVAS1', canvas1)  # TODO smoke test for SeqCanvas.__str__
