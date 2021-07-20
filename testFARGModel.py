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
    CellRef, LitPainter
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

class TestFARGModel(unittest.TestCase):

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
        # Does it have initial activation?
        self.assertEqual(fm.a(ca), 1.0)

        # TODO Check builder and behalf_of

        # TODO Log the BUILT?
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas
        ad = ArithDelta((4, 5), 9, plus)
        state1 = SeqState((9, 6), ad)
        fm.paint(cr, state1)  # TODO paint_on()?
        self.assertEqual(ca[1], state1)

    def test_litpainter(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas
        state1 = SeqState((9, 6), ArithDelta((4, 5), 9, plus))
        lp = fm.build(LitPainter(cr, state1))
        lp.go(fm, None, None)  # TODO Call fm.go() and let it fill in args?
        # TODO A threshold for painting
        self.assertEqual(ca[1], state1)

    def test_no_duplicate(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas

        state1 = SeqState((9, 6), ArithDelta((4, 5), 9, plus))
        lp1a = fm.build(LitPainter(cr, state1))
        lp1b = fm.build(LitPainter(cr, state1))
        self.assertIs(lp1a, lp1b)

        state2 = SeqState((10, 5), ArithDelta((4, 6), 10, plus))
        lp2 = fm.build(LitPainter(cr, state2))
        self.assertIsNot(lp1a, lp2)
        
    def test_litpainter_antipathy(self):

        pass
        #TODO
        """
        mutual antipathy between LitPainters
        LitPainter done (so don't paint again)
        make a Consume
        make it make a LitPainter

        Detector for 15

        override contents of a Consume
        fill in a Blank?

        query the ws; choose by activation

        query for avails

        filter predicate: Exclude

        tag something
        Blocked as an Agent

        tag GettingCloser

        threshold to really paint

        query the slipnet: hardcoded result
        query the slipnet: for real

        'slip' a Consume: Blank that wants to be filled with avails
        TakeAvailsScout: paint avails on a Consume

        Want creates helpers
        Want gives activation to promising Consumes
        Want.go() returns codelets?

        do a timestep

        "Backtrack": erase a canvas and start over

        (later)
        ImLitPainter: represents a move that we can't do yet because we lack
        avails; not as promising as a LitPainter that can really go

        build a sub-Want
        'slip' a Want?

        Glomming
        NumberLine
        match/similarity measure

        """
