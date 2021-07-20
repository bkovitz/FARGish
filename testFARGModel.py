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

from util import pr, pts, is_iter

from FARGModel import FARGModel, Canvas, SeqCanvas, SeqState, StateDelta, \
    CellRef, LitPainter, Operator, Consume, Blocked, RaiseException, \
    AvailDetector, Agent, Want
from Slipnet import Slipnet, IntFeatures, Before, After
from FMTypes import Value, Addr


@dataclass(frozen=True)
class ArithDelta(StateDelta):
    '''A completed arithmetic operation.'''
    before: Sequence
    after: Union[Value, Collection]
    how: Operator

    def seq_str(self):
        expr = f' {self.how} '.join(str(n) for n in self.before)
        return f'{expr} = {self.after}'

class ArithOperator(Operator):
    statedelta_ctor = ArithDelta

plus = ArithOperator(operator.add, '+')
times = ArithOperator(operator.mul, 'x')
minus = ArithOperator(operator.sub, '-')

class TestFound(Exception):
    pass

class TestWant(Agent):

    def go(self, fm: FARGModel, **kwargs):
        # TODO Don't build these if they're already built
        fm.build(
            Detector(self.target, action=RaiseException(SolvedNumble)),
            builder=self
        )
        '''
        fm.build(
            GettingCloser.Tagger(target=self.target),
            builder=self
        )
        '''
        #self.consult_slipnet(fm)
        fm.sleep(self)


class SlipnetWithInt(IntFeatures, Slipnet):
    pass

class TestFM(FARGModel):
    slipnet_ctor = SlipnetWithInt

    def fill_slipnet(self):
        self.slipnet.add_layer2_nodes(
            Consume(operator, (a, b))
                for a in range(1, 21)
                for b in range(1, 11)
                for operator in [plus, times, minus]
                    if a >= b
        )
        self.mut_inh(Before)
        self.mut_inh(After)
        self.mut_inh(int)
        self.mut_inh(Operator)
        self.mut_inh(Consume)

    def mut_inh(self, pred):
        if is_iter(pred):
            nodes1 = nodes2 = pred
        else:
            nodes1 = self.slipnet.qnodes(pred)
            nodes2 = self.slipnet.qnodes(pred)
        for n1 in nodes1:
            for n2 in nodes2:
                if n1 != n2:
                    self.slipnet.add_edge(n1, n2, weight=-0.2)

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
        cr = fm.build(CellRef(ca, 1))  # pointer to 2nd cell on canvas
        # TODO auto-support between CellRef and Canvas? (as in next line)
        #self.assertEqual(fm.ae_weight(cr, ca), fm.mutual_support_weight)
        delta = ArithDelta((4, 5), 9, plus)
        state1 = SeqState((9, 6), delta)
        fm.paint(cr, state1)  # TODO paint_on()?
        self.assertEqual(ca[1], state1)

    def test_litpainter(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas
        state1 = SeqState((9, 6), ArithDelta((4, 5), 9, plus))
        lp = fm.build(LitPainter(cr, state1))
        lp.go(fm)  # TODO Call fm.go() and let it fill in args?
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
        fm = FARGModel()
        self.assertLess(
            fm.mutual_antipathy_weight, 0.0, 
            'mutual_antipathy_weight must be negative'
        )
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr = CellRef(ca, 1)  # pointer to 2nd cell on canvas

        state1 = SeqState((9, 6), ArithDelta((4, 5), 9, plus))
        lp1 = fm.build(LitPainter(cr, state1))
        state2 = SeqState((10, 5), ArithDelta((4, 6), 10, plus))
        lp2 = fm.build(LitPainter(cr, state2))

        self.assertTrue(fm.has_antipathy_to(lp1, lp2))
        self.assertFalse(fm.has_antipathy_to(lp1, lp1))
        self.assertEqual(fm.ae_weight(lp1, lp2), fm.mutual_antipathy_weight)

        #TODO LitPainter done (so don't paint again)

    def test_avail_detector(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = fm.build(CellRef(ca, 1))
        
        # TODO The Detector should only look within ca
        det = fm.build(AvailDetector(9, CellRef, RaiseException(TestFound)))
        det.look(fm)

        fm.paint(cr1, SeqState((6, 9), ArithDelta((4, 5), 9, plus)))
        with self.assertRaises(TestFound):
            for _ in range(40):
                det.look(fm)
        
    def test_consume(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = fm.build(CellRef(ca, 1))

        self.assertCountEqual(cr0.avails, [4, 5, 6])
        self.assertCountEqual(cr1.avails, [])

        co = fm.build(Consume(plus, (4, 5), source=cr0, dest=cr1))
        fm.run(co)
        lp = fm.the(LitPainter)
        self.assertEqual(lp,
            LitPainter(cr1, SeqState((6, 9), ArithDelta((4, 5), 9, plus)))
        )
        self.assertEqual(fm.builder_of(lp), co)
        self.assertEqual(fm.ae_weight(co, lp), fm.mutual_support_weight)
        #TODO UT behalf_of
        self.assertCountEqual(fm.neighbors(co), [lp])
        self.assertEqual(fm.degree(co), 1)

        self.assertCountEqual(cr1.avails, [])
        fm.run(lp)
        self.assertCountEqual(cr1.avails, [6, 9])

    def test_values_not_avail(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)

        # This must fail because there is only one 4 avail
        co = fm.build(Consume(plus, (4, 4), source=cr0, dest=cr1))
        self.assertTrue(fm.can_go(co))
        fm.run(co)
        self.assertIsNone(fm.the(LitPainter))
        # TODO fm.tags_of()
        self.assertTrue(fm.is_tagged(co, Blocked))
        self.assertTrue(fm.is_blocked(co))
        self.assertFalse(fm.can_go(co))
        # TODO assert that co is the builder_of the Blocked, and that there
        # is mutual support between them. This will likely require writing
        # another query function (or extending .tags_of() to find just the
        # Blocked).
        # TODO assert that the Blocked has the right ValuesNotAvail.

    def test_want(self):
        fm = TestFM()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = cr0.next_cellref()
        wa = fm.build(
            Want(target=15, startcell=cr0, sk=RaiseException(TestFound))
        )
        co0 = Consume(plus, (5, 4))
        fm.run(wa, force_slipnet_result=[co0])
        co = fm.the(co0)
        self.assertEqual(co, Consume(plus, (5, 4), source=cr0, dest=cr1))
        self.assertEqual(fm.builder_of(co), wa)
        # NEXT assert that co was built by wa; better yet, query for BuiltBy(wa)

        fm.run(co)
        #pr(fm, edges=True) #DEBUG

        # TODO Somehow the cr1 object needs to get built in the ws so activation
        # can flow through it.
        #cr1a = fm.the(cr1)
        #self.assertEqual(cr1a, cr1)
        
        """
        Blocked: build a Detector for the missing operand, and build an
        agent to scout for avail operands.

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
