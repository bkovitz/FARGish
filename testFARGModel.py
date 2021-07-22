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

from util import pr, pts, is_iter, first

from FARGModel import FARGModel, Canvas, SeqCanvas, SeqState, StateDelta, \
    CellRef, LitPainter, Operator, Consume, Blocked, RaiseException, \
    AvailDetector, Agent, Want, Succeeded, MustCheckIfSucceeded, Active
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

class TestFoundIt(Exception):
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

    # TODO Pass this as a function, possibly one among many, to
    # FARGModel.__init__. Then get rid of TestFM; there should be no need to
    # inherit from FARGModel.
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

        # Did it initialize a random-number seed?
        self.assertIsInstance(fm.seed, int)
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
        self.assertTrue(fm.is_mutual_support(lp, cr))

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

        self.assertTrue(fm.can_go(lp1))
        self.assertTrue(fm.can_go(lp2))

        self.assertFalse(cr.has_value())

        # When lp1 paints, lp2 should be locked out
        fm.run(lp1)
        self.assertEqual(cr.value, state1)
        self.assertTrue(cr.has_value())
        self.assertFalse(fm.can_go(lp2))

    def test_avail_detector(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = fm.build(CellRef(ca, 1))
        
        # TODO The Detector should only look within ca
        det = fm.build(AvailDetector(9, CellRef, RaiseException(TestFoundIt)))
        det.look(fm)

        fm.paint(cr1, SeqState((6, 9), ArithDelta((4, 5), 9, plus)))
        with self.assertRaises(TestFoundIt):
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
        #self.assertCountEqual(fm.neighbors(co), [lp])
        self.assertIn(lp, fm.neighbors(co))
        self.assertIn(cr0, fm.neighbors(co))
        self.assertIn(cr1, fm.neighbors(co))

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
        bl = first(e for e in fm.built_by(co) if isinstance(e, Blocked))
        self.assertIsNotNone(bl)
        self.assertEqual(bl.reason.avails, (4, None))
        self.assertEqual(bl.reason.unavails, (None, 4))
        self.assertGreater(fm.ae_weight(co, bl), 0.0)
        # TODO assert that the Blocked has the right ValuesNotAvail.

    def test_want_w_forced(self):
        fm = TestFM()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = cr0.next_cellref()
        wa = fm.build(
            Want(target=15, startcell=cr0, sk=RaiseException(TestFoundIt)),
            min_a = 4.0
        )
        # TODO assert that cr1 got build as an Elem in the ws?
        self.assertEqual(fm.a(wa), 4.0)

        # The Want should build a Consume
        expected_co = Consume(plus, (5, 4))
        fm.run(wa, force_slipnet_result=[expected_co])
        co = fm.the(expected_co)
        self.assertEqual(co, Consume(plus, (5, 4), source=cr0, dest=cr1))
        self.assertEqual(fm.builder_of(co), wa)
        self.assertIn(co, fm.built_by(wa))
        self.assertTrue(fm.can_go(co))

        # The Consume should build a LitPainter
        fm.run(co)
        expected_value = SeqState((6, 9), ArithDelta((5, 4), 9, plus))
        lp = fm.the(LitPainter)
        self.assertEqual(lp, LitPainter(cr1, expected_value))
        self.assertTrue(fm.is_mutual_support(co, lp))
        self.assertFalse(fm.can_go(co))

        # The LitPainter shouldn't paint yet: activation is too low
        self.assertFalse(fm.ok_to_paint(lp, cr1))
        self.assertFalse(fm.can_go(lp))

        # Eventually, the LitPainter should be able to paint
        fm.propagate_a(num=20)
        self.assertTrue(fm.ok_to_paint(lp, cr1))
        self.assertTrue(fm.can_go(lp))
        self.assertEqual(fm.the(cr1), cr1)

        # When the LitPainter paints, it should be marked Succeeded
        fm.run(lp)
        self.assertEqual(fm.agent_state(lp), Succeeded())
        self.assertTrue(fm.has_succeeded(lp))
        self.assertFalse(fm.can_go(lp))

        # The Consume should be marked MustCheckIfSucceeded
        self.assertIsInstance(fm.agent_state(co), MustCheckIfSucceeded)

        # Running the Consume should make it see that it has succeeded
        fm.run(co)
        self.assertTrue(fm.has_succeeded(co))
        self.assertFalse(fm.can_go(co))
        self.assertIsInstance(fm.agent_state(wa), MustCheckIfSucceeded)

        # Running the Want should make it see that it has still not succeeded
        fm.run(wa)
        self.assertIsInstance(fm.agent_state(wa), Active)
        self.assertFalse(fm.has_succeeded(wa))

    def test_timesteps(self):
        # Little more than a smoke test: start with Want and a SeqCanvas,
        # run a few timesteps, and verify that there are some more Elems.
        fm = TestFM()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        self.assertEqual(len(fm), 1)
        cr0 = CellRef(ca, 0)
        wa = fm.build(
            Want(target=15, startcell=cr0, sk=RaiseException(TestFoundIt)),
            min_a = 4.0
        )
        fm.do_timestep(until=10)
        #pr(fm, edges=True, seed=True, extra=True) #DEBUG
        self.assertEqual(fm.t, 10)
        self.assertGreaterEqual(len(fm), 10)


        
        """
        MIN PATH TO DEMOABLE AGAIN

        NEXT
        do a timestep
            query the ws; choose by activation

        Allow AwaitingDelegate to take multiple delegates

        Make a runner or something in __main__ so I can watch the action
        starting from a Want.


        give_boost on success

        Blocked: build a Detector for the missing operand, and build an
        agent to scout for avail operands.

        Want gives support to promising LitPainters, Consumes
        
        tag GettingCloser

        'slip' a Consume: Blank that wants to be filled with avails
        TakeAvailsScout: paint avails on a Consume

        "Backtrack": erase a canvas and start over


        TODO
        Replace state-setting methods with .set_agent_state() only.

        Specify Slipnet features func in FARGModel ctor.

        some unit tests to verify that the slipnet is returning reasonable
        Consumes

        override contents of a Consume
        fill in a Blank?

        query for avails

        filter predicate: BuiltBy

        tag something
        Blocked as an Agent

        Want creates helpers
        Want gives activation to promising Consumes
        Want.go() returns codelets?

        a Promisingness scout

        (later)
        A FARGModel method to mark an object as a delegate, or maybe pass
        that information when building it.

        Instead of MustCheckIfSucceeded, build a Detector that gets a boost
        when a delegate succeeds.

        When an elem is unhappy, query the slipnet for an Agent whose after is
        MadeHappy. Having a Blocked tag counts as unhappy.

        A Cell or CellRef should store the threshold to paint on it. Possibly
        a minimum activation level for the painter, and/or 'the painter clearly
        beats its competition.'

        ImLitPainter: represents a move that we can't do yet because we lack
        avails; not as promising as a LitPainter that can really go

        build a sub-Want
        'slip' a Want?

        Glomming
        NumberLine
        match/similarity measure

        """
