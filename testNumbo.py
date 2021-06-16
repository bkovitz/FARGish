# testNumbo.py -- unit tests for Numbo.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import os
import csv

from Slipnet import Slipnet, IntFeatures
from FARGish2 import FARGModel, Elem, NoGo, ImCell
from Numbo import Numbo, SeqCanvas, SeqState, Want, Consume, Blocked, \
    Detector, AgentSeq, CellRef, SolvedNumble, plus, times, minus
from util import tupdict, pr, pts


class TestNumbo(unittest.TestCase):

    def test_numbo_smoke_test(self):
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        fm.do_timestep(num=10)

    def test_is_blocked(self):
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        co1 = fm.build(
            Consume(operator=plus, operands=(9, 6), source=CellRef(ca, 0))
        )

        self.assertFalse(fm.is_blocked(co1))
        co1.go(fm)
        print(fm)

        self.assertTrue(fm.is_blocked(co1))

    def test_mut_antipathy_and_support(self):
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        co1 = fm.build(
            Consume(operator=plus, operands=(5, 4), source=CellRef(ca, 0))
        )
        co2 = fm.build(
            Consume(operator=plus, operands=(6, 4), source=CellRef(ca, 0))
        )
        w12 = fm.ae_weight(co1, co2)
        w21 = fm.ae_weight(co2, co1)
        self.assertLess(w12, 0.0)
        self.assertEqual(w12, w21)

        wcr = fm.ae_weight(co1, CellRef(ca, 0))
        wrc = fm.ae_weight(CellRef(ca, 0), co1)
        self.assertGreater(wcr, 0.0)
        self.assertEqual(wcr, wrc)

    def test_log_activations(self):
        fm = Numbo(seed=1)
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        fm.do_timestep(num=40)
        # TODO Check the a.csv file

    def test_as_fmpred(self):
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        co1 = fm.build(
            Consume(operator=plus, operands=(5, 4), source=CellRef(ca, 0))
        )
        co2 = fm.build(
            Consume(operator=plus, operands=(6, 4), source=CellRef(ca, 0))
        )
        fm.do_timestep(ag=co1)
        fm.do_timestep(ag=co2)

        # None = match everything
        es = list(fm.elems())
        self.assertCountEqual(fm.elems(None), es)
        pts(es)

        # A class = all elems of that class
        self.assertCountEqual(fm.elems(Consume), [co1, co2])

        # A tuple of classes = match all classes in tuple
        imcells = list(fm.elems(ImCell))
        assert(len(imcells) == 2)
        self.assertCountEqual(fm.elems((Consume, ImCell)), [co1, co2] + imcells)

        # A func with first argument annotated as FARGModel
        co1_a = fm.a(co1)
        def pred1(fm: FARGModel, o: Hashable) -> bool:
            return fm.a(o) == co1_a
        self.assertCountEqual(fm.elems(pred1), [co1])

        # A func with only one argument
        def pred2(o: Hashable) -> bool:
            try:
                return 6 in o.operands
            except AttributeError:
                return False
        self.assertCountEqual(fm.elems(pred2), [co2])

        # An object with some but not all of its members filled in
        self.assertCountEqual(fm.elems(Consume(operands=(5, 4))), [co1])

        # A tuple with an object and a class
        self.assertCountEqual(
            fm.elems((Consume(operands=(6, 4)), ImCell)),
            imcells + [co2]
        )

    def test_logpred(self):
        os.unlink('logpred.csv')
        fm = Numbo(logpred=Consume, alog='logpred.csv')
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        fm.do_timestep(num=20)

        with open('logpred.csv', mode='r') as f:
            reader = csv.reader(f, quoting=csv.QUOTE_NONNUMERIC)
            for row in reader:
                t, node, a = row
                self.assertTrue(node.startswith('Consume'))


    def test_winning_consume_attracts_support(self):
        fm = Numbo(
            seed=1886246070452261567,
            #mutual_antipathy_weight=-0.2,
            #mutual_support_weight=5.0
            positive_feedback_rate=2.0,
            logpred=(Want, Consume(operands=(9, 6)))
        )
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        co1 = fm.build(Consume(operands=(5, 4), operator=plus, source=cr0))
        co2 = fm.build(Consume(operands=(9, 6), operator=plus, source=cr1))

        co3 = fm.build(Consume(operands=(9, 6), operator=times, source=cr1))
        co4 = fm.build(Consume(operands=(9, 6), operator=minus, source=cr1))

        fm.do_timestep(co1, act=True)
        print('UT', cr1.contents, type(cr1.contents))
        self.assertCountEqual(cr1.contents.avails, (6, 9))
        self.assertTrue(fm.is_tagged(co1, NoGo))

        fm.do_timestep(co2)
        fm.do_timestep(co3)
        fm.do_timestep(co4)
        fm.do_timestep(until=9)
        print(fm)
        pr(fm, (Want, Consume, ImCell), edges=True)
        #pts(sorted(fm.elems(Consume), key=fm.a, reverse=True))
        pr(fm, Consume, edges=True)
        #print(fm.seed)


    @unittest.skip('On hold until we can force what Want builds.')
    def test_hardcoded_pons_asinorum(self):
        fm = Numbo()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        wa.go(fm) # Builds Consume objects and Detector

        for co in fm.ws_query(Consume, builder=wa):
            co.go(fm)

        bl = fm.ws_query1(Blocked)
        bl.go(fm)

        # TODO We need a way, in a test, to force Want(15) to build
        # this Detector.
        d9 = fm.ws_query1(Detector, target=9)
        d9.go(fm)

        co1 = fm.ws_query1(Consume, operands=(4, 5))
        co2 = fm.ws_query1(Consume, operands=(9, 6))
        aseq0 = fm.build(
            AgentSeq(
                (co1, co2),
                initial_kwargs=tupdict(
                    source=CellRef(ca, 0),
                    dest=CellRef(ca, 1)
                )
            )
        )
        aseq0.go(fm)  #This should not complain

        aseq = fm.ws_query1(AgentSeq)
        aseq.act(fm)

        d15 = fm.ws_query1(Detector, target=15)
        try:
            d15.go(fm)
            self.fail('Detector did not detect solution.')
        except SolvedNumble as exc:
            self.assertEqual(
                str(exc),
                '4 + 5 = 9; 9 + 6 = 15'
            )

        #print(fm)
