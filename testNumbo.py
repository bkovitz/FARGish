# testNumbo.py -- unit tests for Numbo.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence

from Slipnet import Slipnet, IntFeatures
from FARGish2 import FARGModel, Elem
from Numbo import Numbo, SeqCanvas, SeqState, Want, Consume, Blocked, \
    Detector, AgentSeq, CellRef, SolvedNumble, plus, times, minus
from util import tupdict


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
