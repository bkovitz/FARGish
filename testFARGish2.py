# testFARGish2.py -- Unit tests for FARGish2.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence

from FARGish2 import FARGModel, ActivationGraph
    #, SeqCanvas, SeqState, Want, Consume, Blocked, \
    #Detector, AgentSeq, CellRef, SolvedNumble
from util import tupdict, pts


class TestFARGish2(unittest.TestCase):

    def test_global_params(self):
        fm = FARGModel(seed=1)
        self.assertEqual(fm.seed, 1)
        self.assertTrue(isinstance(fm.activation_g, ActivationGraph))

        fm = FARGModel(aprop=dict(alpha=0.92))
        self.assertEqual(fm.activation_g.propagator.alpha, 0.92)
        #print('UT', fm.globals)

    @unittest.skip('obsolete; see testNumbo.py')
    def test_hardcoded_pons_asinorum(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        wa.go(fm) # Builds Consume objects and Detector

        for co in fm.ws_query(Consume, builder=wa):
            co.go(fm)

        bl = fm.ws_query1(Blocked)
        bl.go(fm)

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
