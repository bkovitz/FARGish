# testAgents.py -- Unit tests for Agents.py

import unittest
from pprint import pprint as pp
import inspect

from Agents import LitPainter, Consumer, Want
from FARGModel import FARGModel, CellRef, Succeeded
from Canvas import Step, StepDelta, StepCanvas
from Equation import plus
from Graph import Graph
from Slipnet import Slipnet

from util import pr, pts, trace


class TestAgents(unittest.TestCase):

    step0 = Step((4, 5, 6))
    step1 = Step((6, 9), StepDelta((4, 5), 9, plus))

    def pons_start_canvas(self) -> StepCanvas:
        return StepCanvas([self.step0])

    def test_litpainter(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr1 = CellRef(ca, 1)

        lp = fm.build(LitPainter(dest=cr1, value=self.step1))
        fm.run_agent(lp)
        self.assertEqual(ca[1], self.step1)
        self.assertEqual(fm.agent_state(lp), Succeeded)

    def test_consumer(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)

        ag = fm.build(Consumer(
            operator=plus,
            operands=(4, 5),
            source=cr0,
            dest=cr1)
        )
        fm.run_agent(ag)
        fm.run_agent(fm.the(LitPainter))
        
        self.assertEqual(ca[1], self.step1)

    @unittest.skip('NEXT: QuerySlipnetForDelegate should try to fill in the Nones in the Consumer.')
    def test_want(self) -> None:
        slipnet = Slipnet(Graph.with_features([
            Consumer.make(plus, (5, 4))
        ]))
        fm = FARGModel(slipnet=slipnet)
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        wa = fm.build(Want(startcell=cr0, target=9))

        fm.run_agent(wa)
        co = fm.the(Consumer)
        self.assertEqual(fm.builder_of(co), wa)
        pr(fm)
        fm.run_agent(co)
