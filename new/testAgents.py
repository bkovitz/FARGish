# testAgents.py -- Unit tests for Agents.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, replace

from Agents import LitPainter, Consumer
from FARGModel import FARGModel, CellRef, Succeeded
from Canvas import Step, StepDelta, StepCanvas
from Equation import plus


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
