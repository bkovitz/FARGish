# testQArgs.py -- Unit tests for QArgs.py

import unittest
from pprint import pprint as pp
import inspect

from QArgs import QBeforeFromAvails, QAfter
from Agents import LitPainter, Consumer
from FARGModel import FARGModel, CellRef
from Canvas import Step, StepDelta, StepCanvas
from Equation import plus
from Graph import Before, After
from util import pr, as_iter


class TestQArgs(unittest.TestCase):

    step0 = Step((4, 5, 6))
    step1 = Step((6, 9), StepDelta((4, 5), 9, plus))

    def pons_start_canvas(self) -> StepCanvas:
        return StepCanvas([self.step0])

    def test_before_from_avails(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)

        qarg = QBeforeFromAvails(source=cr0)
        self.assertCountEqual(
            as_iter(fm.qarg_items(qarg, [])),
            [
                Before(4),
                Before(5),
                Before(6)
            ]
        )

    def test_qafter(self) -> None:
        fm = FARGModel()
        qarg = QAfter(features=15)
        self.assertCountEqual(
            as_iter(fm.qarg_items(qarg, {'features': 15})),
            [
                After(15)
            ]
        )
