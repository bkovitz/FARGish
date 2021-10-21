# testQArgs.py -- Unit tests for QArgs.py

import unittest
from pprint import pprint as pp
import inspect

from QArgs import QBeforeFromAvails, QAfter, SearchFor
from Agents import LitPainter, Consumer
from FARGModel import FARGModel, CellRef
from Canvas import Step, StepDelta, StepCanvas
from Equation import Equation, plus
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
            as_iter(qarg.get_items(fm, [])),
            [
                Before(4),
                Before(5),
                Before(6)
            ]
        )

    def test_qafter(self) -> None:
        fm = FARGModel()
        qarg = QAfter()
        self.assertCountEqual(
            as_iter(qarg.get_items(fm, {'features': 15})),
            [
                After(15)
            ]
        )

    def test_slipnet_kwargs1(self) -> None:
        fm = FARGModel()
        qargs = (
            Before(4),
            After(11),
            SearchFor(Equation)
        )
        self.assertEqual(
            fm.mk_slipnet_args(qargs, []),
            dict(
                activations_in=dict((
                    (Before(4), 1.0),
                    (After(11), 1.0)
                )),
                pred=(Equation,),
                k=20,
                num_get=1
            )
        )

    def test_slipnet_kwargs2(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)

        qargs = (
            QBeforeFromAvails(),
            QAfter(),
            SearchFor()
        )
        sources = dict(
            source=cr0,
            features=15,
            type=Equation
        )
        self.assertEqual(
            fm.mk_slipnet_args(qargs, sources),
            dict(
                activations_in=dict((
                    (Before(4), 1.0),
                    (Before(5), 1.0),
                    (Before(6), 1.0),
                    (After(15), 1.0)
                )),
                pred=(Equation,),
                k=20,
                num_get=1
            )
        )
        # TODO again, with QInputs
