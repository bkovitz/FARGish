# testCanvas.py -- Unit tests for Canvas.py

import unittest
from pprint import pprint as pp
import inspect

from Canvas import CellRef, Step, StepDelta, ValuesNotAvail, StepCanvas
from util import as_iter


class TestStepsCanvas(unittest.TestCase):

    def test_step_take_avails(self) -> None:
        step = Step([4, 5, 6])
        taken, remaining = step.take_avails([4, 5])
        self.assertCountEqual(taken, [4, 5])
        self.assertCountEqual(remaining, [6])
        self.assertEqual(step, Step([4, 5, 6]))

        self.assertTrue(step.has_avail_value(4))
        self.assertFalse(step.has_avail_value(7))

    def test_step_take_avails_fail(self) -> None:
        step = Step([4, 5, 6])
        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = step.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(None, (4, None), (None, 7))
        )

    def test_stepcanvas_basics(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        self.assertEqual(ca[0], Step([4, 5, 6]))
        self.assertEqual(ca[1], None)
        self.assertEqual(ca[10], None)
        self.assertEqual(len(ca), 1)

        ca[1] = Step([6], StepDelta([4, 5], [9], '+'))
        self.assertEqual(ca[1], Step([6], StepDelta([4, 5], [9], '+')))
        self.assertEqual(len(ca), 2)

    def test_cellref_basics(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)

        # StepCanvas[0]
        self.assertEqual(cr0.value, Step([4, 5, 6]))
        self.assertCountEqual(as_iter(cr0.avails), [4, 5, 6])
        taken, remaining = cr0.take_avails([4, 5])
        self.assertCountEqual(taken, [4, 5])
        self.assertCountEqual(remaining, [6])
        self.assertEqual(cr0.value, Step([4, 5, 6]))
        self.assertTrue(cr0.has_a_value())

        self.assertTrue(cr0.has_avail_value(4))
        self.assertFalse(cr0.has_avail_value(7))

        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = cr0.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(cr0, (4, None), (None, 7))
        )

        #StepCanvas[1]
        self.assertEqual(cr1.value, None)
        self.assertFalse(cr1.has_a_value())
        self.assertCountEqual(as_iter(cr1.avails), [])
        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = cr1.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(cr1, (4, 7), ())
        )

        # paint
        cr1.paint(Step([6], StepDelta([4, 5], [9], '+')))
        self.assertEqual(ca[1], Step([6], StepDelta([4, 5], [9], '+')))
