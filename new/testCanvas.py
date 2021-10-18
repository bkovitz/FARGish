# testCanvas.py -- Unit tests for Canvas.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field

from Canvas import Step, ValuesNotAvail

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
