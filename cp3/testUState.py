# testUState.py -- Unit tests for UState

import unittest
import inspect

from Model import FullIndex, I, J, Letter, UState

from Log import lo, set_log_level
from util import pts, reseed, short


class TestUState(unittest.TestCase):
    maxDiff = None

    def test_value_at(self) -> None:
        us = UState.make_from('aj a')
        self.assertEqual(us.value_at(2), Letter('j'))

    def test_loop_through_sourcevar(self) -> None:
        us = UState.make_from('aj a')
        c = us.canvas_in_focus
        self.assertCountEqual(
            us.loop_through_sourcevar(I),
            [
                us.unify(I, FullIndex(c, 1)),
                us.unify(I, FullIndex(c, 2)),
                us.unify(I, FullIndex(c, 4)),
            ]
        )

    def test_loop_through_sourcevar_constant(self) -> None:
        us = UState.make_from('aj a')
        c = us.canvas_in_focus
        us = us.unify(I, FullIndex(c, 2))
        self.assertCountEqual(
            us.loop_through_sourcevar(I),
            [
                us.unify(I, FullIndex(c, 2)),
            ]
        )

    def test_as_canvas_and_index(self) -> None:
        us = UState.make_from('aj a')
        c = us.canvas_in_focus
        us = us.unify(I, 2).unify(J, FullIndex(c, 1))
        self.assertEqual(
            us.as_canvas_and_index(2),
            (c, 2)
        )
        self.assertEqual(
            us.as_canvas_and_index(I),
            (c, 2)
        )
        self.assertEqual(
            us.as_canvas_and_index(J),
            (c, 1)
        )
        # TODO Test for FizzleNoValue?

    def test_loop_through_targetvar_second(self):
        us = UState.make_from('aj a').unify(I, 1)
        c = us.canvas_in_focus
        self.assertCountEqual(
            us.loop_through_targetvar_second(I, J),
            [
                us.unify(J, FullIndex(c, 2)),
                us.unify(J, FullIndex(c, 3)),
                us.unify(J, FullIndex(c, 4)),
            ]
        )

    def test_loop_through_targetvar_first(self) -> None:
        us = UState.make_from('a j a b').unify(J, 6)
        # J=6 means that I should look through 1..5, not including the
        # blank at cell 6.
        c = us.canvas_in_focus
        self.assertCountEqual(
            us.loop_through_targetvar_first(I, J),
            [
                us.unify(I, FullIndex(c, 1)),
                us.unify(I, FullIndex(c, 2)),
                us.unify(I, FullIndex(c, 3)),
                us.unify(I, FullIndex(c, 4)),
                us.unify(I, FullIndex(c, 5))
            ]
        )

