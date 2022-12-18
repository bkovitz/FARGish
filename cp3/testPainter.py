# testPainter.py -- Unit tests for painters


import unittest
import inspect

from Model import Apart, FullIndex, I, J, Letter, \
    Paint, PaintAt, PaintValue, Painter, UState, Succ

from Log import lo, set_log_level
from util import pts, reseed, short


class TestPainter(unittest.TestCase):

    def test_p3_left_to_right(self) -> None:
        us = UState.make_from('bj b')
        c = us.canvas_in_focus
        p3 = Painter((I, J), (Apart(2, I, J), Succ(I, J)))

        self.assertCountEqual(
            p3.generate_actions_left_to_right(
                us.unify(I, FullIndex(c, 2))   # us.unify(I, 2) would be more convenient
            ),
            [
                Paint(FullIndex(c, 4), Letter('k'))
            ]
        )

    def test_p3_right_to_left(self) -> None:
        us = UState.make_from('bj b')
        c = us.canvas_in_focus
        p3 = Painter((I, J), (Apart(2, I, J), Succ(I, J)))

        self.assertCountEqual(
            p3.generate_actions_right_to_left(
                us.unify(J, FullIndex(c, 4))   # us.unify(J, 4) would be more convenient
            ),
            [
                Paint(FullIndex(c, 2), Letter('a'))
            ]
        )
