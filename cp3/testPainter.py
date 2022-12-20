# testPainter.py -- Unit tests for painters


import unittest
import inspect

from Model import Apart, FullIndex, I, J, Letter, \
    Paint, PaintAt, PaintValue, Painter, UState, Succ

from Log import lo, set_log_level
from util import pts, reseed, short


class TestPainter(unittest.TestCase):

    p3 = Painter((I, J), (Apart(2, I, J), Succ(I, J)))

    def test_p3_left_to_right(self) -> None:
        us = UState.make_from('bj b')
        c = us.canvas_in_focus

        self.assertCountEqual(
            self.p3.generate_actions_left_to_right(
                us.unify(I, FullIndex(c, 2))   # us.unify(I, 2) would be more convenient
            ),
            [
                Paint(FullIndex(c, 4), Letter('k'))
            ]
        )

    def test_p3_right_to_left(self) -> None:
        us = UState.make_from('bj b')
        c = us.canvas_in_focus

        self.assertCountEqual(
            self.p3.generate_actions_right_to_left(
                us.unify(J, FullIndex(c, 4))   # us.unify(J, 4) would be more convenient
            ),
            [
                Paint(FullIndex(c, 2), Letter('a'))
            ]
        )

    def test_p3(self) -> None:
        us = UState.make_from('  j  ')
        c = us.canvas_in_focus

        self.assertCountEqual(
            self.p3.generate_actions(us),
            [
                Paint(FullIndex(c, 1), Letter('i')),
                Paint(FullIndex(c, 5), Letter('k'))
            ]
        )

    #def test_p1(self) -> None:
        

#    def test_see_related_pair(self) -> None:
#        us = UState.make_from('ajaqb')
#        c = us.canvas_in_focus
#
#        self.assertCountEqual(
#            self.ip1.generate_actions(us),
#            [
#                MakePainter(
#                    Painter((I, J), (Apart(2, I, J), Same(I, J))),
#                    anchored_at=((FullIndex(c, 1), FullIndex(c, 3)))
#                ),
#                MakePainter(
#                    Painter((I, J), (Apart(2, I, J), Succ(I, J)))),
#                    anchored_at=((FullIndex(c, 3), FullIndex(c, 5)))
#                ),
#                MakePainter(
#                    Painter((I, J), (Apart(2, I, J), Succ(I, J)))),
#                    anchored_at=((FullIndex(c, 1), FullIndex(c, 5)))
#                ),
#            ]
#        )
