# testPredicates.py -- Unit tests for predicate classes

import unittest
import inspect

from Model import Apart, FullIndex, I, J, Letter, PaintAt, PaintValue, \
    UState, Succ

from Log import lo, set_log_level
from util import pts, reseed, short


class TestApart(unittest.TestCase):

    def test_target_ok(self) -> None:
        us = UState.make_from('aj a')
        us12 = us.unify(I, 1).unify(J, 2)
        us13 = us.unify(I, 1).unify(J, 3)
        apart2 = Apart(2, I, J)
        self.assertFalse(apart2.target_ok(us12, I, J))
        #self.assertFalse(apart2.target_ok(us12, 1, 2))
        self.assertTrue(apart2.target_ok(us13, I, J))
        #self.assertTrue(apart2.target_ok(us13, 1, 3))

    def test_spec(self) -> None:
        us = UState.make_from('aj a').unify(I, 2).unify(J, 4)
        apart2 = Apart(2, I, J)
        self.assertCountEqual(
            apart2.spec_left_to_right(us, I, J),
            [PaintAt(FullIndex(us.canvas_in_focus, 4))]
        )
        self.assertCountEqual(
            apart2.spec_right_to_left(us, I, J),
            [PaintAt(FullIndex(us.canvas_in_focus, 2))]
        )

class TestSucc(unittest.TestCase):

    def test_spec(self) -> None:
        us = UState.make_from('bj b').unify(I, 2).unify(J, 4)
        succ = Succ(I, J)
        self.assertCountEqual(
            succ.spec_left_to_right(us, I, J),
            [PaintValue(Letter('k'))]
        )
        self.assertCountEqual(
            succ.spec_right_to_left(us, I, J),
            [PaintValue(Letter('a'))]
        )
