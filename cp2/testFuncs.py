# testFuncs.py -- Unit tests for functions, i.e. the third argument of Painters

import unittest
import inspect

from copy import deepcopy

from Model import Model, Painter, Succ, succ, F, I, Plus, pred, same, const, \
    MirrorOf, Subst

class TestFuncs(unittest.TestCase):

    def test_succ_eq(self) -> None:
        succ2 = deepcopy(succ)
        self.assertEqual(succ, succ2)

        p1 = Painter(I, Plus(I, 2), Succ())
        p2 = deepcopy(p1)  #Painter(I, Plus(I, 2), Succ())
        self.assertEqual(p1, p1)
        self.assertEqual(p1, p2)

    def test_pred_eq(self) -> None:
        pred2 = deepcopy(pred)
        self.assertEqual(pred, pred2)

    def test_same_eq(self) -> None:
        same2 = deepcopy(same)
        self.assertEqual(same, same2)

    def test_const_eq(self) -> None:
        const1a = const(1)
        const1b = deepcopy(const1a)
        self.assertEqual(const1a, const1b)
        const2 = const(2)
        self.assertNotEqual(const1a, const2)

    def test_mirror_of(self) -> None:
        m = Model.make_from('ajaqb')
        self.assertEqual(m.mirror_of(succ), pred)
        self.assertEqual(m.mirror_of(pred), succ)
        self.assertEqual(m.mirror_of(same), same)
        # TODO Mirrors of other kinds of functions. What is the mirror of
        # (I, I+1, 'j')?

    def test_Mirror_of(self) -> None:
        m = Model.make_from('ajaqb')
        mf = MirrorOf(F)
        got = m.apply_func(
            Subst.make_from((F, succ)),
            mf,
            'b'
        )
        self.assertEqual(got, 'a')
