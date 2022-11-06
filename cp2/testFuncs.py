# testFuncs.py -- Unit tests for functions, i.e. the third argument of Painters

import unittest
import inspect

from copy import deepcopy

from Model import Model, Painter, Succ, succ, F, I, J, Plus, pred, same, \
    const, Index, Letter, mirror_of, \
    Subst, MakeBetweenPainter, MakeRelativeIndirectPainter, MirrorOf
from Log import lo, trace, set_log_level

class TestFuncs(unittest.TestCase):

    def tearDown(self) -> None:
        set_log_level(0)

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
        self.assertEqual(mirror_of(succ), pred)
        self.assertEqual(mirror_of(pred), succ)
        self.assertEqual(mirror_of(same), same)
        # TODO Mirrors of other kinds of functions. What is the mirror of
        # (I, I+1, 'j')?

    def test_simplify_make_between_painter(self) -> None:
        mbp1 = MakeBetweenPainter(I, J, F)
        su = Subst.make_from((I, Index(1)), (J, Index(3)), (F, succ))
        mbp2 = mbp1.simplify(su)
        self.assertEqual(mbp2, MakeBetweenPainter(Index(1), Index(3), succ))
        # TODO Call apply_func
        self.assertEqual(su.simplify(mbp1), mbp2)

    def test_simplify_make_relative_indirect_painter(self) -> None:
        mrip1 = MakeRelativeIndirectPainter(I, J, F)
        su = Subst.make_from((I, Index(3)), (J, Index(5)), (F, succ))
        mrip2 = mrip1.simplify(su)
        self.assertEqual(
            mrip2,
            MakeRelativeIndirectPainter(Index(3), Index(5), succ)
        )
        self.assertEqual(su.simplify(mrip1), mrip2)

    def test_make_relative_indirect_painter_to_detfuncs(self) -> None:
        m = Model.make_from('ajaqb')
        mrip = MakeRelativeIndirectPainter(I, J, F)
        su = Subst.make_from((I, Index(1)), (J, Index(3)), (F, same))

    def test_call_mirror_of(self) -> None:
        # Is this test necessary? We should never call MirrorOf directly;
        # it should always be simplified into some other function before
        # being called.
        m = Model.make_from('ajaqb')
        mf = MirrorOf(F)
        got = m.apply_func(
            Subst.make_from((F, succ)),
            mf,
            Letter('b')
        )
        self.assertEqual(got, Letter('a'))

    def test_simplify_mirror_of(self) -> None:
        mf = MirrorOf(F)
        su = Subst.make_from((F, pred))
        self.assertEqual(mf.simplify(su), succ)

    def test_mirror_of_to_detfuncs(self) -> None:
        m = Model.make_from('ajaqb')
        mf = MirrorOf(F)
        su = Subst.make_from((F, succ))
        self.assertCountEqual(
            m.func_to_detfuncs(su, F, mf),
            [pred]
        )
