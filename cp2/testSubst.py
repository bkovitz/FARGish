# testSubst.py -- Unit tests.for Subst.py

import unittest
import inspect

from Subst import Subst, Plus
from Model import Model
from Types import I, J, WorkingSoup

class TestSubst(unittest.TestCase):

    def test_subst_empty(self) -> None:
        su = Subst()
        self.assertEqual(su.simplify(I), None)
        # TODO Make this next assertion pass
        #self.assertEqual(su.simplify(Plus(I, 2)), Plus(I, 2))

    def test_subst_I(self) -> None:
        su = Subst().unify(I, 1)
        self.assertEqual(su.simplify(I), 1)
        self.assertEqual(su.simplify(Plus(I, 2)), 3)

    '''
    def test_match_a(self) -> None:
        m = Model()
        m.set_canvas(' a   ')
        subst, ii = m.eval_as_detaddr(empty_subst, 'a')
        self.assertEqual(subst, empty_subst)
        self.assertEqual(ii, 2)
        
    def test_plus_in_target(self) -> None:
        m = Model()
        m.absorb('ajaqb')
        m.set_canvas('a    ')
        m.run_painter(('a', WorkingSoup, (I, Plus(I, 2), same)))
        print(m.state_str())
        self.assertTrue(m.ws.has_painter((1, 3, same)))
    '''

    def test_subst_merge(self) -> None:
        su0 = Subst()
        sui = Subst.make_from((I, 1))
        self.assertEqual(sui, Subst().unify(I, 1))
        self.assertEqual(sui.simplify(I), 1)

        su1 = sui.merge(su0)
        self.assertEqual(su1, sui)

        su2 = su0.merge(sui)
        self.assertEqual(su2, su1)

    def test_simplify(self) -> None:
        su = Subst.make_from((I, 1), (J, I))
        self.assertEqual(su.simplify(3), 3)
        self.assertEqual(su.simplify(I), 1)
        self.assertEqual(su.simplify(J), 1)
