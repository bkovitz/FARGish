# testSubst.py -- Unit tests.for Subst.py

import unittest
import inspect

from Model import Model, Index, Painter, \
    F, I, Indices, J, SR, \
    Subst, Plus, bottom_subst, empty_subst, \
    succ
from Log import lo, set_log_level

class TestSubst(unittest.TestCase):

    def tearDown(self) -> None:
        set_log_level(0)

    def test_subst_empty(self) -> None:
        su = Subst()
        self.assertEqual(su.simplify(I), I)
        self.assertEqual(su.simplify(Plus(I, 2)), Plus(I, 2))

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
        m.run_painter(('a', SR.WorkingSoup, (I, Plus(I, 2), same)))
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
        self.assertEqual(su.simplify(Index(3)), Index(3))
        self.assertEqual(su.simplify(I), Index(1))
        self.assertEqual(su.simplify(J), Index(1))

    def test_unify_integers(self) -> None:
        su = Subst().unify(1, 1)
        self.assertEqual(su, empty_subst)

        su = Subst().unify(1, 2)
        self.assertEqual(su, bottom_subst)

    def test_unify_indices(self) -> None:
        su = Subst().unify(Indices(1, 2), Indices(1, 2))
        self.assertEqual(su, empty_subst)

        su = Subst().unify(Indices(2, 1), Indices(1, 2))
        self.assertEqual(su, empty_subst)

        su = Subst().unify(Indices(1, 2), Indices(1, 3))
        self.assertEqual(su, bottom_subst)

        su = Subst().unify(1, Indices(1))
        self.assertEqual(su, empty_subst)

        su = Subst().unify(Indices(1), 1)
        self.assertEqual(su, empty_subst)

        su = Subst().unify(I, Indices(1, 2))
        self.assertEqual(su.simplify(I), Indices(1, 2))


        # TODO
        #su = Subst().unify(I, Indices(Plus(J, 2), 7)).unify(J, 3)
        #self.assertEqual(su.simplify(I, Indices(5, 7)))

    def test_unify_painters(self) -> None:
        su = Subst().unify(Painter(I, J, F), Painter(Index(3), Index(5), succ))
        self.assertEqual(su.simplify(I), 3)
        self.assertEqual(su.simplify(J), 5)
        self.assertEqual(su.simplify(F), succ)
        
    def test_unify_variables(self) -> None:
        su = Subst().unify(I, J)
        self.assertEqual(su.simplify(I), J)

        su = Subst().unify(I, J).unify(J, 1)
        self.assertEqual(su.simplify(I), 1)

        su = Subst().unify(I, 1).unify(J, 2).unify(I, J)
        self.assertEqual(su, bottom_subst)

        su = Subst().unify(I, J).unify(I, 1).unify(J, 2)
        self.assertEqual(su, bottom_subst)

    def test_unify_gratuitous_plus(self) -> None:
        su = Subst().unify(I, Plus(I))
        self.assertEqual(su.simplify(I), I)
        self.assertEqual(su.simplify(Plus(I)), I)

        su = Subst().unify(Plus(I), 8)
        self.assertEqual(su.simplify(I), 8)

        su = Subst().unify(I, 2).unify(I, Plus(I))
        self.assertEqual(su.simplify(I), 2)
        self.assertEqual(su.simplify(Plus(I)), 2)

    def test_unify_plus_plus(self) -> None:
        su = Subst().unify(Plus(I, 2), Plus(J, -2))
        self.assertEqual(su.simplify(I), I)
        self.assertEqual(su.simplify(J), J)
        self.assertEqual(su.simplify(Plus(I, 2)), Plus(I, 2))
        self.assertEqual(su.simplify(Plus(J, -2)), Plus(J, -2))

        su = su.unify(I, 3)
        self.assertEqual(su.simplify(Plus(I, 2)), 5)
        self.assertEqual(su.simplify(J), 7)

    def test_unify_painters_with_forward_reference(self) -> None:
        su = Subst().unify(
            Painter(Plus(J, -2), J, F),
            Painter(Index(3), Index(5), succ)
        )
        self.assertEqual(su.simplify(I), 3)
        self.assertEqual(su.simplify(J), 5)
        self.assertEqual(su.simplify(F), succ)

    def test_unify_plus_with_soup(self) -> None:
        su = Subst().unify(Plus(I, 2), SR.WorkingSoup) # You can't do that
        self.assertEqual(su, bottom_subst)

    def test_unify_i_with_painter(self) -> None:
        su = Subst().unify(I, (I, Plus(I, 2), succ))  # You can't do that
        self.assertEqual(su, bottom_subst)
