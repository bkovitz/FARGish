# test_ajaqb.py -- Unit tests for ajaqb.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from ajaqb import Model, Soup, WorkingSoup, same, succ, Subst, empty_subst, \
    Plus, I, J
from util import pts


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

class TestSoup(unittest.TestCase):

    def test_is_match_1(self) -> None:
        soup = Soup()
        self.assertEqual(
            soup.is_match((I, Plus(I, 2), succ), (1, 3, succ)),
            Subst.make_from((I, 1))
        )

    def test_is_match_2(self) -> None:
        soup = Soup()
        self.assertFalse(soup.is_match((I, Plus(I, 2), succ), (1, 3, same)))
        
class TestModel(unittest.TestCase):

    @unittest.skip('Need to implement Subst.merge() to make this pass')
    def test_indirect_painter(self) -> None:
        m = Model()
        m.paint(WorkingSoup, (1, 3, succ))
        m.run_painter(
            ((I, Plus(I, 2), succ), WorkingSoup, (I, Plus(I, 1), 'q'))
        )
        print(m.state_str())
        # NEXT The I must be bound to the 1; then evaluate (I, Plus(I, 1), 'q').
        # eval_as_detaddr() must return a Subst.
        self.assertTrue(m.ws.has_painter((1, 2, 'q')))

        
    def test_absolute_sponts(self) -> None:
        m = Model()
        self.assertCountEqual(
            m.absolute_spont_painters('a a '),  # blanks should be ignored
            [
                (1, 3, same)
            ]
        )
