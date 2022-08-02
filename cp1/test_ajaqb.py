# test_ajaqb.py -- Unit tests for ajaqb.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from ajaqb import Model, WorkingSoup, same, succ, Subst, Plus, I


class TestAjaqb(unittest.TestCase):

    def test_subst_empty(self) -> None:
        su = Subst()
        self.assertEqual(su.eval_as_index(I), None)
        # TODO Make this next assertion pass
        #self.assertEqual(su.eval_as_index(Plus(I, 2)), Plus(I, 2))

    def test_subst_I(self) -> None:
        su = Subst().unify(I, 1)
        self.assertEqual(su.eval_as_index(I), 1)
        self.assertEqual(su.eval_as_index(Plus(I, 2)), 3)

    def test_plus_simplify(self) -> None:
        m = Model()
        m.absorb('ajaqb')
        m.set_canvas('a    ')
        m.run_painter(('a', WorkingSoup, (I, Plus(I, 2), same)))
        self.assertTrue(m.ws.has_painter((1, 3, same)))
        #print(m.state_str())

    def test_subst_merge(self) -> None:
        su0 = Subst()
        sui = Subst.make_from((I, 1))
        self.assertEqual(sui, Subst().unify(I, 1))
        self.assertEqual(sui.eval_as_index(I), 1)

        su1 = sui.merge(su0)
        self.assertEqual(su1, sui)

        su2 = su0.merge(sui)
        self.assertEqual(su2, su1)

    @unittest.skip('Need to implement Subst.merge() to make this pass')
    def test_indirect_qpainter(self) -> None:
        m = Model()
        m.paint(WorkingSoup, (1, 3, succ))
        m.run_painter(
            ((I, Plus(I, 2), succ), WorkingSoup, (I, Plus(I, 1), 'q'))
        )
        print(m.state_str())
        # NEXT The I must be bound to the 1; then evaluate (I, Plus(I, 1), 'q').
        # eval_as_detaddr() must return a Subst.
        self.assertTrue(m.ws.has_painter((1, 2, 'q')))

        
