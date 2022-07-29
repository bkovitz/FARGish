# test_ajaqb.py -- Unit tests for ajaqb.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from ajaqb import Model, WorkingSoup, same, Subst, Plus, I


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
        print(m.state_str())
        
