# test_ajaqb.py -- Unit tests for ajaqb.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from ajaqb import Subst, Plus, I


class TestAjaqb(unittest.TestCase):

    def test_subst_empty(self) -> None:
        su = Subst()
        self.assertEqual(su.value_of(I), None)
        # TODO Make this next assertion pass
        #self.assertEqual(su.value_of(Plus(I, 2)), Plus(I, 2))

    def test_subst_I(self) -> None:
        su = Subst().unify(I, 1)
        self.assertEqual(su.value_of(I), 1)
        self.assertEqual(su.value_of(Plus(I, 2)), 3)
