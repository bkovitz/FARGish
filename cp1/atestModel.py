# atestModel.py -- Acceptance tests for Model.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Model import Model, Canvas1D, same, succ, pred, DeterminateAddress, \
    RPainter, FizzleValueNotFound
from util import short


class TestModel(unittest.TestCase):

    def test_abc(self) -> None:
        m = Model.make_from('abc')
        ps = list(m.make_spont())
        m.set_canvas('a  ')
        m.regenerate(ps, vv=0)
        self.assertEqual(short(m.canvas), 'abc')
        self.assertTrue(all(cl >= 3 for cl in m.canvas.all_clarities()))
