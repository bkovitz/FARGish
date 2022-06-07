# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Model import Model, Canvas1D, succ, DeterminateAddress, RPainter
from util import short

class TestModel(unittest.TestCase):

    def test_canvas1d_basics(self) -> None:
        c = Canvas1D.make_from('a  ')
        self.assertEqual(c[1], 'a')
        self.assertEqual(c[2], None)
        self.assertEqual(str(c), '[a None None]')
        self.assertEqual(short(c), 'a  ')

        c[2] = succ(c[1])
        self.assertEqual(short(c), 'ab ')

        self.assertEqual(c.addr_of('b'), DeterminateAddress(c, 2))

    def test_simple_run_painter(self) -> None:
        p = (1, 2, succ)
        m = Model.make_from('a  ')
        m.run_painter(p)
        self.assertEqual(short(m), 'ab ')

    def test_simple_rpainter(self) -> None:
        p = RPainter.make('a', 1, succ)   # TODO  1 -> right1
        m = Model.make_from(' a ')
        m.run_painter(p)
        self.assertEqual(short(m), ' ab')
