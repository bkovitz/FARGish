# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Model import Canvas1D, succ
from util import short

class TestModel(unittest.TestCase):

    def test_canvas1d_basics(self) -> None:
        c = Canvas1D.make_from('a  ')
        self.assertEqual(str(c), '[a None None]')
        self.assertEqual(short(c), 'a  ')

        c[2] = succ(c[1])
        self.assertEqual(short(c), 'ab ')

