# test_util.py -- Unit tests for util.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, InitVar
from types import SimpleNamespace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, runtime_checkable

from util import PushAttr, asdict_with_classvars


class TestUtil(unittest.TestCase):

    def test_with_pushattr(self):
        o = SimpleNamespace()
        o.myattr = 'FIRST'

        with PushAttr(o, 'myattr'):
            o.myattr = 'SECOND'
            self.assertEqual(o.myattr, 'SECOND')
        self.assertEqual(o.myattr, 'FIRST')

    def test_asdict_with_classvars(self):
        @dataclass
        class Blah:
            x: int
            y: ClassVar[str] = 'the classvar'

        blah = Blah(22)
        self.assertEqual(
            asdict_with_classvars(blah),
            {'x': 22, 'y': 'the classvar'}
        )
