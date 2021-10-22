# test_util.py -- Unit tests for util.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, InitVar
from types import SimpleNamespace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, runtime_checkable
from random import randrange
from collections import Counter

from util import PushAttr, asdict_with_classvars, HasRngSeed, pr


class TestUtil(unittest.TestCase):

    def test_with_pushattr(self) -> None:
        o = SimpleNamespace()
        o.myattr = 'FIRST'

        with PushAttr(o, 'myattr'):
            o.myattr = 'SECOND'
            self.assertEqual(o.myattr, 'SECOND')
        self.assertEqual(o.myattr, 'FIRST')

    def test_asdict_with_classvars(self) -> None:
        @dataclass
        class Blah:
            x: int
            y: ClassVar[str] = 'the classvar'

        blah = Blah(22)
        self.assertEqual(
            asdict_with_classvars(blah),
            {'x': 22, 'y': 'the classvar'}
        )

    def test_hasrngseed(self) -> None:
        @dataclass
        class Blah(HasRngSeed):
            x: int = 1  # UGLY: default value is required by HasRngSeed

            def random_number(self) -> int:
                return randrange(self.x)

        # deterministic run
        arrays: List[Tuple[int, ...]] = []
        for _ in range(3):
            blah = Blah(x=22, seed=1)
                # UGLY: x must be named if you reverse arg order
            arrays.append(tuple((blah.random_number() for _ in range(10))))
        counter = Counter(arrays)
        self.assertEqual(len(counter), 1) # deterministic

        # nondeterministic run
        arrays = []
        for _ in range(3):
            blah = Blah(x=22)  # no seed given
            arrays.append(tuple((blah.random_number() for _ in range(10))))
        counter = Counter(arrays)
        self.assertEqual(len(counter), 3) # nondeterministic

    def test_hasrngseed_none(self) -> None:
        class Blah(HasRngSeed):
            pass

        blah = Blah()
        self.assertTrue(isinstance(blah.seed, int))

