# test_util.py -- Unit tests for util.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, InitVar
from types import SimpleNamespace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable, get_type_hints, get_origin, get_args
import typing
from random import randrange
from collections import Counter

from util import PushAttr, asdict_with_classvars, HasRngSeed, pr, \
    is_type_instance


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

class TestIsTypeInstance(unittest.TestCase):

    @dataclass(frozen=True)
    class Blah:
        pass

    def f(
        self,
        blah: Blah,
        int: int,
        none: None,
        opt: Optional[Blah],
        dict: Dict[str, int],
        seq: Sequence[int],
        tup: Tuple[str, int],
        tupell: Tuple[int, ...],
        type0: Type,
        type1: Type[Hashable],
        type2: Type[Blah],
        any: Any
    ) -> None:
        pass

    hints = get_type_hints(f)

    def test_int(self) -> None:
        i = self.hints['int']
        self.assertTrue(is_type_instance(2, i))
        self.assertFalse(is_type_instance('foo', i))
        self.assertFalse(is_type_instance(None, i))
        
    def test_class(self) -> None:
        blah = self.hints['blah']
        b = self.Blah()
        self.assertTrue(is_type_instance(b, blah))
        self.assertFalse(is_type_instance(None, blah))

    def test_none(self) -> None:
        none = self.hints['none']
        self.assertTrue(is_type_instance(None, none))
        self.assertFalse(is_type_instance(self.Blah(), none))

    def test_optional(self) -> None:
        typ = self.hints['opt']
        self.assertTrue(is_type_instance(None, typ))
        self.assertTrue(is_type_instance(self.Blah(), typ))
        self.assertFalse(is_type_instance(2, typ))

    def test_dict(self) -> None:
        typ = self.hints['dict']
        dgood = {'key': 22}
        dbad = {self.Blah(): 22}
        dempty: Dict[Any, Any] = {}
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertTrue(is_type_instance(dgood, typ))
        self.assertTrue(is_type_instance(dempty, typ))
        self.assertFalse(is_type_instance(dbad, typ))

    def test_tuple(self) -> None:
        typ = self.hints['tup']
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertFalse(is_type_instance(22, typ))
        self.assertFalse(is_type_instance((), typ))
        self.assertFalse(is_type_instance(('nope',), typ))
        self.assertTrue(is_type_instance(('yes', 22), typ))
        self.assertFalse(is_type_instance(('nope', 'nope'), typ))
        self.assertFalse(is_type_instance((22, 'nope'), typ))

    def test_tuple_with_ellipsis(self) -> None:
        typ = self.hints['tupell']
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertFalse(is_type_instance(22, typ))
        self.assertTrue(is_type_instance((), typ))
        self.assertFalse(is_type_instance(('nope',), typ))
        self.assertFalse(is_type_instance(('yes', 22), typ))
        self.assertTrue(is_type_instance((21,), typ))
        self.assertTrue(is_type_instance((21, 22), typ))
        self.assertTrue(is_type_instance((21, 22, 23), typ))
        self.assertFalse(is_type_instance((21, 22, 23, 'nope'), typ))

    def test_type_without_parameter(self) -> None:
        typ = self.hints['type0']
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertTrue(is_type_instance(self.Blah, typ))
        self.assertTrue(is_type_instance(int, typ))

    def test_type_with_parameter(self) -> None:
        typ = self.hints['type1']
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertTrue(is_type_instance(self.Blah, typ))
        self.assertTrue(is_type_instance(int, typ))

        typ = self.hints['type2']
        self.assertFalse(is_type_instance(None, typ))
        self.assertFalse(is_type_instance('nope', typ))
        self.assertTrue(is_type_instance(self.Blah, typ))
        self.assertFalse(is_type_instance(int, typ))

    def test_any(self) -> None:
        typ = self.hints['any']
        #print()
        #print(typ)
        #print(get_origin(typ))
        #print(get_args(typ))
        #print()
        self.assertTrue(is_type_instance(None, typ))
        self.assertTrue(is_type_instance('nope', typ))
        self.assertTrue(is_type_instance(self.Blah, typ))
        self.assertTrue(is_type_instance(int, typ))
