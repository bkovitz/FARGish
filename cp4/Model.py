# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain
import re

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import short


Index = int


########## The canvas ##########

@dataclass(frozen=True)
class Canvas:
    contents: str

    @classmethod
    def make_from(cls, s: str) -> Canvas:
        return Canvas(s)

    @classmethod
    def parse_analogy_string(cls, s: str) -> List[Canvas]:
        #old_world, new_world = s.split(";")
        m = re.match('([a-z]+)\s*->\s*([a-z]+)\s*;\s*([a-z]+)\s*->\s*\?', s)
        if m is not None:
            return [
                cls.make_from(group) for group in m.groups()
            ] + [cls.make_unknown()]
        else:
            return []

    @classmethod
    def make_unknown(cls) -> Canvas:
        return Canvas('')

    # TODO UT
    def __getitem__(self, i: Index) -> str:
        return self.contents[i - 1]
        # TODO what if the index is out of bounds?

    def __len__(self) -> int:
        return len(self.contents)

    def __str__(self) -> str:
        return self.contents


class Op(ABC):

    @classmethod
    @abstractmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        pass

    # TODO UT  Fail when reaching 'z' or 'a'
    @classmethod
    def make(cls, start_letter: str, n: int) -> str:
        result: List[str] = [start_letter]
        for _ in range(n - 1):
            result.append(cls.next_letter(result[-1]))
        return ''.join(result)

    @classmethod
    @abstractmethod
    def next_letter(cls, letter: str) -> str:
        pass

class Succ(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        match (x1, x2):
            case (str(), str()):
                if x1 >= 'z':
                    return False
                return ord(x1) + 1 == ord(x2)
        return False

    @classmethod
    def next_letter(cls, letter: str) -> str:
        return chr(ord(letter) + 1)

class Pred(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        match (x1, x2):
            case (str(), str()):
                if x1 <= 'a':
                    return False
                return ord(x1) - 1 == ord(x2)
        return False

    # TODO UT  Fail when reaching 'a'
    @classmethod
    def next_letter(cls, letter: str) -> str:
        return chr(ord(letter) - 1)

@dataclass(frozen=True)
class Same(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        return x1 == x2

    @classmethod
    def next_letter(cls, letter: str) -> str:
        return letter

def detect_repetition(canvas: Canvas) -> Optional[Tuple[Seed, Callable]]:
    start_letter = canvas[1]
    second_letter = canvas[2]
    for op in [Same, Succ, Pred]:
        perfect = op.make(start_letter, len(canvas))
        if str(canvas) == perfect:
            return Seed(start_letter, 1), op
    return None

    
@dataclass(frozen=True)
class Seed:
    letter: str
    i: Index
