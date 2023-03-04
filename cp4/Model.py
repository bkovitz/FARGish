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

@dataclass
class Canvas:
    contents: str
    length: Optional[int]

    @classmethod
    def make_from(cls, s: str) -> Canvas:
        return Canvas(contents=s, length=len(s))

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
    def make_unknown(cls, length: Optional[int]=None) -> Canvas:
        return Canvas(contents='', length=length)

    # TODO UT
    def __getitem__(self, i: Index) -> Optional[str]:
        try:
            return self.contents[i - 1]
        except IndexError:
            return None

    def all_indices(self) -> Iterable[Index]:
        if self.length is None:
            return ()
        else:
            return range(1, self.length + 1)

    def replace_contents(self, s: str) -> None:
        self.contents = s

    def __str__(self) -> str:
        return self.contents

class Op(ABC):

    @classmethod
    @abstractmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        pass

    # TODO UT  Fail when reaching 'z' or 'a'
    @classmethod
    def make(cls, start_letter: str, length: int, start_index: int=1) -> str:
        return (
            cls.reverse_sequence(start_letter, 1, start_index)
            +
            cls.sequence(start_letter, start_index, length)
        )

    @classmethod
    def reverse_sequence(
        cls,
        start_letter: str,
        from_index: Index,
        end_index_exclusive: Index
    ) -> str:
        result: List[str] = []
        for _ in range(from_index, end_index_exclusive):
            start_letter = cls.prev_letter(start_letter)
            result.append(start_letter)
        result.reverse()
        return ''.join(result)

    @classmethod
    def sequence(cls, start_letter: str, from_index: Index, to_index: Index) \
    -> str:
        result: List[str] = []
        for i in range(from_index, to_index + 1):
            if i != from_index:
                start_letter = cls.next_letter(start_letter)
            result.append(start_letter)
        return ''.join(result)

    @classmethod
    @abstractmethod
    def next_letter(cls, letter: str) -> str:
        pass

    @classmethod
    @abstractmethod
    def prev_letter(cls, letter: str) -> str:
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

    # TODO what about 'z'?
    @classmethod
    def next_letter(cls, letter: str) -> str:
        return chr(ord(letter) + 1)

    # TODO what about 'a'?
    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return chr(ord(letter) - 1)

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

    # TODO what about 'z'?
    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return chr(ord(letter) + 1)

@dataclass(frozen=True)
class Same(Op):

    @classmethod
    def has_relation(cls, x1: Any, x2: Any) -> bool:
        return x1 == x2

    @classmethod
    def next_letter(cls, letter: str) -> str:
        return letter

    @classmethod
    def prev_letter(cls, letter: str) -> str:
        return letter

@dataclass(frozen=True)
class Skip:
    i: Index

@dataclass(frozen=True)
class Succeeded:
    info: Any

ops: Iterable[Type[Op]] = (Same, Succ, Pred)

def detect_repetition(canvas: Canvas) -> Optional[Repeat]:
    if canvas.length is None:
        return None
    start_letter = canvas[1]
    if start_letter is not None:
        for op in ops:
            perfect = op.make(start_letter, canvas.length)
            match op_to_repeater(op, canvas, perfect):
                case Succeeded(flaw):
                    return Repeat(
                        canvas,
                        Seed(start_letter, 1),
                        op,
                        exception=flaw
                    )
    return None

def op_to_repeater(
    op: Type[Op],
    canvas: Canvas,
    perfect: str
) -> Optional[Succeeded]:
    flaw: Optional[Skip] = None
    j = 0  # index into 'perfect'
    for i in canvas.all_indices():
        if canvas[i] == perfect[j]:
            pass
        elif canvas[i] == op.next_letter(perfect[j]):
            if flaw is not None:
                return None   # 2nd flaw, so give up
            else:
                flaw = Skip(i)
                j += 1
        else:
            return None
        j += 1
    return Succeeded(flaw)

@dataclass(frozen=True)
class Seed:
    letter: str
    i: Index

@dataclass(frozen=True)
class Repeat:
    canvas: Canvas
    seed: Seed
    op: Type[Op]
    exception: Optional[Skip] = None

    def fill(self) -> None:
        if self.canvas.length is not None:
            self.canvas.replace_contents(
                self.op.make(self.seed.letter, self.canvas.length, self.seed.i)
            )
