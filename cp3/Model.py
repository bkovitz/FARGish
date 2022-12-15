# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import short


Index = int

@dataclass(frozen=True)
class Variable:
    # Don't instantiate this. Instantiate only subclasses.
    name: str

    def __repr__(self) -> str:
        return self.name

@dataclass(frozen=True)
class IndexVariable(Variable):
    pass

I = IndexVariable('I')
J = IndexVariable('J')

VarSpec = IndexVariable   # TODO Union with PainterVariable, CanvasVariable,
                          # CompoundVariable

########## Canvas-cell contents ##########

@dataclass(frozen=True)
class Letter:
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if len(self.c) != 1 or self.c < 'a' or self.c > 'z':
            raise ValueError(f"Letter {self.c!r}: must be in range 'a'..'z'.")

    @classmethod
    def from_str(self, c: str) -> Union[Letter, Blank]:
        if len(c) != 1:
            raise ValueError('Letter.from_str(): {c!r} must have len==1')
        if c == ' ':
            return Blank()
        else:
            return Letter(c)

    def succ(self) -> Letter:
        if self.c >= 'z':
            raise FizzleNoSucc
        else:
            return Letter(chr(ord(self.c) + 1))

    def pred(self) -> Letter:
        if self.c <= 'a':
            raise FizzleNoPred
        else:
            return Letter(chr(ord(self.c) - 1))

    def short(self) -> str:
        return repr(self.c)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.c!r})'

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.c!r})'

@dataclass(frozen=True)
class Blank:

    def __repr__(self) -> str:
        return self.__class__.__name__

    short = __repr__

    def __str__(self) -> str:
        return ' '

def is_blank(x: Any) -> TypeGuard[Blank]:
    return isinstance(x, Blank)

CanvasValue = Union[Letter, Blank]  # TODO change str to Letter

def is_canvas_value(x: Any) -> TypeGuard[CanvasValue]:
    return (
        isinstance(x, Letter)
        or
        isinstance(x, Blank)
    )

########## The canvas ##########

@dataclass
class Canvas:
    d: Dict[Index, CanvasValue] = field(default_factory=lambda: {})
    min_index: Optional[Index] = None
    max_index: Optional[Index] = None

    @classmethod
    def make_from(cls, s: str) -> Canvas:
        if not s:
            return cls()
        else:
            d: Dict[Index, CanvasValue] = {}
            for i, c in zip(range(1, len(s) + 1), s):
                d[i] = Letter.from_str(c)
            return cls(d, min_index=1, max_index=len(s))

    def __getitem__(self, a: Index) -> Optional[CanvasValue]:
        return self.d.get(a, None)

    def __setitem__(self, a: Index, v: CanvasValue) -> None:
        self.d[a] = v
        if self.min_index is None:
            self.min_index = a
            self.max_index = a
        else:
            assert self.max_index is not None
            self.min_index = min(a, self.min_index)
            self.max_index = max(a, self.max_index)

    def all_indices(self) -> Iterable[Index]:
        if self.min_index is None:
            return
        else:
            assert self.max_index is not None
            yield from range(self.min_index, self.max_index + 1)

########## Exceptions ##########

class Fizzle(Exception):
    pass

class FizzleCantGoThere(Fizzle):
    '''Generic class for FizzleNoSucc, FizzleNoPred, and any other kind of
    fizzling that involves 'running out of room'--i.e. being unable to
    apply a function to a certain constant.'''
    pass

@dataclass(frozen=True)
class FizzleNoSucc(FizzleCantGoThere):

    def __str__(self) -> str:
        return 'No successor'

@dataclass(frozen=True)
class FizzleNoPred(FizzleCantGoThere):

    def __str__(self) -> str:
        return 'No predecessor'

