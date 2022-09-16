# Types.py -- Type hints used throughout the canvas-and-painters model.
#             Functions to convert between types.

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from util import short


@dataclass(frozen=True)
class SoupRef:
    name: str

    def __str__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')

CanvasValue = Union[str, None]
Index = int   # the index of a cell within a Canvas
MaybeIndex = Union[Index, None, List[Index]]  # TODO rename or divide into type types: List[Index] is not always appropriate
Expr = Any
Addr = Any
Func = Any
Painter = Tuple[Addr, Addr, Func]
DetAddr = Union[Index, Painter, SoupRef]
Value = Union[CanvasValue, Painter, Func]

@dataclass(frozen=True)
class Variable:
    name: str

    def __str__(self) -> str:
        return self.name

I = Variable('i')
J = Variable('j')

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

# TODO rm; Subst.as_index() does this correctly
def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return e
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle(f"as_index: Can't convert {e!r} to index")

def is_painter(x: Func) -> TypeGuard[Painter]:
    #return isinstance(x, tuple)
    match x:
        case (i, j, func):
            return True
        case _:
            return False

def painter_str(p: Painter) -> str:
    i, j, func = p
    return f'({addr_str(i)}, {addr_str(j)}, {func_str(func)})'

def addr_str(a: Addr) -> str:
    if isinstance(a, int):
        return str(a)
    elif isinstance(a, str):
        return repr(a)
    elif isinstance(a, SoupRef):
        return a.name
    elif is_painter(a):
        return painter_str(a)
    elif isinstance(a, Variable):
        return a.name
    else:
        return str(a)

def func_str(func: Func) -> str:
    if callable(func):
        return short(func)
    elif is_painter(func):
        return painter_str(func)
    else:
        return repr(func)

### Exceptions ###

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class FizzleNotIndex(Fizzle):
    e: Expr

    def __str__(self) -> str:
        return f"can't reduce to Index: {repr(self.e)}"

