# TODO rm this whole file; it's been merged into Model.py
# Funcs.py -- Functions: objects to fill the third slot of a painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field

from Types import Addr, Expr, Func, Value, Painter, is_painter, painter_str, Fizzle
from Subst import Subst
from util import short

### The basic relational functions

def same(subst: Subst, v: Value) -> Value:
    return v

def succ(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle("succ: Can't take successor of {v}")

def pred(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'a'
    if isinstance(v, str):
        return chr(ord(v) - 1)
    elif isinstance(v, int):
        return v - 1
    raise Fizzle("pred: Can't take predecessor of {v}")

### The constant function

@dataclass(frozen=True)
class const:
    v: Value

    def __call__(self, subst: Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        if is_painter(self.v):
            return f'{cl}({painter_str(self.v)})'
        else:
            return f'{cl}({short(self.v)})'

### apply_func()

def apply_func(subst: Subst, f: Func, v: Value) \
-> Union[Value, Painter]:
    if isinstance(f, str) or isinstance(f, int) or is_painter(f):
        return f
    elif callable(f):
        return f(subst, v)
    else:
        raise NotImplementedError(f"apply_func: can't apply {f}")
