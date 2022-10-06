# TODO rm this whole file; it's been merged into Model.py
# Funcs.py -- Functions: objects to fill the third slot of a painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, Expr, Func, I, Value, WorkingSoup, Painter, is_painter, \
    painter_str, Fizzle
import Subst as SM
import Model as MM

from util import short


class SimpleFuncClass:
    '''A Func that gets counted as a SimpleFunc during unification.'''

    @abstractmethod
    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        pass

### The basic relational functions

class Same(SimpleFuncClass):

    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        return v

    def __repr__(self) -> str:
        return 'same'

class Succ(SimpleFuncClass):

    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'z'
        if isinstance(v, str):
            return chr(ord(v) + 1)
        elif isinstance(v, int):
            return v + 1
        raise Fizzle("succ: Can't take successor of {v}")

    def __repr__(self) -> str:
        return 'succ'

class Pred(SimpleFuncClass):

    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'a'
        if isinstance(v, str):
            return chr(ord(v) - 1)
        elif isinstance(v, int):
            return v - 1
        raise Fizzle("pred: Can't take predecessor of {v}")

    def __repr__(self) -> str:
        return 'pred'

same = Same()
succ = Succ()
pred = Pred()

### The constant function

@dataclass(frozen=True)
class const(SimpleFuncClass):
    v: Value

    def __call__(self, model: MM.Model, subst: SM.Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        if is_painter(self.v):
            return f'{cl}({painter_str(self.v)})'
        else:
            return f'{cl}({short(self.v)})'

### Painter-building functions

@dataclass(frozen=True)
class MakeBetweenPainter:
    '''Makes a painter that paints a value between two others.'''
    i: Addr
    j: Addr
    f: Func

    def __call__(self, model: MM.Model, subst: SM.Subst, ignored: Value) -> Value:
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise Fizzle  # TODO More-specific Fizzle
        value = model.canvas[result_i + 1]
        if value is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_f = subst[self.f]
        if result_f is None:
            raise Fizzle  # TODO More-specific Fizzle
        return (
            (I, SM.Plus(I, result_j - result_i), result_f),
            WorkingSoup,
            (I, SM.Plus(I, 1), value)
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short

@dataclass(frozen=True)
class MakeRelativeIndirectPainter:
    i: Addr
    j: Addr
    f: Func

    def __call__(self, model: MM.Model, subst: SM.Subst, ignored: Value) -> Value:
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise Fizzle  # TODO More-specific Fizzle
        #value = model.canvas[result_i]
        bundle = model.canvas.as_bundle(result_i)
        if bundle is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_f = subst[self.f]
        if result_f is None:
            raise Fizzle  # TODO More-specific Fizzle
        return (
            bundle.value if bundle.value_only() else bundle,
            WorkingSoup,
            (I, SM.Plus(I, result_j - result_i), result_f)
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short

