# TODO rm this whole file; it's been merged into Model.py
# Funcs.py -- Functions: objects to fill the third slot of a painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, Expr, Func, Value, Painter, is_painter, \
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

    def __str__(self) -> str:
        return 'same'

class Succ(SimpleFuncClass):

    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'z'
        if isinstance(v, str):
            return chr(ord(v) + 1)
        elif isinstance(v, int):
            return v + 1
        raise Fizzle("succ: Can't take successor of {v}")

    def __str__(self) -> str:
        return 'succ'

class Pred(SimpleFuncClass):

    def __call__(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'a'
        if isinstance(v, str):
            return chr(ord(v) - 1)
        elif isinstance(v, int):
            return v - 1
        raise Fizzle("pred: Can't take predecessor of {v}")

    def __str__(self) -> str:
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

### apply_func()

#def apply_func(subst: SM.Subst, f: Func, v: Value) \
#-> Union[Value, Painter]:
#    if isinstance(f, str) or isinstance(f, int) or is_painter(f):
#        return f
#    elif callable(f):
#        return f(subst, v)
#    else:
#        raise NotImplementedError(f"apply_func: can't apply {f}")
