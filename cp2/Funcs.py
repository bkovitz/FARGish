# TODO rm this whole file; it's been merged into Model.py
# Funcs.py -- Functions: objects to fill the third slot of a painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import CanvasValue, CellContent, Expr, Fizzle, FizzleGotNone, \
    is_cell_content, Letter
from Addrs import Addr, I, make_addr, ToAddr, Variable, WorkingSoup
import Subst as SM
import Model as MM
from Painters import Painter
from Log import trace, lo, set_log_level
from util import short


class CallableFunc(ABC):

    @abstractmethod
    def apply(self, model: MM.Model, subst: SM.Subst, value: Value) \
    -> Value:
        pass

    @abstractmethod
    def to_detfuncs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetFunc]:
        pass

    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        '''Will this be a valid function given the substitutions in 'subst'?'''
        return True

@dataclass(frozen=True)
class SimpleFunc:
    var: Variable

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.var})'

Func = Union[CallableFunc, CellContent, Variable, SimpleFunc]
Value = Union[CellContent, Painter, Func, None]

def is_func(x: Any) -> TypeGuard[Func]:
    if isinstance(x, CallableFunc):
        return True
    elif is_cell_content(x):
        return True
    else:
        return False

# A determinate Func: no variables
#DetFunc = Union[Value, DetPainter, Callable] 
DetFunc = Union[CallableFunc, CellContent, SimpleFunc] # TODO Allow SimpleFunc?

# A convenience type for DetPainter.make_from()
ToDetFunc = Union[DetFunc | str, Tuple[ToAddr, ToAddr, DetFunc | str]]

@no_type_check  # causes mypy 0.971 to crash
def make_detfunc(f: ToDetFunc) -> DetFunc:
    match f:
        case (i, j, ff):
            return Painter(make_addr(i), make_addr(j), make_detfunc(ff))
        case str():
            return Letter.from_str(f)
        case _:
            return f

class SimpleFuncClass(CallableFunc):
    '''A Func that gets counted as a SimpleFunc during unification.'''

    @abstractmethod
    def apply(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        pass

    def to_detfuncs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetFunc]:
        yield self

### The basic relational functions

class Same(SimpleFuncClass):

    def apply(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        return v

    def __repr__(self) -> str:
        return 'same'

class Succ(SimpleFuncClass):

    def apply(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'z'
        if isinstance(v, Letter):
            return v.succ()
# TODO succ(Index)
#        elif isinstance(v, int):
#            return v + 1
        raise Fizzle("succ: Can't take successor of {v}")

    def __repr__(self) -> str:
        return 'succ'

class Pred(SimpleFuncClass):

    def apply(self, model: MM.Model, subst: SM.Subst, v: Value) -> Value:
        # TODO Deal with 'a'
        if isinstance(v, Letter):
            return v.pred()
# TODO pred(Index)
#        elif isinstance(v, int):
#            return v - 1
        raise Fizzle("pred: Can't take predecessor of {v}")

    def __repr__(self) -> str:
        return 'pred'

same = Same()
succ = Succ()
pred = Pred()

### The constant function

@dataclass(frozen=True)
class const(SimpleFuncClass):
    v: Any  # TODO restore type hint: Value

    def apply(self, model: MM.Model, subst: SM.Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.v)})'

### Painter-building functions

@dataclass(frozen=True)
class MakeBetweenPainter(CallableFunc):
    '''Makes a painter that paints a value between two others.'''
    i: Addr
    j: Addr
    f: Func

    def apply(self, model: MM.Model, subst: SM.Subst, ignored: Value) \
    -> Painter:
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise FizzleGotNone(self, f'i={self.i}')
        value = model.canvas[result_i + 1]
        if value is None:
            raise FizzleGotNone(self, f'canvas[{result_i + 1}]')
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise FizzleGotNone(self, f'j={self.j}')
        result_f = subst[self.f]
        if result_f is None:
            raise FizzleGotNone(self, f'f={self.f}')
        assert is_func(result_f)
        return Painter(
            Painter(I, SM.Plus(I, result_j - result_i), result_f),
            WorkingSoup,
            Painter(I, SM.Plus(I, 1), value)
        )

    def to_detfuncs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetFunc]:
        if self.can_make(model, subst):
            yield self

    # TODO UT
    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        result_i = subst.as_index(self.i)
        if (
            result_i is None
            or
            model.canvas[result_i] is None
            or
            model.canvas[result_i] == ' '
            or
            model.canvas[result_i + 1] is None
            or
            model.canvas[result_i + 1] == ' '
        ):
            return False
        result_j = subst.as_index(self.j)
        if (
            result_j is None
            or
            model.canvas[result_j] is None
            or
            model.canvas[result_j] == ' '
        ):
            return False
        result_f = subst[self.f]
        if result_f is None:
            return False
        return True

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short

@dataclass(frozen=True)
class MakeRelativeIndirectPainter(CallableFunc):
    '''Makes a painter to match the contents of cell 'i' and recreate both
    its spatial and value relations to cell 'j'.'''
    i: Addr
    j: Addr
    f: Func

    def apply(self, model: MM.Model, subst: SM.Subst, ignored: Value) \
    -> Painter:
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
        assert is_func(result_f)
        if result_f is None:
            raise Fizzle  # TODO More-specific Fizzle
        return Painter(
            #bundle.value if bundle.value_only() else bundle,
            bundle.simplest(),
            WorkingSoup,
            Painter(I, SM.Plus(I, result_j - result_i), result_f)
        )

    def to_detfuncs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetFunc]:
        if self.can_make(model, subst):
            yield self

    # TODO UT
    def can_make(self, model: MM.Model, subst: SM.Subst) -> bool:
        result_i = subst.as_index(self.i)
        if (
            result_i is None
            or
            model.canvas[result_i] is None
            or
            model.canvas[result_i] == ' '
        ):
            return False
        bundle = model.canvas.as_bundle(result_i)
        if bundle is None:
            return False
        result_j = subst.as_index(self.j)
        if (
            result_j is None
            or
            model.canvas[result_j] is None
            or
            model.canvas[result_j] == ' '
        ):
            return False
        result_f = subst[self.f]
        if result_f is None:
            return False
        return True

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short
