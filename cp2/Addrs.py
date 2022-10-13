# Addrs.py -- Special types of address that can fill the first two elements
#             of a Painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import AnnotationType, CellContent, Expr, Fizzle, is_cell_content, \
    Letter
from Canvas import Canvas
from Funcs import CallableFunc, Func, DetFunc, SimpleFunc
from Painters import Painter
import Subst as SM
import Model as MM
from util import short, force_setattr


class Addr(ABC):

    @abstractmethod
    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        pass

# TODO rm and replace by simply defining __str__ in Addr classes

#def addr_str(a: Addr) -> str:
#    if isinstance(a, int):
#        return str(a)
#    elif isinstance(a, str):
#        return repr(a)
#    elif isinstance(a, SoupRef):
#        return a.name
#    elif is_painter(a):
#        return painter_str(a)
#    elif isinstance(a, Variable):
#        return a.name
#    else:
#        return str(a)

# A determinate Addr: no variables, no patterns, nothing to match or expand
#DetAddr = Union[Index, Indices, Painter, SoupRef]
DetAddr = Addr  # TODO Make a correct type hint

# A convenience type for DetAddr.make_from()
ToDetAddr = Union[int, SoupRef, Indices]

# A convenience function for unit tests
def make_detaddr(a: ToDetAddr) -> DetAddr:
    match a:
        case int():
            return Index(a)
        case SoupRef():
            return a
        case Indices():
            return a

# A convenience type for Painter.make_from()
ToAddr = Union[int, str, SoupRef, SM.Plus]

# A convenience function for unit tests
def make_addr(a: ToAddr) -> Addr:
    match a:
        case int():
            return Index(a)
        case SoupRef() | SM.Plus():
            return a
        case str():
            return MatchContent(Letter.from_str(a))

@dataclass(frozen=True)
class Index(DetAddr):
    i: int

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst.unify(var, self), self)

    def __add__(self, offset: int) -> Index:
        return Index(self.i + offset)

    def __sub__(self, other: Index | int) -> Index:
        o: int = other if isinstance(other, int) else other.i
        return Index(self.i - o)

    def __lt__(self, other: Index) -> bool:
        return self.i < other.i

    def __le__(self, other: Index) -> bool:
        return self.i <= other.i

    def __gt__(self, other: Index) -> bool:
        return self.i > other.i

    def __ge__(self, other: Index) -> bool:
        return self.i >= other.i

    @classmethod
    def from_to(cls, lb: Index | int, ub: Index | int) -> Iterable[Index]:
        return (Index(i) for i in range(as_int(lb), as_int(ub) + 1))

def as_int(x: Index | int) -> int:
    if isinstance(x, int):
        return x
    else:
        return x.i

def to_index(i: Index | int) -> Index:
    return Index(i) if isinstance(i, int) else i

Index2 = Union[Index, Tuple[Index, AnnotationType]]

def as_index2(x: Union[int, Index2]):
    if isinstance(x, int):
        return Index(x)
    else:
        return x

@no_type_check  # crashes mypy 0.971
def extract_index(i: Index2) -> Index:
    match i:
        case int():
            return i
        case (int(ii), _):
            return ii
    assert False, f"extract: should not go past 'match' stmt, {i}"

@dataclass(frozen=True)
class Indices(Addr):
    '''Zero or more Index2s.'''
    elems: FrozenSet[Index2]

    def __init__(self, *ii: Index2 | int):
        force_setattr(self, 'elems', frozenset(as_index2(i) for i in ii))

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst.unify(var, self), self)

    def __iter__(self) -> Iterable[Index2]:
        yield from self.elems
        
    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f"{cl}({', '.join(str(e) for e in self.elems)})"

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

def is_index(e: Any) -> TypeGuard[Index]:
    return isinstance(e, int)

# TODO rm; Subst.as_index() does this correctly
def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return Index(e)
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle(f"as_index: Can't convert {e!r} to index")

@dataclass(frozen=True)
class SoupRef(Addr):
    name: str

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst, self)

    def __repr__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')

@dataclass(frozen=True)
class DetAddrWithSubst:
    subst: SM.Subst
    addr: DetAddr

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.subst)}, {short(self.addr)})'

    __str__ = short
    __repr__ = short

# A way to refer to a cell's value or an annotation within the cell
@dataclass(frozen=True)
class Variable(Addr):
    name: str

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        if self in subst:
            yield from (
                subst.simplify(self).to_detaddrs(model, subst, var)
            )
        else:
            yield from (
                DetAddrWithSubst(
                    subst.unify(var, index).unify(self, index),
                    index
                ) for index in model.canvas.all_addrs()
            )
        
    def __repr__(self) -> str:
        return self.name

I = Variable('I')
J = Variable('J')
F = Variable('F')

# TODO rm
class SpecialAddr(ABC):

    @abstractmethod
    def to_detaddrs(
        self,
        model: MM.Model,
        canvas: Canvas,
        primitive_funcs: Iterable[Func]
    ) -> Iterable[DetAddrWithSubst]:
        pass

@dataclass(frozen=True)
class RelatedPair(Addr):
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Func

#    def to_detaddrs(
#        self,
#        model: MM.Model,
#        canvas: Canvas,
#        primitive_funcs: Iterable[Func]
#    ) -> Iterable[DetAddrWithSubst]:
#        match (self.i, self.j):
#            case (Variable(), Variable()):
#                for i in canvas.all_addrs():
#                    for j in canvas.all_addrs():
#                        if i >= j:
#                            continue
#                        for f in self.func_iter(self.f, primitive_funcs):
#                            if model.are_related_by(i, j, f):
#                                yield DetAddrWithSubst(
#                                    SM.Subst.make_from(
#                                        (self.i, i), (self.j, j), (self.f, f)
#                                    ),
#                                    Indices(i, j)
#                                )
#            case _:
#                raise NotImplementedError(
#                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
#                )

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in model.canvas.all_addrs():
                    for j in model.canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in self.func_iter(self.f, model.primitive_funcs):
                            if model.are_related_by(i, j, f):
                                yield DetAddrWithSubst(
                                    SM.Subst.make_from(
                                        (self.i, j), (self.j, j), (self.f, f)
                                    ),
                                    Indices(i, j)
                                )
            case _:
                raise NotImplementedError(
                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
                )

    def func_iter(self, f: Func, primitive_funcs: Iterable[DetFunc]) \
    -> Iterable[DetFunc]:
        match f:
            case Variable():
                yield from primitive_funcs
            case CallableFunc():
                yield f
            case _ if is_cell_content(f):
                yield f
            case SimpleFunc():
                yield f
            case _:
                raise NotImplementedError
            # TODO Other cases? Could a Variable be in an expression?

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

# NEXT
@dataclass(frozen=True)
class MatchContent(Addr):
    '''An Addr to match any sort of content in canvas cells: letters and/or
    annotations.'''
    content: CellContent

    def to_detaddrs(self, model: MM.Model, subst: SM.Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield from (
            DetAddrWithSubst(subst.unify(var, index), index)
                for index in model.canvas.all_matching(self.content)
        )
