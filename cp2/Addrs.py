# Addrs.py -- Special types of address that can fill the first two elements
#             of a Painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, CellContent, Func, Index, Indices, Painter, SoupRef, \
    Variable
from Canvas import Canvas
import Subst as SM
import Model as MM
from util import short


# A determinate Addr: no variables, no patterns, nothing to match or expand
DetAddr = Union[Index, Indices, Painter, SoupRef]

@dataclass(frozen=True)
class DetAddrWithSubst:
    subst: SM.Subst
    addr: DetAddr

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.subst)}, {short(self.addr)})'

    __str__ = short
    __repr__ = short

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
class RelatedPair(SpecialAddr):
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Func

    def to_detaddrs(
        self,
        model: MM.Model,
        canvas: Canvas,
        primitive_funcs: Iterable[Func]
    ) -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in canvas.all_addrs():
                    for j in canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in self.func_iter(self.f, primitive_funcs):
                            if model.are_related_by(i, j, f):
                                yield DetAddrWithSubst(
                                    SM.Subst.make_from(
                                        (self.i, i), (self.j, j), (self.f, f)
                                    ),
                                    Indices(i, j)
                                )
            case _:
                raise NotImplementedError(
                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
                )

    def func_iter(self, f: Func, primitive_funcs: Iterable[Func]) \
    -> Iterable[Func]:
        match f:
            case Variable():
                yield from primitive_funcs
            case _:
                yield f
            # TODO Other cases? Could a Variable be in an expression?

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

# NEXT
@dataclass(frozen=True)
class MatchContent:
    content: CellContent
