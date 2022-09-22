# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, product

from Types import Addr, CanvasValue, DetAddr, F, FizzleValueNotFound, Func, I, \
    Index, Indices, J, MaybeIndex, Painter, SoupRef, Value, Variable, \
    addr_str, func_str
from Canvas import Canvas, Canvas1D
from Soup import Soup
from Subst import Subst, empty_subst, Plus
from Funcs import same, pred, succ
from Log import lo
from util import short, nf, Numeric


@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    subst: Subst
    source: DetAddr
    target: Union[DetAddr, SoupRef]
    func: Func
    prob_weight: Numeric
    basis: Optional[Painter] = None  # what this DetPainter was made from

    def is_valid_for(self, canvas: Canvas) -> bool:
        match (self.source, self.target):
            case (int(), int()):
                return (
                    canvas.has_addr(self.source)
                    and
                    canvas.has_addr(self.target)
                )
            case _:
                return True

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'({addr_str(self.source)}, {addr_str(self.target)}, {func_str(self.func)}; {nf(self.prob_weight)})'

@dataclass(frozen=True)
class DetAddrWithSubst:
    subst: Subst
    addr: DetAddr

@dataclass(frozen=True)
class RelatedPair:
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Variable   # TODO Should allow some way to restrict or specify a
                  # list of allowable Funcs, i.e. the allowable relations
                  # between cells.

    def to_detaddrs(self, canvas: Canvas, funcs: Iterable[Func]) \
    -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in canvas.all_addrs():
                    for j in canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in funcs:
                            if canvas.are_related_by(i, j, f):
                                yield DetAddrWithSubst(
                                    Subst.make_from(
                                        (self.i, i), (self.j, j), (self.f, f)
                                    ),
                                    Indices(i, j)
                                )
            case _:
                raise NotImplementedError(
                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
                )

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    primitive_funcs: FrozenSet = frozenset([same, succ, pred])

    @classmethod
    def canvas_from(cls, s: str) -> Model:
        return Model(canvas=Canvas1D.make_from(s))

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)

    def painter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
        source, target, func = p
        det_sources = self.addr_to_detaddrs(empty_subst, I, source)
        det_targets = chain.from_iterable(
            self.addr_to_detaddrs(ds.subst, J, target)
                for ds in det_sources
        )
        det_funcs = chain.from_iterable(
            self.func_to_detfuncs(dt.subst, F, func)
                for dt in det_targets
        )
        # NEED to pair a subst with each DetAddr
        return (
            DetPainter(
                df.subst,
                ds.addr,
                dt.addr,
                df.func,
                1.0,   # TODO prob_weight,
                p  # basis, "author"
            ) for ds, dt, df in product(det_sources, det_targets, det_funcs)
        )

    # NEXT Pass variable to assign index to
    def addr_to_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[DetAddrWithSubst]:
        match addr:
            case int():
                yield DetAddrWithSubst(subst.unify(var, addr), addr)
            case str():
                yield from (
                    # NEXT subst needs to unify var with index
                    DetAddrWithSubst(subst.unify(var, index), index)
                        for index in self.canvas.all_matching(addr)
                )
            case Variable():
                if addr in subst:
                    yield from (
                        self.addr_to_detaddrs(subst, var, subst.simplify(addr))
                    )
                else:
                    yield from (
                        DetAddrWithSubst(
                            subst.unify(var, index).unify(addr, index),
                            index
                        )
                            for index in self.canvas.all_addrs()
                    )
            case Plus():
                match subst.as_index(addr):
                    case None:
                        return
                    case int() as index:
                        yield DetAddrWithSubst(subst.unify(var, index), index)
                    case _:
                        raise NotImplementedError(f"Can't match Plus that simplifies to {addr}, {type(addr)}")
            case RelatedPair():
                yield from addr.to_detaddrs(self.canvas, self.primitive_funcs)
            case _:
                raise NotImplementedError(
                    f'Addr {addr} has unknown type {type(addr)}.'
                )
            # TODO

    def func_to_detfuncs(self, subst: Subst, var: Variable, func: Func) \
    -> Iterable[Any]:  # TODO Create an appropriate type
        pass
