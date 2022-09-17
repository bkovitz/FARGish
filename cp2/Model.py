# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, CanvasValue, DetAddr, FizzleValueNotFound, Index, \
    MaybeIndex, Value
from Soup import Soup
from Subst import Subst
from Log import lo
from util import short, Numeric


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

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    def painter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
        source, target, func = p
        det_sources = self.addr_to_detaddrs(empty_subst, I, source)
        det_targets = chain.from_iterable(
            self.addr_to_detaddrs(ds.subst, J, target)
                for ds in det_sources
        )
        det_funcs = chain.from_iterable(
            self.func_to_detfuncs(dt.subst, func)
                for dt in det_targets
        )
        # NEED to pair a subst with each DetAddr
        return (
            DetPainter(
                df.subst,
                ds.addr,
                dt.addr,
                df.func,
                # prob_weight,
                p  # basis, "author"
            ) for ds, dt, df in product(det_sources, det_targets, det_funcs)
        )

    # NEXT Pass variable to assign index to
    def addr_to_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[DetAddrWithSubst]:
        match addr:
            case int():
                yield DetAddrWithSubst(subst, addr)
            case str():
                yield from self.canvas.all_matching(addr)
            # TODO
