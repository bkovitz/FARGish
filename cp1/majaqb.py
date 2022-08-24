# majaqb.py -- The ajaqb model, implemented in a way that's modeled on
#              Mathematica

''' WANT

For a given state of the model:
  Find all painters that can run.
  Make determinate painters from them.
  Choose a determinate painter by weight.
  Run it by:
    

Internal representation has classes. External representation is simple.
There's a way to translate both ways.

'''

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from random import choice, choices, random
import operator
from functools import reduce
from io import StringIO
from collections import defaultdict
import sys

from pyrsistent import pmap
from pyrsistent.typing import PMap

from util import Numeric, short, as_tuple, as_list, pts, force_setattr, union, \
    reseed, safe_issubclass, newline, psa, pr, nf
from Log import trace, lo, logging


@dataclass(frozen=True)
class SoupRef:
    name: str

    def __str__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')

Expr = Any
Addr = Any
Func = Any
Painter = Tuple[Addr, Addr, Func]

@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    source: Index
    target: Union[Index, SoupRef, Painter]
    func: Func
    basis: Painter  # what this DetPainter was made from


class Model:

    def painter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
#        for subst, source in self.matching_sources(p.source):
#            for subst, target in self.matching_targets(subst, p.target):
#                yield self.as_determinate_painter(subst, 

        for subst, i in self.matching_detaddrs(empty_subst, I, source):
            for subst, j in self.matching_detaddrs(subst, J, target):
                # TODO apply the subst to func?
                yield DetPainter(i, j, func, p)

    def matching_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[Tuple[Subst, DetAddr]]:
        # NEXT Return Subst, too
        match source:
            case str():
                yield from self.canvas.addrs_containing_value(source)
            case int():
                yield source
            case Painter():
                yield from self.ws.matching_painters(source)
            case _:
                raise NotImplementedError(addr)

    def run_detpainter(self, dp: DetPainter) -> None:
        oldval = self.get_value(dp.source)
        newval = self.apply_func(dp.subst, dp.func, oldval)
        self.paint(dp.target, newval)

    def get_value(self, addr: Addr) -> Value:
        match addr:
            case int():
                return self.canvas[addr]
            case WorkingSoup:
                return self.ws
            case (WorkingSoup, p) if is_painter(p):
                return addr
            case _:
                raise NotImplementedError(addr)

    def regen_timestep(self) -> None:
        '''Runs one timestep of regeneration.'''
        self.ws.decay()
        # make determinate painters
        # assign a probability to each
        # choose a painter
        # run_detpainter()
