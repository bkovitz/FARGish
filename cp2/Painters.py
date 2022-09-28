# Painters.py -- Special types of Painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field

from Types import addr_str, func_str, Painter, Value
from Canvas import Canvas
from Addrs import DetAddr
from Subst import Subst, empty_subst
from util import short, nf, Numeric


@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    subst: Subst
    source: DetAddr
    target: DetAddr
    func: DetFunc
    prob_weight: Numeric
    basis: Optional[Painter] = None  # what this DetPainter was made from

    @classmethod
    def make_from(cls, painter: Tuple[DetAddr, DetAddr, DetFunc]) -> DetPainter:
        '''An easy way to construct a DetPainter in a unit test. Not for use
        in the model proper.'''
        source, target, func = painter
        return cls(
            empty_subst.unify_ijf(source, target, func),
            source,
            target,
            func,
            1,
            None
        )

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

    def as_painter(self) -> Painter:
        return (self.source, self.target, self.func)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'({short(self.subst)}; {addr_str(self.source)}, {addr_str(self.target)}, {func_str(self.func)}; {nf(self.prob_weight)})'

# A determinate Func: no variables
DetFunc = Union[Value, DetPainter, Callable] 

