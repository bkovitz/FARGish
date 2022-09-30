# Painters.py -- Special types of Painter

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from io import StringIO

from Types import painter_str, Painter, Value
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
        return f'{cl}({painter_str(self.as_painter()):80s}; {short(self.subst):30s}; {nf(self.prob_weight)})'

    def __str__(self) -> str:
        sio = StringIO()
        print(short(self.as_painter()), file=sio)
        print(short(self.subst), file=sio)
        print(nf(self.prob_weight), file=sio)
        print(f'basis={short(self.basis)}', file=sio)
        return sio.getvalue()

# A determinate Func: no variables
DetFunc = Union[Value, DetPainter, Callable] 

# A determinate painter. Unlike DetPainter, a DetPainter0 includes only the
# minimal painter info, not additional information like a Subst.
DetPainter0 = Tuple[DetAddr, DetAddr, DetFunc]
