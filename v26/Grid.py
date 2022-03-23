# Grid.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING


Canvas = Sequence[Sequence[int]]  # 8x8 grid, [x][y], [0][0] is upper left
CANVAS_WIDTH = 8
CANVAS_HEIGHT = 8
Loc = Tuple[int, int]             # x,y of a Canvas, 0-relative

two_cs: Canvas = [
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, -1, -1, -1, -1],
    [-1, +1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, +1, -1, -1, -1],
    [-1, -1, -1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
]

@dataclass(frozen=True)
class BadPPainterLoc(Exception):
    x: int
    y: int

    def __str__(self) -> str:
        return f'Bad PPainter location: ({self.x}, {self.y})'


@dataclass(frozen=True)
class PPainter:
    ul: int  # value of upper left pixel.
             # 1 or -1 for black or white; 0 for don't know
    ur: int  # etc.
    ll: int
    lr: int

    def paint(self, c: Canvas, loc: Loc) -> None:
        pass
        #TODO

    @classmethod
    def from_canvas(cls, c: Canvas, x: int, y: int) -> PPainter:
        '''Returns a PPainter that draws the 2x2 square of pixels contained
        in 'c' with upper left corner ('x', 'y').'''
        # TODO fail if bad loc
        if x < 0 or x+1 >= CANVAS_WIDTH or y < 0 or y+1 >= CANVAS_HEIGHT:
            raise BadPPainterLoc(x, y)
        else:
            return PPainter(
                c[x][y], c[x+1][y], c[x][y+1], c[x+1][y+1]
            )

PTriple = Tuple[

@dataclass(frozen=True)
class QPainter:
    triple1: PPainter
    triple2: PPainter


def make_ppainters(c: Canvas) -> Iterable[PPainter]:
    for x in range(0, CANVAS_WIDTH - 1):
        for y in range(0, CANVAS_HEIGHT - 1):
            yield PPainter.from_canvas(c, x, y)

def pps_to_qqs(c: Canvas, pps: Collection[PPainter]) -> Iterable[QPainter]:
