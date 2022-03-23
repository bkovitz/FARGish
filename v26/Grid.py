# Grid.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from util import Numeric


CanvasData = Sequence[Sequence[int]]  # 8x8 grid, [x][y], [0][0] is upper left
CANVAS_WIDTH = 8
CANVAS_HEIGHT = 8


@dataclass(frozen=True)
class AddrDC:
    '''x,y of a Canvas, 0-relative, (1, 8) is upper left.'''
    x: int
    y: int

    def __str__(self) -> str:
        return f'({self.x}, {self.y})'

A = AddrDC
Addr = AddrDC | Tuple[int, int]

def as_xy(a: Addr | int, y: int | None=None) \
-> Tuple[int, int]:
    if isinstance(a, tuple):
        return a
    elif isinstance(a, int):
        assert isinstance(y, int)
        return a, y
    else:
        return (a.x, a.y)

def as_addrdc(a: Addr | Tuple[int, int] | int, y: int | None=None) -> AddrDC:
    if isinstance(a, AddrDC):
        return a
    elif isinstance(a, tuple):
        return AddrDC(*a)
    else:
        assert isinstance(y, int)
        return AddrDC(a, y)

@dataclass(frozen=True)
class BadPPainterAddr(Exception):
    x: int
    y: int

    def __str__(self) -> str:
        return f'Bad PPainter location: ({self.x}, {self.y})'

@dataclass(frozen=True)
class BadAddr(Exception):
    a: AddrDC

    def __str__(self) -> str:
        return f'Bad Addr: {self.a}'

    @classmethod
    def make(cls, a: Addr | Tuple[int, int] | int, y: int | None=None) \
    -> BadAddr:
        return BadAddr(as_addrdc(a, y))

two_cs: CanvasData = [
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
class Canvas:
    '''8x8 grid with clarities, [x, y], [1, CANVAS_HEIGHT] is upper left.'''
    pixels: List[List[int]]     # [x][y], [0][0] is upper left
    clarities: List[List[int]]  # [x][y], [0][0] is upper left

    def __getitem__(self, a: Addr | int, y: int | None=None) -> int:
        x, y = as_xy(a)
        if (
            x < 1 or x > CANVAS_WIDTH
            or
            y < 1 or y > CANVAS_HEIGHT
        ):
            raise BadAddr.make(a, y)
        return self.pixels[CANVAS_HEIGHT - y][x - 1]

    @classmethod
    def from_data(cls, d: CanvasData) -> Canvas:
        return Canvas(cls.canvasdata_to_lists(d), [[0] * 8] * 8)

    @classmethod
    def canvasdata_to_lists(cls, d: CanvasData) -> List[List[int]]:
        '''Copies 'd' to a list of lists.'''
        return [list(row) for row in d]

@dataclass(frozen=True)
class PPainter:
    ul: int  # value of upper left pixel.
             # 1 or -1 for black or white; 0 for don't know
    ur: int  # etc.
    ll: int
    lr: int

    def paint(self, c: Canvas, addr: Addr) -> None:
        pass
        #TODO

    @classmethod
    def from_canvas(cls, c: Canvas, a: Addr) -> PPainter:
        '''Returns a PPainter that draws the 2x2 square of pixels contained
        in 'c' with upper left corner at 'a'.'''
        x, y = as_xy(a)
        return PPainter(
            c[x, y], c[x+1, y], c[x, y-1], c[x+1, y-1]
        )

    def match_amt(self, c: Canvas, addr: Addr) -> Numeric:
        pass

Painter = PPainter   # union this with QPainter, DPainter, anything else

VarAddr = str
PMatcher = Tuple[VarAddr, Painter]  # Matches a Painter at an Addr

@dataclass(frozen=True)
class QPainter:
    triple1: PPainter
    triple2: PPainter


def make_ppainters(c: Canvas) -> Iterable[PPainter]:
    for x in range(0, CANVAS_WIDTH - 1):
        for y in range(0, CANVAS_HEIGHT - 1):
            yield PPainter.from_canvas(c, A(x, y))

def pps_to_qqs(c: Canvas, pps: Collection[PPainter]) -> Iterable[QPainter]:
    pass


if __name__ == '__main__':
    c = Canvas.from_data(two_cs)
    for y in range(8, 0, -1):
        for x in range(1, 9):
            print(c[x, y], end='')
        print()
