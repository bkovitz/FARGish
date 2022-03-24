# Grid.py -- PPainters and QPainters, and the Canvas is a grid

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import choices

from util import Numeric, pts
from Log import lo


CanvasData = Sequence[Sequence[int]]  # 8x8 grid, [x][y], [0][0] is upper left
CANVAS_WIDTH = 8
CANVAS_HEIGHT = 8


@dataclass(frozen=True)
class AddrDC:
    '''x,y of a Canvas, 0-relative, (1, 8) is upper left.'''
    x: int
    y: int

    def right(self) -> AddrDC:
        return AddrDC(self.x + 1, self.y)

    def down(self) -> AddrDC:
        return AddrDC(self.x, self.y - 1)

    def sq2x2(self) -> Iterable[AddrDC]:
        '''Yields AddrDCs in a 2x2 square with (x, y) at upper left, in
        row-major order.'''
        yield self
        yield AddrDC(self.x + 1, self.y)
        yield AddrDC(self.x, self.y - 1)
        yield AddrDC(self.x + 1, self.y - 1)

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

def as_xo(v: int) -> str:
    if v < 0:
        return 'O'
    elif v > 0:
        return 'X'
    else:
        return '.'

@dataclass(frozen=True)
class BadPPainterAddr(Exception):
    x: int
    y: int

    def __str__(self) -> str:
        return f'Bad PPainter location: ({self.x}, {self.y})'

@dataclass(frozen=True)
class BadAddr(IndexError):
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

    MAX_CLARITY: int = 6

    #def __getitem__(self, a: Addr | int, y: int | None=None) -> int:
    def __getitem__(self, a: Addr) -> int:
#        x, y = as_xy(a)
#        if (
#            x < 1 or x > CANVAS_WIDTH
#            or
#            y < 1 or y > CANVAS_HEIGHT
#        ):
#            raise BadAddr.make(a, y)
#        return self.pixels[CANVAS_HEIGHT - y][x - 1]
        i, j = self.addr_to_indices(a)
        return self.pixels[i][j]

    #def __setitem__(self, a: Addr | int, y: int | None, v: int | None=None) \
    def __setitem__(self, a: Addr, v: int) \
    -> None:
        '''Sets cell (x, y) to v. Same calling convention for (x, y) as
        .__getitem__().'''
#        xx: int
#        yy: int
#        vv: int
#        if v is None:
#            assert(isinstance(y, int))
#            vv = y
#            xx, yy = as_xy(a)
#        else:
#            vv = v
#            xx, yy = as_xy(a, y)
#        if (
#            xx < 1 or xx > CANVAS_WIDTH
#            or
#            yy < 1 or yy > CANVAS_HEIGHT
#        ):
#            raise BadAddr.make(xx, yy)
#        self.pixels[CANVAS_HEIGHT - yy][xx - 1] = vv
#        if vv == 0:
#            self.clarities[CANVAS_HEIGHT - yy][xx - 1] = 0
        i, j = self.addr_to_indices(a)
        self.pixels[i][j] = v

    def addr_to_indices(self, a: Addr | int, y: int | None=None) \
    -> Tuple[int, int]:
        '''Returns tuple of indices to access the element in either .pixels
        or .clarities corresponding to 'a'. Raises BadAddr if 'a' is out
        of range.'''
        x, y = as_xy(a, y)
        if (
            x < 1 or x > CANVAS_WIDTH
            or
            y < 1 or y > CANVAS_HEIGHT
        ):
            raise BadAddr.make(x, y)
        return CANVAS_HEIGHT - y, x - 1

    def paint(self, a: Addr, v: int) -> None:
        #oldv = self[a]
        #oldc =
        pass
        
    def all_addrs(self) -> Iterable[A]:
        for y in range(CANVAS_HEIGHT, 0, -1):
            for x in range(1, CANVAS_WIDTH + 1):
                yield A(x, y)

    def all_2x2_addrs(self) -> Iterable[A]:
        for y in range(CANVAS_HEIGHT, 1, -1):
            for x in range(1, CANVAS_WIDTH):
                yield A(x, y)

    def reweight(self, match_wt: Numeric) -> Numeric:
        '''Reweights a painter's match_wt to give huge emphasis to 19.'''
        #lo(match_wt, match_wt < 12)
        if match_wt < 12:
            return match_wt / 12.0
        elif match_wt <= 16:
            return (match_wt - 11) * 250
        else:
            return 5 - (20 - match_wt)

    def pp_weights_everywhere(self, pps: Collection[PPainter]) \
    -> Iterable[Tuple[Tuple[PPainter, A], Numeric]]:
        for a in self.all_2x2_addrs():
            for pp in pps:
                wt = pp.match_wt(self, a)
                if wt > 0:
                    yield ((pp, a), self.reweight(wt))

    def choose_painter(self, pps: Collection[PPainter]): # -> PPainter:
        # TODO Should return Optional[PPainter]
        pairs, weights = zip(*self.pp_weights_everywhere(pps))
        i = choices(range(len(pairs)), weights)[0]
        lo(pairs[i], weights[i])
        return pairs[i]
        
    def blank_all_but(self, *addrs: Addr) -> None:
        addrs: Set[Addr] = set(as_addrdc(a) for a in addrs)
        for a in self.all_addrs():
            if a not in addrs:
                self[a] = 0

    @classmethod
    def from_data(cls, d: CanvasData) -> Canvas:
        # TODO clarity should be 0 for pixels with value 0
        return Canvas(cls.canvasdata_to_lists(d), [[4] * 8] * 8)

    @classmethod
    def canvasdata_to_lists(cls, d: CanvasData) -> List[List[int]]:
        '''Copies 'd' to a list of lists.'''
        return [list(row) for row in d]

    def __str__(self) -> str:
        return '\n'.join(
            ''.join(as_xo(self[x, y]) for x in range(1, CANVAS_WIDTH + 1))
                for y in range(CANVAS_HEIGHT, 0, -1)
        )

@dataclass(frozen=True)
class PPainter:
    ul: int  # value of upper left pixel.
             # 1 or -1 for black or white; 0 for don't know
    ur: int  # etc.
    ll: int
    lr: int

    def paint(self, c: Canvas, addr: Addr) -> None:
        for a, v in zip(as_addrdc(addr).sq2x2(), self.values()):
            c[a] = v

    @classmethod
    def from_canvas(cls, c: Canvas, a: Addr) -> PPainter:
        '''Returns a PPainter that draws the 2x2 square of pixels contained
        in 'c' with upper left corner at 'a'.'''
        x, y = as_xy(a)
        return PPainter(
            c[x, y], c[x+1, y], c[x, y-1], c[x+1, y-1]
        )

    def match_wt(self, c: Canvas, addr: Addr) -> int:
        '''How well does this painter match the canvas at 'addr'? The returned
        value is the sum of match scores for each pixel: 5 points for an exact
        match with a non-zero value on the canvas, 1 point for a zero on the
        canvas, and no points for non-zero value that disagrees.'''
        result = 0
        for a, v in zip(as_addrdc(addr).sq2x2(), self.values()):
            cv = c[a]
            if cv == 0:
                result += 1
            elif cv == v:
                result += 5
        return result

    def values(self) -> Iterable[int]:
        '''Returns an iterable containing the values of this painter, in
        row-major order.'''
        yield self.ul
        yield self.ur
        yield self.ll
        yield self.lr

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.as_xos()})'

    def as_xos(self) -> str:
        return ''.join(as_xo(v) for v in self.values())

Painter = PPainter   # union this with QPainter, DPainter, anything else

VarAddr = str
PMatcher = Tuple[VarAddr, Painter]  # Matches a Painter at an Addr

@dataclass(frozen=True)
class QPainter:
    triple1: PPainter
    triple2: PPainter


def make_ppainters(c: Canvas) -> Iterable[PPainter]:
    for a in c.all_2x2_addrs():
        yield PPainter.from_canvas(c, a)

def pps_to_qqs(c: Canvas, pps: Collection[PPainter]) -> Iterable[QPainter]:
    pass


if __name__ == '__main__':
    c = Canvas.from_data(two_cs)
    c.blank_all_but((2, 7), (3, 7))
    print(str(c))
    print()
    p = PPainter(-1, -1, -1, -1)
    p.paint(c, (3, 7))
    print(str(c))
    print()

    '''
    c = Canvas.from_data(two_cs)
    pps = list(make_ppainters(c))
    #pts(pps)
    print(pps[0].match_wt(c, (1, 8)))
    print(pps[0].match_wt(c, (5, 8)))
    print()
    pps = set(pps)
    c.blank_all_but((2, 7), (3, 7))
    tups = list(c.pp_weights_everywhere(pps))
    pts(tups)
    print()

    for _ in range(30):
        p = c.choose_painter(pps)
        print(p)
    '''
