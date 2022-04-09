# Grid2.py -- Refactoring of Grid2.py, 06-Apr-2022

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import choice, choices
from copy import deepcopy
from itertools import chain, product as cartesian_product
from abc import ABC, abstractmethod
from collections import defaultdict
from operator import itemgetter

from util import Numeric, pts, nf, sample_without_replacement, union, \
    unique_everseen, sstr, first
from Log import lo, trace

@dataclass
class Canvas:
    '''8x8 grid with clarities, [x, y], [1, CANVAS_HEIGHT] is upper left.'''
    pixels: List[List[int]]     # [row][column], [0][0] is upper left
    clarities: List[List[int]]  # [row][column], [0][0] is upper left

    MAX_CLARITY: int = 6

    def __getitem__(self, a: Addr) -> int:
        i, j = self.addr_to_indices(a)
        return self.pixels[i][j]

    def __setitem__(self, a: Addr, v: int) -> None:
        '''Sets cell (x, y) to v. Same calling convention for (x, y) as
        .__getitem__().'''
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
        i, j = self.addr_to_indices(a)
        oldv = self.pixels[i][j]
        if oldv == v:
            if self.clarities[i][j] < self.MAX_CLARITY:
                self.clarities[i][j] += 1
        elif oldv == 0:
            if v != 0:
                self.pixels[i][j] = v
                self.clarities[i][j] = 1
        else:  # else there is a different value there
            clarity = self.clarities[i][j]
            clarity -= 1
            if clarity < 1:
                self.pixels[i][j] = 0  # i.e. unknown pixel value
                self.clarities[i][j] = 0
            else:
                self.clarities[i][j] = clarity
        
    def clarity(self, a: Addr | int, y: int | None=None) -> int:
        i, j = self.addr_to_indices(a, y)
        return self.clarities[i][j]

    def set_clarity(self, a: Addr, clarity: int) -> None:
        i, j = self.addr_to_indices(a)
        self.clarities[i][j] = clarity

    def all_addrs(self) -> Iterable[A]:
        for y in range(CANVAS_HEIGHT, 0, -1):
            for x in range(1, CANVAS_WIDTH + 1):
                yield A(x, y)

    def all_2x2_addrs(self) -> Iterable[A]:
        for y in range(CANVAS_HEIGHT, 1, -1):
            for x in range(1, CANVAS_WIDTH):
                yield A(x, y)

    @classmethod
    def is_valid_2x2_addr(cls, a: Addr) -> bool:
        x, y = as_xy(a)
        return x >= 1 and x <= 7 and y >= 2 and y <= 8

    already_all_painted = [
        'SameValue', 'SameValue', 'SameValue', 'SameValue'
    ]

    num_matches_weights = {
        0: 0.0,
        1: 1.0,
        2: 5.0,
        3: 10.0,
        4: 1.0
    }

    # TODO Move this to Weigher?
    def painter_wt(self, p: PPainter, addr: Addr) -> Numeric:
        source_strength = 0.0
        target_strength = 0.0
        num_targets = 0
        for a, mtype in p.match_types(self, addr):
            cl = self.clarity(a)
            w = cl / self.MAX_CLARITY
            if mtype != 'SameValue':
                target_strength += w + 0.01
                num_targets += 1
            else:
                if cl <= 2:
                    target_strength += 2 * w + 0.01
                    num_targets += 1
                else:
                    source_strength += w
        if num_targets:
            return source_strength / target_strength
        else:
            return 0
                
    def blank_addr(self, a: Addr) -> None:
        self[a] = 0
        self.set_clarity(a, 0)  # TODO OAOO

    def blank_all_but(self, addrs: Iterable[Addr]) -> None:
        addrs: Set[Addr] = set(as_addrdc(a) for a in addrs)
        for a in self.all_addrs():
            if a not in addrs:
                self[a] = 0
                self.set_clarity(a, 0)

    def blank_random(self, num=4) -> None:
        coords = list(self.all_2x2_addrs())
        for a in sample_without_replacement(coords, k=4):
            self.blank_addr(a)
            #self[a] = 0
            #self.set_clarity(a, 0)

    @classmethod
    def from_data(cls, d: CanvasData) -> Canvas:
        return Canvas(
            cls.canvasdata_to_lists(d),
            #[[0] * 8 for _ in range(8)]
            [cls.initial_row_clarities(row) for row in d]
        )

    @classmethod
    def initial_row_clarities(cls, row: Sequence[int]) -> List[int]:
        '''Initial clarities for a row initialized in .from_data(): each
        non-zero value gets a clarity of 4; each zero value gets a clarity
        of 0.'''
        return [0 if v == 0 else 4 for v in row]

    @classmethod
    def canvasdata_to_lists(cls, d: CanvasData) -> List[List[int]]:
        '''Copies 'd' to a list of lists.'''
        return [list(row) for row in d]

    @classmethod
    def empty(self) -> Canvas:
        return Canvas(
            [[0] * 8 for _ in range(8)],
            [[0] * 8 for _ in range(8)]
        )

    def levdist(self, other: Canvas) -> Numeric:
        '''Levenshtein distance between 'self' and 'other', counting a blank
        in one Canvas corresponding to a non-blank in the other Canvas as only
        0.5 wrong. Insertions and deletions are not possible.'''
        result: Numeric = 0
        for a in self.all_addrs():
            x = self[a]
            y = other[a]
            if x != y:
                if x == 0 or y == 0:
                    result += 0.5
                else:
                    result += 1
        return result

    def __str__(self) -> str:
        return '\n'.join(
            ''.join(' ' + as_xo(self[x, y]) for x in range(1, CANVAS_WIDTH + 1))
                for y in range(CANVAS_HEIGHT, 0, -1)
        )

    def claritystr(self) -> str:
        '''Returns a string representing all the clarities as a matrix.'''
        return '\n'.join(
            ''.join(f'{self.clarity(x, y):2d}'
                for x in range(1, CANVAS_WIDTH + 1))
                    for y in range(CANVAS_HEIGHT, 0, -1)
        )

    def pr(self) -> None:
        '''Prints both the Canvas and its claritystr().'''
        print(self)
        print()
        print(self.claritystr())
        print()


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

def xo_to_num(xo: str) -> int:
    if xo == 'X':
        return +1
    elif xo == 'O':
        return -1
    elif xo == '.':
        return 0
    else:
        raise ValueError(f'xo_to_num: invalid xo: {xo!r}')

def are_within_radius(addr1: Addr, addr2: Addr, radius: int) -> bool:
    x1, y1 = as_xy(addr1)
    x2, y2 = as_xy(addr2)
    return abs(x1 - x2) <= radius and abs(y1 - y2) <= radius


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

MatchType = Literal['Target0', 'SameValue', 'DifferentValue']

@dataclass(frozen=True)
class PPainter:
    ul: int  # value of upper left pixel.
             # 1 or -1 for black or white; 0 for don't know
    ur: int  # etc.
    ll: int
    lr: int

    def paint(self, c: Canvas, addr: Addr) -> None:
        for a, v in zip(as_addrdc(addr).sq2x2(), self.values()):
            #c[a] = v
            c.paint(a, v)

    @classmethod
    def from_canvas(cls, c: Canvas, a: Addr) -> PPainter:
        '''Returns a PPainter that draws the 2x2 square of pixels contained
        in 'c' with upper left corner at 'a'.'''
        x, y = as_xy(a)
        return PPainter(
            c[x, y], c[x+1, y], c[x, y-1], c[x+1, y-1]
        )

    @classmethod
    def multi_from_canvas(cls, c: Canvas, a: Addr) -> Iterable[PPainter]:
        '''Returns zero or more PPainters that draw the 2x2 square of pixels
        contained in 'c' with upper left corner at 'a'. If all four pixels
        are blank, returns an empty set. Otherwise, for each blank cell,
        returns two painters: one to draw -1 and one to draw +1.'''
        addrs = list(as_addrdc(a).sq2x2())
        vals = [c[a] for a in addrs]
        if sum(1 for v in vals if v == 0) == 4:
            return
        else:
            for vs in cartesian_product(*(
                [v] if v != 0 else [-1, +1]
                    for v in vals
            )):
                yield PPainter(*vs)

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

    # TODO UT
    def match_types(self, c: Canvas, addr: Addr) \
    -> Iterable[Tuple[A, MatchType]]:
        '''Returns an iterable of four tuples (A, MatchType), corresponding to
        the matches between the painter cells and the canvas cells as 'addr',
        in row-major order.'''
        for a, v in zip(as_addrdc(addr).sq2x2(), self.values()):
            cv = c[a]
            if cv == 0:
                yield (a, 'Target0')
            elif cv == v:
                yield (a, 'SameValue')
            else:
                yield (a, 'DifferentValue')

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.as_xos()})'

    def as_xos(self) -> str:
        return ''.join(as_xo(v) for v in self.values())

    @classmethod
    def from_xos(cls, xos: str) -> PPainter:
        return PPainter(*(
            xo_to_num(xo) for xo in xos
        ))

P = PPainter.from_xos


#@dataclass
#class ModelState:
#    ltsoup: LongTermSoup
#    wsoup: WorkingSoup
#    c: Canvas
#
#def iterate(m: ModelState) -> None:
#    m.ltsoup.decay()
#    m.wsoup.decay()
#    painters = list(chain(
#        m.ltsoup.all_painters(m),
#        m.wsoup.all_painters(m)
#    ))
#    weights = [weight_of(m, p) for p in painters]
#    # TODO Create painter
#    p = choices(painters, weights)[0]
#    p.paint(m)
#
#def weight_of(m: ModelState, p: MPainter) -> float:
#    match p:
#        case PPainter():
#            lo('PPainter')
#    return 0.0 # TODO

#@dataclass
#class LongTermSoup:
#    rpainters: Dict[RPainter, float]
#        # RPainter -> activation level
#
#    def all_painters(self, m: ModelState) -> Iterable[RPRQ]:
#        return (
#            RPQP(rp, qp)
#                for rp, qp in cartesian_product(
#                    self.rpainters, m.wsoup.qpainters
#                )
#                    if rp.is_match(qp)
#        )
#
#    def decay(self, factor: float=0.9) -> None:
#        for rp in self.rpainters:
#            self.rpainters[rp] *= factor
#
#    
#@dataclass
#class WorkingSoup:
#    qpainters: Dict[QPainter, float] = \
#        field(default_factory=lambda: defaultdict(float))
#        # QPainter -> activation level
#
#    def all_painters(self, m: Model) -> Iterable[QPainter]:
#        return self.qpainters.keys()
#
#    def decay(self, factor: float=0.9) -> None:
#        for qp in self.qpainters:
#            self.qpainters[qp] *= factor
#
#
#### Painters
#
#@dataclass(frozen=True)
#class PPainter:
#    ul: int  # value of upper left pixel.
#             # 1 or -1 for black or white; 0 for don't know
#    ur: int  # etc.
#    ll: int
#    lr: int
#
#class MPainter(ABC):
#    '''Generic class for a painter that paint to a Model (not necessarily to
#    a Canvas).'''
#
#    @abstractmethod
#    def mpaint(self, m: Model) -> None:
#        pass
#
#    @abstractmethod
#    def boost_target(self, m: Model) -> None:
#        '''What to do after painting: boost the activation level of whatever
#        got painted, if that is appropriate to the type of MPainter.'''
#        pass
#
#@dataclass(frozen=True)
#class RPainter:
#    '''A painter that matches a QPainter and paints one or more other
#    QPainters.'''
#    qpts: Tuple[QPainterTemplate, ...]
#    
#@dataclass(frozen=True)
#class RPQP(MPainter):
#    '''An RPainter and a QPainter.'''
#    rpainter: RPainter
#    qpainter: QPainter
#
#    def weight(self, m: Model) -> float:
#        return (
#            m.ltsoup.activation(self.rpainter)
#            *
#            weight_of(m, self.qpainter)
#        )
#
#
##class Model:
##
##
##class MPainter:
##
##
##class PPainter:
##
##class QPainter:
##
##class RPainter:
##
##class RPQP:
##
##
##class Weigher:
##
##
##class WorkingSoup:
##
##class LongTermSoup:
##
##
##
##class Pixel:  # ?
