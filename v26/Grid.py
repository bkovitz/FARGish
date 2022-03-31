# Grid.py -- PPainters, and QPainters, and the Canvas is a grid

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from random import choices
from copy import deepcopy

from util import Numeric, pts, sample_without_replacement, union
from Log import lo, trace


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

AVar = str
ATerm = AVar | int
AOpExpr = Tuple[ATerm, Literal['+', '-'], ATerm]
AExpr = Union[ATerm, AOpExpr]

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
    pixels: List[List[int]]     # [row][column], [0][0] is upper left
    clarities: List[List[int]]  # [row][column], [0][0] is upper left

    MAX_CLARITY: int = 6

    #def __getitem__(self, a: Addr | int, y: int | None=None) -> int:
    def __getitem__(self, a: Addr) -> int:
        i, j = self.addr_to_indices(a)
        return self.pixels[i][j]

    #def __setitem__(self, a: Addr | int, y: int | None, v: int | None=None) \
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

    # TODO rm?
    def reweight(self, match_wt: Numeric) -> Numeric:
        '''Reweights a painter's match_wt to give huge emphasis to 19.'''
        #lo(match_wt, match_wt < 12)
        if match_wt < 12:
            return match_wt / 12.0
        elif match_wt <= 16:
            return (match_wt - 11) * 250
        else:
            return 5 - (20 - match_wt)

    # TODO rm?
    def OLDpp_weights_everywhere(self, pps: Collection[PPainter]) \
    -> Iterable[Tuple[Tuple[PPainter, A], Numeric]]:
        for a in self.all_2x2_addrs():
            for pp in pps:
                wt = pp.match_wt(self, a)
                if wt > 0:
                    yield ((pp, a), self.reweight(wt))

    def pp_weights_everywhere(self, pps: Collection[PPainter]) \
    -> Iterable[Tuple[Tuple[PPainter, A], Numeric]]:
        for a in self.all_2x2_addrs():
            for pp in pps:
                wt = self.painter_wt(pp, a)
                if wt > 0:
                    #lo('PPW', (pp, a), wt, '   ', list(pp.match_types(self, a)))
                    yield ((pp, a), wt)

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
                
    def choose_painter(self, pps: Collection[PPainter]) -> Tuple[PPainter, A]:
        # TODO Should return Optional[PPainter]
        # TODO Optionally, print sorted list of choices, with info
        # TODO ChoiceInfo
        pairs, weights = zip(*self.pp_weights_everywhere(pps))
        i = choices(range(len(pairs)), weights)[0]
        #lo('CHOSE', pairs[i], weights[i])
        return pairs[i]
        
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

    def regenerate(self, pset: Set[PPainter], niters: int=20) -> None:
        '''Loops through choosing painters and letting them paint.'''
        for i in range(niters):
            pp, a = self.choose_painter(pset)
            pp.paint(self, a)

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

@dataclass(frozen=True)
class QPainter:
    '''A PPainter with a specific Addr that it paints to.'''
    a: Addr
    ppainter: PPainter

    def paint(self, c: Canvas) -> None:
        self.ppainter.paint(c, self.a)

    @classmethod
    def derive_from_canvas(cls, c: Canvas) -> Iterable[QPainter]:
        for a in c.all_2x2_addrs():
            yield QPainter(a, PPainter.from_canvas(c, a))

@dataclass(frozen=True)
class QPainterTemplate:
    '''Like a QPainter, but the Addr is only a template, containing variables
    rather than absolute addresses.'''
    a: Tuple[AExpr, AExpr]
    ppainter: PPainter

    def make_env(self, qp: QPainter) -> Optional[Tuple[Subst, Subst]]:
        qpx, qpy = as_xy(qp.a)
        ex, ey = self.a
        ux = unify([ex], [qpx])
        uy = unify([ey], [qpy])
        if ux and uy:
            return ux, uy

    def make_qpainter(self, env: Tuple[Subst, Subst]) -> QPainter:
        abs_addr = tuple(eval_ae(s, a) for s, a in zip(env, self.a))
        # TODO eval_ae() fail?
        return QPainter(
            abs_addr,
            self.ppainter
        )

@dataclass(frozen=True)
class RPainter:
    '''A painter that matches a QPainter and paints one or more other
    QPainters.'''
    qpts: Tuple[QPainterTemplate, ...]
    
    def make_qpainters(self, qp: QPainter) -> Iterable[QPainter]:
        for qpt in self.qpts:
            if qpt.ppainter == qp.ppainter:
                subs = qpt.make_env(qp)
                if subs:
                    for other_qpt in self.qpts:  # all except qpt
                        if qpt == other_qpt:
                            continue
                        qp_out = other_qpt.make_qpainter(subs)
                        if Canvas.is_valid_2x2_addr(qp_out.a):
                            yield qp_out

Painter = PPainter   # union this with QPainter, DPainter, anything else

VarAddr = str
PMatcher = Tuple[VarAddr, Painter]  # Matches a Painter at an Addr

'''
@dataclass(frozen=True)
class QPainter:
    triple1: PPainter
    triple2: PPainter
'''

def make_ppainters(c: Canvas) -> Iterable[PPainter]:
    for a in c.all_2x2_addrs():
        yield PPainter.from_canvas(c, a)

def pps_to_qqs(c: Canvas, pps: Collection[PPainter]) -> Iterable[QPainter]:
    pass

c: Canvas
pps: Set[PPainter]

def go(niters: int=1) -> None:
    global c, pps
    for _ in range(niters):
        c.regenerate(pps, niters=1)
        print(str(c))
        print()
        print(c.claritystr())
        print()

def et(e: AExpr) -> None:
    '''AExpr test.'''
    print(e)
    match e:
        case name if isinstance(name, str):
            print('name:', name)
        case n if isinstance(n, int):
            print('n:', n)
        case (a, op, b):
            print(f'a: {a}, op: {op}, b: {b}')

def extract_offset(e: AExpr) -> int:
    match e:
        case name if isinstance(name, str):
            return 0
        case (a, op, b) if isinstance(a, str) and isinstance(b, int):
            return b
        case (a, op, b) if isinstance(a, int) and isinstance(b, str):
            return a
        case _:
            raise ValueError(f'extract_offset: Can\'t find offset in {e}')

@dataclass(frozen=True)
class Subst:
    '''A 'substitution': a variable and the value to give it.'''
    var: AVar
    v: int

    def __str__(self) -> str:
        return f'{self.var}={self.v}'

def eval_ae(s: Subst, e: AExpr) -> int:
    '''Returns value of 'e' given 's'. Ignores the name of the variable in
    's'.'''
    return extract_offset(e) + s.v

def unify(es: Sequence[AExpr], ns: Sequence[int]) -> Optional[Subst]:
    '''Poor man's unification.'''
    offsets = [extract_offset(e) for e in es]
    mino = min(offsets)
    normalized_offsets = [o - mino for o in offsets]
    minn = min(ns)
    normalized_ns = [n - minn for n in ns]
    if normalized_ns == normalized_offsets:
        return Subst('x', ns[0] - offsets[0])
            
            
if __name__ == '__main__':
    c = Canvas.from_data(two_cs)
    qps = set(QPainter.derive_from_canvas(c))
    pts(qps)
    print()
    e: AExpr = ('x', '+', 2)
    #et(e)
    #et('x')
    #et(2)
    p1 = PPainter(-1, -1, -1, 1)
    p2 = PPainter(-1, -1, 1, 1)
    qp1 = QPainter((1, 8), p1)
    qp2 = QPainter((2, 8), p2)
    qpt1 = QPainterTemplate(
        ('x', 'y'), p1
    )
    qpt2 = QPainterTemplate(
        (('x', '+', 1), 'y'), p2
    )
    #subs = qpt1.make_env(qp1)
    #print(subs)

    rp = RPainter((qpt1, qpt2))
    got = list(rp.make_qpainters(qp1))
    pts(got)

    '''
    got = unify(['x', ('x', '+', 1)], (1, 2))
    print(got)  # want x=1
    got = unify([('x', '+', 1), 'x'], (1, 2))
    print(got)  # want nothing
    got = unify([('x', '+', 1), ('x', '+', 2)], (1, 2))
    print(got)  # want x=0
    '''

    '''
    pps = set(make_ppainters(c))
    lpps = list(pps)

    print(c)
    print()
    c.blank_random()
    print(c)
    '''

    """
    save = [
        (1, 8), (2, 8), (3, 8), (4, 8),
        (1, 7), (2, 7), (3, 7),
                (2, 6), (3, 6)
    ]
    c.blank_all_but(save)
    print(str(c))
    print()
    print(c.claritystr())
    print()
    pts(lpps)

    p1 = lpps[1]
    p2 = lpps[-1]

    print(c.painter_wt(p1, (2, 8)))
    print(c.painter_wt(p2, (2, 7)))
    #c.regenerate(pps, niters=100)
    #print(str(c))
    #print()
    """

    # NEXT Call c.regenerate()

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
