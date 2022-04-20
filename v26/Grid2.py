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
    unique_everseen, sstr, first, short
from Log import lo, trace


epsilon = 0.00001

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

### Addresses of canvas cells ###

@dataclass(frozen=True)
class AddrDC:
    '''"Address Dataclass": x,y of a Canvas cell. (1, 8) is upper left.'''
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

### Expressions and substitutions ###

AVar = str
ATerm = AVar | int
AOpExpr = Tuple[ATerm, Literal['+', '-'], ATerm]
AExpr = Union[ATerm, AOpExpr]

def difference_aexpr(val1: int, val2: int, varname: str) -> AExpr:
    '''Returns AExpr for val1 - val2, assigning varname to val2.'''
    if val1 == val2:
        return varname
    elif val1 < val2:
        return (varname, '+', val2 - val1)
    else:
        return (varname, '-', val1 - val2)

def astr(e: AExpr) -> str:
    if isinstance(e, AVar):
        return e
    elif isinstance(e, int):
        return str(e)
    elif isinstance(e, tuple):
        return ''.join(astr(ee) for ee in e)
    else:
        raise ValueError(f'astr({e!r})')

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

# e: AExpr  Bug in mypy 0.942
def extract_offset(e: Union[str, int, tuple]) -> int:  # type: ignore[return]
    '''Returns the 'offset' part an an AExpr, e.g. the 2 in (x, '+', 2), or
    0 if the AExpr contains no offset.'''
    match e:
        case name if isinstance(name, str):
            return 0
        case (str(a), op, int(b)):
            if op == '+':
                return b
            else:
                return -b
        case (int(a), op, str(b)):
            if op == '+':
                return a
            else:
                return -a
        case _:
            raise ValueError(f'extract_offset: Can\'t find offset in {e}')

def unify(es: Sequence[AExpr], ns: Sequence[int]) -> Optional[Subst]:
    '''Poor man's unification.'''
    offsets = [extract_offset(e) for e in es]
    mino = min(offsets)
    normalized_offsets = [o - mino for o in offsets]
    minn = min(ns)
    normalized_ns = [n - minn for n in ns]
    if normalized_ns == normalized_offsets:
        return Subst('x', ns[0] - offsets[0])
    else:
        return None
            
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

#    def match_wt(self, c: Canvas, addr: Addr) -> int:
#        '''How well does this painter match the canvas at 'addr'? The returned
#        value is the sum of match scores for each pixel: 5 points for an exact
#        match with a non-zero value on the canvas, 1 point for a zero on the
#        canvas, and no points for non-zero value that disagrees.'''
#        result = 0
#        for a, v in zip(as_addrdc(addr).sq2x2(), self.values()):
#            cv = c[a]
#            if cv == 0:
#                result += 1
#            elif cv == v:
#                result += 5
#        return result

    def values(self) -> Iterable[int]:
        '''Returns an iterable containing the values of this painter, in
        row-major order.'''
        yield self.ul
        yield self.ur
        yield self.ll
        yield self.lr

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

    def short(self) -> str:
        return self.as_xos()

    def as_xos(self) -> str:
        return ''.join(as_xo(v) for v in self.values())

    sstr = as_xos

    @classmethod
    def from_xos(cls, xos: str) -> PPainter:
        return PPainter(*(
            xo_to_num(xo) for xo in xos
        ))

P = PPainter.from_xos


@dataclass(frozen=True)
class WeightScheme:
    empty_cell_weight: Numeric = 1
    matched_cell_weight: Numeric = 5

    def weight_of(self, m: Model, p: MPainter) -> float:
        match p:
            case QPainter():
                return self.qpainter_weight(m, p)
            case RPQP(rp, qp):
                return (
                    m.ltsoup.activation(rp)
                    *
                    self.weight_of(m, qp)
                )
            case _:
                raise NotImplementedError(p)

    def qpainter_weight(self, m: Model, qp: QPainter) -> float:
        return (
            m.wsoup.activation(qp)
            *
            self.ppainter_weight(m.c, qp.ppainter, qp.a)
        )

    def ppainter_weight(self, c: Canvas, pp: PPainter, addr: Addr) -> Numeric:
        source_strength = 0.0
        target_strength = 0.0
        num_targets = 0
        for a, mtype in pp.match_types(c, addr):
            cl = c.clarity(a)
            w = cl / c.MAX_CLARITY
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
        

    def pp_match_weight(self, c: Canvas, pp: PPainter, addr: Addr) -> Numeric:
        '''How well does this painter match the canvas at 'addr'?'''
        result: Numeric = 0
        for a, v in zip(as_addrdc(addr).sq2x2(), pp.values()):
            canvas_value = c[a]
            if canvas_value == 0:
                result += self.empty_cell_weight
            elif canvas_value == v:
                result += self.matched_cell_weight
        return result

default_weight_scheme = WeightScheme()

@dataclass
class Model:
    ltsoup: LongTermSoup
    wsoup: WorkingSoup
    c: Canvas
    wts: WeightScheme = field(default=default_weight_scheme)

    def decay(self) -> None:
        self.ltsoup.decay()
        self.wsoup.decay()

    def boost(self, p: MPainter) -> None:
        match p:
            case QPainter():
                self.wsoup.boost(p)
            case _:
                raise NotImplementedError(p)

    def deboost(self, p: MPainter | RPainter) -> None:
        match p:
            case QPainter():
                self.wsoup.deboost(p)
            case RPainter():
                self.ltsoup.deboost(p)
            case RPQP(rp, qp):
                self.deboost(rp)
            case _:
                raise NotImplementedError(p)

    def all_painters(self) -> Sequence[MPainter]:
        return list(chain(
            self.ltsoup.all_painters(self),
            self.wsoup.all_painters(self),
        ))

    def iteration(self) -> None:
        self.decay()
        painters = self.all_painters()
        weights = [self.weight_of(self, p) for p in painters]
        # TODO What if no painters or 0 weight?
        #pts(painters, key=short)
        i = choices(range(len(painters)), weights)[0]
        lo('CHOSE', f'{short(painters[i])}  {weights[i]:8.3f}')
        p = painters[i]
        p.mpaint(self)
        p.boost_target(self)
        self.deboost(p)

    def weight_of(self, p: MPainter) -> float:
        return self.wts.weight_of(self, p)

    def pr(self) -> None:
        print(self.c)
        print()
        print(self.c.claritystr())
        print()

#        m.ltsoup.all_painters(m),
#        m.wsoup.all_painters(m)
#    ))
#    weights = [weight_of(m, p) for p in painters]
#    # TODO Create painter
#    p = choices(painters, weights)[0]
#    p.paint(m)
#
#def weight_of(m: Model, p: MPainter) -> float:
#    match p:
#        case PPainter():
#            lo('PPainter')
#    return 0.0 # TODO

#@dataclass
#class LongTermSoup:
#    rpainters: Dict[RPainter, float]
#        # RPainter -> activation level
#
#    def all_painters(self, m: Model) -> Iterable[RPRQ]:
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
@dataclass
class WorkingSoup:
    qpainters: Dict[QPainter, float] = \
        field(default_factory=lambda: defaultdict(float))
        # QPainter -> activation level

    def add(self, qp: QPainter) -> None:
        self.qpainters[qp] += 100.0

    def all_painters(self, m: Model) -> Iterable[QPainter]:
        return self.qpainters.keys()

    def decay(self, factor: float=0.9) -> None:
        for qp in self.qpainters:
            self.qpainters[qp] *= factor

    def activation(self, qp: QPainter) -> float:
        return self.qpainters.get(qp, 0.0)

    def boost(self, qp: QPainter) -> None:
        if qp in self.qpainters:
            self.qpainters[qp] += 100.0

    def deboost(self, qp: QPainter) -> None:
        if qp in self.qpainters:
            self.qpainters[qp] *= 0.1
            if self.qpainters[qp] < epsilon:
                del self.qpainters[qp]

    @classmethod
    def make_from_canvas(cls, c: Canvas) -> WorkingSoup:
        result = WorkingSoup()
        for qp in QPainter.derive_from_canvas(c):
            # TODO Higher activation for repeated QPainters?
            result.qpainters[qp] = 1.0
        return result

#### Painters

class MPainter(ABC):
    '''Generic class for a painter that paint to a Model (not necessarily to
    a Canvas).'''

    @abstractmethod
    def mpaint(self, m: Model) -> None:
        pass

    @abstractmethod
    def boost_target(self, m: Model) -> None:
        '''What to do after painting: boost the activation level of whatever
        got painted, if that is appropriate to the type of MPainter.'''
        pass

    @abstractmethod
    def weight(self, m: Model) -> float:
        pass

@dataclass(frozen=True)
class QPainter(MPainter):
    '''A PPainter with a specific Addr that it paints to.'''
    a: Addr
    ppainter: PPainter

    def paint(self, c: Canvas) -> None:
        self.ppainter.paint(c, self.a)

    def mpaint(self, m: Model) -> None:
        return self.paint(m.c)

    def boost_target(self, m: Model) -> None:
        pass

    def weight(self, m: Model) -> float:
        return (
            m.wsoup.activation(self)
            *
            m.c.painter_wt(self.ppainter, self.a)
        )

    @classmethod
    def derive_from_canvas(cls, c: Canvas) -> Iterable[QPainter]:
        for a in c.all_2x2_addrs():
            #yield QPainter(a, PPainter.from_canvas(c, a))
            for pp in PPainter.multi_from_canvas(c, a):
                yield QPainter(a, pp)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.a}, {self.ppainter.as_xos()})'

    def short(self) -> str:
        x, y = as_xy(self.a)
        return f'({x}, {y}, {self.ppainter.as_xos()})'

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
        else:
            return None

    def make_qpainter(self, env: Tuple[Subst, Subst]) -> QPainter:
        abs_addr = tuple(eval_ae(s, a) for s, a in zip(env, self.a))
        # TODO eval_ae() fail?
        return QPainter(
            abs_addr,  # type: ignore[arg-type]
            self.ppainter
        )

    '''
    def weight(self, m: Model) -> float:
        return max(
            (qp.weight(m) for qp in m.matching_qpainters(self.ppainter)),
            default=0.0
        )
    '''

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f"{cl}({', '.join(astr(aa) for aa in self.a)}, {sstr(self.ppainter)})"

    def sstr(self) -> str:
        return f'({astr(self.a[0])}, {astr(self.a[1])}, {self.ppainter.as_xos()})'

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

    def is_match(self, x: Any) -> bool:
        return isinstance(x, QPainter) and x.ppainter in self.ppainters()

    def ppainters(self) -> Iterable[PPainter]:
        return (qpt.ppainter for qpt in self.qpts)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({sstr(self.qpts)})'

    def sstr(self) -> str:
        return ' <-> '.join(sstr(qpt) for qpt in self.qpts)

    @classmethod
    def derive_from_qpainters(
        cls, qpainters: Collection[QPainter], radius: int=1
    ) -> Set[RPainter]:
        result: Set[RPainter] = set()
        for qp1, qp2 in cls.qpainter_pairs(qpainters, radius):
            x1, y1 = as_xy(qp1.a)
            x2, y2 = as_xy(qp2.a)
            result.add(RPainter((
                QPainterTemplate(('x', 'y'), qp1.ppainter),
                QPainterTemplate(
                   (difference_aexpr(x2, x1, 'x'),
                    difference_aexpr(y2, y1, 'y')),
                   qp2.ppainter
                )
            )))
        return result
            
    @classmethod
    def qpainter_pairs(cls, qpainters: Collection[QPainter], radius: int=1) \
    -> Set[FrozenSet[QPainter]]:
        '''Returns a generator containing unordered pairs (sets of two) of
        'qpainters' that are within 'radius' of each other.'''
        result: Set[FrozenSet[QPainter]] = set()
        for qp1, qp2 in cartesian_product(qpainters, qpainters):
            if qp1 != qp2:
                pair = frozenset((qp1, qp2))
                if pair not in result:
                    if are_within_radius(qp1.a, qp2.a, radius):
                        result.add(pair)
        return result

@dataclass(frozen=True)
class RPQP(MPainter):
    '''An RPainter and a QPainter.'''
    rpainter: RPainter
    qpainter: QPainter

    def weight(self, m: Model) -> float:
        return (
            m.ltsoup.activation(self.rpainter)
            #self.rpainter.weight(m)
            *
            #m.wsoup.activation(self.qpainter)
            self.qpainter.weight(m)
        )

    def mpaint(self, m: Model) -> None:
        for qp in self.rpainter.make_qpainters(self.qpainter):
            m.wsoup.add(qp)

    def boost_target(self, m: Model) -> None:
        m.boost(self.qpainter)
        lo('BOOSTED', f'{self.qpainter.weight(m):8.3f}', self.qpainter)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({sstr(self.rpainter)}, {sstr(self.qpainter)})'

    def sstr(self) -> str:
        return f'{sstr(self.rpainter)}  =>  {sstr(self.qpainter)}'

@dataclass
class LongTermSoup:
    rpainters: Dict[RPainter, float]
        # RPainter -> activation level

    def all_painters(self, m: Model) -> Iterable[RPQP]:
        return (
            RPQP(rp, qp)
                for rp, qp in cartesian_product(
                    self.rpainters, m.wsoup.qpainters
                )
                    if rp.is_match(qp)
        )

    @classmethod
    def make_from_canvas(cls, c: Canvas) -> LongTermSoup:
        result = LongTermSoup({})
        qps = set(QPainter.derive_from_canvas(c))
        for rp in RPainter.derive_from_qpainters(qps):
            # TODO Higher activation for repeated RPainters?
            result.rpainters[rp] = 1.0
        return result

    def activation(self, rp: RPainter) -> float:
        return self.rpainters.get(rp, 0.0)

    def decay(self, factor: float=0.9) -> None:
        for rp in self.rpainters:
            self.rpainters[rp] *= factor

    def boost(self, rp: RPainter) -> None:
        if rp in self.rpainters:
            self.rpainters[rp] += 100.0

    def deboost(self, rp: RPainter) -> None:
        if rp in self.rpainters:
            self.rpainters[rp] *= 0.1

    def __len__(self) -> int:
        return len(self.rpainters)

    def pr(self) -> None:
        for rp, activation in sorted(self.rpainters.items(), key=itemgetter(1)):
            print(f'{activation:8.3f}', rp)


##class Pixel:  # ?

default_seed_addrs = [
    (1, 8), (2, 8), (3, 8), (4, 8),
    (1, 7), (2, 7), (3, 7),
            (2, 6), (3, 6)
]

def make_small_seed_model(
    wts: WeightScheme=default_weight_scheme,
    seed_addrs: Sequence[Addr]=default_seed_addrs,
) -> Model:
    c = Canvas.from_data(two_cs)
    ltsoup = LongTermSoup.make_from_canvas(c)
    c.blank_all_but(seed_addrs)
    return Model(
        ltsoup,
        WorkingSoup.make_from_canvas(c),
        c
    )

def run_small_seed(
    num_t: int=20,
    wts: WeightScheme=default_weight_scheme,
    seed_addrs: Sequence[Addr]=default_seed_addrs,
) -> Model:
    '''Runs the small-seed experiment with given WeightScheme.'''
    m = make_small_seed_model(wts=wts, seed_addrs=seed_addrs)

    m.pr()
    for t in range(1, num_t + 1):
        input()
        print(f't={t}')
        m.iteration()
        m.pr()

    return m

if __name__ == '__main__':
    run_small_seed()
