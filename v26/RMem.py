# RMem.py -- Classes and functions for regenerative memory

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, final
import operator
from random import choice, choices
from collections import defaultdict, Counter
from functools import reduce
import itertools
from io import StringIO
from copy import deepcopy
from abc import ABC, abstractmethod

from Log import lo, trace
from util import pr, ps, pts, psa, union, Numeric, as_tuple, short, as_list, \
    newline, force_setattr, sample_without_replacement, first, is_iter, \
    singleton


epsilon = 0.0001

@singleton
@dataclass(frozen=True)
class Outcome:
    '''Succeeded or Failed.'''
    name: str

    def __str__(self) -> str:
        return self.name

Succeeded = Outcome('Succeeded')
Failed = Outcome('Failed')

class Jumper(ABC):
    @abstractmethod
    def to(self, canvas: Canvas, a: Addr) -> Addr:
        pass

    @abstractmethod
    def could_jump(self, canvas: Canvas, fro: From) -> bool:
        pass

    @abstractmethod
    def could_jump_to(self, c: Canvas, a: Addr) -> Iterable[To]:
        '''Returns all the Addrs that this Jumper could jump to from 'a',
        or, if that can't be determined, returns this Jumper.'''
        pass

class Matcher(ABC):
    @abstractmethod
    def is_match(self, x: Any) -> Addr:
        pass

    def matching_addrs(self, canvas: Canvas) -> Collection[Addr]:
        return set(a for a in canvas.all_addrs() if self.is_match(canvas[a]))

BaseValue = Union[int, str, None]
BaseValueTup = Tuple[BaseValue, ...]

BaseFunc = Callable[['Func'], 'Func']  # type: ignore[misc]
Func = Union[BaseFunc, BaseValue] # type: ignore[misc]

Addr = int | Tuple['Addr', 'Addr']   # type: ignore[misc]
#Matcher = Callable[[Any], bool]
From = Union[Addr, Matcher]  # type: ignore[misc]
To = Union[Addr, Jumper]  # type: ignore[misc]
FromTo = Tuple[From, To]   # type: ignore[misc]
AbsFromTo = Tuple[Addr, To]   # type: ignore[misc]

Generator = Tuple[From, To, Func] # type: ignore[misc]
Painter = Generator  # type: ignore[misc]
AbsPainter = Tuple[Addr, To, Func]  # type: ignore[misc]

GSet = Dict[FromTo, Func] # type: ignore[misc]
PSet = GSet  # type: ignore[misc]
AbsPSet = Dict[AbsFromTo, Func]  # type: ignore[misc]

Value = Func # type: ignore[misc]
ValueTup = Tuple[Value, ...]  # type: ignore[misc]

def make_eqns(operands=range(1, 11), operators=('+', '-', 'x', '/')) \
-> Iterable[Tuple[BaseValue, ...]]:
    for operand1 in operands:
        for operator in operators:
            for operand2 in operands:
                try:
                    result = calculate(operand1, operator, operand2)
                except ArithmeticFailed:
                    continue
                if result > 0:
                    yield (
                        operand1,
                        operator,
                        operand2,
                        '=',
                        result
                    )

rator_symbol_to_func = {
    '+': operator.add,
    '-': operator.sub,
    'x': operator.mul,
    '/': operator.truediv
}

def calculate(operand1, operator, operand2):
    try:
        result = rator_symbol_to_func[operator](operand1, operand2)
    except ZeroDivisionError:
        raise ArithmeticFailed
    if result != int(result):
        raise ArithmeticFailed
    else:
        return int(result)

class ArithmeticFailed(Exception):
    pass
    
class NoRunnableGenerators(Exception):
    pass


@dataclass(kw_only=True)  # type: ignore[call-overload]
class CanvasDataclassMixin:
    MAX_CLARITY: Numeric = 6
    INITIAL_CLARITY: Numeric = 5

class Canvas(CanvasDataclassMixin, ABC):
    """
    MAX_CLARITY: ClassVar[Numeric] = 6  # TODO move this to a dataclass or  # 5
                                        # maybe to RMem
    """
    @abstractmethod
    def all_addrs(self) -> Iterable[Addr]:
        pass
    
    @abstractmethod
    def as_tuple(self) -> ValueTup:
        pass

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, x: Value) -> None:
        pass

    @abstractmethod
    def has_addr(self, addr: Addr) -> bool:
        pass

    @abstractmethod
    def clarity(self, addr: Addr) -> Numeric:
        pass

    def all_clarities(self) -> Iterable[Numeric]:
        for addr in self.all_addrs():
            yield self.clarity(addr)

    @abstractmethod
    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        pass
        
    @classmethod
    def make_from(
        cls,
        c: CanvasAble,
        MAX_CLARITY: Optional[int]=None,
        INITIAL_CLARITY: Optional[int]=None
    ) -> Canvas:
        if isinstance(c, Canvas):
            return c  # TODO Update with MAX_CLARITY?
#        elif isinstance(c, list)
#            return Canvas1D(c)  # TODO copy the list
#        elif isinstance(c, tuple):
#            return Canvas1D(list(c))
        elif is_iter(c):
            #MAX_CLARITY: int = MAX_CLARITY if isinstance(MAX_CLARITY, int) 
            if MAX_CLARITY is None:
                return Canvas1D(contents=as_list(c))
            else:
                return Canvas1D(contents=as_list(c), MAX_CLARITY=MAX_CLARITY)
        else:
            raise NotImplementedError

@dataclass
class Canvas1D(Canvas):
    contents: List[Value] #= field(default_factory=list)
        # Always supply a value for 'contents'! The default is only to
        # avoid an error for following MAX_CLARITY, which has a default.
    clarities: List[Numeric] = field(  # same # of elems as 'contents'
        default_factory=list,
        init=False
    )

    def __post_init__(self) -> None:
        self.clarities = [
            0 if x is None else int(self.INITIAL_CLARITY)   # - 1  * 0.61
                for i, x in enumerate(self.contents)
        ]

    def all_addrs(self) -> Iterable[Addr]:
        return range(1, len(self.contents) + 1)

    def as_tuple(self) -> ValueTup:
        return as_tuple(self.contents)

    def has_addr(self, addr: Addr) -> bool:
        if isinstance(addr, int):
            addr = addr - 1
            return addr >= 0 and addr < len(self.contents)
        else:
            return False

    def __getitem__(self, addr: Addr) -> Value:
        if isinstance(addr, int):
            try:
                return self.contents[addr - 1]
            except IndexError:
                return None
        else:
            return None

    def __setitem__(self, addr: Addr, x: Value) -> None:
        if isinstance(addr, int):
            addr = addr - 1
            if addr < 0:  # off the left edge of the canvas
                return
            if self.clarities[addr] == 0:
                try:
                    self.contents[addr] = x
                except IndexError:
                    # TODO Stretch the canvas?
                    return
                if x is not None:
                    self.clarities[addr] = 1
            elif x != self.contents[addr]:  # Trying to overwrite a value
                self.clarities[addr] -= 1
                if self.clarities[addr] <= 0:
                    self.contents[addr] = None
            else:  # Trying to write the value that is already there
                if self.clarities[addr] < self.MAX_CLARITY:
                    self.clarities[addr] += 1
        else:
            pass  # raise an exception?

    def clarity(self, addr: Addr) -> Numeric:
        if isinstance(addr, int):
            addr = addr - 1
            try:
                return self.clarities[addr]
            except IndexError:
                return self.MAX_CLARITY
        else:
            return 0  # raise an exception?

    def all_clarities(self) -> Iterable[Numeric]:
        return self.clarities

    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        if isinstance(addr, int):
            addr -= 1
            self.clarities[addr] = clarity

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        citems = ' '.join(short(c) for c in self.clarities)
        return f'[{items}]'
        #return f'[{items}]{newline}[{citems}]'

CanvasAble = Union[Sequence[Value], Canvas]  # type: ignore[misc]
CanvasPrep = Callable[[Canvas, 'RMem'], Canvas]
    # Function to call before absorbing a Canvas, e.g. to add redundancy.

@dataclass
class Canvas2D(Canvas):
    '''A two-dimensional Canvas.'''
    contents: Dict[Addr, Value]
    clarities: Dict[Addr, Numeric]
    size_x: int
    size_y: int

    def all_addrs(self) -> Iterable[Addr]:
        return (
            (x, y)
                for x in range(self.size_x)
                    for y in range(self.size_y)
        )

    def has_addr(self, addr: Addr) -> bool:
        if isinstance(addr, tuple) and len(addr) == 2:
            x, y = addr
            return (
                isinstance(x, int) and isinstance(y, int)
                and
                x >= 1 and y >= 1
                and
                x <= self.size_x and y <= self.size_y
            )
        else:
            return False

    def as_tuple(self) -> ValueTup:
        raise NotImplementedError

    def __getitem__(self, addr: Addr) -> Value:
        if isinstance(addr, tuple):
            return self.contents.get(addr, None)
        else:
            return None

    def __setitem__(self, addr: Addr, x: Value) -> None:
        if isinstance(addr, tuple):
            self.contents[addr] = x
            # TODO adjust clarity
        else:
            pass

    def clarity(self, addr: Addr) -> Numeric:
        return self.clarities.get(addr, 0)  # type: ignore[arg-type]

    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        self.clarities[addr] = clarity

@dataclass
class CanvasD(Canvas):
    '''A Canvas with arbitrary Addrs, represented by a dict that includes
    only cells for which an Addr and Value have been provided (in other words,
    a "sparse" Canvas).'''
    contents: Dict[Addr, Value]
    clarities: Dict[Addr, Numeric]

    def all_addrs(self) -> Iterable[Addr]:
        return self.contents.keys()

    def as_tuple(self) -> ValueTup:
        raise NotImplementedError

    def has_addr(self, addr: Addr) -> bool:
        return True

    def __getitem__(self, addr: Addr) -> Value:
        return self.contents.get(addr, None)

    def __setitem__(self, addr: Addr, x: Value) -> None:
        self.contents[addr] = x
        # TODO adjust clarity

    def clarity(self, addr: Addr) -> Numeric:
        return self.clarities.get(addr, 0)

    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        self.clarities[addr] = clarity

def ndups(n: int=1) -> CanvasPrep:
    def ndups_f(c: Canvas, rmem: RMem) -> Canvas:
        if isinstance(c, Canvas1D):
            return Canvas1D(c.contents * n)
        else:
            raise NotImplementedError
    return ndups_f

def no_prep(c: Canvas, rmem: RMem) -> Canvas:
    '''An identity function for Canvases.'''
    return c

def pad_tup(tup: Tuple, ndups: int) -> Tuple:
    return tuple(None for _ in range(ndups * len(tup))) + tup

def correction_redundancy(ndups: int=2, npartial: int=2, niters: int=50) \
-> CanvasPrep:
    def correction_redundancy_f(c: Canvas, rmem: RMem) -> Canvas1D:
        if not isinstance(c, Canvas1D):
            raise NotImplementedError
        tup_in = c.as_tuple()
        lo(tup_in)
        tup_out: ValueTup
        if len(rmem) == 0:
           tup_out = pad_tup(tup_in, ndups)
        else:
            lo('TUP_IN', tup_in)
            #pr(rmem.gset)
            relateds: Set[Tuple] = set()
            while len(relateds) < npartial:
                cue = pad_tup(
                    partial_tup(c.as_tuple(), k=npartial),
                    ndups=ndups
                )
                canvas_to_correct = rmem.run_gset(cue, niters=niters)
                t = canvas_to_correct.as_tuple()[-len(tup_in):]
                relateds.add(canvas_to_correct.as_tuple()[-len(tup_in):])
                lo('CC2C', cue, canvas_to_correct, t, len(relateds), npartial)
                #pr(rmem.lsteps)
            tup_out = reduce(operator.add, relateds) + tup_in
        lo(tup_out)
        return Canvas1D(list(tup_out))
    return correction_redundancy_f

def partial_tup(tup: ValueTup, k: int=3) -> ValueTup:
    r = range(len(tup))
    addrs = set(sample_without_replacement(r, k=k))
    return tuple(
        tup[a] if a in addrs else None
            for a in r
    )

@dataclass
class RMem:
    '''Regenerative memory.'''
    Q = TypeVar('Q', bound='RMem')
    gset: GSet = field(default_factory=dict)
    lsteps: List[LoggedStep] = field(default_factory=list)
    niters: int = 40
    termination_threshold: int = 4
    max_clarity: int = 6
    initial_clarity: int = 5

    # Factories / converters

    @classmethod
    def make_from(
        cls: Type[Q],
        cs: Iterable[CanvasAble],
        prep: CanvasPrep=no_prep
    ) -> Q:
        rmem = cls()
        cs = (prep(rmem.as_canvas(c), rmem) for c in cs)
        return rmem.absorb_canvases(cs)

    @classmethod
    def make_instance(
        cls: Type[Q],
        mixins: Tuple[Type[RMem], ...],
        **kwargs
    ) -> Q:
        class_name = cls.__name__ + ''.join(
            getattr(mixin, 'mixin_name', '') for mixin in mixins
        )
        ty = type(class_name, mixins + (cls,), {})
        return ty()

    def as_canvas(self, c: CanvasAble) -> Canvas:
        '''Converts c to a Canvas.'''
        return Canvas.make_from(
            c,
            MAX_CLARITY=self.max_clarity,
            INITIAL_CLARITY=self.initial_clarity
        )

    # Making generators (i.e. painters)

    def canvas_to_painters(self, c: CanvasAble) -> Set[Generator]:
        '''Makes painters as determined by .painter_from_to().'''
        c: Canvas = self.as_canvas(c)
        result: Set[Generator] = set()
        for addr1 in c.all_addrs():
            for addr2 in c.all_addrs():
                p = self.painter_from_to(addr1, addr2, c[addr1], c[addr2])
                if p is not None:
                    result.add(p)
        return result

    @classmethod
    def make_next_order_painters(cls, painters: Collection[Generator]) \
    -> Iterable[Generator]:
        '''Returns painters that, given one painter from 'painters', can
        paint another painter from 'painters'.'''
        for (xa, xb, xf) in painters:
            for (ya, yb, yf) in painters:
                if xa != ya or xb != yb:
                    zf = cls.func_from_to(xf, yf)
                    if zf is not None:
                        yield ((xa, xb), (ya, yb), zf)

    # Running generators

    def run_generator(self, canvas: Canvas, gen: Generator) -> Outcome:
        abspainters = list(self.as_absolute_painters(canvas, gen))
        if not abspainters:
            return Failed
        weights = [self.simple_painter_weight(canvas, p) for p in abspainters]
        return self.run_absolute_painter(
            canvas,
            choices(abspainters, weights)[0]
        )

#        fro, _, _ = gen
#        a = self.as_addr_from(canvas, fro)
#        if isinstance(a, int):
#            return self.paint_from_abs_addr(canvas, gen, a)
#        elif a is None:
#            return Failed
#        else:
#            raise NotImplementedError(a)

#        addr1, addr2, func = gen
#        result: Func = None
#        if canvas[addr1] is not None:
#            if callable(func):
#                result = func(canvas[addr1])
#            else:
#                result = func
#        else:
#            if isinstance(func, str):
#                result = func
#        result = self.apply_func(func, canvas[addr1])
#        if result is not None:
#            canvas[addr2] = result
#        return Succeeded

    def run_absolute_painter(self, c: Canvas, painter: AbsPainter) -> Outcome:
        return self.paint_from_abs_addr(c, painter, painter[0])

    # TODO UT
    @classmethod
    def as_absolute_painters(cls, c: Canvas, painter: Painter) \
    -> Iterable[AbsPainter]:
        a, b, f = painter
        if isinstance(a, Matcher):
            for addr1 in a.matching_addrs(c):
                if isinstance(b, Jumper):
                    for to in b.could_jump_to(c, addr1):
                        yield (addr1, to, f)
                else:
                    yield (addr1, b, f)
        else:
            yield painter  # type: ignore[misc]

    def paint_from_abs_addr(
        self,
        canvas: Canvas,
        painter: AbsPainter,
        a: Addr
    ) -> Outcome:
        _, to, func = painter
        x = canvas[a]
        if x is None:
            return Failed
        result = self.apply_func(func, x)
        if result is None:
            return Failed
        b = self.as_addr_to(canvas, a, to)
        if b is None:
            return Failed
        canvas[b] = result
        return Succeeded

    @trace
    def as_addr_from(self, canvas: Canvas, fro: From) -> Addr | None:
        if isinstance(fro, int):
            return fro
        elif isinstance(fro, Matcher):
            return self.choose_matching_addr(canvas, fro)
        else:
            raise NotImplementedError(fro)

    def choose_matching_addr(self, canvas: Canvas, m: Matcher) -> Addr | None:
        '''Searches for an absolute address containing a value that 'm'
        matches. Default implementation chooses randomly, weighted by
        .from_weight_absolute(). Returns None if no canvas cell matches.'''
        pairs: List[Tuple[Addr, float]] = [
            (a, self.from_weight_absolute(a, canvas))
                for a in m.matching_addrs(canvas)
        ]
        if len(pairs):
            return choices(*zip(*pairs))[0]
        else:
            return None

    def as_addr_to(self, canvas: Canvas, a: Addr, to: To) -> Addr | None:
        if isinstance(to, int):
            return to
        elif isinstance(to, Jumper):
            return to.to(canvas, a)
        else:
            raise NotImplementedError

    # Running GSets

    def run_gset(
        self,
        canvas: CanvasAble,
        gset: Optional[GSet]=None, niters: Optional[int]=None,
        vv: int=0  # verbosity
    ) -> Canvas:
        '''Attempts to fill in canvas by running gset.'''
        gset: GSet = self.gset if gset is None else gset
        niters: int = self.niters if niters is None else niters
        canvas: Canvas = self.as_canvas(self.prep_regen(canvas))
        self.lsteps = [LoggedStep(canvas=deepcopy(canvas), t=0)]
        try:
            for i in range(niters):
                self.start_lstep()
                gen = self.choose_runnable_painter(canvas, gset)
                self.add_to_lstep(painter=gen)
                self.run_generator(canvas, gen)
                self.add_to_lstep(canvas=canvas)
                #print(canvas, '       ', short(gen))
                if vv >= 4:
                    print(self.lsteps[-1])
                if self.termination_condition(canvas):
                    break
        except NoRunnableGenerators:
            pass
        return canvas

    def run1(
        self,
        canvas: CanvasAble,
        gset: Optional[GSet]=None, niters: Optional[int]=None,
        vv: int=4
    ) -> None:
        '''Like .run_gset() but for running from the Python REPL.'''
        print()
        print()
        self.run_gset(canvas, gset, vv=vv)

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        '''.run_gset() calls this before regenerating from c. Default
        implementation does nothing. Override to do things like prepend
        a bunch of Nones to the canvas.'''
        return c

    # TODO Factor out the constant
    def termination_condition(self, canvas: Canvas) -> bool:
        #threshold = int(0.7 * canvas.MAX_CLARITY)   # 0.5  0.7
        #threshold = 4
        #return all(cl >= threshold for cl in canvas.clarities)
        # TODO Make that -5 a parameter, or refer to 'central canvas'
        return all(
            cl >= self.termination_threshold
                for cl in list(canvas.all_clarities())[-5:]
        )

    def choose_runnable_painter(self, canvas: Canvas, gset: GSet) \
    -> Generator:
        pws = list(self.painter_weights(canvas, gset))
        #pts(sorted([p[1], p[0]] for p in pws)) #DEBUG
        ps, ws = zip(*pws)
        #pr(pws) #DEBUG
        if len(ps) == 0:
            raise NoRunnableGenerators
        try:
            n = choices(range(len(ps)), weights=ws)[0]
        except ValueError as exc:  # screwy way to check for sum(ws) <= 0
            #print(exc)
            raise NoRunnableGenerators
        a1, a2 = ps[n]
        f = gset[(a1, a2)]
        painter = (a1, a2, f)
        self.add_to_lstep(painter=painter, painter_weight=ws[n] / sum(ws))
        return painter

    @classmethod
    def painter_weights(cls, canvas: Canvas, gset: GSet) \
    -> Iterable[Tuple[FromTo, Numeric]]:
        for ((a1, a2), f) in gset.items():
            if isinstance(a1, int) and canvas[a1] is None:
                continue
            else:
                #w1 = canvas.clarity(a1) / canvas.MAX_CLARITY
                #w2 = 1.0 - (canvas.clarity(a2) / (canvas.MAX_CLARITY * 1.00))
                #yield ((a1, a2), w1 * w2 * natural_func_weight(f))
                #w = cls.a_to_w(a1, a2, f, canvas)
                w = cls.painter_weight(a1, a2, f, canvas)
                if w >= epsilon:
                    yield ((a1, a2), w)

    # TODO Redefine Painter as Tuple[From, To, Func]
    # TODO Make this a method of Canvas
    # TODO Matchers and matching_addrs() should return weights
    @classmethod
    def matching_addrs(cls, c: Canvas, a: From) -> Iterable[Addr]:
        if isinstance(a, int) or isinstance(a, tuple):
            yield a
        else:  # a is a Matcher
            for addr in c.all_addrs():
                if a.is_match(c[addr]):
                    yield addr

    @classmethod
    def painter_weight(cls, a1: From, a2: To, f: Func, c: Canvas) -> Numeric:
        '''Address weights are a linear function of current cell clarity.'''
#        if isinstance(a1, int) and isinstance(a2, int):
#            w1 = c.clarity(a1) / c.MAX_CLARITY
#            w2 = 1.0 - (c.clarity(a2) / (c.MAX_CLARITY * 1.00))
#            return w1 * w2 * cls.natural_func_weight(f)
#        elif callable(a1):  # a1 is a Matcher
#            if cls.could_run(a1, a2, f, c):
#                return 1.0  # HACK: Should assign weight based on something
#                            # something more than whether the painter could run
#            else:
#                return 0.0
#        else:
#            raise NotImplementedError

#        w1 = (
#            cls.from_weight_relative(a1, a2, f, c)
#            if isinstance(a1, Matcher)
#            else cls.from_weight_absolute(a1, c)
#        )
#        w2 = (
#            cls.to_weight_relative(a1, a2, c)
#            if isinstance(a2, Jumper)
#            else cls.to_weight_absolute(a2, c)
#        )
#        return w1 * w2 * cls.natural_func_weight(f)

        return max([
            cls.simple_painter_weight(c, absp)
                for absp in cls.as_absolute_painters(c, (a1, a2, f))
        ], default=0)

    @classmethod
    def simple_painter_weight(cls, c: Canvas, painter: AbsPainter) -> Numeric:
        a, b, f = painter
        w1 = cls.from_weight_absolute(a, c)
        w2 = 1.0 if isinstance(b, Jumper) else cls.to_weight_absolute(b, c)
        return w1 * w2 * cls.natural_func_weight(f)

    @classmethod
    def from_weight_absolute(cls, a: Addr, c: Canvas) -> float:
        return c.clarity(a) / c.MAX_CLARITY

    @classmethod
    def from_weight_relative(
        cls, a: Matcher, b: To, f: Func, c: Canvas
    ) -> Numeric:
        if cls.could_run(a, b, f, c):
            return 1.0  # TODO Base weight on the matched cell?
        else:
            return 0.0

    @classmethod
    def to_weight_absolute(cls, b: Addr, c: Canvas) -> Numeric:
        return 1.0 - (c.clarity(b) / c.MAX_CLARITY)

    @classmethod
    def to_weight_relative(cls, a: From, b: Jumper, c: Canvas) -> Numeric:
        return 1.0  # TODO

    @classmethod
    def could_run(cls, a: From, b: To, f: Func, c: Canvas) -> bool:
        if isinstance(a, Matcher):
            if not any(a.is_match(c[i]) for i in c.all_addrs()):
                return False
        else:
            if not c.has_addr(a):
                return False
        if isinstance(b, Jumper):
            if not b.could_jump(c, a):
                return False
        else:
            if not c.has_addr(b):
                return False
        return True

    @classmethod
    def natural_func_weight(cls, f: Func) -> Numeric:
        if hasattr(f, 'natural_func_weight'):
            return f.natural_func_weight()  # type: ignore[union-attr]
        elif callable(f):
            return 1.0
        else:
            return 0.2  # Low probability for constant painter

    # Making GSets (sets of generators)

    def make_gset(self, gs: Iterable[Generator]) -> GSet:
        result: PSet = {}
        for g in gs:
            a1, a2, f = g
            oldf = result.get((a1, a2), None)
            #lo('MKGS', a1, a2, f, oldf)
            if oldf is not None:
                f = self.avg_of_funcs(oldf, f)
                #lo('MKGS2', f)
            result[(a1, a2)] = f
        return result

    def canvas_to_pset(self, c: CanvasAble) -> PSet:
        '''Makes a PSet of painters as determined by .canvas_to_painters().'''
        return self.make_gset(self.canvas_to_painters(c))

    def add_two_gsets(self, gset1: GSet, gset2: GSet) -> GSet:
        '''Combine the gsets, analogous to '+' in the Hopfield equation to
        combine two images.'''
        result: GSet = defaultdict(set)  # type: ignore[arg-type]
        edges = union(gset1.keys(), gset2.keys())
        for edge in edges:
            if edge in gset1:
                if edge in gset2:
                    f = self.avg_of_funcs(
                        gset1[edge],
                        gset2[edge]
                    )
                else:
                    f = gset1[edge]
            else:
                f = gset2[edge]
            result[edge] = f
        return dict(  # remove None funcs
            (k, v) for k, v in result.items() if v is not None
        )

    def add_gsets(self, gsets: Iterable[GSet]) -> GSet:
        result: GSet = reduce(self.add_two_gsets, gsets, {})
        return dict(  # remove None funcs
            (k, v) for k, v in result.items() if v is not None
        )

    def raw_absorb_gset(self, new_gset: GSet) -> None:
        self.gset = self.add_two_gsets(self.gset, new_gset)

    # TODO Break off this notion: canvas_to_psets_for_absorption() ?
    def raw_absorb_canvas(self, c: Canvas) -> None:
        '''Absorbs c into self.gset, without modifying c. Override
        .raw_absorb_canvas() to change the way absorption works. Override
        .prep_absorb() to add columns to c or otherwise modify c before
        absorbing it.'''
        #self.raw_absorb_gset(self.make_gset(self.canvas_to_painters(c)))
        self.raw_absorb_gset(self.canvas_to_pset(c))

    @final
    def absorb_canvas(self, c: CanvasAble, prep: CanvasPrep=no_prep) -> None:
        c = self.as_canvas(self.prep_absorb(c))
        self.raw_absorb_canvas(prep(c, self))

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        '''.absorb_canvas() calls this before absorbing c. Default
        implementation does nothing. Override to do things like prepend
        additional information to the canvas.'''
        return c

    def absorb_canvases(
        self: Q,
        cs: Iterable[CanvasAble],
        prep: CanvasPrep=no_prep
    ) -> Q:
        for c in cs:
            self.absorb_canvas(c)
        return self

        '''
        for c in cs:
            c = self.as_canvas(c)
            new_gset = self.make_gset(
                self.canvas_to_painters(prep(c, self))
            )
            #lo('ABS', c)
            #pr(new_gset)
            self.gset = self.add_two_gsets(self.gset, new_gset)
        return self
        '''

    # Calling a function

    @classmethod
    def apply_func(cls, func: Func, x: Value) -> Value:
        if callable(func):
            return func(x)
        else:
            return func

    # Painter-makers

    def painter_from_to(self, a: Addr, b: Addr, xa: Value, xb: Value) \
    -> Painter | None:
        '''Returns a painter that, given the value xa at address a, will paint
        the value xb at address b. Default implementation returns an absolute
        painter.'''
        if a != b and xa is not None and xb is not None:
            func = self.func_from_to(xa, xb)
            if func is not None:
                return (a, b, func)
        return None

    # Function-makers

    @classmethod
    def func_from_to(cls, x1: Value, x2: Value) -> Func:
        '''Returns a function that maps x1 to x2.'''
        if x2 is None:
            return None
        elif x1 == x2:
            return cls.same
        elif isinstance(x1, int) and isinstance(x2, int):
            return cls.int_func_from_to(x1, x2)
        else:
            return cls.make_K(x2)

    @classmethod
    def make_K(cls, x: Value) -> Func:
        '''Returns a constant Func that paints x.'''
        if isinstance(x, int) or isinstance(x, str) or x is None:
            return x
        else:
            return cls.K(x)

    @classmethod
    def int_func_from_to(cls, x1: int, x2: int) -> Func:
        if x1 == x2:
            return cls.same
        elif x1 > x2:
            if x2 != 0:
                factor = x1 // x2
                if x1 * factor == x2:
                    return cls.div_by(factor)
            return cls.sub_n(x1 - x2)
        else:
            if x1 != 0:
                factor = x2 // x1
                if x1 * factor == x2:
                    return cls.mul_by(factor)
            return cls.add_n(x2 - x1)

    def avg_of_funcs(self, f1: Func, f2: Func) -> Func:
        if f1 is None or f2 is None:
            return None
        elif f1 == f2:
            return f1
        elif hasattr(f1, 'avg_with'):
            return f1.avg_with(f2)  # type: ignore[union-attr]
        elif hasattr(f2, 'avg_with'):
            return f2.avg_with(f1)  # type: ignore[union-attr]
        else:
            return self.rndfunc.make(self, [f1, f2])

    # Functions

    @classmethod
    def same(cls, x: Any):
        return x

    @dataclass(frozen=True)
    class K:
        '''Constant Func.'''
        x: Value

        def __call__(self, _: Any) -> Value:
            return self.x

        def __str__(self) -> str:
            return f'K({short(self.x)})'

    @dataclass(frozen=True)
    class add_n:
        n: int

        def __call__(self, x: int) -> int:
            return x + self.n

    @dataclass(frozen=True)
    class sub_n:
        n: int

        def __call__(self, x: int) -> int:
            return x - self.n

    @dataclass(frozen=True)
    class mul_by:
        factor: int

        def __call__(self, x: int) -> int:
            return x * self.factor

    @dataclass(frozen=True)
    class div_by:
        factor: int

        def __call__(self, dividend: int) -> int:
            return dividend // self.factor

    @dataclass(frozen=True)
    class rndfunc:
        Q = TypeVar('Q', bound='RMem.rndfunc')
        rmem: RMem
        funcs: Tuple[Func]     # funcs and weights must have same # of elems
        weights: Tuple[Numeric]

        nfw: Numeric = field(default=0.0, init=False)
        weights_sum: Numeric = field(default=0.0, init=False)

        def __post_init__(self) -> None:
            force_setattr(self, 'nfw', 3.0 / sum(
                (1.0 / self.rmem.natural_func_weight(f))
                #0.5 if callable(f) else 1.0
                    for f in self.funcs
            ))
            force_setattr(self, 'weights_sum', sum(self.weights))

        def __call__(self, x: Value) -> Value:
            n = choices(range(len(self.funcs)), weights=self.weights)[0]
            func = self.funcs[n]
            self.rmem.add_to_lstep(
                real_painter=func,
                real_painter_weight=self.weights[n] / self.weights_sum
            )
            return self.rmem.apply_func(func, x)

        def natural_func_weight(self) -> Numeric:
            return self.nfw

        @classmethod
        def make(
            cls: Type[Q],
            rmem: RMem,
            funcs: Sequence[Func]
        ) -> Q:
            # TODO What if funcs is empty?
            return cls._make(cls, rmem, Counter(funcs))

        def avg_with(self, other: Func) -> Func:
            if other is None:
                return self
            else:
                c: Dict[Func, int] = Counter(
                    dict(zip(self.funcs, self.weights))
                )
                c.update([other])  # type: ignore[list-item]
                return self._make(self.__class__, self.rmem, c)

        @classmethod
        def _make(
            _cls,
            cls: Type[Q],
            rmem: RMem,
            c: Dict[Func, int]
        ) -> Q:
            return cls(
                rmem=rmem,
                funcs=as_tuple(c.keys()),  # type: ignore[arg-type]
                weights=as_tuple(c.values())  # type: ignore[arg-type]
            )

        def short(self) -> str:
            cl = self.__class__.__name__
            return f'{cl}({self.nfw:1.3f} {short(self.weights)}, {short(self.funcs)})'

    # Queries

    def __len__(self) -> int:
        return len(self.gset)

    def __str__(self) -> str:
        return self.__class__.__name__

    def __repr__(self) -> str:
        return self.__class__.__name__

    # Logging

    def start_lstep(self) -> None:
        self.lsteps.append(LoggedStep(t=self.lsteps[-1].t + 1))

    def add_to_lstep(
        self,
        canvas: Optional[Canvas]=None,
        painter: Optional[Painter]=None,
        painter_weight: Optional[Numeric]=None,
        real_painter: Union[Painter, Func, None]=None,
        real_painter_weight: Optional[Numeric]=None
    ):
        lstep = self.lsteps[-1]
        if canvas is not None:
            lstep.canvas = deepcopy(canvas)
        if painter is not None:
            lstep.painter = painter
        if painter_weight is not None:
            lstep.painter_weight = painter_weight
        if real_painter is not None:
            lstep.real_painter = real_painter
        if real_painter_weight is not None:
            lstep.real_painter_weight = real_painter_weight

    # Running experiments

    @classmethod
    def run(
        cls: Type[Q],
        startc=(None, '+', 3, None, 5),
        operands=range(1, 11),
        operators=('+', '-', 'x', '/'),
        prep: CanvasPrep=no_prep,
        nruns=1,
        niters=40
    ) -> Q:
        '''Runs a whole experiment from start to finish. Creates and returns
        an RMem object so you can inspect the generators afterward.'''
        rmem = cls.make_from(
            make_eqns(operands=operands, operators=operators),
            prep=prep
        )

        for _ in range(nruns):
            #c = list(startc)
            c = Canvas.make_from(startc)
            if prep:
                c = prep(c, rmem)
            #print(c)
            rmem.run_gset(c, niters=niters)
            #print()

        return rmem

@dataclass
class SkewedPainterWeight(RMem):
    weight_from: ClassVar[List[Numeric]] = [0, 5,  10, 25, 50, 90, 100]
    weight_to: ClassVar[List[Numeric]] =  [100, 100, 90, 80,  20,  5,  1]

#    @classmethod
#    def painter_weight(cls, a: From, b: To, f: Func, c: Canvas) -> Numeric:
#        if isinstance(a, int) and isinstance(b, int):
#            return (
#                (
#                    cls.weight_from[int(c.clarity(a))]
#                    *
#                    cls.weight_to[int(c.clarity(b))]
#                )
#                *
#                cls.natural_func_weight(f)
#            )
#        else:
#            raise NotImplementedError

    @classmethod
    def from_weight_absolute(cls, a: Addr, c: Canvas) -> float:
        if isinstance(a, int):
            return cls.weight_from[int(c.clarity(a))]
        else:
            raise NotImplementedError

    @classmethod
    def to_weight_absolute(cls, b: Addr, c: Canvas) -> Numeric:
        if isinstance(b, int):
            return cls.weight_to[int(c.clarity(b))]
        else:
            raise NotImplementedError

@dataclass
class LoggedStep:
    canvas: Optional[Canvas] = None  # The resulting Canvas
    painter: Optional[Painter] = None
    painter_weight: Optional[Numeric] = None
    real_painter: Union[Painter, Func, None] = None
    real_painter_weight: Optional[Numeric] = None
    t: int = 0

    def __str__(self) -> str:
        sio = StringIO()
        if self.canvas is None:
            co = ''
            cl = ''
        else:
            if isinstance(self.canvas, Canvas1D):
                co = f'{short(self.canvas.contents):40}'
                cl = f'{short(self.canvas.clarities):40}'
            else:
                raise NotImplementedError
        painter = f'{short(self.painter):.180}'
        if self.real_painter:
            real_painter = f'{short(self.real_painter):.180}'
        else:
            real_painter = ''
        if self.painter_weight is None:
            pw = ' --  '
        else:
            pw = f'{self.painter_weight:1.3f}'
        if self.real_painter_weight is None:
            rw = ' --  '
        else:
            rw = f'{self.real_painter_weight:1.3f}'
        if self.t is None:
            t = ' ' * 4
        else:
            t = f'{self.t:4}'
        b = ' ' * 4
            
        print(f'{t}  {co}  {pw} {painter}', file=sio)
        print(f'{b}  {cl}  {rw} {real_painter}', file=sio)
        return sio.getvalue()

@dataclass(frozen=True)
class Match(Matcher):
    v: Value

    def is_match(self, x: Any) -> bool:
        return x == self.v

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.v!r})'

@dataclass(frozen=True)
class Right(Jumper):
    n: int

    def to(self, canvas: Canvas, a: Addr) -> Addr:
        if isinstance(a, int):
            return a + self.n
        else:
            raise NotImplementedError

    def could_jump(self, canvas: Canvas, fro: From) -> bool:
        if isinstance(fro, int):
            return canvas.has_addr(fro + 1)
        elif isinstance(fro, tuple):
            raise NotImplementedError
        else:
            return True

    def could_jump_to(self, c: Canvas, a: Addr) -> Iterable[To]:
        if isinstance(a, int):
            if c.has_addr(a + 1):
                yield a + 1
        else:
            raise NotImplementedError

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.n})'

@dataclass(frozen=True)
class Left(Jumper):
    n: int

    def to(self, canvas: Canvas, a: Addr) -> Addr:
        if isinstance(a, int):
            return a - self.n
        else:
            raise NotImplementedError

    def could_jump(self, canvas: Canvas, fro: From) -> bool:
        if isinstance(fro, int):
            return canvas.has_addr(fro - 1)
        elif isinstance(fro, tuple):
            raise NotImplementedError
        else:
            return True

    def could_jump_to(self, c: Canvas, a: Addr) -> Iterable[To]:
        if isinstance(a, int):
            if c.has_addr(a - 1):
                yield a - 1
        else:
            raise NotImplementedError

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.n})'

class WithAdjacentRelativePainters(RMem):
    mixin_name: ClassVar[str] = 'AdjRel'

    def painter_from_to(self, a: Addr, b: Addr, xa: Value, xb: Value) \
    -> Painter | None:
        if a != b and xa is not None and xb is not None:
            if isinstance(a, int) and isinstance(b, int):
                jumper: Jumper
                if b == a + 1:
                    jumper = Right(1)
                elif b == a - 1:
                    jumper = Left(1)
                else:
                    return None
                func = self.func_from_to(xa, xb)
                if func is None:
                    return None
                return (Match(xa), jumper, func)
            else:
                raise NotImplementedError((a, b))
        return None

if __name__ == '__main__':
#    rmem = RMem.run(
#        operands=range(1, 8),   # 4
#        startc=(None, '+', 1, None, 3),
#        prep=ndups(3),
#        niters=1000
#    )
    #rmtype = type('RMemAdjacent', (WithAdjacentRelativePainters, RMem), {})
    #rmem = rmtype(niters=30)
    rmem = RMem.make_instance(
        (SkewedPainterWeight, WithAdjacentRelativePainters),
        niters=30
    )

    #p = rmem.painter_from_to(1, 2, 1, '+')
    #print(p)

    #p: Painter = (Match(1), Right(1), '+')
    #c = Canvas1D.make_from((1, None, None, None, None))
    #rmem.run_generator(c, p)
    #print(c)

    eqn = (1, '+', 1, '=', 2)
    pps = rmem.canvas_to_painters(eqn)
    #pr(pps)
    pset = rmem.make_gset(pps)
    pr(pset)

    startc = (1, None, None, None, None)
    c = rmem.run_gset(startc, pset, vv=4)
    print(c)
