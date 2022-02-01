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
    newline, force_setattr, sample_without_replacement, first, is_iter


BaseValue = Union[int, str, None]
BaseValueTup = Tuple[BaseValue, ...]
BaseFunc = Callable[['Func'], 'Func']  # type: ignore[misc]
Func = Union[BaseFunc, BaseValue] # type: ignore[misc]
Matcher = Callable[[Any], bool]
Addr = int | Tuple['Addr', 'Addr']   # type: ignore[misc]
From = Union[Addr, Matcher]  # type: ignore[misc]
FromTo = Tuple[From, Addr]   # type: ignore[misc]
Generator = Tuple[Addr, Addr, Func] # type: ignore[misc]
Painter = Generator  # type: ignore[misc]
GSet = Dict[Tuple[Addr, Addr], Func] # type: ignore[misc]
PSet = GSet  # type: ignore[misc]
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

    def as_canvas(self, c: CanvasAble) -> Canvas:
        '''Converts c to a Canvas.'''
        return Canvas.make_from(
            c,
            MAX_CLARITY=self.max_clarity,
            INITIAL_CLARITY=self.initial_clarity
        )

    # Making generators (i.e. painters)

    def make_generators(self, c: CanvasAble) -> Set[Generator]:
        '''Makes absolute painters.'''
        c: Canvas = self.as_canvas(c)
        result = set()
        for addr1 in c.all_addrs():
            for addr2 in c.all_addrs():
                if (
                    addr1 != addr2
                    and
                    c[addr1] is not None
                    and
                    c[addr2] is not None
                ):
                    f = self.func_from_to(c[addr1], c[addr2])
                    #lo('MKG', addr1, addr2, c[addr2], c)
                    result.add(
                        (addr1, addr2, self.func_from_to(c[addr1], c[addr2]))
                    )
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

    def run_generator(self, canvas: Canvas, gen: Generator) -> Canvas:
        addr1, addr2, func = gen
        result: Func = None
        if canvas[addr1] is not None:
            if callable(func):
                result = func(canvas[addr1])
            else:
                result = func
        else:
            if isinstance(func, str):
                result = func
        result = self.apply_func(func, canvas[addr1])
        if result is not None:
            canvas[addr2] = result
        return canvas

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
                gen = self.choose_runnable_generator(canvas, gset)
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

    def choose_runnable_generator(self, canvas: Canvas, gset: GSet) \
    -> Generator:
        pws = list(self.painter_weights(canvas, gset))
        #pts(sorted([p[1], p[0]] for p in pws)) #DEBUG
        ps, ws = zip(*pws)
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
            if canvas[a1] is None:
                continue
            else:
                #w1 = canvas.clarity(a1) / canvas.MAX_CLARITY
                #w2 = 1.0 - (canvas.clarity(a2) / (canvas.MAX_CLARITY * 1.00))
                #yield ((a1, a2), w1 * w2 * natural_func_weight(f))
                #w = cls.a_to_w(a1, a2, f, canvas)
                yield ((a1, a2), cls.painter_weight(a1, a2, f, canvas))

    # TODO Redefine Painter as Tuple[From, To, Func]
    # TODO Make this a method of Canvas
    # TODO Matchers and matching_addrs() should return weights
    @classmethod
    def matching_addrs(cls, c: Canvas, a: From) -> Iterable[Addr]:
        if isinstance(a, int) or isinstance(a, tuple):
            yield a
        else:  # a is a Matcher
            for addr in c.all_addrs():
                if a(c[addr]):
                    yield addr

    @classmethod
    def painter_weight(cls, a1: Addr, a2: Addr, f: Func, c: Canvas) -> Numeric:
        '''Address weights are a linear function of current cell clarity.'''
        w1 = c.clarity(a1) / c.MAX_CLARITY
        w2 = 1.0 - (c.clarity(a2) / (c.MAX_CLARITY * 1.00))
        return w1 * w2 * cls.natural_func_weight(f)

    @classmethod
    def natural_func_weight(cls, f: Func) -> Numeric:
        if hasattr(f, 'natural_func_weight'):
            return f.natural_func_weight()  # type: ignore[union-attr]
        elif callable(f):
            return 1.0
        else:
            return 0.2  # Low probability for constant painter

    # Making GSets (sets of generators)

    @classmethod
    def make_gset(cls, gs: Iterable[Generator]) -> GSet:
        result: GSet = defaultdict(set)  # type: ignore[arg-type]
        for g in gs:
            a1, a2, f = g
            result[(a1, a2)] = f
        return result

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
        self.raw_absorb_gset(self.make_gset(self.make_generators(c)))

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
                self.make_generators(prep(c, self))
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

    @classmethod
    def painter_weight(cls, a1: Addr, a2: Addr, f: Func, c: Canvas) -> Numeric:
        return (
            (
                cls.weight_from[int(c.clarity(a1))]
                *
                cls.weight_to[int(c.clarity(a2))]
            )
            *
            cls.natural_func_weight(f)
        )

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

if __name__ == '__main__':
    rmem = RMem.run(
        operands=range(1, 8),   # 4
        startc=(None, '+', 1, None, 3),
        prep=ndups(3),
        niters=1000
    )
