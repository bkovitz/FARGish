# RMem.py -- Classes and functions for regenerative memory

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field, \
    is_dataclass, make_dataclass
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
    singleton, dc_type_of


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


@dataclass(kw_only=True)  # type: ignore[call-overload, misc]
class Canvas(ABC):
    MAX_CLARITY: Numeric = 6
    INITIAL_CLARITY: Numeric = 5

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

@dataclass  # type: ignore[misc]
class RMem(ABC):
    '''Abstract base class for regenerative memory. To make an actual
    regenerative-memory (RMem) object, you must override two methods:
    .absorb_canvas() and .regenerate().'''
    Q = TypeVar('Q', bound='RMem')

    pset: PSet = field(default_factory=dict)  # the memory
    niters: int = 40
    canvas_cls: ClassVar[Type[Canvas]] = Canvas1D
    lsteps: List[LoggedStep] = field(default_factory=list)

    @abstractmethod
    def absorb_canvas(self, c: CanvasAble) -> None:
        '''Absorbs canvas 'c' into the memory. The memory can then guide the
        .regenerate() method. Updates .pset.'''
        pass

    @abstractmethod
    def regenerate(
        self,
        canvas: CanvasAble,
        pset: Optional[PSet]=None,
        niters: Optional[int]=None,
        vv: int=0  # verbosity
    ) -> Canvas:
        '''Regenerates 'canvas' from the PSet in the memory. 'canvas' should
        be a partial canvas, i.e. containing some Nones in place of determinate
        values. Returns the resulting canvas. 'pset' and 'niters' default to
        the corresponding fields in the RMem object. An implementation of
        .regenerate() should log each step in self.lsteps.'''
        pass

    def absorb_canvases(
        self: Q,
        cs: Iterable[CanvasAble]
    ) -> Q:
        for c in cs:
            self.absorb_canvas(c)
        return self

    @classmethod
    def make_class(
        cls: Type[Q],
        mixins: Tuple[Type[RMem], ...],
        **kwargs
    ) -> Type[Q]:
        '''Dynamically defines an RMem class from cls, mixins, and kwargs.'''
        class_name = cls.__name__ + ''.join(
            getattr(mixin, 'mixin_name', 'X') for mixin in mixins
        )
        return type(class_name, mixins + (cls,), kwargs)

    @classmethod
    def make_instance(
        cls: Type[Q],
        mixins: Tuple[Type[RMem], ...],
        **kwargs
    ) -> Q:
        '''Dynamically defines an RMem class from cls, mixins, and kwargs, and
        returns an instance ot it.'''
        return cls.make_class(mixins)(**kwargs)

    @classmethod
    def as_canvas(cls, c: CanvasAble) -> Canvas:
        '''Converts something that could be treated as a canvas, such as a
        tuple or list, into a Canvas object, by calling .canvas_cls to
        construct it.  Override to pass additional arguments to each canvas
        constructed.'''
        return cls.canvas_cls.make_from(c)

    # Queries

    def __len__(self) -> int:
        return len(self.pset)

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

class RMemFuncs(RMem):

    @classmethod
    def apply_func(cls, func: Func, x: Value) -> Value:
        if callable(func):
            return func(x)
        else:
            return func

    @classmethod
    def natural_func_weight(cls, f: Func) -> Numeric:
        if hasattr(f, 'natural_func_weight'):
            return f.natural_func_weight()  # type: ignore[union-attr]
        elif callable(f):
            return 1.0
        else:
            return 0.2  # Low probability for constant painter

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
        Q = TypeVar('Q', bound='RMemFuncs.rndfunc')
        rmem: RMemFuncs
        funcs: Tuple[Func, ...]  # funcs and weights must have same # of elems
        weights: Tuple[Numeric, ...]

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
            rmem: RMemFuncs,
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

        def __eq__(self, other: Any) -> bool:
            '''This should be needed only for unit testing.'''
            return (
                isinstance(other, self.__class__)
                and
                self._normalized() == other._normalized()
            )

        def _normalized(self) -> Tuple[Tuple[Func, ...], Tuple[Numeric, ...]]:
            indices = sorted(
                range(len(self.funcs)), key=lambda i: str(self.funcs[i])
            )
            return (
                tuple(self.funcs[i] for i in indices),
                tuple(self.weights[i] for i in indices)
            )

        @classmethod
        def _make(
            _cls,
            cls: Type[Q],
            rmem: RMemFuncs,
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

class Absorb(RMemFuncs, RMem, ABC):
    '''Basic implementation of .absorb_canvases() and ancillary methods.
    Provides all but implementations of .canvas_to_psets(); a concrete class
    must override this.'''

    ### Top-level absorption methods

    @final
    def absorb_canvas(self, c: CanvasAble) -> None:
        '''Calls .prep_absorb() on c before absorbing it through
        .raw_absorb_canvas(). Override .raw_absorb_canvas() to provide
        a specific way of absorbing a canvas.''' # TODO fix comment
        for pset in self.canvas_to_psets(
            self.prep_absorb(c)
        ):
            self.absorb_pset(pset)

    @abstractmethod
    def canvas_to_psets(self, c: CanvasAble) -> Iterable[PSet]:
        '''Should yield zero or more PSets to be 'added' into self.pset
        by .absorb_pset() in order to absorb canvas 'c'.'''
        pass

    def absorb_pset(self, new_pset: PSet) -> None:
        self.pset = self.add_two_psets(self.pset, new_pset)

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        '''.absorb_canvas() calls this before absorbing c. Default
        implementation does nothing. Override to do things like prepend
        additional information to the canvas.'''
        return c

    ### Making and combining PSets

    def painters_to_pset(self, ps: Iterable[Painter]) -> PSet:
        result: PSet = {}
        for p in ps:
            a1, a2, f = p
            oldf = result.get((a1, a2), None)
            if oldf is not None:
                f = self.avg_of_funcs(oldf, f)
            result[(a1, a2)] = f
        return result

    def add_two_psets(self, pset1: PSet, pset2: PSet) -> PSet:
        '''Combines the psets, analogous to '+' in the Hopfield equation to
        combine two images. The default implementation calls .avg_of_funcs()
        on the Cartesian product of all the edges (FromTo addresses) in
        both psets.'''
        result: PSet = defaultdict(set)  # type: ignore[arg-type]
        edges = union(pset1.keys(), pset2.keys())
        for edge in edges:
            if edge in pset1:
                if edge in pset2:
                    f = self.avg_of_funcs(
                        pset1[edge],
                        pset2[edge]
                    )
                else:
                    f = pset1[edge]
            else:
                f = pset2[edge]
            result[edge] = f
        return dict(  # remove None funcs
            (k, v) for k, v in result.items() if v is not None
        )

@dataclass  # type: ignore[misc]
class Regenerate(RMem, ABC):
    '''Basic implementation of .regenerate() and ancillary methods. Provides
    all but an implementation of .painter_weight().'''
    termination_threshold: int = 3

    @abstractmethod
    def run_painter(self, canvas: Canvas, painter: Painter) -> Outcome:
        '''Should run 'painter' on 'canvas', returning Succeeded or Failed
        as appropriate.'''
        pass

    ### Top-level regeneration methods

    def regenerate(
        self,
        canvas: CanvasAble,
        pset: Optional[PSet]=None,
        niters: Optional[int]=None,
        vv: int=0  # verbosity
    ) -> Canvas:
        '''Attempts to fill in canvas by running pset. Calls .prep_regen()
        on 'canvas' before starting the filling-in process.'''
        pset: PSet = self.pset if pset is None else pset
        niters: int = self.niters if niters is None else niters
        canvas: Canvas = self.as_canvas(self.prep_regen(canvas))
        self.lsteps = [LoggedStep(canvas=deepcopy(canvas), t=0)]
        try:
            for i in range(niters):
                self.start_lstep()
                p = self.choose_runnable_painter(canvas, pset)
                self.add_to_lstep(painter=p)
                self.run_painter(canvas, p)
                self.add_to_lstep(canvas=canvas)
                #print(canvas, '       ', short(gen))
                if vv >= 4:
                    print(self.lsteps[-1])
                if self.termination_condition(canvas):
                    break
        except NoRunnableGenerators:
            pass
        return canvas

    def termination_condition(self, canvas: Canvas) -> bool:
        '''Is the state of 'canvas' such that .regenerate() should stop even
        if .niters iterations haven't been completed?'''
        # TODO Make that -5 a parameter, or refer to 'central canvas'
        return all(
            cl >= self.termination_threshold
                for cl in list(canvas.all_clarities())[-5:]
        )

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        '''.regenerate() calls this before regenerating from c. Default
        implementation does nothing. Override to do things like prepend
        a bunch of Nones to the canvas.'''
        return c

    def run1(
        self,
        canvas: CanvasAble,
        pset: Optional[PSet]=None,
        niters: Optional[int]=None,
        vv: int=4
    ) -> None:
        '''Like .regenerate() but for running from the Python REPL.'''
        print()
        print()
        self.regenerate(canvas, pset, niters, vv=vv)

    ### Choosing a painter

    def choose_runnable_painter(self, canvas: Canvas, pset: PSet) \
    -> Painter:
        pws = list(self.painter_weights(canvas, pset))
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
        f = pset[(a1, a2)]
        painter = (a1, a2, f)
        self.add_to_lstep(painter=painter, painter_weight=ws[n] / sum(ws))
        return painter

    def painter_weights(self, canvas: Canvas, pset: PSet) \
    -> Iterable[Tuple[FromTo, Numeric]]:
        # NEXT Convert the PSet into an iterable of 'actual' painters
        for ((a1, a2), f) in pset.items():
            if isinstance(a1, int) and canvas[a1] is None:
                continue
            else:
                w = self.painter_weight(a1, a2, f, canvas)
                if w >= epsilon:
                    yield ((a1, a2), w)

    @abstractmethod
    def painter_weight(self, a: From, b: To, f: Func, c: Canvas) -> Numeric:
        '''Returns the weight to be given to a painter that reads from cell 'a'
        of the canvas, passed it through Func 'f', and paints the result 
        to cell 'b'.'''
        pass

class ClarityWeight(RMem, ABC):

    @classmethod
    @abstractmethod
    def from_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        '''Should return the weight associated with a canvas cell being painted
        *from*, with clarity 'cl'. Normally, the greater the clarity of a
        'from' cell, the greater the weight.'''
        pass

    @classmethod
    @abstractmethod
    def to_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        '''Should return the weight associated with a canvas cell being painted
        *to*, with clarity 'cl'. Normally, the lower the clarity of a
        'to' cell, the greater the weight.'''
        pass

class LinearClarityWeight(ClarityWeight):

    @classmethod
    def from_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        return cl / c.MAX_CLARITY

    @classmethod
    def to_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        return 1.0 - (cl / c.MAX_CLARITY)

@dataclass  # type: ignore[misc]
class SkewedClarityWeight(ClarityWeight):
    weight_from: ClassVar[List[Numeric]] = [0, 5,  10, 25, 50, 90, 100]
    weight_to: ClassVar[List[Numeric]] =  [100, 100, 90, 80,  20,  5,  1]

    @classmethod
    def from_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        return cls.weight_from[int(cl)]

    @classmethod
    def to_clarity_weight(cls, c: Canvas, cl: Numeric) -> float:
        return cls.weight_to[int(cl)]

class CanvasToPainters(RMem, ABC):
    '''Includes a .canvas_to_painters() method. Requires that a concrete class
    override .painter_from_to().'''

    def canvas_to_painters(self, c: CanvasAble) -> Set[Painter]:
        '''Makes painters as determined by .painter_from_to().'''
        c: Canvas = self.as_canvas(c)
        result: Set[Painter] = set()
        for addr1 in c.all_addrs():
            for addr2 in c.all_addrs():
                p = self.painter_from_to(addr1, addr2, c[addr1], c[addr2])
                if p is not None:
                    result.add(p)
        return result

    @abstractmethod
    def painter_from_to(self, a: Addr, b: Addr, xa: Value, xb: Value) \
    -> Painter | None:
        '''Returns a painter that, given the value xa at address a, will paint
        the value xb at address b.'''
        pass

class CanvasToOnePSet(CanvasToPainters, Absorb, ABC):
    '''Overrides .canvas_to_psets() with a method that yields a single PSet,
    constructed by calling .painter_from_to() on every pair of addresses
    in the canvas. .painter_from_to() must be overridden in a concrete class.'''

    def canvas_to_psets(self, c: CanvasAble) -> Iterable[PSet]:
        yield self.painters_to_pset(self.canvas_to_painters(c))

class WithAbsolutePainters(CanvasToOnePSet, Absorb, Regenerate, ClarityWeight):
    mixin_name: ClassVar[str] = 'Abs'

    # Absorption

    def painter_from_to(self, a: Addr, b: Addr, xa: Value, xb: Value) \
    -> Painter | None:
        if a != b and xa is not None and xb is not None:
            func = self.func_from_to(xa, xb)
            if func is not None:
                return (a, b, func)
        return None

    ### Regeneration

    def run_painter(self, canvas: Canvas, painter: Painter) -> Outcome:
        a, b, f = painter
        assert not isinstance(a, Matcher)
        assert not isinstance(b, Jumper)
        #return self.paint_from_abs_addr(c, p)
        x = canvas[a]
        if x is None:
            return Failed
        y = self.apply_func(f, x)
        if y is None:
            return Failed
        canvas[b] = y
        return Succeeded

    def painter_weight(self, a: From, b: To, f: Func, c: Canvas) -> Numeric:
        if not isinstance(a, int):
            raise NotImplementedError(a)
        if not isinstance(b, int):
            raise NotImplementedError(b)
        wa = self.from_clarity_weight(c, c.clarity(a))
        wb = self.to_clarity_weight(c, c.clarity(b))
        return wa * wb * self.natural_func_weight(f)

class WithAdjacentRelativePainters(
    CanvasToOnePSet, Absorb, Regenerate, ClarityWeight
):
    mixin_name: ClassVar[str] = 'AdjRel'

    ### Absorption

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

    ### Regeneration

    def run_painter(self, canvas: Canvas, painter: Painter) -> Outcome:
        abspainters = list(self.as_absolute_painters(canvas, painter))
        if not abspainters:
            return Failed
        weights = [self.simple_painter_weight(canvas, p) for p in abspainters]
        return self.run_absolute_painter(
            canvas,
            choices(abspainters, weights)[0]
        )
        
    def run_absolute_painter(self, c: Canvas, painter: AbsPainter) -> Outcome:
        return self.paint_from_abs_addr(c, painter, painter[0])

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

    def as_addr_to(self, canvas: Canvas, a: Addr, to: To) -> Addr | None:
        if isinstance(to, int):
            return to
        elif isinstance(to, Jumper):
            return to.to(canvas, a)
        else:
            raise NotImplementedError

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

    def painter_weight(self, a: From, b: To, f: Func, c: Canvas) -> Numeric:
        return max([
            self.simple_painter_weight(c, absp)
                for absp in self.as_absolute_painters(c, (a, b, f))
        ], default=0)

    def simple_painter_weight(self, c: Canvas, painter: AbsPainter) -> Numeric:
        a, b, f = painter
        wa = self.from_clarity_weight(c, c.clarity(a))
        wb = 1.0 if isinstance(b, Jumper) \
                 else self.to_clarity_weight(c, c.clarity(b))
        return wa * wb * self.natural_func_weight(f)

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

@dataclass  # type: ignore[misc]
class WithNDupsDataclassMixin(RMem):
    ndups: ClassVar[int] = 2

@dataclass  # type: ignore[misc]
class WithNDups(Absorb, Regenerate):
    '''Replaces every canvas with .ndups duplicates preceding itself.'''
    ndups: int = 2

    ### Absorption

    def prep_absorb(self, c: CanvasAble) -> CanvasAble:
        return as_tuple(c) * (self.ndups + 1)

    ### Regeneration

    def prep_regen(self, c: CanvasAble) -> CanvasAble:
        t = as_tuple(c)
        blankdup: Tuple[Value, ...] = (None,) * len(t)
        return (blankdup * self.ndups) + t

class RMemAbs(
    WithAbsolutePainters, Absorb, LinearClarityWeight, Regenerate, RMem
):
    pass

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

if __name__ == '__main__':
#    rmem = RMem.run(
#        operands=range(1, 8),   # 4
#        startc=(None, '+', 1, None, 3),
#        prep=ndups(3),
#        niters=1000
#    )
    #rmtype = type('RMemAdjacent', (WithAdjacentRelativePainters, RMem), {})
    #rmem = rmtype(niters=30)
    rmem = RMemAbs.make_instance(
        (SkewedClarityWeight, WithAdjacentRelativePainters),
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
    pset = rmem.painters_to_pset(pps)
    pr(pset)

    startc = (1, None, None, None, None)
    c = rmem.regenerate(startc, pset, vv=4)
    print(c)
