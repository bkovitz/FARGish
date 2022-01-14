# RMem.py -- Classes and functions for regenerative memory

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
import operator
from random import choice, choices
from collections import defaultdict, Counter
from functools import reduce
import itertools

from Log import lo, trace
from util import pr, ps, psa, union, Numeric, as_tuple, short, as_list


BaseValue = Union[int, str, None]
BaseFunc = Callable[['Func'], 'Func']  # type: ignore[misc]
Func = Union[BaseFunc, BaseValue] # type: ignore[misc]
Addr = int
Generator = Tuple[Addr, Addr, Func] # type: ignore[misc]
GSet = Dict[Tuple[Addr, Addr], Func] # type: ignore[misc]
Value = Func # type: ignore[misc]

def make_eqns(operands=range(1, 11), operators=('+', '-', 'x', '/'), ndups=1):
    for operand1 in operands:
        for operator in operators:
            for operand2 in operands:
                try:
                    result = calculate(operand1, operator, operand2)
                except ArithmeticFailed:
                    continue
                if result > 0:
                    eqn = (
                        operand1,
                        operator,
                        operand2,
                        '=',
                        result
                    )
                    yield eqn * ndups

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


@dataclass
class Canvas:
    contents: List[Value]

    def all_addrs(self) -> Iterable[Addr]:
        return range(len(self.contents))

    def __getitem__(self, addr: Addr) -> Value:
        try:
            return self.contents[addr]
        except IndexError:
            return None

    def __setitem__(self, addr: Addr, x: Value) -> None:
        try:
            self.contents[addr] = x
        except IndexError:
            # TODO Stretch the canvas?
            pass

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        return f'[{items}]'

CanvasAble = Union[Sequence[Value], Canvas]  # type: ignore[misc]

@dataclass
class RMem:
    '''Regenerative memory.'''
    Q = TypeVar('Q', bound='RMem')
    gset: GSet = field(default_factory=dict)

    # Factories / converters

    @classmethod
    def make_from(cls: Type[Q], cs: Iterable[CanvasAble]) -> Q:
        return cls().absorb_canvases(cs)

    @classmethod
    def as_canvas(cls, c: CanvasAble) -> Canvas:
        '''Converts c to a Canvas.'''
        if isinstance(c, Canvas):
            return c
        else:
            return Canvas(as_list(c))

    # Making generators (i.e. painters)

    @classmethod
    def make_generators(cls, c: CanvasAble) -> Set[Generator]:
        c: Canvas = cls.as_canvas(c)
        result = set()
        for addr1 in c.all_addrs():
            for addr2 in c.all_addrs():
                if addr1 != addr2:
                    result.add(
                        (addr1, addr2, cls.func_from_to(c[addr1], c[addr2]))
                    )
        return result

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
        self, canvas: CanvasAble, gset: Optional[GSet]=None, niters: int=20
    ) -> Canvas:
        '''Attempts to fill in canvas by running gset.'''
        gset: GSet = self.gset if gset is None else gset
        canvas: Canvas = self.as_canvas(canvas)
        for i in range(niters):
            gen = self.choose_runnable_generator(canvas, gset)
            self.run_generator(canvas, gen)
            print(canvas, '       ', short(gen))
        return canvas

    def choose_runnable_generator(self, canvas: Canvas, gset: GSet) \
    -> Generator:
        keys = [
            (a1, a2)
                for (a1, a2) in gset.keys()
                    #if canvas[a1] is not None and canvas[a2] is None
                    if canvas[a1] is not None
        ]
        if not keys:
            return (0, 0, None)
        else:
            a1, a2 = choice(keys)
            f = gset[(a1, a2)]
            return (a1, a2, f)

    # Making GSets (sets of generators)

    @classmethod
    def make_gset(cls, gs: Iterable[Generator]) -> GSet:
        result: GSet = defaultdict(set)  # type: ignore[arg-type]
        for g in gs:
            a1, a2, f = g
            result[(a1, a2)] = f
        return result

    @classmethod
    def add_two_gsets(cls, gset1: GSet, gset2: GSet) -> GSet:
        '''Combine the gsets, analogous to '+' in the Hopfield equation to
        combine two images.'''
        result: GSet = defaultdict(set)  # type: ignore[arg-type]
        edges = union(gset1.keys(), gset2.keys())
        for edge in edges:
            if edge in gset1:
                if edge in gset2:
                    f = cls.avg_of_funcs(
                        gset1[edge],
                        gset2[edge]
                    )
                else:
                    f = gset1[edge]
            else:
                f = gset2[edge]
            result[edge] = f
        return result

    @classmethod
    def add_gsets(cls, gsets: Iterable[GSet]) -> GSet:
        result: GSet = reduce(cls.add_two_gsets, gsets, {})
        return dict(  # remove None funcs
            (k, v) for k, v in result.items() if v is not None
        )

    def absorb_canvases(self: Q, cs: Iterable[CanvasAble]) -> Q:
        self.gset = self.add_gsets(
            itertools.chain(
                [self.gset],
                (self.make_gset(self.make_generators(c)) for c in cs)
            )
        )
        return self

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
        if x1 == x2:
            return cls.same
        elif isinstance(x1, int) and isinstance(x2, int):
            return cls.int_func_from_to(x1, x2)
        else:
            return x2

    @classmethod
    def int_func_from_to(cls, x1: int, x2: int):
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

    @classmethod
    def avg_of_funcs(cls, f1: Func, f2: Func) -> Func:
        if f1 is None or f2 is None:
            return None
        elif f1 == f2:
            return f1
        elif hasattr(f1, 'avg_with'):
            return f1.avg_with(f2)  # type: ignore[union-attr]
        elif hasattr(f2, 'avg_with'):
            return f2.avg_with(f1)  # type: ignore[union-attr]
        else:
            return cls.rndfunc.make(cls, [f1, f2])

    # Functions

    @classmethod
    def same(cls, x: Any):
        return x

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
        rmem: Union[RMem, Type[RMem]]
        funcs: Tuple[Func]     # funcs and weights must have same # of elems
        weights: Tuple[Numeric]

        def __call__(self, x: Value) -> Value:
            func: Func = choices(self.funcs, weights=self.weights)[0]
            return self.rmem.apply_func(func, x)

        @classmethod
        def make(
            cls: Type[Q],
            rmem: Union[RMem, Type[RMem]],
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
            rmem: Union[RMem, Type[RMem]],
            c: Dict[Func, int]
        ) -> Q:
            return cls(
                rmem=rmem,
                funcs=as_tuple(c.keys()),  # type: ignore[arg-type]
                weights=as_tuple(c.values())  # type: ignore[arg-type]
            )

    @classmethod
    def run(
        cls: Type[Q],
        startc=(None, '+', 3, None, 5),
        operands=range(1, 11),
        operators=('+', '-', 'x', '/'),
        ndups=1,
        nruns=1,
        niters=40
    ) -> Q:
        '''Runs a whole experiment from start to finish. Creates and returns
        an RMem object so you can inspect the generators afterward.'''
        rmem = cls.make_from(
            make_eqns(operands=operands, operators=operators, ndups=ndups)
        )

        for _ in range(nruns):
            c = list(startc) * ndups
            print(c)
            rmem.run_gset(c, niters=niters)
            print()

        return rmem

if __name__ == '__main__':
    rmem = RMem()
    rmem.run(
        operands=range(1, 4),
        startc=(None, '+', 1, None, 3),
        ndups=5
    )
