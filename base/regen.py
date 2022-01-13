# regen.py -- A simple test of a 'regenerative network' for grade-school
#             arithmetic

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
import operator
from random import choice
from collections import defaultdict
from functools import reduce

from Log import lo, trace
from util import pr, ps, psa, union


Func = Union[Callable[[int], int], int, str, None]
Addr = int
Generator = Tuple[Addr, Addr, Func]
GSet = Dict[Tuple[Addr, Addr], Func]

def make_eqns(rands=range(1, 11), rators=('+', '-', 'x', '/'), ndups=1):
    for operand1 in rands:
        for operator in rators:
            for operand2 in rands:
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

def make_generators(eqn: Tuple) -> Set[Generator]:
    result = set()
    for addr1 in range(len(eqn)):
        for addr2 in range(len(eqn)):
            if addr1 != addr2:
                result.add(
                    (addr1, addr2, func_from_to(eqn[addr1], eqn[addr2]))
                )
    return result

def make_gset(gs: Iterable[Generator]) -> GSet:
    result: GSet = defaultdict(set)
    for g in gs:
        a1, a2, f = g
        result[(a1, a2)] = f
    return result

def add_two_gsets(gset1: GSet, gset2: GSet) -> GSet:
    '''Combine the gsets, analogous to '+' in the Hopfield equation to
    combine two images.'''
    result: GSet = defaultdict(set)
    edges = union(gset1.keys(), gset2.keys())
    for edge in edges:
        if edge in gset1:
            if edge in gset2:
                f = avg_of_funcs(
                    gset1[edge],
                    gset2[edge]
                )
            else:
                f = gset1[edge]
        else:
            f = gset2[edge]
        result[edge] = f
    return result

def add_gsets(*gset: GSet) -> GSet:
    result = reduce(add_two_gsets, gset, {})
    return dict(  # remove None funcs
        (k, v) for k, v in result.items() if v is not None
    )

def run_generator(canvas: List, gen: Generator) -> List:
    addr1, addr2, func = gen
    result = None
    if canvas[addr1] is not None:
        if callable(func):
            result = func(canvas[addr1])
        else:
            result = func
    else:
        if isinstance(func, str):
            result = func
    if result is not None:
        canvas[addr2] = result
    return canvas

def run_gset(canvas: List, gset: GSet, niters: int=20) -> List:
    for i in range(niters):
        gen = choose_runnable_generator(canvas, gset)
        run_generator(canvas, gen)
        print(canvas, '       ', gen)
    return canvas

def choose_runnable_generator(canvas: List, gset: GSet) -> Generator:
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

def avg_of_funcs(f1: Func, f2: Func) -> Func:
    if f1 is None or f2 is None:
        return None
    elif f1 == f2:
        return f1
    elif callable(f1) and callable(f2):
        return rndfunc(f1, f2)  # TODO Equalize probabilities when f1 or f2
                                # is itself a rndfunc
    else:
        return None

def func_from_to(x1, x2):
    '''Returns a function that maps x1 to x2.'''
    if x1 == x2:
        return same
    elif isinstance(x1, int) and isinstance(x2, int):
        return int_func_from_to(x1, x2)
    else:
        return x2

def int_func_from_to(x1: int, x2: int):
    if x1 == x2:
        return same
    elif x1 > x2:
        if x2 != 0:
            factor = x1 // x2
            if x1 * factor == x2:
                return div_by(factor)
        return sub_n(x1 - x2)
    else:
        if x1 != 0:
            factor = x2 // x1
            if x1 * factor == x2:
                return mul_by(factor)
        return add_n(x2 - x1)

def same(x: Any):
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
    f1: Callable
    f2: Callable

    def __call__(self, x: int) -> int:
        return choice((self.f1, self.f2))(x)

def run(
    startc=(None, '+', 3, None, 5),
    rands=range(1, 11),
    rators=('+', '-', 'x', '/'),
    ndups=1,
    nruns=10,
    niters=20
):
    global gg, eqns
    eqns = list(make_eqns(rands=rands, rators=rators, ndups=ndups))
    gg = add_gsets(*
        (make_gset(make_generators(e))
            for e in eqns
        )
    )
    for _ in range(nruns):
        c = list(startc) * ndups
        print(c)
        run_gset(c, gg, niters=niters)
        print()

def run_regen_one_eqn(
    eqn=(2, '+', 3, '=', 5),
    startc=(None, None, 3, None, 5),
    ndups=1,
    nruns=10,
    niters=20
):
    eqn = eqn * ndups
    global gg
    gg = make_gset(make_generators(eqn))
    for _ in range(nruns):
        c = list(startc) * ndups
        print(c)
        run_gset(c, gg, niters=niters)
        print()


if __name__ == '__main__':
    eqns = set(make_eqns())
    gens = union(*(make_generators(e) for e in eqns))
    psa(*gens)
    startc = (None, '+', 3, None, 10)
    c = list(startc)
    print(c)
    g = (0, 4, add_n(n=1))  #(4, 2, add_n(n=1))
    run_generator(c, g)
    print(c)
    a, b, f = g
    #print(avg_of_funcs('+', '-'))
    gset1 = make_gset(make_generators((2, '+', 3, '=', 5)))
    psa(*gset1.items())
    gset2 = make_gset(make_generators((4, 'x', 6, '=', 24)))
    print()
    psa(*gset1.items())
    gset = add_gsets(gset1, gset2)
    print()
    psa(*gset.items())
    print()
    gsets = [make_gset(make_generators(e)) for e in eqns]
    gg = add_gsets(*gsets)
    #psa(*gg.items())

    run(
        rands=range(1, 4),
        startc=(None, '+', 1, None, None),
        ndups=10,
        nruns=1,
        niters=100
    )
