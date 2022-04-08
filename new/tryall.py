#! env python3
# tryall.py -- Brute-force, "computer-science" numble-solver
#
# tryall() solves numbles by trying all possibilities.

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from itertools import chain, combinations
from pprint import pprint as pp
import sys

from Log import lo, trace
from util import pts, unique_everseen


@dataclass(frozen=True, eq=False)
class Op:
    x: int
    y: int

    symbol: ClassVar[str]

    def __str__(self) -> str:
        return f'({self.x} {self.symbol} {self.y})'

    def O__eq__(self, other) -> bool:
        return str(self) == str(other)

    def __hash__(self) -> int:
        return hash((self.__class__, self.x, self.y))

class Commutative:
    def __eq__(self, other) -> bool:
        match other:
            case self.__class__(ox, oy):
                return (
                    (self.x == ox and self.y == oy)
                    or
                    (self.x == oy and self.y == ox)
                )
            case _:
                return False

@dataclass(frozen=True, eq=False)
class Add(Commutative, Op):
    symbol = '+'

    def __hash__(self) -> int:
        return hash((self.__class__, frozenset([self.x, self.y])))

@dataclass(frozen=True, eq=False)
class Sub(Op):
    symbol = '-'

    def __eq__(self, other) -> bool:
        match other:
            case Sub(x, y) if x == self.x and y == self.y:
                return True
            case Revsub(y, x) if x == self.x and y == self.y:
                return True
            case _:
                return False

    def __hash__(self) -> int:
        return hash(frozenset([self.x, self.y]))

@dataclass(frozen=True, eq=False)
class Revsub(Sub):
    symbol = '-'

    def __str__(self) -> str:
        return f'({self.y} {self.symbol} {self.x})'

    def __eq__(self, other) -> bool:
        match other:
            case Sub(y, x) if x == self.x and y == self.y:
                return True
            case Revsub(x, y) if x == self.x and y == self.y:
                return True
            case _:
                return False

    def __hash__(self) -> int:
        return hash(frozenset([self.x, self.y]))

@dataclass(frozen=True, eq=False)
class Mul(Commutative, Op):
    symbol = '*'

    def __hash__(self) -> int:
        return hash((self.__class__, frozenset([self.x, self.y])))

@dataclass(frozen=True)
class Div(Op):
    symbol = '/'

@dataclass(frozen=True)
class RevDiv(Revsub):
    symbol = '/'

def genall_(
    xs: List[int],
    ops: List[Callable] = [Add, Sub, Revsub, Mul]
) -> Iterable[int | Op]:
    match xs:
        case []:
            return
        case [a]:
            yield a
        case [a, b]:
            yield a
            yield b
            yield from (op(a, b) for op in ops)
        case _:
            for xs1, xs2 in all_2partitions(xs):
                for expr1 in genall(xs1, ops):
                    yield expr1
                    for expr2 in genall(xs2, ops):
                        yield expr2
                        for op in ops:
                            yield op(expr1, expr2)

def genall(
    xs: List[int],
    ops: List[Callable] = [Add, Sub, Revsub, Mul]
) -> Iterable[str]:
    yield from unique_everseen(genall_(xs, ops))

def all_2partitions(xs: List[int]) -> Iterable[Tuple[Tuple[int], Tuple[int]]]:
    '''All non-empty partitions of xs into two subsets.'''
    for lhs in (
        chain.from_iterable(
            combinations(xs, n) for n in range(1, len(xs))
        )
    ):
        yield (lhs, tuple(x for x in xs if x not in lhs))

def tryall(
    target: int,
    bricks: List[int],
    ops: List[Callable] = [Add, Sub, Revsub, Mul]
) -> Iterable[str]:
    for expr in genall(bricks, ops):
        if eval(str(expr)) == target:
            yield f'{expr} = {target}'
            #yield repr(expr)

def run(
    target: int,
    bricks: List[int],
    ops: List[Callable] = [Add, Sub, Revsub, Mul]
) -> None:
    #solutions = sorted(tryall(target, bricks, ops))
    solutions = list(tryall(target, bricks, ops))
    for solution in solutions:
        print(solution)
    if not solutions:
        print('No solution.')
    else:
        print(len(solutions), 'solutions')


if __name__ == '__main__':
    #run(146, [2, 3, 4, 6, 13, 15])
    #run(152, [2, 3, 4, 6, 13, 15])
    #run(464, [11, 15, 22, 80, 19])  # no solutions
    #run(464, [2, 11, 15, 22, 80, 19])
    #run(463, [2, 11, 15, 22, 80, 19])
    #run(77, [2, 3, 6, 9])  # no solutions
    #run(156, [1, 4, 7, 12])  # no solutions
    #run(156, [1, 4, 7, 12, 14])   # good
    #run(91, [5, 6, 7, 8, 10])  # good
    #run(121, [1, 1, 3, 5, 7, 10])  # good
    #run(171, [2, 4, 8, 11, 12])  # good  #4  (hard)

    #print(sys.argv[1])
    #print(sys.argv[2:])

    args = [int(a) for a in sys.argv[1:]]
    run(args[0], args[1:])

    #pts(genall([2, 3, 4, 5, 6, 7]))

