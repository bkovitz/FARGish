#! env python3
# tryall.py -- Brute-force, "computer-science" numble-solver
#
# tryall() solves numbles by trying all possibilities.

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from pprint import pprint as pp
import sys


def add(x, y):
    return f'({x} + {y})'

def sub(x, y):
    return f'({x} - {y})'

def revsub(x, y):
    return f'({y} - {x})'

def mul(x, y):
    return f'({x} * {y})'

def div(x, y):
    return f'({x} / {y})'

def genall(
    xs: List[int],
    ops: List[Callable] = [add, sub, revsub, mul]
):
    if not xs:
        return
    for i in range(len(xs)):
        x = xs[i]
        yield str(x)
        ys = xs[:i] + xs[i+1:]
        for y in genall(ys, ops):
            for op in ops:
                yield op(x, y)

def tryall(
    target: int,
    bricks: List[int],
    ops: List[Callable] = [add, sub, revsub, mul]
) -> Iterable[str]:
    for expr in genall(bricks, ops):
        if eval(expr) == target:
            yield f'{expr} = {target}'

def run(
    target: int,
    bricks: List[int],
    ops: List[Callable] = [add, sub, revsub, mul]
) -> None:
    solutions = sorted(tryall(target, bricks, ops))
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

