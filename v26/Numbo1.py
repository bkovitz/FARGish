# Numbo1.py -- 1st Numbo in ad hoc style

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
import operator

from util import as_tuple, T, sample_without_replacement


@dataclass(frozen=True)
class Operator:
    name: str
    f: Callable

    def __call__(self, *args) -> int:
        return self.f(*args)

@dataclass(frozen=True)
class SolutionStep:
    operator: Operator
    operands: Tuple[int, ...]

    def result(self) -> int:
        return self.operator(*self.operands)

    def __str__(self) -> str:
        return f' {self.operator.name} '.join(str(n) for n in self.operands)

#@dataclass(frozen=True)
#class NumboState


def detect_three_tens(avails: Collection[int]) -> Optional[Sequence[int]]:
    matching = [a for a in avails if a >=10 and a <= 19]
    if len(matching) < 3:
        return None
    else:
        return list(sample_without_replacement(matching, k=3))

def dt3_and_mul(avails: Collection[int]) -> Optional[SolutionStep]:
    operands = detect_three_tens(avails)
    if operands:
        return SolutionStep(times, as_tuple(operands))
    else:
        return None

@dataclass(frozen=True)
class CantSolve(Exception):
    pass

def without(a: Collection[T], b: Collection[T]) -> Tuple[T, ...]:
    '''Returns the result of removing all the elements of 'b' from 'a'.'''
    # TODO Throw exception or something if 'b' includes an element that is
    # not in 'a'.
    a: List[T] = list(a)
    for xb in b:
        i = a.index(xb)  # TODO will raise ValueError if xb is not found
        del a[i]
    return tuple(a)

def next_avails(avails: Collection[int], step: SolutionStep) -> Tuple[int, ...]:
    '''Returns the avails that result when performing 'step' on 'avails'.
    TODO: Throw exception or something if step cannot be performed because
    an operand is missing.'''
    return without(avails, step.operands) + (step.result(),)
    # rm operands from avails
    # perform step
    # put the result into the avails


Solution = Tuple[SolutionStep, ...]

plus = Operator('+', operator.add)
times = Operator('*', operator.mul)

def numbo(bricks: Collection[int], target: int) -> Solution:
    step = SolutionStep(plus, as_tuple(bricks))
    avails = next_avails(bricks, step)
    if target in avails:
        return (step,)
    else:
        raise CantSolve

if __name__ == '__main__':
    solution = numbo([1, 1], 2)
    print(', '.join(str(step) for step in solution))
