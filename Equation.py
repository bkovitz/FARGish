# Equation.py

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from dataclasses import dataclass, field, InitVar
import math
import operator
from operator import itemgetter, attrgetter

from Graph2 import Node, Feature, FeatureWrapper


# Features

@dataclass(frozen=True)
class IncreaseOrDecrease(Feature):
    name: str

Increase = IncreaseOrDecrease('Increase')
Decrease = IncreaseOrDecrease('Decrease')

@dataclass(frozen=True)
class NumOperands(Feature):
    num: int

class Before(Feature):
    x: Node

class After(Feature):
    x: Node

class MinBefore(Feature):
    x: Node

class MaxBefore(Feature):
    x: Node

class Doubled(Feature):
    x: Node


# Operators and Equation

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str

    def __call__(self, *operands: int) -> int:
        return self.func(*operands)

    def __str__(self):
        return self.name

plus = Operator(operator.add, '+')
times = Operator(operator.mul, 'x')
minus = Operator(operator.sub, '-')

@dataclass(frozen=True)
class Equation:
    operands: Tuple[int]
    operator: Operator
    result: int

    @classmethod
    def make(cls, operands: Sequence[int], operator: Operator) -> 'Equation':
        return Equation(tuple(operands), operator, operator(*operands))

    def features(self) -> Iterable[Hashable]:
        for operand in self.operands:
            yield operand
            yield Before(operand)
        yield self.operator
        yield self.result
        yield After(self.result)
        if all(self.result > operand for operand in self.operands):
            yield Increase()
        elif any(self.result < operand for operand in self.operands):
            yield Decrease()
        counter = Counter(self.operands)
        for operand, count in counter.items():
            if count == 2:
                yield Doubled(operand)
        yield NumOperands(len(self.operands))
        mino = min(self.operands)
        maxo = max(self.operands)
        yield MinBefore(mino)
        yield MaxBefore(maxo)

    def __str__(self):
        expr = f' {self.operator} '.join(str(n) for n in self.operands)
        return f'{expr} = {self.result}'

