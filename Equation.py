# Equation.py

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from dataclasses import dataclass, field, InitVar
import math
import operator
from operator import itemgetter, attrgetter
from collections import Counter

from Graph2 import Node, Feature, FeatureWrapper


# Features

@dataclass(frozen=True)
class IncreaseOrDecrease(Feature):
    name: str

    def __str__(self):
        return self.name

Increase = IncreaseOrDecrease('Increase')
Decrease = IncreaseOrDecrease('Decrease')

@dataclass(frozen=True)
class NumOperands(Feature):
    num: int

    def features_of(self):
        yield self.num

@dataclass(frozen=True)
class Before(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class After(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class MinBefore(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class MaxBefore(Feature):
    x: Node

    def features_of(self):
        yield self.x

@dataclass(frozen=True)
class Doubled(Feature):
    x: Node

    def features_of(self):
        yield self.x


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
    operands: Tuple[int, ...]
    operator: Operator
    result: int

    @classmethod
    def make(cls, operands: Sequence[int], operator: Operator) -> 'Equation':
        return Equation(tuple(operands), operator, operator(*operands))

    @classmethod
    def make_table(
        cls,
        rands1: Iterable[int],
        rands2: Iterable[int],
        rators: Iterable[Operator]
    ) -> Iterable['Equation']:
        for rand1 in rands1:
            for rand2 in rands2:
                for rator in rators:
                    if rand1 >= rand2:
                        e = cls.make([rand1, rand2], rator)
                        if e.result != rand1 and e.result != rand2:
                            yield e

    def features_of(self) -> Iterable[Hashable]:
        for operand in self.operands:
            #yield operand
            yield Before(operand)
        yield self.operator
        #yield self.result
        yield After(self.result)
        if all(self.result > operand for operand in self.operands):
            yield Increase
        elif any(self.result < operand for operand in self.operands):
            yield Decrease
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

    def __repr__(self):
        return f'Equation({str(self)})'
