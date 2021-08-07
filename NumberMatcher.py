# NumberMatcher.py -- Classes for matching numbers against a target

from pprint import pprint as pp
import inspect
from time import process_time
import csv

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from numbers import Number, Real
from math import exp, log10, copysign
import math
from abc import ABC, abstractmethod
from itertools import chain


@dataclass
class NumberMatcher:
    lb: float
    ub: float
    targets: List[float]
    peakwidth: float

    multiplier: float = field(default=1.0, init=False)

    def __post_init__(self):
        if self.targets:
            # Normalize peaks so max f(target) = 1.0
            self.multiplier = 1.0 / max(
                self.f(p) for p in self.targets
            )
    # TODO When do we make use of ub?

    #TODO noise
    #TODO scaling   log or linear
    def f(self, x: float, lb: float=None) -> float:
        if lb is not None:
            x /= lb / self.lb
        return sum(
            self.peakf(target, x) for target in self.targets
        ) * self.multiplier

    __call__ = f

    def peakf(self, target, x) -> float:
        return (
            self.peakwidth
            /
            (math.pi * (x - target)**2 + (self.peakwidth / 2.0)**2)
        )

    # TODO Let caller specify lb and ub?
    # TODO Allow multiple targets.
    @classmethod
    def make(cls, target: int) -> Callable[[Any], float]:
        lb, ub = oom_bounds(target)
        return NumberMatcher(
            lb=lb, ub=ub, targets=[target], peakwidth=0.01
        )

# TODO UT
def oom_bounds(target: int) -> Tuple[int, int]:
    '''Returns (lb, ub): lower and upper bounds of order of magnitude that
    contains target.'''
    # TODO Handle zero
    # TODO Handle negative numbers
    characteristic = int(log10(target))
    return (10 ** characteristic, 10 ** (characteristic + 1))
