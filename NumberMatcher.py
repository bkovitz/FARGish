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

from util import as_list


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
    def make(cls, *targets: Union[int, Sequence[int]], peakwidth=0.01) \
    -> Callable[[Any], float]:
        #lb, ub = oom_bounds(target)
        targets = as_list(targets)
        if not targets:
            lb, ub = oom_bounds(0)
        else:
            bs = list(chain.from_iterable(oom_bounds(t) for t in targets))
            lb = min(bs)
            ub = max(bs)
        return NumberMatcher(
            lb=lb, ub=ub, targets=targets, peakwidth=peakwidth
        )

# TODO UT
def oom_bounds(target: int) -> Tuple[int, int]:
    '''Returns (lb, ub): lower and upper bounds of order of magnitude that
    contains target.'''
    # TODO Handle zero
    sgn = copysign(1.0, target)
    if target == 0:
        return (-1, +1)
    characteristic = int(log10(abs(target)))
    lb, ub = 10 ** characteristic, 10 ** (characteristic + 1)
    if sgn >= 0.0:
        return (lb, ub)
    else:
        return (-ub, -lb)
        
