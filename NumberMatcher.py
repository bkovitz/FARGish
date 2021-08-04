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
from math import exp, log10
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
            self.multiplier = (
                1.0 / max(
                    self.f(p) for p in self.targets
                )
            )
    # TODO When do we make use of ub?

    #TODO noise
    #TODO scaling   log or linear
    def f(self, x: float, lb: float=None) -> float:
        if lb is not None:
            x /= lb / self.lb
        return sum(self.peakf(target, x) for target in self.targets) * self.multiplier

    def peakf(self, target, x) -> float:
        return (
            self.peakwidth
            /
            (math.pi * (x - target)**2 + (self.peakwidth / 2.0)**2)
        )
