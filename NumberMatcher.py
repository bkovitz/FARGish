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
from math import exp
import math
from abc import ABC, abstractmethod
from itertools import chain


@dataclass
class NumberMatcher:
    lb: float
    ub: float
    peaks: List[float]
    peakwidth: float

    multiplier: float = field(default=1.0, init=False)

    def __post_init__(self):
        if self.peaks:
            # Normalize peaks so max f(peak) = 1.0
            self.multiplier = (
                1.0 / max(
                    self.f(p) for p in self.peaks
                )
            )
    # TODO When do we make use of lb and ub?

    #TODO noise
    #TODO scaling   log or linear
    def f(self, x: float) -> float:
        return sum(self.peakf(peak, x) for peak in self.peaks) * self.multiplier

    def peakf(self, peak, x) -> float:
        return (
            self.peakwidth
            /
            (math.pi * (x - peak)**2 + (self.peakwidth / 2.0)**2)
        )
