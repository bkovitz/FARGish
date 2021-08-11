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
from itertools import chain, permutations
from functools import reduce
import operator
from operator import attrgetter

from util import as_list, as_iter


@dataclass
class SingleNumberMatcher:
    lb: float
    ub: float
    targets: Sequence[float]
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
    # TODO type hint should allow any kind of number
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

class NumberMatcher:
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
        return SingleNumberMatcher(
            lb=lb, ub=ub, targets=targets, peakwidth=peakwidth
        )

# This might be the right way to do NumbleTupleMatcher, but that's not clear
# as of 11-Aug-2021, so I'm going with a simpler approach.
class WrongNumberTupleMatcher:
    '''A horrible hack way of matching against a tuple. We just try all the
    possible sequences of matchers for the individual members of the tuple
    and return the highest match value.'''
    targetss: Sequence[Sequence[float]]
    threshold: float = 0.1

    matchers: List[NumberMatcher] = field(default_factory=list, init=False)
    peakwidth: float

    def __post_init__(self):
        for targets in targetss:
            self.matchers.append(
                NumberMatcher.make(*targets, peakwidth=peakwidth)
            )

    def __call__(self, x: Union[int, None, Sequence[int]]) -> float:
        '''How well does x match self.targetss?'''
        target = as_list(target)
        return max(
            self.try_ms(ms, x)
                for ms in permutations(self.matchers)
        )

    def try_ms(self, ms: Tuple[Callable], x: List[int]) -> float:
        '''The result of trying ms on x, one matcher/number at a time.'''
        return reduce(
            operator.mul,
            (m(val) for m, val in zip(ms, x)),
            1.0
        )

@dataclass(frozen=True)
class IndexAndWeight:
    i: int     # index of matched item (in some sequence not specified here)
    w: float   # weight associated with the value at that index

@dataclass
class NumberTupleMatcher:
    '''Returns how well a sequence of arbitrary length matches a given target
    tuple of SingleNumberMatchers. For example, if there are two
    SingleNumberMatchers, each tuned to 4, then the sequence (3, 4, 7 12, 4, 4)
    is a very good match because it contains at least two 4's.

    This is probably not the right way to match a tuple against a sequence,
    but it's simple and it'll do for now. 11-Aug-2021.'''
    matchers: Sequence[SingleNumberMatcher]

    def __call__(self, x: Union[int, None, Sequence[int]]) -> float:
        '''How well does x match self.targetss?'''
        x = list(as_iter(x))  # local copy of x, so we can remove items
                              # when matched
        if len(x) < len(self.matchers):
            return 0.0

        best_matches: List[IndexAndWeight] = []
        for m in self.matchers:
            bm = max(
                (IndexAndWeight(i, m(x[i])) for i in range(len(x))),
                key=attrgetter('w')
            )
            if bm.w < 0.001:
                return 0.0
            best_matches.append(bm)
            del x[bm.i]
        print('NTM', best_matches)
        return reduce(operator.mul, (bm.w for bm in best_matches))
                
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
        