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

from util import as_list, as_iter, is_iter


class NumberMatcher(ABC):
    # TODO Let caller specify lb and ub?
    # TODO Allow multiple targets.
    '''
    @classmethod
    def OLDmake(cls, *targets: Union[int, Sequence[int]], peakwidth=0.01) \
    -> Callable[[Any], float]:
        #lb, ub = oom_bounds(target)
        targets: List[int] = as_list(targets)
        if not targets:
            lb, ub = oom_bounds(0)
        else:
            bs = list(chain.from_iterable(oom_bounds(t) for t in targets))
            lb = min(bs)
            ub = max(bs)
        return SingleNumberMatcher(
            lb=lb, ub=ub, targets=targets, peakwidth=peakwidth
        )
    '''
    @abstractmethod
    def __call__(self, x: float, lb: float=None) -> float:
        pass

    @classmethod
    def make(cls, *targets: Union[int, Sequence[int]], peakwidth=0.01) \
    -> Callable[[Any], float]:
        if len(targets) == 1:
            return cls.matcher_for_one_target(targets[0], peakwidth=peakwidth)
        else:
            return cls.Or(*(
                cls.make(t, peakwidth=peakwidth) for t in as_iter(targets)
            ))

    @classmethod
    def Or(cls, *matchers) -> Callable[[Any], float]:
        if len(matchers) == 1:
            return matchers[0]
        else:
            return SumOfMatchers(matchers)

    @classmethod
    def matcher_for_one_target(cls, target, peakwidth=0.01) \
    -> Callable[[Any], float]:
        if is_iter(target):
            return NumberTupleMatcher(tuple(
                cls.make(t, peakwidth=peakwidth) for t in target  # type: ignore  #BUG
            ))
        else:
            return SingleNumberMatcher(target, peakwidth=peakwidth)

@dataclass
class OLDSingleNumberMatcher:
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

@dataclass
class SingleNumberMatcher:
    target: float
    peakwidth: float
    lb: float = field(default=None)  # type: ignore[assignment]  # mypy how?
    ub: float = field(default=None)  # type: ignore[assignment]  # mypy how?

    multiplier: float = field(default=1.0, init=False)

    def __post_init__(self):
        if self.lb is None or self.ub is None:
            self.lb, self.ub = oom_bounds(self.target)
        self.multiplier = 1.0 / self(self.target)

    def __call__(self, x: float, lb: float=None) -> float:
        if lb is not None:
            x /= lb / self.lb
        return self.multiplier * (
            self.peakwidth
            /
            (math.pi * (x - self.target)**2 + (self.peakwidth / 2.0)**2)
        )


@dataclass
class SumOfMatchers:
    matchers: Sequence

    lb: float = field(default=None)  # type: ignore[assignment]  # mypy how?
    ub: float = field(default=None)  # type: ignore[assignment]  # mypy how?

    def __post_init__(self):
        if self.lb is None or self.ub is None:
            self.lb, self.ub = oom_for_all(self.matchers)
        self.matchers = [
            replace(m, lb=self.lb, ub=self.ub) for m in self.matchers
        ]

    def __call__(self, x, lb=None) -> float:
        return min([1.0, sum(m(x, lb=lb) for m in self.matchers)])

# This might be the right way to do NumbleTupleMatcher, but that's not clear
# as of 11-Aug-2021, so I'm going with the simpler approach in
# NumberTupleMatcher for now. (BEN)
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
        target: List[int] = as_list(x)
        return max(
            self.try_ms(ms, target)  # type: ignore[arg-type]  # mypy how?
                for ms in permutations(self.matchers)
        )

    def try_ms(self, ms: Tuple[Callable[..., float]], x: List[int]) -> float:
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

    lb: float = field(default=None)  # type: ignore[assignment]  # mypy how?
    ub: float = field(default=None)  # type: ignore[assignment]  # mypy how?

    def __post_init__(self):
        if self.lb is None or self.ub is None:
            self.lb, self.ub = oom_for_all(self.matchers)
        self.matchers = [
            replace(m, lb=self.lb, ub=self.ub) for m in self.matchers
        ]

    def __call__(self, x, lb=None) -> float:
        '''How well does x satisfy 'has an element for each of the matchers'?'''
        x = list(as_iter(x))  # local copy of x, so we can remove items
                              # when matched
        if len(x) < len(self.matchers):
            return 0.0

        best_matches: List[IndexAndWeight] = []
        for m in self.matchers:
            bm = max(
                (IndexAndWeight(i, m(x[i], lb=lb)) for i in range(len(x))),
                key=attrgetter('w')
            )
            if bm.w < 0.001:
                return 0.0
            best_matches.append(bm)
            del x[bm.i]
        return reduce(operator.mul, (bm.w for bm in best_matches))
                
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

def oom_for_all(matchers) -> Tuple[int, int]:
    bs = list(chain.from_iterable((m.lb, m.ub) for m in matchers))
    lb = min(bs)
    ub = max(bs)
    return lb, ub
