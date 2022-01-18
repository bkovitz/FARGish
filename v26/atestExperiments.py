# atestExperiments.py -- Experiments to save and repeat for the dissertation

import unittest
from pprint import pprint as pp
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from collections import defaultdict

from Experiments import xp_single, just_1_1_2
from RMem import RMem, CanvasPrep, make_eqns, BaseValue, ndups
from Log import lo, trace
from util import as_tuple, pr, pts, ps, pss, psa, sample_without_replacement, \
    reseed


def partial_eqn(eqn: Tuple[BaseValue, ...], k: int=3) -> Tuple[BaseValue, ...]:
    r = range(len(eqn))
    addrs = set(sample_without_replacement(r, k=k))
    return tuple(
        eqn[a] if a in addrs else None
            for a in r
    )

EqnCounter = Dict[Tuple[BaseValue, ...], int]

def run_full_table_test(
    show: bool=False,
    prep: Optional[CanvasPrep]=None,
    n_per_eqn: int=3,
    n_eqns: int=20,
    niters: int=50,
    seed: int=None
) -> EqnCounter:
    reseed(seed)
    full_table = tuple(make_eqns())
    l = len(full_table[0])
    rmem = RMem.make_from(full_table, prep=prep)
    counter: EqnCounter = defaultdict(int)

    for eqn in sample_without_replacement(full_table, k=n_eqns):
        if show:
            print(eqn)
        for i in range(n_per_eqn):
            startc = partial_eqn(eqn)
            got = rmem.run_gset(canvas=startc, niters=niters).as_tuple()
            if show:
                print(got)
            if got == startc:
                counter[eqn] += 1
        if show:
            print()
    return counter

class ATestExperiments(unittest.TestCase):

    def test_xp_single(self) -> None:
        # Verifies that an RMem trained on a single equation can regenerate
        # that equation reliably.
        rmem = just_1_1_2()
        expect = (1, '+', 1, '=', 2)
        for startc in [
            (1, None, None, None, None),
            (None, '+', 1, None, None),
            (None, None, None, None, 2)
        ]:
            got = [rmem.run_gset(canvas=startc) for _ in range(20)]
            self.assertTrue(
                all(canvas.as_tuple() == expect for canvas in got),
                f'Failed on {startc}'
            )

    def test_full_table_crude(self) -> None:
        counter = run_full_table_test(n_per_eqn=2, n_eqns=20)
        num_correct = sum(counter.values())
        #pr(counter)
        # There's no assertion, because the number of correct completions
        # is expected to be very low, even zero much of the time. So, this
        # is just a smoke test.

    def test_full_table_with_dups(self) -> None:
        counter = run_full_table_test(
            prep=ndups(3), seed=0, niters=50, n_per_eqn=3, n_eqns=20
        )
        num_correct = sum(counter.values())
        self.assertGreaterEqual(num_correct, 2)
            # a low standard: this method gets poor results
