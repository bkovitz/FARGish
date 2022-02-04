# atestExperiments.py -- Experiments to save and repeat for the dissertation

import unittest
from pprint import pprint as pp
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Experiments import xp_single, just_1_1_2, eqn_test
from RMem import RMem, CanvasPrep, make_eqns, BaseValue, ndups, no_prep
from Log import lo, trace
from util import as_tuple, pr, pts, ps, pss, psa, sample_without_replacement, \
    reseed


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
            got = [rmem.regenerate(canvas=startc) for _ in range(20)]
            self.assertTrue(
                all(canvas.as_tuple() == expect for canvas in got),
                f'Failed on {startc}'
            )

    def test_full_table_crude(self) -> None:
        counter = eqn_test(n_per_eqn=2, n_eqns=20)
        num_correct = sum(counter.values())
        #pr(counter)
        # There's no assertion, because the number of correct completions
        # is expected to be very low, even zero much of the time. So, this
        # is just a smoke test.

    def test_full_table_with_dups(self) -> None:
        # TODO Run a Cartesian test on these parameters and save the
        # results.
        counter = eqn_test(
            #prep=ndups(3),
            seed=1, niters=20, n_per_eqn=10, n_eqns=50
            # 3, 1, 20, 10, 50  -->  num_correct = 10, 22-Jan-2022
        )
        num_correct = sum(counter.values())
        # lo('NUMC', num_correct)
        self.assertGreaterEqual(num_correct, 2)
            # a low standard: this method gets poor results
