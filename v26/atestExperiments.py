# atestExperiments.py -- Experiments to save and repeat for the dissertation

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from RMem import RMem, RMemAbs, WithAbsolutePainters, Absorb, Value, \
    LinearClarityWeight, Regenerate, WithNDups, make_eqns, BaseValue
from Mixins import WithCountColumns, WithRandomSalt, WithSequentialSalt
from Experiments import xp_single, just_1_1_2, eqn_test
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
        cls = RMem.make_class(
            (WithAbsolutePainters, WithNDups, Absorb, LinearClarityWeight,
             Regenerate)
        )
        counter = eqn_test(
            rm=cls,
            seed=1, niters=20, n_per_eqn=10, n_eqns=50
            # 3, 1, 20, 10, 50  -->  num_correct = 10, 22-Jan-2022
        )
        num_correct = sum(counter.values())
        # lo('NUMC', num_correct)
        self.assertGreaterEqual(num_correct, 2)
            # a low standard: this method gets poor results

    eqns = list(make_eqns(operands=[1, 2, 3], operators=['+']))

    @dataclass(frozen=True)
    class CC:
        '''A cue to give an RMem to regenerate from, and the correct response
        that it should regenerate.'''
        cue: Tuple[Value, ...]
        correct: Tuple[Value, ...]

    ccs = [
        CC((None, '+', 1, '=', 2), (1, '+', 1, '=', 2)),
        CC((1, None, None, None, 2), (1, '+', 1, '=', 2)),
        CC((3, None, 1, None, None), (3, '+', 1, '=', 4))
    ]

    def quick_test(
        self,
        rmem_cls: Type[RMem],
        show: bool=False,
        **kwargs
    ) -> int:
        '''Runs a quick test on an RMem, giving it a standard small set of
        canvases to absorb, and checking its ability to reconstruct a few
        equations.'''
        reseed(0)
        rmem = rmem_cls(**kwargs).absorb_canvases(self.eqns)
        #print(rmem.termination_threshold, rmem_cls.niters, rmem.niters) # type: ignore[attr-defined]
        num_correct = 0
        for cc in self.ccs:
            for i in range(40):
                got = rmem.regenerate(cc.cue).as_tuple()
                if show:
                    lo(cc.cue, got)
                if got[-len(cc.cue):] == cc.cue:
                    num_correct += 1
        if show:
            lo(f'num_correct = {num_correct}')
        return num_correct

    def test_count_columns(self) -> None:
        n = self.quick_test(
            RMem.make_class((WithCountColumns, RMemAbs)),
            #niters=100,
            #termination_threshold=3
        )
        self.assertGreaterEqual(n, 2)

    def test_random_salt(self) -> None:
        n = self.quick_test(
            RMem.make_class((WithRandomSalt, RMemAbs)),
        )
        self.assertGreaterEqual(n, 8)

    def test_sequential_salt(self) -> None:
        n = self.quick_test(
            RMem.make_class((WithSequentialSalt, RMemAbs)),
        )
        self.assertGreaterEqual(n, 5)
