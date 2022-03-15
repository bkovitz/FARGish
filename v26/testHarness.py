# testHarness.py -- Unit tests for Harness.py, the test harness

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, final

from Harness import TestSpec, EquationMaker, PartialCueMaker
from Experiments import RMemSalt
from Log import lo, trace
from util import pr, pts, short

class TestTestSpec(unittest.TestCase):

    def test_test_spec(self) -> None:
        tspec = TestSpec(
            vv=0,
            cls=RMemSalt,
            kwargs=dict(
                nsalt=2, operands=range(1, 3), operators=['+'], niters=20,
                npartial=4
            ),
            n_per_sample=2,
            nsamples=3,
            initial_canvases_cls=EquationMaker
        )
        result = tspec.run()
        #print(result)
        self.assertIsInstance(result.seed, int)
        self.assertEqual(result.tspec, tspec)
        self.assertEqual(result.num_tests, 6)  # 3 samples * 2 per sample
        self.assertIsInstance(result.duration, float)
        self.assertEqual(result.num_initial_canvases, 4)
        self.assertIsInstance(result.rmem, RMemSalt)
        self.assertEqual(result.rmem.nsalt, 2) # type: ignore[attr-defined]
        self.assertEqual(result.rmem.saltrange, (0, 11)) # type: ignore[attr-defined]
        self.assertEqual(result.kwargs, tspec.kwargs)
        self.assertEqual(
            result.initial_canvases_f, # type: ignore[misc]
            EquationMaker(operands=range(1, 3), operators=['+'])
        )
        self.assertEqual(
            result.cue_maker, # type: ignore[misc]
            PartialCueMaker(npartial=4)
        )
        s = str(result) # smoke test

        # TODO Test determinism by repeating with same seed

