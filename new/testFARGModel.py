# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, Agent, Codelet, Ref, R
from Canvas import StepCanvas, Step
from util import pr, pts, is_iter, first


@dataclass(frozen=True)
class DummyCodelet(Codelet):
    late_bound: R[str]
    
    def go(   # type: ignore[override]
        self, fm, late_bound: str
    ):
        pass

@dataclass(frozen=True)
class DummyAgent(Agent):
    agstr: str  # string in Agent; will be copied to DummyAgent.late_bound
    codelet: R[Codelet] = None


class TestFARGModel(unittest.TestCase):

    def test_basics(self):
        fm = FARGModel()
        # Did it initialize a random-number seed?
        self.assertIsInstance(fm.seed, int)
        # Build something
        ca = fm.build(StepCanvas([Step([4, 5, 6])]))
        # Is it there?
        self.assertTrue(fm.the(StepCanvas))

        self.assertEqual(fm.t, 0)

    def test_codelet_replace_refs(self) -> None:
        fm = FARGModel()
        co0 = DummyCodelet(late_bound=Ref('agstr'))
        ag = DummyAgent(agstr='FROM AGENT')
        co1 = co0.replace_refs(fm, [ag])
        self.assertEqual(co1, DummyCodelet(late_bound='FROM AGENT'))

    def test_agent_replace_refs(self) -> None:
        fm = FARGModel()
        ag0 = DummyAgent(
            agstr='FROM AGENT',
            codelet=DummyCodelet(late_bound=Ref('agstr'))
        )
        ag1 = ag0.replace_refs(fm, [])
        self.assertEqual(
            ag1,
            DummyAgent(
                agstr='FROM AGENT',
                codelet=DummyCodelet(late_bound='FROM AGENT')
            )
        )
