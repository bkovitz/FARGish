# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, Agent, Born, Codelet, Ref, R
from Canvas import StepCanvas, Step
from util import pr, pts, is_iter, first


@dataclass(frozen=True)
class DummyCodelet(Codelet):
    depository: ClassVar[str] = ''  # .run() will write a string here
    late_bound: R[str] = None
    
    def run(   # type: ignore[override]
        self, fm: FARGModel, late_bound: str
    ):
        DummyCodelet.depository = late_bound

@dataclass(frozen=True)
class DummyAgent(Agent):
    agstr: str = 'default'
        # string in Agent; will be copied to DummyAgent.late_bound

ag = DummyAgent(
    agstr='FROM AGENT',
    born=DummyCodelet(late_bound=Ref('agstr'))
)

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
        ag = DummyAgent(agstr='FROM AGENT', born=DummyCodelet())
        co1 = co0.replace_refs(fm, [ag])
        self.assertEqual(co1, DummyCodelet(late_bound='FROM AGENT'))

    def test_agent_replace_refs(self) -> None:
        fm = FARGModel()
        ag1 = ag.replace_refs(fm, [])
        self.assertEqual(
            ag1,
            DummyAgent(
                agstr='FROM AGENT',
                born=DummyCodelet(late_bound='FROM AGENT')
            )
        )

    def test_codelet_args(self) -> None:
        fm = FARGModel()
        codelet: Codelet = fm.replace_refs(ag.born, ag) # type: ignore[arg-type]
        self.assertEqual(
            fm.codelet_args(codelet, ag),
            dict(fm=fm, late_bound='FROM AGENT')
                # This dict holds the arguments to ag.born's .run() method
        )

    def test_run_agent(self) -> None:
        fm = FARGModel()
        DummyCodelet.depository = ''
        fm.run_agent(ag, Born)
        self.assertEqual(DummyCodelet.depository, 'FROM AGENT')
