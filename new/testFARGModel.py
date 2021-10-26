# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, Agent, Born, Defunct, Codelet, Ref, R
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

    def test_fargmodel_basics(self) -> None:
        fm = FARGModel()
        self.assertEqual(fm.t, 0)
        # Did it initialize a random-number seed?
        self.assertIsInstance(fm.seed, int)

        # Build something
        ca = fm.build(StepCanvas([Step([4, 5, 6])]))
        # Is it there?
        self.assertTrue(fm.the(StepCanvas))

        # Build an Agent
        self.assertEqual(fm.agent_state(ag), Defunct) # not built yet
        agent = fm.build(ag)
        self.assertEqual(fm.agent_state(agent), Born)

        # Build and link to builder
        one = fm.build(1, builder=agent)
        self.assertEqual(fm.ae_weight(agent, one), 1.0)
        self.assertEqual(fm.builder_of(one), agent)
        self.assertCountEqual(fm.behalf_of(one), [agent])
        self.assertCountEqual(fm.built_by(agent), [one])

    def test_codelet_replace_refs(self) -> None:
        fm = FARGModel()
        co0 = DummyCodelet(late_bound=Ref('agstr'))
        ag = DummyAgent(agstr='FROM AGENT', born=DummyCodelet())
        co1 = co0.replace_refs(fm, ag)
        self.assertEqual(co1, DummyCodelet(late_bound='FROM AGENT'))

    def test_agent_replace_refs(self) -> None:
        fm = FARGModel()
        ag1 = ag.replace_refs(fm, None)
        self.assertEqual(
            ag1,
            DummyAgent(
                agstr='FROM AGENT',
                born=DummyCodelet(late_bound=Ref('agstr'))
            )
        )

    def test_codelet_args(self) -> None:
        fm = FARGModel()
        #codelet: Codelet = fm.replace_refs(ag.born, ag) # type: ignore[arg-type]
        codelet: Codelet = ag.born  # type: ignore[assignment]
        self.assertEqual(
            fm.mk_func_args(codelet.run, ag),
            dict(fm=fm, late_bound='FROM AGENT')
                # This dict holds the arguments to ag.born's .run() method
        )

    def test_run_agent(self) -> None:
        fm = FARGModel()
        DummyCodelet.depository = ''
        fm.run_agent(ag, Born)  # ag does not exist yet
        self.assertEqual(DummyCodelet.depository, '')
        agent = fm.build(ag)
        fm.run_agent(agent, Born)
        self.assertEqual(DummyCodelet.depository, 'FROM AGENT')
