# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from io import StringIO

from FMTypes import Ref, R
from FARGModel import FARGModel, Agent, Born, Defunct, Codelet, \
    first_arg_is_ws, Workspace, as_wspred, Nonexistent
from Log import log_to, lenable, ldisable, ldisable_all, logging_is_enabled, \
    logfile
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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.agstr})'

ag = DummyAgent(
    agstr='FROM AGENT',
    born=DummyCodelet(late_bound=Ref('agstr'))
)

ag2 = DummyAgent(
    agstr='ag2',
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
        self.assertEqual(fm.agent_state(ag), Nonexistent)
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

    def test_first_arg_is_ws(self) -> None:
        self.assertTrue(first_arg_is_ws(Agent.CanRun))
        self.assertFalse(first_arg_is_ws(lambda ws, x: x))
        def f(ws: Workspace, x: int) -> int:
            return x
        self.assertTrue(first_arg_is_ws(f))

        self.assertEqual(as_wspred(Agent.CanRun), Agent.CanRun)

    def test_set_num_iterations(self) -> None:
        fm = FARGModel()
        self.assertEqual(fm.slipnet.propagator.num_iterations, 10)
        fm = FARGModel(num_slipnet_iterations=20)
        self.assertEqual(fm.slipnet.propagator.num_iterations, 20)

    def test_logging(self) -> None:
        def run() -> List[str]:
            fm = FARGModel()
            sio = StringIO()
            log_to(sio)

            #self.assertTrue(logging_is_enabled(Agent))
            #print(type(ag))

            agent1 = fm.build(ag)
            agent2 = fm.build(ag2)
            fm.run_agent(agent1)
            fm.run_agent(agent2)
            sio.seek(0)
            return [line.rstrip() for line in sio.readlines()]
            print(sio.readlines())
            return sio.getvalue()

        # Logging disabled
        self.assertFalse(logging_is_enabled(ag))
        got = run()
        self.assertEqual(got, [])

        # Logging enabled for all agents
        lenable(Agent)
        self.assertTrue(logging_is_enabled(ag))
        got = run()
        self.assertEqual(got, [
            'AGENT DummyAgent(FROM AGENT)     state=born t=0',
            'AGENT DummyAgent(ag2)            state=born t=0'
        ])

        # Logging disabled again
        ldisable(Agent)
        self.assertFalse(logging_is_enabled(ag))
        got = run()
        self.assertEqual(got, [])

        # Logging enabled for just ag2
        lenable(ag2)
        self.assertFalse(logging_is_enabled(ag))
        self.assertTrue(logging_is_enabled(ag2))
        got = run()
        self.assertEqual(got, [
            'AGENT DummyAgent(ag2)            state=born t=0',
        ])

        # Logging disabled again, via ldisable_all()
        ldisable_all()
        self.assertFalse(logging_is_enabled(ag))
        got = run()
        self.assertEqual(got, [])

        # Logging enabled for codelets
        lenable(Codelet)
        got = run()
        self.assertEqual(got, [
            "    CODELET DummyCodelet               late_bound=FROM AGENT",
            "    CODELET DummyCodelet               late_bound=ag2"
        ])

        ldisable_all()
