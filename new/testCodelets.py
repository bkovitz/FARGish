# testCodelets.py -- Unit tests for Codelets.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, replace

from FARGModel import FARGModel, Agent, Born, Wake, Codelets
from Codelets import BuildCompanion
from util import pr


@dataclass(frozen=True)
class DummyAgent(Agent):
    born: Codelets = BuildCompanion()

ddd = DummyAgent()

@dataclass(frozen=True)
class DummyCompanion(Agent):
    pass

class TestCodelets(unittest.TestCase):

    def test_build_companion(self) -> None:
        fm = FARGModel()

        dag = fm.build(DummyAgent())
        self.assertEqual(fm.agent_state(dag), Born)

        bc = BuildCompanion(companion=DummyCompanion())
        fm.run_codelet(bc, agent=dag)

        self.assertTrue(fm.has_node(DummyCompanion()))
        companion = fm.the(DummyCompanion)
        self.assertEqual(fm.ae_weight(dag, companion), 1.0)
        self.assertEqual(fm.builder_of(companion), dag)
        self.assertEqual(fm.agent_state(dag), Wake)
