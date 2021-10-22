# testCodelets.py -- Unit tests for Codelets.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, replace

from FARGModel import FARGModel, Agent, Born, Wake, Snag, Succeeded, Codelets, \
    NeedMoreSupportToPaint, QArgs
from Codelets import BuildCompanion, Paint, Consume, BuildLitPainter, \
    QuerySlipnetForDelegate
from Agents import LitPainter
from Canvas import Step, StepCanvas, StepDelta, CellRef
from Equation import plus, minus, times
from Slipnet import Slipnet
from Graph import Graph
from QArgs import QBeforeFromAvails, QAfter, SearchFor
from util import pr


@dataclass(frozen=True)
class DummyAgent(Agent):
    born: Codelets = BuildCompanion()

ddd = DummyAgent()

@dataclass(frozen=True)
class DummyCompanion(Agent):
    pass

class TestCodelets(unittest.TestCase):
    maxDiff = None

    step0 = Step((4, 5, 6))
    step1 = Step((6, 9), StepDelta((4, 5), 9, plus))

    def pons_start_canvas(self) -> StepCanvas:
        return StepCanvas([self.step0])

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

        # Verify that NewState updated dag's state
        self.assertEqual(fm.agent_state(dag), Wake)

    def test_paint_codelet(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr1 = fm.build(CellRef(ca, 1))

        fm.run_codelet(Paint(cr1, self.step1))
        self.assertEqual(ca[1], self.step1)

    def test_paint_codelet_fail(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr1 = fm.build(CellRef(ca, 1))
        ag = fm.build(DummyAgent(), init_a=0.2)

        # ag's activation is below the threshold: fail
        codelet = Paint(cr1, self.step1)
        with self.assertRaises(NeedMoreSupportToPaint) as cm:
            fm.run_codelet(codelet, ag)
        self.assertEqual(cm.exception.agent, ag)
        self.assertIsNone(ca[1])
        self.assertEqual(fm.agent_state(ag), Snag)

        # with sufficient activation, succeed
        fm.set_a(ag, 1.0)
        fm.run_codelet(Paint(cr1, self.step1), agent=ag)
        self.assertEqual(ca[1], self.step1)
        self.assertEqual(fm.agent_state(ag), Succeeded)

    def test_consume(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = fm.build(CellRef(ca, 0))
        cr1 = fm.build(CellRef(ca, 1))
        
        codelet = Consume(
            operator=plus,
            operands=(4, 5),
            source=cr0
        )
        sources = fm.run_codelet(codelet)
        self.assertEqual(fm.look_up_by_name('result', sources), self.step1)

    def test_buildlitpainter(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr1 = fm.build(CellRef(ca, 1))

        codelet = BuildLitPainter(dest=cr1, value=self.step1)
        fm.run_codelet(codelet)
        lp: LitPainter = fm.the(LitPainter) # type: ignore[assignment]
        self.assertIsNotNone(lp)
        fm.run_agent(lp)
        self.assertEqual(ca[1], self.step1)
        self.assertEqual(fm.agent_state(lp), Succeeded)

    def test_query_slipnet_for_delegate(self) -> None:
        graph = Graph.with_features(Consume.make_table(
            range(1, 20), range(1, 20), [plus, minus, times]
        ))
        fm = FARGModel(slipnet=Slipnet(graph))
        ca = fm.build(self.pons_start_canvas())
        cr0 = fm.build(CellRef(ca, 0))

        codelet = QuerySlipnetForDelegate(
            qargs=(QBeforeFromAvails(), QAfter(15), SearchFor(Consume))
        )

        fm.run_codelet_and_follow_ups(codelet, {'source': cr0})
        consume = fm.the(Consume)
        self.assertIsNotNone(consume)
