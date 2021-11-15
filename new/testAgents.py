# testAgents.py -- Unit tests for Agents.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import fields
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable, get_type_hints, get_origin, get_args

from Agents import LitPainter, Consumer, Want, VariantMakerFromAvails
from FMTypes import match_wo_none, Exclude, as_pred
from FARGModel import FARGModel, Detector, CellRef, Born, Wake, Sleeping, \
    Succeeded, SolvedPuzzle, Agent, Fizzle, Snag, Failed, ValuesNotAvail, \
    NoResultFromSlipnet, Agent, Codelet, LogPulse, QueryForSnagFixer, Built
from Codelets import RaiseException, Paint
from Canvas import Step, StepDelta, StepCanvas
from Detectors import AvailDetector
from Equation import plus
from Graph import Graph
from Slipnet import Slipnet
from Log import trace, lo, lenable, ldisable_all
from Propagator import LogAdjustedDeltas
from util import pr, pts, short, first


desnag_graph = Graph.with_features(
    [VariantMakerFromAvails()]
)

class TestAgents(unittest.TestCase):

    step0 = Step((4, 5, 6))
    step1 = Step((6, 9), StepDelta((4, 5), 9, plus))

    def pons_start_canvas(self) -> StepCanvas:
        return StepCanvas([self.step0])

    def test_litpainter(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr1 = CellRef(ca, 1)

        lp = fm.build(LitPainter(dest=cr1, value=self.step1))
        fm.run_agent(lp)
        self.assertEqual(ca[1], self.step1)
        self.assertEqual(fm.agent_state(lp), Succeeded)

    def test_consumer(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)

        ag = fm.build(Consumer(
            operator=plus,
            operands=(4, 5),
            source=cr0,
            #dest=cr1
        ))
        fm.run_agent(ag)
        fm.run_agent(fm.the(LitPainter))
        
        self.assertEqual(ca[1], self.step1)

    def test_want(self) -> None:
        slipnet = Slipnet(Graph.with_features([
            Consumer.make(plus, (4, 5))
        ]))
        fm = FARGModel(slipnet=slipnet)
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        wa = fm.build(Want(
            startcell=cr0,
            target=9,
            on_success=RaiseException(SolvedPuzzle)
        ))
        self.assertEqual(wa.target, 9)

        self.assertEqual(fm.agent_state(wa), Born)
        fm.run_agent(wa)  # first action: build AvailDetector

        det: Any = fm.the(AvailDetector)
        self.assertIsInstance(det, AvailDetector)
        self.assertEqual(det.target, 9)
        self.assertEqual(det.startcell, cr0)
        self.assertEqual(det.on_success, RaiseException(SolvedPuzzle))

        self.assertEqual(fm.agent_state(wa), Wake)
        fm.run_agent(wa)  # second action: build Consumer
        self.assertEqual(fm.agent_state(wa), Sleeping)

        #pr(fm)

        co: Any = fm.the(Consumer)
        self.assertIsInstance(co, Consumer)
        self.assertEqual(fm.builder_of(co), wa)
        self.assertEqual(co.source, cr0)
            # source came from wa.startcell because wa.startcell is declared
            # to be of type CellRef, and Consume.source needs a CellRef
        fm.run_agent(co)
        self.assertEqual(fm.agent_state(co), Sleeping)

        lp: Any = fm.the(LitPainter)
        fm.run_agent(lp)
        self.assertEqual(fm.agent_state(lp), Succeeded)
        #pr(fm)
        self.assertEqual(ca[1], self.step1)

        with self.assertRaises(SolvedPuzzle):
            fm.run_detector(det)

    def test_timestep1(self) -> None:
        # Verifies that Want does something on its first two timesteps,
        # and then sleeps, and then another Agent gets called upon.
        slipnet = Slipnet(Graph.with_features([
            Consumer.make(plus, (4, 5))
        ]))
        for seed in range(1, 2):
            fm = FARGModel(slipnet=slipnet, seed=seed, paint_threshold=0.0)
            ca = fm.build(self.pons_start_canvas())
            cr0 = CellRef(ca, 0)

            wa = fm.build(Want(
                startcell=cr0,
                target=9,
                on_success=RaiseException(SolvedPuzzle)
            ))
            self.assertEqual(fm.agent_state(wa), Born)
            self.assertEqual(fm.t, 0)

            fm.do_timestep()  # the Want should build a Detector
            self.assertEqual(fm.t, 1)
            #pr(fm.codelets_just_run)
            self.assertEqual(fm.agent_state(wa), Wake)

            fm.do_timestep()  # the Want should build a Consumer
            self.assertEqual(fm.t, 2)
            self.assertEqual(fm.agent_state(wa), Sleeping)
            co: Any = fm.the(Consumer)
            self.assertIsInstance(co, Consumer)
            self.assertCountEqual(fm.nodes(Agent.CanRun), [co])

            fm.do_timestep()  # the Consumer should build a LitPainter
            self.assertEqual(fm.t, 3)
            self.assertTrue(fm.agent_just_ran(co))
            self.assertEqual(fm.agent_state(co), Sleeping)
            lp: Any = fm.the(LitPainter)
            self.assertIsInstance(lp, LitPainter)
            self.assertEqual(fm.builder_of(lp), co)

            fm.do_timestep()  # the LitPainter should Paint
            self.assertEqual(fm.t, 4)
            self.assertTrue(fm.agent_just_ran(lp))
            self.assertTrue(any(cr.clis(Paint) for cr in fm.codelets_just_run))
            self.assertEqual(fm.agent_state(lp), Succeeded)

    def test_timestepper(self) -> None:
        slipnet = Slipnet(Graph.with_features([
            Consumer.make(plus, (4, 5))
        ]))
        for seed in range(1, 5):
            fm = FARGModel(slipnet=slipnet, seed=seed, paint_threshold=0.0)
            ca = fm.build(self.pons_start_canvas())
            cr0 = CellRef(ca, 0)
            wa = fm.build(Want(
                startcell=cr0,
                target=9,
                on_success=RaiseException(SolvedPuzzle)
            ))

            with self.assertRaises(
                SolvedPuzzle, msg=f'SolvedPuzzle not raised; seed={seed}'
            ):
                fm.do_timestep(num=6)
                print()
                print('WA', short(wa))
                print()
                pr(fm, extra=True)
            self.assertEqual(ca[1], self.step1, f'seed={seed}')

    def test_snag_tag_and_no_result_from_slipnet(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        
        ag = fm.build(Consumer(
            operator=plus,
            operands=(3, 5),  # 3 is not avail, so this Consumer must fail
            source=cr0,
            #dest=cr1
        ))
        fm.run_agent(ag)
        snag_tag = fm.the(Fizzle)
        self.assertIsNotNone(snag_tag)
        self.assertEqual(fm.builder_of(snag_tag), ag)
        self.assertEqual(fm.agent_state(ag), Snag)

    def test_fail_after_no_result_from_slipnet(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        
        ag = fm.build(Consumer(
            operator=plus,
            operands=(3, 5),  # 3 is not avail, so this Consumer must fail
            source=cr0,
        ))
        fm.add_tag(ag, ValuesNotAvail())
        self.assertTrue(fm.has_tag(ag, ValuesNotAvail))
        fm.set_state(ag, Snag)

        fm.run_agent(ag)
        self.assertTrue(fm.has_tag(ag, NoResultFromSlipnet))
        self.assertEqual(fm.agent_state(ag), Failed)

    def test_variant_maker(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = fm.build(CellRef(ca, 0))
        ag1 = fm.build(Consumer(
            operator=plus,
            operands=(4, 4),
            source=cr0
        ))
        ag2 = fm.build(VariantMakerFromAvails(
            agent=ag1,
            cellref=cr0,
            avails=(4, None),
            unavails=(None, 4)
        ))

        # The VariantMakerFromAvails should make a new Consumer, one whose
        # operands include 4 and another real avail, i.e. 5 or 6.
        #lenable(Agent, Codelet, Fizzle)
        #pr(fm)
        fm.run_agent(ag2)
        new_consumer = first(fm.nodes((Consumer, Exclude(ag1))))
        #ldisable_all()
        self.assertTrue(
            match_wo_none(new_consumer, Consumer.make(plus, (4, 5)))
            or
            match_wo_none(new_consumer, Consumer.make(plus, (4, 6)))
        )

    def test_snag_leads_to_variant_maker(self) -> None:
        #lenable(Codelet, Fizzle, LogPulse, Built)
        fm = FARGModel(slipnet=Slipnet(desnag_graph))
        ca = fm.build(self.pons_start_canvas())
        cr0 = fm.build(CellRef(ca, 0))
        ag1 = fm.build(Consumer(
            operator=plus,
            operands=(4, 4),
            source=cr0
        ))
        fm.run_agent(ag1, num=2)
        """
        pr(fm)
        vm: Any = fm.the(VariantMakerFromAvails)
        print()
        print(vm)
        print()

        print(type(vm.cellref))
        print()
        pts(vm.need_args())
        #print('\nGOT')
        #print(vm)
        #print('\nVMFA:')
        #print(VariantMakerFromAvails())
        assert fm.has_tag(ag1, ValuesNotAvail)
        assert fm.was_just_run(QueryForSnagFixer)

        print()
        pr(fm)
        print()
        print('SLIPNET:')
        bg = fm.slipnet.base_graph
        pr(bg.nodes)
        print()
        pr(bg.edges)
        print()
        al = first(fm.alogs.logs.values())
        al.plot()
        print()
        """
        vm = fm.the(VariantMakerFromAvails)
        #print(vm)

        # TODO
        #print(match_wo_none(vm, VariantMakerFromAvails()))
        self.assertTrue(fm.the(VariantMakerFromAvails))
        self.assertEqual(
            fm.the(VariantMakerFromAvails),
            VariantMakerFromAvails(
                agent=ag1,
                cellref=cr0,
                avails=(4, None),
                unavails=(None, 4)
            )
        )
        #self.assertTrue(fm.has_node(VariantMakerFromAvails()))
        # TODO Check the VariantMakerFromAvails' arguments.
        #input('key...')

    """
    def test_source_nodes_seq(self) -> None:
        fm = FARGModel(slipnet=Slipnet(desnag_graph))
        ca = fm.build(self.pons_start_canvas())
        cr0 = fm.build(CellRef(ca, 0))
        ag1 = fm.build(Consumer(
            operator=plus,
            operands=(4, 4),
            source=cr0
        ))
        fm.run_agent(ag1, num=2)
        pr(fm)
        print()
        pr(fm.activation_g.nodes)
        print()
        pr(fm.activation_g.edges)
        print()
        pr(list(fm.source_nodes_seq(ag1)))
    """


if __name__ == '__main__':
    from inspect import signature
    ag = Consumer()
