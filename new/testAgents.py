# testAgents.py -- Unit tests for Agents.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable, get_type_hints, get_origin, get_args

from Agents import LitPainter, Consumer, Want
from FARGModel import FARGModel, Detector, CellRef, Born, Wake, Sleeping, \
    Succeeded, SolvedPuzzle
from Codelets import RaiseException, Paint
from Canvas import Step, StepDelta, StepCanvas
from Detectors import AvailDetector
from Equation import plus
from Graph import Graph
from Slipnet import Slipnet

from util import pr, pts, trace, short


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

            fm.do_timestep()  # the Consumer should build a LitPainter
            self.assertEqual(fm.t, 3)
            self.assertTrue(fm.agent_just_ran(co))
            self.assertEqual(fm.agent_state(co), Sleeping)
            lp: Any = fm.the(LitPainter)
            self.assertIsInstance(lp, LitPainter)

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


if __name__ == '__main__':
    from inspect import signature
    ag = Consumer()
