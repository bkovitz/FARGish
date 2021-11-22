# testDetectors.py -- Unit tests for Detectors.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable, get_type_hints, get_origin, get_args

from FARGModel import FARGModel, Detector, FARGException, CellRef, Codelet
from Detectors import AvailDetector, DeadEndDetector
from Agents import Want
from Codelets import RaiseException
from Consume import Consume
from Canvas import Step, StepDelta, StepCanvas
from Equation import plus, minus, times
from Log import lenable, ldisable_all
from Tags import DeadEnd
from util import pr, pts


@dataclass(frozen=True)
class UTDeadEndFound(FARGException):
    dead_end: CellRef

class TestDetectors(unittest.TestCase):

    step0     = Step((4, 5, 6))
    step1     = Step((6, 9), StepDelta((4, 5), 9, plus))
    step2bad  = Step((3,), StepDelta((9, 6), 3, minus))
    step2good = Step((15,), StepDelta((9, 6), 3, plus))

    def pons_start_canvas(self) -> StepCanvas:
        return StepCanvas([self.step0])

    def test_dead_end_detector(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        cr2 = CellRef(ca, 2)
        det = fm.build(DeadEndDetector(
            target=15,
            startcell=cr0,
            on_success=RaiseException(UTDeadEndFound)
        ))
        fm.run_detector(det)  # no exception
        cr1.paint(self.step1)
        fm.run_detector(det)  # no exception
        cr2.paint(self.step2good)
        fm.run_detector(det)  # no exception
        cr2.paint(self.step2bad)
        #lenable(Codelet)
        with self.assertRaises(UTDeadEndFound) as cm:
            fm.run_detector(det)
        self.assertEqual(cm.exception, UTDeadEndFound(cr2))

    def test_dead_end_detector_links_to_want(self) -> None:
        fm = FARGModel()
        ca = fm.build(self.pons_start_canvas())
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        cr2 = CellRef(ca, 2)
        wa = fm.build(Want(startcell=cr0, target=15))
        fm.run_agent(wa)
        fm.paint(cr1, self.step1)
        fm.paint(cr2, self.step2bad)
        fm.run_detector(fm.the(DeadEndDetector))
        dead_end: Any = fm.the(DeadEnd)
        self.assertIsNotNone(dead_end)
        self.assertEqual(dead_end.for_goal, wa)
