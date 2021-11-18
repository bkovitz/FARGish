# testCanvas.py -- Unit tests for Canvas.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable

from FARGModel import FARGModel, CellRef, Codelet
from Canvas import Step, StepDelta, ValuesNotAvail, StepCanvas
from Consume import Consume
from Equation import plus
from Agents import Consumer, VariantMakerFromAvails, LitPainter
from Graph import Graph
from Slipnet import Slipnet
from Log import trace, lo, lenable, ldisable_all
from util import as_iter, as_list, pr, pts


class TestStepsCanvas(unittest.TestCase):

    def test_step_take_avails(self) -> None:
        step = Step([4, 5, 6])
        taken, remaining = step.take_avails([4, 5])
        self.assertCountEqual(taken, [4, 5])
        self.assertCountEqual(remaining, [6])
        self.assertEqual(step, Step([4, 5, 6]))

        self.assertTrue(step.has_avail_value(4))
        self.assertFalse(step.has_avail_value(7))

    def test_step_take_avails_fail(self) -> None:
        step = Step([4, 5, 6])
        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = step.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(avails=(4, None), unavails=(None, 7))
        )

    def test_stepcanvas_basics(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        self.assertEqual(ca[0], Step([4, 5, 6]))
        self.assertEqual(ca[1], None)
        self.assertEqual(ca[10], None)
        self.assertEqual(len(ca), 1)

        ca[1] = Step([6], StepDelta([4, 5], [9], '+'))
        self.assertEqual(ca[1], Step([6], StepDelta([4, 5], [9], '+')))
        self.assertEqual(len(ca), 2)

    def test_cellref_basics(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)

        # StepCanvas[0]
        self.assertEqual(cr0.value, Step([4, 5, 6]))
        self.assertCountEqual(as_iter(cr0.avails), [4, 5, 6])
        taken, remaining = cr0.take_avails([4, 5])
        self.assertCountEqual(taken, [4, 5])
        self.assertCountEqual(remaining, [6])
        self.assertEqual(cr0.value, Step([4, 5, 6]))
        self.assertTrue(cr0.has_a_value())

        self.assertTrue(cr0.has_avail_value(4))
        self.assertFalse(cr0.has_avail_value(7))

        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = cr0.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(cellref=cr0, avails=(4, None), unavails=(None, 7))
        )

        #StepCanvas[1]
        self.assertEqual(cr1.value, None)
        self.assertFalse(cr1.has_a_value())
        self.assertCountEqual(as_iter(cr1.avails), [])
        with self.assertRaises(ValuesNotAvail) as cm:
            taken, remaining = cr1.take_avails([4, 7])
        self.assertEqual(
            cm.exception,
            ValuesNotAvail(cellref=cr1, avails=(4, 7), unavails=())
        )

        # paint
        cr1.paint(Step([6], StepDelta([4, 5], [9], '+')))
        self.assertEqual(ca[1], Step([6], StepDelta([4, 5], [9], '+')))

    def test_next_cellref(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        
        self.assertEqual(ca.next_addr(0), 1)
        self.assertEqual(ca.next_cellref(cr0), cr1)
        self.assertEqual(cr0.next_cellref(), cr1)

    def test_last_painted_cellref(self) -> None:
        ca = StepCanvas([Step([4, 5, 6])])
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        cr2 = CellRef(ca, 2)

        self.assertEqual(ca.last_painted_addr(), 0)
        self.assertEqual(ca.last_painted_cellref(), cr0)
        self.assertEqual(list(ca.cellrefs()), [cr0])
        self.assertEqual(cr0.last_painted_cellref(), cr0)
        self.assertEqual(cr1.last_painted_cellref(), cr0)
        self.assertEqual(cr2.last_painted_cellref(), cr0)

        cr1.paint(Step([6, 9], StepDelta([4, 5], [9], '+')))

        self.assertEqual(ca.last_painted_addr(), 1)
        self.assertEqual(ca.last_painted_cellref(), cr1)
        self.assertEqual(list(ca.cellrefs()), [cr0, cr1])
        self.assertEqual(cr0.last_painted_cellref(), cr1)
        self.assertEqual(cr1.last_painted_cellref(), cr1)
        self.assertEqual(cr2.last_painted_cellref(), cr1)

    def test_vma(self) -> None:
        # Regression test for a bug: When a canvas cell was painted over
        # after a VariantMakerFromAvails was created using it as a source,
        # the VariantMakerFromAvails would produce an invalid Consumer
        # (missing an operand).
        fm = FARGModel(slipnet=Slipnet(
            Graph.with_features([VariantMakerFromAvails()])
        ))
        ca = fm.build(StepCanvas([Step([4, 5, 6])]))
        cr0 = CellRef(ca, 0)
        cr1 = CellRef(ca, 1)
        cr1.paint(Step([5, 2], StepDelta([6, 4], [2], '-')))

        # Consumer(12+5) will snag: 5 is avail but 12 is unavail
        co1 = fm.build(Consumer(
            operator=plus,
            operands=(12, 5),
            source=cr1
        ))
        fm.run_agent(co1, num=2)
        vma: Any = fm.the(VariantMakerFromAvails)
        assert vma, 'Consumer did not produce a VariantMakerFromAvails'

        # Some other agent paints over cr1, removing the avail 5
        cr1.paint(Step([4, 11], StepDelta([6, 5], [11], '+')))

        fm.run_agent(vma)
        co2: Any = fm.built_by(vma)[0]
        assert isinstance(co2, Consumer)

        self.assertEqual(len(as_list(co2.operands)), 2)
        fm.run_agent(co2)  # Check for crash

        lp: Any = fm.built_by(co2)[0]
        self.assertIsInstance(lp, LitPainter)
