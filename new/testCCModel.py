# testCCModel.py -- Unit tests for the "codelets in canvases" model

from __future__ import annotations

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from io import StringIO

from CCModel import FARGModel, SeqCanvas, ArgsMap, Avails, Plus, Mult, \
    run, Cell, empty_args_map, RunAborted, Complex, Paint, NotEnoughOperands, \
    FillFromAvails, FinishStructure, Detector, HasTag, HasAvail
from FMTypes import match_wo_none
from Log import lo
from util import ps, pr


class TestCCModel(unittest.TestCase):

    def test_4_plus_5(self) -> None:
        ca = SeqCanvas.make(
            Avails(4, 5),
            Plus(4, 5),
            None,
        )
        run(ca, empty_args_map)
        self.assertEqual(ca[2], Avails(9))

        sio = StringIO()
        ps(ca, file=sio)
        self.assertEqual(sio.getvalue(), '[ 4 5 ][ 4 + 5 ][ 9 ]\n')

    def test_stop_at_empty_cell(self) -> None:
        ca = SeqCanvas.make(
            Avails(4, 5),
            None,  # No codelet
            None,
        )
        with self.assertRaises(RunAborted) as cm:
            run(ca, empty_args_map)
        self.assertEqual(cm.exception.canvas, ca)
        #self.assertEqual(cm.exception.step.addr, 1)
        # TODO Verify that the RunAborted shows that Cell 1 failed.

    def test_complex(self) -> None:
        c1 = Complex(Plus(4, 5), operands=(4, 6))
        ca = SeqCanvas.make(
            Avails(4, 5, 6),
            c1,
            None,
        )
        run(ca, empty_args_map)
        self.assertEqual(ca[2], Avails(5, 10))

        c2 = Complex(c1, operands=(5, 6))
        ca.paint(1, c2)
        run(ca, empty_args_map)
        self.assertEqual(ca[2], Avails(4, 11))
        #pr(ca)  # TODO Make short(Complex) return the short of the most
        # reduced nugget.

    def test_painting_over(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            None,  # No codelet
            None,
        ))
        fm.paint(ca.cellref(1), Plus(4, 5))
        self.assertEqual(ca[1], Plus(4, 5))

        fm.paint(ca.cellref(1), Mult(2, 3))
        self.assertEqual(ca[1], Mult(2, 3))
        self.assertTrue(fm.has_node(Plus(4, 5)))

        fm.paint(ca.cellref(1), dict(operands=(4, 5)))
        self.assertEqual(ca[1], Complex(Mult(2, 3), operands=(4, 5)))

        run(ca, empty_args_map)
        self.assertEqual(ca[2], Avails(20))

    def test_paint(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            None,  # No codelet
            None,
        ))
        p = Paint(noderef=ca.cellref(2), content=Avails(9))
        fm.run(p, empty_args_map)
        self.assertEqual(ca[2], Avails(9))
        
    def test_missing_operands_fill_from_avails(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        with self.assertRaises(NotEnoughOperands) as cm:
            run(ca, empty_args_map)
        self.assertEqual(cm.exception.codelet, Plus())
        self.assertTrue(fm.has_tag(ca.cellref(1), NotEnoughOperands))

        co = FillFromAvails(noderef=ca.cellref(1))
        paint: Any = fm.run(co, empty_args_map)
        self.assertTrue(match_wo_none(
            paint,
            Paint(noderef=ca.cellref(1))
        ))
        self.assertCountEqual(paint.content['operands'], (4, 5))
        self.assertTrue(fm.has_tag(ca.cellref(1), NotEnoughOperands))

        fm.run(paint, empty_args_map)
        fm.run(ca, empty_args_map)
        self.assertEqual(ca[2], Avails(9))
        # TODO Should the NotEnoughOperands tag have come off now?

    """
        co = FillMissingOperands(CellRef(ca, 1))
        run(co, empty_args_map)
        self.assertEqual(ca[1], Plus(4, 5))  # or 5, 4?

        fm.run_through(...)  # keep running produced codelets until there
                             # are no more.
    """

    def test_finish_structure(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        co1 = FinishStructure(ca.cellref(1))
        co2 = fm.run(co1, empty_args_map)
        self.assertEqual(co2, FillFromAvails(noderef=ca.cellref(1)))

    def test_detector(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        de = fm.build(Detector(
            watch=ca,
            watch_for=HasTag(NotEnoughOperands),
            then=FinishStructure
        ))
        fm.do_timestep(run=ca)
        assert fm.has_tag(ca.cellref(1), NotEnoughOperands)
        fm.run(de)
        self.assertTrue(fm.has_node(FinishStructure(ca.cellref(1))))

    def test_avail_tagger(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        cr0 = ca.cellref(0)
        ha5 = HasAvail(target=5)
        ha9 = HasAvail(target=9)
        self.assertTrue(ha5.run(noderef=cr0))
        self.assertFalse(ha9.run(noderef=cr0))
        # run the ha predicate on ca[0], ca[1], ca[2]

        #sc = fm.build(Tagger(HasAvail, target=9))
        # run the tagger on ca[0]
        # run the tagger on ca[1]
        # run the tagger on ca[2]
        # run the canvas

        # TODO 

    """
    def test_detect_successfully_completed_canvas(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        success_tag = SuccessfulCanvas(9)
        de = fm.build(Detector(
            watch=ca,
            watch_for=SuccessfulCanvas(9),
            then=ApplyTag
        ))
    """
        

    @unittest.skip('not implemented yet')
    def test_respond_to_missing_operands(self) -> None:
        fm = FARGModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        fm.run(ca, empty_args_map)
