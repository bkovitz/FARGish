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

from FMTypes import match_wo_none
from CCModel import StdModel, ArgsMap, Avails, Plus, Mult, \
    Cell, empty_args_map, Complex, Paint, NotEnoughOperands, \
    FillFromAvails, FinishStructure, Detector
from Canvas import RunAborted, ArithmeticToHere, SeqCanvas
from run import run
from Tag import Tagger, PTag, HasAvail, HasTag
from Tags import SuccessfulCanvas
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
        fm = StdModel()
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
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            None,  # No codelet
            None,
        ))
        p = Paint(noderef=ca.cellref(2), content=Avails(9))
        fm.run(p, empty_args_map)
        self.assertEqual(ca[2], Avails(9))
        
    def test_missing_operands_fill_from_avails(self) -> None:
        fm = StdModel()
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
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        co1 = FinishStructure(ca.cellref(1))
        co2 = fm.run(co1, empty_args_map)
        self.assertEqual(co2, FillFromAvails(noderef=ca.cellref(1)))

    def test_detector(self) -> None:
        fm = StdModel()
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
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(4, 5),
            None,
        ))
        cr0 = ca.cellref(0)
        cr1 = ca.cellref(1)
        cr2 = ca.cellref(2)
        ha5 = HasAvail(target=5)
        ha9 = HasAvail(target=9)
        self.assertTrue(ha5.run(noderef=cr0))
        self.assertFalse(ha9.run(noderef=cr0))
        self.assertFalse(ha5.run(noderef=cr1))
        self.assertFalse(ha9.run(noderef=cr1))
        self.assertFalse(ha5.run(noderef=cr2))
        self.assertFalse(ha9.run(noderef=cr2))

        sc = fm.build(Tagger(HasAvail(target=9)))
        sc.run_on(fm, cr2)
        self.assertFalse(fm.has_tag(cr2, HasAvail), cr2.get())

        fm.run(ca)
        self.assertFalse(ha5.run(noderef=cr2))
        self.assertTrue(ha9.run(noderef=cr2))

        sc.run_on(fm, cr2)
        self.assertTrue(fm.has_tag(cr2, HasAvail), cr2.get())

    def test_arithmetic_to_here(self) -> None:
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5, 6),
            Plus(4, 5),
            None,
            None,
            None
        ))
        self.assertFalse(ca.cell_at(2).has_tag(ArithmeticToHere))
        self.assertFalse(ca.cell_at(4).has_tag(ArithmeticToHere))
        try:
            fm.run(ca)
        except RunAborted:
            pass
        self.assertTrue(ca.cell_at(2).has_tag(ArithmeticToHere))
        self.assertFalse(ca.cell_at(4).has_tag(ArithmeticToHere))

        ca.paint(3, Plus(9, 6))
        self.assertTrue(ca.cell_at(2).has_tag(ArithmeticToHere))
        self.assertFalse(ca.cell_at(4).has_tag(ArithmeticToHere))

        fm.run(ca)
        self.assertTrue(ca.cell_at(2).has_tag(ArithmeticToHere))
        self.assertTrue(ca.cell_at(4).has_tag(ArithmeticToHere))

        # Now we check that painting on the canvas removes ArithmeticToHere()
        # from the painted cell to the end of the canvas.
        ca.paint(3, None)
        self.assertTrue(ca.cell_at(2).has_tag(ArithmeticToHere))
        self.assertFalse(ca.cell_at(4).has_tag(ArithmeticToHere))

    def test_hasavail9_to_tagpred(self) -> None:
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(4, 5),
            Avails(9),  # This test will fail if this cell contains None
        ))
        cr0 = ca.cellref(0)
        cr1 = ca.cellref(1)
        cr2 = ca.cellref(2)
        ha5 = HasAvail(target=5)

        got = HasTag.make_from(HasAvail(5))
        self.assertEqual(got, HasTag(PTag(HasAvail(5))))

        # cr0 has the condition but not the tag
        self.assertTrue(ha5.run(noderef=cr0))
        self.assertFalse(fm.has_tag(cr2, PTag(HasAvail(5))))
        self.assertFalse(got(cr0))

        # Falsely adding the tag to cr2
        fm.add_tag(cr2, PTag(HasAvail(5)))
        self.assertFalse(ha5.run(noderef=cr2))
        self.assertTrue(fm.has_tag(cr2, PTag(HasAvail(5))))
        self.assertTrue(got(cr2))

    def test_subtaggers(self) -> None:
        ta1 = Tagger(SuccessfulCanvas(9))

    def test_detect_successfully_completed_canvas(self) -> None:
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        sc = fm.build(Tagger(
            SuccessfulCanvas(9)
            #TagConjunction((HasAvail(9), ArithmeticToHere)),
            #scope=ca
        ))
        # NEXT Build a tagger for each conjunct?
#        success_tag = SuccessfulCanvas(9)
#        de = fm.build(Detector(
#            watch=ca,
#            watch_for=SuccessfulCanvas(9),
#            then=ApplyTag
#        ))
        

    @unittest.skip('not implemented yet')
    def test_respond_to_missing_operands(self) -> None:
        fm = StdModel()
        ca = fm.build(SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        ))
        fm.run(ca, empty_args_map)
