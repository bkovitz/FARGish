# testCCModel.py -- Unit tests for the "codelets in canvases" model

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from io import StringIO

from CCModel import FARGModel, SeqCanvas, ArgsMap, Avails, Plus, Mult, \
    run, Cell, empty_args_map, RunAborted, Complex
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

    def test_paint(self) -> None:
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

    """
    def test_missing_operands_fill_from_avails(self) -> None:
        ca = SeqCanvas.make(
            Avails(4, 5),
            Plus(),
            None,
        )
        with self.assertRaises(NotEnoughOperands):
            run(ca, empty_args_map)
        co = FillMissingOperands(CellRef(ca, 1))
        run(co, empty_args_map)
        self.assertEqual(ca[1], Plus(4, 5))  # or 5, 4?

        fm.run_through(...)  # keep running produced codelets until there
                             # are no more.
    """
