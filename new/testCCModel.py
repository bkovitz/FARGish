# testCCModel.py -- Unit tests for the "codelets in canvases" model

import unittest
from pprint import pprint as pp
import inspect

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from io import StringIO

from CCModel import SeqCanvas, ArgsMap, Avails, Plus, run, Cell, \
    empty_args_map, RunAborted
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
