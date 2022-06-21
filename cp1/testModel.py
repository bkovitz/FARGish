# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING

from Model import Model, Canvas1D, same, succ, pred, DeterminateAddress, \
    FizzleValueNotFound, OffsetAddr, CanvasAddress, PainterAddr, const, \
    AbsPainterTemplate, Env
from util import short


class TestModel(unittest.TestCase):

    def test_canvas1d_basics(self) -> None:
        c = Canvas1D.make_from('a  ')
        self.assertEqual(c[1], 'a')
        self.assertEqual(c[2], None)
        self.assertEqual(str(c), '[a None None]')
        self.assertEqual(short(c), 'a  ')

        c[2] = succ(c[1])
        self.assertEqual(short(c), 'ab ')

        self.assertEqual(c.addr_of('b'), CanvasAddress(c, 2))

    def test_simple_run_painter(self) -> None:
        p = (1, 2, succ)
        m = Model.make_from('a  ')
        m.run_painter(p)
        self.assertEqual(short(m), 'ab ')

    def test_simple_relative_painter(self) -> None:
        p = ('a', OffsetAddr('I', +1), succ)
        m = Model.make_from(' a ')
        m.run_painter(p)
        self.assertEqual(short(m), ' ab')

    def test_fizzle_value_not_found(self) -> None:
        c = Canvas1D.make_from('a  ')
        with self.assertRaises(FizzleValueNotFound) as cm:
            i = c.addr_of('b')
        self.assertEqual(cm.exception, FizzleValueNotFound('b'))

    def test_simple_make_spont(self) -> None:
        m = Model.make_from('ajaqb')
        self.assertCountEqual(
            m.make_spont(),
            [
                (1, 3, same),
                (3, 1, same),
                (1, 5, succ),
                (5, 1, pred),
                (3, 5, succ),
                (5, 3, pred)
            ]
        )

    def test_paint_a_painter_to_ws(self) -> None:
        m = Model.make_from('     ')
        m.paint(PainterAddr(2, 3), succ)
        self.assertCountEqual(
            m.painters_with_target(3),
            [(2, 3, succ)]
        )
        # self.assertEqual(m.clarity((2, 3, succ)), 1)

    def test_const(self) -> None:
        m = Model.make_from('a    ')
        m.run_painter((1, 3, const('g')))
        self.assertEqual(short(m), 'a g  ')

    def test_match_abs_painter(self) -> None:
        m = Model.make_from('a    ')
        env = m.fresh_env()

        ma = AbsPainterTemplate(OffsetAddr('X'), OffsetAddr('X', 2), same)
        got = ma.is_match(env, (1, 3, same))
        if isinstance(got, Env): 
            self.assertEqual(
                got.determinate_address('X'),
                CanvasAddress(m.canvas, 1)
            )
        else:
            self.fail('.is_match() failed to match')

        got = ma.is_match(env, (1, 2, same))
        self.assertIsNone(got)
