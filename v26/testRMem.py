# testRMem.py -- Unit tests for RMem.py

import unittest
from pprint import pprint as pp
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, final

from RMem import RMem, CanvasPrep, make_eqns, BaseValue, ndups, no_prep, \
    Canvas1D, SkewedPainterWeight, Match, Right, Left, Painter, \
    WithAdjacentRelativePainters
from Log import lo, trace
from util import as_tuple, pr, pts, ps, pss, psa, sample_without_replacement, \
    reseed

class TestRMem(unittest.TestCase):

    def test_make_painters_partial_canvas(self) -> None:
        rmem = RMem()
        self.assertCountEqual(
            rmem.canvas_to_painters((1, '+', None, None, None)),
            [(1, 2, '+'), (2, 1, 1)]
        )

    def test_canvas1d(self) -> None:
        c = Canvas1D([None, '+', 2, None, 5])
        self.assertEqual(c.all_clarities(), [0, 5, 5, 0, 5])

        c[1] = 3  # 1-based indexing
        self.assertEqual(as_tuple(c), (3, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [1, 5, 5, 0, 5])
            # clarity is up

        c[1] = -8  # should return cell 1 to None, clarity to 0
        self.assertEqual(as_tuple(c), (None, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [0, 5, 5, 0, 5])

        c[3] = 8  # should only decrease clarity, not affect cell
        self.assertEqual(as_tuple(c), (None, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [0, 5, 4, 0, 5])

        c = Canvas1D([None, '+', 2, None, 5], INITIAL_CLARITY=4)
        self.assertEqual(c.all_clarities(), [0, 4, 4, 0, 4])

        c = Canvas1D.make_from((1, None, None, None, None))
        c[0] = 88  # Should do nothing, since addr is off canvas
        self.assertEqual(c.as_tuple(), (1, None, None, None, None))

    def test_painter_weight(self) -> None:
        c = Canvas1D([None, '+', 2, None, 5])
        RMemSkewed: Type[RMem] = \
            type('RMemSkewed', (SkewedPainterWeight, RMem), {})

        # source has clarity 0, so painter weight is 0
        self.assertEqual(c.clarity(1), 0)
        self.assertEqual(RMem.painter_weight(1, 2, '+', c), 0)
        self.assertEqual(RMemSkewed.painter_weight(1, 2, '+', c), 0)

        # source has high clarity, target low clarity, so high painter weight
        self.assertAlmostEqual(
            RMem.painter_weight(2, 1, '+', c), 0.167, places=3
        )
        self.assertAlmostEqual(
            RMemSkewed.painter_weight(2, 1, '+', c), 1800, places=3
        )

        # source has low clarity, target high clarity
        c.set_clarity(2, 1)
        self.assertAlmostEqual(
            RMem.painter_weight(2, 3, '+', c), 0.00556, places=5
        )
        self.assertAlmostEqual(
            RMemSkewed.painter_weight(2, 3, '+', c), 5.0, places=3
        )

    def test_painter_right(self) -> None:
        rmem = RMem()
        p: Painter = (Match(1), Right(1), '+')
        c = Canvas1D.make_from((1, None, None, None, None))
        rmem.run_generator(c, p)
        self.assertEqual(c.as_tuple(), (1, '+', None, None, None))

    def test_painter_left(self) -> None:
        rmem = RMem()
        p: Painter = (Match(1), Left(1), '+')
        c = Canvas1D.make_from((1, None, None, None, None))
        rmem.run_generator(c, p)
        self.assertEqual(c.as_tuple(), (1, None, None, None, None))

    def test_make_adjacent_painter(self) -> None:
        rmadjtype = type(
            'RMemAdjacent', (WithAdjacentRelativePainters, RMem), {}
        )
        rmem = rmadjtype()

        p = rmem.painter_from_to(1, 2, 1, '+')
        self.assertEqual(p, (Match(1), Right(1), '+'))

    def test_make_adjacent_painters(self) -> None:
        rmadjtype = type(
            'RMemAdjacent', (WithAdjacentRelativePainters, RMem), {}
        )
        rmem = rmadjtype()

        eqn = (1, '+', 1, '=', 2)
        painters = rmem.canvas_to_painters(eqn)
        self.assertCountEqual(
            painters,
            set([
                (Match('+'), Left(1), 1),
                (Match('+'), Right(1), 1),
                (Match(1), Left(1), '+'),
                (Match(1), Right(1), '+'),
                (Match(1), Right(1), '='),
                (Match(2), Left(1), '='),
                (Match('='), Left(1), 1),
                (Match('='), Right(1), 2),
            ])
        )

        pset = rmem.canvas_to_pset(eqn)
        #pr(pset) #DEBUG
        self.assertEqual(
            pset,
            {
                (Match('+'), Left(1)): 1,
                (Match('+'), Right(1)): 1,
                (Match(1), Left(1)): '+',
                (Match(1), Right(1)): rmem.rndfunc(rmem, ('=', '+'), (1, 1)),
                (Match(2), Left(1)): '=',
                (Match('='), Left(1)): 1,
                (Match('='), Right(1)): 2,
                
            }
        )
