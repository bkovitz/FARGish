# testMixins.py -- Unit tests for Mixins.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, final

from RMem import RMem, RMemAbs, RMemFuncs, Absorb, Regenerate, make_eqns, \
    WithNDups, WithAdjacentRelativePainters, Canvas, LinearClarityWeight, \
    Match, Right
from Mixins import WithCountColumns, WithRandomSalt, WithSequentialSalt, \
    WithRelsPaintAbsolutes
from Log import lo, trace
from util import as_tuple, pts


class TestMixins(unittest.TestCase):

    eqns = make_eqns(operands=[1, 2, 3], operators=['+'])
    cues = [
        (1, None, None, None, 2),
        (1, None, None, None, 2),
        (1, None, None, None, 2),
        (3, None, 1, None, None),
        (3, None, 1, None, None),
        (3, None, 1, None, None)
    ]

    def testCountColumns(self) -> None:
        rmem = RMem.make_instance((WithCountColumns, RMemAbs))
        rmem.absorb_canvases(self.eqns)
        #print(rmem.niters)
        for cue in self.cues:
            #print(rmem.regenerate(cue))
            pass
            # TODO Make a unit test out of this or remove it

    def test_with_sequential_salt(self) -> None:
        rmem: WithSequentialSalt = \
            RMem.make_instance((WithSequentialSalt, RMemAbs)) # type: ignore[assignment]
        
        eqn = (1, '+', 1, '=', 2)
        self.assertEqual(
            as_tuple(rmem.prep_absorb(eqn)),
            (1, 2, 3, 4, 5) + eqn
        )
        self.assertEqual(
            as_tuple(rmem.prep_regen(eqn)),
            (None, None, None, None, None) + eqn
        )

    def test_with_random_salt(self) -> None:
        rmem: WithRandomSalt = \
            RMem.make_instance((WithRandomSalt, RMemAbs)) # type: ignore[assignment]
        eqn = (1, '+', 1, '=', 2)
        self.assertEqual(
            len(as_tuple(rmem.prep_absorb(eqn))),
            15  # prepended rmem.nsalt random numbers
        )
        self.assertEqual(
            as_tuple(rmem.prep_regen(eqn)),
            (None,) * 10 + eqn
        )

    def test_with_ndups(self) -> None:
        rmem: WithNDups = \
            RMem.make_instance((WithNDups, RMemAbs), ndups=3) # type: ignore[assignment]
        eqn = (1, '+', 1, '=', 2)
        self.assertEqual(
            rmem.prep_absorb(eqn),
            eqn * 4
        )
        self.assertEqual(
            rmem.prep_regen(eqn),
            (None,) * 5 * 3 + eqn
        )

    def test_as_abs_painters(self) -> None:
        canvas = Canvas.make_from((1, None, 1, '=', 2))
        rmem: WithAdjacentRelativePainters = RMem.make_instance( # type: ignore[assignment]
            (WithAdjacentRelativePainters, LinearClarityWeight)
        )
        p = (Match(1), Right(1), '+')
        self.assertCountEqual(
            rmem.as_abs_painters(canvas, p),
            [(1, 2, '+'), (3, 4, '+')]
        )
