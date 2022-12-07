# testLoop.py -- Unit tests for Loop objects

import unittest
import inspect

from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check

from Model import Canvas, empty_subst, I, \
    Letter, Loop, LoopBody, LoopOverCanvas, \
    Subst, UState

from Log import lo, set_log_level
from util import pts, reseed, short

class TestBody(LoopBody):

    def run(self, subst: Subst, us: UState) -> Iterable[Tuple[Subst, Any]]:
        letter = us.value_at(subst.as_addr(I))
        if letter is not None and letter != Letter('a'):
            yield (subst, letter)

def make_test_us(s: str) -> UState:
    return UState(Canvas.make_from(s))

class TestLoop(unittest.TestCase):

    def test_simple_loop(self) -> None:
        loop = LoopOverCanvas(I)
        us = make_test_us('ajaqb')
        body = TestBody()
        got = list(loop.run(empty_subst, us, body))
        self.assertCountEqual(
            got,
            [
                (Subst.make_from((I, 2)), Letter('j')),
                (Subst.make_from((I, 4)), Letter('q')),
                (Subst.make_from((I, 5)), Letter('b'))
            ]
        )

