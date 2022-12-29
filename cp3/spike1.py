# spike1.py -- Just 'abc -> abd; ijk -> ?', with little flexibility

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field

from Model import Blank, Canvas, CanvasValue, FizzleNoSucc, Index, Letter, \
    Predicate, Succ, succ_of


#PaintNext = Callable[[Canvas, Index], Index]
# Paints the next value in a repetition. Returns the index where it painted.
# 

NextValueF = Callable[[CanvasValue], CanvasValue]
# Returns the next value (left to right) to paint in a repetition.

def next_succ(v: CanvasValue):
    match v:
        case Letter():
            return v.succ()
        case Blank():
            raise FizzleNoSucc()

@dataclass
class Repeater:
    start_value: CanvasValue
    next_value_f: NextValueF

    def fill_canvas(self, canvas: Canvas) -> None:
        # Should I return a list of anchored Painters?
        if canvas.min_index is None:
            return

        v = self.start_value
        first_time = True
        for i, j in canvas.all_index_steps():
            if first_time:
                canvas[i] = v
                first_time = False
            nextv = self.next_value_f(v)
            canvas[j] = nextv
            v = nextv

def detect_repetition(
    canvas: Canvas,
    num_errors_allowed: int=0
) -> Optional[Repeater]:
    # There could be many possible repetitions to detect. Should we start
    # a repetition-detector for each one? In this version, we detect only
    # the simplest kind of repetition.
    if canvas.min_index is None:
        return None
    first_letter = canvas[canvas.min_index]
    if not isinstance(first_letter, Letter):
        return None
    width = canvas.width()
    if width is None:
        return None
    expected_letters = make_letter_sequence(first_letter, succ_of, width)
    num_errors = 0
    for ci, ei in zip(canvas.all_indices(), range(len(expected_letters))):
        if canvas[ci] != expected_letters[ei]:
            num_errors += 1
            if num_errors > num_errors_allowed:
                return None
    return Repeater(first_letter, next_succ)

def detect_flaw(
    canvas: Canvas,
    repeater: Repeater
) -> Optional[Index]:
    '''Returns the index of "the" flaw, or None if there is no flaw. It can
    only detect one flaw.'''
    width = canvas.width()
    if width is None:
        return None
    perfect_canvas = Canvas.make_from_width(width)
    repeater.fill_canvas(perfect_canvas)

    for i in canvas.all_indices():
        if canvas[i] != perfect_canvas[i]:
            return i
    return None


def make_letter_sequence(
    first_letter: Letter,
    next_: Callable[[Letter], Letter],
    num_letters: int
) -> Sequence[Letter]:
    result: List[Letter] = [first_letter]
    l = first_letter
    for i in range(num_letters - 1):
        l = next_(l)
        result.append(l)
    return result

c = Canvas.make_from('abd')

r: Repeater = detect_repetition(c, 1)  # type: ignore[assignment]
print(r)

c2 = Canvas.make_from_width(5)
r.fill_canvas(c2)
print(c2)

print(detect_flaw(c, r))
