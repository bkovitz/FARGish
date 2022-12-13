# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import short


@dataclass(frozen=True)
class Painter:
    arguments: Tuple[VarSpec, VarSpec]
    predicates: List[Predicate]

    def generate_actions_left_to_right(self, ustate: UState) \
    -> Iterable[Action]:
        sourcevar, targetvar = self.arguments
        for outer_us in ustate.loop_through_sourcevar(sourcevar):
            if not self.source_ok(outer_us, sourcevar):
                continue
            for inner_us in outer_us.loop_through_targetvar_second(
                sourcevar, targetvar
            ):
                if not self.target_ok(inner_us, sourcevar, targetvar):
                    continue
                yield from self.make_actions_left_to_right(
                    inner_us, sourcevar, targetvar
                )

    def source_ok(us: UState, sourcevar) -> bool:
        if not us.exists(sourcevar):
            return False
        return all(
            predicate.source_ok(us, sourcevar)
                for predicate in predicates
        )
            
    def target_ok(us: UState, sourcevar, targetvar) -> bool:
        # check if targetvar location is blank?
        return all(
            predicate.target_ok(us, sourcevar, targetvar)
                for predicate in predicates
        )

    def make_actions_left_to_right(
        us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[Action]:
        spec = list(chain(
            predicate.spec_left_to_right(us, sourcevar, targetvar)
                for predicate in self.predicates
        ))
        return us.spec_to_action(spec)
            
@dataclass(frozen=True)
class UState:
    ws: Workspace
    d: Dict[Variable, Value]
    canvas_in_focus: Canvas

    def loop_through_sourcevar(self, sourcevar: VarSpec) -> Iterable[UState]:
        match vs_type(sourcevar):
            case VT.IndexVar:
                return (
                    self.unify(sourcevar, FullIndex(self.canvas_in_focus, i))
                        for i in self.canvas_in_focus.all_filled_indices()
                )

            case VT.CompoundIndexVar:
                # Need to step through possibly many nested loops, one for
                # each snippet level in sourcevar
#                return (
#                    self.unify(sourcevar, FullIndex(c, i))
#                        for c in ws.all_canvases():
#                            for i in 
#                )
                raise NotImplementedError(sourcevar)
            case _:
                raise NotImplementedError(sourcevar)

    def loop_through_targetvar_second(
        self, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[UState]:

    def exists(self, var: VarSpec) -> bool:
        match vs_type(var):
            case VT.IndexVar:
                return self.canvas_in_focus.index_exists(var)
            case _:  # TODO
                raise NotImplementedError(var)


@dataclass(frozen=True)
class Apart(Predicate):
    distance: int
    arg1: VarSpec
    arg2: VarSpec

    def source_ok(self, us: UState, sourcevar: VarSpec) -> bool:
        return True

    def target_ok(self, us: UState, sourcevar: VarSpec, targetvar: VarSpec) \
    -> bool:
        source_snippet, source_index = us.as_snippet_and_index(sourcevar)
        target_snippet, target_index = us.as_snippet_and_index(targetvar)
        return (
            source_snippet == target_snippet
            and
            abs(source_index - target_index) == self.distance
        )

    def spec_left_to_right(
        self, us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[ActionSpec]:
        match vs_type(sourcevar):
            case VT.IndexVar:
                snippet, index = us.as_snippet_and_index(sourcevar)
                new_index = index + self.distance
                yield PaintAt(FullIndex(snippet, new_index))
            case _:
                raise NotImplementedError(sourcevar)

    def spec_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[ActionSpec]:
        match vs_type(sourcevar):
            case VT.IndexVar:
                snippet, index = us.as_snippet_and_index(sourcevar)
                new_index = index - self.distance
                yield PaintAt(FullIndex(snippet, new_index))
            case _:
                raise NotImplementedError(sourcevar)

@dataclass(frozen=True)
class Succ(Predicate):
    arg1: VarSpec
    arg2: VarSpec

    def source_ok(self, us: UState, sourcevar: VarSpec) -> bool:
        return True

    def target_ok(self, us: UState, sourcevar: VarSpec, targetvar: VarSpec) \
    -> bool:
        return True

    def spec_left_to_right(
        self, us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[ActionSpec]:
        yield PaintValue(succ_of(us.value_at(sourcevar)))

    def spec_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[ActionSpec]:
        yield PaintValue(pred_of(us.value_at(sourcevar)))
