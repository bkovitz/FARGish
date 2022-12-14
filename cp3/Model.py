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
class Variable:
    # Don't instantiate this. Instantiate only subclasses.
    name: str

    def __repr__(self) -> str:
        return self.name

@dataclass(frozen=True)
class IndexVariable(Variable):
    pass

I = IndexVariable('I')
J = IndexVariable('J')

VarSpec = IndexVariable   # TODO Union with PainterVariable, CanvasVariable,
                          # CompoundVariable

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
class Subst:
    '''A set of substitutions, i.e. mappings from variables to values.
    bottom_subst indicates a failed substitution.'''
    d: PMap[VarSpec, Value] = field(default_factory=lambda: pmap())

    def __bool__(self) -> bool:
        # Returns True if valid Subst, False if BottomSubst (i.e. failed).
        return True

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        # Is this all the unification we need? No need to store expressions
        # like I->J+2 and then set I to 5 if J gets unified with 3?
        match (lhs, rhs):
            case (x, y) if x == y:
                return self
            case (IndexVariable(), int()):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
            case _:
                raise NotImplementedError((lhs, rhs))

class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

@dataclass(frozen=True)
class UState:
    ws: Workspace
    canvas_in_focus: Canvas
    subst: Subst = field(default_factory=lambda: Subst())

    # NEXT .value_at()

    def loop_through_sourcevar(self, sourcevar: VarSpec) -> Iterable[UState]:
        match sourcevar:
            case IndexVariable():
                return (
                    self.unify(sourcevar, FullIndex(self.canvas_in_focus, i))
                        for i in self.canvas_in_focus.all_filled_indices()
                )

#            case VT.CompoundIndexVar:
#                # Need to step through possibly many nested loops, one for
#                # each snippet level in sourcevar
##                return (
##                    self.unify(sourcevar, FullIndex(c, i))
##                        for c in ws.all_canvases():
##                            for i in 
##                )
#                raise NotImplementedError(sourcevar)
            case _:
                raise NotImplementedError(sourcevar)

    def loop_through_targetvar_second(
        self, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[UState]:
        match targetvar:
            case IndexVariable():
                source_canvas, source_index = self.as_canvas_and_index(
                    sourcevar
                )
                return (
                    self.unify(
                        targetvar, FullIndex(source_canvas, target_index)
                    )
                        for target_index in source_canvas.all_indices_right_of(
                            source_index
                        )
                )
            case _:
                raise NotImplementedError(targetvar)

    def exists(self, var: VarSpec) -> bool:
        match var:
            case IndexVariable():
                return self.canvas_in_focus.index_exists(var)
            case _:  # TODO
                raise NotImplementedError(var)

    def unify(self, lhs: Expr, rhs: Expr) -> UState:
        subst = self.subst.unify(lhs, rhs)
        if subst is self.subst:
            return self
        else:
            return replace(self, subst=subst)

@dataclass(frozen=True)
class Apart(Predicate):
    distance: int
    arg1: VarSpec
    arg2: VarSpec

    def source_ok(self, us: UState, sourcevar: VarSpec) -> bool:
        return True

    def target_ok(self, us: UState, sourcevar: VarSpec, targetvar: VarSpec) \
    -> bool:
        source_snippet, source_index = us.as_canvas_and_index(sourcevar)
        target_snippet, target_index = us.as_canvas_and_index(targetvar)
        return (
            source_snippet == target_snippet
            and
            abs(source_index - target_index) == self.distance
        )

    def spec_left_to_right(
        self, us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[ActionSpec]:
        match sourcevar:
            case IndexVariable():
                snippet, index = us.as_canvas_and_index(sourcevar)
                new_index = index + self.distance
                yield PaintAt(FullIndex(snippet, new_index))
            case _:
                raise NotImplementedError(sourcevar)

    def spec_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[ActionSpec]:
        match sourcevar:
            case IndexVariable():
                snippet, index = us.as_canvas_and_index(sourcevar)
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
