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

########## Canvas-cell contents ##########

@dataclass(frozen=True)
class Letter:
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if self.c < 'a' or self.c > 'z':
            raise ValueError(f"Letter {self.c!r}: must be in range 'a'..'z'.")

    @classmethod
    def from_str(self, c: str) -> Union[Letter, Blank]:
        if len(c) != 1:
            raise ValueError('Letter.from_str(): {c!r} must have len==1')
        if c == ' ':
            return Blank()
        else:
            return Letter(c)

    def succ(self) -> Letter:
        if self.c >= 'z':
            raise FizzleNoSucc
        else:
            return Letter(chr(ord(self.c) + 1))

    def pred(self) -> Letter:
        if self.c <= 'a':
            raise FizzleNoPred
        else:
            return Letter(chr(ord(self.c) - 1))

    def short(self) -> str:
        return repr(self.c)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.c!r})'

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.c!r})'


@dataclass(frozen=True)
class Blank:

    def __repr__(self) -> str:
        return self.__class__.__name__

    short = __repr__

    def __str__(self) -> str:
        return ' '

def is_blank(x: Any) -> TypeGuard[Blank]:
    return isinstance(x, Blank)

CanvasValue = Union[Letter, Blank]  # TODO change str to Letter

def is_canvas_value(x: Any) -> TypeGuard[CanvasValue]:
    return (
        isinstance(x, Letter)
        or
        isinstance(x, Blank)
    )

########## The canvas ##########

@dataclass
class Canvas:
    d: Dict[Index, CanvasValue] = field(default_factory=lambda: {})
    min_index: Optional[Index] = None
    max_index: Optional[Index] = None

    @classmethod
    def make_from(cls, s: str) -> Canvas:
        if not s:
            return cls()
        else:
            d: Dict[Index, CanvasValue] = {}
            for i, c in zip(range(1, len(s) + 1), s):
                d[i] = Letter.from_str(c)
            return cls(d, min_index=1, max_index=len(s) + 1)

    def __getitem__(self, a: Index) -> Optional[CanvasValue]:
        return self.d.get(a, None)

    def __setitem__(self, a: Index, v: CanvasValue) -> None:
        self.d[a] = v
        if self.min_index is None:
            self.min_index = a
            self.max_index = a
        else:
            assert self.max_index is not None
            self.min_index = min(a, self.min_index)
            self.max_index = max(a, self.max_index)

    def all_indices(self) -> Iterable[Index]:
        if self.min_index is None:
            return
        else:
            assert self.max_index is not None
            yield from range(self.min_index, self.max_index + 1)

    def all_filled_indices(self) -> Iterable[Index]:
        yield from self.d.keys()

########## Painter ##########

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
        spec = tuple(chain(
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

    def as_index(self, e: VarSpec) -> Optional[Index]:
        match g.get(e, None):
            case None:
                return None
            case int() as i:
                return i
            case _:
                raise NotImplementedError((e, type(e)))

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

    @classmethod
    def make_from(cls, s: str) -> UState:
        

    def value_at(self, i: IndexVariable | Index) -> CanvasValue:
        match i:
            case int():
                return self.canvas_in_focus.value_at(i)
            case IndexVariable():
                return self.canvas_in_focus.value_at(
                    self.subst.value_of(i)
                )

    def as_canvas_and_index(self, e: VarSpec) -> Tuple[Canvas, Index]:
        match e:
            case IndexVariable():
                return (self.canvas_in_focus, self.subst.value_of(e))
            # TODO CompoundVariable
            # TODO int
            case _:
                raise NotImplementedError(e, type(e))

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

    def spec_to_actions(self, 

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
