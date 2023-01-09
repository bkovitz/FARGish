# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import short


Index = int
Value = Index  # TODO Union[Index, Painter, Canvas]?

@dataclass(frozen=True)
class Variable:
    # Don't instantiate this. Instantiate only subclasses.
    name: str

    def __repr__(self) -> str:
        return self.name

    def short(self) -> str:
        return self.name

@dataclass(frozen=True)
class IndexVariable(Variable):

    def __repr__(self) -> str:
        return self.name


D = Variable('D')  # "distance", in Apart
I = IndexVariable('I')
J = IndexVariable('J')

VarSpec = Variable   # TODO Union with IndexVariable, PainterVariable,
                     # CanvasVariable, CompoundVariable
Expr = Any

########## Canvas-cell contents ##########

@dataclass(frozen=True)
class Letter:
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if len(self.c) != 1 or self.c < 'a' or self.c > 'z':
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
        return self.c

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

########## Substitutions ##########

@dataclass(frozen=True)
class Subst:
    '''A set of substitutions, i.e. mappings from variables to values.
    bottom_subst indicates a failed substitution.'''
    d: PMap[VarSpec, Value] = field(default_factory=lambda: pmap())

    @classmethod
    def make_from(cls, *pairs: Tuple[VarSpec, Value]) -> Subst:
        result = cls()
        for lhs, rhs in pairs:
            result = result.unify(lhs, rhs)
        return result

    def __bool__(self) -> bool:
        # Returns True if valid Subst, False if BottomSubst (i.e. failed).
        return True

    def __contains__(self, e: Optional[VarSpec]) -> bool:
        '''Does this Subst have a value defined for 'e'?'''
        return e in self.d

    def as_index(self, e: VarSpec) -> Optional[Index]:
        match self.d.get(e, None):
            case None:
                return None
            case int() as i:
                return i
            case _:
                raise NotImplementedError((e, type(e)))

    def __getitem__(self, e: VarSpec) -> Optional[Value]:
        return self.d.get(e, None)

    def value_of(self, e: VarSpec) -> Optional[Value]:
        return self.d.get(e, None)

    def value_of_or_fizzle(self, e: VarSpec) -> Value:
        result = self.value_of(e)
        if result is None:
            raise FizzleNoValue(e)
        else:
            return result

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        # Is this all the unification we need? No need to store expressions
        # like I->J+2 and then set I to 5 if J gets unified with 3?
        match (lhs, rhs):
            case (x, y) if x == y:
                return self
            case (IndexVariable(), int()):
                return self.set_lhs_rhs(lhs, rhs)
            case (IndexVariable(), FullIndex()):
                return self.set_lhs_rhs(lhs, rhs)
            case (VarSpec(), int()):
                return self.set_lhs_rhs(lhs, rhs)
            case _:
                raise NotImplementedError((lhs, rhs))
        assert False, "shouldn't get here"  # MYPY bug?
        return bottom_subst

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs in self.d:
            if self.d[lhs] == rhs:
                return self
            else:
                return bottom_subst
        else:
            return Subst(self.d.set(lhs, rhs))

    def pairs(self) -> Iterable[Tuple[VarSpec, Value]]:
        yield from self.d.items()

    def vars(self) -> Sequence[VarSpec]:
        return list(self.d.keys())

    # TODO UT
    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

    def veryshort(self) -> str:
        return ', '.join(f'{short(k)}={short(v)}' for k, v in self.d.items())

    __repr__ = short

class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

empty_subst = Subst()
bottom_subst = BottomSubst()

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
            return cls(d, min_index=1, max_index=len(s))

    @classmethod
    def make_from_width(cls, width: int) -> Canvas:
        assert width > 0
        return cls(min_index=1, max_index=width)

    def __getitem__(self, a: Optional[Index]) -> Optional[CanvasValue]:
        if a is None:
            return None
        else:
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

    def value_at(self, i: Optional[Index]) -> Optional[CanvasValue]:
        match i:
            case None:
                return None
            case int():
                return self[i]

    def value_at_or_fizzle(self, i: Optional[Index]) -> CanvasValue:
        match i:
            case None:
                raise FizzleNoValueThere(i)
            case int():
                result = self.d.get(i, None)
                if result is None:
                    raise FizzleNoValueThere(i)
                else:
                    return result

    def exists(self, i: Optional[Index]) -> bool:
        '''Does a Letter exist with index 'i'?'''
        match i:
            case None:
                return False
            case _:
                return isinstance(self.value_at(i), Letter)

    def has_index(self, i: Optional[Index]) -> bool:
        if i is None:
            return False
        if self.min_index is None:
            return i in self.d
        else:
            assert self.max_index is not None
            return i >= self.min_index and i <= self.max_index

    def all_indices(self) -> Iterable[Index]:
        if self.min_index is None:
            return
        assert self.max_index is not None
        yield from range(self.min_index, self.max_index + 1)

    def all_filled_indices(self) -> Iterable[Index]:
        for k, v in self.d.items():
            if not is_blank(v):
                yield k

    # TODO UT
    def all_indices_right_of(self, i: Index) -> Iterable[Index]:
        if self.min_index is None:
            return
        if i < self.min_index:
            yield from self.all_indices()
        else:
            assert self.max_index is not None
            yield from range(i + 1, self.max_index + 1)
        
    # TODO UT
    def all_indices_left_of(self, i: Index) -> Iterable[Index]:
        if self.max_index is None:
            return
        if i > self.max_index:
            yield from self.all_indices()
        else:
            assert self.min_index is not None
            yield from range(self.min_index, i)

    # TODO UT
    def all_index_steps(self) -> Iterable[Tuple[Index, Index]]:
        if self.min_index is None:
            return
        assert self.max_index is not None
        for i in range(self.min_index, self.max_index):
            yield (i, i + 1)

    def all_index_pairs(self) -> Iterable[Tuple[Index, Index]]:
        '''Returns all pairs (i, j) where i and j are indices into the canvas
        and j > i.'''
        for i in self.all_indices():
            for j in self.all_indices_right_of(i):
                yield (i, j)

    def is_filled(self, i: Optional[Index]) -> bool:
        '''A filled cell is one that contains a Letter.'''
        return isinstance(self.value_at(i), Letter)

    # TODO UT
    def width(self) -> Optional[int]:
        if self.min_index is None or self.max_index is None:
            return None
        return self.max_index - self.min_index + 1

    def __str__(self) -> str:
        return ''.join(str(self[i]) for i in self.all_indices())

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f"{cl}({str(self)!r})"


@dataclass(frozen=True)
class FullIndex:
    canvas: Canvas
    i: Index

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({str(self.canvas)!r}, {self.i})'

########## Workspace ##########

@dataclass(frozen=True)
class Workspace:
    canvases: Dict[str, Canvas]   # name -> Canvas
    #painters: Dict[str, Painter]  # name -> Painter

#    def __getitem__(self, i: Union[Index, FullIndex, None]) \
#    -> Union[Value, None]:  # TODO also can return Painter?
#        match i:
#            case int():
#                return self[

    @classmethod
    def make_from(cls, c: Canvas) -> Workspace:
        return Workspace({'C0': c})


########## UState ##########

@dataclass(frozen=True)
class UState:
    '''"Unification state": a stage in the process of assigning values to
    variables when running a Painter to produce Actions.'''

    ws: Workspace
    canvas_in_focus: Canvas
    subst: Subst = field(default_factory=lambda: Subst())

    @classmethod
    def make_from(cls, s: str) -> UState:
        canvas = Canvas.make_from(s)
        return cls(Workspace.make_from(canvas), canvas, Subst())

    def value_at(self, i: IndexVariable | Index) -> Optional[CanvasValue]:
        match i:
            case int():
                return self.canvas_in_focus[i]
            case IndexVariable():
                #return self.canvas_in_focus.value_at(self.subst.value_of(i))
                c, ii = self.as_canvas_and_index(i)
                return c.value_at(ii)

    def value_at_or_fizzle(self, i: IndexVariable | Index) -> CanvasValue:
        match i:
            case int():
                return self.canvas_in_focus.value_at_or_fizzle(i)
            case IndexVariable():
                c, ii = self.as_canvas_and_index(i)
                return c.value_at_or_fizzle(ii)

    #def as_index(self, i: 

    @no_type_check
    def exists(self, e: Index | VarSpec | Tuple[Canvas, Index]) -> bool:
        match e:
            case int():
                return self.canvas_in_focus.exists(e)
            case IndexVariable():
                return self.exists(self.as_canvas_and_index(e))
            case (Canvas() as c, int(i)):
                return c.exists(i)
            case _:
                raise NotImplementedError(e, type(e))

    def as_canvas_and_index(self, e: VarSpec | Index) -> Tuple[Canvas, Index]:
        match e:
            case int():
                return (self.canvas_in_focus, e)
            case IndexVariable():
                v = self.subst.value_of_or_fizzle(e)
                match v:
                    case int():
                        return (self.canvas_in_focus, v)
                    case FullIndex(c, i):
                        return (c, i)
            # TODO CompoundVariable
            # TODO int
            case _:
                raise NotImplementedError(e, type(e))
        raise ValueError  # This should never be reached; only needed to
                          # placate mypy 0.971

    def loop_through_sourcevar(self, sourcevar: VarSpec) -> Iterable[UState]:
        match sourcevar:
            case IndexVariable():
                if sourcevar in self.subst:
                    c, i = self.as_canvas_and_index(sourcevar)
                    if c.is_filled(i):
                        yield self
                else:
                    for i in self.canvas_in_focus.all_filled_indices():
                        u = self.unify(
                            sourcevar,
                            FullIndex(self.canvas_in_focus, i)
                        )
                        if u.is_valid():
                            yield u

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

    def loop_through_targetvar_first(
        self, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[UState]:
        '''Looping through the targetvar first means running the inner loop
        (the "second" loop) but the targetvar is the first argument.
        Consequently, the sourcevar (the second argument) is fixed to a
        constant and the targetvar loops through all other available values.
        If both the targetvar and sourcevar are IndexVariables, the targetvar
        moves only through indices to the left of sourcevar, at least when
        both refer to the same canvas.

        Unlike .loop_through_sourcevar(), this method loops through all
        cell indices with an appropriate index, regardless of their contents,
        i.e. including both filled and unfilled cells.'''
        match targetvar:
            case IndexVariable():
                source_canvas, source_index = self.as_canvas_and_index(
                    sourcevar
                )
                return (
                    self.unify(
                        targetvar, FullIndex(source_canvas, target_index)
                    )
                        for target_index in source_canvas.all_indices_left_of(
                            source_index
                        )
                )
            case _:
                raise NotImplementedError(targetvar)

#    def exists(self, var: VarSpec) -> bool:
#        match var:
#            case IndexVariable():
#                return self.canvas_in_focus.index_exists(var)
#            case _:  # TODO
#                raise NotImplementedError(var)
#
    def unify(self, lhs: Expr, rhs: Expr) -> UState:
        subst = self.subst.unify(lhs, rhs)
        if subst is self.subst:
            return self
        else:
            return replace(self, subst=subst)

    def is_valid(self) -> bool:
        '''False iff self.subst is BottomSubst, i.e. if self is the result of
        a failed attempt to unify.'''
        return bool(self.subst)

    def spec_to_actions(self, spec: Tuple[ActionSpec, ...]) -> Iterable[Action]:
        fi: Optional[FullIndex] = None
        v: Optional[CanvasValue] = None

        for sp in spec:
            match sp:
                case PaintAt(spec_fi):
                    fi = spec_fi
                case PaintValue(spec_v):
                    v = spec_v
                case _:
                    raise NotImplementedError(sp)
        if fi is not None and v is not None:
            yield Paint(fi, v)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.subst)})'

########## Painter ##########

@dataclass(frozen=True)
class Painter:
    arguments: Tuple[VarSpec, VarSpec]
    predicates: Tuple[Predicate, ...]

    def generate_actions(self, us: UState) -> Iterable[Action]:
        yield from self.generate_actions_left_to_right(us)
        yield from self.generate_actions_right_to_left(us)

    def generate_actions_left_to_right(self, ustate: UState) \
    -> Iterable[Action]:
        sourcevar, targetvar = self.arguments
        for outer_us in ustate.loop_through_sourcevar(sourcevar):
            try:
                if not self.source_ok(outer_us, sourcevar):
                    continue
                for inner_us in outer_us.loop_through_targetvar_second(
                    sourcevar, targetvar
                ):
                    try:
                        if not self.target_ok(inner_us, sourcevar, targetvar):
                            continue
                        yield from self.make_actions_left_to_right(
                            inner_us, sourcevar, targetvar
                        )
                    except Fizzle as exc:
                        lo(9, 'FIZZ1', exc)
                        continue
            except Fizzle as exc:
                lo(9, 'FIZZ2', exc)
                continue

    def generate_actions_right_to_left(self, ustate: UState) \
    -> Iterable[Action]:
        targetvar, sourcevar = self.arguments
        for outer_us in ustate.loop_through_sourcevar(sourcevar):
            #lo('OUTER', outer_us)
            try:
                if not self.source_ok(outer_us, sourcevar):
                    continue
                for inner_us in outer_us.loop_through_targetvar_first(
                    targetvar, sourcevar
                ):
                    #lo('INNER', inner_us)
                    try:
                        if not self.target_ok(inner_us, sourcevar, targetvar):
                            continue
                        yield from self.make_actions_right_to_left(
                            inner_us, targetvar, sourcevar
                        )
                    except Fizzle as exc:
                        lo(9, 'FIZZ3', exc)
                        continue
            except Fizzle as exc:
                lo(9, 'FIZZ4', exc)
                continue

    def source_ok(self, us: UState, sourcevar) -> bool:
        if not us.exists(sourcevar):
            return False
        return all(
            predicate.source_ok(us, sourcevar)
                for predicate in self.predicates
        )
            
    def target_ok(self, us: UState, sourcevar, targetvar) -> bool:
        # check if targetvar location is blank?
        return all(
            predicate.target_ok(us, sourcevar, targetvar)
                for predicate in self.predicates
        )

    def make_actions_left_to_right(
        self, us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[Action]:
        spec = tuple(chain.from_iterable(
            predicate.spec_left_to_right(us, sourcevar, targetvar)
                for predicate in self.predicates
        ))
        yield from us.spec_to_actions(spec)

    def make_actions_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[Action]:
        spec = tuple(chain.from_iterable(
            predicate.spec_right_to_left(us, targetvar, sourcevar)
                for predicate in self.predicates
        ))
        yield from us.spec_to_actions(spec)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.arguments)}, {short(self.predicates)}'

#class Painter:
#    spatial_relation: SRelation
#    value_relation: VRelation
#
#    Apart[2, I, J]
#    Succ[I, J]

# NEXT
#@dataclass(frozen=True)
#class AnchoredPainter:
#    painter: Painter
#    arguments: Tuple[Index, Index]  # TODO allow other types
#
#    def generate_actions(self, us: UState) -> Iterable[Action]:
        

########## Predicates ##########

class Predicate(ABC):

    @abstractmethod
    def source_ok(self, us: UState, sourcevar: VarSpec) -> bool:
        pass

    @abstractmethod
    def target_ok(self, us: UState, sourcevar: VarSpec, targetvar: VarSpec) \
    -> bool:
        pass

    @abstractmethod
    def spec_left_to_right(
        self, us: UState, sourcevar: VarSpec, targetvar: VarSpec
    ) -> Iterable[ActionSpec]:
        pass

    @abstractmethod
    def spec_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[ActionSpec]:
        pass

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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.distance)}, {short(self.arg1)}, {short(self.arg2)})'

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
        yield PaintValue(succ_of(us.value_at_or_fizzle(sourcevar)))

    def spec_right_to_left(
        self, us: UState, targetvar: VarSpec, sourcevar: VarSpec
    ) -> Iterable[ActionSpec]:
        yield PaintValue(pred_of(us.value_at_or_fizzle(sourcevar)))

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.arg1)}, {short(self.arg2)}'

def succ_of(v: Optional[CanvasValue]) -> Letter:
    match v:
        case Letter():
            return v.succ()
        case _:
            raise FizzleNoSucc

def pred_of(v: Optional[CanvasValue]) -> Letter:
    match v:
        case Letter():
            return v.pred()
        case _:
            raise FizzleNoPred

########## Actions ##########

class ActionSpec:
    pass

@dataclass(frozen=True)
class PaintAt(ActionSpec):
    fi: FullIndex

@dataclass(frozen=True)
class PaintValue(ActionSpec):
    v: CanvasValue

class Action(ABC):

    @abstractmethod
    def go(self, ws: Workspace):
        pass

@dataclass(frozen=True)
class Paint(Action):
    fi: FullIndex
    v: CanvasValue

    def go(self, ws: Workspace):
        print(self)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.fi)}, {short(self.v)})'

########## Exceptions ##########

class Fizzle(Exception):
    pass

class FizzleCantGoThere(Fizzle):
    '''Generic class for FizzleNoSucc, FizzleNoPred, and any other kind of
    fizzling that involves 'running out of room'--i.e. being unable to
    apply a function to a certain constant.'''
    pass

@dataclass(frozen=True)
class FizzleNoSucc(FizzleCantGoThere):

    def __str__(self) -> str:
        return 'No successor'

@dataclass(frozen=True)
class FizzleNoPred(FizzleCantGoThere):

    def __str__(self) -> str:
        return 'No predecessor'

@dataclass(frozen=True)
class FizzleNoValue(Fizzle):
    v: VarSpec

    def __str__(self) -> str:
        return f'No value defined for {self.v}'

@dataclass(frozen=True)
class FizzleNoValueThere(Fizzle):
    i: Union[FullIndex, Index, None]

    def __str__(self) -> str:
        return f'No value at {self.i}'
