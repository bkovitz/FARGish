# spike2.py -- A quick test of the idea of detecting *everything* all at once
#
# Specifically, this spike tries out running all predicates as detectors,
# on the Cartesian product of all canvas indices × all canvas indices.

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, combinations
from collections import defaultdict
import operator
from io import StringIO

from Model import Canvas, CanvasValue, D, Expr, \
    Fizzle, FizzleNoPred, FizzleNoSucc, FizzleNoValue, FullIndex, \
    I, Index, IndexVariable, \
    J, K, L, Letter, LetterVariable, PainterVariable, P, \
    PMap, pmap, pred_of, succ_of, \
    Value, Variable, VarSpec
    # from Model2.py
from Log import lo, trace
from util import intersection, Numeric, pr, short, veryshort

# NEXT Rewrite succ_of to return None if it fails.

# TODO Pass Workspace, not Canvas. Or maybe pass UState.

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
            case (PainterVariable(), Painter()):
                return self.set_lhs_rhs(lhs, rhs)
            case (LetterVariable(), Letter()):
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
        '''Returns all of the associations between variables and values.'''
        yield from self.d.items()

    def vars(self) -> Sequence[VarSpec]:
        return list(self.d.keys())

    # TODO UT
    def merge(self, other: Subst) -> Subst:
        '''Other's pairs override self's pairs.'''
        return Subst(self.d.update(other.d))

    # TODO UT
    def merge_with_unify(self, other: Subst) -> Subst:
        '''If any pair in 'other' contradicts a pair in 'self', we return
        BottomSubst.'''
        result = self
        for lhs, rhs in other.pairs():
            result = result.unify(lhs, rhs)
        return result

    def remove(self, *vars: VarSpec) -> Subst:
        '''Returns Subst with vars undefined. It is not an error to remove a
        variable that does not exist.'''
        d = self.d
        for var in vars:
            d = d.discard(var)
        return Subst(d)

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(sorted(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        ))
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

##### Predicates #####

class Predicate(ABC):

    @abstractmethod
    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        pass

    @abstractmethod
    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        '''Steps through all values for both principal variables of the
        Predicate.'''
        pass

    @abstractmethod
    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass

    @abstractmethod
    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass

    @abstractmethod
    def principal_arguments(self) -> Collection[Variable]:
        pass

def safe_eq(a: Any, b: Any) -> bool:
    if a is None or b is None:
        return False
    else:
        return a == b

def safe_sub(a: Optional[int], b: Optional[int]) -> Optional[int]:
    if a is None or b is None:
        return None
    else:
        return a - b

def safe_add(a: Optional[int], b: Optional[int]) -> Optional[int]:
    if a is None or b is None:
        return None
    else:
        return a + b


@dataclass(frozen=True)
class Apart(Predicate):
    '''Apart[D, I, J]'''
    default_subst: ClassVar[Subst] = Subst.make_from((D, 2))

    def principal_arguments(self) -> Collection[Variable]:
        return (I, J)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        su = self.default_subst.merge(su)
        if (
            c.has_index(su[I])
            and
            c.has_index(su[J])
            and
            safe_eq(su[D], safe_sub(su.as_index(J), su.as_index(I)))
        ):
            return su
        else:
            return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for i, j in c.all_index_pairs():
            yield Subst.make_from((I, i), (J, j))

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        j = safe_add(su.as_index(I), su.as_index(D))
        if not c.has_index(j):
            return None
        else:
            assert j is not None
            return (su.unify(J, j), PaintAt(j))

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        i = safe_sub(su.as_index(J), su.as_index(D))
        if not c.has_index(i):
            return None
        else:
            assert i is not None
            return (su.unify(I, i), PaintAt(i))

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.default_subst)})'

@dataclass(frozen=True)
class Same(Predicate):
    '''Same[I, J]'''

    def principal_arguments(self) -> Collection[Variable]:
        return (I, J)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        if safe_eq(c[su.as_index(I)], c[su.as_index(J)]):
            return su
        else:
            return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for i, j in c.all_index_pairs():
            yield Subst.make_from((I, i), (J, j))

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        if isinstance(letter := c[su.as_index(I)], Letter):
            return (su, PaintValue(letter))
        else:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        if isinstance(letter := c[su.as_index(J)], Letter):
            return (su, PaintValue(letter))
        else:
            return None

@dataclass(frozen=True)
class Succ(Predicate):
    '''Succ[I, J]'''

    def principal_arguments(self) -> Collection[Variable]:
        return (I, J)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(succ_of(c[su.as_index(I)]), c[su.as_index(J)]):
                return su
            else:
                return None
        except Fizzle:
            return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for i, j in c.all_index_pairs():
            yield Subst.make_from((I, i), (J, j))

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := succ_of(c[su.as_index(I)]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoSucc:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := pred_of(c[su.as_index(J)]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoPred:
            return None

@dataclass(frozen=True)
class Pred(Predicate):
    '''Pred[I, J], i.e. the value at I must be the predecessor of the value
    at J.'''

    def principal_arguments(self) -> Collection[Variable]:
        return (I, J)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(pred_of(c[su.as_index(I)]), c[su.as_index(J)]):
                return su
            else:
                return None
        except Fizzle:
            return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for i, j in c.all_index_pairs():
            yield Subst.make_from((I, i), (J, j))

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := pred_of(c[su.as_index(I)]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoPred:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := succ_of(c[su.as_index(J)]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoSucc:
            return None

@dataclass(frozen=True)
class Inside(Predicate):
    '''Inside[P, K]'''
    
    def principal_arguments(self) -> Collection[Variable]:
        return (P, K)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        match (su[P], su[K]):
            case (Painter(_, psubst, _), int(k)):
                match (psubst[I], psubst[J]):
                    case (int(i), int(j)):
                        if (
                            i < k and k < j
                            and
                            c.has_index(i) and c.has_index(j)
                        ):
                            return su  # TODO Should we add I and J to su?
                        else:
                            return None
                    case _:
                        return None
            case _:
                return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for p in painters:
            for k in c.all_indices():
                yield Subst.make_from((P, p), (K, k)) # type: ignore[arg-type]

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass  # TODO

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass  # TODO

@dataclass(frozen=True)
class FilledWith(Predicate):
    '''FilledWith[K, L]'''

    def principal_arguments(self) -> Collection[Variable]:
        return (K,)

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        match su.as_index(K):
            case int(k):
                match letter := c[k]:
                    case Letter():
                        return su.unify(L, letter)
                    case _:
                        return None
            case _:
                return None

    def all_substs(self, c: Canvas, painters: Collection[Painter]) \
    -> Iterable[Subst]:
        for k in c.all_indices():
            yield Subst.make_from((K, k))

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass  # TODO

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass  # TODO

##### AnchorAttributes #####

class AnchorAttribute(ABC):

    @classmethod
    @abstractmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        pass

    @abstractmethod
    def is_match(self, c: Canvas, su: Subst) -> bool:
        '''Do the Canvas and Subst match the condition of this
        AnchorAttribute?'''
        pass

@dataclass(frozen=True)
class Holds(AnchorAttribute):
    var: IndexVariable
    letter: Letter

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if isinstance(letter := c[su.as_index(k)], Letter):
                        yield Holds(k, letter)
                case _:
                    pass

    def is_match(self, c: Canvas, su: Subst) -> bool:
        return c[su.as_index(self.var)] == self.letter

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.var)}, {short(self.letter)})'

@dataclass(frozen=True)
class LeftmostIndex(AnchorAttribute):
    var: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if su[k] == 1:
                        yield LeftmostIndex(k)
                case _:
                    pass

    def is_match(self, c: Canvas, su: Subst) -> bool:
        return safe_eq(c.min_index, su[self.var])
        
    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.var)})'

@dataclass(frozen=True)
class RightmostIndex(AnchorAttribute):
    var: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if safe_eq(su[k], c.max_index):
                        yield RightmostIndex(k)
                case _:
                    pass

    def is_match(self, c: Canvas, su: Subst) -> bool:
        return safe_eq(c.max_index, su[self.var])

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.var)})'


@dataclass(frozen=True)
class InextremeIndex(AnchorAttribute):
    var: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if cls.is_inextreme(c, v):
                        yield InextremeIndex(k)
                case _:
                    pass

    @classmethod
    def is_inextreme(cls, c: Canvas, x: Any) -> bool:
        match x:
            case int(i):
                if c.max_index is None:
                    return False
                return i != 1 and i != c.max_index
            case _:
                return False

    def is_match(self, c: Canvas, su: Subst) -> bool:
        return (
            c.has_index(su[self.var])
            and
            not safe_eq(c.min_index, su[self.var])
            and
            not safe_eq(c.max_index, su[self.var])
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.var)})'

@dataclass(frozen=True)
class Painter:
    predicates: FrozenSet[Predicate]
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]
    left_argument: Variable
    right_argument: Variable

    @classmethod
    def from_detections(cls, detections: FrozenSet[Detection]) -> Painter:
        predicates: Set[Predicate] = set()
        subst = Subst()
        anchor_attributes: Set[AnchorAttribute] = set()
        principal_arguments: Set[Variable] = set()

        for d in detections:
            #predicate, su, aas = detection
            #predicates.add(predicate)
            #subst = subst.merge(su)
            #anchor_attributes |= aas
            predicates.add(d.predicate)
            subst = subst.merge(d.subst)
            anchor_attributes |= d.anchor_attributes
            principal_arguments.update(d.predicate.principal_arguments())
        assert len(principal_arguments) == 2
        return cls(
            frozenset(predicates),
            subst,
            frozenset(anchor_attributes),
            cls.get_left_arg(principal_arguments),
            cls.get_right_arg(principal_arguments)
        )

    @classmethod
    def get_left_arg(cls, principal_arguments: Iterable[Variable]) \
    -> Variable:
        # TODO What if there is no left arg?
        for var in principal_arguments:
            if var.place == 'LeftArgument':
                return var
        raise NotImplementedError(f'no left arg: {principal_arguments}')

    @classmethod
    def get_right_arg(cls, principal_arguments: Iterable[Variable]) \
    -> Variable:
        # TODO What if there is no right arg?
        for var in principal_arguments:
            if var.place == 'RightArgument':
                return var
        raise NotImplementedError(f'no right arg: {principal_arguments}')

#    def left_substs(self, c: Canvas) -> Iterable[Subst]:
#        '''Clears J and steps I through all indices in canvas.'''
#        su = self.subst.remove(I, J)
#        for i in c.all_indices():
#            yield su.unify(I, i)
#
#    def right_substs(self, c: Canvas) -> Iterable[Subst]:
#        su = self.subst.remove(I, J)
#        for j in c.all_indices():
#            yield su.unify(J, j)

    def left_substs(self, ws: Workspace) -> Iterable[Subst]:
        '''Clears both principal arguments and steps the left argument
        through all its possible values.'''
        su = self.subst.remove(self.left_argument, self.right_argument)
        match self.left_argument:
            case IndexVariable():
                for ix in ws.all_indices():
                    yield su.unify(self.left_argument, ix)
            case PainterVariable():
                for px in ws.all_painters():
                    yield su.unify(self.left_argument, px)

    def right_substs(self, ws: Workspace) -> Iterable[Subst]:
        '''Clears both principal arguments and steps the right argument
        through all its possible values.'''
        su = self.subst.remove(self.left_argument, self.right_argument)
        match self.right_argument:
            case IndexVariable():
                for ix in ws.all_indices():
                    yield su.unify(self.right_argument, ix)
            case PainterVariable():
                for px in ws.all_painters():
                    yield su.unify(self.right_argument, px)

    def result_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[PainterResult]:
        specs: List[ActionSpec] = []
        for predicate in self.predicates:
            tup = predicate.spec_left_to_right(c, su)
            if tup is None:
                return None
            else:
                su = tup[0]
                specs.append(tup[1])
        action = PainterAction.from_specs(specs)
        if action is None:
            return None
        #TODO Should we make a fresh set of anchor_attributes?
        new_aas = frozenset(
            aa for aa in self.anchor_attributes if aa.is_match(c, su)
        )
        matchcount = len(new_aas)
        return PainterResult(self, su, action, new_aas, matchcount)
            
    def result_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[PainterResult]:
        specs: List[ActionSpec] = []
        for predicate in self.predicates:
            tup = predicate.spec_right_to_left(c, su)
            if tup is None:
                return None
            else:
                su = tup[0]
                specs.append(tup[1])
        action = PainterAction.from_specs(specs)
        if action is None:
            return None
        #TODO Should we make a fresh set of anchor_attributes?
        new_aas = frozenset(
            aa for aa in self.anchor_attributes if aa.is_match(c, su)
        )
        matchcount = len(new_aas)
        return PainterResult(self, su, action, new_aas, matchcount)
            
#WANT:
#PainterResult(self, {D=2, I=1, J=3}, PainterAction(PaintAt(3), PaintValue('a')))

    def short(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(short(p) for p in self.predicates))
        ss = veryshort(self.subst)
        aa = ', '.join(sorted(short(a) for a in self.anchor_attributes))
        return f'{cl}({ps}; {ss}; {aa})'

#Detection = Tuple[Predicate, Subst, FrozenSet[AnchorAttribute]]
@dataclass(frozen=True)
class Detection:
    predicate: Predicate
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]

DetectionResult = Union[None, Subst]

def can_be_principal_argument(x: Any) -> bool:
    match x:
        case IndexVariable():
            return True
        # TODO PainterVariable
    return False

def detections_to_painters(detections: Sequence[Detection]) \
-> Iterable[Painter]:
    for d1, d2 in combinations(detections, 2):
        if can_be_painter(d1, d2):
            yield Painter.from_detections(frozenset([d1, d2]))

def can_be_painter(d1: Detection, d2: Detection) -> bool:
    # TODO Check that both spatial and value predicates are detected?
    # TODO Check that the AnchorAttributes don't contradict each other?
#    _, su1, _ = d1
#    _, su2, _ = d2
#    d1pairs = [
#        (k, v)
#            for k, v in su1.pairs()
#                if can_be_principal_argument(k)
#    ]
#    d2pairs = [
#        (k, v)
#            for k, v in su2.pairs()
#                if can_be_principal_argument(k)
#    ]
#    return bool(
#        intersection(d1pairs, d2pairs) and su1.merge_with_unify(su2)
#    )
    d1pairs = [
        (k, v) for k, v in d1.subst.pairs() if can_be_principal_argument(k)
    ]
    d2pairs = [
        (k, v) for k, v in d2.subst.pairs() if can_be_principal_argument(k)
    ]
    return bool(
        intersection(d1pairs, d2pairs) and d1.subst.merge_with_unify(d2.subst)
    )


class ActionSpec(ABC):
    pass

@dataclass(frozen=True)
class PaintAt(ActionSpec):
    index: Index

@dataclass(frozen=True)
class PaintValue(ActionSpec):
    value: CanvasValue

@dataclass(frozen=True)
class PainterAction:
    index: Index
    value: CanvasValue
    
    @classmethod
    def from_specs(cls, specs: Sequence[ActionSpec]) \
    -> Optional[PainterAction]:
        index: Optional[Index] = None
        value: Optional[CanvasValue] = None

        for spec in specs:
            match spec:
                case PaintAt(i):
                    index = i  # TODO What if there already is an index?
                case PaintValue(v):
                    value = v  # TODO What if there already is a value?
                case _:
                    raise NotImplementedError(spec)
        if index is None or value is None:
            return None
        else:
            return PainterAction(index, value)

    def run(self, ws: Workspace) -> None:
        ws.set_value(self.index, self.value)
    
@dataclass(frozen=True)
class PainterResult:
    painter: Painter
    subst: Subst
    action: PainterAction
    anchor_attributes: FrozenSet[AnchorAttribute]
    matchcount: int

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.painter)}; {veryshort(self.subst)}, {veryshort(self.action)}; matchcount={self.matchcount})'

    def run(self, ws: Workspace) -> None:
        self.action.run(ws)

    def add_to_workspace(self, ws: Workspace) -> None:
        # TODO Update anchor_attributes
        new_painter = replace(
            self.painter,
            subst=self.subst,
            anchor_attributes=self.anchor_attributes
        )
        ws.add_painter(new_painter)

@dataclass(frozen=True)
class Workspace:
    canvas: Canvas
    painters: Set[Painter]

    @classmethod
    def make_from(cls, s: str) -> Workspace:
        return Workspace(Canvas.make_from(s), set())

    def all_indices(self) -> Iterable[Index]:
        yield from self.canvas.all_indices()

    def all_painters(self) -> Iterable[Painter]:
        yield from self.painters

    def set_value(self, i: Index, v: Optional[CanvasValue]) -> None:
        if v is None:
            pass
        else:
            self.canvas[i] = v

    def get_painter_results(self, painters: Iterable[Painter]) \
    -> Set[PainterResult]:
        results: Set[PainterResult] = set()
        for p in painters:
            for su in p.left_substs(self):
                if (result := p.result_left_to_right(self.canvas, su)):
                    results.add(result)
            for su in p.right_substs(self):
                if (result := p.result_right_to_left(self.canvas, su)):
                    results.add(result)
        return results

    def add_painter(self, painter: Painter) -> None:
        self.painters.add(painter)

    def short(self) -> str:
        result = StringIO()
        print(short(self.canvas), file=result)
        pr(self.painters, file=result)
        return result.getvalue()

predicates = [Apart(), Same(), Succ(), Pred(), Inside(), FilledWith()]
anchor_attributes: List[Type[AnchorAttribute]] = \
    [Holds, LeftmostIndex, RightmostIndex, InextremeIndex]

def make_detections(c: Canvas, painters: FrozenSet[Painter]) \
-> Iterable[Detection]:
    for predicate in predicates:
        for su in predicate.all_substs(c, painters):
            if (tu := predicate.args_ok(c, su)):
                lo(predicate, tu)
                aas = list(chain.from_iterable(
                    aa.make_for(c, tu) for aa in anchor_attributes
                ))
                if aas:
                    lo(' ', aas)
                yield Detection(predicate, tu, frozenset(aas))

def make_painters(c: Canvas, painters: FrozenSet[Painter]) \
-> FrozenSet[Painter]:
    return frozenset(detections_to_painters(list(make_detections(c, painters))))

def choose_result(results: Set[PainterResult]) -> PainterResult:
    # TODO Handle empty results
    return max(results, key=operator.attrgetter('matchcount'))

if __name__ == '__main__':
    # Make painters
    c = Canvas.make_from('ajaqb')
    painters = make_painters(c, frozenset())
    print()
    pr(painters)

    #NEXT Make p2 and p4 by cycling P and K.
    print()
    painters = make_painters(c, painters)
    print()
    pr(painters)

    # Find painters that can run
    #c = Canvas.make_from('a    ')
    ws = Workspace.make_from('a    ')
    results = ws.get_painter_results(painters)
    print()
    pr(results)

    # Run the painters
    # 0. Choose the painter.
    result = choose_result(results)
    print()
    pr(result)

    # 1. Actually run the painter in the new place.
    result.run(ws)

    # 2. Add the painter to the workspace.
    result.add_to_workspace(ws)
    print()
    pr(ws)

    # Find painters that can run; should find p2
    results = ws.get_painter_results(painters)
    print()
    pr(results)

