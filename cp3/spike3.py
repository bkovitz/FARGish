# spike3.py -- A quick test of Predicates that return Completion objects

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, combinations

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Model import Canvas, CanvasValue, Expr, I, J, K, P, \
    VarSpec, Value, Index, \
    FizzleNoSucc, FizzleNoPred, FizzleNoValue, \
    IndexVariable, FullIndex, PainterVariable, Letter, LetterVariable, \
    Variable, \
    succ_of, pred_of
from Log import lo, trace
from util import intersection, pr, short, veryshort


# Predicate
# Predicate.loop_through_principal_variables()
# Predicate.complete_me()
# Completion
# Completion.already_complete()
# completion_to_detection()
# Workspace
# Model.from_str()

########## Substitutions ##########

@dataclass(frozen=True)
class Subst:
    '''A set of substitutions, i.e. mappings from variables to values.
    bottom_subst indicates a failed substitution.'''
    d: PMap[VarSpec, Value] = field(default_factory=lambda: pmap())

    @classmethod
    def make_from(cls, *pairs: Tuple[VarSpec, Union[Index, Painter]]) -> Subst:
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

##########

@dataclass(frozen=True)
class Painter:
    predicates: FrozenSet[Predicate]
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]
    # TODO Need left_argument and right_argument?

    @classmethod
    def from_detections(cls, detections: FrozenSet[Detection]) -> Painter:
        predicates: Set[Predicate] = set()
        subst = Subst()
        anchor_attributes: Set[AnchorAttribute] = set()
        principal_arguments: Set[Variable] = set()

        for d in detections:
            predicates.add(d.predicate)
            subst = subst.merge(d.subst)
            anchor_attributes |= d.anchor_attributes
        return cls(frozenset(predicates), subst, frozenset(anchor_attributes))

    def short(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(short(p) for p in self.predicates))
        ss = veryshort(self.subst)
        aa = ', '.join(sorted(short(a) for a in self.anchor_attributes))
        return f'{cl}({ps}; {ss}; {aa})'

@dataclass
class Workspace:
    canvas: Canvas
    painters: Set[Painter] = field(default_factory=set)

    @classmethod
    def from_str(cls, s: str) -> Workspace:
        return Workspace(canvas=Canvas.make_from(s))

    @classmethod
    def empty(cls) -> Workspace:
        return Workspace(canvas=Canvas.make_from_width(5))

    def get(self, i: Optional[Index]) -> Optional[CanvasValue]:
        return self.canvas[i]

    def has_index(self, i: Optional[Index]) -> bool:
        return self.canvas.has_index(i)
    
########## Predicates ##########

class Predicate(ABC):
    
    @abstractmethod
    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        pass

    @abstractmethod
    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        pass

    def short(self) -> str:
        return self.__class__.__name__
    
class PredicateIJ(Predicate):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for i, j in ws.canvas.all_index_pairs():  # TODO LoD
            yield Subst.make_from((I, i), (J, j))

class Apart(PredicateIJ):
    '''Hard-coded to distance 2. (No 'D' variable.)'''

    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if j - i == 2:
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Apart({su[I]}, {su[J]})')

class Same(PredicateIJ):

    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if ws.get(i) == ws.get(j):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Same({su[I]}, {su[J]})')
                
class Succ(PredicateIJ):

    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if is_succ(ws.get(i), ws.get(j)):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Succ({su[I]}, {su[J]})')
                
class Pred(PredicateIJ):

    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if is_pred(ws.get(i), ws.get(j)):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Pred({su[I]}, {su[J]})')

class Inside(Predicate):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for p in ws.painters:
            for k in ws.canvas.all_indices():
                yield Subst.make_from((P, p), (K, k))

    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        lo('CM1', su)
        match (su[P], su[K]):
            case (Painter() as p, int(k)):
                psubst = p.subst
                lo('CM2', psubst)
                match (psubst[I], psubst[J]):
                    case (int(i), int(j)):
                        if (
                            i < k and k < j
                            and
                            ws.has_index(i) and ws.has_index(j)
                        ):
                            return AlreadyComplete(self, su)
                        else:
                            return CantComplete()
            case _:
                raise NotImplementedError(
                    f'Inside({veryshort(su[P])}, {su[K]})'
                )

class Completion:
    pass

@dataclass(frozen=True)
class AlreadyComplete(Completion):
    predicate: Predicate
    subst: Subst

class CantComplete(Completion):
    pass

@dataclass(frozen=True)
class Detection:
    predicate: Predicate
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]

    def short(self) -> str:
        cl = self.__class__.__name__
        aas = ', '.join(sorted(veryshort(aa) for aa in self.anchor_attributes))
        return f'{cl}({short(self.predicate)}, {short(self.subst)}, {aas})'

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

########## Functions ##########

def safe_eq(a: Any, b: Any) -> bool:
    if a is None or b is None:
        return False
    else:
        return a == b

def is_succ(a: Any, b: Any) -> bool:
    try:
        return safe_eq(succ_of(a), b)
    except FizzleNoSucc:
        return False

def is_pred(a: Any, b: Any) -> bool:
    try:
        return safe_eq(pred_of(a), b)
    except FizzleNoPred:
        return False

def can_be_principal_argument(x: Any) -> bool:
    match x:
        case IndexVariable():
            return True
        # TODO PainterVariable
    return False

########## Model ##########

default_predicates: Collection[Predicate] = (
    Apart(), Same(), Succ(), Pred(), Inside()
)
default_anchor_attributes: Collection[Type[AnchorAttribute]] = (
    Holds, LeftmostIndex, RightmostIndex, InextremeIndex
)  # type: ignore[assignment]  # mypy 0.971 bug?

@dataclass
class Model:
    ws: Workspace = Workspace.empty()
    all_predicates: Collection[Predicate] = default_predicates
    all_anchor_attributes: Collection[Type[AnchorAttribute]] = \
        default_anchor_attributes

    @classmethod
    def from_str(cls, s: str) -> Model:
        return Model(ws=Workspace.from_str(s))

    def make_painters(self) -> FrozenSet[Painter]:
        return frozenset(
            self.detections_to_painters(list(self.make_detections()))
        )

    def make_detections(self) -> Iterable[Detection]:
        for predicate in self.all_predicates:
            for su in predicate.loop_through_principal_variables(
                self.ws
            ):
                if isinstance(
                    completion := predicate.complete_me(su, self.ws),
                    AlreadyComplete
                ):
                    #yield Detection.from_already_complete(completion, self.ws)
                    yield self.add_anchor_attributes(completion)

    def add_anchor_attributes(self, already_complete: AlreadyComplete) \
    -> Detection:
        return Detection(
            already_complete.predicate,
            already_complete.subst,
            frozenset(
                chain.from_iterable(
                    aa.make_for(self.ws.canvas, already_complete.subst)
                        for aa in self.all_anchor_attributes
                )
            )
        )

    def detections_to_painters(self, detections: Sequence[Detection]) \
    -> Iterable[Painter]:
        for d1, d2 in combinations(detections, 2):
            if self.can_be_painter(d1, d2):
                yield Painter.from_detections(frozenset([d1, d2]))

    @classmethod
    def can_be_painter(cls, d1: Detection, d2: Detection) -> bool:
        # TODO Check that both spatial and value predicates are detected?
        # TODO Check that the AnchorAttributes don't contradict each other?
        d1pairs = [
            (k, v) for k, v in d1.subst.pairs() if can_be_principal_argument(k)
        ]
        d2pairs = [
            (k, v) for k, v in d2.subst.pairs() if can_be_principal_argument(k)
        ]
        return bool(
            intersection(d1pairs, d2pairs)
            and
            d1.subst.merge_with_unify(d2.subst)
        )

    @classmethod
    def can_be_principal_argument(cls, x: Any) -> bool:
        match x:
            case IndexVariable():
                return True
            # TODO PainterVariable
        return False


########## Main ##########

if __name__ == '__main__':
    print("\n-- Making painters from 'ajaqb':")
    m = Model.from_str('ajaqb')

    painters = m.make_painters()
    print()
    pr(painters)

    m.ws.painters = set(painters)
    print("\n -- 2nd round: making painters now that p1 and p3 exist:")
    detections = list(m.make_detections())
#    painters = m.detections_to_painters(detections)
    
    print()
    pr(detections)



