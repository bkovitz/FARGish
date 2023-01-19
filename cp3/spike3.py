# spike3.py -- A quick test of Predicates that return Completion objects

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Model import Canvas, CanvasValue, Expr, I, J, VarSpec, Value, Index, \
    FizzleNoSucc, FizzleNoPred, FizzleNoValue, \
    IndexVariable, FullIndex, PainterVariable, Letter, LetterVariable, \
    succ_of, pred_of
from Log import lo, trace
from util import pr, short


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

##########

@dataclass(frozen=True)
class Painter:
    pass

@dataclass
class Workspace:
    canvas: Canvas
    #painters: Set[Painter]

    @classmethod
    def from_str(cls, s: str) -> Workspace:
        return Workspace(canvas=Canvas.make_from(s))

    @classmethod
    def empty(cls) -> Workspace:
        return Workspace(canvas=Canvas.make_from_width(5))

    def get(self, i: Optional[Index]) -> Optional[CanvasValue]:
        return self.canvas[i]
    
class Predicate(ABC):
    
    @abstractmethod
    def complete_me(self, su: Subst, ws: Workspace) -> Completion:
        pass

    @abstractmethod
    def loop_through_principal_variables(self, c: Canvas) \
    -> Iterable[Subst]:
        pass

    def short(self) -> str:
        return self.__class__.__name__
    
class PredicateIJ(Predicate):

    def loop_through_principal_variables(self, c: Canvas) \
    -> Iterable[Subst]:
        for i, j in c.all_index_pairs():
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
    #anchor_attributes: FrozenSet[AnchorAttribute]

    @classmethod
    def from_already_complete(cls, ac: AlreadyComplete, ws: Workspace) \
    -> Detection:
        # TODO Add anchor_attributes.
        return cls(ac.predicate, ac.subst)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.predicate)}, {short(self.subst)})'

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

########## Model ##########

@dataclass
class Model:
    ws: Workspace = Workspace.empty()
    all_predicates: List[Predicate] = \
        field(default_factory=lambda: [Apart(), Same(), Succ(), Pred()])

    @classmethod
    def from_str(cls, s: str) -> Model:
        return Model(ws=Workspace.from_str(s))

    def make_detections(self) -> Iterable[Detection]:
        for predicate in self.all_predicates:
            for su in predicate.loop_through_principal_variables(
                self.ws.canvas
            ):
                if isinstance(
                    completion := predicate.complete_me(su, self.ws),
                    AlreadyComplete
                ):
                    yield Detection.from_already_complete(completion, self.ws)
                    # this adds the anchor attributes

if __name__ == '__main__':
    print("\n-- Making painters from 'ajaqb':")
    m = Model.from_str('ajaqb')
    detections = m.make_detections()
    pr(detections)
#    painters = make_painters(c, frozenset())
#    print()
#    pr(painters)



