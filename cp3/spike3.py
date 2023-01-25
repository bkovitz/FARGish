# spike3.py -- A quick test of Predicates that return Completion objects
#
# Started on 18-Jan-2023

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, combinations, product as cartesian_product
from functools import cmp_to_key, reduce
from operator import attrgetter

from pyrsistent import pmap  # type: ignore[import]
from pyrsistent.typing import PMap  # type: ignore[import]

#from Model import Canvas, FullIndex, Value
from Log import indent_log, lo, trace
from util import as_list, intersection, pr, pts, short, veryshort


# Predicate
# Predicate.loop_through_principal_variables()
# Predicate.complete_me()
# Completion
# Completion.already_complete()
# completion_to_detection()
# Workspace
# Model.from_str()

########## Indices and variables ##########

Index = int
ArgumentPlace = Literal['LeftArgument', 'RightArgument', 'NotPrincipal']

@dataclass(frozen=True)
class Variable(ABC):
    name: str
    place: ArgumentPlace

    @abstractmethod
    def make_loop(self, ws: Workspace) -> Iterable[Subst]:
        pass

    def __repr__(self) -> str:
        return self.name

    def short(self) -> str:
        return self.name

@dataclass(frozen=True)
class IndexVariable(Variable):

    def make_loop(self, ws: Workspace) -> Iterable[Subst]:
        return (Subst.make_from((self, i)) for i in ws.canvas.all_indices())

    def __repr__(self) -> str:
        return self.name

@dataclass(frozen=True)
class PainterVariable(Variable):

    def make_loop(self, ws: Workspace) -> Iterable[Subst]:
        return (Subst.make_from((self, p)) for p in ws.painters)

    def __repr__(self) -> str:
        return self.name

@dataclass(frozen=True)
class LetterVariable(Variable):

    def make_loop(self, ws: Workspace) -> Iterable[Subst]:
        return ()

    def __repr__(self) -> str:
        return self.name

#D = Variable('D', 'NotPrincipal')  # "distance", in Apart
I = IndexVariable('I', 'LeftArgument')
J = IndexVariable('J', 'RightArgument')
K = IndexVariable('K', 'RightArgument')
P = PainterVariable('P', 'LeftArgument')
Q = PainterVariable('Q', 'LeftArgument')
L = LetterVariable('L', 'NotPrincipal')

class Vars:
    '''This class exists not to be instantiated but to provide a holder
    for Variables so they can be referred to in match-case statements.'''
    I = I
    J = J

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
    def make_from(
        cls, *pairs: Tuple[Variable, Union[Index, Painter, Letter]]
    ) -> Subst:
        result = cls()
        for lhs, rhs in pairs:
            result = result.unify(lhs, rhs)
        return result

    @classmethod
    def fill_from(cls, su: Subst, variables: Iterable[Variable]) -> Subst:
        '''Returns a new Subst, with a definition for each variable in
        'variables', with values taken from 'su'.'''
        result = Subst()
        for variable in variables:
            result = result.unify(variable, su[variable])
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
        return ', '.join(sorted(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        ))

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

SubstLoop = Iterable[Subst]

########## Actions ##########

class Action:
    pass

@dataclass(frozen=True)
class Paint(Action):
    index: Index
    value: CanvasValue

@dataclass(frozen=True)
class MakePainter(Action):
    predicates: FrozenSet[Predicate]
    subst: Subst

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.predicates)}, {veryshort(self.subst)})'

########## Span ##########

@dataclass(frozen=True)
class RealSpan:
    left_boundary: Index
    right_boundary: Index

    @classmethod
    def are_consecutive(cls, span1: Span, span2: Span) -> bool:
        match (span1, span2):
            case (RealSpan() as s1, RealSpan() as s2):
                return (
                    s1.right_boundary == s2.left_boundary
                    or
                    s2.right_boundary + 1 == s2.left_boundary
                )
            case _:
                return False

    def union(self, other: Span) -> Span:
        match other:
            case RealSpan():
                return RealSpan(
                    min(self.left_boundary, other.left_boundary),
                    max(self.right_boundary, other.right_boundary)
                )
            case _:
                return self

class NullSpan:

    def union(self, other: Span) -> Span:
        return other

Span = Union[RealSpan, NullSpan]

########## Painter ##########

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

    @classmethod
    def have_same_spatial_relation(cls, p: Painter, q: Painter) -> bool:
        psr = p.get_spatial_relations()
        qsr = q.get_spatial_relations()
        return (
            bool(psr) and bool(qsr)
            and
            p.get_spatial_relations() == q.get_spatial_relations()
        )
#        p_relation = p.get_spatial_relation()
#        q_relation = q.get_spatial_relation()
#        return (
#            p_relation == q_relation
#            and
#            p.subst.only(p_relation.my_vars) == q.subst.only(q_relation.my_vars)
#        )

    def span(self) -> Span:
        result: Span = NullSpan()
        for predicate in self.predicates:
            result = result.union(predicate.span(self.subst))
        return result

#    def actions(self, ws: Workspace) -> Iterable[PainterAction]:
#        for su in self.loop_through_left_variable():
#
#        for su in self.loop_through_right_variable():
#            
#        # Then loop through the completions
            
    #def completions_to_actions(self, 

    def get_spatial_relations(self) -> FrozenSet[SpatialRelation]:
        return frozenset(
            predicate
                for predicate in self.predicates
                    if (
                        isinstance(predicate, SpatialRelation)
                        and
                        not isinstance(predicate, Apart)  # HACK
                    )
        )

    def su_to_actions(self, su: Subst, ws: Workspace) -> Iterable[Action]:
        yield from self.completions_to_actions(
            su,
            ws,
            self.su_to_completions(su, ws)
        )

    def su_to_completions(self, su: Subst, ws: Workspace) \
    -> Collection[Completion]:
        completions: List[Completion] = []
        for predicate in self.predicates:
            completion = predicate.complete_me(self, su, ws)
            if isinstance(completion, CantComplete):
                return []
            #completions.append(completion)
            completions += as_list(completion)
        return completions

    def evaled_completions_to_action(
        self, completions: Iterable[Completion], ws: Workspace
    ) -> Iterable[Action]:
        index: Optional[Index] = None
        value: Optional[CanvasValue] = None

        for completion in completions:
            match completion:
                case PaintAt(i):
                    index = i
                case PaintValue(v):
                    value = v
                case AlreadyComplete():
                    continue
                case MaybeMakePainter():
                    yield from completion.make_action(ws)
                case _:
                    raise NotImplementedError(
                        f'completions_to_actions: {completion}'
                    )
        if index is not None and value is not None:
            yield Paint(index, value)

    def completions_to_actions(
        self, start_su: Subst, ws: Workspace, completions: Iterable[Completion]
    ) -> Iterable[Action]:
        loop_objects, condition_objects, action_elements = \
            self.prepare_completion_loop(completions)
        for su in self.loopvars_to_substs(start_su, ws, loop_objects):
            if all(
                condition.eval(su).passes()
                    for condition in condition_objects
            ):
                yield from self.evaled_completions_to_action(
                    (action_element.eval(su)
                        for action_element in action_elements),
                    ws
                )

    def prepare_completion_loop(self, completions: Iterable[Completion]) \
    -> Tuple[
        Collection[LoopVariable],
        Collection[ConditionObject],
        Collection[ActionElement]
    ]:
        loop_objects: List[LoopVariable] = []
        condition_objects: List[ConditionObject] = []
        action_elements: List[ActionElement] = []

        for completion in completions:
            match completion:
                case LoopVariable():
                    loop_objects.append(completion)
                case ConditionObject():
                    condition_objects.append(completion)
                case ActionElement():
                    action_elements.append(completion)
                case _:
                    raise NotImplementedError(
                        f'prepare_completion_loop {completion}'
                    )
        return (
            sorted(loop_objects, key=attrgetter('name')),
            condition_objects,
            action_elements
        )

    def loopvars_to_substs(
        self,
        start_su: Subst,
        ws: Workspace,
        loop_objects: Collection[LoopVariable]
    ) -> Iterable[Subst]:
        # TODO If no loop_objects, run once with an empty Subst
        for tup in cartesian_product(*self.loopvars_to_loops(ws, loop_objects)):
            yield reduce(lambda su, tu: su.merge(tu), (start_su,) + tup)

    def loopvars_to_loops(
        self, ws: Workspace, loopvars: Collection[LoopVariable], subst_loops=[]
    ) -> List[SubstLoop]:
        match loopvars:
            case []:
                return subst_loops
            case [LoopVariable(Vars.I), LoopVariable(Vars.J), *more]:
                return self.loopvars_to_loops(
                    ws, more, subst_loops + [self.make_ij_loop(ws)]
                )
            case [LoopVariable() as lv, *more]:
                return self.loopvars_to_loops(
                    ws, more, subst_loops + [lv.make_loop(ws)]
                )
            case _:
                raise NotImplementedError(f'loopvars_to_loops {loopvars}')

    def make_ij_loop(self, ws) -> SubstLoop:
        for i, j in ws.canvas.all_index_pairs():
            yield Subst.make_from((I, i), (J, j))

    def short(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(short(p) for p in self.predicates))
        ss = veryshort(self.subst)
        aa = ', '.join(sorted(short(a) for a in self.anchor_attributes))
        return f'{cl}({ps}; {ss}; {aa})'

    __str__ = short

    def veryshort(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(veryshort(p) for p in self.predicates))
        ss = veryshort(self.subst)
        return f'{cl}({ps}, {ss})'

Value = Union[Index, Painter]  # TODO Union[Index, Painter, Canvas]?

########## Predicates ##########

@dataclass(frozen=True)
class Predicate(ABC):
    my_vars: ClassVar[Tuple[Variable]]
    
    @abstractmethod
    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) \
    -> Completions:
        pass

    def passes(self, owner: Optional[Painter], su: Subst, ws: Workspace) \
    -> bool:
        return isinstance(self.complete_me(owner, su, ws), AlreadyComplete)

    @abstractmethod
    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        pass

    @abstractmethod
    def span(self, su: Subst) -> Span:
        pass

    def short(self) -> str:
        return self.__class__.__name__
    
class SpatialRelation(Predicate):
    pass

class ValueRelation(Predicate):
    pass

class PainterRelation(Predicate):
    pass

class PredicateIJ(Predicate):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for i, j in ws.canvas.all_index_pairs():  # TODO LoD
            yield Subst.make_from((I, i), (J, j))

    def span(self, su: Subst) -> Span:
        match (su[I], su[J]):
            case (int(i), int(j)):
                return RealSpan(i, j)
            case _:
                return NullSpan()

class PredicatePQ(Predicate):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for p, q in ws.all_painter_pairs():
            yield Subst.make_from((P, p), (Q, q))

    def span(self, su: Subst) -> Span:
        match (su[P], su[Q]):
            case (Painter() as p, Painter() as q):
                return p.span().union(q.span())
            case _:
                return NullSpan()

class Apart(PredicateIJ, SpatialRelation):
    '''Hard-coded to distance 2. (No 'D' variable.)'''
    d: ClassVar[Index] = 2

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) \
    -> Completions:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if j - i == self.d:
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case (int(i), None):
                j = i + self.d
                if ws.has_index(j):
                    return PaintAt(j)
                else:
                    return CantComplete()
            case (None, int(j)):
                i = j - self.d
                if ws.has_index(i):
                    return PaintAt(i)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Apart({su[I]}, {su[J]})')

class Same(PredicateIJ, ValueRelation):

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) -> Completions:
        match (su[I], su[J]):
            case (int(i), int(j)):
                return self.value_completion(su, ws, i, j)
            case (int(i), None):
                match ws.get(i):
                    case Letter() as l:
                        return PaintValue(l)
                    case _:
                        return CantComplete()
            case (None, int(j)):
                match ws.get(j):
                    case Letter() as l:
                        return PaintValue(l)
                    case _:
                        return CantComplete()
            case _:
                raise NotImplementedError(f'Same({su[I]}, {su[J]})')

    def value_completion(
        self, su: Subst, ws: Workspace, i: Index, j: Index
    ) -> Completion:
        '''Returns completion for case where both I and J contain
        indices.'''
        match (ws.get(i), ws.get(j)):
            case (Letter() as li, Letter() as lj):
                if li == lj:
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case (Letter() as li, Blank()):
                return PaintValueAt(j, li)
            case (Blank(), Letter() as lj):
                return PaintValueAt(i, lj)
            case _:
                return CantComplete()
                
class Succ(PredicateIJ, ValueRelation):

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) -> Completions:
        match (su[I], su[J]):
            case (int(i), int(j)):
                return self.value_completion(su, ws, i, j)
            case (int(i), None):
                try:
                    return PaintValue(succ_of(ws.get(i)))
                except FizzleNoSucc:
                    return CantComplete()
            case (None, int(j)):
                try:
                    return PaintValue(pred_of(ws.get(j)))
                except FizzleNoPred:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Succ({su[I]}, {su[J]})')
                
    def value_completion(
        self, su: Subst, ws: Workspace, i: Index, j: Index
    ) -> Completion:
        match (ws.get(i), ws.get(j)):
            case (Letter() as li, Letter() as lj):
                if is_succ(li, lj):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case (Letter() as li, Blank() as lj):
                try:
                    return PaintValueAt(j, succ_of(li))
                except FizzleNoSucc:
                    return CantComplete()
            case (Blank(), Letter() as lj):
                try:
                    return PaintValueAt(i, pred_of(lj))
                except FizzleNoPred:
                    return CantComplete()

            case _:
                return CantComplete()

class Pred(PredicateIJ, ValueRelation):

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) -> Completions:
        match (su[I], su[J]):
            case (int(i), int(j)):
                if is_pred(ws.get(i), ws.get(j)):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(f'Pred({su[I]}, {su[J]})')

class Inside(SpatialRelation):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for p in ws.painters:
            for k in ws.canvas.all_indices():
                yield Subst.make_from((P, p), (K, k))

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) -> Completions:
        match (su[P], su[K]):
            case (Painter() as p, int(k)):
                psubst = p.subst
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
                        return CantComplete()
            case (Painter() as p, None):
                psubst = p.subst
                match (psubst[I], psubst[J]):
                    case (int(i), int(j)):
                        return [LoopVariable(K), LessThan(i, K), LessThan(K, j)]
                    case _:
                        return CantComplete()
            case (None, int(k)):  # Given K, deduce P
                # TODO Programmatically determine which variables to loop through?
                match owner:
                    case Painter():
                        match owner.subst[P]:  # TODO LoD
                            case Painter() as p:
                                return [
                                    LoopVariable(I),
                                    LoopVariable(J),
                                    LessThan(I, k),
                                    LessThan(k, J),
                                    MaybeMakePainter(p, [I, J])
                                ]
                            case _:
                                return CantComplete()
                    case None:
                        return CantComplete()
            case _:
                raise NotImplementedError(
                    f'Inside({veryshort(su[P])}, {su[K]})'
                )

    def span(self, su: Subst) -> Span:
        match su[P]:
            case Painter() as p:
                return p.span()
            case _:
                return NullSpan()

class FilledWith(ValueRelation):

    def loop_through_principal_variables(self, ws: Workspace) \
    -> Iterable[Subst]:
        for k in ws.canvas.all_indices():   # TODO LoD
            yield Subst.make_from((K, k))

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) -> Completions:
        match (su[K], su[L]):
            case (int(k), Letter() as l):
                if ws.get(k) == l:
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case (int(k), None):
                match letter := ws.get(k):
                    case Letter():
                        # TODO What if .unify() returns BottomSubst?
                        return AlreadyComplete(self, su.unify(L, letter))
                    case _:
                        return CantComplete()
            case (None, Letter() as l):
                return [PaintAt(K), PaintValue(l)]
            case _:
                raise NotImplementedError(f'FilledWith({su[K]}, {su[L]})')

    def span(self, su: Subst) -> Span:
        match su[K]:
            case int(k):
                return RealSpan(k, k)
            case _:
                return NullSpan()

class SameSpatialRelation(PainterRelation, PredicatePQ):

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) \
    -> Completions:
        match (su[P], su[Q]):
            case (Painter() as p, Painter() as q):
                if Painter.have_same_spatial_relation(p, q):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(
                    f'SameSpatialRelation({su[P]}, {su[Q]}'
                )

class ConsecutiveLocations(PainterRelation, PredicatePQ):

    def complete_me(self, owner: Optional[Painter], su: Subst, ws: Workspace) \
    -> Completions:
        match (su[P], su[Q]):
            case (Painter() as p, Painter() as q):
                if RealSpan.are_consecutive(p.span(), q.span()):
                    return AlreadyComplete(self, su)
                else:
                    return CantComplete()
            case _:
                raise NotImplementedError(
                    f'ConsecutiveLocations({su[P]}, {su[Q]})'
                )

########## Completions ##########

class Completion:

    def eval(self, su: Subst) -> Completion:
        '''Returns a Completion that contains only constants, no
        variables. Default implementation returns self; override if the
        Completion accepts variables.'''
        return self

    def passes(self) -> bool:
        '''Only relevant for ConditionObjects: Do the member fields
        satisfy the condition of the ConditionObject?'''
        return True

Completions = Union[Completion, List[Completion]]

class ActionElement(Completion):
    '''A Completion that specifies part or all of an action to perform on
    the workspace.'''
    pass

#TODO DeterminateActionElement?

class ConditionObject(Completion, ABC):
    '''A Completion that specifies a condition that must be satisfied
    in order to perform the ActionElements in a loop iteration.'''

    @abstractmethod
    def passes(self) -> bool:
        pass

@dataclass(frozen=True)
class AlreadyComplete(ActionElement):
    predicate: Predicate
    subst: Subst

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.predicate)}, {veryshort(self.subst)})'

class CantComplete(ActionElement):

    def __str__(self) -> str:
        return self.__class__.__name__

@dataclass(frozen=True)
class PaintAt(ActionElement):
    index: Index

    def eval(self, su: Subst) -> Completion:
        match self.index:
            case int():
                return self
            case Variable() as var:
                return PaintAt(su[var])

@dataclass(frozen=True)
class PaintValue(ActionElement):
    value: CanvasValue  # TODO eval so that a Variable can go here

@dataclass(frozen=True)
class PaintValueAt(ActionElement):
    index: Index
    value: CanvasValue

@dataclass(frozen=True)
class LoopVariable(Completion):
    var: Variable

    @property
    def name(self) -> str:
        return self.var.name

    def make_loop(self, ws: Workspace) -> Iterable[Subst]:
        return self.var.make_loop(ws)

@dataclass(frozen=True)
class LessThan(ConditionObject):
    a: Union[Index, Variable]
    b: Union[Index, Variable]

    def eval(self, su: Subst) -> LessThan:
        # TODO Address possibility of failure, i.e. a variable not defined in su
        match (self.a, self.b):
            case (int(), int()):
                return self
            case (int(), Variable() as b):
                return LessThan(self.a, su[b])  # type: ignore[arg-type]
            case (Variable() as a, int()):
                return LessThan(su[a], self.b)  # type: ignore[arg-type]
            case (Variable() as a, Variable() as b):
                return LessThan(su[a], su[b])  # type: ignore[arg-type]
            case _:
                raise ValueError('LessThan.eval {self.a} {self.b}')

    def passes(self) -> bool:
        if isinstance(self.a, int) and isinstance(self.b, int):
            return self.a < self.b
        else:
            raise ValueError(f'{self}: Variables must be replaced with constants before calling passes().')

@dataclass(frozen=True)
class MaybeMakePainter(ActionElement):
    template_painter: Painter
    variables: Collection[Variable]
    subst: Optional[Subst] = None  # This will override template_painter's Subst

    def eval(self, su: Subst) -> MaybeMakePainter:
        return replace(self, subst=Subst.fill_from(su, self.variables))

    def make_action(self, ws) -> Iterable[MakePainter]:
        '''Yields a MakePainter if the resulting Painter's predicates would
        all pass. Otherwise yields nothing.'''
        assert self.subst is not None
        predicates = self.template_painter.predicates
        subst = self.template_painter.subst.merge(self.subst)
        if all(predicate.passes(None, subst, ws) for predicate in predicates):
            yield MakePainter(predicates, subst)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.template_painter)}, {veryshort(self.variables)}, {veryshort(self.subst)})'

    __str__ = short

########## Detection ##########

@dataclass(frozen=True)
class Detection:
    '''A Detection represents having noticed that objects in the Workspace
    (cells and/or Painters) meet the condition of a Predicate.'''
    predicate: Predicate
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]

    @classmethod
    def has_spatial_and_value_relations(cls, d1: Detection, d2: Detection) \
    -> bool:
        return (
            isinstance(d1.predicate, SpatialRelation)
            and
            isinstance(d2.predicate, ValueRelation)
        ) or (
            isinstance(d1.predicate, ValueRelation)
            and
            isinstance(d2.predicate, SpatialRelation)
        )

    @classmethod
    def both_painter_relations(cls, d1: Detection, d2: Detection) \
    -> bool:
        return (
            isinstance(d1.predicate, PainterRelation)
            and
            isinstance(d2.predicate, PainterRelation)
        )
 
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

#def can_be_principal_argument(x: Any) -> bool:
#    match x:
#        case IndexVariable():
#            return True
#        # TODO PainterVariable
#    return False

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

########## The canvas ##########

@dataclass
class Canvas:
    '''Canvas cells have 1-based indexing.'''
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

    def has_index(self, x: Any) -> bool:
        match x:
            case int(i):
                if self.min_index is None:
                    return i in self.d
                else:
                    assert self.max_index is not None
                    return i >= self.min_index and i <= self.max_index
            case _:
                return False

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

########## Workspace and Model ##########

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

    def all_painter_pairs(self) -> Iterable[Tuple[Painter, Painter]]:
        # TODO Don't allow mirror-image duplicates, e.g. (p1, p2) and (p2, p1)?
        for p in self.painters:
            for q in self.painters:
                if p != q:
                    yield (p, q)

    
default_predicates: Collection[Predicate] = (
    Apart(), Same(), Succ(), Pred(), Inside(), FilledWith(),
    SameSpatialRelation(), ConsecutiveLocations()
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

#NEXT
#    def try_painters(self) -> FrozenSet[Painter]:
#        return frozenset(chain.from_iterable(
#            p.actions(self.ws) for p in self.ws.painters
#        ))

    ### Ancillary methods ###

    def make_detections(self) -> Iterable[Detection]:
        for predicate in self.all_predicates:
            for su in predicate.loop_through_principal_variables(
                self.ws
            ):
                if isinstance(
                    completion := predicate.complete_me(None, su, self.ws),
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
        if (
            not Detection.has_spatial_and_value_relations(d1, d2)
            and
            not Detection.both_painter_relations(d1, d2)
        ):
            return False
        d1pairs = [
            (k, v) for k, v in d1.subst.pairs()
                       if cls.can_be_principal_argument(k)
        ]
        d2pairs = [
            (k, v) for k, v in d2.subst.pairs()
                       if cls.can_be_principal_argument(k)
        ]
        return bool(
            intersection(d1pairs, d2pairs)
            and
            d1.subst.merge_with_unify(d2.subst)
        )

    @classmethod
    def can_be_principal_argument(cls, x: Any) -> bool:
        return isinstance(x, (IndexVariable, PainterVariable))

########## Main ##########

if __name__ == '__main__':
    print("\n-- Making painters from 'ajaqb':")
    m = Model.from_str('ajaqb')

    painters = m.make_painters()  # These should go into LTM
    print()
    pr(painters)

    m.ws.painters = set(painters)  # Unrealistic: during normal run,
        # painters don't get added to the Workspace until they're run.
    print("\n -- 2nd round: making painters now that p1 and p3 exist:")
    print()
    painters = list(sorted(m.make_painters(), key=str))
    pts(painters)

    #print("\n -- Regenerate from 'a____'")
    print("\n -- Generate completions and actions for p3")
    print()
    p = painters[1]
    lo('PAINTER', p)
    # We're skipping generating the su
    cs = p.su_to_completions(Subst.make_from((I, 1)), m.ws)
    print()
    pts(cs)
    print()
    actions = p.completions_to_actions(Subst(), m.ws, cs)
    pts(actions)

    print("\n -- Generate completions and actions for p2, given P=p1(1, 3)")
    print()
    p1 = painters[0]
    p2 = painters[2]
    p4 = painters[3]
    lo('PAINTER', p2)
    cs = p2.su_to_completions(Subst.make_from((P, p1), (L, Letter('j'))), m.ws)
    print()
    pts(cs)
    print()
    actions = list(p.completions_to_actions(Subst(), m.ws, cs))
    pts(actions)

    print("\n -- Generate completions for p2, given K=2")
    print()
    cs = p2.su_to_completions(Subst.make_from((K, 2), (L, Letter('j'))), m.ws)
    # WANT
    # LoopVariable(I)
    # LoopVariable(J)
    # LessThan(I, K)
    # LessThan(K, J)
    # MaybeMakePainter(p1, Subst('I=I, 'J=J)
    pts(cs)

    # Generate actions
    print()
    actions = list(p2.completions_to_actions(Subst(), m.ws, cs))
    pts(actions)
    # WANT
    # MakePainter(p1, Subst((I, 1), (J, 3)))

    #print("\n -- Detect the relationship to be embodied by p5")
    #print()
    #es = as_list(SameSpatialRelation().complete_me(None, Subst.make_from((P, p2), (Q, p4)), m.ws))
    #es = as_list(ConsecutiveLocations().complete_me(None, Subst.make_from((P, p2), (Q, p4)), m.ws))
    #pts(es)

    print("\n -- Detect the relationship between p2 and p4\n")
    m.ws.painters = set(painters)
    #detections = list(sorted(m.make_detections(), key=str))
    #pts(detections)
    painters = list(sorted(m.make_painters(), key=str))
    pts(painters)
