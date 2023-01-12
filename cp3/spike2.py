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

from Model import Canvas, CanvasValue, D, Fizzle, FizzleNoPred, FizzleNoSucc, \
    I, Index, IndexVariable, \
    J, Letter, pred_of, Subst, succ_of, Value, Variable, VarSpec, Workspace
    # from Model2.py
from Log import lo, trace
from util import intersection, Numeric, pr, short, veryshort

# NEXT Rewrite succ_of to return None if it fails.

# TODO Pass Workspace, not Canvas. Or maybe pass UState.

##### Predicates #####

DetectionResult = Union[None, Subst]

class Predicate(ABC):

    @abstractmethod
    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        pass

    @abstractmethod
    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        pass

    @abstractmethod
    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
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

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        su = self.default_subst.merge(su)
        if (
            c.has_index(su[I])
            and
            c.has_index(su[J])
            and
            safe_eq(su[D], safe_sub(su[J], su[I]))
        ):
            return su
        else:
            return None

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        j = safe_add(su[I], su[D])
        if not c.has_index(j):
            return None
        else:
            assert j is not None
            return (su.unify(J, j), PaintAt(j))

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        i = safe_sub(su[J], su[D])
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

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        if safe_eq(c[su[I]], c[su[J]]):
            return su
        else:
            return None

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        if isinstance(letter := c[su[I]], Letter):
            return (su, PaintValue(letter))
        else:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        if isinstance(letter := c[su[J]], Letter):
            return (su, PaintValue(letter))
        else:
            return None

@dataclass(frozen=True)
class Succ(Predicate):
    '''Succ[I, J]'''

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(succ_of(c[su[I]]), c[su[J]]):
                return su
            else:
                return None
        except Fizzle:
            return None

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := succ_of(c[su[I]]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoSucc:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := pred_of(c[su[J]]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoPred:
            return None

@dataclass(frozen=True)
class Pred(Predicate):
    '''Pred[I, J], i.e. the value at I must be the predecessor of the value
    at J.'''

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(pred_of(c[su[I]]), c[su[J]]):
                return su
            else:
                return None
        except Fizzle:
            return None

    def spec_left_to_right(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := pred_of(c[su[I]]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoPred:
            return None

    def spec_right_to_left(self, c: Canvas, su: Subst) \
    -> Optional[Tuple[Subst, ActionSpec]]:
        try:
            if isinstance(letter := succ_of(c[su[J]]), Letter):
                return (su, PaintValue(letter))
            else:
                return None
        except FizzleNoSucc:
            return None

#@dataclass(frozen=True)
#class Inside(Predicate):
#    '''Inside[P, K]'''
#    
#    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
#        match (su[P], su[K]):
#            case (Painter(_, psubst, _), int(k)):
#                match (psubst[I], psubst[J]):
#                    case (int(i), int(j)):
#                        return (
#                            i < k and k < j
#                            and
#                            c.has_index(i) and c.has_index(j)
#                        )
#                    case _:
#                        return None
#            case _:
#                return None
#
#    def spec_left_to_right(self, c: Canvas, su: Subst) \
#    -> Optional[Tuple[Subst, ActionSpec]]:
#        pass
#
#    def spec_right_to_left(self, c: Canvas, su: Subst) \
#    -> Optional[Tuple[Subst, ActionSpec]]:
#        pass

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
                    if isinstance(letter := c[su[k]], Letter):
                        yield Holds(k, letter)
                case _:
                    pass

    def is_match(self, c: Canvas, su: Subst) -> bool:
        return c[su[self.var]] == self.letter

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
    def is_inextreme(cls, c: Canvas, i: Index) -> bool:
        if c.max_index is None:
            return False
        return i != 1 and i != c.max_index

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
    #principal_arguments: Tuple[VarSpec, VarSpec]
    predicates: FrozenSet[Predicate]
    subst: Subst
    anchor_attributes: FrozenSet[AnchorAttribute]

    @classmethod
    def from_detections(cls, detections: FrozenSet[Detection]) -> Painter:
        predicates: Set[Predicate] = set()
        subst = Subst()
        anchor_attributes: Set[AnchorAttribute] = set()

        for detection in detections:
            predicate, su, aas = detection
            predicates.add(predicate)
            subst = subst.merge(su)
            anchor_attributes |= aas
        return Painter(
            frozenset(predicates),
            subst,
            frozenset(anchor_attributes)
        )

    def left_substs(self, c: Canvas) -> Iterable[Subst]:
        '''Clears J and steps I through all indices in canvas.'''
        su = self.subst.remove(I, J)
        for i in c.all_indices():
            yield su.unify(I, i)

    def right_substs(self, c: Canvas) -> Iterable[Subst]:
        su = self.subst.remove(I, J)
        for j in c.all_indices():
            yield su.unify(J, j)

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
        matchcount = sum(
            1 for aa in self.anchor_attributes if aa.is_match(c, su)
        )
        return PainterResult(self, su, action, matchcount)
            
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
        matchcount = sum(
            1 for aa in self.anchor_attributes if aa.is_match(c, su)
        )
        return PainterResult(self, su, action, matchcount)
            
#WANT:
#PainterResult(self, {D=2, I=1, J=3}, PainterAction(PaintAt(3), PaintValue('a')))

    def short(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(short(p) for p in self.predicates))
        ss = veryshort(self.subst)
        aa = ', '.join(sorted(short(a) for a in self.anchor_attributes))
        return f'{cl}({ps}; {ss}; {aa})'

Detection = Tuple[Predicate, Subst, FrozenSet[AnchorAttribute]]

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
    _, su1, _ = d1
    _, su2, _ = d2
    d1pairs = [
        (k, v)
            for k, v in su1.pairs()
                if can_be_principal_argument(k)
    ]
    d2pairs = [
        (k, v)
            for k, v in su2.pairs()
                if can_be_principal_argument(k)
    ]
    return bool(
        intersection(d1pairs, d2pairs) and su1.merge_with_unify(su2)
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
    
@dataclass(frozen=True)
class PainterResult:
    painter: Painter
    subst: Subst
    action: PainterAction
    matchcount: int

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.painter)}; {veryshort(self.subst)}, {veryshort(self.action)}; matchcount={self.matchcount})'


predicates = [Apart(), Same(), Succ(), Pred()]
anchor_attributes: List[Type[AnchorAttribute]] = \
    [Holds, LeftmostIndex, RightmostIndex, InextremeIndex]

if __name__ == '__main__':
    # Make painters
    c = Canvas.make_from('ajaqb')
    found: List[Detection] = []
    for predicate in predicates:
        for i, j in c.all_index_pairs():
            su = Subst.make_from((I, i), (J, j))
            if (tu := predicate.args_ok(c, su)):
                lo(predicate, tu)
                aas = list(chain.from_iterable(
                    aa.make_for(c, tu) for aa in anchor_attributes
                ))
                if aas:
                    lo(' ', aas)
                found.append((predicate, tu, frozenset(aas)))
    print()
    painters = list(detections_to_painters(found))
    pr(painters)


    # Find painters that can run
    c = Canvas.make_from('a    ')
    results: List[PainterResult] = []
    for p in painters:
        for su in p.left_substs(c):
            if (result := p.result_left_to_right(c, su)):
                results.append(result)
        for su in p.right_substs(c):
            if (result := p.result_right_to_left(c, su)):
                results.append(result)
    print()
    pr(results)
    # WANT  p1's matchcount should be 3
    #       p3's matchcount should be 1

    #NEXT Make p2 and p4 by cycling P and K.
    # How do we get P and K?
    # 

# When creating a painter, how do we decide which arguments are the principal
# ones, and how do we decide which is right and which is left?
