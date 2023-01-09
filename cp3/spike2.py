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
from itertools import chain
from collections import defaultdict

from Model import Canvas, D, Fizzle, I, Index, IndexVariable, J, Letter, \
    pred_of, Subst, succ_of, Value, Variable, VarSpec, Workspace
    # from Model2.py
from Log import lo
from util import Numeric, pr, short, veryshort

# TODO Pass Workspace, not Canvas. Or maybe pass UState.

##### Predicates #####

DetectionResult = Union[None, Subst]

class Predicate(ABC):

    @abstractmethod
    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        pass


def safe_eq(a: Any, b: Any) -> bool:
    if a is None or b is None:
        return False
    else:
        return a == b

def safe_sub(a: Optional[Numeric], b: Optional[Numeric]) -> Optional[Numeric]:
    if a is None or b is None:
        return None
    else:
        return a - b

@dataclass(frozen=True)
class Apart(Predicate):
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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({veryshort(self.default_subst)})'

@dataclass(frozen=True)
class Same(Predicate):

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        if safe_eq(c[su[I]], c[su[J]]):
            return su
        else:
            return None

@dataclass(frozen=True)
class Succ(Predicate):

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(succ_of(c[su[I]]), c[su[J]]):
                return su
            else:
                return None
        except Fizzle:
            return None

@dataclass(frozen=True)
class Pred(Predicate):
    '''Predicate for 'predecessor'.'''

    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        try:
            if safe_eq(pred_of(c[su[I]]), c[su[J]]):
                return su
            else:
                return None
        except Fizzle:
            return None

##### AnchorAttributes #####

class AnchorAttribute(ABC):

    @classmethod
    @abstractmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        pass

@dataclass(frozen=True)
class Holds(AnchorAttribute):
    i: IndexVariable
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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.letter)})'

@dataclass(frozen=True)
class LeftmostIndex(AnchorAttribute):
    i: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if su[k] == 1:
                        yield LeftmostIndex(k)
                case _:
                    pass

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)})'

@dataclass(frozen=True)
class RightmostIndex(AnchorAttribute):
    i: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if safe_eq(su[k], c.max_index):
                        yield RightmostIndex(k)
                case _:
                    pass

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)})'


@dataclass(frozen=True)
class InextremeIndex(AnchorAttribute):
    i: IndexVariable

    @classmethod
    def make_for(cls, c: Canvas, su: Subst) -> Iterable[AnchorAttribute]:
        for k, v in su.pairs():
            match k:
                case IndexVariable():
                    if not cls.is_inextreme(c, v):
                        yield InextremeIndex(k)
                case _:
                    pass

    @classmethod
    def is_inextreme(cls, c: Canvas, i: Index) -> bool:
        if c.max_index is None:
            return False
        return i != 1 and i != c.max_index

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)})'

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

    def short(self) -> str:
        cl = self.__class__.__name__
        ps = ', '.join(sorted(short(p) for p in self.predicates))
        ss = veryshort(self.subst)
        aa = ', '.join(sorted(short(a) for a in self.anchor_attributes))
        return f'{cl}({ps}; {ss}; {aa})'

predicates = [Apart(), Same(), Succ(), Pred()]
anchor_attributes: List[Type[AnchorAttribute]] = \
    [Holds, LeftmostIndex, RightmostIndex, InextremeIndex]

Detection = Tuple[Predicate, Subst, FrozenSet[AnchorAttribute]]

def can_be_principal_argument(x: Any) -> bool:
    match x:
        case IndexVariable():
            return True
        # TODO PainterVariable
    return False

def var_value_pairs(detections: Sequence[Detection]) \
-> FrozenSet[Tuple[VarSpec, Value]]:
    '''Includes only variables that can be principal arguments, i.e.
    IndexVariables and Painters.'''
    return frozenset(
        (var, val)
            for d in detections
                for var, val in d[1].pairs()
                    if can_be_principal_argument(var)
    )

def overlaps_to_painters(detections: Sequence[Detection]) -> FrozenSet[Painter]:
    d: Dict[Tuple[VarSpec, Value], Set[Detection]] = defaultdict(
        set,
        ((var, set()) for var in var_value_pairs(detections))
    )
    for detection in detections:
        for k, v in detection[1].pairs():
            if can_be_principal_argument(k):
                d[(k, v)].add(detection)
    print()
    for kk, vv in d.items():
        lo(f'{kk}: {len(vv)}  {vv}')
    print()
    # NEXT Reject combinations of Detections with incompatible values for
    # the same variable or incompatible AnchorAttributes.
    # Subst.merge_and_unify()
    return frozenset(
        Painter.from_detections(frozenset(detections))
            for detections in d.values()
                if len(detections) >= 2
    )

if __name__ == '__main__':
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
    painters = overlaps_to_painters(found)
    pr(painters)

    print()
    pr(var_value_pairs(found))
