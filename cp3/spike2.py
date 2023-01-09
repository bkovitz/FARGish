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

from Model import Canvas, D, I, J, Subst, Variable   # from Model2.py
from Log import lo
from util import Numeric, short, veryshort


DetectionResult = Union[None, Subst]

class Predicate(ABC):

    @abstractmethod
    def args_ok(self, c: Canvas, su: Subst) -> DetectionResult:
        pass


def safe_eq(a: Optional[Numeric], b: Optional[Numeric]) -> bool:
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

# NEXT Where do we store Apart's parameters? In the class definition or
# in each object?
predicates = [Apart()]

if __name__ == '__main__':
    c = Canvas.make_from('ajaqb')
    for i, j in c.all_index_pairs():
        su = Subst.make_from((I, i), (J, j))
        for predicate in predicates:
            if (tu := predicate.args_ok(c, su)):
                lo(predicate, tu)
