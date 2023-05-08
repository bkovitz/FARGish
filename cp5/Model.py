# Model.py -- Canvas-and-painters model with rewrite rules

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain
from collections import defaultdict
import re
from pprint import pp

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, field_names_and_values, first, force_setattr, \
    intersection, pr, safe_issubclass, short, union


Canvas = Any
Index = int
Letter = str  # of length 1, only 'a'..'z'

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class Item:
    head: Any
    args: Tuple[Any, ...]

    def __init__(self, head: Any, *args: Any):
        force_setattr(self, 'head', head)
        force_setattr(self, 'args', args)

@dataclass(frozen=True)
class AtCell:
    canvas: Canvas
    index: Index
    letter: Letter

DataType = Union[Canvas, Index, Letter]

Elem = Union[Item, DataType, Type[DataType]]

def elems_of(elem) -> Iterable[Elem]:
    match elem:
        case Item(head, args):
            yield head
            yield from args
        case AtCell(c, i, l):
            yield AtCell
            yield c
            yield i
            yield l
        case _:
            yield elem

@dataclass(frozen=True)
class Variable:
    name: str
    type: DataType

    def __repr__(self) -> str:
        return self.name

C = Variable('C', Canvas)
I = Variable('I', Index)
L = Variable('L', Letter)

@dataclass(frozen=True)
class Subst:
    d: PMap[Variable, Any]

    @classmethod
    def from_tups(cls, *tups: Tuple[Variable, Any]) -> Subst:
        return cls(
            d=pmap(tups)
        )

    def is_bottom(self) -> bool:
        return False

    def pmatch(self, lhs: Any, rhs: Any) -> Subst:
        match (lhs, rhs):
            case (Variable, str()):
                # TODO check of lhs is already defined
                return Subst(self.d.set(lhs, rhs))
            case (Variable, int()):
                # TODO check of lhs is already defined
                return Subst(self.d.set(lhs, rhs))
            case (Item(head, args), AtCell(c, i, ll)):
                # STUB
                # TODO Check that head == AtCell
#                return \
#                    self.pmatch(args[0], c) \
#                    .pmatch(args[1], i) \
#                    .pmatch(args[2], l)
                
                result = self
                # TODO check number of elems
                for l, r in zip(elems_of(lhs), elems_of(rhs)):
                    result = result.pmatch(l, r)
                return result
        return self #STUB

empty_subst = Subst.from_tups()

if __name__ == '__main__':
    c1 = 'canvas'
    lhs = Item(AtCell, C, I, L)
    rhs = AtCell(c1, 2, 'b')
