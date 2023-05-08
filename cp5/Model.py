# Model.py -- Canvas-and-painters model with rewrite rules

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field, \
    astuple, is_dataclass
from abc import ABC, abstractmethod
from itertools import chain
from collections import defaultdict
import re
from pprint import pp
from inspect import isclass

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

class UndefinedVariable(Fizzle):
    pass

class CompoundItem:
    pass

@dataclass(frozen=True)
class Item(CompoundItem):
    head: Any
    args: Tuple[Any, ...]

    def __init__(self, head: Any, *args: Any):
        force_setattr(self, 'head', head)
        force_setattr(self, 'args', args)

@dataclass(frozen=True)
class AtCell(CompoundItem):
    canvas: Canvas
    index: Index
    letter: Letter

@dataclass(frozen=True)
class Succ:
    arg: Union[Letter, Variable]

def pred_of(x: Letter) -> Letter:
    return chr(ord(x) - 1)

DataType = Union[Canvas, Index, Letter]

Elem = Union[Item, CompoundItem, DataType, Type[DataType]]

def elems_of(elem) -> Iterable[Elem]:
    match elem:
        case Item(head, args):
            yield head
            yield from args
        case _ if is_dataclass(elem):
            yield elem.__class__
            yield from astuple(elem)
        case _:
            yield elem

ElemType = Literal['Primitive', 'Variable', 'Compound', 'Plus']
def elemtype(x: Elem) -> ElemType:
    match x:
        case int():
            return 'Primitive'
        case str():
            return 'Primitive'
        case Item():
            return 'Compound'
        case Variable():
            return 'Variable'
        case AtCell():
            return 'Compound'
        case Plus():
            return 'Plus'
        case _ if isclass(x):
            return 'Primitive'
    raise NotImplementedError

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
class Plus:
    v: Variable
    n: int

@dataclass(frozen=True)
class Subst:
    d: PMap[Variable, Any] = field(default_factory=pmap)

    @classmethod
    def from_tups(cls, *tups: Tuple[Variable, Any]) -> Subst:
        return cls(
            d=pmap(tups)
        )

    def eval(self, v: Variable) -> Item:
        try:
            return self.d[v]
        except KeyError:
            raise UndefinedVariable

    def is_bottom(self) -> bool:
        return False

    def pmatch(self, lhs: Any, rhs: Any) -> Subst:
#        import pdb; pdb.set_trace()
#        match (lhs, rhs):
#            case (Variable(), str()):
#                # TODO check of lhs is already defined
#                return Subst(self.d.set(lhs, rhs))
#            case (Variable(), int()):
#                # TODO check of lhs is already defined
#                return Subst(self.d.set(lhs, rhs))
#            case (Item(head, args), AtCell(c, i, ll)):
#                # STUB
#                # TODO Check that head == AtCell
##                return \
##                    self.pmatch(args[0], c) \
##                    .pmatch(args[1], i) \
##                    .pmatch(args[2], l)
#                
#                result = self
#                # TODO check number of elems
#                for l, r in zip(elems_of(lhs), elems_of(rhs)):
#                    result = result.pmatch(l, r)
#                return result
#            case (int(), int()):
#                return self

#        match (elemtype(lhs), elemtype(rhs)):
#            case ('Primitive', 'Primitive'):
#                if lhs == rhs:
#                    return self
#                else:
#                    return bottom_subst
#            case ('Variable', x):
#                if lhs in self.d:
#                    return self.pmatch(self.d[lhs], rhs)
#                else:
#                    return Subst(self.d.set(lhs, rhs))
#            case ('Compound', 'Compound'):
#                result = self
#                # TODO check number of elems
#                for l, r in zip(elems_of(lhs), elems_of(rhs)):
#                    result = result.pmatch(l, r)
#                return result
#            case ('Plus', 'Primitive'):
#                match (lhs, rhs):
#                    case (Plus(v, n), int(m)):
#                        return self.pmatch(v, m - n)

        if lhs == rhs:
            return self
        match (lhs, rhs):
            case (int(), int()):
                return bottom_subst  # we know that lhs != rhs
            case (str(), str()):
                return bottom_subst
            case (_, _) if isclass(lhs) and isclass(rhs):
                return bottom_subst
            case (Variable(), _):
                if lhs in self.d:
                    return self.pmatch(self.d[lhs], rhs)
                else:
                    return Subst(self.d.set(lhs, rhs))
            case (CompoundItem(), CompoundItem()):
                result = self
                # TODO check number of elems
                for l, r in zip(elems_of(lhs), elems_of(rhs)):
                    result = result.pmatch(l, r)
                return result
            case (Plus(v, n), int(m)):
                return self.pmatch(v, m - n)
            case (Succ(Variable() as v), str()):
                return self.pmatch(v, pred_of(rhs))
                        
        raise NotImplementedError

class BottomSubst(Subst):
    
    def is_bottom(self) -> bool:
        return True

    def pmatch(self, lhs: Any, rhs: Any) -> Subst:
        return self

empty_subst = Subst.from_tups()
bottom_subst = BottomSubst()

if __name__ == '__main__':
    c1 = 'canvas'
    lhs = Item(AtCell, C, I, L)
    rhs = AtCell(c1, 2, 'b')
