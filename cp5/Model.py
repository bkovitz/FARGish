# Model.py -- Canvas-and-painters model with rewrite rules

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field, \
    astuple, is_dataclass
from abc import ABC, abstractmethod
from itertools import chain, combinations
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

    def short(self) -> str:
        args = ', '.join(short(a) for a in self.args)
        return f'{short(self.head)}({args})'

    __repr__ = short

@dataclass(frozen=True)
class AtCell(CompoundItem):
    canvas: Canvas
    index: Index
    letter: Letter

    def short(self) -> str:
        return f"{self.canvas}.{self.index}='{self.letter}'"

@dataclass(frozen=True)
class Succ:
    arg: Union[Letter, Variable]

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.arg)})'

    __repr__ = short

@dataclass(frozen=True)
class Seq(CompoundItem):
    canvas: Canvas
    op: Type[Succ]  # TODO allow any operation
    start_index: Index
    end_index: Index
    start_letter: Letter
    end_letter: Letter

def pred_of(x: Letter) -> Letter:
    # TODO can't pred 'a'
    return chr(ord(x) - 1)

def succ_of(x: Letter) -> Letter:
    # TODO can't succ 'z'
    return chr(ord(x) + 1)

DataType = Union[Canvas, Index, Letter]

def datatype_of(x: Any):
    match x:
        case int():
            return Index
        case str() if len(x) == 1: #HACK
            return Letter
        case str(): #HACK
            return Canvas
    raise NotImplementedError

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

    def short(self) -> str:
        return self.name

    __repr__ = short

C = Variable('C', Canvas)
I = Variable('I', Index)
I1 = Variable('I1', Index)
I2 = Variable('I2', Index)
L = Variable('L', Letter)
L1 = Variable('L1', Letter)
L2 = Variable('L2', Letter)

@dataclass(frozen=True)
class Plus:
    v: Variable
    n: int

    def short(self) -> str:
        return f'{short(self.v)}+{self.n}'

    __repr__ = short

ClassObject = Union[Type[Succ], Type[AtCell]]
    # A class object that can create a workspace item

LhsType = Union[
    CompoundItem, Item, Variable, DataType, Succ, ClassObject
]
    # Items appropriate for lhs of Subst.pmatch(): things that *could* contain
    # or be Variables.

RhsType = Union[
    CompoundItem, DataType, Succ, ClassObject
]
    # A thing that can be in the workspace. An RhsType must be completely
    # determinate: it must contain no variables.

def is_class_object(x: Any) -> TypeGuard[ClassObject]:
    return isclass(x)  # HACK

@dataclass(frozen=True)
class Subst:
    d: PMap[Variable, Any] = field(default_factory=pmap)

    @classmethod
    def from_tups(cls, *tups: Tuple[Variable, Any]) -> Subst:
        return cls(
            d=pmap(tups)
        )

    def eval(self, expr: LhsType) -> RhsType:
        match expr:
            case Variable():
                try:
                    return self.d[expr]
                except KeyError:
                    raise UndefinedVariable
            case str() | int():
                return expr
            case Item(head, args):
                cls = self.eval_get_class_object(head)
                return cls(*(self.eval(arg) for arg in args)) # type: ignore[arg-type]
            case Plus(v, n):
                i: int = self.eval(v)  # type: ignore[assignment]
                #TODO call eval_get_int() instead
                return i + n
            case Succ(arg):
                l: Letter = self.eval(arg)  # type: ignore[assignment]
                return succ_of(l)  # TODO what if l has no successor?
            case _ if isclass(expr):
                return expr

        raise NotImplementedError(expr)

    def eval_get_class_object(self, expr: LhsType) -> ClassObject:
        result = self.eval(expr)
        if is_class_object(result):
            return result
        raise NotImplementedError(expr, result)

    def is_bottom(self) -> bool:
        return False

    def pmatch(self, lhs: LhsType, rhs: RhsType) -> Subst:
        if lhs == rhs:
            return self
        match (lhs, rhs):
            case (int(), int()):
                return bottom_subst  # we know that lhs != rhs
            case (str(), str()):
                return bottom_subst
            case (_, _) if isclass(lhs) and isclass(rhs):
                return bottom_subst
            case (Variable(_, typ) as var, _):
                if typ != datatype_of(rhs):
                    return bottom_subst
                if lhs in self.d:
                    return self.pmatch(self.d[var], rhs)
                else:
                    return Subst(self.d.set(var, rhs))
            case (CompoundItem(), CompoundItem()):
                result = self
                # TODO check number of elems
                for l, r in zip(elems_of(lhs), elems_of(rhs)):
                    result = result.pmatch(l, r)
                return result
            case (CompoundItem(), _):
                return bottom_subst
            case (Plus(v, n), int(m)):
                return self.pmatch(v, m - n)
            case (Plus(), _):
                return bottom_subst  # type clash on rhs
            case (Succ(Variable() as v), str() as s):
                return self.pmatch(v, pred_of(s))
            case ([*lhs_items], [*rhs_items]):
                result = self
                for l, r in zip(lhs_items, rhs_items):
                    result = result.pmatch(l, r)
                return result
                        
        raise NotImplementedError(lhs, rhs)

class BottomSubst(Subst):
    
    def is_bottom(self) -> bool:
        return True

    def pmatch(self, lhs: Any, rhs: Any) -> Subst:
        return self

    def short(self) -> str:
        return self.__class__.__name__

    __repr__ = short

empty_subst = Subst.from_tups()
bottom_subst = BottomSubst()

@dataclass
class Model:
    rules: Collection[Rule] = ()
    ws: Set[RhsType] = field(default_factory=set)
    
    def add_canvas(self, name: str, contents: str) -> Canvas:
        self.ws.add(name) #HACK
        c = name
        for i, letter in enumerate(contents):
            self.ws.add(AtCell(c, i + 1, letter))
        return c

    def try_all_rules(self) -> Iterable[RhsType]:
        for rule in self.rules:
            for target in self.ws_targets(len(rule)):
                if (produced := rule.run(target)) is not None:
                    yield produced

    def ws_targets(self, num: int) -> Iterable[Tuple[RhsType, ...]]:
        yield from combinations(sorted(self.ws, key=self.target_sort_key), num)

    @classmethod
    def target_sort_key(cls, item: RhsType) -> Tuple[str, int]:
        match item:
            case AtCell(c, i, l):
                return c, i
            case int():
                return '', item
            case Succ(arg):
                return str(arg), 0
            case str():
                return item, 0
            case CompoundItem():
                return item.__class__.__name__, 0
        raise NotImplementedError(item)

    def __repr__(self) -> str:
        return str(self.ws)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.ws)})'


@dataclass(frozen=True)
class Rule:
    lhs: Tuple[Item, ...]
    rhs: Item

    def run(self, target: Collection[RhsType]) -> Optional[RhsType]:
        su = empty_subst
        for lhs_item, target_item in zip(self.lhs, target):
            #lo('RULE', lhs_item, target_item)
            su = su.pmatch(lhs_item, target_item)
        #lo('GOT', su)
        if su.is_bottom():
            return None  # TODO UT
        return su.eval(self.rhs)

    def __len__(self) -> int:
        '''Length of .lhs'''
        return len(self.lhs)

    def __repr__(self) -> str:
        l = ', '.join(short(item) for item in self.lhs)
        return f'{l} -> {short(self.rhs)}'

if __name__ == '__main__':
    c1 = 'canvas'
    lhs = Item(AtCell, C, I, L)
    rhs = AtCell(c1, 2, 'b')

    rules = [
        Rule(
            (Item(AtCell, C, I, L), Item(AtCell, C, Plus(I, 1), Succ(L))),
            Item(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
        )
    ]

    #m = Model(rules)
    m = Model()
    c1 = m.add_canvas('canvas1', 'abc')
    print(short(m))
