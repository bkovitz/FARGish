# Model.py -- Canvas-and-painters model with rewrite rules

# ABANDONED 31-May-2023

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check, get_type_hints, get_args
from dataclasses import dataclass, field, fields, replace, InitVar, Field, \
    astuple, is_dataclass
from abc import ABC, abstractmethod
from itertools import chain, combinations, permutations
from collections import defaultdict
import re
from pprint import pp
from inspect import isclass
from random import choice

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import as_iter, field_names_and_values, first, force_setattr, \
    intersection, pr, safe_issubclass, short, union, field_values


#Canvas = str  # Must have length of at least 2, e.g. 'c1'
@dataclass(frozen=True)
class Canvas:
    name: str

Index = int
Letter = str  # of length 1, only 'a'..'z'

class Fizzle(Exception):
    pass

class UndefinedVariable(Fizzle):
    pass

class CompoundItem:
    '''A workspace item that contains multiple items within it.'''
    pass

@dataclass(frozen=True)
class Lt:
    '''An "lhs tuple": a tuple suitable for the lhs of a Rule, capable of
    matching a workspace Item. An Lt is just like an Item except that it
    can contain Variables as well as other matchers, such as Plus and Succ,
    and even its head (the 'class' of the Item) can be a Variable.'''
    head: Head
    args: Tuple[Any, ...]

    def __init__(self, head: Any, *args: Any):
        force_setattr(self, 'head', head)
        force_setattr(self, 'args', args)

    def short(self) -> str:
        args = ', '.join(short(a) for a in self.args)
        return f'{short(self.head)}[{args}]'

    __repr__ = short

@dataclass(frozen=True)
class AtCell(CompoundItem):
    canvas: Canvas
    index: Index
    letter: Letter

    def short(self) -> str:
        return f"{self.canvas}.{self.index}='{self.letter}'"

class Tag(CompoundItem):
    pass

class TagValue:
    pass

#Side = Literal['lhs', 'rhs']
class Side(TagValue): pass
@dataclass(frozen=True)
class Lhs(Side): pass
@dataclass(frozen=True)
class Rhs(Side): pass

@dataclass(frozen=True)
class SideTag(Tag):
    canvas: Canvas
    side: Side

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}[{short(self.canvas)} {short(self.side)}]'

    __repr__ = short

class World(TagValue): pass
# TODO rm dataclass
@dataclass(frozen=True)
class OldWorld(World): pass
@dataclass(frozen=True)
class NewWorld(World): pass

@dataclass(frozen=True)
class WorldTag(Tag):
    canvas: Canvas
    world: World

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}[{short(self.canvas)} {short(self.world)}]'

    __repr__ = short

@dataclass(frozen=True)
class OtherSide(CompoundItem):
    left: Canvas
    right: Canvas

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}[{short(self.left)} {short(self.right)}]'

    __repr__ = short

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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}[{short(self.canvas)} {short(self.op)} {self.start_index} {self.end_index} {self.start_letter!r} {self.end_letter!r}]'

    __repr__ = short

@dataclass(frozen=True)
class SuccPair(CompoundItem):
    canvas1: Canvas
    i1: Index
    canvas2: Canvas
    i2: Index
    letter1: Letter
    letter2: Letter

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
        case Canvas():
            return Canvas
        case World():
            return World
    lo('DT', x, type(x))
    raise NotImplementedError(x)

#Elem = Any  #Union[Lt, CompoundItem, DataType, World] #, Type[DataType]]

## TODO rename flatten()?
#def elems_of(elem: Elem) -> Iterable[Elem]:
#    #lo('ELEM', elem)
#    match elem:
#        case Lt(head, args):
#            yield head
#            yield from args
#        case WorldTag(c, w):  # TODO as_tuple?
#            yield WorldTag
#            yield c
#            yield w
#        case Canvas():
#            yield elem
#        case _ if is_dataclass(elem):
#            yield elem.__class__
#            #yield from astuple(elem)
#            yield from field_values(elem)
#        case _:
#            yield elem

def args_of(item: CompoundItem) -> Iterable[Arg]:
    yield from field_values(item)
        

'''
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
'''

@dataclass(frozen=True)
class Variable:
    name: str
    type: Type[DataType] | Type[World]

    def short(self) -> str:
        return self.name

    __repr__ = short

C = Variable('C', Canvas)
C1 = Variable('C1', Canvas)
C2 = Variable('C2', Canvas)
I = Variable('I', Index)
I1 = Variable('I1', Index)
I2 = Variable('I2', Index)
I3 = Variable('I3', Index)
J = Variable('J', Index)
L = Variable('L', Letter)
L1 = Variable('L1', Letter)
L2 = Variable('L2', Letter)
L3 = Variable('L3', Letter)
W = Variable('W', World)

@dataclass(frozen=True)
class Plus:
    v: Variable
    n: int

    def short(self) -> str:
        return f'{short(self.v)}+{self.n}'

    __repr__ = short

##### The new types #####

"""
TagValue = Side, World  # inherit
Lt  # a tuple suitable for the lhs argument to pmatch; should match an Item
"""
#TODO Document all these
Op = Type[Succ]
Head = Type[CompoundItem]
Arg = Union[Canvas, Index, Letter, Op, TagValue]
WSItem = Union[CompoundItem, Canvas]  # No variables allowed
LtHead = Union[Head, Type[Plus], Type[Succ]]
LtArg = Union[Arg, Variable, Plus, Op, Succ]
Lvalue = Union[Lt, Head, LtArg]
Rvalue = Union[WSItem, Head, Arg]

##### Some of the old types (as of 11-May-2023) #####

#ClassObject = Union[Type[Succ], Type[AtCell]]
    # A class object that can create a workspace item

#LhsType = Union[
#    CompoundItem, Lt, Variable, Plus, DataType, Succ, ClassObject
#]
    # Items appropriate for lhs of Subst.pmatch(): things that *could* contain
    # or be Variables.

#RhsType = Union[
#    CompoundItem, DataType, Succ, ClassObject
#]
    # A thing that can be in the workspace. An RhsType must be completely
    # determinate: it must contain no variables.

#def is_class_object(x: Any) -> TypeGuard[ClassObject]:
#    return isclass(x)  # HACK

@dataclass(frozen=True)
class Subst:
    d: PMap[Variable, Any] = field(default_factory=pmap)

    @classmethod
    def from_tups(cls, *tups: Tuple[Variable, Any]) -> Subst:
        return cls(
            d=pmap(tups)
        )

    def eval(self, expr: Lvalue) -> Rvalue:
        match expr:
            case Variable():
                try:
                    return self.d[expr]
                except KeyError:
                    raise UndefinedVariable(expr)
            case str() | int():
                return expr
            case Lt(head, args):
                return head(*(self.eval(arg) for arg in args))
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

    def is_bottom(self) -> bool:
        return False

    def pmatch(
        self,
        lhs: Lvalue | List[Lvalue],
        rhs: Rvalue | List[Rvalue]
    ) -> Subst:
        if lhs == rhs:
            return self
        match (lhs, rhs):
            case (int(), int()):
                return bottom_subst  # we know that lhs != rhs
            case (str(), str()):
                return bottom_subst
            case (TagValue(), TagValue()):
                return bottom_subst
            case (Canvas(), Canvas()):
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
            case (Lt(head, args), CompoundItem()):
                assert isinstance(rhs, CompoundItem) #mypy bug
                result = self.pmatch(head, rhs.__class__)
                for l, r in zip(args, args_of(rhs)):
                    result = result.pmatch(l, r)
                return result
            case (Lt(), Canvas()):
                return bottom_subst
            case (CompoundItem(), CompoundItem()):
                assert isinstance(lhs, CompoundItem) #mypy bug
                assert isinstance(rhs, CompoundItem) #mypy bug
                result = self.pmatch(lhs.__class__, rhs.__class__)
                for l, r in zip(args_of(lhs), args_of(rhs)):
                    result = result.pmatch(l, r)
                return result
            case (CompoundItem(), _):
                return bottom_subst
            case (Plus(v, n), int(m)):
                if m <= 1:
                    return bottom_subst
                return self.pmatch(v, m - n)
            case (Plus(), _):
                return bottom_subst  # type clash on rhs
            case (Succ(Variable() as v), str() as s):
                return self.pmatch(v, pred_of(s))
            case ([*lhs_items], [*rhs_items]):
                result = self
                for ll, rr in zip(lhs_items, rhs_items):
                    result = result.pmatch(ll, rr)
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

@dataclass(frozen=True)
class Rule:
    lhs: Tuple[Lt, ...]
    rhs: Lt

    def run(self, target: Collection[Rvalue]) -> Optional[Rvalue]:
        su = empty_subst
        for lhs_item, target_item in zip(self.lhs, target):
            #lo('RULE', lhs_item, target_item)
            su = su.pmatch(lhs_item, target_item)
        #lo('GOT', su)
#        match target:
#            case [Tag(), Tag(), Tag(), Tag()]:
#                print('TARGET', target)
#                print('SU', su)
        if su.is_bottom():
            return None  # TODO UT
        return su.eval(self.rhs)

    def __len__(self) -> int:
        '''Length of .lhs'''
        return len(self.lhs)

    def __repr__(self) -> str:
        l = ', '.join(short(item) for item in self.lhs)
        return f'{l} -> {short(self.rhs)}'

@dataclass
class Model:
    rules: Collection[Rule] = ()
    ws: Set[Rvalue] = field(default_factory=set)
    
    def add_canvas(
        self,
        name: str,
        contents: str,
        side: Optional[Side]=None,
        world: Optional[World]=None
    ) -> Canvas:
        c = Canvas(name)
        self.ws.add(c) #HACK
        for i, letter in enumerate(contents):
            self.ws.add(AtCell(c, i + 1, letter))
        if side is not None:
            self.ws.add(SideTag(c, side))
        if world is not None:
            self.ws.add(WorldTag(c, world))
        return c

    def add(self, x: Rvalue) -> Rvalue:
        self.ws.add(x)
        return x

    def do_timestep(self, num: int=1) -> None:
        for _ in range(num):
            results = [r for r in self.try_all_rules() if r not in self.ws]
            if results:
                self.ws.add(choice(results))
            #lo('DOT', len(results), self.ws)

    def try_all_rules(self) -> Iterable[Rvalue]:
        for rule in self.rules:
            #lo('TAL', len(list(self.ws_targets(len(rule)))))
            for target in self.ws_targets(len(rule)):
                if (produced := rule.run(target)) is not None:
                    yield produced

    def ws_targets(self, num: int) -> Iterable[Tuple[Rvalue, ...]]:
        #yield from combinations(sorted(self.ws, key=self.target_sort_key), num)
        yield from permutations(self.ws, num)

    """
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
            case Seq(c, op, i1, i2, l1, l2):
                return 'Seq', i2  # HACK, probably wrong
            case SideTag(c, 'lhs'):
                return 'SideTag' + c, 0
            case SideTag(c, 'rhs'):
                return 'SideTag' + c, 1
            case WorldTag(c, OldWorld()):
                return 'WorldTag' + c, 0
            case WorldTag(c, NewWorld()):
                return 'WorldTag' + c, 1
            case CompoundItem():
                return item.__class__.__name__, 0
        raise NotImplementedError(item)
    """

    def __repr__(self) -> str:
        return str(self.ws)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.ws)})'


if __name__ == '__main__':
    c1 = Canvas('c1')
    lhs = Lt(AtCell, C, I, L)
    rhs = AtCell(c1, 2, 'b')

    rules = [
        Rule(
            (Lt(AtCell, C, I, L), Lt(AtCell, C, Plus(I, 1), Succ(L))),
            Lt(Seq, C, Succ, I, Plus(I, 1), L, Succ(L))
        ),
        Rule(
            (Lt(AtCell, C, I, L),
             Lt(Seq, C, Succ, Plus(I1, 1), I2, Succ(L1), L2)
            ),
            Lt(Seq, C, Succ, I1, I2, L1, L2)
        ),
        Rule(
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(AtCell, C, Plus(I2, 1), Succ(L2))),
            Lt(Seq, C, Succ, I1, I2, L1, L2)
        ),
        Rule(  # Seq + Seq (no overlap)
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(Seq, C, Succ, Plus(I2, 1), I3, Succ(L2), L3)),
            (Lt(Seq, C, Succ, I1, I3, L1, L3))
        ),
        Rule(  # Seq + Seq (overlap at one letter)
            (Lt(Seq, C, Succ, I1, I2, L1, L2),
             Lt(Seq, C, Succ, I2, I3, L2, L3)),
            (Lt(Seq, C, Succ, I1, I3, L1, L3))
        )
    ]

    m = Model(rules)
    c1 = m.add_canvas('c1', 'abc')
    print(short(m))

    for _ in range(5):
        xs = [x for x in m.try_all_rules() if x not in m.ws]
        x = choice(xs)
        m.add(x)
        pr(m.ws)
        print()

