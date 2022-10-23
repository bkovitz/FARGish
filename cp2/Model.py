# Model.py -- The canvas-and-painters model

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from itertools import chain, product
from functools import reduce
from collections import defaultdict
from io import StringIO
from copy import deepcopy
from random import choice, choices, random
import sys
from operator import itemgetter
import operator

from pyrsistent import pmap
from pyrsistent.typing import PMap

#from Types import CanvasValue, CellContent, End, \
#    Fizzle, FizzleNoDetPainters, FizzleValueNotFound, \
#    is_cell_content, \
#    Start
#import Addrs
#from Canvas import Canvas, Canvas1D
#from Soup import Soup
#from Subst import Subst, empty_subst, Plus
#from Funcs import CallableFunc, Func, same, pred, succ, MakeRelativeIndirectPainter, \
#    MakeBetweenPainter, SimpleFunc, Value
#from Addrs import Addr, DetAddr, DetAddrWithSubst, F, I, Index, Indices, J, RelatedPair, SpecialAddr, \
#    MatchContent, SoupRef, Variable, WorkingSoup
#from Painters import Painter, DetPainter0, DetPainter, DetFunc
from Log import lo, trace, indent_log, set_log_level
from util import first, force_setattr, short, nf, Numeric, empty_set, rescale, \
    filter_none

class CallableFunc(ABC):

    @abstractmethod
    def apply(self, model: Model, subst: Subst, value: Value) \
    -> Value:
        pass

    @abstractmethod
    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        pass

    def can_make(self, model: Model, subst: Subst) -> bool:
        '''Will this be a valid function given the substitutions in 'subst'?'''
        return True

########## Canvas-cell contents ##########

@dataclass(frozen=True)
class Letter(CallableFunc):
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if self.c < 'a' or self.c > 'z':
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

    def apply(self, model: Model, subst: Subst, value: Value) \
    -> Value:
        return self

    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        yield self

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

Expr = Any  # TODO restrict

@dataclass(frozen=True)
class AnnotationType:
    name: str
    matchable: bool = True
    # Should MatchContent match this AnnotationType?

    def __str__(self) -> str:
        return self.name

Anchor = AnnotationType('Anchor')
ImmutableType = AnnotationType('ImmutableType', matchable=False)

def applies_here_no(c: Canvas, i: int | Index) -> bool:
    return False

@dataclass(frozen=True)
class Annotation:
    type: AnnotationType
    name: str
    applies_here: Callable[[Canvas, int | Index], bool] = applies_here_no
    # Should cell i in canvas c get this annotation?

    def is_matchable(self) -> bool:
        return self.type.matchable

    def __str__(self) -> str:
        return self.name

def start_applies_here(c: Canvas, i: int | Index) -> bool:
    return to_index(i) == c.min_index()

def end_applies_here(c: Canvas, i: int | Index) -> bool:
    return to_index(i) == c.max_index()

def inextreme_applies_here(c: Canvas, i: int | Index) -> bool:
    return not(
        start_applies_here(c, i) or end_applies_here(c, i)
    )

Start = Annotation(Anchor, 'Start', start_applies_here)
End = Annotation(Anchor, 'End', end_applies_here)
Inextreme = Annotation(Anchor, 'Inextreme', inextreme_applies_here)
Immutable = Annotation(ImmutableType, 'Immutable')

@dataclass(frozen=True)
class Annotations:
    elems: FrozenSet[Annotation]

    #TODO Shouldn't this method allow multiple arguments?
    @classmethod
    def make_from(cls, v: Union[Annotation, Annotations, None]) -> Annotations:
        match v:
            case Annotation():
                return cls(frozenset([v]))
            case Annotations():
                return v
            case None:
                return empty_annotations
                
    def elems_str(self) -> str:
        return ', '.join(sorted([short(e) for e in self.elems]))

    def simplest(self) -> Union[None, Annotation, Annotations]:
        match len(self.elems):
            case 0:
                return None
            case 1:
                return first(self.elems)
            case _:
                return self

    def exclude_unmatchable(self) -> Union[Annotation, Annotations]:
        new_elems = [
            elem
                for elem in self.elems
                    if elem.is_matchable()
        ]
        if len(new_elems) == 0:
            return empty_annotations
        elif len(new_elems) == len(self.elems):
            return self
        else:
            return Annotations(frozenset(new_elems))

    def __add__(self, a: Annotation | Annotations) -> Annotations:
        match a:
            case Annotation():
                return Annotations(self.elems | frozenset([a]))
            case Annotations():
                return Annotations(self.elems | a.elems)

    def __iter__(self, *args, **kwargs) -> Iterator[Annotation]:
        return self.elems.__iter__(*args, **kwargs)

    def __contains__(self, v: Annotation) -> bool:
        return v in self.elems

    def __ge__(self, other: Annotations) -> bool:
        return self.elems >= other.elems

    def __bool__(self) -> bool:
        return bool(self.elems)

    def __str__(self) -> str:
        return f'Annotations({self.elems_str()})'

empty_annotations = Annotations(empty_set)

@dataclass(frozen=True)
class CellBundle:
    '''Everything that can be held simultaneously in one cell, i.e. up to one
    value and any number of annotations, all bundled into one convenient
    object.'''
    #value: Union[None, Letter, Blank]
    value: Union[Letter, Blank]
    annotations: Annotations

    # TODO During __post_init__, check if value is a space; if so, change
    # it to None

    @classmethod
    def make_from(cls, *v: Union[CellContent, str]) -> CellBundle:
        result = cls(Blank(), empty_annotations)
        for vv in v:
            result = result + vv
        return result

    def value_only(self) -> bool:
        '''Does this CellBundle contain only a value and no annotations?'''
        return not self.annotations

    #def simplest(self) -> Any:  # TODO restore type hint: Union[Value, Annotation, Annotations, CellBundle]:
    # TODO UT
    def simplest(self) -> Union[None, CellContent]:
        match (not is_blank(self.value), bool(self.annotations)):
            case (False, False):
                return None
            case (False, True):
                return self.annotations.simplest()
            case (True, False):
                return self.value
            case (True, True):
                return self

        if is_blank(self.value):
            return None
        elif self.value_only():
            return self.value
        elif self.value is None:
            return self.annotations.simplest()
        else:
            return self

    def exclude_unmatchable(self) -> Optional[CellContent]:
        return self.make_from(
            self.value,
            self.annotations.exclude_unmatchable()
        ).simplest()

    def __iter__(self) -> Iterable[CellContent1]:
        if self.value is not None:
            yield self.value
        yield from self.annotations

    def __add__(self, v: CellContent | str) -> CellBundle:
        match v:
            case str():
                l = Letter.from_str(v)
                if l == self.value:
                    return self
                else:
                    return replace(self, value=l)
            case Letter():
                if v == self.value:
                    return self
                else:
                    return replace(self, value=v)
            case Blank():
                if v == self.value:
                    return replace(self, value=None)
                else:
                    return self
            case Annotation():
                if v in self.annotations:
                    return self
                else:
                    return replace(self, annotations=self.annotations + v)
            case Annotations():
                if v == self.annotations:
                    return self
                else:
                    return replace(self, annotations=self.annotations + v)
            case CellBundle():
                result = self
                for vv in v:  # type: ignore[attr-defined]  # mypy bug
                    result = result + vv
                return result
        assert False, f"CellBundle.__add__(): should not go past 'match' stmt; {v}"

    def is_match(self, v: CellContent) -> bool:
        '''Returns True if 'self' contains all the content within 'v', False
        if 'v' contains any value or annotation not in 'self'.'''
        match v:
#            case str():
#                return v == self.value
#            case None:
#                return True
            case Letter():
                return v == self.value
            case Blank():
                return v == self.value
            case Annotation():
                return v in self.annotations
            case Annotations():
                return self.annotations >= v
            case CellBundle():
                return all(self.is_match(vv) for vv in unbundle_cell_content(v))

    def __repr__(self) -> str:
        return f'CellBundle({short(self.value, inside=True)}; {self.annotations.elems_str()})'

empty_cell_bundle = CellBundle(Blank(), empty_annotations)

#CellContent1 = Union[CanvasValue, None, Annotation]
CellContent1 = Union[CanvasValue, Annotation]
CellContent = Union[CellContent1, Annotations, CellBundle]

def is_cell_content(x: Any) -> TypeGuard[CellContent]:
    return (
        isinstance(x, str)
        or
        isinstance(x, CellBundle)
        or
        isinstance(x, Annotation)
        or
        isinstance(x, Annotations)
        or
        x is None
    )

def unbundle_cell_content(v: CellContent) -> Iterable[CellContent1]:
    match v:
        case str():
            yield v
#        case None:
#            pass
        case Annotation():
            yield v
        case Annotations():
            yield from v
        case CellBundle():
            if v.value is not None:
                yield v.value
            yield from v.annotations

def exclude_unmatchable(x: Optional[CellContent]) -> Optional[CellContent]:
    match x:
        case Annotation():
            if x.is_matchable():
                return x
            else:
                return empty_annotations
        case Annotations() | CellBundle():
            return x.exclude_unmatchable()
        case _:
            return x
    raise ValueError(f'exclude_unmatchable: invalid argument: {x!r}')

#def is_painter(x: Any) -> TypeGuard[Painter]:
#    #return isinstance(x, tuple)
#    match x:
#        case (i, j, func):
#            return True
#        case _:
#            return False

#def is_func(x: Any) -> TypeGuard[Func]:
#    match x:
#        case str():
#            return True
#        case (i, j, func):
#            return True
#        case c if callable(c):
#            return True
#    return False
            
def is_space(cc: CellContent) -> bool:
    match cc:
        case str():
            return cc == ' '
        case None:
            return False
        case CellBundle():
            return cc.simplest() == ' '
        case _:
            return False

########## Addresses ##########

class Addr(ABC):

    @abstractmethod
    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        pass

# A convenience function for unit tests
def make_addr(a: ToAddr) -> Addr:
    match a:
        case int():
            return Index(a)
        case SoupRef() | Plus():
            return a
        case str():
            return MatchContent(Letter.from_str(a))

# A determinate Addr: no variables, no patterns, nothing to match or expand
#DetAddr = Union[Index, Indices, Painter, SoupRef]
DetAddr = Addr  # TODO Make a correct type hint

@dataclass(frozen=True)
class Index(DetAddr):
    i: int

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst.unify(var, self), self)

    def __add__(self, offset: int) -> Index:
        return Index(self.i + offset)

    def __sub__(self, other: Index | int) -> int:
        o: int = other if isinstance(other, int) else other.i
        return self.i - o

    def __lt__(self, other: Index) -> bool:
        return self.i < other.i

    def __le__(self, other: Index) -> bool:
        return self.i <= other.i

    def __gt__(self, other: Index) -> bool:
        return self.i > other.i

    def __ge__(self, other: Index) -> bool:
        return self.i >= other.i

    @classmethod
    def from_to(cls, lb: Index | int, ub: Index | int) -> Iterable[Index]:
        return (Index(i) for i in range(as_int(lb), as_int(ub) + 1))

#    def short(self) -> str:
#        cl = self.__class__.__name__
#        return f'{cl}({self.i})'

    def short(self) -> str:
        return str(self.i)

    __repr__ = short

def as_int(x: Index | int) -> int:
    if isinstance(x, int):
        return x
    else:
        return x.i

def to_index(i: Index | int) -> Index:
    return Index(i) if isinstance(i, int) else i

Index2 = Union[Index, Tuple[Index, AnnotationType]]

def as_index2(x: Union[int, Index2]):
    if isinstance(x, int):
        return Index(x)
    else:
        return x

@no_type_check  # crashes mypy 0.971
def extract_index(i: Index2) -> Index:
    match i:
        case Index():
            return i
        case (Index() as ii, _):
            return ii
    assert False, f"extract: should not go past 'match' stmt, {i}"

@dataclass(frozen=True)
class Indices(Addr):
    '''Zero or more Index2s.'''
    elems: FrozenSet[Index2]

    def __init__(self, *ii: Index2 | int):
        force_setattr(self, 'elems', frozenset(as_index2(i) for i in ii))

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst.unify(var, self), self)

    def __iter__(self) -> Iterable[Index2]:
        yield from self.elems
        
    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f"{cl}({', '.join(str(e) for e in self.elems)})"

def is_index(e: Any) -> TypeGuard[Index]:
    #return isinstance(e, int)
    match e:
        case Index():
            return True
        case (Index(), AnnotationType()):
            return True
        case _:
            return False

# TODO rm; Subst.as_index() does this correctly
def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return Index(e)
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle(f"as_index: Can't convert {e!r} to index")

@dataclass(frozen=True)
class SoupRef(Addr):
    name: str

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield DetAddrWithSubst(subst, self)

    def __repr__(self) -> str:
        return self.name

class SR:
    '''Just a holder for SoupRefs to make it easy to refer to them in
    'case' statements. Never make an instance of this class.'''
    WorkingSoup = SoupRef('WorkingSoup')
    LongTermSoup = SoupRef('LongTermSoup')

# A convenience type for DetAddr.make_from()
ToDetAddr = Union[int, SoupRef, Indices]

# A convenience function for unit tests
def make_detaddr(a: ToDetAddr) -> DetAddr:
    match a:
        case int():
            return Index(a)
        case SoupRef():
            return a
        case Indices():
            return a

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

# TODO Should move this somewhere else in the file; it's needed only in
# substitutions, as an Expr. It's here only to be defined before ToAddr.
@dataclass(frozen=True)
class Plus(Addr, HasAsIndex):
    args: Tuple[Expr, ...]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __repr__(self) -> str:
        return '+'.join(str(a) for a in self.args)

    def value_of(self, subst: Subst) -> Expr:
        return self.simplify(subst, self.args)

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        match subst.as_index(self):
            case None:
                return
            case Index() | Indices() as i:
                yield DetAddrWithSubst(subst.unify(var, i), i)
            case x:
                raise NotImplementedError(f"Can't match Plus that simplifies to {x}, {type(x)}")

    @classmethod
    @no_type_check  # mypy 0.971 crashes on '*more' below
    def simplify(cls, subst: Subst, args: Tuple[Expr], init: int=0) -> Expr:
        '''Evaluates 'args' as a sum to as simple a form as possible. The
        simplest form is an int; if a variable in 'args' is undefined, then
        we return an Expr containing that variable.'''
        match args:
            case ():
                return init
            case (expr,):
                return cls.try_to_add(init, subst.simplify(expr))
            case (expr, *more):
                #lo('init=', init, 'expr=', expr, subst)
                return cls.simplify(
                    subst,
                    more,
                    cls.try_to_add(init, subst.simplify(expr))
                )

    @classmethod
    def try_to_add(cls, a: Any, b: Any) -> Expr:
        match (a, b):
            case (0, _):
                return b
            case (int(), int()):
                return a + b
            case (Index(i), int()):
                return Index(i + b)
            case (Plus(args_a), Plus(args_b)):
                return Plus(*(args_a + args_b))
            case (Plus(args_a), _):
                return Plus(*(args_a + (b,)))
            case (_, Plus(args_b)):
                return Plus(*((a,) + args_b))
            case _:
                return Plus(a, b)

    def as_index(self) -> Index:
        return Index(
            reduce(operator.add, (as_index(arg) for arg in self.args), 0)
        )

# A convenience type for Painter.make_from()
ToAddr = Union[int, str, SoupRef, Plus]

@dataclass(frozen=True)
class DetAddrWithSubst:
    subst: Subst
    addr: DetAddr

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.subst)}, {short(self.addr)})'

    __str__ = short
    __repr__ = short

# A way to refer to a cell's value or an annotation within the cell
@dataclass(frozen=True)
class Variable(Addr):
    name: str

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        if self in subst:
            yield from (
                subst.simplify(self).to_detaddrs(model, subst, var)
            )
        else:
            yield from (
                DetAddrWithSubst(
                    subst.unify(var, index).unify(self, index),
                    index
                ) for index in model.canvas.all_addrs()
            )
        
    def __repr__(self) -> str:
        return self.name

I = Variable('I')
J = Variable('J')
F = Variable('F')

# TODO rm
class SpecialAddr(ABC):

    @abstractmethod
    def to_detaddrs(
        self,
        model: Model,
        canvas: Canvas,
        primitive_funcs: Iterable[Func]
    ) -> Iterable[DetAddrWithSubst]:
        pass

@dataclass(frozen=True)
class RelatedPair(Addr):
    '''A kind of Addr: a RelatedPair matches any pair of canvas cells that
    are related by a function, like same, pred, or succ.'''
    i: Addr
    j: Addr
    f: Func

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        match (self.i, self.j):
            case (Variable(), Variable()):
                for i in model.canvas.all_addrs():
                    for j in model.canvas.all_addrs():
                        if i >= j:
                            continue
                        for f in self.func_iter(self.f, model.primitive_funcs):
                            if model.are_related_by(i, j, f):
                                yield DetAddrWithSubst(
                                    Subst.make_from(
                                        (self.i, i), (self.j, j), (self.f, f)
                                    ),
                                    Indices(i, j)
                                )
            case _:
                raise NotImplementedError(
                    f"RelatedPair.to_detaddrs: can't search over ({self.i}, {self.j})"
                )

    def func_iter(self, f: Func, primitive_funcs: Iterable[DetFunc]) \
    -> Iterable[DetFunc]:
        match f:
            case Variable():
                yield from primitive_funcs
            case CallableFunc():
                yield f
            case _ if is_cell_content(f):
                yield f
            case SimpleFunc():
                yield f
            case _:
                raise NotImplementedError
            # TODO Other cases? Could a Variable be in an expression?

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

@dataclass(frozen=True)
class MatchContent(Addr):
    '''An Addr to match any sort of content in canvas cells: letters and/or
    annotations.'''
    content: CellContent

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        yield from (
            DetAddrWithSubst(subst.unify(var, index), index)
                for index in model.canvas.all_matching(self.content)
        )

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.content)})'

########## The Canvas ##########

@dataclass(kw_only=True)  # type: ignore[call-overload, misc]
class Canvas(ABC):
    MAX_CLARITY: Numeric = 6
    INITIAL_CLARITY: Numeric = 5

    """
    MAX_CLARITY: ClassVar[Numeric] = 6  # TODO move this to a dataclass or  # 5
                                        # maybe to RMem
    """
    @abstractmethod
    def all_addrs(self) -> Iterable[Index]:
        pass
    
    @abstractmethod
    def __getitem__(self, addr: Index | int) -> Optional[CellContent]:
        pass

    @abstractmethod
    def __setitem__(self, addr: Index | int, x: CellContent) -> None:
        pass

    @abstractmethod
    def has_addr(self, addr: Addr) -> bool:
        pass

    @abstractmethod
    def clarity(self, addr: Index2) -> Numeric:
        pass

    def all_clarities(self) -> Iterable[Numeric]:
        for addr in self.all_addrs():
            yield self.clarity(addr)

    @abstractmethod
    def set_clarity(self, addr: Index2, clarity: Numeric) -> None:
        pass

    @abstractmethod
    def addr_of(self, v: CanvasValue) -> Index:
        '''Returns the Index of the cell that contains v, or
        raises FizzleValueNotFound if no cell contains v.'''
        pass

    def addrs_containing_value(self, v: Value) -> Iterable[Index]:
        return (a for a in self.all_addrs() if self[a] == v)

    @abstractmethod
    def min_index(self) -> Index:
        pass

    @abstractmethod
    def max_index(self) -> Index:
        pass

@dataclass
class ContentAndClarity:
    content: CellContent1
    clarity: Numeric

    def paint(self, v: CellContent1) -> None:
#        if v is None:
#            pass
#        elif v == ' ':
#            self.dec_clarity()
        if v is None:
            self.dec_clarity()
        elif v == self.content:
            self.inc_clarity()
        else:
            if self.clarity == 0:
                self.content = v
                self.clarity = 1
            else:
                self.dec_clarity()

    def inc_clarity(self) -> None:
        if self.clarity < Canvas.MAX_CLARITY:
            self.clarity += 1

    def dec_clarity(self) -> None:
        if self.clarity > 0:
            self.clarity -= 1
        if self.clarity == 0:
            self.content = Blank()

    def set_clarity(self, clarity: Numeric) -> None:
        self.clarity = clarity
        if self.clarity == 0:
            self.content = Blank()

@dataclass
class ContentsAndClarities:
    d: Dict[Index2, ContentAndClarity] = field(default_factory=dict)
    min_index: Index = Index(1)
    max_index: Index = Index(10)  # TODO Why 10??

    def __setitem__(self, i: Index2, v: CellContent1) -> None:
        if is_index(i):  # WRONG: need to extract index from i
            pass # TODO check bounds
        if i in self.d:
            self.d[i].paint(v)
        else:
            self.d[i] = ContentAndClarity(v, 1)

    def __getitem__(self, i: Index2) -> Optional[CellContent]:
        try:
            return self.d[i].content
        except KeyError:
            return None

    @no_type_check  # crashes mypy 0.971
    def __iter__(self) -> Iterator[Tuple[Index, CellContent1]]:
        for i, v in self.d.items():
            match i:
                case int():
                    yield i, v.content
                case (int(ii), AnnotationType()):
                    yield ii, v.content

    def clarity(self, i) -> Numeric:
        try:
            return self.d[i].clarity
        except KeyError:
            return 0

    def set_clarity(self, i: Index2, clarity: Numeric) -> None:
        if i in self.d:
            self.d[i].set_clarity(clarity)

    def all_indices(self) -> Iterable[Index]:
        '''Returns all the possible indices of cell values, whether anything
        has been stored there or not. Does not include indices for
        Annotations.'''
        return Index.from_to(self.min_index, self.max_index)

    def all_indices_and_values(self) -> Iterable[Tuple[Index, CanvasValue]]:
        for i in self.all_indices():
            yield (i, self[i])  # type: ignore[misc]

    def all_indices_and_bundles(self) -> Iterable[Tuple[Index, CellBundle]]:
        results: Dict[Index, CellBundle] = dict()
        for i, v in self.d.items():
            ii = extract_index(i)
            if ii in results:
                results[ii] = results[ii] + v.content
            else:
                results[ii] = CellBundle.make_from(v.content)
        for ii in sorted(results.keys(), key=as_int):
            yield ii, results[ii]

    def as_bundle(self, i: Index) -> CellBundle:
        result = empty_cell_bundle
        for i2, cc in self.d.items():
            if extract_index(i2) != i:
                continue
            result = result + cc.content
        return result

    # TODO UT
    def all_indices2(self) -> Iterable[Index2]:
        '''Returns the indices of every element stored, both cell values (which
        have an Index) and annotations (which have (Index, AnnotationType)).'''
        return self.d.keys()

    def all_values(self) -> List[CanvasValue]:
        '''Returns a list of the value of each plain-index cell (i.e. no
        annotations).'''
        return [
            self[i] for i in self.all_indices()  # type: ignore[misc]
        ]

    def all_clarities(self) -> List[Numeric]:
        '''Returns a list of the clarity of each plain-index cell (i.e. no
        annotations).'''
        return [
            self.clarity(i) for i in self.all_indices()
        ]

    def all_matching(self, v: CellContent) -> List[Index]:
        '''Returns a list of the indices of all cells that match 'v'.
        A match is one that contains the same value as 'v' and at least all
        the annotations that 'v' has (and possibly more). If 'v' is only an
        Annotation or Annotations, then cell values don't affect the match.
        A value of None matches only None; to specify a value that matches
        any value, pass Any.'''
        return [
            i for i, bundle in self.all_indices_and_bundles()
                if bundle.is_match(v)
        ]

# TODO How do you erase an annotation?

default_auto_annotations = (Start, End, Inextreme)

@dataclass
class Canvas1D(Canvas):
    contents: ContentsAndClarities = field(
        default_factory=lambda: ContentsAndClarities()
    )

    @classmethod
    def make_from(
        cls,
        s: str, 
        auto_annotate: Iterable[Annotation]=default_auto_annotations
    ) -> Canvas1D:
        result = cls()
        result.contents.min_index = Index(1)
        result.contents.max_index = Index(len(s))
        for ii, c in zip(range(1, len(s) + 1), s):
            i = Index(ii)
            result[i] = Letter.from_str(c)
            if not is_blank(result[i]):
                result.set_clarity(i, cls.INITIAL_CLARITY)
            else:
                result.set_clarity(i, 0)
#        if auto_annotate:
#            result.auto_annotate()
        for ann in auto_annotate:
            for i in Index.from_to(   # Should be .all_indices(); move to Canvas
                result.contents.min_index,
                result.contents.max_index
            ):
                if ann.applies_here(result, i):
                    result[i] = ann
                    result.set_clarity(
                        (i, ann.type),
                        result.INITIAL_CLARITY
                    )
        return result

    def auto_annotate(self) -> None:
        for i in Index.from_to(
            self.contents.min_index,
            self.contents.max_index
        ):
            ann: Annotation
            match i:
                case self.contents.min_index:
                    ann = Start
                case self.contents.max_index:
                    ann = End
                case _:
                    ann = Inextreme
                    pass
            self[i] = ann
            self.set_clarity(
                (i, Anchor),
                self.INITIAL_CLARITY
            )

    def all_addrs(self) -> Iterable[Index]:  # TODO rename to all_indices
        return Index.from_to(
            self.contents.min_index,
            self.contents.max_index
        )

    all_indices = all_addrs

    def min_index(self) -> Index:
        return self.contents.min_index

    def max_index(self) -> Index:
        return self.contents.max_index

    def has_addr(self, addr: Addr) -> bool:
        if is_index(addr):
            return (
                addr >= self.contents.min_index
                and
                addr <= self.contents.max_index
            )
        else:
            return False

    has_index = has_addr

    def __getitem__(self, addr: Index | int) -> Optional[CellContent]:
        '''Allowing 'int' is strictly a convenience for unit testing and
        debugging.'''
        match addr:
            case int():
                return self.contents[Index(addr)]
            case i if is_index(i):
                return self.contents[addr]
            case (i, AnnotationType()) if is_index(i):
                return self.contents[addr]
            case (i, Annotation() as ann) if is_index(i):
                return self.contents[(i, ann.type)]
            case _:
                return None
        assert False, "Canvas.__getitem__(): should not go past 'match' stmt"
        return None # Needed only to please mypy; stops [return] error

    def __setitem__(self, i: Index | int, v: CellContent) -> None:
        i: Index = Index(i) if isinstance(i, int) else i
        for ii, vv in self.as_internal_args(i, v):
            if self.has_annotation(i, Immutable) and is_canvas_value(vv):
                if vv != self[i]:
                    raise FizzleImmutable(i, self[i], vv)
            self.contents[ii] = vv

    # TODO UT
    def has_letter(self, i: Index | int) -> bool:
        i: Index = Index(i) if isinstance(i, int) else i
        v = self.contents[i]
        return isinstance(v, Letter)

    def as_internal_args(self, i: Index, v: CellContent) \
    -> Iterable[Tuple[Index2, CellContent1]]:
        match v:
            case None:
                yield (i, None)
            case c if is_canvas_value(c):
                yield (i, c)
            case Annotation():
                yield ((i, v.type), v)
            case Annotations():
                for elem in v:
                    yield ((i, elem.type), elem)
            case CellBundle():
                if v.value is not None:
                    yield (i, v.value)
                yield from self.as_internal_args(i, v.annotations)
            case _:
                raise ValueError(f'as_internal_args({i!r}, {v!r}): unrecognized cell content type ({type(v)})')

    def clarity(self, i: Index2 | int) -> Numeric:
        match i:
            case int():
                return self.contents.clarity(Index(i))
            case _:
                return self.contents.clarity(i)

    def set_clarity(self, i: Index2, clarity: Numeric) -> None:
        self.contents.set_clarity(i, clarity)

    def addr_of(self, v: CanvasValue) -> Index:
        for i in self.contents.all_indices():
            if self.contents[i] == v:
                return i
        raise FizzleValueNotFound(v)

    # rename to all_matching_indices
    def all_matching(self, v: CellContent) -> List[Index]:
        return [
            i
                for i, vv in self.contents.all_indices_and_bundles()
                    if vv.is_match(v)
        ]

    def is_match(self, i: Index | int, v: CellContent) -> bool:
        i: Index = Index(i) if isinstance(i, int) else i
        return self.as_bundle(i).is_match(v)

    def has_annotation(self, i: Index | int, ann: Annotation) -> bool:
        i: Index = Index(i) if isinstance(i, int) else i
        return self.is_match(i, ann)

    def as_bundle(self, i: Index | int) -> CellBundle:
        i: Index = Index(i) if isinstance(i, int) else i
        return self.contents.as_bundle(i)

    def all_indices_and_values(self) -> Iterable[Tuple[Index, CanvasValue]]:
        return self.contents.all_indices_and_values()

    def __str__(self) -> str:
        clarities = ', '.join(str(c) for c in self.contents.all_clarities())
        return f"'{self.short_str()}'  {clarities}"

    def short(self) -> str:
        return repr(self.short_str())

    def short_str(self) -> str:
        return ''.join(
            'N' if x is None else str(x)
                for x in self.contents.all_values()
        )

    def state_str(self) -> str:
        return f"{self.short()}  {' '.join(str(c) for c in self.contents.all_clarities())}"

########## Exceptions ##########

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Any  # TODO restore type hint: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class FizzleNotIndex(Fizzle):
    e: Expr

    def __str__(self) -> str:
        return f"can't reduce to Index: {repr(self.e)}"

@dataclass(frozen=True)
class FizzleGotBlank(Fizzle):
    o: Any   # Whatever painter or func or whatever fizzled
    e: Any   # Whatever evaluated to Blank and shouldn't have
    
    def __str__(self) -> str:
        return f'{short(self.o)}: {short(self.e)} evaluated to Blank'

@dataclass(frozen=True)
class FizzleGotNone(Fizzle):
    o: Any   # Whatever painter or func or whatever fizzled
    e: Any   # Whatever evaluated to None and shouldn't have
    
    def __str__(self) -> str:
        return f'{short(self.o)}: {short(self.e)} evaluated to None'

@dataclass(frozen=True)
class FizzleNoDetPainters(Fizzle):

    def __str__(self) -> str:
        return 'No determinate painters'

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

def fizzle_if_none(x: Any, o: Any, e: str) -> None:
    if x is None:
        raise FizzleGotNone(o, e)

def fizzle_if_blank(x: Any, o: Any, e: str) -> None:
    if is_blank(x):
        raise FizzleGotBlank(o, e)

@dataclass(frozen=True)
class FizzleImmutable(Fizzle):
    i: Index
    old: Optional[CellContent]
    new: Optional[CellContent]

    def __str__(self) -> str:
        return f'Tried to overwrite Immutable cell {self.i}: old={self.old!r}, new={self.new!r}'

########## Soups ##########

@dataclass
class Soup:
    #painters: Set = field(default_factory=lambda: set())
    painters: Dict[Painter, Numeric] = field(
        default_factory=lambda: defaultdict(int)
    )  # map Painter to clarity
    #painters: List = field(default_factory=list)
    authors: Dict[Painter, Set[Painter]] = field(
        default_factory=lambda: defaultdict(set)
    )  # value is all who painted the key
    pids: Dict[Painter|int, int|Painter] = field(default_factory=dict)
    # dict of id numbers to painters and back
    nextpid: int = 1

    @classmethod
    def make_from(cls, painters: Iterable[Painter]) -> Soup:
        return Soup(defaultdict(int, ((p, 1) for p in painters)))

    def add(self, *painters: Painter) -> None:
        for p in painters:
            if p not in self.pids:
                pid = self.nextpid
                self.nextpid += 1
                self.pids[p] = pid
                self.pids[pid] = p
            self.painters[p] += 1

    def add_author(self, painter: Painter, author: Painter) -> None:
        self.authors[painter].add(author)

    # TODO UT author
    def paint(self, painter: Painter, author: Optional[Painter]=None) -> None:
        self.add(painter)
        if author is not None:
            self.add_author(painter, author)

    # TODO rm all references to painter in self.authors
    def remove(self, *painters: Painter) -> None:
        '''It is not an error to remove a painter that does not exist.'''
        for p in painters:
            if p in self.painters:
                del self.painters[p]

    def clarity(self, p: Painter) -> Numeric:
        return self.painters.get(p, 0.0)

    def decay(self, factor=0.9) -> None:
        for p in self.painters:
            self.painters[p] *= factor

    def punish(self, painter: Painter, factor=0.6) -> None:
        if painter in self.painters:
            self.painters[painter] *= factor
            for author in self.authors[painter]:
                self.punish(author, 1.0 - (1.0 - factor) * 0.8)

    def __contains__(self, p: Painter) -> bool:
        return p in self.painters

    def __iter__(self) -> Iterator[Painter]:
        return iter(self.painters)

    def clear(self) -> None:
        self.painters.clear()

# Commented out to prevent circular import (of Subst), and since nothing else
# seems to call these functions.
#    def matching_painters(self, xp: Painter) -> List[Tuple[Subst, Painter]]:
#        result = []
#        for p in self.painters:
#            subst = self.is_match(xp, p)
#            if subst:
#                result.append((subst, p))
#        return result
#
#    def is_match(self, xp: Painter, p: Painter) -> Subst:
#        '''Viewing xp as a painter template (possibly with variables that
#        need to be filled in), does p match xp?
#
#        Returning a BottomSubst() means no match.
#        '''
#        # TODO Require match of func, too
#        xi, xj, xf = xp
#        pi = as_index(p[0])
#        pj = as_index(p[1])
#        pf = p[2]
#
#        if xf == pf:
#            return empty_subst.unify(xi, pi).unify(xj, pj)
#        else:
#            return bottom_subst

    def has_painter(self, p: Painter) -> bool:
        return p in self.painters

    def choose(self) -> Painter:
        return choices(
            list(self.painters.keys()),
            list(self.painters.values())
        )[0]

    def id_to_painter(self, i: int) -> Optional[Painter]:
        result = self.pids.get(i, None)
        if isinstance(result, Painter):
            return result
        else:
            return None

    @classmethod
    def union(cls, *soups: Soup) -> Soup:
        # TODO What about clarities?
        #return Soup(union(*(soup.painters for soup in soups)))
        #return Soup(reduce(operator.add, (soup.painters for soup in soups), []))
        d: Dict[Painter, Numeric] = defaultdict(int)
        for soup in soups:
            for painter, clarity in soup.painters.items():
                d[painter] += clarity
        return Soup(d)

    def short(self) -> str:
        cl = self.__class__.__name__
        return cl

    def __str__(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(short(p) for p in self.painters)
        return f'{cl}({items})'

    def state_str(self) -> str:
        sio = StringIO()
        for pstr in sorted(
            f'{short(p)} cl={nf(cl)}' for p, cl in self.painters.items()
        ):
            print(pstr, file=sio)
        return sio.getvalue()

    def state_str_with_authors(self) -> str:
        sio = StringIO()
        for p in sorted(self, key=short):
            print(f'{short(p)}, cl={nf(self.clarity(p))}', file=sio)
            author_set = self.authors.get(p, None)
            if author_set:
                print(f'  by: {short(author_set)}', file=sio)
        return sio.getvalue()

    __repr__ = state_str

########## Substitutions ##########

@dataclass(frozen=True)
class Subst:
    '''A substitution table, i.e. a mapping from Variables to Exprs.'''
    d : PMap[Expr, Index] = field(default_factory=lambda: pmap())

    def __bool__(self) -> bool:
        return True

    @classmethod
    def make_from(cls, *pairs: Tuple[Expr, Expr]) -> Subst:
        result = cls()
        for lhs, rhs in pairs:
            if isinstance(rhs, int): # HACK for convenience in UTs and the REPL
                result = result.unify(lhs, Index(rhs))
            else:
                result = result.unify(lhs, rhs)
        return result

    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def __contains__(self, x: Hashable) -> bool:
        return x in self.d

    def __getitem__(self, x: Hashable) -> Optional[Any]:  #TODO proper type hint
        return self.d.get(x, None)

    def simplify(self, expr: Expr) -> Expr:
        match expr:
            case int():
                return expr
            case Plus():
                return expr.value_of(self)
            case _:
                match self.d.get(expr, None):
                    case None:
                        return expr
                    case v:
                        return self.simplify(v)

    # TODO rm?
    def as_index_fizzle(self, expr: Expr) -> Index:
        '''Same as .simplify() but Fizzles if the result is not an Index.'''
        result = self.simplify(expr)
        if isinstance(result, int):
            return Index(result)
        else:
            raise FizzleNotIndex(expr)

    def as_index(self, expr: Expr) -> Optional[Index]:
        '''Same as .simplify() but returns None if the result is not an
        Index.'''
        result = self.simplify(expr)
        match result:
            case int():
                return Index(result)
            case Index():
                return result
            case _:
                return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        if isinstance(lhs, int):
            self.raise_int_error(lhs)
        if isinstance(rhs, int):
            self.raise_int_error(rhs)
        with indent_log(8, 'UNIFY', lhs, rhs):
            match (lhs, rhs):
                case (x, y) if x == y:
                    return self
                case (x, Plus(args)) | (Plus(args), x) if len(args) == 1:
                    return self.unify(x, args[0])
                case (Variable(), Index()):
                    return self.substitute(lhs, rhs).set_lhs_rhs(lhs, rhs)
                case (Variable(), Indices()):
                    return self.set_lhs_rhs(lhs, rhs)
                    # WRONG Need to simplify the elements of Indices
                    # And need to substitute().
                case (Index(), Index()):
                    return bottom_subst  # we already know they're not equal
                case (Indices(), Indices()):
                    return bottom_subst  # we already know they're not equal
                case (Index(), Indices()):
                    return self if Indices(lhs) == rhs else bottom_subst
                case (Indices(), Index()):
                    return self if lhs == Indices(rhs) else bottom_subst
                case (Index(), Plus()):
                    return self.unify(rhs, lhs)
                case (Index(), Plus()):
                    return self.unify(rhs, lhs)
                case (Plus(args=(Variable() as v, int(n))), Index(r)):
                    lo(9, 'GOTPP', v, n, r)
                    match self.simplify(v):
                        case Index(vv):
                            return self.unify(Index(vv + n), Index(r))
                        case Variable() as var:
                            return self.unify(var, Index(r - n))
                        case None: # Is this still necessary?
                            return self.set_lhs_rhs(v, Index(r - n))
                            #return Subst(self.d.set(v, r - n))
                        case _:
                            lo("Unimplemented Plus() unification:", lhs, type(lhs), ' with ', rhs, type(rhs), self.simplify(v))
                            raise NotImplementedError((lhs, rhs))
                case (Variable(), Plus() as rator):
                    #print('GOTVP')
                    rvalue = rator.value_of(self)
                    #if lhs in self.
                    return self # TODO
                case (Variable(), Variable()) | (Plus(), Plus()):
                    lhsimple = self.simplify(lhs)
                    rhsimple = self.simplify(rhs)
                    if lhsimple != lhs or rhsimple != rhs:
                        return self.unify(lhsimple, rhsimple)
                        # Could this make an infinite loop?
                    else:
                        return self.set_lhs_rhs(lhs, rhs)
                case (Variable(), SoupRef()):
                    if lhs in self.d:
                        if self.d[lhs] == rhs:
                            return self
                        else:
                            return bottom_subst
                    else:
                        return Subst(self.d.set(lhs, rhs))
                case (Variable(), (SoupRef(), p)) if (
                        isinstance(p, Painter)  # type: ignore[has-type]
                ):
                    if lhs in self.d:
                        if self.d[lhs] == rhs:
                            return self
                        else:
                            return bottom_subst
                    else:
                        return Subst(self.d.set(lhs, rhs))
                case (Variable(), (i, j, f)):
                    (ii, jj, ff) = (
                        self.simplify(i),
                        self.simplify(j),
                        self.simplify(f)
                    )
                    if (
                        occurs_in(lhs, ii)
                        or
                        occurs_in(lhs, jj)
                        or
                        occurs_in(lhs, ff)
                    ):
                        return bottom_subst
                    else:
                        return self.set_lhs_rhs(lhs, (ii, jj, ff))
                        # Is that wrong? What if ii, etc. have an expr that
                        # needs to be evaluated later?
                case (Variable(), f) if is_func(f):
                    if lhs in self.d:
                        if self.d[lhs] == rhs:
                            return self
                        else:
                            return bottom_subst
                    else:
                        return Subst(self.d.set(lhs, rhs))
                case (Variable(), SpecialAddr()):
                    return bottom_subst
                case (Variable(), _):
                    return bottom_subst
                case (SimpleFunc(var), rhs):
                    if isinstance(rhs, SimpleFuncClass):
                        return self.set_lhs_rhs(var, rhs)
                    else:
                        return bottom_subst
                case (f, g) if callable(f) and callable(g):
                    if f == g:
                        return self
                    else:
                        return bottom_subst
                case (f, _) if callable(f):  # BUG If _ is var, should unify
                    return bottom_subst
                case (Painter(i1, j1, f1), Painter(i2, j2, f2)):
                    # We define I, J, F to be i2, j2, f2 unless I, J, F
                    # get unified in the course of unifying i1 with i2, etc.
                    result = self.unify(i1, i2)
                    if I not in result:
                        result = result.unify(I, i2)

                    result = result.unify(j1, j2)
                    if J not in result:
                        result = result.unify(J, j2)

                    result = result.unify(f1, f2)
                    if F not in result:
                        result = result.unify(F, f2)
                    return result
                case (x, Variable()):
                    return self.unify(rhs, x)
                case (x, SoupRef()):
                    return bottom_subst
                case (CallableFunc(), CallableFunc()):
                    return bottom_subst  # we already know they're not equal
                case _:
                    lo("Unimplemented unification:", lhs, type(lhs), ' with ', rhs, type(rhs))
                    raise NotImplementedError((lhs, rhs))
            assert False, "unify(): should not go past 'match' stmt"
            return self # Needed only to please mypy; stops [return] error

    def raise_int_error(self, x: Any) -> None:
        raise ValueError(f'tried to unify with integer ({x}); should be Index()')

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs in self.d:
            if self.d[lhs] == rhs:
                return self
            else:
                return bottom_subst
        else:
            return Subst(self.d.set(lhs, rhs))

    def unify_if_undefined(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs not in self:
            return self.unify(lhs, rhs)
        else:
            return self

    def unify_ijf(self, source: Addr, target: Addr, func: Func) -> Subst:
        return self. \
            unify_if_undefined(I, source). \
            unify_if_undefined(J, target). \
            unify_if_undefined(F, func)

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

    def substitute(self, lhs: Expr, rhs: Expr) -> Subst:
        '''Returns a Subst in which every occurence of 'lhs' has been replaced
        by 'rhs'.'''
        result = empty_subst
        for l, r in self.d.items():
            result = result.unify(
                expr_substitute(l, lhs, rhs),
                expr_substitute(r, lhs, rhs)
            )
            if not result:
                return bottom_subst
        return result

def expr_substitute(e: Expr, lhs: Expr, rhs: Expr) -> Expr:
    '''Returns a new Expr consisting of e where every occurrence of lhs has
    been replaced by rhs.'''
    match e:
        case x if x == lhs:
            return rhs
        case Plus(args):
            return Plus(*(expr_substitute(a, lhs, rhs) for a in args))
        case (i, j, f):
            return (
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case RelatedPair(i, j, f):
            return RelatedPair(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case MakeBetweenPainter(i, j, f):
            return MakeBetweenPainter(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        # TODO UT
        case MakeRelativeIndirectPainter(i, j, f):
            return MakeRelativeIndirectPainter(
                expr_substitute(i, lhs, rhs),
                expr_substitute(j, lhs, rhs), 
                expr_substitute(f, lhs, rhs)
            )
        case _:  # no match; nothing to substitute
            return e

def occurs_in(var: Variable, expr: Expr) -> bool:
    match expr:
        case Variable():
            return var == expr
        case (i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case Plus(args):
            return any(occurs_in(var, a) for a in args)
        case RelatedPair(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case MakeBetweenPainter(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case MakeRelativeIndirectPainter(i, j, f):
            return occurs_in(var, i) or occurs_in(var, j) or occurs_in(var, f)
        case _:
            return False

class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def simplify(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        return self

empty_subst = Subst()
bottom_subst = BottomSubst()

########## Functions ##########

@dataclass(frozen=True)
class SimpleFunc:
    '''A function like succ, pred, or same.'''
    var: Variable

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.var})'

Func = Union[CallableFunc, CellContent, Variable, SimpleFunc]

def is_func(x: Any) -> TypeGuard[Func]:
    if isinstance(x, CallableFunc):
        return True
    elif is_cell_content(x):
        return True
    else:
        return False

# A determinate Func: no variables
#DetFunc = Union[Value, DetPainter, Callable] 
DetFunc = Union[CallableFunc, CellContent, SimpleFunc] # TODO Allow SimpleFunc?

# A convenience type for DetPainter.make_from()
ToDetFunc = Union[DetFunc | str, Tuple[ToAddr, ToAddr, DetFunc | str]]

@no_type_check  # causes mypy 0.971 to crash
def make_detfunc(f: ToDetFunc) -> DetFunc:
    match f:
        case (i, j, ff):
            return Painter(make_addr(i), make_addr(j), make_detfunc(ff))
        case str():
            return Letter.from_str(f)
        case _:
            return f

class SimpleFuncClass(CallableFunc):
    '''A Func that gets counted as a SimpleFunc during unification.'''

    @abstractmethod
    def apply(self, model: Model, subst: Subst, v: Value) -> Value:
        pass

    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        yield self

    def __eq__(self, other: Any) -> bool:
        return self.__class__ == other.__class__

    def __hash__(self):
        return hash(self.__class__)

### The basic relational functions

class Same(SimpleFuncClass):

    def apply(self, model: Model, subst: Subst, v: Value) -> Value:
        return v

    def __repr__(self) -> str:
        return 'same'

class Succ(SimpleFuncClass):

    def apply(self, model: Model, subst: Subst, v: Value) -> Value:
        # TODO Deal with 'z'
        if isinstance(v, Letter):
            return v.succ()
# TODO succ(Index)
#        elif isinstance(v, int):
#            return v + 1
        raise Fizzle(f"succ: Can't take successor of {v}")

    def __repr__(self) -> str:
        return 'succ'

class Pred(SimpleFuncClass):

    def apply(self, model: Model, subst: Subst, v: Value) -> Value:
        # TODO Deal with 'a'
        if isinstance(v, Letter):
            return v.pred()
# TODO pred(Index)
#        elif isinstance(v, int):
#            return v - 1
        raise Fizzle(f"pred: Can't take predecessor of {v}")

    def __repr__(self) -> str:
        return 'pred'

same = Same()
succ = Succ()
pred = Pred()

### The constant function

@dataclass(frozen=True)
class const(SimpleFuncClass):
    v: Any  # TODO restore type hint: Value

    def apply(self, model: Model, subst: Subst, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.v)})'

### Painter-building functions

@dataclass(frozen=True)
class MakeBetweenPainter(CallableFunc):
    '''Makes a painter that paints a value between two others.'''
    i: Addr
    j: Addr
    f: Func

    @no_type_check
    def apply(self, model: Model, subst: Subst, ignored: Value) \
    -> Painter:
        result_i = subst.as_index(self.i)
        fizzle_if_none(result_i, self, f'i={self.i}')
        fizzle_if_blank(model.canvas[result_i], self, f'canvas[{result_i}]')

        value = model.canvas[result_i + 1]
        fizzle_if_none(value, self, f'canvas[{result_i + 1}]')
        fizzle_if_blank(value, self, f'canvas[{result_i + 1}]')

        result_j = subst.as_index(self.j)
        fizzle_if_none(result_j, self, f'j={self.j}')
        fizzle_if_blank(model.canvas[result_j], self, f'canvas[{result_j}]')

        result_f = subst[self.f]
        fizzle_if_none(result_f, self, f'f={self.f}')
        assert is_func(result_f)

        return Painter(
            Painter(I, Plus(I, result_j - result_i), result_f),
            SR.WorkingSoup,
            Painter(I, Plus(I, 1), value)
        )

    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        if self.can_make(model, subst):
            yield self

    # TODO UT
    def can_make(self, model: Model, subst: Subst) -> bool:
        result_i = subst.as_index(self.i)
        if (
            result_i is None
            or
            model.canvas[result_i] is None
            or
            model.canvas[result_i] == ' '
            or
            model.canvas[result_i + 1] is None
            or
            model.canvas[result_i + 1] == ' '
        ):
            return False
        result_j = subst.as_index(self.j)
        if (
            result_j is None
            or
            model.canvas[result_j] is None
            or
            model.canvas[result_j] == ' '
        ):
            return False
        result_f = subst[self.f]
        if result_f is None:
            return False
        return True

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short

@dataclass(frozen=True)
class MakeRelativeIndirectPainter(CallableFunc):
    i: Addr
    j: Addr
    f: Func

    def apply(self, model: Model, subst: Subst, ignored: Value) \
    -> Painter:
        result_i = subst.as_index(self.i)
        if result_i is None:
            raise Fizzle  # TODO More-specific Fizzle
        #value = model.canvas[result_i]
        bundle = model.canvas.as_bundle(result_i)
        if bundle is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_j = subst.as_index(self.j)
        if result_j is None:
            raise Fizzle  # TODO More-specific Fizzle
        result_f = subst[self.f]
        assert is_func(result_f)
        if result_f is None:
            raise Fizzle  # TODO More-specific Fizzle
        content = exclude_unmatchable(bundle.simplest())
        if not content:
            raise Fizzle  # TODO more-specific Fizzle
        return Painter(
            MatchContent(content),
            SR.WorkingSoup,
            Painter(I, Plus(I, result_j - result_i), result_f)
        )

    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        if self.can_make(model, subst):
            yield self

    # TODO UT
    def can_make(self, model: Model, subst: Subst) -> bool:
        result_i = subst.as_index(self.i)
        if (
            result_i is None
            or
            model.canvas[result_i] is None
            or
            model.canvas[result_i] == ' '
        ):
            return False
        bundle = model.canvas.as_bundle(result_i)
        if bundle is None:
            return False
        result_j = subst.as_index(self.j)
        if (
            result_j is None
            or
            model.canvas[result_j] is None
            or
            model.canvas[result_j] == ' '
        ):
            return False
        result_f = subst[self.f]
        if result_f is None:
            return False
        return True

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.i)}, {short(self.j)}, {short(self.f)})'

    __str__ = short

########## Painters ##########

@dataclass(frozen=True)
class Painter(Addr, CallableFunc):
    source: Addr
    target: Addr
    func: Func

    @classmethod
    def make_from(cls, i: ToAddr, j: ToAddr, f: ToDetFunc) -> Painter:
        '''An easy way to construct a Painter in a unit test. Not for use
        in the model proper.'''
        return Painter(make_addr(i), make_addr(j), make_detfunc(f))

    def as_triple(self) -> Tuple[Addr, Addr, Func]:
        return (self.source, self.target, self.func)

    def to_detaddrs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetAddrWithSubst]:
        for painter in model.soups():
            #lo('TODETA', self, painter)
            subst2 = subst.unify(self, painter)
            if subst2:
                #lo('GOTA', DetAddrWithSubst(subst2, painter))
                yield DetAddrWithSubst(subst2, painter)

    def to_detpainters(self, model: Model) -> Iterable[DetPainter]:
        #source, target, func = p

        #det_sources = list(self.source.to_detaddrs(model, empty_subst, I))
        det_sources = list(model.addr_to_detaddrs(empty_subst, I, self.source))
        with indent_log(8, 'DET SOURCES'):
            for det_source in det_sources:
                lo(8, det_source)

        source_target_pairs = (
            (ds, dt)
                for ds in det_sources
                    #for dt in self.addr_to_detaddrs(ds.subst, J, self.target)
                    for dt in self.target.to_detaddrs(model, ds.subst, J)
        )
        source_target_pairs = list(source_target_pairs) #DEBUG
        #lo(8, 'STPAIRS', list(zip(*source_target_pairs)))
        #lo(8, 'DETTARG', source_target_pairs[1])
        with indent_log(8, 'DET SOURCES+TARGETS'):
            for source_target_pair in source_target_pairs:
                lo(8, source_target_pair)

        triples = (
            (ds, dt, df)
                for (ds, dt) in source_target_pairs
                    #for df in self.func_to_detfuncs(dt.subst, F, self.func)
                    #for df in self.func.to_detfuncs(model, dt.subst, F)
                    for df in model.func_to_detfuncs(dt.subst, F, self.func)
        )
        triples = list(triples) #DEBUG
        #lo(7, 'DETFUNCS', triples)  # we really want the 3rd "column"
        #lo(7, 'DETFUNCS', triples[2])
        with indent_log(7, 'DET TRIPLES'):
            for triple in triples:
                lo(7, triple)

        for ds, dt, df in triples:
            dp = DetPainter(
                dt.subst,
                ds.addr,
                dt.addr,
                df,
                model.suppression((ds.addr, dt.addr, df)),
                    # TODO figure painter clarity into prob_weight?
                self  # basis, "author"
            )
            lo(5, dp)
            yield dp

    def to_detfuncs(self, model: Model, subst: Subst, var: Variable) \
    -> Iterable[DetFunc]:
        for i, j, f in product(
            #self.source.to_detaddrs(model, subst, I),
            #self.target.to_detaddrs(model, subst, J),
            model.addr_to_detaddrs(subst, I, self.source),
            model.addr_to_detaddrs(subst, J, self.target),
            model.func_to_detfuncs(subst, F, self.func),
        ):
            yield Painter(i.addr, j.addr, f)

    def apply(self, model: Model, subst: Subst, value: Value) \
    -> Value:
        return self

    def __repr__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.source)}, {short(self.target)}, {short(self.func)})'

    def short(self) -> str:
        return f'({short(self.source)}, {short(self.target)}, {short(self.func)})'

@dataclass(frozen=True)
class CPainter(Painter):
    '''A canvas-painter.'''
    pass

@dataclass(frozen=True)
class PPainter(Painter):
    '''A painter-painter.'''
    pass


@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    subst: Subst
    source: DetAddr
    target: DetAddr
    func: DetFunc
    prob_weight: Numeric
    basis: Optional[Painter] = None  # what this DetPainter was made from

    @classmethod
    def make_from(cls, triple: Tuple[ToDetAddr, ToDetAddr, ToDetFunc]) \
    -> DetPainter:
        '''An easy way to construct a DetPainter in a unit test. Not for use
        in the model proper.'''
        source = make_detaddr(triple[0])
        target = make_detaddr(triple[1])
        func = make_detfunc(triple[2])
        return cls(
            empty_subst.unify_ijf(source, target, func),
            source,
            target,
            func,
            1,
            None
        )

    def is_valid_for(self, canvas: Canvas) -> bool:
        match (self.source, self.target):
            case (int(), int()):
                return (
                    canvas.has_addr(self.source)
                    and
                    canvas.has_addr(self.target)
                )
            case _:
                return True

    # TODO rename to as_triple()
    def as_painter(self) -> DetPainter0:
        return (self.source, self.target, self.func)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.as_painter()):80s}; {short(self.subst):30s}; pw={nf(self.prob_weight)})'

    def __str__(self) -> str:
#        sio = StringIO()
#        print(short(self.as_painter()), file=sio)
#        print(short(self.subst), file=sio)
#        print(nf(self.prob_weight), file=sio)
#        print(f'basis={short(self.basis)}', file=sio)
#        return sio.getvalue()
        pstr = short(self.as_painter())
        sstr = short(self.subst)
        bstr = f'basis={short(self.basis)}'
        return f'{pstr}; {sstr}; pw={nf(self.prob_weight)}; {bstr}'

# A determinate painter. Unlike DetPainter, a DetPainter0 includes only the
# minimal painter info, not additional information like a Subst.
DetPainter0 = Tuple[DetAddr, DetAddr, DetFunc]
Value = Union[CellContent, Painter, Func, None]

########## The Model ##########

default_primitive_funcs: FrozenSet[DetFunc] = frozenset([same, succ, pred])
default_initial_painters: List[Painter] = [
    Painter(RelatedPair(I, J, F), SR.WorkingSoup, Painter(I, J, F)),
    Painter(Painter(I, J, SimpleFunc(F)), SR.WorkingSoup, MakeRelativeIndirectPainter(I, J, F)),
    Painter(Painter(I, Plus(I, 2), F), SR.WorkingSoup, MakeBetweenPainter(I, J, F))
]

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup.make_from(
        default_initial_painters
    ))
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    primitive_funcs: FrozenSet = default_primitive_funcs
    t: int = 0   # current timestep
    suppressions: Dict[DetPainter0, float] = field(default_factory=dict)
    dps_run: Dict[int, DetPainter] = field(default_factory=dict)
        # DetPainters run: a record of all the DetPainters that this model
        # has run, and in what timestep.
    canvas_history: Dict[int, Canvas1D] = field(default_factory=dict)
    ws_history: Dict[int, Soup] = field(default_factory=dict)
    auto_annotate: Iterable[Annotation] = default_auto_annotations
    high_weight_favoritism: Numeric = 0.6 
        # For adjusting probability weights # to favor the highest weights.
        # 0 => all weights become (nearly) 1.0
        # 0.5 => weights are unchanged
        # 1.0 => all weights except the highest become 0.0

    @classmethod
    def canvas_from(cls, s: str) -> Model:
        return cls(canvas=Canvas1D.make_from(s))

    @classmethod
    def make_from(
        cls,
        *args,
        auto_annotate: Iterable[Annotation]=default_auto_annotations,
        **kwargs
    ) -> Model:
        match len(args):
            case 0:
                pass
            case 1:
                if isinstance(args[0], str):
                    canvas = Canvas1D.make_from(
                        args[0],
                        auto_annotate=auto_annotate
                    )
                    kwargs['canvas'] = canvas
            case _:
                raise ValueError(
                    f'Can only pass a string to Model.make_from(); got {repr(args)}'
                )
        return cls(auto_annotate=auto_annotate, **kwargs)

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s, auto_annotate=self.auto_annotate)

    def contents_at(self, addr: Addr) -> Optional[Value]:
        #TODO Look up a painter by addr?
        if isinstance(addr, Index):
            return self.canvas[addr]
        else:
            return None

    def paint(self, a: Index, x: CellContent) -> None:
        self.canvas[a] = x

    def absorb(self, s: str, timesteps: int=20):
        with indent_log(2, 'ABSORB', repr(s)):
            self.clear_history()
            self.set_canvas(s)
            for i in self.canvas.all_indices():
                if not is_blank(self.canvas[i]):
                    self.canvas[i] = Immutable
                    #self.canvas.set_clarity(i, 3) # TODO should be 1
            # TODO Set a mode where painters get penalized for painting the
            # wrong things
            # Run a little while, let some painters develop
            for t in range(timesteps):
                self.do_timestep()
                lo(2, self.state_str())
            # Save the abstract painters to the lts
            for p in self.ws:
                if self.is_absorbable(p):
                    self.lts.add(p)

    def is_absorbable(self, painter: Painter) -> bool:
        match painter:
            case Painter(_, SoupRef(), _):
                return True
            case Painter(MatchContent(), _, _):
                return True
            case _:
                return False

    def regen_from(self, s: str, nsteps: int=40) -> None:
        '''Regenerates canvas starting from 's'.'''
        if nsteps <= 0:
            return
        with indent_log(1, 'REGENERATE from', repr(s)):
            self.ws.clear()
            self.clear_suppressions()
            self.dps_run.clear()
            self.clear_history()
            with indent_log(3, 'LONG-TERM SOUP'):
                lo(3, self.lts.state_str())
            self.set_canvas(s)
            #lo(1, list(self.canvas.all_indices_and_values())) #DEBUG
            self.t = 0
            self.save_into_history()
            for t in range(nsteps):
                self.do_timestep()
                lo(1, self.state_str())

    def do_timestep(self) -> None:
        self.t += 1
        self.ws.decay()
        self.decay_suppressions()
        lo(1, f't={self.t}')
        dp = self.choose_detpainter(self.soups())
        try:
            self.run_detpainter(dp)
        except Fizzle as exc:
            lo(3, 'FIZZLE', exc)
            self.suppress(dp.as_painter())
            if dp.basis is not None:
                self.ws.punish(dp.basis)
        self.suppress(dp.as_painter())
        self.save_into_history()

    def save_into_history(self) -> None:
        '''Saves the current canvas and working soup into the history.'''
        self.canvas_history[self.t] = deepcopy(self.canvas)
        self.ws_history[self.t] = deepcopy(self.ws)

    def clear_history(self) -> None:
        self.canvas_history.clear()
        self.ws_history.clear()

    def history_of(self, pid: int) -> List[Tuple[int, Numeric]]:
        '''Returns the history of the painter in the working soup with id
        'pid'. Each item in the history is an ordered pair (t, cl) where t is
        the timestep and cl is the painter's clarity at the end of that
        timestep.'''
        painter = self.ws.id_to_painter(pid)
        lo('HOP', painter)
        if painter is not None:
            result: List[Tuple[int, Numeric]] = []
            for t, hws in self.ws_history.items():
                result.append((t, hws.clarity(painter)))
            return result
        else:
            raise ValueError(f'no painter has id {pid}')

    def choose_detpainter(self, soup: Soup) -> DetPainter:
        det_painters = list(chain.from_iterable(
            self.painter_to_detpainters(p)  #, soup.clarity(p))
                for p in soup
        ))
        weights = self.adjusted_weights([
            self.detpainter_to_probability_weight(dp)
                for dp in det_painters
        ])
        with indent_log(4, 'DETPAINTERS'):
            if det_painters:
                for w, dp in sorted(
                    zip(weights, det_painters), key=itemgetter(0)
                ):
                    #lo(4, nf(w), dp)
                    lo(4, f'{w:0.8f}', dp)
#                for k in range(len(det_painters)):
#                    lo(4, nf(weights[k]), det_painters[k])
            else:
                lo(4, 'No det_painters.')

        if det_painters:
            ii = choices(range(len(det_painters)), weights)[0]
            dp = det_painters[ii]
            lo(4, 'CHOSE DETPAINTER', dp, '  ', nf(weights[ii]))
            return dp
        else:
            raise FizzleNoDetPainters

    def detpainter_to_probability_weight(self, dp: DetPainter) -> Numeric:
        return (
            self.source_weight(dp.source)
            *
            self.target_weight(dp.target)
            *
            dp.prob_weight
        )

    def adjusted_weights(self, weights: List[Numeric]) -> List[Numeric]:
        return [
            w ** (2 ** (10 * (2 * self.high_weight_favoritism - 1)))
                for w in rescale(weights)
        ]

    def suppress(self, dp0: DetPainter0) -> None:
        if dp0 in self.suppressions:
            self.suppressions[dp0] *= 0.5
        else:
            self.suppressions[dp0] = 0.1
        lo(7, 'SUPPRESS', dp0, self.suppressions[dp0])

    def suppression(self, dp0: DetPainter0) -> float:
        return self.suppressions.get(dp0, 1.0)

    def decay_suppressions(self) -> None:
        new_suppressions: Dict[DetPainter0, float] = {}
        for dp0, sup in self.suppressions.items():
            new_sup = sup * 1.1
            if new_sup < 1.0:
                new_suppressions[dp0] = new_sup
        self.suppressions = new_suppressions

    def clear_suppressions(self) -> None:
        self.suppressions.clear()

    def source_weight(self, a: DetAddr) -> Numeric:
        match a:
            case Index() | int():
                return self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case Painter():
                return 0.5
            case Indices(elems):
                return sum(
                    self.source_weight(elem)
                        for elem in elems
                            if isinstance(elem, Index) #HACK
                )
        assert False, "source_weight(): should not go past 'match' stmt"

    def target_weight(self, a: DetAddr) -> Numeric:
        match a:
            case Index() | int():
                return 1.0 - self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case SoupRef():
                return 0.5
            case Painter():
                return 0.5
        assert False, "target_weight(): should not go past 'match' stmt"

    def run_detpainter(
        self,
        dp: DetPainter
    ) -> None:
        with indent_log(3, 'RUN_DETPAINTER', str(dp)):
            self.dps_run[self.t] = dp
            v = self.apply_func(dp.subst, dp.func, self.contents_at(dp.source))
            match dp.target:
                case Index():
                    lo(3, 'PAINT', v, 'in cell', dp.target)
                    self.canvas[dp.target] = v  # type: ignore[assignment]
                case SR.WorkingSoup:
                    match v:
                        case Painter():
                            lo(3, 'MAKE PAINTER', v)
                            self.ws.paint(v, author=dp.basis)
                        case x:
                            raise ValueError(f'run_detpainter: try to paint {x} (type {type(x)}) to the workspace.')
                #TODO Painting to long-term soup
                case _:
                    raise NotImplementedError(f"run_detpainter: can't paint to target; dp={dp}")

    def ldp(self) -> Optional[DetPainter]:
        '''The last DetPainter run (if any).'''
        return self.dps_run.get(self.t, None)

    def see_ldps(self) -> None:
        '''Prints all the DetPainters and their timesteps.'''
        for t in sorted(self.dps_run.keys()):
            print(f't={t}\n{self.dps_run[t]}')

    def see_canvas_history(self) -> None:
        '''Prints the canvas history.'''
        for t in sorted(self.canvas_history.keys()):
            print(f't={t:4}  {self.canvas_history[t].state_str()}')

    def painter_to_detpainters(self, p: Painter) -> Iterable[DetPainter]:
        yield from p.to_detpainters(self)
#        with indent_log(5, 'PAINTER to DETPAINTERS', p):
#            source, target, func = p
#
#            det_sources = self.addr_to_detaddrs(empty_subst, I, source)
#            det_sources = list(det_sources) #DEBUG
#            #lo(8, 'DETSRC', det_sources)
#            with indent_log(8, 'DET SOURCES'):
#                for det_source in det_sources:
#                    lo(8, det_source)
#
#            source_target_pairs = (
#                (ds, dt)
#                    for ds in det_sources
#                        for dt in self.addr_to_detaddrs(ds.subst, J, target)
#            )
#            source_target_pairs = list(source_target_pairs) #DEBUG
#            #lo(8, 'STPAIRS', list(zip(*source_target_pairs)))
#            #lo(8, 'DETTARG', source_target_pairs[1])
#            with indent_log(8, 'DET SOURCES+TARGETS'):
#                for source_target_pair in source_target_pairs:
#                    lo(8, source_target_pair)
#
#            triples = (
#                (ds, dt, df)
#                    for (ds, dt) in source_target_pairs
#                        for df in self.func_to_detfuncs(dt.subst, F, func)
#            )
#            triples = list(triples) #DEBUG
#            #lo(7, 'DETFUNCS', triples)  # we really want the 3rd "column"
#            #lo(7, 'DETFUNCS', triples[2])
#            with indent_log(7, 'DET TRIPLES'):
#                for triple in triples:
#                    lo(7, triple)
#
#            for ds, dt, df in triples:
#                dp = DetPainter(
#                    dt.subst,
#                    ds.addr,
#                    dt.addr,
#                    df,
#                    self.suppression((ds.addr, dt.addr, df)),
#                        # TODO figure painter clarity into prob_weight?
#                    p  # basis, "author"
#                )
#                lo(5, dp)
#                yield dp

    # TODO Rename to addr_to_detaddrs_with_subst
    def addr_to_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[DetAddrWithSubst]:
        with indent_log(6,
            'ADDR to DETADDRS',
            addr,
            f'(in Subst: {subst[addr]})' if addr in subst else '(not in Subst)',
            subst
        ):
            yield from addr.to_detaddrs(self, subst, var)
#            match addr:
#                case Index():
#                    yield DetAddrWithSubst(subst.unify(var, addr), addr)
#                case _ if is_cell_content(addr):
#                    yield from (
#                        DetAddrWithSubst(subst.unify(var, index), index)
#                            for index in self.canvas.all_matching(addr)
#                    )
#                case MatchContent():
#                    lo('HERE1', addr)
#                    yield from (
#                        DetAddrWithSubst(subst.unify(var, index), index)
#                            for index in self.canvas.all_matching(addr.content)
#                    )
#                case Variable():
#                    if addr in subst:
#                        yield from (
#                            self.addr_to_detaddrs(subst, var, subst.simplify(addr))
#                        )
#                    else:
#                        yield from (
#                            DetAddrWithSubst(
#                                subst.unify(var, index).unify(addr, index),
#                                index
#                            )
#                                for index in self.canvas.all_addrs()
#                        )
#                case Plus():
#                    match subst.as_index(addr):
#                        case None:
#                            return
#                        case int() as index:
#                            yield DetAddrWithSubst(subst.unify(var, index), index)
#                        case _:
#                            raise NotImplementedError(f"Can't match Plus that simplifies to {addr}, {type(addr)}")
#                case RelatedPair():
#                    #yield from addr.to_detaddrs(self.canvas, self.primitive_funcs)
##                    yield from addr.to_detaddrs(
##                        self, self.canvas, self.primitive_funcs
##                    )
#                    yield from addr.to_detaddrs(
#                        self, subst, var
#                    )
#                case SoupRef():
#                    yield DetAddrWithSubst(subst, addr)
#                case Painter():
#                    for painter in self.soups():
#                        pi, pj, pf = painter.as_triple()
#                        subst2 = subst.unify(addr, painter)
#                        if subst2:
#                            yield DetAddrWithSubst(subst2, painter)
#                case _:
#                    raise NotImplementedError(
#                        f'Addr {addr} has unknown type {type(addr)}.'
#                    )

    def soups(self) -> Soup:
        return Soup.union(self.lts, self.ws)

    def func_to_detfuncs(self, subst: Subst, var: Variable, func: Func) \
    -> Iterable[DetFunc]:  # TODO Create an appropriate type
        with indent_log(6, 'FUNC to DETFUNCS', func, subst):
            match func:
                case Variable():
                    if func in subst:
                        yield subst[func]  # type: ignore[misc]
                    else:
                        yield from self.primitive_funcs
                case x if is_cell_content(x):
                    yield x
                case CallableFunc():
#                    yield from func.to_detfuncs(self, subst, var)
#                    if self.can_make_func(func, subst):
#                        yield func
                    for f in func.to_detfuncs(self, subst, var):
                        if self.can_make_func(f, subst):
                            yield f
                case _:  #SimpleFunc():
                    raise NotImplementedError(func)

    # TODO rm? Funcs that need to test this can do it themselves inside their
    # own .to_detfuncs() method.
    def can_make_func(self, func: Func, subst: Subst) -> bool:
        if hasattr(func, 'can_make'):
            return func.can_make(self, subst)  # type: ignore[union-attr]
        else:
            return True

    def are_related_by(self, i: Index, j: Index, f: DetFunc) -> bool:
        if not (self.canvas.has_letter(i) and self.canvas.has_letter(j)):
            return False
        # TODO take the Subst as an argument to are_related_by
        try:
            return self.apply_func(empty_subst, f, self.canvas[i]) == self.canvas[j]
        except FizzleCantGoThere:
            return False

    def apply_func(self, subst: Subst, f: DetFunc, v: Value) \
    -> Union[Value]:
        with indent_log(6,
            f'APPLY FUNC {short(f)}({short(v, inside=True)})  {short(subst)}'
        ):
            if isinstance(f, CallableFunc):
                return f.apply(self, subst, v)
            elif is_cell_content(f):
                return f
#            elif isinstance(f, str) or isinstance(f, int) or is_painter(f):
#                return f
            else:
                raise NotImplementedError(f"apply_func: can't apply {f}")

    def state_str(self) -> str:
        sio = StringIO()
        print('canvas:', self.canvas.state_str(), file=sio)
        print(self.ws.state_str(), file=sio)
        return sio.getvalue()

    def __str__(self) -> str:
        return self.__class__.__name__

    __repr__ = state_str
