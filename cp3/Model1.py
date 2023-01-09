# ABANDONED 13-Dec-2022
# Model.py -- The canvas-and-painters model

# This version was an attempt to make Loop objects to dynamically construct
# loops nested to any depth. I gave this up when I realized that painters
# could search for places to paint with one loop nested to a depth of two.

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from pyrsistent import pmap
from pyrsistent.typing import PMap

from Log import lo, trace
from util import short

########## Probably need to give these proper definitions ##########

Addr = Any
Expr = Any
Index = Any

########## Variables ##########

@dataclass(frozen=True)
class Variable:
    name: str

    def __repr__(self) -> str:
        return self.name

I = Variable('I')
J = Variable('J')

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
                #result = result.unify(lhs, Index(rhs))
                result = result.unify(lhs, rhs)
            else:
                result = result.unify(lhs, rhs)
        return result

    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def __contains__(self, x: Hashable) -> bool:
        return x in self.d

    def __getitem__(self, x: Hashable) -> Optional[Any]:  #TODO proper type hint
        return self.d.get(x, None)

    def as_addr(self, e: Expr) -> Addr:
        # TODO What if we can't find or evaluate e and get an Addr?
        return self[e]

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        match (lhs, rhs):
            case (x, y) if x == y:
                return self
            case (Variable(), int()):
                return self.set_lhs_rhs(lhs, rhs)
            case (Variable(), Variable()):
                return self.substitute(lhs, rhs).set_lhs_rhs(lhs, rhs)
        lo("Unimplemented unification:", lhs, type(lhs), ' with ', rhs, type(rhs))
        raise NotImplementedError((lhs, rhs))

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> Subst:
        if lhs in self.d:
            if self.d[lhs] == rhs:
                return self
            else:
                return bottom_subst
        else:
            return Subst(self.d.set(lhs, rhs))

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

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

def expr_substitute(e: Expr, lhs: Expr, rhs: Expr) -> Expr:
    '''Returns a new Expr consisting of e where every occurrence of lhs has
    been replaced by rhs.'''
    match e:
        case x if x == lhs:
            return rhs
        case _:  # no match; nothing to substitute
            return e

class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def unify(self, lhs: Expr, rhs: Expr) -> BottomSubst:
        return self

    def set_lhs_rhs(self, lhs: Expr, rhs: Expr) -> BottomSubst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> BottomSubst:
        return self

empty_subst = Subst()
bottom_subst = BottomSubst()

########## Canvas-cell contents ##########

@dataclass(frozen=True)
class Letter:
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

    def short(self) -> str:
        return repr(self.c)

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.c!r})'

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

########## The canvas ##########

@dataclass
class Canvas:
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
            return cls(d, min_index=1, max_index=len(s) + 1)

    def __getitem__(self, a: Index) -> Optional[CanvasValue]:
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

    def all_indices(self) -> Iterable[Index]:
        if self.min_index is None:
            return
        else:
            assert self.max_index is not None
            yield from range(self.min_index, self.max_index + 1)

########## New stuff as of 7-Dec-2022 ##########

@dataclass(frozen=True)
class UState:
    '''A 'unification state': an interface to the workspace and the long-term
    memory, including all canvases, snippets, and painters, and providing
    a mapping from addresses to objects.'''
    canvas: Canvas
    
    def value_at(self, a: Addr) -> Optional[CanvasValue]:
        return self.canvas[a]

    def all_canvas_indices(self) -> Iterable[Index]:
        yield from self.canvas.all_indices()

class LoopBody(ABC):

    @abstractmethod
    def run(self, subst: Subst, us: UState) -> Iterable[Tuple[Subst, Any]]:
        pass

class Loop(ABC):
    '''Holds the information needed to loop through some elements in the
    workspace, and can run the loop.'''

    @abstractmethod
    def run(self, subst: Subst, us: UState, body: LoopBody) \
    -> Iterable[Tuple[Subst, Any]]:
        pass

@dataclass(frozen=True)
class LoopOverCanvas(Loop):
    var: Variable

    def run(self, subst: Subst, us: UState, body: LoopBody) \
    -> Iterable[Tuple[Subst, Any]]:
        for i in us.all_canvas_indices():
            su = subst.unify(self.var, i)
            yield from body.run(su, us)

@dataclass(frozen=True)
class Succ:
    i: Variable
    j: Variable

    def restrict_for_source(
        self, us: UState, subst: Subst, source_var: Variable
    ) -> Loop:
        if source_var == self.i and target_var == self.j:
            
        elif source_var == self.j and target_var == self.i:
        else:

########## The Model ##########


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

