# ajaqb.py -- Just enough model to run the ajaqb test

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from random import choice
import operator
from functools import reduce

from pyrsistent import pmap
from pyrsistent.typing import PMap

from util import Numeric, short, as_tuple, as_list, pts, force_setattr, union, \
    reseed

@dataclass
class Soup:
    painters: Set = field(default_factory=lambda: set())

    def add(self, p: Painter) -> None:
        self.painters.add(p)

    def matching_painters(self, xp: Painter) -> List[Painter]:
        return [
            p for p in self.painters if self.is_match(xp, p)
        ]

    def is_match(self, xp: Painter, p: Painter) -> bool:
        '''Viewing xp as a painter template (possibly with variables that
        need to be filled in), does p match xp?'''
        print(f'ISM xp={xp}  p={p}')
        if xp == p:
            return True
        else:
            xi, xj, xf = xp
            pi = as_index(p[0])
            pj = as_index(p[1])
            pf = p[2]  # TODO Call as_func?

            subst: Subst = {}


            print(xi, xj, xf)
            print(pi, pj, pf)
            raise NotImplementedError

    @classmethod
    def union(cls, *soups: Soup) -> Soup:
        return Soup(union(*(soup.painters for soup in soups)))

class WorkingSoup(Soup):
    pass

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

class HasMakeSubst(ABC):

    @abstractmethod
    def make_subst(self, v: Index, subst: Subst) -> Subst:
        pass

@dataclass(frozen=True)
class Variable(HasAsIndex, HasMakeSubst):
    name: str

    def __str__(self) -> str:
        return self.name

    def as_index(self) -> Index:
        raise NotImplementedError   # TODO Look this up in an Env

    def make_subst(self, v: Index, subst: Subst) -> Subst:
        # NEXT subst.unify()?
        got = subst.eval_as_index(self)
        match got:
            case int():
                return subst
            case BottomSubst():
                return got
            case OpenSubst
        if subset.eval_as_index(self) != v:
            return empty_subst
        else:
            

I = Variable('i')
J = Variable('j')

@dataclass(frozen=True)
class Plus(HasAsIndex):
    args: Tuple[Expr]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __str__(self) -> str:
        return '+'.join(str(a) for a in self.args)

    def as_index(self) -> Index:
        return reduce(operator.add, (as_index(arg) for arg in self.args), 0)

Value = Hashable
Index = int   # the index of a cell within a Canvas
Func = Union[Callable[[Value], Value], Value]
Expr = Union[Variable, Plus, int, Func]  # TODO Move Func to Painter?
Addr = Union[Index, str, Type[WorkingSoup], Expr]
Painter = Tuple[Addr, Addr, Expr]

Subst = Union[Dict[Variable, Index], None]  # a substitution table

@dataclass(frozen=True)
class Subst:
    '''A mapping from Variables to Exprs.'''
    d : PMap[Variable, Index] = field(default_factory=lambda: pmap())

    def value_of(self, expr: Expr) -> Union[Index, None]:
        return self.d.get(expr, None)

    def unify(self, var: Variable, rhs: Expr) -> Subst:
        if var in self.d:
            if var == rhs:
                return self
            elif 
                return self.substitute(
        else:
            return Subst(self.d.set(var, rhs))

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        '''Returns a Subst in which every occurence of 'var' has been replaced
        by 'rhs'.'''
        return Subst(
            pmap(
                (expr_substitute(v, var, rhs), expr_substitute(e, var, rhs))
                    for v, e in self.d.items()
        )
        
class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything.'''

    def value_of(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, var: Variable, rhs: Expr) -> Subst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        return self

bottom_subst = BottomSubst()

def expr_substitute(e: Expr, v: Variable, rhs: Expr) -> Expr:
    '''Returns 'e', with each occurrence of 'v' replaced by 'rhs'.'''
    match e:
        case Variable():
            if e == v:
                return rhs
            else:
                return e
        case int():
            return e
        case Plus(args):  # TODO Change to CompoundExpr()?
            return Plus(*(expr_substitute(a, v, rhs) for a in e.args))
        case _:
            raise NotImplementedError(e)

def run_all() -> None:
    '''Run the whole simulation. Absorb 'ajaqb' and see what regenerates from
    'a    '.'''
    m = Model()
    m.absorb('ajaqb')
    for p in m.lts.painters:
        print(painter_str(p))
    m.regen_from('a    ')
    print(f"'{short(m.canvas)}'")

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class CanvasAddr:
    canvas: Canvas
    index: Index

    def __repr__(self) -> str:
        return str(self.index)

PaintableSoupRef = Type[WorkingSoup]
PainterRef = Tuple[PaintableSoupRef, Painter]
DeterminateAddr = Union[CanvasAddr, PaintableSoupRef, PainterRef]

@dataclass(kw_only=True)  # type: ignore[call-overload, misc]
class Canvas(ABC):
    MAX_CLARITY: Numeric = 6
    INITIAL_CLARITY: Numeric = 5

    """
    MAX_CLARITY: ClassVar[Numeric] = 6  # TODO move this to a dataclass or  # 5
                                        # maybe to RMem
    """
    @abstractmethod
    def all_addrs(self) -> Iterable[Addr]:
        pass
    
    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, x: Value) -> None:
        pass

    @abstractmethod
    def has_addr(self, addr: Addr) -> bool:
        pass

    @abstractmethod
    def clarity(self, addr: Addr) -> Numeric:
        pass

    def all_clarities(self) -> Iterable[Numeric]:
        for addr in self.all_addrs():
            yield self.clarity(addr)

    @abstractmethod
    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        pass

    @abstractmethod
    def addr_of(self, v: Value) -> DeterminateAddr:
        '''Returns the DeterminateAddr of the cell that contains v, or
        raises FizzleValueNotFound if no cell contains v.'''
        pass

    @abstractmethod
    def all_ixjypairs(self) -> Iterable[Tuple[Index, Value, Index, Value]]:
        '''Returns a generator of all tuples (i, x, j, y) where i and j
        are distinct indices of cells within this Canvas and x = self[i]
        and y = self[j]. Skips cells that contain None.'''
        pass

@dataclass
class Canvas1D(Canvas):
    contents: List[Value] #= field(default_factory=list)
        # Always supply a value for 'contents'! The default is only to
        # avoid an error for following MAX_CLARITY, which has a default.
    clarities: List[Numeric] = field(  # same # of elems as 'contents'
        default_factory=list,
        init=False
    )

    def __post_init__(self) -> None:
        self.clarities = [
            0 if x is None else int(self.INITIAL_CLARITY)   # - 1  * 0.61
                for i, x in enumerate(self.contents)
        ]

    @classmethod
    def make_from(cls, s: str) -> Canvas1D:
        return Canvas1D(contents=list(c if c != ' ' else None for c in s))

    def all_addrs(self) -> Iterable[Index]:  # TODO rename to all_indices
        return range(1, len(self.contents) + 1)

    def has_addr(self, addr: Addr) -> bool:
        if isinstance(addr, int):
            addr = addr - 1
            return addr >= 0 and addr < len(self.contents)
        else:
            return False

    def __getitem__(self, addr: Addr) -> Value:
        if isinstance(addr, int):
            try:
                return self.contents[addr - 1]
            except IndexError:
                return None
        else:
            return None

    def __setitem__(self, addr: Addr, x: Value) -> None:
        if isinstance(addr, int):
            addr = addr - 1
            if addr < 0:  # off the left edge of the canvas
                return
            if self.clarities[addr] == 0:
                try:
                    self.contents[addr] = x
                except IndexError:
                    # TODO Stretch the canvas?
                    return
                if x is not None:
                    self.clarities[addr] = 1
            elif x != self.contents[addr]:  # Trying to overwrite a value
                self.clarities[addr] -= 1
                if self.clarities[addr] <= 0:
                    self.contents[addr] = None
            else:  # Trying to write the value that is already there
                if self.clarities[addr] < self.MAX_CLARITY:
                    self.clarities[addr] += 1
        else:
            pass  # raise an exception?

    # TODO UT
    def all_ixjypairs(self) -> Iterable[Tuple[Index, Value, Index, Value]]:
        for i in self.all_addrs():
            x = self[i]
            if x is not None:
                for j in self.all_addrs():
                    if i != j:
                        y = self[j]
                        if y is not None:
                            yield i, x, j, y

    def clarity(self, addr: Addr) -> Numeric:
        if isinstance(addr, int):
            addr = addr - 1
            try:
                return self.clarities[addr]
            except IndexError:
                return self.MAX_CLARITY
        else:
            return 0  # raise an exception?

    def all_clarities(self) -> Iterable[Numeric]:
        return self.clarities

    def set_clarity(self, addr: Addr, clarity: Numeric) -> None:
        if isinstance(addr, int):
            addr -= 1
            self.clarities[addr] = clarity

    def addr_of(self, v: Value) -> CanvasAddr:
        for i, x in enumerate(self.contents):
            if x == v:
                return CanvasAddr(self, i + 1)
        raise FizzleValueNotFound(v)

    def all_matching(self, v: Value) -> List[CanvasAddr]:
        return [
            CanvasAddr(self, i + 1)
                for i, x in enumerate(self.contents)
                    if x == v
        ]

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        citems = ' '.join(short(c) for c in self.clarities)
        return f'[{items}]'
        #return f'[{items}]{newline}[{citems}]'

    def short(self) -> str:
        return ''.join(
            ' ' if x is None else str(x)
                for x in self.contents
        )

### The basic relational functions

def same(v: Value) -> Value:
    return v

def succ(v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle

def pred(v: Value) -> Value:
    # TODO Deal with 'a'
    if isinstance(v, str):
        return chr(ord(v) - 1)
    elif isinstance(v, int):
        return v - 1
    raise Fizzle


def painter_str(p: Painter) -> str:
    i, j, func = p
    return f'({addr_str(i)}, {addr_str(j)}, {func_str(func)})'

def addr_str(a: Addr) -> str:
    if isinstance(a, int):
        return str(a)
    elif isinstance(a, str):
        return repr(a)
    elif a == WorkingSoup:
        return 'WorkingSoup'
    elif is_painter(a):
        return painter_str(a)
    elif isinstance(a, Variable):
        return a.name
    else:
        return str(a)

def func_str(func: Func) -> str:
    if callable(func):
        return short(func)
    elif is_painter(func):
        return painter_str(func)
    else:
        return repr(func)

def is_painter(x: Func) -> TypeGuard[Painter]:
    return isinstance(x, tuple)

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    def absorb(self, s: str) -> None:
        for i, j, func in self.find_related_cells(s):
            self.lts.add((s[i-1], WorkingSoup, (I, Plus(I, (j - i)), func)))
            if j - i == 2:  # if need a painter to put a letter between two
                            # letters
                self.lts.add(
                    ((I, Plus(I, 2), func),
                     WorkingSoup, 
                     (I, Plus(I, 1), s[i]))
                )

    def find_related_cells(self, s: str)\
    -> Iterable[Tuple[Index, Index, Func]]:
        for i in range(len(s)):
            for j in range(i + 1, len(s)):
                if s[i] == s[j]:
                    yield (i+1, j+1, same)
                elif ord(s[i]) == ord(s[j]) - 1:
                    yield (i+1, j+1, succ)
                elif ord(s[i]) == ord(s[j]) + 1:
                    yield (i+1, j+1, pred)

    def regen_from(self, s: str) -> None:
        '''Regenerates canvas from 's'.'''
        self.set_canvas(s)
        for t in range(1, 20):  # TODO replace arbitrary constant
            p = self.choose_painter()
            print(t, painter_str(p))
            self.run_painter(p)

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)
        
    def add_painter(self, pr: PainterRef) -> None:
        ref, p = pr
        if ref == WorkingSoup:
            self.ws.add(p)
        else:
            raise NotImplementedError

    def choose_painter(self) -> Painter:
        return choice(as_list(Soup.union(self.lts, self.ws).painters))

    def run_painter(self, p: Painter) -> None:
        i, j, func = p
        print(f'i = {i}')
        I = self.eval_as_detaddr(i)
        print(f'I = {I}\n')
        # NEXT eval_as_detaddr(i)
        #      eval_as_detaddr(j)
        # get the value for i
        # pass it through func
        # paint it at j

    def eval_as_detaddr(self, a: Addr) -> DeterminateAddr:
        if isinstance(a, str):
            addrs = self.canvas.all_matching(a)
            if not addrs:
                raise Fizzle
            else:
                return choice(addrs)
        elif is_paintable_soup(a):
            return a
        elif is_painter(a):
            # find painters that match a
            painters = self.ws.matching_painters(a)
            if not painters:
                raise Fizzle
            else:
                return (WorkingSoup, choice(painters))
        elif is_expr(a):
            return CanvasAddr(self.canvas, as_index(a))
#        elif # Expr
#            # ?
        else:
            print(f'\na = {a}')
            raise Fizzle

def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return e
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle

def is_paintable_soup(x: Any) -> TypeGuard[PaintableSoupRef]:
    return x == WorkingSoup

def is_expr(x: Any) -> TypeGuard[Expr]:
    return isinstance(x, int) or hasattr('as_index', x)

if __name__ == '__main__':
#    reseed(0)
#    run_all()

    m = Model()
    m.set_canvas('ajaqb')
    m.add_painter((WorkingSoup, (1, 3, same)))
    pts(m.ws.painters)
    print()
    print(m.eval_as_detaddr((I, Plus(I, 2), same)))

