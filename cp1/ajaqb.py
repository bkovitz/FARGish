# ajaqb.py -- Just enough model to run the ajaqb test

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from random import choice
import operator
from functools import reduce
from io import StringIO

from pyrsistent import pmap
from pyrsistent.typing import PMap

from util import Numeric, short, as_tuple, as_list, pts, force_setattr, union, \
    reseed, safe_issubclass, newline, psa, pr
from Log import trace

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
        #print(f'ISM xp={xp}  p={p}')
#        if xp == p:
#            return True
#        else:
        xi, xj, xf = xp
        pi = as_index(p[0])
        pj = as_index(p[1])
        pf = p[2]  # TODO Call as_func?

        #subst: Subst = {}
        subst = Subst()


        #print(xi, xj, xf)
        #print(pi, pj, pf)
        subst = subst.unify(xi, pi)
        subst = subst.unify(xj, pj)
        #print('SUBST', subst)
        match subst:
            case BottomSubst():
                return False
            case Subst():
                return True
            case _:
                raise NotImplementedError

    @classmethod
    def union(cls, *soups: Soup) -> Soup:
        return Soup(union(*(soup.painters for soup in soups)))

    def short(self) -> str:
        cl = self.__class__.__name__
        return cl

class WorkingSoup(Soup):
    pass

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

class HasMakeSubst(ABC):

#    @abstractmethod
#    def make_subst(self, v: Index, subst: Subst) -> Subst:
#        pass
    pass

@dataclass(frozen=True)
class Variable(HasAsIndex, HasMakeSubst):
    name: str

    def __str__(self) -> str:
        return self.name

    def as_index(self) -> Index:
        raise NotImplementedError   # TODO Look this up in an Env

#    def make_subst(self, v: Index, subst: Subst) -> Subst:
#        got = subst.eval_as_index(self)
#        match got:
#            case int():
#                return subst
#            case BottomSubst():
#                return got
#            #case OpenSubst
#        if subset.eval_as_index(self) != v:
#            return empty_subst
#        else:
#            pass # TODO?

I = Variable('i')
J = Variable('j')

@dataclass(frozen=True)
class Plus(HasAsIndex):
    args: Tuple[Expr]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __str__(self) -> str:
        return '+'.join(str(a) for a in self.args)

    def value_of(self, subst: Subst) -> Expr:
        return self.simplify(subst, self.args)

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
                return cls.try_to_add(init, subst.eval_as_index(expr))
            case (expr, *more):
                return cls.simplify(
                    subst,
                    more,
                    cls.try_to_add(init, subst.eval_as_index(expr))
                )

    @classmethod
    def try_to_add(cls, a: Any, b: Any) -> Expr:
        match (a, b):
            case (0, _):
                return b
            case (int(), int()):
                return a + b
            case (Plus(args_a), Plus(args_b)):
                return Plus(*(args_a + args_b))
            case (Plus(args_a), _):
                return Plus(*(args_a + (b,)))
            case (_, Plus(args_b)):
                return Plus(*((a,) + args_b))
            case _:
                return Plus(a, b)

    def as_index(self) -> Index:
        return reduce(operator.add, (as_index(arg) for arg in self.args), 0)

#Value = Hashable
#Value = Union[int, str, 'Painter']
Value = Union[int, str, None]
Index = int   # the index of a cell within a Canvas

# This little group enables everything to type-check, though at much cost
# to type-safety.
Expr = Any
Addr = Any
Func = Any
ProperFunc = Callable[[Value], Value]
Painter = Tuple[Addr, Addr, Func]


@dataclass(frozen=True)
class Subst:
    '''A substitution table, i.e. a mapping from Variables to Exprs.'''
    d : PMap[Expr, Index] = field(default_factory=lambda: pmap())

    def eval_as_index(self, expr: Expr) -> Union[Index, None]:
        match expr:
            case int():
                return expr
            case Plus():
                return expr.value_of(self)
            case _:
                return self.d.get(expr, None)

#    def unify(self, var: Variable, rhs: Expr) -> Subst:
#        if var in self.d:
#            if var == rhs:
#                return self
#            elif 
#                return self.substitute(
#        else:
#            return Subst(self.d.set(var, rhs))

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        #print('HERE', lhs, rhs)
        match (lhs, rhs):
            case (int(l), int(r)):
                if l == r:
                    return self
                else:
                    return bottom_subst
            case (Variable(), int(r)):
                #print('GOTN', lhs, r)
                if lhs in self.d:
                    if self.d[lhs] == r:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, r))
            case (Plus(args=(Variable() as v, int(n))), int(r)):
                #print('GOTPP', v, n, r)
                match self.eval_as_index(v):
                    case int(vv):
                        if vv + n == r:
                            return self
                        else:
                            return bottom_subst
                    case None:
                        return Subst(self.d.set(v, r - n))
            case (Variable(), Plus() as rator):
                #print('GOTVP')
                rvalue = rator.value_of(self)
                #if lhs in self.
                return self # TODO
            case (Variable(), soup) if safe_issubclass(soup, Soup):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
            case (Variable(), (soup, p)) if (
                    safe_issubclass(soup, Soup)
                    and
                    is_painter(p)  # type: ignore[has-type]
            ):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
                
            case _:
                print('WHAT?', type(lhs), type(rhs))
                raise NotImplementedError((lhs, rhs))
        assert False, "unify(): should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error

#    def substitute(self, var: Variable, rhs: Expr) -> Subst:
#        '''Returns a Subst in which every occurence of 'var' has been replaced
#        by 'rhs'.'''
#        return Subst(
#            pmap(
#                (expr_substitute(v, var, rhs), expr_substitute(e, var, rhs))
#                    for v, e in self.d.items()
#        )
        
class BottomSubst(Subst):
    '''A Subst that maps nothing to nothing and can't unify or substitute
    anything.'''

    def eval_as_index(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
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

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

#@dataclass(frozen=True)
#class CanvasAddr:
#    canvas: Canvas
#    index: Index
#
#    def __repr__(self) -> str:
#        return str(self.index)

PaintableSoupRef = Type[WorkingSoup]
PainterRef = Tuple[PaintableSoupRef, Painter]
DeterminateAddr = Union[int, PaintableSoupRef, PainterRef]

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

    def addr_of(self, v: Value) -> int:
        for i, x in enumerate(self.contents):
            if x == v:
                return i + 1
        raise FizzleValueNotFound(v)

    def all_matching(self, v: Value) -> List[int]:
        return [
            i + 1
                for i, x in enumerate(self.contents)
                    if x == v
        ]

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        citems = ' '.join(short(c) for c in self.clarities)
        return f'[{items}]'
        #return f'[{items}]{newline}[{citems}]'

    def short(self) -> str:
        s = ''.join(
            ' ' if x is None else str(x)
                for x in self.contents
        )
        return repr(s)

### The basic relational functions

def same(v: Value) -> Value:
    return v

def succ(v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle("succ: Can't take successor of {v}")

def pred(v: Value) -> Value:
    # TODO Deal with 'a'
    if isinstance(v, str):
        return chr(ord(v) - 1)
    elif isinstance(v, int):
        return v - 1
    raise Fizzle("pred: Can't take predecessor of {v}")

### The constant function

@dataclass(frozen=True)
class const:
    v: Value

    def __call__(self, ignored: Value) -> Value:
        return self.v

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.v})'

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
    #return isinstance(x, tuple)
    match x:
        case (i, j, func):
            return True
        case _:
            return False

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    def absorb(self, s: str) -> None:
        for i, j, func in self.find_related_cells(s):
#            a1 : Addr = s[i-1]
#            a2 : Addr = WorkingSoup
#            a31 : Addr = I
#            a32 : Addr = Plus(I, (j - i))
#            a33 : FuncExpr = func
#            a3 : FuncExpr = (a31, a32, a33)
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
        # TODO Add indentation, nicer logging?
        for t in range(1, 20):  # TODO replace arbitrary constant
            p = self.choose_painter()
            #print(f'{newline}{newline}t={t} {painter_str(p)}')
            print(f'{newline}{newline}t={t}')
            try:
                self.run_painter(p)
            except Fizzle as exc:
                print('FIZZLED!', exc)
            print(self.state_str())

    def state_str(self) -> str:
        sio = StringIO()
        print(short(self.canvas), file=sio)
        pr(self.ws.painters, file=sio)
        return sio.getvalue()

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
        #print('RUN_PAINTER', short(p))
        print('RUN_PAINTER', painter_str(p))
        i, j, func = p
        subst = Subst()
        print(f'i = {short(i)}')
        ii = self.eval_as_detaddr(subst, i)
        print(f'ii = {ii}\n')
        subst = subst.unify(I, ii)

        print(f'j = {j!r}')
        jj = self.eval_as_detaddr(subst, j)
        print(f'jj = {jj}\n')
        subst = subst.unify(J, jj)

        print(f'func = {short(func)}')
        FUNC = self.eval_as_func(subst, func)
        print(f'FUNC = {short(FUNC)}\n')

        oldval = self.get_value(ii)
        print(f'oldval = {short(oldval)}')
        newval = FUNC(oldval)
        print(f'newval = {short(newval)}')

        self.paint(jj, newval)

    def paint(self, addr: Addr, value: Value) -> None:
        print('PAINT', short(addr), short(value))
        match (addr, value):
            case (int(), _):
                self.canvas[addr] = value
            case (ws, p) if ws == WorkingSoup and is_painter(p):
                if self.is_valid_painter(p):
                    self.ws.add(p)
                else:
                    raise Fizzle(f'Invalid painter: {p}')
            case _:
                raise NotImplementedError(addr)

    def is_valid_painter(self, p: Painter) -> bool:
        match p:
            case (int(i), int(j), func):
                return (
                    self.canvas.has_addr(i)
                    and
                    self.canvas.has_addr(j)
                )
            case (i, j, k):
                return True
            case _:
                return False

    def eval_as_detaddr(self, subst: Subst, a: Addr) -> DeterminateAddr:
        if isinstance(a, str):
            addrs = self.canvas.all_matching(a)
            if not addrs:
                raise Fizzle(f'eval_as_detaddr: no match for {a!r}')
            else:
                return choice(addrs)
        elif is_paintable_soup(a):
            return a
        elif is_painter(a):
            # find painters that match a
            painters = self.ws.matching_painters(a)
            if not painters:
                raise Fizzle(
                    f'eval_as_detaddr: no painters match {painter_str(a)}'
                )
            else:
                return (WorkingSoup, choice(painters))
        elif is_expr(a):
            return as_index(a)
        else:
            print(f'\na = {a}')
            raise Fizzle(f'eval_as_detaddr: unrecognized Addr type {a!r}')

    def eval_as_func(self, subst: Subst, x: Func) -> ProperFunc:
        #print('EAF', x, type(x))
        #print('EAF SUBST', subst)
        match x:
            case x if callable(x):
                return x
            case (i, j, f):
                return const(self.eval_as_painter(subst, x))
            case str():
                return const(x)
            case _:
                raise NotImplementedError(x)
        assert False, "eval_as_func: should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error

    def eval_as_painter(self, subst: Subst, x: Painter) -> Painter:
        match x:
            case (i, j, f):
                return (
                    subst.eval_as_index(i),
                    subst.eval_as_index(j),
                    self.eval_as_func(subst, f)
                )
            case _:
                raise NotImplementedError(x)

    def get_value(self, addr: Addr) -> Value:
        match addr:
            case int():
                return self.canvas[addr]
            case soup if safe_issubclass(soup, Soup):
                return soup
            case (ws, p) if ws == WorkingSoup and is_painter(p):  # type: ignore[has-type]
                return addr
            case _:
                raise NotImplementedError(addr)
        assert False, "get_value(): should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error
                

def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return e
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle(f"as_index: Can't convert {e!r} to index")

def is_paintable_soup(x: Any) -> TypeGuard[PaintableSoupRef]:
    return x == WorkingSoup

def is_expr(x: Any) -> TypeGuard[Expr]:
    return isinstance(x, int) or hasattr('as_index', x)

def run_all() -> None:
    '''Run the whole simulation. Absorb 'ajaqb' and see what regenerates from
    'a    '.'''
    m = Model()
    m.absorb('ajaqb')
    for p in m.lts.painters:
        print(painter_str(p))
    print()
    m.regen_from('a    ')
    #print(short(m.canvas))

def run_this() -> None:
    '''Bug of the day, 28-Jul-2022'''
    m = Model()
    m.absorb('ajaqb')
    m.set_canvas('a    ')
    m.run_painter(('a', WorkingSoup, (I, Plus(I, 2), same)))
    print(m.state_str())


if __name__ == '__main__':
#    reseed(0)
#    run_all()

    run_this()

#    m = Model()
#    m.set_canvas('ajaqb')
#    p = (1, 3, same)
#    m.add_painter((WorkingSoup, p))
#    pts(m.ws.painters)
#    print()
#    #print(m.eval_as_detaddr((1, 4, same)))
#    print(m.eval_as_detaddr((I, Plus(I, 2), same)))

#    s0 = Subst()
#    s1 = s0.unify(I, 1)
#    print(s1)
#    print(Plus().value_of(s1))
#    pl = Plus(I)
#    print(pl.value_of(s1))
#    pl = Plus(I, 2)
#    print(pl.value_of(s1))
