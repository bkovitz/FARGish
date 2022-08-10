# ajaqb.py -- Just enough model to run the ajaqb test

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from random import choice, random
import operator
from functools import reduce
from io import StringIO
from collections import defaultdict

from pyrsistent import pmap
from pyrsistent.typing import PMap

from util import Numeric, short, as_tuple, as_list, pts, force_setattr, union, \
    reseed, safe_issubclass, newline, psa, pr
from Log import trace, lo, logging

@dataclass(frozen=True)
class Variable:
    name: str

    def __str__(self) -> str:
        return self.name

I = Variable('i')
J = Variable('j')

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

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
CanvasValue = Union[int, str, None]
Index = int   # the index of a cell within a Canvas

# This little group enables everything to type-check, though at much cost
# to type-safety.
Expr = Any
Addr = Any
Func = Any
Painter = Tuple[Addr, Addr, Func]
Value = Union[CanvasValue, Painter]

@dataclass
class Soup:
    #painters: Set = field(default_factory=lambda: set())
#    painters: Dict[Painter, Numeric] = field(
#        default_factory=lambda: defaultdict(int)
#    )  # map Painter to clarity
    painters: List = field(default_factory=list)

    def add(self, p: Painter) -> None:
        #self.painters.add(p)
        #self.painters[p] += 1
        self.painters.append(p)

    def matching_painters(self, xp: Painter) -> List[Tuple[Subst, Painter]]:
        result = []
        for p in self.painters:
            subst = self.is_match(xp, p)
            if subst:
                result.append((subst, p))
        return result

    def is_match(self, xp: Painter, p: Painter) -> Subst:
        '''Viewing xp as a painter template (possibly with variables that
        need to be filled in), does p match xp?

        Returning a BottomSubst() means no match.
        '''
        # NEXT Require match of func, too
        xi, xj, xf = xp
        pi = as_index(p[0])
        pj = as_index(p[1])
        pf = p[2]

        if xf == pf:
            return empty_subst.unify(xi, pi).unify(xj, pj)
        else:
            return bottom_subst

    def has_painter(self, p: Painter) -> bool:
        return p in self.painters

    @classmethod
    def union(cls, *soups: Soup) -> Soup:
        # TODO What about clarities?
        #return Soup(union(*(soup.painters for soup in soups)))
        return Soup(reduce(operator.add, (soup.painters for soup in soups), []))

    def short(self) -> str:
        cl = self.__class__.__name__
        return cl

@dataclass(frozen=True)
class Subst:
    '''A substitution table, i.e. a mapping from Variables to Exprs.'''
    d : PMap[Expr, Index] = field(default_factory=lambda: pmap())

    def __bool__(self) -> bool:
        return True

    @classmethod
    def make_from(cls, *pairs: Tuple[Expr, Expr]) -> Subst:
        e = pmap().evolver()  # type: ignore[var-annotated]
        for k, v in pairs:
            e[k] = v
        return cls(e.persistent())

    def merge(self, other: Subst) -> Subst:
        return Subst(self.d.update(other.d))

    def __contains__(self, x: Hashable) -> bool:
        return x in self.d

    def simplify(self, expr: Expr) -> Union[Expr, None]:
        match expr:
            case int():
                return expr
            case Plus():
                return expr.value_of(self)
            case _:
                match self.d.get(expr, None):
                    case None:
                        return None
                    case v:
                        return self.simplify(v)

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
                match self.simplify(v):
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
            case (Variable(), SoupRef()):
                if lhs in self.d:
                    if self.d[lhs] == rhs:
                        return self
                    else:
                        return bottom_subst
                else:
                    return Subst(self.d.set(lhs, rhs))
            case (Variable(), (SoupRef(), p)) if (
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
                lo("Can't unify:", type(lhs), type(rhs))
                raise NotImplementedError((lhs, rhs))
        assert False, "unify(): should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error

    def short(self) -> str:
        cl = self.__class__.__name__
        items = ', '.join(
            f'{short(k)}={short(v)}' for k, v in self.d.items()
        )
        return f'{cl}({items})'

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
    anything. As a bool, equivalent to False.'''

    def __bool__(self) -> bool:
        return False

    def simplify(self, expr: Expr) -> Union[Index, None]:
        return None

    def unify(self, lhs: Expr, rhs: Expr) -> Subst:
        return self

    def substitute(self, var: Variable, rhs: Expr) -> Subst:
        return self

empty_subst = Subst()
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

@dataclass(frozen=True)
class SoupRef:
    name: str

    def __str__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')

PainterRef = Tuple[SoupRef, Painter]
DeterminateAddr = Union[int, SoupRef, PainterRef]

def is_soupref(x: Any) -> TypeGuard[SoupRef]:
    return safe_issubclass(x, Soup)

def is_painterref(x: Any) -> TypeGuard[PainterRef]:
    match x:
        case (soupref, pp):
            return isinstance(soupref, SoupRef) and is_painter(pp)
        case _:
            return False

def get_painter(p: Union[Painter, PainterRef]) -> Painter:
    if is_painter(p):
        return p
    elif is_painterref(p):
        return get_painter(p[1])
    else:
        raise NotImplementedError(p)

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
    def __getitem__(self, addr: Addr) -> CanvasValue:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, x: CanvasValue) -> None:
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
    def addr_of(self, v: CanvasValue) -> DeterminateAddr:
        '''Returns the DeterminateAddr of the cell that contains v, or
        raises FizzleValueNotFound if no cell contains v.'''
        pass

    @abstractmethod
    def all_ixjypairs(self) -> Iterable[Tuple[Index, CanvasValue, Index, CanvasValue]]:
        '''Returns a generator of all tuples (i, x, j, y) where i and j
        are distinct indices of cells within this Canvas and x = self[i]
        and y = self[j]. Skips cells that contain None.'''
        pass

@dataclass
class Canvas1D(Canvas):
    contents: List[CanvasValue] #= field(default_factory=list)
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

    def __getitem__(self, addr: Addr) -> CanvasValue:
        if isinstance(addr, int):
            try:
                return self.contents[addr - 1]
            except IndexError:
                return None
        else:
            return None

    def __setitem__(self, addr: Addr, x: CanvasValue) -> None:
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
    def all_ixjypairs(self) -> Iterable[Tuple[Index, CanvasValue, Index, CanvasValue]]:
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

    def addr_of(self, v: CanvasValue) -> int:
        for i, x in enumerate(self.contents):
            if x == v:
                return i + 1
        raise FizzleValueNotFound(v)

    def all_matching(self, v: CanvasValue) -> List[int]:
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
        return repr(self.short_str())

    def short_str(self) -> str:
        return ''.join(
            ' ' if x is None else str(x)
                for x in self.contents
        )

    def state_str(self) -> str:
        return f"{self.short()}  {' '.join(str(c) for c in self.clarities)}"

### The basic relational functions

def same(subst: Subst, v: Value) -> Value:
    return v

def succ(subst: Subst, v: Value) -> Value:
    # TODO Deal with 'z'
    if isinstance(v, str):
        return chr(ord(v) + 1)
    elif isinstance(v, int):
        return v + 1
    raise Fizzle("succ: Can't take successor of {v}")

def pred(subst: Subst, v: Value) -> Value:
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

    def __call__(self, subst: Subst, ignored: Value) -> Value:
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
    elif isinstance(a, SoupRef):
        return a.name
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
        for p in self.relative_spont_painters(s):
            self.lts.add(p)

    def relative_spont_painters(self, s: str) -> Iterable[Painter]:
        for i, j, func in self.find_related_cells(s):
            yield(s[i-1], WorkingSoup, (I, Plus(I, (j - i)), func))
            if s[i] != ' ' and j - i == 2:
                # if need a painter to put a letter between two letters
                yield(
                    ((I, Plus(I, 2), func),
                     WorkingSoup, 
                     (I, Plus(I, 1), s[i]))
                )

    def absolute_spont_painters(self, s: str) -> Iterable[Painter]:
        for i, j, func in self.find_related_cells(s):
            yield (i, j, func)
            if s[i] != ' ' and j - i == 2:
                yield (i, i + 1, s[i])

    def find_related_cells(self, s: str) \
    -> Iterable[Tuple[Index, Index, Func]]:
        for i in range(len(s)):
            for j in range(i + 1, len(s)):
                if s[i] == ' ' or s[j] == ' ':
                    continue
                if s[i] == s[j]:
                    yield (i+1, j+1, same)
                elif ord(s[i]) == ord(s[j]) - 1:
                    yield (i+1, j+1, succ)
                elif ord(s[i]) == ord(s[j]) + 1:
                    yield (i+1, j+1, pred)

    def regen_from(self, s: str, nsteps=20) -> None:
        '''Regenerates canvas from 's'.'''
        self.set_canvas(s)
        # TODO Add indentation, nicer logging?
        for t in range(1, nsteps + 1):
            lo(f'{newline}{newline}t={t}')
            with logging(None):
                p = self.choose_painter()
                #print(f'{newline}{newline}t={t} {painter_str(p)}')
                try:
                    self.run_painter(p)
                except Fizzle as exc:
                    lo('FIZZLED:', exc)
                lo('\n' + self.state_str())

    def state_str(self) -> str:
        sio = StringIO()
        print('canvas:', self.canvas.state_str(), file=sio)
        for pstr in sorted(painter_str(p) for p in self.ws.painters):
            print(pstr, file=sio)
        return sio.getvalue()

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)
        
    def add_painter(self, pr: PainterRef) -> None:
        ref, p = pr
        if ref == WorkingSoup:
            self.ws.add(get_painter(p))
        elif ref == LongTermSoup:
            self.lts.add(get_painter(p))
        else:
            raise NotImplementedError

    def choose_painter(self) -> Painter:
        current_painters = Soup.union(self.lts, self.ws).painters
        sponts = (
            set(self.absolute_spont_painters(self.canvas.short_str()))
            -
            #current_painters
            set(current_painters)
        )
        if sponts and random() <= 0.3:
            spont = choice(list(sponts))
            lo('SPONT')
            return (1, WorkingSoup, spont)  # The 1 is irrelevant
        else:
            return choice(list(current_painters))

    def run_painter(self, p: Painter) -> None:
        #print('RUN_PAINTER', short(p))
        lo('RUN_PAINTER', painter_str(p))
        i, j, func = p
        subst = Subst()
        subst, ii = self.eval_as_detaddr(subst, i)
        lo(f'ii =', ii)
        if I not in subst:
            subst = subst.unify(I, ii)
        lo(subst)

        lo(f'j = {j!r}')
        subst, jj = self.eval_as_detaddr(subst, j)
        lo(f'jj = {jj}\n')
        if J not in subst:
            subst = subst.unify(J, jj)

        lo('subst =', subst)

        lo(f'func = {short(func)}')
        FUNC = self.eval_as_func(subst, func)
        lo(f'FUNC = {short(FUNC)}\n')

        oldval = self.get_value(ii)
        lo(f'oldval = {short(oldval)}')
        newval = self.apply_func(subst, FUNC, oldval)
        lo(f'newval = {short(newval)}')

        self.paint(jj, newval)

    def paint(self, addr: Addr, value: Union[Value, Painter]) -> None:
        lo('PAINT', short(addr), short(value))
        match (addr, value):
            case (int(), _) if is_canvasvalue(value):
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

    def eval_as_detaddr(self, subst: Subst, a: Addr) \
    -> Tuple[Subst, DeterminateAddr]:
        if isinstance(a, str):
            addrs = self.canvas.all_matching(a)
            if not addrs:
                raise Fizzle(f'eval_as_detaddr: no match for {a!r}')
            else:
                return (subst, choice(addrs))
        elif is_paintable_soup(a):
            return (subst, a)
        elif is_painter(a):
            # find painters that match a
            pmatches = self.ws.matching_painters(a)
            if not pmatches:
                raise Fizzle(
                    f'eval_as_detaddr: no painters match {painter_str(a)}'
                )
            else:
                psubst, painter = choice(pmatches)
                detaddr = (WorkingSoup, painter)
                return (subst.merge(psubst), detaddr)
        elif is_expr(a):
            return (subst, as_index(a))
        else:
            lo(f'\na = {a}')
            raise Fizzle(f'eval_as_detaddr: unrecognized Addr type {a!r}')

    def eval_as_func(self, subst: Subst, x: Func) -> Func:
        #print('EAF', x, type(x))
        #print('EAF SUBST', subst)
        match x:
            case x if callable(x):
                return x
            case (i, j, f):
                return const(self.eval_as_painter(subst, x))
            case str():
                return x
            case _:
                raise NotImplementedError(x)
        assert False, "eval_as_func: should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error

    def eval_as_painter(self, subst: Subst, x: Painter) -> Painter:
        match x:
            case (i, j, f):
                return (
                    subst.simplify(i),
                    subst.simplify(j),
                    self.eval_as_func(subst, f)
                )
            case _:
                raise NotImplementedError(x)

    def apply_func(self, subst: Subst, f: Func, v: Value) \
    -> Union[Value, Painter]:
        if isinstance(f, str) or isinstance(f, int) or is_painter(f):
            return f
        elif callable(f):
            return f(subst, v)
        else:
            raise NotImplementedError(f"apply_func: can't apply {f}")

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

def is_canvasvalue(x: Any) -> TypeGuard[CanvasValue]:
    return (
        isinstance(x, int)
        or
        isinstance(x, str)
        or
        x is None
    )
def is_paintable_soup(x: Any) -> TypeGuard[SoupRef]:
    return x == WorkingSoup

def is_expr(x: Any) -> TypeGuard[Expr]:
    return isinstance(x, int) or hasattr(x, 'as_index')

m : Model

def run_all() -> None:
    '''Run the whole simulation. Absorb 'ajaqb' and see what regenerates from
    'a    '.'''
    global m
    m = Model()
    m.absorb('ajaqb')
    for p in m.lts.painters:
        print(painter_str(p))
    print()
    m.regen_from('a    ', nsteps=120)
    #print(short(m.canvas))

def run_this() -> None:
    '''Bug of the day, 28-Jul-2022'''
    m = Model()
    m.absorb('ajaqb')
    m.set_canvas('a    ')
    m.run_painter(('a', WorkingSoup, (I, Plus(I, 2), same)))
    print(m.state_str())


if __name__ == '__main__':
    reseed(0)
    run_all()

#    run_this()

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
