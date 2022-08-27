# majaqb.py -- The ajaqb model, implemented in a way that's modeled on
#              Mathematica

''' WANT

For a given state of the model:
  Find all painters that can run.
  Make determinate painters from them.
  Choose a determinate painter by weight.
  Run it by:
    

Internal representation has classes. External representation is simple.
There's a way to translate both ways.

'''

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING, no_type_check
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod
from random import choice, choices, random
import operator
from functools import reduce
from io import StringIO
from collections import defaultdict
import sys
from itertools import chain

from pyrsistent import pmap
from pyrsistent.typing import PMap

from util import Numeric, short, as_tuple, as_list, pts, force_setattr, union, \
    reseed, safe_issubclass, newline, psa, pr, nf
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

@dataclass
class Soup:
    #painters: Set = field(default_factory=lambda: set())
    painters: Dict[Painter, Numeric] = field(
        default_factory=lambda: defaultdict(int)
    )  # map Painter to clarity
    #painters: List = field(default_factory=list)

    @classmethod
    def make_from(cls, painters: Iterable[Painter]) -> Soup:
        return Soup(defaultdict(int, ((p, 1) for p in painters)))

    def add(self, p: Painter) -> None:
        #self.painters.add(p)
        self.painters[p] += 1
        #self.painters.append(p)

    def clarity(self, p: Painter) -> Numeric:
        return self.painters.get(p, 0.0)

    def decay(self, factor=0.9) -> None:
        for p in self.painters:
            self.painters[p] *= factor

    def __iter__(self) -> Iterator[Painter]:
        return iter(self.painters)

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

    def choose(self) -> Painter:
        return choices(
            list(self.painters.keys()),
            list(self.painters.values())
        )[0]

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

    def state_str(self) -> str:
        sio = StringIO()
        for pstr in sorted(
            f'{painter_str(p)} {nf(cl)}' for p, cl in self.painters.items()
        ):
            print(pstr, file=sio)
        return sio.getvalue()

@dataclass(frozen=True)
class SoupRef:
    name: str

    def __str__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')


CanvasValue = Union[str, None]
Index = int   # the index of a cell within a Canvas
MaybeIndex = Union[Index, None, List[Index]]  # TODO rename or divide into type types: List[Index] is not always appropriate
Expr = Any
Addr = Any
Func = Any
Painter = Tuple[Addr, Addr, Func]
DetAddr = Union[Index, Painter, SoupRef]
Value = Union[CanvasValue, Painter, Func]

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
    def addr_of(self, v: CanvasValue) -> DetAddr:
        '''Returns the DetAddr of the cell that contains v, or
        raises FizzleValueNotFound if no cell contains v.'''
        pass

    @abstractmethod
    def all_ixjypairs(self) -> Iterable[Tuple[Index, CanvasValue, Index, CanvasValue]]:
        '''Returns a generator of all tuples (i, x, j, y) where i and j
        are distinct indices of cells within this Canvas and x = self[i]
        and y = self[j]. Skips cells that contain None.'''
        pass

    def addrs_containing_value(self, v: Value) -> Iterable[Index]:
        return (a for a in self.all_addrs() if self[a] == v)

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

#    def all_matching(self, v: CanvasValue) -> List[int]:
#        return [
#            i + 1
#                for i, x in enumerate(self.contents)
#                    if x == v
#        ]

    def all_matching(self, v: CanvasValue) -> List[int]:
        target = self.v_to_target(v)
        return [
            i
                for i, v in self.all_indices_and_values()
                    if self.is_match((i, v), target)
        ]

    def is_match(
        self,
        candidate: Tuple[MaybeIndex, CanvasValue],
        target: Tuple[MaybeIndex, CanvasValue]
    ) -> bool:
        ci, cv = candidate
        ti, tv = target
#        if ci is None or ti is None:
#            pass
#        else:
#            if ci != ti:
#                return False
#        return cv == tv
        match (ci, ti):
            case (None, _):
                pass
            case (_, None):
                pass
            case (int(), int()):
                if ci != ti:
                    return False
                else:
                    pass
            case (int(), list()):
                if ci not in ti:  # type: ignore[operator]
                    return False
                else:
                    pass
        return cv == tv

    def v_to_target(self, v: CanvasValue) \
    -> Tuple[MaybeIndex, CanvasValue]:
        ii: MaybeIndex = None
        vv: CanvasValue = None
        lo('VTO', v)
        if v is None:
            return (None, None)
        else:
            for x in v:
                match x:
                    case '(':
                        ii = 1
                    case ')':
                        ii = len(self.contents)
                    case '-':
                        ii = list(range(2, len(self.contents)))
                        if not ii:
                            ii = None
                    case _:
                        vv = x
            return (ii, vv)

    def all_indices_and_values(self) -> Iterable[Tuple[Index, CanvasValue]]:
        for i, x in enumerate(self.contents):
            yield i + 1, x

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

# TODO rm; Subst.as_index() does this correctly
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
@dataclass(frozen=True)
class DetPainter:
    '''A determinate painter: it can paint one thing in one place; there is
    no matching or searching to be done in order to run it.'''
    subst: Subst
    source: DetAddr
    target: Union[DetAddr, SoupRef]
    func: Func
    prob_weight: Numeric
    basis: Optional[Painter] = None  # what this DetPainter was made from

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

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'({addr_str(self.source)}, {addr_str(self.target)}, {func_str(self.func)}; {nf(self.prob_weight)})'

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

    def as_index(self, expr: Expr) -> Index:
        '''Same as .simplify() but Fizzles if the result is not an Index.'''
        result = self.simplify(expr)
        if isinstance(result, int):
            return result
        else:
            raise FizzleNotIndex(expr)

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

### Exceptions ###

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class FizzleNotIndex(Fizzle):
    e: Expr

    def __str__(self) -> str:
        return f"can't reduce to Index: {repr(self.e)}"

def is_painter(x: Func) -> TypeGuard[Painter]:
    #return isinstance(x, tuple)
    match x:
        case (i, j, func):
            return True
        case _:
            return False

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
        if is_painter(self.v):
            return f'{cl}({painter_str(self.v)})'
        else:
            return f'{cl}({short(self.v)})'

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

### Distinctive to majaqb.py ###

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )
    do_spont: bool = True

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
        for p in self.lts.painters:
            print(painter_str(p))
        print()
        self.set_canvas(s)
        # TODO Add indentation, nicer logging?
        self.regen(nsteps)

    def regen(self, nsteps: int=20) -> None:
        for t in range(1, nsteps + 1):
            lo(f'{newline}{newline}t={t}')
            self.regen_timestep()

    def regen_timestep(self) -> None:
        '''Runs one timestep of regeneration.'''
        self.ws.decay()
        with logging(None):
            soups = Soup.union(self.lts, self.ws)
            det_painters = list(chain.from_iterable(
                self.painter_to_detpainters(p, soups.clarity(p))
                    for p in soups
            ))
            weights = [
                self.detpainter_to_probability_weight(dp)
                    for dp in det_painters
            ]
            for ii in range(len(det_painters)):
                lo(det_painters[ii], nf(weights[ii]))
            lo()
                
            ii = choices(range(len(det_painters)), weights)[0]
            dp = det_painters[ii]
            lo('dp =', dp, '  ', nf(weights[ii]))
            try:
                self.run_detpainter(dp)
            except Fizzle as exc:
                lo('FIZZLED:', exc)
            lo('\n' + self.state_str())

    def detpainter_to_probability_weight(self, dp: DetPainter) -> Numeric:
        return (
            self.source_weight(dp.source)
            *
            self.target_weight(dp.target)
            *
            dp.prob_weight
        )

    def source_weight(self, a: DetAddr) -> Numeric:
        match a:
            case int():
                return self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case p if is_painter(p):
                return 1.0  # TODO: find out painter "clarity"
        assert False, "source_weight(): should not go past 'match' stmt"

    def target_weight(self, a: DetAddr) -> Numeric:
        match a:
            case int():
                return 1.0 - self.canvas.clarity(a) / self.canvas.MAX_CLARITY
            case SoupRef():
                return 1.0
            case p if is_painter(p):
                return 1.0  # TODO: find out painter "clarity"
        assert False, "target_weight(): should not go past 'match' stmt"

    def painter_to_detpainters(self, p: Painter, prob_weight: Numeric) \
    -> Iterable[DetPainter]:
        source, target, func = p
        for subst, i in self.matching_detaddrs(empty_subst, I, source):
            #lo('SOURCE', source, i)
            for subst, j in self.matching_detaddrs(subst, J, target):
                # TODO apply the subst to func?
                #lo('TARGET', target, j)
                ff = self.eval_as_func(subst, func)
                dp = DetPainter(subst, i, j, ff, prob_weight, p)
                if dp.is_valid_for(self.canvas):
                    yield dp

    def matching_detaddrs(self, subst: Subst, var: Variable, addr: Addr) \
    -> Iterable[Tuple[Subst, DetAddr]]:
        #lo('MDA', addr)
        match addr:
            case str():
                for a in self.canvas.addrs_containing_value(addr):
                    yield (subst.unify(var, a), a)
            case int():
                yield (subst.unify(var, addr), addr)
            case p if is_painter(p):
                yield from self.ws.matching_painters(addr)
            case SoupRef():
                yield (subst, addr)
            case _:
                raise NotImplementedError(addr)

    def run_detpainter(self, dp: DetPainter) -> None:
        oldval = self.get_value(dp.source)
        newval = self.apply_func(dp.subst, dp.func, oldval)
        self.paint(dp.target, newval)

    def eval_as_func(self, subst: Subst, x: Func) -> Func:
        #print('EAF', x, type(x))
        #print('EAF SUBST', subst)
        match x:
            case x if callable(x):
                return x
            case (i, j, f):
                # NEXT TODO Rule out painters that can't run
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
            case ws if ws is WorkingSoup:
                return self.ws
            case (ws, p) if ws is WorkingSoup and is_painter(p):  # type: ignore[has-type]
                return addr
            case p if is_painter(p):
                return addr
            case _:
                raise NotImplementedError(addr)
        assert False, "get_value(): should not go past 'match' stmt"
        return same # Needed only to please mypy; stops [return] error

    def paint(self, addr: Addr, value: Union[Value, Painter]) -> None:
        lo('PAINT', short(addr), short(value))
        match (addr, value):
            case (int(), _) if is_canvasvalue(value):
                self.canvas[addr] = value
            case (ws, p) if ws is WorkingSoup and is_painter(p):
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

    def set_canvas(self, s: str) -> None:
        self.canvas = Canvas1D.make_from(s)

    def state_str(self) -> str:
        sio = StringIO()
        print('canvas:', self.canvas.state_str(), file=sio)
        print(self.ws.state_str(), file=sio)
#        for pstr in sorted(painter_str(p) for p in self.ws.painters):
#            print(pstr, file=sio)
        return sio.getvalue()

    def short(self) -> str:
        return self.__class__.__name__

m: Model

def run_ajaqb(seed: str='a    ', ltm: List[str]=['ajaqb']) -> None:
    '''Run the whole simulation. Absorb 'ajaqb' and see what regenerates from
    'a    '.'''
    global m
    m = Model()
    for s in ltm:
        m.absorb(s)
    m.regen_from(seed, nsteps=120)
    #print(short(m.canvas))

def run_abs() -> None:
    global m
    m = Model(do_spont=False)
    m.lts = Soup.make_from([
        (1, 3, same),
        (3, 5, succ),
        (1, 2, 'j'),
        (3, 4, 'q'),
        (1, 5, succ)
    ])
    m.regen_from('a    ', nsteps=20)

def run_4(seed='a    ') -> None:
    run_ajaqb(ltm=['ajaqb', 'cjcqd', 'mjmqn', 'ijklm'], seed=seed)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        #seed = 4993487641984628738  #None
        seed = None
    else:
        seed = int(sys.argv[1])
    seed = reseed(seed)
    print(f'seed={seed}')
    print()

    run_ajaqb()
    #run_abs()
