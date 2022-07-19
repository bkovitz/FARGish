# ajaqb.py -- Just enough model to run the ajaqb test

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from util import Numeric, short, as_tuple, pts, force_setattr

@dataclass
class Soup:
    painters: Set = field(default_factory=lambda: set())

    def add(self, p: Painter) -> None:
        self.painters.add(p)

class WorkingSoup(Soup):
    pass


Value = Hashable
Index = int   # the index of a cell within a Canvas
Expr = Hashable   # Restrict this better?
Addr = Union[Index, str, Type[WorkingSoup], Expr]
Func = Union[Callable[[Value], Value], Value]
Painter = Tuple[Addr, Addr, Expr]

def run_all() -> None:
    '''Run the whole simulation. Absorb 'ajaqb' and see what regenerates from
    'a    '.'''
    m = Model()
    m.absorb('ajaqb')
    #m.regen_from('a    ')
    print(m.canvas)
    for p in m.lts.painters:
        print(painter_str(p))

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

DeterminateAddr = CanvasAddr

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


@dataclass(frozen=True)
class Variable:
    name: str

    def __str__(self) -> str:
        return self.name

I = Variable('i')
J = Variable('j')

@dataclass(frozen=True)
class Plus:
    args: Tuple[Expr]

    def __init__(self, *args: Expr):
        force_setattr(self, 'args', args)

    def __str__(self) -> str:
        return '+'.join(str(a) for a in self.args)

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
    elif isinstance(a, tuple):
        return painter_str(a)  # type: ignore[arg-type]
    elif isinstance(a, Variable):
        return a.name
    else:
        return str(a)

def func_str(func: Func) -> str:
    if callable(func):
        return short(func)
    elif isinstance(func, tuple):
        return painter_str(func)  # type: ignore[arg-type]
    else:
        return repr(func)

@dataclass
class Model:
    lts: Soup = field(default_factory=lambda: Soup())
    ws: Soup = field(default_factory=lambda: Soup())
    canvas: Canvas1D = field(
        default_factory=lambda: Canvas1D.make_from('     ')
    )

    def absorb(self, s: str) -> None:
        for i, j, func in self.find_related_cells(s):
            #print(i, j, func)
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

if __name__ == '__main__':
    run_all()
