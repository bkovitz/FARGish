# Model.py -- The canvas-and-painters model

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from util import Numeric, short


Addr = Numeric

Value = Hashable
ValueTup = Tuple[Value, ...]

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
    def as_tuple(self) -> ValueTup:
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
        

@dataclass(frozen=True)
class DeterminateAddress:
    canvas: Canvas
    abs_addr: int


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

    def all_addrs(self) -> Iterable[Addr]:
        return range(1, len(self.contents) + 1)

    def as_tuple(self) -> ValueTup:
        return as_tuple(self.contents)

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

    def __str__(self) -> str:
        items = ' '.join(short(x) for x in self.contents)
        citems = ' '.join(short(c) for c in self.clarities)
        return f'[{items}]'
        #return f'[{items}]{newline}[{citems}]'

    def short(self) -> str:
        return ''.join(
            ' ' if x is None else x
                for x in self.contents
        )

Func = Callable[[Value], Value]
Painter = Tuple[Addr, Addr, Func]

def set_value(dtarget: DeterminateAddress) -> None:
    pass

def get_value(dsource: DeterminateAddress) -> Value:
    pass

def source_of(p: Painter) -> Addr:
    return p[0]

def target_of(p: Painter) -> Addr:
    return p[1]

def func_of(p: Painter) -> Func:
    return p[2]

def run_painter_on_canvas(
    p: Painter, c: Canvas
) -> None:
    dsource = to_determinate_address(source_of(p))
    dtarget = to_determinate_address(dtarget_of(p))

    set_value(dtarget, func_of(p)(get_value(dsource)))
    

@dataclass
class Model:
#    lts: LongTermSoup
#    ws: WorkingSoup
#    canvas: Canvas
#
#    def update(self) -> None:
#        self.run_painter(self.choose_painter())
#
#    def run_painter(self, p: Painter) -> None:
#        pass
#
#    def choose_painter(self) -> Painter:
#        pass

    @classmethod
    def to_determinate_address(cls, addr: Addr) -> DeterminateAddress:
        pass
        

def succ(v: str) -> str:
    return chr(ord(v) + 1)

p = (1, 2, succ)
c = Canvas1D.make_from('a  ')
#run_painter_on_canvas(p, c)
c[2] = succ(c[1])

print(c)
