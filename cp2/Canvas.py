# Canvas.py -- The 'Canvas' and 'Canvas1D' classes

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, CanvasValue, DetAddr, FizzleValueNotFound, Index, \
    MaybeIndex, Value
from Log import lo
from util import short, Numeric


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
        #lo('VTO', v)
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
