# Canvas.py -- Canvases, Cells, things with the notion of avail values

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod

from FMTypes import Value, Addr


@dataclass(frozen=True)
class ValuesNotAvail(Exception):
    #container: Hashable  # Change this to a CellRef?
    cellref: Union['CellRef', None]
    avails: Tuple[Value, ...]
        # These values were avail; indices match indices in seeker's request
    unavails: Tuple[Value, ...]
        # These values were unavail; indices match indices in seeker's request

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cellref}, avails={self.avails}, unavails={self.unavails})'
        
class HasAvailValues(ABC):
    '''Mix-in for cells and other things that (can) have avail values.'''
    
    def has_avail_value(self, v: Value) -> bool:
        return (v in self.avails) if self.avails else False

    @abstractmethod
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        # TODO Require a determinate Collection, not just an Iterable (since
        # we might fail and need to put 'values' into an exception).
        pass

    @property
    @abstractmethod
    def avails(self) -> Union[Sequence[Value], None]:
        pass


class Canvas(ABC):
    '''Something on which items can be painted.'''

    # TODO get an iterator of all CellRefs, search for a value

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value) -> None:
        pass

@dataclass(frozen=True)
class CellRef:  #TODO (HasAvailValues):
    canvas: Union[Canvas, None] = None
    addr: Union[Addr, None] = None

@dataclass(frozen=True)
class Step(HasAvailValues):
    avails: Union[Sequence[Value], None] = None

    # TODO Put this into a WithAvails mix-in.
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        remaining_avails: List[Value] = \
            [] if self.avails is None else list(self.avails)
        taken_avails: List[Value] = []
        missing_avails: List[Value] = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                taken_avails.append(None)
                missing_avails.append(v)
            else:
                taken_avails.append(v)
                missing_avails.append(None)
        if any(t is None for t in taken_avails):
            raise ValuesNotAvail(
                None,
                tuple(taken_avails),
                tuple(missing_avails)
            )
        return (taken_avails, remaining_avails)
