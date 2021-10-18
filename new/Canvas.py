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
class CellRef(HasAvailValues):
    '''A reference to a cell in a Canvas.'''
    canvas: Union[Canvas, None] = None
    addr: Union[Addr, None] = None

    # TODO .next_cellref(), .last_nonblank_cellref(), cellrefs_forward()

    @property
    def value(self) -> Value:
        if (
            self.canvas is not None
            and
            self.addr is not None
        ):
            return self.canvas[self.addr]
        else:
            return None

    def has_a_value(self) -> bool:
        return self.value is not None

    def paint(self, v: Value) -> None:
        if (  # OAOO this if?  (might be hard with mypy)
            self.canvas is not None
            and
            self.addr is not None
        ):
            self.canvas[self.addr] = v

    @property
    def avails(self) -> Union[Sequence[Value], None]:
        if isinstance(self.value, HasAvailValues):
            return self.value.avails
        else:
            return None

    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        v = self.value
        if isinstance(v, HasAvailValues):
            try:
                return v.take_avails(values)
            except ValuesNotAvail as vna:
                raise replace(vna, cellref=self)
        else:
            raise ValuesNotAvail(self, tuple(values), ())

    def __str__(self):
        return f'canvas[{self.addr}]'

@dataclass(frozen=True)
class StepDelta:
    '''A change from one Step to another.'''
    before: Any    # What got replaced
    after: Any     # What replaced it
    how: Union[Any, None]   # Some clue about how it happened

    def seq_str(self):
        '''How to display this inside SeqState.__str__.'''
        return str(self)

@dataclass(frozen=True)
class Step(HasAvailValues):
    '''One *state* in a sequence of steps, i.e. a StepCanvas. The actual change
    that happened during one step is a StepDelta. The first ('zeroth') step in
    the sequence normally has no last_delta.'''
    avails: Union[Sequence[Value], None] = None
    last_delta: Union[StepDelta, None] = None

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

@dataclass(eq=False)
class StepCanvas(Canvas):
    '''A Canvas consisting of a sequence of Steps. A StepCanvas is mutable
    (as painters paint on it). It is possible for two StepCanvases in the same
    workspace to be identical, so equality comparison is only an id()
    comparison.'''
    steps: List[Union[Step, None]] = field(default_factory=list)
    
    def __hash__(self):
        return id(self)

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        #print('SEQCGET', addr, len(self.steps))
        if isinstance(addr, int) and addr < len(self.steps):
            return self.steps[addr]
        else:
            return None

    def __setitem__(self, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        # TODO Accept a builder argument?
        if isinstance(addr, int):
            while len(self.steps) <= addr:
                self.steps.append(None)
            self.steps[addr] = v  # type: ignore[call-overload]  # mypy bug?
        else:
            raise NotImplementedError(f'addr must be int; was {type(addr)}: {addr}')

    def __len__(self):
        return len(self.steps)

    def __str__(self):
        cl = self.__class__.__name__
        return f"{cl}({'; '.join(str(st) for st in self.steps)})"
