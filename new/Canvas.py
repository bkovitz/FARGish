# Canvas.py -- Canvases, Cells, things with the notion of avail values

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod

from FMTypes import Value, Addr
from FARGModel import Canvas, CellRef, ValuesNotAvail, HasAvailValues, Fizzle
from Log import trace, lo
from util import short, is_iter, as_iter, pr, pts


@dataclass(frozen=True)
class StepDelta:
    '''A change from one Step to another.'''
    before: Any    # What got replaced
    after: Any     # What replaced it
    how: Union[Any, None]   # Some clue about how it happened

    def seq_str(self):
        '''How to display this inside SeqState.__str__.'''
        return str(self)

    def short(self) -> str:
        if is_iter(self.before):
            operand = f' {short(self.how)} '
            return operand.join(str(b) for b in self.before)
        else:
            return short(self.how) + short(self.before)

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
                avails=tuple(taken_avails),
                unavails=tuple(missing_avails)
            )
        return (taken_avails, remaining_avails)

    def short(self) -> str:
        delta = short(self.last_delta) if self.last_delta else ''
        avs = ' '.join(str(a) for a in as_iter(self.avails))
        ls: List = []
        if delta:
            ls.append(delta)
        if avs:
            ls.append(avs)
        inside = '; '.join(ls)
        return f'[{inside}]'

@dataclass(eq=False)
class StepCanvas(Canvas):
    '''A Canvas consisting of a sequence of Steps. A StepCanvas is mutable
    (as painters paint on it). It is possible for two StepCanvases in the same
    workspace to be identical, so equality comparison is only an id()
    comparison.'''
    steps: List[Union[Step, None]] = field(default_factory=list)

    instance_count: ClassVar[int] = 0
    
    def __post_init__(self) -> None:
        self.__class__.instance_count += 1

    def __hash__(self):
        '''This is necessary to maintain determinism.'''
        return hash(self.instance_count)

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

    def next_addr(self, addr: Addr) -> Addr:
        if isinstance(addr, int):
            return addr + 1
        else:
            cl = self.__class__.__name__
            raise NotImplementedError(
                f"{cl} can't take next_addr of a non-int ({addr}, {type(addr)}."
            )

    def first_addr(self) -> Addr:
        return 0

    def addrs(self, starting_at: Optional[Addr]=None) -> Iterable[Addr]:
        if self.steps is None:
            return
        else:
            return range(len(self.steps))

    def __len__(self):
        return len(self.steps)

    def __str__(self):
        cl = self.__class__.__name__
        return f"{cl}({'; '.join(str(st) for st in self.steps)})"

    def short(self):
        ss: List[str] = []
        for step in self.steps:
            if step is None:
                ss.append('[]')
            else:
                ss.append(short(step))
        return ''.join(ss)

@dataclass(frozen=True)
class NotEnoughOperands(Fizzle):
    actual_num_operands: Optional[int] = None
    min_num_operands: Optional[int] = None

@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str
    min_num_operands: int = 2
    
    def __call__(self, *operands) -> int:  # HACK Numbo-specific return type
        if len(operands) < self.min_num_operands:
            raise NotEnoughOperands(
                actual_num_operands=len(operands),
                min_num_operands=self.min_num_operands
            )
        else:
            return self.func(*operands)

    def __str__(self):
        return self.name

    def __hash__(self):
        '''It's necessary to omit self.func from __hash__ in order to maintain
        determinism. The default hash code of a function is its address in
        memory, which is non-deterministic. If necessary, one could include
        str(self.func).'''
        return hash(str(self))

    def consume(self, source: CellRef, operands: Sequence[Value]) -> Step:
        taken_avails, remaining_avails = source.take_avails(operands)
        result = self(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = StepDelta(tuple(taken_avails), result, self)
        return Step(new_avails, delta)
