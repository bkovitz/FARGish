
from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from Canvas import CellRef
from FMTypes import Value


@dataclass(frozen=True)
class Agent:
    pass
    """
    born: Codelets = None
    wake: Codelets = None
    snag: Codelets = None
    delegate_succeeded: Codelets = None
    delegate_failed: Codelets = None
    succeeded: Codelets = None
    failed: Codelets = None
    """

@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str
    
    def __call__(self, *operands) -> int:  # HACK Numbo-specific return type
        return self.func(*operands)

    def __str__(self):
        return self.name

@dataclass(frozen=True)
class Consume(Agent):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value, ...], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    dest: Union[CellRef, None] = None    # where to paint result

