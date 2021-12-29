# Program.py -- Abstract base class for Program, and associated types

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    get_type_hints, runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from util import force_setattr, short, as_dstr, is_type_instance
if TYPE_CHECKING:
    from Canvas import CellContents


if TYPE_CHECKING:
    ProgramResult = Union[Produced, CellContents]

@dataclass(frozen=True)
class Produced:
    '''Something produced by a run of something, which probably should be
    deposited into the next cell of the current canvas, if appropriate.'''
    v: CellContents

class Program(ABC):

    @abstractmethod
    def run(self, **kwargs) -> ProgramResult:
        pass

    def short(self) -> str:
        return as_dstr(self)

    def __str__(self) -> str:
        return short(self)
