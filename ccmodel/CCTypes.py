# CCTypes.py -- Abstract types for the codelets-in-canvases model

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from FMTypes import Pred, as_pred, N, T, Value
from Program import Program
from Log import trace, lo
from util import as_dstr, short
if TYPE_CHECKING:
    from Tag import Tag
    from ArgsMap import ArgsMap
    from Canvas import Cell, CellRef


class HasWithTag:
    '''Mix-in for classes with a .with_tag() method, returning a new version
    of the object, containing the given tags.'''
    Q = TypeVar('Q', bound='HasWithTag')

    @abstractmethod
    def with_tag(self: Q, *tag: Tag) -> Q:
        pass

class HasAddTag:
    '''Mix-in for classes with an .add_tag() method, which adds the given
    tags to the object by modifying it.'''

    @abstractmethod
    def add_tag(self, *tag: Tag) -> None:
        pass

class HasHasTag:
    '''Mix-in for classes with a .has_tag() method.'''

    @abstractmethod
    def has_tag(self, pred: Pred) -> bool:
        '''Is 'self' tagged with a tag that matches 'pred'?'''
        pass

@dataclass(frozen=True)
class ValueWrapper:
    v: Value

class Codelet(Program, HasHasTag):

    def has_tag(self, pred: Pred) -> bool:
        return False
