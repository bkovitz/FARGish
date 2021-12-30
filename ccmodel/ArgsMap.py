# ArgsMap.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from FMTypes import Value, Pred
from CCTypes import HasHasTag, HasWithTag
from Fizzle import Fizzle
from util import short, as_tuple, is_dataclass_instance, as_dict, force_setattr
if TYPE_CHECKING:
    from Tag import Tag, tagmatch
    from Canvas import Cell, CellRef


def as_argsmap(x: Any) -> ArgsMap:
    # NEXT To prevent circular import, add a .as_argsmap() method to
    # ArgsMap, Cell, and CellRef. This will remove the need to import
    # is_cell_or_cellref(), Cell, and CellRef. Here, just test for the
    # .as_argsmap() attribute.
    """
    if isinstance(x, ArgsMap):
        return x
    elif is_cell_or_cellref(x):
        return as_argsmap(x.get())
    """
    if hasattr(x, 'as_argsmap'):
        return x.as_argsmap()
    elif is_dataclass_instance(x) or isinstance(x, dict):
        return ArgsDict(as_dict(x))
    else:
        return empty_args_map

class ArgsMap(HasHasTag, HasWithTag, ABC):
    '''An ArgsMap is immutable. To 'add' key-value pairs to an ArgsMap, you
    must .prepend() another ArgsMap to it.'''

    def get(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        if k == 'args':
            return self
        else:
            return self.xget(k, default)

    @abstractmethod
    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        pass

    @abstractmethod
    def is_empty(self) -> bool:
        pass

    def prepend(self, args: ArgsMap) -> ArgsMap:
        return ArgsMapSeries.make(args, self)

    @classmethod
    def empty(cls) -> EmptyArgsMap:
        return empty_args_map

    def with_tag(self, *tag: Tag) -> ArgsMap:
        # TODO Don't add tags that are already there?
        # TODO Just return self if self already has all the tags.
        return ArgsMapWithTags(self, tag)

    def has_tag(self, pred: Pred) -> bool:
        return False

    """
    def short(self) -> str:
        # TODO
        return '(empty)'
    """


@dataclass(frozen=True)
class ArgsMapWithTags(ArgsMap):
    argsmap: ArgsMap
    tags: Tuple[Tag, ...]

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        return self.argsmap.xget(k, default)

    def is_empty(self) -> bool:
        return self.argsmap.is_empty()

    def has_tag(self, pred: Pred) -> bool:
        return any(tagmatch(t, pred) for t in self.tags)

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.argsmap)}, tags={short(self.tags)})'

@dataclass(frozen=True)
class ArgsDict(ArgsMap):
    d: Dict[str, Value]

    def is_empty(self) -> bool:
        return not self.d

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        return self.d.get(k, default)

    def __hash__(self) -> int:
        return hash(self.d.values())

    def __eq__(self, other) -> bool:
        if not isinstance(other, ArgsDict):
            return False
        else:
            return self.d == other.d

    def short(self) -> str:
        return ' '.join(f'{short(k)}={short(v)}' for k, v in self.d.items())

class EmptyArgsMap(ArgsMap):

    def is_empty(self) -> bool:
        return True

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        return default

empty_args_map: EmptyArgsMap = EmptyArgsMap()

@dataclass(frozen=True)
class ArgsMapSeries(ArgsMap):
    argss: Tuple[ArgsMap, ...]

    def is_empty(self) -> bool:
        return all(args.is_empty() for args in self.argss)

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        for args in self.argss:
            v = args.get(k)
            if v is not None:
                return v
        return default

    @classmethod
    def make(cls, *argss: Union[None, Dict[str, Value], ArgsMap]) \
    -> ArgsMapSeries:
        # TODO Special treatment for empty and for existing ArgsMapSeries?
        return ArgsMapSeries(as_tuple(cls.flatten(argss)))

    @classmethod
    def flatten(cls, argss: Iterable[Union[None, Dict[str, Value], ArgsMap]]) \
    -> Iterable[ArgsMap]:
        for args in argss:
            if args is None:
                continue
            elif isinstance(args, dict):
                if not args:
                    continue
                else:
                    yield ArgsDict(args)
            elif args.is_empty():
                continue
            elif isinstance(args, ArgsMapSeries):
                yield from cls.flatten(args.argss)
            else:
                yield args

    def has_tag(self, pred: Pred) -> bool:
        return any(args.has_tag(pred) for args in self.argss)

@dataclass(frozen=True)
class Avails(ArgsMap):
    values: Tuple[Value, ...]

    def __init__(self, *values: Value):
        force_setattr(self, 'values', values)

    def is_empty(self) -> bool:
        return False

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        if k == 'avails':
            return self
        else:
            return default

    def add_avail(self, v: Value):
        return Avails(*(self.values + (v,)))

    def has_avail(self, v: Value) -> bool:
        return v in self.values

    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Sequence[Value], Avails]:
        remaining_avails: List[Value] = list(self.values)
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
        return (taken_avails, Avails(*remaining_avails))

    def short(self) -> str:
        if self.values:
            return ' '.join(short(v) for v in self.values)
        else:
            return '(no avails)'

@dataclass(frozen=True)
class ValuesNotAvail(Fizzle):
    #container: Hashable  # Change this to a CellRef?
    #cellref: Union[CellRef, None] = None
    avails: Tuple[Value, ...] = ()
        # These values were avail; indices match indices in seeker's request
    unavails: Tuple[Value, ...] = ()
        # These values were unavail; indices match indices in seeker's request

    '''
    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.actor)}, {short(self.codelet)}, {self.cellref}, avails={self.avails}, unavails={self.unavails})'

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.cellref)}, {short(self.avails)}, {short(self.unavails)})'
    '''

