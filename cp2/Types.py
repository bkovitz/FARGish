# Types.py -- Type hints used throughout the canvas-and-painters model.
#             Functions to convert between types.

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, no_type_check, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from util import empty_set, force_setattr, short


@dataclass(frozen=True)
class SoupRef:
    name: str

    def __str__(self) -> str:
        return self.name

WorkingSoup = SoupRef('WorkingSoup')
LongTermSoup = SoupRef('LongTermSoup')

CanvasValue = str
Index = int   # the index of a cell within a Canvas
MaybeIndex = Union[Index, None, List[Index]]  # TODO rename or divide into type types: List[Index] is not always appropriate
Expr = Any
Addr = Any
Func = Any
Painter = Tuple[Addr, Addr, Func]
Value = Union[CanvasValue, Painter, Func]


@dataclass(frozen=True)
class AnnotationType:
    name: str

    def __str__(self) -> str:
        return self.name

Anchor = AnnotationType('Anchor')

@dataclass(frozen=True)
class Annotation:
    type: AnnotationType
    name: str

    def __str__(self) -> str:
        return self.name

Start = Annotation(Anchor, 'Start')
End = Annotation(Anchor, 'End')

@dataclass(frozen=True)
class Annotations:
    elems: FrozenSet[Annotation]

    #TODO Shouldn't this method allow multiple arguments?
    @classmethod
    def make_from(cls, v: Union[Annotation, Annotations, None]) -> Annotations:
        match v:
            case Annotation():
                return cls(frozenset([v]))
            case Annotations():
                return v
            case None:
                return empty_annotations
                
    def elems_str(self) -> str:
        return ', '.join(sorted([short(e) for e in self.elems]))

    def __add__(self, a: Annotation | Annotations) -> Annotations:
        match a:
            case Annotation():
                return Annotations(self.elems | frozenset([a]))
            case Annotations():
                return Annotations(self.elems | a.elems)

    def __iter__(self, *args, **kwargs) -> Iterator[Annotation]:
        return self.elems.__iter__(*args, **kwargs)

    def __contains__(self, v: Annotation) -> bool:
        return v in self.elems

    def __ge__(self, other: Annotations) -> bool:
        return self.elems >= other.elems

    def __str__(self) -> str:
        return f'Annotations({self.elems_str()})'

empty_annotations = Annotations(empty_set)

@dataclass(frozen=True)
class CellBundle:
    '''Everything that can be held simultaneously in one cell, i.e. up to one
    value and any number of annotations, all bundled into one convenient
    object.'''
    value: Optional[CanvasValue]
    annotations: Annotations

    @classmethod
    def make_from(cls, *v: CellContent) -> CellBundle:
        result = cls(None, empty_annotations)
        for vv in v:
            result = result + vv
        return result

    def __iter__(self) -> Iterable[CellContent1]:
        if self.value is not None:
            yield self.value
        yield from self.annotations

    def __add__(self, v: CellContent) -> CellBundle:
        match v:
            case str():
                if v == self.value:
                    return self
                else:
                    return replace(self, value=v)
            case None:
                if v == self.value:
                    return replace(self, value=None)
                else:
                    return self
            case Annotation():
                if v in self.annotations:
                    return self
                else:
                    return replace(self, annotations=self.annotations + v)
            case Annotations():
                if v == self.annotations:
                    return self
                else:
                    return replace(self, annotations=self.annotations + v)
            case CellBundle():
                result = self
                for vv in v:  # type: ignore[attr-defined]  # mypy bug
                    result = result + vv
                return result
        assert False, f"CellBundle.__add__(): should not go past 'match' stmt; {v}"

    def is_match(self, v: CellContent) -> bool:
        '''Returns True if 'self' contains all the content within 'v', False
        if 'v' contains any value or annotation not in 'self'.'''
        match v:
            case str():
                return v == self.value
            case None:
                return True
            case Annotation():
                return v in self.annotations
            case Annotations():
                return self.annotations >= v
            case CellBundle():
                return all(self.is_match(vv) for vv in unbundle_cell_content(v))

    def __str__(self) -> str:
        return f'CellBundle({short(self.value)}; {self.annotations.elems_str()})'

empty_cell_bundle = CellBundle(None, empty_annotations)

CellContent1 = Union[CanvasValue, None, Annotation]
CellContent = Union[CellContent1, Annotations, CellBundle]

def unbundle_cell_content(v: CellContent) -> Iterable[CellContent1]:
    match v:
        case str():
            yield v
        case None:
            pass
        case Annotation():
            yield v
        case Annotations():
            yield from v
        case CellBundle():
            yield v.value
            yield from v.annotations

# A way to refer to a cell's value or an annotation within the cell
Index2 = Union[Index, Tuple[Index, AnnotationType]]

@no_type_check  # crashes mypy 0.971
def extract_index(i: Index2) -> Index:
    match i:
        case int():
            return i
        case (int(ii), _):
            return ii
    assert False, f"extract: should not go past 'match' stmt, {i}"

@dataclass(frozen=True)
class Indices:
    '''Zero or more Index2s.'''
    elems: Tuple[Index2, ...]

    def __init__(self, *i: Index2):
        force_setattr(self, 'elems', tuple(i))

DetAddr = Union[Index, Indices, Painter, SoupRef]  # A determined Addr

@dataclass(frozen=True)
class Variable:
    name: str

    def __str__(self) -> str:
        return self.name

I = Variable('i')
J = Variable('j')
F = Variable('f')

class HasAsIndex(ABC):

    @abstractmethod
    def as_index(self) -> Index:
        pass

def is_index(e: Any) -> TypeGuard[Index]:
    return isinstance(e, int)

# TODO rm; Subst.as_index() does this correctly
def as_index(e: Expr) -> Index:
    if isinstance(e, int):
        return e
    elif isinstance(e, HasAsIndex):
        return e.as_index()
    else:
        raise Fizzle(f"as_index: Can't convert {e!r} to index")

def is_painter(x: Func) -> TypeGuard[Painter]:
    #return isinstance(x, tuple)
    match x:
        case (i, j, func):
            return True
        case _:
            return False

def painter_str(p: Painter) -> str:
    i, j, func = p
    return f'({addr_str(i)}, {addr_str(j)}, {func_str(func)})'

def addr_str(a: Addr) -> str:
    if isinstance(a, int):
        return str(a)
    elif isinstance(a, str):
        return repr(a)
    elif isinstance(a, SoupRef):
        return a.name
    elif is_painter(a):
        return painter_str(a)
    elif isinstance(a, Variable):
        return a.name
    else:
        return str(a)

def func_str(func: Func) -> str:
    if callable(func):
        return short(func)
    elif is_painter(func):
        return painter_str(func)
    else:
        return repr(func)

### Exceptions ###

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class FizzleNotIndex(Fizzle):
    e: Expr

    def __str__(self) -> str:
        return f"can't reduce to Index: {repr(self.e)}"
