# Types.py -- Type hints used throughout the canvas-and-painters model.
#             Functions to convert between types.

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, no_type_check, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Log import trace, lo, set_log_level
from util import empty_set, first, force_setattr, short


@dataclass(frozen=True)
class Letter:
    '''A letter in the range a..z.'''
    c: str

    def __post_init__(self):
        if self.c < 'a' or self.c > 'z':
            raise ValueError(f"Letter {self.c!r}: must be in range 'a'..'z'.")

    @classmethod
    def from_str(self, c: str) -> Union[Letter, Blank]:
        if len(c) != 1:
            raise ValueError('Letter.from_str(): {c!r} must have len==1')
        if c == ' ':
            return Blank()
        else:
            return Letter(c)

    def succ(self) -> Letter:
        if self.c >= 'z':
            raise FizzleNoSucc
        else:
            return Letter(chr(ord(self.c) + 1))

    def pred(self) -> Letter:
        if self.c <= 'a':
            raise FizzleNoPred
        else:
            return Letter(chr(ord(self.c) - 1))

    def short(self) -> str:
        return repr(self.c)

@dataclass(frozen=True)
class Blank:

    def short(self) -> str:
        return 'Blank'

    __str__ = short
    __repr__ = short

CanvasValue = Union[Letter, Blank]  # TODO change str to Letter
#Index = int   # the index of a cell within a Canvas
#MaybeIndex = Union[Index, None, List[Index]]  # TODO rename or divide into type types: List[Index] is not always appropriate
Expr = Any
#Addr = Any  # An address, possibly with variables, patterns, special objects
#            # that search for matches; possibly a Painter.

#Func = Any  # A Value, Painter, or callable 
# The above are defined as Any only because mypy can't handle recursive type
# definitions, which would result from defining Painter as containing them.
#Painter = Tuple[Addr, Addr, Func]
#Value = Union[CanvasValue, Painter, Func]


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
Inextreme = Annotation(Anchor, 'Inextreme')

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

    def simplest(self) -> Union[None, Annotation, Annotations]:
        match len(self.elems):
            case 0:
                return None
            case 1:
                return first(self.elems)
            case _:
                return self

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

    def __bool__(self) -> bool:
        return bool(self.elems)

    def __str__(self) -> str:
        return f'Annotations({self.elems_str()})'

empty_annotations = Annotations(empty_set)

@dataclass(frozen=True)
class CellBundle:
    '''Everything that can be held simultaneously in one cell, i.e. up to one
    value and any number of annotations, all bundled into one convenient
    object.'''
    value: Union[None, Letter, Blank]
    annotations: Annotations

    # TODO During __post_init__, check if value is a space; if so, change
    # it to None

    @classmethod
    def make_from(cls, *v: Union[CellContent, str]) -> CellBundle:
        result = cls(Blank(), empty_annotations)
        for vv in v:
            result = result + vv
        return result

    def value_only(self) -> bool:
        '''Does this CellBundle contain only a value and no annotations?'''
        return not self.annotations

    def simplest(self) -> Any:  # TODO restore type hint: Union[Value, Annotation, Annotations, CellBundle]:
        if self.value_only():
            return self.value
        elif self.value is None:
            return self.annotations.simplest()
        else:
            return self

    def __iter__(self) -> Iterable[CellContent1]:
        if self.value is not None:
            yield self.value
        yield from self.annotations

    def __add__(self, v: CellContent | str) -> CellBundle:
        match v:
            case str():
                l = Letter.from_str(v)
                if l == self.value:
                    return self
                else:
                    return replace(self, value=l)
            case Letter():
                if v == self.value:
                    return self
                else:
                    return replace(self, value=v)
            case Blank():
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
#            case str():
#                return v == self.value
#            case None:
#                return True
            case Letter(c):
                return c == self.value
            case Blank():
                return c == self.value
            case Annotation():
                return v in self.annotations
            case Annotations():
                return self.annotations >= v
            case CellBundle():
                return all(self.is_match(vv) for vv in unbundle_cell_content(v))

    def __repr__(self) -> str:
        return f'CellBundle({short(self.value, inside=True)}; {self.annotations.elems_str()})'

empty_cell_bundle = CellBundle(None, empty_annotations)

#CellContent1 = Union[CanvasValue, None, Annotation]
CellContent1 = Union[Letter, Blank, Annotation]
CellContent = Union[CellContent1, Annotations, CellBundle]

def is_cell_content(x: Any) -> TypeGuard[CellContent]:
    return (
        isinstance(x, str)
        or
        isinstance(x, CellBundle)
        or
        isinstance(x, Annotation)
        or
        isinstance(x, Annotations)
        or
        x is None
    )

def unbundle_cell_content(v: CellContent) -> Iterable[CellContent1]:
    match v:
        case str():
            yield v
#        case None:
#            pass
        case Annotation():
            yield v
        case Annotations():
            yield from v
        case CellBundle():
            if v.value is not None:
                yield v.value
            yield from v.annotations

#def is_painter(x: Any) -> TypeGuard[Painter]:
#    #return isinstance(x, tuple)
#    match x:
#        case (i, j, func):
#            return True
#        case _:
#            return False

#def is_func(x: Any) -> TypeGuard[Func]:
#    match x:
#        case str():
#            return True
#        case (i, j, func):
#            return True
#        case c if callable(c):
#            return True
#    return False
            
def is_space(cc: CellContent) -> bool:
    match cc:
        case str():
            return cc == ' '
        case None:
            return False
        case CellBundle():
            return cc.simplest() == ' '
        case _:
            return False

#def painter_str(p: Painter) -> str:
#    i, j, func = p
#    return f'({addr_str(i)}, {addr_str(j)}, {func_str(func)})'

#def func_str(func: Func) -> str:
#    if callable(func):
#        return short(func)
#    elif is_painter(func):
#        return painter_str(func)
#    else:
#        return repr(func)

### Exceptions ###

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class FizzleValueNotFound(Fizzle):
    v: Any  # TODO restore type hint: Value

    def __str__(self) -> str:
        return f'value not found: {repr(self.v)}'

@dataclass(frozen=True)
class FizzleNotIndex(Fizzle):
    e: Expr

    def __str__(self) -> str:
        return f"can't reduce to Index: {repr(self.e)}"

@dataclass(frozen=True)
class FizzleGotNone(Fizzle):
    o: Any   # Whatever painter or func or whatever fizzled
    e: Any   # Whatever evaluated to None and shouldn't have
    
    def __str__(self) -> str:
        return f'{short(self.o)}: {short(self.e)} evaluated to None'

@dataclass(frozen=True)
class FizzleNoDetPainters(Fizzle):

    def __str__(self) -> str:
        return 'No determinate painters'

@dataclass(frozen=True)
class FizzleNoSucc(Fizzle):

    def __str__(self) -> str:
        return 'No successor'

@dataclass(frozen=True)
class FizzleNoPred(Fizzle):

    def __str__(self) -> str:
        return 'No predecessor'
