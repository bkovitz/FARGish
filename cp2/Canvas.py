# Canvas.py -- The 'Canvas' and 'Canvas1D' classes

from __future__ import annotations
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeGuard, TypeVar, Union, \
    runtime_checkable, no_type_check, TYPE_CHECKING
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from abc import ABC, abstractmethod

from Types import Addr, Anchor, Annotation, Annotations, AnnotationType, \
    Blank, CanvasValue, \
    CellBundle, CellContent1, CellContent, End, FizzleValueNotFound, \
    Func, Index, Index2, Inextreme, Letter, MaybeIndex, Start, Value, \
    empty_cell_bundle, \
    extract_index, is_index, unbundle_cell_content
from Log import lo, trace
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
    def __getitem__(self, addr: Addr) -> Optional[CellContent]:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, x: CellContent) -> None:
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
    def addr_of(self, v: CanvasValue) -> Index:
        '''Returns the Index of the cell that contains v, or
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

    @abstractmethod
    def min_index(self) -> int:
        pass

    @abstractmethod
    def max_index(self) -> int:
        pass

@dataclass
class ContentAndClarity:
    content: CellContent1
    clarity: Numeric

    def paint(self, v: CellContent1) -> None:
#        if v is None:
#            pass
#        elif v == ' ':
#            self.dec_clarity()
        if v is None:
            self.dec_clarity()
        elif v == self.content:
            self.inc_clarity()
        else:
            if self.clarity == 0:
                self.content = v
                self.clarity = 1
            else:
                self.dec_clarity()

    def inc_clarity(self) -> None:
        if self.clarity < Canvas.MAX_CLARITY:
            self.clarity += 1

    def dec_clarity(self) -> None:
        if self.clarity > 0:
            self.clarity -= 1
        if self.clarity == 0:
            self.content = Blank()

    def set_clarity(self, clarity: Numeric) -> None:
        self.clarity = clarity
        if self.clarity == 0:
            self.content = Blank()

@dataclass
class ContentsAndClarities:
    d: Dict[Index2, ContentAndClarity] = field(default_factory=dict)
    min_index: Index = 1
    max_index: Index = 10

    def __setitem__(self, i: Index2, v: CellContent1) -> None:
        if is_index(i):  # WRONG: need to extract index from i
            pass # TODO check bounds
        if i in self.d:
            self.d[i].paint(v)
        else:
            self.d[i] = ContentAndClarity(v, 1)

    def __getitem__(self, i: Index2) -> Optional[CellContent]:
        try:
            return self.d[i].content
        except KeyError:
            return None

    @no_type_check  # crashes mypy 0.971
    def __iter__(self) -> Iterator[Tuple[Index, CellContent1]]:
        for i, v in self.d.items():
            match i:
                case int():
                    yield i, v.content
                case (int(ii), AnnotationType()):
                    yield ii, v.content

    def clarity(self, i) -> Numeric:
        try:
            return self.d[i].clarity
        except KeyError:
            return 0

    def set_clarity(self, i: Index2, clarity: Numeric) -> None:
        if i in self.d:
            self.d[i].set_clarity(clarity)

    def all_indices(self) -> Iterable[Index]:
        '''Returns all the possible indices of cell values, whether anything
        has been stored there or not. Does not include indices for
        Annotations.'''
        return range(self.min_index, self.max_index + 1)

    def all_indices_and_values(self) -> Iterable[Tuple[Index, CanvasValue]]:
        for i in self.all_indices():
            yield (i, self[i])  # type: ignore[misc]

    def all_indices_and_bundles(self) -> Iterable[Tuple[Index, CellBundle]]:
        results: Dict[Index, CellBundle] = dict()
        for i, v in self.d.items():
            ii = extract_index(i)
            if ii in results:
                results[ii] = results[ii] + v.content
            else:
                results[ii] = CellBundle.make_from(v.content)
        for ii in sorted(results.keys()):
            yield ii, results[ii]

    def as_bundle(self, i: Index) -> CellBundle:
        result = empty_cell_bundle
        for i2, cc in self.d.items():
            if extract_index(i2) != i:
                continue
            result = result + cc.content
        return result

    # TODO UT
    def all_indices2(self) -> Iterable[Index2]:
        '''Returns the indices of every element stored, both cell values (which
        have an Index) and annotations (which have (Index, AnnotationType)).'''
        return self.d.keys()

    def all_values(self) -> List[CanvasValue]:
        '''Returns a list of the value of each plain-index cell (i.e. no
        annotations).'''
        return [
            self[i] for i in self.all_indices()  # type: ignore[misc]
        ]

    def all_clarities(self) -> List[Numeric]:
        '''Returns a list of the clarity of each plain-index cell (i.e. no
        annotations).'''
        return [
            self.clarity(i) for i in self.all_indices()
        ]

    def all_matching(self, v: CellContent) -> List[Index]:
        '''Returns a list of the indices of all cells that match 'v'.
        A match is one that contains the same value as 'v' and at least all
        the annotations that 'v' has (and possibly more). If 'v' is only an
        Annotation or Annotations, then cell values don't affect the match.
        A value of None matches only None; to specify a value that matches
        any value, pass Any.'''
        return [
            i for i, bundle in self.all_indices_and_bundles()
                if bundle.is_match(v)
        ]

# TODO How do you erase an annotation?

@dataclass
class Canvas1D(Canvas):
    contents: ContentsAndClarities = field(
        default_factory=lambda: ContentsAndClarities()
    )

    @classmethod
    def make_from(cls, s: str, auto_annotate: bool=True) -> Canvas1D:
        result = cls()
        result.contents.min_index = 1
        result.contents.max_index = len(s)
        for i, c in zip(range(1, len(s) + 1), s):
            result[i] = Letter.from_str(c)
            if c != ' ':
                result.set_clarity(i, cls.INITIAL_CLARITY)
            else:
                result.set_clarity(i, 0)
        if auto_annotate:
            for i in range(
                result.contents.min_index, result.contents.max_index + 1
            ):
                ann: Annotation
                match i:
                    case result.contents.min_index:
                        ann = Start
                    case result.contents.max_index:
                        ann = End
                    case _:
                        ann = Inextreme
                result[i] = ann
                result.set_clarity(
                    (i, Anchor),
                    cls.INITIAL_CLARITY
                )
        return result

    def all_addrs(self) -> Iterable[Index]:  # TODO rename to all_indices
        return range(self.contents.min_index, self.contents.max_index + 1)

    all_indices = all_addrs

    def min_index(self) -> int:
        return self.contents.min_index

    def max_index(self) -> int:
        return self.contents.max_index

    def has_addr(self, addr: Addr) -> bool:
        if is_index(addr):
            return (
                addr >= self.contents.min_index
                and
                addr <= self.contents.max_index
            )
        else:
            return False

    has_index = has_addr

    def __getitem__(self, addr: Addr) -> Optional[CellContent]:
        match addr:
            case i if is_index(i):
                return self.contents[addr]
            case (i, AnnotationType()) if is_index(i):
                return self.contents[addr]
            case (i, Annotation() as ann) if is_index(i):
                return self.contents[(i, ann.type)]
            case _:
                return None
        assert False, "Canvas.__getitem__(): should not go past 'match' stmt"
        return None # Needed only to please mypy; stops [return] error

    def __setitem__(self, i: Index, v: CellContent) -> None:
        for ii, vv in self.as_internal_args(i, v):
            self.contents[ii] = vv

    # TODO UT
    def has_letter(self, i: Index) -> bool:
        v = self.contents[i]
        return v is not None and v != ' '

    def as_internal_args(self, i: Index, v: CellContent) \
    -> Iterable[Tuple[Index2, CellContent1]]:
        match v:
            case None:
                yield (i, None)
            case str():
                yield (i, v)
            case Annotation():
                yield ((i, v.type), v)
            case Annotations():
                for elem in v:
                    yield ((i, elem.type), elem)
            case CellBundle():
                if v.value is not None:
                    yield (i, v.value)
                yield from self.as_internal_args(i, v.annotations)

    # TODO UT
    def all_ixjypairs(self) -> Iterable[Tuple[Index, CanvasValue, Index, CanvasValue]]:
        for i in self.all_addrs():
            x: str = self[i]  # type: ignore[assignment]
            if x is not None:
                for j in self.all_addrs():
                    if i != j:
                        y: str = self[j]  # type: ignore[assignment]
                        if y is not None:
                            yield i, x, j, y

    def clarity(self, i: Index2) -> Numeric:
        return self.contents.clarity(i)

    def set_clarity(self, i: Index2, clarity: Numeric) -> None:
        self.contents.set_clarity(i, clarity)

    def addr_of(self, v: CanvasValue) -> int:
        for i in self.contents.all_indices():
            if self.contents[i] == v:
                return i
        raise FizzleValueNotFound(v)

    def all_matching(self, v: CellContent) -> List[Index]:
        return [
            i
                for i, vv in self.contents.all_indices_and_bundles()
                    if vv.is_match(v)
        ]

    def is_match(self, i: Index, v: CellContent) -> bool:
        return self.as_bundle(i).is_match(v)

    def has_annotation(self, i: Index, ann: Annotation) -> bool:
        return self.is_match(i, ann)

    def as_bundle(self, i: Index) -> CellBundle:
        return self.contents.as_bundle(i)

    def all_indices_and_values(self) -> Iterable[Tuple[Index, CanvasValue]]:
        return self.contents.all_indices_and_values()

    def __str__(self) -> str:
        clarities = ', '.join(str(c) for c in self.contents.all_clarities())
        return f"'{self.short_str()}'  {clarities}"

    def short(self) -> str:
        return repr(self.short_str())

    def short_str(self) -> str:
        return ''.join(
            ' ' if x is None else str(x)
                for x in self.contents.all_values()
        )

    def state_str(self) -> str:
        return f"{self.short()}  {' '.join(str(c) for c in self.contents.all_clarities())}"
