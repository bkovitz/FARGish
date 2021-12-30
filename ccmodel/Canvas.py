# Canvas.py -- Classes for canvases and cells

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
from abc import ABC, abstractmethod

from FMTypes import Node, Value, Addr, Pred, CellContents
from Program import Program, Produced, ProgramResult
from CCTypes import HasHasTag, HasAddTag
from ArgsMap import ArgsMap, empty_args_map, as_argsmap, Avails
from Fizzle import Fizzle
from run import run
from Tag import Tag, is_cell_tag_pred, tagmatch, HasWithTag, TagConjunction, \
    SimpleTag, CellTag, has_tag, HasAvail
from util import short


@dataclass(frozen=True)
class RunAborted(Fizzle):
    canvas: Optional[Canvas] = None
    step: Optional[Program] = None

Paintable = Union[CellContents, Dict[str, Value]]
# What is Paintable is that which can be painted onto something else
# (not that on which something is painted).

class Canvas(ABC):
    '''A Canvas is mutable. The contents of its cells may change, and the
    number of its cells may change.'''

    @abstractmethod
    def avails_at(self, addr: int) -> Tuple[Value, ...]:
        pass

@dataclass
class Cell(Program, HasHasTag, HasAddTag):
    '''A Cell is mutable. Its .contents may change, but its .canvas and .addr
    may not. A Cell may not exist outside of a Canvas.'''
    contents: CellContents
    canvas: SeqCanvas
    addr: Addr
    tags: Set[Tag] = field(default_factory=set)

    # TODO
    #def paint(
    #def erase(self, fm: FARGModel):

    def paint(self, contents: CellContents) -> None:
        #lo('CELL.paint', contents)
        self.contents = contents
        self.canvas.clear_arithmetic_to_here(self.addr)

    def get(self) -> CellContents:
        return self.canvas[self.addr]

    def run(self, args: Optional[ArgsMap]=None) -> ProgramResult:  # type: ignore[override]
        if args is None:
            args = empty_args_map
        if self.contents is None:
            raise RunAborted(canvas=self.canvas, step=self)
        elif isinstance(self.contents, ArgsMap):
            return args.prepend(self.contents)
        elif isinstance(self.contents, Program):
            #return self.contents.run(args=args)
            try:
                return run(self.contents, args)
            except Fizzle as fiz:
                # TODO Don't we need a FARGModel to hold the untagged
                # contents that we paint over?
                fiz.paint_tagged_codelet(self)
                raise
        else:
            # ValueWrapper
            return None  # TODO Should 'running' a value mean something?

    def has_tag(self, pred: Pred) -> bool:
        if is_cell_tag_pred(pred):
            return any(tagmatch(t, pred) for t in self.tags)
        else:
            return has_tag(self.contents, pred)

    # TODO rm?
    """
    def cell_has_tag(self, pred: Any) -> bool:
        return any(tagmatch(t, pred) for t in self.tags)
    """

    def add_tag(self, *tag: Tag) -> None:
        contents = self.get()
        #lo('CELLA', self, self.addr, type(contents), contents, tag)
        #print_stack()
        if isinstance(contents, HasWithTag):
            self.paint(contents.with_tag(*tag))
            #lo('CELLATG', self.contents, type(self.contents))
        elif contents is None:
            pass
        else:
            raise NotImplementedError(f'Cell.add_tag() {type(contents)}, {short(contents)}')

    def add_tag_to_cell(self, *tag: Tag) -> None:
        for t in tag:
            self.tags.add(t)

    def remove_tag_from_cell(self, pred: Pred) -> None:
        for tag in list(self.tags):
            if tagmatch(tag, pred):
                self.tags.remove(tag)

    def avails_at(self) -> Tuple[Value, ...]:
        return self.canvas.avails_at(self.addr)

    def as_cell(self) -> Cell:
        return self

    def short(self) -> str:
        if self.contents is None:
            s = '(empty cell)'
        else:
            s = short(self.contents)
        return f'[ {s} ]'

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({self.addr}: {short(self.contents)})'

@dataclass(frozen=True)
class CellRef(HasHasTag, HasAddTag):
    canvas: SeqCanvas
    index: int

    def paint(self, content: CellContents) -> None:
        self.canvas.paint(self.index, content)

    def get(self) -> CellContents:
        return self.canvas[self.index]

    def avails_at(self) -> Tuple[Value, ...]:
        return self.canvas.avails_at(self.index)

    def has_tag(self, pred: Pred) -> bool:
        return has_tag(self.get(), pred)

    """
    # TODO rm?
    def cell_has_tag(self, pred: Pred) -> bool:
        return self.as_cell().cell_has_tag(pred)
    """

    def add_tag(self, *tag: Tag) -> None:
        self.as_cell().add_tag(*tag)

    def add_tag_to_cell(self, *tag: Tag) -> None:
        self.as_cell().add_tag_to_cell(*tag)

    def remove_tag_from_cell(self, pred: Pred) -> None:
        self.as_cell().remove_tag_from_cell(pred)

    def as_cell(self) -> Cell:
        return self.canvas.cell_at(self.index)

    def short(self) -> str:
        return f'[{self.index}]'

NodeRef = Union[Node, Cell, CellRef]

@dataclass
class SeqCanvas(Canvas, Program):
    _cells: List[Cell]

    instance_count: ClassVar[int] = 0
    
    def __post_init__(self) -> None:
        self.__class__.instance_count += 1

    def __hash__(self):
        '''This is necessary to maintain determinism.'''
        return hash(self.instance_count)

    def __getitem__(self, addr: int) -> CellContents:
        try:
            return self._cells[addr].contents
        except IndexError:
            # TODO What if addr < 0?
            return None

    def cellref(self, addr: int) -> CellRef:
        return CellRef(self, addr)

    # TODO UT
    def cellrefs(self) -> Iterable[CellRef]:
        return (CellRef(self, i) for i in range(len(self._cells)))

    def cell_at(self, addr: Addr) -> Cell:
        # TODO How to handle addr out of range?
        #lo('CELL_AT', addr)
        return self._cells[addr]

    @classmethod
    #def make(cls, num_cells: Optional[int]=None) -> SeqCanvas:
    def make(cls, *cellcontents: CellContents) -> SeqCanvas:
        result = SeqCanvas.empty()
        for content in cellcontents:
            result.append_cell(content)
        return result

    def append_cell(self, content: CellContents) -> None:
        self._cells.append(Cell(content, self, len(self._cells)))

    def paint(self, addr: int, content: CellContents) -> None:
        self._cells[addr].paint(content)

    def clear_arithmetic_to_here(self, start_from: Addr) -> None:
        '''Removes all the ArithmeticToHere tags starting from 'start_from'
        to the end of the canvas.'''
        for cell in self._cells[start_from:]:
            cell.remove_tag_from_cell(ArithmeticToHere)

    def run(self, args: Optional[ArgsMap]=None) -> ArgsMap:  # type: ignore[override]
        if args is None:
            args = ArgsMap.empty()
        if self._cells:
            for addr, cell in enumerate(self._cells):
                result = cell.run(args)
                if isinstance(result, Produced):
                    self.paint(addr + 1, result.v)
                    self._cells[addr + 1].add_tag_to_cell(ArithmeticToHere())
                elif isinstance(result, ArgsMap):
                    args = result
        return args  # type: ignore[return-value]  # mypy bug?

    @classmethod
    def empty(cls) -> SeqCanvas:
        return SeqCanvas(_cells=[])

    def short(self) -> str:
        if not self._cells:
            return '(empty seqcanvas)'
        else:
            return ''.join(short(c) for c in self._cells)

    def avails_at(self, addr: int) -> Tuple[Value, ...]:
        while addr >= 0:
            args = as_argsmap(self[addr])
            avails: Avails = args.get('avails')  # type: ignore[assignment]
            if avails is not None:
                return avails.values
            else:
                addr -= 1
        return ()

### Tags ###

@dataclass(frozen=True)
class SuccessfulCanvas(TagConjunction):

    def __init__(self, target: int):
        super().__init__((HasAvail(target), ArithmeticToHere))

@dataclass(frozen=True)
class ArithmeticToHere(SimpleTag, CellTag):
    pass


