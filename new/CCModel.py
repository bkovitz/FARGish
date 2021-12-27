# CCModel.py -- A spike for "codelets in canvases"

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable, get_type_hints
from abc import ABC, abstractmethod
from collections import defaultdict
import inspect
from inspect import isclass, signature
from contextlib import contextmanager
import sys
from types import MethodType, FunctionType
from random import randrange
import operator
from traceback import print_stack
from random import choice

from FMTypes import Node, Nodes, Value, WSPred, match_wo_none, Pred, \
    as_pred, ADict, AndFirst, CallablePred, MatchWoNone, IsInstance, \
    combine_preds, AlwaysTrue, HasBindWs, HasArgs, Ref, N, T, Exclude
from Log import trace, lo
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement, pr, pts, is_type_instance, \
    is_dataclass_instance, make_nonoptional, dict_str, short, class_of, omit, \
    as_dict, fields_for, transitive_closure, as_tuple, ps, as_dstr


Addr = int  #Hashable

TypeAnnotation = Any  # In lieu of a type annotation for 'type annotation'

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

def is_cell_tag_pred(pred: Pred) -> bool:
    '''Does 'pred' test for a CellTag?'''
    return (
        isinstance(pred, CellTag)
        or
        (isclass(pred) and issubclass(pred, CellTag))
    )

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

class Program(ABC):

    @abstractmethod
    def run(self, **kwargs) -> ProgramResult:
        pass

    def short(self) -> str:
        return as_dstr(self)

    def __str__(self) -> str:
        return short(self)

@dataclass(frozen=True)
class Detector(Program):
    watch: Optional[Node] = None
    watch_for: Optional[Callable] = None  # TODO broader match criterion
    then: Optional[Type[Program]] = None

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        watch: Union[Canvas, CellRef],  # Allow Workspace?
        watch_for: Callable,  # TODO more general type for predicate
        then: Type[Program]  # TODO allow an instance
    ) -> ProgramResult:
        found_node = fm.node_query(watch, watch_for)
        if found_node is None:
            return None
        else:
            then = fm.build(then, ArgsDict(dict(noderef=found_node)))  # type: ignore[arg-type]  # mypy bug?
            return fm.run(then)

    def short(self) -> str:
        cl = self.__class__.__name__
        return cl  # TODO Show the fields, but in a very short form.
                   # as_dstr() makes a long and confusing string when the
                   # Detector is watching a canvas.

@dataclass(frozen=True)
class Complex(Program, HasHasTag):
    # TODO Replace .nugget and .override with a single dict? This would make
    # it straightforward to override the nugget.
    nugget: Complex
    override: ArgsDict
    tags: Optional[Tuple[Value]]

    def __init__(
        self,
        nugget: Union[Program, Complex],
        *args: Union[Dict[str, Value], ArgsMap],
        tags: Optional[Sequence[Value]]=None,
        **kwargs
    ):
        force_setattr(self, 'nugget', nugget)
        force_setattr(self, 'override', ArgsMapSeries.make(
            *args,
            ArgsDict(kwargs)
        ))
        if tags:
            force_setattr(self, 'tags', as_tuple(tags))
        else:
            force_setattr(self, 'tags', None)
        #lo('COMPL', self.tags)

    def run(self, args: Optional[ArgsMap]=None) -> ProgramResult:  # type: ignore[override]
        return run(self.nugget, ArgsMapSeries.make(
            args, self.override
        ))

    def has_tag(self, pred: Pred) -> bool:
        return any(c._has_tag(pred) for c in self.complexes_all_down())

    def _has_tag(self, pred: Pred) -> bool:
        #return any(isinstance(t, tag) for t in as_iter(self.tags))
        return any(tagmatch(t, pred) for t in as_iter(self.tags))

    def complexes_all_down(self) -> Iterable[Complex]:
        x = self
        while isinstance(x, Complex):
            yield x
            if not isinstance(x.nugget, Complex):
                break
            x = x.nugget

    def short(self) -> str:
        return f'{short(self.override)}/{short(self.nugget)}'

def as_argsmap(x: Any) -> ArgsMap:
    if isinstance(x, ArgsMap):
        return x
    elif isinstance(x, (Cell, CellRef)):
        return as_argsmap(x.get())
    elif is_dataclass_instance(x) or isinstance(x, dict):
        return ArgsDict(as_dict(x))
    else:
        return empty_args_map

def run(program: Program, args: ArgsMap) -> ProgramResult:
    try:
        return program.run(
            **mk_func_args(program.run, ArgsMapSeries.make(
                args,
                as_argsmap(program)
            ))
        )
    except Fizzle as fiz:
        if fiz.codelet is None:
            force_setattr(fiz, 'codelet', program)
        raise

def mk_func_args(func: Callable, args: ArgsMap) -> Dict[str, Any]:
    d: Dict[str, Any] = {}
    for param_name, param_type in params_of(func):
        value = args.get(param_name, None)
        if not is_type_instance(value, param_type):
            # TODO Distinguish between missing argument and argument of
            # wrong type?
            raise MissingArgument(
                func=func,
                param_name=param_name,
                param_type=param_type,
                value=value,
                actual_type=type(value)
            )
        else:
            d[param_name] = value
    return d

def params_of(func: Callable) -> Iterable[Tuple[str, TypeAnnotation]]:
    type_hints = get_type_hints(func)
    for param_name in inspect.signature(func).parameters:
        if param_name == 'return':
            continue  # disregard return type
        yield (param_name, type_hints.get(param_name, Any))

@dataclass(frozen=True)
class ValueWrapper:
    v: Value

CellContents = Union[ArgsMap, Program, Value, None]
Paintable = Union[CellContents, Dict[str, Value]]
# What is Paintable is that which can be painted onto something else
# (not that on which something is painted).

@dataclass(frozen=True)
class Produced:
    '''Something produced by a run of something, which probably should be
    deposited into the next cell of the current canvas, if appropriate.'''
    v: CellContents

ProgramResult = Union[Produced, CellContents]

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

    def cell_has_tag(self, pred: Pred) -> bool:
        return self.as_cell().cell_has_tag(pred)

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

@dataclass
class FARGModel:
    nodes: Set[Node] = field(default_factory=set)

    def run(self, program: Program, args: ArgsMap=empty_args_map) \
    -> ProgramResult:
        return run(program, ArgsMapSeries.make(args, ArgsDict(dict(fm=self))))

    def build(self, node_: Union[N, Type[N]], args: ArgsMap=empty_args_map) \
    -> N:
        # TODO If it's a Canvas or other compound structure, build all its
        # contents, too.
        node: N
        if isclass(node_):
            node = self.build_instance(node_, args)  # type: ignore[arg-type]
        else:
            node = node_  # type: ignore[assignment]
        self.nodes.add(node)
        return node

    def build_instance(self, ntype: Type[N], args: ArgsMap=empty_args_map) -> N:
        # TODO Maybe return a Complex that includes any unused args
        return ntype(**mk_func_args(ntype, args))  # type: ignore[call-arg]

    def add_tag(self, noderef: NodeRef, *tag: Tag) -> None:
        if isinstance(noderef, HasWithTag):
            self.build(noderef.with_tag(*tag))
        elif isinstance(noderef, HasAddTag):
            noderef.add_tag(*tag)
        else:
            raise NotImplementedError(f'FARGModel.add_tag: {type(noderef)}, {short(noderef)}')
        
    def add_tag_to_cell(self, c: Union[Cell, CellRef], *tag: Tag) -> None:
        c.add_tag_to_cell(*tag)

    """
    def OLDpaint(
        self,
        noderef: NodeRef,
        content: Paintable
    ) -> None:
        '''If 'content' is a dict or ArgsMap, painting incorporates 'content'
        into whatever is already in the cell.'''
        if isinstance(content, dict) or isinstance(content, ArgsMap):
            #old = noderef.get()
            old = self.deref(noderef)
            if old is None:
                noderef.paint(as_argsmap(content))
            elif isinstance(old, ArgsMap):
                noderef.paint(ArgsMapSeries.make(as_argsmap(content), old))
            else:
                noderef.paint(Complex(old, content))
        else:
            content = self.build(content)
            noderef.paint(content)
            # TODO Save what was already painted there?
    """

    def paint(
        self,
        noderef: NodeRef,
        content: Paintable
    ) -> Node:
        '''Returns what got painted.''' # TODO Explain the decision about
        # what gets painted and what 'painting' actually is.
        old_node = self.deref(noderef)
        new_node = self.updated_content(old_node, content)
        if isinstance(noderef, (Cell, CellRef)):
            noderef.paint(new_node)
        if not isinstance(new_node, dict):
            self.build(new_node)
        return new_node

    def updated_content(self, old: CellContents, new: Paintable) \
    -> CellContents:
        '''Returns the new thing that results when you paint 'new' on
        top of 'old'.'''
        if new is None or isinstance(new, Program):
            return new
        elif isinstance(new, ArgsMap) or isinstance(new, dict):
            if old is None:
                return as_argsmap(new)
            elif isinstance(old, ArgsMap):
                return ArgsMapSeries.make(new, old)
            elif isinstance(old, Program):
                return Complex(old, as_argsmap(new))
            else:
                return as_argsmap(new)
        else:
            # new is a Program or a Value
            return new
            

        """
        if old_node is None:
            return as_cellcontents(content)
        elif isinstance(old_node, ArgsMap):
            if content
            return ArgsMapSeries.make(as_argsmap(content
        elif isinstance(old_node, Program):
            # TODO
        else:
            return as_cellcontents(content)
        """

    def deref(self, noderef: NodeRef) -> CellContents:
        if isinstance(noderef, CellRef):
            return noderef.get()
        elif isinstance(noderef, Cell):
            return noderef.contents
        else:
            return noderef

    def has_node(self, node: Node) -> bool:
        return node in self.nodes

    def has_tag(self, x: Union[Node, Cell, CellRef], pred: Pred) -> bool:
        # TODO If x is not in the workspace, return False.
        return has_tag(x, pred)

    def cell_has_tag(self, c: Union[Cell, CellRef], pred: Pred) -> bool:
        return c.cell_has_tag(pred)

    def do_timestep(self, run: Optional[Program]=None) -> Any:
        if run is None:
            raise NotImplementedError('need to choose codelet')
        result: Any
        try:
            result = self.run(run)
        except Fizzle as fiz:
            result = fiz
            #pr(fiz)
        return result

    def node_query(self, node: Node, pred: Callable) -> Optional[CellRef]:
        if isinstance(node, SeqCanvas):
            # TODO Weighted choice
            matching_nodes = [n for n in node.cellrefs() if pred(n)]
            try:
                return choice(matching_nodes)
            except IndexError:
                return None
        else:
            return None # TODO Query other kinds of nodes

    def avails_at(self, noderef: NodeRef) -> Tuple[Value, ...]:
        if isinstance(noderef, Cell) or isinstance(noderef, CellRef):  # TODO this right
            return noderef.avails_at()
        else:
            return ()

@dataclass(frozen=True)
class Fizzle(Exception):
    codelet: Optional[Program] = None  # This should always be a Codelet
        # The global run() sets .codelet so individual Codelets' .run()
        # methods don't have to.

    def paint_tagged_codelet(self, cellref: Union[Cell, CellRef]) -> None:
        if self.codelet is not None:
            cellref.paint(Complex(self.codelet, tags=(self,)))
        
    def __str__(self) -> str:
        return as_dstr(self)

    def short(self) -> str:
        return str(self)

@dataclass(frozen=True)
class MissingArgument(Fizzle):
    func: Optional[Callable] = None
    param_name: Optional[str] = None
    param_type: TypeAnnotation = None
    value: Any = None
    actual_type: Optional[Type] = None

    def short(self) -> str:
        cl = self.__class__.__name__
        #lo('MAR', type(self.func))
        return f'{cl}({short(self.func)}, {repr(self.param_name)}, {short(self.param_type)}, {short(self.value)}, {short(self.actual_type)})'

    __str__ = short

@dataclass(frozen=True)
class NotEnoughOperands(Fizzle):
    actual_num_operands: Optional[int] = None
    min_num_operands: Optional[int] = None

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

@dataclass(frozen=True)
class RunAborted(Fizzle):
    canvas: Optional[Canvas] = None
    step: Optional[Program] = None

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
class Operator:
    func: Callable
    name: str
    min_num_operands: int = 2

    def __call__(self, *operands) -> int:  # HACK Numbo-specific return type
                                           # Likely fix: a type parameter
        if len(operands) < self.min_num_operands:
            raise NotEnoughOperands(
                actual_num_operands=len(operands),
                min_num_operands=self.min_num_operands
            )
        else:
            return self.func(*operands)

    def __hash__(self):
        '''It's necessary to omit self.func from __hash__ in order to maintain
        determinism. The default hash code of a function is its address in
        memory, which is non-deterministic. If necessary, one could include
        str(self.func).'''
        return hash(str(self))

    def mk_short(self, operands: Sequence[Value]) -> str:
        if operands:
            nm = f' {self.name} '
            return nm.join(short(o) for o in operands)
        else:
            return f'_{self.name}_'

plus = Operator(operator.add, '+')
mult = Operator(operator.mul, '*')

class Codelet(Program, HasHasTag):

    def has_tag(self, pred: Pred) -> bool:
        return False

@dataclass(frozen=True)
class Consume(Codelet):
    name: str
    operator: Operator
    operands: Tuple[Value, ...]

    #def run(self, args: ArgsMap) -> ArgsMap:
    def run(  # type: ignore[override]
        self,
        avails: Avails,
        operator: Operator,
        operands: Tuple[Value, ...]
    ) -> ProgramResult:
        operands, new_avails = avails.take_avails(operands)
        return Produced(new_avails.add_avail(operator(*operands)))

    def short(self) -> str:
        return self.operator.mk_short(self.operands)

def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))

def Mult(*operands: int) -> Consume:
    return Consume(name='Mult', operator=mult, operands=as_tuple(operands))

@dataclass(frozen=True)
class Paint(Codelet):
    noderef: Optional[NodeRef] = None
    content: Optional[Paintable] = None

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        noderef: NodeRef,
        content: Paintable
    ) -> ProgramResult:
        fm.paint(noderef, content)
        return None

@dataclass(frozen=True)
class FillFromAvails(Codelet):
    noderef: Optional[NodeRef] = None
    # TODO A 'bias' or 'features' parameter to favor some avails over others
    # TODO Name of argument of interest, e.g. 'operands'
    # TODO How many avails are needed

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        noderef: NodeRef,
    ) -> ProgramResult:
        return Paint(
            noderef=noderef,
            content=dict(operands=as_tuple(sample_without_replacement(
                #cellref.avails_at(),
                fm.avails_at(noderef),
                k=2
            )))
        )
        # TODO Select by augmented slipnet query, not by
        # sample_without_replacement.

@dataclass(frozen=True)
class FinishStructure(Codelet):
    noderef: Optional[NodeRef] = None
    # TODO What about objects that aren't in canvases? What about finishing
    # whole canvases? This codelet really should work by querying the
    # slipnet.

    def run(  # type: ignore[override]
        self,
        noderef: NodeRef,
    ) -> ProgramResult:
        return FillFromAvails(noderef=noderef)
        # TODO Replace this HACK with something more flexible.

### Tags ###

def has_tag(x: Any, pred: Pred) -> bool:
    if isinstance(x, HasHasTag):
        return x.has_tag(pred)
#    elif isinstance(x, (Complex, Cell, CellRef)):  # TODO rm
#        return x.has_tag(tag)
    else:
        return False

def tagmatch(tag: Any, pred: Pred) -> bool:
    if isinstance(tag, PTag) and not isinstance(pred, PTag):
        return as_pred(pred)(tag.tagpred)
    else:
        return as_pred(pred)(tag)

@dataclass(frozen=True)
class HasTag:
    '''A predicate to check if an object has a given tag or type of tag, but
    not to check if the object has the TagPred for that tag.'''
    pred: Pred

    def __call__(self, noderef: NodeRef) -> bool:
        return has_tag(noderef, self.pred)

    @classmethod
    def make_from(cls, tagspec: TagSpec) -> HasTag:
        if isinstance(tagspec, TagPred):
            return HasTag(PTag(tagspec))
        else:
            return HasTag(tagspec)

class TagPred:
    '''A predicate for determining if a node (or some nodes, or anything)
    meets the criteria for a tag. Derive tag classes from TagPred.
    Instances of those tag classes (possibly including arguments) go inside
    Tag objects.'''

    @abstractmethod
    def condition(self, **kwargs) -> bool:
        pass

    def run(self, args: ArgsMap=empty_args_map, **kwargs) -> bool:
        return self.condition(**mk_func_args(
            self.condition, ArgsMapSeries.make(
                ArgsDict(kwargs),
                args,
                as_argsmap(self)
            ))
        )

@dataclass(frozen=True)
class PTag:
    '''A Tag that means that its taggee satisfies a certain TagPred.'''
    tagpred: TagPred

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.tagpred)})'

@dataclass(frozen=True)
class Tagger(Codelet):
    tagpred: TagPred

    def run(self, fm: FARGModel) -> ProgramResult:  # type: ignore[override]
        pass  # TODO Look for something to tag, and call .run_on() on it.

    def run_on(
        self,
        fm: FARGModel,
        noderef: NodeRef,
        args: ArgsMap=empty_args_map
    ) -> None:
        if self.tagpred.run(args, fm=fm, noderef=noderef):
            fm.add_tag(noderef, PTag(self.tagpred))

@dataclass(frozen=True)
class HasAvail(TagPred):
    target: Optional[Value] = None

    def condition(self, noderef: NodeRef, target: Value) -> bool:  # type: ignore[override]
        av = as_argsmap(noderef).get('avails')
        return isinstance(av, Avails) and av.has_avail(target)

class SimpleTag:
    '''A tag that doesn't have a predicate.'''
    pass

class CellTag:
    '''A tag that goes on a Cell rather than CellContents.'''
    pass

@dataclass(frozen=True)
class ArithmeticToHere(SimpleTag, CellTag):
    pass

#TagConjunct = Union[SimpleTag, Fizzle, TagPred]
Tag = Union[SimpleTag, PTag, Fizzle]
TagSpec = Union[Tag, TagPred, Type[Tag], Type[TagPred]]

@dataclass(frozen=True)
class TagConjunction(TagPred):
    '''A Tag predicate (tester) for multiple tags at once.'''
    conjuncts: Tuple[Pred, ...]
    #scope: SeqCanvas  # TODO Allow other scopes, like the whole workspace

    def __init__(self, conjuncts_: Tuple[TagSpec, ...]):
        force_setattr(
            self, 'conjuncts', tuple(HasTag.make_from(c) for c in conjuncts_)
        )

    def condition(self, fm: FARGModel, conjuncts: Tuple[Pred]) -> bool:  # type: ignore[override]
        """
        for noderef in self.scope:
            # How do we decide whether to call has_tag or cell_has_tag?
            # How do we get the predicate "HasTag" from the conjunct?
            pass #TODO
        """
            
        # WANT Find the node or cell that has all the conjuncts
        return False # TODO


if __name__ == '__main__':
    #ca = SeqCanvas.make(num_cells=3)
    ca = SeqCanvas.make(
        Avails(4, 5),
        Plus(4, 5),
        None, #ArgsMap.empty()
    )
    ps(ca)
    #args = run(ca._cells[0], ArgsMap.empty())
    run(ca, ArgsMap.empty())
    ps(ca)
