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

class ArgsMap(ABC):
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

    """
    def short(self) -> str:
        # TODO
        return '(empty)'
    """

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
class Complex(Program):
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

    def has_tag(self, tag: Type[Node]) -> bool:
        return any(c._has_tag(tag) for c in self.complexes_all_down())

    def _has_tag(self, tag: Type[Node]) -> bool:
        return any(isinstance(t, tag) for t in as_iter(self.tags))

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
class Cell(Program):
    '''A Cell is mutable. Its .contents may change, but its .canvas and .addr
    may not. A Cell may not exist outside of a Canvas.'''
    contents: CellContents
    canvas: Canvas
    addr: Addr

    # TODO
    #def paint(
    #def erase(self, fm: FARGModel):

    def paint(self, contents: CellContents) -> None:
        self.contents = contents

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

    def has_tag(self, tag: Type[Node]) -> bool:
        return has_tag(self.contents, tag)

    def avails_at(self) -> Tuple[Value, ...]:
        return self.canvas.avails_at(self.addr)

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
class CellRef:
    canvas: SeqCanvas
    index: int

    def paint(self, content: CellContents) -> None:
        self.canvas.paint(self.index, content)

    def get(self) -> CellContents:
        return self.canvas[self.index]

    def avails_at(self) -> Tuple[Value, ...]:
        return self.canvas.avails_at(self.index)

    def has_tag(self, tag: Type[Node]) -> bool:
        return has_tag(self.get(), tag)

    def short(self) -> str:
        return f'[{self.index}]'

NodeRef = Union[Node, Cell, CellRef]

def has_tag(x: Any, tag: Type[Node]) -> bool:
    if isinstance(x, (Complex, Cell, CellRef)):
        return x.has_tag(tag)
    else:
        return False

@dataclass(frozen=True)
class HasTag:
    tag: Type[Node]

    def __call__(self, nref: NodeRef) -> bool:
        return has_tag(nref, self.tag)

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

    @classmethod
    #def make(cls, num_cells: Optional[int]=None) -> SeqCanvas:
    def make(cls, *cellcontents: CellContents) -> SeqCanvas:
        result = SeqCanvas.empty()
        for content in cellcontents:
            result.append_cell(content)
        return result

    def append_cell(self, content: CellContents) -> None:
        self._cells.append(Cell(content, self, len(self._cells) + 1))

    def paint(self, addr: int, content: CellContents) -> None:
        self._cells[addr].paint(content)

    def run(self, args: Optional[ArgsMap]=None) -> ArgsMap:  # type: ignore[override]
        if args is None:
            args = ArgsMap.empty()
        if self._cells:
            for addr, cell in enumerate(self._cells):
                result = cell.run(args)
                if isinstance(result, Produced):
                    self.paint(addr + 1, result.v)
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

    def has_tag(self, x: Union[Node, Cell, CellRef], tag: Type[Node]) -> bool:
        return has_tag(x, tag)

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

class Codelet(Program):
    pass

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
