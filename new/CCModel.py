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

from FMTypes import Node, Nodes, Addr, Value, WSPred, match_wo_none, Pred, \
    as_pred, ADict, AndFirst, CallablePred, MatchWoNone, IsInstance, \
    combine_preds, AlwaysTrue, HasBindWs, HasArgs, Ref, T, Exclude
from Log import trace, lo
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement, pr, pts, is_type_instance, \
    is_dataclass_instance, make_nonoptional, dict_str, short, class_of, omit, \
    as_dict, fields_for, transitive_closure, as_tuple, ps


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

    def short(self) -> str:
        # TODO
        return '(empty)'

@dataclass(frozen=True)
class ArgsDict(ArgsMap):
    d: Dict[str, Value]

    def is_empty(self) -> bool:
        return not self.d

    def xget(self, k: str, default: Optional[Value]=None) \
    -> Optional[Value]:
        return self.d.get(k, default)

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
    def make(cls, *argss: ArgsMap) -> ArgsMapSeries:
        # TODO Special treatment for empty and for existing ArgsMapSeries?
        return ArgsMapSeries(as_tuple(cls.flatten(argss)))

    @classmethod
    def flatten(cls, argss: Iterable[ArgsMap]) -> Iterable[ArgsMap]:
        for args in argss:
            if args.is_empty():
                continue
            elif isinstance(args, ArgsMapSeries):
                yield from cls.flatten(args.argss)
            else:
                yield args

class Program(ABC):

    @abstractmethod
    #def run(self, fm: FARGModel, **kwargs) -> None:
    def run(self, **kwargs) -> ProgramResult:
        pass

def as_argsmap(x: Any) -> ArgsMap:
    if isinstance(x, ArgsMap):
        return x
    elif is_dataclass_instance(x) or isinstance(x, dict):
        return ArgsDict(as_dict(x))
    else:
        return empty_args_map

def run(program: Program, args: ArgsMap) -> ProgramResult:
    return program.run(
        **mk_func_args(program.run, args.prepend(as_argsmap(program)))
    )

def mk_func_args(func: Callable, args: ArgsMap) -> Dict[str, Any]:
    d: Dict[str, Any] = {}
    for param_name, param_type in params_of(func):
        value = args.get(param_name, None)
        if not is_type_instance(value, param_type):
            raise MissingArgument(
                func=func,
                param_name=param_name,
                param_type=param_type,
                value=value
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

CellContents = Union[ArgsMap, Program, None]

@dataclass(frozen=True)
class Produced:
    '''Something produced by a run of something, which probably should be
    deposited into the next cell of the current canvas, if appropriate.'''
    v: CellContents

ProgramResult = Union[Produced, CellContents]

class Canvas:
    '''A Canvas is mutable. The contents of its cells may change, and the
    number of its cells may change.'''
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
            return args
        elif isinstance(self.contents, ArgsMap):
            return args.prepend(self.contents)
        else:
            #return self.contents.run(args=args)
            return run(self.contents, args)

    def short(self) -> str:
        if self.contents is None:
            s = '(empty cell)'
        else:
            s = short(self.contents)
        return f'[ {s} ]'

@dataclass
class SeqCanvas(Canvas, Program):
    _cells: List[Cell]

    def __getitem__(self, addr: int) -> CellContents:
        try:
            return self._cells[addr].contents
        except IndexError:
            # TODO What if addr < 0?
            return None

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

class FARGModel:

    def run(self) -> None:
        # TODO
        pass

class Fizzle(Exception):
    pass

@dataclass(frozen=True)
class MissingArgument(Fizzle):
    func: Optional[Callable] = None
    param_name: Optional[str] = None
    param_type: TypeAnnotation = None
    value: Any = None

    def __str__(self) -> str:
        return self.short()

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.func)}, {repr(self.param_name)}, {short(self.param_type)}, {short(self.value)})'

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
    canvas: Optional[SeqCanvas]
    step: Optional[Program]

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
        nm = f' {self.name} '
        return nm.join(short(o) for o in operands)

plus = Operator(operator.add, '+')

@dataclass(frozen=True)
class Consume(Program):
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
        #return args

    def short(self) -> str:
        return self.operator.mk_short(self.operands)

def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))

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
