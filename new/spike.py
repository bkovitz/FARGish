# spike.py -- A spike for "codelets in canvases"

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
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
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement, pr, pts, is_type_instance, \
    is_dataclass_instance, make_nonoptional, dict_str, short, class_of, omit, \
    as_dict, fields_for, transitive_closure, as_tuple


class ArgsMap:

    @classmethod
    def empty(cls) -> ArgsMap:
        return ArgsMap()

    def short(self) -> str:
        # TODO
        return '(empty)'

class Program(ABC):

    @abstractmethod
    def run(self, fm: FARGModel, **kwargs) -> None:
        pass

CellContents = Union[ArgsMap, Program, None]

class Canvas:
    '''A Canvas is mutable. The contents of its cells may change, and the
    number of its cells may change.'''
    pass

@dataclass
class Cell:
    '''A Cell is mutable. Its .contents may change, but its .canvas and .addr
    may not. A Cell may not exist outside of a Canvas.'''
    contents: CellContents
    canvas: Canvas
    addr: Addr

    # TODO
    #def paint(
    #def erase(self, fm: FARGModel):

@dataclass
class SeqCanvas(Canvas):
    _cells: List[Cell]

    @classmethod
    #def make(cls, num_cells: Optional[int]=None) -> SeqCanvas:
    def make(cls, *cellcontents: CellContents) -> SeqCanvas:
        result = SeqCanvas.empty()
        for content in cellcontents:
            result.append_cell(content)
        return result

    def append_cell(self, content: CellContents) -> None:
        self._cells.append(Cell(content, self, len(self._cells) + 1))

    @classmethod
    def empty(cls) -> SeqCanvas:
        return SeqCanvas(_cells=[])

    def short(self) -> str:
        cl = self.__class__.__name__
        return f"[ {' | '.join(short(c.contents) for c in self._cells)} ]"

class FARGModel:

    def run(self) -> None:
        # TODO
        pass


@dataclass(frozen=True)
class Avails(ArgsMap):
    values: Tuple[Value, ...]

    def __init__(self, *values: Value):
        force_setattr(self, 'values', values)

    def short(self) -> str:
        if self.values:
            return ' '.join(short(v) for v in self.values)
        else:
            return ' (no avails) '

@dataclass(frozen=True)
class Operator:
    func: Callable
    name: str
    min_num_operands: int = 2

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

    def run(self, fm: FARGModel, **kwargs) -> None:
        # TODO
        pass

    def short(self) -> str:
        return self.operator.mk_short(self.operands)
    
def Plus(*operands: int) -> Consume:
    return Consume(name='Plus', operator=plus, operands=as_tuple(operands))

if __name__ == '__main__':
    #ca = SeqCanvas.make(num_cells=3)
    ca = SeqCanvas.make(
        Avails(4, 5),
        Plus(4, 5),
        ArgsMap.empty()
    )
    print(short(ca))
