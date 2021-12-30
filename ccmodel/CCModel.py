# CCModel.py -- A spike for "codelets in canvases"

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Any, Callable, ClassVar, Collection, Dict, FrozenSet, \
    Hashable, IO, Iterable, Iterator, List, Literal, NewType, Optional, \
    Protocol, Sequence, Sequence, Set, Tuple, Type, TypeVar, Union, \
    runtime_checkable, TYPE_CHECKING
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
from FARGBase import FARGModel
from Log import trace, lo
from CCTypes import HasWithTag, HasAddTag, HasHasTag, Program, Codelet
from Fizzle import Fizzle
from Complex import Complex
from Tag import HasHasTag, HasWithTag, Tag, tagmatch, HasAddTag, \
    is_cell_tag_pred, has_tag, SimpleTag, CellTag, TagConjunction, \
    HasAvail
from ArgsMap import ArgsMap, ArgsMapWithTags, ArgsDict, EmptyArgsMap, \
    empty_args_map, ArgsMapSeries, as_argsmap, Avails
from run import run, mk_func_args
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement, pr, pts, is_type_instance, \
    is_dataclass_instance, make_nonoptional, dict_str, short, class_of, omit, \
    as_dict, fields_for, transitive_closure, as_tuple, ps, as_dstr
from Canvas import Cell
from Canvas import Canvas, CellContents, Paintable, CellRef, NodeRef, \
    SeqCanvas
from Program import ProgramResult, Produced


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

@dataclass
class StdModel(FARGModel):
    #nodes: Set[Node] = field(default_factory=set)

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

    """
    # TODO rm?
    def cell_has_tag(self, c: Union[Cell, CellRef], pred: Pred) -> bool:
        return c.cell_has_tag(pred)
    """

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
class NotEnoughOperands(Fizzle):
    actual_num_operands: Optional[int] = None
    min_num_operands: Optional[int] = None

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
