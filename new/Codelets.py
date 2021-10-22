# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value, Node
from FARGModel import FARGModel, Codelet, Codelets, Ref, R, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded, CodeletResults, QArg, QArgs, Sources
from Canvas import StepCanvas, Step, CellRef, Operator
from Graph import Before, After
from util import as_iter, trace, pr, pts


@dataclass(frozen=True)
class BuildCompanion(Codelet):
    '''Builds one or more nodes that serve as companions for the acting
    node.'''
    behalf_of: R[Agent] = None
    companion: R[Nodes] = None

    def run(  # type: ignore[override]
        self, fm, behalf_of: Optional[Agent], companion=Optional[Nodes]
    ) -> CodeletResults:
        for c in as_iter(self.companion):
            fm.build(c, builder=behalf_of)
        if behalf_of:
            return NewState(behalf_of, Wake)
        else:
            return None

@dataclass(frozen=True)
class NewState(Codelet):
    '''Sets an Agent to a new AgentState.'''
    agent: R[Agent] = None
    state: R[AgentState] = None

    def run(  # type: ignore[override]
        self, fm, agent: Agent, state: AgentState
    ) -> CodeletResults:
        fm.set_state(agent, state)
        return None

@dataclass(frozen=True)
class Paint(Codelet):
    '''Paints a value in a Canvas cell.'''
    dest: R[CellRef] = Ref('dest')
    value: R[Value] = Ref('value')
    sk: R[Codelets] = NewState(Ref('behalf_of'), Succeeded)
    fk: R[Codelets] = NewState(Ref('behalf_of'), Snag)

    def run(  # type: ignore[override]
        self, fm, dest: CellRef, value: Value, behalf_of: Optional[Agent],
        sk: Optional[Codelet]
    ) -> CodeletResults:
        #print('PAINT', fm.a(behalf_of), behalf_of)
        fm.paint(dest, value, behalf_of)
        return None

"""
@dataclass(frozen=True)
class TakeOperands(Codelet):
    operands: R[Tuple[Value, ...]] = Ref('operands')
    cellref: R[CellRef] = Ref('source')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        operands: Tuple[Value, ...],
        cellref: CellRef,
    ) -> CodeletResults:
        taken, remaining = cellref.take_avails(operands)
        return DefineRefs(taken=taken, remaining=remaining)
"""

@dataclass(frozen=True)
class Consume(Codelet):
    operator: R[Operator] = Ref('operator')
    operands: R[Tuple[Value, ...]] = Ref('operands')
    source: R[CellRef] = Ref('source')
    result_in: R[str] = 'result'

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        operator: Operator,
        operands: Tuple[Value, ...],
        source: CellRef,
        result_in: str
    ) -> CodeletResults:
        return dict([(result_in, operator.consume(source, operands))])

    def features_of(self) -> Iterable[Node]:
        # NEXT Dereference
        for operand in as_iter(self.operands):
            yield Before(operand)
        if self.operator:
            yield self.operator
        if self.operands and self.operator:
            result = self.operator(*self.operands)
            yield After(result)

    @classmethod
    def make_table(
        cls,
        rands1: Iterable[int],
        rands2: Iterable[int],
        rators: Iterable[Operator]
    ) -> Iterable['Consume']:
        for rand1 in rands1:
            for rand2 in rands2:
                for rator in rators:
                    if rand1 >= rand2:
                        result = rator(rand1, rand2)
                        if result != rand1 and result != rand2:
                            yield cls(rator, (rand1, rand2))

@dataclass(frozen=True)
class BuildLitPainter(Codelet):
    value: R[Value] = Ref('value')
    dest: R[CellRef] = Ref('dest')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        value: Value,
        dest: CellRef
    ) -> CodeletResults:
        fm.build(Agents.LitPainter(value=value, dest=dest))
        return None

# TODO rename -> QuerySlipnetAndBuild?
@dataclass(frozen=True)
class QuerySlipnetForDelegate(Codelet):
    qargs: R[QArgs] = Ref('qargs')
    #result_type: R[Type[Node]] = Ref('result_type')
    #sk: R[Codelets] = Sleep(Ref('behalf_of'))  TODO

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        behalf_of: Optional[Agent],
        qargs: QArgs,  # TODO Require at least one QArg?
        sources: Sources
        #sk: Optional[Codelets]
    ) -> CodeletResults:
        #return None # TODO STUB

        kwargs = fm.mk_slipnet_args(qargs, sources)
        slipnet_results = fm.pulse_slipnet(**kwargs)
        # TODO if no results, then fk
        for node in slipnet_results:
            fm.build(node, builder=behalf_of)
        return None

import Agents
