# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value
from FARGModel import FARGModel, Codelet, Codelets, Ref, R, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded, CodeletResults
from Canvas import StepCanvas, Step, CellRef, Operator
from util import as_iter, trace


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

"""
@dataclass(frozen=True)
class QuerySlipnetForDelegate(Codelet):
    features: R[RelevantFeatures] = None
    slipnode_type: R[SlipnodeType] = None
    sk: R[Codelets] = Sleep(Ref('behalf_of'))

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        behalf_of: Optional[Agent],
        features: RelevantFeatures,
        slipnode_type: SlipnodeType,
        sk: Optional[Codelets]
    ) -> CodeletResults:
        
"""

import Agents
