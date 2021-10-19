# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value
from FARGModel import FARGModel, Codelet, Codelets, Ref, R, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded
from Canvas import StepCanvas, Step, CellRef
from util import as_iter


@dataclass(frozen=True)
class BuildCompanion(Codelet):
    '''Builds one or more nodes that serve as companions for the acting
    node.'''
    behalf_of: R[Agent] = None
    companion: R[Nodes] = None

    def run(  # type: ignore[override]
        self, fm, behalf_of: Optional[Agent], companion=Optional[Nodes]
    ) -> Codelets:
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
    ) -> Codelets:
        fm.set_state(agent, state)
        return None

@dataclass(frozen=True)
class Paint(Codelet):
    '''Paints a value in a Canvas cell.'''
    cellref: R[CellRef] = None
    value: R[Value] = None
    behalf_of: R[Agent] = None
    sk: R[Codelet] = NewState(Ref('behalf_of'), Succeeded)
    fk: R[Codelet] = NewState(Ref('behalf_of'), Snag)

    def run(  # type: ignore[override]
        self, fm, cellref: CellRef, value: Value, behalf_of: Optional[Agent],
        sk: Optional[Codelet]
    ) -> Codelets:
        fm.paint(cellref, value, behalf_of)
        return sk
