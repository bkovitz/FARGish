# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FARGModel import FARGModel, Codelet, Codelets, R, Agent, Nodes, \
    AgentState, Wake
from util import as_iter


@dataclass(frozen=True)
class BuildCompanion(Codelet):
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
    agent: R[Agent] = None
    state: R[AgentState] = None

    def run(  # type: ignore[override]
        self, fm, agent: Agent, state: AgentState
    ) -> Codelets:
        fm.set_state(agent, state)
        return None
