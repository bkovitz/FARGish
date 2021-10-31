# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value, Node
from FARGModel import FARGModel, Codelet, Codelets, Ref, R, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded, CodeletResults, QArg, QArgs, Sources, \
    NoResultFromSlipnet, CellRef
from Log import ALogger
from util import as_iter, trace, pr, pts, short


@dataclass(frozen=True)
class Build(Codelet):
    '''Builds one or more nodes that serve as companions for the acting
    node.'''
    to_build: R[Nodes] = None
    behalf_of: R[Agent] = None

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        behalf_of: Optional[Agent],
        to_build: Nodes,
        sources: Sources
    ) -> CodeletResults:
        for node in as_iter(to_build):
            node = fm.replace_refs(node, sources)
            fm.build(node, builder=behalf_of)
        if behalf_of:
            return NewState(behalf_of, Wake)
        else:
            return None

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.to_build)})'

@dataclass(frozen=True)
class NewState(Codelet):
    '''Sets an Agent to a new AgentState.'''
    agent: R[Agent] = None
    state: R[AgentState] = None

    def run(  # type: ignore[override]
        self, fm, agent: Optional[Agent], state: AgentState
    ) -> CodeletResults:
        if agent:
            fm.set_state(agent, state)
        #print('NEWST', short(agent), fm.agent_state(agent))
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
    #sk: R[Codelets] = Sleep(Ref('behalf_of'))  TODO

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        behalf_of: Optional[Agent],
        qargs: QArgs,  # TODO Require at least one QArg?
        sources: Sources
    ) -> CodeletResults:
        kwargs = fm.mk_slipnet_args(qargs, sources)
        alogger = ALogger(t=fm.t, filename=short(behalf_of) + '.csv') #, mode='w')
        slipnet_results = fm.pulse_slipnet(alogger=alogger, **kwargs)
        if not slipnet_results:
            raise NoResultFromSlipnet(qargs=qargs)
        return [
            Build(
                to_build=fm.try_to_fill_nones(node, sources, behalf_of),
                behalf_of=behalf_of
            )
                for node in slipnet_results
        ]
        """
        for node in slipnet_results:
            node = fm.try_to_fill_nones(node, sources, behalf_of)
            fm.build(node, builder=behalf_of)
        return None
        """

@dataclass(frozen=True)
class Sleep(Codelet):
    agent: R[Agent] = Ref('agent')
    sleep_duration: int = 3

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        agent: Agent,
        sleep_duration: int
    ) -> CodeletResults:
        fm.sleep(agent, sleep_duration)
        return None

@dataclass(frozen=True)
class RaiseException(Codelet):
    exctype: R[Type[Exception]] = Ref('exctype')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        exctype: Type[Exception]
    ) -> CodeletResults:
        raise exctype

import Agents
