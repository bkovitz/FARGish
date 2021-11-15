# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable

from FMTypes import Value, Node, Ref, R
from FARGModel import FARGModel, Codelet, Codelets, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded, CodeletResults, QArg, QArgs, Sources, \
    NoResultFromSlipnet, CellRef
from Log import trace
from util import as_iter, as_list, pr, pts, short, sample_without_replacement


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
        dest: CellRef,
        behalf_of: Optional[Agent]
    ) -> CodeletResults:
        fm.build(Agents.LitPainter(value=value, dest=dest), builder=behalf_of)
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
        slipnet_results = fm.pulse_slipnet(
            alog=fm.start_alog((behalf_of, self)),
            **kwargs
        )
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

@dataclass(frozen=True)
class MakeVariantFromAvails(Codelet):
    agent: R[Agent] = Ref('agent')
    cellref: R[CellRef] = Ref('source')
    avails: R[Tuple[Value, ...]] = Ref('avails')
        # These values were avail; indices match indices in seeker's request
    unavails: R[Tuple[Value, ...]] = Ref('unavails')
        # These values were unavail; indices match indices in seeker's request

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        cellref: CellRef,
        agent: Agent,  # the Agent to make a variant of
        avails: Tuple[Value, ...],
        unavails: Tuple[Value, ...],
        behalf_of: Optional[Agent]=None
    ) -> CodeletResults:
        true_avails = list(as_iter(cellref.avails))
        new_operands: List[Value] = []

        #print('MK0', true_avails, new_operands)
        for a, u in zip(avails, unavails):  # TODO zip_longest?
            if a is not None:
                if a in true_avails:
                    new_operands.append(a)
                    true_avails.remove(a)
                else:
                    pass # TODO What if a isn't avail?
            else:
                ua: Sequence[Value] = choose_most_similar(true_avails, u)
                if ua:
                    new_operands += ua
                    for v in ua:
                        true_avails.remove(v)
                else:
                    pass  # TODO raise an exception?
            #print('MK', a, u, true_avails, new_operands)
        # TODO Don't make a new agent that's already there
        if not isinstance(agent, Agents.Consumer):
            return None  # TODO raise exception?
        else:
            return Build(
                # TODO Standardize order of operands when commutative?
                to_build=replace(agent, operands=tuple(new_operands)),
                behalf_of=behalf_of
            )

def choose_most_similar(avails: Sequence[Value], target: Value) \
-> Sequence[Value]:
    d: Dict[Value, float] = dict(
        (a, similarity_to(target, a)) for a in avails
    )
    return list(sample_without_replacement(d.keys(), weights=d.values()))

def similarity_to(target: Value, v: Value) -> float:
    '''Returns a measure of how similar v is to target. 1.0 is maximum
    similarity; 0.0 is no similarity. Extremely crude as of 10-Nov-2021.'''
    # TODO If we're comparing integers, consider the range. The caller needs
    # to provide the range--or rather, a Similarity object.''
    if isinstance(target, int) and isinstance(v, int):
        return 10 / (10 + (target - v) ** 2)
    else:
        # TODO
        return 0.1

import Agents
