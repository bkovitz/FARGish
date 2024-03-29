# Codelets.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable, get_type_hints
from inspect import isclass

from FMTypes import Value, Node, Ref, R
from FARGModel import FARGModel, Codelet, Codelets, Actor, Agent, Nodes, \
    AgentState, Wake, Snag, Succeeded, CodeletResults, QArg, QArgs, Sources, \
    NoResultFromSlipnet, CellRef, Delegate_succeeded, Fizzle
from Indenting import Indenting
from Log import trace, lo, logging
from util import as_iter, as_list, pr, pts, short, sample_without_replacement, \
    first


@dataclass(frozen=True)
class Build(Codelet):
    '''Builds one or more nodes that serve as companions for the acting
    node.'''
    to_build: R[Nodes] = None
    builder: R[Actor] = Ref('running_agent')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        builder: Optional[Actor],
        to_build: Nodes,
        sources: Sources
    ) -> CodeletResults:
        built: List[Node] = []
        for node in as_iter(to_build):
            node = fm.replace_refs(node, sources)
            node = fm.build(node, builder=builder)
            built.append(node)
        '''
        if builder:
            return NewState(builder, Wake)
        else:
            return None
        '''
        return {'built': set(built)}

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
    sk: R[Codelets] = NewState(Ref('running_agent'), Succeeded)
    fk: R[Codelets] = NewState(Ref('running_agent'), Snag)

    def run(  # type: ignore[override]
        self, fm, dest: CellRef, value: Value, running_agent: Optional[Agent],
        sk: Optional[Codelet]
    ) -> CodeletResults:
        #print('PAINT', fm.a(running_agent), running_agent)
        fm.paint(dest, value, running_agent)
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
        running_agent: Optional[Agent]
    ) -> CodeletResults:
        fm.build(
            Agents.LitPainter(value=value, dest=dest),
            builder=running_agent
        )
        return None

# TODO rename -> QuerySlipnetAndBuild?
@dataclass(frozen=True)
class QuerySlipnetForDelegate(Codelet):
    qargs: R[QArgs] = Ref('qargs')
    #sk: R[Codelets] = Sleep(Ref('running_agent'))  TODO

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        running_agent: Optional[Agent],
        qargs: QArgs,  # TODO Require at least one QArg?
        sources: Sources
    ) -> CodeletResults:
        kwargs = fm.mk_slipnet_args(qargs, sources)
        slipnet_results = fm.pulse_slipnet(
            alog=fm.start_alog((running_agent, self)),
            **kwargs
        )
        if not slipnet_results:
            raise NoResultFromSlipnet(qargs=qargs)
        return [
            Build(
                to_build=fm.try_to_fill_nones(node, sources, running_agent),
                builder=running_agent
            )
                for node in slipnet_results
        ]
        """
        for node in slipnet_results:
            node = fm.try_to_fill_nones(node, sources, running_agent)
            fm.build(node, builder=running_agent)
        return None
        """

@dataclass(frozen=True)
class Sleep(Codelet):
    agent: R[Actor] = Ref('agent')
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
class TaggeeDoesNotExist(Fizzle):
    taggee: Node = None

@dataclass(frozen=True)
class AddTag(Codelet):
    taggee: R[Node] = Ref('taggee')
    tag: R[Node] = Ref('tag')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        taggee: Node,
        tag: Node,
        sources: Sources
    ) -> CodeletResults:
        if not fm.has_node(taggee):
            if isinstance(taggee, CellRef):  # HACKish and needs UT
                sources = fm.run_codelet_and_follow_ups(
                    Build(to_build=taggee),
                    sources
                )
        if fm.has_node(taggee):
            if isclass(tag):
                # TODO Supply arguments to tag ctor
                tag = tag()  # type: ignore[operator]
            sources = fm.run_codelet_and_follow_ups(
                Build(to_build=tag),
                sources
            )
            tag = first(fm.look_up_by_name('built', sources))
            # TODO What if tag is None now?
            fm.add_tag(taggee, tag)
        else:
            raise TaggeeDoesNotExist(taggee=taggee)

        return None

@dataclass(frozen=True)
class RaiseException(Codelet):
    exctype: R[Type[Exception]] = Ref('exctype')

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        exctype: Type[Exception],
        sources: Sources
    ) -> CodeletResults:
        raise exctype(**fm.mk_func_args(exctype, sources))  # type: ignore[call-arg]

@dataclass(frozen=True)
class ISucceeded(Codelet):

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        running_agent: Optional[Agent]
    ) -> CodeletResults:
        if running_agent:
            fm.set_state(fm.behalf_of(running_agent), Delegate_succeeded)
            return NewState(running_agent, Succeeded)
        else:
            return None

# TODO UT
@dataclass(frozen=True)
class FindLastPaintedCell(Codelet):
    '''Sets 'startcell' to the last painted cellref of whatever startcell
    pointed to on entry.'''
    # TODO Allow 'startcell' to be overridden with a target Ref? (indirection)

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        startcell: CellRef  # start searching from here
    ) -> CodeletResults:
        result = startcell.last_painted_cellref()
        with logging(self, cellref=result):
            return {'startcell': result}

    def log(self, f: Indenting, **kwargs) -> None:
        lo(self.__class__, kwargs.get('cellref', None))

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
        running_agent: Optional[Agent]=None
    ) -> CodeletResults:
        true_avails = list(as_iter(cellref.avails))
        new_operands: List[Value] = []

        for a, u in zip(avails, unavails):  # TODO zip_longest?
            if a is not None:
                if a in true_avails:
                    new_operands.append(a)
                    true_avails.remove(a)
                else:
                    self.fill_unavail_from_avails(a, new_operands, true_avails)
            if u is not None:
                self.fill_unavail_from_avails(u, new_operands, true_avails)
        # TODO Don't make a new agent that's already there
        if not isinstance(agent, Agents.Consumer):
            return None  # TODO raise exception?
        else:
            return Build(
                # TODO Standardize order of operands when commutative?
                to_build=replace(agent, operands=tuple(new_operands)),
                builder=running_agent
            )

    @classmethod
    def fill_unavail_from_avails(
        cls,
        u: Value,
        new_operands: List[Value],
        true_avails: List[Value]
    ) -> None:
        '''Chooses one or more replacements for 'u' from 'true_avails',
        removes them from 'true_avails', and appends them to 'new_operands'.'''
        ua: Sequence[Value] = choose_most_similar(true_avails, u)
        if ua:
            new_operands += ua
            for v in ua:
                true_avails.remove(v)
        else:
            pass  # TODO raise an exception?
        

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
