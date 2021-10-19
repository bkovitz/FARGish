# spike4.py -- Is it feasible to define Agents as collections of Codelets?

from __future__ import annotations
from dataclasses import dataclass, field, fields, InitVar, asdict, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, Optional, runtime_checkable
from abc import ABC, abstractmethod
import inspect

from FMTypes import Elem, Elems, Value, Addr, Pred
from util import as_iter, pr, pts


@dataclass(frozen=True)
class CodeletDataclassMixin:
    name: ClassVar[str]

class Codelet(ABC, CodeletDataclassMixin):

    @abstractmethod
    def go(self, fm: 'FARGModel', **kwargs) -> 'Codelets':
        '''Should do the codelet's action, and return any follow-up codelets
        to execute next, in the same timestep.'''
        pass

    def replace_refs(self, fm, sources: Sequence) -> Codelet:
        d: Dict[str, Any] = {}
        for attrname in self.__dataclass_fields__:  # type: ignore[attr-defined]
            try:
                attr = getattr(self, attrname)
            except AttributeError:
                continue
            if isinstance(attr, Ref):
                d[attrname] = fm.look_up_by_name(attr.name, sources)
            elif hasattr(attr, 'replace_refs'):
                new_attr = attr.replace_refs(fm, sources)  # TODO (car self sources)?
                if new_attr is not attr:
                    d[attrname] = new_attr
        if d:
            return replace(self, **d)
        else:
            return self

Codelets = Union[None, Codelet, Sequence[Codelet]]

@dataclass(frozen=True)
class CellRef:
    pass

@dataclass(frozen=True)
class Ref:
    '''A reference to a member of an Agent.'''
    name: str

@dataclass(frozen=True)
class AgentState:
    name: str

Wake = AgentState('Wake')

@dataclass(frozen=True)
class Agent(ABC):
    born: ClassVar[Codelets]

    def replace_refs(self, fm, sources: Sequence) -> Agent:
        d: Dict[str, Any] = {}
        for attrname in self.__dataclass_fields__:  # type: ignore[attr-defined]
            try:
                attr = getattr(self, attrname)
            except AttributeError:
                continue
            if isinstance(attr, Ref):
                d[attrname] = fm.look_up_by_name(attr.name, sources)
        if d:
            return replace(self, **d)
        else:
            return self

@dataclass
class FARGModel:
    ws: Set[Elem] = field(default_factory=set)
    
    def build(self, elem: Elem, **kwargs) -> Elem:
        # TODO Call ctor? Other args?
        self.ws.add(elem)
        return elem

    def set_state(self, agent: Agent, state: AgentState) -> None:
        raise NotImplementedError

    def run_agent(self, agent: Agent) -> None:
        for codelet in as_iter(agent.born):  # TODO appropriate state
            self.run_codelet(codelet, agent)

    def run_codelet(self, codelet: Codelet, agent: Optional[Agent]=None):
        codelet.go(**self.codelet_args(codelet, agent))

    def codelet_args(self, codelet: Codelet, agent: Optional[Agent]=None) \
    -> Dict[str, Any]:
        codelet = self.replace_refs(codelet, as_list(agent))
        return dict(
            (param_name, self.value_for_codelet_arg(codelet, param_name, agent))
                for param_name in inspect.signature(codelet.go).parameters
        )

    def replace_refs(self, o, behalf_of=Optional[Agent]) -> Elem:
        pass

    def value_for_codelet_arg(
        self,
        codelet: Codelet,
        param_name: str,
        agent: Optional[Agent]=None
    ) -> Any:
        if param_name == 'fm':
            return self
        if param_name == 'behalf_of':
            return agent  # TODO What if agent is None?
        try:
            return getattr(codelet, param_name)
        except AttributeError:
            return None

    def look_up_by_name(
        self,
        name: str,
        sources: List
    ) -> Any:
        if name == 'fm':
            return self
        for source in sources:
            try:
                return getattr(source, name)
            except AttributeError:
                continue
        return None

# Codelets

@dataclass(frozen=True)
class BuildCompanion(Codelet):
    #behalf_of: Agent
    companion_info: Hashable

    def go(   # type: ignore[override]
        self, fm, behalf_of: Agent, companion_info: Hashable
    ):
        for ci in as_iter(companion_info):
            fm.build(ci, behalf_of=behalf_of)
        return NewState(behalf_of, Wake)

@dataclass(frozen=True)
class NewState(Codelet):
    agent: Agent
    state: AgentState

    def go(self, fm, agent: Agent, state: AgentState): # type: ignore[override]
        fm.set_state(agent, state)

# Agents

@dataclass(frozen=True)
class AvailDetector(Agent):  # TODO should inherit from Detector
    target: Hashable
    # TODO filter: FMPred
    # TODO action

    # TODO def look()...
    
@dataclass(frozen=True)
class Want(Agent):
    target: Hashable = None
    startcell: Union[CellRef, None] = None
    sk: Union[Codelet, None] = None

    born = BuildCompanion(
        AvailDetector(Ref('target'))
    )

def rs(o):
    for f in o.__dataclass_fields__:
        try:
            attr = getattr(o, f)
        except AttributeError:
            continue
        if isinstance(attr, Ref):
            print(attr)

if __name__ == '__main__':
    fm = FARGModel()
    wa: Want = fm.build(Want(target=15))  # type: ignore[assignment]
    b = wa.born
    d = fm.codelet_args(wa.born, wa)
    #pr(d)
    a = d['companion_info']
    print(a)
    #print(fm.replace_refs(a, behalf_of=wa))
    #rs(a)
    a2 = a.replace_refs(fm, [wa])
    print(a2)
    print()
    b2 = b.replace_refs(fm, [wa])
    print(b2)


