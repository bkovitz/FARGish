
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, runtime_checkable


@dataclass(frozen=True)
class Codelet(ABC):
    name: ClassVar[str]

    @abstractmethod
    def go(self, fm: 'FARGModel', **kwargs) -> 'Codelets':
        '''Should do the codelet's action, and return any follow-up codelets
        to execute next, in the same timestep.'''
        pass

Codelets = Union[None, Codelet, Sequence[Codelet]]

@dataclass(frozen=True)
class BuildCompanion(Codelet):
    behalf_of: Agent
    companion_info: Hashable

    def go(self, fm):
        for ci in self.companion_info:
            fm.build(ci, behalf_of=self.behalf_of)
        return NewState(self.behalf_of, Wake)

@dataclass(frozen=True)
class NewState(Codelet):
    agent: Agent
    state: AgentState

    def go(self, fm, agent: Agent, state: AgentState):
        fm.set_state(agent, state)

@dataclass(frozen=True)
class QuerySlipnetAndBuildDelegate(Codelet):
    features: Features
    slipnode_type: SlipnodeType

    sk = Sleep(Ref('behalf_of'))

    def go(
        self,
        fm,
        behalf_of: Agent,
        features: Features,
        slipnode_type: SlipnodeType
    ):
        # set up activations_in
        # exclude delegates already built
        # pulse_slipnet
        # build a copy of the agent(s) found, linked to behalf_of
        raise NotImplementedError

@dataclass(frozen=True)
class Sleep(Codelet):
    agent: Agent

    def go(self, fm, agent: Agent, num_timesteps: int):
        fm.sleep(agent=agent, num_timesteps=num_timesteps)

@dataclass(frozen=True)
class TakeAvails(Codelet):
    cellref: CellRef
    '''Take avails from cellref and then call sk to build a LitPainter.'''

if __name__ == '__main__':
    class Want(Agent):
        '''An Agent that wants to build a sequence of LitPainters that
        paint a target value onto a SeqCanvas, constructed from avails in
        a given cell--as in Numbo.'''
        target: Hashable
        startcell: Union[CellRef, None]
        sk: Union[Codelet, None]

        born = BuildCompanion([
            AvailDetector(Ref('target'), sk=NotifyMeSuccess),
            GettingCloser.Tagger(Ref('target'))
        ])
        wake = QuerySlipnetAndBuildDelegate([
            BeforeFromAvails(Ref('startcell')),
            AfterFrom(Ref('target')),
            SlipnodeType(Consume)
        ])
        
    fm = FARGModel()
    ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
    wa = fm.build(
        Want(target=15, startcell=cr0, sk=RaiseException(TestFoundIt))
    )

