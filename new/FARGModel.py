# FARGModel.py

from __future__ import annotations
from dataclasses import dataclass, field, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, \
    Iterator, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, \
    runtime_checkable
from abc import ABC, abstractmethod
import inspect
from inspect import isclass, signature

from FMTypes import Node, Nodes, WSPred, match_wo_none
from Propagator import Propagator
from Graph import Graph, Hop, WithActivations, GraphPropagatorOutgoing
from Slipnet import Slipnet
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement


@dataclass(frozen=True)
class Ref:
    '''A reference by name to a member of an enclosing Agent, Codelet, or
    FARGModel.'''
    name: str

T = TypeVar('T')
N = TypeVar('N', bound=Node)
W = TypeVar('W', bound='Workspace')

# Wrap the type of any field of an Agent of Codelet in R[] to allow a Ref
# in its place, e.g.  my_string: R[str] = None
R = Union[T, Ref, None]

Sources = Any  # A Sequence of sources for looking up the values of Refs,
               # or just a single source.

class CanReplaceRefs:
    '''A mix-in for dataclasses whose fields may be Ref objects.'''

    def replace_refs(self: T, fm: FARGModel, sources: Sources) -> T:
        d: Dict[str, Any] = {}
        #sources1: Sequence = [self] + [s for s in sources]
        sources1 = fm.prepend_source(self, sources)
        for attrname in self.__dataclass_fields__:  # type: ignore[attr-defined]
            try:
                attr = getattr(self, attrname)
            except AttributeError:
                continue
            if isinstance(attr, Ref):
                d[attrname] = fm.look_up_by_name(attr.name, sources)
            elif isinstance(attr, CanReplaceRefs):
                new_attr = attr.replace_refs(fm, sources1)
                if new_attr is not attr:
                    d[attrname] = new_attr
        if d:
            return replace(self, **d)
        else:
            return self

@dataclass(frozen=True)
class BehalfOf:
    behalf_of: Agent

### Agent and ancillary classes and objects ###

@dataclass(frozen=True)
class Agent(CanReplaceRefs):
    born: Codelets = None
    wake: Codelets = None
    snag: Codelets = None
    delegate_succeeded: Codelets = None
    delegate_failed: Codelets = None
    succeeded: Codelets = None
    failed: Codelets = None

@dataclass(frozen=True)
class AgentState:
    name: str

Born = AgentState('born')
Wake = AgentState('wake')
Snag = AgentState('snag')
Delegate_succeeded = AgentState('delegate_succeeded')
Delegate_failed = AgentState('delegate_failed')
Succeeded = AgentState('succeeded')
Failed = AgentState('failed')
Defunct = AgentState('defunct')

### Codelet and ancillary classes ###

@dataclass(frozen=True)
class CodeletDataclassMixin:
    name: ClassVar[str]

class Codelet(ABC, CodeletDataclassMixin, CanReplaceRefs):

    @abstractmethod
    def run(self, fm: FARGModel, **kwargs) -> 'Codelets':
        '''Should do the codelet's action, and return any follow-up codelets
        to execute next, in the same timestep.'''
        pass

Codelets = Union[None, Codelet, Sequence[Codelet]]

class NullCodelet(Codelet):

    def run(self) -> Codelets:  # type: ignore[override]
        return None

### Workspace and ancillary classes ###

@dataclass
class ActivationGraph(WithActivations, Graph):
    propagator: Propagator = field(
        default_factory=lambda: GraphPropagatorOutgoing()
    )

@dataclass
class NodeInWS:
    '''A Node in the Workspace and information about it.'''
    node: Node
    builder: Union[Agent, None]
        # .builder should not be used by Agents; it's strictly for debugging
        # and reporting on model behavor.
    behalf_of: List[Agent] = field(init=False, default_factory=list)
        # .behalf_of is all the Agents that want this Node to exist, including
        # but not limited to its builder.
    tob: int   # time of birth (when Node was added to the ws)
    state: AgentState  # applicable only when node is an Agent
    # overrides for contents?
    # overrides for orientation?

    def add_behalf_of(self, agents):
        self.behalf_of += as_iter(agents)

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

@dataclass
class Workspace(HasRngSeed):
    '''The Workspace is strictly a container of nodes and the activation
    graph that holds and connects their activations. All notions of Agents
    and Codelets are left to the FARGModel class.'''
    wsd: Dict[Node, NodeInWS] = field(default_factory=dict)
    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph.empty
    )
    t: int = 0  # TODO inherit this from something that knows about time
    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    #def build(self, *args, **kwargs) -> Node:
    def build(
        self: W,
        obj: N,
        **kwargs
    ) -> N:
        '''The arguments specify an Node to build in the workspace. If such
        an Node already exists, we don't build anything. Returns the built
        or found Node.'''
        """
        if not args:
            raise NotImplementedError('still need to provide object to .build')
        if isclass(args[0]):
            raise NotImplementedError(".build can't yet construct the object for you")
        obj = args[0]
        """
        if obj is None:  # attempting to build None builds nothing
            return None

        builder: Optional[Agent] = kwargs.pop('builder', None)
        niws = self.get_niws(obj)
        if niws is None:  # if the Node is not there, really build it
            niws = self._really_build(obj, builder, **kwargs)
        else:
            obj = niws.node  # type: ignore[assignment]
        if builder:
            niws.add_behalf_of(builder)
            self.add_mutual_support(builder, obj)
        return obj

    def _really_build(
        self, obj: Node, builder: Union[Agent, None], **kwargs
    ) -> NodeInWS:
        init_a = kwargs.pop('init_a', None)
        min_a = kwargs.pop('min_a', None) # only has effect on true build
        max_a = kwargs.pop('max_a', None) # only has effect on true build

        self.wsd[obj] = niws = NodeInWS(
            obj, builder=builder, tob=self.t, state=self.initial_state(obj)
        )

        # Set up activation stuff
        if init_a is None:
            if builder is None:
                init_a = 1.0
            else:
                init_a = min(1.0, self.a(builder))
        if min_a is not None:
            force_setattr(obj, 'min_a', min_a)
        min_a = getattr(obj, 'min_a', None)
        if max_a is not None:
            force_setattr(obj, 'max_a', max_a)
        max_a = getattr(obj, 'max_a', None)
        init_a = clip(min_a, max_a, init_a)
        self.activation_g.add_node(obj)
        self.activation_g.set_a(obj, init_a)

        return niws

        # create antipathy between obj and its enemies
        """ TODO
        for node in self.nodes(HasAntipathyTo(obj, ignore=builder)):
            self.add_mutual_antipathy(obj, node)
        """

        # TODO Something needs to call an Agent's 'build' codelets
        # Put an .on-build() in this class and call it? FARGModel
        # can override it.

    def initial_state(self, obj: Node) -> AgentState:
        if isinstance(obj, Agent):
            if obj.born:
                return Born
            else:
                return Wake
        else:
            return Born

    def set_state(self, node: Node, state: AgentState) -> None:
        '''Sets the state of node. No effect if node does not exist.'''
        niws = self.get_niws(node)
        if niws:
            niws.state = state

    # Activation / support

    def a(self, node: Node) -> float:
        return self.activation_g.a(node)

    def ae_weight(self, from_node: Node, to_node: Node) -> float:
        return self.activation_g.hop_weight(from_node, to_node)

    def add_mutual_support(
        self, a: Node, b: Node, weight: Optional[float]=None
    ) -> None:
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_hop(Hop(a, b, weight))
        self.activation_g.add_hop(Hop(b, a, weight))
        
    # Querying

    def get_niws(self, obj: Node) -> Union[NodeInWS, None]:
        '''Returns obj's NodeInWS from the workspace if it exists, otherwise
        None.'''
        return self.wsd.get(obj, None)
        
    def nodes(self, pred: WSPred=None, es=None) -> Iterable[Node]:
        '''Returns a generator for *all* matches of pred. Unlike .ws_query(),
        .nodes() does not make a weighted choice.'''
        # TODO Explain es. (It's a subset to search.)
        wspred = as_wspred(pred)
        if es is None:
            es = self.wsd.keys()
        return (e for e in as_iter(es) if wspred(self, e))

    def ws_query(self, pred: WSPred, min_a: Union[float, None]=None, k: int=1) \
    -> Iterable[Node]:
        '''Returns generator of up to k nodes that match pred,
        chosen randomly, weighted by activation.'''
        nodes = self.nodes(pred)
        if min_a is not None:
            nodes = (e for e in nodes if self.a(e) >= min_a)
        nodes = list(nodes)
        activations = [self.a(e) for e in nodes]
        yield from sample_without_replacement(
            nodes, weights=activations, k=k
        )

    def ws_query1(self, pred: WSPred, min_a: Union[float, None]=None) \
    -> Union[Node, None]:
        '''Like .ws_query() except returns only the first choice, or None
        if there are no matches. Conveniently, the result is not inside
        a generator.'''
        return first(self.ws_query(pred=pred, min_a=min_a, k=1))

    def the(self, pred: WSPred, es=None) -> Union[Node, None]:
        '''Returns the first element from .nodes(), or None if there isn't
        one.'''
        return first(self.nodes(pred=pred, es=es))

    def has_node(self, node: Node) -> bool:
        return node in self.wsd

    def agent_state(self, agent: Agent) -> AgentState:
        '''Returns the current state of 'agent', or Defunct if 'agent'
        does not exist.'''
        niws = self.get_niws(agent)
        if niws:
            return niws.state
        else:
            return Defunct

    # Node info

    def builder_of(self, node: Node) -> Union[Agent, None]:
        niws = self.get_niws(node)
        if niws:
            return niws.builder
        else:
            return None

    def behalf_of(self, node: Node) -> Sequence[Agent]:
        try:
            return self.wsd[node].behalf_of
        except KeyError:
            return []

    def built_by(self, agent: Agent) -> Collection[Node]:
        # TODO Optimize: this checks all Elems in the ws
        return [
            e for e in self.nodes()
                if self.builder_of(e) is agent
        ]

def as_wspred(o: WSPred) -> Callable[[Workspace, Any], bool]:
    '''Returns a predicate function that takes two arguments: a Workspace
    and an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return lambda ws, x: isinstance(x, o)  # type: ignore[arg-type]  # mypy bug?
    elif isinstance(o, tuple):
        preds = tuple(as_wspred(p) for p in o)
        return lambda ws, x: any(p(ws, x) for p in preds)
    elif callable(o):
        if first_arg_is_ws(o):
            return o  # type: ignore[return-value]
        else:
            return lambda ws, x: o(x)  # type: ignore[call-arg, operator, misc]  # mypy bug
    elif o is None:
        return lambda ws, x: True
    else:
        return lambda ws, x: match_wo_none(x, o)

def first_arg_is_ws(o: Callable) -> bool:
    p0 = first(signature(o).parameters.values())
    try:
        return issubclass(p0.annotation, Workspace)
    except TypeError:
        return False

### The generic FARGModel ###

@dataclass
class FARGModel(Workspace):
    '''A generic FARG model.'''

    def replace_refs(
        self,
        codelet: Union[None, Codelet],  # TODO Allow Codelets. Agent, too?
        sources: Sources
    ) -> Codelet:
        if codelet is None:
            return NullCodelet()
        return codelet.replace_refs(self, sources)

    # TODO Make agent_state optional; if None, call agent's current state.
    def run_agent(self, agent: Agent, agent_state: AgentState) -> None:
        '''Fills in agent's Refs and runs agent. Has no effect if agent is not
        a node in the workspace.'''
        if not self.has_node(agent):
            return
        agent = agent.replace_refs(self, None)
        sources = self.mk_sources(agent)
        """
        codelet: Codelet
        for codelet in as_iter(getattr(agent, agent_state.name)):
            codelet.run(**self.codelet_args(codelet, agent))
        """
        # WANT Run each codelet, followed by all of the codelets that it
        # generates, and so on, until one of them fails.
        codelet: Codelet
        for codelet in as_iter(getattr(agent, agent_state.name)):
            self._run_codelet_and_follow_ups(codelet, sources)
            sources = self.prepend_source(codelet, sources)

    def run_codelet(self, codelet: Codelets, agent: Optional[Agent]=None) \
    -> None:
        '''Fills in codelet's Refs and runs codelet. Normally you should
        call .run_agent(), not .run_codelet(). .run_codelet() is mainly
        for unit testing.'''
        sources = self.mk_sources(agent)
        for c in as_iter(codelet):
            c = c.replace_refs(self, sources)
            self._run_codelet_and_follow_ups(c, sources)
            sources = self.prepend_source(c, sources)

    def _run_codelet_and_follow_ups(
        self, codelet: Codelet, sources: Sources
    ) -> None:
        '''codelet must have its Refs already filled in. Runs codelet.
        If codelet returns follow-up codelets, we fill in their refs and
        run those, too, as well as any follow-ups that they return, and
        so on. If any of them fails, we don't catch the exception.'''
        kwargs = self.codelet_args(codelet, sources)
        more_codelets = codelet.run(**kwargs)
        for c in as_iter(more_codelets):
            c = c.replace_refs(self, sources)
            self._run_codelet_and_follow_ups(c, sources)
            sources = self.prepend_source(c, sources)

    def codelet_args(self, codelet: Codelet, sources: Sources) \
    -> Dict[str, Any]:
        '''Returns a dict containing the arguments to pass to codelet's
        .run() method. codelet must not contain any Refs. Pass a codelet
        through .replace_refs() before calling .codelet_args().'''
        codelet = codelet.replace_refs(self, self.mk_sources(sources))
        return dict(
            #(param_name, self.value_for_codelet_arg(codelet, param_name, agent))
            (param_name,
             self.value_for_codelet_arg(codelet, param_name, sources))
                for param_name in inspect.signature(codelet.run).parameters
        )

    def mk_sources(self, agent: Optional[Agent]=None) -> Sequence:
        if agent:
            return [agent, BehalfOf(agent)]
        else:
            return []

    def prepend_source(self, car: Any, sources: Sources) -> Sources:
        return as_list(car) + [s for s in as_iter(sources)]

    # TODO rm?
    def value_for_codelet_arg(
        self,
        codelet: Codelet,
        param_name: str,
        sources: Sources
    ) -> Any:
        if param_name == 'fm':
            return self
        sources = self.prepend_source(codelet, sources)
        return self.look_up_by_name(param_name, sources)
        """
        if param_name == 'behalf_of':
            return agent  # TODO What if agent is None?
        try:
            return getattr(codelet, param_name)
        except AttributeError:
            return None
        """

    def look_up_by_name(
        self,
        name: str,
        sources: Sequence
    ) -> Any:
        if name == 'fm':
            return self
        for source in as_iter(sources):
            try:
                result = getattr(source, name)
            except AttributeError:
                continue
            if result is not None:
                return result
        return None

    def __str__(self):
        return self.__class__.__name__

    __repr__ = __str__
