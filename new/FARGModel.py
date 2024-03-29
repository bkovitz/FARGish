# FARGModel.py

from __future__ import annotations
from dataclasses import dataclass, field, fields, replace, InitVar, Field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable, get_type_hints
from abc import ABC, abstractmethod
from collections import defaultdict
import inspect
from inspect import isclass, signature
from contextlib import contextmanager
import sys
from types import MethodType, FunctionType
from random import randrange

from FMTypes import Node, Nodes, Addr, Value, WSPred, match_wo_none, Pred, \
    as_pred, ADict, AndFirst, CallablePred, MatchWoNone, IsInstance, \
    combine_preds, AlwaysTrue, HasBindWs, HasArgs, Ref, N, T, Exclude
from Propagator import Propagator, ActivationLogs, ActivationLog, \
    PropagatorOutgoing
from Graph import Graph, Hop, WithActivations, Feature
from Slipnet import Slipnet
from Indenting import Indenting, indent
from Log import lo, trace, logging, Loggable, logging_is_enabled, logfile, \
    LogKwargs, enabled_for_logging
from util import as_iter, as_list, first, force_setattr, clip, HasRngSeed, \
    sample_without_replacement, pr, pts, is_type_instance, \
    is_dataclass_instance, make_nonoptional, dict_str, short, class_of, omit, \
    as_dict, fields_for, transitive_closure


#N = TypeVar('N', bound=Node)
W = TypeVar('W', bound='Workspace')

### Classes for named references inside Codelets and Agents ###

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
                v = fm.look_up_by_name(attr.name, sources)
                #if v is not None:
                d[attrname] = v
#            elif isinstance(attr, CanReplaceRefs):
#                new_attr = attr.replace_refs(fm, sources1)
#                if new_attr is not attr:
#                    d[attrname] = new_attr
            # NEW
#            elif isinstance(attr, tuple):
#                d[attrname] = tuple(
#                    x.replace_refs(fm, sources)
#                        if isinstance(x, CanReplaceRefs) else x
#                            for x in attr
#                )
#                lo('DATTR', self, attrname, str(attr), str(d[attrname]))
        if d:
            return replace(self, **d)
        else:
            return self

def replace_refs_in_dataclass_instance(
    o: T, fm: FARGModel, sources: Sources
) -> T:
        d: Dict[str, Any] = {}
        for attrname in o.__dataclass_fields__:  # type: ignore[attr-defined]
            try:
                attr = getattr(o, attrname)
            except AttributeError:
                continue
            if isinstance(attr, Ref):
                v = fm.look_up_by_name(attr.name, sources)
                d[attrname] = v
        if d:
            return replace(o, **d)
        else:
            return o

@dataclass(frozen=True)
class RunningAgent:
    running_agent: Actor

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.running_agent)})'

### Logging ###

class FMLoggable(ABC):
    '''Mix-in for classes that know how to print their own log entries and
    require a FARGModel as the first argument to .log().'''

    @abstractmethod
    def log(self, fm: FARGModel, f: Indenting, **kwargs) -> None:
        '''Called by the 'logging()' context manager to indicate that self
        should make a log entry. Must print to 'f'.'''
        pass

@contextmanager
def fmlogging(fm: FARGModel, arg: Any, **kwargs):
    global logfile
    try:
        if logging_is_enabled(arg):
            if isinstance(arg, FMLoggable):
                arg.log(fm, logfile(), **kwargs)
            else:
                raise NotImplementedError(arg)
        with indent(logfile()):
            yield logfile()
    finally:
        pass

class LogPulse(LogKwargs):
    name = 'PULSE'
log_pulse = LogPulse()

### Detectors ###

@dataclass(frozen=True)
class DetectorDataclassMixin:
    recently_seen: Set[Node] = field(default_factory=set, compare=False)

    id: int = field(init=False)  # unique id
    lastid: ClassVar[int] = 0

    def __post_init__(self) -> None:
        self.__class__.lastid += 1
        force_setattr(self, 'id', self.__class__.lastid)

    # TODO Track the timestep when a node was seen, so the Detector can
    # forget seeing it after a while.
    def add_recently_seen(self, node: Node) -> None:
        self.recently_seen.add(node)

    def was_recently_seen(self, node: Node) -> bool:
        return node in self.recently_seen

    """
    def __hash__(self) -> int:
        lo('HASH')
        return hash((self.__class__.__name__, self.id))

    def __eq__(self, other: Any) -> bool:
        return (
            isinstance(other, DetectorDataclassMixin)
            and
            self.id == other.id
        )
    """

class Actor:
    '''Generic marker class for Agents and Detectors: nodes that are capable
    of running codelets and building nodes.'''
    pass

@dataclass(frozen=True)
class Painter(Actor):
    '''An Actor class that can paint to canvas cells. We need this to trace
    who painted what.'''
    value: Optional[Value] = None
    dest: Optional[CellRef] = None

class Detector(ABC, Actor, CanReplaceRefs, DetectorDataclassMixin):

    @abstractmethod
    def look(self, fm: FARGModel, **kwargs) -> CodeletResults:
        '''Try to detect whatever the Detector is looking for, and return
        codelets in response to whatever is found (or not).'''
        pass

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({dict_str(as_dict(self), xform=short)})'

class Tag:
    '''Mix-in for nodes that can serve as tags of other nodes. The .also_tag()
    method makes a tag applied to one node automatically tag other nodes
    related to it.'''

    def also_tag(self, fm: FARGModel, taggee: Node) -> Iterable[Node]:
        return ()

### Codelet and ancillary classes ###

@dataclass(frozen=True)
class CodeletDataclassMixin:
    name: ClassVar[str]

class Codelet(CodeletDataclassMixin, CanReplaceRefs, Loggable, ABC):

    @abstractmethod
    def run(self, fm: FARGModel, **kwargs) -> CodeletResults:
        '''Should do the codelet's action, and return any follow-up codelets
        to execute next, in the same timestep.'''
        pass

    def log(self, f: Indenting, **kwargs) -> None:
        cl = self.__class__.__name__
        kw = omit(kwargs.get('kwargs', {}), ['fm', 'running_agent', 'sources'])
        lo(f'CODELET {cl:25s}  {dict_str(kw, short)}')

    def short(self) -> str:
        return self.__class__.__name__

Codelets = Union[None, Codelet, Sequence[Codelet]]
CodeletResult = Union[None, Codelet, Dict[str, Any]]
CodeletResults = Union[CodeletResult, Sequence[CodeletResult]]

@dataclass(frozen=True)
class CodeletRun:
    '''A record of one instance of a specific codelet running.'''
    codelet: Union[Codelet, Type[Codelet]]
    kwargs: Dict[str, Any]  # the arguments that were passed to codelet.run
    t: int                  # the timestep
    agent: Optional[Agent]  # the agent that ran the codelet

    def __str__(self) -> str:
        return f'{short(self.codelet)} {dict_str(self.kwargs, short)} t={self.t} {short(self.agent)}'

    def clis(self, c: Union[Codelet, Type[Codelet]]) -> bool:
        '''Is .codelet the same class as c?'''
        return class_of(c) == class_of(self.codelet)

class NullCodelet(Codelet):

    def run(self) -> Codelets:  # type: ignore[override]
        return None

class QArg:
    '''An argument for a slipnet query, to be passed through
    FARGModel.mk_slipnet_args() on its way to being passed to
    FARGModel.pulse_slipnet().'''

    def get_activations_in(self, fm: FARGModel, sources: Sources) \
    -> Dict[Node, float]:
        '''Returns a dictionary of Nodes and their activations to include
        in the initial pulse that starts spreading activation. The default
        implementation returns an empty dict.'''
        return {}

    def get_pred(self, fm: FARGModel, sources: Sources) -> Pred:
        '''Returns a Pred to add (disjunctively) to any other predicates
        restricting what nodes can be returned from a slipnet query. The
        default implementation returns None, i.e. no restriction.'''
        return None
    
    def get_k(self) -> Union[int, None]:
        '''Value to change k to, or None if no change. The default
        implementation returns None.'''
        return None

    def get_num_get(self) -> Union[int, None]:
        '''Value to change num_get to, or None if no change. The default
        implementation returns None.'''
        return None

class QInput(ABC, QArg):  # TODO rename -> QInput
    '''An object that generates input nodes for a slipnet query, possibly
    after examining something in the workspace.'''

    @abstractmethod
    def items(self, **kwargs) -> Nodes:
        pass

    def get_items(self, fm: FARGModel, sources: Sources) -> Nodes:
        return self.items(**fm.mk_func_args(self.items, sources))
        
    def get_activations_in(self, fm: FARGModel, sources: Sources) \
    -> Dict[Node, float]:
        return dict(
            (item, 1.0) for item in as_iter(self.get_items(fm, sources))
        )

class QPred(QArg):
    '''An object specifying a predicate for a slipnet query.'''

    @abstractmethod
    def pred(self, **kwargs) -> Pred:
        pass

    def get_pred(self, fm: FARGModel, sources: Sources) -> Pred:
        return self.pred(**fm.mk_func_args(self.pred, sources))

QArgs = Union[None, Node, QArg, Tuple[Union[Node, QArg], ...]]

@dataclass(frozen=True)
class SnaggedAgent(Feature):
    '''A Feature labeling an Agent as being snagged 'before' the the action
    tagged with this Feature.'''
    agent: Actor

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.agent)})'

@dataclass(frozen=True)
# TODO rm? Desnag
class Unsnag(Feature):
    tag: Node

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.tag)})'

@dataclass(frozen=True)
class QueryForSnagFixer(Codelet):
    
    default_slipnet_kwargs: ClassVar[Dict[str, Any]] = dict(k=4)

    def run(  # type: ignore[override]
        self,
        fm: FARGModel,
        running_agent: Optional[Actor],
        sources: Optional[Sources]
    ) -> CodeletResults:
        if not running_agent:
            return None  # TODO Fizzle?
        qargs: QArgs = (
            (SnaggedAgent(running_agent), QArgsModule.SearchFor(Agent))
            +
            tuple(
                Desnag(tag) if isinstance(tag, Fizzle) else tag
                    for tag in fm.tags_of(running_agent)
            )
        )
        kwargs = (
            fm.mk_slipnet_args(qargs, sources)
            |
            self.default_slipnet_kwargs
        )

        #print('QARGS', short(qargs))
        #print()
        #pr(kwargs, key=short)
        alog = fm.start_alog((running_agent, self))
        slipnet_results = fm.pulse_slipnet(alog=alog, **kwargs) # type: ignore[arg-type]
        """ # TODO
        if not slipnet_results:
            raise NoResultFromSlipnet(activations_in.keys())
        """
        result: List[Codelet] = []
        if slipnet_results:
            result += [
                CodeletsModule.Build(
                    #to_build = fm.try_to_fill_nones(node, sources, running_agent),
                    to_build = fm.try_to_fill_refs(
                        node,
                        situation=running_agent,
                        sources=sources
                    ),
                    builder=running_agent
                )
                    for node in slipnet_results
            ]
        else:
            raise NoResultFromSlipnet(qargs=qargs)
            #print('FAILURE QueryForSnagFixer: no slipnet_results!', file=logfile)
            #fm.set_state(running_agent, Failed)
        #lo('QSNAG', str(result[0]))
        result.append(CodeletsModule.Sleep(
            agent=running_agent,
            sleep_duration=10
        ))
        return result

### Agent and ancillary classes and objects ###

@dataclass(frozen=True)
class Agent(Actor, CanReplaceRefs, FMLoggable, HasArgs):
    '''An Agent has no Python code. It holds only fields containing zero
    or more codelets for each AgentState, and, optionally, fields for
    parameters to the Agent. Due to the rules of dataclass inheritance,
    you must specify those optional parameters by explicit keyword
    arguments when constructing an instance of an Agent subclass.'''
    born: Codelets = None
    wake: Codelets = None
    snag: Codelets = QueryForSnagFixer()
    delegate_succeeded: Codelets = None
    delegate_failed: Codelets = None
    succeeded: Codelets = None
    failed: Codelets = None

    all_state_names: ClassVar[FrozenSet[str]] = frozenset((
        'born', 'wake', 'snag', 'delegate_succeeded', 'delegate_failed',
        'succeeded', 'failed'
    ))

    def get_codelets(self, agent_state: AgentState) -> Codelets:
        return getattr(self, agent_state.name, None)

    @classmethod
    def CanRun(cls, fm: FARGModel, node: Node) -> bool:
        '''Is 'node' an Agent that can run right now? An Agent can run if it's
        not sleeping and it has codelets defined for its current state.'''
        if isinstance(node, Agent):
            if fm.is_sleeping(node):
                return False
            else:
                return bool(node.get_codelets(fm.agent_state(node)))
        else:
            return False

    def need_args(self) -> Set[Ref]:
        result: Set[Ref] = set()
        for name, v in as_dict(self).items():
            if name in self.all_state_names:
                continue
            if v is None:
                result.add(Ref(name))
            elif isinstance(v, Ref):
                result.add(v)
            else:
                continue
        return result
        
    def log(self, fm: FARGModel, f: Indenting, **kwargs) -> None:
        print(
            f'AGENT {short(self):25s}  state={fm.agent_state(self)} t={fm.t}',
            file=f
        )

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}()'

    def __str__(self) -> str:
        cl = self.__class__.__name__
        kvs = ', '.join(
            f'{name}={short(v)}'
                for name, v in self.argsdict().items()
        )
        return f'{cl}({kvs})'

    def argsdict(self) -> Dict[str, Any]:
        return omit(as_dict(self), self.all_state_names)

@dataclass(frozen=True)
class AgentState:
    name: str

    # State to transition to if no codelets are defined for this state, or
    # None to stay in this state.
    if_quiescent: Optional[AgentState]

    def __str__(self):
        return self.name

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, AgentState):
            return self.name == other.name
        else:
            return False

Sleeping = AgentState('sleeping', None)
Wake = AgentState('wake', Sleeping)
Born = AgentState('born', Wake)
Snag = AgentState('snag', Wake)
Delegate_succeeded = AgentState('delegate_succeeded', Wake)
Delegate_failed = AgentState('delegate_failed', Wake)
Succeeded = AgentState('succeeded', None)
Failed = AgentState('failed', None)
Defunct = AgentState('defunct', None)
Nonexistent = AgentState('nonexistent', None)

### Canvas, CellRef, things with the notion of avail values ###

class HasAvailValues(ABC):
    '''Mix-in for cells and other things that (can) have avail values.'''
    
    def has_avail_value(self, v: Value) -> bool:
        return (v in self.avails) if self.avails else False

    @abstractmethod
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        # TODO Require a determinate Collection, not just an Iterable (since
        # we might fail and need to put 'values' into an exception).
        pass

    @property
    @abstractmethod
    def avails(self) -> Union[Sequence[Value], None]:
        pass


class Canvas(ABC):
    '''Something on which items can be painted.'''

    # TODO get an iterator of all CellRefs, search for a value

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value) -> None:
        pass

    @abstractmethod
    def next_addr(self, addr: Addr) -> Addr:
        '''The exact meaning of 'next' must vary depending on the type of
        Canvas. TODO Add another argument to indicate direction, as needed in a
        2-dimensional Canvas or to go forward or backward in a 1-dimensional
        Canvas.'''
        pass

    @abstractmethod
    def first_addr(self) -> Addr:
        pass

    @abstractmethod
    def addrs(self, starting_at: Optional[Addr]=None) -> Iterable[Addr]:
        pass

    def cellrefs(
        self,
        starting_at: Optional[CellRef]=None
    ) -> Iterable[CellRef]:
        addrs: Iterable[Addr]
        if starting_at is not None:
            addrs = self.addrs(starting_at.addr)
        else:
            addrs = self.addrs()
        return (CellRef(self, addr) for addr in addrs)

    def has_a_value(self, addr: Addr) -> bool:
        return self[addr] is not None

    def next_cellref(self, cellref: CellRef) -> CellRef:
        return replace(cellref, addr=self.next_addr(cellref.addr))

    def last_painted_addr(self, starting_at: Optional[Addr]=None) -> Addr:
        last_addr: Addr
        if starting_at is None:
            last_addr = self.first_addr()
        else:
            last_addr = starting_at
        for addr in self.addrs(starting_at=starting_at):
            if not self.has_a_value(addr):
                break
            last_addr = addr
        return last_addr

    def last_painted_cellref(
        self,
        starting_at: Optional[CellRef]=None
    ) -> CellRef:
        return CellRef(self, self.last_painted_addr(starting_at=starting_at))

@dataclass(frozen=True)
class CellRef(HasAvailValues):
    '''A reference to a cell in a Canvas.'''
    canvas: Union[Canvas, None] = None
    addr: Union[Addr, None] = None

    # TODO .next_cellref(), .last_nonblank_cellref(), cellrefs_forward()

    @property
    def value(self) -> Value:
        if (
            self.canvas is not None
            and
            self.addr is not None
        ):
            return self.canvas[self.addr]
        else:
            return None

    def has_a_value(self) -> bool:
        return self.value is not None

    def paint(self, v: Value) -> None:
        if (  # OAOO this if?  (might be hard with mypy)
            self.canvas is not None
            and
            self.addr is not None
        ):
            self.canvas[self.addr] = v

    @property
    def avails(self) -> Union[Sequence[Value], None]:
        if isinstance(self.value, HasAvailValues):
            return self.value.avails
        else:
            return None

    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        v = self.value
        if isinstance(v, HasAvailValues):
            try:
                return v.take_avails(values)
            except ValuesNotAvail as vna:
                raise replace(vna, cellref=self)
        else:
            raise ValuesNotAvail(
                cellref=self, avails=tuple(values), unavails=()
            )

    def next_cellref(self) -> CellRef:
        if isinstance(self.canvas, Canvas):
            return self.canvas.next_cellref(self)
        else:
            return self

    def last_painted_cellref(self) -> CellRef:
        if isinstance(self.canvas, Canvas):
            return self.canvas.last_painted_cellref(self)
        else:
            return self  #???

    def __str__(self):
        return f'canvas[{self.addr}]'

### Workspace and ancillary classes ###

@dataclass
class ActivationGraph(WithActivations, Graph):
    propagator: Propagator = field(
        default_factory=lambda: PropagatorOutgoing()
    )

@dataclass
class NodeInWS(Loggable):
    '''A Node in the Workspace and information about it. As a Loggable object,
    logs a node just built.'''
    node: Node
    builder: Union[Node, None]
        # .builder should not be used by Agents; it's strictly for debugging
        # and reporting on model behavor.
    behalf_of: List[Node] = field(init=False, default_factory=list)
        # .behalf_of is all the Agents that want this Node to exist, including
        # but not limited to its builder.
    tob: int   # time of birth (when Node was added to the ws)
    state: AgentState  # applicable only when node is an Agent
    id: int = field(init=False)   # unique id (among all nodes ever in the workspace)
    # overrides for contents?
    # overrides for orientation?

    lastid: ClassVar[int] = 0

    def __post_init__(self) -> None:
        self.__class__.lastid += 1
        self.id = self.__class__.lastid

    def add_behalf_of(self, agents):
        # TODO Shouldn't .behalf_of be a Set?
        self.behalf_of += as_iter(agents)

    def __str__(self):
        result = f'{self.id:4d} {self.node}  builder={self.builder} tob={self.tob}'
        if isinstance(self.node, Agent):
            result += f' state={self.state}'
        return result

    def short(self) -> str:
        result = f'{self.id:4d} {short(self.node)}  builder={short(self.builder)} tob={self.tob}'
        if isinstance(self.node, Agent):
            result += f' state={self.state}'
        return result

    def log(self, f: Indenting, **kwargs) -> None:
        st = f'  {self.state}' if isinstance(self.node, Agent) else ''
        print(
            f'BUILT {self.node}{st}  builder={short(self.builder)}',
            file=f
        )
Built = NodeInWS  # Convenient alias for logging


@dataclass
class Workspace(HasRngSeed):
    '''The Workspace is strictly a container of nodes and the activation
    graph that holds and connects their activations. All notions of Agents
    and Codelets are left to the FARGModel class.'''
    wsd: Dict[Node, NodeInWS] = field(default_factory=dict)
    _tags_of: Dict[Node, Set[Node]] = field(
        default_factory=lambda: defaultdict(set),
        init=False
    )
    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph.empty
    )
    _painted_by: Dict[Tuple[CellRef, Value], Set[Actor]] = field(
        default_factory=lambda: defaultdict(set),
        init=False
    )
    t: int = 0  # TODO inherit this from something that knows about time
    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2
    paint_threshold: float = 1.0
        # a Painter must have at least this much activation

    #def build(self, *args, **kwargs) -> Node:
    def build(
        self: W,
        obj: N,
        **kwargs
    ) -> N:
        '''The arguments specify a Node to build in the workspace. If such
        a Node already exists, we don't build anything. Returns the built
        or found Node.'''
        """
        if not args:
            raise NotImplementedError('still need to provide object to .build')
        if isclass(args[0]):
            raise NotImplementedError(".build can't yet construct the object for you")
        obj = args[0]
        """
        #print('BBBB', obj.__class__.__name__)
        #lo('BBBBB', str(obj))
        if obj is None:  # attempting to build None builds nothing
            return None

        builder: Optional[Agent] = kwargs.pop('builder', None)
        niws = self.get_niws(obj)
        #lo('NIWS', obj, niws)
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
        #lo('RLLY', obj, builder)

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

        with logging(niws):
            return niws

        # create antipathy between obj and its enemies
        """ TODO
        for node in self.nodes(HasAntipathyTo(obj, ignore=builder)):
            self.add_mutual_antipathy(obj, node)
        """

        # TODO Something needs to call an Agent's 'build' codelets
        # Put an .on-build() in this class and call it? FARGModel
        # can override it.

    """
    # TODO UT
    def _add_tag(self, taggee: Node, tag: Node, builder: Optional[Node]=None) \
    -> Node:
        # TODO Make the tag's builder the agent in the tag?
        tag = self.build(tag, builder=builder)
        self._tags_of[taggee].add(tag)
        return tag
    """

    def initial_state(self, obj: Node) -> AgentState:
        if isinstance(obj, Agent):
            if obj.born:
                return Born
            else:
                return Wake
        else:
            return Born

    def paint(
        self, cellref: CellRef, value: Value, painter: Optional[Actor]=None
    ) -> None:
        # TODO Raise exception if agent lacks sufficient activation to
        # paint on the cell.
        # TODO Raise exception if the Canvas doesn't exist.
        if painter and self.a(painter) < self.paint_threshold:
            raise NeedMoreSupportToPaint(actor=painter)
        cellref.paint(value)
        if painter:
            self._painted_by[(cellref, cellref.value)].add(painter)

    def painters_of(self, cellref: CellRef, value: Optional[Value]) \
    -> Set[Actor]:
        if value is not None:
            return self._painted_by[(cellref, value)]
        else:
            raise NotImplementedError('when value is None, need to return all painters that painted to cellref')

    # Activation / support

    def a(self, node: Node) -> float:
        '''Returns the activation of node, or 0.0 if node does not exist.'''
        return self.activation_g.a(node)

    def ae_weight(self, from_node: Node, to_node: Node) -> float:
        '''Returns the edge weight from from_node to to_node, or 0.0 if
        the edge does not exist.'''
        return self.activation_g.hop_weight(from_node, to_node)

    def set_a(self, node: Node, a: float) -> None:
        '''Sets the activation of node.'''
        # TODO clip for min_a and max_a
        self.activation_g.set_a(node, a)

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
        # TODO Rename 'es' to 'within'.
        wspred = as_wspred(pred)
        if es is None:
            es = list(self.wsd.keys())  # INEFFICIENT to create list but
                                        # seems needed because .wsd can change
                                        # while caller reads from the generator
                                        # we are about to return.
        return (e for e in as_iter(es) if wspred(self, e))

    def neighbors(self, node: Node) -> Set[Node]:
        return self.activation_g.neighbors(node)

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

    def has_node(self, node: Node) -> Optional[Node]:
#        for n in self.wsd:
#            lo('HNO', n, node, match_wo_none(n, node))
#        lo('HNO2', [
#            n for n in self.wsd
#                if match_wo_none(n, node)
#        ])
        return first(
            n for n in self.wsd
                if match_wo_none(n, node)
        )

    # Node info

    def builder_of(self, node: Node) -> Union[Node, None]:
        niws = self.get_niws(node)
        if niws:
            return niws.builder
        else:
            return None

    def behalf_of(self, node: Node) -> Sequence[Node]:
        try:
            return self.wsd[node].behalf_of
        except KeyError:
            return []

    def built_by(self, agent: Node) -> Sequence[Node]:
        # TODO Optimize: this checks all Elems in the ws
        return [
            e for e in self.nodes()
                if self.builder_of(e) is agent
        ]

    # Walking the ActivationGraph

### Workspace predicates ###

CallableWSPred = Callable[[Workspace, Any], bool]

def as_wspred(o: WSPred) -> Callable[[Workspace, Any], bool]:
    '''Returns a predicate function that takes two arguments: a Workspace
    and an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return WSIsInstance(o)  # type: ignore[arg-type]  # mypy bug?
    elif isinstance(o, tuple):
        return combine_wspreds(*o)
    elif callable(o):
        if first_arg_is_ws(o):
            return o  # type: ignore[return-value]
        else:
            return IgnoreWSArg(o)
    elif o is None:
        return lambda ws, x: True
    else:
        return lambda ws, x: match_wo_none(x, o)

# TODO Make this into a method of Workspace.
def bind_ws(ws: Workspace, o: WSPred) -> Pred:
    '''Like as_wspred(), but returns a Pred that passes 'ws' as the
    first argument to 'pred' if 'pred' takes a Workspace as its first
    argument.'''
    if isinstance(o, HasBindWs):
        return o.bind_ws(ws)
    elif callable(o) and not isclass(o):
        if first_arg_is_ws(o):
            return WithBoundWS(ws, o)  # type: ignore[arg-type]
        else:
            return o
    elif isinstance(o, tuple):
        return combine_preds(*(bind_ws(ws, p) for p in o))
    else:
        return as_pred(o)

@dataclass(frozen=True)
class WSIsInstance(HasBindWs):
    cl: Type

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return isinstance(x, self.cl)

    # TODO UT
    def bind_ws(self, ws: Workspace) -> Pred:
        return IsInstance(self.cl)
    
@dataclass(frozen=True)
class WithBoundWS:
    ws: Workspace
    wspred: CallableWSPred

    def __call__(self, x: Any) -> bool:
        return self.wspred(self.ws, x)  # type: ignore  # mypy bugs?

@dataclass(frozen=True)
class IgnoreWSArg:
    pred: Pred

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return self.pred(x)  # type: ignore[call-arg, operator, misc]  # mypy bug

def combine_wspreds(*preds: WSPred) -> CallableWSPred:
    preds: List[WSPred] = [
        p for p in preds
            if p is not None and not isinstance(p, WSAlwaysTrue)
            # TODO WSAlwaysTrue?
    ]
    if not preds:
        return WSAlwaysTrue()
    elif len(preds) == 1:
        return as_wspred(preds[0])
    else:
        preds_to_or: List[WSPred] = []
        preds_to_and_first: List[WSPred] = []
        for p in preds:
            if isinstance(p, AndFirst):
                preds_to_and_first.append(p)
            else:
                preds_to_or.append(p)
        if not preds_to_or:
            return WSAndPreds(*preds_to_and_first)
        elif not preds_to_and_first:
            return WSOrPreds(*preds_to_or)
        else:
            return WSAndPreds(*preds_to_and_first, WSOrPreds(*preds_to_or))

def first_arg_is_ws(o: Callable) -> bool:
    #lo('FARG', hasattr(o, '__call__'), type(o))
    if isinstance(o, MethodType) or isinstance(o, FunctionType):
        arg1type = first(get_type_hints(o).values())
    elif hasattr(o, '__call__'):
        arg1type = first(get_type_hints(o.__call__).values()) # type: ignore[operator]  # mypy bug?
        #lo('ARG1TYPE', arg1type, get_type_hints(o.__call__)) # type: ignore
    else:
        arg1type = first(get_type_hints(o, globalns=globals()).values())
    try:
        return issubclass(arg1type, Workspace)
    except TypeError:
        return False

@dataclass(frozen=True)
class WSAlwaysTrue:

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return True

@dataclass(frozen=True)
class WSAndPreds:
    preds: Tuple[CallableWSPred, ...]

    def __init__(self, *preds: WSPred):
        object.__setattr__(self, 'preds', tuple(as_wspred(p) for p in preds))

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return all(p(ws, x) for p in self.preds)

@dataclass(frozen=True)
class WSOrPreds:
    preds: Tuple[CallableWSPred, ...]

    def __init__(self, *preds: WSPred):
        object.__setattr__(self, 'preds', tuple(as_wspred(p) for p in preds))

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return any(p(ws, x) for p in self.preds)

@dataclass(frozen=True)
class ExcludeExisting(AndFirst, HasBindWs, QArg):
    '''WSPred to exclude existing nodes.'''

    def __call__(self, ws: Workspace, x: Any) -> bool:
        return not ws.has_node(x)

    def bind_ws(self, ws: Workspace) -> Pred:
        return ExcludeExistingWS(ws)

    def get_pred(self, fm: FARGModel, sources: Sources) -> Pred:
        return self

@dataclass(frozen=True)
class ExcludeExistingWS(AndFirst):
    '''Pred to exclude nodes that exist in a specified Workspace.'''
    ws: Workspace

    def __call__(self, x: Any) -> bool:
        return not self.ws.has_node(x)

### The generic FARGModel ###

@dataclass
class FARGModel(Workspace):
    '''A generic FARG model.'''
    slipnet: Slipnet = field(default_factory=lambda: Slipnet.empty())

    sleepers: Dict[Agent, int] = field(default_factory=dict, init=False)
        # Agent: timestep at which to wake up
    codelets_just_run: List[CodeletRun] = field(
        default_factory=list, init=False
    )   # the codelets that were run in the current timestep
    agents_just_run: List[Agent] = field(default_factory=list, init=False)
        # the agents that were run in the current timestep
    alogs: ActivationLogs = field(default_factory=ActivationLogs)

    num_slipnet_iterations: InitVar[Optional[int]] = None

    def __post_init__(self, num_slipnet_iterations: Optional[int]=None):
        if num_slipnet_iterations is not None:
            self.slipnet.set_params(num_iterations=num_slipnet_iterations)
        super().__post_init__()

    # TODO Move this somewhere below
    def was_just_run(self, pred: Pred) -> bool:
        pred = as_pred(pred)
        return any(pred(cr.codelet) for cr in self.codelets_just_run)

    # TODO Move this somewhere below
    def plot_last_alog(self) -> None:
        # TODO LoD
        list(self.alogs.logs.values())[-1].plot()

    def run_agent(
        self,
        agent: Node,
        agent_state: Optional[AgentState]=None,
        num: int=1
    ) -> None:
        '''Fills in agent's Refs and runs agent. Has no effect if agent is not
        a node in the workspace.'''
        # TODO Reconsider: does it actually make sense to fill in an Agent's
        # Refs?
        if not isinstance(agent, Agent):
            raise AttributeError(f'run_agent: {agent} is not an Agent.')
        if not self.has_node(agent):
            return
        if agent_state is None:
            agent_state = self.agent_state(agent)
        #lo('RA', agent, agent_state, self.has_node(agent))
        self.agents_just_run.append(agent)
        #agent = agent.replace_refs(self, None)
        for i in range(num):
            with fmlogging(self, agent):
                try:
                    #self.run_codelet(getattr(agent, agent_state.name), agent)
                    #lo('CDS', agent.get_codelets(agent_state))
                    self.run_codelet(agent.get_codelets(agent_state), agent)
                except Fizzle as fiz:
                    #lo('RAFIZ', short(agent), short(fiz))
                    self.add_tag(agent, fiz, builder=agent)
                    self.set_state(agent, fiz.next_agent_state)
                agent_state = self.agent_state(agent)
        """
        sources = self.mk_sources(agent)
        codelet: Codelet
        for codelet in as_iter(getattr(agent, agent_state.name)):
            self._run_codelet_and_follow_ups(codelet, sources)
            sources = self.prepend_source(codelet, sources)
        """

    def run_detector(
        self,
        detector: Node
    ) -> Sources:
        if not isinstance(detector, Detector):
            raise AttributeError(f'run_detector: {detector} is not a Detector')
        if not self.has_node(detector):
            return
        #lo('DETECTOR', detector)
        sources = [detector, RunningAgent(detector)]
        try:
            kwargs = self.mk_func_args(detector.look, sources)
            codelets: CodeletResults = detector.look(**kwargs)
            # TODO rename 'codelets', 'codelet'
            for codelet in as_iter(codelets):
                if isinstance(codelet, dict):
                    sources = self.prepend_source(codelet, sources)
                else:
                    sources = self.run_codelet_and_follow_ups(codelet, sources)
        except Fizzle as fiz:
            pass  # If anything fizzles, quit with no error
        return sources

    def run_detectors(self):
        # TODO Choose Detectors by activation level
        for detector in self.nodes(Detector):
            self.run_detector(detector)
        
    def run_codelet(self, codelet: Codelets, agent: Optional[Agent]=None) \
    -> Sources:
        '''Fills in codelet's Refs and runs codelet.'''
        sources = self.mk_sources(agent)
        for c in as_iter(codelet):
            sources = self.run_codelet_and_follow_ups(c, sources)
        return sources

    def run_codelet_and_follow_ups(
        self, codelet: Codelet, sources: Sources
    ) -> Sources:
        '''Runs codelet, its follow-ups (including dictionaries), its
        sk if the codelet succeeds, and its fk if it fails.'''
        try:
            #lo('RCF', codelet, sources)
            codelet_results = self.run_one_codelet(codelet, sources)
        except Fizzle as fiz:
            fiz = replace(
                fiz,
                codelet=codelet,
                actor=self.look_up_by_name('running_agent', sources)
            )
            #lo('FIZ0', str(fiz))
            if fiz.fk:
                fk = fiz.fk.replace_refs(self, sources)
                #lo('FIZ', fiz.fk, sources)
                self.run_codelet_and_follow_ups(fk, sources)
            raise fiz
        #print('RAFC', codelet, '--', codelet_results)
        except FARGException:
            raise
        except Exception as exc:
            print()
            print(exc)
            print(codelet)
            print()
            pr(self)
            raise
        if hasattr(codelet, 'sk'):
            codelet_results = as_list(codelet_results)
            codelet_results.insert(0, codelet.sk)  # type: ignore[attr-defined]
        codelet_result: CodeletResult
        for codelet_result in as_iter(codelet_results):
            sources = self.prepend_source(codelet_result, sources)
            if isinstance(codelet_result, Codelet):
                sources = self.run_codelet_and_follow_ups(
                    codelet_result, sources
                )
        return sources

        # NEXT run_codelet_result()
            
    def run_one_codelet(
        self, codelet: Codelet, sources: Sources
    ) -> CodeletResults:
        '''This should be the only place in the entire program that
        calls codelet.run().'''
        codelet = codelet.replace_refs(self, sources)
        running_agent = self.look_up_by_name('running_agent', sources)
        try:
            kwargs = self.mk_func_args(codelet.run, sources)
            #lo('1CODELET', codelet.__class__.__name__, dict_str(kwargs, short))
            self.codelets_just_run.append(CodeletRun(
                codelet, kwargs, self.t, running_agent
            ))
            with logging(codelet, kwargs=kwargs):
                return codelet.run(**kwargs)
        except Fizzle as fiz:
            with logging(fiz, running_agent=running_agent):
                raise

    def agent_just_ran(self, agent: Agent) -> bool:
        '''Did agent just run, i.e. in the current timestep? Mostly useful for
        unit testing and debugging.'''
        return agent in self.agents_just_run

    def mk_sources(self, agent: Optional[Agent]=None) -> Sequence:
        if agent:
            return [agent, RunningAgent(agent)]
        else:
            return []

    def prepend_source(self, car: Any, sources: Sources) -> Sources:
        return as_list(car) + [s for s in as_iter(sources)]

    def replace_refs(self, o: N, sources: Sources) -> N:
        if isinstance(o, CanReplaceRefs):
            return o.replace_refs(self, sources)  # type: ignore[return-value]
        elif is_dataclass_instance(o):
            return replace_refs_in_dataclass_instance(o, self, sources)
        else:
            return o

    # TODO rm? Just call .look_up_by_name?
    '''
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
        if param_name == 'running_agent':
            return agent  # TODO What if agent is None?
        try:
            return getattr(codelet, param_name)
        except AttributeError:
            return None
        """
    '''

    def look_up_by_name(
        self,
        name: str,
        sources: Sources
    ) -> Any:
        if name == 'fm':
            return self
        if name == 'sources':
            return sources
        for source in as_iter(sources):
            if isinstance(source, dict):
                try:
                    result = source[name]
                except KeyError:
                    continue
            else:
                try:
                    result = getattr(source, name)
                except AttributeError:
                    continue
            if isinstance(result, Ref):
                name = result.name
                continue
            if result is not None:
                return result
        return None

    def look_up_by_type(
        self,
        typ: Type,
        sources: Sources
    ) -> Any:
        # TODO It would be nice if we returned some record of where we
        # found the value, if we find one.
        for v in self.typeable_sources_iterator(sources):
            if (
                v is not None
                and
                not isinstance(v, Ref)
                and
                is_type_instance(v, typ)
            ):
                return v
        return None

    def typeable_sources_iterator(self, sources: Sources) -> Iterable[Any]:
        for source in as_iter(sources):
            if isinstance(source, dict):
                yield from source.values()
            elif is_dataclass_instance(source):
                for fieldname in source.__dataclass_fields__:
                    yield getattr(source, fieldname, None)

    def try_to_fill_nones(
        self, node: Node, sources: Sources, running_agent: Optional[Agent]=None
    ) -> Node:
        '''HACK? Tries to fill in fields of an Agent 'node' that are None with
        values from 'sources', going first by name and then by type. Returns
        a new Node object, with None values filled in, or the original
        'node' if we could not fill in any fields. If 'node' is neither an
        Agent nor a dataclass instance, then we return the original 'node'.

        We need this function, or perhaps something much more sophisticated,
        so that when an Agent queries the slipnet for a delegate, the
        delegate comes back missing the arguments that tell where the delegate
        should operate. For example, when Want gets a Consume, the Consume's
        'source' comes back None. It should get set to Want.start_cell. This
        function fills in Consume.source. It would probably be better, though,
        if some general notion of orientation or lodestar guided the search,
        maybe even consulting the slipnet for how to fill in the missing
        fields.'''
        # IDEA Instead of passing running_agent, pass a 'lodestar'.
        # TODO rm running_agent, since it's never referenced within the function?
        if not isinstance(node, Agent):
            return node
        elif is_dataclass_instance(node):
            d: Dict[str, Hashable] = {}
            for fieldname, typ in self.fillable_fields(node):
                if getattr(node, fieldname) is None: # if need to fill
                    v = self.look_up_by_name(fieldname, sources)
                    if v is None or not is_type_instance(v, typ):
                        v = self.look_up_by_type(typ, sources)
                    if v is not None:
                        d[fieldname] = v
            if d:
                return replace(node, **d)
            else:
                return node
        else:
            return node

    def fillable_fields(self, node) -> Iterable[Tuple[str, Type]]:
        if is_dataclass_instance(node):
            for name, typ in get_type_hints(node.__class__).items():
                if name not in Agent.all_state_names:
                    yield name, make_nonoptional(typ)

    def args_for(self, node: HasArgs, situation: Node, sources: Sources) \
    -> Dict[str, Any]:
        # Get needed arg names from node.
        # Look at concentric neighbors of situation for nodes that have
        # values.
        # Extract those values and put them into the result dict.
        result: Dict[str, Any] = {}
        for ref in node.need_args():
            v = self.find_value_in_situation(ref.name, situation)
            if v is None:
                v = self.look_up_by_name(ref.name, sources)
            if v is None and ref.name == 'agent': # HACK
                v = self.look_up_by_name('running_agent', sources)
            if v is not None:
                result[ref.name] = v
        return result

    def find_value_in_situation(self, name: str, situation: Node) -> Value:
        for node in self.activation_g.concentric_walk(situation, 3):
            if hasattr(node, name):
                return getattr(node, name)
        return None

    def try_to_fill_refs(
        self,
        node: Node,
        situation: Node,
        sources: Sources
    ) -> Node:
        '''Returns a node with 'node's Refs and Nones filled in, to the extent
        possible, from values in 'situation', i.e. within 2 hops in 
        activation_g.'''
        if not isinstance(node, HasArgs):
            return node
        else:
            d = self.args_for(node, situation, sources)
            #lo('T2FR', dict_str(d))
            return self.replace_refs(node, d)
            """
            if d:
                return replace(node, **d)
            else:
                return node
            """

    # TODO rm; replaced by QInput.get_items()
    """
    def qinput_items(self, qarg: QInput, sources: Sources) -> Nodes:
        sources = self.prepend_source(qarg, sources)
        kwargs = dict(
            (param_name,
             self.look_up_by_name(param_name, sources))
                for param_name in inspect.signature(qarg.get_items).parameters
        )
        return qarg.get_items(**kwargs)
    """

    def mk_func_args(self, func: Callable, sources: Sources) -> Dict[str, Any]:
        '''Returns a dictionary that provides a value for every parameter
        of func, filled in from sources. If func is a method on an object,
        the sources checked include that object (searched first).'''
        try:
            obj = func.__self__  # type: ignore[attr-defined]
        except AttributeError:
            pass
        else:
            sources = self.prepend_source(obj, sources)
        d: Dict[str, Any] = {}
        type_hints = get_type_hints(func)
        #for param_name, annotation in get_type_hints(func).items():
        for param_name in inspect.signature(func).parameters:
            if param_name == 'return':
                continue  # disregard return type
            value = self.look_up_by_name(param_name, sources)
            annotation = type_hints.get(param_name, Any)
            #lo('MKFARGS', repr(param_name), value, annotation, sources)
            if not is_type_instance(value, annotation):
                # TODO Somehow raise multiple exceptions if more than one
                # argument is missing.
                raise MissingArgument(
                    func=func,
                    param_name=param_name,
                    value=value,
                    type_needed=annotation
                )
            d[param_name] = value
        #lo('MKFUNCARGS', func, short(d))
        return d
        """
        return dict(
            (param_name,
             self.look_up_by_name(param_name, sources))
                for param_name in inspect.signature(func).parameters
        )
        """

    def mk_slipnet_args(self, qargs: QArgs, sources: Sources) -> Dict[str, Any]:
        activations_in = {}
        pred = []
        k = 20
        num_get = 1
        for qarg in as_iter(qargs):
            if isinstance(qarg, QArg):
                activations_in.update(qarg.get_activations_in(self, sources))
                p = qarg.get_pred(self, sources)
                if p is not None:
                    pred.append(p)
                qk = qarg.get_k()
                if qk is not None:
                    k = qk
                qnum_get = qarg.get_num_get()
                if qnum_get is not None:
                    num_get = qnum_get
            else:
                activations_in[qarg] = 1.0
        return dict(
            activations_in=activations_in,
            pred=tuple(pred),
            k=k,
            num_get=num_get
        )

    def start_alog(self, label: Hashable) -> ActivationLog:
        return self.alogs.start_alog(self.t, label)

    def pulse_slipnet(
        self,
        activations_in: Dict[Node, float],
        pred: WSPred=None,
        k: int=20,      # max number of most active slipnodes to choose among
        num_get: int=1, # max number of slipnodes to return
        alog: Optional[ActivationLog]=None
    ) -> List[Node]:
        temp_slipnet = self.slipnet.augment(
            Graph.with_features(activations_in.keys())
        )
        with logging(
            log_pulse,
            activations_in=activations_in,
            pred=pred,
            k=k,
            num_get=num_get
        ):
            sd = temp_slipnet.dquery(activations_in=activations_in, alog=alog)
        pred: Pred = bind_ws(self, pred)
        nas = temp_slipnet.topna(sd, pred=pred, k=k)
        """
        cpred = as_pred(pred)  #DEBUG
        for node in sd:  #DEBUG
            lo(node, cpred(node)) #DEBUG
        """
        result = list(sample_without_replacement(
            [na.node for na in nas],
            k=num_get,
            weights=[na.a for na in nas]
        ))
        with logging(log_pulse, sd=sd, nas=nas, result=str(result)):
            return result

    def is_sleeping(self, node: Node) -> bool:
        return node in self.sleepers

    def sleep(self, agent: Agent, sleep_duration: int) -> None:
        #lo('SLEEP', short(agent), sleep_duration)
        if not self.has_node(agent):
            return
        old_wake_t = self.sleepers.get(agent, self.t)
        self.sleepers[agent] = max(
            old_wake_t + sleep_duration,
            self.t + sleep_duration
        )

    def wake_sleepers(self) -> None:
        for agent, wake_t in list(self.sleepers.items()):
            if self.t >= wake_t:
                del self.sleepers[agent]
                #print('WAKING', short(agent), wake_t, self.t)
                self.set_state(agent, Wake)

    def wake_sleeper(self, agent: Agent) -> None:
        try:
            del self.sleepers[agent]
        except KeyError:
            pass

    def set_state(self, nodes: Nodes, state: AgentState) -> None:
        '''Sets the state of nodes to state. No effect if a node does not
        exist.'''
        for node in as_iter(nodes):
            if isinstance(node, Agent):
                niws = self.get_niws(node)
                # TODO Log or throw exception if node does not exist; it could
                # be a bug.
                if niws:
                    if node.get_codelets(state):
                        niws.state = state
                        self.wake_sleeper(node)
                    else: # Agent has no codelets for 'state'
                        next_state = state.if_quiescent
                        if next_state:
                            self.set_state(node, next_state)
                        else:
                            niws.state = state

    def agent_state(self, agent: Optional[Agent]) -> AgentState:
        '''Returns the current state of 'agent', or Defunct if 'agent'
        does not exist.'''
        niws = self.get_niws(agent)
        if niws:
            if self.is_sleeping(agent):
                return Sleeping
            else:
                return niws.state
        else:
            return Nonexistent

    def do_timestep(
        self,
        ag: Optional[Agent]=None,
        num: int=1,  # ignored if caller passes 'until'
        until: Optional[int]=None
    ) -> None:
        if until is None:
            until = self.t + num
        while self.t < until:
            self.t += 1
            self.codelets_just_run.clear()
            self.agents_just_run.clear()
            self.wake_sleepers()
            self.run_detectors()
            if ag is None:
                agent = self.choose_agent_by_activation()
            else:
                agent = ag
            #print(f'\nAGENT {agent.__class__.__name__}  {self.agent_state(agent)}  t={self.t}')
            #pr(self)
            if agent:
                self.run_agent(agent)
            self.activation_g.propagate()
            #self.log_activations()

    def choose_agent_by_activation(self) -> Optional[Agent]:
        # TODO Include "not sleeping" in the predicate
        """
        agents: List[Agent] = [
            ag for ag in self.nodes(Agent) if not self.is_sleeping(ag)   # type: ignore[misc]
        ]
        """
        agents: List[Agent] = list(self.nodes(Agent.CanRun))  # type: ignore[arg-type]
        #print('AGENTS', short(agents))
        activations = [self.a(ag) for ag in agents]

        #print(f'CHOO t={self.t}')
        #pts(agents)
        #pts(activations)

        return first(sample_without_replacement(agents, weights=activations))

    # Tagging

    def add_tag(self, taggee: Node, tag: Node, builder: Optional[Node]=None) \
    -> Node:
        #lo('ADD_TAG', short(taggee), short(tag), short(builder))
        tag = self.build(tag, builder=builder)
        if not self.has_node(taggee):
            taggee = self.build(taggee, builder=builder)
        if isinstance(tag, Tag):
            def more_taggees(x: Node) -> Iterable[Node]:
                return tag.also_tag(self, x) # type: ignore[attr-defined]  # mypy bug?
            all_taggees = transitive_closure(more_taggees, taggee)
        else:
            all_taggees = {taggee}
        for n in all_taggees:
            if n != tag:
                self._tags_of[n].add(tag)
        return tag

    # TODO UT
    def tags_of(self, taggee: Node) -> Iterable[Node]:
        for tag in as_iter(self._tags_of.get(taggee, None)):
            yield tag

    def taggees_of(self, tag: Node) -> Iterable[Node]:
        # INEFFICIENT: loops through all nodes in ws! FARGModel should probably
        # have a _taggees_of member.
        for node in self.nodes(Exclude(tag)):
            if self.has_tag(node, tag):
                yield node

    # TODO UT
    def has_tag(self, node: Node, pred: Pred) -> Optional[Node]:
        pred = as_pred(pred)
        return first(tag for tag in self.tags_of(node) if pred(tag))

    # Display

    def __str__(self):
        return self.__class__.__name__

    __repr__ = __str__

    def l1str(self, niws: Union[NodeInWS, Node], indent=None) -> str:
        '''The one-line string for a workspace node, showing its activation.'''
        if indent is None:
            indent = '  '
        if not isinstance(niws, NodeInWS):
            niws = self.get_niws(niws)  # TODO if the node does not exist
        slee = '  sleeping' if self.is_sleeping(niws.node) else ''
        result = f'{indent}{self.a(niws.node): 7.3f}  {short(niws)}{slee}'
        return result

    def e1str(self, node1: Node, node2: Node, indent=None) -> str:
        '''The one-line string for the edge from node1 to node2. Does not
        show node1. Indented one level further than 'indent'.'''
        if indent is None:
            indent = '  '
        outgoing_weight = self.ae_weight(node1, node2)
        incoming_weight = self.ae_weight(node1, node2)
        if outgoing_weight != 0.0:
            if incoming_weight != 0.0:
                arrow = f'{outgoing_weight: 6.3f} <--> {incoming_weight: 6.3f}'
            else:
                arrow = f'{outgoing_weight: 6.3f}  -->       '
        else:
            arrow = f'       <--  {incoming_weight: 6.3f}'
        return f'{indent}  {arrow} {short(node2)}  a={self.a(node2):2.3f}'

    def pr(
        self,
        indent=None,
        file=None,
        edges=False,
        extra=False,  # extra stuff like t, sum_a, and seed
        seed=False,   # show seed?
        show_tags=True,
        key=None  # ignored
    ) -> None:
        '''Prints a subset of the workpace.'''
        if extra:
            print()
            print(f't={self.t}', file=file)
        count = 0
        for s, node in sorted(
            (self.l1str(node, indent), node)
                for node in self.nodes()
        ):
            count += 1
            print(s, file=file)
            if show_tags:
                tags = list(self.tags_of(node))
                if tags:
                    print(' ' * 17, ', '.join(short(t) for t in tags))
            if edges:
                for e in sorted(
                    self.e1str(node, neighbor)
                        for neighbor in self.neighbors(node)
                ):
                    print(' ', e, file=file)
        if seed:
            print(f'seed={self.seed}')

### Exceptions ###

class FARGException(Exception):
    pass

@dataclass(frozen=True)
class Fizzle(FARGException, Loggable):
    actor: Optional[Actor] = None
    codelet: Optional[Codelet] = None
    next_agent_state: ClassVar[AgentState] = Snag

    def __str__(self):
        return repr(self)

    def log(self, f: Indenting, **kwargs) -> None:
        cl = self.__class__.__name__
        print(f'FIZZLE {short(self)}  {dict_str(kwargs, short)}', file=f)
        
    def short(self) -> str:
        cl = self.__class__.__name__
        return cl

    @property
    def fk(self) -> Optional[Codelet]:
        '''Returns the codelet's failure continuation, if it has one.'''
        if self.codelet and hasattr(self.codelet, 'fk'):
            return self.codelet.fk  # type: ignore[attr-defined]
        else:
            return None

class HaltFARGModel(FARGException):
    pass

@dataclass(frozen=True)
class SolvedPuzzle(HaltFARGModel):
    pass  # TODO Store some info about who found the solution and what the
          # solution was.
        
@dataclass(frozen=True)
class NeedMoreSupportToPaint(Fizzle):
    pass
    #agent: Optional[Agent] = None

@dataclass(frozen=True)
class ValuesNotAvail(Fizzle):
    #container: Hashable  # Change this to a CellRef?
    cellref: Union[CellRef, None] = None
    avails: Tuple[Value, ...] = ()
        # These values were avail; indices match indices in seeker's request
    unavails: Tuple[Value, ...] = ()
        # These values were unavail; indices match indices in seeker's request

    def __str__(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.actor)}, {short(self.codelet)}, {self.cellref}, avails={self.avails}, unavails={self.unavails})'

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.cellref)}, {short(self.avails)}, {short(self.unavails)})'

@dataclass(frozen=True)
class NoResultFromSlipnet(Fizzle):
    qargs: QArgs = None
    next_agent_state: ClassVar[AgentState] = Failed

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.qargs)})'

@dataclass(frozen=True)
class MissingArgument(Fizzle):
    func: Optional[Callable] = None
    param_name: Optional[str] = None
    value: Any = None
    type_needed: Any = None  # type annotation

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.func)}, {repr(self.param_name)}, {short(self.value)}, {short(self.type_needed)})'

### Features

@dataclass(frozen=True)
class Desnag(Feature):
    x: Union[Fizzle, Type[Fizzle]]

    def short(self) -> str:
        cl = self.__class__.__name__
        return f'{cl}({short(self.x)})'

### At end of file to avoid circular imports ###

import Codelets as CodeletsModule
import QArgs as QArgsModule
