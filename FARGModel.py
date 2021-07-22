# FARGModel.py -- The class that holds a FARG model, and ancillary classes
# that are not specific to any one FARG model.

from pprint import pprint as pp
import inspect
from time import process_time
import csv

from dataclasses import dataclass, field, fields, replace, is_dataclass, InitVar
import dataclasses
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
from numbers import Number, Real
from math import exp
from abc import ABC, abstractmethod
from itertools import chain
from copy import copy
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from collections import Counter, defaultdict
from io import StringIO
from inspect import isclass
import inspect
from random import choice

import networkx as nx
import matplotlib.pyplot as plt
#import netgraph

from FMTypes import Elem, Elems, Value, Addr, FMPred
from Slipnet import Slipnet, empty_slipnet, Before, After
from FMGraphs import ActivationGraph
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement, clip, reseed, default_field_value, d_subset, \
    fields_for, filter_none, force_setattr, PushAttr


# Global functions

def as_fmpred(o: FMPred) -> Callable:
    '''Returns a predicate function that takes two arguments: a FARGModel and
    an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return lambda fm, x: isinstance(x, o)
    elif isinstance(o, tuple):
        preds = tuple(as_fmpred(p) for p in o)
        return lambda fm, x: any(p(fm, x) for p in preds)
    elif callable(o):
        if first_arg_is_fargmodel(o):
            return o
        else:
            return lambda fm, x: o(x)
    elif o is None:
        return lambda fm, x: True
    else:
        return lambda fm, x: match_wo_none(x, o)
#        raise ValueError(
#            f'as_pred: {repr(o)} must be a class, a callable, or None.'
#        )

def first_arg_is_fargmodel(o: Callable) -> bool:
    p0 = first(inspect.signature(o).parameters.values())
    #print('FIRARG', o, p0, p0.annotation, type(p0.annotation))
    try:
        # TODO What if annotation is a string?
        return issubclass(p0.annotation, FARGModel)
    except TypeError:
        return False

def match_wo_none(other, obj_template) -> bool:
    '''Does obj_template == other if we ignore any fields in obj_template
    with a value of None? If other is an object of a subclass of obj_template's
    class, that also counts as a match.'''
    if not isinstance(other, obj_template.__class__):
        return False
    if not is_dataclass(obj_template) or not is_dataclass(other):
        return obj_template == other
    other_d = dataclasses.asdict(other)
    return all(
        v is None or v == other_d.get(k, None)
            for k, v in dataclasses.asdict(obj_template).items()
    )

def has_avail_value(
    elem: Union['CellRef', Elem, None],
    v: Value
) -> Union[bool, None, 'CellRef', Elem]:
    if elem is None:
        return False
    try:
        #return elem.has_avail_value(v)
        return any(elem.has_avail_value(v1) for v1 in as_iter(v))
    except AttributeError:
        if elem == v:
            return elem
        else:
            return False

# Element classes

@dataclass
class AgentState(ABC):
    succeeded: ClassVar[bool] = False

    @abstractmethod
    def can_go(self) -> bool:
        '''While in this AgentState, can the Agent go?'''
        pass

@dataclass
class Active(AgentState):

    def can_go(self):
        return True

@dataclass
class Succeeded(AgentState):
    succeeded: ClassVar[bool] = True

    def can_go(self):
        return False

@dataclass
class AwaitingDelegate(AgentState):
    delegate: 'Agents'

    def can_go(self):
        return False

    def add_delegate(self, new_delegate: 'Agents'):
        if not isinstance(self.delegate, list):
            self.delegate = [self.delegate]
        self.delegate += as_iter(new_delegate)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.delegate})'

'''
@dataclass
class AwaitingOK(AgentState):
'''

@dataclass
class MustCheckIfSucceeded(AgentState):
    prev_agentstate: AgentState
    delegate: 'Agent'

    def can_go(self):
        return True
    
    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.prev_agentstate}, {self.delegate})'

@dataclass(frozen=True)
class Agent(ABC): #(Elem):
    '''A workspace element that does things.'''

    #orientation: Dict[str, Hashable]

    @abstractmethod
    # TODO Maybe put desired args explicitly in go()'s signature
    def go(self, fm: 'FARGModel', contents, orientation):
        pass

    # TODO Default impl should check if Agent is Blocked
    def can_go(self, fm: 'FARGModel', **kwargs) -> bool:
        '''Is the Agent capable of executing its .go() method? If False,
        FARGModel.do_timestep() will give the Agent a zero probability of
        running. Default implementation: True unless Agent has a Blocked
        tag.'''
        #TODO Default implementation should also return False if Agent has
        # any Blanks.
        return not fm.is_blocked(self)

    def update_support(cls, fm: 'FARGModel'):
        '''The Agent should update the weights from itself to other Elems.
        Default implementation: do nothing.'''
        # TODO Return an iterable of updates so they're easy to log.
        pass

    def check_if_succeeded(self, fm: 'FARGModel', **kwargs):
        '''The Agent should look at the ws to see if it has succeeded, and
        update its AgentState accordingly. Default implementation: revert
        to previous AgentState if current AgentState is MustCheckIfSucceeded.'''
        st = fm.agent_state(self)
        if isinstance(st, MustCheckIfSucceeded):
            fm.set_agent_state(self, st.prev_agentstate)

Agents = Union[Agent, List[Agent], None]

@dataclass(frozen=True)
class Detector(ABC):

    @abstractmethod
    def look(self, fm: 'FARGModel'):
        pass

@dataclass
class ElemInWS:
    '''An element in the workspace.'''
    elem: Elem
    builder: Union[Agent, None]
        # .builder should not be used by Agents; it's strictly for debugging
        # and reporting on model behavor.
    behalf_of: List[Agent] = field(init=False, default_factory=list)
        # .behalf_of is all the Agents that want this Elem to exist, including
        # but not limited to its builder.
    tob: int   # time of birth (when Elem was added to the ws)
    # activation: float = 1.0
    # overrides for contents?
    # overrides for orientation?

    def add_behalf_of(self, agents):
        self.behalf_of += as_iter(agents)

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

# Exceptions

@dataclass(frozen=True)
class ValuesNotAvail(Exception):
    #container: Hashable  # Change this to a CellRef?
    cellref: Union['CellRef', None]
    avails: Tuple[Value]
    unavails: Tuple[Value]

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cellref}, avails={self.avails}, unavails={self.unavails})'
        

# Generic FARG model

@dataclass
class FARGModel:
    ws: Dict[Elem, ElemInWS] = field(default_factory=dict)
    t: int = 0
    slipnet: Slipnet = None
    slipnet_ctor: ClassVar[Callable[[], Slipnet]] = Slipnet
    seed: Union[int, None] = None

    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph, init=False
    )
    sleeping: Dict[Elem, int] = field(
        default_factory=dict, init=False
    )
    _agent_states: Dict[Elem, AgentState] = field(
        default_factory=dict, init=False
    )

    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    globals: Dict[str, Any] = field(default_factory=dict, init=False)

    force_slipnet_result: Hashable = field(default=None, init=False)

    # Initialization

    def __post_init__(self):
        self.seed = reseed(self.seed)
        if self.slipnet is None:
            self.slipnet = self.slipnet_ctor()
        self.fill_slipnet()

    def fill_slipnet(self):
        '''Should add nodes and edges to self.slipnet(). Default implementation
        does nothing.'''
        pass
        
    # Whole-codelet functions

    def build(self, *args, **kwargs) -> Elem:
        '''The arguments specify an Elem to build in the workspace. If such
        an Elem already exists, we don't build anything. Returns the built
        or found Elem.'''
        if not args:
            raise NotImplementedError('still need to provide object to .build')
        if isclass(args[0]):
            raise NotImplementedError(".build can't yet construct the object for you")
        # TODO with logging('BUILT', args, kwargs)
        obj = args[0]
        builder = kwargs.pop('builder', None)
        init_a = kwargs.pop('init_a', None)
        min_a = kwargs.pop('min_a', None) # only has effect on true build
        max_a = kwargs.pop('max_a', None) # only has effect on true build
        if obj is None:  # attempting to build None builds nothing
            return None
        eiws = self.get_eiws(obj)
        if eiws is None:  # the Elem is not there, so now we really build it
            self.ws[obj] = eiws = ElemInWS(obj, builder=builder, tob=self.t)
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
            self.activation_g.add_node(obj, a=init_a)
            # create antipathy between obj and its enemies
            for elem in self.elems(HasAntipathyTo(obj, ignore=builder)):
                self.add_mutual_antipathy(obj, elem)
            # call the obj's .on_build() method
            if isinstance(obj, Agent):
                self._agent_states[obj] = Active()
            try:
                obj.on_build(self, **kwargs)
            except AttributeError:
                pass
        else:  # the Elem is already there, so don't build a new one
            obj = eiws.elem
        if builder:
            eiws.add_behalf_of(builder)
            self.add_mutual_support(builder, obj)
        return obj

    def paint(self, cr: 'CellRef', v: Value):
        cr.paint(v)

    def run(self, agent: Agent, **kwargs):
        '''Calls the agent's .go() method regardless of whether its .can_go()
        would return False.'''
        # TODO Not if agent doesn't exist?
        # TODO Document force_slipnet_result
        with PushAttr(self, 'force_slipnet_result'):
            self.force_slipnet_result = kwargs.pop('force_slipnet_result', None)
            if isinstance(self.agent_state(agent), MustCheckIfSucceeded):
                agent.check_if_succeeded(self, **kwargs)
            else:
                agent.go(self, **kwargs)  # TODO Supply overrides from eiws?
            agent.update_support(self)

    def pulse_slipnet(
        self,
        activations_in: Dict[Hashable, float],
        type: Union[Type, None]=None,
        k: int=20,
        num_get: int=1,  # number of slipnodes to return
        filter: Union[Callable, None]=lambda x: True
    ) -> List[Hashable]:
        if self.force_slipnet_result is not None:
            return as_list(self.force_slipnet_result)
        q = self.slipnet.query(
            activations_in=activations_in, type=type, k=k, filter=filter
        )
        #print('PULSE')
        #pts(q)
        return list(sample_without_replacement(
            [nas.node for nas in q],
            k=num_get,
            weights=[nas.a for nas in q]
        ))

    # Activation

    def a(self, node: Hashable) -> float:
        '''Current activation level of node.'''
        return self.activation_g.a(node)

    def ae_weight(self, from_node: Hashable, to_node: Hashable) -> float:
        '''Activation edge weight. 0.0 if either node does not exist.'''
        try:
            return self.activation_g.edges[from_node, to_node]['weight']
        except KeyError:
            return 0.0

    def sum_a(self) -> float:
        return sum(self.a(elem) for elem in self.elems())

    def boost(self, node: Hashable, amt=None):
        self.activation_g.boost(node, amt=amt)

    def give_boost(self, from_elem: Elem, to_elem: Elems):
        self.boost(to_elem, amt=self.a(from_elem))

    def downboost(self, node: Hashable):
        # HACK to make a node quiet down for a bit after it's done something;
        # better to explicitly sleep for a few timesteps?
        self.activation_g.nodes[node]['a'] /= 2.0

    def deactivate(self, node: Hashable):
        self.activation_g.nodes[node]['a'] = 0.0

    def propagate_a(self, num: int=1):
        '''Propagate activation. num is the number of iterations--how many
        times we call the propagator, not how many spreading steps happen
        in each iteration.'''
        for _ in range(num):
            self.activation_g.propagate()

    # Support

    def add_mutual_support(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    set_mutual_support = add_mutual_support

    def set_support_edge(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)

    def add_mutual_antipathy(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_antipathy_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    # Querying

    def ws_query(
        self,
        pred: Union[Type, Callable, None]=None,
        min_a: Union[float, None]=None,
        max_n: int=1
    ) -> Iterable[Elem]:
        '''Returns generator of up to max_n nodes that match pred,
        chosen randomly, weighted by activation.'''
        elems = self.elems(pred)
        if min_a is not None:
            elems = (e for e in elems if self.a(e) >= min_a)
        elems = list(elems)
        activations = [self.a(e) for e in elems]
        yield from sample_without_replacement(
            elems, weights=activations, k=max_n
        )

    def is_tagged(self, elems, tagpred) -> bool:
        '''Are any of elems tagged with a tag matching tagpred?'''
        # TODO Optimize; this now searches the entire ws
        return any(
            self.is_tagging(tag, elem)
                for elem in as_iter(elems)
                    for tag in self.elems(pred=tagpred)
        )

    def is_tagging(self, tag: Elem, elem: Elem) -> bool:
        try:
            return tag.is_tagging(elem)
        except AttributeError:
            return elem in self.taggees_of(tag)

    def taggees_of(self, tag: Elem) -> Iterable[Elem]:
        try:
            yield tag.taggees_of()
        except AttributeError:
            yield tag.taggee
        except AttributeError:
            yield from tag.taggees
        except AttributeError:
            return

    def is_blocked(self, elems) -> bool:
        return self.is_tagged(elems, Blocked)

    def is_sleeping(self, elem: Elem):
        return elem in self.sleeping
            
    def degree(self, a: Elem) -> int:
        return len(self.neighbors(a))

    def neighbors(self, e: Elem) -> List[Elem]:
        g = self.activation_g
        return set(list(g.successors(e)) + list(g.predecessors(e)))


    # Ancillary functions, callable by codelets and Agents

    def elems(self, pred=None, es=None) -> Iterable[Elem]:
        '''Returns a generator for *all* matches of pred. Unlike .ws_query(),
        .elems() does not make a weighted choice.'''
        fmpred = as_fmpred(pred)
        if es is None:
            es = self.ws.keys()
        return (e for e in as_iter(es) if fmpred(self, e))

    def get_eiws(self, obj) -> Union[ElemInWS, None]:
        '''Returns the ElemInWS from the workspace if it exists, otherwise
        None.'''
        return self.ws.get(obj, None)

    # TODO UT
    def override(self, elem: Elem, **kwargs):
        '''Replaces elem with an elem containing new values, taken from
        kwargs.'''
        # MAJOR TODO We really should store the overrides in the ElemInWS,
        # and require that all code dereference an Elem to get the overridden
        # version. Tracking stale references is just too hard. If all
        # Agent actions are done inside Codelet objects, called by fm,
        # this shouldn't be too hard. The model itself should figure out
        # how to deal with messy situations that result from overrides.
        # TODO This should be done as a paint operation.
        # HACK This assumes that Elem is a dataclass.
        if not kwargs:
            return
        old_eiws = self.get_eiws(elem)
        if not old_eiws:
            return
        new_elem = replace(elem, **kwargs)
        if new_elem in self.ws:
            raise NotImplementedError
        else:
            self.ws[new_elem] = new_eiws = replace(old_eiws, elem=new_elem)
            self.activation_g.add_node(new_elem, a=self.a(elem))
            for u, v, d in chain(
                self.activation_g.in_edges(elem, data=True),
            ):
                self.activation_g.add_edge(u, new_elem, **d)
            for u, v, d in chain(
                self.activation_g.out_edges(elem, data=True)
            ):
                self.activation_g.add_edge(new_elem, v, **d)
            self.set_agent_state(new_elem, self.agent_state(elem))
            self.remove_elem(elem)

    def remove_elem(self, elem):
        # TODO Must remove all references to elem
        for d in [self.ws, self._agent_states, self.sleeping]:
            try:
                del d[elem]
            except KeyError:
                pass
        self.activation_g.remove_node(elem)

    def builder_of(self, elem: Elem) -> Union[Elem, None]:
        try:
            eiws = self.ws[elem]
        except KeyError:
            return None
        return eiws.builder

    def built_by(self, agent: Agent) -> Collection[Elem]:
        # TODO Optimize: this checks all Elems in the ws
        return [
            e for e in self.elems()
                if self.builder_of(e) is agent
        ]

    def behalf_of(self, elem: Elem) -> List[Elem]:
        try:
            return self.ws[elem].behalf_of
        except KeyError:
            return []

    def has_antipathy_to(self, a: Hashable, b: Hashable) -> bool:
        try:
            return a.has_antipathy_to(b)
        except AttributeError:
            return False

    def sleep(self, elem: Elem, num_timesteps=2):
        if elem in self.sleeping:
            self.sleeping[elem] += num_timesteps
        else:
            # TODO only if elem is in the ws
            self.sleeping[elem] = self.t + num_timesteps

    def can_go(self, agent: Agent) -> bool:
        if not isinstance(agent, Agent):
            return False
        if self.is_sleeping(agent):
            return False
        if not self._agent_states[agent].can_go():
            return False
        if self.is_tagged(agent, Blocked):
            return False
        return agent.can_go(self)

    def ok_to_paint(self, painter: Agent, cellref: 'CellRef') -> bool:
        # TODO Check that the painter beats its competition?
        # TODO Get threshold information from cellref?
        return (
            not cellref.has_value()
            and
            self.a(painter) >= 1.0
        )

    def agent_state(self, agent: Agent):
        return self._agent_states.get(agent, None)

    def set_agent_state(self, agent: Agent, agent_state: AgentState):
        self._agent_states[agent] = agent_state

    def awaiting_delegate(self, agent: Agent, delegate: Agent):
        '''Marks that agent is waiting for delegate to succeed.'''
        # TODO UT for multiple delegates
        st = self.agent_state(agent)
        if isinstance(st, AwaitingDelegate):
            st.add_delegate(delegate)
        else:
            self.set_agent_state(agent, AwaitingDelegate(delegate))

    def succeeded(self, agent: Agent):
        self._agent_states[agent] = Succeeded()
        for a in self.behalf_of(agent):
            if not isinstance(a, Agent):
                continue
            if not self.has_succeeded(a):
                self.must_check_if_succeeded(a, agent)

    def has_succeeded(self, agent: Agent) -> bool:
        try:
            return self.agent_state(agent).succeeded
        except AttributeError:  # if no such agent
            return False

    def must_check_if_succeeded(
        self, agent: Agent, delegate: Union[Agent, None]=None
    ):
        prev_agentstate = self._agent_states[agent]
        self._agent_states[agent] = \
            MustCheckIfSucceeded(prev_agentstate, delegate)
        self.give_boost(delegate, agent)
        self.unsleep(agent)  # TODO UT

    # Timestep functions

    def do_timestep(
        self, ag: Union[Agent, None]=None, num: int=1, until=None
    ):
        '''act: whether to force agents to .act even if the current timestep
        isn't designated for acting.
        until: last timestep; overrides num.'''
        if until is None:
            until = self.t + num
        while self.t < until:
            self.t += 1
            self.remove_sleepers()
            self.run_detectors()
            if ag is None:
                agent = self.choose_agent_by_activation(self.can_go)
            else:
                agent = ag
            if agent:
                #print('AGENT', agent)
                self.run(agent)
            #self.activation_g.decay()
            self.activation_g.propagate()
            #self.activation_g.pr_flows()  #DIAG
            self.log_activations()
            #print(self) #DIAG
                #agent.go(self)

    def log_activations(self):
        mode = 'w' if self.t == 1 else 'a'
        with open(self.globals.get('alog', 'a.csv'), mode=mode, newline='') \
        as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
            for node in self.nodes_to_log():
                writer.writerow([self.t, node, self.a(node)])

    def nodes_to_log(self) -> Iterable[Hashable]:
        return self.elems(self.globals.get('logpred', None))

    def run_detectors(self):
        for detector in list(self.elems(Detector)):
            #print('look:', detector)  #DIAG
            detector.look(self)

    def choose_agent_by_activation(self, pred: Callable):
        # TODO OAOO .search_ws
        agents = list(self.ws_query(pred))
        # GLOBAL constant in next line
        activations = [self.a(agent) ** 2.0 for agent in agents]
        return first(sample_without_replacement(agents, weights=activations))

    def remove_sleepers(self):
        for waking in [
            elem for (elem, t) in self.sleeping.items() if t <= self.t
        ]:
            del self.sleeping[waking]

    def unsleep(self, agent: Agent):
        '''Removes agent from the sleeping list. Does not boost agent's
        activation or otherwise notify agent.'''
        try:
            del self.sleeping[agent]
        except KeyError:
            pass

    # Debugging and reporting

    def the(self, pred, es=None) -> Union[Elem, None]:
        '''Returns the first element from .elems(), or None if there isn't
        one.'''
        return first(self.elems(pred=pred, es=es))

    def __len__(self):
        '''Returns number of Elems in the ws.'''
        return len(self.ws)

    def __repr__(self):
        cl = self.__class__.__name__
        return f'<{cl} object, t={self.t}>'

    def __str__(self):
        result = StringIO()
        #print(f't={self.t}  sum_a={self.sum_a():2.3f}', file=result)
        #print(self.elines(self.elems(), tofile=result))
        self.pr(tofile=result, extra=True, seed=True)
        return result.getvalue()

    def is_mutual_support(self, a: Elem, b: Elem) -> bool:
        return (
            self.ae_weight(a, b) > 0.0
            and
            self.ae_weight(b, a) > 0.0
        )

    def l1str(self, eiws: Union[ElemInWS, Elem], indent=None) -> str:
        '''The one-line string for a ws elem, showing its activation.'''
        if indent is None:
            indent = '  '
        if not isinstance(eiws, ElemInWS):
            eiws = self.ws[eiws]  # TODO if the elem does not exist
        result = f'{indent}{self.a(eiws.elem): 7.3f}  {eiws} deg={self.degree(eiws.elem)}'
        if isinstance(eiws.elem, Agent):
            if self.is_blocked(eiws.elem):
                bl = '  Blocked+'
            else:
                bl = ''
            result += f'  {bl}{self.agent_state(eiws.elem)}'
        return result

    def e1str(self, node1: Elem, node2: Elem, indent=None) -> str:
        '''The one-line string for the edge from node1 to node2. Does not
        show node1. Indented one level further than 'indent'.'''
        if indent is None:
            indent = '  '
        outgoing_weight = self.ae_weight(node1, node2)
        incoming_weight = self.ae_weight(node2, node1)
        if outgoing_weight != 0.0:
            if incoming_weight != 0.0:
                arrow = f'{outgoing_weight: 6.3f} <--> {incoming_weight: 6.3f}'
            else:
                arrow = f'{outgoing_weight: 6.3f}  -->       '
        else:
            arrow = f'       <--  {incoming_weight: 6.3f}'
        #return f'{indent}  {weight: 7.3f} --> {node2}  a={self.a(node2):2.3f}'
        return f'{indent}  {arrow} {node2}  a={self.a(node2):2.3f}'

    def pr(
        self,
        pred: Union[Type, Callable, None]=None,
        es=None,  # Elem, Elems, or None for all Elems
        tofile=None,
        indent=None,
        edges=False,
        extra=False,  # extra stuff like t, sum_a, and seed
        seed=False,   # show seed?
        **kwargs
    ):
        '''Prints a subset of the workspace.'''
        if extra:
            print(f't={self.t}  elems={len(self.ws)}  sum_a={self.sum_a():2.3f}', file=tofile)
        count = 0
        for s, elem in sorted(
            (self.l1str(elem, indent), elem)
                for elem in self.elems(pred=pred, es=es)
        ):
            count += 1
            print(s, file=tofile)
            if edges:
                for e in sorted(
                    self.e1str(elem, neighbor)
                        for neighbor in self.neighbors(elem)
                ):
                    print(' ', e, file=tofile)
        if pred:
            print(f'n={count}', file=tofile)
        if seed:
            print(f'seed={self.seed}')

    def pr_flows(self):
        print(f'FLOWS t={self.t}')
        self.activation_g.pr_flows()


@dataclass(frozen=True)
class HasAntipathyTo:
    elem: Elem
    ignore: Union[Elem, None]=None

    def __call__(self, fm: FARGModel, elem) -> bool:
        if elem == self.ignore:
            return False
        else:
            return fm.has_antipathy_to(self.elem, elem)

@dataclass(frozen=True)
class StateDelta:
    '''A change from one state to another.'''
    before: Any    # What got replaced
    after: Any     # What replaced it
    how: Union[Any, None]   # Some clue about how it happened

    def seq_str(self):
        '''How to display this inside SeqState.__str__.'''
        return str(self)

@dataclass(frozen=True)
class SeqState:  # TODO Inherit from Value?
    avails: Union[Tuple[int], None] = None
    last_move: Union[StateDelta, None] = None

    # TODO Put this into a WithAvails mix-in.
    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        remaining_avails = [] if self.avails is None else list(self.avails)
        taken_avails = []
        missing_avails = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                taken_avails.append(None)
                missing_avails.append(v)
            else:
                taken_avails.append(v)
                missing_avails.append(None)
        if any(t is None for t in taken_avails):
            raise ValuesNotAvail(
                None,
                tuple(taken_avails),
                tuple(missing_avails)
            )
        return (taken_avails, remaining_avails)

    def has_avail_value(self, v):
        return v in self.avails

    def last_move_str(self):
        try:
            return self.last_move.seq_str()
        except AttributeError:
            return str(self.last_move)

    def __str__(self):
        if self.avails is None:
            avails_str = str(self.avails)
        else:
            avails_str = f"({' '.join(str(a) for a in self.avails)})"
        if self.last_move is None:
            return avails_str
        else:
            return f'{self.last_move_str()} {avails_str}'

@dataclass(eq=False)
class Canvas(ABC):
    # TODO get an iterator of all CellRefs, search for a value

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def __setitem__(self, addr: Addr, v: Value):
        pass

@dataclass(eq=False)
class SeqCanvas(Canvas):
    states: List[SeqState] = field(default_factory=list)

    min_a: ClassVar[float] = 1.0

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        #print('SEQCGET', addr, len(self.states))
        if addr < len(self.states):
            return self.states[addr]
        else:
            return None

    def __setitem__(self, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        # TODO Accept a builder argument?
        while len(self.states) <= addr:
            self.states.append(None)
        self.states[addr] = v

    def last_nonblank_cellref(self) -> 'CellRef':
        # TODO Actually check for 'blank' SeqStates (which will require
        # implementing that concept)
        return CellRef(self, len(self.states) - 1)

    # TODO UT
    def cellrefs_forward(self, addr: Addr) -> Iterable['CellRef']:
        for i in range(addr, len(self.states)):
            yield CellRef(self, i)

    def __str__(self):
        return f"SeqCanvas({'; '.join(str(st) for st in self.states)})"

@dataclass(frozen=True)
class CellRef:
    canvas: Union[Canvas, None] = None
    addr: Union[Addr, None] = None

    def paint(self, v: Value):
        self.canvas[self.addr] = v

    @property
    def value(self) -> Union[Value, None]:
        return self.canvas[self.addr]

    def has_value(self) -> bool:
        return self.value is not None

    # TODO UT
    def cellrefs_forward(self) -> Iterable['CellRef']:
        yield from self.canvas.cellrefs_forward(self.addr)

    @property
    def avails(self) -> Sequence[Value]:
        cell = self.value
        if cell is None:
            return []
        else:
            return cell.avails

    def take_avails(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValuesNotAvail.'''
        # TODO Require a determinate Collection, not just an Iterable (since
        # we might fail and need to put 'values' into an exception).
        cell = self.canvas[self.addr]
        if cell is None:
            raise ValuesNotAvail(self, values)
        return cell.take_avails(values)

    def has_avail_value(self, v):
        return has_avail_value(self.canvas[self.addr], v)

    def last_nonblank_cellref(self) -> 'CellRef':
        return self.canvas.last_nonblank_cellref()

    def next_cellref(self):
        '''Returns CellRef for the next cell in the canvas.'''
        # HACK TODO This is wrong: it's limited to SeqCanvas.
        return replace(self, addr=self.addr+1)

    def __str__(self):
        cl = self.__class__.__name__
        return f'canvas[{self.addr}]'

@dataclass(frozen=True)
class RaiseException:
    exc_class: Type

    def __call__(self, fm: FARGModel, *args, **kwargs):
        raise self.exc_class(*args, **kwargs)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.exc_class.__name__})'

@dataclass(frozen=True)
class AvailDetector(Detector):
    target: Union[Value, Collection[Value]]
    filter: FMPred  # Detector looks only at elements that match this predicate
    action: Callable

    # TODO Don't look at things previously found
    def look(self, fm):
        # TODO Should has_avail_value be included in pred?
        found = set(fm.ws_query(self.filter))
        # TODO log found
        for elem in found:
            if has_avail_value(elem, self.target):
                # TODO action should be a codelet
                self.action(fm, elem)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.target}, {self.action})'
    
@dataclass(frozen=True)
class LitPainter(Agent):
    cellref: CellRef = None
    value: Value = None

    def on_build(self, fm, **kwargs):
        fm.build(self.cellref, builder=self)

    def go(self, fm, **kwargs):
        fm.paint(self.cellref, self.value)
        fm.succeeded(self)

    def can_go(self, fm, **kwargs):
        return fm.ok_to_paint(self, self.cellref)

    def has_antipathy_to(self, other) -> bool:
        return (
            self is not other
            and
            isinstance(other, LitPainter)
            and
            self.cellref == other.cellref
        )

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cellref}, {self.value})'

@dataclass(frozen=True)
class Blocked(Agent):
    taggee: Elem
    reason: Hashable

    def go(self, fm: FARGModel):
        #fm.build(GoIsDone(taggee=self))
        self.make_want_unavail(fm, self.reason)
        # TODO Alternatively, TakeFromAvails

        return

        # TODO The right way: by consulting the slipnet. (Even better:
        # an unhappiness Detector notices the Blocked and consults the slipnet.)
        [
            After(Removed(self))
            #Maybe get some more context from the ws, like the behalf_of
            #of the taggee and what's in the SeqCanvas.
        ]
        # Get one or two Agents from the slipnet.
        # Build them, orienting them to self and/or taggee.
        # Should get: Detector with follow-up of rerunning the Consume
        # on a different cell. And a TakeAvailsScout that will make a Consume
        # whose operands are avail now, with the same operator. Both of these
        # are really slippages of the original Consume: SlipWaitForAvail and
        # SlipToAvail.

        # Instead of simply building a copy of what we get from the slipnet,
        # we should build a Copycat agent to do the reorienting and slipping.

    # HACK Some other Agent should do this.
    def make_want_unavail(self, fm: FARGModel, vna: ValuesNotAvail):
        target = choice([a for a in vna.unavails if a is not None])
            # TODO Not right: need to cover 4+4; crude anyway
        fm.build(Want(
            target,
            vna.cellref,
            RemoveBlocked(self)
        ))

    def can_go(self, fm):
        return True

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee}, {self.reason})'

@dataclass(frozen=True)
class RemoveBlocked:
    blocked: Blocked

    def __call__(self, fm: FARGModel, found: Elem):
        taggee = self.blocked.taggee
        # HACK This assumes that the taggee is a Consume; should delegate
        # 'what to do when unblocked' to something else. Really, this is
        # a form of slippage and should be explicitly coded that way.
        assert isinstance(taggee, Consume)
        fm.override(taggee, source=found, dest=found.next_cellref())
        fm.remove_elem(self.blocked)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.blocked})'

@dataclass(frozen=True)
class Operator:
    '''Computes the result when Consume consumes operands.'''
    func: Callable
    name: str

    statedelta_ctor: ClassVar[Callable] = StateDelta

    def call(self, *operands) -> int:
        return self.func(*operands)

    def consume(self, source_state: 'SeqState', operands: Collection[Value]) \
    -> 'SeqState':
        '''Should try to consume operands from the avails in source_state,
        returning the resulting SeqState. For example, a 'plus' Operator
        consuming (4, 5) should return a SeqState with the 4 and 5 removed
        and replaced with a 9, and a StateDelta describing that. If any
        of the operands are not avail in source_state, then should raise
        ValuesNotAvail.'''
        taken_avails, remaining_avails = source_state.take_avails(operands)
        result = self.call(*taken_avails)
        new_avails = tuple(remaining_avails) + (result,)
        delta = self.statedelta_ctor(tuple(taken_avails), result, self)
        return SeqState(new_avails, delta)

    def __str__(self):
        return self.name

@dataclass(frozen=True)
class Consume(Agent):
    operator: Union[Operator, None] = None
    operands: Union[Tuple[Value], None] = None
    source: Union[CellRef, None] = None  # where to get operands
    dest: Union[CellRef, None] = None    # where to paint result

    def on_build(self, fm: FARGModel, **kwargs):
        fm.build(self.source, builder=self)
        fm.build(self.dest, builder=self)

    def go(self, fm, *args, **kwargs):
        try:
            s1 = self.operator.consume(self.source, self.operands)
        except ValuesNotAvail as exc:
            # tag self as Blocked
            reason = replace(exc, cellref=self.source)
            fm.build(Blocked(taggee=self, reason=reason), builder=self)
            return
        lp = fm.build(LitPainter(self.dest, s1), builder=self)
        fm.awaiting_delegate(self, lp)
        #TODO Shut down completely when and if delegate succeeds

    def has_antipathy_to(self, other) -> bool:
        return (
            self is not other
            and
            isinstance(other, Consume)
            and
            self.source == other.source
        )

    def check_if_succeeded(self, fm, **kwargs):
        st = fm.agent_state(self)
        if isinstance(st, MustCheckIfSucceeded):
            if fm.has_succeeded(st.delegate):
                fm.succeeded(self)
            else:
                fm.set_agent_state(st.prev_agentstate)

    def __str__(self):
        cl = self.__class__.__name__
        os = ' '.join(str(o) for o in [self.operator] + as_list(self.operands))
        # TODO Include canvas and addr
        xs = [os]
        if self.source is not None and self.dest is not None:
            xs.append(f'{self.source}->{self.dest}')
        else:
            if self.source is not None:
                xs.append(f'source={self.source}')
            if self.dest is not None:
                xs.append(f'dest={self.dest}')
        return f"{cl}({', '.join(xs)})"

    def features(self) -> Iterable[Hashable]:
        for operand in self.operands:
            yield operand
            yield Before(operand)
        yield self.operator
        result = self.operator.call(*self.operands)
        yield result
        yield After(result)

@dataclass(frozen=True)
class Want(Agent):
    '''Agent that guides the creation of a chain of Consumes to produce a
    wanted avail value within a SeqCanvas.'''
    #TODO Rename Want to something that suggests more narrowly what it tries
    #to build.
    target: Value = None
    startcell: CellRef = None
    sk: Callable = None   # success continuation

    max_a: ClassVar[float] = 4.0

    def on_build(self, fm: FARGModel, **kwargs):
        fm.build(self.startcell, builder=self)

    def go(self, fm: FARGModel, **kwargs):
        # TODO Don't build these if they're already built
        fm.build(
            AvailDetector(self.target, filter=CellRef, action=self.sk),
            builder=self
        )
        '''
        fm.build(
            GettingCloser.Tagger(target=self.target),
            builder=self
        )
        '''
        self.consult_slipnet(fm, **kwargs)
        fm.sleep(self, num_timesteps=5)

    def consult_slipnet(self, fm: FARGModel, **kwargs):
        source = self.startcell.last_nonblank_cellref()
        activations_in = {}
        self.update_activations_in(fm, source, activations_in)

        exclude = Exclude(fm.neighbors(self))
        try:
            # HACK
            agents = kwargs['force_slipnet_result']
        except KeyError:
            agents = fm.pulse_slipnet(
                # GLOBAL constants in next line
                activations_in, k=20, type=Agent, num_get=1, filter=exclude
            )
            #print('XXX', activations_in)
            #pts(agents)
            #pr(fm.slipnet)
            # TODO We need a way to get an Agent from the slipnet that means
            # something like "Just add what you have."
            #print('WANT got from slipnet:', [str(a) for a in agents]) #DIAG
        # TODO Save the results of the pulse so we can inspect it later.
        dest = source.next_cellref()
        for agent in agents:
            if isinstance(agent, Consume):  #HACK
                agent = replace(agent, source=source, dest=dest)
            fm.build(agent, builder=self, init_a=0.1)

    def update_activations_in(
        self, fm: FARGModel, cr: CellRef, activations_in: Dict[Hashable, float]
    ) -> Dict[Hashable, float]:
        # TODO Should add to existing activation levels, not overwrite
        for avail in cr.avails:
            activations_in[Before(avail)] = 1.0
        activations_in[After(self.target)] = 1.0
        # TODO (in a subclass specific to arithmetic)
#        if all(avail > self.target for avail in avails):
#            activations_in[Increase()] = 10.0

    def check_if_succeeded(self, fm, **kwargs):
        # TODO Separate checking for success from following up
        for cellref in self.startcell.cellrefs_forward():
            if has_avail_value(cellref, self.target):
                fm.succeeded(self)
                # TODO What follows should be in FARGModel.succeeded()
                if self.sk:
                    self.sk(fm, cellref)
        else:
            st = fm.agent_state(self)
            # TODO What follows should be a method of AgentState
            if isinstance(st, MustCheckIfSucceeded):
                fm.set_agent_state(self, st.prev_agentstate)

    def __str__(self):
        cl = self.__class__.__name__
        # TODO Include canvas and addr
        return f'{cl}({self.target})'

# TODO UT
@dataclass(frozen=True)
class Exclude:
    '''A filter predicate: returns True iff its argument is not in .items,
    using a weaker condition than strict equality. This enables excluding
    Consume objects only on the basis of their non-null fields.'''
    items: Collection[Hashable]

    def __call__(self, x: Hashable) -> bool:
        if isinstance(x, Consume):  #HACK
            return not any(
                (isinstance(item, Consume)
                 and
                 item.operands == x.operands
                 and
                 item.operator == x.operator
                ) for item in self.items
            )
        else:
            return x not in self.items


"""
class Copycat(Agent):
    
    def go(self, fm, @@):
        '''
        What's missing? (Do we need to build the copy with blanks?)
        
        We are done when-- A copy is built? No, we might build competing
        copies. When our master is satisfied? No, we might have multiple
        masters.
        '''
"""
