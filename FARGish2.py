# FARGish2.py -- The generic classes and functions for making a FARG model

# Unlike in FARGish.py, this version does not represent sequences with a Cell
# for each step (containing multiple, competing Values). Instead, every
# distinct temporal sequence is represented by a separate object.


# Consume: provide arguments; get them from a source


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

import networkx as nx   # type: ignore[import]
import matplotlib.pyplot as plt   # type: ignore[import]
#import netgraph

from Slipnet import Slipnet, empty_slipnet
from FMTypes import FMPred, Value, Addr
from Propagator import Propagator, Delta
from util import is_iter, as_iter, as_list, pts, pl, pr, csep, ssep, \
    as_hashable, backslash, singleton, first, tupdict, as_dict, short, \
    sample_without_replacement, clip, reseed, default_field_value, d_subset, \
    fields_for, filter_none


# Types

class Elem(ABC):
    '''Anything that can go in the ws without being contained by something
    else; not a CellRef and not a Value (except for special CellRefs and
    Values that have been made to serve as Elems).'''

    '''
    @abstractmethod
    def search(self, fm: 'FARGModel', pred: Union[Type, Callable]) \
    -> Iterable[Union['Elem', 'CellRef']]:
        pass
    '''

Searchable1 = Union[Elem, 'CellRef']
Searchable = Union[Searchable1, Iterable[Searchable1]]

class Agent(Elem):

    @abstractmethod
    def go(self, fm: 'FARGModel', **overrides):
        '''The Agent should do its agenda tentatively, i.e. without painting
        on any real canvases.'''
        pass

    @abstractmethod
    def can_go(self, fm: 'FARGModel') -> bool:
        '''Is the Agent capable of executing its .go() method right now?
        Should return False if the Agent has a Blocked tag or it's already
        "gone" and is only waiting to .act().'''
        pass

    def act(self, fm: 'FARGModel', **overrides):
        '''The Agent should do agenda officially, including painting values on
        real canvases.'''
        pass

    def can_act(self, fm: 'FARGModel') -> bool:
        '''Is the Agent capable of executing its .act() method right now?'''
        return False

# Generic functions with defaults

def name_of(o: Any):
    try:
        return o.__name__
    except AttributeError:
        return short(o)

def is_real(x: Any) -> bool:
    try:
        return x.is_real()
    except AttributeError:
        return False

def has_avail_value(
    elem: Union['CellRef', Elem, None],
    v: Value
) -> Union[bool, None, 'CellRef', Elem]:
    if elem is None:
        return False
    elif isinstance(elem, CellRef):
        return any(elem.has_avail_value(v1) for v1 in as_iter(v))
    elif elem == v:
        return elem
    else:
        return False
        
        
    '''
    try:
        #return elem.has_avail_value(v)
        return any(elem.has_avail_value(v1) for v1 in as_iter(v))
    except AttributeError:
        if elem == v:
            return elem
        else:
            return False
    '''

def dig_attr(elem: Union[Elem, None], attr: str) -> Hashable:
    '''Returns value of attr within elem. If elem contains no attr,
    digs through elem.contents, the .contents of that, and so on, until
    finding an attribute named attr. Returns None if the search fails.'''
    if elem is None:
        return None
    try:
        return getattr(elem, attr)
    except AttributeError:
        return dig_attr(getattr(elem, 'contents', None), attr)

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

def as_fmpred(o: Union[Type, Tuple, Callable, None]) -> Callable:
    '''Returns a predicate function that takes two arguments: a FARGModel and
    an object.'''
    # TODO Document the many ways this thing constructs a function.
    if isclass(o):
        return lambda fm, x: isinstance(x, o)  # type: ignore[arg-type] # mypy bug?
    elif isinstance(o, tuple):
        preds = tuple(as_fmpred(p) for p in o)
        return lambda fm, x: any(p(fm, x) for p in preds)
    elif callable(o):
        if first_arg_is_fargmodel(o):
            return o
        else:
            return lambda fm, x: o(x)  # type: ignore[operator, misc] # mypy bug?
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

def search(
    fm: 'FARGModel',
    items: Searchable,
    pred: Union[Type, Callable]
) -> Iterable[Union['Elem', 'CellRef']]:
    print('SEARCH', items, pred)
    for item in as_iter(items):
        if hasattr(item, 'search'):
            yield from item.search(fm, pred)
        else:
            # TODO Figure out the correct response here
            print('NO-SEARCH', item)
            raise NotImplementedError

def search1(*args, **kwargs):
    # TODO Give higher probability based on activation
    return first(search(*args, **kwargs))

def source_cellref_of(fm: 'FARGModel', elem: Elem) -> Union['CellRef', None]:
    '''Returns the first .source attr found in elem, elem's builder, and elem's
    builder's builder, or None if not found.'''
    # TODO Should check that the .source is annotated as a CellRef.
    try:
        return elem.source
    except AttributeError:
        bu = fm.builder_of(elem)
        if bu is None:
            return None
        else:
            try:
                return bu.source
            except AttributeError:
                bu = fm.builder_of(elem)
                try:
                    return bu.source
                except AttributeError:
                    return None

# Exceptions

@dataclass(frozen=True)
class Halt(Exception):
    pass

# TODO rm (replaced by ValuesNotAvail)
@dataclass(frozen=True)
class ValueNotAvail(Exception):
    container: Any
    value: Any

    def try_to_fix(
        self, fm: 'FARGModel', behalf_of: 'Agent', builder=Union[Agent, None]
    ):
        # TODO The decision of how to go about fixing the problem should
        # result from a slipnet search rather than being hard-coded.
        #source = getattr(builder, 'source', None)
        source = source_cellref_of(fm, builder)
        if source is not None:
            mca = MustComeAfter(source)
        else:
            mca = None
        #print('VNAVAIL', self, builder, source)  #DIAG
        fm.build(
            Detector(
                self.value,
                MakeAgentSeq(tail=behalf_of),
                mca
            ),
            builder=builder
        )

@dataclass(frozen=True)
class ValuesNotAvail(Exception):
    container: Hashable
    avails: Tuple[Hashable]
    unavails: Tuple[Hashable]

    def try_to_fix(
        self, fm: 'FARGModel', behalf_of: 'Agent', builder=Union[Agent, None]
    ):
        # TODO The decision of how to go about fixing the problem should
        # result from a slipnet search rather than being hard-coded.
        #source = getattr(builder, 'source', None)
        source = source_cellref_of(fm, builder)
        if source is not None:
            mca = MustComeAfter(source)
        else:
            mca = None
        fm.build(
            Detector(
                #self.value,
                tuple(a for a in self.avails if a is not None),
                MakeAgentSeq(tail=behalf_of),
                mca
            ),
            builder=builder
        )
        if hasattr(behalf_of, 'rebuild_with_blanks'):
            behalf_of.rebuild_with_blanks(fm, self.avails, self.unavails)


@dataclass(frozen=True)
class MustComeAfter:
    cellref: 'CellRef'

    def __call__(self, x: Hashable) -> bool:
        if isinstance(x, CellRef):
            try:
                #print('MCA  ', x, self.cellref)  #DIAG
                return x.addr > self.cellref.addr
            except (AttributeError, TypeError):
                return True   # don't filter CellRef with indeterminate addr
        else:
            return True  # if not a CellRef, don't filter

# Graphs

class ActivationGraph(nx.DiGraph):

    def __init__(self, **kwargs):
        super().__init__()
        self.propagator = ActivationPropagator(**kwargs.get('aprop', {}))
#            **fields_for(ActivationPropagator, kwargs)
#        )

    def ns(self, node) -> List[str]:
        '''Returns list of neighbors represented as strings.'''
        return [gstr(neighbor) for neighbor in self.neighbors(node)]

    def add_node(self, node: Hashable, a=1.0):
        super().add_node(node, a=a)

    def add_edge(self, node1: Hashable, node2: Hashable, weight=1.0):
        if abs(weight) < 0.001:
            self.remove_edge(node1, node2)
        else:
            super().add_edge(node1, node2, weight=weight)
            
    def remove_edge(self, node1: Hashable, node2: Hashable):
        if self.has_edge(node1, node2):
            super().remove_edge(node1, node2)

    def a_dict(self) -> Dict[Elem, float]:
        '''Activations dictionary.'''
        return dict(
            (node, self.a(node)) for node in self.nodes
        )

    def a(self, node: Elem) -> float:
        try:
            return self.nodes[node]['a']
        except KeyError:
            return 0.0

    def propagate(self) -> Dict:  # TODO Update the graph
        d = self.propagator.propagate(self, self.a_dict())
        for node, a in d.items():
            self.nodes[node]['a'] = a
        return d

#    def pr_flows(self):
#        lines = [
#            f'{self.arrow(k):75s} {v:= 3.5f}'
#                for k, v in self.propagator.flows.items()
#        ]
#        pr(lines)
#
#    @classmethod
#    def arrow(cls, k) -> str:
#        fromnode = k[0]
#        tonode = k[1]
#        return f'{fromnode} -> {tonode}'
    def pr_flows(self):
        pr(self.propagator.flows)

    def draw(self):
        global pos, node_labels, plot_instance
        pos = nx.layout.spring_layout(self)
        node_labels = dict((node, gstr(node)) for node in self.nodes)
#        nx.draw(self, pos, with_labels=True, labels=node_labels)
#        nx.draw_networkx_edge_labels(
#            self, pos, edge_labels=nx.get_edge_attributes(self, 'weight')
#        )
        plot_instance = netgraph.InteractiveGraph(
            self,
            node_positions=pos,
            node_labels=node_labels,
            node_label_font_size=6
        )
        #plot_instance = netgraph.InteractiveGraph(self)

    def pr(self):
        '''Prints alphabetized list of nodes with activations.'''
        for s in sorted(self.nodestr(n) for n in self.nodes):
            print(s)

    def nodestr(self, node):
        return f"{node!s:50s} {self.nodes[node]['a']:2.5f}"

    def decay(self):
        for node in self.nodes:
            self.nodes[node]['a'] *= 0.95

    def boost(self, nodes):  # TODO type annotation
        for node in as_iter(nodes):
            a = self.nodes[node]['a']
            #incr = max(min(1.0, a), 2.0)
            incr = clip(0.5, 1.0, a)
            self.nodes[node]['a'] += incr
            print('BOOST', node, 'TO', self.a(node)) #DIAG

@dataclass
class ActivationPropagator(Propagator):
    '''This works like StdGraph.StdSupportPropagator, except that activation
    rather than support flows between nodes, and every edge (not just edges
    with certain labels) is taken as a path for activation to flow.'''
    noise: float = 0.0  #0.005
    max_total: float = 40.0
    positive_feedback_rate: float = 0.1 #0.5  # higher -> initial features matter more
    sigmoid_p: float = 0.9 #1.05  # higher -> sharper distinctions, more salience
    num_iterations: int = 10
    alpha: float = 0.98
    inflation_constant: float = 5.0  # 2.0 is minimum  # TODO rm?

#    def min_value(self, g, node):
#        return 0.0
    def clip_a(self, g, node, a):
        lb = getattr(node, 'min_a', 0.0)
        ub = getattr(node, 'max_a', 10.0)
        return clip(lb, ub, a)

    def make_deltas(self, g, old_d):
        return chain.from_iterable(
            self.deltas_from(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_from(self, g, old_d, node) -> Iterable[Delta]:
        '''Deltas specify changes to node's neighbors, not to node.'''
        node_a = old_d.get(node, 0.0)  # node's current activation
        outgoing_deltas = []
        for neighbor, edge_d in g.adj[node].items():
            weight = edge_d.get('weight', 1.0)
            neighbor_a = old_d.get(node, 0.0)  # neighbor's current activation
            outgoing_deltas.append(
                Delta(
                    neighbor,
                    weight, # + self.positive_feedback_rate * neighbor_a,
                    node
                )
            )
        return self.rescale_deltas(outgoing_deltas, node_a)

    def rescale_deltas(self, deltas, node_a) -> List[Delta]:
        if not deltas:
            return deltas
        ssum = sum(delta.amt for delta in deltas)
        if ssum <= node_a:
            return deltas
        else:
            multiplier = node_a / ssum
            return [
                Delta(d.nodeid, d.amt * multiplier, d.neighborid)
                    for d in deltas
            ]

# Classes

@dataclass
class ElemInWS:
    '''An element in the workspace.'''
    elem: Elem
    builder: Union[Agent, None]
    tob: int   # time of birth (when Elem was added to the ws)
        # TODO Allow multiple builders?
    # activation: float = 1.0

    def __str__(self):
        return f'{self.elem}  builder={self.builder} tob={self.tob}'

@dataclass
class FARGModel:
    #ws: Set[Hashable] = field(default_factory=set, init=False)
    ws: Dict[Hashable, ElemInWS] = field(default_factory=dict)
    t: int = 0
    slipnet: Slipnet = empty_slipnet  # TODO copy empty_slipnet?
    seed: Union[int, None] = None

    activation_g: ActivationGraph = field(
        default_factory=ActivationGraph, init=False
    )
    sleeping: InitVar[Dict[Elem, int]] = None # = field(
#        default_factory=dict, init=False
#    )

    mutual_support_weight: float = 1.0
    mutual_antipathy_weight: float = -0.2

    globals: Dict[str, Any] = field(default_factory=dict, init=False)

    def __init__(self, **kwargs):
        self.globals = kwargs  # TODO Supply defaults
        for f in fields(self):
            if not f.init:
                continue
            try:
                v = kwargs.pop(f.name)
            except KeyError:
                setattr(self, f.name, default_field_value(f))
            else:
                setattr(self, f.name, v)
            #print('INIT', f.name, getattr(self, f.name))  #DIAG
        self.seed = reseed(self.seed)
        self.sleeping = {}
        #self.activation_g = ActivationGraph(**kwargs)
        #self.activation_g = ActivationGraph(**d_subset(kwargs, fields(ActivationGraph)))
        self.activation_g = ActivationGraph(aprop=kwargs.get('aprop', {}))
        self.make_slipnet()  # TODO rename to fill_slipnet()

    def make_slipnet(self):
        '''Subclasses should override this to initialize the slipnet.'''
        pass

    def slipnodes(self) -> List[Hashable]:
        return list(self.slipnet.nodes)

    #def paint_value(self, canvas: 'Canvas', addr: Addr, v: Value):
    def paint_value(
        self, dest: 'CellRef', v: Value, builder: Union[Agent, None]=None
    ) -> 'CellRef':
        cr = dest.paint_value(self, v, builder=builder)

        if cr.is_real():
            #print(f'PAINTED {v} in {cr}')  #DIAG
            pass
        return cr

    # Codelet functions

    def build(
        self,
        obj,
        builder: Union[Agent, None]=None,
        edge_weight: Union[float, None]=None,
        init_a: Union[float, None]=None
    ) -> Hashable:
        if obj is None:
            return None
        existing_o = self.in_ws(obj)
        #print('BU', obj, existing_o)
        if existing_o is not None:
            obj = existing_o
        else:
            self.ws[obj] = ElemInWS(obj, builder, self.t)
            if init_a is None:
                if builder is None:
                    init_a = 1.0
                else:
                    init_a = min(1.0, self.a(builder))
            self.activation_g.add_node(obj, a=init_a)
            #print('BUILT', obj) #DIAG
            for elem in self.elems(HasAntipathyTo(obj, ignore=builder)):
                self.add_mut_antipathy(obj, elem)
            try:
                obj.on_build(self)
            except AttributeError:
                pass
        if builder:
            self.add_mut_support(builder, obj)
        return obj

    def search_ws1(self, pred: Union[Type, Callable]) \
    -> Union[Elem, bool, 'CellRef']:
        # TODO Search randomly; weight by activation
        for elem in self.ws.keys():
            found = search1(self, elem, pred)
            if found:
                return found
        return False

    def search_ws(
        self,
        #pred: Union[Type, Callable, None]=None,
        pred: FMPred,
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

    def pulse_slipnet(
        self,
        activations_in: Dict[Hashable, float],
        type: Union[Type, None]=None,
        k: int=20,
        num_get: int=1,  # number of slipnodes to return
        filter: Union[Callable, None]=lambda x: True
    ) -> List[Hashable]:
        q = self.slipnet.query(
            activations_in=activations_in, type=type, k=k, filter=filter
        )
        return list(sample_without_replacement(
            [nas.node for nas in q],
            k=num_get,
            weights=[nas.a for nas in q]
        ))

    def sleep(self, elem: Elem, num_timesteps=2):
        if elem in self.sleeping:
            self.sleeping[elem] += num_timesteps
        else:
            self.sleeping[elem] = self.t + num_timesteps

    # Ancillary functions, callable by codelets and Agents

    def in_ws(self, obj: Hashable) -> Hashable:
        '''Returns the object from the workspace if it exists, otherwise
        None.'''
        try:
            eiws = self.ws[obj]
        except KeyError:
            return None
        return eiws.elem

    def elems(self, pred=None, es=None) -> Iterable[Elem]:
        fmpred = as_fmpred(pred)
        if es is None:
            es = self.ws.keys()
        return (e for e in as_iter(es) if fmpred(self, e))

    def agents(self) -> List[Agent]:
        return list(self.ws_query(Agent))

    def is_tagged(self, elems, tagpred) -> bool:
        '''Are any of elems tagged with a tag matching tagpred?'''
        return any(
            tag.is_tagging(elem)
                for elem in as_iter(elems)
                    for tag in self.elems(pred=tagpred)
        )

    def is_blocked(self, elems) -> bool:
        return self.is_tagged(elems, Blocked)

    def can_go(self, elem: Elem):
        return CanGo(self, elem)

    def can_act(self, elem: Elem):
        return CanAct(self, elem)

    def is_sleeping(self, elem: Elem):
        return elem in self.sleeping
            
    # TODO Should we allow **overrides?
    def is_blocked(self, elem: Hashable) -> bool:
        '''Does elem have a Blocked tag?'''
        # TODO INEFFICIENT (False result will check entire ws)
        def pred(fm: 'FARGModel', x: Elem) -> bool:
            return (
                isinstance(x, Blocked) and x.taggee == elem
            )
        return first(self.ws_query(pred))

    def builder_of(self, elem: Elem) -> Union[Elem, None]:
        try:
            eiws = self.ws[elem]
        except KeyError:
            return None
        return eiws.builder

    # Timestep functions

    def do_timestep(
        self, ag: Union[Agent, None]=None, num: int=1, act=False, until=None
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
            if act or self.t % 10 == 0:
                pred = CanAct
                run = CallAct
            else:
                pred = CanGo
                run = CallGo
            if ag is None:
                agent = self.choose_agent_by_activation(pred)
            else:
                agent = ag
            if agent:
                run(self, agent)
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
        for detector in list(self.elems(BaseDetector)):
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

    def boost(self, node: Hashable):
        self.activation_g.boost(node)

    def downboost(self, node: Hashable):
        # HACK to make a node quiet down for a bit after it's done something;
        # better to explicitly sleep for a few timesteps?
        self.activation_g.nodes[node]['a'] /= 2.0

    def deactivate(self, node: Hashable):
        self.activation_g.nodes[node]['a'] = 0.0

    # Support

    def add_mut_support(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    set_mut_support = add_mut_support

    def set_support_edge(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_support_weight
        self.activation_g.add_edge(a, b, weight=weight)

    def add_mut_antipathy(
        self, a: Hashable, b: Hashable, weight: Union[float, None]=None
    ):
        if weight is None:
            weight = self.mutual_antipathy_weight
        self.activation_g.add_edge(a, b, weight=weight)
        self.activation_g.add_edge(b, a, weight=weight)

    def has_antipathy_to(self, a: Hashable, b: Hashable) -> bool:
        try:
            return a.has_antipathy_to(b)
        except AttributeError:
            return False

    def support_weight(self, a: Hashable, b: Hashable) -> float:
        '''Returns the mutual support between nodes 'a' and 'b'. If neither
        node exists, or there is no edge between them in self.support_g,
        then the weight is 0.0.'''
        try:
            return self.activation_g.edges[a, b]['weight']
        except KeyError:
            return 0.0

    def neighbors(self, e: Elem) -> List[Elem]:
        #return list(self.activation_g.adj[e].keys())
        g = self.activation_g
        return set(list(g.successors(e)) + list(g.predecessors(e)))

    def degree(self, a: Elem) -> int:
        return self.activation_g.degree(a)

    # Functions for display and debugging

    def ws_query(self, pred: Union[Type, Callable], **kwargs) \
    -> Iterable[Hashable]:
        builder = kwargs.pop('builder', None)
        if isclass(pred):
            cl = copy(pred)
            pred = lambda _, x: isinstance(x, cl)
        for eiws in list(self.ws.values()):
            if (
                pred(self, eiws.elem)
                and
                (builder is None or eiws.builder == builder)
                and
                self.argsmatch(kwargs, eiws.elem)
            ):
                yield eiws.elem

    def ws_query1(self, pred: Union[Type, Callable], **kwargs) -> Hashable:
        return first(self.ws_query(pred, **kwargs))
        
    @classmethod
    def argsmatch(cls, kwargs: Dict, elem) -> bool:
        try:
            return all(
                getattr(elem, k) == v
                    for k, v in kwargs.items()
            )
        except AttributeError:
            return False

    def __repr__(self):
        cl = self.__class__.__name__
        return f'<{cl} object, t={self.t}>'

    def __str__(self):
        result = StringIO()
        print(f't={self.t}  sum_a={self.sum_a():2.3f}', file=result)
        #print(self.elines(self.elems(), tofile=result))
        self.pr(tofile=result, show_n=True)
        return result.getvalue()

    def l1str(self, eiws: Union[ElemInWS, Elem], indent=None) -> str:
        '''The one-line string for a ws elem, showing its activation.'''
        if indent is None:
            indent = '  '
        if not isinstance(eiws, ElemInWS):
            eiws = self.ws[eiws]  # TODO if the elem does not exist
        return f'{indent}{self.a(eiws.elem): 7.3f}  {eiws} deg={self.degree(eiws.elem)}'

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
        show_n=False,
        edges=False,
        **kwargs
    ):
        '''Prints a subset of the workspace.'''
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
        if show_n:
            print(f'n={count}', file=tofile)

    def pr_flows(self):
        print(f'FLOWS t={self.t}')
        self.activation_g.pr_flows()

def CanGo(fm: FARGModel, elem: Elem) -> bool:
    return (
        isinstance(elem, Agent)
        and
        not fm.is_tagged(elem, NoGo)
        and
        not fm.is_sleeping(elem)
        and
        elem.can_go(fm)
    )

def CallGo(fm: FARGModel, elem: Elem):
    #print(f'go: {elem}') #DIAG
    elem.go(fm)

def CanAct(fm: FARGModel, elem: Elem) -> bool:
    return (
        isinstance(elem, Agent)
        and
        not fm.is_tagged(elem, NoAct)
        and
        elem.can_act(fm)
    )

def CallAct(fm: FARGModel, elem: Elem):
    #print(f'act: {elem}')  #DIAG
    elem.act(fm)

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

    #TODO In __iter__, make a tuple out of avails if it's not Hashable
    #TODO UT, UT taking the same value twice
    def take_avails0(self, values: Iterable[Value]) \
    -> Tuple[Iterable[Value], Iterable[Value]]:
        '''Returns (taken_avails, remaining_avails). Might raise
        ValueNotAvail.'''
        remaining_avails = [] if self.avails is None else list(self.avails)
        taken_avails = []
        for v in values:
            try:
                remaining_avails.remove(v)
            except ValueError:
                raise ValueNotAvail(self, v)
            taken_avails.append(v)
        return (taken_avails, remaining_avails)

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
                #raise ValueNotAvail(self, v)
                taken_avails.append(None)
                missing_avails.append(v)
            else:
                taken_avails.append(v)
                missing_avails.append(None)
        if any(t is None for t in taken_avails):
            #raise ValueNotAvail(self, first(m for m in missing_avails if m is not None))
            raise ValuesNotAvail(
                self,
                tuple(taken_avails),
                tuple(missing_avails)
            )
        return (taken_avails, remaining_avails)
        
    def has_avail_value(self, v: Value) -> bool:
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


@dataclass(frozen=True)  # TODO Allow AgentSeq to change
class AgentSeq(Agent):
    agents: Tuple[Agent]
    initial_kwargs: Collection[Tuple[str, Hashable]]

    def go(self, fm: FARGModel, **overrides):
        kwargs = {**as_dict(self.initial_kwargs), **overrides}
        source = kwargs.get('source', None)

        for agent in self.agents:
            agent = replace(agent, **kwargs)
            dest = source.imaginary_next_cellref()
            source = agent.go(fm, source=source, dest=dest, builder=self)
            #kwargs = self.next_kwargs(kwargs)
        # TODO Return a value

    def act(self, fm: FARGModel, **overrides):
        kwargs = {**as_dict(self.initial_kwargs), **overrides}
        source = kwargs.get('source', None)

        for agent in self.agents:
            agent = replace(agent, **kwargs)
            dest = source.next()
            source = agent.act(fm, source=source, dest=dest, builder=self)
        # TODO Return a value

    '''
    def next_kwargs(self, kwargs: Dict[str, Hashable]) -> Dict:
        # TODO This method needs to be specified as an arg itself somehow
        result = copy(kwargs)
        dest = result['dest']
        result['source'] = dest
        result['dest'] = dest.next()
        return result
    '''

    def __str__(self):
        cl = self.__class__.__name__
        ags = ', '.join(str(a) for a in self.agents)
        return f'{cl}({ags})'

@dataclass(frozen=True)
class MakeAgentSeq:
    tail: Agent  # TODO Allow an AgentSeq, too

    def __call__(self, fm: FARGModel, elem: Union[Elem, None]):
        # TODO
        #print('MAKE AGENT SEQ', self.tail)  #DIAG
        pass

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}(tail={self.tail})'

@dataclass(frozen=True)
class HasAvailValue:
    v: Value

    def __call__(self, fm: FARGModel, elem) -> bool:
        return has_avail_value(elem, self.v)

@dataclass(frozen=True)
class HasAntipathyTo:
    elem: Elem
    ignore: Union[Elem, None]=None

    def __call__(self, fm: FARGModel, elem) -> bool:
        if elem == self.ignore:
            return False
        else:
            return fm.has_antipathy_to(self.elem, elem)

@dataclass(eq=False)
class Canvas(Elem, ABC):

    @abstractmethod
    def raw_paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        '''Put Value v at address addr. The lowest-level canvas-writing
        operation.'''
        pass

    # TODO Make this into __getitem__?
    @abstractmethod
    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        '''Returns the value at addr, or None if the cell at addr is blank.'''
        pass

    @abstractmethod
    def __getitem__(self, addr: Addr) -> Value:
        pass

    @abstractmethod
    def cellrefs(self) -> Iterable['CellRef']:
        pass

@dataclass(eq=False)
class SeqCanvas(Canvas):
    states: List[SeqState] = field(default_factory=list)

    max_a: float = 1.0

    def raw_paint_value(self, fm: FARGModel, addr: Addr, v: Value):
        # TODO Handle addr that doesn't work as a list index
        # TODO Accept a builder argument?
        while len(self.states) <= addr:
            self.states.append(None)
        self.states[addr] = v

    def get_value(self, fm: FARGModel, addr: Addr) -> Union[Value, None]:
        # TODO Handle addr that can't be found or is not an index
        return self.states[addr]

    def __getitem__(self, addr: Addr) -> Value:
        # TODO Handle addr that can't be found or is not an index
        #print('SEQCGET', addr, len(self.states))
        if addr < len(self.states):
            return self.states[addr]
        else:
            return None

    def search(self, fm, pred):
        yield from search(fm, self.cellrefs(), pred)

    def cellrefs(self):
        for addr, state in enumerate(self.states):
            yield CellRef(self, addr)
        
    def has_avail_value(self, v: Value):
        for addr, state in enumerate(self.states):
            if has_avail_value(state, v):
                return CellRef(self, addr)
        return False

    def as_solution(self) -> str:
        return '; '.join(
            st.last_move_str() for st in self.states if st.last_move
        )

    def last_nonblank(self) -> 'CellRef':
        # TODO Actually check for 'blank' SeqStates (which will require
        # implementing that concept)
        return CellRef(self, len(self.states) - 1)

    def __str__(self):
        return f"SeqCanvas({'; '.join(str(st) for st in self.states)})"

@dataclass(frozen=True)
class CellRef:
    canvas: Union[SeqCanvas, None] = None
    addr: Union[int, None] = None

    max_a: float = 2.0

    @property
    def contents(self) -> Hashable:
        # TODO What if .canvas is not in FARGModel?
        return self.canvas[self.addr]

    def on_build(self, fm: FARGModel):
        fm.add_mut_support(self, self.canvas)

    def paint_value(
        self, fm: FARGModel, v: Value, builder: Union[Agent, None]=None
    ) -> 'CellRef':
        # TODO Should we do something with builder?
        self.canvas.raw_paint_value(fm, self.addr, v)
        return self

    def has_avail_value(self, v):
        return has_avail_value(self.contents, v)

    def preceding_contents(self) -> Hashable:
        if self.addr <= 0:
            return self.contents
        else:
            return self.canvas[self.addr - 1]

    def next(self) -> 'CellRef':
        return replace(self, addr=self.addr + 1)

    def is_real(self):
        return True

    def imaginary_next_cellref(self):
        '''Returns an ImCell to hold a hypothetical/imagined next cell after
        this one.'''
        return ImCell(canvas=self.canvas, addr=self.addr + 1)

    def __str__(self):
        cl = self.__class__.__name__
        return f'canvas[{self.addr}]'

@dataclass(frozen=True)
class ImCell(CellRef):
    '''An imaginary cell, not in a canvas. The .contents is a Value that Agents
    are considering constructing or treating hypothetically as existing.'''
    contents: Value = None    # None means empty; empty ImCells are not allowed
                              # in the workspace.

    def paint_value(
        self, fm: FARGModel, v: Value, builder: Union[Agent, None]=None
    ) -> CellRef:
        '''Builds ImCell(v) if v is different than .contents.'''
        if v != self.contents:
            #print('IMCELL', v) #DIAG
            return fm.build(replace(self, contents=v), builder=builder)
        else:
            return self
        
    def is_real(self):
        return False

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.contents})'
    

@dataclass(frozen=True)
class Painter(ABC):

    @abstractmethod
    # TODO Do we really need to pass 'fm'?
    def paint(self, fm: FARGModel):
        pass

# TODO rm?
@dataclass(frozen=True)
class LiteralPainter(Painter):
    cell: CellRef
    value: Value

    def paint(self, fm: FARGModel):
        # TODO Throw exc if any members are missing? Or should that be an
        # assertion (if it's illegal to create a LiteralPainter that's missing
        # any args)?
        #self.canvas.paint_value(fm, self.addr, self.value)
        self.cell.paint_value(fm, self.value)

    def has_avail_value(self, v: Value) -> bool:
        return has_avail_value(self.value, v)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.cell}, {self.value})'

@dataclass(frozen=True)
class Tag(Elem, ABC):
    '''A workspace element that indicates something about another workspace
    element.'''

    @abstractmethod
    def is_tagging(self, elems: Union[Elem, Iterable[Elem], None]) -> bool:
        '''Is this Tag on any of 'elems'?'''
        pass

@dataclass(frozen=True)
class TaggeeTag(Tag):
    taggee: Hashable

    def is_tagging(self, elems):
        return any(self.taggee == elem for elem in as_iter(elems))

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee})'

@dataclass(frozen=True)
class NoGo(TaggeeTag):
    '''Indicates that the taggee's .go() method should not be called.'''
    taggee: Agent

@dataclass(frozen=True)
class NoAct(TaggeeTag):
    '''Indicates that the taggee's .act() method should not be called.'''
    taggee: Agent

@dataclass(frozen=True)
class GoIsDone(NoGo):
    '''Indicates that taggee's .go() method has been run successfully and there
    is no need to call it again, even if the taggee has high activation.'''

@dataclass(frozen=True)
class ActIsDone(NoGo, NoAct):
    '''Indicates that taggee's .act() method has been run successfully and there
    is no need to call it again, nor to call its .go() method, even if the
    taggee has high activation.'''

@dataclass(frozen=True)
class Blocked(NoGo, Agent):
    taggee: Hashable
    reason: Hashable

    def go(self, fm: FARGModel):
        # TODO The .reason might not have a .try_to_fix method (and probably
        # shouldn't).
        self.reason.try_to_fix(fm, behalf_of=self.taggee, builder=self)
        fm.build(GoIsDone(taggee=self))

    def can_go(self, fm):
        return True

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.taggee}, {self.reason})'
    
@dataclass(frozen=True)
class BaseDetector(ABC):

    @abstractmethod
    def look(self, fm: FARGModel):
        pass

@dataclass(frozen=True)
class Detector(BaseDetector):
    target: Value  # Change to a match function?
    action: Callable
    filter: Union[Elem, None] = None

    def look(self, fm: FARGModel):
        # TODO See if self.target is there, favoring new elems

        #found = CellWithAvailValue(self.target).search(fm)
        found = fm.search_ws(self.make_pred())
        found = set(found)
        if found:
            #print(f"FOUND {self} {', '.join(str(f) for f in found)}") #DIAG
            pass
        for cellref in found:
            if cellref.is_real():
                self.action(fm, cellref)
            #self.action(fm, found, agent=self)

    def make_pred(self):
        if self.filter:
            f = as_fmpred(self.filter)
            def pred(fm: FARGModel, x: Elem) -> bool:
                return (
                    f(fm, x)
                    and
                    isinstance(x, CellRef) and has_avail_value(x, self.target)
                )
        else:
            def pred(fm: FARGModel, x: Elem) -> bool:
                return (
                    isinstance(x, CellRef) and has_avail_value(x, self.target)
                )
        return pred

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.target}, {name_of(self.action)})'

def halt(fm: FARGModel, *args, **kwargs):
    raise Halt(*args, **kwargs)

@dataclass(frozen=True)
class RaiseException:
    exc_class: Type

    def __call__(self, fm: FARGModel, *args, **kwargs):
        raise self.exc_class(*args, **kwargs)

    def __str__(self):
        cl = self.__class__.__name__
        return f'{cl}({self.exc_class.__name__})'

#TODO rm
@dataclass(frozen=True)
class Parg(Painter):
    '''A complex of painter objects and objects specifying arguments for
    them, suitable for support relationships between PargComplexes, painters,
    arguments, and the sources thereof.'''
    painter: Painter
    kwargs: Dict[str, Any] = field(default_factory=dict)

    def add_arg(self, **kwargs):
        '''Alter .painter and .kwargs to match kwargs.'''
        # TODO Update support_g?
        # TODO If an arg already has a non-None value defined, do we create
        # a new Parg with mutual antipathy with the old Parg?
        self.kwargs.update(kwargs)
        self.painter = self.painter.__class__(**self.kwargs)

    def paint(self, fm: FARGModel):
        # TODO Throw exception of .painter doesn't have enough args
        self.painter.paint(fm)

@dataclass(frozen=True)
class MatchFunc(ABC):
    '''Give a MatchFunc a thing and a context, and it will give you a
    number from 0.0 to 1.0. Each MatchFunc specifies how sensitive it
    is to different kinds of things, relative to their context.'''

    @abstractmethod
    def __call__(self, thing, context) -> float:
        pass

def x_in_context(thing, context) -> Real:
    # TODO Consider the context; force thing to be a number if it's not.
    return thing

@dataclass(frozen=True)
class MatchByPeaks(MatchFunc):
    '''A MatchByPeaks is a function with one or more centers of peak
    sensitivity, and a specificity parameter determining how sensitivity falls
    off depending on how far away a stimulus is from a peak. The greater the
    specificity, the faster the function decays as x gets further from a peak.
    The result of a MatchByPeaks is the maximum of the results of trying
    the thing against each peak.'''
    # TODO Add noise

    peaks: Set[Number]
    squeeze: Real = 1.0

    def __call__(self, thing, context) -> float:
        x = x_in_context(thing, context)
        return max(
            (self.peak_f(peak, x) for peak in self.peaks),
            default=0.0
        )

    def peak_f(self, peak, x) -> float:
        return exp(- self.squeeze * (peak - x) **2)

@dataclass(frozen=True)
class ExactMatchFunc(MatchFunc):
    target: Hashable

    def __call__(self, thing, context) -> float:
        if thing == self.target:
            return 1.0
        else:
            return 0.0

@dataclass(frozen=True)
class CellWithAvailValue:
    v: Value

    def is_candidate(self, fm: FARGModel, x: Any) -> bool:
        '''Is x even something we want to check for a match?'''
        return isinstance(x, (CellRef, Canvas))

    def candidates(self, fm: FARGModel) -> Iterable[CellRef]:
        for x in fm.ws_query(self.is_candidate):
            if isinstance(x, Canvas):
                yield from x.cellrefs()
            else:
                yield x

    def search(self, fm: FARGModel) -> Iterable[CellRef]:
        for c in self.candidates(fm):
            if has_avail_value(c, self.v):
                yield c
        
    #def matchpct(self, 

@dataclass
class Glom:
    _members: Counter = field(
        default_factory=lambda: Counter()
    )

    @property
    def members(self) -> List[Hashable]:
        return list(self._members.elements())

    @classmethod
    def make_from(cls, f: Callable, avails: Iterable[Hashable]) -> 'Glom':
        members = Counter()
        for a in avails:
            #print('F', a, f(a))  #DIAG
            if f(a) > 0.80:
                members[a] += 1
        return Glom(members)


if __name__ == '__main__':
    if False:
        # TODO Make a UT out of this
        fm = FARGModel()
        s0 = SeqState((4, 5, 6), None)
        c = SeqCanvas([s0])
        cu = Consume(plus, (4, 5), c, 1)
        cu.paint(fm)
        print(c)

    if False:
        # TODO Make a UT out of this
        fm = FARGModel()
        s0 = SeqState((4, 5, 6), None)
        c = SeqCanvas([s0])
        p = Parg(Consume())
        p.add_arg(canvas=c)
        p.add_arg(addr=1, operator=plus, operands=(5, 4))
        p.paint(fm)
        print(c)

    if False:
        fm = FARGModel()
        c = SeqCanvas()
        s0 = SeqState((4, 5, 6), None)
        lp = LiteralPainter(c, 0, s0)
        lp.paint(fm)
        print(c)

    if False:  # sketching
        fm = FARGModel()
        ca = fm.add(SeqCanvas([SeqState((4, 5, 6), None)]))
        co = fm.add(Consume(plus, (4, 5)))  # missing canvas and addr
        # make a Consume
        # let it build LiteralPainters
        # LiteralPainters paint on SeqCanvas

    if False:
        mf = MatchByPeaks({4, 7}, 2.0)
        ef = ExactMatchFunc(4)
        for x in range(1, 11):
            print(x, mf(x, None), ef(x, None))


    #s0 = fm.add(Top, SeqState((2, 3, 4, 5, 11), None))
    #g0 = fm.make_seqglom(s0)
    #s1 = fm.consume(s0, Times, (5, 11))
    #g1 = fm.append(g0, s1)
    #s2 = fm.consume(s0, Times, (4, 11))
    #g2 = fm.append(g0, s2)
    #? g2 = fm.consume(g0, Times, (4, 11))
    # Now see the support graph

    # Pons asinorum, hard-coded codelet sequence
    if False:
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        wa = fm.build(Want(15, canvas=ca, addr=0))
        wa.go(fm) # Builds Consume objects and Detector

        for co in fm.ws_query(Consume, builder=wa):
            co.go(fm)

        bl = fm.ws_query1(Blocked)
        bl.go(fm)

        d9 = fm.ws_query1(Detector, target=9)
        d9.go(fm)

        co1 = fm.ws_query1(Consume, operands=(4, 5))
        co2 = fm.ws_query1(Consume, operands=(9, 6))
        aseq0 = fm.build(
            AgentSeq(
                (co1, co2),
                initial_kwargs=tupdict(
                    source=CellRef(ca, 0),
                    dest=CellRef(ca, 1)
                )
            )
        )
        print(f'aseq0: {aseq0}')
        aseq0.go(fm)  #This should not complain

        aseq = fm.ws_query1(AgentSeq)
        aseq.act(fm)

        d15 = fm.ws_query1(Detector, target=15)
        try:
            d15.go(fm)
            print('FAILED! Did not detect 15.')
        except Halt as exc:
            print('SUCCEEDED', exc)

        print(fm)
        print()
        pred = CellWithAvailValue(15)
        l = (list(pred.search(fm)))
        pl(l)
        print(len(l))
