# Propagator.py

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass, replace, field, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from random import gauss
from collections import defaultdict
from heapq import nlargest
from itertools import chain
from copy import copy
import sys

import matplotlib.pyplot as plt  # type: ignore[import]

from FMTypes import ADict, Node, Pred, as_pred, epsilon
from util import trace, short, pl, first, pr, pts

def sigmoid(x: float, p: float=0.5):
    '''Returns reverse sigmoid of x, where p is the exponent.
    If x is negative, returns reverse sigmoid of -x.

    When p < 1.0, the function is a reverse sigmoid centered at x=0.5:
    x values below 0.5 map to higher values; x values above 0.5 map to
    lower values.

    When p > 1.0, the function is an ordinary sigmoid centered at x=0.5:
    "the rich get richer and the poor get poorer".

    When p = 1.0, the function is equivalent to y=x.
    '''
    if x < 0:
        x = -x
    return x ** p / (x ** p + (1 - x) ** p)

@dataclass(frozen=True)
class SentA:
    '''Activation sent from one node to another.'''
    #TODO nodeid -> node, neighborid -> neighbor
    #nodeid: Node    # The node whose value is to be changed
    #amt: float        # The amount by which it is to be changed
    #neighborid: Node  # The neighbor that is the source of the change
    to_node: Node
    a: float
    from_node: Node

    def __str__(self) -> str:
        return f'{self.to_node!s:20s} {self.a:1.10f}   {self.from_node}'

@dataclass
class Delta:
    '''A change to a node's activation (without regard to where it came
    from).'''
    node: Node   # The node affected
    amt: float   # The amount of the change

@dataclass(frozen=True)
class Flows:
    '''Holds a record of activation flows.'''
    _fromto: Dict[Tuple[Node, Node], float] = \
        field(default_factory=lambda: defaultdict(float))
    _total_in: Dict[Node, float] = \
        field(default_factory=lambda: defaultdict(float))
    _total_out: Dict[Node, float] = \
        field(default_factory=lambda: defaultdict(float))

    def clear(self):
        self._fromto.clear()
        self._total_in.clear()
        self._total_out.clear()

    def add_flow(self, fromnode: Node, tonode: Node, a: float):
        self._fromto[(fromnode, tonode)] += a
        self._total_in[tonode] += a
        self._total_out[fromnode] += a

    def pr(self):
        for node in self.nodes:
            #print(f'{str(node):30s}  in={self._total_in[node]:= 3.5f} out={self._total_out[node]:= 3.5f}')
            print(f'{str(node):50s}  {self._total_in[node]:= 3.5f}  {self._total_out[node]:= 3.5f}')
            for neighbor in self.nodes:
                a_in = self._fromto.get((neighbor, node), None)
                a_out = self._fromto.get((node, neighbor), None)
                #a_out = self._fromto.get((node, neighbor), None)
                if a_in is None and a_out is None:
                    continue
                elif a_in is None:
                    a_in = 0.0
                elif a_out is None:
                    a_out = 0.0
                print(f'  {str(neighbor):70s}  {a_in:= 3.5f}  {a_out:= 3.5f}')

    @property
    def nodes(self) -> Iterable[Node]:
        '''Returns generator of nodes, sorted by str.'''
        for node in sorted(self._total_in.keys(), key=str):
            yield node

@dataclass
class PropagatorDataclassMixin:
    '''Needed only to get around mypy bug https://stackoverflow.com/a/69344698/1393162'''
    positive_feedback_rate: float = 1.2
        # constant for favoring already-supported/active nodes
    alpha: float = 0.98
        # continuity constant; decay rate
    sigmoid_p: float = 0.5
        # exponent for sigmoid function
    max_total: float = 10.0
        # total support/activation allowed at end of timestep
    noise: float = 0.05
        # sigma parameter for normal dist. sampled and added
    num_iterations: int = 1

#    flows: Dict[Tuple(Node, Node), float] = \
#        field(default_factory=lambda: defaultdict(float))
    flows: Flows = field(default_factory=Flows)

class Propagator(ABC, PropagatorDataclassMixin):
    '''Generic spreading-activation propagator class.
    
    Abstract class: concrete class must define .make_sentas().'''

    def propagate_once(self, g: Graph, old_d: Dict[Node, float]) -> ADict:
        """
        #print('PONCE', len(old_d))
        # decay
        new_d: ADict = defaultdict(float,
            #((nodeid, a * self.alpha)
            #((nodeid, max(self.min_value(g, nodeid), a * self.alpha))
            ((nodeid, self.clip_a(g, nodeid, a * self.alpha))
                for nodeid, a in old_d.items()
            )
        )
        # apply all the sentas
        for senta in self.make_sentas(g, old_d):
            #print('DELT0', senta)
            actual_senta = (
                (
                    senta.a
                    * (1.0 + self.positive_feedback_rate
                             * old_d.get(senta.to_node, 0.0))
                    * (1.0 - self.alpha)
                    #+
                )
                + gauss(0.0, self.noise)
            )
            #print(f'{senta.nodeid!s:20s} {senta.amt!s:20s}  {actual_senta!s:20s}   {senta.neighborid}') #DEBUG
            #print('PONCE', senta.nodeid, old_d[senta.nodeid], new_d[senta.nodeid], senta.amt, actual_senta)
            #print('DELTA', replace(senta, amt=actual_senta))
            new_d[senta.to_node] += actual_senta
            #self.flows[(senta.neighborid, senta.nodeid)] += actual_senta
            self.flows.add_flow(senta.from_node, senta.to_node, actual_senta)
            if senta.from_node not in new_d:
                new_d[senta.from_node] = 0.0
        # clip to min_value
        new_d = defaultdict(float,
            #((nodeid, max(self.min_value(g, nodeid), s))
            ((nodeid, self.clip_a(g, nodeid, s))
                for nodeid, s in new_d.items()
            )
        )
        """

        initial_d = self.initial_new_d(g, old_d)
        deltas = self.sentas_to_deltas(
            self.adjust_senta(s, old_d) for s in self.make_sentas(g, old_d)
        )
        new_d = self.apply_deltas(g, old_d, initial_d, deltas)
        return self.normalize(new_d)

    def initial_new_d(self, g: Graph, old_d: ADict) -> ADict:
        '''Returns an ADict containing the activation levels of each node,
        after decay and clipping.'''
        return defaultdict(float,
            ((node, self.clip_a(g, node, a * self.alpha))
                for node, a in old_d.items()
            )
        )

    def apply_deltas(
        self, g: Graph, old_d: ADict, initial_d: ADict, deltas: Iterable[Delta]
    ) -> ADict:
        '''Returns a new ADict, resulting from applying 'deltas' to 'initial_d',
        possibly considering the activations from the previous timestep, in
        'old_d'.'''
        new_d: ADict = defaultdict(float, initial_d)
        for delta in deltas:
            new_d[delta.node] += delta.amt
        return new_d
        # TODO clip

    def sentas_to_deltas(self, sentas: Iterable[SentA]) \
    -> Iterable[Delta]:
        '''Coalesces the SentA's into Deltas, one Delta for each node whose
        activation is to change. Override this to change how the coalescing
        works. The default implementation simply adds up the .a values in
        the sentas and then multiplies by (1.0 - self.alpha).'''
        d: Dict[Node, Delta] = {}
        for senta in sentas:
            try:
                delta = d[senta.to_node]
            except KeyError:
                delta = Delta(senta.to_node, 0.0)
                d[senta.to_node] = delta
            delta.amt += senta.a
        multiplier = 1.0 - self.alpha
        for delta in d.values():
            delta.amt *= multiplier
            if abs(delta.amt) >= epsilon:
                yield delta
            
    def adjust_senta(self, senta: SentA, old_d: ADict) -> SentA:
        '''Applies noise and positive feedback to 'senta'. Returns a new
        SentA object if there is any change.'''
        new_a = (
            senta.a * (1.0 + self.positive_feedback_rate
                             * old_d.get(senta.to_node, 0.0))
        ) + gauss(0.0, self.noise)
        return replace(senta, a=new_a)

    def propagate(
        self,
        g: Graph,
        old_d: Union[ADict, None],
        num_iterations=None,
        alog: Optional[ActivationLog]=None
        #alogger: Optional[ALogger]=None  # TODO rm alogger
    ) -> ADict:
        '''Propagates activation, starting with the nodes and activations in
        old_d, num_iterations times. Returns new activation dictionary.
        Side-effect: self.alog holds a log of all the activations in this call
        to .propagate(), at each iteration.'''
        if old_d is None:
            old_d = {}
        self.flows.clear()
        if alog:
            alog.add_dict(old_d)
        if num_iterations is None:
            num_iterations = self.num_iterations
        new_d = old_d
        for i in range(num_iterations):
            new_d = self.propagate_once(g, new_d)
            if alog:
                alog.add_dict(new_d)
        return new_d

    @abstractmethod
    def make_sentas(self, g: Graph, old_d: Dict[Node, float]) \
    -> Iterable[SentA]:
        pass

#    @abstractmethod
#    def min_value(self, g, nodeid: Node) -> float:
#        pass
    def clip_a(self, g: Graph, node: Node, a: float) -> float:
        return a

    def normalize(self, d: Dict[Node, float]) -> Dict[Node, float]:
        '''Returns d normalized so the sum of all the values does not
        exceed self.max_total.'''
        result = {}
        total = sum(d.values())
        #print('NORM', self.__class__.__name__, total, self.max_total)
        if total <= self.max_total:
            return d
        scale_down = 1.0 / max(d.values())
        for node, value in d.items():
            result[node] = sigmoid(scale_down * value, p=self.sigmoid_p)
        scale_up = self.max_total / sum(result.values())
        for node in result:
            result[node] *= scale_up
        return result

    def sentas_to(self, g: Graph, old_d: ADict, node: Node) -> Iterable[SentA]:
        '''Makes a SentA for each incoming edge of 'node'.
        Each SentA goes to 'node' to a neighbor, with weight = the edge
        weight times the neighbor's activation in 'old_d' (or 0.0 if the
        neighbor is not in 'old_d'.'''
        for hop in g.hops_to_node(node):
            if abs(hop.weight) >= epsilon:
                neighbor_a = old_d.get(hop.from_node, 0.0)
                if abs(neighbor_a) >= epsilon:
                    yield SentA(node, hop.weight * neighbor_a, hop.from_node)

    def sentas_from(self, g: Graph, old_d: ADict, node: Node) \
    -> Iterable[SentA]:
        '''Makes a SentA for each outgoing edge of 'node'.
        Each SentA goes from 'node' to a neighbor, with weight = the edge
        weight times 'node's activation in 'old_d' (or 0.0 if 'node' is not
        in 'old_d').'''
        node_a = old_d.get(node, 0.0)
        if abs(node_a) >= epsilon:
            #print('DFF', node, type(node), node_a, list(g.hops_from_node(node)))
            for hop in g.hops_from_node(node):
                if abs(hop.weight) >= epsilon:
                    yield SentA(hop.to_node, hop.weight * node_a, node)

ALogsKey = Tuple[Optional[int], Hashable]  # (timestep, label)
    # key to look up an ActivationLog in ActivationLogs.

@dataclass
class ActivationLogs:
    '''A collection of ActivationLog objects, indexed by arbitrary labels.
    An ActivationLogs object can make an ALogger object to pass down the
    stack, ultimately to Propagator.propagate() so it knows where to record
    node activations during one sequence of spreading activation.'''
    logs: Dict[ALogsKey, ActivationLog] = field(default_factory=dict)

    # TODO UT
    def start_alog(self, t: int, label: Hashable) -> ActivationLog:
        alog = ActivationLog(t=t, label=label)
        self.logs[(t, label)] = alog
        return alog

    def pr(self) -> None:
        for alog in self.logs.values():
            print(f'{alog.t:3} {short(alog.label):40} {alog.num_pulsed_nodes()}, {alog.num_nodes()}')

@dataclass
class ActivationLog:
    '''A record of a series of activations from the beginning to the end of
    one call to Propagator.propagate().'''
    tsd: Dict[Node, NodeTimeseries] = field(
        init=False, default_factory=dict
    )
    adict0: InitVar[Optional[ADict]] = None
    subt: Optional[int] = None  # Current timestep within spreading activation
    label: Hashable = None
    t: Optional[int] = None  # Current timestep at the top level of the model

    def __str__(self) -> str:
        ls: List[str] = []
        if self.t is not None:
            ls.append(f't={self.t}')
        if self.label is not None:
            ls.append(short(self.label))
        else:
            cl = self.__class__.__name__
            ls.append(f'{cl}/{self.num_nodes()}')
        return ' '.join(ls)

    def __post_init__(self, adict0: Optional[ADict]=None):
        '''adict0 is activations_in at timestep 0.'''
        if adict0:
            self.add_dict(adict0, subt=self.subt)

    def add_item(self, node: Node, subt: int, a: float) -> None:
        try:
            ts = self.tsd[node]
        except KeyError:
            ts = NodeTimeseries(node)
            self.tsd[node] = ts
        ts.add(subt, a)

    def add_dict(self, d: ADict, subt: Optional[int]=None) -> None:
        if subt is None:
            if self.subt is None:
                self.subt = 0
            else:
                self.subt += 1
        else:
            self.subt = subt
        for node, a in d.items():
            self.add_item(node, self.subt, a)
        
    # TODO UT
    def num_nodes(self) -> int:
        '''Returns maximum number of nodes at any timestep.'''
        return len(self.tsd)

    # TODO UT
    def num_pulsed_nodes(self) -> int:
        '''Returns the number of nodes with non-zero activation at subt=0.'''
        return sum(
            1 for ts in self.tsd.values() if ts.is_pulsed_node()
        )

    def pulsed_nodes(self) -> Iterable[Node]:
        '''Returns an iterable containing all the nodes with non-zero
        activation at subt=0.'''
        for ts in self.tsd.values():
            if ts.is_pulsed_node():
                yield ts.node

    def pr(self, *args, **kwargs) -> None:
        for ts in self.tsd.values():
            pr(ts)

    def prlast(self, pred: Pred, n: Optional[int]=None) -> None:
        key = lambda ts: ts.last_a()
        if n is None:
            tss = sorted(self.tss(pred), key=key)
        else:
            tss = nlargest(n, self.tss(pred), key=key)
        for ts in tss:
            print(f'{short(ts.node):40s} {ts.last_a():1.5f}')

    def plot(self, pred: Pred=None, *, n: Optional[int]=None, pr: bool=False) \
    -> None:
        plt.ion()
        plt.clf()
        plt.gcf().set_size_inches(8, 8)
        plt.xlabel('subt')
        plt.ylabel('a')
        #tss: Iterable[NodeTimeseries] = self.tsd.values()
        tss = self.tss(pred)
        if n:
            tss = nlargest(n, tss, lambda ts: ts.max_a())
        for ts in tss:
            ts.plot()
        max_subt = max((ts.max_subt() for ts in tss), default=0)
        max_a = max((ts.max_a() for ts in tss), default=0.0)
        plt.axis([0, max_subt, 0, max_a])
        plt.legend()
        plt.suptitle(str(self))
        if pr:
            self.prlast(pred=pred, n=n)

    def tss(self, pred: Pred=None) -> List[NodeTimeseries]:
        '''pred selects Nodes.'''
        pred: Callable[[Any], bool] = as_pred(pred)
        return [ts for ts in self.tsd.values() if pred(ts.node)]

@dataclass
class NodeTimeseries:
    '''A record of a single Node's activations during one run of
    Propagator.propagate().'''
    node: Node
    aa: Dict[int, float] = field(default_factory=dict)   # timeseries

    def min_subt(self) -> int:
        '''First timestep.'''
        return min(self.aa.keys(), default=0)

    def max_subt(self) -> int:
        '''Last timestep.'''
        return max(self.aa.keys(), default=0)

    def min_a(self) -> float:
        '''Minimum activation.'''
        return min(self.aa.values(), default=0.0)

    def max_a(self) -> float:
        '''Maximum activation.'''
        return max(self.aa.values(), default=0.0)

    def first_a(self) -> float:
        '''First activation.'''
        return first(self.aa.values())

    def last_a(self) -> float:
        '''Last activation.'''
        return list(self.aa.values())[-1]

    def add(self, subt: int, a: float) -> None:
        self.aa[subt] = a

    def plot(self) -> None:
        plt.plot(self.aa.keys(), self.aa.values(), label=short(self.node))

    def is_pulsed_node(self) -> bool:
        return 0 in self.aa and self.aa[0] > 0.0

    def __len__(self) -> int:
        return len(self.aa)

    def short(self) -> str:
        return f'{self.node:30} {self.min_subt()}..{self.max_subt()}  {self.min_a():6.3f} ..{self.max_a():6.3f}  {self.first_a():6.3f} ..{self.last_a():6.3f}'

### Some concrete Propagator classes ###

@dataclass
class PropagatorIncoming(Propagator):
    '''A Propagator that follows hops that lead to nodes whose keys are in the
    activations dictionary. Each timestep, g.hops_to_node() supplies all the
    edges. Because of this, only nodes explicitly included in old_d can
    *ever* receive any activation when .propagate() is called.'''

    def make_sentas(self, g: Graph, old_d: ADict) -> Iterable[SentA]:
        return chain.from_iterable(
            self.sentas_to(g, old_d, node) for node in old_d
        )

@dataclass
class PropagatorOutgoing(Propagator):
    '''A Propagator that follows hops that lead to nodes whose keys are in the
    activations dictionary. Each timestep, g.hops_to_node() supplies all the
    edges. Because of this, only nodes explicitly included in old_d can
    *ever* receive any activation when .propagate() is called.'''

    def make_sentas(self, g: Graph, old_d) -> Iterable[SentA]:
        return chain.from_iterable(
            self.sentas_from(g, old_d, node) for node in old_d
        )

import Graph as GraphModule
Graph = GraphModule.Graph
