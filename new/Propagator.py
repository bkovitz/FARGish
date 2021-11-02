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
import sys

import matplotlib.pyplot as plt  # type: ignore[import]

from FMTypes import ADict, Node
from util import trace, short, pl, first, pr, pts


def reverse_sigmoid(x: float, p: float=0.5):
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

@dataclass
class Delta:
    #TODO nodeid -> node, neighborid -> neighbor
    nodeid: Node    # The node whose value is to be changed
    amt: float        # The amount by which it is to be changed
    neighborid: Node  # The neighbor that is the source of the change

    def __str__(self):
        return f'{self.nodeid!s:20s} {self.amt:1.10f}   {self.neighborid}'

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
        # exponent for reverse_sigmoid
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
    
    Abstract class: concrete class must define .make_deltas().'''

    def propagate_once(self, g, old_d: Dict[Node, float]):
        #print('PONCE', len(old_d))
        # decay
        new_d: Dict[Node, float] = defaultdict(float,
            #((nodeid, a * self.alpha)
            #((nodeid, max(self.min_value(g, nodeid), a * self.alpha))
            ((nodeid, self.clip_a(g, nodeid, a * self.alpha))
                for nodeid, a in old_d.items()
            )
        )
        # apply all the deltas
        for delta in self.make_deltas(g, old_d):
            #print('DELT0', delta)
            actual_delta = (
                (
                    delta.amt
                    * (1.0 + self.positive_feedback_rate
                             * old_d.get(delta.nodeid, 0.0))
                    * (1.0 - self.alpha)
                    #+
                )
                + gauss(0.0, self.noise)
            )
            #print(f'{delta.nodeid!s:20s} {delta.amt!s:20s}  {actual_delta!s:20s}   {delta.neighborid}') #DEBUG
            #print('PONCE', delta.nodeid, old_d[delta.nodeid], new_d[delta.nodeid], delta.amt, actual_delta)
            #print('DELTA', replace(delta, amt=actual_delta))
            new_d[delta.nodeid] += actual_delta
            #self.flows[(delta.neighborid, delta.nodeid)] += actual_delta
            self.flows.add_flow(delta.neighborid, delta.nodeid, actual_delta)
            if delta.neighborid not in new_d:
                new_d[delta.neighborid] = 0.0
        # clip to min_value
        new_d = defaultdict(float,
            #((nodeid, max(self.min_value(g, nodeid), s))
            ((nodeid, self.clip_a(g, nodeid, s))
                for nodeid, s in new_d.items()
            )
        )
        return self.normalize(new_d)

    def propagate(
        self,
        g,
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
    def make_deltas(self, g, old_d: Dict[Node, float]) -> Iterable[Delta]:
        pass

#    @abstractmethod
#    def min_value(self, g, nodeid: Node) -> float:
#        pass
    def clip_a(self, g, nodeid, a: float) -> float:
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
            result[node] = reverse_sigmoid(scale_down * value, p=self.sigmoid_p)
        scale_up = self.max_total / sum(result.values())
        for node in result:
            result[node] *= scale_up
        return result

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

    def plot(self, n: Optional[int]=None) -> None:
        plt.ion()
        plt.clf()
        plt.gcf().set_size_inches(8, 8)
        plt.xlabel('subt')
        plt.ylabel('a')
        tss: Iterable[NodeTimeseries] = self.tsd.values()
        if n:
            tss = nlargest(n, tss, lambda ts: ts.max_a())
        for ts in tss:
            ts.plot()
        max_subt = max((ts.max_subt() for ts in self.tsd.values()), default=0)
        max_a = max((ts.max_a() for ts in self.tsd.values()), default=0.0)
        plt.axis([0, max_subt, 0, max_a])
        plt.legend()
        plt.suptitle(str(self))

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


