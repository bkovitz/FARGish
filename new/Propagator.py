# Propagator.py

from abc import ABC, abstractmethod
from dataclasses import dataclass, replace, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from random import gauss
from collections import defaultdict
import sys

from FMTypes import ADict, Node


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
        num_iterations=None
    ) -> ADict:
        if old_d is None:
            old_d = {}
        self.flows.clear()
        if num_iterations is None:
            num_iterations = self.num_iterations
        new_d = old_d
        for i in range(num_iterations):
            new_d = self.propagate_once(g, new_d)
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
