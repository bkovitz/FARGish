# Propagator.py

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from random import gauss
from collections import defaultdict

from Node import NodeId


def reverse_sigmoid(x: float, p: float=0.5):
    '''Returns reverse sigmoid of x, where p is the exponent.
    If x is negative, returns reverse sigmoid of -x.'''
    if x < 0:
        x = -x
    return x ** p / (x ** p + (1 - x) ** p)

@dataclass
class Delta:
    nodeid: NodeId    # The node whose value is to be changed
    amt: float        # The amount by which it is to be changed

@dataclass
class Propagator(ABC):
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

    def propagate_once(self, g, old_d: Dict[NodeId, float]):
        new_d: Dict[NodeId, float] = defaultdict(float,
            #((nodeid, a * self.alpha)
            ((nodeid, max(self.min_value(g, nodeid), a * self.alpha))
                for nodeid, a in old_d.items()
            )
        )
        # apply all the deltas
        for delta in self.make_deltas(g, old_d):
            actual_delta = \
                delta.amt * (1.0 - self.alpha) + gauss(0.0, self.noise)
            #print('PONCE', delta.nodeid, old_d[delta.nodeid], new_d[delta.nodeid], delta.amt, actual_delta)
            new_d[delta.nodeid] += actual_delta
        # clip to min_value
        new_d = defaultdict(float,
            ((nodeid, max(self.min_value(g, nodeid), s))
                for nodeid, s in new_d.items()
            )
        )
        return self.normalize(new_d)

    def propagate(self, g, old_d: Dict[NodeId, float], num_iterations=None):
        if num_iterations is None:
            num_iterations = self.num_iterations
        new_d = old_d
        for i in range(num_iterations):
            new_d = self.propagate_once(g, new_d)
        return new_d
        
    @abstractmethod
    def make_deltas(self, g, old_d: Dict[NodeId, float]) -> Iterable[Delta]:
        pass

    @abstractmethod
    def min_value(self, g, nodeid: NodeId) -> float:
        pass

    def normalize(self, d: Dict[NodeId, float]) -> Dict[NodeId, float]:
        '''Returns d normalized so the sum of all the values does not
        exceed self.max_total.'''
        result = {}
        total = sum(d.values())
        #print('NORM', total, self.max_total)
        if total <= self.max_total:
            return d
        scale_down = 1.0 / max(d.values())
        for node, value in d.items():
            result[node] = reverse_sigmoid(scale_down * value, p=self.sigmoid_p)
        scale_up = self.max_total / sum(result.values())
        for node in result:
            result[node] *= scale_up
        return result
