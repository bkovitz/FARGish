# Propagator.py

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar
from random import gauss

from Node import NodeId


def reverse_sigmoid(x: float, p: float=0.5):
    '''Returns reverse sigmoid of x, where p is the exponent.
    If x is negative, returns reverse sigmoid of -x.'''
    if x < 0:
        x = -x
    return x ** p / (x ** p + (1 - x) ** p)

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

    def propagate(self, g, old_d: Dict[NodeId, float]):
        new_d: Dict[NodeId, float] = {}
        for node, old in old_d.items():
            new_value = old * self.alpha + sum(
                (1.0 - self.alpha) *
                (self.positive_feedback_rate * old_d[neighbor]) *
                self.hop_weight(g, neighbor, node) *
                gauss(1.0, self.noise)
                    for neighbor in self.incoming_neighbors(g, node)
            )
            new_d[node] = max(self.min_value(g, node), new_value)
        new_d = self.normalize(new_d)
        for node, new_value in new_d.items():
            self.set_value(g, node, new_value)
        
    @abstractmethod
    def incoming_neighbors(self, g, nodeid: NodeId) -> Iterable[NodeId]:
        pass

    @abstractmethod
    def hop_weight(self, g, fromid: NodeId, toid: NodeId) -> float:
        pass

    @abstractmethod
    def min_value(self, g, nodeid: NodeId) -> float:
        pass

    @abstractmethod
    def set_value(self, g, node: NodeId, new_value: float):
        pass

    def normalize(self, d: Dict[NodeId, float]) -> Dict[NodeId, float]:
        '''Returns d normalized so the sum of all the values does not
        exceed self.max_total.'''
        result = {}
        total = sum(d.values())
        if total <= self.max_total:
            return d
        scale_down = 1.0 / max(d.values())
        for node, value in d.items():
            result[node] = reverse_sigmoid(scale_down * value, p=self.sigmoid_p)
        scale_up = self.max_total / sum(result.values())
        for node in result:
            result[node] *= scale_up
        return result
