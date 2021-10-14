# Slipnet.py -- Spreading activation on long-term memory, possibly augmented
#               by additional nodes and edges from the workspace

from dataclasses import dataclass, field, InitVar
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal, Protocol, runtime_checkable
from copy import copy
from collections import defaultdict
from heapq import nlargest
from operator import itemgetter, attrgetter

from FMTypes import Activation, ADict, epsilon, Pred, as_pred
from Graph import Graph, Node, GraphPropagatorOutgoing
from Propagator import Propagator
from util import as_iter, union


@dataclass(frozen=True)
class NodeA:
    '''Node and activation.'''
    node: Node
    a: float

    def __str__(self):
        try:
            nodestr = self.node.__name__
        except AttributeError:
            nodestr = str(self.node)
        return f'{nodestr:20s} {self.a:2.5f}'

@dataclass
class TyrrellPropagator(GraphPropagatorOutgoing):
    # The formulas that adjust incoming weights come from p. 206 of Toby
    # Tyrrell's doctoral dissertation.
    tyrrell_alpha: float = 0.05
        # constant for reducing influence of edge multiplicity: positive inputs
    tyrrell_beta: float = 0.05
        # constant for reducing influence of edge multiplicity: negative inputs

    def propagate_once(self, g, old_d):
        # Decay.
        new_d: Dict[Node, float] = defaultdict(float,
            ((node, self.clip_a(g, node, a * self.alpha))
                for node, a in old_d.items()
            )
        )
        # TODO Remove nodes with a < epsilon

        # Find max incoming activation and sum of incoming activations for
        # each node.
        maxin_d: Dict[Node, float] = defaultdict(float) # positive influences
        possumin_d: Dict[Node, float] = defaultdict(float)
        minin_d: Dict[Node, float] = defaultdict(float) # negative influences
        negsumin_d: Dict[Node, float] = defaultdict(float)

        for delta in self.make_deltas(g, old_d):
            amt = (
                delta.amt
                * (1.0 + self.positive_feedback_rate
                         * old_d.get(delta.nodeid, 0.0))
                * (1.0 - self.alpha)
            )
            '''
            if delta.nodeid == Before(7) and delta.amt < 0.0:
                print()
                print('DE', delta, '   ', amt)
                print()
            '''
            if amt >= epsilon:
                maxin_d[delta.nodeid] = max(maxin_d[delta.nodeid], amt)
                possumin_d[delta.nodeid] += amt
            elif amt <= -epsilon:
                minin_d[delta.nodeid] = min(minin_d[delta.nodeid], amt)
                negsumin_d[delta.nodeid] += amt

        # Apply the Tyrrell averages of the deltas

        for node in union(maxin_d.keys(), minin_d.keys()):
            #print('PR', node, maxin_d.get(node, 0.0), possumin_d.get(node, 0.0), minin_d.get(node, 0.0), negsumin_d.get(node, 0.0))
            '''
            print('PR1', node, minin_d.get(node, 0.0), negsumin_d.get(node, 0.0), (
                (minin_d.get(node, 0.0)
                  + self.tyrrell_beta * negsumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_beta)
            ))
            '''
            new_a = new_d[node] + (
                (maxin_d.get(node, 0.0)
                  + self.tyrrell_alpha * possumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_alpha)
            ) + (
                (minin_d.get(node, 0.0)
                  + self.tyrrell_beta * negsumin_d.get(node, 0.0))
                /
                (1 + self.tyrrell_beta)
            )
            new_d[node] = self.clip_a(g, node, new_a)
            # TODO Record this in self.flows?

        return self.normalize(new_d)

default_tyrrell_propagator = TyrrellPropagator(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=0.1,
    sigmoid_p=1.5,
    num_iterations=10,
    alpha=0.95,
    tyrrell_alpha=0.05,  # 0.2
    tyrrell_beta=0.1
)

@dataclass
class Slipnet:
    base_graph: Graph
    propagator: Propagator = default_tyrrell_propagator

    def dquery(
        self,
        features: Union[Sequence[Hashable], None]=None,
        activations_in: Union[ADict, None]=None
    ) -> ADict:
        '''Performs the propagation (spreading activation) starting from
        activations_in, and returns the resulting dictionary of activations.'''
        return self.propagator.propagate(
            self.base_graph,
            self.make_activations_in(features, activations_in)
        )

    @classmethod
    def topna(
        cls,
        d: Dict[Node, float],
        pred: Pred=None,
        k: Union[int, None]=1
    ) -> List[NodeA]:
        '''Returns a list of the top k nodes in d, by activation, restricted to
        nodes of 'type' and that pass 'filter'.'''
        pred: Callable[[Any], bool] = as_pred(pred)
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if pred(node)
        ]
        if k is None:
            return sorted(nas, key=attrgetter('a'), reverse=True)
        else:
            return nlargest(k, nas, key=attrgetter('a'))

    @classmethod
    def top(cls, *args, **kwargs) -> List[Node]:
        return [na.node for na in cls.topna(*args, **kwargs)]

    @classmethod
    def top1(cls, *args, **kwargs) -> Union[Node, None]:
        try:
            return cls.top(*args, **kwargs)[0]
        except IndexError:
            return None

    @classmethod
    def make_activations_in(
        cls,
        features: Union[Sequence[Hashable], None]=None,
        activations_in: Union[ADict, None]=None
    ) -> Union[ADict, None]:
        if not features:
            return activations_in
        if activations_in is None:
            activations_in = {}
        else:
            activations_in = copy(activations_in)
        for feature in as_iter(features):
            if isinstance(feature, NodeA):
                activations_in[feature.node] = feature.a
            else:
                activations_in[feature] = max(
                    1.0,
                    activations_in.get(feature, 1.0)
                )
        return activations_in

    @classmethod
    def empty(cls, propagator: Propagator=default_tyrrell_propagator) \
    -> 'Slipnet':
        return Slipnet(Graph.empty(), propagator)
