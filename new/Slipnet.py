# Slipnet.py -- Spreading activation on long-term memory, possibly augmented
#               by additional nodes and edges from the workspace

from dataclasses import dataclass, field, InitVar, replace
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterator, \
    Iterable, Any, NewType, Type, ClassVar, Sequence, Callable, Hashable, \
    Collection, Sequence, Literal, Protocol, Optional, TypeVar, IO, \
    runtime_checkable
from copy import copy
from collections import defaultdict
from heapq import nlargest
from operator import itemgetter, attrgetter
from math import copysign

from FMTypes import Activation, ADict, epsilon, Pred, as_pred
from Graph import Graph, Node
from Propagator import Propagator, ActivationLog, PropagatorOutgoing, \
    Delta, SentA
from util import as_iter, as_list, union, pr


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
class TyrrellPropagator(PropagatorOutgoing):
    # The formulas that adjust incoming weights come from p. 206 of Toby
    # Tyrrell's doctoral dissertation.
    tyrrell_alpha: float = 0.05
        # constant for reducing influence of edge multiplicity: positive inputs
    tyrrell_beta: float = 0.05
        # constant for reducing influence of edge multiplicity: negative inputs

    def sentas_to_deltas(self, sentas: Iterable[SentA]) \
    -> Iterable[Delta]:
        # Find max incoming activation and sum of incoming activations for
        # each node.
        maxin_d: Dict[Node, float] = defaultdict(float) # positive influences
        possumin_d: Dict[Node, float] = defaultdict(float)
        minin_d: Dict[Node, float] = defaultdict(float) # negative influences
        negsumin_d: Dict[Node, float] = defaultdict(float)

        for senta in sentas:
            if senta.a >= 0:
                maxin_d[senta.to_node] = max(maxin_d[senta.to_node], senta.a)
                possumin_d[senta.to_node] += senta.a
            elif senta.a < 0:
                minin_d[senta.to_node] = min(minin_d[senta.to_node], senta.a)
                negsumin_d[senta.to_node] += senta.a

        # Make Deltas from the Tyrrell averages of the SentA's
        multiplier = 1.0 - self.alpha
        for node in union(maxin_d.keys(), minin_d.keys()):
            amt = multiplier * (
                (
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
            )
            if abs(amt) >= epsilon:
                yield Delta(node, amt)

    """
    # TODO Move this to a mix-in
    def adjust_deltas(self, g: Graph, deltas: Iterable[Delta]) \
    -> Iterable[Delta]:
        deltas: List[Delta] = as_list(deltas)
        mx = max((delta.amt for delta in deltas), default=0.0)
        def adj(amt: float) -> float:
            # max delta becomes 0.1; highest deltas get space between them;
            # lowest deltas get pushed to zero.
            return copysign(((amt / mx) ** 30) * 0.1, amt)
        for delta in deltas:
            new_amt = adj(delta.amt)
            if abs(new_amt) >= epsilon:
                yield replace(delta, amt=new_amt)
    """

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

    @property
    def nodes(self):
        return self.base_graph.nodes

    def has_node(self, x: Any) -> bool:
        return self.base_graph.has_node(x)

    def neighbors(self, node: Node) -> Set[Node]:
        return self.base_graph.neighbors(node)

    def set_params(self, num_iterations: Optional[int]=None) -> None:
        if num_iterations is not None:
            self.propagator.num_iterations = num_iterations

    def dquery(
        self,
        features: Union[Sequence[Hashable], None]=None,
        activations_in: Union[ADict, None]=None,
        alog: Optional[ActivationLog]=None
    ) -> ADict:
        '''Performs the propagation (spreading activation) starting from
        activations_in, and returns the resulting dictionary of activations.'''
        return self.propagator.propagate(
            self.base_graph,
            self.make_activations_in(features, activations_in),
            alog=alog
        )

    def __len__(self) -> int:
        return len(self.base_graph)

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
        #print('TOPNA', k, pred, nas)
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
