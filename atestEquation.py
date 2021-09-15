# atestEquation.py -- Acceptance tests related to Equations

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from collections import defaultdict
import operator
from operator import itemgetter, attrgetter
from heapq import nlargest
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Equation import Equation, Operator, plus, times, minus
from Equation import IncreaseOrDecrease, Increase, Decrease, NumOperands, \
    Before, After, MaxBefore, MinBefore
from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of, GraphPropagatorOutgoing
from Propagator import Propagator
from FMTypes import epsilon
from util import as_iter, union, pts, pr


@dataclass
class TyrrellPropagator(GraphPropagatorOutgoing):
    # The formulas that adjust of incoming weights come from p. 206 of Toby
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
            if amt >= epsilon:
                maxin_d[delta.nodeid] = max(maxin_d[delta.nodeid], amt)
                possumin_d[delta.nodeid] += amt
            elif amt <= -epsilon:
                minin_d[delta.nodeid] = min(minin_d[delta.nodeid], amt)
                negsumin_d[delta.nodeid] += amt

        # Apply the Tyrrell averages of the deltas

        for node in union(maxin_d.keys(), minin_d.keys()):
            #print('PR', node, maxin_d.get(node, 0.0), possumin_d.get(node, 0.0), minin_d.get(node, 0.0), negsumin_d.get(node, 0.0))
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

eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
).add_edges(MutualInhibition((Feature, Equation, int), weight=-0.2))
p = TyrrellPropagator(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=1.0,  # higher -> initial features matter more
    sigmoid_p=1.15,  # higher -> sharper distinctions, more salience
    num_iterations=80,
    alpha=0.95,
    tyrrell_alpha=0.1,
    tyrrell_beta=0.05
)

@dataclass(frozen=True)
class NodeA:
    '''Node and activation.'''
    node: Node
    a: float

    def __str__(self):
        return f'{self.node!s:20s} {self.a:2.5f}'

def slipnet_dquery(
    g: Graph,
    p: Propagator,
    features: Iterable[Hashable]=None,
    activations_in: Dict[Hashable, float]=None
) -> Dict[Hashable, float]:
    '''Pass either features or a dictionary of activations.
    Returns dictionary of activations.'''
    if activations_in is None:
        activations_in = {}
        for f in as_iter(features):
            activations_in[f] = 1.0
    #print('DQ', type(activations_in))
    return p.propagate(g, activations_in)

def top(
    d: Dict[Hashable, float],
    type: Type=None,
    k: Union[int, None]=None,
    filter: Union[Callable, None]=None
) -> List[NodeA]:
    if filter is None:
        filter = lambda x: True
    if type is None:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if filter(node)
        ]
    else:
        nas = [
            NodeA(node, a)
                for (node, a) in d.items()
                    if isinstance(node, type) and filter(node)
        ]
    if k is None:
        return sorted(nas, key=attrgetter('a'), reverse=True)
    else:
        return nlargest(k, nas, key=attrgetter('a'))

if __name__ == '__main__':
    out_d = slipnet_dquery(eqn_graph, p, features=[Before(4), After(10)])
    pts(top(out_d, k=30))
    print()
    pts(top(out_d, type=Equation, k=30))
    print()
    pts(top(out_d, type=int, k=30))

    '''
    for eqn in sorted(eqn_graph.query(None), key=str):
        print(eqn, '    ', eqn_graph.degree_out(eqn), eqn_graph.degree_in(eqn))
    '''
