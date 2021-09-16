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
from time import process_time

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
    positive_feedback_rate=1.5,  # higher -> initial features matter more
    sigmoid_p=1.25,  # higher -> sharper distinctions, more salience
    num_iterations=10,
    alpha=0.95,
    tyrrell_alpha=0.05,
    tyrrell_beta=0.1
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

def topna(
    d: Dict[Node, float],
    type: Type=None,
    k: Union[int, None]=1,
    filter: Union[Callable, None]=None
) -> List[NodeA]:
    '''Returns a list of the top k nodes in d, by activation, restricted to
    nodes of 'type' and that pass 'filter'.'''
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

def top(*args, **kwargs) -> List[Node]:
    return [na.node for na in topna(*args, **kwargs)]

def top1(*args, **kwargs) -> Union[Node, None]:
    try:
        return top(*args, **kwargs)[0]
    except IndexError:
        return None

class ATestEquation(unittest.TestCase):

    p1 = TyrrellPropagator(
        max_total=10.0,
        noise=0.0,
        positive_feedback_rate=1.5,
        sigmoid_p=1.25,
        num_iterations=10,
        alpha=0.95,
        tyrrell_alpha=0.05,
        tyrrell_beta=0.1
    )

    def test_4_10(self):
        t0 = process_time()
        out_d = slipnet_dquery(
            eqn_graph, self.p1, features=[Before(4), After(10)]
        )
        elapsed = process_time() - t0
        self.assertEqual(
            top1(out_d, type=Equation),
            Equation.make([6, 4], plus)
        )
        self.assertCountEqual(
            top(out_d, type=int, k=3),
            [6, 4, 10]
        )
        self.assertLessEqual(elapsed, 5.0)

    def test_backwash(self):
        # Pulsing an Equation should activate the nodes for its main
        # features.
        out_d = slipnet_dquery(
            eqn_graph, self.p1, features=[Equation.make([6, 4], plus)]
        )
        self.assertCountEqual(
            top(out_d, type=int, k=3),
            [6, 4, 10]
        )
        self.assertEqual(
            top1(out_d, type=After),
            After(10)
        )
        self.assertCountEqual(
            top(out_d, type=Before, k=2),
            [Before(6), Before(4)]
        )
        

def run(features, p=p, g=eqn_graph, k=30):
    print('Input:')
    pr(features)
    print()
    t0 = process_time()
    out_d = slipnet_dquery(eqn_graph, p, features=features)
    t1 = process_time()
    pts(topna(out_d, k=k))
    print()
    pts(topna(out_d, type=Equation, k=k))
    print()
    pts(topna(out_d, type=int, k=k))
    print()
    print(f'{t1 - t0:1.3f} sec')

if __name__ == '__main__':
    #run([Before(4), After(10)])
    run([Equation.make([6, 4], plus)])

    '''
    for eqn in sorted(eqn_graph.query(None), key=str):
        print(eqn, '    ', eqn_graph.degree_out(eqn), eqn_graph.degree_in(eqn))
    '''
