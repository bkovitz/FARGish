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
from itertools import chain

from Equation import Equation, Operator, plus, times, minus
from Equation import IncreaseOrDecrease, Increase, Decrease, NumOperands, \
    Before, After, MaxBefore, MinBefore
from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of, GraphPropagatorOutgoing, \
    PrefixedGraph, PrefixedNode, WithPrefix
from Propagator import Propagator
from FMTypes import epsilon
from util import as_iter, as_dict, union, pts, pr


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

eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Equation, int), weight=-0.2))
p = TyrrellPropagator(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=0.0, #1.5,  # higher -> initial features matter more
    sigmoid_p=1.2,  #1.5 higher -> sharper distinctions, more salience
    num_iterations=10,
    alpha=0.95,
    tyrrell_alpha=0.05,  # 0.2
    tyrrell_beta=0.1
)
p1a = TyrrellPropagator(  # Makes 6+4=10 just barely win
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=0.0, #1.5,  # higher -> initial features matter more
    sigmoid_p=1.2,  #1.5 higher -> sharper distinctions, more salience
    num_iterations=50,
    alpha=0.95,
    tyrrell_alpha=0.05,  # 0.2
    tyrrell_beta=0.1
)
p1 = TyrrellPropagator(
    max_total=100.0,
    noise=0.0,
    positive_feedback_rate=0.0, #1.5,  # higher -> initial features matter more
    sigmoid_p=1.2,  #1.5 higher -> sharper distinctions, more salience
    num_iterations=20,
    alpha=0.95,
    tyrrell_alpha=0.02,  # 0.2
    tyrrell_beta=0.1
)
p2 = GraphPropagatorOutgoing(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=1.5,  # higher -> initial features matter more
    sigmoid_p=1.5,  # higher -> sharper distinctions, more salience
    num_iterations=10,
    alpha=0.95,
)

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
            if isinstance(f, NodeA):
                a = f.a
                f = f.node
            else:
                a = 1.0
            activations_in[f] = a
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
        sigmoid_p=1.5,
        num_iterations=10,
        alpha=0.95,
        tyrrell_alpha=0.2,
        tyrrell_beta=0.1
    )

    def test_4_10(self):
        t0 = process_time()
        out_d = slipnet_dquery(
            eqn_graph, self.p1, features=[Before(4), After(10), Equation]
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
    pr(as_dict(p))
    print()
    print('Input:')
    pr(features)
    print()
    t0 = process_time()
    out_d = slipnet_dquery(g, p, features=features)
    t1 = process_time()
    pts(topna(out_d, k=k))
    print()
    pts(topna(out_d, type=Equation, k=k))
    print()
    pts(topna(out_d, type=int, k=k))
    print()
    pts(topna(out_d, type=Before, k=20))
    print()
    pts(topna(out_d, type=After, k=20))
    print()
    pts(topna(out_d, type=Type, k=20))
    print()
    #print(out_d[NumOperands(2)])
    print(out_d[PrefixedNode(1, After(10))])
    print(out_d[PrefixedNode(2, Before(10))])
    print(f'{t1 - t0:1.3f} sec')

if __name__ == '__main__':
    #run([Before(4), After(10), Equation], p=p1)
    #run([Before(4), Before(6), Equation, After], p=p1)
    #run([Equation.make([6, 4], plus)], p=p)
    #run([Before(4), Before(5), Before(6), After(15), NodeA(After, 10.0), Equation], p=p1)
    #run([Before(4), Before(5), Before(6), After(15), After, Equation], p=p1)
    #run([Before(4), Before(5), Before(6), After(15)])
    #run([4, 5, 6, 15])

    g2 = Graph.augment(
        PrefixedGraph(1, eqn_graph),
        PrefixedGraph(2, eqn_graph),
    )
    hops = []
    for p_after in g2.query(WithPrefix(1, OfClass(After))):
        hops.append(Hop(
            p_after,
            PrefixedNode(2, Before(p_after.unprefixed().x)),
            weight=100.0
        ))
        hops.append(Hop(
            PrefixedNode(2, Before(p_after.unprefixed().x)),
            p_after,
            weight=0.0
        ))

    '''
    g2 = g2.add_edges(EnumEdges(Hops.from_pairs(chain.from_iterable([
        [(p_after, PrefixedNode(2, Before(p_after.unprefixed().x))),
         (PrefixedNode(2, Before(p_after.unprefixed().x)), p_after)]
            for p_after in g2.query(WithPrefix(1, OfClass(After)))
    ]), weight=1000.0)))
    '''
    g2 = g2.add_edges(EnumEdges(hops))

    run([
        PrefixedNode(1, Before(4)),
        PrefixedNode(1, Before(5)),
        PrefixedNode(1, Before(6)),
        PrefixedNode(1, After(15)),
        PrefixedNode(1, Equation),
        PrefixedNode(2, Before(4)),
        PrefixedNode(2, Before(5)),
        PrefixedNode(2, Before(6)),
        PrefixedNode(2, After(15)),
        PrefixedNode(2, Equation),
    ], g=g2, k=None, p=p1)
    print()
    pts(g2.hops_from_node(PrefixedNode(1, After(10))))

    # NEXT
    # custom features_of:
    #   UT for custom features of an int
    #
    #   DivisibleBy(10)
    #   NumDigits(1, 2)
    #   FirstDigit
    #   LastDigit
    #   Before1
    #   Before2
    # Can we pulse just the numbers and see which features activate, in order
    # to glom based on those features or activate those features when pulsing
    # to solve the numble?
    #
    # How can the slipnet model "this equation is special because..."?
    #
    # "Doubling": two slipnets, joined by some plumbing so that After(*) in
    # the first slipnet links to Before(*) in the second slipnet.

    '''
    for eqn in sorted(eqn_graph.query(None), key=str):
        print(eqn, '    ', eqn_graph.degree_out(eqn), eqn_graph.degree_in(eqn))
    '''
