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
from inspect import isclass

from Equation import Equation, Operator, plus, times, minus
from Equation import IncreaseOrDecrease, Increase, Decrease, NumOperands, \
    Before, After, MaxBefore, MinBefore
from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of, GraphPropagatorOutgoing, \
    PrefixedGraph, PrefixedNode, WithPrefix
from Slipnet2 import Slipnet, NodeA, TyrrellPropagator, \
    default_tyrrell_propagator
from FMTypes import epsilon
from util import as_iter, as_dict, union, pts, pr


eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Operator, Equation, int), weight=-5.0))
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

# Somewhat passable winners in doubled-graph experiment with one-way After->
# Before bridge with weight 100.0, but by awfully tiny margins
p1b = TyrrellPropagator(
    max_total=100.0,
    noise=0.0,
    positive_feedback_rate=0.0, #1.5,  # higher -> initial features matter more
    sigmoid_p=1.2,  #1.5 higher -> sharper distinctions, more salience
    num_iterations=20,
    alpha=0.95,
    tyrrell_alpha=0.02,  # 0.2
    tyrrell_beta=0.1
)

p1 = TyrrellPropagator(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=10.0,  # higher -> initial features matter more
    sigmoid_p=1.2,  #0.99,  #1.5 higher -> sharper distinctions, more salience
    num_iterations=10,
    alpha=0.95,
    tyrrell_alpha=0.05,  #0.002,  # 0.2
    tyrrell_beta=0.02
)
p2 = GraphPropagatorOutgoing(
    max_total=10.0,
    noise=0.0,
    positive_feedback_rate=1.5,  # higher -> initial features matter more
    sigmoid_p=1.5,  # higher -> sharper distinctions, more salience
    num_iterations=10,
    alpha=0.95,
)

class ATestEquation(unittest.TestCase):

    '''
    p1 = TyrrellPropagator(
        max_total=10.0,
        noise=0.0,
        positive_feedback_rate=0.1,
        sigmoid_p=1.5,
        num_iterations=10,
        alpha=0.95,
        tyrrell_alpha=0.02,
        tyrrell_beta=0.1
    )
    '''
    sl = Slipnet(eqn_graph, propagator=default_tyrrell_propagator)

    def test_4_10(self):
        t0 = process_time()
        out_d = self.sl.dquery(features=[Before(4), After(10), Equation])
        elapsed = process_time() - t0
        self.assertEqual(
            self.sl.top1(out_d, pred=Equation),
            Equation.make([6, 4], plus)
        )
        self.assertCountEqual(
            self.sl.top(out_d, pred=int, k=3),
            [6, 4, 10]
        )
        self.assertLessEqual(elapsed, 5.0)

    def test_backwash(self):
        # Pulsing an Equation should activate the nodes for its main
        # features.
        out_d = self.sl.dquery(features=[Equation.make([6, 4], plus)])
        self.assertCountEqual(
            self.sl.top(out_d, pred=int, k=3),
            [6, 4, 10]
        )
        self.assertEqual(
            self.sl.top1(out_d, pred=After),
            After(10)
        )
        self.assertCountEqual(
            self.sl.top(out_d, pred=Before, k=2),
            [Before(6), Before(4)]
        )
        
sl = Slipnet.empty()

def run(features, p=p, g=eqn_graph, k=30):
    global sl
    sl = Slipnet(g, p)
    pr(as_dict(p))
    print()
    print('Input:')
    pr(features)
    print()
    t0 = process_time()
    out_d = sl.dquery(features=features)
    t1 = process_time()
    pts(sl.topna(out_d, k=k))
    print()
    pts(sl.topna(out_d, pred=Equation, k=k))
    print()
    pts(sl.topna(out_d, pred=int, k=k))
    print()
    pts(sl.topna(out_d, pred=Before, k=20))
    print()
    pts(sl.topna(out_d, pred=After, k=20))
    print()
    pts(sl.topna(out_d, pred=isclass, k=20))
    print()
    pts(sl.topna(out_d, pred=Operator, k=20))
    print()
    #print(out_d[NumOperands(2)])
    #print(out_d[PrefixedNode(1, After(10))])
    #print(out_d[PrefixedNode(2, Before(10))])
    print(f'{t1 - t0:1.3f} sec')

if __name__ == '__main__':
    run([Before(4), After(10), Equation], p=p1)
    #run([Before(4), Before(6), Equation, After], p=p1)
    #run([Equation.make([6, 4], plus)], p=p)
    #run([Before(4), Before(5), Before(6), After(15), NodeA(After, 10.0), Equation], p=p1)
    #run([Before(4), Before(5), Before(6), After(15), After, Equation], p=p1)
    #run([Before(4), Before(5), Before(6), After(15)])
    #run([4, 5, 6, 15])

    """
    g2 = Graph.augment(
        PrefixedGraph(1, eqn_graph),
        PrefixedGraph(2, eqn_graph),
    )
    hops = []
    for p_after in g2.query(WithPrefix(1, OfClass(After))):
        hops.append(Hop(
            p_after,
            PrefixedNode(2, Before(p_after.unprefixed().x)),
            weight=10.0
        ))
        hops.append(Hop(
            PrefixedNode(2, Before(p_after.unprefixed().x)),
            p_after,
            weight=10.0
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
        #PrefixedNode(1, After(15)),
        #PrefixedNode(1, After),
        PrefixedNode(1, Equation),
        PrefixedNode(2, Before(4)),
        PrefixedNode(2, Before(5)),
        PrefixedNode(2, Before(6)),
        PrefixedNode(2, After(15)),
        PrefixedNode(2, Equation),
    ], g=g2, k=None, p=p1)
    print()
    pts(g2.hops_from_node(PrefixedNode(1, After(10))))
    """

    # NEXT
    # Per-node sigmoid function
    #
    # Visualizations
    #
    # Better querying for type= argument.
    #
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
