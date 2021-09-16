# testEquation.py -- Unit tests for Equation.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Equation import Equation, Operator, plus, times, minus
from Equation import IncreaseOrDecrease, Increase, Decrease, NumOperands, \
    Before, After, MaxBefore, MinBefore

from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of
from util import pts, pr


class TestEquation(unittest.TestCase):

    def test_equation(self):
        e = Equation.make([3, 5], times)
        self.assertEqual(e.result, 15)
        self.assertEqual(str(e), '3 x 5 = 15')

    def test_features(self):
        e = Equation.make([3, 4], plus)
        self.assertCountEqual(
            features_of(e),
            [plus, 3, 4, 7, Before(3), Before(4), After(7), Increase,
             MaxBefore(4), MinBefore(3), NumOperands(2), Equation
            ]
        )

    def test_features_graph(self):
        e = Equation.make([3, 4], plus)
        g = Graph.with_features([e])
        #print(type(g.edges))
        #pr(g.edges)
        self.assertTrue(g.has_node(NumOperands(2)))
        self.assertTrue(g.has_node(2))

        self.assertEqual(g.hop_weight(e, plus), 1.0)
        self.assertEqual(g.hop_weight(plus, e), 1.0)
        self.assertEqual(g.hop_weight(e, 2), 0.0)

    def test_equation_table(self):
        g = Graph.with_features(
            Equation.make_table(
                range(1, 11), range(1, 11), [plus, minus, times]
            )
        ).add_edges(MutualInhibition(Feature))
        self.assertEqual(g.hop_weight(Before(2), Before(3)), -0.2)
        # TODO Mutual inhibition between numbers

    def test_pons_asinorum(self):
        #TODO
        pass


#NEXT
# ?test mutual inh
#   two Equations
#   add MutualInhibition
#   test for inh across Equation features
#  
# make Propagator work with Graph2  DONE
#
# test backwash
#
# test doubled graph
