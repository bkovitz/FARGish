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
             MaxBefore(4), MinBefore(3), NumOperands(2)
            ]
        )

    def test_features_graph(self):
        e = Equation.make([3, 4], plus)
        g = Graph.with_features([e])
        #print(type(g.edges))
        #pr(g.edges)
        self.assertTrue(g.has_node(NumOperands(2)))
        self.assertTrue(g.has_node(2))

#NEXT
# secondary features
#  Why do we need secondary features? To link positively betw primary and
#  secondary feature.
# test mutual inh
