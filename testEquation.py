# testEquation.py -- Unit tests for Equation.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Equation import Equation, Operator, plus, times, minus

from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of
from util import pts, pr


class TestEquation(unittest.TestCase):

    def test_equation(self):
        e = Equation.make([3, 5], times)
        self.assertEqual(e.result, 15)
        self.assertEqual(str(e), '3 x 5 = 15')

#NEXT
# test features
# test mutual inh
