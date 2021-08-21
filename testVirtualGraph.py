# testVirtualGraph.py -- Unit tests for VirtualGraph

import unittest
from pprint import pprint as pp
import inspect

from VirtualGraphPrimitives import VirtualGraphPrimitives, Hop

class AlphabetGraph(VirtualGraphPrimitives):

    def has_node(self, x):
        try:
            return len(x) == 1 and x >= 'a' and x <= 'z'
        except TypeError:
            return False

    def successors_of(self, x):
        try:
            if self.has_node(x) and x < 'z':
                yield chr(ord(x) + 1)
        except TypeError:
            pass

    def predecessors_of(self, x):
        try:
            if x > 'a':
                yield chr(ord(x) - 1)
        except TypeError:
            pass


class TestVirtualGraph(unittest.TestCase):

    def test_alphabet_graph(self):
        g = AlphabetGraph()
        self.assertTrue(g.has_node('c'))
        self.assertFalse(g.has_node('A'))
        self.assertFalse(g.has_node(1))

        self.assertCountEqual(g.successors_of('a'), ['b'])
        self.assertCountEqual(g.successors_of('A'), [])
        self.assertCountEqual(g.successors_of('z'), [])
        self.assertCountEqual(g.successors_of(1), [])

        self.assertCountEqual(g.predecessors_of('a'), [])
        self.assertCountEqual(g.predecessors_of('A'), [])
        self.assertCountEqual(g.predecessors_of('z'), ['y'])
        self.assertCountEqual(g.predecessors_of(1), [])

        self.assertCountEqual(g.hops_from_node('a'),
            [Hop('a', 'b', 1.0)]
        )
        self.assertCountEqual(g.hops_to_node('g'),
            [Hop('f', 'g', 1.0)]
        )
