# testGraph2.py -- Unit tests for Graph2.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Graph2 import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges


class AlphabetNodes:

    def has_node(self, x):
        try:
            return len(x) == 1 and x >= 'a' and x <= 'z'
        except TypeError:
            return False

class AlphabetEdges:

    def hops_from_node(self, nodes, x):
        try:
            if nodes.has_node(x) and x < 'z':
                succ = chr(ord(x) + 1)
                yield Hop(x, succ, 1.0)
        except TypeError:
            pass

    def hops_to_node(self, nodes, x):
        try:
            if nodes.has_node(x) and x > 'a':
                pred = chr(ord(x) - 1)
                yield Hop(pred, x, 1.0)
        except TypeError:
            pass

    def find_hop(self, nodes, from_node, to_node):
        try:
            if (
                nodes.has_node(from_node)
                and
                nodes.has_node(to_node)
                and
                ord(from_node) + 1 == ord(to_node)
            ):
                return Hop(from_node, to_node, 1.0)
        except TypeError:
            return None

def AlphabetGraph():
    '''A graph consisting of nodes for 'a'..'z', with an edge from each letter
    to its immediate successor. Illustrates how to make a virtual graph,
    where every node and edge as computed as it searched for, rather than
    each being represented explicitly by a separate object.'''
    return Graph(nodes=AlphabetNodes(), edges=AlphabetEdges())

'''
def Graph123():
    return Graph(
        nodes=LiteralNodes(frozenset([1, 2, 3])),
        edges=WantEdges({1: {'a'}, 2: {'b'}, 3: {'c'}})
    )
'''

class TestGraph(unittest.TestCase):

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

        '''
        self.assertEqual(g.hop_weight('a', 'b'), 1.0)
        self.assertEqual(g.hop_weight('b', 'a'), 0.0)
        self.assertEqual(g.hop_weight(1, 'a'), 0.0)
        '''

        self.assertTrue(g.find_hop('a', 'b'))
        self.assertFalse(g.find_hop('a', 'c'))
        self.assertEqual(g.find_hop('a', 'b'), Hop('a', 'b', 1.0))

    def test_enum_graph(self):
        g = Graph(
            nodes=EnumNodes(['x', 'y', 'z']),
            edges=EnumEdges()
        )
        self.assertTrue(g.has_node('y'))
        self.assertFalse(g.has_node('a'))
        self.assertCountEqual(g.predecessors_of('y'), [])

    def test_enum_hops(self):
        g = Graph(
            nodes=EnumNodes(['x', 'y', 'z']),
            edges=EnumEdges(Hops.from_dict(x={'y', 'z'}))
        )
        self.assertTrue(g.has_node('y'))
        self.assertFalse(g.has_node('a'))
        self.assertCountEqual(g.predecessors_of('y'), ['x'])
        self.assertCountEqual(g.predecessors_of('x'), [])

    def test_augment(self):
        g1 = AlphabetGraph()
        g2 = Graph(
            nodes=EnumNodes(range(1, 4)),
            edges=EnumEdges(Hops.from_pairs((1, 'a'), (2, 'b'), (3, 'c')))
        )
        g = Graph.augment(g2, g1)

        self.assertTrue(g2.has_node(1))
        self.assertFalse(g2.has_node('a'))
        self.assertFalse(g2.find_hop(1, 'a'))
        self.assertCountEqual(g2.successors_of(2), [])
        self.assertCountEqual(g2.predecessors_of('c'), [])

        self.assertTrue(g.has_node(1))
        self.assertTrue(g.has_node('a'))
        self.assertTrue(g.find_hop(1, 'a'))
        self.assertCountEqual(g.successors_of(2), ['b'])
        self.assertCountEqual(g.predecessors_of('b'), [2, 'a'])
