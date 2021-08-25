# testGraph.py -- Unit tests for Graph classes

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal

from Graph import Graph, Node, Hop, LiteralGraph, WantEdges


class AlphabetGraph(Graph):
    '''Each node is a letter in a..z; each letter except z has an edge leading
    to the next letter. Demonstrates a minimal 'virtual graph': the nodes
    and edges are computed on the fly rather than stored as explicit node
    objects. With this approach, you could easily make an infinite graph.'''

    def all_nodes(self):
        for c in range(ord('a'), ord('z') + 1):
            yield chr(c)

    def has_node(self, x):
        print('ALPHAS', x)
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

    def find_hop(self, from_node, to_node):
        if self.has_node(from_node) and from_node < 'z':
            try:
                if ord(from_node) + 1 == ord(to_node):
                    return Hop(from_node, to_node, 1.0)
            except TypeError:
                return None

    def __len__(self):
        return 26

@dataclass
class Graph123(WantEdges, LiteralGraph):
    '''Graph123 contains nodes 1, 2, and 3, which want to link to 'a', 'b',
    and 'c' respectively, but Graph123 does not contain 'a', 'b', or 'c'.
    If Graph123 is augmented onto a Graph with those nodes, the edges will
    appear automatically.'''

    def __init__(self):
        super().__init__(
            literals=[1, 2, 3], want_edges={1: {'a'}, 2: 'b', 3: 'c'}
        )
    


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

        self.assertEqual(g.hop_weight('a', 'b'), 1.0)
        self.assertEqual(g.hop_weight('b', 'a'), 0.0)
        self.assertEqual(g.hop_weight(1, 'a'), 0.0)

        self.assertTrue(g.find_hop('a', 'b'))
        self.assertFalse(g.find_hop('a', 'c'))
        self.assertEqual(g.find_hop('a', 'b'), Hop('a', 'b', 1.0))
        '''
        self.assertTrue(g.has_edge('a', 'b'))
        self.assertTrue(g.has_hop('a', 'b'))
        self.assertFalse(g.has_edge('a', 'c'))
        self.assertFalse(g.has_hop('a', 'c'))
        '''

        self.assertEqual(len(g), 26)

        self.assertCountEqual(g.all_nodes(),
            list('abcdefghijklmnopqrstuvwxyz')
        )

        self.assertCountEqual(g.nodes(['e', 'm', 'v']), list('emv'))

    def test_literal_graph(self):
        g = LiteralGraph(['x', 'y', 'z'])
        self.assertCountEqual(g.all_nodes(), list('xyz'))
        self.assertEqual(len(g), 3)
        self.assertTrue(g.has_node('y'))
        self.assertFalse(g.has_node('a'))
        self.assertCountEqual(g.predecessors_of('y'), [])

    def test_augment_graph(self):
        g1 = AlphabetGraph()
        g2 = Graph123()
        g = Graph.augment(g2, g1)

        self.assertTrue(g2.has_node(1))
        self.assertFalse(g2.has_node('a'))
        self.assertFalse(g2.find_hop(1, 'a'))

        print('UT', list(g1.all_nodes()), list(g2.all_nodes()))
        self.assertTrue(g.has_node(1))
        self.assertTrue(g.has_node('a'))
        self.assertCountEqual(g.all_nodes(),
            [1, 2, 3] + list('abcdefghijklmnopqrstuvwxyz')
        )
        self.assertTrue(g.find_hop(1, 'a'))
