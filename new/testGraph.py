# testGraph.py -- Unit tests for Graph.py

import unittest
from pprint import pprint as pp
import inspect

from dataclasses import dataclass, field
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence, Literal
from itertools import chain

from Graph import Graph, Node, Hop, Hops, Nodes, Edges, EnumNodes, EnumEdges, \
    OfClass, MutualInhibition, Feature, features_of, GraphPropagatorIncoming, \
    GraphPropagatorOutgoing, PrefixedNode, PrefixedGraph, WithPrefix, \
    WithActivations
from Propagator import Propagator
from util import pts, pr


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

class TestGraph(unittest.TestCase):

    def test_alphabet_graph(self) -> None:
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

    def test_enum_graph(self) -> None:
        g = Graph(
            nodes=EnumNodes(['x', 'y', 'z']),
            edges=EnumEdges()
        )
        self.assertTrue(g.has_node('y'))
        self.assertFalse(g.has_node('a'))
        self.assertCountEqual(g.predecessors_of('y'), [])

        g.add_node('a')
        self.assertTrue(g.has_node('a'))

        g.add_hop(Hop('a', 'y'))
        self.assertCountEqual(g.predecessors_of('y'), ['a'])
        self.assertCountEqual(g.successors_of('a'), ['y'])

        g.remove_node('a')
        self.assertFalse(g.has_node('a'))
        self.assertCountEqual(g.predecessors_of('y'), [])
        self.assertCountEqual(g.successors_of('a'), [])

        g.remove_node('a')  # tests that no exception is raised

        g.add_node('a')  # re-adding the node should not re-add its edge
        self.assertCountEqual(g.predecessors_of('y'), [])
        self.assertCountEqual(g.successors_of('a'), [])

    def test_enum_hops(self) -> None:
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

    def test_features(self):
        @dataclass(frozen=True)
        class Parity(Feature):
            name: str

        Even = Parity('Even')
        Odd = Parity('Odd')

        @dataclass(frozen=True)
        class BaseNode:
            n: int

            def features_of(self):
                if self.n & 1:
                    yield Odd
                else:
                    yield Even
                yield self.n

        g = Graph.with_features([BaseNode(1), BaseNode(2), BaseNode(3)])

        self.assertCountEqual(g.query(OfClass(Parity)), [Odd, Even])
        self.assertCountEqual(
            g.hops_to_node(BaseNode(1)),
            [Hop(Odd, BaseNode(1), 1.0),
             Hop(1, BaseNode(1), 1.0),
             Hop(BaseNode, BaseNode(1), 1.0)]
        )
        self.assertCountEqual(
            g.hops_from_node(Odd),
            [Hop(Odd, BaseNode(1), 1.0),
             Hop(Odd, BaseNode(3), 1.0),
             Hop(Odd, Parity, 1.0)]
        )
        self.assertEqual(
            g.find_hop(BaseNode(2), Even),
            Hop(BaseNode(2), Even, 1.0)
        )
        self.assertEqual(g.find_hop(Even, Odd), None)

        # Now add mutual inhibition
        g = g.add_edges(MutualInhibition((Feature, int)))

        self.assertEqual(g.find_hop(Even, Odd), Hop(Even, Odd, -0.2))
        self.assertEqual(g.find_hop(Odd, Even), Hop(Odd, Even, -0.2))
        self.assertEqual(g.find_hop(1, 2), Hop(1, 2, -0.2))

    def test_graph_propagator_incoming(self):
        g = Graph(
            nodes=EnumNodes(['a', 'b', 'o']),
            edges=EnumEdges(Hops.from_pairs(('a', 'b'), ('b', 'o')))
        )
        p = GraphPropagatorIncoming(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total=1.0,
            noise=0.0
        )
        in_d = dict(a=1.0, b=0.5, o=0.0)
        out_d = p.propagate(g, in_d)
        self.assertAlmostEqual(out_d['a'], 0.569072143062159)
        self.assertAlmostEqual(out_d['b'], 0.319848331226608)
        self.assertAlmostEqual(out_d['o'], 0.11107952571123292)

    def test_graph_propagator_outgoing(self):
        g = Graph(
            nodes=EnumNodes(['a', 'b', 'o']),
            edges=EnumEdges(Hops.from_pairs(('a', 'b'), ('b', 'o')))
        )
        p = GraphPropagatorOutgoing(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total=1.0,
            noise=0.0
        )
        in_d = dict(a=1.0, b=0.5) # 'o' is not in dict
        out_d = p.propagate(g, in_d)
        #pts(out_d.items())
        self.assertAlmostEqual(out_d['a'], 0.569072143062159)
        self.assertAlmostEqual(out_d['b'], 0.319848331226608)
        self.assertAlmostEqual(out_d['o'], 0.11107952571123292)

    def test_prefix(self):
        g0 = Graph(
            nodes=EnumNodes(['a', 'b', 'o']),
            edges=EnumEdges(Hops.from_pairs(('a', 'b'), ('b', 'o')))
        )
        g = PrefixedGraph(2, g0)

        # methods that go to Nodes

        self.assertFalse(g.has_node('a'))
        self.assertTrue(g.has_node(PrefixedNode(2, 'a')))
        self.assertFalse(g.has_node(PrefixedNode(1, 'a')))

        self.assertCountEqual(
            g.query(OfClass(str)),
            [
                PrefixedNode(2, 'a'),
                PrefixedNode(2, 'b'),
                PrefixedNode(2, 'o')
            ]
        )

        # methods that go to Edges

        # .hops_from_node() 
        self.assertCountEqual(
            g.hops_from_node('a'),
            []
        )
        self.assertCountEqual(
            g.hops_from_node(PrefixedNode(2, 'a')),
            [Hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b'), 1.0)]
        )
        self.assertCountEqual(
            g.hops_from_node(PrefixedNode(1, 'a')),
            []
        )

        # .hops_to_node()
        self.assertCountEqual(
            g.hops_to_node('b'),
            []
        )
        self.assertCountEqual(
            g.hops_to_node(PrefixedNode(2, 'b')),
            [Hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b'), 1.0)]
        )
        self.assertCountEqual(
            g.hops_to_node(PrefixedNode(1, 'b')),
            []
        )

        # .find_hop()
        self.assertIsNone(g.find_hop('a', 'b'))
        self.assertEqual(
            g.find_hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b')),
            Hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b'), 1.0)
        )
        self.assertEqual(
            g.find_hop(PrefixedNode(1, 'a'), PrefixedNode(2, 'b')),
            None
        )
        self.assertEqual(
            g.find_hop(PrefixedNode(2, 'a'), PrefixedNode(1, 'b')),
            None
        )

        # .degree_out() and .degree_in()
        self.assertEqual(g.degree_out('a'), 0)
        self.assertEqual(g.degree_out(PrefixedNode(1, 'a')), 0)
        self.assertEqual(g.degree_out(PrefixedNode(2, 'a')), 1)
        self.assertEqual(g.degree_in('b'), 0)
        self.assertEqual(g.degree_in(PrefixedNode(1, 'b')), 0)
        self.assertEqual(g.degree_in(PrefixedNode(2, 'b')), 1)

        # .successors_of() and .predecessors_of()
        self.assertCountEqual(g.successors_of('a'), [])
        self.assertCountEqual(g.successors_of(PrefixedNode(1, 'a')), [])
        self.assertCountEqual(
            g.successors_of(PrefixedNode(2, 'a')),
            [PrefixedNode(2, 'b')]
        )
        self.assertCountEqual(
            g.predecessors_of(PrefixedNode(2, 'b')),
            [PrefixedNode(2, 'a')]
        )

        # .hop_weight()
        self.assertEqual(
            g.hop_weight(PrefixedNode(2, 'a'), PrefixedNode(2, 'b')),
            1.0
        )
        self.assertEqual(
            g.hop_weight(PrefixedNode(1, 'a'), PrefixedNode(2, 'b')),
            0.0
        )
        self.assertEqual(
            g.hop_weight(PrefixedNode(2, 'a'), PrefixedNode(1, 'b')),
            0.0
        )

        # .add_edges()

        g2 = g.add_edges(EnumEdges(Hops.from_pairs(
            (PrefixedNode(2, 'a'), PrefixedNode(2, 'o'))
        )))
        self.assertEqual(
            g2.find_hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'o')),
            Hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'o'), 1.0)
        )
        self.assertEqual(
            g2.find_hop('a', 'o'),
            None
        )

    def test_doubled_graph(self):
        g0 = Graph(
            nodes=EnumNodes(['a', 'b', 'o']),
            edges=EnumEdges(Hops.from_pairs(('a', 'b'), ('b', 'o')))
        )
        g = Graph.augment(
            PrefixedGraph(1, g0),
            PrefixedGraph(2, g0)
        )

        self.assertTrue(g.has_node(PrefixedNode(1, 'a')))
        self.assertTrue(g.has_node(PrefixedNode(2, 'a')))
        self.assertFalse(g.has_node(PrefixedNode(3, 'a')))
        self.assertFalse(g.has_node('a'))
        self.assertEqual(
            g.find_hop(PrefixedNode(1, 'a'), PrefixedNode(1, 'b')),
            Hop(PrefixedNode(1, 'a'), PrefixedNode(1, 'b'), 1.0)
        )
        self.assertEqual(
            g.find_hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b')),
            Hop(PrefixedNode(2, 'a'), PrefixedNode(2, 'b'), 1.0)
        )
        self.assertEqual(
            g.find_hop(PrefixedNode(1, 'a'), PrefixedNode(2, 'b')),
            None
        )
        self.assertEqual(
            g.find_hop('a', 'b'),
            None
        )

        # .add_edges() from one PrefixedGraph to the other

        g2 = g.add_edges(EnumEdges(Hops.from_pairs(
            (PrefixedNode(1, 'a'), PrefixedNode(2, 'a'))
        )))
        self.assertEqual(
            g2.find_hop(PrefixedNode(1, 'a'), PrefixedNode(2, 'a')),
            Hop(PrefixedNode(1, 'a'), PrefixedNode(2, 'a'), 1.0)
        )

        self.assertCountEqual(
            g2.query(OfClass(str)),
            [
                PrefixedNode(1, 'a'),
                PrefixedNode(1, 'b'),
                PrefixedNode(1, 'o'),
                PrefixedNode(2, 'a'),
                PrefixedNode(2, 'b'),
                PrefixedNode(2, 'o')
            ]
        )
        self.assertCountEqual(
            g2.query(WithPrefix(2, OfClass(str))),
            [
                PrefixedNode(2, 'a'),
                PrefixedNode(2, 'b'),
                PrefixedNode(2, 'o')
            ]
        )
        self.assertCountEqual(
            g0.query(WithPrefix(2, OfClass(str))),
            []
        )

    def test_with_activations(self):
        @dataclass
        class GA(WithActivations, Graph):
            propagator: Propagator = GraphPropagatorOutgoing

        g = GA.empty()

        # a non-existent node
        self.assertEqual(g.a('a'), 0.0)
        g.boost('a')
        self.assertEqual(g.a('a'), 0.0)
        g.set_a('a', 2.0)
        self.assertEqual(g.a('a'), 0.0)

        # the same, with a node that exists
        g.add_node('a')
        self.assertEqual(g.a('a'), 0.0)
        g.boost('a')
        self.assertEqual(g.a('a'), 0.5)
        g.set_a('a', 2.0)
        self.assertEqual(g.a('a'), 2.0)

        # NEXT set up some edges and propagate

        g.add_node('b')
        g.add_node('o')
        g.add_hop(Hop('a', 'b'))
        g.add_hop(Hop('b', 'o'))
        g.set_a('a', 1.0)
        g.set_a('b', 0.5)
        # We don't set the activation of 'o'; it defaults to 0.0

        g.propagate()
        for node in ['a', 'b', 'o']:
            print(g.a(node))


"""
if __name__ == '__main__':
    # TODO mv this someplace where make_equations() is defined
    g = Graph.with_features(make_equations(1, 20, {plus, minus, times})) \
             .add_edges(MutualInhibition(Feature))
"""
