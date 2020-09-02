import unittest

from PortGraph import PortGraph, Hop
from testNodeClasses import *

empty_set = frozenset([])

class TestPortGraph(unittest.TestCase):

    def test_port_graph(self):
        g = PortGraph()
        g.add_nodes_from(['A', 'B', 'O'])
        g.add_edge('A', 'in', 'B', 'out')
        g.add_edge('O', 'members', 'A', 'member_of')

        self.assertCountEqual(g.neighbors('A'),        ['B', 'O'])
        self.assertCountEqual(g.neighbors('A', 'in'),  ['B'])
        self.assertCountEqual(g.neighbors('B', 'out'), ['A'])
        self.assertCountEqual(g.hops_to_neighbor('A', 'B'),
                         [Hop('A', 'in', 'B', 'out', 0)])
        self.assertCountEqual(g.hops_to_neighbor('B', 'A'),
                         [Hop('B', 'out', 'A', 'in', 0)])
        self.assertCountEqual(g.hops_from_port('A', 'in'),
                         [Hop('A', 'in', 'B', 'out', 0)])
        self.assertCountEqual(g.hops_from_port('B', 'out'),
                         [Hop('B', 'out', 'A', 'in', 0)])
        self.assertEqual(g.find_hop('A', 'in', 'B', 'out'),
                         Hop('A', 'in', 'B', 'out', 0))

        g.remove_edge('A', 'in', 'B', 'out')

        self.assertCountEqual(g.neighbors('A'),             ['O'])
        self.assertCountEqual(g.neighbors('A', 'in'),       empty_set)
        self.assertCountEqual(g.neighbors('B', 'out'),      empty_set)
        self.assertCountEqual(g.hops_to_neighbor('A', 'B'), empty_set)
        self.assertCountEqual(g.hops_to_neighbor('B', 'A'), empty_set)
        self.assertCountEqual(g.hops_from_port('A', 'in'),  empty_set)
        self.assertCountEqual(g.hops_from_port('B', 'out'), empty_set)
        self.assertEqual(g.find_hop('A', 'in', 'B', 'out'), None)

#    def test_no_dup_tag(self):
#        g = PortGraph()
#        tens = [g.make_node(Number(10)), g.make_node(Number(10))]
#        g.add_tag(SameNumber, tens)
#        g.add_tag(SameNumber, reversed(tens))
#        #TODO  Not sure yet that add_tag shouldn't double-tag

    def test_nodes(self):
        g = PortGraph()
        a = g.mknode('A')
        b = g.mknode('B')
        g.add_edge(a, 'in', b, 'out')
        # g.nodes is all the nodeids in g
        self.assertCountEqual([1, 2], g.nodes)
        # g.edges is a set of tuples (nodeid, nodeid, key)
        self.assertCountEqual([(1, 2, 0)], g.edges)
        # In a PortGraph, you're probably more interested in hops than edges:
        self.assertCountEqual([Hop(a, 'in', b, 'out', 0)], list(g.all_hops()))

    def test_partition_nodes(self):
        g = PortGraph()
        n1 = g.make_node(Number(1))
        n2 = g.make_node(Number(2))
        n3 = g.make_node(Number(3))
        n4 = g.make_node(Number(4))
        even, odd = g.partition_nodes(g.nodes, is_even)
        self.assertCountEqual(even, [n2, n4])
        self.assertCountEqual(odd, [n1, n3])

    def test_seed(self):
        # This shows how to get the value of a graph attribute.
        g = PortGraph(seed=25)
        self.assertEqual(g.graph['seed'], 25)

def is_even(g, nodeid):
    return g.datum(nodeid).value & 1 == 0
