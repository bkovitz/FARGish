import unittest

from PortGraph import PortGraph, Hop

empty_set = frozenset([])

class TestPortGraph(unittest.TestCase):

    def test_basics(self):
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

    def test_no_dup_tag(self):
        g = PortGraph()
        tens = [g.make_node(Number(10)), g.make_node(Number(10))]
        g.add_tag(SameNumber, tens)
        g.add_tag(SameNumber, tens)
        #TODO  Not sure yet that add_tag shouldn't double-tag
