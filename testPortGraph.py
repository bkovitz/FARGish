import unittest

from PortGraph import PortGraph, Hop

empty_set = frozenset([])

class TestPortGraph(unittest.TestCase):

    def test_basics(self):
        g = PortGraph()
        g.add_edge('A', 'in', 'B', 'out')
        g.add_edge('O', 'members', 'A', 'member_of')

        self.assertEqual(set(g.neighbors('A')),        set(['B', 'O']))
        self.assertEqual(set(g.neighbors('A', 'in')),  set(['B']))
        self.assertEqual(set(g.neighbors('B', 'out')), set(['A']))
        self.assertEqual(set(g.hops_to_neighbor('A', 'B')),
                         set([Hop('A', 'in', 'B', 'out', 0)]))
        self.assertEqual(set(g.hops_to_neighbor('B', 'A')),
                         set([Hop('B', 'out', 'A', 'in', 0)]))
        self.assertEqual(set(g.hops_from_port('A', 'in')),
                         set([Hop('A', 'in', 'B', 'out', 0)]))
        self.assertEqual(set(g.hops_from_port('B', 'out')),
                         set([Hop('B', 'out', 'A', 'in', 0)]))

        g.remove_edge('A', 'in', 'B', 'out')

        self.assertEqual(set(g.neighbors('A')),             set(['O']))
        self.assertEqual(set(g.neighbors('A', 'in')),       empty_set)
        self.assertEqual(set(g.neighbors('B', 'out')),      empty_set)
        self.assertEqual(set(g.hops_to_neighbor('A', 'B')), empty_set)
        self.assertEqual(set(g.hops_to_neighbor('B', 'A')), empty_set)
        self.assertEqual(set(g.hops_from_port('A', 'in')),  empty_set)
        self.assertEqual(set(g.hops_from_port('B', 'out')), empty_set)

        
