# testSa.py -- Unit tests for spreading activation

from PortGraph import PortGraph, pg
from sa import set_activations, simple_sa, activations, T, spread_activation

import unittest


class TestSpreadingActivation(unittest.TestCase):

    def test_spreading_activation(self):
        g = PortGraph()
        g.add_nodes_from(['A', 'B', 'O'])
        g.add_edge('A', 'sa', 'B', 'sa')
        g.add_edge('O', 'sa', 'A', 'sa')

        g.nodes['A']['a'] = 1.0

        def go():
            set_activations(g, simple_sa(g))
        go()
        # print(timeit.timeit(go, number=1000))
        # print(activations(g))
        self.assertAlmostEqual(g.nodes['A']['a'], 0.9969358984814931)
        self.assertAlmostEqual(g.nodes['B']['a'], 0.9741717893936888)
        self.assertAlmostEqual(g.nodes['O']['a'], 0.9741717893936888)

    def test_spread_activation(self):
        g = PortGraph()
        g.add_nodes_from(['A', 'B', 'O', 'IGNORED'])
        g.add_edge('A', 'sa', 'B', 'sa')
        g.add_edge('O', 'sa', 'A', 'sa')
        def only_nodes(g, node):
            return node in set(['A', 'B', 'O'])
        
        set_activations(g, {'O': 1.0}, attr='A')
        self.assertFalse('A' in g.nodes['IGNORED'])
        spread_activation(
            g,
            attr='A', via_port_label='sa', num_steps=10, transfer=T, decay=1.0,
            only_nodes=only_nodes
        )
        self.assertAlmostEqual(g.nodes['A']['A'], 0.9969358984814931)
        self.assertAlmostEqual(g.nodes['B']['A'], 0.9741717893936888)
        self.assertAlmostEqual(g.nodes['O']['A'], 0.9741717893936888)
        self.assertFalse('A' in g.nodes['IGNORED'])
