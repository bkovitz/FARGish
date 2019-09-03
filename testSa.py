# testSa.py -- Unit tests for spreading activation

from PortGraph import PortGraph
from sa import set_activations, simple_sa, activations

import unittest


class TestSpreadingActivation(unittest.TestCase):

    def test_basics(self):
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
