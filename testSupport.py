# testSupport.py -- Unit tests for support.py

from support import propagate_support, support_dict
from PortGraph import PortGraph, Hop

import unittest

class TestSupport(unittest.TestCase):

    def test_basics(self):
        g = PortGraph()
        g.add_nodes_from(['A', 'B', 'O'])
        g.add_edge('A', 'in', 'B', 'out')
        g.add_edge('O', 'members', 'A', 'member_of')

        g.set_support_for('A', 1.0)
        g.set_support_for('B', 0.5)
        g.add_edge('A', 'support_to', 'B', 'support_from')
        g.add_edge('B', 'support_to', 'O', 'support_from')
        #TODO edge weights

        propagate_support(g, max_total_support=1.0)
        d = support_dict(g)
        self.assertAlmostEqual(d['A'], 0.817665950444724)
        self.assertAlmostEqual(d['B'], 0.1822909838416653)
        self.assertAlmostEqual(d['O'], 4.306571361059501e-05)
