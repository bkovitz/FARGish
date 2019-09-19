# testSupport.py -- Unit tests for support.py

from support import Propagator, support_dict
from PortGraph import PortGraph, Hop

import unittest

class TestSupport(unittest.TestCase):

    def test_basics(self):
        g = PortGraph()
        p = Propagator(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total_support=1.0
        )
        g.add_nodes_from(['A', 'B', 'O'])
        g.add_edge('A', 'in', 'B', 'out')
        g.add_edge('O', 'members', 'A', 'member_of')

        g.set_support_for('A', 1.0)
        g.set_support_for('B', 0.5)
        g.add_edge('A', 'support_to', 'B', 'support_from')
        g.add_edge('B', 'support_to', 'O', 'support_from')
        #TODO edge weights

        p.propagate(g)
        d = support_dict(g)
        self.assertAlmostEqual(d['A'], 0.569072143062159)
        self.assertAlmostEqual(d['B'], 0.319848331226608)
        self.assertAlmostEqual(d['O'], 0.11107952571123292)

    def test_min_support_for(self):
        #TODO
        pass
