# testSupport.py -- Unit tests for support.py

from support import Propagator, support_dict
from PortGraph import PortGraph, Hop, Node, pg, ps

import unittest

class AutoSupportedNode(Node):
    min_support_for = 4.0

class TestSupport(unittest.TestCase):

    def test_support(self):
        g = PortGraph()
        p = Propagator(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total_support=1.0,
            noise=0.0
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
        g = PortGraph()
        node = g.make_node(AutoSupportedNode)
        self.assertAlmostEqual(g.min_support_for(node), 4.0)
        self.assertAlmostEqual(g.support_for(node), 4.0)

if __name__ == '__main__':
    from TimeStepper import TimeStepper
    class TestGraph(TimeStepper, PortGraph):
        pass
    g = TestGraph(
            seed=1,
            support_propagator=Propagator(max_total_support=4,  #300
                                          positive_feedback_rate=0.1,
                                          sigmoid_p=0.9,
        # The high sigmoid_p here seems to make a smoother transition once
        # max_total_support is reached.
                                          alpha=0.95
                                         ),
            support_steps=1
    )

    n1 = g.make_node(Node)
    n2 = g.make_node(Node)
    n3 = g.make_node(Node)
    n4 = g.make_node(Node)
    g.add_mutual_support(n1, n2)
    g.add_mutual_opposition(n2, n3)
    g.add_mutual_support(n3, n4)
    g.set_support_for(n1, 1.0)
    g.set_support_for(n2, 0.1)
    g.set_support_for(n3, 0.1)
    g.set_support_for(n4, 2.0)
    g.do_timestep(num=82)
    ps(g)
