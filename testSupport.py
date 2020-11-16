# testSupport.py -- Unit tests for support.py

import unittest
from pprint import pprint as pp
import inspect

from Propagator import Propagator
from Node import Node
from NodeParams import NodeParams, AttrParam
from StdGraph import Graph, StdSupportPropagator
from ActiveGraph import pg, ps

class MyNode(Node):
    node_params = NodeParams(AttrParam('name'))

class TestGraph(Graph):

    support_propagator = StdSupportPropagator(
        positive_feedback_rate=0.2,
        alpha=0.98,
        max_total=100.0,
        noise=0.0,  # no noise for test
        sigmoid_p=0.98,
        num_iterations=3,
    )

class TestSupport(unittest.TestCase):

    def test_support(self):
        g = TestGraph()
        a = g.add_node(MyNode('A'))
        b = g.add_node(MyNode('B'))
        c = g.add_node(MyNode('C'))

        self.assertEqual(g.support_for(a), 1.0)
        g.set_support_from_to(a, b, 1.0)
        g.set_support_from_to(b, c, 1.0)
        g.propagate_support()

        self.assertGreater(g.support_for(b), g.support_for(a))
        self.assertGreater(g.support_for(c), g.support_for(b))
