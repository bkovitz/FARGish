# testPropagator.py -- Unit tests for Propagator

import unittest
from pprint import pprint as pp
import inspect

from Propagator import Propagator
from Node import Node
from NodeParams import NodeParams, AttrParam
from testNetworkxPortGraph import GraphWithNetworkxActivation


class MyPropagator(Propagator):

    def incoming_neighbors(self, g, nodeid):
        return g.incoming_activation_neighbors(nodeid)

    def hop_weight(self, g, fromid, toid):
        return g.activation_from_to(fromid, toid)

    def min_value(self, g, nodeid):
        return g.min_activation(nodeid)

    def set_value(self, g, nodeid, new_value):
        g.set_activation(nodeid, new_value)

class MyNode(Node):
    node_params = NodeParams(AttrParam('name'))
    initial_activation = 0.0

class TestPropagator(unittest.TestCase):

    def test_propagator(self):
        g = GraphWithNetworkxActivation()
        p = MyPropagator(
            positive_feedback_rate=1.2,
            alpha=0.9,
            max_total=1.0,
            noise=0.0
        )
        a = g.add_node(MyNode('A'))
        b = g.add_node(MyNode('B'))
        o = g.add_node(MyNode('O'))
        g.set_activation(a, 1.0)
        g.set_activation(b, 0.5)
        g.set_activation_from_to(a, b, 1.0)
        g.set_activation_from_to(b, o, 1.0)

        p.propagate(g, g.activation_dict())
        #print('AD', g.activation_dict())

        self.assertAlmostEqual(g.activation(a), 0.5709545145009379)
        self.assertAlmostEqual(g.activation(b), 0.31759853198595706)
        self.assertAlmostEqual(g.activation(o), 0.11144695351310495)
