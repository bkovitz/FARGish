# testSupport.py -- Unit tests for support.py

import unittest
from pprint import pprint as pp
import inspect

from Propagator import Propagator
from Node import Node
from NodeParams import NodeParams, AttrParam
from StdGraph import Graph
from ActiveGraph import pg, ps

class MyNode(Node):
    node_params = NodeParams(AttrParam('name'))

class TestSupport(unittest.TestCase):

    def test_support(self):
        g = Graph()
        a = g.add_node(MyNode('A'))
        b = g.add_node(MyNode('B'))
        c = g.add_node(MyNode('C'))
        g.set_support_from_to(a, b, 1.0)
        g.set_support_from_to(b, c, 1.0)
        ps(g)
        pg(g)
