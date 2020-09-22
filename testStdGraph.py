# testStdGraph.py -- Unit tests for StdGraph

import unittest
from pprint import pprint as pp
import inspect

from StdGraph import Graph, pg 
from ActiveGraph import Node, NodeId, NRef, as_nodeid, as_node
from NodeParams import NodeParams, AttrParam, MateParam

class Brick(Node):
    node_params = NodeParams(AttrParam('value'))

class Avail(Node):
    is_tag = True
    node_params = NodeParams(MateParam('taggees', 'tags'))


class TestStdGraph(unittest.TestCase):

    def test_stdgraph(self):
        g = Graph()

        b1 = g.add_node(Brick, value=1)
        b2 = g.add_node(Brick(2))
        self.assertEqual(str(b1), 'Brick(1)')
        self.assertEqual(str(b2), 'Brick(2)')
        self.assertIs(as_node(g, b1.id), b1)
        self.assertIs(as_node(g, b2.id), b2)
        self.assertEqual(as_nodeid(b1), b1.id)
        self.assertEqual(as_nodeid(b2), b2.id)

        a1 = g.add_node(Avail, b1)
        pg(g)


if __name__ == '__main__':
    class Brick(Node):
        node_params = NodeParams(AttrParam('value'))

    g = Graph()
    b1 = g.add_node(Brick, value=1)
    b2 = g.add_node(Brick(2))
    a1 = g.add_node(Avail, [b1])
    a11 = g.add_node(Avail, [b1])
    pg(g)
