# testStdGraph.py -- Unit tests for StdGraph

import unittest
from pprint import pprint as pp
import inspect

from StdGraph import Graph, pg 
from ActiveGraph import Node, NodeId, NRef, as_nodeid, as_node, as_nodeids, \
    as_nodes
from NodeParams import NodeParams, AttrParam, MateParam
from util import PushAttr


class Tag(Node):
    is_tag = True
    node_params = NodeParams(MateParam('taggees', 'tags'))

class Brick(Node):
    node_params = NodeParams(AttrParam('value'))
    is_duplicable = True

class Avail(Tag):
    pass

class UniqueNumber(Node):
    node_params = NodeParams(AttrParam('value'))

class Group(Node):
    node_params = NodeParams(MateParam('members', 'member_of'))

class Workspace(Group):
    pass

class Numble(Group):
    pass

class TestGraph(Graph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.port_mates += [('bricks', 'numble')]

class TestStdGraph(unittest.TestCase):

    def test_stdgraph_nodebuilding(self):
        g = Graph()

        # Make two Bricks
        b1 = g.add_node(Brick, value=1)
        b2 = g.add_node(Brick(2))

        self.assertEqual(str(b1), 'Brick(1)')
        self.assertEqual(str(b2), 'Brick(2)')
        self.assertNotEqual(b1.id, b2.id)

        self.assertIs(as_node(g, b1.id), b1)
        self.assertIs(as_node(g, b2.id), b2)
        self.assertEqual(as_nodeid(b1), b1.id)
        self.assertEqual(as_nodeid(b2), b2.id)

        self.assertCountEqual(as_nodeids([b1, b2, None]), [b1.id, b2.id])
        self.assertCountEqual(as_nodes(g, [b1.id, b2, None]), [b1, b2])

        self.assertEqual(g.value_of(b1), 1)
        self.assertEqual(g.value_of(b2), 2)

        # Tag one Brick Avail
        a1 = g.add_node(Avail, b1)

        self.assertTrue(g.has_edge(b1, 'tags', a1, 'taggees'))
        self.assertEqual(len(g.hops_from_port(b1, 'tags')), 1)

        alr = g.already_built(Avail, b1)
        self.assertIs(a1, alr)

        # Try to double-tag it: nothing should happen
        a1_a = g.add_node(Avail, b1)

        #pg(g)
        self.assertEqual(len(g.hops_from_port(b1, 'tags')), 1)
        self.assertIs(a1_a, a1)

    def test_unique_node_via__init__(self):
        g = Graph()

        # Make the nodes by calling Node.__init__
        u1 = g.add_node(UniqueNumber(1))
        u1_a = g.add_node(UniqueNumber(1))
        u2 = g.add_node(UniqueNumber(2))

        self.assertIs(g.already_built(UniqueNumber(1)), u1)
        self.assertIs(u1, u1_a)
        self.assertIsNot(u1, u2)
        self.assertEqual(g.num_nodes(), 2)

    def test_unique_node(self):
        g = Graph()

        self.assertIs(g.already_built(UniqueNumber(1)), None)

        # Make the nodes by getting g.add_node() to construct them
        u1 = g.add_node(UniqueNumber, 1)
        u1_a = g.add_node(UniqueNumber, 1)
        u2 = g.add_node(UniqueNumber, 2)

        self.assertIs(g.already_built(UniqueNumber, 1), u1)
        self.assertIs(u1, u1_a)
        self.assertIsNot(u1, u2)
        self.assertEqual(g.num_nodes(), 2)

    def test_auto_membership(self):
        g = TestGraph()
        ws = g.add_node(Workspace)

        # Explicit membership
        numble = g.add_node(Numble, member_of=ws)
        self.assertTrue(g.is_port_label('members'))
        self.assertTrue(g.is_port_label('member_of'))
        self.assertTrue(g.has_hop(ws, 'members', numble, 'member_of'))

        b1 = g.add_node(Brick, value=1, member_of=numble)

        # Implicit membership
        numble_tag = g.add_node(Tag, numble)
        self.assertTrue(g.is_member(numble_tag, ws))

        brick_tag = g.add_node(Tag, b1)
        self.assertTrue(g.is_member(brick_tag, numble))

    def test_mark_builder(self):
        g = TestGraph()

        node1 = g.add_node(Node)

        with PushAttr(g, 'builder'):
            g.builder = node1
            node2 = g.add_node(Node)

        self.assertTrue(g.builder_of(node2), node1)

    def test_on_build(self):
        class NodeWithOnBuild(Node):
            def on_build(self):
                self.late_attr = 'ON_BUILD'

        g = TestGraph()

        self.assertIs(NodeWithOnBuild().late_attr, None)

        node = g.add_node(NodeWithOnBuild)
        self.assertEqual(g.value_of(node, 'late_attr'), 'ON_BUILD')

    def test_seed(self):
        g = TestGraph(seed=25)
        self.assertEqual(g.seed, 25)
        g = TestGraph()
        self.assertTrue(isinstance(g.seed, int))

if __name__ == '__main__':
    g = Graph()
    b1 = g.add_node(Brick, value=1)
    b2 = g.add_node(Brick(2))
    a1 = g.add_node(Avail, [b1])
    a11 = g.add_node(Avail, [b1])
    pg(g)
    print(g.hops_from_node(b1))
