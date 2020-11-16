# testStdGraph.py -- Unit tests for StdGraph

import unittest
from pprint import pprint as pp
import inspect

from StdGraph import Graph, pg 
from Node import Node, NodeId, NRef, as_nodeid, as_node, as_nodeids, \
    as_nodes
from NodeParams import NodeParams, AttrParam, MateParam
from util import PushAttr
from log import *


class Tag(Node):
    is_tag = True
    node_params = NodeParams(MateParam('taggees', 'tags'))

class Number(Node):
    node_params = NodeParams(AttrParam('value'))

class Brick(Number):
    initial_activation = 0.72
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

class Slipnet(Group):
    pass

class WNode(Node):
    '''Node for testing g.walk().'''
    node_params = NodeParams(MateParam('from', 'to'))
    is_duplicable = True

    wnode_count = 0
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__class__.wnode_count += 1
        self.wid = self.wnode_count

    def __eq__(self, other):
        return self.__class__ == other.__class__ and self.wid == other.wid

    def __hash__(self):
        return hash(self.wid)

    def __repr__(self):
        return f'{self.__class__.__name__}({self.wid})'

class W2Node(WNode):
    pass

class TestGraph(Graph):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.port_mates += [
            ('bricks', 'numble'), ('to', 'from'), ('wto', 'wfrom')
        ]

class TestStdGraph(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

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

        self.assertEqual(g.activation(b1), 0.72)

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

    def test_already_built__subclass(self):
        self.assertNotEqual(Number(1), Brick(1))
        self.assertNotEqual(Brick(1), Number(1))

        g = TestGraph()
        b1 = g.add_node(Brick, 1)
        self.assertIs(g.already_built(Number, 1), None)
        self.assertIs(g.already_built(Number(1)), None)
        self.assertIs(g.already_built(Number), None)

    def test_already_built__missing_arg(self):
        g = TestGraph()
        n1 = g.add_node(Number, 1)
        self.assertIs(g.already_built(Number), None)
        nnone = g.add_node(Number)
        self.assertIs(g.already_built(Number), nnone)
        self.assertIs(g.already_built(Number(None)), nnone)

        self.assertEqual(nnone, Number(None))
        #self.assertEqual(nnone, Number())  # Not equal, because Node.__init__
            # doesn't do the same thing as g.add_node(), but maybe that's OK.
            # BEN 02-Oct-2020

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

        node1 = g.add_node(Brick(1))

        with PushAttr(g, 'builder'):
            g.builder = node1
            node2 = g.add_node(Brick(2))

        self.assertTrue(g.builder_of(node2), node1)

    def test_on_build(self):
        class NodeWithOnBuild(Node):
            def on_build(self):
                self.late_attr = 'ON_BUILD'

        g = TestGraph()

        self.assertIs(NodeWithOnBuild().late_attr, None)

        node = g.add_node(NodeWithOnBuild)
        self.assertEqual(g.value_of(node, 'late_attr'), 'ON_BUILD')

    def test_spreading_activation(self):
        g = TestGraph()
        b1 = g.add_node(Brick, value=1)
        b2 = g.add_node(Brick(2))

        b1_before = g.activation(b1)
        g.boost_activation(b1)
        b1_after = g.activation(b1)
        self.assertGreater(b1_after, b1_before)

        g.set_activation_from_to(b1, b2)

        g.do_timestep()
        self.assertGreater(
            g.activation(b2),
            Brick.initial_activation - 5 * g.activation_propagator.noise
        )
        self.assertLess(g.activation(b1), b1_after)

    def test_seed(self):
        g = TestGraph(seed=25)
        self.assertEqual(g.seed, 25)
        g = TestGraph()
        self.assertTrue(isinstance(g.seed, int))

    def test_partition_nodes(self):
        g = TestGraph()
        n1 = g.add_node(Brick(1))
        n2 = g.add_node(Brick(2))
        n3 = g.add_node(Brick(3))
        n4 = g.add_node(Brick(4))
        even, odd = g.partition_nodes(g.nodes(), is_even)
        self.assertCountEqual(even, [n2, n4])
        self.assertCountEqual(odd, [n1, n3])

    def test_walk(self):
        g = TestGraph()
#        g.build_subgraph('''
#b1: Brick(1)
#b2: Brick(2)
#plus: Plus
#b1 -- plus
#b2 -- plus
#plus -- Block(3)
#'''
        n1 = g.add_node(WNode)
        n1_a1 = g.add_node(W2Node, n1)
        g.add_edge(n1, 'wto', n1_a1, 'from')
        n1_a2 = g.add_node(WNode, n1)
        g.add_edge(n1, 'to', n1_a2, 'wfrom')
        n1_a3 = g.add_node(WNode, n1)
        n1_a1_b1 = g.add_node(WNode, n1_a1)
        n1_a1_b2 = g.add_node(WNode, n1_a1)
        g.add_edge(n1_a1, 'wto', n1_a1_b2, 'from')
        n1_a1_b1_c1 = g.add_node(WNode, n1_a1_b1)

        # No restrictions on walk returns everything (since the graph is
        # connected).
        self.assertCountEqual(
            g.as_nodes(g.walk(n1)),
            [n1, n1_a1, n1_a2, n1_a3, n1_a1_b1, n1_a1_b2, n1_a1_b1_c1]
        )

        # Limited # of hops
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, max_hops=1)),
            [n1, n1_a1, n1_a2, n1_a3]
        )
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, max_hops=2)),
            [n1, n1_a1, n1_a2, n1_a3, n1_a1_b1, n1_a1_b2]
        )

        # Limited by neighbor_class
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, neighbor_class=Brick)),
            [n1]
        )
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, neighbor_class=W2Node)),
            [n1, n1_a1]
        )

        # Limited by port_label
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, port_label='wto')),
            [n1, n1_a1, n1_a1_b2]
        )

        # Limited by neighbor_label
        self.assertCountEqual(
            g.as_nodes(g.walk(n1, neighbor_label='wfrom')),
            [n1, n1_a2]
        )

    def teest_slipnet_search(self):
        # TODO Link the slipnodes; build an initial activation dict
        # from the Bricks in the workspace.
        g = TestGraph()
        slipnet = g.add_node(Slipnet)
        b1 = g.add_node(Brick, 1)
        b2 = g.add_node(Brick, 2)
        arch_b1 = g.add_node(Brick, 1, member_of=slipnet)
        arch_b2 = g.add_node(Brick, 2, member_of=slipnet)
        arch_n1 = g.add_node(Number, 1, member_of=slipnet)
        arch_n2 = g.add_node(Number, 2, member_of=slipnet)
        arch_n = g.add_node(Number, member_of=slipnet)
        print(arch_n1, arch_n2, arch_n)
        pg(g)


def is_even(g, node: NRef):
    return g.value_of(node) & 1 == 0

if __name__ == '__main__':
    g = Graph()
    b1 = g.add_node(Brick, value=1)
    b2 = g.add_node(Brick(2))
    a1 = g.add_node(Avail, [b1])
    a11 = g.add_node(Avail, [b1])
    pg(g)
    print(g.hops_from_node(b1))
