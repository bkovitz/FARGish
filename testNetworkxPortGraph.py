# testNetworkxPortGraph.py -- Unit tests for NetworkxPortGraph

import unittest
from dataclasses import dataclass

from NetworkxPortGraph import NetworkxPortGraph
from ActiveGraph import Node, Hop

@dataclass
class MyNode(Node):
    name: str


class TestNetworkxPortGraph(unittest.TestCase):

    def test_networkx_port_graph_basics(self):
        g = NetworkxPortGraph()
        self.assertEqual(len(g), 0)
        self.assertEqual(g.num_nodes(), 0)
        self.assertEqual(g.num_edges(), 0)

        # Create two nodes
        nodeid1 = g._add_node(MyNode('A'))
        nodeid2 = g._add_node(MyNode('B'))

        # Verify that the nodes are what we expect
        datum1 = g.datum(nodeid1)
        self.assertEqual(datum1.id, 1)
        self.assertEqual(datum1.g, g)
        self.assertEqual(datum1.name, 'A')

        datum2 = g.datum(nodeid2)
        self.assertEqual(datum2.id, 2)
        self.assertEqual(datum2.g, g)
        self.assertEqual(datum2.name, 'B')

        self.assertEqual(g.num_nodes(), 2)
        self.assertCountEqual(g._nodeids(), [1, 2])
        self.assertCountEqual(g._nodes(), [MyNode('A'), MyNode('B')])

        # Verify that there are no edges or ports yet
        self.assertCountEqual(g._neighbors(nodeid1), [])
        self.assertCountEqual(g.hops_from_node(nodeid1), [])
        self.assertCountEqual(g.hops_from_node(nodeid2), [])
        self.assertCountEqual(g.hops_from_node(nodeid1), [])
        self.assertCountEqual(g.hops_from_node(nodeid2), [])
        self.assertCountEqual(g.hops_from_port(nodeid1, 'to'), [])
        self.assertCountEqual(g.hops_from_port(nodeid2, 'from'), [])
        self.assertCountEqual(g._port_labels(nodeid1), [])

        # Create an edge between the nodes
        g._add_edge(nodeid1, 'to', nodeid2, 'from')

        self.assertEqual(g.num_edges(), 1)
        self.assertCountEqual(g._neighbors(nodeid1), [nodeid2])
        self.assertCountEqual(g._neighbors(nodeid2), [nodeid1])

        # .hops_from_node()
        self.assertCountEqual(
            g.hops_from_node(nodeid1),
            [Hop(nodeid1, 'to', nodeid2, 'from', 0)]
        )
        self.assertCountEqual(
            g.hops_from_node(nodeid2),
            [Hop(nodeid2, 'from', nodeid1, 'to', 0)]
        )

        # .hops_from_port()
        self.assertCountEqual(
            g.hops_from_port(nodeid1, 'to'),
            [Hop(nodeid1, 'to', nodeid2, 'from', 0)]
        )
        self.assertCountEqual(
            g.hops_from_port(nodeid2, 'from'),
            [Hop(nodeid2, 'from', nodeid1, 'to', 0)]
        )

        # .hops_to_neighbor()
        self.assertCountEqual(
            g.hops_to_neighbor(nodeid1, nodeid2),
            [Hop(nodeid1, 'to', nodeid2, 'from', 0)]
        )
        self.assertCountEqual(
            g.hops_to_neighbor(nodeid2, nodeid1),
            [Hop(nodeid2, 'from', nodeid1, 'to', 0)]
        )

        # Remove the edge
        g._remove_edge(nodeid1, 'to', nodeid2, 'from')

        self.assertEqual(g.num_edges(), 0)
        self.assertEqual(len(g.g.edges), 0)  # Checking internal representation
        self.assertCountEqual(g._neighbors(nodeid1), [])
        self.assertCountEqual(g._neighbors(nodeid2), [])

        # .hops_from_node()
        self.assertCountEqual(g.hops_from_node(nodeid1), [])
        self.assertCountEqual(g.hops_from_node(nodeid2), [])

        # .hops_from_port()
        self.assertCountEqual(g.hops_from_port(nodeid1, 'to'), [])
        self.assertCountEqual(g.hops_from_port(nodeid2, 'from'), [])

        # .hops_to_neighbor()
        self.assertCountEqual(g.hops_to_neighbor(nodeid1, nodeid2), [])
        self.assertCountEqual(g.hops_to_neighbor(nodeid2, nodeid1), [])

        # ._port_labels()
        self.assertCountEqual(g._port_labels(nodeid1), ['to'])
        self.assertCountEqual(g._port_labels(nodeid2), ['from'])

    def test_networkx_port_graph_remove_node(self):
        g = NetworkxPortGraph()
        
        # Create two nodes and link them
        nodeid1 = g._add_node(MyNode('A'))
        nodeid2 = g._add_node(MyNode('B'))
        g._add_edge(nodeid1, 'to', nodeid2, 'from')

        # Now remove one node. The edge should disappear automatically.
        g._remove_node(nodeid1)

        self.assertEqual(g.num_nodes(), 1)

        self.assertCountEqual(g.hops_from_node(nodeid2), [])
        self.assertCountEqual(g.hops_from_port(nodeid2, 'from'), [])
        self.assertCountEqual(g.hops_to_neighbor(nodeid2, nodeid1), [])
        self.assertCountEqual(g._neighbors(nodeid2), [])
        self.assertEqual(g.num_edges(), 0)
