import unittest

from submatch import matching_subgraphs, is_node_match
from PortGraph import PortGraph, Node, pg
from numbonodes import Number

import numbers


def make_graph(*nodes):
    g = PortGraph()
    for i, node in enumerate(nodes):
        if isinstance(node, numbers.Number):
            datum = Number(node)
            d = {'_class': Number}
        else:
            datum = Node()
            d = {'_class': Node}
        d['value'] = node
        #g.add_node(i, **d)
        g.make_node(datum)
    return g


class TestSubmatch(unittest.TestCase):

    def test_no_edges(self):
        tg = make_graph(2, 2)  # "target" graph
        hg = make_graph(2, 2, '+')  # "host" graph

        self.assertTrue(is_node_match(tg, 1, hg, 1))
        self.assertFalse(is_node_match(tg, 1, hg, 3))
        self.assertCountEqual(matching_subgraphs(tg, hg),
                              [{1: 1, 2: 2}, {1: 2, 2: 1}])

    def test_with_edges(self):
        tg = make_graph(2, 2)  # "target" graph
        hg = make_graph(2, 2, '+')  # "host" graph

        tg.add_edge(1, 'in', 2, 'out')  # Now tg has an edge that hg lacks.
        self.assertCountEqual(matching_subgraphs(tg, hg), [])

        hg.add_edge(2, 'in', 1, 'out')  # Now there's an edge, allowing only
                                        # one way to match up the 2's.
        self.assertCountEqual(matching_subgraphs(tg, hg), [{1: 2, 2: 1}])
