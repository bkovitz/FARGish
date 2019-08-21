import unittest

from submatch import matching_subgraphs, is_node_match
from PortGraph import PortGraph, Node, Number

import numbers


def make_graph(*nodes):
    g = PortGraph()
    for i, node in enumerate(nodes):
        if isinstance(node, numbers.Number):
            d = {'_class': Number}
        else:
            d = {'_class': Node}
        d['value'] = node
        g.add_node(i, **d)
    return g


class TestSubmatch(unittest.TestCase):

    def test_no_edges(self):
        tg = make_graph(2, 2)  # "target" graph
        hg = make_graph(2, 2, '+')  # "host" graph

        self.assertTrue(is_node_match(tg, 0, hg, 0))
        self.assertFalse(is_node_match(tg, 0, hg, 2))
        self.assertCountEqual(matching_subgraphs(tg, hg),
                              [{0: 0, 1: 1}, {0: 1, 1: 0}])

    def test_with_edges(self):
        tg = make_graph(2, 2)  # "target" graph
        hg = make_graph(2, 2, '+')  # "host" graph

        tg.add_edge(0, 'in', 1, 'out')  # Now tg has an edge that hg lacks.
        self.assertCountEqual(matching_subgraphs(tg, hg), [])

        hg.add_edge(1, 'in', 0, 'out')  # Now there's an edge, allowing only
                                        # one way to match up the 2's.
        self.assertCountEqual(matching_subgraphs(tg, hg), [{0: 1, 1: 0}])
