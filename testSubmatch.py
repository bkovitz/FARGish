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

tg = make_graph(2, 2)  # "target" graph
hg = make_graph(2, 2, '+')  # "host" graph


class TestSubmatch(unittest.TestCase):

    def test_basics(self):
        self.assertTrue(is_node_match(tg, 0, hg, 0))
        self.assertFalse(is_node_match(tg, 0, hg, 2))
        self.assertCountEqual(matching_subgraphs(tg, hg),
                              [{0: 0, 1: 1}, {0: 1, 1: 0}])
