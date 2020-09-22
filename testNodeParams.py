import unittest
from pprint import pprint as pp

from NodeParams import NodeParams, AttrParam, MateParam
from LinkSpec import LinkSpec
#from PortGraph import PortGraph, Node, pg
from Node import Node
from StdGraph import Graph, pg
from PortMates import PortMates

class Number(Node):
    node_params = NodeParams(AttrParam('n'))

class Plus(Node):
    node_params = NodeParams(
        MateParam('operands', 'consumer'), MateParam('result', 'source')
    )


class TestGraph(Graph):
    port_mates = PortMates([('operands', 'consumer'), ('result', 'source')])


class TestNodeParams(unittest.TestCase):

    def test_AttrParam(self):
        g = TestGraph()
        two = g.add_node(Number(n=2))
        self.assertEqual(len(g), 1)
        self.assertEqual(g.value_of(two, 'n'), 2)

    def test_AttrParamImplicit(self):
        g = TestGraph()
        two = g.add_node(Number(2))
        self.assertEqual(len(g), 1)
        self.assertEqual(g.value_of(two, 'n'), 2)

    def test_MateParam(self):
        g = TestGraph()
        two = g.add_node(Number(n=2))
        three = g.add_node(Number(n=3))
        five = g.add_node(Number(n=5))
        plus = g.add_node(    Plus, operands=[two, three], result=five)
        self.assertTrue(g.has_hop(plus, 'operands', two, 'consumer'))
        self.assertTrue(g.has_hop(plus, 'operands', three, 'consumer'))
        self.assertTrue(g.has_hop(plus, 'result', five, 'source'))

    def test_is_already_built(self):
        g = TestGraph()
        self.assertFalse(g.already_built(Number, (2,)))
        self.assertFalse(g.already_built(Number(2)))
        two = g.add_node(Number(n=2))

        #self.assertTrue(g.already_built(Number, (2,)))
        # Commented out because as of 18-Mar-2020 we're ignoring attr values
        # when determining whether a node is is_already_built.

        self.assertFalse(g.already_built(Number, (3,)))
        self.assertFalse(g.already_built(Number(3)))
        three = g.add_node(Number(3))
        five = g.add_node(Number(5))

        kwargs = {
            'operands': [two, three],
            'result': five
        }
        self.assertFalse(g.already_built(Plus, **kwargs))
        plus = g.add_node(Plus, operands=[two, three], result=five)
        self.assertTrue(g.already_built(Plus, **kwargs))

    def test_missing_arg(self):
        pass

if __name__ == '__main__':
    g = TestGraph()
    plus = g.add_node(Plus)
    print('\n', g.datumstr(plus))
    pg(g)
