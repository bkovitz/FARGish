import unittest
from pprint import pprint as pp

from NodeParams import NodeParams, AttrParam, MateParam
from LinkSpec import LinkSpec
from PortGraph import PortGraph, Node, pg

class Number(Node):
    node_params = NodeParams(AttrParam('n'))

class Plus(Node):
    node_params = NodeParams(
        MateParam('operands', 'consumer'), MateParam('result', 'source')
    )

class TestNodeParams(unittest.TestCase):

    def testAttrParam(self):
        g = PortGraph()
        two = g.make_node(Number(n=2))
        self.assertEqual(len(g), 1)
        self.assertEqual(g.value_of(two, 'n'), 2)

    def testAttrParamImplicit(self):
        g = PortGraph()
        two = g.make_node(Number(2))
        self.assertEqual(len(g), 1)
        self.assertEqual(g.value_of(two, 'n'), 2)

    def testMateParam(self):
        g = PortGraph()
        two = g.make_node(Number(n=2))
        three = g.make_node(Number(n=3))
        five = g.make_node(Number(n=5))
        plus = g.make_node(Plus(operands=[two, three], result=five))
        self.assertTrue(g.has_hop(plus, 'operands', two, 'consumer'))
        self.assertTrue(g.has_hop(plus, 'operands', three, 'consumer'))
        self.assertTrue(g.has_hop(plus, 'result', five, 'source'))

    def testAlreadyBuilt(self):
        g = PortGraph()
