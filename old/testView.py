# testView.py -- Unit tests for View class

from View import View, NodeCriterion
#from PortGraph import PortGraph, Node, pg, ps
from StdGraph import Graph, pg
from Node import Node
from WithActivation import WithActivation
from NodeParams import NodeParams, AttrParam, MateParam
from PortMates import PortMates
#from numbonodes import Number, Avail

import unittest


class Number(Node):
    node_params = NodeParams(AttrParam('value'))
class Avail(Node):
    node_params = NodeParams(MateParam('taggees', 'tags'))

port_mates = PortMates([('taggees', 'tags')])

g = None

class TestView(unittest.TestCase):

    def test_view(self):
        global g
        g = Graph(port_mates=port_mates)
        v = g.add_node(View(NodeCriterion(nodeclass=Number, tagclass=Avail)))
        dv = g.datum(v)
        n5 = g.add_node(Number(5))
        g.add_tag(Avail, n5)
        n6 = g.add_node(Number(6)) # not tagged Avail
        g.do_touches()

        self.assertCountEqual(dv.viewing(), [n5.id])

        g.remove_tag(n5, Avail)  # Removing the tag should remove it from View
        g.do_touches()

        self.assertCountEqual(dv.viewing(), [])
