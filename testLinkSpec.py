import unittest

from LinkSpec import LinkSpec
from PortGraph import PortGraph, Node, pg

class Number(Node):
    def __init__(self, n):
        self.value = n

class Plus(Node):
    pass

class TestLinkSpec(unittest.TestCase):

    def test_basics(self):
        g = PortGraph()
        b4 = g.make_node(Number(4))
        b5 = g.make_node(Number(5))
        #t9 = g.make_node(Target(9))
        link_spec = LinkSpec('consumer', 'source', old_node=Number)

        self.assertFalse(link_spec.meets(g, None, None))

        plus = g.make_node(Plus)
        self.assertFalse(link_spec.meets(g, old_node=b4, new_nodeid=plus))

        link_spec.make(g, old_nodeid=b4, new_nodeid=plus)
        self.assertTrue(link_spec.meets(g, old_node=b4, new_nodeid=plus))
        self.assertTrue(link_spec.meets_exactly(
            g, old_node=b4, new_nodeid=plus)
        )

        g.add_edge(plus, 'wrong-label', b5, 'wrong-label')
        self.assertFalse(link_spec.meets(g, old_node=b5, new_nodeid=plus))

