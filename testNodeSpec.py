import unittest
import itertools

from NodeSpec import NodeOfClass, NodeWithTag, NodeWithValue, HasSameValue, \
    And, Not, CartesianProduct, no_dups, TupAnd, NotLinkedToSame, BuildSpec, \
    LinkSpec
from numbospec import *
from bases import NewLinkSpec, make_link
from PortGraph import PortGraph, pg
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper
from util import reseed

reseed(1)

class TestGraph(TimeStepper, ExprAsEquation, PortGraph):
    def __init__(self, numble, **kwargs):
        kwargs['seed'] = 1
        super().__init__(**kwargs)
        self.graph['numble'] = numble
        self.ws = self.make_node(Workspace)
        numble.build(self, self.ws)

class ConsumeOperands(Node):

    link_specs = [
        NewLinkSpec('proposer', 'consume-operand', ),
        NewLinkSpec('proposer', 'consume-operand', ),
        NewLinkSpec('proposer', 'proposed-operator')
    ]

    @classmethod
    def build_and_link(cls, g, operand1id, operand2id, operatorid):
        nodeid = g.make_node(cls)
        old_nodes = [operand1id, operand2id, operatorid]
        for link_spec, old_node in zip(cls.link_specs, old_nodes):
            make_link(g, link_spec, nodeid, old_node)

class Want(Node):
    pass
class Agent(Node):
    pass

class TestNodeSpec(unittest.TestCase):

    def test_node_of_class(self):
        spec = NodeOfClass(Number)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = {Brick(4), Brick(5), Brick(6), Target(15)}
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

        got = g.datum(spec.see_one(g))

    def test_node_with_tag(self):
        spec = NodeWithTag(Number, Avail)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = [Brick(4), Brick(5), Brick(6)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

    def test_has_same_value(self):
        g = TestGraph(Numble([4, 5, 15, 6], 15))
        targetid = NodeOfClass(Target).see_one(g)
        spec = HasSameValue(targetid)
        expect = [Brick(15)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

    def test_and_not(self):
        g = TestGraph(Numble([4, 5, 15, 6], 15))
        targetid = NodeOfClass(Target).see_one(g)
        spec = And(
            Not(HasSameValue(targetid)),
            NodeWithTag(Number, Avail)
        )
        expect = [Brick(4), Brick(5), Brick(6)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

    def test_cartesian_product(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail)
        )
        got = [tuple(g.datum(nodeid) for nodeid in tup)
                for tup in finder.see_all(g)]
        bricks = [Brick(4), Brick(5), Brick(6)]
        expect = list(itertools.product(bricks, bricks))
        self.assertCountEqual(got, expect)

    def test_cartesian_product_no_dups(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail),
            whole_tuple_criterion=no_dups
        )
        got = [tuple(g.datum(nodeid) for nodeid in tup)
                for tup in finder.see_all(g)]
        expect = [(Brick(4), Brick(5)),
                  (Brick(4), Brick(6)),
                  (Brick(5), Brick(4)),
                  (Brick(5), Brick(6)),
                  (Brick(6), Brick(4)),
                  (Brick(6), Brick(5))]
        self.assertCountEqual(got, expect)

    def test_cartesian_product_not_linked_to_same(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        plus = g.make_node(Plus)
        times = g.make_node(Times)
        b4 = NodeWithValue(4).see_one(g)
        b5 = NodeWithValue(5).see_one(g)
        b6 = NodeWithValue(6).see_one(g)
        ConsumeOperands.build_and_link(g, b4, b5, plus)
        ConsumeOperands.build_and_link(g, b4, b6, plus)
        # Now 4+5 and 4+6 are each linked to a ConsumeOperands node.
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail),
            NodeOfClass(Operator),
            whole_tuple_criterion=NotLinkedToSame(
                *[link_spec.old_node_port_label
                    for link_spec in ConsumeOperands.link_specs]
            )
        )
        got = [tuple(g.datum(nodeid) for nodeid in tup)
                for tup in finder.see_all(g)]
        # We see all possibilities except 4+5 and 4+6.
        # HMM, this assumes a sort of commutativity, which won't be
        # appropriate when we search for ways to build a Minus expression.
        expect = [(Brick(4), Brick(4), Times()),
                  (Brick(4), Brick(5), Times()),
                  (Brick(4), Brick(6), Times()),
                  (Brick(5), Brick(6), Plus()),
                  (Brick(5), Brick(4), Times()),
                  (Brick(5), Brick(5), Times()),
                  (Brick(5), Brick(6), Times()),
                  (Brick(6), Brick(5), Plus()),
                  (Brick(6), Brick(4), Times()),
                  (Brick(6), Brick(5), Times()),
                  (Brick(6), Brick(6), Times())]
        # Since we did not pass no_dups to CartesianProduct, some of the
        # found possibilities include the same Brick twice.
        self.assertCountEqual(got, expect)

    def test_cartesian_product_not_linked_to_same_no_dups(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        plus = g.make_node(Plus)
        times = g.make_node(Times)
        b4 = NodeWithValue(4).see_one(g)
        b5 = NodeWithValue(5).see_one(g)
        b6 = NodeWithValue(6).see_one(g)
        ConsumeOperands.build_and_link(g, b4, b5, plus)
        ConsumeOperands.build_and_link(g, b4, b6, plus)
        # Now 4+5 and 4+6 are each linked to a ConsumeOperands node.
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail),
            NodeOfClass(Operator),
            whole_tuple_criterion=TupAnd(
                NotLinkedToSame(
                    *[link_spec.old_node_port_label
                        for link_spec in ConsumeOperands.link_specs]
                ),
                no_dups
            )
        )
        got = [tuple(g.datum(nodeid) for nodeid in tup)
                for tup in finder.see_all(g)]
        # We see all possibilities except 4+5 and 4+6.
        # HMM, this assumes a sort of commutativity, which won't be
        # appropriate when we search for ways to build a Minus expression.
        expect = [(Brick(4), Brick(5), Times()),
                  (Brick(4), Brick(6), Times()),
                  (Brick(5), Brick(6), Plus()),
                  (Brick(5), Brick(4), Times()),
                  (Brick(5), Brick(6), Times()),
                  (Brick(6), Brick(5), Plus()),
                  (Brick(6), Brick(4), Times()),
                  (Brick(6), Brick(5), Times())]
        # Thanks to no_dups, no possibility proposes consuming the same
        # Brick more than once.
        self.assertCountEqual(got, expect)

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

class TestBuildSpec(unittest.TestCase):

    def teest_plus(self):
        g = PortGraph()
        b4 = g.make_node(Number(4))
        b5 = g.make_node(Number(5))
        t9 = g.make_node(Target(9))
        spec = BuildSpec(
            Plus,
            [
                LinkSpec('source', 'consumer', old_node=b4),
                LinkSpec('source', 'consumer', old_node=b5),
                LinkSpec('consumer', 'source', old_node=t9)
            ]
        )
        self.assertFalse(spec.already_built(g, spec))
        spec.build(g, spec)
        self.assertTrue(spec.already_built(g, spec))

    def teest_agent(self):
        g = PortGraph()
        w = g.make_node(Want)
        a = g.make_node(Agent)
        spec = BuildSpec(
            Agent,
            LinkSpec('agents', 'behalf_of', Agent)
        )
        self.assertFalse(spec.already_built(g, spec))
        spec.build(g, spec)
        self.assertTrue(spec.already_built(g, spec))
