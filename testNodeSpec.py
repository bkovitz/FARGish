import unittest
import itertools
from pprint import pprint as pp

from NodeSpec import NodeSpec, NodeOfClass, NodeWithTag, NodeWithValue, \
    HasSameValue, And, Not, CartesianProduct, no_dups, TupAnd, \
    NotLinkedToSame, OLDBuildSpec as BuildSpec
from Node import Node
from NodeParams import NodeParams, MateParam
from StdGraph import Graph, pg
from ExprAsEquation import ExprAsEquation
from Numble import make_numble_class
from testNodeClasses import Workspace, Number, Brick, Target, Block, Want, \
    Avail, Allowed, Operator, Plus, Times, port_mates
from util import reseed

reseed(1)

Numble = make_numble_class(
    Brick, Target, Want, Avail, Allowed, [Plus, Times]
)

class TestGraph(ExprAsEquation, Graph):
    port_mates = port_mates  # imported from testNodeClasses

    def __init__(self, numble, **kwargs):
        kwargs['seed'] = 1
        super().__init__(**kwargs)
        self.numble = numble # TODO rm?
        self.ws = self.add_node(Workspace)
        numble.build(self, self.ws)

class ConsumeOperands(Node):
    node_params = NodeParams(
        MateParam('proposed_operator', 'proposer'),
        MateParam('consume_operand', 'proposer'),
        MateParam('consume_operand', 'proposer')
    )

#class Want(Node):
#    pass
class Agent(Node):
    pass

class TestNodeSpec(unittest.TestCase):

    def test_node_of_class(self):
        spec = NodeOfClass(Number)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = [Brick(4), Brick(5), Brick(6), Target(15)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

        got = g.datum(spec.see_one(g))

    def test_node_with_tag(self):
        spec = NodeWithTag(Number, Avail)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = [Brick(4), Brick(5), Brick(6)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

    def test_nodespec(self):
        g = TestGraph(Numble([4, 5, 6], 15))

        spec = NodeSpec()  # All nodes
        expect = [Brick(4), Brick(5), Brick(6), Target(15), Workspace(),
                  Want(), Plus(), Times(), Allowed(), Allowed(),
                  Avail(), Avail(), Avail()]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

        spec = NodeSpec(nodeclass=Number)
        expect = [Brick(4), Brick(5), Brick(6), Target(15)]
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

        spec = NodeSpec(tagclass=Avail)
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
        plus = g.node_of_class(Plus)
        times = g.node_of_class(Plus)
        b4 = NodeWithValue(4).see_one(g)
        b5 = NodeWithValue(5).see_one(g)
        b6 = NodeWithValue(6).see_one(g)
        #ConsumeOperands.build_and_link(g, b4, b5, plus)
        #ConsumeOperands.build_and_link(g, b4, b6, plus)
        co45 = g.add_node(ConsumeOperands,
            consume_operand=[b4, b5],
            proposed_operator=plus
        )
        #The next line tests passing args rather than kwargs
        co46 = g.add_node(ConsumeOperands, plus, b4, b6)
        #pg(g)
        # Now 4+5 and 4+6 are each linked to a ConsumeOperands node.
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail),
            NodeOfClass(Operator),
            whole_tuple_criterion=NotLinkedToSame(
                *ConsumeOperands.defined_roles()
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
        self.maxDiff = None
        self.assertCountEqual(got, expect)

    def test_cartesian_product_not_linked_to_same_no_dups(self):
        g = TestGraph(Numble([4, 5, 6], 15))
        plus = g.node_of_class(Plus)
        times = g.node_of_class(Plus)
        b4 = NodeWithValue(4).see_one(g)
        b5 = NodeWithValue(5).see_one(g)
        b6 = NodeWithValue(6).see_one(g)
        g.add_node(ConsumeOperands, plus, b4, b5)
        g.add_node(ConsumeOperands, plus, b4, b6)
        # Now 4+5 and 4+6 are each linked to a ConsumeOperands node.
        finder = CartesianProduct(
            NodeWithTag(Number, Avail),
            NodeWithTag(Number, Avail),
            NodeOfClass(Operator),
            whole_tuple_criterion=TupAnd(
                NotLinkedToSame(
                    *ConsumeOperands.defined_roles()
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

# TODO 28-Sep-2020 Can we get rid of LinkSpec?
#class TestBuildSpec(unittest.TestCase):
#
#    def teest_plus(self):
#        g = Graph()
#        b4 = g.add_node(Number(4))
#        b5 = g.add_node(Number(5))
#        t9 = g.add_node(Target(9))
#        spec = BuildSpec(
#            Plus,
#            [
#                LinkSpec('source', 'consumer', Number),
#                LinkSpec('source', 'consumer', Number),
#                LinkSpec('consumer', 'source', Number)
#            ]
#        )
#        self.assertFalse(spec.is_already_built(g))
#        spec.build(g) #TODO Think of some satisfactory way to specify b4, b5, t9
#                      # as the "old nodes" that the new Plus node should link to
#        self.assertTrue(spec.is_already_built(g))
#
#    def test_agent(self):
#        g = Graph()
#        w = g.add_node(Want)
#        spec = BuildSpec(
#            Agent,
#            LinkSpec('agents', 'behalf_of')
#        )
#        self.assertFalse(spec.is_already_built(g, w))
#        spec.build(g, w)
#        self.assertTrue(spec.is_already_built(g, w))
#        self.assertEqual(len(g), 2)
#
#    def test_agent_func_action(self):
#        g = Graph()
#        w = g.add_node(Want)
#        spec = BuildSpec(
#            Agent,
#            LinkSpec('agents', 'behalf_of')
#        )
#        action = spec.maybe_make_build_action(g, w)
#        action.go(g)
#        self.assertEqual(len(g), 2)
#        a = g.neighbor(w, port_label='agents')
#        self.assertTrue(g.has_hop(a, 'behalf_of', w, 'agents'))
