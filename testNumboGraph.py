# testNumboGraph.py -- Unit tests for generic NumboGraph and ancillary classes

import unittest
from pprint import pprint as pp
import inspect

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa


class TestNumboGraph(unittest.TestCase):

    def test_operator_with_avail_operands(self):
        criterion = OperatorWithAvailOperands()

        g = NumboGraph(Numble([4, 5, 6], 15))

        # Don't accept the Plus that is tagged Allowed and has no operands
        ur_plus = g.look_for(And(Plus, CTagged(Allowed)), within=g.ws)
        self.assertFalse(criterion(g, ur_plus))

        (b4, b5) = g.look_for([Brick(4), Brick(5)], within=g.ws)
        (plus, block) = g.build_op_and_result(Plus, operands=(b4, b5))
        self.assertTrue(criterion(g, plus))

        (consumer_plus, block2) = g.consume_operands(Plus, operands=(b4, b5))
        self.assertFalse(criterion(g, plus))
        self.assertFalse(criterion(g, consumer_plus))

    def test_consume_result(self):
        # Tests that building a new Block that links to an existing Plus
        # makes a link to the Plus's 'result' port rather than creating
        # a 'consumer' port for the Plus, even though Block's definition
        # says that it links 'source -- consumer'.
        g = NumboGraph(Numble([4, 5, 6], 15))
        (b4, b5) = g.look_for([Brick(4), Brick(5)], within=g.ws)

        plus = g.add_node(Plus, operands=(b4, b5))
        self.assertCountEqual(
            plus.defined_port_labels(),
            ['operands', 'result']
        )

        block = g.add_node(Block, value=9, source=plus)
        self.assertTrue(g.has_hop(plus, 'result', block, 'source'))