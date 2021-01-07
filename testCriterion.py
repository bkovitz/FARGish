# testCriterion.py -- Unit tests for Criterion objects

import unittest
from pprint import pprint as pp
import inspect

from NumboGraph import *
from log import *
from ActiveGraph import pg, pa

class TestCriterion(unittest.TestCase):

    def test_and(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        for _ in range(5):
            ur_plus = g.look_for(And(Plus, CTagged(Allowed)), within=g.ws)
            self.assertTrue(
                g.is_of_class(ur_plus, Plus),
                f'{g.nodestr(ur_plus)} is not a Plus'
            )

    def test_not_the_args_of(self):
        g = NumboGraph(Numble([4, 5, 6], 15))
        b4 = g.look_for(Brick(4))
        b5 = g.look_for(Brick(5))
        plus, block9 = g.build_op_and_result(Plus, operands=[b4, b5])
        assert b4 and b5 and plus and block9

        msg = 'NotTheArgsOf failed to recognize Plus(4, 5)'
        ntao_o = NotTheArgsOf(Plus, 'operands')
        self.assertFalse(ntao_o(g, [b4, b5]), msg)
        ntao_s = NotTheArgsOf(Plus, 'source')
        self.assertFalse(ntao_s(g, [b4, b5]), msg)
