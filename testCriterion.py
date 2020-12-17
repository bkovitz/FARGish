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
