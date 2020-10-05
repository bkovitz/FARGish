# atest_numbo.py -- Tests that Numbo can solve certain problems

import unittest

from numbo5 import newg, Numble
from log import *


class NumboTest(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_11111_5(self):
        g = newg(Numble([1, 1, 1, 1, 1], 5))
        g.do_timestep(num=30)
        self.assertTrue(g.succeeded())
        # TODO Test that the equation is 1 + 1 + 1 + 1 + 1 = 5

    def test_22222_10(self):
        g = newg(Numble([2, 2, 2, 2, 2], 10))
        g.do_timestep(num=30)
        self.assertTrue(g.succeeded())
        # TODO Test for expected equation
