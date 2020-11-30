# atest_numbo.py -- Tests that Numbo can solve certain problems

import unittest

from numbo5 import newg, Numble
from log import *
from ActiveGraph import pg, pa


class NumboTest(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_11111_5(self):
        #ShowAnnotations.start_logging()
        #ShowActiveNodes.start_logging()
        #ShowActionList.start_logging()
        ##ShowActionsChosen.start_logging()
        #ShowActionsPerformed.start_logging()
        #ShowPrimitives.start_logging()
        g = newg(Numble([1, 1, 1, 1, 1], 5))
        g.do_timestep(num=60)
        self.assertTrue(g.succeeded())
        # TODO Test that the equation is 1 + 1 + 1 + 1 + 1 = 5

    def test_22222_10(self):
        g = newg(Numble([2, 2, 2, 2, 2], 10))
        g.do_timestep(num=60)
        self.assertTrue(g.succeeded())
        # TODO Test for expected equation

if __name__ == '__main__':
    g = newg(Numble([1, 1, 1, 1, 1], 5))
    ShowActionsPerformed.start_logging()
    ShowPrimitives.start_logging()
    #g.do_timestep(num=60)
    #pg(g)
    
