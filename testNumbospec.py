# testNumbospec.py -- Unit tests for numbospec.py

import unittest

from numbospec import *
from PortGraph import PortGraph, pg, pn, pt
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper


class TestGraph(TimeStepper, ExprAsEquation, PortGraph):
    pass

class TestNumbospec(unittest.TestCase):

    def test_brick_is_target(self):
        g = TestGraph()
        ws = g.make_node(Workspace)
        numble = Numble([4, 5, 6, 15], 15)
        numble.build(g, ws)
        self.assertFalse(g.done())
        g.do_timestep()
        self.assertTrue(g.done())
        #pg(g)
