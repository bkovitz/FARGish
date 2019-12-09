# testNumbospec.py -- Unit tests for numbospec.py

import unittest

from numbospec import *
from PortGraph import PortGraph, pg, pn, pt
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper
from log import ShowActiveNodes, ShowActionList, ShowActionsChosen


class TestGraph(TimeStepper, ExprAsEquation, PortGraph):
    def __init__(self, numble):
        super().__init__()
        self.graph['numble'] = numble
        self.ws = self.make_node(Workspace)
        numble.build(self, self.ws)

class TestNumbospec(unittest.TestCase):

    def test_brick_is_target(self):
        g = TestGraph(Numble([4, 5, 6, 15], 15))
        self.assertFalse(g.done())
        g.do_timestep()
        self.assertTrue(g.done())

    def test_4_5_6_15(self):
        pass


ShowActiveNodes.start_logging()
ShowActionList.start_logging()
ShowActionsChosen.start_logging()
g = TestGraph(Numble([4, 5, 6], 15))
g.do_timestep()
pg(g)
