import unittest

from NodeSpec import NodeOfClass
from numbospec import *
from PortGraph import PortGraph, Hop
from ExprAsEquation import ExprAsEquation
from TimeStepper import TimeStepper

class TestGraph(TimeStepper, ExprAsEquation, PortGraph):
    def __init__(self, numble, **kwargs):
        super().__init__(**kwargs)
        self.graph['numble'] = numble
        self.ws = self.make_node(Workspace)
        numble.build(self, self.ws)

class TestBasics(unittest.TestCase):

    def test_node_of_class(self):
        spec = NodeOfClass(Number)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = {Brick(4), Brick(5), Brick(6), Target(15)}
        got = set(g.datum(nodeid) for nodeid in spec.see_all(g))
        self.assertEqual(got, expect)
        #print(got)
