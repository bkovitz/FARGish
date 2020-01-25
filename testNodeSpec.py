import unittest

from NodeSpec import NodeOfClass, NodeWithTag, HasSameValue
from numbospec import *
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

class TestBasics(unittest.TestCase):

    def test_node_of_class(self):
        spec = NodeOfClass(Number)
        g = TestGraph(Numble([4, 5, 6], 15))
        expect = {Brick(4), Brick(5), Brick(6), Target(15)}
        got = [g.datum(nodeid) for nodeid in spec.see_all(g)]
        self.assertCountEqual(got, expect)

        got = g.datum(spec.see_one(g))
        print(got)

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
        #pg(g)
        #spec = HasSameValue(
