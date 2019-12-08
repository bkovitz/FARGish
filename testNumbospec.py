# testNumbospec.py -- Unit tests for numbospec.py

import unittest

from numbospec import *
from PortGraph import PortGraph, pg, pn, pt

class TestNumbospec(unittest.TestCase):

    def test_simple_run(self):
        g = PortGraph()
        ws = g.make_node(Workspace)
        #target = g.make_node(Target(15))
        numble = Numble([4, 5, 6], 15)
        numble.build(g, ws)
        #pg(g)
