# testBrute.py -- Unit test for brute-force numble solver

import unittest

from brute import run, NumboSuccess, Brick, Want
from log import *
from PortGraph import PortGraph, pg, ps

class TestBrute(unittest.TestCase):

    def setUp(self):
        stop_all_logging()

    def test_brute(self):
        g = run(seed=4730533389549952010)
        self.assertTrue(isinstance(g.done(), NumboSuccess))
        self.assertCountEqual(
            [g.display_name(n) for n in g.nodes_of_class((Brick, Want))],
            ['Want', 'Brick(4)', 'Brick(5)', 'Brick(6)']
        )