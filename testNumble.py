# testNumble.py -- Unit tests for Numble class

import unittest

from Numble import make_numble_class
from PortGraph import PortGraph, pg
from testNodeClasses import *


class TestNumble(unittest.TestCase):

    def test_numble(self):
        Numble = make_numble_class(
            Brick, Target, Want, Avail, Allowed, [Plus, Times]
        )
        g = PortGraph(port_mates=port_mates)
        numble = Numble([4, 5, 6], 15)
        numble.build(g, None)

        #pg(g) #DEBUG

        ids = g.nodes_of_class(Target)
        self.assertEqual(len(ids), 1)
        targetid = ids[0]
        self.assertEqual(g.value_of(targetid), 15)

        ids = g.nodes_of_class(Want)
        self.assertEqual(len(ids), 1)
        wantid = ids[0]
        self.assertTrue(g.has_hop(wantid, 'target', targetid, 'tags'))

        brickids = g.nodes_of_class(Brick)
        self.assertEqual(len(brickids), 3)
        self.assertCountEqual([g.value_of(b) for b in brickids], [4, 5, 6])

        ids = g.nodes_of_class(Avail)
        self.assertEqual(len(ids), 3)
        for b in brickids:
            self.assertTrue(g.has_tag(b, Avail))

        operatorids = g.nodes_of_class(Operator)
        self.assertEqual(len(operatorids), 2)

        ids = g.nodes_of_class(Allowed)
        self.assertEqual(len(ids), 2)
        for o in operatorids:
            self.assertTrue(g.has_tag(o, Allowed))
