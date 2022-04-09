# testGrid2.py -- Unit tests for Grid2.py

import unittest
from pprint import pprint as pp
import inspect

from Grid2 import Canvas, CanvasData, A, PPainter, P#, QPainter, \
    #QPainterTemplate, RPainter, Subst, unify, all_white
from Log import lo


two_cs: CanvasData = [
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, -1, -1, -1, -1],
    [-1, +1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, +1, -1, -1, -1],
    [-1, -1, -1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
]

class TestCanvas(unittest.TestCase):
    
    def test_get_and_set(self) -> None:
        c = Canvas.from_data(two_cs)
        self.assertEqual(c[1, 8], -1)
        self.assertEqual(c[(1, 8)], -1)
        self.assertEqual(c[A(1, 8)], -1)

        self.assertEqual(c[2, 7], +1)

        c[1, 8] = 2
        self.assertEqual(c[1, 8], 2)
        c[(1, 8)] = 3
        self.assertEqual(c[1, 8], 3)
        c[A(1, 8)] = 4
        self.assertEqual(c[1, 8], 4)

    def test_addr_to_indices(self) -> None:
        c = Canvas.from_data(two_cs)
        self.assertEqual(c.addr_to_indices(3, 7), (1, 2))
        self.assertEqual(c.addr_to_indices((3, 7)), (1, 2))
        self.assertEqual(c.addr_to_indices(A(3, 7)), (1, 2))
        # TODO Get a BadAddr

    def test_paint(self) -> None:
        c = Canvas.empty()

        c.paint((1, 8), +1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 1)
        self.assertEqual(c[1, 7], 0)  # should be unchanged
        self.assertEqual(c.clarity(1, 7), 0)  # should be unchanged
        
        c.paint((1, 8), +1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 2)
        
        c.paint((1, 8), -1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 1)

class TestPPainter(unittest.TestCase):
    
    def test_ppainter(self) -> None:
        c = Canvas.empty()
        p = PPainter(1, -1, 1, -1)

        self.assertEqual(p.as_xos(), 'XOXO')
        self.assertCountEqual(p.values(), [1, -1, 1, -1])

        self.assertEqual(p.match_wt(c, (1, 8)), 4)

        p.paint(c, (1, 8))
        self.assertEqual(c[1, 8], 1)
        self.assertEqual(c[2, 8], -1)
        self.assertEqual(c[1, 7], 1)
        self.assertEqual(c[2, 7], -1)

        self.assertEqual(p.match_wt(c, (1, 8)), 20)

    def test_ppainter_from_canvas(self) -> None:
        c = Canvas.from_data(two_cs)
        p = PPainter.from_canvas(c, (1, 8))
        self.assertEqual(p, PPainter(-1, -1, -1, 1))

