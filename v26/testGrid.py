# testGrid.py -- Unit tests for Grid.py

import unittest
from pprint import pprint as pp
import inspect

from Grid import Canvas, CanvasData, A


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
        
