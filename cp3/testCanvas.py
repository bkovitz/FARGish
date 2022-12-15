# testCanvas.py -- Unit tests for Canvas

import unittest
import inspect

from Model import Canvas, Letter, Blank

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_set_and_get(self) -> None:
        c = Canvas.make_from('aj a')
        self.assertEqual(c[1], Letter('a'))  # 1-based indexing
        self.assertEqual(c[3], Blank())

    def test_all_indices(self) -> None:
        c = Canvas.make_from('aj a')
        self.assertEqual(c.min_index, 1)
        self.assertEqual(c.max_index, 4)
        self.assertCountEqual(list(c.all_indices()), [1, 2, 3, 4])
