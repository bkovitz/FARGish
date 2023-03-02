# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Canvas

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_len(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(len(canvas), 3)
