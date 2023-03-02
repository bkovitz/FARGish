# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Canvas, detect_repetition, Seed, Succ

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_str(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(str(canvas), 'abc')

    def test_len(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(len(canvas), 3)

class TestRepetitionDetection(unittest.TestCase):

    def test_abc(self) -> None:
        #detector = D
        canvas = Canvas.make_from('abc')
        got = detect_repetition(canvas)
        assert got is not None
        seed, op = got
        self.assertEqual(seed, Seed('a', 1))
        self.assertEqual(op, Succ)
