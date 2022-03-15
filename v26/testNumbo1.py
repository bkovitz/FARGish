# testNumbo1.py -- Unit tests for Numbo1.py

import unittest
from pprint import pprint as pp
import inspect

from Numbo1 import detect_three_tens


class TestNumbo1(unittest.TestCase):

    def test_detect_three_tens(self) -> None:
        self.assertIsNone(detect_three_tens([]))
        self.assertIsNone(detect_three_tens([1, 2, 3]))
        self.assertIsNone(detect_three_tens([1, 10, 2, 11]))
        self.assertCountEqual(detect_three_tens(
            [1, 10, 2, 11, 10]),
            [10, 10, 11]
        )
        got = detect_three_tens([1, 10, 2, 19, 20, 11, 10])
        self.assertEqual(len(got), 3)
