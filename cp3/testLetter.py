# testLetter.py -- Unit tests for Letter and Blank

import unittest
import inspect

from Model import Letter, Blank, FizzleNoSucc, FizzleNoPred

from Log import lo, set_log_level
from util import pts, reseed, short


class TestLetter(unittest.TestCase):

    def test_letter(self) -> None:
        self.assertEqual(Letter('a').succ(), Letter('b'))
        self.assertEqual(Letter.from_str('c'), Letter('c'))
        self.assertEqual(Letter.from_str(' '), Blank())
        with self.assertRaises(FizzleNoSucc):
            Letter('z').succ()
        with self.assertRaises(FizzleNoPred):
            Letter('a').pred()
        with self.assertRaises(ValueError):
            Letter('aa')
        with self.assertRaises(ValueError):
            Letter(' ')
