# testSoup.py -- Unit tests for Soup.py

import unittest
import inspect

from Types import I
from Soup import Soup
from Subst import Subst, Plus
from Funcs import succ, same


class TestSoup(unittest.TestCase):

    def test_is_match_1(self) -> None:
        soup = Soup()
        self.assertEqual(
            soup.is_match((I, Plus(I, 2), succ), (1, 3, succ)),
            Subst.make_from((I, 1))
        )

    def test_is_match_2(self) -> None:
        soup = Soup()
        self.assertFalse(soup.is_match((I, Plus(I, 2), succ), (1, 3, same)))
        
