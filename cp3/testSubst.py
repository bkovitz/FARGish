# testSubst.py -- Unit tests for Subst

import unittest
import inspect

from Model import bottom_subst, I, J, Subst

from Log import lo, set_log_level
from util import pts, reseed, short


class TestSubst(unittest.TestCase):

    def test_basics(self) -> None:
        su = Subst()
        self.assertEqual(su.as_index(I), None)
        su = su.unify(I, 2)
        self.assertEqual(su.as_index(I), 2)
        su = su.unify(I, 3)
        self.assertEqual(su, bottom_subst)
