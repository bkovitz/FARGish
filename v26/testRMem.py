# testRMem.py -- Unit tests for RMem.py

import unittest
from pprint import pprint as pp
import inspect


from RMem import RMem, CanvasPrep, make_eqns, BaseValue, ndups, no_prep
from util import as_tuple, pr, pts, ps, pss, psa, sample_without_replacement, \
    reseed

class TestRMem(unittest.TestCase):

    def test_make_generators_partial_canvas(self) -> None:
        self.assertCountEqual(
            RMem.make_generators((1, '+', None, None, None)),
            [(1, 2, '+'), (2, 1, 1)]
        )
