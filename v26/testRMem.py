# testRMem.py -- Unit tests for RMem.py

import unittest
from pprint import pprint as pp
import inspect


from RMem import RMem, CanvasPrep, make_eqns, BaseValue, ndups, no_prep, \
    Canvas1D
from util import as_tuple, pr, pts, ps, pss, psa, sample_without_replacement, \
    reseed

class TestRMem(unittest.TestCase):

    def test_make_generators_partial_canvas(self) -> None:
        self.assertCountEqual(
            RMem.make_generators((1, '+', None, None, None)),
            [(1, 2, '+'), (2, 1, 1)]
        )

    def test_canvas1d(self) -> None:
        c = Canvas1D([None, '+', 2, None, 5])
        self.assertEqual(c.all_clarities(), [0, 5, 5, 0, 5])

        c[1] = 3  # 1-based indexing
        self.assertEqual(as_tuple(c), (3, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [1, 5, 5, 0, 5])
            # clarity is up

        c[1] = -8  # should return cell 1 to None, clarity to 0
        self.assertEqual(as_tuple(c), (None, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [0, 5, 5, 0, 5])

        c[3] = 8  # should only decrease clarity, not affect cell
        self.assertEqual(as_tuple(c), (None, '+', 2, None, 5))
        self.assertEqual(c.all_clarities(), [0, 5, 4, 0, 5])
