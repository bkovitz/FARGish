# testNumberMatcher.py -- Unit tests for NumberMatcher.py

import unittest
from pprint import pprint as pp
import inspect
from dataclasses import dataclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable, Hashable, Collection, \
    Sequence
import operator
from operator import itemgetter, attrgetter

from util import pr, pts, is_iter, first

from NumberMatcher import NumberMatcher, oom_bounds


class TestNumberMatcher(unittest.TestCase):

    def test_numbermatcher_one_peak(self):
        nm = NumberMatcher(lb=1, ub=10, targets=[4.0], peakwidth=0.001)
        self.assertAlmostEqual(nm.f(4.0), 1.0)
        self.assertAlmostEqual(nm.f(5.0), 0.0, places=5)

    def test_numbermatcher_two_peaks(self):
        nm = NumberMatcher(lb=1, ub=10, targets=[4.0, 6.0], peakwidth=1.0)
        f0, f3, f4, f5, f6 = [nm.f(x) for x in [0.0, 3.0, 4.0, 5.0, 6.0]]
        #print(f0, f3, f4, f5, f6)
        self.assertAlmostEqual(f4, 1.0)
        self.assertAlmostEqual(f6, 1.0)
        self.assertLess(f5, f4)
        self.assertLess(f3, f5)
        self.assertLess(f0, f3)

    def test_numbermatcher_no_peaks(self):
        nm = NumberMatcher(lb=1, ub=10, targets=[], peakwidth=1.0)
        self.assertAlmostEqual(nm.f(4.0), 0.0)
        self.assertAlmostEqual(nm.f(5.0), 0.0)

    def test_numbermatcher_4_40(self):
        # Verifies match in a different order of magnitude
        nm = NumberMatcher(lb=1, ub=10, targets=[4.0, 6.0], peakwidth=1.0)
        self.assertAlmostEqual(nm.f(4.0), nm.f(40.0, lb=10))
        self.assertAlmostEqual(nm.f(2.0), nm.f(20.0, lb=10))

    def test_oom_bounds_4(self):
        lb, ub = oom_bounds(4)
        self.assertEqual(lb, 1)
        self.assertEqual(ub, 10)

    def test_oom_bounds_10(self):
        lb, ub = oom_bounds(10)
        self.assertEqual(lb, 10)
        self.assertEqual(ub, 100)

    def test_oom_bounds_minus_4(self):
        lb, ub = oom_bounds(-4)
        self.assertEqual(lb, -10)
        self.assertEqual(ub, -1)

    def test_oom_bounds_0(self):
        lb, ub = oom_bounds(0)
        self.assertEqual(lb, -1)
        self.assertEqual(ub, +1)
