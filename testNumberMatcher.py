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

from NumberMatcher import SingleNumberMatcher, NumberMatcher, oom_bounds, \
    NumberTupleMatcher


class TestNumberMatcher(unittest.TestCase):

    def test_numbermatcher_one_peak(self):
        nm = SingleNumberMatcher(4.0, peakwidth=0.001)
        self.assertAlmostEqual(nm(4.0), 1.0)
        self.assertAlmostEqual(nm(5.0), 0.0, places=5)

    def test_numbermatcher_two_peaks(self):
        #nm = SingleNumberMatcher(lb=1, ub=10, targets=[4.0, 6.0], peakwidth=1.0)
        nm = NumberMatcher.make(4.0, 6.0, peakwidth=1.0)
        f0, f3, f4, f5, f6 = [nm(x) for x in [0.0, 3.0, 4.0, 5.0, 6.0]]
        #print('TWOPEAKS', f0, f3, f4, f5, f6)
        self.assertAlmostEqual(f4, 1.0)
        self.assertAlmostEqual(f6, 1.0)
        self.assertLess(f5, f4)
        self.assertLess(f3, f5)
        self.assertLess(f0, f3)

    @unittest.skip('not sure this is needed anymore')
    def test_numbermatcher_no_peaks(self):
        nm = SingleNumberMatcher(lb=1, ub=10, targets=[], peakwidth=1.0)
        self.assertAlmostEqual(nm(4.0), 0.0)
        self.assertAlmostEqual(nm(5.0), 0.0)

    def test_numbermatcher_4_40(self):
        # Verifies match in a different order of magnitude
        #nm = SingleNumberMatcher(lb=1, ub=10, targets=[4.0, 6.0], peakwidth=1.0)
        nm = NumberMatcher.make(4.0, 6.0, peakwidth=1.0)
        self.assertAlmostEqual(nm(4.0), nm(40.0, lb=10))
        self.assertAlmostEqual(nm(2.0), nm(20.0, lb=10))

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

    def test_numbermatcher_make_4(self):
        nm = NumberMatcher.make(4)
        self.assertEqual(nm.lb, 1)
        self.assertEqual(nm.ub, 10)
        self.assertAlmostEqual(nm(4), 1.0)
        self.assertAlmostEqual(nm(5), 0.0, places=4)
        
    def test_numbermatcher_make_4_or_5(self):
        nm = NumberMatcher.make(4, 5)
        self.assertEqual(nm.lb, 1)
        self.assertEqual(nm.ub, 10)
        self.assertAlmostEqual(nm(4), 1.0)
        self.assertAlmostEqual(nm(5), 1.0)
        self.assertAlmostEqual(nm(6), 0.0, places=4)
        
    def test_numbermatcher_make_4_or_15(self):
        nm = NumberMatcher.make(4, 15)
        self.assertEqual(nm.lb, 1)
        self.assertEqual(nm.ub, 100)
        self.assertAlmostEqual(nm(4), 1.0)
        self.assertAlmostEqual(nm(15), 1.0)
        self.assertAlmostEqual(nm(6), 0.0, places=4)

    def test_numbertuplematcher(self):
        nm = NumberTupleMatcher((
            NumberMatcher.make(4),
            NumberMatcher.make(4)
        ))
        ls = [4, 5]
        self.assertEqual(nm(ls), 0.0)
        self.assertEqual(ls, [4, 5])  # verify non-destructive
        self.assertEqual(nm((4, 4)), 1.0)

        nm = NumberTupleMatcher((
            NumberMatcher.make(4, peakwidth=1.0),
            NumberMatcher.make(4, peakwidth=1.0)
        ))
        #print('UT', nm((4, 5)))  0.074? This seems too low; 11-Aug-2021
        self.assertGreater(nm((4, 5)), 0.0)
        self.assertLess(nm((4, 5)), 1.0)

    def test_tuple_4_5(self):
        nm = NumberMatcher.make((4, 5))
        self.assertEqual(nm.lb, 1)
        self.assertEqual(nm.ub, 10)
        self.assertAlmostEqual(nm((4, 5)), 1.0)
        self.assertAlmostEqual(nm((4, 4)), 0.0, places=4)

    def test_tuple_4_5_against_40_50(self):
        nm = NumberMatcher.make((4, 5))
        self.assertAlmostEqual(nm((40, 50), lb=10), 1.0)
        self.assertAlmostEqual(nm((4, 5), lb=10), 0.0, places=4)
