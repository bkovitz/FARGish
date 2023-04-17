# atestModel3.py -- Acceptance tests for the "FakeIt" canvas-and-painters model
#                   as it grows into a real model

import unittest
import inspect
from pprint import pp
if 'unittest.util' in __import__('sys').modules:
    # Show full diff in self.assertEqual.
    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999

from Model3 import run


class TestModel3(unittest.TestCase):

    def test_ab_(self) -> None:
        self.assertEqual(run('ab_'), 'abc')
        # run Succ detector on 'ab_', get Succ((C1, 1), (C1, 2))
        # run ArgumentRelationDetector on that Succ, get PainterCluster
        # run PainterCluster on AA1=(C1, 2), get Succ((C1, 2), (C1, 3))
        # run that Succ, get a new canvas: 'abc'

