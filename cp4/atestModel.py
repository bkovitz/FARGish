# atestModel.py -- Acceptance tests for the canvas-and-painters model

import unittest
import inspect
from pprint import pp
if 'unittest.util' in __import__('sys').modules:
    # Show full diff in self.assertEqual.
    __import__('sys').modules['unittest.util']._MAX_LENGTH = 999999999

from Model import Workspace


class TestModel(unittest.TestCase):

    def test_make_and_run_succ(self) -> None:
        ws = Workspace()
        # run Succ detector on 'ab_', get Succ((C1, 1), (C1, 2))
        # run ArgumentRelationDetector on that Succ, get PainterCluster
        # run PainterCluster on AA1=(C1, 2), get Succ((C1, 2), (C1, 3))
        # run that Succ, get a new canvas: 'abc'
