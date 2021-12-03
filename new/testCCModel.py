# testCCModel.py -- Unit tests for the "codelets in canvases" model

import unittest
from pprint import pprint as pp
import inspect

from CCModel import SeqCanvas, ArgsMap, Avails, Plus, run
from util import ps, pr


class TestCCModel(unittest.TestCase):

    def test_4_plus_5(self):
        ca = SeqCanvas.make(
            Avails(4, 5),
            Plus(4, 5),
            None, #ArgsMap.empty()
        )
        run(ca, ArgsMap.empty())
        c = ca._cells[2].contents
        self.assertEqual(c, Avails(9))
