# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect

from FARGModel import FARGModel, SeqCanvas, SeqState

class testFARGModel(unittest.TestCase):

    def test_basics(self):
        fm = FARGModel()
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        eiws = fm.get_eiws(ca)
        self.assertEqual(eiws.elem, ca)

