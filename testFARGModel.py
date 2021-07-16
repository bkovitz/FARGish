# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect

from FARGModel import FARGModel, Canvas, SeqCanvas, SeqState

class testFARGModel(unittest.TestCase):

    def test_basics(self):
        fm = FARGModel()

        # Build something
        ca = fm.build(SeqCanvas([SeqState((4, 5, 6), None)]))
        # Is it there?
        eiws = fm.get_eiws(ca)
        self.assertEqual(eiws.elem, ca)
        # Can we find it by exact match?
        self.assertEqual(fm.the(ca), ca)
        # Can we find it by searching for its class?
        self.assertEqual(fm.the(Canvas), ca)

        # TODO Check builder and behalf_of

        # NEXT Paint the next state? Log the BUILT?

