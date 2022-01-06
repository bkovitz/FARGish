# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect

from FARGModel import FARGModel, ActionCanvas, Avails, Plus, plus, Produced
from util import ps, pss, pr

class TestFARGModel(unittest.TestCase):

    def test_actioncanvas(self) -> None:
        ca = ActionCanvas.make(Avails(4, 5, 6), Plus(4, 5))
        self.assertEqual(pss(ca), '[ avails=(4, 5, 6) / 4 + 5 ]')

        fm = FARGModel()
        ca = fm.build(ca)
        #ps(ca)

        got = ca.run()
        self.assertEqual(got, Produced(Avails(6, 9)))

        got = fm.run(ca)
        self.assertEqual(got, Produced(Avails(6, 9)))

        # doing a Consume (without a Canvas or FARGModel)
    def test_consume(self):
        got = Plus(4, 5).run((4, 5, 6), plus, (4, 5))
        self.assertEqual(got, Produced(Avails(6, 9)))
