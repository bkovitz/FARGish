# testFARGModel.py -- Unit tests for FARGModel.py

import unittest
from pprint import pprint as pp
import inspect

from FARGModel import FARGModel, ActionCanvas, Avails, Plus, plus, Produced, \
    Addr, NEXT, PREV, SITUATION, ACTION, RelationUnknownToCanvas
from util import ps, pss, pr, empty_set


class TestFARGModel(unittest.TestCase):

    def test_actioncanvas(self) -> None:
        ca = ActionCanvas.make(Avails(4, 5, 6), Plus(4, 5))
        self.assertEqual(pss(ca), '[ avails=(4, 5, 6) / 4 + 5 ]')

        fm = FARGModel()
        ca = fm.build(ca)
        #ps(ca)

        got = ca.run()
        self.assertEqual(got, Produced(Avails(6, 9)))

        #got = fm.run(ca)
        #self.assertEqual(got, Produced(Avails(6, 9)))

        self.assertEqual(
            pss(ca),
            '[ avails=(4, 5, 6) / 4 + 5 ][ avails=(6, 9) / None ]'
        )

        # doing a Consume (without a Canvas or FARGModel)
    def test_consume(self):
        got = Plus(4, 5).run((4, 5, 6), plus, (4, 5))
        self.assertEqual(got, Produced(Avails(6, 9)))

    def test_actioncanvas_jump(self) -> None:
        ca = ActionCanvas.make()
        as0 = Addr(ca, 'situation', 0)
        as1 = Addr(ca, 'situation', 1)
        aa0 = Addr(ca, 'action', 0)
        aa1 = Addr(ca, 'action', 1)

        self.assertEqual(ca.jump(as0, NEXT), {as1})
        self.assertEqual(ca.jump(as0, PREV), empty_set)

        self.assertEqual(ca.jump(as0, ACTION), {aa0})
        self.assertEqual(ca.jump(as0, SITUATION), {as0})
        self.assertEqual(ca.jump(aa0, ACTION), {aa0})
        self.assertEqual(ca.jump(aa0, SITUATION), {as0})

        with self.assertRaises(RelationUnknownToCanvas) as cm:
            ca.jump(as0, 'undefined')
        self.assertEqual(
            cm.exception,
            RelationUnknownToCanvas(ca, as0, 'undefined')
        )
