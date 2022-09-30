# testPainters.py -- Unit tests for painters (not just the ones in Painters.py)

import unittest
import inspect

from Types import F, I, Indices, J, Painter, SimpleFunc, WorkingSoup
from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ
from Subst import Subst, empty_subst, Plus
from Funcs import MakeBetweenPainter, MakeRelativeIndirectPainter
from Soup import Soup
from Log import lo, set_log_level
from util import pts, reseed, short

class TestPainters(unittest.TestCase):

    def tearDown(self) -> None:
        set_log_level(0)

    def painter_to_one_detpainter(self, model: Model, p: Painter) -> DetPainter:
        dpainters = list(model.painter_to_detpainters(p))
        self.assertEqual(len(dpainters), 1,
            f'Painter {p} generated multiple DetPainters: {short(dpainters)}'
        )
        return dpainters[0]

    def test_abs_painter(self) -> None:
        p: Painter = (1, 3, same)
        model = Model.canvas_from('a    ')
        model.run_detpainter(DetPainter.make_from(p))
        self.assertEqual(model.canvas.short_str(), 'a a  ')

    def test_rel_painter(self) -> None:
        p: Painter = ('a', Plus(I, 2), succ)
        model = Model.canvas_from('a    ')

        dp = self.painter_to_one_detpainter(model, p)
        self.assertEqual(dp.as_painter(), (1, 3, succ))
        model.run_detpainter(dp)
        self.assertEqual(model.canvas.short_str(), 'a b  ')

    def test_related_pair_painter(self) -> None:
        model = Model.canvas_from('ajaqb')
        dp = DetPainter.make_from(
            (Indices(1, 3), WorkingSoup, (1, 3, same))
        )
        #model.run_detpainter((Indices(1, 3), WorkingSoup, (1, 3, same)))
        model.run_detpainter(dp)
        #lo(model.ws.state_str())
        self.assertTrue((1, 3, same) in model.ws)

    def test_make_between_painter(self) -> None:
        '''A cascade of three painters.'''
        p1: Painter = (
            (I, Plus(I, 2), F),
                # TODO Better:  (Filled(I), Filled(Plus(I, 2), F)
            WorkingSoup,
            MakeBetweenPainter(I, J, F)
        )
        p2: Painter = ((I, Plus(I, 2), same), WorkingSoup, (I, Plus(I, 1), 'j'))
        p3: Painter = (1, 2, 'j')

        model = Model.make_from('ajaqb', lts=Soup())
        model.ws.add((1, 3, same))  # p1 will match this painter

        dpainters = list(model.painter_to_detpainters(p1))  # TODO OAOO
        self.assertEqual(len(dpainters), 1)
        #lo('GOT', dpainters[0])
        model.run_detpainter(dpainters[0])
        self.assertIn(p2, model.ws)

        dp2s = list(model.painter_to_detpainters(p2))  # TODO OAOO
        self.assertEqual(len(dp2s), 1)
        model.run_detpainter(dp2s[0])
        self.assertIn(p3, model.ws)

    def test_make_relative_indirect_painter(self) -> None:
        p1: Painter = (
            (I, J, SimpleFunc(F)),
            WorkingSoup,
            MakeRelativeIndirectPainter(I, J, F)
        )
        p2: Painter = ('a', WorkingSoup, (I, Plus(I, 2), same))
        dp2 = DetPainter(
            Subst.make_from((I, 1)),
            1,
            WorkingSoup,
            (1, 3, same),
            1,
            p2
        )
        p3: Painter = (1, 3, same)

        model = Model.canvas_from('ajaqb')
        model.ws.add((1, 3, same))  # p1 will match this painter

        dp = self.painter_to_one_detpainter(model, p1)
        model.run_detpainter(dp)
        self.assertIn(p2, model.ws)

        model.ws.remove((1, 3, same))
        dp2s = list(model.painter_to_detpainters(p2))
        self.assertIn(dp2, dp2s)
        model.run_detpainter(dp2)
        self.assertIn(p3, model.ws)
