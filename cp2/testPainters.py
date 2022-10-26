# testPainters.py -- Unit tests for painters (not just the ones in Painters.py)

import unittest
import inspect

#from Types import Annotations, CellBundle, Letter, Start
#from Addrs import F, I, Index, Indices, J, MatchContent, WorkingSoup
#from Painters import Painter
#from Funcs import SimpleFunc
#from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
#    same, succ
#from Subst import Subst, empty_subst, Plus
#from Funcs import MakeBetweenPainter, MakeRelativeIndirectPainter, same
#from Soup import Soup

from Model import Annotations, CellBundle, Letter, Start, \
    F, I, Index, Indices, J, MatchContent, SR, \
    Painter, \
    SimpleFunc, \
    Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ, \
    Subst, empty_subst, Plus, \
    MakeBetweenPainter, MakeRelativeIndirectPainter, same, \
    Soup, \
    FizzleGotBlank, Succ, TwoAdjacentLetters, MakeDigraphPainter
    
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
        p = (1, 3, same)
        model = Model.canvas_from('a    ')
        model.run_detpainter(DetPainter.make_from(p))
        self.assertEqual(model.canvas.short_str(), 'a a  ')

    def test_rel_painter(self) -> None:
        p = Painter.make_from('a', Plus(I, 2), succ)
        model = Model.canvas_from('a    ')

        dp = self.painter_to_one_detpainter(model, p)
        self.assertEqual(dp.as_painter(), (Index(1), Index(3), succ))
        model.run_detpainter(dp)
        self.assertEqual(model.canvas.short_str(), 'a b  ')

    def test_related_pair_painter(self) -> None:
        model = Model.canvas_from('ajaqb')
        dp = DetPainter.make_from(
            (Indices(1, 3), SR.WorkingSoup, (1, 3, same))
        )
        #model.run_detpainter((Indices(1, 3), WorkingSoup, (1, 3, same)))
        model.run_detpainter(dp)
        #lo(model.ws.state_str())
        self.assertTrue(Painter.make_from(1, 3, same) in model.ws)

    def test_make_between_painter(self) -> None:
        '''A cascade of three painters.'''
        p1: Painter = Painter(
            Painter(I, Plus(I, 2), F),
                # TODO Better:  (Filled(I), Filled(Plus(I, 2), F)
            SR.WorkingSoup,
            MakeBetweenPainter(I, J, F)
        )
        p2: Painter = Painter(
            Painter(I, Plus(I, 2), same),
            SR.WorkingSoup,
            Painter(I, Plus(I, 1), Letter('j'))
        )
        p3: Painter = Painter.make_from(1, 2, 'j')

        model = Model.make_from('ajaqb', lts=Soup())
        model.ws.add(Painter.make_from(1, 3, same)) # p1 will match this painter

        dpainters = list(model.painter_to_detpainters(p1))  # TODO OAOO
        #lo('GOT', dpainters[0])
        self.assertEqual(len(dpainters), 1)
        model.run_detpainter(dpainters[0])
        self.assertIn(p2, model.ws)

        dp2s = list(model.painter_to_detpainters(p2))  # TODO OAOO
        self.assertEqual(len(dp2s), 1)
        model.run_detpainter(dp2s[0])
        self.assertIn(p3, model.ws)

    def test_mbt_fizzle_if_blank(self) -> None:
        model = Model.make_from('ab ')
        self.helper_mbt_fizzle(model)

        model = Model.make_from('a a')
        self.helper_mbt_fizzle(model)

        model = Model.make_from(' ba')
        self.helper_mbt_fizzle(model)

    def helper_mbt_fizzle(self, model: Model) -> None:
        f = MakeBetweenPainter(I, J, F)
        su = Subst.make_from((I, Index(1)), (J, Index(3)), (F, same))
        with self.assertRaises(FizzleGotBlank):
            got = model.apply_func(su, f, Letter('x'))

    def test_make_relative_indirect_painter(self) -> None:
        p1 = Painter(
            Painter(I, J, SimpleFunc(F)),
            SR.WorkingSoup,
            MakeRelativeIndirectPainter(I, J, F)
        )
        p2 = Painter.make_from('a', SR.WorkingSoup, Painter(I, Plus(I, 2), same))
        dp2 = DetPainter(
            Subst.make_from((I, 1)),
            Index(1),
            SR.WorkingSoup,
            Painter.make_from(1, 3, same),
            1,
            p2
        )
        p3: Painter = Painter.make_from(1, 3, same)

        model = Model.make_from('ajaqb', auto_annotate=[])
        #lo(model.canvas.as_bundle(Index(1)))
        model.ws.add(Painter.make_from(1, 3, same))  # p1 will match this painter

        dp = self.painter_to_one_detpainter(model, p1)
        model.run_detpainter(dp)
        self.assertIn(p2, model.ws)

        model.ws.remove(Painter.make_from(1, 3, same))
        dp2s = list(model.painter_to_detpainters(p2))
        self.assertIn(dp2, dp2s)
        model.run_detpainter(dp2)
        self.assertIn(p3, model.ws)

    maxDiff = None
    def test_make_relative_indirect_painter_with_annotation(self) -> None:
        model = Model.canvas_from('ajaqb')
        func = MakeRelativeIndirectPainter(I, J, F)
        subst = Subst.make_from((I, 1), (J, 3), (F, same))
        self.assertEqual(
            model.apply_func(subst, func, None),
            Painter(
                MatchContent(
                    CellBundle(Letter('a'),
                    Annotations.make_from(Start))
                ),
                SR.WorkingSoup,
                Painter(I, Plus(I, 2), same)
            )
        )

    def test_digraph_painter(self) -> None:
        m = Model.canvas_from(' a  ')
        m.run_painter(Painter.make_from('a', Plus(I, 1), 'j'))
        self.assertEqual(m.canvas.short_str(), ' aj ')

    def test_make_digraph_painter(self) -> None:
        m = Model.make_from(' aj b')
        m.run_painter(
            Painter(TwoAdjacentLetters(), SR.WorkingSoup, MakeDigraphPainter())
        )
        self.assertIn(
            Painter(MatchContent(Letter('a')), Plus(I, 1), Letter('j')),
            m.ws
        )

