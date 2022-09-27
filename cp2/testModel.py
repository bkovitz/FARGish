# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Types import F, I, Indices, J, Painter, WorkingSoup
from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ, MakeBetweenPainter
from Subst import Subst, empty_subst, Plus
from Log import lo, set_log_level
from util import short


class TestModel(unittest.TestCase):

    def test_detaddr_1(self) -> None:
        model = Model()
        model.set_canvas('a    ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, 1),
            [DetAddrWithSubst(Subst.make_from((I, 1)), 1)]
        )

    def test_detaddr_a(self) -> None:
        model = Model()
        model.set_canvas('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, 'a'),
            [
                DetAddrWithSubst(Subst.make_from((I, 1)), 1),
                DetAddrWithSubst(Subst.make_from((I, 3)), 3),
            ]
        )

    def test_detaddr_I(self) -> None:
        '''Matching against an undefined Variable returns all indices
        in the canvas.'''
        model = Model()
        model.set_canvas('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, I),
            [
                DetAddrWithSubst(Subst.make_from((I, 1)), 1),
                DetAddrWithSubst(Subst.make_from((I, 2)), 2),
                DetAddrWithSubst(Subst.make_from((I, 3)), 3),
                DetAddrWithSubst(Subst.make_from((I, 4)), 4),
                DetAddrWithSubst(Subst.make_from((I, 5)), 5),
            ]
        )

    def test_detaddr_I_already_defined(self) -> None:
        # Matching against a defined Variable returns whatever Addr(s)
        # make up the value of that Variable.
        model = Model()
        model.set_canvas('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(Subst.make_from((I, 2)), I, I),
            [
                DetAddrWithSubst(Subst.make_from((I, 2)), 2),
            ]
        )

    def test_detaddr_I_plus_2(self) -> None:
        # Matching against I+2 when I is undefined returns no determinate
        # addresses.
        # TODO Should we return all but indices 1 and 2?
        model = Model()
        model.set_canvas('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, J, Plus(I, 2)),
            []
        )

    def test_detaddr_I_plus_2_already_defined(self) -> None:
        # Matching against I+2 when I is defined returns the index I+2
        # (evaluated).
        model = Model()
        model.set_canvas('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(Subst.make_from((I, 1)), J, Plus(I, 2)),
            [
                DetAddrWithSubst(Subst.make_from((I, 1), (J, 3)), 3),
            ]
        )

    def test_related_pair_detaddrs(self) -> None:
        model = Model.canvas_from('ajaqb')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, RelatedPair(I, J, F)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    Indices(1, 3)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 5), (F, succ)),
                    Indices(1, 5)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 3), (J, 5), (F, succ)),
                    Indices(3, 5)
                )
            ]
        )

        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, RelatedPair(I, J, same)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3)),
                    Indices(1, 3)
                ),
            ]
        )

    maxDiff = None
    def test_related_pair_detpainters(self) -> None:
        model = Model.canvas_from('ajaqb')
        p: Painter = (RelatedPair(I, J, F), WorkingSoup, (I, J, F))
#        lo('RESULT')
#        for dp in model.painter_to_detpainters(p):
#            lo(type(dp), dp)
        self.assertCountEqual(
            [dp.as_painter() for dp in model.painter_to_detpainters(p)],
            [
                (Indices(1, 3), WorkingSoup, (1, 3, same)),
                (Indices(3, 5), WorkingSoup, (3, 5, succ)),
                (Indices(1, 5), WorkingSoup, (1, 5, succ))
            ]
        )

    def test_related_pair_painter(self) -> None:
        model = Model.canvas_from('ajaqb')
        dp = DetPainter.make_from(
            (Indices(1, 3), WorkingSoup, (1, 3, same))
        )
        #model.run_detpainter((Indices(1, 3), WorkingSoup, (1, 3, same)))
        model.run_detpainter(dp)
        #lo(model.ws.state_str())
        self.assertTrue((1, 3, same) in model.ws)

    def test_match_all_abs_painters(self) -> None:
        model = Model.canvas_from('ajaqb')
        model.ws.add(
            (1, 3, same),
            (3, 5, succ),
            (1, 5, succ)
        )
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, (I, J, F)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    (1, 3, same)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 3), (J, 5), (F, succ)),
                    (3, 5, succ)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 5), (F, succ)),
                    (1, 5, succ)
                )
            ]
        )

    def test_match_some_abs_painters(self) -> None:
        model = Model.canvas_from('ajaqb')
        model.ws.add(
            (1, 3, same),
            (3, 5, succ),
            (1, 5, succ)
        )
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, (I, Plus(I, 2), F)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    (1, 3, same)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 3), (J, 5), (F, succ)),
                    (3, 5, succ)
                )
            ]
        )

    def test_make_between_painter(self) -> None:
        '''A cascade of three painters.'''
        set_log_level(9)
        p1: Painter = (
            (I, Plus(I, 2), F),
                # TODO Better:  (Filled(I), Filled(Plus(I, 2), F)
            WorkingSoup,
            MakeBetweenPainter(I, J, F)
        )
        p2: Painter = ((I, Plus(I, 2), same), WorkingSoup, (I, Plus(I, 1), 'j'))
        p3: Painter = (1, 2, 'j')

        model = Model.canvas_from('ajaqb')
        model.ws.add((1, 3, same))  # p1 will match this painter

        dpainters = list(model.painter_to_detpainters(p1))  # TODO OAOO
        self.assertEqual(len(dpainters), 1)
        model.run_detpainter(dpainters[0])
        self.assertIn(p2, model.ws)

        dp2s = list(model.painter_to_detpainters(p2))  # TODO OAOO
        self.assertEqual(len(dp2s), 1)
        model.run_detpainter(dp2s[0])
        self.assertIn(p3, model.ws)

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

#    # I think this test is wrong in its very conception
#    def test_rel_indirect_painter(self) -> None:
#        p1: Painter = ('a', WorkingSoup, (I, Plus(I, 2), succ))
#        p2: Painter = (3, 5, succ)
#        model = Model.canvas_from('  a  ')
#
#        dp = self.painter_to_one_detpainter(model, p1)
#        self.assertEqual(
#            dp.as_painter(),
#            (3, WorkingSoup, (I, Plus(I, 2), succ))
#        )
#        model.run_detpainter(dp)
#        self.assertIn(p2, model.ws)

    def painter_to_one_detpainter(self, model: Model, p: Painter) -> DetPainter:
        dpainters = list(model.painter_to_detpainters(p))
        self.assertEqual(len(dpainters), 1,
            f'Painter {p} generated multiple DetPainters: {short(dpainters)}'
        )
        return dpainters[0]

    def test_absorb(self) -> None:
        model = Model()
        model.absorb('ajaqb')
        print(model.lts.state_str())
        # TODO Test the contents of the lts
