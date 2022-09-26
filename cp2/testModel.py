# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Types import F, I, Indices, J, Painter, WorkingSoup
from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ, MakeBetweenPainter
from Subst import Subst, empty_subst, Plus
from Log import lo


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

#    def test_fill_variables_in_make_between_painter(self) -> None:
#        model = Model.canvas_from('ajaqb')
#        subst = Subst.make_from((I, 1), (J, 3), (F, same))
#        self.assertCountEqual(
#            model.func_to_detfuncs(
#                subst, F, MakeBetweenPainter(I, Plus(I, 1), F)
#            ),
#            [
#                DetFunc
#            ]
#        )
        
    def test_make_between_painter(self) -> None:
        model = Model.canvas_from('ajaqb')
        #model.ws.add((Indices(1, 3), WorkingSoup, (1, 3, same)))
        model.ws.add((1, 3, same))
        p: Painter = (
            (I, Plus(I, 2), F),
            WorkingSoup,
            MakeBetweenPainter(I, Plus(I, 1), F)
        )
        # TODO Better:  (Filled(I), Filled(Plus(I, 2), F)
        dpainters = list(model.painter_to_detpainters(p))
        #lo('DP', dpainters[0])
        self.assertEqual(len(dpainters), 1)
        model.run_detpainter(dpainters[0])
        #print(model.ws.state_str())
        self.assertIn((1, 2, (I, Plus(I, 1), 'j')), model.ws)
        #self.assertIn((1, 2, 'j'), model.ws)
