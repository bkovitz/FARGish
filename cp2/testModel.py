# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Types import F, I, Indices, J, Painter, WorkingSoup
from Model import Model, DetAddrWithSubst, RelatedPair
from Subst import Subst, empty_subst, Plus
from Funcs import same, succ


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

    def test_detaddr_related_pairs(self) -> None:
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

    def test_detpainter_related_pairs(self) -> None:
        model = Model.canvas_from('ajaqb')
        p: Painter = (RelatedPair(I, J, F), WorkingSoup, (I, J, F))
        self.assertCountEqual(
            model.painter_to_detpainters(p),
            [
                (1, 3, same),
                (3, 5, succ),
                (1, 5, succ)
            ]
        )
