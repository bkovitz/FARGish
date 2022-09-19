# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Types import I, J
from Model import Model, DetAddrWithSubst
from Subst import Subst, empty_subst


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

    # TODO 'I' where 'I' already has a value
