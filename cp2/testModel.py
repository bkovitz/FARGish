# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Model


class TestModel(unittest.TestCase):

    def test_detaddr_1(self) -> None:
        model = Model.make_from('a    ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, 1),
            [DetAddrWithSubst(Subst(I=1), 1)]
        )

    def test_detaddr_a(self) -> None:
        model = Model.make_from('a a  ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, 'a'),
            [
                DetAddrWithSubst(I=1, 1),
                DetAddrWithSubst(I=3, 3),
            ]
        )
