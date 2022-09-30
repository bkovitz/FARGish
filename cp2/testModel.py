# testModel.py -- Unit tests for Model.py

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


class TestModel(unittest.TestCase):

    def tearDown(self) -> None:
        set_log_level(0)

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

    def test_match_all_abs_painters(self) -> None:
        model = Model.make_from('ajaqb', lts=Soup())
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
        '''Tests (I, Plus(I, 2), F).'''
        model = Model.make_from('ajaqb', lts=Soup())
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

    def test_absorb(self) -> None:
        reseed(0)
        model = Model()
        model.absorb('ajaqb')
        print(model.lts.state_str())
        # TODO Test the contents of the lts
