# testModel.py -- Unit tests for Model.py

import unittest
import inspect

#from Addrs import F, I, Indices, J, WorkingSoup 
#from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
#    same, succ
#from Subst import Subst, empty_subst, Plus
#from Funcs import MakeBetweenPainter, MakeRelativeIndirectPainter, SimpleFunc
#from Painters import Painter
#from Soup import Soup

from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ, \
    F, I, Indices, J, SR, \
    Subst, empty_subst, Plus, \
    MakeBetweenPainter, MakeRelativeIndirectPainter, SimpleFunc, \
    Painter, \
    Soup
from Log import lo, set_log_level
from util import pts, reseed, short

class TestModel(unittest.TestCase):

    def tearDown(self) -> None:
        set_log_level(0)

    maxDiff = None
    def test_related_pair_detpainters(self) -> None:
        model = Model.canvas_from('ajaqb')
        p = Painter(RelatedPair(I, J, F), SR.WorkingSoup, Painter(I, J, F))
#        lo('RESULT')
#        for dp in model.painter_to_detpainters(p):
#            lo(type(dp), dp)
        self.assertCountEqual(
            [dp.as_painter() for dp in model.painter_to_detpainters(p)],
            [
                (Indices(1, 3), SR.WorkingSoup, Painter.make_from(1, 3, same)),
                (Indices(3, 5), SR.WorkingSoup, Painter.make_from(3, 5, succ)),
                (Indices(1, 5), SR.WorkingSoup, Painter.make_from(1, 5, succ))
            ]
        )

    def test_match_all_abs_painters(self) -> None:
        model = Model.make_from('ajaqb', lts=Soup())
        model.ws.add(
            Painter.make_from(1, 3, same),
            Painter.make_from(3, 5, succ),
            Painter.make_from(1, 5, succ)
        )
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, Painter(I, J, F)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    Painter.make_from(1, 3, same)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 3), (J, 5), (F, succ)),
                    Painter.make_from(3, 5, succ)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 5), (F, succ)),
                    Painter.make_from(1, 5, succ)
                )
            ]
        )

    def test_match_some_abs_painters(self) -> None:
        '''Tests (I, Plus(I, 2), F).'''
        model = Model.make_from('ajaqb', lts=Soup())
        model.ws.add(
            Painter.make_from(1, 3, same),
            Painter.make_from(3, 5, succ),
            Painter.make_from(1, 5, succ)
        )
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, Painter(I, Plus(I, 2), F)),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    Painter.make_from(1, 3, same)
                ),
                DetAddrWithSubst(
                    Subst.make_from((I, 3), (J, 5), (F, succ)),
                    Painter.make_from(3, 5, succ)
                )
            ]
        )

    def test_absorb(self) -> None:
        reseed(0)
        model = Model()
        model.absorb('ajaqb')
        print(model.lts.state_str())
        # TODO Test the contents of the lts
