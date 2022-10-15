# testAddrs.py -- Unit tests for various Addrs

import unittest
import inspect

#from Types import CellBundle, Letter
#from Subst import Subst, empty_subst, Plus
#from Addrs import F, I, Index, Indices, J, MatchContent, WorkingSoup
#from Painters import Painter
#from Funcs import MakeBetweenPainter, MakeRelativeIndirectPainter, SimpleFunc


from Model import Model, DetAddrWithSubst, DetPainter, RelatedPair, \
    same, succ, \
    CellBundle, Letter, \
    Subst, empty_subst, Plus, \
    F, I, Index, Indices, J, MatchContent, \
    Painter, \
    MakeBetweenPainter, MakeRelativeIndirectPainter, SimpleFunc

from Log import lo, set_log_level
from testCanvas import MyAnnotation
from util import pts, reseed, short


class TestAddrs(unittest.TestCase):

    def test_detaddr_1(self) -> None:
        model = Model()
        model.set_canvas('a    ')
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, Index(1)),
            [DetAddrWithSubst(Subst.make_from((I, 1)), Index(1))]
        )

    def test_detaddr_a(self) -> None:
        model = Model()
        model.set_canvas('a a  ')
        lo('CANVAS', model.canvas)
        set_log_level(6) #DEBUG
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, MatchContent(Letter('a'))),
            [
                DetAddrWithSubst(Subst.make_from((I, 1)), Index(1)),
                DetAddrWithSubst(Subst.make_from((I, 3)), Index(3)),
            ]
        )


    def test_detaddr_annotation(self) -> None:
        model = Model.make_from('aaaaa')
        model.paint(Index(2), MyAnnotation)
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, MatchContent(MyAnnotation)),
            [
                DetAddrWithSubst(Subst.make_from((I, 2)), Index(2)),
            ]
        )

    def test_detaddr_letter_and_annotation(self) -> None:
        model = Model.make_from('abbc')
        model.paint(Index(1), MyAnnotation)
        model.paint(Index(3), MyAnnotation)
        self.assertCountEqual(
            model.addr_to_detaddrs(
                empty_subst,
                I,
                MatchContent(CellBundle.make_from('b', MyAnnotation))
            ),
            [
                DetAddrWithSubst(Subst.make_from((I, 3)), Index(3)),
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
                DetAddrWithSubst(Subst.make_from((I, 1)), Index(1)),
                DetAddrWithSubst(Subst.make_from((I, 2)), Index(2)),
                DetAddrWithSubst(Subst.make_from((I, 3)), Index(3)),
                DetAddrWithSubst(Subst.make_from((I, 4)), Index(4)),
                DetAddrWithSubst(Subst.make_from((I, 5)), Index(5)),
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
                DetAddrWithSubst(Subst.make_from((I, 2)), Index(2)),
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
        set_log_level(8)  #DEBUG
        self.assertCountEqual(
            model.addr_to_detaddrs(Subst.make_from((I, 1)), J, Plus(I, 2)),
            [
                DetAddrWithSubst(Subst.make_from((I, 1), (J, 3)), Index(3)),
            ]
        )

    maxDiff = None
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

    def test_match_simplefunc(self) -> None:
        model = Model.canvas_from('ajaqb')
        model.ws.add(
            Painter.make_from(1, 3, same),
            Painter.make_from(1, 3, MakeBetweenPainter(I, J, F)) # this painter is invalid
        )                                       # for actual use because the
                                                # F can't refer to anything
                                                # but good enough for this test
        self.assertCountEqual(
            model.addr_to_detaddrs(empty_subst, I, Painter(I, J, SimpleFunc(F))),
            [
                DetAddrWithSubst(
                    Subst.make_from((I, 1), (J, 3), (F, same)),
                    Painter.make_from(1, 3, same)
                )
            ]
        )
