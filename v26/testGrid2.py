# testGrid2.py -- Unit tests for Grid2.py

import unittest
from pprint import pprint as pp
import inspect

from Grid2 import Canvas, CanvasData, A, PPainter, P, WeightScheme, \
    default_weight_scheme
    #, QPainter, \
    #QPainterTemplate, RPainter, Subst, unify, all_white
from Log import lo
from util import pts

two_cs: CanvasData = [
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, -1, -1, -1, -1],
    [-1, +1, -1, -1, -1, -1, -1, -1],
    [-1, +1, +1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, +1, -1, -1, -1],
    [-1, -1, -1, -1, +1, +1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
    [-1, -1, -1, -1, -1, -1, -1, -1],
]

class TestCanvas(unittest.TestCase):
    
    def test_get_and_set(self) -> None:
        c = Canvas.from_data(two_cs)
        self.assertEqual(c[1, 8], -1)
        self.assertEqual(c[(1, 8)], -1)
        self.assertEqual(c[A(1, 8)], -1)

        self.assertEqual(c[2, 7], +1)

        c[1, 8] = 2
        self.assertEqual(c[1, 8], 2)
        c[(1, 8)] = 3
        self.assertEqual(c[1, 8], 3)
        c[A(1, 8)] = 4
        self.assertEqual(c[1, 8], 4)

    def test_addr_to_indices(self) -> None:
        c = Canvas.from_data(two_cs)
        self.assertEqual(c.addr_to_indices(3, 7), (1, 2))
        self.assertEqual(c.addr_to_indices((3, 7)), (1, 2))
        self.assertEqual(c.addr_to_indices(A(3, 7)), (1, 2))
        # TODO Get a BadAddr

    def test_paint(self) -> None:
        c = Canvas.empty()

        c.paint((1, 8), +1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 1)
        self.assertEqual(c[1, 7], 0)  # should be unchanged
        self.assertEqual(c.clarity(1, 7), 0)  # should be unchanged
        
        c.paint((1, 8), +1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 2)
        
        c.paint((1, 8), -1)
        self.assertEqual(c[1, 8], +1)
        self.assertEqual(c.clarity(1, 8), 1)

class TestPPainter(unittest.TestCase):
    
    def test_ppainter(self) -> None:
        c = Canvas.empty()
        p = PPainter(1, -1, 1, -1)

        self.assertEqual(p.as_xos(), 'XOXO')
        self.assertCountEqual(p.values(), [1, -1, 1, -1])

        self.assertEqual(
            default_weight_scheme.pp_match_weight(c, p, (1,8)),
            4
        )

        p.paint(c, (1, 8))
        self.assertEqual(c[1, 8], 1)
        self.assertEqual(c[2, 8], -1)
        self.assertEqual(c[1, 7], 1)
        self.assertEqual(c[2, 7], -1)

        self.assertEqual(
            default_weight_scheme.pp_match_weight(c, p, (1,8)),
            20
        )

    def test_match_types(self) -> None:
        c = Canvas.empty()
        # Upper left corner:
        #    O O
        #    X .
        c[1, 8] = -1
        c[2, 8] = -1
        c[1, 7] = 1
        c[2, 7] = 0

        pp = P('OXOX')
        self.assertEqual(
            list(pp.match_types(c, (1, 8))),
            [
                (A(1, 8), 'SameValue'),
                (A(2, 8), 'DifferentValue'),
                (A(1, 7), 'DifferentValue'),
                (A(2, 7), 'Target0')
            ]
        )

    def test_ppainter_from_canvas(self) -> None:
        c = Canvas.from_data(two_cs)
        p = PPainter.from_canvas(c, (1, 8))
        self.assertEqual(p, PPainter(-1, -1, -1, 1))

    def test_ppainter_multi_from_canvas(self) -> None:
        c = Canvas.from_data(two_cs)
        c.blank_all_but([(1, 8), (2, 8), (3, 8), (1, 7), (2, 7)])
        self.assertCountEqual(
            PPainter.multi_from_canvas(c, (1, 8)),
            [P('OOOX')]
        )
        self.assertCountEqual(
            PPainter.multi_from_canvas(c, (2, 8)),
            [P('OOXO'), P('OOXX')]
        )
        self.assertCountEqual(
            PPainter.multi_from_canvas(c, (1, 7)),
            [P('OXOO'), P('OXOX'), P('OXXO'), P('OXXX')]
        )
        self.assertCountEqual(
            PPainter.multi_from_canvas(c, (4, 4)),
            []
        )

class TestWeightScheme(unittest.TestCase):

    def test_ppainter_weight(self) -> None:
        c = Canvas.empty()
        # Upper left corner:
        #    O O
        #    X .
        c[1, 8] = -1;  c.set_clarity((1, 8), 4)
        c[2, 8] = -1;  c.set_clarity((2, 8), 4)
        c[1, 7] = 1;   c.set_clarity((1, 7), 4)
        c[2, 7] = 0;   c.set_clarity((2, 7), 0)

        pp = P('OOXX')
        #print(default_weight_scheme.ppainter_weight(c, pp, (1, 8)))
        self.assertEqual(
            default_weight_scheme.ppainter_weight(c, pp, (1, 8)),
            200.0
        )
