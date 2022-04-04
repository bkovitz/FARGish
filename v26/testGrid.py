# testGrid.py -- Unit tests for Grid.py

import unittest
from pprint import pprint as pp
import inspect

from Grid import Canvas, CanvasData, A, PPainter, P, QPainter, \
    QPainterTemplate, RPainter, Subst, unify, all_white
from Log import lo


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

        self.assertEqual(p.match_wt(c, (1, 8)), 4)

        p.paint(c, (1, 8))
        self.assertEqual(c[1, 8], 1)
        self.assertEqual(c[2, 8], -1)
        self.assertEqual(c[1, 7], 1)
        self.assertEqual(c[2, 7], -1)

        self.assertEqual(p.match_wt(c, (1, 8)), 20)

    def test_ppainter_from_canvas(self) -> None:
        c = Canvas.from_data(two_cs)
        p = PPainter.from_canvas(c, (1, 8))
        self.assertEqual(p, PPainter(-1, -1, -1, 1))

    maxDiff = None

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

class TestQPainter(unittest.TestCase):

    def test_qpainter(self) -> None:
        c = Canvas.empty()
        p = QPainter((1, 8), PPainter(1, 1, -1, -1))
        p.paint(c)
        self.assertEqual(c[1, 8], 1)
        self.assertEqual(c[2, 8], 1)
        self.assertEqual(c[1, 7], -1)
        self.assertEqual(c[2, 7], -1)

class TestUnify(unittest.TestCase):

    def test_unify(self) -> None:
        self.assertEqual(
            unify(['x', ('x', '+', 1)], (1, 2)),
            Subst('x', 1)
        )
        self.assertEqual(
            unify(['x', ('x', '+', 1)], (3, 4)),
            Subst('x', 3)
        )
        self.assertEqual(
            unify([('x', '+', 1), 'x'], (1, 2)),
            None
        )
        self.assertEqual(
            unify([('x', '+', 1), ('x', '+', 2)], (1, 2)),
            Subst('x', 0)
        )
        self.assertEqual(
            unify(['x', ('x', '-', 2)], (5, 3)),
            Subst('x', 5)
        )

class TestQPainterTemplate(unittest.TestCase):

    def test_make_qpainter(self) -> None:
        p1 = PPainter(-1, -1, -1, 1)
        p2 = PPainter(-1, -1, 1, 1)
        qp1 = QPainter((1, 8), p1)
        qp2 = QPainter((2, 8), p2)
        qpt1 = QPainterTemplate(
            ('x', 'y'), p1
        )
        qpt2 = QPainterTemplate(
            (('x', '+', 1), 'y'), p2
        )
        got = qpt2.make_qpainter(qpt1.make_env(qp1))  # type: ignore[arg-type]
        self.assertEqual(got, qp2)

class TestRPainter(unittest.TestCase):

    def test_make_qpainters(self) -> None:
        p1 = PPainter(-1, -1, -1, 1)
        p2 = PPainter(-1, -1, 1, 1)
        qp1 = QPainter((1, 8), p1)
        qp2 = QPainter((2, 8), p2)
        qpt1 = QPainterTemplate(
            ('x', 'y'), p1
        )
        qpt2 = QPainterTemplate(
            (('x', '+', 1), 'y'), p2
        )
        rp = RPainter((qpt1, qpt2))

        self.assertCountEqual(
            rp.make_qpainters(QPainter((1, 8), p1)),
            [QPainter((2, 8), p2)]
        )
        self.assertCountEqual(
            rp.make_qpainters(QPainter((2, 8), p1)),
            [QPainter((3, 8), p2)]
        )
        self.assertCountEqual(
            rp.make_qpainters(QPainter((7, 8), p1)),
            []
        )
        self.assertCountEqual(
            rp.make_qpainters(QPainter((8, 8), p1)),
            []
        )

        self.assertCountEqual(
            rp.make_qpainters(QPainter((1, 2), p1)),
            [QPainter((2, 2), p2)]
        )
        self.assertCountEqual(
            rp.make_qpainters(QPainter((1, 1), p1)),
            []
        )

        self.assertCountEqual(
            rp.make_qpainters(QPainter((2, 8), p2)),
            [QPainter((1, 8), p1)]
        )
        self.assertCountEqual(
            rp.make_qpainters(QPainter((1, 8), p2)),
            []
        )

    def test_derive_from_qpainters(self) -> None:
        c = Canvas.from_data(all_white)
        qps = set(QPainter.derive_from_canvas(c))
        assert len(qps) == 49  # 8x8 without last row and column
        rps = RPainter.derive_from_qpainters(qps)
        self.assertEqual(len(rps), 8)  # same PPainter in all 8 directions

    def test_match_qpainter(self) -> None:
        p1 = PPainter(-1, -1, -1, +1)
        p2 = PPainter(-1, -1, +1, +1)
        p3 = PPainter(-1, -1, +1, -1)
        qp1 = QPainter((1, 8), p1)
        qp2 = QPainter((2, 8), p2)
        qp3 = QPainter((1, 8), p3)
        qpt1 = QPainterTemplate(
            ('x', 'y'), p1
        )
        qpt2 = QPainterTemplate(
            (('x', '+', 1), 'y'), p2
        )
        rp = RPainter((qpt1, qpt2))
        self.assertTrue(rp.is_match(qp1))
        self.assertTrue(rp.is_match(qp2))
        self.assertFalse(rp.is_match(qp3))
