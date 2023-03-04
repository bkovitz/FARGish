# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Canvas, detect_repetition, Seed, Succ, Same, Pred, Repeat

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_str(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(str(canvas), 'abc')

    def test_len(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(canvas.length, 3)

class TestRepetitionDetection(unittest.TestCase):

    def test_abc(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('a', 1), Succ)
        )

    def test_cba(self) -> None:
        canvas = Canvas.make_from('cba')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('c', 1), Pred)
        )

    def test_eee(self) -> None:
        canvas = Canvas.make_from('eee')
        self.assertEqual(
            detect_repetition(canvas),
            Repeat(canvas, Seed('e', 1), Same)
        )

    def test_on_canvas_with_unknown_length(self) -> None:
        canvas = Canvas.make_unknown()
        self.assertIsNone(detect_repetition(canvas))

#    def test_abd(self) -> None:
#        canvas = Canvas.make_from('abd')
#        got = detect_repetition(canvas)
#        assert got is not None
#        seed, op = got
#        self.assertEqual(seed, Seed('a', 1))
#        self.assertEqual(op, Succ)

class TestRepeat(unittest.TestCase):

    def test_repeat_succ(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('a', 1), Succ)
        repeat.fill()
        self.assertEqual(str(canvas), 'abc')

    def test_repeat_succ_seed_2(self) -> None:
        canvas = Canvas.make_unknown(length=3)
        repeat = Repeat(canvas, Seed('j', 2), Succ)
        repeat.fill()
        self.assertEqual(str(canvas), 'ijk')


class TestParseInputString(unittest.TestCase):

    def test_abc_abd_ijk(self) -> None:
        parsed = Canvas.parse_analogy_string('abc->abd; ijk->?');

        expect_snippet1 = Canvas.make_from('abc')
        expect_snippet2 = Canvas.make_from('abd')
        expect_snippet3 = Canvas.make_from('ijk')
        expect_snippet4 = Canvas.make_unknown()

        self.assertEqual(
            parsed,
            [expect_snippet1, expect_snippet2, expect_snippet3, expect_snippet4]
        )

        # These canvases need "addresses": C.1, C.2, C.3, C.4
        # These canvases also need variables: S1, S2, S3, S4
        # There also need to be relations between these canvases: OtherSide
        # and OtherWorld.

