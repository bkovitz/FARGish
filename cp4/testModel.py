# testModel.py -- Unit tests for Model.py

import unittest
import inspect

from Model import Canvas, detect_repetition, Seed, Succ, Same, Pred

from Log import lo, set_log_level
from util import pts, reseed, short


class TestCanvas(unittest.TestCase):

    def test_str(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(str(canvas), 'abc')

    def test_len(self) -> None:
        canvas = Canvas.make_from('abc')
        self.assertEqual(len(canvas), 3)

class TestRepetitionDetection(unittest.TestCase):

    def test_abc(self) -> None:
        canvas = Canvas.make_from('abc')
        got = detect_repetition(canvas)
        assert got is not None
        seed, op = got
        self.assertEqual(seed, Seed('a', 1))
        self.assertEqual(op, Succ)

    def test_cba(self) -> None:
        canvas = Canvas.make_from('cba')
        got = detect_repetition(canvas)
        assert got is not None
        seed, op = got
        self.assertEqual(seed, Seed('c', 1))
        self.assertEqual(op, Pred)

    def test_eee(self) -> None:
        canvas = Canvas.make_from('eee')
        got = detect_repetition(canvas)
        assert got is not None
        seed, op = got
        self.assertEqual(seed, Seed('e', 1))
        self.assertEqual(op, Same)

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

