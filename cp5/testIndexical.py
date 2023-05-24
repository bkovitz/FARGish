# testIndexical.py -- Unit tests for "indexical" features

import unittest

b = Letter('b')

class TestIndexical(unittest.TestCase):

    def test_see_predominantly(self) -> None:
        got = m.enchunk('bbb')
        self.assertIn(Span(Predominantly(b), 1, 3), got)
