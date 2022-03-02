# testSoup.py -- unit tests for Soup.py

import unittest
from pprint import pprint as pp
import inspect

from Soup import make_wtp


class TestSoup(unittest.TestCase):

    def test_make_wtp_simple(self) -> None:
        cases = [
            ('a', 'a'),   # pattern, seg
            ('a', 'aj'),
            ('aj', 'a'),
            ('aj', 'j'),
            ('ja', 'a'),
            ('jjaa', 'a')
        ]
        expect = [
            'a',
            'aj',
            'aj',
            'aj',
            'ja',
            'a'
        ]
        for c, e in zip(cases, expect):
            got = ''.join(make_wtp(*c).result())
            self.assertEqual(got, e, f'passed {c}, expected {e!r}, got {got!r}')
