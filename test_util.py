# test_util.py -- Unit tests for util.py

import unittest
from pprint import pprint as pp
import inspect

from types import SimpleNamespace

from util import PushAttr


class TestUtil(unittest.TestCase):

    def test_with_pushattr(self):
        o = SimpleNamespace()
        o.myattr = 'FIRST'

        with PushAttr(o, 'myattr'):
            o.myattr = 'SECOND'
            self.assertEqual(o.myattr, 'SECOND')
        self.assertEqual(o.myattr, 'FIRST')
