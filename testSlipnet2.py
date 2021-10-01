# test_slipnet2.py -- Tests of operations involving the slipnet

import unittest
from pprint import pprint as pp
import inspect

from Slipnet2 import Slipnet

class TestSlipnet(unittest.TestCase):

    def test_make_activations_in(self):
        slipnet = Slipnet.empty()
        self.assertEqual(
            slipnet.make_activations_in(['a']),
            {'a': 1.0}
        )
        a_in = dict(a=1.5, b=0.5, c=1.2)
        self.assertEqual(
            slipnet.make_activations_in(
                features=['a', 'b', 'd'],
                activations_in=a_in
            ),
            dict(a=1.5, b=1.0, c=1.2, d=1.0)
        )
        self.assertEqual(a_in, dict(a=1.5, b=0.5, c=1.2))
