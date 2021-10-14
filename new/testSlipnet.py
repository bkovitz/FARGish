# test_slipnet.py -- Tests of operations involving the slipnet

import unittest
from pprint import pprint as pp
import inspect

from Slipnet import Slipnet, NodeA
from Equation import Equation, Before, plus, minus, times
from Graph import Graph
from util import is_iter, as_iter, pts, pr

eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Equation, int), weight=-0.02))

class TestSlipnet(unittest.TestCase):

    def test_make_activations_in(self):
        slipnet = Slipnet.empty()  # slipnet contents are ignored in this test
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

    def test_dquery(self):
        slipnet = Slipnet(eqn_graph)
        d = slipnet.dquery([Before(4), Before(5)])
        a = d[Equation.make([5, 4], plus)]
        self.assertGreater(a, 0.05)
        self.assertLess(a, 1.0)

    def test_topna_and_top(self):
        slipnet = Slipnet.empty()  # slipnet contents are ignored in this test
        d = {'a': 0.9, 'b': 0.2, 'c': 0.3, 'd': 0.4, 'e': 0.5, 'f': 0.6}
        pred = lambda x: x > 'b'
        self.assertEqual(
            slipnet.topna(d, pred=pred, k=None),
            [
                NodeA('f', 0.6),
                NodeA('e', 0.5),
                NodeA('d', 0.4),
                NodeA('c', 0.3)
            ]
        )
        self.assertEqual(
            slipnet.top(d, pred=pred, k=None),
            ['f', 'e', 'd', 'c']
        )
        self.assertEqual(
            slipnet.top1(d, pred=pred, k=None),
            'f'
        )
        self.assertEqual(
            slipnet.topna(d, pred=pred, k=2),
            [NodeA('f', 0.6), NodeA('e', 0.5)]
        )
        self.assertEqual(
            slipnet.topna(d, pred=pred), # default k: pick only the top 1
            [NodeA('f', 0.6)]
        )
        self.assertEqual(
            slipnet.topna(d, k=2),
            [NodeA('a', 0.9), NodeA('f', 0.6)]
        )

    def test_filter_by_type(self):
        slipnet = Slipnet.empty()  # slipnet contents are ignored in this test
        d = {'a': 0.2, 'b': 0.3, 1: 0.2, 2: 0.3}
        self.assertEqual(
            slipnet.top(d, pred=str),
            ['b']
        )
        self.assertEqual(
            slipnet.top(d, pred=int),
            [2]
        )

        
