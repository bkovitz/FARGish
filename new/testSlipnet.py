# test_slipnet.py -- Tests of operations involving the slipnet

import unittest
from pprint import pprint as pp
import inspect

from Slipnet import Slipnet, NodeA
from FMTypes import as_pred, ADict, Exclude, CallablePred
from Equation import Equation, Before, plus, minus, times
from Graph import Graph, Before, After
from FARGModel import FARGModel, ExcludeExisting
from util import is_iter, as_iter, pts, pr

eqn_graph = Graph.with_features(
    Equation.make_table(
        range(1, 11), range(1, 11), [plus, minus, times]
    )
) #.add_edges(MutualInhibition((Feature, Equation, int), weight=-0.02))

class TestSlipnet(unittest.TestCase):

    def test_make_activations_in(self) -> None:
        slipnet = Slipnet.empty()  # slipnet contents are ignored in this test
        self.assertEqual(
            slipnet.make_activations_in(['a']),
            {'a': 1.0}
        )
        a_in: ADict = dict(a=1.5, b=0.5, c=1.2)
        self.assertEqual(
            slipnet.make_activations_in(
                features=['a', 'b', 'd'],
                activations_in=a_in
            ),
            dict(a=1.5, b=1.0, c=1.2, d=1.0)
        )
        self.assertEqual(a_in, dict(a=1.5, b=0.5, c=1.2))

    def test_dquery(self) -> None:
        slipnet = Slipnet(eqn_graph)
        d = slipnet.dquery([Before(4), Before(5)])
        a = d[Equation.make([5, 4], plus)]
        self.assertGreater(a, 0.05)
        self.assertLess(a, 1.0)

    def test_topna_and_top(self) -> None:
        slipnet = Slipnet.empty()  # slipnet contents are ignored in this test
        d: ADict = {'a': 0.9, 'b': 0.2, 'c': 0.3, 'd': 0.4, 'e': 0.5, 'f': 0.6}
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

    def test_filter_by_type(self) -> None:
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

    def test_as_pred_type(self) -> None:
        pred = as_pred(Equation)
        self.assertFalse(pred(After))
        self.assertTrue(pred(Equation.make([5, 4], plus)))
        self.assertFalse(pred(Equation))

    def test_pulse_slipnet(self) -> None:
        fm = FARGModel(seed=1, slipnet=Slipnet(eqn_graph))
        activations_in = {
            Before(4): 1.0,
            After(9): 1.0
        }
        nodes = fm.pulse_slipnet(
            activations_in=activations_in, # type: ignore[arg-type]
            pred=Equation,
            num_get=20
        )
        self.assertTrue(all(isinstance(node, Equation) for node in nodes))
        self.assertTrue(Equation.make([5, 4], plus) in nodes)

    def test_exclude_existing_nodes_from_slipnet_results(self) -> None:
        fm = FARGModel(seed=1, slipnet=Slipnet(eqn_graph))
        eqn1 = fm.build(Equation.make([5, 4], plus))
        eqn2 = Equation.make([6, 4], plus)
        eqn3 = Equation.make([4, 4], plus)
        # eqn1 is a node in the workspace; eqn2 and eqn3 are not.

        activations_in: ADict = {
            Before(4): 1.0,
            After(9): 1.0
        }
        d = fm.slipnet.dquery(activations_in=activations_in)
        self.assertIn(eqn1, Slipnet.top(d=d, k=None))
        self.assertIn(eqn2, Slipnet.top(d=d, k=None))
        self.assertIn(eqn3, Slipnet.top(d=d, k=None))
        self.assertIn(Before(5), Slipnet.top(d=d, k=None))

        excl1 = Exclude(eqn1)
        returned_slipnodes = Slipnet.top(d=d, pred=excl1, k=None)
        self.assertNotIn(eqn1, returned_slipnodes)
        self.assertIn(eqn2, returned_slipnodes)
        self.assertIn(Before(5), returned_slipnodes)
        self.assertIn(5, returned_slipnodes)

        pred2 = Equation
        returned_slipnodes = Slipnet.top(d=d, pred=pred2, k=None)
        self.assertIn(eqn1, returned_slipnodes)
        self.assertIn(eqn2, returned_slipnodes)
        self.assertNotIn(Before(5), returned_slipnodes)
        self.assertNotIn(5, returned_slipnodes)

        pred3 = (Equation, int)
        returned_slipnodes = Slipnet.top(d=d, pred=pred3, k=None)
        self.assertIn(eqn1, returned_slipnodes)
        self.assertIn(eqn2, returned_slipnodes)
        self.assertNotIn(Before(5), returned_slipnodes)
        self.assertIn(5, returned_slipnodes)

        pred4 = (Equation, int, Exclude(eqn1))
        returned_slipnodes = Slipnet.top(d=d, pred=pred4, k=None)
        self.assertNotIn(eqn1, returned_slipnodes)
        self.assertIn(eqn2, returned_slipnodes)
        self.assertNotIn(Before(5), returned_slipnodes)
        self.assertIn(5, returned_slipnodes)

        ee: CallablePred = ExcludeExisting().bind_ws(fm) # type: ignore[assignment]
        pred5 = (Equation, int, ee)
        returned_slipnodes = Slipnet.top(d=d, pred=pred5, k=None)
        self.assertNotIn(eqn1, returned_slipnodes)
        self.assertIn(eqn2, returned_slipnodes)
        self.assertNotIn(Before(5), returned_slipnodes)
        self.assertIn(5, returned_slipnodes)
