# testPropagator.py -- Unit tests for Propagator

import unittest
from pprint import pprint as pp
import inspect
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from itertools import chain

from FMTypes import ADict
from Propagator import Propagator, SentA, ActivationLog
from Graph import Graph, Node, EnumNodes, EnumEdges, Hops
from util import pr, pts


class MyPropagator(Propagator):

    def make_sentas(self, g: Graph, old_d: Dict[Node, float]) \
    -> Iterable[SentA]:
        for node in old_d:
            for hop in g.hops_to_node(node):
                neighbor_a = old_d.get(hop.from_node, 0.0)
                yield SentA(node, hop.weight * neighbor_a, hop.from_node)

class TestPropagator(unittest.TestCase):

    def test_propagator(self):
        g = Graph(
            nodes=EnumNodes(['A', 'B', 'O']),
            edges=EnumEdges(Hops.from_pairs(
                ('A', 'B'), ('B', 'O')
            ))
        )
        p = MyPropagator(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total=1.0,
            noise=0.0
        )
        activations_in = dict(A=1.0, B=0.5, O=0.0)

        d = p.propagate(g, activations_in)

        self.assertAlmostEqual(d['A'], 0.569072143062159)
        self.assertAlmostEqual(d['B'], 0.319848331226608)
        self.assertAlmostEqual(d['O'], 0.11107952571123292)

    def test_activation_log(self) -> None:
        activations_in: ADict = dict(A=1.0, B=0.5, O=0.0)
        alog = ActivationLog(activations_in)
        # Just a smoke test for now
        # pr(alog)
        # TODO alog.num_nodes() should == 3
