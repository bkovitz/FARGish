# testPropagator.py -- Unit tests for Propagator

import unittest
from pprint import pprint as pp
import inspect
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from itertools import chain

from Propagator import Propagator, Delta
from Node import Node, NodeId
from NodeParams import NodeParams, AttrParam
from testNetworkxPortGraph import GraphWithNetworkxActivation


class MyPropagator(Propagator):

    def incoming_neighbors(self, g, nodeid):
        return g.incoming_activation_neighbors(nodeid)

    def hop_weight(self, g, fromid, toid):
        return g.activation_from_to(fromid, toid)

    def min_value(self, g, nodeid):
        return g.min_activation(nodeid)

    def make_deltas(self, g, old_d: Dict[NodeId, float]) -> Iterable[Delta]:
        return chain.from_iterable(
            self.deltas_to(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_to(self, g, old_d: Dict[NodeId, float], nodeid: NodeId) \
    -> List[Delta]:
        incoming_deltas = []
        for neighborid in g.incoming_activation_neighbors(nodeid):
            support_for_neighbor = old_d.get(neighborid, 0.0)
            incoming_deltas.append(Delta(
                nodeid,
                self.hop_weight(g, neighborid, nodeid) * support_for_neighbor,
                neighborid
            ))
        return incoming_deltas

class MyNode(Node):
    node_params = NodeParams(AttrParam('name'))
    initial_activation = 0.0

class TestPropagator(unittest.TestCase):

    def test_propagator(self):
        g = GraphWithNetworkxActivation()
        p = MyPropagator(
            positive_feedback_rate=0.2,
            alpha=0.9,
            max_total=1.0,
            noise=0.0
        )
        a = g.add_node(MyNode('A'))
        b = g.add_node(MyNode('B'))
        o = g.add_node(MyNode('O'))
        g.set_activation(a, 1.0)
        g.set_activation(b, 0.5)
        g.set_activation_from_to(a, b, 1.0)
        g.set_activation_from_to(b, o, 1.0)

        d = p.propagate(g, g.activation_dict())

        self.assertAlmostEqual(d[a.id], 0.569072143062159)
        self.assertAlmostEqual(d[b.id], 0.319848331226608)
        self.assertAlmostEqual(d[o.id], 0.11107952571123292)
