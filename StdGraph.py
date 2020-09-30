# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

from dataclasses import dataclass
from inspect import isclass

from ActiveGraph import ActiveGraph, pg
from NetworkxPortGraph import NetworkxPortGraph, NetworkxActivation
from NodeParams import NodeParams, AttrParam, MateParam
from Primitives import ActivationPolicy
from Propagator import Propagator
from Node import NRef, NRefs
from util import as_iter


@dataclass
class StdActivationPropagator(Propagator):

    def incoming_neighbors(self, g, nodeid):
        return g.incoming_activation_neighbors(nodeid)

    def hop_weight(self, g, fromid, toid):
        return g.activation_from_to(fromid, toid)

    def min_value(self, g, nodeid):
        return g.min_activation(nodeid)

    def set_value(self, g, nodeid, new_value):
        g.set_activation(nodeid, new_value)

class StdActivationPolicy(ActivationPolicy):

    activation_propagator = StdActivationPropagator(
        positive_feedback_rate=1.0,
        alpha=0.98,
        max_total=100.0,
        noise=0.02
    )

    def boost_activation(self, node: NRef, boost_amount: float=0.2):
        self.set_activation(
            node, self.activation(node) + boost_amount
        )

    def propagate_activation(self):
        self.activation_propagator.propagate(self, self.activation_dict())

    def transient_inhibit(self, from_node: NRefs, to_node: NRefs):
        for f in as_iter(from_node):
            fa = self.activation(f)
            delta = min(-0.3 * fa, -0.1)
            for t in as_iter(to_node):
                ta = self.activation(t)
                self.set_activation(t, ta + delta)

    def reset_activation(self, node: NRefs):
        for n in as_iter(node):
            nd = self.as_node(n)
            self.set_activation(n, nd.initial_activation)
        

class Graph(
    StdActivationPolicy, ActiveGraph, NetworkxActivation, NetworkxPortGraph
):
    pass
