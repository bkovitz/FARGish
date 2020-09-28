# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

from dataclasses import dataclass
from inspect import isclass

from ActiveGraph import ActiveGraph, pg
from NetworkxPortGraph import NetworkxPortGraph, NetworkxActivation
from NodeParams import NodeParams, AttrParam, MateParam
from Primitives import ActivationPolicy
from Propagator import Propagator
from Node import NRef


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
        

class Graph(
    StdActivationPolicy, ActiveGraph, NetworkxActivation, NetworkxPortGraph
):
    pass
