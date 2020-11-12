# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

import csv
from dataclasses import dataclass
from inspect import isclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable

from ActiveGraph import ActiveGraph, pg
from NetworkxPortGraph import NetworkxPortGraph, NetworkxActivation
from NodeParams import NodeParams, AttrParam, MateParam
from Primitives import ActivationPrimitives, ActivationPolicy, \
    SupportPrimitives, SupportPolicy, SlipnetPolicy
from Propagator import Propagator
from Node import NRef, MaybeNRef, NRefs, NodeId
from util import as_iter


@dataclass
class StdActivationPropagator(Propagator):

    def incoming_neighbors(self, g, nodeid):
        return g.incoming_activation_neighbors(nodeid)

    def hop_weight(self, g, fromid, toid):
        return g.activation_from_to(fromid, toid)

    def min_value(self, g, nodeid):
        return g.min_activation(nodeid)

class StdActivationPolicy(ActivationPolicy):

    activation_propagator = StdActivationPropagator(
        positive_feedback_rate=1.0,
        alpha=0.98,
        max_total=100.0,
        noise=0.02,
        sigmoid_p=0.98,
        num_iterations=3,
    )

    def boost_activation(self, node: NRef, boost_amount: float=0.2):
        self.set_activation(
            node, self.activation(node) + boost_amount
        )

    def propagate_activation(self) -> Dict[NodeId, float]:
        d = self.activation_propagator.propagate(self, self.activation_dict())
        for node, new_value in d.items():
            self.set_activation(node, new_value)
        return d

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

    # TODO Make into a Primitive
    def log_activation(self):
        '''Log file format:  timestep, node, activation'''
        t = self.t
        mode = 'a'
        if t <= 1:
            mode = 'w'
        with open('activation.csv', mode=mode, newline='') as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
            for node, a in self.activation_dict().items():
                writer.writerow([t, node, a])

class StdSupportPolicy(SupportPolicy):

    def support_for(self, nref: MaybeNRef) -> float:
        # TODO Set up inheritance to ensure .getattr, etc.
        result = self.getattr(nref, 'support_for')
        if result is None:
            return 0.0
        else:
            return result

    def set_support_for(self, node: MaybeNRef, supp: float):
        self.set_attr(node, 'support_for', supp)

    def support_from_to(
        self, from_node: MaybeNRef, to_node: MaybeNRef
    ) -> float:
        return self.edge_weight(
            from_node, 'support_to', to_node, 'support_from'
        )

    def set_support_from_to(
        self, from_node: MaybeNRef, to_node: MaybeNRef, weight: float
    ):
        self.add_edge(
            from_node, 'support_to', to_node, 'support_from', weight=weight
        )

    def support_hops_from(self, from_node: MaybeNRef):
        return self.hops_from_port(from_node, 'support_to')

    def support_dict(
        self, nodes=Union[Iterable[NRef], None]
    ) -> Dict[NodeId, float]:
        if nodes is None:
            nodes = self._nodeids()  # TODO self.nodeids() ?
        # INEFFICIENT Calling as_nodeid() unnecessarily if nodes==None
        return dict(
            (nodeid, self.support_for(nodeid))
                for nodeid in map(as_nodeid, nodes)
        )

    def propagate_support(self):
        #TODO
        pass

class StdSlipnetPolicy(SlipnetPolicy, ActivationPrimitives):
# TODO Inherit from something that guarantees .members_recursive().
    
    slipnet_propagator = StdActivationPropagator(
        positive_feedback_rate=1.0,
        alpha=0.98,
        max_total=100.0,
        noise=0.02
    )

    def slipnet_search(self, nodes: NRefs, slipnodes: Set[NodeId]) \
    -> Set[NodeId]:
        '''Returns set of activated slipnodes. Leaves dict of slipnet
        activations in self.slipnet_d.'''
        start_d = self.activation_dict(nodes)
        self.slipnet_d = self.slipnet_propagator.propagate(self, start_d)
        slipnodes = self.members_recursive(self.slipnet)
        return set(
            nodeid
                for nodeid, a in self.slipnet_d.items()
                    if a >= 1.0 and nodeid in slipnodes
        )

class Graph(
    StdSupportPolicy, StdSlipnetPolicy, StdActivationPolicy, ActiveGraph,
    NetworkxActivation, NetworkxPortGraph
):
    pass
