# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

import csv
from dataclasses import dataclass
from inspect import isclass
from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Callable
from itertools import chain

from ActiveGraph import ActiveGraph, Context, MyContext, InWorkspace, pg
from NetworkxPortGraph import NetworkxPortGraph, NetworkxActivation
from NodeParams import NodeParams, AttrParam, MateParam
from Primitives import ActivationPrimitives, ActivationPolicy, \
    SupportPrimitives, SupportPolicy, SlipnetPolicy
from Propagator import Propagator, Delta
from Node import NRef, MaybeNRef, NRefs, NodeId
from log import *
from util import as_iter


@dataclass
class StdActivationPropagator(Propagator):

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
                g.activation_from_to(neighborid, nodeid) *
                    support_for_neighbor
            ))
        return incoming_deltas

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
        if ShowPrimitives.is_logging():
            print('boost_activation', self.nodestr(node), boost_amount)
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

    def log_activation(self):
        '''Log-file format:  timestep, node, activation'''
        t = self.t
        mode = 'a'
        if t <= 1:
            mode = 'w'
        with open('activation.csv', mode=mode, newline='') as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
            for node, a in self.activation_dict().items():
                writer.writerow([t, node, a])

@dataclass
class StdSupportPropagator(Propagator):

    def min_value(self, g, nodeid):
        return g.min_support_for(nodeid)

    def make_deltas(self, g, old_d: Dict[NodeId, float]) -> Iterable[Delta]:
        return chain.from_iterable(
            self.deltas_from(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_from(self, g, old_d: Dict[NodeId, float], nodeid: NodeId) \
    -> Iterable[Delta]:
        support_for_node = old_d.get(nodeid, 0.0)
        outgoing_deltas = []
        for hop in g.support_hops_from(nodeid):
            neighborid = hop.to_node
            support_for_neighbor = old_d.get(neighborid, 0.0)
            hop_weight = g._hop_weight(hop)
            outgoing_deltas.append(Delta(
                neighborid,
                hop_weight + 
                    self.positive_feedback_rate * support_for_neighbor
            ))
        return self.rescale_deltas(outgoing_deltas, support_for_node)

    def rescale_deltas(
        self, outgoing_deltas: List[Delta], support_for_node: float
    ) -> List[Delta]:
        if not outgoing_deltas:
            return outgoing_deltas
        ssum = sum(delta.amt for delta in outgoing_deltas)
        if ssum <= support_for_node:
            return outgoing_deltas
        else:
            multiplier = support_for_node / ssum
            return [
                Delta(d.nodeid, d.amt * multiplier)
                    for d in outgoing_deltas
            ]


class StdSupportPolicy(SupportPolicy):

    support_propagator = StdSupportPropagator(
        positive_feedback_rate=0.2,
        alpha=0.98,
        max_total=100.0,
        noise=0.02,
        sigmoid_p=0.98,
        num_iterations=3,
    )

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
        self, nodes: Union[Iterable[NRef], None]=None
    ) -> Dict[NodeId, float]:
        if nodes is None:
            nodes = self._nodeids()  # TODO self.nodeids() ?
        # INEFFICIENT Calling as_nodeid() unnecessarily if nodes==None
        return dict(
            (nodeid, self.support_for(nodeid))
                for nodeid in map(self.as_nodeid, nodes)
        )

    def propagate_support(self) -> Dict[NodeId, float]:
        d = self.support_propagator.propagate(self, self.support_dict())
        for node, new_value in d.items():
            self.set_support_for(node, new_value)
        return d

    def log_support(self):
        '''Log-file format: timestep, node, support_for'''
        t = self.t
        mode = 'a'
        if t <= 1:
            mode = 'w'
        with open('support.csv', mode=mode, newline='') as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
            for node, s in self.support_dict().items():
                writer.writerow([t, node, s])


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
