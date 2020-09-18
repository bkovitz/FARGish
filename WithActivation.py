# WithActivation.py -- Mix-in to add activation to PortGraph

from dataclasses import dataclass
from random import gauss

from PortGraph import PortGraph
from support import normalize


#TODO UT
class WithActivation:
    '''Assumes PortGraph as superclass.

    Networkx node attribute 'A' holds activation.

    Edges between port labels 'activation_to' and 'activation_from' define
    flow of activation.'''

    def activation(self, node) -> float:
        try:
            return max(
                self.nodes[node]['A'],
                self.min_activation(node)
            )
        except KeyError:
            return 0.0

    def min_activation(self, node) -> float:
        try:
            return self.datum(node).min_activation
        except AttributeError:
            return 0.0

    def set_activation(self, node, a: float):
        self.nodes[node]['A'] = a

    def set_activation_from_to(self, from_node, to_node, weight=0.2):
        if weight is None or abs(weight) < 0.001:
            self.remove_edge(
                from_node, 'activation_to', to_node, 'activation_from'
            )
        else:
            self.add_edge(
                from_node, 'activation_to', to_node, 'activation_from',
                weight=weight
            )

    def activation_from_to(self, from_node, to_node):
        return self.hop_weight(
            from_node, 'activation_to', to_node, 'activation_from'
        )

    def remove_outgoing_activation_edges(self, node):
        self.remove_hops_from_port(node, 'activation_to')

    def remove_incoming_activation_edges(self, node):
        self.remove_hops_from_port(node, 'activation_from')

    def boost_activation(
        self, node, new_activation=None, multiplier=1.1, min_boost=0.2
    ):
        old_activation = self.activation(node)
        if new_activation is None:
            new_activation = max(
                old_activation + min_boost,
                self.activation(node) * multiplier
            )
        self.set_activation(node, new_activation)

    # TODO rm, since already done by Propagator?
    def decay_activations(self):
        for node in self.nodes:
            self.decay_activation(node)

    # TODO rm, since already done by Propagator?
    def decay_activation(self, node):
        self.set_activation(node, 0.9 * self.activation(node))

def activation_dict(g, nodes=None):
    if nodes is None:
        nodes = g.nodes
    return dict((node, g.activation(node)) for node in nodes)


#TODO UT
@dataclass
class Propagator:
    alpha: float = 0.95
        # continuity constant; decay rate
    sigmoid_p: float = 0.5
        # exponent for reverse_sigmoid
    max_total_activation: float = 10.0
        # total activation points allowed at end of timestep
    noise: float = 0.01
        # sigma parameter for noise added to activation

    def propagate(self, g, nodes=None):
        old_d = activation_dict(g, nodes=nodes)  # old activation
        new_d = {}  # new activation
        for node, old in old_d.items():
            incoming_neighbors = g.neighbors(node, port_label='activation_from')
            new_activation = old * self.alpha + sum(
                (1 - self.alpha)
                * 1.7 * old_d[neighbor]  #HACK (positive feedback)
                * g.activation_from_to(neighbor, node)
                * gauss(1.0, self.noise)
                    for neighbor in incoming_neighbors
            )
            new_d[node] = max(g.min_activation(node), new_activation)
        new_d = normalize(
            new_d,
            max_total=self.max_total_activation,
            p=self.sigmoid_p
        )
        #print('NEWA', new_d)
        for node, new_activation in new_d.items():
            g.set_activation(node, new_activation)
