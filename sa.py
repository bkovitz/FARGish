# sa.py -- Spreading activation

import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import statistics as stats

from random import random, randint
from collections import defaultdict, namedtuple
import timeit
from math import exp

def sa(d_t0, neighbors, edge_weight, T, decay=1.0, num_steps=10):
    '''Generic spreading activation

    d_t0 = dict of node -> activation at time t0
    neighbors = function(node) -> neighbors spreading activation to node
    edge_weight = function(from_node, to_node) -> edge weight
    T = transfer function
    decay = multiplier for each node's activation before passing to T
    num_steps = number of timesteps

    Returns dict of node -> activation after num_steps of spreading
    activation.'''
    d_t1 = d_t0
    for i in range(num_steps):
        d_t0, d_t1 = d_t1, {}
        for node, activation in d_t0.items():
            d_t1[node] = T(d_t0[node] * decay
                           +
                           sum(d_t0[neighbor] * edge_weight(neighbor, node)
                               for neighbor in neighbors(node)
                           )
                         )
    return d_t1

def activations(g, attr='a'):
    return {node: g.nodes[node].get(attr, 0.0) for node in g.nodes}

def set_activations(g, d, attr='a'):
    for node, activation in d.items():
        g.nodes[node][attr] = activation

def T(x):
    return 2.0 / (1 + exp(-2.2 * x)) - 1.0

def edge_weight_always_one(from_node, to_node):
    return 1.0

def simple_sa(g, attr='a', decay=1.0, num_steps=10):
    def neighbors(node):
        return g.neighbors(node)
    return sa(activations(g, attr=attr), neighbors, edge_weight_always_one, T,
              decay=decay, num_steps=num_steps
           )

def spread_activation(
    g,
    attr='a', via_port_label='sa', num_steps=10, transfer=T, decay=1.0,
    edge_weight=edge_weight_always_one
):
    '''Spreads activation through graph g.'''
    def neighbors(node):
        return g.neighbors(node, port_label=via_port_label)
    old_as = activations(g, attr=attr)
    new_as = sa(
        old_as,
        neighbors,
        edge_weight,
        transfer,
        decay=decay,
        num_steps=num_steps
    )
    set_activations(g, new_as, attr=attr)


if __name__ == '__main__':
    from PortGraph import PortGraph
    g = PortGraph()
    g.add_nodes_from(['A', 'B', 'O'])
    g.add_edge('A', 'sa', 'B', 'sa')
    g.add_edge('O', 'sa', 'A', 'sa')

    g.nodes['A']['a'] = 1.0

    def go():
        set_activations(g, simple_sa(g))
    go()
    print(timeit.timeit(go, number=1000))
    print(activations(g))
