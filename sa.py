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

def activations(g):
    d_t0 = {}
    for node in g.nodes:
        d_t0[node] = g.nodes[node].get('a', 0.0)
    return d_t0

def set_activations(g, d):
    for node, activation in d.items():
        g.nodes[node]['a'] = activation

def simple_sa(g, decay=1.0, num_steps=10):
    def neighbors(node):
        return g.neighbors(node)
    def edge_weight(from_node, to_node):
        return 1.0
    def T(x):
        return 2.0 / (1 + exp(-2.2 * x)) - 1.0
    return sa(activations(g), neighbors, edge_weight, T,
              decay=decay, num_steps=num_steps
           )


if __name__ == '__main__':
    from PortGraph import PortGraph
    g = PortGraph()
    g.add_edge('A', 'sa', 'B', 'sa')
    g.add_edge('O', 'sa', 'A', 'sa')

    g.nodes['A']['a'] = 1.0
    set_activations(g, simple_sa(g))
    print(activations(g))
