# support.py -- The support network for a PortGraph
#
# Nodes give support to other nodes.

import csv
from random import gauss

from util import nice_object_repr


def support_dict(g, nodes=None):
    if nodes is None:
        nodes = g.nodes
    return dict((node, g.support_for(node)) for node in nodes)

def reverse_sigmoid(x, p=0.5):
    '''Returns reverse sigmoid function of x, where p is the exponent.
    If x is negative, returns reverse sigmoid of -x.'''
    if x < 0:
        x = -x
    return x ** p / (x ** p + (1 - x) ** p)

def normalize(d, max_total=10.0, p=0.5):
    '''d is dictionary {node: support for node}. Returns d normalized so
    the sum of all the support does not exceed max_total.'''
    result = {}
    total = sum(d.values())
    #print('NORMALIZE', total)
    if total <= max_total:
        return d
    #scale_down = 1.0 / total  # to scale the initial sum down to 1.0
    #scale_down = max_total / total
    scale_down = 1.0 / max(d.values())
    for node, support in d.items():
        result[node] = reverse_sigmoid(scale_down * support, p=p)
    scale_up = max_total / sum(result.values())
    #print()
    #ddre = dict((n, s * scale_down) for n, s in d.items())
    #print('D-RE', ddre, sum(ddre.values()))
    #print('RESULT0', result)
    #print('SCALE', total, scale_down, scale_up, max_total, sum(result.values()))
    for node in result:
        result[node] *= scale_up
    return result


class Propagator:

    def __init__(
        self,
        positive_feedback_rate=0.2,
            # constant for favoring already-supported nodes
        alpha = 0.95,
            # continuity constant; decay rate
        sigmoid_p = 0.5,
            # exponent for reverse_sigmoid
        max_total_support = 10.0,
            # total support points allowed at end of timestep
        noise = 0.01,
            # sigma parameter for noise added to support
    ):
        self.positive_feedback_rate = positive_feedback_rate
        self.alpha = alpha
        self.sigmoid_p = sigmoid_p
        self.max_total_support = max_total_support
        self.noise = noise

    def propagate(self, g, nodes=None):
        old_d = support_dict(g, nodes=nodes)  # old support
        new_d = {}  # new support
        for node, old in old_d.items():
            positive_feedback = 1.0 + self.positive_feedback_rate * old
            incoming_neighbors = g.neighbors(node, port_label='support_from')
            new_support = old * self.alpha + sum(
                (1 - self.alpha) * (positive_feedback * old_d[neighbor])
                * g.hop_weight(neighbor, 'support_to', node, 'support_from')
                * gauss(1.0, self.noise)
                    for neighbor in incoming_neighbors
            )
            new_d[node] = max(g.min_support_for(node), new_support)
        new_d = normalize(
            new_d,
            max_total=self.max_total_support,
            p=self.sigmoid_p
        )
        for node, new_support in new_d.items():
            g.set_support_for(node, new_support)

    __repr__ = nice_object_repr


def log_support(g):
    '''Log file format:  timestep, node, support_for'''
    t = g.graph['t']
    mode = 'a'
    if t <= 1:
        mode = 'w'
    with open('support.csv', mode=mode, newline='') as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_NONNUMERIC)
        for node, supp in support_dict(g).items():
            writer.writerow([t, node, supp])
