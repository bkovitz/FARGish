# support.py -- The support network for a PortGraph
#
# Nodes give support to other nodes.

positive_feedback_rate = 0.2
alpha = 0.9

def support_dict(g, nodes=None):
    if nodes is None:
        nodes = g.nodes
    return dict((node, g.support_for(node)) for node in nodes)

def reverse_sigmoid(x, p=3.0):
    '''Returns reverse sigmoid function of x, where p is the exponent.
    If x is negative, returns reverse sigmoid of -x.'''
    if x < 0:
        x = -x
    return x ** p / (x ** p + (1 - x) ** p)

def normalize(d, max_total_support=10.0):
    '''d is dictionary {node: support for node}. Returns d normalized so
    the sum of all the support does not exceed max_total_support.'''
    result = {}
    total_support = sum(d.values())
    if total_support <= max_total_support:
        return d
    scale_down = 1.0 / total_support  # to scale the initial sum down to 1.0
    for node, support in d.items():
        result[node] = reverse_sigmoid(scale_down * support)
    scale_up = max_total_support / sum(result.values())
    for node in result:
        result[node] *= scale_up
    return result

def propagate_support(g, nodes=None, max_total_support=10.0):
    old_d = support_dict(g)  # old support
    new_d = {}  # new support
    for node, old in old_d.items():
        positive_feedback = 1.0 + positive_feedback_rate * old
        incoming_neighbors = g.neighbors(node, port_label='support_from')
        new_support = old * alpha + sum(
            (1 - alpha) * (positive_feedback * old_d[neighbor])
            # TODO need to multiply the edge weight   hops_from_port
                for neighbor in incoming_neighbors
        )
        new_d[node] = max(g.min_support_for(node), new_support)
    new_d = normalize(new_d, max_total_support=max_total_support)
    for node, new_support in new_d.items():
        g.set_support_for(node, new_support)
