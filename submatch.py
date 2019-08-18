# submatch.py -- Subgraph matching

from PortGraph import PortGraph

import networkx as nx

from collections import defaultdict
import numbers

'''
TODO
is_edge_match
'''


class Node:

    @classmethod
    def is_attrs_match(cls, node_attrs, host_node_attrs):
        return True


class Number(Node):

    @classmethod
    def is_attrs_match(cls, node_attrs, host_node_attrs):
        try:
            return (
                node_attrs['value'] == host_node_attrs['value']
            )
        except KeyError:
            return False


def is_node_match(tg, target_node, hg, host_node):
    try:
        target_attrs = tg.nodes[target_node]
        host_attrs = hg.nodes[host_node]
        target_class = target_attrs['_class']
        host_class = host_attrs['_class']
        return (
            issubclass(host_class, target_class)
            and
            target_class.is_attrs_match(target_attrs, host_attrs)
        )
    except KeyError:
        return False

def make_graph(*nodes):
    g = PortGraph()
    for i, node in enumerate(nodes):
        if isinstance(node, numbers.Number):
            d = {'_class': Number}
        else:
            d = {'_class': Node}
        d['value'] = node
        g.add_node(i, **d)
    return g

tg = make_graph(2, 2)  # "target" graph
hg = make_graph(2, 2, '+')  # "host" graph



print(tg.nodes(data=True))
print(hg.nodes(data=True))
print(is_node_match(tg, 0, hg, 0))  # True
print(is_node_match(tg, 0, hg, 2))  # False
