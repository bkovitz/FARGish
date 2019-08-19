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

def is_port_label_match(target_port_label, host_port_label):
    if (
        isinstance(target_port_label, str)
        or
        isinstance(target_port_label, numbers.Number)
    ):
        return target_port_label == host_port_label
    else:
        return issubclass(host_port_label, target_port_label)

def cart_ds(d):
    """
    >>> list(cart_ds(dict(number=[1,2], character='ab')))
    [{'character': 'a', 'number': 1},
     {'character': 'a', 'number': 2},
     {'character': 'b', 'number': 1},
     {'character': 'b', 'number': 2}]
    """
    return (dict(zip(d.keys(), vs)) for vs in product(*d.values()))

#TODO Better name for this
# It filters out duplicate mappings to the same value
def c(d):
    num_keys = len(d)
    return (e for e in cart_ds(d)
                  if len(set(e.values())) == num_keys)

def subgraph_match_by_nodes_only(tg, hg):
    nodes_d = {}
    for tnode in tg.nodes:
        matching_host_nodes = [
            hnode for hnode in hg.nodes
                      if is_node_match(tg, tnode, hg, hnode)
        ]
        if matching_host_nodes:
            nodes_d[tnode] = matching_host_nodes
        else:
            return {}
    return nodes_d

def matching_subgraphs(tg, hg):
    nodes_d = subgraph_match_by_nodes_only(tg, hg)

    def edge_matches(d, edge):
        tg_node1, tg_node2, tg_edge_key = edge
        tg_port_label1, tg_port_label2 = tg.edge_port_labels(tg_node1, tg_node2)
        return hg.has_edge(
                   d[tg_node1], d[tg_node2], tg_port_label1, tg_port_label2
               )
    return (d for d in c(nodes_d)
                  if all(edge_matches(d, e) for e in tg.edges))

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
print(subgraph_match(tg, hg))
