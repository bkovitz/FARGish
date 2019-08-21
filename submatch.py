# submatch.py -- Subgraph matching

from PortGraph import PortGraph, Node, Number, is_node_match, \
    is_port_label_match

from collections import defaultdict
from itertools import product

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
