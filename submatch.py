# submatch.py -- Subgraph matching

from PortGraph import PortGraph, Node, is_node_match, \
    is_port_label_match

from collections import defaultdict
from itertools import product

def dict_product(d):
    """
    >>> list(dict_product(dict(number=[1,2], character='ab')))
    [{'character': 'a', 'number': 1},
     {'character': 'a', 'number': 2},
     {'character': 'b', 'number': 1},
     {'character': 'b', 'number': 2}]
    """
    return (dict(zip(d.keys(), vs)) for vs in product(*d.values()))

def dict_product_no_multiple_mappings_to_same_value(d):
    '''Like dict_product but filters out dictionaries where a single value
    has more than one key that maps to it. For efficiency, assumes that d
    already has this property.'''
    num_keys = len(d)
    return (e for e in dict_product(d)
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
    'Returns seq of bindings' #TODO Explain more
    def edge_matches(d, edge):
        tg_node1, tg_node2, tg_edge_key = edge
        tg_hop = tg.edge_to_hop(edge)
        try:
            return hg.has_hop(
                       d[tg_node1],
                       tg_hop.from_port_label,
                       d[tg_node2],
                       tg_hop.to_port_label
                   )
        except KeyError:
            return False
    return (nodes_d for nodes_d in 
                dict_product_no_multiple_mappings_to_same_value(
                    subgraph_match_by_nodes_only(tg, hg)
                ) if all(edge_matches(nodes_d, e) for e in tg.edges))

def bdxs_for_datums(datums, hg, nodes=None):
    '''hg: Host graph
    datums = iterable of Datum objects to compare with

    Returns iter of dicts of bindings from datums to nodes in bg. Edges in
    hg are ignored. No dict will bind the same datum to the two different
    nodes. Some datums might not have a mate.'''
    if nodes is None:
        nodes = hg.nodes
    d = dict((datum, set(hg.nodes_matching_datum(datum, nodes=nodes)))
                for datum in datums)
    #TODO Allow a datum to map to nothing without shutting down the other
    #mappings. Maybe just omit its key from d.
    return dict_product_no_multiple_mappings_to_same_value(d)

