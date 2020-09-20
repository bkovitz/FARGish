# NetworkxPortGraph.py -- Implementation of PortGraphPrimitives using the
#                         networkx library to hold the graph

from typing import Union, List, Set, Iterable, Any, NewType, Type, FrozenSet, \
    Dict
from dataclasses import dataclass
from collections import UserDict

import networkx as nx

from ActiveGraph import PortGraphPrimitives, Node, NodeId, NRef, PortLabel, \
    Hop, Hops
from util import empty_set


@dataclass
class HopDict:
    d_from_port_label: Dict[PortLabel, Set[Hop]] = None
    d_to_node: Dict[PortLabel, Set[Hop]] = None

    def __post_init__(self):
        if self.d_from_port_label is None:
            self.d_from_port_label = {}
        if self.d_to_node is None:
            self.d_to_node = {}

    def all_hops(self) -> Set[Hop]:
        result = set()
        for hset in self.d_from_port_label.values():
            result = result.union(hset)
        return result

    def from_port_labels(self) -> Iterable[PortLabel]:
        return self.d_from_port_label.keys()

    def add(self, hop: Hop):
        try:
            self.d_from_port_label[hop.from_port_label].add(hop)
        except KeyError:
            self.d_from_port_label[hop.from_port_label] = set([hop])
        try:
            self.d_to_node[hop.to_node].add(hop)
        except KeyError:
            self.d_to_node[hop.to_node] = set([hop])

    def remove(self, hop: Hop):
        '''It is not an error to remove a hop that doesn't exist.'''
        try:
            self.d_from_port_label[hop.from_port_label].discard(hop)
        except KeyError:
            pass
        try:
            self.d_to_node[hop.to_node].discard(hop)
        except KeyError:
            pass

    def remove_all_hops_to(self, to_node: NodeId):
        '''It is not an error if there are no hops to to_node.'''
        for hop in list(self.hops_to_neighbor(to_node)):
            self.remove(hop)

    def hops_from_port_label(self, from_port_label: PortLabel) -> Set[Hop]:
        try:
            return self.d_from_port_label[from_port_label]
        except KeyError:
            return empty_set

    def hops_to_port_label(self, to_port_label: PortLabel) -> Iterable[Hop]:
        return (hop for hop in self.all_hops()
                       if hop.to_port_label == to_port_label)
        
    def hops_to_neighbor(self, neighbor_node: NodeId) -> Set[Hop]:
        try:
            return self.d_to_node[neighbor_node]
        except KeyError:
            return empty_set

class NodeAttrDict(UserDict):
    '''Custom dict that maps nodes in a Graph to their attributes. Every
    node automatically gets an attribute named '_hops' whose value is a
    defaultdict(set).'''

    def __setitem__(self, node: NodeId, node_attrs: Dict):
        super().__setitem__(node, node_attrs)
        self.data[node]['_hops'] = HopDict()

class NetworkxPortGraphImpl(nx.MultiGraph):
    node_dict_factory = NodeAttrDict

class NetworkxPortGraph(PortGraphPrimitives):

    def __init__(self, *args, **kwargs):
        self.g = NetworkxPortGraphImpl()
        super().__init__(*args, **kws)

    def datum(self, nodeid: NRef) -> Union[Node, None]:
        try:
            return self.g.nodes[nodeid]['datum']
        except KeyError:
            return None

    def _add_node(self, nodeid, datum):
        self.g.add_node(nodeid, datum=datum)

    def _add_edge(self, node1, port_label1, node2, port_label2, **attr) -> int:
        '''If the edge already exists, doesn't make a new one. Regardless,
        returns the key of the edge. Even if the edge already exists, we
        update its weight from attr.'''
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            key = hop.key
            if 'weight' in attr:  # HACK: What about other attrs?
                self[node1][node2][key]['weight'] = attr['weight']
        else:  # create edge and two hops, one for each direction
            key = self.g.add_edge(node1, node2, **attr)
            hop1 = Hop(node1, port_label1, node2, port_label2, key)
            hop2 = Hop(node2, port_label2, node1, port_label1, key)
            self.g.nodes[node1]['_hops'].add(hop1)
            self.g.nodes[node2]['_hops'].add(hop2)
        return key

    def _remove_node(self, nodeid):
        self._remove_all_hops_to(nodeid)
        self.g.remove_node(node)

    def _remove_edge(self, node1, port_label1, node2, port_label2):
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            self.remove_hop(hop)

    def remove_hop(self, hops: Hops):
        for hop in as_list(hops):
            node1 = hop.from_node
            node2 = hop.to_node
            self.g.nodes[node1]['_hops'].remove(hop)
            self.g.nodes[node2]['_hops'].remove(hop.reverse())
            self.g.remove_edge(node1, node2, hop.key)

    def hops_from_node(self, nodeid) -> FrozenSet[Hop]:
        return self.g.nodes[nodeid]['_hops'].all_hops()
        
    def hops_from_port(self, nodeid, port_label) -> FrozenSet[Hop]:
        try:
            return self.g.nodes[node]['_hops'].hops_from_port_label(port_label)
        except KeyError:
            return empty_set
        
    #TODO port_labels() ?

    @property
    def nodes(self):
        return self.g.nodes
