# NetworkxPortGraph.py -- Implementation of PortGraphPrimitives using the
#                         networkx library to hold the graph

from typing import Union, List, Set, Iterable, Any, NewType, Type, FrozenSet, \
    Dict, ClassVar
from dataclasses import dataclass
from collections import UserDict

import networkx as nx

from ActiveGraph import PortGraphPrimitives, Node, NodeId, NRef, PortLabel, \
    PortLabels, Hop, Hops
from util import as_iter, as_list, empty_set


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
        for hop in as_list(self.hops_to_neighbor(to_node)):
            # as_list because Hop sets will change during iteration
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
    node automatically gets an attribute named 'hops' whose value is a
    defaultdict(set).'''

    def __setitem__(self, node: NodeId, node_attrs: Dict):
        super().__setitem__(node, node_attrs)
        self.data[node]['hops'] = HopDict()

class NetworkxPortGraphImpl(nx.MultiGraph):
    node_dict_factory = NodeAttrDict

class NetworkxPortGraph(PortGraphPrimitives):

    def __init__(self, *args, **kwargs):
        self.g = NetworkxPortGraphImpl()
        super().__init__(*args, **kwargs)

    def datum(self, nodeid: NRef) -> Union[Node, None]:
        try:
            return self.g.nodes[nodeid]['datum']
        except KeyError:
            return None

    def _add_node(self, datum) -> NodeId:
        nodeid = self._bump_nextid()
        self.g.add_node(nodeid, datum=datum)
        datum.id = nodeid
        datum.g = self
        return nodeid

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
            self.g.nodes[node1]['hops'].add(hop1)
            self.g.nodes[node2]['hops'].add(hop2)
        return key

    def _remove_node(self, nodeid):
        self._remove_all_hops_to(nodeid)
        self.g.remove_node(nodeid)

    def _remove_edge(self, node1, port_label1, node2, port_label2):
        hop = self.find_hop(node1, port_label1, node2, port_label2)
        if hop:
            self.remove_hop(hop)

    def _neighbors(self, nodeid: NodeId) -> Iterable[NodeId]:
        return self.g.neighbors(nodeid)

    def _remove_all_hops_to(self, nodeid: NodeId):
        for neighbor in as_list(self._neighbors(nodeid)):
            # as_list because Hop sets will change during iteration
            self.g.nodes[neighbor]['hops'].remove_all_hops_to(nodeid)

    def remove_hop(self, hops: Hops):
        for hop in as_iter(hops):
            node1 = hop.from_node
            node2 = hop.to_node
            self.g.nodes[node1]['hops'].remove(hop)
            self.g.nodes[node2]['hops'].remove(hop.reverse())
            self.g.remove_edge(node1, node2, hop.key)

    def _hops_from_node(self, nodeid) -> FrozenSet[Hop]:
        return self.g.nodes[nodeid]['hops'].all_hops()
        
    def _hops_from_port(self, nodeid, port_label) -> FrozenSet[Hop]:
        try:
            return self.g.nodes[nodeid]['hops'].hops_from_port_label(
                port_label
            )
        except KeyError:
            return empty_set
        
    def _hops_to_neighbor(self, nodeid, neighbor_nodeid):
        return (
            self.g.nodes[nodeid]['hops'].hops_to_neighbor(neighbor_nodeid)
        )

    def _port_labels(self, nodeid: NodeId) -> PortLabels:
        return self.g.nodes[nodeid]['hops'].from_port_labels()

    def num_nodes(self):
        return len(self.g.nodes)

    def num_edges(self):
        return len(self.g.edges)

    def _nodeids(self):
        return self.g.nodes

    def _nodes(self):
        return (self.datum(n) for n in self._nodeids())
