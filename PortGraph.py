# PortGraph.py -- PortGraph class

import networkx as nx

from collections import defaultdict, namedtuple, UserDict

empty_set = frozenset()

PortNeighborInfo = namedtuple('PortNeighborInfo',
    ['neighbor', 'neighbor_port_label', 'edge_key']
)

Hop = namedtuple('Hop',
    ['from_node', 'from_port_label', 'to_node', 'to_port_label', 'key']
)

class HopDict:

    def __init__(self):
        self.d_from_port_label = {}  # from_port_label: set(Hop)
        self.d_to_node = {}          # to_node: set(Hop)

    def add(self, hop):
        try:
            self.d_from_port_label[hop.from_port_label].add(hop)
        except KeyError:
            self.d_from_port_label[hop.from_port_label] = set([hop])
        try:
            self.d_to_node[hop.to_node].add(hop)
        except KeyError:
            self.d_to_node[hop.to_node] = set([hop])

    def remove(self, hop):
        '''It is not an error to remove a hop that doesn't exist.'''
        try:
            self.d_from_port_label[hop.from_port_label].discard(hop)
        except KeyError:
            pass
        try:
            self.d_to_node[hop.to_node].discard(hop)
        except KeyError:
            pass

    def hops_from_port_label(self, from_port_label):
        '''Returns a set of Hops.'''
        try:
            return self.d_from_port_label[from_port_label]
        except KeyError:
            return empty_set

    def hops_to_neighbor(self, neighbor_node):
        '''Returns a set of Hops.'''
        try:
            return self.d_to_node[neighbor_node]
        except KeyError:
            return empty_set


class NodeAttrDict(UserDict):
    '''Custom dict that maps nodes in a Graph to their attributes. Every
    node automatically gets an attribute named '_ports' whose value is a
    defaultdict(set).'''

    def __setitem__(self, node, node_attrs):
        '''node_attrs must be a dictionary object.'''
        super().__setitem__(node, node_attrs)
        self.data[node]['_hops'] = HopDict()
        self.data[node]['_ports'] = defaultdict(set)


class PortGraph(nx.MultiGraph):

    node_dict_factory = NodeAttrDict

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        key = super(PortGraph, self).add_edge(node1, node2, **attr)
        self.nodes[node1]['_ports'][port_label1].add(
            PortNeighborInfo(node2, port_label2, key)
        )
        self.nodes[node2]['_ports'][port_label2].add(
            PortNeighborInfo(node1, port_label1, key)
        )
        hop1 = Hop(node1, port_label1, node2, port_label2, key)
        hop2 = Hop(node2, port_label2, node1, port_label1, key)
        self.nodes[node1]['_hops'].add(hop1)
        self.nodes[node2]['_hops'].add(hop2)
        return key

    def pnis(self, node, port_label):
        'Returns set of PortNeighborInfos for node.port_label'
        return self.nodes[node]['_ports'].get(port_label, empty_set)

    def _remove_pni(self, node, port_label, pni):
        self.nodes[node]['_ports'][port_label].discard(pni)

    def edge_keys(self, node, port_label):
        'Returns iterator over edge keys from node.port_label'
        return (i.edge_key for i in
            self.nodes[node]['_ports'].get(port_label, empty_set))

    def remove_edge(self, node1, port_label1, node2, port_label2):
        if node1 in self and node2 in self:
            pnis1 = [pni for pni in self.pnis(node1, port_label1)
                            if pni.neighbor == node2 and
                               pni.neighbor_port_label == port_label2]
            pnis2 = [pni for pni in self.pnis(node2, port_label2)
                            if pni.neighbor == node1 and
                               pni.neighbor_port_label == port_label1]
            for pni in pnis1:
                super().remove_edge(node1, node2, pni.edge_key)
                self._remove_pni(node1, port_label1, pni)
            for pni in pnis2:
                self._remove_pni(node2, port_label2, pni)

    def hops_from_port(self, node, port_label):
        return self.nodes[node]['_hops'].hops_from_port_label(port_label)

    def hops_to_neighbor(self, node, neighbor_node):
        return (
            self.nodes[node]['_hops'].hops_to_neighbor(neighbor_node)
        )

    def has_hop(self, from_node, from_port_label, to_node, to_port_label):
        return any(hop.from_port_label == from_port_label
                   and
                   hop.to_port_label == to_port_label
                       for hop in self.hops_to_neighbor(from_node, to_node))

    def neighbors(self, node, port_label=None):
        if port_label is None:
            return super().neighbors(node)
        else:
            return (i.neighbor for i in self.pnis(node, port_label))


if __name__ == '__main__':
    g = PortGraph()
    g.add_edge('A', 'in', 'B', 'out')
    g.add_edge('O', 'members', 'A', 'member_of')
    #print(g.nodes(data=True))
    print(list(g.neighbors('A')))
    print(list(g.neighbors('A', 'in')))
    print(list(g.neighbors('B', 'out')))
    print(g.hops_to_neighbor('A', 'B'))
    g.remove_edge('A', 'in', 'B', 'out')
    print(list(g.neighbors('A')))
    print(list(g.neighbors('A', 'in')))
    print(list(g.neighbors('B', 'out')))

    #print(g.adj)
    #print(list(g.adjacency()))
    #g.remove_edge('A', 'in', 'B', 'out')
