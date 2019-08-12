# Daves's and Ben's spreading-activation experiment on port graphs

import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
import statistics as stats

from random import random, randint
from collections import defaultdict, namedtuple
import timeit

class PortGraph0(nx.MultiGraph):
    def __init__(self, *args, **kwargs):
        self.ports = {}
        super(PortGraph, self).__init__(*args, **kwargs)

    def add_node(self, node_for_adding, **attr):
        self.ports[node_for_adding] = {}
        super(PortGraph, self).add_node(node_for_adding, **attr)

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        if not node1 in self.ports:
            self.add_node(node1)
        if not port_label1 in self.ports[node1]:
            self.ports[node1] = set()
        key = super(PortGraph, self).add_edge(node1, node2, **attr)
        labels = self.ports[node1].get(port_label1, set())
        labels.add(frozenset([node1, node2, key]))

        if not node2 in self.ports:
            self.add_node(node2)
        if not port_label2 in self.ports[node2]:
            self.ports[node2] = {}
        labels = self.ports[node2].get(port_label2, set())
        labels.add(frozenset([node1, node2, key]))

empty_set = frozenset()

PortNeighborInfo = namedtuple('PortNeighborInfo',
    ['neighbor', 'neighbor_port_label', 'edge_key']
)

class PortGraph(nx.MultiGraph):

    def add_node(self, node_for_adding, **attr):
        attr['_ports'] = defaultdict(set) # {port_label: set(PortNeighborInfo)}
        super(PortGraph, self).add_node(node_for_adding, **attr)

    def add_edge(self, node1, port_label1, node2, port_label2, **attr):
        if node1 not in self:
            self.add_node(node1)
        if node2 not in self:
            self.add_node(node2)
        key = super(PortGraph, self).add_edge(node1, node2, **attr)
        self.nodes[node1]['_ports'][port_label1].add(
            PortNeighborInfo(node2, port_label2, key)
        )
        self.nodes[node2]['_ports'][port_label2].add(
            PortNeighborInfo(node1, port_label1, key)
        )
        return key

    def pnis(self, node, port_label):
        'Returns set of PortNeighborInfos for node.port_label'
        return self.nodes[node]['_ports'].get(port_label, empty_set)

    def _remove_pni(self, node, port_label, pni):
        self.nodes[node]['_ports'][port_label].discard(pni)

    def edge_keys(self, node, port_label):
        'Returns iterator over edge keys from node.port_label'
        return (i.edge_key for i in
            self.nodes[node]['_ports'].get(port_label, []))

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

#            edge_keys1 = set(self.edge_keys(node1, port_label1))
#            edge_keys2 = set(self.edge_keys(node2, port_label2))
#            print(edge_keys1.intersection(edge_keys2))
#            for key in edge_keys1.intersection(edge_keys2):
#                super().remove_edge(node1, node2, key)
#                edge_keys1.remove(key)
#                edge_keys2.remove(key)

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
    g.remove_edge('A', 'in', 'B', 'out')
    print(list(g.neighbors('A')))
    print(list(g.neighbors('A', 'in')))
    print(list(g.neighbors('B', 'out')))
    #print(g.adj)
    #print(list(g.adjacency()))
    #g.remove_edge('A', 'in', 'B', 'out')
