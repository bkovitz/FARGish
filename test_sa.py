# test_sa.py -- Test of spreading activation

#if __name__ == '__main__':

import unittest
from dataclasses import dataclass

from typing import Union, List, Tuple, Dict, Set, FrozenSet, Iterable, Any, \
    NewType, Type, ClassVar, Sequence, Callable
from itertools import chain

import networkx as nx

from Propagator import Propagator, Delta


NodeId = NewType('NodeId', int)

@dataclass
class MyProp(Propagator):

    noise: float = 0.0
    
    def make_deltas(self, g, old_d):
        return chain.from_iterable(
            self.deltas_to(g, old_d, nodeid)
                for nodeid in old_d
        )

    def deltas_to(self, g, old_d: Dict[NodeId, float], nodeid: NodeId) \
    -> List[Delta]:
        result: List[Delta] = []
        for neighborid, edge_d in g.adj[nodeid].items():
            neighbor_a = old_d.get(neighborid, 0.0)
            weight = edge_d.get('weight', 1.0)
            result.append(Delta(
                nodeid,
                weight * neighbor_a
            ))
        return result

    def min_value(self, g, nodeid):
        return 0.0

class TestSA(unittest.TestCase):

    def test_sa(self):
        p = MyProp()
        self.assertEqual(p.noise, 0.0)

        g = nx.Graph()  # undirected graph

        g.add_edge(1, 2, weight=1.0)
        g.add_edge(1, 3, weight=1.3)
        g.add_node(4)

        #print(g.edges[1, 2]['weight'])

        #for neighbor in g.adj[1].items():
            #print(neighbor)

        # Let's give all nodes activation=1.0.
        initial_a_dict = dict((nodeid, 1.0) for nodeid in g.nodes)

        # Propagate
        got: Dict[NodeId, float] = p.propagate(g, initial_a_dict)
        self.assertEqual(got, {1: 1.026, 2: 1.0, 3: 1.006, 4: 0.98})
