# submatch.py -- Subgraph matching

from PortGraph import PortGraph

import networkx as nx

from collections import defaultdict

'''
is_node_match
is_edge_match
'''


class Node:
    class_to_nextid = {}  # cls: id to assign next instance of cls

    @classmethod
    def assign_id(cls):
        n = Node.class_to_nextid.get(cls, 1)
        Node.class_to_nextid[cls] = n + 1
        return cls.__name__ + str(n)

    def __init__(self):
        self.id = self.assign_id()

    def __repr__(self):
        return self.id

class Number(Node):

    def __init__(self, n=None):
        super().__init__()
        self.n = n

tg = PortGraph()
tg.add_node(Number(2))
tg.add_node(Number(2))

g = PortGraph()
g.add_nodes_from([(0, {'salience': 0.1})])

