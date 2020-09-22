# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

from ActiveGraph import ActiveGraph, Node
from NetworkxPortGraph import NetworkxPortGraph
from NodeParams import NodeParams, AttrParam, MateParam


class Graph(NetworkxPortGraph, ActiveGraph):
    pass

def pg(g):
    for node in g.nodes():
        print(node)

if __name__ == '__main__':
    class Brick(Node):
        node_params = NodeParams(AttrParam('value'))

    g = Graph()
    b1 = g.add_node(Brick, value=1)
    b2 = g.add_node(Brick(2))
    pg(g)
