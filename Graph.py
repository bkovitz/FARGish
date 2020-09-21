# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

from ActiveGraph import ActiveGraph, Node
from NetworkxPortGraph import NetworkxPortGraph
from NodeParams import NodeParams, AttrParam, MateParam


class Graph(NetworkxPortGraph, ActiveGraph):
    pass

if __name__ == '__main__':
    class Brick(Node):
        node_params = NodeParams(AttrParam('value'))

    g = Graph()
    b = g.add_node(Brick, value=1)
