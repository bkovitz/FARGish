# StdGraph.py -- The usual ActiveGraph class that represents the FARG model

from ActiveGraph import ActiveGraph
from NetworkxPortGraph import NetworkxPortGraph, NetworkxActivation
from NodeParams import NodeParams, AttrParam, MateParam


class Graph(ActiveGraph, NetworkxActivation, NetworkxPortGraph):
    pass

def pg(g):
    for node in g.nodes():
        print(node)
