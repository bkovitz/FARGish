# BuildSpec.py -- Class for specifying a node to build

from util import NiceRepr


def make_buildspec(g, nodeclass, args=(), kwargs=None):
    return BuildSpec(
        nodeclass,
        nodeclass.make_filled_params(g, args, kwargs)
    )

#TODO rm
#def make_node(g, nodeclass, *args, **kwargs):
#    spec = make_buildspec(g, nodeclass, args=args, kwargs=kwargs)
#    return spec.build(g)

class BuildSpec(NiceRepr):

    def __init__(self, nodeclass, filled_params):
        self.nodeclass = nodeclass
        self.filled_params = filled_params

    def already_built(self, g):
        return any(
            self.filled_params.is_match(g, self.nodeclass, nodeid)
                for nodeid in self.filled_params.potential_neighbors()
        )

    def build(self, g):
        '''Builds specified node unless already built. Returns nodeid of
        new node, or None if not built. Adds links as specified, adds
        attrs to the datum object, as specified, and adds an .id member
        to the datum, holding the nodeid.'''
        if not self.already_built(g):
            nodeid = g.mknode(self.nodeclass)
            self.filled_params.apply_to_node(g, nodeid)
            return nodeid
        else:
            return None

if __name__ == '__main__':
    from PortMates import PortMates
    from PortGraph import PortGraph, Node, pg
    from NodeParams import NodeParams, AttrParam, MateParam
    pms = PortMates([('from', 'to'), ('behalf_of', 'agent')])
    g = PortGraph(port_mates=pms)
    class Blah(Node):
        node_params = NodeParams(MateParam('from', 'to'))
    class Number(Node):
        node_params = NodeParams(AttrParam('value'))
    sp = make_buildspec(g, Blah)
    b1 = sp.build(g)
    sp2 = make_buildspec(g, Blah, kwargs={'from': b1})
    b2 = sp2.build(g)
    n1 = g.make_node(Number, 1, to=b2)
    pg(g)
