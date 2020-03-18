# BuildSpec.py -- Class for specifying a node to build

from util import is_iter, NiceRepr


def make_buildspec(g, nodeclass, args=(), kwargs=None):
    '''nodeclass can be either a class that inherits from Node or an instance
    of such a class.'''
    return BuildSpec(
        nodeclass,
        nodeclass.make_filled_params(g, args, kwargs)
    )

def to_args_kwargs(lis, tups):
    '''Unpacks a list into args and kwargs suitable for passing to
    make_buildspec(), according to a list of tuples (name, index) telling the
    name of each value and its index in lis. Returns a tuple (args, kwargs).'''
    args = []
    kwargs = {}
    for tup in tups:
        name, index = tup
        value = lis[index]
        if name == '_args':
            args.append(value)
        else:
            try:
                v = kwargs[name]
            except KeyError:
                kwargs[name] = value
                continue
            if is_iter(v):
                v += [value]
            else:
                v = [v] + [value]
            kwargs[name] = v
    return (args, kwargs)

class BuildSpec(NiceRepr):

    def __init__(self, nodeclass, filled_params):
        self.nodeclass = nodeclass
        self.filled_params = filled_params

    def already_built(self, g):
        candidates = g.neighbors(self.filled_params.potential_neighbors())
        if not candidates:
            #candidates = set(g.nodes)
            return False  # HACK This assumes that attr-matches don't count
                          # for determining if a node is already_built.
        #print('BS_ALR', candidates, g.nodes)
        return any(
            self.filled_params.is_match(g, self.nodeclass, nodeid)
                for nodeid in candidates
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
    n3 = g.make_node(Number(3))
    n4 = g.make_node(Number(4), to=n3)
    pg(g)
