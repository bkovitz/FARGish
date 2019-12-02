# FARGSpec.py -- Classes that define a spec for a FARG model

from util import nice_object_repr


class EdgeInfo:
    def __init__(self, source_port_label, target_port_label):
        self.source_port_label = source_port_label
        self.target_port_label = target_port_label

    def key(self):
        '''Returns a frozenset containing the source_port_label and
        target_port_label, to enable finding this EdgeInfo in a dict.'''
        return frozenset([self.source_port_label, self.target_port_label])

    def make_edgedict(self):
        return {'source_port_label': self.source_port_label,
                'target_port_label': self.target_port_label}

    __repr__ = nice_object_repr


class FARGSpec:
    def __init__(self, edgeinfos):
        '''edgeinfos = list of EdgeInfo objects'''
        self.edgeinfos = edgeinfos

        self.d_edgeinfos = dict(
            (e.key(), e) for e in self.edgeinfos
        )

    def edgedict(self, g, edge):
        '''Creates and returns dict to send to browser to render edge.
        g is a PortGraph.
        edge is a tuple (from_node, to_node, key).'''
        hop = g.edge_to_hop(edge)
        frset = frozenset([hop.from_port_label, hop.to_port_label])
        edgeinfo = self.d_edgeinfos.get(frset)
        if edgeinfo is None:
            l1, l2 = sorted(frset)
            m = { 'source_port_label': l1, 'target_port_label': l2 }
        else:
            m = edgeinfo.make_edgedict()
        m['weight'] = g.edge_weight(edge)
        if m['source_port_label'] == hop.from_port_label:
            m['source'] = hop.from_node
            m['target'] = hop.to_node
        else:
            m['source'] = hop.to_node
            m['target'] = hop.from_node
        return m

    __repr__ = nice_object_repr
