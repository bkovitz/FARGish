# ModelWrapper.py -- A wrapper class around a FARG model, for runner.py to
#                    send commands to the model and get a JSON representation
#                    of it to send to a web browser

import json

#from numbo import NumboGraph, new_graph
#from numbo3 import new_graph, Numble
from demo2 import new_graph, Numble
#from numble import Numble
from numbospec import spec as numbo_spec  # TODO OBSOLETE!
from PortGraph import long_nodestr

class ModelWrapper:

    def __init__(self):
        self.g = None
        #TODO rm Numbo-specific code
        #self.numble = Numble([120, 1, 2, 3, 4, 5], 121)
        self.numble = Numble([4, 5, 6], 15)

    def step(self, num=1):
        if self.g is None:
            self.reset()
        else:
            self.g.do_timestep(num=num)

    def reset(self, data=None):
        #TODO Catch errors and send an error message to the client
        #print('DATA', data)
        if data:
            try:
                bricks = [int(b) for b in data['bricks'][0].split()]
                target = int(data['target'][0])
                self.numble = Numble(bricks, target)
            except KeyError:
                pass
        self.g = new_graph(self.numble)  #, seed=7691554214943035002)
        print('SEED', self.g.graph['seed']) #TODO Show this in the browser

    def nodeinfo(self, node):
        return json.dumps(long_nodestr(self.g, node))

    def get_model(self):
        if self.g is None:
            self.reset()

    def json_status(self):
        'Returns a JSON string describing the current state of the graph.'
        #STUB
        #return '{a: 1}\n'
        d = {
            't': self.g.graph['t'],
            'nodes': [self.nodedict(n) for n in self.g.nodes],
            'links': [self.edgedict(e) for e in self.g.edges],
            'numble': self.numble.as_dict()
        }
        return json.dumps(d, default=lambda x: '??', indent=2)

    def nodedict(self, nodeid):
        n = self.g.nodes[nodeid]
        datum = n['datum']
        d = {
            'id': nodeid,
            'support': self.g.support_for(nodeid),
            'salience': self.g.raw_salience(nodeid),
            'class': datum.__class__.__name__,
            'display-name': datum.display_name(self.g, nodeid),
            'nodestr': self.g.nodestr(nodeid),
            'tag?': bool(datum.is_tag),
            'members': self.g.members_of(nodeid),
            'membersRecursive': list(self.g.members_recursive(nodeid)),
            'memberOf': list(self.g.member_of(nodeid)),
            'taggees': list(self.g.taggees_of(nodeid)),
            'd3height': 2,
            'd3width': 2
            #TODO: members, membersRecursive
        }
        if d['members']: # if container
            #HACK Should calculate d3 width & height as in the Racket version
            d['d3height'] = 20
            d['d3width'] = 15
        d.update(n['datum'].__dict__)
        #print('D', d)
        return d

    def edgedictOLD(self, edge):
        hop = self.g.edge_to_hop(edge)
        return {
            'source': hop.from_node,
            'source_port_label': hop.from_port_label,
            'target': hop.to_node,
            'target_port_label': hop.to_port_label,
            'weight': self.g.edge_weight(edge)
        }

    def edgedictOLD2(self, edge):
        hop = self.g.edge_to_hop(edge)
        m = make_edge_label_map(hop.from_port_label, hop.to_port_label)
        m['weight'] = self.g.edge_weight(edge)
        if m['source_port_label'] == hop.from_port_label:
            m['source'] = hop.from_node
            m['target'] = hop.to_node
        else:
            m['source'] = hop.to_node
            m['target'] = hop.from_node
        return m

    def edgedict(self, edge):
        return numbo_spec.edgedict(self.g, edge)

# TODO Make this part of the model definition
edge_label_map = {
    frozenset(['taggees', 'tags']):
        { 'source_port_label': 'taggees',
          'target_port_label': 'tags' },
    frozenset(['support_from', 'support_to']):
        { 'source_port_label': 'support_from',
          'target_port_label': 'support_to' }
}

def make_edge_label_map(port_label1, port_label2):
    m = edge_label_map.get(frozenset([port_label1, port_label2]))
    if m is None:
        l1, l2 = sorted([port_label1, port_label2])
        m = { 'source_port_label': l1, 'target_port_label': l2 }
    else:
        m = dict(m)
    return m
