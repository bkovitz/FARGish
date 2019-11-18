# runner.py -- A command interface to numbo.py suitable for calling from
# a web server like the one in server.py

import sys
import signal
from urllib.parse import urlparse, parse_qs
import argparse
import json

from numbo import NumboGraph, new_graph
from numble import Numble
from log import ShowResponseList, ShowResponseResults
from PortGraph import long_nodestr

parser = argparse.ArgumentParser(description='FIFO test')
parser.add_argument('--rfifo', dest='rfifo')
parser.add_argument('--wfifo', dest='wfifo')
args = parser.parse_args()

print('R1')
fin = open(args.rfifo, mode='r')
print('R2')
fout = open(args.wfifo, mode='w')
print('R3')

def write_fifo(s):
    s = s.strip()
    fout.write(s)
    fout.write('\n\n')
    fout.flush()

# Ignore Ctrl-C so that server.py gets the Ctrl-C and shuts down correctly.
# This is an ugly HACK. If you run runner.py directly from the shell, Ctrl-C
# should still work, but this disables it.
signal.signal(signal.SIGINT, signal.SIG_IGN)

class Runner:

    def __init__(self):
        self.g = None
        self.numble = Numble([120, 1, 2, 3, 4, 5], 121)

    def step(self, num=1):
        if self.g is None:
            self.reset()
        else:
            self.g.do_timestep(num=num)

    def reset(self, data=None):
        #TODO Catch errors and send an error message to the client
        print('DATA', data)
        if data:
            try:
                bricks = [int(b) for b in data['bricks'][0].split()]
                target = int(data['target'][0])
                self.numble = Numble(bricks, target)
            except KeyError:
                pass
        self.g = new_graph(self.numble)

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

    def edgedict(self, edge):
        hop = self.g.edge_to_hop(edge)
        return {
            'source': hop.from_node,
            'source_port_label': hop.from_port_label,
            'target': hop.to_node,
            'target_port_label': hop.to_port_label,
            'weight': self.g.edge_weight(edge)
        }

# ORIGINAL TEST CODE
#while True:
#    line = sys.stdin.readline()
#    url = urlparse(line)
#    qs = parse_qs(url.query)
#    print('GOT', url, qs, flush=True)

runner = Runner()
ShowResponseResults.start_logging()
ShowResponseList.start_logging()
while True:
    line = fin.readline()
    url = urlparse(line)
    data = parse_qs(url.query)
    command = url.path.strip()
    if command.startswith('/'):
        command = command[1:]
    print('COMMAND', command, file=sys.stderr)
    if command == 'reset':
        runner.reset(data)
        write_fifo(runner.json_status())
    elif command == 'step':
        runner.step()
        write_fifo(runner.json_status())
    elif command == 'step10':
        runner.step(num=10)
        write_fifo(runner.json_status())
    elif command == 'getModel':
        runner.get_model()
        write_fifo(runner.json_status())
    elif command == 'nodeinfo':
        write_fifo(runner.nodeinfo(int(data['node'][0])))
    else:
        print("Unrecognized command: %s" % line, file=sys.stderr, flush=True)
        write_fifo('')
