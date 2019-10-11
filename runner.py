# runner.py -- A command interface to numbo.py suitable for calling from
# a web server like the one in server.py

import sys
import signal
from urllib.parse import urlparse, parse_qs
import argparse
import json

from numbo import NumboGraph, new_graph
from numble import Numble

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
    fout.write(s)
    fout.write('\n')
    fout.flush()

# Ignore Ctrl-C so that server.py gets the Ctrl-C and shuts down correctly.
# This is an ugly HACK. If you run runner.py directly from the shell, Ctrl-C
# should still work, but this disables it.
signal.signal(signal.SIGINT, signal.SIG_IGN)

class Runner:

    def __init__(self):
        self.g = None
        self.numble = Numble([120, 1, 2, 3, 4, 5], 121)

    def step(self):
        if self.g is None:
            self.reset()
        else:
            self.g.do_timestep()

    def reset(self):
        self.g = new_graph(self.numble)

    def json_status(self):
        'Returns a JSON string describing the current state of the graph.'
        #STUB
        #return '{a: 1}\n'
        d = {
            't': self.g.graph['t'],
            'nodes': [self.nodedict(n) for n in self.g.nodes],
            'links': [self.edgedict(e) for e in self.g.edges]
        }
        return json.dumps(d, default=lambda x: '??', indent=2) + '\n'

    def nodedict(self, nodeid):
        n = self.g.nodes[nodeid]
        datum = n['datum']
        d = {
            'id': nodeid,
            'support': n.get('support', 0.0),
            'salience': n.get('salience', 0.0),
            'class': datum.__class__.__name__,
            'display-name': datum.display_name(self.g, nodeid),
            'd3height': 2,
            'd3width': 2
            #TODO: members, membersRecursive
        }
        d.update(n['datum'].__dict__)
        print('D', d)
        return d

    def edgedict(self, edge):
        hop = self.g.edge_to_hop(edge)
        return {
            'source': hop.from_node,
            'source_port_label': hop.from_port_label,
            'target': hop.to_node,
            'target_port_label': hop.to_port_label
            #TODO weight
        }

# ORIGINAL TEST CODE
#while True:
#    line = sys.stdin.readline()
#    url = urlparse(line)
#    qs = parse_qs(url.query)
#    print('GOT', url, qs, flush=True)

runner = Runner()
while True:
    line = fin.readline()
    url = urlparse(line)
    command = url.path.strip()
    if command.startswith('/'):
        command = command[1:]
    print('COMMAND', command, file=sys.stderr)
    if command == 'reset':
        runner.reset()
        write_fifo(runner.json_status())
    elif command == 'step':
        runner.step()
        write_fifo(runner.json_status())
    else:
        print("Unrecognized command: %s" % line, file=sys.stderr)
        write_fifo('\n')
