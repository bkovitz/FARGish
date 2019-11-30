# runner.py -- A command interface to numbo.py suitable for calling from
# a web server like the one in server.py

import sys
import signal
from urllib.parse import urlparse, parse_qs
import argparse

from ModelWrapper import ModelWrapper
from log import ShowResponseList, ShowResponseResults

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

# ORIGINAL TEST CODE
#while True:
#    line = sys.stdin.readline()
#    url = urlparse(line)
#    qs = parse_qs(url.query)
#    print('GOT', url, qs, flush=True)

model_wrapper = ModelWrapper()
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
        model_wrapper.reset(data)
        write_fifo(model_wrapper.json_status())
    elif command == 'step':
        model_wrapper.step()
        write_fifo(model_wrapper.json_status())
    elif command == 'step10':
        model_wrapper.step(num=10)
        write_fifo(model_wrapper.json_status())
    elif command == 'getModel':
        model_wrapper.get_model()
        write_fifo(model_wrapper.json_status())
    elif command == 'nodeinfo':
        write_fifo(model_wrapper.nodeinfo(int(data['node'][0])))
    else:
        print("Unrecognized command: %s" % line, file=sys.stderr, flush=True)
        write_fifo('')
