from subprocess import Popen, PIPE
import http.server
import socketserver
from urllib.parse import urlparse, parse_qs
import os
import time
import sys

from util import read_to_blank_line

def make_fifo(name):
    if os.access(name, os.F_OK):
        os.unlink(name)
    os.mkfifo(name)

fifo_out = 'server_out'  # We send commands to the runner through this...
fifo_in = 'server_in'    # ...and read the results back through this.
make_fifo(fifo_out)
make_fifo(fifo_in)
#fout = None  # We'll replace these with file objects after we start the
#fin = None   # subprocess.

PORT = 8081
runner_args = ['python3', 'runner.py', '--rfifo', fifo_out, '--wfifo', fifo_in]
commands = set(['step', 'step10', 'reset', 'get-model', 'nodeinfo'])


class RunnerInterface:

    def __init__(self):
        self.fout = None  # FIFO file objects
        self.fin = None
        self.subp = None  # subprocess
        self.start_time = None

    def write(self, s):
        if self.subp_ok():
            print(s, file=self.fout, flush=True)

    def read(self):
        if self.subp_ok():
            return read_to_blank_line(self.fin)
        else:
            return '\n'

    def subp_ok(self):
        if self.subp is None:
            return self.start_subp()
        elif self.subp.poll() is not None:
            print('Subprocess exited (return_code=%d)' % self.subp.poll())
            return self.start_subp()
        elif self.start_time < os.path.getmtime('runner.py'):
            print('runner.py has been modified.')
            return self.start_subp()
        else:
            return True

    def start_subp(self):
        if self.subp is not None:
            print('Restarting subprocess')
            self.subp.kill()
        try:
            self.subp = Popen(runner_args)
            self.start_time = time.time()
            self.fout = open(fifo_out, mode='w')
            self.fin = open(fifo_in, mode='r')
        except exc:
            self.subp = None
            self.fout = None
            self.fin = None
            print(exc)
            return False
        return True

rif = RunnerInterface()

class RunnerRequestHandler(http.server.SimpleHTTPRequestHandler):

    def do_GET(self):
        url = urlparse(self.path)
        command = url.path
        if command.startswith('/'):
            command = command[1:]
        # If custom command, send it to the runner process and return whatever
        # it returns.
        if command in commands:
            #print(self.path, file=fout, flush=True)
            #reply = read_to_blank_line(fin)
            print('DEBUG', self.path, flush=True, file=sys.stderr)
            rif.write(self.path)
            reply = rif.read()
            self.send_response(200)
            self.end_headers()
            self.wfile.write(bytearray(reply, encoding='utf-8'))
        else:
            super().do_GET()

#    def log_message(self, *args, **kwargs):
#        'Overriding to disable server logging.'
#        pass

class CustomServer(socketserver.TCPServer):

    def server_close(self):
        print('SERVER_CLOSE')
        self.shutdown()
        super().server_close()


with CustomServer(("localhost", PORT), RunnerRequestHandler) as httpd:
    print("server at port", PORT)
    #fout = open(fifo_out, mode='w')
    #fin = open(fifo_in, mode='r')
    httpd.serve_forever()
#    print('POST-FIN')
#    while True:
#        if runner.poll() is not None:
#            print('Subprocess exited (return_code=%d)' % runner.poll())
#            break
#        #httpd.serve_forever()
#        try:
#            httpd.handle_request()
#        except:
#            runner.kill()
#            raise
