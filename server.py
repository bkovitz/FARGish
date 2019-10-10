from subprocess import Popen, PIPE
import http.server
import socketserver
from urllib.parse import urlparse, parse_qs

PORT = 8080
runner_args = ['python3', 'runner.py']
commands = set(['step', 'reset', 'get-model'])

class RunnerRequestHandler(http.server.SimpleHTTPRequestHandler):

    def do_GET(self):
        url = urlparse(self.path)
        command = url.path
        if command.startswith('/'):
            command = command[1:]
        # If custom command, send it to the runner process and return whatever
        # it returns.
        if command in commands:
            print(self.path, file=runner.stdin, flush=True)
            l = runner.stdout.readline()
            self.send_response(200)
            self.end_headers()
            self.wfile.write(bytearray(l, encoding='utf-8'))
        else:
            super().do_GET()

    def log_message(self, *args, **kwargs):
        'Overriding to disable server logging.'
        pass

class CustomServer(socketserver.TCPServer):

    def server_close(self):
        print('SERVER_CLOSE')
        super().server_close()


with Popen(runner_args, stdin=PIPE, stdout=PIPE, text=True) as runner, \
     CustomServer(("", PORT), RunnerRequestHandler) as httpd:
        print("server at port", PORT)
        httpd.serve_forever()
