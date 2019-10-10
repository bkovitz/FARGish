import sys
from urllib.parse import urlparse, parse_qs

while True:
    line = sys.stdin.readline()
    url = urlparse(line)
    qs = parse_qs(url.query)
    print('GOT', url, qs, flush=True)
