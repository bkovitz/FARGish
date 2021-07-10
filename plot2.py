#! /usr/bin/env python3
import csv
from collections import defaultdict

import matplotlib.pyplot as plt

f = open('a.csv', mode='r')
reader = csv.reader(f, quoting=csv.QUOTE_NONNUMERIC)
d = defaultdict(list)
max_t = 0
max_a = 0
for row in reader:
    t, node, a = row
    if not (node.startswith('Consume') or node.startswith('ImCell')):
        continue
    max_t = max(t, max_t)
    max_a = max(a, max_a)
    d[node].append((int(t), a))

plt.ion()
plt.xlabel('t')
plt.ylabel('a')
for node, series in d.items():
    plt.plot(*zip(*series), label=node)
plt.axis([0, max_t, 0, max_a])
plt.legend()
