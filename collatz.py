#!/usr/bin/env python

import networkx as nx
import matplotlib.pyplot as plt

dg = nx.DiGraph()
dg.add_edge(4, 2)
dg.add_edge(1, 4)
dg.add_edge(2, 1)

def collatz(x):
    if x % 2:
        return x * 3 + 1
    else:
        return x / 2

for i in xrange(1, 10):
    for x in (i * 6 - 1, i * 6 + 1):
        l = [x]
        while x not in dg:
            x = collatz(x)
            l.append(x)
        if len(l) > 1:
            dg.add_path(l)

nx.draw(dg)
plt.savefig("collatz.png")
plt.show()
