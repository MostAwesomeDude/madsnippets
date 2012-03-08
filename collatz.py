#!/usr/bin/env python

import networkx as nx

dg = nx.DiGraph()

def collatz(x):
    if x % 2:
        return x * 3 + 1
    else:
        return x / 2

pots = [2**i for i in range(32, -1, -1)]
dg.add_path(pots)

pots = set(pots)

marker = 100000

for i in xrange(6, 2**20, 6):
    if i > marker:
        print "Examined %d entrypoints, %d nodes in the graph" % (i,
                dg.number_of_nodes())
        marker += 100000

    for x in (i - 1, i + 1):
        l = [x]
        while x not in dg:
            x = collatz(x)
            l.append(x)
            if x in pots:
                dg.add_path(l)
                break
            elif not x % 3:
                l = [collatz(x)]

nx.write_dot(dg, "collatz.dot")
