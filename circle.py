#!/usr/bin/env python

import itertools
import math
import pprint
import sys

try:
    from termcolor import colored, COLORS
    color_names = COLORS.keys()
    def print_depth(z):
        print colored("X", color_names[z % len(color_names)], attrs=["bold"]),
except ImportError:
    def print_depth(z):
        print chr(ord("A") + z),

if len(sys.argv) < 2:
    print "Usage: %s <circle|sphere> <size>" % sys.argv[0]
    sys.exit()

if sys.argv[1] == "circle":
    if len(sys.argv) < 3:
        print "Circles need a size."
        sys.exit()

    size = int(sys.argv[2])

    matrix = dict(
        (t, False)
        for t in itertools.product(range(size + 1), range(size + 1)))

    for i in range(size):
        x = size**2 - i**2
        if x >= 0:
            matrix[i, int(round(math.sqrt(x)))] = True

    for x, y in matrix.keys():
        if matrix[x, y]:
            matrix[y, x] = True

    for y in range(size + 1):
        for x in range(size + 1):
            if matrix[x, y]:
                print "X",
            else:
                print ".",
        print ""

elif sys.argv[1] == "sphere":
    if len(sys.argv) < 3:
        print "Spheres need a size."
        sys.exit()

    size = int(sys.argv[2])

    matrix = dict(
        (t, False)
        for t in itertools.product(range(size + 1), range(size + 1),
            range(size + 1)))

    for i in range(size):
        for j in range(size):
            x = size**2 - (i**2 + j**2)
            if x >= 0:
                matrix[i, j, int(round(math.sqrt(x)))] = True

    for x, y, z in matrix.keys():
        if matrix[x, y, z]:
            matrix[y, x, z] = True
            matrix[x, z, y] = True
            matrix[z, y, x] = True

    for y in range(size + 1):
        for x in range(size + 1):
            cell = [matrix[x, y, z] for z in range(size + 1)]
            if cell.count(True) == 0:
                print ".",
            else:
                z = cell.index(True)
                print_depth(z)
        print ""
