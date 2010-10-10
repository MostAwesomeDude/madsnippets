import itertools
import math
import pprint

from termcolor import colored, COLORS

color_names = COLORS.keys()

m = dict((t, False) for t in itertools.product(range(21), range(21), range(21)))

for i in range(20):
    for j in range(20):
        x = 20**2 - (i**2 + j**2)
        if x >= 0:
            m[i, j, int(round(math.sqrt(x)))] = True

for x, y, z in m.keys():
    if m[x, y, z]:
        m[y, x, z] = True
        m[x, z, y] = True
        m[z, y, x] = True

for y in range(21):
    for x in range(21):
        cell = [m[x, y, z] for z in range(21)]
        if cell.count(True) == 0:
            print ".",
        else:
            z = cell.index(True)
            print colored("X", color_names[z % len(color_names)],
            attrs=["bold"]),
    print ""
