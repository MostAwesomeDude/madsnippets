#!/usr/bin/env python

import itertools
import pickle

import Image

def get_color(value):
    v = int(value * 255)
    if value > 0.95:
        return (255, 51, 204) # violet
    elif value > 0.9:
        return (255, 204, 0) # gold
    elif value > 0.8:
        return (255, 255, 204) # silver
    elif value > 0.6:
        return (v, 255, v) # greens
    elif value > 0.4:
        return (204, v + 51, v + 51) # reds
    elif value > 0.2:
        return (v + 102, v + 51, 102) # blues
    elif value:
        return (v + 51, v + 51, v + 51) # grays
    else:
        return (0, 0, 0) # blacks

pixels = pickle.load(open("buddha.P", "r"))

HEIGHT, WIDTH = len(pixels), len(pixels[0])

print "Loaded %dx%d array of values!" % (HEIGHT, WIDTH)

out = Image.new("RGB",(HEIGHT,WIDTH))

maxdepth = 0

print "Calculating max depth..."

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    if pixels[i][j] > maxdepth:
        maxdepth = pixels[i][j]

print "Max depth is %d" % maxdepth

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    value = pixels[i][j] / maxdepth
    out.putpixel((i,j), get_color(value))

out.save("buddha.png")
