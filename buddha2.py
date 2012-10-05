#!/usr/bin/env python

from __future__ import division

from itertools import product
import pickle
import sys

import Image

def get_color(value):
    v = int(value * 255)
    if value > 3/4:
        return (255, 51, 204) # violet
    elif value > 1/2:
        return (255, 204, 0) # gold
    elif value > 8/64:
        return (255, 255, 204) # silver
    elif value > 7/64:
        return (v, 255, v) # greens
    elif value > 6/64:
        return (204, v + 51, v + 51) # reds
    elif value > 5/64:
        return (v + 102, v + 51, 102) # blues
    elif value > 4/64:
        return (v + 51, v + 51, v + 51) # grays
    elif value:
        return (v, v, v) # blacks
    else:
        return (0, 0, 0) # No hits at all

def fair_color(value):
    v = int(value * 768)
    r = max(v - 512, 0)
    g = min(255, v)
    b = min(255, max(v - 256, 0))

    return (r, g, b)

def lum(value):
    v = int(value * 255)
    return (v, v, v)

def load_file(path):
    pixels = pickle.load(open(path, "r"))
    w, h = len(pixels), len(pixels[0])
    print "Loaded %dx%d array of values!" % (w, h)
    return pixels

first = sys.argv[1]
filenames = sys.argv[2:]

base = load_file(first)
w = len(base)
h = len(base[0])

print "Will use %dx%d for dimensions" % (w, h)

for filename in filenames:
    pixels = load_file(filename)
    if len(pixels) != w or len(pixels[0]) != h:
        print "Discarding %r: Wrong size" % filename
    else:
        for i, j in product(xrange(w), xrange(h)):
            base[i][j] += pixels[i][j]

out = Image.new("RGB",(w, h))

maxdepth = 0

print "Calculating max depth..."

maxdepth = max(max(j for j in i) for i in base)

print "Max depth is %d" % maxdepth

for (i, j) in product(xrange(w), xrange(h)):
    value = base[i][j] / maxdepth
    out.putpixel((i, j), get_color(value))
    #out.putpixel((i, j), fair_color(value))
    #out.putpixel((i, j), lum(value))

print "Resampling..."

out = out.resize((w // 2, h // 2), Image.ANTIALIAS)
out.save("buddha.png")
