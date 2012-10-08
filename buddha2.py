#!/usr/bin/env python

from __future__ import division

from itertools import product
import pickle
import sys

import Image

def clamp(x):
    return min(255, max(0, x))

def scale(x):
    return int(x * 255)

def get_color(value):
    v = int(value * 255)
    if value > 8/64:
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
    d, w, h = len(pixels), len(pixels[0]), len(pixels[0][0])
    print "Loaded %dx%dx%d array of values!" % (d, w, h)
    return pixels, (d, w, h)

first = sys.argv[1]
filenames = sys.argv[2:]

arrays, dwh = load_file(first)
d, w, h = dwh

print "Will use %dx%dx%d for dimensions" % dwh

for filename in filenames:
    data, dwh = load_file(filename)
    if (d, w, h) != dwh:
        print "Discarding %r: Wrong size %r" % (filename, dwh)
    else:
        for i, j, k in product(xrange(d), xrange(w), xrange(h)):
            arrays[i][j][k] += data[i][j][k]

out = Image.new("RGB", (w, h))

print "Calculating max depth..."

maxes = [max(max(j for j in i) for i in arr) for arr in arrays]

print "Max depths are %r" % (maxes,)

def sample(arr, i, j):
    try:
        return arr[i][j] + arr[i + 1][j] + arr[i][j + 1] + arr[i + 1][j + 1]
    except:
        return arr[i][j] * 4

for (j, k) in product(xrange(w), xrange(h)):
    values = [scale(sample(arrays[i], j, k) / maxes[i] / 2) for i in xrange(d)]

    blue = values[0]
    green = values[1]
    red = values[2]
    violet = values[3]

    blue += violet * 2
    red += violet * 2

    out.putpixel((j, k), (red, green, blue))

print "Resampling..."

out = out.resize((w // 2, h // 2), Image.ANTIALIAS)
out.save("buddha.png")
