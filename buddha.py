#!/usr/bin/env python

from __future__ import division

import itertools
import random
import sys

import Image

HEIGHT = 1050
WIDTH = 1680

# Min and max iterations
SETD = 20
SETI = 50000

MINH = -1.1
MAXH = 1.1
MINW = -2.2
MAXW = 1.1

COUNT = 50000

FILENAME = "buddha.png"

invalid_ranges = (
    (-1.2, -1.1, 0, 0.1),
    (-1.1, -0.9, 0, 0.2),
    (-0.9, -0.8, 0, 0.1),
    (-0.69, -0.61, 0, 0.2),
    (-0.61, -0.5, 0, 0.37),
    (-0.5, -0.39, 0, 0.48),
    (-0.39, 0.14, 0, 0.55),
    (0.14, 0.29, 0.07, 0.42),
)

def checkrange(i, j):
    # Borrowed from Evercat on Wikipedia

    for (mini, maxi, minj, maxj) in invalid_ranges:
        if mini < i <= maxi and minj < abs(j) < maxj:
            return False

    return True

def update(string, prevlen=[0]):
    if prevlen[0]:
        sys.stdout.write("\b"*prevlen[0])
    else:
        sys.stdout.write("\n")
    sys.stdout.write(string)
    sys.stdout.flush()
    prevlen[0] = len(string)

def genrandom():
    d = {}
    count = 0
    while len(d) < COUNT:
        (i, j) = (random.uniform(MINW, MAXW), random.uniform(MINH, MAXH))
        if checkrange(i, j):
            d[complex(i, j)] = complex(0)
        else:
            count += 1
    update("Generated %d, trimmed %d initial values" % (len(d), count), [0])
    return d

pixels = [[0 for j in xrange(WIDTH)] for i in xrange(HEIGHT)]

update("Preparing...", [0])

d = genrandom()

keystokeep = []

deletedkeys = 0
totalkeys = len(d)

for i in xrange(SETI):
    update("Set: %d/%d Divergents: %d/%d Remaining: %d" % (i + 1, SETI, deletedkeys, totalkeys, len(d)))
    keystodel = []
    for (c, z) in d.iteritems():
        d[c] = complex(z**2 + c)
        pixh = int((d[c].imag - MINH) * HEIGHT/(MAXH-MINH))
        pixw = int((d[c].real - MINW) * WIDTH/(MAXW-MINW))
        if not abs(d[c]) <= 2 or not 0 <= pixh < HEIGHT or not 0 <= pixw < WIDTH:
            keystodel.append(c)
    deletedkeys += len(keystodel)
    if i > SETD:
        [keystokeep.append(d.pop(key)) for key in keystodel]
    else:
        [d.pop(key) for key in keystodel]

temp = {}

for key in keystokeep:
    temp[key] = complex(0)

d = temp
update("Will trace %d divergents" % len(d), [0])

update("Calculating...", [0])
totalkeys = len(d)
deletedkeys = 0

i = 0

while d:
    update("Buddha: %d/? Divergents: %d/%d" % (i, deletedkeys, totalkeys))
    keystodel = []
    for (c, z) in d.iteritems():
        d[c] = complex(z**2 + c)
        pixh = int((d[c].imag - MINH) * HEIGHT/(MAXH-MINH))
        pixw = int((d[c].real - MINW) * WIDTH/(MAXW-MINW))
        if 0 <= pixh < HEIGHT and 0 <= pixw < WIDTH:
            pixels[pixh][pixw] += 1
        else:
            keystodel.append(c)
    deletedkeys += len(keystodel)
    for key in keystodel:
        del d[key]
    i += 1

update("No more divergents, exiting...", [0])

maxdepth = 0

update("Calculating max depth...", [0])

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    if pixels[i][j] > maxdepth:
        maxdepth = pixels[i][j]
        update("Depth of %d at %d,%d" % (maxdepth, i, j))

depthfix = maxdepth/255 if maxdepth/255 else 1

out = Image.new("RGB",(HEIGHT,WIDTH))

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    value = int(pixels[i][j]/depthfix)
    out.putpixel((i,j),(value, value, value))
#    pix = (pixels[i][j] - 511, pixels[i][j] - 255, pixels[i][j])
#    out.putpixel((i,j), pix)

out.save(FILENAME)
update("Done!", [0])
