#!/usr/bin/env python

from __future__ import division

import optparse
import random
import sys

import Image

HEIGHT = 1050
WIDTH = 1680

SETI = 50000
BUDDHAI = 50000

MINH = -1.1
MINW = -2.2
MAXH = 1.1
MAXW = 1.1

RESOLUTION = 0.05

FILENAME = "buddha.png"

class BuddhaError(Exception):
    pass

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

def genrandom(resolution):
    d = {}
    HSTEPS = int((MAXH-MINH)/resolution)
    WSTEPS = int((MAXW-MINW)/resolution)
    count = 0
    while len(d) < (HSTEPS*WSTEPS):
        (i, j) = (random.uniform(MINW, MAXW), random.uniform(MINH, MAXH))
        if checkrange(i, j):
            d[complex(i, j)] = complex(0)
        else:
            count += 1
    print "Generated %d, trimmed %d initial values" % (HSTEPS*WSTEPS, count)
    return d

parser = optparse.OptionParser()
parser.add_option("-r", "--resolution", dest="resolution", type="float",
    help="Set resolution to FLOAT", metavar="FLOAT")

(opts, args) = parser.parse_args()

if opts.resolution:
    RESOLUTION = opts.resolution

pixels = [[0 for j in xrange(WIDTH)] for i in xrange(HEIGHT)]

update("Preparing...", [0])

d = genrandom(RESOLUTION)

keystokeep = []

deletedkeys = 0
totalkeys = len(d)

for i in range(SETI):
    update("Set: %d/%d Divergents: %d/%d Remaining: %d" % (i + 1, SETI, deletedkeys, totalkeys, len(d)))
    keystodel = []
    for (c, z) in d.iteritems():
        try:
            d[c] = complex(z**2 + c)
            assert abs(d[c]) <= 2
            pixh = int((d[c].imag - MINH) * HEIGHT/(MAXH-MINH))
            pixw = int((d[c].real - MINW) * WIDTH/(MAXW-MINW))
            assert 0 <= pixh < HEIGHT
            assert 0 <= pixw < WIDTH
        except (AssertionError, OverflowError):
            keystokeep.append(c)
            keystodel.append(c)
    for key in keystodel:
        deletedkeys += 1
        del d[key]

temp = {}

for key in keystokeep:
    temp[key] = complex(0)

d = temp
update("Will trace %d divergents" % len(d), [0])

update("Calculating...", [0])
totalkeys = len(d)
deletedkeys = 0

for i in range(BUDDHAI):
    update("Buddha: %d/%d Divergents: %d/%d" % (i + 1, BUDDHAI, deletedkeys, totalkeys))
    keystodel = []
    if not d:
        update("No more divergents, exiting early...", [0])
        break
    for (c, z) in d.iteritems():
        d[c] = complex(z**2 + c)
        try:
            pixh = int((d[c].imag - MINH) * HEIGHT/(MAXH-MINH))
            pixw = int((d[c].real - MINW) * WIDTH/(MAXW-MINW))
            assert 0 <= pixh < HEIGHT
            assert 0 <= pixw < WIDTH
            pixels[pixh][pixw] +=1
        except AssertionError:
            keystodel.append(c)
    for key in keystodel:
        deletedkeys += 1
        del d[key]

maxdepth = 0

update("Calculating max depth...", [0])

for i in xrange(HEIGHT):
    for j in xrange(WIDTH):
        if pixels[i][j] > maxdepth:
            maxdepth = pixels[i][j]
            update("Depth of %d at %d,%d" % (maxdepth, i, j))

depthfix = maxdepth/255 if maxdepth/255 else 1

out = Image.new("RGB",(HEIGHT,WIDTH))

for i in xrange(HEIGHT):
    for j in xrange(WIDTH):
        value = int(pixels[i][j]/depthfix)
        out.putpixel((i,j),(value, value, value))
        pix = (pixels[i][j] - 511, pixels[i][j] - 255, pixels[i][j])
#        out.putpixel((i,j), pix)

out.save(FILENAME)
update("Done!", [0])
