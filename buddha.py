#!/usr/bin/env python

from __future__ import division

import cPickle
import gzip
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

RESOLUTION = 0.1

FILENAME = "buddha.png"

PALETTE = None

class BuddhaError(Exception):
    pass

def checkrange(i, j):
    # Borrowed from WP user Evercat

    if -1.2 < i <= -1.1 and -0.1 < j < 0.1:
        return False
    if -1.1 < i <= -0.9 and -0.2 < j < 0.2:
        return False
    if -0.9 < i <= -0.8 and -0.1 < j < 0.1:
        return False
    if -0.69 < i <= -0.61 and -0.2 < j < 0.2:
        return False
    if -0.61 < i <= -0.5 and -0.37 < j < 0.37:
        return False
    if -0.5 < i <= -0.39 and -0.48 < j < 0.48:
        return False
    if -0.39 < i <= 0.14 and -0.55 < j < 0.55:
        return False
    if 0.14 < i <= 0.29 and 0.07 < abs(j) < 0.42:
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

def gentable(resolution):
    d = {}
    HSTEPS = int((MAXH-MINH)/resolution)
    WSTEPS = int((MAXW-MINW)/resolution)
    count = 0
    i = MINH
    for k in xrange(HSTEPS):
        j = MINW
        for l in xrange(WSTEPS):
            if checkrange(i, j):
                d[complex(i, j)] = complex(0)
            else:
                count += 1
            j += resolution
        i += resolution
    print "Generated %d, trimmed %d initial values" % (HSTEPS*WSTEPS, count)
    return

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
parser.add_option("-g", "--generate", dest="genfile",
    help="Pregenerate table and save to FILE", metavar="FILE")
parser.add_option("-t", "--table", dest="tablefile",
    help="Use FILE as complex table", metavar="FILE")
parser.add_option("-r", "--resolution", dest="resolution", type="float",
    help="Set resolution to FLOAT", metavar="FLOAT")
parser.add_option("-p", "--palette", dest="palette",
    help="Use FILE for a palette (GIMP-style)", metavar="FILE")
parser.add_option("-o", "--output", dest="filename",
    help="Output to FILE", metavar="FILE")
parser.add_option("-R", "--random", dest="random", action="store_true",
    help="Switch to random mode", default=False)

(opts, args) = parser.parse_args()

gencomplex = genrandom if opts.random else gentable

if opts.resolution:
    RESOLUTION = opts.resolution

if opts.genfile:
    d = gencomplex(RESOLUTION)
    f = gzip.open(opts.genfile, "wb")
    update("Dumping table...", [0])
    cPickle.dump(d, f)
    f.close()
    sys.exit()

pixels = [[0 for j in xrange(WIDTH)] for i in xrange(HEIGHT)]

if opts.tablefile:
    f = gzip.open(opts.tablefile, "rb")
    update("Loading table...", [0])
    d = cPickle.load(f)
else:
    d = gencomplex(RESOLUTION)

if opts.filename:
    FILENAME = opts.filename

if opts.palette:
    PALETTE = []
    f = open(opts.palette, "r")
    for line in f:
        if len(PALETTE) > 255:
            break
        try:
            l = [i.strip() for i in line.split()]
            PALETTE.append((int(l[0]),int(l[1]),int(l[2])))
        except:
            continue

update("Preparing...", [0])

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
    if not len(d):
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
        if PALETTE:
            pix = PALETTE[pixels[i][j]]
        else:
            pix = (pixels[i][j] - 511, pixels[i][j] - 255, pixels[i][j])
#        out.putpixel((i,j), pix)

out.save(FILENAME)
update("Done!", [0])
