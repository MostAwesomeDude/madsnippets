#!/usr/bin/env python

from __future__ import division

import itertools
import random
import sys
import time

import Image

HEIGHT = 1050
WIDTH = 1680

MINH = -1.1
MAXH = 1.1
MINW = -2.2
MAXW = 1.1

COUNT = 50000
PLOTGOAL = 10000

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

def checkrange(c):
    # Borrowed from Evercat on Wikipedia

    for (mini, maxi, minj, maxj) in invalid_ranges:
        if mini < c.real <= maxi and minj < abs(c.imag) < maxj:
            return False

    return True

print "Making pixel array..."

pixels = [[0 for j in xrange(WIDTH)] for i in xrange(HEIGHT)]

print "Getting started..."

t = time.time()

try:
    total, plotted, skipped = 0, 0, 0
    while plotted < PLOTGOAL:
        c = complex(random.uniform(MINW, MAXW), random.uniform(MINH, MAXH))
        i = 0
        if not checkrange(c):
            total += 1
            skipped += 1
            continue
        z = c
        pixh, pixw = 0, 0
        while (i < COUNT and abs(c) <= 2 and
            0 <= pixh < HEIGHT and 0 <= pixw < WIDTH):

            z = z**2 + c

            pixh = (z.imag - MINH) * HEIGHT/(MAXH-MINH)
            pixw = (z.real - MINW) * WIDTH/(MAXW-MINW)
            i += 1

        if 10 < i < COUNT:
            plotted += 1
            z = c
            pixh, pixw = 0, 0
            i -= 1

            while i:
                z = z**2 + c
                pixh = int((z.imag - MINH) * HEIGHT/(MAXH-MINH))
                pixw = int((z.real - MINW) * WIDTH/(MAXW-MINW))
                pixels[pixh][pixw] += 1
                i -= 1

        total += 1
        if not total % 1000:
            print ("Points (plotted/skipped/total): %d/%d/%d" %
                (plotted, skipped, total))

except KeyboardInterrupt:
    print ("Total of %d points, skipped %d (%.2f%%) plotted %d (%.2f%%)" %
        (total, skipped, skipped*100/total, plotted, plotted*100/total))

elapsed = time.time() - t
print "Elapsed time: %.2fs (plotted %.2f/s)" % (elapsed, plotted/elapsed)

maxdepth = 0

print "Calculating max depth..."

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    if pixels[i][j] > maxdepth:
        maxdepth = pixels[i][j]

print "Max depth is %d" % maxdepth

out = Image.new("RGB",(HEIGHT,WIDTH))

print "Plotting..."

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

for (i, j) in itertools.product(xrange(HEIGHT), xrange(WIDTH)):
    value = pixels[i][j] / maxdepth
    out.putpixel((i,j), get_color(value))

out.save(FILENAME)
print "Done!"
