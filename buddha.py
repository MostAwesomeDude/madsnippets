#!/usr/bin/env python

from __future__ import division

import itertools
import pickle
import random
import sys
import time

widescreen = False

if widescreen:
    HEIGHT = 1050
    WIDTH = 1680

    MINH = -1.0
    MAXH = 1.0
    MINW = -2.0
    MAXW = 1.2
else:
    HEIGHT = 768
    WIDTH = 1024

    MINH = -1.0
    MAXH = 1.0
    MINW = -2.0
    MAXW = 0.66

HEIGHT *= 2
WIDTH *= 2

COUNT = 20000
PLOTGOAL = 100000

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

def mma(old, new, weight=100):
    """
    Performs a Moving Modified Average, using the old value, new value,
    and a weight.

    Weight must be greater than zero.
    """

    return ((weight - 1) * old + new) / weight

print "Making pixel array..."

pixels = [[0 for j in xrange(WIDTH)] for i in xrange(HEIGHT)]

print "Getting started..."

t = time.time()

try:
    total, plotted, skipped, avg_iters = 0, 0, 0, 0
    while plotted < PLOTGOAL:
        c = complex(random.uniform(MINW, MAXW), random.uniform(MINH, MAXH))
        i = 0
        if not checkrange(c):
            total += 1
            skipped += 1
            continue
        z = c
        while MINH < z.imag < MAXH and MINW < z.real < MAXW and i < COUNT:
            z = z**2 + c
            i += 1

        if i < 10 or i == COUNT:
            continue

        avg_iters = mma(avg_iters, i)

        plotted += 1
        z = c**2 + c
        pixh = int((z.imag - MINH) * HEIGHT/(MAXH-MINH))
        pixw = int((z.real - MINW) * WIDTH/(MAXW-MINW))

        while i and (0 <= pixh < HEIGHT) and (0 <= pixw < WIDTH):
            z = z**2 + c
            pixels[pixh][pixw] += 1
            pixh = int((z.imag - MINH) * HEIGHT/(MAXH-MINH))
            pixw = int((z.real - MINW) * WIDTH/(MAXW-MINW))
            i -= 1

        total += 1
        if not total % 1000:
            print ("Points (plotted/skipped/total): %d/%d/%d Avg. iters. %d" %
                (plotted, skipped, total, avg_iters))

except KeyboardInterrupt:
    print ("Total of %d points, skipped %d (%.2f%%) plotted %d (%.2f%%)" %
        (total, skipped, skipped*100/total, plotted, plotted*100/total))

elapsed = time.time() - t
print "Elapsed time: %.2fs (plotted %.2f/s)" % (elapsed, plotted/elapsed)

print "Dumping..."

pickle.dump(pixels, open("buddha.P", "w"))

print "Done!"
