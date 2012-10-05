#!/usr/bin/env python

from __future__ import division

from itertools import islice, takewhile
import os
import pickle
from random import SystemRandom
import time

r = SystemRandom()

widescreen = False

if widescreen:
    WIDTH = 1680
    HEIGHT = 1050

    MINW = -2.0
    MAXW = 2.0
    MINH = -1.6
    MAXH = 0.9
else:
    WIDTH = 1024
    HEIGHT = 768

    MINW = -1.6
    MAXW = 1.6
    MINH = -1.6
    MAXH = 0.8

WIDTH *= 2
HEIGHT *= 2

LOWER = 5
UPPER = 1000
PLOTGOAL = 500000

PLOTTED = object()
SKIPPED = object()

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
        if mini < c.real < maxi and minj < abs(c.imag) < maxj:
            return True

    return False

def bounded(c):
    return MINH < c.real < MAXH and MINW < c.imag < MAXW

def mma(old, new, weight=100):
    """
    Performs a Moving Modified Average, using the old value, new value,
    and a weight.

    Weight must be greater than zero.
    """

    return ((weight - 1) * old + new) / weight

def ibrot(c):
    """
    Yield a list of Mandelbrot numbers.

    Mandelbrot numbers are given by z = z**2 + c, where z starts at c and then
    iterates recursively.
    """

    z = c
    l = []

    for i in range(UPPER):
        z = z * z + c
        l.append(z)

    return l

def worker():
    bad = True

    while bad:
        c = complex(r.uniform(MINH, MAXH), r.uniform(MINW, MAXW))
        bad = checkrange(c)

    brots = list(islice(takewhile(bounded, ibrot(c)), UPPER))

    if LOWER > len(brots):
        return SKIPPED

    for z in brots:
        pixw = int((z.imag - MINW) * WIDTH/(MAXW-MINW))
        pixh = int((z.real - MINH) * HEIGHT/(MAXH-MINH))
        pixels[pixw][pixh] += 1

    return PLOTTED

print "Making pixel array..."

pixels = [[0 for j in xrange(HEIGHT)] for i in xrange(WIDTH)]

print "Getting started..."

t = time.time()

try:
    total, plotted, skipped = 0, 0, 0
    while plotted < PLOTGOAL:
        rv = worker()

        if rv is PLOTTED:
            plotted += 1
        elif rv is SKIPPED:
            skipped += 1

        total += 1
        if not total % 1000:
            elapsed = time.time() - t
            print ("Points (plotted/skipped/total): %d/%d/%d (%.2f/s)" %
                (plotted, skipped, total, plotted / elapsed))

except KeyboardInterrupt:
    print ("Total of %d points, skipped %d (%.2f%%) plotted %d (%.2f%%)" %
        (total, skipped, skipped*100/total, plotted, plotted*100/total))

elapsed = time.time() - t
print "Elapsed time: %.2fs (plotted %.2f/s)" % (elapsed, plotted/elapsed)

pickle_name_template = "buddha%04d.P"
pickle_name = ""

for i in xrange(1000):
    if not os.path.exists(pickle_name_template % i):
        pickle_name = pickle_name_template % i
        break

if pickle_name:
    print "Dumping..."
    pickle.dump(pixels, open(pickle_name, "w"))
    print "Done!"
else:
    print "Too many savefiles, couldn't dump."
