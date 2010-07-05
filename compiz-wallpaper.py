#!/usr/bin/python

# compiz-wallpaper.py
# A very simple script to control wallpapers in compiz.
# (c) Corbin Simpson, 2008
# Redistributable under the terms of the GNU Public License
#
# v0.3.1 - Screen option; lets one specify which output/screen to modify.
#        - Fix possible bug with version detection on older Python.
#
# v0.3   - Switch to four-space tabs.
#        = Options! Now you can specify:
#          - Per-desktop mode
#          - Random mode
#          - Verbose mode
#          - Show current wallpapers
#
# v0.2   - Limit files included to just images that are loadable by Compiz.
#        - Clean up some code to make it more Pythonic.
#
# v0.1   - Initial release.

"""
Compiz Wallpaper Script
-----------------------

A simple piece of code I wrote to control the wallpapers in Compiz Fusion.
This takes a directory and randomly selects a handful of wallpapers to put on
each viewport.

Requirements
============

 * Python 2.3
 * libcompizconfig

Changelog
=========

 * v0.3.1
    * Screen option; lets one specify which output/screen to modify.
    * Fix possible bug with version detection on older Python.
 * v0.3
    * Switch to four-space tabs.
    * Options! Now you can specify:
       * Per-desktop mode
       * Random mode
       * Verbose mode
       * Show current wallpapers
 * v0.2
    * Limit files included to just images that are loadable by Compiz.
    * Clean up some code to make it more Pythonic.
 * v0.1
    * Initial release.
"""

import optparse
import os
import os.path
import random
import sys

import compizconfig

is_2_5 = (sys.version_info[1] >= 5)

suffixes = (".png", ".jpg", ".jpeg", ".svg", ".PNG", ".JPG", ".JPEG", ".SVG")

SCALE_N_CROP, SCALE, CENTER, TILE, CENTER_N_TILE = range(5)

parser = optparse.OptionParser(version="%prog 0.3.1")
parser.add_option("-c", "--screen", type="int", default=0, help="Screen (starts at 0)")
parser.add_option("-d", "--desktop", type="int", help="Desktop number (per-desktop mode)")
parser.add_option("-r", "--random", action="store_true",
                        dest="random", default=False, help="Random mode (per-directory)")
parser.add_option("-s", "--show", action="store_true",
                        dest="show", default=False, help="Show current wallpapers")
parser.add_option("-v", "--verbose", action="store_true",
                        dest="verbose", default=False, help="Give verbose output")
(options, args) = parser.parse_args()

def error(message):
    print "Error:", message
    sys.exit(1)

def info(message):
    if options.verbose:
        print "Info:", message

if (not options.random) and (not options.show) and (options.desktop == None):
    parser.error("Please specify an action. (-d, -r, -s)")

context = compizconfig.Context(plugins=['core','wallpaper'], basic_metadata=True)

if "wallpaper" not in context.Plugins:
    error("Couldn't find wallpaper plugin!")

plugin = context.Plugins['wallpaper']

if options.screen >= len(context.Plugins['core'].Screens):
    error("No such screen %d" + % options.screen)

core_settings = context.Plugins['core'].Screens[options.screen]

screen = plugin.Screens[options.screen]

viewports = core_settings['hsize'].Value * core_settings['vsize'].Value

if options.show:
    for pair in enumerate(screen['bg_image'].Value):
        print "Paper on desktop %d: %s" % pair

elif options.desktop != None:
    if len(args) < 1:
        error("No image specified.")
    elif options.desktop > viewports:
        error("Desktop "+str(options.desktop)+" doesn't exist!")

    info("Using desktop "+str(options.desktop)+"...")

    paper = os.path.abspath(args[0])

    if not os.path.exists(paper):
        error("Image"+paper+"not found!")

    # Very fun memory quirk here...
    # Memory must be copied, not just modified, in order to work.
    papers = screen['bg_image'].Value
    papers[options.desktop - 1] = paper
    screen['bg_image'].Value = papers

    info("Using image: "+paper)

    context.Write()

elif options.random:
    if len(args) > 0:
        working_dir = os.path.abspath(args[0])
    else:
        working_dir = os.getcwd()

    if not os.path.isdir(working_dir):
        error("You mistyped the directory you wanted...")

    info("Using %s as the image source directory..." % working_dir)

    direct = [os.path.join(working_dir, i) for i in os.listdir(working_dir)]
    direct.sort()

    # FIXME make this work for < 2.5
    if is_2_5:
        for paper in direct:
            if not paper.endswith(suffixes):
                info("Discarding bad wallpaper %s" % paper)
                direct.remove(paper)

    if len(direct) < viewports:
        error("Directory doesn't contain enough images!")

    bg_image_pos = [SCALE_N_CROP for i in range(viewports)]

    papers = random.sample(direct, viewports)

    screen['bg_image_pos'].Value = bg_image_pos
    screen['bg_image'].Value = papers

    for paper in papers:
        info("Using image: %s" % paper)

    context.Write()
