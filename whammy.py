#!/usr/bin/env python

from __future__ import division

import subprocess

import pygtk
pygtk.require("2.0")
import gtk
import gtk.glade

def program_change(preset, enabled=True):
    if not enabled:
        preset += 17
    return "C0%02x" % preset

def pitch_control(pitch):
    return "B00B%02x" % pitch

def clamp(value):
    return max(0, min(127, int(round(value))))

def hsteps_to_ratio(hsteps):
    return (2 ** (hsteps / 12)) - 1

def octave_up_preamble():
    return program_change(3)

def two_octaves_up_preamble():
    return program_change(2)

def octave_up(hsteps):
    return clamp(hsteps_to_ratio(hsteps) * 127)

def two_octaves_up(hsteps):
    return clamp(hsteps_to_ratio(hsteps) * (127/3))

def prebend(hsteps, force_double=False):
    if hsteps > 12 or force_double:
        return two_octaves_up_preamble() + \
            pitch_control(two_octaves_up(hsteps))
    else:
        return octave_up_preamble() + \
            pitch_control(octave_up(hsteps))

def bend_up_to_pitch(hsteps, speed, force_double=False):
    if hsteps > 12 or force_double:
        l = [pitch_control(i)*speed for i in range(two_octaves_up(hsteps))]
        return two_octaves_up_preamble() + "".join(l)
    else:
        l = [pitch_control(i)*speed for i in range(octave_up(hsteps))]
        return octave_up_preamble() + "".join(l)

def bend_release_from_pitch(hsteps, speed, force_double=False):
    if hsteps > 12 or force_double:
        l = [pitch_control(i)*speed for i
            in range(two_octaves_up(hsteps), 0, -1)]
    else:
        l = [pitch_control(i)*speed for i
            in range(octave_up(hsteps), 0, -1)]
    return "".join(l)

def make_starburst():
    l = []
    j = 0
    for i in xrange(18):
        l.append(program_change(j))
        j = (j + 5) % 17
    return l

class Whammy(object):

    def discover(self):
        proc = subprocess.Popen(["amidi", "-l"], stdout=subprocess.PIPE)
        info = proc.communicate()[0]
        for i in info.split():
            if i.startswith("hw"):
                self.port = i
                break

    def __init__(self):
        self.discover()

    def fire(self, payload):
        subprocess.call(["amidi", "-p", self.port, "-S", payload])

    def fire_starburst(self):
        [self.fire(i) for i in make_starburst()]

    def fire_reset(self, value=0):
        self.fire("B00B00")

class WhammyGUI(object):

    def delete_event(self, widget, event, data=None):
        return False

    def destroy(self, widget, data=None):
        gtk.main_quit()

    def key_press(self, widget, event, data=None):
        # Bend up
        if event.string == "g":
            self.whammy.fire(bend_up_to_pitch(1, 24))
        elif event.string == "h":
            self.whammy.fire(bend_up_to_pitch(2, 12))
        elif event.string == "j":
            self.whammy.fire(bend_up_to_pitch(3, 8))
        elif event.string == "k":
            self.whammy.fire(bend_up_to_pitch(4, 6))
        elif event.string == "l":
            self.whammy.fire(bend_up_to_pitch(7, 3))
        elif event.string == ";":
            self.whammy.fire(bend_up_to_pitch(10, 2))
        elif event.string == "'":
            self.whammy.fire(bend_up_to_pitch(12, 2))
        # Prebend up
        elif event.string == "H":
            self.whammy.fire(prebend(2))
        elif event.string == "J":
            self.whammy.fire(prebend(3))
        elif event.string == "K":
            self.whammy.fire(prebend(4))
        else:
            print "press %s" % event.string
            return False
        return True

    def key_release(self, widget, event, data=None):
        # Snap-release bends
        if event.string in "ghjkl;'":
            self.whammy.fire_reset()
        # Prebend release
        elif event.string == "H":
            self.whammy.fire(bend_release_from_pitch(2, 120))
        elif event.string == "J":
            self.whammy.fire(bend_release_from_pitch(3, 80))
        elif event.string == "K":
            self.whammy.fire(bend_release_from_pitch(4, 60))
        else:
            print "release %s" % event.string
            return False
        return True

    def __init__(self):
        self.glade = gtk.glade.XML("whammy.glade")
        self.window = self.glade.get_widget("window")
        self.window.show()

        self.window.connect("delete_event", self.delete_event)
        self.window.connect("destroy", self.destroy)
        self.window.connect("key_press_event", self.key_press)
        self.window.connect("key_release_event", self.key_release)

        self.whammy = Whammy()
        self.whammy.fire_starburst()

    def run(self):
        gtk.main()

WhammyGUI().run()
