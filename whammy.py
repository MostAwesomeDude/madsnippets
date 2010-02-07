#!/usr/bin/env python

from __future__ import division

import subprocess

import pygtk
pygtk.require("2.0")
import gtk
import gtk.glade

import pypm
pypm.Initialize()

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
        l = [pitch_control(i) for i in range(two_octaves_up(hsteps))]
        return [two_octaves_up_preamble()] + l
    else:
        l = [pitch_control(i) for i in range(octave_up(hsteps))]
        return [octave_up_preamble()] + l

def bend_release_from_pitch(hsteps, speed, force_double=False):
    if hsteps > 12 or force_double:
        l = [pitch_control(i)*speed for i
            in range(two_octaves_up(hsteps), 0, -1)]
    else:
        l = [pitch_control(i)*speed for i
            in range(octave_up(hsteps), 0, -1)]
    return l

def make_starburst():
    l = []
    j = 0
    for i in xrange(18):
        l.append(program_change(j))
        j = (j + 5) % 17
    return l

class Whammy(object):

    def __init__(self):
        self.out = pypm.Output(2)

    def fire(self, payload):
        assert type(payload) == str
        bytes = []
        while payload:
            bytes.append(int(payload[:2], 16))
            payload = payload[2:]
        assert len(bytes) <= 4

        self.out.Write([[bytes, pypm.Time()]])

    def fire_multiple(self, payloads):
        assert type(payloads) == list
        [self.fire(i) for i in payloads]

    def fire_starburst(self):
        self.fire_multiple(make_starburst())

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
            self.pitchup.set_fraction(1/24)
            self.whammy.fire_multiple(bend_up_to_pitch(1, 24))
        elif event.string == "h":
            self.pitchup.set_fraction(2/24)
            self.whammy.fire_multiple(bend_up_to_pitch(2, 12))
        elif event.string == "j":
            self.pitchup.set_fraction(3/24)
            self.whammy.fire_multiple(bend_up_to_pitch(3, 8))
        elif event.string == "k":
            self.pitchup.set_fraction(4/24)
            self.whammy.fire_multiple(bend_up_to_pitch(4, 6))
        elif event.string == "l":
            self.pitchup.set_fraction(7/24)
            self.whammy.fire_multiple(bend_up_to_pitch(7, 3))
        elif event.string == ";":
            self.pitchup.set_fraction(10/24)
            self.whammy.fire_multiple(bend_up_to_pitch(10, 2))
        elif event.string == "'":
            self.pitchup.set_fraction(12/24)
            self.whammy.fire_multiple(bend_up_to_pitch(12, 2))
        # Prebend up
        elif event.string == "H":
            self.whammy.fire_multiple(prebend(2))
        elif event.string == "J":
            self.whammy.fire_multiple(prebend(3))
        elif event.string == "K":
            self.whammy.fire_multiple(prebend(4))
        else:
            print "press %s" % event.string
            return False
        return True

    def key_release(self, widget, event, data=None):
        # Snap-release bends
        if event.string in "ghjkl;'":
            self.pitchup.set_fraction(0)
            self.whammy.fire_reset()
        # Prebend release
        elif event.string == "H":
            self.whammy.fire_multiple(bend_release_from_pitch(2, 120))
        elif event.string == "J":
            self.whammy.fire_multiple(bend_release_from_pitch(3, 80))
        elif event.string == "K":
            self.whammy.fire_multiple(bend_release_from_pitch(4, 60))
        else:
            print "release %s" % event.string
            return False
        return True

    def __init__(self):
        self.glade = gtk.glade.XML("whammy.glade")
        self.window = self.glade.get_widget("window")

        self.pitchup = self.glade.get_widget("pitchup")
        self.pitchdown = self.glade.get_widget("pitchdown")

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
