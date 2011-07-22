#!/usr/bin/env python

import os.path

import pygtk
pygtk.require("2.0")

import gtk

def expand(name, *args):
    prefix = "Fit%s" % name.capitalize()
    l = []
    m = [
        ("Core", prefix),
        ("Final Smash", "%sMotionEtc" % prefix),
        ("Original", "%s00" % prefix),
    ]
    for label, suffix in args:
        m.append((label, "%s%s" % (prefix, suffix)))
    for label, filename in m:
        l.append(("%s PAC" % label, "%s.pac" % filename))
        l.append(("%s PCS" % label, "%s.pcs" % filename))
    return l

fighters = [
    ["Captain Falcon", "captain", expand("captain",
        ("Blood Hawk", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("Pink", "05"),
        ),
    ],
    ["King Dedede", "dedede", expand("dedede",
        ("Green", "02"),
        ("White", "03"),
        ("Grayscale", "04"),
        ("Purple", "05"),
        ("Pink", "06"),
        ),
    ],
    ["Diddy Kong", "diddy", expand("diddy",
        ("Green", "02"),
        ("Blue", "03"),
        ("Pink", "04"),
        ("Yellow", "05"),
        ("Purple", "06"),
        ),
    ],
    ["Donkey Kong", "donkey", expand("donkey",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("White", "05"),
        ),
    ],
    ["Fox", "fox", expand("fox",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("White", "05"),
        ),
    ],
    ["Jigglypuff", "purin", expand("purin",
        ("Flower (red)", "01"),
        ("Nightcap (green)", "02"),
        ("Sun Hat (blue)", "03"),
        ("Trainer's Cap (white)", "04"),
        ),
    ],
    ["Kirby", "kirby", expand("kirby",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Yellow", "04"),
        ("White", "05"),
        ),
    ],
    ["Lucario", "lucario", expand("lucario",
        ("Red", "01"),
        ("Green", "02"),
        ("Light Blue", "04"),
        ("White", "05"),
        ),
    ],
    ["Luigi", "luigi", expand("luigi",
        ("Mario (red)", "01"),
        ("Blue (unused)", "02"),
        ("Blue", "03"),
        ("Fire (white)", "04"),
        ("Orange", "05"),
        ("Waluigi (purple)", "06"),
        ),
    ],
    ["Mario", "mario", expand("mario",
        ("Luigi (green)", "02"),
        ("Blue", "03"),
        ("Wario (yellow)", "04"),
        ("Dark", "05"),
        ("Fire (white)", "06"),
        ),
    ],
    ["Marth", "marth", expand("marth",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("White", "05"),
        ),
    ],
    ["Ness", "ness", expand("ness",
        ("Green", "02"),
        ("Blue", "03"),
        ("Bumblebee (yellow/black)", "04"),
        ("Fuel (white)", "05"),
        ("Mr. Saturn (black)", "06"),
        ),
    ],
    ["Peach", "peach", expand("peach",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("White", "04"),
        ("Daisy (yellow)", "05"),
        ),
    ],
    ["Pikachu", "pikachu", expand("pikachu",
        ("Trainer's Cap (red)", "01"),
        ("Bandana (green)", "02"),
        ("Goggles (blue)", "03"),
        ),
    ],
    ["Pit", "pit", expand("pit",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Gold", "04"),
        ("Black", "05"),
        ),
    ],
    ["Sheik", "sheik", expand("sheik",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("Ocarina of Time", "05"),
        ),
    ],
    ["Snake", "snake", expand("snake",
        ("Red/Black", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("White", "04"),
        ("Wild Feline", "05"),
        ),
    ],
    ["Wolf", "wolf", expand("wolf",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("Red & White", "05"),
        ),
    ],
    ["Yoshi", "yoshi", expand("yoshi",
        ("Red", "01"),
        ("Blue", "03"),
        ("Yellow", "04"),
        ("Pink", "05"),
        ("Aqua (light blue)", "06"),
        ),
    ],
    ["Zelda", "zelda", expand("zelda",
        ("Red", "01"),
        ("Green", "02"),
        ("Blue", "03"),
        ("Black", "04"),
        ("Ocarina of Time", "05"),
        ),
    ],
]

paths = fighters

shim = "private/wii/app/RSBE/pf/fighter"

class Brawl(object):

    def __init__(self):
        self.gui = gtk.Builder()
        self.gui.add_from_file("brawl.glade")

        self.gui.connect_signals(self)

        self.populate()

    def on_window_destroy(self, widget):
        gtk.main_quit()

    def on_sd_card_file_set(self, widget):
        folder = widget.get_filename()
        self.examine_folder(folder)

    def on_refresh_clicked(self, widget):
        folder = self.gui.get_object("sd_card").get_filename()
        self.examine_folder(folder)

    on_sd_card_current_folder_changed = on_sd_card_file_set

    def on_paths_view_row_activated(self, treeview, path, view_column):
        if treeview.row_expanded(path):
            treeview.collapse_row(path)
        else:
            treeview.expand_row(path, False)

    def on_paths_view_row_expanded(self, treeview, it, path):
        def f(tv, p, user_data):
            if any(p != q for (p, q) in zip(p, path)):
                tv.collapse_row(p)
        treeview.map_expanded_rows(f, None)

    def populate(self):
        root = self.gui.get_object("paths")

        def recurse(p, it, parent):
            for t in p:
                desc, path = t[:2]
                if parent:
                    path = os.path.join(parent, path)
                itt = root.append(it, [desc, path, False])
                if len(t) == 3:
                    recurse(t[2], itt, path)

        recurse(paths, None, None)

    def examine_folder(self, folder):
        def foreach(model, path, it, user_data):
            relative, = model.get(it, 1)
            absolute = os.path.join(folder, shim, relative)
            whether = os.path.exists(absolute)
            model.set_value(it, 2, whether)

        self.gui.get_object("paths").foreach(foreach, None)

def main():
    b = Brawl()
    gtk.main()

if __name__ == "__main__":
    main()
