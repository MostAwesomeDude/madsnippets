#!/usr/bin/env python
# Kongregate stats parser.

from __future__ import division

import collections
import json
import UserDict

import pprint

def print_percentage(label, current, total):
    """
    Pretty-print a percentage in a uniform manner.
    """

    print "%s: %d of %d (%2.2f%% complete)" % (label, current, total,
        (current * 100) / total)

def print_stats(user_badges, total_badges):
    """
    Print badge statistics.

    user_badges is an iterable of ints.
    total_badges is a `BadgeDict`.
    """

    user_count = len(user_badges)
    total_count = len(total_badges)

    # XXX replace with LC if it's faster, maybe
    user_imp_count, imp_count = 0, 0
    for imp in total_badges.iter_by_difficulty("impossible"):
        imp_count += 1
        if imp["id"] in user_badges:
            user_imp_count += 1

    print_percentage("Acquired Badges", user_count, total_count)
    print_percentage("Impossibles", user_imp_count, imp_count)

class BadgeDict(UserDict.IterableUserDict):
    def __init__(self, iterable=[]):
        # XXX IterableUserDict isn't new-style ( >&)
        UserDict.IterableUserDict.__init__(self)

        for entry in iterable:
            self[entry["id"]] = entry

    def count_by_difficulty(self):
        # XXX missing_method-style mangling maybe?
        d = collections.defaultdict(int)
        for entry in self.itervalues():
            d[entry["difficulty"]] += 1
        return d

    def iter_by_difficulty(self, difficulty):
        # XXX mm?
        for entry in self.itervalues():
            if entry["difficulty"] == difficulty:
                yield entry

badges = BadgeDict(json.load(file("badges.json", "r")))
madbadges = set(i["badge_id"] for i in
    json.load(file("MostAwesomeDude.json", "r")))

print_stats(madbadges, badges)
