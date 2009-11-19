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

    print_percentage("Acquired Badges", user_count, total_count)

    imp_count = total_badges.count_by_difficulty()["impossible"]
    user_imp_count = sum(1 for imp in
        total_badges.iter_by_difficulty("impossible")
        if imp["id"] in user_badges)

    print_percentage("Impossibles", user_imp_count, imp_count)

class BadgeDict(dict):
    def __init__(self, iterable=[]):
        super(BadgeDict, self).__init__()

        for entry in iterable:
            self[entry["id"]] = entry

    def __getattr__(self, name):
        type, chaff, target = name.partition("_by_")
        if not target:
            raise AttributeError

        if type == "count":
            def f(self):
                d = collections.defaultdict(int)
                for entry in self.itervalues():
                    d[entry[target]] += 1
                return d

            setattr(self.__class__, name, f)
        elif type == "iter":
            def f(self, filter):
                for entry in self.itervalues():
                    if entry[target] == filter:
                        yield entry
            setattr(self.__class__, name, f)

        return getattr(self, name)

badges = BadgeDict(json.load(file("badges.json", "r")))
madbadges = set(i["badge_id"] for i in
    json.load(file("MostAwesomeDude.json", "r")))

print_stats(madbadges, badges)

# XXX debugging
for i in badges.itervalues():
    pprint.pprint(i)
    break
