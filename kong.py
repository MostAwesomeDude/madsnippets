#!/usr/bin/env python
# Kongregate stats parser.

from __future__ import division

import json
import UserDict

def print_stats(user_count, total_count):
    print "Acquired Badges: %d of %d (%2.2f%% complete)" % (
        user_count, total_count, (user_count * 100) / total_count)

class BadgeDict(UserDict.IterableUserDict):
    def __init__(self, iterable=[]):
        # XXX IterableUserDict isn't new-style ( >&)
        UserDict.IterableUserDict.__init__(self)

        for entry in iterable:
            if "id" in entry:
                self[entry["id"]] = entry
            elif "badge_id" in entry:
                self[entry["badge_id"]] = entry

badges = BadgeDict(json.load(file("badges.json", "r")))
madbadges = BadgeDict(json.load(file("MostAwesomeDude.json", "r")))

print_stats(len(madbadges), len(badges))
