#!/usr/bin/env python
# Kongregate stats parser.
# TODO:
# ~ Get user's signup date, for calculating badges/day and completion date

from __future__ import division

import collections
import datetime
import json
import sys
import urllib2

def print_percentage(label, current, total):
    """
    Pretty-print a percentage in a uniform manner.
    """

    print "- %s: %d of %d (%2.2f%% complete)" % (label, current, total,
        (current * 100) / total)

def print_stats(user_badges, total_badges):
    """
    Print badge statistics.

    user_badges is an iterable of ints.
    total_badges is a `BadgeDict`.
    """

    print "-- copy below this line --"
    print datetime.date.today().strftime("%B %d, %Y")

    user_count = len(user_badges)
    total_count = len(total_badges)

    print_percentage("Acquired Badges", user_count, total_count)

    imp_count = total_badges.count_by_difficulty()["impossible"]
    user_imp_count = sum(1 for imp in
        total_badges.iter_by_difficulty("impossible")
        if imp["id"] in user_badges)

    print_percentage("Impossibles", user_imp_count, imp_count)

    total_points_from_badges = sum(badge["points"] for badge in
        total_badges.itervalues()
        if badge["id"] in user_badges)

    print "- Average Points per Badge: %2.2f" % \
        (total_points_from_badges / user_count)
    print "-- end of stats --"

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


if len(sys.argv) < 2:
    print "Usage: %s <account>" % sys.argv[0]
    sys.exit(1)

account = sys.argv[1]

print "Acquiring badges.json... ",
badge_json = urllib2.urlopen("http://www.kongregate.com/badges.json").read()
print "OK!"

print "Acquiring %s's badges.json... " % account,
account_json = urllib2.urlopen(
    "http://www.kongregate.com/accounts/%s/badges.json" % account).read()
print "OK!"

badges = BadgeDict(json.loads(badge_json))
user_badges = set(i["badge_id"] for i in json.loads(account_json))

print_stats(user_badges, badges)
