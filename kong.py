#!/usr/bin/env python
# Kongregate stats parser.

from __future__ import division

import collections
import datetime
import json
import sys
import urllib2

def acquire_json(name, d={}):
    if name not in d:
        print "Acquiring %s..." % name,
        handle = urllib2.urlopen("http://www.kongregate.com/%s.json" % name)
        data = handle.read()
        d[name] = json.loads(data)
        print "OK!"
    return d[name]

def print_percentage(label, current, total):
    """
    Pretty-print a percentage in a uniform manner.
    """

    print "- %s: %d of %d (%2.2f%%)" % (label, current, total,
        (current * 100) / total)

def stats(account_name):
    """
    Print badge statistics.
    """

    badge_json = acquire_json("badges")
    account_json = acquire_json("accounts/%s" % account_name)
    account_badges_json = acquire_json("accounts/%s/badges" % account_name)

    total_badges = BadgeDict(badge_json)
    user_badges = set(i["badge_id"] for i in account_badges_json)

    start_date = account_json["created_at"]
    then = datetime.datetime.strptime(start_date, "%Y-%m-%d %H:%M:%S")
    today = datetime.date.today()
    day_delta = today - then.date()

    print "-- copy below this line --"
    print today.strftime("%B %d, %Y")

    user_count = len(user_badges)
    total_count = len(total_badges)

    print_percentage("Acquired Badges", user_count, total_count)

    for difficulty in ("easy", "medium", "hard", "impossible"):
        difficulty_count = total_badges.count_by_difficulty()[difficulty]
        user_difficulty_count = sum(1 for chaff in
            total_badges.iter_by_difficulty(difficulty)
            if chaff["id"] in user_badges)

        print_percentage("%ss" % difficulty.capitalize(),
            user_difficulty_count, difficulty_count)

    total_points_from_badges = sum(badge["points"] for badge in
        total_badges.itervalues()
        if badge["id"] in user_badges)

    print_percentage("Points from Badges", total_points_from_badges,
        account_json["points"])

    print "- Average Points per Badge: %.2f" % \
        (total_points_from_badges / user_count)

    badges_per_day = user_count / day_delta.days

    print "- Average Badges per Day: %.2f" % badges_per_day

    days_remaining = (total_count - user_count) / badges_per_day

    print "- Estimated date of completion: %s" % \
        (today + datetime.timedelta(days_remaining)).strftime("%B %d, %Y")

    print "-- end of stats --"

class BadgeDict(dict):
    def __init__(self, iterable=[]):
        super(BadgeDict, self).__init__()

        for entry in iterable:
            self[entry["id"]] = entry

    def __getattr__(self, name):
        type, chaff, target = name.partition("_by_")
        if not target:
            raise AttributeError, "No target provided"

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
        else:
            raise AttributeError, "Unknown type %s" % type

        return getattr(self, name)


if len(sys.argv) < 3:
    print "Usage: %s <action> <account>" % sys.argv[0]
    sys.exit(1)

action = sys.argv[1]
account = sys.argv[2]

if action == "stats":
    stats(account)
else:
    print "Unknown command %s" % action
    sys.exit(1)
