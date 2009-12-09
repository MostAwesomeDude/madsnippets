#!/usr/bin/env python
# Kongregate stats parser.

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

def print_stats(user_badges, total_badges, start_date):
    """
    Print badge statistics.

    user_badges is an iterable of ints.
    total_badges is a `BadgeDict`.
    start_date is an ISO datetime.
    """

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

print "Acquiring %s.json... " % account,
account_json = urllib2.urlopen(
    "http://www.kongregate.com/accounts/%s.json" % account).read()
print "OK!"

print "Acquiring %s's badges.json... " % account,
account_badges_json = urllib2.urlopen(
    "http://www.kongregate.com/accounts/%s/badges.json" % account).read()
print "OK!"

badges = BadgeDict(json.loads(badge_json))
user_badges = set(i["badge_id"] for i in json.loads(account_badges_json))
user_join_date = json.loads(account_json)["created_at"]

print_stats(user_badges, badges, user_join_date)
