#!/usr/bin/env python

from collections import defaultdict
import sys

if len(sys.argv) < 2:
    sys.exit(0)

needle = "".join(sorted(sys.argv[1].lower()))

haystack = defaultdict(list)

with open("/usr/share/dict/words") as f:
    for word in f:
        word = word.strip()
        scrambled = "".join(sorted(word.lower()))
        haystack[scrambled].append(word)

print "Found these:", haystack[needle]
