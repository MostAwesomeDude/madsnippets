#!/usr/bin/env python

import sys

if len(sys.argv) < 2:
    print "Usage: %s <filename>" % sys.argv[0]
    sys.exit(1)

with open(sys.argv[1], "wb") as f:
    for chunk in sys.stdin.read():
        f.write(chunk)
        sys.stdout.write(chunk)
