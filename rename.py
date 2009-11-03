#!/usr/bin/env python

# Yet another CDS Python script. Use to rename an entire directory, with
# find 'n' replace.

# XXX add recursive dir support

import os
import os.path
import sys

def replace(f, old, new):
    handle = open(f, "rb")
    buf = handle.read()
    handle.close()
    buf = buf.replace(old, new).replace(old.upper(), new.upper()).replace(old.lower(), new.lower())
    handle = open(f, "wb")
    handle.write(buf)
    handle.close()

def rename(f, old, new):
    fnew = f.replace(old, new)
    if fnew != f:
        os.rename(f, fnew)

def shabam(f, old, new):
    if os.path.isfile(f):
        replace(f, old, new)
        rename(f, old, new)

[shabam(i, sys.argv[1], sys.argv[2]) for i in os.listdir(".")]

