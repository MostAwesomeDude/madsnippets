# Copyright (C) 2014 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
from collections import defaultdict
import sys


def guess(s):
    """
    Make a map of possible letters, based on English word frequencies.
    """

    az = "abcdefghijklmnopqrstuvwxyz"
    t = "etaoinshrdlu"
    d = defaultdict(int)

    for c in s:
        c = c.lower()
        if c in az:
            d[c] += 1

    map = {}
    ks = zip(*sorted(d.items(), key=lambda item: -item[1]))[0]

    for i, k in enumerate(ks):
        if i >= len(t):
            break
        map[k] = t[i]

    return map


def trans(map, data):
    l = []
    for char in data:
        if char.isupper():
            l.append(map.get(char.lower(), char).upper())
        else:
            l.append(map.get(char, char))
    return "".join(l)


if __name__ == "__main__":
    t = sys.argv[1]
    f = sys.argv[2]
    data = open(sys.argv[3]).read()
    guesses = guess(data)
    map = dict(zip(t, f))
    guesses.update(map)
    print data
    print trans(guesses, data)
