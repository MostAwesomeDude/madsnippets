#!/usr/bin/env python
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
#
# Author: Corbin Simpson <cds@corbinsimpson.com>
#
# With inspiration from Scott Sanders <scott@jssjr.com>
#
# Read from Varnish's logs and send some statistics to Graphite.

from twisted.internet.defer import Deferred
from twisted.internet.endpoints import clientFromString
from twisted.internet.protocol import Factory, Protocol, ProcessProtocol
from twisted.internet.task import react


class GraphiteWriter(Protocol):
    def sendMetric(self, key, value, time):
        s = "%s %d %d\n" % (key, value, time)
        self.transport.write(s)
        # print key, value, time


class GraphiteFactory(Factory):
    protocol = GraphiteWriter


class VarnishScraper(ProcessProtocol):
    def __init__(self, target):
        self.target = target
        self.buf = ""

    def outReceived(self, data):
        # Fuck it, we'll do it quadratic.
        self.buf += data
        parts = self.buf.split('\n')
        self.buf = parts.pop()
        for part in parts:
            self.process(part)

    def process(self, line):
        host, handling, response, timestamp = line.split(' ')
        # Varnish uses dots for namespacing, so we'll use hyphens for domains.
        host = host.replace('.', '-')
        # Also remove the www. if it's there; we'd like to largely alias the
        # numbers for these domains.
        if host.startswith("www-"):
            host = host[4:]
        # Convert response from s to us. If we get a NaN, then abandon this
        # sample.
        try:
            response = int(float(response) * 1000000)
        except ValueError:
            return
        timestamp = int(timestamp)
        prefix = "varnish.hosts.%s.%s" % (host, handling)
        self.target.sendMetric(prefix + ".firstbyte", response, timestamp)

    def whenDone(self):
        self._done = Deferred()
        return self._done

    def connectionLost(self):
        self._done.callback(None)


def go(reactor):
    endpoint = clientFromString(reactor, "tcp:host=127.0.0.1:port=2003")

    d = endpoint.connect(GraphiteFactory())
    @d.addCallback
    def cb(writer):
        scraper = VarnishScraper(writer)
        fmt = "%{Host}i %{Varnish:handling}x %{Varnish:time_firstbyte}x %{%s}t"
        reactor.spawnProcess(scraper, "varnishncsa", ["varnishncsa", "-F",
            fmt])
        return scraper.whenDone()
    @d.addErrback
    def eb(err):
        return None

    return d


if __name__ == "__main__":
    react(go)
