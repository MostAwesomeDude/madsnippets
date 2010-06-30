#!/usr/bin/env python
#    Python script to make Skype use notify-osd

# Copyright (c) 2009, Lightbreeze

# 2010 Corbin Simpson <MostAwesomeDude@gmail.com>
# Modification for notification-daemon compatibility

# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
This is a simple script for sending Skype events to notification-daemon.

To use this script, go into Skype's options menu (<Ctrl>+O) and select the
Notifications panel. Enable the Advanced View, and add the following script
line to execute on any event:
/path/to/my/copy/of/skype-notify-fdo.py -e"%type" -n"%sname" -f"%fname" -p"%fpath" -m"%smessage" -s%fsize -u%sskype
"""

import optparse
import subprocess
import sys

def notify(summary, message, icon):
    """Send a notification to notification-daemon."""

    l = ["/usr/bin/notify-send", "-i", icon, "-h", "string:append:allowed", summary, message]
    print l
    subprocess.call(l)

parser = optparse.OptionParser()
parser.add_option("-e", "--event", dest="event", help="type of SKYPE_EVENT")
parser.add_option("-n", "--sname", dest="sname",
    help="display name of contact")
parser.add_option("-u", "--skype", dest="sskype",
    help="skype-username of contact")
parser.add_option("-m", "--smessage", dest="smessage", help="message body",
    metavar="FILE")
parser.add_option("-p", "--path", dest="fpath", help="path to file")
parser.add_option("-s", "--size", dest="fsize", help="incoming file size")
parser.add_option("-f", "--filename", dest="fname", help="file name",
    metavar="FILE")

options = parser.parse_args()[0]

print vars(options)

# Arguments for dispatch dict.
# event: summary, body, icon
notifications = {
    "SkypeLogin":
        ("Skype", "You have logged into Skype with {sname}", "skype"),
    "SkypeLogout":
        ("You have logged out of Skype", "", "user-offline"),
    "SkypeLoginFailed":
        ("Skype login failed", "", "user-offline"),
    "CallConnecting":
        ("Dialing {sname}...", "", "skype"),
    "CallRingingIn":
        ("{sname}", "is calling you", "skype"),
    "CallRingingOut":
        ("Calling {sname}...", "", "skype"),
    "CallAnswered":
        ("Call answered", "", "skype"),
    "VoicemailReceived":
        ("{sname}", "Voicemail received", "skype"),
    "VoicemailSent":
        ("Voicemail sent", "", "skype"),
    "ContactOnline":
        ("{sname}", "is online", "skype"),
    "ContactOffline":
        ("{sname}", "is offline", "skype"),
    "ContactDeleted":
        ("Contact Deleted", "{sname} has been removed from your contacts", "skype"),
    "ChatIncomingInitial":
        ("{sname}", "{smessage}", "notification-message-IM"),
    "ChatIncoming":
        ("{sname}", "{smessage}", "notification-message-IM"),
    "ChatOutgoing":
        ("{sname}", "{smessage}", "notification-message-IM"),
    "ChatJoined":
        ("{sname} joined chat", "{smessage}", "emblem-people"),
    "ChatParted":
        ("{sname} left chat", "{smessage}", "skype"),
    "TransferComplete":
        ("Transfer Complete", "{fpath}/{fname}", "gtk-save"),
    "TransferComplete":
        ("Transfer Failed", "{fname}", "error"),
    "Birthday":
        ("{sname}", "has a birthday tomorrow", "appointment-soon"),
}

if options.event in notifications:
    summary, body, icon = notifications[options.event]
    # I apologize for this next line. ~ C.
    notify(summary.format(**vars(options)), body.format(**vars(options)), icon)
