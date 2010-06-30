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
    if summary == None: summary = " "

    l = ["/usr/bin/notify-send", "-i", icon, "-h", "string:append:allowed", summary, message]
    print l
    subprocess.call(l)

parser = optparse.OptionParser()
parser.add_option("-e", "--event", dest="type", help="type of SKYPE_EVENT")
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

print options.type
        
# If event type x show notification (summary, body, icon)
# Summary should not be None
if options.type == 'SkypeLogin': notify("Skype","You have logged into Skype with {contact}".format(contact=options.sname),"skype")
#        elif options.type == 'SkypeLogout': notify("You have logged out of Skype",None,"user-offline")
elif options.type == 'SkypeLoginFailed': notify("Skype login failed",None,"user-offline")
#        elif options.type == 'CallConnecting': notify("Dailing... {contact}".format(contact=options.sname),None,"skype") #some of these should be merged and update to the same bubble: Call Connecting -> CallRingingOut -> Call Answered
elif options.type == 'CallRingingIn': notify(options.sname,"is calling you","skype")
#elif options.type == 'CallRingingOut': notify("Calling {contact}".format(contact=options.sname),"skype") #merge ^^ see above
#elif options.type == 'CallAnswered': notify("Call Answered",None,"skype")
elif options.type == 'VoicemailReceived': notify(options.sname,"Voicemail Received","skype")
elif options.type == 'VoicemailSent': notify("Voicemail Sent",None,"skype")
elif options.type == 'ContactOnline':
    notify(options.sname,"is online","skype")
    #self.addIndicator(options.type, options.sname, options.smessage)
elif options.type == 'ContactOffline': notify(options.sname,"is offline","skype")
elif options.type == 'ContactDeleted': notify("Contact Deleted", "{contact} has been deleted from your contact list".format(contact=options.sname),"skype")
elif options.type == 'ChatIncomingInitial':
    #notify(options.sname,options.smessage,"notification-message-IM")
    self.addIndicator(options.type, options.sname, options.smessage)
elif options.type == 'ChatIncoming':
    #notify(options.sname,options.smessage,"notification-message-IM")
    self.addIndicator(options.type, options.sname, options.smessage)
#elif options.type == 'ChatOutgoing': notify(options.sname,options.smessage,"notification-message-IM")
elif options.type == 'ChatJoined': notify("{contact} joined chat".format(contact=options.sname),options.smessage,"emblem-people")
elif options.type == 'ChatParted': notify("{contact} left chat".format(contact=options.sname),options.smessage,None)
elif options.type == 'TransferComplete': 
    notify("Transfer Complete","{path}/{filename}".format(filename=options.fname,path=options.fpath),"gtk-save")
    #TODO show dialog [ok] [open file] [reveal in folder]
    # or preview in gloobus :D if suitable format
elif options.type == 'TransferFailed': notify("Transfer Failed","{filename}".format(filename=options.fname),"error")
elif options.type == 'Birthday': notify(options.sname,"has a birthday Tomorrow","appointment-soon")
