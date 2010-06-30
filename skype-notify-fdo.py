#!/usr/bin/env python
#    Python script to make Skype use notify-osd

# Copyright (c) 2009, Lightbreeze

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
#
#
# to use this script: Open Skype -> Open the menu and press 'Options' or press Ctrl-O
# -> hit the 'Advanced' button and check 'Execute the following script on _any_ event'
# -> paste: python /path/to/skype-notify.py -e"%type" -n"%sname" -f"%fname" -p"%fpath" -m"%smessage" -s%fsize -u%sskype
# -> disable or enable the notifications you want to receive.

# Hax'd for FDO notify-send compatibility by Corbin Simpson <MostAwesomeDude@gmail.com>
# (Public domain, duh :3 )

# TODO: add sound if it is not synchronising; add indicator-applet presence; add buddy photo in IM
import sys
from optparse import OptionParser

import subprocess

class NotifyForSkype:
    def __init__(self):
        # Add argument parser options
        parser = OptionParser()
        parser.add_option("-e", "--event", dest="type", help="type of SKYPE_EVENT")
        parser.add_option("-n", "--sname", dest="sname", help="display-name of contact")
        parser.add_option("-u", "--skype", dest="sskype", help="skype-username of contact")
        parser.add_option("-m", "--smessage", dest="smessage", help="message body", metavar="FILE")
        parser.add_option("-p", "--path", dest="fpath", help="path to file")
        parser.add_option("-s", "--size", dest="fsize", help="incoming file size")
        parser.add_option("-f", "--filename", dest="fname", help="file name", metavar="FILE")
        (o, args) = parser.parse_args()

        print(args)
        print(sys.argv)
        print(o.type)
        
        # If event type x show notification (summary, body, icon)
        # Summary should not be None
        if o.type == 'SkypeLogin': self.showNotification("Skype","You have logged into Skype with {contact}".format(contact=o.sname),"skype")
#        elif o.type == 'SkypeLogout': self.showNotification("You have logged out of Skype",None,"user-offline")
        elif o.type == 'SkypeLoginFailed': self.showNotification("Skype login failed",None,"user-offline")
#        elif o.type == 'CallConnecting': self.showNotification("Dailing... {contact}".format(contact=o.sname),None,"skype") #some of these should be merged and update to the same bubble: Call Connecting -> CallRingingOut -> Call Answered
        elif o.type == 'CallRingingIn': self.showNotification(o.sname,"is calling you","skype")
        #elif o.type == 'CallRingingOut': self.showNotification("Calling {contact}".format(contact=o.sname),"skype") #merge ^^ see above
        #elif o.type == 'CallAnswered': self.showNotification("Call Answered",None,"skype")
        elif o.type == 'VoicemailReceived': self.showNotification(o.sname,"Voicemail Received","skype")
        elif o.type == 'VoicemailSent': self.showNotification("Voicemail Sent",None,"skype")
        elif o.type == 'ContactOnline':
            self.showNotification(o.sname,"is online","skype")
            #self.addIndicator(o.type, o.sname, o.smessage)
        elif o.type == 'ContactOffline': self.showNotification(o.sname,"is offline","skype")
        elif o.type == 'ContactDeleted': self.showNotification("Contact Deleted", "{contact} has been deleted from your contact list".format(contact=o.sname),"skype")
        elif o.type == 'ChatIncomingInitial':
            #self.showNotification(o.sname,o.smessage,"notification-message-IM")
            self.addIndicator(o.type, o.sname, o.smessage)
        elif o.type == 'ChatIncoming':
            #self.showNotification(o.sname,o.smessage,"notification-message-IM")
            self.addIndicator(o.type, o.sname, o.smessage)
        #elif o.type == 'ChatOutgoing': self.showNotification(o.sname,o.smessage,"notification-message-IM")
        elif o.type == 'ChatJoined': self.showNotification("{contact} joined chat".format(contact=o.sname),o.smessage,"emblem-people")
        elif o.type == 'ChatParted': self.showNotification("{contact} left chat".format(contact=o.sname),o.smessage,None)
        elif o.type == 'TransferComplete': 
            self.showNotification("Transfer Complete","{path}/{filename}".format(filename=o.fname,path=o.fpath),"gtk-save")
            #TODO show dialog [ok] [open file] [reveal in folder]
            # or preview in gloobus :D if suitable format
        elif o.type == 'TransferFailed': self.showNotification("Transfer Failed","{filename}".format(filename=o.fname),"error")
        elif o.type == 'Birthday': self.showNotification(o.sname,"has a birthday Tomorrow","appointment-soon")

    def showNotification(self, summary, message, ikon):
        '''takes a title summary a message, and an icon to display the notification. Returns the created notification object'''
        if summary == None: summary = " "

        l = ["/usr/bin/notify-send", "-i", ikon, "-h", "string:append:allowed", summary, message]
        print l
        subprocess.call(l)

        print("showNotification..")

cm = NotifyForSkype()
sys.exit(-1) #hack TODO make sure this program quits properly
