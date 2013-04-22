# The 'pickle' module lets us store Python objects in a file

import pickle

# The 'io' module lets us use stuff like 'msg_print'

import io

# The 'event' module lets us handle game events

import event


#
# Save some script-specific info to the savefile
#

def event_save(fff):

	# Store script name
	pickle.dump(scpt_name, fff)

	# Store the number of things we'll be saving
	pickle.dump(0, fff)

	# Success
	return 0


#
# Load the script-specific info from the savefile
#

def event_load(fff):
	global scpt_name
	global vars

	# Look for the script name
	s = pickle.load(fff)

	# Make sure we can handle it
	if (s != scpt_name):
		t = "Bad savefile (can't handle savefiles from script '" + s + "')."
		io.msg_print(t)
		return 1

	# Load number of stored objects
	n = pickle.load(fff)

	# Make sure there's not too many
	if (n > 0):
		t = "Bad savefile (too many stored objects)."
		io.msg_print(t)
		return 1

	# Success
	return 0



#
# This code is run every time the game is started.
#
# Here we associate some events to functions
#

scpt_name = "PAngband default script"

event.add_handler(event.EVENT_SAVE, event_save)
event.add_handler(event.EVENT_LOAD, event_load)
