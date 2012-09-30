#
# Intercept what would normally be a bad command.
#

import spell

from io import msg_print

def event_command(cmd):
	if (cmd == "y"):
                msg_print("Come, come stupid monster! You CAN'T kill me!")
                spell.aggravate_monsters(-1)
		return 1

	# Still unrecognized, throw "bad command" error
	return 0
