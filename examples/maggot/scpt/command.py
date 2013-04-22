#
# Intercept what would normally be a bad command.
#
# The only intercepted command is 'O', which is used to converse with NPCs.
#

import talk

def event_command(cmd):

	# Intercept the talk command
	if (cmd == "O"):
		talk.talk()
		return 1

	# Still unrecognized, throw "bad command" error
	return 0
