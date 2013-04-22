#
# Control Farmer Maggot
#
# Farmer Maggot has some rudimentary conversation skills, and he will
# also give the player mushrooms.
#

import io
import object
import string
import player

#
# Delete the characters listed in "chars" from the string "s".
#
# This is useful for stripping punctuation from user input.
#

def delchars(s, chars):
	table = string.maketrans(chars, " " * len(chars))
	return string.translate(s, table)


#
# Maggot is giving the player a mushroom
#

def give_mushroom():
	mushrooms = { "poison" : 5, "blindness" : 6, "paranoia" : 7,
	              "confusion" : 8, "constitution" : 11, "restoring" : 12,
		      "strength" : 18, "wounds" : 20, "con" : 11, "str" : 18,
		      "unhealth" : 10, "disease" : 19, "serious" : 20,
		      "restoration" : 12 }

	# Ask for type
	input = io.get_string("What kind of mushroom would you like? ")
	input = delchars(input, ",.?!")
	words = string.split(string.lower(input))

	type = 0

	# Try finding one of the words in the dictionary above
	for i in words:
		if mushrooms.has_key(i):
			type = mushrooms[i]

	# Not found
	if not type:
		io.msg_print("Sorry, I don't have any of those on hand.")
		return 0

	# Create a new mushroom of that type
	mush = object.new(type)
	io.msg_print("Farmer Maggot gives you " + mush.desc() + ".")

	# And give it to the player
	mush.carry()

	return 1


#
# The player is talking to Maggot
#

def talk():
	conversation = { "yes" : 1, "aye" : 1, "yep" : 1, "y" : 1,
			 "no" : 2, "nay" : 2, "nope" : 2, "n" : 2
	               }	

	# Get a nickname based on player sex
	p = player.get()
	if (p.psex):
		nick = "sonny"
	else:
		nick = "missy"

	io.msg_print("You see Farmer Maggot.")
	io.msg_print("He says, 'Hello there, " + nick + "!  Would you like a mushroom?'")

	while 1:
		input = io.get_string("You say: ")
		input = delchars(input, ",.?!")
		words = string.split(string.lower(input))

		response = 0

		# Try finding response index
		for i in words:
			if conversation.has_key(i):
				response = conversation[i]

		if not response:
			io.msg_print("'Sorry, I don't understand.'")
			continue

		# Answer is 'yes'
		if response == 1:
			give_mushroom()
			return

		# Answer is 'no'
		elif response == 2:
			io.msg_print("'Very well.  Have a nice day.'")
			return
