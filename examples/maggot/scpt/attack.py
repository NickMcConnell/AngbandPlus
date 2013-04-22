import monster
import string
import io


#
# The player has bumped into a friendly monster.  Ask for attack
# verification.
#

def event_attack(m_idx):
	
	# Get monster
	m = monster.get(m_idx)

	# Make sure before attacking friendly monsters
	if (m.friendly >= 128):
		name = m.desc(4)
		return (not io.get_check("Really attack " + name + "? "))
	return 0


#
# The player has hurt a friendly monster.  Make it hostile.
#

def event_hurt(m_idx):

	# Get monster
	m = monster.get(m_idx)

	# Check for friendliness
	if (m.friendly >= 128):
		
		# Make it not-so-friendly
		m.friendly = 0

		# Message
		name = m.desc(4)
		words = string.split(name, ' ', 1)
		words[0] = string.capitalize(words[0])
		name = string.join(words, ' ')
		io.msg_print(name + " howls in anger!")
