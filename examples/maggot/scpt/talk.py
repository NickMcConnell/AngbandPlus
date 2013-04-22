#
# Talk to a monster
#

import io
import monster
import player
import cave

import maggot

def talk():
	# Get a direction to talk in
	dir = io.get_rep_dir()

	if (dir == 0): return

	# Get location we are talking to
	p = player.get()
	delta = cave.dd(dir)
	loc = (delta[0] + p.py, delta[1] + p.px)
	m_idx = cave.get_m_idx(loc)

	# Whoops, empty space
	if (m_idx == 0):
		io.msg_print("You see no one there.")
		return

	# Get monster entry
	m = monster.get(m_idx)

	# Check for Maggot
	if (m.r_idx == 4):
		maggot.talk()

	# Monster doesn't understand talking
	else: io.msg_print("You get no response.")
