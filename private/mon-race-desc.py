#!/usr/bin/env python

"""
mon_race_desc version 0.9.9a1
Copyright (c) 2004-2005 Alexander Ulyanov

Usage:
	$ ./mon_race_desc n > output
Where n is race number from lib/edit/p_race.txt. This script parses
various files to create the spoiler for the progression of
chosen race.

Notes:
 - This script was created and tested on Linux system with Python 2.2.
   It SHOULD work anywhere, but I can't guarantee this.
 - This script parses following files:
	lib/edit/p_race.txt
	lib/edit/monster.txt
	src/powers_strings.inc
	src/powers_titles.inc
 - This script should be used by maintainer only. Check
   lib/help/monrace.txt if you're not me :).
 - This script implies that all the files are correct, that lines
   in monster.txt are in correct order (N<B<L<A<M<O<P) and that the init1.c
   and tables.c files have r_info_pwr[] and pwr_desc[] arrays respectively.
 - Admittedly this is very crude and hacky code. I do not know Python well.
 - Hand editing may be needed if the progression is not a tree
   (e.g. Qs). Duplicating entries will need to be removed
"""

# Print the given item and its children
def print_progression(prog_ind, prog, depth, lines):
	if depth == 0:
		print prog[prog_ind][0]
	else:
		for i in range(depth-1):
			if i in lines:
				print " |  ",
			else:
				print "    ",
		if depth-1 in lines:
			print " |->", prog[prog_ind][0]
		else:
			print " \->", prog[prog_ind][0]
	newprog = []
	for i in range(len(prog)):
		if prog[i][2] == prog_ind:
			newprog.append([i, prog, depth + 1, lines])
	for i in range(len(newprog)):
		if i == len(newprog) - 1:
			print_progression(newprog[i][0], newprog[i][1], newprog[i][2], newprog[i][3])
		else:
			print_progression(newprog[i][0], newprog[i][1], newprog[i][2], newprog[i][3] + [depth])

import sys
import string

if len(sys.argv) == 1:
	print "Usage: mon_race_desc <race_number>"
	sys.exit(-1)

p_race = int(sys.argv[1])
prog_tree = []

# Open lib/edit/p_race.txt, read some basic data
p_race_txt = file("lib/edit/p_race.txt", "r")
while 1:
	line = p_race_txt.readline()
	if line == "":
		print "Error: race", p_race, "not found in p_race.txt file"
		sys.exit(-1)
	if line[0:2] == "N:":
		n = int(line[2:line.find(":", 2)])
		if n == p_race:
			p_race_name = line[line.rfind(":")+1:-1]
			print p_race_name, "progression:"
			print
			while 1:
				line = p_race_txt.readline()
				if line == "" or line[0:2] == "N:":
					print "Error: race", p_race, "is not a monster race"
					sys.exit(-1)
				if line[0:2] == "M:":
					prog_tree.append(["", int(line[2:]), -1])
					break
			break

p_race_txt.close()

# Build a full progression tree
monster_txt = file("lib/edit/monster.txt", "r")
r_info = monster_txt.readlines()
monster_txt.close()

pending = [[prog_tree[0][1], 0]]

while len(pending):
	# Find a "N:" entry
	i = r_info[0]
	j = 0
	while not (i[0:2] == "N:" and pending[0][0] == int(i[2:i.find(":", 2)])):
		j = j + 1
		i = r_info[j]
	prog_tree[pending[0][1]][0] = i[i.rfind(":")+1:-1]
	# Proceed to "L:"
	while not (i[0:2] == "L:"):
		j = j + 1
		i = r_info[j]
	k = 1
	m = 0
	l_entry = []
	while i.find(":", k) != -1:
		nextk = i.find(":", k+1)
		if nextk == -1:
			n = int(i[k+1:])
		else:
			n = int(i[k+1:nextk])
		if m > 1 and n != 0:
			l_entry.append(n)
		k = nextk
		m = m + 1
	for i in l_entry:
		prog_tree.append(["", i, pending[0][1]])
		pending.append([i, len(prog_tree)-1])
	del pending[0]

# Print the tree
print_progression(0, prog_tree, 0, [])
print

# Pre-construct power names mapping

pwrnames = {}
# Open source files
# Changed in Pos 0.3.0 -- the relevant arrays are now moved to
# auto-generated .inc files.
init1_c = file("src/powers_strings.inc", "r")
tables_c = file("src/powers_titles.inc", "r")
# Seek to r_info_pwr[] array
s = "zZ"
while s != "static cptr r_info_pwr[] =\n" and s != "":
	s = init1_c.readline()
# Seek two more lines (three now)
s = init1_c.readline()
s = init1_c.readline()
s = init1_c.readline()
# Seek to pwr_desc[] array
t = "zZ"
while t != "cptr pwr_desc[] =\n" and t != "":
	t = tables_c.readline()
# Seek three more lines
t = tables_c.readline()
t = tables_c.readline()
t = tables_c.readline()
# Build the mappings
while s.find("NULL") == -1:
	# Strip the lines
	while not s[0].isalnum():
		s = s[1:]
	while not s[-1].isalnum():
		s = s[:-1]
	while not t[0].isalnum():
		t = t[1:]
	while not t[-1].isalnum():
		t = t[:-1]
	# Create mapping
	pwrnames[s] = t
	s = init1_c.readline()
	t = tables_c.readline()
# Close files
init1_c.close()
tables_c.close()

# Print the full data
for i in prog_tree:
	# Find a "N:" entry
	s = r_info[0]
	j = 0
	while not (s[0:2] == "N:" and i[1] == int(s[2:s.find(":", 2)])):
		j = j + 1
		s = r_info[j]

	# Proceed to "B:"
	while not (s[0:2] == "B:" or s[0:2] == "F:"):
		j = j + 1
		s = r_info[j]
	attacks = []
	while s[0:2] == "B:":
		newattk = []
		m = 2
		n = s.find(":", m)
		t = s[m:n]
		m = n + 1
		newattk.append(t.lower())
		n = s.find(":", m)
		t = s[m:n]
		m = n + 1
		newattk.append(t.lower())
		t = s[m:-1]
		newattk.append(t.lower())
		attacks.append(newattk)
		# Next attack
		j = j + 1
		s = r_info[j]

	# Proceed to "L:"
	while not (s[0:2] == "L:"):
		j = j + 1
		s = r_info[j]
	lev_start = s[2:s.find(":", 2)]
	t = s.find(":", 2)
	lev_end = s[t+1:s.find(":", t+1)]
	if int(lev_end) > 50:
		lev_end = "50"
	print "Level " + lev_start + "-" + lev_end + ": " + i[0]
	prefixes = [ "STR:", "INT:", "WIS:", "DEX:", "CON:", "CHR:" ]

	# Proceed to "A:"
	while not (s[0:2] == "A:"):
		j = j + 1
		s = r_info[j]
	# Print stats info
	m = 2
	print "\t",
	for t in prefixes:
		n = s.find(":", m)
		if int(s[m:n]) >= 0:
			print t + " +" + s[m:n] + " ",
		else:
			print t + " " + s[m:n] + " ",
		m = n + 1
	print
	# Print armor info
	n = s.find(":", m)
	armor_start = s[m:n]
	m = n + 1
	n = s.find(":", m)
	armor_end = s[m:n]
	m = n + 1
	if int(armor_start) == int(armor_end):
		print "\tNatural armor: " + armor_start
	else:
		print "\tNatural armor: " + armor_start + "..." + armor_end
	# Print speed info
	n = s.find(":", m)
	speed_start = s[m:n]
	if int(speed_start) >= 0:
		speed_start = "+" + speed_start
	m = n + 1
	n = s.find(":", m)
	speed_end = s[m:n]
	if int(speed_end) >= 0:
		speed_end = "+" + speed_end
	m = n + 1
	if int(speed_start) == int(speed_end):
		print "\tNatural speed: " + speed_start
	else:
		print "\tNatural speed: " + speed_start + "..." + speed_end

	# Proceed to "M:"
	while not (s[0:2] == "M:"):
		j = j + 1
		s = r_info[j]
	m = 2
	n = s.find(":", m)
	print "\tHit die: " + s[m:n]
	m = n + 1
	n = s.find(":", m)
	if int(s[m:n]) == 0:
		print "\tInfravision: none"
	else:
		print "\tInfravision: " + s[m:n] + "0 ft"
	# Remember powers stat
	m = n + 1
	pstat = int(s[m:])

	# Proceed to "K:"
	while not (s[0:2] == "K:"):
		j = j + 1
		s = r_info[j]
	slots = []
	m = 2
	for t in range(11):
		n = s.find(":", m)
		if t == 10:
			slots.append(int(s[m:]))
		else:
			slots.append(int(s[m:n]))
		m = n + 1
	print "\tEquip:",
	equip = []
	if slots[0] == 1:
		equip.append("weapon")
	elif slots[0] > 1:
	    	equip.append(str(slots[0]) + " weapons")
	if slots[1] == 1:
		equip.append("bow")
	elif slots[1] > 1:
	    	equip.append(str(slots[1]) + " bows")
	if slots[2] == 1:
		equip.append("ring")
	elif slots[2] > 1:
		equip.append(str(slots[2]) + " rings")
	if slots[3] == 1:
		equip.append("amulet")
	elif slots[3] > 1:
	    	equip.append(str(slots[3]) + " amulets")
	if slots[4] > 0:
	    	equip.append("light source")
	if slots[5] == 1:
		equip.append("body armor")
	elif slots[5] > 1:
	    	equip.append(str(slots[5]) + " body armors")
	if slots[6] == 1:
		equip.append("cloak")
	elif slots[6] > 1:
	    	equip.append(str(slots[6]) + " cloaks")
	if slots[7] == 1:
		equip.append("shield")
	elif slots[7] > 1:
		equip.append(str(slots[7]) + " shields")
	if slots[8] == 1:
		equip.append("helmet")
	elif slots[8] > 1:
		equip.append(str(slots[8]) + " helmets")
	if slots[9] == 1:
		equip.append("gloves")
	elif slots[9] > 1:
		equip.append(str(slots[9]) + " pairs of gloves")
	if slots[10] == 1:
		equip.append("boots")
	elif slots[10] > 1:
		equip.append(str(slots[10]) + " pairs of boots")
	i = 0
	for i in equip:
	    	if i == 0:
			print "\tEquip: ", capitalize(i), ",",
		elif i == len(equip) - 1
			print i
		elif i == 5:
		    	print i, ","
		elif i == 6
			print "\n\t  ", i
			

	# Proceed to "O:"
	while not (s[0:2] == "O:" or s[0:2] == "P:" or s[0:2] == "Z:" or s[0:2] == "N:"):
		j = j + 1
		s = r_info[j]
	props = []
	while s[0:2] == "O:":
		m = 2
		while s[m] != "\n":
			while s[m] == " " or s[m] == "|":
				m = m + 1
			n = m
			while s[m] == "_" or s[m].isalnum():
				m = m + 1
			props.append(s[n:m])
			if s[m] == "\n":
				break
			while s[m] == " " or s[m] == "|":
				m = m + 1
		# Next line
		j = j + 1
		s = r_info[j]
	props.sort()

	# Innate attacks
	if attacks == [] or "NO_INNATE" in props:
		print "\tInnate attacks: none"
	else:
		print "\tInnate attacks:"
		for t in attacks:
			if t[2] == "":
				continue
			print "\t\t" + t[2] + " " + t[0],
			if t[1] == "un_bonus" or t[1] == "un_power" or t[1] == "lose_mana" or t[1] == "hallu":
				print "to disenchant"
			elif t[1] == "disease" or t[1] == "poison":
				print "to poison"
			elif t[1] == "eat_lite":
				print "to attack with darkness"
			elif t[1] == "acid":
				print "to shoot acid"
			elif t[1] == "elec":
				print "to electrify"
			elif t[1] == "fire":
				print "to burn"
			elif t[1] == "cold":
				print "to freeze"
			elif t[1] == "blind" or t[1] == "confuse":
				print "to confuse"
			elif t[1] == "terrify":
				print "to terrify"
			elif t[1] == "paralyze":
				print "to sleep"
			elif t[1][0:3] == "exp":
				print "to attack with nether"
			else:
				print

	props_map = {
		"SUST_STR" : "Sustain Strength",
		"SUST_INT" : "Sustain Intelligence",
		"SUST_WIS" : "Sustain Wisdom",
		"SUST_DEX" : "Sustain Dexterity",
		"SUST_CON" : "Sustain Constitution",
		"SUST_CHR" : "Sustain Charisma",
		"IM_POIS"  : "Immunity to Poison",
		"IM_ACID"  : "Immunity to Acid",
		"IM_ELEC"  : "Immunity to Lightning",
		"IM_FIRE"  : "Immunity to Fire",
		"IM_COLD"  : "Immunity to Cold",
		"RES_ACID" : "Resist Acid",
		"RES_ELEC" : "Resist Lightning",
		"RES_FIRE" : "Resist Fire",
		"RES_COLD" : "Resist Cold",
		"RES_POIS" : "Resist Poison",
		"RES_FEAR" : "Resist Fear",
		"RES_LITE" : "Resist Light",
		"RES_DARK" : "Resist Darkness",
		"RES_BLIND": "Resist Blindness",
		"RES_CONFU": "Resist Confusion",
		"RES_SOUND": "Resist Sound",
		"RES_SHARD": "Resist Shards",
		"RES_NEXUS": "Resist Nexus",
		"RES_NETHR": "Resist Nether",
		"RES_CHAOS": "Resist Chaos",
		"RES_DISEN": "Resist Disenchant",
		"SLOW_DIGEST" : "Slow Digestion",
		"FEATHER"     : "Levitation",
		"LITE"        : "Permanent Light (rad 1)",
		"REGEN"       : "Regeneration",
		"SUPER_REGEN" : "Super Regeneration",
		"TELEPATHY"   : "Telepathy",
		"SEE_INVIS"   : "See Invisible",
		"FREE_ACT"    : "Free Action",
		"HOLD_LIFE"   : "Hold Life",
		"TELEPORT"    : "Random Teleportation",
		"AGGRAVATE"   : "Aggravation",
		"DRAIN_EXP"   : "Experience Draining",
		"ACID_TOUCH"  : "Melting Touch",
		"ELEC_TOUCH"  : "Shocking Touch",
		"FIRE_TOUCH"  : "Burning Touch",
		"COLD_TOUCH"  : "Freezing Touch",
		"EAT_WALL"    : "Eats Walls",
		"EAT_STATUE"  : "Eats Stone Statues",
		"PASS_WALL"   : "Immaterial",
		"UNDEAD_RACE" : "Undead",
		"GIANT_WIELD" : "Huge Weapons",
		"GIANT_WEAR"  : "Huge Armor",
		"THROW_BOULDER" : "Affinity with Boulders",
		"ALIGN_CHAOS" : "",
		"NO_INNATE"   : "",
		"RESISTANCE" : "Resist Low Elements",
		"POIS_BLOOD"  : "Poisonous Blood"
	}
	if len(props) == 0:
		print "\tInnate properties: none"
	else:
		print "\tInnate properties:"
		for t in props:
			if props_map[t] != "":
				print "\t\t" + props_map[t]

	# Proceed to "P:"
	while not (s[0:2] == "P:" or s[0:2] == "Z:" or s[0:2] == "N:"):
		j = j + 1
		s = r_info[j]
	pwrs = []
	while s[0:2] == "P:":
		newpwr = []
		m = 2
		n = s.find(":", m)
		t = s[m:n]
		m = n + 1
		newpwr.append(int(t))
		n = s.find(":", m)
		t = s[m:n]
		m = n + 1
		newpwr.append(int(t))
		n = s.find(":", m)
		t = s[m:n]
		m = n + 1
		newpwr.append(t)
		pwrs.append(newpwr)
		# Next power
		j = j + 1
		s = r_info[j]
	if pwrs == []:
		print "\tInnate powers: none"
	else:
		print "\tInnate powers:"
		print "\t\tLev Mana Name"
		print "\t\t--- ---- ---------------------"
		for t in pwrs:
			print "\t\t%2d  %3d  %s" % (t[0], t[1], pwrnames[t[2]])
		prefixes = [ "STR", "INT", "WIS", "DEX", "CON", "CHR" ]
		print "\tPowers stat:", prefixes[pstat]

	print
