import spell

def event_spell(num):
	
	# Only provide a definition for spell 59 (Dispel Monsters)
	# all other spells are handled by the default

	if (num == 59):
		spell.dispel_monsters(100)
		return 1

	# Let default handle it
	return 0
