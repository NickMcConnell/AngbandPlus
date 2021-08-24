/*
 * File: list-elements.h
 * Purpose: Elements used in spells and other attacks.
 *
 * Fields:
 * index - type index
 * name - text name
 * description - description of the effect
 * blind desc - text description of attack if blind
 * num - numerator for resistance
 * denom - denominator for resistance (random_value)
 * divisor - what to divide hp by to give breath damage
 * cap - breath damage cap
 * color - color of the effect
 * PvP type - type of attack (for PvP)
 */

/* index  name  description  blind desc  num  denom  divisor  cap  color  PvP type */
ELEM(ACID, "acid", "acid", "acid", 1, RV(3, 0, 0, 0), 3, 1600, COLOUR_SLATE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(ELEC, "lightning", "lightning", "lightning", 1, RV(3, 0, 0, 0), 3, 1600, COLOUR_BLUE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(FIRE, "fire", "fire", "fire", 1, RV(3, 0, 0, 0), 3, 1600, COLOUR_RED, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(COLD, "cold", "frost", "cold", 1, RV(3, 0, 0, 0), 3, 1600, COLOUR_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(POIS, "poison", "poison", "poison", 1, RV(3, 0, 0, 0), 3, 800, COLOUR_GREEN, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(LIGHT, "light", "light", "something", 4, RV(6, 1, 6, 0), 6, 400, COLOUR_ORANGE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(DARK, "dark", "darkness", "something", 4, RV(6, 1, 6, 0), 6, 400, COLOUR_L_DARK, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(SOUND, "sound", "sound", "noise", 5, RV(6, 1, 6, 0), 6, 500, COLOUR_YELLOW, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(SHARD, "shards", "shards", "something sharp", 6, RV(6, 1, 6, 0), 6, 500, COLOUR_UMBER, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(NEXUS, "nexus", "nexus", "something strange", 6, RV(6, 1, 6, 0), 6, 400, COLOUR_L_RED, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(NETHER, "nether", "nether", "something cold", 6, RV(6, 1, 6, 0), 6, 550, COLOUR_L_GREEN, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(CHAOS, "chaos", "chaos", "something strange", 6, RV(6, 1, 6, 0), 6, 500, COLOUR_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(DISEN, "disenchantment", "disenchantment", "something strange", 6, RV(6, 1, 6, 0), 6, 500, COLOUR_VIOLET, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(WATER, "water", "water", "water", 0, RV(0, 0, 0, 0), 3, 300, COLOUR_SLATE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(ICE, "ice", "", "something sharp", 1, RV(3, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(GRAVITY, "gravity", "", "something strange", 6, RV(6, 1, 6, 0), 3, 200, COLOUR_L_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(INERT, "inertia", "inertia", "something strange", 0, RV(0, 0, 0, 0), 6, 200, COLOUR_L_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(FORCE, "force", "", "something hard", 0, RV(0, 0, 0, 0), 6, 200, COLOUR_UMBER, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(TIME, "time", "", "something strange", 6, RV(6, 1, 6, 0), 3, 150, COLOUR_L_BLUE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(PLASMA, "plasma", "", "something", 0, RV(0, 0, 0, 0), 6, 150, COLOUR_RED, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(METEOR, "a meteor", "", "something", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_RED, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(MISSILE, "a missile", "magical energy", "something", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(MANA, "mana", "mana", "something", 6, RV(6, 1, 6, 0), 3, 250, COLOUR_L_DARK, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(HOLY_ORB, "a holy orb", "", "something", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_L_DARK, ATT_DAMAGE | ATT_NON_PHYS)
ELEM(ARROW_X, "a seeker arrow", "magical energy", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
ELEM(ARROW_1, "a shot", "", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
ELEM(ARROW_2, "an arrow", "", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
ELEM(ARROW_3, "a bolt", "", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
ELEM(ARROW_4, "a missile", "", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
ELEM(BOULDER, "a boulder", "", "something sharp", 0, RV(0, 0, 0, 0), 0, 0, COLOUR_WHITE, ATT_DAMAGE)
