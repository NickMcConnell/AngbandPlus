/*
 * File: list-object-flags.h
 * Purpose: Object flags for all objects
 *
 * Each sustain flag (SUST_*) has a matching stat in src/list-stats.h,
 * which should be at the same index in that file as the sustain in this file (plus one for OF_NONE).
 *
 * Flag properties are defined in lib/gamedata/object_property.txt
 *
 * Fields:
 * symbol - the flag name
 * birth-descr - description of the flag for use in the birth menus
 */

/* symbol  birth-descr */
OF(NONE, NULL)
OF(SUST_STR, "Sustains strength")
OF(SUST_INT, "Sustains intelligence")
OF(SUST_WIS, "Sustains wisdom")
OF(SUST_DEX, "Sustains dexterity")
OF(SUST_CON, "Sustains constitution")
OF(PROT_FEAR, "Resists fear")
OF(PROT_BLIND, "Resists blindness")
OF(PROT_CONF, "Resists confusion")
OF(PROT_STUN, "Resists stunning")
OF(SLOW_DIGEST, "Digests food slowly")
OF(FEATHER, "Floats just above the floor")
OF(REGEN, "Regenerates quickly")
OF(SEE_INVIS, "Sees invisible creatures")
OF(FREE_ACT, "Resists paralysis")
OF(HOLD_LIFE, "Sustains experience")
OF(IMPACT, NULL)
OF(BLESSED, NULL)
OF(BURNS_OUT, NULL)
OF(TAKES_FUEL, NULL)
OF(NO_FUEL, NULL)
OF(IMPAIR_HP, "Impaired hitpoint recovery")
OF(IMPAIR_MANA, "Impaired mana recovery")
OF(AFRAID, NULL)
OF(NO_TELEPORT, NULL)
OF(AGGRAVATE, NULL)
OF(DRAIN_EXP, NULL)
OF(STICKY, NULL)
OF(FRAGILE, NULL)
OF(LIGHT_1, NULL)
OF(LIGHT_2, NULL)
OF(LIGHT_3, NULL)
OF(DIG_1, NULL)
OF(DIG_2, NULL)
OF(DIG_3, NULL)
OF(EXPLODE, NULL)
OF(KNOWLEDGE, NULL)
OF(AMMO_MAGIC, NULL)
OF(NO_ACTIVATION, NULL)
OF(ESP_ANIMAL, "Can sense animals")
OF(ESP_EVIL, "Can sense evil")
OF(ESP_UNDEAD, "Can sense undead")
OF(ESP_DEMON, "Can sense demons")
OF(ESP_ORC, "Can sense orcs")
OF(ESP_TROLL, "Can sense trolls")
OF(ESP_GIANT, "Can sense giants")
OF(ESP_DRAGON, "Can sense dragons")
OF(ESP_ALL, "Has telepathic powers")
OF(ESP_RADIUS, "Has telepathic awareness")
