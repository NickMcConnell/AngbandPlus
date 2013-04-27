/* x-spell.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#define SPELL_MAGIC_MISSILE             0
#define SPELL_DETECT_MONSTERS           1
#define SPELL_PHASE_DOOR                2
#define SPELL_LIGHT_AREA                3
#define SPELL_FIND_TRAPS_DOORS          4
#define SPELL_CURE_LIGHT_WOUNDS         5
#define SPELL_TREASURE_DETECTION        6
#define SPELL_OBJECT_DETECTION          7
#define SPELL_IDENTIFY                  8
#define SPELL_DETECT_INVISIBLE          9
#define SPELL_DETECT_ENCHANTMENT        10
#define SPELL_STINKING_CLOUD            11
#define SPELL_LIGHTNING_BOLT            12
#define SPELL_CONFUSE_MONSTER           13
#define SPELL_SLEEP_MONSTER             14
#define SPELL_WONDER                    15
#define SPELL_FROST_BOLT                16
#define SPELL_ACID_BOLT                 17
#define SPELL_FIRE_BOLT                 18
#define SPELL_TRAP_DOOR_DESTRUCTION     19
#define SPELL_SPEAR_OF_LIGHT            20
#define SPELL_TURN_STONE_TO_MUD         21
#define SPELL_DOOR_CREATION             22
#define SPELL_EARTHQUAKE                23
#define SPELL_STAIR_CREATION            24
#define SPELL_CURE_POISON               25
#define SPELL_SATISFY_HUNGER            26
#define SPELL_HEROISM                   27
#define SPELL_BERSERKER                 28
#define SPELL_HASTE_SELF                29
#define SPELL_TELEPORT_SELF             30
#define SPELL_SLOW_MONSTER              31
#define SPELL_TELEPORT_OTHER            32
#define SPELL_TELEPORT_LEVEL            33
#define SPELL_WORD_OF_RECALL            34
#define SPELL_POLYMORPH_OTHER           35
#define SPELL_SHOCK_WAVE                36
#define SPELL_EXPLOSION                 37
#define SPELL_CLOUD_KILL                38
#define SPELL_MASS_SLEEP                39
#define SPELL_BEDLAM                    40
#define SPELL_REND_SOUL                 41
#define SPELL_WORD_OF_DESTRUCTION       42
#define SPELL_CHAOS_STRIKE              43
#define SPELL_RESIST_COLD               44
#define SPELL_RESIST_FIRE               45
#define SPELL_RESIST_POISON             46
#define SPELL_RESISTANCE                47
#define SPELL_SHIELD                    48
#define SPELL_RUNE_OF_PROTECTION        49
#define SPELL_RECHARGE_ITEM_I           50
#define SPELL_ENCHANT_ARMOR             51
#define SPELL_ENCHANT_WEAPON            52
#define SPELL_RECHARGE_ITEM_II          53
#define SPELL_ELEMENTAL_BRAND           54
#define SPELL_FROST_BALL                55
#define SPELL_ACID_BALL                 56
#define SPELL_FIRE_BALL                 57
#define SPELL_ICE_STORM                 58
#define SPELL_BANISHMENT                59
#define SPELL_METEOR_SWARM              60
#define SPELL_MASS_BANISHMENT           61
#define SPELL_RIFT                      62
#define SPELL_MANA_STORM                63

#define STRICTMAX_SPELL					(SPELL_MANA_STORM+1)				

/* Beginners Handbook */
#define PRAYER_DETECT_EVIL              0
#define PRAYER_CURE_LIGHT_WOUNDS        1
#define PRAYER_BLESS                    2
#define PRAYER_REMOVE_FEAR              3
#define PRAYER_CALL_LIGHT               4
#define PRAYER_FIND_TRAPS               5
#define PRAYER_DETECT_DOORS_STAIRS      6
#define PRAYER_SLOW_POISON              7

/* Words of Wisdom */
#define PRAYER_SCARE_MONSTER            8
#define PRAYER_PORTAL                   9
#define PRAYER_CURE_SERIOUS_WOUNDS     10
#define PRAYER_CHANT                   11
#define PRAYER_SANCTUARY               12
#define PRAYER_SATISFY_HUNGER          13
#define PRAYER_REMOVE_CURSE            14
#define PRAYER_RESIST_HEAT_COLD        15

/* Chants and Blessings */
#define PRAYER_NEUTRALIZE_POISON       16
#define PRAYER_ORB_OF_DRAINING         17
#define PRAYER_CURE_CRITICAL_WOUNDS    18
#define PRAYER_SENSE_INVISIBLE         19
#define PRAYER_PROTECTION_FROM_EVIL    20
#define PRAYER_EARTHQUAKE              21
#define PRAYER_SENSE_SURROUNDINGS      22
#define PRAYER_CURE_MORTAL_WOUNDS      23
#define PRAYER_TURN_UNDEAD             24

/* Exorcism and Dispelling */
#define PRAYER_PRAYER                  25
#define PRAYER_DISPEL_UNDEAD           26
#define PRAYER_HEAL                    27
#define PRAYER_DISPEL_EVIL             28
#define PRAYER_GLYPH_OF_WARDING        29
#define PRAYER_HOLY_WORD               30

/* Godly Insights */
#define PRAYER_DETECT_MONSTERS         31
#define PRAYER_DETECTION               32
#define PRAYER_PERCEPTION              33
#define PRAYER_PROBING                 34
#define PRAYER_CLAIRVOYANCE            35

/* Purifications and Healing */
#define PRAYER_CURE_SERIOUS_WOUNDS2    36
#define PRAYER_CURE_MORTAL_WOUNDS2     37
#define PRAYER_HEALING                 38
#define PRAYER_RESTORATION             39
#define PRAYER_REMEMBRANCE             40

/* Wrath of God */
#define PRAYER_DISPEL_UNDEAD2          41
#define PRAYER_DISPEL_EVIL2            42
#define PRAYER_BANISH_EVIL             43
#define PRAYER_WORD_OF_DESTRUCTION     44
#define PRAYER_ANNIHILATION            45

/* Holy Infusions */
#define PRAYER_UNBARRING_WAYS          46
#define PRAYER_RECHARGING              47
#define PRAYER_DISPEL_CURSE            48
#define PRAYER_ENCHANT_WEAPON          49
#define PRAYER_ENCHANT_ARMOUR          50
#define PRAYER_ELEMENTAL_BRAND         51

/* Ethereal openings */
#define PRAYER_BLINK                   52
#define PRAYER_TELEPORT_SELF           53
#define PRAYER_TELEPORT_OTHER          54
#define PRAYER_TELEPORT_LEVEL          55
#define PRAYER_WORD_OF_RECALL          56
#define PRAYER_ALTER_REALITY           57

#define STRICTMAX_PRAYER					(PRAYER_ALTER_REALITY+1)

extern int get_spell_book(int index);


