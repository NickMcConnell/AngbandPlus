/*
 * File: list-player-flags.h
 * Purpose: Player race and class flags
 *
 * Fields:
 * symbol - the flag name
 * descr - description of the flag effect
 * birth-descr - description of the flag for use in the birth menus
 */

/* symbol  descr  birth-descr */
PF(NONE,             "", NULL)
PF(EXTRA_SHOT,       "receive extra shots with tension bows at levels 20 and 40", "Gains extra shots with bow")
PF(BRAVERY_30,       "become immune to fear at level 30", "Gains immunity to fear")
PF(BLESS_WEAPON,     "may only wield blessed or hafted weapons", "Prefers blunt/blessed weapons")
PF(CUMBER_GLOVE,     "have difficulty using magic with covered hands", NULL)
PF(ZERO_FAIL,        "may obtain a perfect success rate with magic", "Advanced spellcasting")
PF(BEAM,             "frequently turn bolt spells into beams", NULL)
PF(CHOOSE_SPELLS,    "may choose their own spells to study", NULL)
PF(PSEUDO_ID_IMPROV, "get better at pseudo id with experience", NULL)
PF(ELEMENTAL_SPELLS, "get access to elemental spells", NULL)
PF(UNDEAD_POWERS,    "get access to undead powers", NULL)
PF(STEALTH_MODE,     "turn Searching Mode into Stealth Mode", NULL)
PF(STEALING_IMPROV,  "get better chance at stealing", NULL)
PF(SPEED_BONUS,      "get extra speed", NULL)
PF(MONSTER_SPELLS,   "get access to monster spells", NULL)
PF(MARTIAL_ARTS,     "may use fighting abilities when fighting barehanded", NULL)
PF(EXTRA_SHOTS,      "get more extra shots", NULL)
PF(EXTRA_MANA,       "get extra mana", NULL)
PF(BACK_STAB,        "deal extra damage to sleeping and fleeing creatures", NULL)
PF(ANTIMAGIC,        "are surrounded by an antimagic field", NULL)
PF(KNOW_MUSHROOM,    "easily recognize mushrooms", "Identifies mushrooms")
PF(KNOW_ZAPPER,      "easily recognize magic devices", "Identifies magic devices")
PF(SEE_ORE,          "can sense ore in the walls", "Senses ore/minerals")
PF(NO_MANA,          "cannot cast spells", NULL)
PF(ORC,              "belong to the Orc race", NULL)
PF(TROLL,            "belong to the Troll race", NULL)
PF(ANIMAL,           "belong to the Animal race", NULL)
PF(GIANT,            "belong to the Giant race", NULL)
PF(THUNDERLORD,      "belong to the Thunderlord race", NULL)
PF(DRAGON,           "belong to the Dragon race", NULL)
PF(SUMMON_SPELLS,    "get access to summon spells", NULL)
