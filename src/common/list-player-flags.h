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
PF(FAST_SHOT,        "improve shooting speed with tension bows", "Improved shooting speed with bows")
PF(BLESS_WEAPON,     "may only wield blessed or hafted weapons", "Prefers blunt/blessed weapons")
PF(ZERO_FAIL,        "may obtain a perfect success rate with magic", "Advanced spellcasting")
PF(BEAM,             "frequently turn bolt spells into beams", NULL)
PF(CHOOSE_SPELLS,    "may choose their own spells to study", NULL)
PF(KNOW_MUSHROOM,    "easily recognize mushrooms", "Identifies mushrooms")
PF(KNOW_ZAPPER,      "easily recognize magic devices", "Identifies magic devices")
PF(SEE_ORE,          "can sense ore in the walls", "Senses ore/minerals")
PF(NO_MANA,          "cannot cast spells", NULL)
PF(CHARM,            "is extra persuasive to monsters", "Charms monsters")
PF(UNLIGHT,          "resists the dark", "Likes the dark")
PF(STEAL,            "can steal from monsters", "Steals from monsters")
PF(SHIELD_BASH,      "can bash monsters with a shield in melee", "Employs shield-bashes")
PF(ELEMENTAL_SPELLS, "get access to elemental spells", NULL)
PF(UNDEAD_POWERS,    "get access to undead powers", NULL)
PF(STEALTH_MODE,     "get access to Stealth Mode", NULL)
PF(STEALING_IMPROV,  "get better chance at stealing", NULL)
PF(MONSTER_SPELLS,   "get access to monster spells", NULL)
PF(MARTIAL_ARTS,     "may use fighting abilities when fighting barehanded", NULL)
PF(FAST_THROW,       "improve throwing speed", "Improved throwing speed")
PF(BACK_STAB,        "deal extra damage to sleeping and fleeing creatures", NULL)
PF(ORC,              "belong to the Orc race", NULL)
PF(TROLL,            "belong to the Troll race", NULL)
PF(ANIMAL,           "belong to the Animal race", NULL)
PF(GIANT,            "belong to the Giant race", NULL)
PF(THUNDERLORD,      "belong to the Thunderlord race", NULL)
PF(DRAGON,           "belong to the Dragon race", NULL)
PF(SUMMON_SPELLS,    "get access to summon spells", NULL)
PF(SHAPECHANGE,      "can polymorph into monsters", NULL)
