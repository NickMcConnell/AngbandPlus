/* list-mon-flags.h - monster race flags
 *
 * Changing flag order will break savefiles. There was a hard-coded limit of
 * 96 flags, due to 12 bytes of storage for lore flags in the savefile; this 
 * should be fixed now. Flags below start from 1 on line 11, so a flag's 
 * sequence number is its line number minus 10.
 */

/* symbol       descr */
RF(NONE,        "")
RF(UNIQUE,      "")
RF(QUESTOR,     "")
RF(MALE,        "")
RF(FEMALE,      "")
RF(CHAR_CLEAR,  "")
RF(CHAR_MULTI,  "")
RF(ATTR_CLEAR,  "")
RF(ATTR_MULTI,  "")
RF(FORCE_DEPTH, "")
RF(FORCE_MAXHP, "")
RF(FORCE_SLEEP, "")
RF(FORCE_EXTRA, "")
RF(FRIEND,      "")
RF(FRIENDS,     "")
RF(ESCORT,      "")
RF(ESCORTS,     "")
RF(NEVER_BLOW,  "")
RF(NEVER_MOVE,  "")
RF(RAND_25,     "")
RF(RAND_50,     "")
RF(ONLY_GOLD,   "")
RF(ONLY_ITEM,   "")
RF(DROP_60,     "")
RF(DROP_90,     "")
RF(DROP_1D2,    "")
RF(DROP_2D2,    "")
RF(DROP_3D2,    "")
RF(DROP_4D2,    "")
RF(DROP_GOOD,   "")
RF(DROP_GREAT,  "")
RF(DROP_CHEST,  "")
RF(DROP_CHOSEN, "")
RF(STUPID,      "")
RF(SMART,       "")
RF(SPEAKING,    "")
RF(PLAYER_GHOST,"")
RF(INVISIBLE,   "")
RF(COLD_BLOOD,  "")
RF(EMPTY_MIND,  "")
RF(WEIRD_MIND,  "")
RF(MULTIPLY,    "")
RF(REGENERATE,  "")
RF(NO_PLACE,    "")
RF(ANGBAND,     "")
RF(RUDH,        "")
RF(NARGOTHROND, "")
RF(DUNGORTHEB,  "")
RF(GAURHOTH,    "")
RF(OPEN_DOOR,   "")
RF(BASH_DOOR,   "")
RF(PASS_WALL,   "")
RF(KILL_WALL,   "")
RF(MOVE_BODY,   "")
RF(KILL_BODY,   "")
RF(TAKE_ITEM,   "")
RF(KILL_ITEM,   "")
RF(FLYING,      "")
RF(LOW_MANA_RUN,"")
RF(SMASH_WALL,  "")
RF(POWERFUL,    "")
RF(ARCHER,      "")
RF(MORGUL_MAGIC,"")
RF(UDUN_MAGIC,  "")
RF(BRAIN_2,     "")
RF(ORC,         "")
RF(TROLL,       "")
RF(GIANT,       "")
RF(DRAGON,      "")
RF(DEMON,       "")
RF(UNDEAD,      "")
RF(EVIL,        "")
RF(ANIMAL,      "")
RF(TERRITORIAL, "")
RF(RACIAL,      "")
RF(DUNGEON,     "")
RF(ATTR_FLICKER, "")
RF(HURT_LIGHT,  "")
RF(HURT_ROCK,   "")
RF(HURT_FIRE,   "")
RF(HURT_COLD,   "")
RF(IM_ACID,     "")
RF(IM_ELEC,     "")
RF(IM_FIRE,     "")
RF(IM_COLD,     "")
RF(IM_POIS,     "")
RF(XXX5,        "")
RF(RES_NETH,    "")
RF(RES_WATE,    "")
RF(RES_PLAS,    "")
RF(RES_NEXUS,   "")
RF(RES_DISE,    "")
RF(XXX6,        "")
RF(NO_FEAR,     "")
RF(NO_STUN,     "")
RF(NO_CONF,     "")
RF(NO_SLEEP,    "")
RF(HATE_WATER,  "")