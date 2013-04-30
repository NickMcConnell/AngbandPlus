/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
*/

#include "angband.h"


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "lib/file" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * Note that if "ALLOW_TEMPLATES" is not defined, then a lot of the code
 * in this file is compiled out, and the game will not run unless valid
 * "binary template files" already exist in "lib/data".  Thus, one can
 * compile Angband with ALLOW_TEMPLATES defined, run once to create the
 * "*.raw" files in "lib/data", and then quit, and recompile without
 * defining ALLOW_TEMPLATES, which will both save 20K and prevent people
 * from changing the ascii template files in potentially dangerous ways.
 *
 * The code could actually be removed and placed into a "stand-alone"
 * program, but that feels a little silly, especially considering some
 * of the platforms that we currently support.
 */


#ifdef ALLOW_TEMPLATES


#include "init.h"


/*** Helper arrays for parsing ascii template files ***/

/*
 * Room Special Flags
 */
static cptr d_info_sflags[] =
{
	"SEEN",
	"ICKY",
	"BLOODY",
	"CURSED",
	"GLOOMY",
	"PORTAL",
	"SILENT",
	"STATIC"
};

/*
 * Room Level Flags
 */
static cptr d_info_lflags[] =
{
	"WATER",
	"LAVA",
	"ICE",
	"ACID",
	"OIL",
	"CHASM",
	"DARK",  /* Hack -- dark rooms - was destroyed */
	"CROWDED"
};

/*
 * Monster/Trap/Spell Blow Methods
 */
static cptr r_info_blow_method[] =
{
	"",
	"HIT",
	"TOUCH",
	"PUNCH",
	"KICK",
	"CLAW",
	"BITE",
	"STING",
	"VOMIT",
	"BUTT",
	"CRUSH",
	"ENGULF",
	"PECK",
	"CRAWL",
	"DROOL",
	"SLIME",
	"SPIT",
	"GAZE",
	"WAIL",
	"SPORE",
	"LASH",
	"BEG",
	"INSULT",
	"MOAN",
	"THROW",
	"TRAP",
	"SHOOT",
	"AURA",
	"SELF",
	"ADJACENT",
	"HANDS",
	"MISSILE",
	"BOLT_10",
	"BOLT",
	"BEAM",
	"BLAST",
	"WALL",
	"BALL",
	"CLOUD",
	"STORM",
	"BREATH",
	"AREA",
	"LOS",
	"LINE",
	"AIM",
	"ORB",
	"STAR",
	"SPHERE",
	"PANEL",
	"LEVEL",
	"CROSS",
	"STRIKE",
	NULL
};


/*
 * Monster/Trap/Spell Blow Effects
 */
static cptr r_info_blow_effect[] =
{
	"",
	"XXX1",
	"ARROW",
	"MISSILE",
	"MANA",
	"HOLY_ORB",
	"LITE_WEAK",
	"DARK_WEAK",
	"WATER_WEAK",
	"PLASMA",
	"METEOR",
	"ICE",
	"GRAVITY",
	"INERTIA",
	"FORCE",
	"TIME",
	"ACID",
	"ELEC",
	"FIRE",
	"COLD",
	"POISON",
	"XXX2",
	"LITE",
	"DARK",
	"WATER",
	"CONFUSE",
	"SOUND",
	"SHARD",
	"NEXUS",
	"NETHER",
	"CHAOS",
	"DISENCHANT",
	"EXPLODE",
	"KILL_WALL",
	"KILL_DOOR",
	"KILL_TRAP",
	"MAKE_WALL",
	"MAKE_DOOR",
	"MAKE_TRAP",
	"BRIDGE",
	"XXX6",
	"AWAY_UNDEAD",
	"AWAY_EVIL",
	"AWAY_ALL",
	"TURN_UNDEAD",
	"TURN_EVIL",
	"TURN_ALL",
	"DISPEL_UNDEAD",
	"DISPEL_EVIL",
	"DISPEL_ALL",
	"XXX7",
	"CLONE",
	"POLYMORPH",
	"HEAL",
	"HASTE",
	"SLOW_WEAK",
	"CONFUSE_WEAK",
	"SLEEP",
	"DRAIN_LIFE",
	"BWATER",
	"BMUD",
	"HURT",
	"LAVA",
	"UN_BONUS",
	"UN_POWER",
	"EAT_GOLD",
	"EAT_ITEM",
	"EAT_FOOD",
	"EAT_LITE",
	"FALL",
	"FALL_MORE",
	"FALL_SPIKE",
	"FALL_POISON",
	"BLIND",
	"SLOW",
	"TERRIFY",
	"PARALYZE",
	"LOSE_STR",
	"LOSE_INT",
	"LOSE_WIS",
	"LOSE_DEX",
	"LOSE_CON",
	"LOSE_CHR",
	"LOSE_ALL",
	"SHATTER",
	"EXP_10",
	"EXP_20",
	"EXP_40",
	"EXP_80",
	"RAISE",
	"LOWER",
	"PROBE",
	"LOCK_DOOR",
	"SALT_WATER",
	"HALLU",
	"FEATURE",
	"STEAM",
	"VAPOUR",
	"SMOKE",
	"SUFFOCATE",
	"HUNGER",
	"DISEASE",
	"LOSE_MANA",
	"WOUND",
	"BATTER",
	NULL
};

/*
 * Feature info flags
 */
static cptr f_info_flags1[] =
{
	"LOS",
	"PROJECT",
	"MOVE",
	"PLACE",
	"DROP",
	"SECRET",
	"NOTICE",
	"REMEMBER",
	"OPEN",
	"CLOSE",
	"BASH",
	"SPIKE",
	"DISARM",
	"ENTER",
	"TUNNEL",
	"STREAMER",
	"HAS_GOLD",
	"HAS_ITEM",
	"DOOR",
	"TRAP",
	"STAIRS",
	"GLYPH",
	"LESS",
	"MORE",
	"RUN",
	"FLOOR",
	"WALL",
	"PERMANENT",
	"INNER",
	"OUTER",
	"SOLID",
	"HIT_TRAP"
};

/*
 * Feature info flags
 */
static cptr f_info_flags2[] =
{
	"BRIDGE",
	"RIVER",
	"LAKE",
	"BRIDGED",
	"COVERED",
	"GLOW",
	"ATTR_LITE",
	"WATER",
	"LAVA",
	"SHALLOW",
	"DEEP",
	"FILLED",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"HURT_ACID",
	"ICE",
	"ACID",
	"OIL",
	"CHASM",
	"CAN_CLIMB",
	"CAN_FLY",
	"CAN_SWIM",
	"CAN_PASS",
	"CAN_OOZE",
	"CAN_DIG",
	"HIDE_ITEM",
	"HIDE_SNEAK",
	"HIDE_SWIM",
	"HIDE_DIG",
	"KILL_HUGE",
	"KILL_MOVE"
};

/*
 * Feature info flags
 */
static cptr f_info_flags3[] =
{
	"PICK_TRAP",
	"PICK_DOOR",
	"ALLOC",
	"CHEST",
	"DROP_1D2",
	"DROP_2D2",
	"DROP_GOOD",
	"DROP_GREAT",
	"HURT_POIS",
	"HURT_ELEC",
	"HURT_WATER",
	"HURT_BWATER",
	"USE_FEAT",
	"GET_FEAT",
	"GROUND",
	"OUTSIDE",
	"EASY_HIDE",
	"EASY_CLIMB",
	"MUST_CLIMB",
	"TREE",
	"NEED_TREE",
	"BLOOD",
	"DUST",
	"SLIME",
	"LIVING",
	"FLAVOR",
	"INSTANT",
	"EXPLODE",
	"TIMED",
	"ERUPT",
	"STRIKE",
	"SPREAD"
};

/*
 * Monster race flags
 */
static cptr r_info_flags1[] =
{
	"UNIQUE",
	"QUESTOR",
	"MALE",
	"FEMALE",
	"CHAR_CLEAR",
	"CHAR_MULTI",
	"ATTR_CLEAR",
	"ATTR_MULTI",
	"FORCE_DEPTH",
	"FORCE_MAXHP",
	"FORCE_SLEEP",
	"GUARDIAN",
	"ATTR_METAL",
	"FRIENDS",
	"ESCORT",
	"ESCORTS",
	"NEVER_BLOW",
	"NEVER_MOVE",
	"RAND_25",
	"RAND_50",
	"ONLY_GOLD",
	"ONLY_ITEM",
	"DROP_60",
	"DROP_90",
	"DROP_1D2",
	"DROP_2D2",
	"DROP_3D2",
	"DROP_4D2",
	"DROP_GOOD",
	"DROP_GREAT",
	"DROP_USEFUL",
	"DROP_CHOSEN"
};
/*
 * Monster race flags
 */
static cptr r_info_flags2[] =
{
	"STUPID",
	"SMART",
	"CAN_DIG",
	"HAS_LITE",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"CAN_SWIM",
	"MUST_SWIM",
	"POWERFUL",
	"CAN_CLIMB",
	"CAN_FLY",
	"MUST_FLY",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"ARCHER",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"SNEAKY",
	"WARRIOR",
	"PRIEST",
	"MAGE",
	"HAS_AURA",
	"HAS_WEB",
	"NEED_LITE",
	"LOW_MANA_RUN"
};

/*
 * Monster race flags
 */
static cptr r_info_flags3[] =
{
	"ORC",
	"TROLL",
	"GIANT",
	"DRAGON",
	"DEMON",
	"UNDEAD",
	"EVIL",
	"ANIMAL",
	"OOZE",
	"HUGE",
	"XXX3X3",
	"XXX4X3",
	"HURT_LITE",
	"HURT_ROCK",
	"PLANT",
	"INSECT",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	"IM_WATER",
	"RES_NETH",
	"RES_LAVA",
	"RES_PLAS",
	"RES_NEXU",
	"RES_DISE",
	"HURT_WATER",
	"NO_FEAR",
	"NO_STUN",
	"NO_CONF",
	"NO_SLEEP"
};

/*
 * Monster race flags
 */
static cptr r_info_flags4[] =
{
	"SHRIEK",
	"SPORE",
	"GAZE",
	"WAIL",
	"SPIT",
	"SHOOT",
	"EXPLODE",
	"AURA",
	"BR_ACID",
	"BR_ELEC",
	"BR_FIRE",
	"BR_COLD",
	"BR_POIS",
	"BR_NETH",
	"BR_LITE",
	"BR_DARK",
	"BR_CONF",
	"BR_SOUN",
	"BR_CHAO",
	"BR_DISE",
	"BR_NEXU",
	"BR_TIME",
	"BR_INER",
	"BR_GRAV",
	"BR_SHAR",
	"BR_PLAS",
	"BR_WALL",
	"BR_MANA",
	"BR_FEAR",
	"XXX6X4",
	"LASH",
	"THROW"
};

/*
 * Monster race flags
 */
static cptr r_info_flags5[] =
{
	"BA_ACID",
	"BA_ELEC",
	"BA_FIRE",
	"BA_COLD",
	"BA_POIS",
	"BA_NETH",
	"BA_WATE",
	"BA_MANA",
	"BA_DARK",
	"DRAIN_MANA",
	"MIND_BLAST",
	"BRAIN_SMASH",
	"WOUND",
	"HUNGER",
	"",
	"",
	"BO_ACID",
	"BO_ELEC",
	"BO_FIRE",
	"BO_COLD",
	"BO_POIS",
	"BO_NETH",
	"BO_WATE",
	"BO_MANA",
	"BO_PLAS",
	"BO_ICEE",
	"MISSILE",
	"SCARE",
	"BLIND",
	"CONF",
	"SLOW",
	"HOLD"
};

/*
 * Monster race flags
 */
static cptr r_info_flags6[] =
{
	"HASTE",
	"ADD_MANA",
	"HEAL",
	"CURE",
	"BLINK",
	"TPORT",
	"XXX3X6",
	"TELE_SELF_TO",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"XXX5",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"XXX6X6",
	"S_KIN",
	"S_HI_DEMON",
	"S_MONSTER",
	"S_MONSTERS",
	"S_ANIMAL",
	"S_SPIDER",
	"S_HOUND",
	"S_HYDRA",
	"S_ANGEL",
	"S_DEMON",
	"S_UNDEAD",
	"S_DRAGON",
	"S_HI_UNDEAD",
	"S_HI_DRAGON",
	"S_WRAITH",
	"S_UNIQUE"
};

/*
 * Monster race flags
 */
static u32b hack_rf7_flags[52]=
{
/* A */ (RF7_HAS_CORPSE | RF7_HAS_WING | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_SCALE | RF7_HAS_LEG | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_CHEST | RF7_DROP_JEWELRY ),
	(RF7_HAS_CORPSE | RF7_HAS_WING | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_FEATHER |
	 RF7_HAS_HEAD | RF7_HAS_BLOOD | RF7_DROP_JEWELRY | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_FUR | RF7_HAS_HEAD | RF7_HAS_LEG | RF7_HAS_BLOOD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_WING | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_SCALE | RF7_HAS_LEG | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_CHEST | RF7_DROP_JEWELRY ),
/* E */ (RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_WEAPON | RF7_DROP_TOOL),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_SKIN | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_DROP_CLOTHES | RF7_DROP_JUNK | RF7_DROP_TOOL | RF7_DROP_MUSIC | RF7_DROP_WRITING),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_FUR | RF7_HAS_LEG | RF7_HAS_BLOOD | RF7_HAS_FEATHER |
	 RF7_HAS_SCALE | RF7_HAS_WING |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK | RF7_DROP_JEWELRY),
/* I */ (RF7_HAS_CORPSE | RF7_HAS_LEG | RF7_HAS_WING | RF7_HAS_SKIN |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_BLOOD | RF7_HAS_TEETH |
	 RF7_HAS_SKIN | RF7_HAS_HEAD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_LEG | RF7_HAS_SKIN |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_SKULL | RF7_HAS_DUST |
	 RF7_DROP_RSW | RF7_DROP_WRITING | RF7_DROP_JEWELRY | RF7_DROP_CLOTHES),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_WRITING | RF7_DROP_WEAPON),
	(RF7_HAS_SKULL | RF7_HAS_DUST |
	 RF7_DROP_ARMOR | RF7_DROP_WRITING | RF7_DROP_JEWELRY),
/* O */ (RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES | RF7_DROP_CHEST |
	 RF7_DROP_TOOL | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES | RF7_DROP_CHEST |
	 RF7_DROP_TOOL | RF7_DROP_POTION | RF7_DROP_MUSIC | RF7_DROP_LITE),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_FUR |
	 RF7_HAS_LEG | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_WEAPON | RF7_DROP_CHEST | RF7_DROP_FOOD | RF7_DROP_TOOL),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_LEG |
	 RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK | RF7_DROP_WEAPON),
	(RF7_HAS_CORPSE | RF7_HAS_LEG | RF7_HAS_SKIN |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES | RF7_DROP_CHEST |
	 RF7_DROP_TOOL | RF7_DROP_JUNK),
/* U */ (RF7_HAS_SKULL |
	 RF7_DROP_WEAPON | RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_CHEST | RF7_DROP_WRITING),
	(RF7_HAS_SKULL |  RF7_HAS_SKELETON | RF7_HAS_DUST |
	 RF7_DROP_CLOTHES | RF7_DROP_RSW | RF7_DROP_WRITING | RF7_DROP_JEWELRY | RF7_DROP_ARMOR |
	 RF7_DROP_WEAPON),
	(RF7_HAS_SKULL | RF7_HAS_DUST |
	 RF7_DROP_CLOTHES | RF7_DROP_RSW | RF7_DROP_WRITING | RF7_DROP_JEWELRY | RF7_DROP_ARMOR |
	 RF7_DROP_WEAPON),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_ARM | RF7_HAS_LEG | RF7_HAS_SKIN |
	 RF7_DROP_JEWELRY),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD | RF7_HAS_FUR |
	 RF7_DROP_FOOD | RF7_DROP_CLOTHES | RF7_DROP_JUNK),
/* Z */ (RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_WEAPON | RF7_DROP_TOOL),
/* a */ (RF7_HAS_CORPSE | RF7_HAS_LEG | RF7_HAS_SKIN |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_FUR | RF7_HAS_BLOOD |
	 RF7_HAS_WING | RF7_HAS_HEAD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_LEG | RF7_HAS_SKIN |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_WING | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_SCALE | RF7_HAS_LEG | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_JEWELRY ),
/* e */ (RF7_HAS_CORPSE | RF7_HAS_BLOOD |
	 RF7_DROP_POTION | RF7_DROP_JEWELRY | RF7_DROP_WRITING),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_FUR | RF7_HAS_LEG | RF7_HAS_BLOOD | RF7_HAS_HEAD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK),
	(RF7_HAS_ARM | RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD |
	 RF7_DROP_WEAPON | RF7_DROP_TOOL),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES |
	 RF7_DROP_TOOL | RF7_DROP_POTION | RF7_DROP_LITE | RF7_DROP_WRITING),
/* i */ (RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SLIME |
	 RF7_DROP_JEWELRY | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SLIME |
	 RF7_DROP_ARMOR | RF7_DROP_WEAPON | RF7_DROP_JUNK | RF7_DROP_TOOL | RF7_DROP_JEWELRY),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES |
	 RF7_DROP_TOOL | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES | RF7_DROP_MISSILE |
	 RF7_DROP_POTION | RF7_DROP_LITE | RF7_DROP_WRITING | RF7_DROP_JEWELRY),
	(RF7_HAS_SPORE | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_BLOOD | RF7_HAS_TEETH |
	 RF7_HAS_SKIN |
	 RF7_DROP_POTION | RF7_DROP_WRITING),
/* o */ (RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES |
	 RF7_DROP_TOOL | RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_ARMOR | RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES |
	 RF7_DROP_POTION | RF7_DROP_LITE | RF7_DROP_JEWELRY | RF7_DROP_WRITING),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES | RF7_DROP_JEWELRY |
	 RF7_DROP_TOOL | RF7_DROP_POTION | RF7_DROP_LITE | RF7_DROP_WRITING),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH |
	 RF7_HAS_FUR | RF7_HAS_HEAD | RF7_HAS_LEG | RF7_HAS_BLOOD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK | RF7_DROP_JEWELRY),
	(RF7_DROP_ARMOR | RF7_DROP_WEAPON | RF7_DROP_JEWELRY),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_TEETH | RF7_HAS_ARM |
	 RF7_HAS_LEG | RF7_HAS_HAND | RF7_HAS_HEAD | RF7_HAS_BLOOD |
	 RF7_DROP_FOOD | RF7_DROP_WEAPON | RF7_DROP_CLOTHES |
	 RF7_DROP_TOOL | RF7_DROP_POTION | RF7_DROP_LITE | RF7_DROP_MUSIC | RF7_DROP_JEWELRY),
/* u */ (RF7_HAS_SKULL |
	 RF7_DROP_WEAPON | RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_WRITING),
	(RF7_DROP_ARMOR | RF7_DROP_JEWELRY | RF7_DROP_WEAPON | RF7_DROP_TOOL),
	(RF7_HAS_CORPSE | RF7_HAS_SLIME |
	 RF7_DROP_FOOD | RF7_DROP_CLOTHES | RF7_DROP_JUNK),
	(RF7_DROP_JUNK),
	(RF7_HAS_CORPSE | RF7_HAS_SKULL | RF7_HAS_SKELETON | RF7_HAS_LEG |
	 RF7_HAS_SCALE | RF7_HAS_HEAD |
	 RF7_DROP_CLOTHES | RF7_DROP_FOOD | RF7_DROP_JUNK | RF7_DROP_WEAPON | RF7_DROP_CHEST),
/* z */ (RF7_DROP_ARMOR | RF7_DROP_WEAPON | RF7_DROP_JEWELRY)
};

/*
 * Object flags
 */
static cptr k_info_flags1[] =
{
	"STR",
	"INT",
	"WIS",
	"DEX",
	"CON",
	"CHR",
	"XXX1",
	"XXX2",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"BLOWS",
	"SHOTS",
	"MIGHT",
	"SLAY_NATURAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"KILL_DRAGON",
	"KILL_DEMON",
	"KILL_UNDEAD",
	"BRAND_POIS",
	"BRAND_ACID",
	"BRAND_ELEC",
	"BRAND_FIRE",
	"BRAND_COLD"
};

/*
 * Object flags
 */
static cptr k_info_flags2[] =
{
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD",
	"IGNORE_WATER",
	"IGNORE_THEFT",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_FEAR",
	"RES_LITE",
	"RES_DARK",
	"RES_BLIND",
	"RES_CONFU",
	"RES_SOUND",
	"RES_SHARD",
	"RES_NEXUS",
	"RES_NETHR",
	"RES_CHAOS",
	"RES_DISEN"
};

/*
 * Object flags
 */
static cptr k_info_flags3[] =
{
	"SLOW_DIGEST",
	"FEATHER",
	"LITE",
	"REGEN",
	"TELEPATHY",
	"SEE_INVIS",
	"FREE_ACT",
	"HOLD_LIFE",
	"ESP_DEMON",
	"ESP_DRAGON",
	"ESP_GIANT",
	"ESP_ORC",
	"ESP_TROLL",
	"ESP_UNDEAD",
	"ESP_NATURE",
	"IMPACT",
	"DRAIN_HP",
	"DRAIN_MANA",
	"DRAIN_EXP",
	"AGGRAVATE",
	"TELEPORT",
	"RANDOM",
	"ACTIVATE",
	"BLESSED",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"THROWING",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};

/*
 * Weapon style type
 */
static cptr w_info_style[] =
{
	"NONE",
	"UNARMED",
	"ONE-HANDED",
	"TWO-HANDED",
	"WEAPON&SHIELD",
	"TWO-WEAPON",
	"HAFTED",
	"SWORD",
	"POLEARM",
	"THROWN",
	"SLING",
	"BOW",
	"XBOW",
	"BACKSTAB",
	"MAGIC_BOOK",
	"PRAYER_BOOK",
	"SONG_BOOK",
	"INSTRUMENT",
	"POTION",
	"SCROLL",
	"AMULET",
	"RING",
	"WAND",
	"STAFF",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_ANIMAL",
	"SLAY_DEMON"
};


/*
 * Weapon style benefits
 */
static cptr w_info_benefit[] =
{
	"NONE",
	"HIT",
	"DAM",
	"AC",
	"XTRA_BLOW",
	"XTRA_SHOT",
	"XTRA_MIGHT",
	"CRITICAL",
	"ID",
	"POWER",
	"ICKY_HANDS",
	"ICKY_WIELD",
	"BLESSED",
	"HOLD_SONG",
	"RES_FEAR"
};


/*
 * Spell effect flags
 */
static cptr s_info_flags1[] =
{
	"DETECT_DOORS",
	"DETECT_TRAPS",
	"DETECT_STAIRS",
	"DETECT_WATER",
	"DETECT_GOLD",
	"DETECT_OBJECT",
	"DETECT_MAGIC",
	"DETECT_CURSE",
	"DETECT_MONSTER",
	"DETECT_EVIL",
	"DETECT_INVIS",
	"DETECT_ANIMAL",
	"DETECT_UNDEAD",
	"DETECT_DEMON",
	"MAP_AREA",
	"WIZ_LITE",
	"LITE_ROOM",
	"DARK_ROOM",
	"FORGET",
	"SELF_KNOW",
	"IDENT",
	"IDENT_PACK",
	"IDENT_SENSE",
	"IDENT_BONUS",
	"IDENT_RUMOR",
	"IDENT_FULLY",
	"ACQUIREMENT",
	"STAR_ACQUIREMENT",
	"ENCHANT_TOH",
	"ENCHANT_TOD",
	"ENCHANT_TOA",
	"ENCHANT_HIGH"
};

/*
 * Spell effect flags
 */
static cptr s_info_flags2[] =
{
	"AGGRAVATE",
	"CURSE_WEAPON",
	"CURSE_ARMOR",
	"CREATE_STAIR",
	"TELE_LEVEL",
	"ALTER_LEVEL",
	"BANISHMENT",
	"MASS_BANISHMENT",
	"CUT",
	"STUN",
	"POISON",
	"PARALYZE",
	"HALLUC",
	"SLOW",
	"BLIND",
	"CONFUSE",
	"FEAR",
	"INFRA",
	"HASTE",
	"HERO",
	"SHERO",
	"BLESS",
	"SHIELD",
	"INVULN",
	"SEE_INVIS",
	"PROT_EVIL",
	"RECALL",
	"OPP_FIRE",
	"OPP_COLD",
	"OPP_ACID",
	"OPP_ELEC",
	"OPP_POIS"
};

/*
 * Spell effect flags
 */
static cptr s_info_flags3[] =
{
	"INC_STR",
	"INC_INT",
	"INC_WIS",
	"INC_DEX",
	"INC_CON",
	"INC_CHR",
	"CURE_STR",
	"CURE_INT",
	"CURE_WIS",
	"CURE_DEX",
	"CURE_CON",
	"CURE_CHR",
	"INC_EXP",
	"CURE_EXP",
	"SLOW_MANA",
	"CURE_MANA",
	"SLOW_CURSE",
	"CURE_CURSE",
	"SLOW_POIS",
	"CURE_POIS",
	"SLOW_CUTS",
	"CURE_CUTS",
	"CURE_STUN",
	"CURE_CONF",
	"CURE_FOOD",
	"CURE_FEAR",
	"CURE_BLIND",
	"CURE_IMAGE",
	"DEC_FOOD",
	"DEC_EXP",
	"HOLD_SONG",
	"EVIL"
};

/*
 * Spell parametised effects
 */
static cptr s_info_types[] =
{
	"",
	"RECHARGE",
	"IDENT_TVAL",
	"ENCHANT_TVAL",
	"BRAND_WEAPON",
	"BRAND_ARMOR",
	"BRAND_ITEM",
	"BRAND_AMMO",
	"WARD_GLYPH",
	"WARD_TRAP",
	"SUMMON",
	"SUMMON_RACE",
	"CREATE_RACE",
	"CREATE_KIND",
	"EARTHQUAKE",
	"DESTRUCTION",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6",
	"XXX7",
	"XXX8",
	"INVEN_WIELD",
	"INVEN_BOW",
	"INVEN_LEFT",
	"INVEN_RIGHT",
	"INVEN_NECK",
	"INVEN_LITE",
	"INVEN_BODY",
	"INVEN_OUTER",
	"INVEN_ARM",
	"INVEN_HEAD",
	"INVEN_HANDS",
	"INVEN_FEET",
	"INVEN_BELT"
};



/*** Initialize from ascii template files ***/


/*
 * Initialize an "*_info" array, by parsing an ascii "template" file
 */
errr init_info_txt(FILE *fp, char *buf, header *head,
   parse_info_txt_func parse_info_txt_line)
{
	errr err;

	/* Not ready yet */
	bool okay = FALSE;

	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = 0;


	/* Prepare the "fake" stuff */
	head->name_size = 0;
	head->text_size = 0;

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf+2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != head->v_major) ||
				(v2 != head->v_minor) ||
				(v3 != head->v_patch))
			{
				return (PARSE_ERROR_OBSOLETE_FILE);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);

		/* Parse the line */
		if ((err = (*parse_info_txt_line)(buf, head)) != 0)
			return (err);
	}


	/* Complete the "name" and "text" sizes */
	if (head->name_size) head->name_size++;
	if (head->text_size) head->text_size++;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Add a text to the text-storage and store offset to it.
 *
 * Returns FALSE when there isn't enough space available to store
 * the text.
 */
static bool add_text(u32b *offset, header *head, cptr buf)
{
	/* Hack -- Verify space */
	if (head->text_size + strlen(buf) + 8 > z_info->fake_text_size)
		return (FALSE);

	/* New text? */
	if (*offset == 0)
	{
		/* Advance and save the text index */
		*offset = ++head->text_size;	
	}

	/* Append chars to the text */
	strcpy(head->text_ptr + head->text_size, buf);

	/* Advance the index */
	head->text_size += strlen(buf);

	/* Success */
	return (TRUE);
}


/*
 * Add a name to the name-storage and return an offset to it.
 *
 * Returns 0 when there isn't enough space available to store
 * the name.
 */
static u32b add_name(header *head, cptr buf)
{
	u32b index;

	/* Hack -- Verify space */
	if (head->name_size + strlen(buf) + 8 > z_info->fake_name_size)
		return (0);

	/* Advance and save the name index */
	index = ++head->name_size;

	/* Append chars to the names */
	strcpy(head->name_ptr + head->name_size, buf);

	/* Advance the index */
	head->name_size += strlen(buf);
	
	/* Return the name index */
	return (index);
}


/*
 * Initialize the "z_info" structure, by parsing an ascii "template" file
 */
errr parse_z_info(char *buf, header *head)
{
	maxima *z_info = head->info_ptr;

	/* Hack - Verify 'M:x:' format */
	if (buf[0] != 'M') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (!buf[2]) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (buf[3] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);


	/* Process 'D' for "Maximum d_info[] index" */
	if (buf[2] == 'D')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->d_max = max;
	}
	/* Process 'F' for "Maximum f_info[] index" */
	else if (buf[2] == 'F')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->f_max = max;
	}

	/* Process 'K' for "Maximum k_info[] index" */
	else if (buf[2] == 'K')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->k_max = max;
	}

	/* Process 'A' for "Maximum a_info[] index" */
	else if (buf[2] == 'A')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->a_max = max;
	}

	/* Process 'E' for "Maximum e_info[] index" */
	else if (buf[2] == 'E')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->e_max = max;
	}

	/* Process 'X' for "Maximum x_info[] index" */
	else if (buf[2] == 'X')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->x_max = max;
	}

	/* Process 'R' for "Maximum r_info[] index" */
	else if (buf[2] == 'R')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->r_max = max;
	}


	/* Process 'V' for "Maximum v_info[] index" */
	else if (buf[2] == 'V')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->v_max = max;
	}


	/* Process 'P' for "Maximum p_info[] index" */
	else if (buf[2] == 'P')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->p_max = max;
	}

	/* Process 'C' for "Maximum c_info[] index" */
	else if (buf[2] == 'C')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->c_max = max;
	}

	/* Process 'W' for "Maximum w_info[] index" */
	else if (buf[2] == 'W')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->w_max = max;
	}

	/* Process 'S' for "Maximum s_info[] index" */
	else if (buf[2] == 'S')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->s_max = max;
	}

	/* Process 'Y' for "Maximum y_info[] index" */
	else if (buf[2] == 'Y')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->y_max = max;
	}
	/* Process 'T' for "Maximum t_info[] index" */
	else if (buf[2] == 'T')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->t_max = max;
	}

	/* Process 'U' for "Maximum u_info[] index" */
	else if (buf[2] == 'U')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->u_max = max;
	}

	/* Process 'H' for "Maximum h_info[] index" */
	else if (buf[2] == 'H')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->h_max = max;
	}

	/* Process 'B' for "Maximum b_info[] subindex" */
	else if (buf[2] == 'B')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->b_max = max;
	}

	/* Process 'O' for "Maximum o_list[] index" */
	else if (buf[2] == 'O')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->o_max = max;
	}

	/* Process 'M' for "Maximum m_list[] index" */
	else if (buf[2] == 'M')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->m_max = max;
	}

	/* Process 'N' for "Fake name size" */
	else if (buf[2] == 'N')
	{
		long max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->fake_name_size = max;
	}

	/* Process 'I' for "Fake text size" */
	else if (buf[2] == 'I')
	{
		long max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->fake_text_size = max;
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr parse_v_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static vault_type *v_ptr = NULL;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		v_ptr = (vault_type*)head->info_ptr + i;

		/* Store the name */
		if (!(v_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current v_ptr */
		if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&v_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'X' for "Extra info" (one line only) */
	else if (buf[0] == 'X')
	{
		int typ, rat, hgt, wid;

		/* There better be a current v_ptr */
		if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &typ, &rat, &hgt, &wid)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		v_ptr->typ = typ;
		v_ptr->rat = rat;
		v_ptr->hgt = hgt;
		v_ptr->wid = wid;

		/* Check for maximum vault sizes */
		if ((v_ptr->typ == 7) && ((v_ptr->wid > 33) || (v_ptr->hgt > 22)))
			return (PARSE_ERROR_VAULT_TOO_BIG);

		if ((v_ptr->typ == 8) && ((v_ptr->wid > 66) || (v_ptr->hgt > 44)))
			return (PARSE_ERROR_VAULT_TOO_BIG);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one byte flag from a textual string
 */
static errr grab_one_bflag(byte *flags, cptr names[], cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 8; i++)
	{
		if (streq(what, names[i]))
		{
			*flags |= (1L << i);
			return (0);
		}
	}

	return (-1);
}


/*
 * Grab one flag from a textual string and convert to numeric
 */
static errr grab_one_offset(byte *offset, cptr names[], cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, names[i]))
		{
			*offset = *offset+i;
			return (0);
		}
	}

	*offset = *offset+32;
	return (-1);
}


/*
 * Grab one special flag in an desc_type from a textual string
 */
static errr grab_one_special_flag(desc_type *d_ptr, cptr what)
{
	if (grab_one_bflag(&d_ptr->flags, d_info_sflags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown room special flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one level flag in an desc_type from a textual string
 */
static errr grab_one_level_flag(desc_type *d_ptr, cptr what)
{
	if (grab_one_bflag(&d_ptr->l_flag, d_info_lflags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown room level flag '%s'.", what);

	/* Error */			
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab an race flag in an desc_type from a textual string
 */
static errr grab_one_room_race_flag(desc_type *d_ptr, cptr what)
{
	d_ptr->r_flag = 1;

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags1, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags2, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags3, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags4, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown room race flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the "d_info" array, by parsing an ascii "template" file
 */
errr parse_d_info(char *buf, header *head)
{
	int i;

	char *s,*t;

	/* Current entry */
	static desc_type *d_ptr = NULL;

	/* Process 'N' for "New/Number" */
	if (buf[0] == 'N')
	{
		int prv, nxt, prc, min;

		/* Hack - get the index */
		i = error_idx + 1;

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		d_ptr = (desc_type*)head->info_ptr + i;

		/* Scan for the values */
		if (4 != sscanf(buf, "N:%d:%d:%d:%d",
			    &prv, &nxt, &prc, &min)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		d_ptr->chart = prv;
		d_ptr->next = nxt;
		d_ptr->roll = prc;
		d_ptr->level = min;

		/* Initialize other values */
		d_ptr->flags = 0;
		d_ptr->tval = 0;
		d_ptr->feat = 0;
		d_ptr->r_flag = 0;
		d_ptr->r_char = 0;

	}
	/* Process 'A' for "Name1" */
	else if (buf[0] == 'A')
	{
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the name */
		if (!(d_ptr->name1 = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	/* Process 'B' for "Name2" */
	else if (buf[0] == 'B')
	{

		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the name */
		if (!(d_ptr->name2 = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&d_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	/* Hack -- Process 'S' for special flags */
	else if (buf[0] == 'S')
	{
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_special_flag(d_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}
	/* Hack -- Process 'L' for level flags */
	else if (buf[0] == 'L')
	{
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_level_flag(d_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		d_ptr->r_char = buf[2];
	}

	/* Process 'K' for "Kind" (one line only) */
	else if (buf[0] == 'K')
	{
		int kind;

		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d", &kind)) return (1);

		/* Save the values */
		d_ptr->tval = kind;
	}

	/* Process 'F' for "Feature" (one line only) */
	else if (buf[0] == 'F')
	{
		int feat;

		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d", &feat)) return (1);

		/* Save the values */
		d_ptr->feat = feat;

	}

	/* Process 'R' for "Race flag" (once only) */
	else if (buf[0] == 'R')
	{
		/* There better be a current f_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Set to the first field */
		s=buf+2;

		/* Parse this entry */
		if (0 != grab_one_room_race_flag(d_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one flag from a textual string
 */
static errr grab_one_flag(u32b *flags, cptr names[], cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, names[i]))
		{
			*flags |= (1L << i);
			return (0);
		}
	}

	return (-1);
}


/*
 * Grab one flag in an feature_type from a textual string
 */
static errr grab_one_feat_flag(feature_type *f_ptr, cptr what)
{
	if (grab_one_flag(&f_ptr->flags1, f_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&f_ptr->flags2, f_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&f_ptr->flags3, f_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown feature flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab an action in an feature_type from a textual string
 */
static errr grab_one_feat_action(feature_type *f_ptr, cptr what, int count)
{
	if (grab_one_offset(&f_ptr->state[count].action, f_info_flags1, what) == 0)
		return (0);

	if (grab_one_offset(&f_ptr->state[count].action, f_info_flags2, what) == 0)
		return (0);

	if (grab_one_offset(&f_ptr->state[count].action, f_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown feature action '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab an spell in an feature_type from a textual string
 */
static errr grab_one_feat_spell(feature_type *f_ptr, cptr what)
{
	f_ptr->spell = 96;

	if (grab_one_offset(&f_ptr->spell, r_info_flags4, what) == 0)
		return (0);

	if (grab_one_offset(&f_ptr->spell, r_info_flags5, what) == 0)
		return (0);

	if (grab_one_offset(&f_ptr->spell, r_info_flags6, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown feature spell '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr parse_f_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static feature_type *f_ptr = NULL;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		f_ptr = (feature_type*)head->info_ptr + i;

		/* Store the name */
		if (!(f_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Default "mimic" */
		f_ptr->mimic = i;

		/* Default "unseen" */
		f_ptr->unseen = i;

		/* Default "state change" -- if not specified */
		f_ptr->defaults = i;

		/* Default "states" */
		for (i = 0; i < MAX_FEAT_STATES; i++) f_ptr->state[i].action = FS_FLAGS_END;

		/* Hack -- handle graphics */
		/* Note that in a future version of Unangband, a preference 'Use special lighting
		 * for all features' will set this flag for all features, and the features that are
		 * dynamically lit in vanilla Angband will have this flag in terrain.txt.
		 */
		f_ptr->flags2 |= (FF2_ATTR_LITE);
	}

	/* Process 'M' for "Mimic" (one line only) */
	else if (buf[0] == 'M')
	{
		int mimic;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &mimic)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->mimic = mimic;
	}

	/* Process 'U' for "Unseen" (one line only) */
	else if (buf[0] == 'U')
	{
		int unseen;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &unseen)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->unseen = unseen;
	}

	/* Process 'O' for "Object" (one line only) */
	else if (buf[0] == 'O')
	{
		int k_idx;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &k_idx)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->k_idx = k_idx;
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		int tmp;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract the attr */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->d_attr = tmp;
		f_ptr->d_char = buf[2];
	}
	
	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* XXX XXX XXX Hack -- Read feature power */
			if (1 == sscanf(s, "POWER_%d", &i))
			{
				/* Extract a "frequency" */
				f_ptr->power =  i;

				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_feat_flag(f_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, priority,edge;
		
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
				&level, &rarity, &priority, &edge)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->level = level;
		f_ptr->rarity = rarity;
		f_ptr->priority = priority;
		f_ptr->edge = edge;
	}

	/* Process 'K' for "States" (up to four lines + default (which cannot be last)) */
	else if (buf[0] == 'K')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty state slot (if any) */
		for (i = 0; i < MAX_FEAT_STATES; i++) if (f_ptr->state[i].action == FS_FLAGS_END) break;

		/* Oops, no more slots */
		if (i == MAX_FEAT_STATES) return (PARSE_ERROR_GENERIC);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Is this default entry? */
		if (streq(s, "DEFAULT"))
		{
			/* Analyze result */
			f_ptr->defaults = atoi(t);
		}
		else
		{
			/* Reset */
			f_ptr->state[i].action = 0;
			
			/* Parse this entry */
			if (0 != grab_one_feat_action(f_ptr, s, i)) return (PARSE_ERROR_INVALID_FLAG);

			/* Analyze result */
			f_ptr->state[i].result = atoi(t);
		}
	}

	/* Process 'T' for "Traps" (one line only) */
	else if (buf[0] == 'T')
	{
		int n1;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze effect */
		for (n1 = 0; r_info_blow_effect[n1]; n1++)
		{
			if (streq(s, r_info_blow_effect[n1])) break;
		}

		/* Invalid effect */
		if (!r_info_blow_effect[n1]) return (PARSE_ERROR_GENERIC);

		/* Analyze the third field */
		for (s = t; *t && (*t != 'd'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Save the method */
		f_ptr->blow.method = RBM_TRAP;

		/* Save the effect */
		f_ptr->blow.effect = n1;

		/* Extract the damage dice and sides */
		f_ptr->blow.d_dice = atoi(s);
		f_ptr->blow.d_side = atoi(t);
	}

	/* Process 'S' for "Spell" (once only) */
	else if (buf[0] == 'S')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Set to the first field */
		s=buf+2;

		/* Parse this entry */
		if (0 != grab_one_feat_spell(f_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&f_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}

/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
	if (grab_one_flag(&k_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown object flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr parse_k_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static object_kind *k_ptr = NULL;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		k_ptr = (object_kind*)head->info_ptr + i;

		/* Store the name */
		if (!(k_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char sym;
		int tmp;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract the char */
		sym = buf[2];

		/* Extract the attr */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->d_attr = tmp;
		k_ptr->d_char = sym;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->tval = tval;
		k_ptr->sval = sval;
		k_ptr->pval = pval;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, extra, wgt;
		long cost;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			    &level, &extra, &wgt, &cost)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->level = level;
		k_ptr->extra = extra;
		k_ptr->weight = wgt;
		k_ptr->cost = cost;
	}

	/* Process 'A' for "Allocation" (one line only) */
	else if (buf[0] == 'A')
	{
		int i;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* XXX Simply read each number following a colon */
		for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
		{
			/* Sanity check */
			if (i > 3) return (PARSE_ERROR_TOO_MANY_ALLOCATIONS);

			/* Default chance */
			k_ptr->chance[i] = 1;

			/* Store the attack damage index */
			k_ptr->locale[i] = atoi(s+1);

			/* Find the slash */
			t = strchr(s+1, '/');

			/* Find the next colon */
			s = strchr(s+1, ':');

			/* If the slash is "nearby", use it */
			if (t && (!s || t < s))
			{
				int chance = atoi(t+1);
				if (chance > 0) k_ptr->chance[i] = chance;
			}
		}
	}

	/* Hack -- Process 'P' for "power" and such */
	else if (buf[0] == 'P')
	{
		int ac, hd1, hd2, th, td, ta;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			    &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

		k_ptr->ac = ac;
		k_ptr->dd = hd1;
		k_ptr->ds = hd2;
		k_ptr->to_h = th;
		k_ptr->to_d = td;
		k_ptr->to_a =  ta;
	}

	/* Process 'Y' for "Rune" (one line only) */
	else if (buf[0] == 'Y')
	{
		int runest,runesc;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
			    &runest,&runesc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->runest = runest;
		k_ptr->runesc = runesc;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_kind_flag(k_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&k_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
	if (grab_one_flag(&a_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown artifact flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr parse_a_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static artifact_type *a_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		a_ptr = (artifact_type*)head->info_ptr + i;

		/* Store the name */
		if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
		a_ptr->flags2 |= (TR2_IGNORE_MASK);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->tval = tval;
		a_ptr->sval = sval;
		a_ptr->pval = pval;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, wgt;
		long cost;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			    &level, &rarity, &wgt, &cost)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->level = level;
		a_ptr->rarity = rarity;
		a_ptr->weight = wgt;
		a_ptr->cost = cost;
	}

	/* Process 'P' for "power" and such */
	else if (buf[0] == 'P')
	{
		int ac, hd1, hd2, th, td, ta;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			    &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

		a_ptr->ac = ac;
		a_ptr->dd = hd1;
		a_ptr->ds = hd2;
		a_ptr->to_h = th;
		a_ptr->to_d = td;
		a_ptr->to_a = ta;
	}

	/* Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_artifact_flag(a_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'A' for "Activation & time" */
	else if (buf[0] == 'A')
	{
		int act, time, rand;

		/* Scan for the values */
		if (3 != sscanf(buf + 2, "%d:%d:%d",&act, &time, &rand)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->activation = act;
		a_ptr->time = time;
		a_ptr->randtime = rand;
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	if (grab_one_flag(&e_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown ego-item flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_obvious_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	if (grab_one_flag(&e_ptr->obv_flags1, k_info_flags1, what) == 0)
        {
                e_ptr->flags1 |= e_ptr->obv_flags1;
		return (0);
        }

	if (grab_one_flag(&e_ptr->obv_flags2, k_info_flags2, what) == 0)
        {
                e_ptr->flags2 |= e_ptr->obv_flags2;
		return (0);
        }

	if (grab_one_flag(&e_ptr->obv_flags3, k_info_flags3, what) == 0)
        {
                e_ptr->flags3 |= e_ptr->obv_flags3;
		return (0);
        }

	/* Oops */
	msg_format("Unknown ego-item flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr parse_e_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static ego_item_type *e_ptr = NULL;

	static int cur_t = 0;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		e_ptr = (ego_item_type*)head->info_ptr + i;

		/* Store the name */
		if (!(e_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Start with the first of the tval indices */
		cur_t = 0;

                /* Fix initialisation */
                e_ptr->aware = 0;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, pad2;
		long cost;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			    &level, &rarity, &pad2, &cost)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->level = level;
		e_ptr->rarity = rarity;
		/* e_ptr->weight = wgt; */
		e_ptr->cost = cost;
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		int slot, rating, xtra;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &slot, &rating, &xtra)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->slot = slot;
		e_ptr->rating = rating;
		e_ptr->xtra = xtra;
	}

	/* Process 'T' for "Types allowed" (up to three lines) */
	else if (buf[0] == 'T')
	{
		int tval, sval1, sval2;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &tval, &sval1, &sval2)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->tval[cur_t] = (byte)tval;
		e_ptr->min_sval[cur_t] = (byte)sval1;
		e_ptr->max_sval[cur_t] = (byte)sval2;

		/* increase counter for 'possible tval' index */
		cur_t++;

		/* only three T: lines allowed */
		if (cur_t > 3) return (PARSE_ERROR_GENERIC);
	}

	/* Hack -- Process 'C' for "creation" */
	else if (buf[0] == 'C')
	{
		int th, td, ta, pv;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &th, &td, &ta, &pv)) return (PARSE_ERROR_GENERIC);

		e_ptr->max_to_h = th;
		e_ptr->max_to_d = td;
		e_ptr->max_to_a = ta;
		e_ptr->max_pval = pv;
	}

	/* Process 'Y' for "Rune" (one line only) */
	else if (buf[0] == 'Y')
	{
		int runest,runesc;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
			    &runest,&runesc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->runest = runest;
		e_ptr->runesc = runesc;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_ego_item_flag(e_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Hack -- Process 'O' for obvious flags */
	else if (buf[0] == 'O')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_obvious_ego_item_flag(e_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the "x_info" array, by parsing an ascii "template" file
 */
errr parse_x_info(char *buf, header *head)
{
	int i;
	
	/* Current entry */
	static flavor_type *x_ptr;


	/* Process 'N' for "Number" */
	if (buf[0] == 'N')
	{
		int tval, sval;
		int result;

		/* Scan the value */
		result = sscanf(buf, "N:%d:%d:%d", &i, &tval, &sval);

		/* Either two or three values */
		if ((result != 2) && (result != 3)) return (PARSE_ERROR_GENERIC);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		x_ptr = (flavor_type*)head->info_ptr + i;

		/* Save the tval */
		x_ptr->tval = (byte)tval;

		/* Save the sval */
		if (result == 2)
		{
			/* Megahack - unknown sval */
			x_ptr->sval = SV_UNKNOWN;
		}
		else
			x_ptr->sval = (byte)sval;
	}

	/* Process 'G' for "Graphics" */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current x_ptr */
		if (!x_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract d_char */
		d_char = buf[2];

		/* If we have a longer string than expected ... */
		if (buf[5])
		{
			/* Advance "buf" on by 4 */
			buf += 4;

			/* Extract the colour */
			d_attr = color_text_to_attr(buf);
		}
		else
		{
			/* Extract the attr */
			d_attr = color_char_to_attr(buf[4]);
		}

		/* Paranoia */
		if (d_attr < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		x_ptr->d_attr = d_attr;
		x_ptr->d_char = d_char;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current x_ptr */
		if (!x_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[1]) return (PARSE_ERROR_GENERIC);
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Store the text */
		if (!add_text(&x_ptr->text, head, buf + 2))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags1, r_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags2, r_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags3, r_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one (spell) flag in a monster_race from a textual string
 */
static errr grab_one_spell_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags4, r_info_flags4, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags5, r_info_flags5, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags6, r_info_flags6, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}




/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr parse_r_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static monster_race *r_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		r_ptr = (monster_race*)head->info_ptr + i;

		/* Store the name */
		if (!(r_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&(r_ptr->text), head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char sym;
		int tmp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract the char */
		sym = buf[2];

		/* Extract the attr */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->d_attr = tmp;
		r_ptr->d_char = sym;

		/* Hack -- set some rf7 flags */
		if ((r_ptr->d_char >='A') && (r_ptr->d_char <='Z'))
		{
			r_ptr->flags7 |= hack_rf7_flags[r_ptr->d_char-'A'];
		}

		/* Hack -- set some rf7 flags */
		if ((r_ptr->d_char >='a') && (r_ptr->d_char <='z'))
		{
			r_ptr->flags7 |= hack_rf7_flags[r_ptr->d_char-'a'+26];
		}

		/* Hack -- nonliving monsters */
		/* Death by Physical attack -- non-living monster */
		if (strchr("Evg", r_ptr->d_char))
		{
			r_ptr->flags3 |= RF3_NONLIVING;
		}

	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp1, hp2, aaf, ac, slp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			    &spd, &hp1, &hp2, &aaf, &ac, &slp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->speed = spd;
		r_ptr->hdice = hp1;
		r_ptr->hside = hp2;
		r_ptr->aaf = aaf;
		r_ptr->ac = ac;
		r_ptr->sleep = slp;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, pad;
		long exp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			    &lev, &rar, &pad, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
		r_ptr->extra = pad;
		r_ptr->mexp = exp;
	}

	/* Process 'B' for "Blows" (up to four lines) */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < 4; i++) if (!r_ptr->blow[i].method) break;

		/* Oops, no more slots */
		if (i == 4) return (PARSE_ERROR_GENERIC);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the method */
		for (n1 = 0; r_info_blow_method[n1]; n1++)
		{
			if (streq(s, r_info_blow_method[n1])) break;
		}

		/* Invalid method */
		if (!r_info_blow_method[n1]) return (PARSE_ERROR_GENERIC);

		/* Hack -- update the rf7 flags */
		switch (n1)
		{
			case RBM_CLAW:
				r_ptr->flags7 |= RF7_HAS_CLAW;
				break;
			case RBM_SPORE:
				r_ptr->flags7 |= RF7_HAS_SPORE;
				break;
			case RBM_SHOOT:
				r_ptr->flags7 |= RF7_DROP_MISSILE;
				break;
		}

		/* Analyze the second field */
		for (s = t; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze effect */
		for (n2 = 0; r_info_blow_effect[n2]; n2++)
		{
			if (streq(s, r_info_blow_effect[n2])) break;
		}

		/* Invalid effect */
		if (!r_info_blow_effect[n2]) return (PARSE_ERROR_GENERIC);

		/* Analyze the third field */
		for (s = t; *t && (*t != 'd'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Save the method */
		r_ptr->blow[i].method = n1;

		/* Save the effect */
		r_ptr->blow[i].effect = n2;

		/* Extract the damage dice and sides */
		r_ptr->blow[i].d_dice = atoi(s);
		r_ptr->blow[i].d_side = atoi(t);
	}

	/* Process 'F' for "Basic Flags" (multiple lines) */
	else if (buf[0] == 'F')
	{
		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_basic_flag(r_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'S' for "Spell Flags" (multiple lines) */
	else if (buf[0] == 'S')
	{
		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* XXX Hack -- Read spell frequency */
			if (1 == sscanf(s, "1_IN_%d", &i))
			{
				/* Sanity check */
				if ((i < 1) || (i > 100))
					return (PARSE_ERROR_INVALID_SPELL_FREQ);

				/* Extract a "frequency" */
				r_ptr->freq_spell = r_ptr->freq_innate = 100 / i;

				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_spell_flag(r_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

#if 0
	/* XXX XXX XXX The ghost is unused */

	/* Mega-Hack -- acquire "ghost" */
	r_ptr = &r_info[z_info->r_max-1];

	/* Get the next index */
	r_ptr->name = head->name_size;
	r_ptr->text = head->text_size;

	/* Save some space for the ghost info */
	head->name_size += 64;
	head->text_size += 64;

	/* Hack -- Default name/text for the ghost */
	strcpy(r_name + r_ptr->name, "Nobody, the Undefined Ghost");
	strcpy(r_text + r_ptr->text, "It seems strangely familiar...");

	/* Hack -- set the attr/char info */
	r_ptr->d_attr = r_ptr->x_attr = TERM_WHITE;
	r_ptr->d_char = r_ptr->x_char = 'G';

	/* Hack -- Try to prevent a few "potential" bugs */
	r_ptr->flags1 |= (RF1_UNIQUE);

	/* Hack -- Try to prevent a few "potential" bugs */
	r_ptr->flags1 |= (RF1_NEVER_MOVE | RF1_NEVER_BLOW);

	/* Hack -- Try to prevent a few "potential" bugs */
	r_ptr->hdice = r_ptr->hside = 1;

	/* Hack -- Try to prevent a few "potential" bugs */
	r_ptr->mexp = 1L;
#endif

	/* Success */
	return (0);
}


/*
 * Grab one flag in a player_race from a textual string
 */
static errr grab_one_racial_flag(player_race *pr_ptr, cptr what)
{
	if (grab_one_flag(&pr_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&pr_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&pr_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown player flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "p_info" array, by parsing an ascii "template" file
 */
errr parse_p_info(char *buf, header *head)
{
	int i, j;

	char *s, *t;

	/* Current entry */
	static player_race *pr_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		pr_ptr = (player_race*)head->info_ptr + i;

		/* Store the name */
		if (!(pr_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'S' for "Stats" (one line only) */
	else if (buf[0] == 'S')
	{
		int adj;

		/* There better be a current pc_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = strchr(s, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			pr_ptr->r_adj[j] = adj;

			/* Next... */
			continue;
		}
	}

	/* Process 'R' for "Racial Skills" (one line only) */
	else if (buf[0] == 'R')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			    &dis, &dev, &sav, &stl,
			    &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pr_ptr->r_dis = dis;
		pr_ptr->r_dev = dev;
		pr_ptr->r_sav = sav;
		pr_ptr->r_stl = stl;
		pr_ptr->r_srh = srh;
		pr_ptr->r_fos = fos;
		pr_ptr->r_thn = thn;
		pr_ptr->r_thb = thb;
	}

	/* Process 'X' for "Extra Info" (one line only) */
	else if (buf[0] == 'X')
	{
		int mhp, exp, infra;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &mhp, &exp, &infra)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pr_ptr->r_mhp = mhp;
		pr_ptr->r_exp = exp;
		pr_ptr->infra = infra;
	}

	/* Hack -- Process 'I' for "info" and such */
	else if (buf[0] == 'I')
	{
		int hist, b_age, m_age;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &hist, &b_age, &m_age)) return (PARSE_ERROR_GENERIC);

		pr_ptr->hist = hist;
		pr_ptr->b_age = b_age;
		pr_ptr->m_age = m_age;
	}

	/* Hack -- Process 'H' for "Height" */
	else if (buf[0] == 'H')
	{
		int m_b_ht, m_m_ht, f_b_ht, f_m_ht;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &m_b_ht, &m_m_ht, &f_b_ht, &f_m_ht)) return (PARSE_ERROR_GENERIC);

		pr_ptr->m_b_ht = m_b_ht;
		pr_ptr->m_m_ht = m_m_ht;
		pr_ptr->f_b_ht = f_b_ht;
		pr_ptr->f_m_ht = f_m_ht;
	}

	/* Hack -- Process 'W' for "Weight" */
	else if (buf[0] == 'W')
	{
		int m_b_wt, m_m_wt, f_b_wt, f_m_wt;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &m_b_wt, &m_m_wt, &f_b_wt, &f_m_wt)) return (PARSE_ERROR_GENERIC);

		pr_ptr->m_b_wt = m_b_wt;
		pr_ptr->m_m_wt = m_m_wt;
		pr_ptr->f_b_wt = f_b_wt;
		pr_ptr->f_m_wt = f_m_wt;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_racial_flag(pr_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Hack -- Process 'C' for class choices */
	else if (buf[0] == 'C')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Hack - Parse this entry */
			pr_ptr->choice |= (1 << atoi(s));

			/* Start the next entry */
			s = t;
		}
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the "c_info" array, by parsing an ascii "template" file
 */
errr parse_c_info(char *buf, header *head)
{
	int i, j;

	char *s;

	/* Current entry */
	static player_class *pc_ptr = NULL;

	static int cur_title = 0;
	static int cur_equip = 0;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		pc_ptr = (player_class*)head->info_ptr + i;

		/* Store the name */
		if (!(pc_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* No titles and equipment yet */
		cur_title = 0;
		cur_equip = 0;
	}

	/* Process 'S' for "Stats" (one line only) */
	else if (buf[0] == 'S')
	{
		int adj;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = strchr(s, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			pc_ptr->c_adj[j] = adj;

			/* Next... */
			continue;
		}
	}

	/* Process 'C' for "Class Skills" (one line only) */
	else if (buf[0] == 'C')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			    &dis, &dev, &sav, &stl,
			    &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_dis = dis;
		pc_ptr->c_dev = dev;
		pc_ptr->c_sav = sav;
		pc_ptr->c_stl = stl;
		pc_ptr->c_srh = srh;
		pc_ptr->c_fos = fos;
		pc_ptr->c_thn = thn;
		pc_ptr->c_thb = thb;
	}

	/* Process 'X' for "Extra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			    &dis, &dev, &sav, &stl,
			    &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->x_dis = dis;
		pc_ptr->x_dev = dev;
		pc_ptr->x_sav = sav;
		pc_ptr->x_stl = stl;
		pc_ptr->x_srh = srh;
		pc_ptr->x_fos = fos;
		pc_ptr->x_thn = thn;
		pc_ptr->x_thb = thb;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int mhp, exp, sense_div, sense_heavy, sense_squared;
		long sense_base;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%ld:%d:%d:%d",
			    &mhp, &exp, &sense_base, &sense_div, &sense_heavy, &sense_squared))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_mhp = mhp;
		pc_ptr->c_exp = exp;
		pc_ptr->sense_base = sense_base;
		pc_ptr->sense_div = sense_div;
		pc_ptr->sense_heavy = sense_heavy;
		pc_ptr->sense_squared = sense_squared;
	}

	/* Process 'A' for "Attack Info" (one line only) */
	else if (buf[0] == 'A')
	{
		int max_attacks, min_weight, att_multiply;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			    &max_attacks, &min_weight, &att_multiply))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->max_attacks = max_attacks;
		pc_ptr->min_weight = min_weight;
		pc_ptr->att_multiply = att_multiply;
	}

	/* Process 'M' for "Magic Info" (one line only) */
	else if (buf[0] == 'M')
	{
		int spell_book, spell_stat, spell_first, spell_weight, spell_power;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
		&spell_book, &spell_stat,
		&spell_first, &spell_weight, &spell_power))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->spell_book = spell_book;
		pc_ptr->spell_stat = spell_stat;
		pc_ptr->spell_first = spell_first;
		pc_ptr->spell_weight = spell_weight;
		pc_ptr->spell_power = spell_power;
	}

	/* Process 'T' for "Titles" */
	else if (buf[0] == 'T')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&pc_ptr->title[cur_title], head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
		
		/* Next title */
		cur_title++;

		/* Limit number of titles */
		if (cur_title > PY_MAX_LEVEL / 5)
			return (PARSE_ERROR_TOO_MANY_ARGUMENTS);
	}

	/* Process 'E' for "Starting Equipment" */
	else if (buf[0] == 'E')
	{
		int tval, sval, min, max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &tval, &sval, &min, &max)) return (PARSE_ERROR_GENERIC);

		if ((min < 0) || (max < 0) || (min > 99) || (max > 99))
			return (PARSE_ERROR_INVALID_ITEM_NUMBER);

		/* Save the values */
		e_ptr->tval = tval;
		e_ptr->sval = sval;
		e_ptr->min = min;
		e_ptr->max = max;

		/* Next item */
		cur_equip++;

		/* Limit number of starting items */
		if (cur_equip > MAX_START_ITEMS)
			return (PARSE_ERROR_GENERIC);
	}

	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one style in a weapon style from a textual string
 */
static errr grab_one_style(weapon_style *w_ptr, cptr what)
{
	int i;

	/* Check styles */
	for (i = 0; i < MAX_WEAP_STYLES; i++)
	{
		if (streq(what, w_info_style[i]))
		{
			w_ptr->styles |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown weapon style '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one style in a weapon style from a textual string
 */
static errr grab_one_benefit(weapon_style *w_ptr, cptr what)
{
	int i;

	/* Check styles */
	for (i = 0; i < MAX_WEAP_BENEFITS; i++)
	{
		if (streq(what, w_info_benefit[i]))
		{
			w_ptr->benefit =i;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown weapon benefit '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "w_info" array, by parsing an ascii "template" file
 */
errr parse_w_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static weapon_style *w_ptr = NULL;

	/* Process 'W' for "Class/Level" */
	if (buf[0] == 'W')
	{
		int class,level;

		/* Hack - get the index */
		i = error_idx + 1;

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &class,&level)) return (PARSE_ERROR_GENERIC);

		/* Point at the "info" */
		w_ptr = (weapon_style*)head->info_ptr + i;

		w_ptr->class = class;
		w_ptr->level = level;
		w_ptr->styles = 0;
	}

	/* Hack -- Process 'S' for styles */
	else if (buf[0] == 'S')
	{
		/* There better be a current w_ptr */
		if (!w_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_style(w_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Hack -- Process 'B' for benefits */
	else if (buf[0] == 'B')
	{
		/* There better be a current w_ptr */
		if (!w_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_benefit(w_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}

	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Initialize the "h_info" array, by parsing an ascii "template" file
 */
errr parse_h_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static hist_type *h_ptr = NULL;


	/* Process 'N' for "New/Number" */
	if (buf[0] == 'N')
	{
		int prv, nxt, prc, soc;

		/* Hack - get the index */
		i = error_idx + 1;

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		h_ptr = (hist_type*)head->info_ptr + i;

		/* Scan for the values */
		if (4 != sscanf(buf, "N:%d:%d:%d:%d",
			    &prv, &nxt, &prc, &soc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		h_ptr->chart = prv;
		h_ptr->next = nxt;
		h_ptr->roll = prc;
		h_ptr->bonus = soc;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current h_ptr */
		if (!h_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&h_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one flag in an spell_type from a textual string
 */
static errr grab_one_cast_flag(spell_type *s_ptr, cptr what)
{
	if (grab_one_flag(&s_ptr->flags1, s_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&s_ptr->flags2, s_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&s_ptr->flags3, s_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown spell flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr parse_s_info(char *buf, header *head)
{
	int i;

	char *s,*t;

	/* Current entry */
	static spell_type *s_ptr = NULL;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		s_ptr = (spell_type*)head->info_ptr + i;

		/* Store the name */
		if (!(s_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'A' for "Appears in" (up to five lines) */
	else if (buf[0] == 'A')
	{
		int tval,sval,slot;

		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty appears slot (if any) */
		for (i = 0; i < MAX_SPELL_APPEARS; i++) if (!s_ptr->appears[i].tval) break;

		/* Check bounds */
		if (i==MAX_SPELL_APPEARS) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",&tval,&sval,&slot)) return (PARSE_ERROR_GENERIC);

		/* Extract the damage dice and sides */
		s_ptr->appears[i].tval = tval;
		s_ptr->appears[i].sval = sval;
		s_ptr->appears[i].slot = slot;
	}

	/* Process 'C' for "Cast by" (up to five lines) */
	else if (buf[0] == 'C')
	{
		int class,level,mana,fail,min;

		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty appears slot (if any) */
		for (i = 0; i < MAX_SPELL_CASTERS; i++) if (!s_ptr->cast[i].class) break;

		/* Check bounds */
		if (i==MAX_SPELL_APPEARS) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
			&class,&level,&mana,&fail,&min)) return (PARSE_ERROR_GENERIC);

		/* Extract the damage dice and sides */
		s_ptr->cast[i].class = class;
		s_ptr->cast[i].level = level;
		s_ptr->cast[i].mana = mana;
		s_ptr->cast[i].fail = fail;
		s_ptr->cast[i].min = min;

	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_cast_flag(s_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}

	}

	/* Process 'B' for "Blows" (up to four lines) */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < 4; i++) if (!s_ptr->blow[i].method) break;

		/* Oops, no more slots */
		if (i == 4) return (PARSE_ERROR_GENERIC);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the method */
		for (n1 = 0; r_info_blow_method[n1]; n1++)
		{
			if (streq(s, r_info_blow_method[n1])) break;
		}

		/* Invalid method */
		if (!r_info_blow_method[n1]) return (PARSE_ERROR_GENERIC);

		/* Analyze the second field */
		for (s = t; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze effect */
		for (n2 = 0; r_info_blow_effect[n2]; n2++)
		{
			if (streq(s, r_info_blow_effect[n2])) break;
		}

		/* Invalid effect */
		if (!r_info_blow_effect[n2]) return (PARSE_ERROR_GENERIC);

		/* Analyze the third field */
		for (s = t; *t && (*t != 'd'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Save the method */
		s_ptr->blow[i].method = n1;

		/* Save the effect */
		s_ptr->blow[i].effect = n2;

		/* Extract the damage dice */
		s_ptr->blow[i].d_dice = atoi(s);

		/* Analyze the fourth field */
		for (s = t; *t && (*t != '+'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Extract the damage sides and plus */
		s_ptr->blow[i].d_side = atoi(s);
		s_ptr->blow[i].d_plus = atoi(t);

	}

	/* Process 'S' for "Spell" */
	else if (buf[0] == 'S')
	{
		int n1;

		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the type */
		for (n1 = 0; s_info_types[n1]; n1++)
		{
			if (streq(s, s_info_types[n1])) break;
		}

		/* Invalid type */
		if (!s_info_types[n1]) return (PARSE_ERROR_GENERIC);

		/* Store the type */
		s_ptr->type=n1;

		/* Store the parameter */
		s_ptr->param=atoi(t); 

	}

	/* Process 'L' for "Lasts" */
	else if (buf[0] == 'L')
	{
		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != 'd'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Extract the lasts dice */
		s_ptr->l_dice = atoi(s);

		/* Analyze the second field */
		for (s = t; *t && (*t != '+'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Extract the damage sides and plus */
		s_ptr->l_side = atoi(s);
		s_ptr->l_plus = atoi(t);

	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&(s_ptr->text), head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


/*
 * Grab one flag in a rune_type from a textual string
 */
static bool grab_one_rune_flag(rune_type *y_ptr, cptr what, int count)
{
	if (grab_one_offset(&y_ptr->flag[count], k_info_flags1, what) == 0)
		return (0);

	if (grab_one_offset(&y_ptr->flag[count], k_info_flags2, what) == 0)
		return (0);

	if (grab_one_offset(&y_ptr->flag[count], k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown rune flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "y_info" array, by parsing an ascii "template" file
 */
errr parse_y_info(char *buf, header *head)
{
	int i;

	char *s,*t;

	/* Current entry */
	static rune_type *y_ptr = NULL;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		y_ptr = (rune_type*)head->info_ptr + i;

		/* Store the name */
		if (!(y_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'Y' for "Rune flag" up to four lines */
	else if (buf[0] == 'Y')
	{
		/* There better be a current y_ptr */
		if (!y_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < 4; i++) if (!y_ptr->count[i]) break;

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Get the count */
		y_ptr->count[i] = atoi(s);

		/* Parse this entry */
		if (0 != grab_one_rune_flag(y_ptr, t, i)) return (PARSE_ERROR_INVALID_FLAG);
	}


	/* Process 'B' for "Blows" (up to four lines) */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current y_ptr */
		if (!y_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < 4; i++) if (!y_ptr->blow[i].method) break;

		/* Oops, no more slots */
		if (i == 4) return (PARSE_ERROR_GENERIC);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the method */
		for (n1 = 0; r_info_blow_method[n1]; n1++)
		{
			if (streq(s, r_info_blow_method[n1])) break;
		}

		/* Invalid method */
		if (!r_info_blow_method[n1]) return (PARSE_ERROR_GENERIC);


		/* Analyze the second field */
		for (s = t; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze effect */
		for (n2 = 0; r_info_blow_effect[n2]; n2++)
		{
			if (streq(s, r_info_blow_effect[n2])) break;
		}

		/* Invalid effect */
		if (!r_info_blow_effect[n2]) return (PARSE_ERROR_GENERIC);

		/* Analyze the third field */
		for (s = t; *t && (*t != 'd'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Save the method */
		y_ptr->blow[i].method = n1;

		/* Save the effect */
		y_ptr->blow[i].effect = n2;

		/* Extract the damage dice */
		y_ptr->blow[i].d_dice = atoi(s);

		/* Analyze the fourth field */
		for (s = t; *t && (*t != '+'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == 'd') *t++ = '\0';

		/* Extract the damage sides and plus */
		y_ptr->blow[i].d_side = atoi(s);
		y_ptr->blow[i].d_plus = atoi(t);

	}
	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current y_ptr */
		if (!y_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&(y_ptr->text), head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}

/*
 * Grab an race flag in an town_type from a textual string
 */
static errr grab_one_town_race_flag(town_type *t_ptr, cptr what)
{
	t_ptr->r_flag = 1;

	if (grab_one_offset(&t_ptr->r_flag, r_info_flags1, what) == 0)
		return (0);

	if (grab_one_offset(&t_ptr->r_flag, r_info_flags2, what) == 0)
		return (0);

	if (grab_one_offset(&t_ptr->r_flag, r_info_flags3, what) == 0)
		return (0);

	if (grab_one_offset(&t_ptr->r_flag, r_info_flags4, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown town race flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "t_info" array, by parsing an ascii "template" file
 */
errr parse_t_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static town_type *t_ptr = NULL;

	/* Zone number */
	static int zone = 0;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		t_ptr = (town_type*)head->info_ptr + i;

		/* Store the name */
		if (!(t_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Reset the counters */
		zone = 0;


	}
	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		t_ptr->r_char = buf[2];
	}

	/* Process 'R' for "Race flag" (once only) */
	else if (buf[0] == 'R')
	{
		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Set to the first field */
		s=buf+2;

		/* Parse this entry */
		if (0 != grab_one_town_race_flag(t_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		int nearby,distant;

		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &nearby, &distant)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		t_ptr->nearby=nearby;
		t_ptr->distant = distant;

	}

	/* Process 'L' for "Levels" (up to four lines) */
	else if (buf[0] == 'L')
	{
		int level,fill,big,small,guard,tower;

		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Oops, no more slots */
		if (zone == 4) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
			&level, &fill, &big, &small, &guard, &tower)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		t_ptr->zone[zone].level=level;
		t_ptr->zone[zone].fill = fill;
		t_ptr->zone[zone].big = big;
		t_ptr->zone[zone].small = small;
		t_ptr->zone[zone].guard = guard;
		t_ptr->zone[zone].tower = tower;

		/* Find the next empty zone slot (if any) */
		zone++;

	}

	/* Process 'S' for "Stores" */
	else if (buf[0] == 'S')
	{
		int store1,store2,store3,store4,store5,store6,store7,store8;

		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			&store1, &store2, &store3, &store4, &store5, &store6, &store7, &store8)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		t_ptr->store[0] = store1;
		t_ptr->store[1] = store2;
		t_ptr->store[2] = store3;
		t_ptr->store[3] = store4;
		t_ptr->store[4] = store5;
		t_ptr->store[5] = store6;
		t_ptr->store[6] = store7;
		t_ptr->store[7] = store8;

	}
	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&t_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}



/*
 * Initialize the "u_info" array, by parsing an ascii "template" file
 */
errr parse_u_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static store_type *u_ptr = NULL;

	static int cur_t = 0;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		u_ptr = (store_type*)head->info_ptr + i;

		/* Store the name */
		if (!(u_ptr->name = add_name(head, s))) return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Reset the store */
		cur_t = 0;
	}

	/* Process 'O' for "Offered" (up to thirty two lines) */
	else if (buf[0] == 'O')
	{
		int tval, sval, count;

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d", &tval, &sval, &count)) return (PARSE_ERROR_GENERIC);

		/* only thirty two O: lines allowed */
		if (cur_t >= STORE_CHOICES) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		u_ptr->tval[cur_t] = (byte)tval;
		u_ptr->sval[cur_t] = (byte)sval;
		u_ptr->count[cur_t] = (byte)count;

		/* increase counter for 'possible tval' index */
		cur_t++;
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);}




/*
 * Initialize the "b_info" array, by parsing an ascii "template" file
 */
errr parse_b_info(char *buf, header *head)
{
	int i, j;

	char *s, *t;

	/* Current entry */
	static owner_type *ot_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the subindex */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the subindex */
		*s++ = '\0';

		/* Get the index */
		i = atoi(buf+2);

		/* Find the colon before the name */
		t = strchr(s, ':');

		/* Verify that colon */
		if (!t) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*t++ = '\0';

		/* Paranoia -- require a name */
		if (!*t) return (PARSE_ERROR_GENERIC);

		/* Get the subindex */
		j = atoi(s);

		/* Verify information */
		if (j >= z_info->b_max) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Get the *real* index */
		i = (i * z_info->b_max) + j;

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		ot_ptr = (owner_type*)head->info_ptr + i;

		/* Store the name */
		if (!(ot_ptr->owner_name = add_name(head, t)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int idx, gld, max, min, hgl, tol;

		/* There better be a current ot_ptr */
		if (!ot_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
			    &idx, &gld, &max, &min, &hgl, &tol)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		ot_ptr->owner_race = idx;
		ot_ptr->max_cost = gld;
		ot_ptr->max_inflate = max;
		ot_ptr->min_inflate = min;
		ot_ptr->haggle_per = hgl;
		ot_ptr->insult_max = tol;
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}




/*
 * Initialize the "g_info" array, by parsing an ascii "template" file
 */
errr parse_g_info(char *buf, header *head)
{
	int i, j;

	char *s;

	/* Current entry */
	static byte *g_ptr;


	/* Process 'A' for "Adjustments" */
	if (buf[0] == 'A')
	{
		int adj;

		/* Start the string */
		s = buf+1;

		/* Initialize the counter to max races */
		j = z_info->p_max;

		/* Repeat */
		while (j-- > 0)
		{
			/* Hack - get the index */
			i = error_idx + 1;

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			g_ptr = (byte*)head->info_ptr + i;

			/* Find the colon before the subindex */
			s = strchr(s, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			*g_ptr = adj;
		}
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}



#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */
