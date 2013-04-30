/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
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
	"HEAR",
	"ENTERED",
	"QUEST",
	"LITE",
	"DARK",
	"SURFACE",
	"BOTTOM",
	"DAYLITE",
	"ICKY",
	"BLOODY",
	"CURSED",
	"GLOOMY",
	"PORTAL",
	"SILENT",
	"STATIC",
	"STATIS",
	"SEALED",
	"HIDDEN",
	"ANCHOR",
	"ECHOES",
	"STENCH",
	"NOISY",
	"WINDY",
	"GRAVE",
	"STORE",
	"DISPEL",
	"RANDOM",
	"PUZZLE",
	"LAIR",
	"OBJECT",
	"TRAP"
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
	"SING",
	"TRAP",
	"BOULDER",
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
	"EXPLODE",
	"ARROW",
	"XBOLT",
	"SPIKE",
	"DART",
	"SHOT",
	"ARC_20",
	"ARC_30",
	"ARC_60",
	"FLASK",
	NULL
};


/*
 * Monster/Trap/Spell Blow Effects
 */
static cptr r_info_blow_effect[] =
{
	"",
	"STORM",
	"WIND",
	"HELLFIRE",
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
	"ANIM_DEAD",
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
	"ANIM_ELEMENT",
	"AWAY_UNDEAD",
	"AWAY_EVIL",
	"AWAY_ALL",
	"TURN_UNDEAD",
	"TURN_EVIL",
	"FEAR_WEAK",
	"DISPEL_UNDEAD",
	"DISPEL_EVIL",
	"DISPEL_ALL",
	"ANIM_OBJECT",
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
	"BLIND_WEAK",
	"RAISE_DEAD",
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
	"LAVA",
	"WATER",
	"FLAVOR",
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
	"LIVING",
	"TREE",
	"NEED_TREE",
	"ATTR_LITE",
	"ATTR_ITEM",
	"ATTR_DOOR",
	"ATTR_WALL",
	"INSTANT",
	"ADJACENT",
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
	"FRIEND",
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
	"NONVOCAL",
	"NONLIVING",
	"HURT_LITE",
	"HURT_ROCK",
	"PLANT",
	"INSECT",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	"RES_WATER",
	"RES_NETHR",
	"RES_LAVA",
	"RES_PLAS",
	"RES_NEXUS",
	"RES_DISEN",
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
	"BLOW_1",
	"BLOW_2",
	"BLOW_3",
	"BLOW_4",
	"SHRIEK",
	"QUAKE",
	"EXPLODE",
	"AURA",
	"BRTH_ACID",
	"BRTH_ELEC",
	"BRTH_FIRE",
	"BRTH_COLD",
	"BRTH_POIS",
	"BRTH_PLAS",
	"BRTH_LITE",
	"BRTH_DARK",
	"BRTH_CONFU",
	"BRTH_SOUND",
	"BRTH_SHARD",
	"BRTH_INERT",
	"BRTH_GRAV",
	"BRTH_WIND",
	"BRTH_FORCE",
	"BRTH_NEXUS",
	"BRTH_NETHR",
	"BRTH_CHAOS",
	"BRTH_DISEN",
	"BRTH_TIME",
	"BRTH_MANA",
	"BRTH_HOLY",
	"BRTH_FEAR",
	"BRTH_DISEA"
};


/*
 * Monster race flags
 */
static cptr r_info_flags5[] =
{
	"BALL_ACID",
	"BALL_ELEC",
	"BALL_FIRE",
	"BALL_COLD",
	"BALL_POIS",
	"BALL_LITE",
	"BALL_DARK",
	"BALL_CONFU",
	"BALL_SOUND",
	"BALL_SHARD",
	"BALL_WIND",
	"BALL_STORM",
	"BALL_NETHR",
	"BALL_CHAOS",
	"BALL_MANA",
	"BALL_WATER",
	"BOLT_ACID",
	"BOLT_ELEC",
	"BOLT_FIRE",
	"BOLT_COLD",
	"BOLT_POIS",
	"BOLT_PLAS",
	"BOLT_ICE",
	"BOLT_WATER",
	"BOLT_NETHR",
	"BOLT_MANA",
	"HOLY_ORB",
	"BEAM_ELEC",
	"BEAM_ICE",
	"BEAM_NETHR",
	"ARC_HFIRE",
	"ARC_FORCE"
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
	"INVIS",
	"TELE_SELF_TO",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"WRAITHFORM",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"DRAIN_MANA",
	"CURSE",
	"DISPEL",
	"MIND_BLAST",
	"ILLUSION",
	"WOUND",
	"BLESS",
	"BESERK",
	"SHIELD",
	"OPPOSE_ELEM",
	"HUNGER",
	"PROBE",
	"SCARE",
	"BLIND",
	"CONF",
	"SLOW",
	"HOLD"
};

/*
 * Monster race flags
 */
static cptr r_info_flags7[] =
{
	"S_KIN",
	"R_KIN",
	"A_DEAD",
	"S_MONSTER",
	"S_MONSTERS",
	"R_MONSTER",
	"R_MONSTERS",
	"S_PLANT",
	"S_INSECT",
	"S_ANIMAL",
	"S_HOUND",
	"S_SPIDER",
	"S_CLASS",
	"S_RACE",
	"S_GROUP",
	"S_FRIEND",
	"S_FRIENDS",
	"S_ORC",
	"S_TROLL",
	"S_GIANT",
	"S_DRAGON",
	"S_HI_DRAGON",
	"A_ELEMENT",
	"A_OBJECT",
	"S_DEMON",
	"S_HI_DEMON",
	"R_UNIQUE",
	"S_UNIQUE",
	"S_HI_UNIQUE",
	"S_UNDEAD",
	"S_HI_UNDEAD",
	"S_WRAITH"
};

/*
 * Monster race flags
 */
static cptr r_info_flags8[] =
{
	"HAS_SKULL",
	"HAS_SKELETON",
	"HAS_TEETH",
	"HAS_CORPSE",
	"HAS_HEAD",
	"HAS_HAND",
	"HAS_CLAW",
	"HAS_ARM",
	"HAS_LEG",
	"HAS_WING",
	"HAS_SKIN",
	"HAS_SCALE",
	"HAS_FEATHER",
	"HAS_FUR",
	"HAS_BLOOD",
	"HAS_SLIME",
	"HAS_SPORE",
	"DROP_JUNK",
	"DROP_CHEST",
	"DROP_MISSILE",
	"DROP_TOOL",
	"DROP_WEAPON",
	"DROP_MUSIC",
	"DROP_CLOTHES",
	"DROP_ARMOR",
	"DROP_LITE",
	"DROP_JEWELRY",
	"DROP_RSW",
	"DROP_WRITING",
	"DROP_POTION",
	"DROP_FOOD",
	"ASSEMBLE"
};


/*
 * Monster race flags
 */
static cptr r_info_flags9[] =
{
	"PLAYER_GHOST",
	"NEVER_MISS",
	"SAME_SPEED",
	"EVASIVE",
	"SCENT",
	"SUPER_SCENT",
	"WATER_SCENT",
	"RES_BLIND",
	"RES_LITE",
	"RES_DARK",
	"RES_CHAOS",
	"RES_TPORT",
	"RES_EDGED",
	"RES_BLUNT",
	"IM_EDGED",
	"IM_BLUNT",
	"NO_CUTS",
	"NO_SLOW",
	"FAMILY",
	"GOOD",
	"NEUTRAL",
	"DWARF",
	"ELF",
	"MAN",
	"TOWNSFOLK",
	"DROP_ESSENCE",
	"DROP_MUSHROOM",
	"DROP_MINERAL",
	"ATTR_METAL",
	"ATTR_INDEX",
	"MORGUL_MAGIC",
	"UDUN_MAGIC"
};



/*
 * Monster race flags
 */
static u32b hack_rf8_flags[52]=
{
/* A */ (RF8_HAS_CORPSE | RF8_HAS_WING | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_SCALE | RF8_HAS_LEG | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_CHEST | RF8_DROP_JEWELRY),
	(RF8_HAS_CORPSE | RF8_HAS_WING | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_FEATHER |
	 RF8_HAS_HEAD | RF8_HAS_BLOOD | RF8_DROP_JEWELRY | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_FUR | RF8_HAS_HEAD | RF8_HAS_LEG | RF8_HAS_BLOOD |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_WING | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_SCALE | RF8_HAS_LEG | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_CHEST | RF8_DROP_JEWELRY),
/* E */ (RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_SKIN | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_DROP_CLOTHES | RF8_DROP_JUNK | RF8_DROP_TOOL | RF8_DROP_MUSIC),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_FUR | RF8_HAS_LEG | RF8_HAS_BLOOD | RF8_HAS_FEATHER |
	 RF8_HAS_SCALE | RF8_HAS_WING |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK | RF8_DROP_JEWELRY),
/* I */ (RF8_HAS_CORPSE | RF8_HAS_LEG | RF8_HAS_WING | RF8_HAS_SKIN |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_BLOOD | RF8_HAS_TEETH |
	 RF8_HAS_SKIN | RF8_HAS_HEAD |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_LEG | RF8_HAS_SKIN |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_HAS_SKULL | 
	 RF8_DROP_RSW | RF8_DROP_WRITING | RF8_DROP_JEWELRY | RF8_DROP_CLOTHES),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_WRITING | RF8_DROP_WEAPON),
	(RF8_HAS_SKULL | 
	 RF8_DROP_ARMOR | RF8_DROP_WRITING | RF8_DROP_JEWELRY),
/* O */ (RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES | RF8_DROP_CHEST |
	 RF8_DROP_TOOL | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_CHEST | RF8_DROP_POTION | RF8_DROP_MUSIC),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_FUR |
	 RF8_HAS_LEG | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_CLOTHES | RF8_DROP_CHEST),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_LEG |
	 RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_LEG | RF8_HAS_SKIN |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES | RF8_DROP_CHEST |
	 RF8_DROP_JUNK),
/* U */ (RF8_HAS_SKULL |
	 RF8_DROP_WEAPON | RF8_DROP_ARMOR | RF8_DROP_WRITING),
	(RF8_HAS_SKULL |  RF8_HAS_SKELETON | 
	 RF8_DROP_CLOTHES | RF8_DROP_RSW | RF8_DROP_WRITING | RF8_DROP_JEWELRY | RF8_DROP_ARMOR |
	 RF8_DROP_WEAPON),
	(RF8_HAS_SKULL | 
	 RF8_DROP_CLOTHES | RF8_DROP_RSW | RF8_DROP_JEWELRY | RF8_DROP_ARMOR | RF8_DROP_POTION |
	 RF8_DROP_WEAPON),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_ARM | RF8_HAS_LEG | RF8_HAS_SKIN |
	 RF8_DROP_JEWELRY),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD | RF8_HAS_FUR |
	 RF8_DROP_FOOD | RF8_DROP_CLOTHES | RF8_DROP_JUNK),
/* Z */ (RF8_DROP_JUNK),
/* a */ (RF8_HAS_CORPSE | RF8_HAS_LEG | RF8_HAS_SKIN |
	 RF8_DROP_CLOTHES | RF8_DROP_FOOD | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_FUR | RF8_HAS_BLOOD |
	 RF8_HAS_WING | RF8_HAS_HEAD |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_LEG | RF8_HAS_SKIN |
	 RF8_DROP_CLOTHES | RF8_DROP_JEWELRY | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_WING | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_SCALE | RF8_HAS_LEG | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_JEWELRY),
/* e */ (RF8_HAS_CORPSE | RF8_HAS_BLOOD |
	 RF8_DROP_POTION | RF8_DROP_JEWELRY | RF8_DROP_RSW),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_FUR | RF8_HAS_LEG | RF8_HAS_BLOOD | RF8_HAS_HEAD |
	 RF8_DROP_CLOTHES | RF8_DROP_JEWELRY | RF8_DROP_JUNK),
	(RF8_HAS_ARM | RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_CORPSE | RF8_ASSEMBLY |
	 RF8_DROP_WEAPON | RF8_DROP_TOOL | RF8_DROP_RSW),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES | RF8_DROP_ARMOR |
	 RF8_DROP_TOOL | RF8_DROP_POTION | RF8_DROP_LITE | RF8_DROP_WRITING | RF8_DROP_MISSILE),
/* i */ (RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SLIME |
	 RF8_DROP_JEWELRY | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SLIME |
	 RF8_DROP_JUNK | RF8_DROP_JEWELRY | RF8_DROP_POTION),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES |
	 RF8_DROP_TOOL | RF8_DROP_JUNK | RF8_DROP_MISSILE ),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES | RF8_DROP_MISSILE |
	 RF8_DROP_POTION | RF8_DROP_LITE | RF8_DROP_WRITING | RF8_DROP_JEWELRY | RF8_DROP_MUSIC),
	(RF8_HAS_SPORE | RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_BLOOD | RF8_HAS_TEETH |
	 RF8_HAS_SKIN |
	 RF8_DROP_POTION | RF8_DROP_WRITING),
/* o */ (RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES |
	 RF8_DROP_TOOL | RF8_DROP_JUNK | RF8_DROP_MISSILE),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_ARMOR | RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES |
	 RF8_DROP_POTION | RF8_DROP_LITE | RF8_DROP_JEWELRY | RF8_DROP_WRITING),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES | RF8_DROP_JEWELRY |
	 RF8_DROP_RSW | RF8_DROP_POTION | RF8_DROP_LITE | RF8_DROP_WRITING),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH |
	 RF8_HAS_FUR | RF8_HAS_HEAD | RF8_HAS_LEG | RF8_HAS_BLOOD |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK),
	(RF8_DROP_ARMOR | RF8_DROP_WEAPON | RF8_DROP_JEWELRY),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_TEETH | RF8_HAS_ARM |
	 RF8_HAS_LEG | RF8_HAS_HAND | RF8_HAS_HEAD | RF8_HAS_BLOOD |
	 RF8_DROP_FOOD | RF8_DROP_WEAPON | RF8_DROP_CLOTHES |
	 RF8_DROP_TOOL | RF8_DROP_POTION | RF8_DROP_LITE | RF8_DROP_MUSIC | RF8_DROP_JEWELRY),
/* u */ (RF8_HAS_SKULL |
	 RF8_DROP_WEAPON | RF8_DROP_ARMOR | RF8_DROP_WRITING),
	(RF8_DROP_ARMOR | RF8_DROP_JEWELRY | RF8_DROP_WEAPON | RF8_DROP_TOOL),
	(RF8_HAS_CORPSE | RF8_HAS_SLIME |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK),
	(RF8_DROP_JUNK),
	(RF8_HAS_CORPSE | RF8_HAS_SKULL | RF8_HAS_SKELETON | RF8_HAS_LEG |
	 RF8_HAS_SCALE | RF8_HAS_HEAD |
	 RF8_DROP_CLOTHES | RF8_DROP_JUNK | RF8_DROP_ARMOR | RF8_DROP_CHEST),
/* z */ (RF8_DROP_ARMOR | RF8_DROP_WEAPON | RF8_DROP_JEWELRY | RF8_DROP_RSW)
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
	"SAVE",
	"DEVICE",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"BLOWS",
	"SHOTS",
	"MIGHT",
	"SLAY_NATURAL",
	"BRAND_HOLY",
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
 * Object flags
 */
static cptr k_info_flags4[] =
{
	"BRAND_DARK",
	"BRAND_LITE",
	"HURT_LITE",
	"HURT_WATER",
	"VAMP_HP",
	"VAMP_MANA",
	"IM_POIS",
	"RES_DISEASE",
	"HUNGER",
	"SLAY_MAN",
	"SLAY_ELF",
	"SLAY_DWARF",
	"ANCHOR",
	"SILENT",
	"STATIC",
	"WINDY",
	"ANIMAL",
	"EVIL",
	"UNDEAD",
	"DEMON",
	"ORC",
	"TROLL",
	"GIANT",
	"DRAGON",
	"MAN",
	"DWARF",
	"ELF",
	"HURT_POIS",
	"HURT_ACID",
	"HURT_ELEC",
	"HURT_FIRE",
	"HURT_COLD"
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
	"CURE_DISEASE",
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
	"INVEN_FEET"
};



/*
 * Quest event flags
 */
static cptr quest_event_info_flags[] =
{
	"TRAVEL",
	"LEAVE",
	"STAY",
	"PASS_QUEST",
	"FAIL_QUEST",
	"FIND_ROOM",
	"FLAG_ROOM",
	"UNFLAG_ROOM",
	"ALTER_FEAT",
	"DEFEND_FEAT",
	"FIND_ITEM",
	"GET_ITEM",
	"DESTROY_ITEM",
	"LOSE_ITEM",
	"TALK_STORE",
	"BUY_STORE",
	"SELL_STORE",
	"GIVE_STORE",
	"STOCK_STORE",
	"GET_STORE",
	"DEFEND_STORE",
	"TALK_RACE",
	"GIVE_RACE",
	"GET_RACE",
	"FIND_RACE",
	"KILL_RACE",
	"ALLY_RACE",
	"HATE_RACE",
	"FEAR_RACE",
	"HEAL_RACE",
	"BANISH_RACE",
	"DEFEND_RACE"
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

	/* Process 'B' for "Maximum q_info[] subindex" */
	else if (buf[2] == 'Q')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->q_max = max;
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
	if (grab_one_flag(&d_ptr->flags, d_info_sflags, what) == 0)
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

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags5, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags6, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags7, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags8, what) == 0)
		return (0);

	if (grab_one_offset(&d_ptr->r_flag, r_info_flags9, what) == 0)
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

	if (grab_one_offset(&f_ptr->spell, r_info_flags7, what) == 0)
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
		f_ptr->flags3 |= (FF3_ATTR_LITE);
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
		f_ptr->level = (u16b)level;
		f_ptr->rarity = (u16b)rarity;
		f_ptr->priority = (u16b)priority;
		f_ptr->edge = (s16b)edge;
	}

	/* Process 'K' for "States" (up to eight lines + default (which cannot be last)) */
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

	if (grab_one_flag(&k_ptr->flags4, k_info_flags4, what) == 0)
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

	if (grab_one_flag(&a_ptr->flags4, k_info_flags4, what) == 0)
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
 * Add a name to the probability tables
 */
static errr build_prob(char *name, names_type *n_ptr)
{
	int c_prev, c_cur, c_next;

	while (*name && !isalpha((unsigned char) *name))
      ++name;

	if (!*name)	return PARSE_ERROR_GENERIC;

	c_prev = c_cur = S_WORD;

	do
	{
		if (isalpha ((unsigned char) *name))
		{
			c_next = A2I (tolower ((unsigned char) *name));
			n_ptr->lprobs[c_prev][c_cur][c_next]++;
			n_ptr->ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
		}
	}
	while (*++name);

	n_ptr->lprobs[c_prev][c_cur][E_WORD]++;
	n_ptr->ltotal[c_prev][c_cur]++;

	return 0;
}

/*
 * Initialize the "n_info" array, by parsing an ascii "template" file
 */
errr parse_n_info(char *buf, header *head)
{
	names_type *n_ptr = head->info_ptr;

	/*
	 * This function is called once, when the raw file does not exist.
	 * If you want to initialize some stuff before parsing the txt file
 	 * you can do:
	 *
	 * static int do_init = 1;
	 *
	 * if (do_init)
	 * {
	 *    do_init = 0;
	 *    ...
	 *    do_stuff_with_n_ptr
	 *    ...
	 * }
	 *
	 */

	if (buf[0] == 'N')
	{
	    	return build_prob (buf + 2, n_ptr);
	}

 	/*
	 * If you want to do something after parsing the file you can add
	 * a special directive at the end of the txt file, like:
	 *
	 * else
	 * if (buf[0] == 'X')          (Only at the end of the txt file)
	 * {
	 *    ...
	 *    do_something_else_with_n_ptr
	 *    ...
	 * }
	 *
	 */
	else
	{
    	/* Oops */
    	return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
  	}
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

	if (grab_one_flag(&e_ptr->flags4, k_info_flags4, what) == 0)
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

	if (grab_one_flag(&e_ptr->obv_flags4, k_info_flags4, what) == 0)
        {
                e_ptr->flags4 |= e_ptr->obv_flags4;
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

	if (grab_one_flag(&r_ptr->flags8, r_info_flags8, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags9, r_info_flags9, what) == 0)
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

	if (grab_one_flag(&r_ptr->flags7, r_info_flags7, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown monster spell '%s'.", what);

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

		/* Hack -- set some rf8 flags */
		if ((r_ptr->d_char >='A') && (r_ptr->d_char <='Z'))
		{
			r_ptr->flags8 |= hack_rf8_flags[r_ptr->d_char-'A'];
		}

		/* Hack -- set some rf8 flags */
		if ((r_ptr->d_char >='a') && (r_ptr->d_char <='z'))
		{
			r_ptr->flags8 |= hack_rf8_flags[r_ptr->d_char-'a'+26];
		}

		/* Hack -- nonliving monsters */
		/* Death by Physical attack -- non-living monster */
		if (strchr("Evg", r_ptr->d_char))
		{
			r_ptr->flags3 |= RF3_NONLIVING;
		}

		/* Canines and hounds and ring wraiths have super scent */
		if ((strchr("CZ", r_ptr->d_char)) || ((r_ptr->d_char == 'W') && (r_ptr->flags1 & (RF1_UNIQUE))))
		{
			r_ptr->flags9 |= (RF9_SUPER_SCENT | RF9_SCENT);
		}

		/* Normal sense of smell */
		if (strchr("ADdfkoQyHORTY", r_ptr->d_char))
		{
			r_ptr->flags9 |= (RF9_SCENT);
		}

		/* Mark men */
		if (strchr("tpq", r_ptr->d_char))
		{
			r_ptr->flags9 |= (RF9_MAN);
		}

		/* Mark elves and maia */
		if (strchr("lM", r_ptr->d_char))
		{
			r_ptr->flags9 |= RF9_ELF;
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
		int lev, rar, grp;
		long exp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			    &lev, &rar, &grp, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
		r_ptr->grp_idx = grp;
		r_ptr->mexp = exp;
	}

	/* Process 'M' for "Magic Info" (one line only) */
	else if (buf[0] == 'M')
	{
		int innate, spell, power, mana;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			    &innate, &spell, &power, &mana)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->freq_innate = innate;
		r_ptr->freq_spell = spell;
		r_ptr->spell_power = power;
		r_ptr->mana = mana;
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

		/* Update the rf4 flags */
		if (n1 > RBM_MIN_RANGED) r_ptr->flags4 |= (RF4_BLOW_1 << i);

		/* Hack -- update the rf7 flags */
		switch (n1)
		{
			case RBM_CLAW:
				r_ptr->flags8 |= RF8_HAS_CLAW;
				break;
			case RBM_SPORE:
				r_ptr->flags8 |= RF8_HAS_SPORE;
				break;
			case RBM_SHOT:
			case RBM_XBOLT:
			case RBM_ARROW:
				r_ptr->flags8 |= RF8_DROP_MISSILE;
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

	if (grab_one_flag(&pr_ptr->flags4, k_info_flags4, what) == 0)
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
		int tval, sval, number_min, number_max, pval_min, pval_max, social_min, social_max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			    &tval, &sval, &number_min, &number_max, &pval_min, &pval_max, &social_min, &social_max)) return (PARSE_ERROR_GENERIC);

		if ((number_min < 0) || (number_max < 0) || (number_min > 99) || (number_max > 99))
			return (PARSE_ERROR_INVALID_ITEM_NUMBER);

		/* Save the values */
		e_ptr->tval = tval;
		e_ptr->sval = sval;
		e_ptr->number_min = number_min;
		e_ptr->number_max = number_max;
		e_ptr->pval_min = pval_min;
		e_ptr->pval_max = pval_max;
		e_ptr->social_min = social_min;
		e_ptr->social_max = social_max;

		/* Next item */
		cur_equip++;

		/* Limit number of starting items */
		if (cur_equip > MAX_CLASS_ITEMS)
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

	if (grab_one_offset(&y_ptr->flag[count], k_info_flags4, what) == 0)
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


/*
 * Grab an action in an quest_type from a textual string
 */
static errr grab_one_quest_action(quest_event *qe_ptr, cptr what)
{
	if (grab_one_offset(&qe_ptr->action, f_info_flags1, what) == 0)
		return (0);

	if (grab_one_offset(&qe_ptr->action, f_info_flags2, what) == 0)
		return (0);

	if (grab_one_offset(&qe_ptr->action, f_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown quest action '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one flag in an quest_event_info_flags from a textual string
 */
static errr grab_one_quest_flag(quest_event *qe_ptr, cptr what)
{
	if (grab_one_flag(&qe_ptr->flags, quest_event_info_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown quest event flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one flag in an room_type from a textual string
 */
static errr grab_one_quest_room_flag(quest_event *qe_ptr, cptr what)
{
	if (grab_one_flag(&qe_ptr->flags, d_info_sflags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown quest event flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "q_info" array, by parsing an ascii "template" file
 */
errr parse_q_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static quest_type *q_ptr = NULL;

	/* Current entry */
	static quest_event *qe_ptr = NULL;

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
		q_ptr = (quest_type*)head->info_ptr + i;

		/* Point at the first event */
		qe_ptr = &(q_ptr->event[0]);

		/* Store the name */
		if (!(q_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Hack -- quest stage */
		q_ptr->stage = 100;
	}

	/* Process 'S' for "Stage" (up to MAX_QUEST_EVENT lines) */
	else if (buf[0] == 'S')
	{
		int stage;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
				&stage)) return (PARSE_ERROR_GENERIC);

		/* Paranoia */
		if (stage < 0) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Paranoia */
		if (stage >= MAX_QUEST_EVENTS) return (PARSE_ERROR_TOO_MANY_ENTRIES);
		
		/* Point at the first event */
		qe_ptr = &q_ptr->event[stage];

		/* Hack -- set first quest stage */
		if (q_ptr->stage == 100) q_ptr->stage = stage;

		/* Hack -- set default number */
		qe_ptr->number = 1;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
			if (0 != grab_one_quest_flag(qe_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'T' for "Travel to" (one line per stage) */
	else if (buf[0] == 'T')
	{
		int dungeon, level, store;
		
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&dungeon, &level, &store)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->dungeon = dungeon;
		qe_ptr->level = level;
		qe_ptr->store = store;
	}

	/* Process 'A' for "Artifact" (one line per stage) */
	else if (buf[0] == 'A')
	{
		int artifact;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &artifact)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->artifact = artifact;
	}
	
	/* Process 'Q' for "Quest" (one line per stage) */
	else if (buf[0] == 'Q')
	{
		int quest;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &quest)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->quest = quest;
	}

	/* Process 'Z' for "Number" (one line per stage) */
	else if (buf[0] == 'Z')
	{
		int number;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			    &number)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->number = number;
	}

	/* Process 'K' for "Kinds" (one line per stage) */
	else if (buf[0] == 'K')
	{
		int kind;
		
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
				&kind)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->kind = kind;
	}

	/* Process 'E' for "Ego item" (one line per stage) */
	else if (buf[0] == 'E')
	{
		int ego_item_type;
		
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
				&ego_item_type)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->ego_item_type = ego_item_type;
	}

	/* Process 'R' for "Races" (one line per stage) */
	else if (buf[0] == 'R')
	{
		int race;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
				&race)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->race = race;
	}

	/* Process 'O' for "Rooms" (one line per stage) */
	else if (buf[0] == 'O')
	{
		int room_type_a, room_type_b;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Parse this entry */
		if (0 != grab_one_quest_room_flag(qe_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

		/* Scan for the values */
		if (2 != sscanf(t, "%d:%d",
				&room_type_a, &room_type_b)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->room_type_a = room_type_a;
		qe_ptr->room_type_b = room_type_b;
	}

	/* Process 'X' for "Features" (one line per stage) */
	else if (buf[0] == 'X')
	{
		int feat;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Parse this entry */
		if (0 != grab_one_quest_action(qe_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

		/* Scan for the values */
		if (1 != sscanf(t, "%d",
				&feat)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->feat = feat;

	}

	/* Process 'W' for "Worth" (one line per stage) */
	else if (buf[0] == 'W')
	{
		int experience, power, gold;
		
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&experience, &power, &gold)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		qe_ptr->experience = experience;
		qe_ptr->power = power;
		qe_ptr->gold = gold;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&q_ptr->text, head, s))
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
 * Initialise the info
 */
errr eval_info(eval_info_power_func eval_info_process, header *head)
{
	int err;

	/* Process the info */
	err = (*eval_info_process)(head);

	return(err);
}



/*
 * Go through the attack types for this monster.
 * We look for the maximum possible maximum damage that this
 * monster can inflict in 10 game turns.
 *
 * We try to scale this based on assumed resists,
 * chance of casting spells and of spells failing,
 * chance of hitting in melee, and particularly speed.
 */

static long eval_max_dam(monster_race *r_ptr)
{
	int i, x;
	u32b dam = 1;
	u32b hp;
	u32b melee_dam, atk_dam, spell_dam;
	byte rlev;
	u32b flag, breath_mask, attack_mask, innate_mask;
	u32b flag_counter;

	/*clear the counters*/
	melee_dam = atk_dam = spell_dam = 0;

	/* Evaluate average HP for this monster */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) hp = r_ptr->hdice * r_ptr->hside;
	else hp = r_ptr->hdice * (r_ptr->hside + 1) / 2;

	/* Extract the monster level, force 1 for town monsters */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	for (x = 0; x < 4; x++)
	{

		/*Get the flags 4 monster flags and masks*/
		switch (x)
		{
			case 0:
			{
		 		flag = r_ptr->flags4;
				attack_mask = RF4_ATTACK_MASK;
				breath_mask = RF4_BREATH_MASK;
				innate_mask = RF4_INNATE_MASK;
				break;
			}
			case 1:
			{
		 		flag = r_ptr->flags5;
				attack_mask = RF5_ATTACK_MASK;
				breath_mask = RF5_BREATH_MASK;
				innate_mask = RF5_INNATE_MASK;
				break;
			}
			case 2:
			{
		 		flag = r_ptr->flags6;
				attack_mask = RF6_ATTACK_MASK;
				breath_mask = RF6_BREATH_MASK;
				innate_mask = RF6_INNATE_MASK;
				break;
			}
			case 3:
			default:
			{
		 		flag = r_ptr->flags7;
				attack_mask = RF7_ATTACK_MASK;
				breath_mask = RF7_BREATH_MASK;
				innate_mask = RF7_INNATE_MASK;
				break;
			}
		}

		/*no spells here, continue*/
		if (!flag) continue;

		flag_counter = 0x00000001;

		/* using 32 assumes a u32b flag size*/
		for (i = 0; i < 32; i++)
		{
			u16b this_dam = 0;

			/* We count ranged blows later */
			if (!(x) && ((flag <= RF4_BLOW_4) || (flag == RF4_EXPLODE) || (flag == RF4_AURA))) continue;

			/* First make sure monster has the flag*/
			if (flag & flag_counter)
			{
				/*Is it a breath? Should only be flag 4*/
				if (breath_mask & flag_counter)
				{
					int which_gf = 0;
					int mult = 1;
					int div_by = 1;

					/*hack - all breaths are in flag 4*/

					if (flag_counter == RF4_BRTH_ACID) 		which_gf = GF_ACID;
					else if (flag_counter == RF4_BRTH_ELEC) which_gf = GF_ELEC;
					else if (flag_counter == RF4_BRTH_FIRE) which_gf = GF_FIRE;
					else if (flag_counter == RF4_BRTH_COLD) which_gf = GF_COLD;
					else if (flag_counter == RF4_BRTH_POIS)
					{
						which_gf = GF_POIS;
						mult = 8;
						div_by = 7;
					}
					else if (flag_counter == RF4_BRTH_PLAS)
					{
						which_gf = GF_PLASMA;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_LITE)
					{
						which_gf = GF_LITE;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_DARK)
					{
						which_gf = GF_DARK;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_CONFU)
					{
						which_gf = GF_CONFUSION;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_SOUND)
					{
						which_gf = GF_SOUND;
						mult = 6;
						div_by = 5;
					}
					else if (flag_counter == RF4_BRTH_SHARD)
					{
						which_gf = GF_SHARD;
						mult = 8;
						div_by = 7;
					}
					else if (flag_counter == RF4_BRTH_INERT)
					{
						which_gf = GF_INERTIA;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_WIND)
					{
						which_gf = GF_WIND;
						mult = 6;
						div_by = 5;
					}
					else if (flag_counter == RF4_BRTH_GRAV)
					{
						which_gf = GF_GRAVITY;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_FORCE)
					{
						which_gf = GF_FORCE;
						mult = 6;
						div_by = 5;
					}
					else if (flag_counter == RF4_BRTH_NEXUS)
					{
						which_gf = GF_NEXUS;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_NETHR)
					{
						which_gf = GF_NETHER;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_CHAOS)
					{
						which_gf = GF_CHAOS;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_DISEN)
					{
						which_gf = GF_DISENCHANT;
						mult = 4;
						div_by = 3;
					}
					else if (flag_counter == RF4_BRTH_TIME)
					{
						which_gf = GF_TIME;
						mult = 3;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_MANA) which_gf = GF_MANA;
					else if (flag_counter == RF4_BRTH_HOLY)
					{
						which_gf = GF_HOLY_ORB;
						div_by = 2;
					}
					else if (flag_counter == RF4_BRTH_FEAR)
					{
						which_gf = GF_TERRIFY;
						mult = 5;
						div_by = 4;
					}
					else if (flag_counter == RF4_BRTH_DISEA)
					{
						which_gf = GF_DISEASE;
						mult = 5;
						div_by = 4;
					}

					if (which_gf)
					{
						this_dam = get_breath_dam(hp, which_gf,
									(r_ptr->flags2 & (RF2_POWERFUL) ? TRUE : FALSE));

						/* handle elemental breaths*/
						switch (which_gf)
						{
							case GF_ACID:
							case GF_FIRE:
							case GF_COLD:
							case GF_ELEC:
							case GF_POIS:
							{
								/* Lets just pretend the player has the right base resist*/
								div_by *= 3;

								break;
							}

						}

						this_dam = (this_dam * mult) / div_by;

						/* Slight bonus for being powerful */
						if (r_ptr->flags2 & (RF2_POWERFUL)) this_dam = this_dam * 8 / 7;
					}
				}

				/*Is it an arrow, bolt, beam, or ball?*/
				else if (attack_mask & flag_counter)
				{
					switch (x)
					{
						case 0:
						{
							this_dam = r_ptr->spell_power * spell_info_RF4[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF4[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 1:
						{
							this_dam = r_ptr->spell_power * spell_info_RF5[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF5[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 2:
						{
							this_dam = r_ptr->spell_power * spell_info_RF6[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF6[i][COL_SPELL_DAM_DIV]);
							break;
						}
						case 3:
						{
							this_dam = r_ptr->spell_power * spell_info_RF7[i][COL_SPELL_DAM_MULT];
							this_dam /=  MAX(1, spell_info_RF7[i][COL_SPELL_DAM_DIV]);
							break;
						}
					}
				}

				else switch (x)
				{
					/*Misc flag4 flags*/
					case 0:
					{
						if (flag_counter == RF4_SHRIEK) this_dam = rlev / 2;
						break;
					}

					case 1:
					{
						/*Right now all flag5 are attack mask spells*/
						break;
					}

					case 2:
					{
						/*Misc flag6 flags*/
						if (flag_counter == RF6_ADD_MANA) this_dam = MAX(r_ptr->mana, 30);
						else if (flag_counter == RF6_BLINK) this_dam = rlev / 3;
						else if (flag_counter == RF6_TELE_SELF_TO) this_dam = rlev * 2;
						else if (flag_counter == RF6_TELE_TO) this_dam = rlev;
						else if (flag_counter == RF6_DARKNESS) this_dam = rlev;
						else if (flag_counter == RF6_TRAPS) this_dam = rlev;
						else if (flag_counter == RF6_FORGET) this_dam = rlev / 3;
						else if (flag_counter == RF6_ILLUSION) this_dam = rlev;
						else if (flag_counter == RF6_DRAIN_MANA) this_dam = rlev * 2;
						else if (flag_counter == RF6_HUNGER) this_dam = rlev;
						else if (flag_counter == RF6_SCARE) this_dam = rlev;
						else if (flag_counter == RF6_BLIND) this_dam = rlev;
						else if (flag_counter == RF6_CONF) this_dam = rlev;
						else if (flag_counter == RF6_SLOW) this_dam = rlev;
						else if (flag_counter == RF6_HOLD) this_dam = 25;

						if (r_ptr->flags2 & (RF2_POWERFUL)) this_dam = this_dam * 3 / 2;
						break;
					}
					/*All flag7 flags*/
					case 3:
					{
						/*Right now all flag7 are summon spells*/
						/* All summons are assigned arbitrary values according to their levels*/
						if 		(flag_counter == RF7_S_KIN) 	this_dam = rlev * 2;
						else if (flag_counter == RF7_R_KIN) 	this_dam = rlev / 2;
						else if (flag_counter == RF7_A_DEAD) 	this_dam = rlev / 5;
						else if (flag_counter == RF7_S_MONSTER)	this_dam = rlev * 2 / 5;
						else if (flag_counter == RF7_S_MONSTERS)this_dam = rlev * 4 / 5;
						else if (flag_counter == RF7_R_MONSTER)	this_dam = rlev * 1 / 5;
						else if (flag_counter == RF7_R_MONSTERS)this_dam = rlev * 2 / 5;
						else if (flag_counter == RF7_S_PLANT)		this_dam = rlev / 5;
						else if (flag_counter == RF7_S_INSECT)		this_dam = rlev / 5;
						else if (flag_counter == RF7_S_ANIMAL)
						{
							/* XXX Feathers, fur or skin? */
							this_dam = rlev * 3 / 2;  /* Includes hydras */
							/* this_dam = rlev / 2;   Fur includes canines / cats */
							/* this_dam = rlev / 2;   Feathers includes birds / hybrids */
						}
						else if (flag_counter == RF7_S_SPIDER)	this_dam = rlev / 5;
						else if (flag_counter == RF7_S_HOUND)	this_dam = rlev;
						else if (flag_counter == RF7_S_CLASS)	this_dam = rlev / 2;
						else if (flag_counter == RF7_S_RACE)	this_dam = rlev / 2;
						else if (flag_counter == RF7_S_GROUP)	this_dam = rlev / 2;
						else if (flag_counter == RF7_S_FRIEND)	this_dam = rlev / 3;
						else if (flag_counter == RF7_S_FRIENDS)	this_dam = rlev * 3 / 4;
						else if (flag_counter == RF7_S_DRAGON)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_DRAGON) this_dam = rlev * 4;
						else if (flag_counter == RF7_A_ELEMENT) this_dam = rlev / 3;
						else if (flag_counter == RF7_A_OBJECT) 	this_dam = rlev / 5;
						else if (flag_counter == RF7_S_DEMON)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_DEMON)this_dam = rlev * 3;
						else if (flag_counter == RF7_S_UNDEAD)	this_dam = rlev * 3 / 2;
						else if (flag_counter == RF7_S_HI_UNDEAD)this_dam = rlev * 4;
						else if (flag_counter == RF7_S_WRAITH)	this_dam = rlev * 9 / 2;
						else if (flag_counter == RF7_S_UNIQUE)	this_dam = rlev * 3;
						else if (flag_counter == RF7_S_HI_UNIQUE)	this_dam = rlev * 5;
						break;
					}
				}

			}

			/* Hack - always allow one attack at maximum */
			if (this_dam > spell_dam) spell_dam = this_dam;

			/* Hack - Apply over 10 rounds */
			this_dam *= 10;

			/* Scale for frequency */
			if (flag_counter & innate_mask)	this_dam = this_dam * r_ptr->freq_innate / 100;
			else this_dam = this_dam * r_ptr->freq_spell / 100;

			/* Scale for spell failure chance */
			if (!(r_ptr->flags2 & RF2_STUPID) && (x > 0)) this_dam = this_dam * MIN(75 + (rlev + 3) / 4, 100) / 100;

			/* Scale for mana requirement */
			if ((x == 0) && (spell_info_RF4[i][0] * 10 > r_ptr->mana))
			{
				this_dam = this_dam * r_ptr->mana / (spell_info_RF4[i][0] * 10);
			}
			else if ((x == 1) && (spell_info_RF5[i][0] * 10 > r_ptr->mana))
			{
				this_dam = this_dam * r_ptr->mana / (spell_info_RF5[i][0] * 10);
			}
			else if ((x == 2) && (spell_info_RF6[i][0] * 10 > r_ptr->mana))
			{
				this_dam = this_dam * r_ptr->mana / (spell_info_RF6[i][0] * 10);
			}
			else if ((x == 3) && (spell_info_RF7[i][0] * 10 > r_ptr->mana))
			{
				this_dam = this_dam * r_ptr->mana / (spell_info_RF7[i][0] * 10);
			}

			if (this_dam > spell_dam) spell_dam = this_dam;

			/*shift one bit*/
			flag_counter = flag_counter << 1;
		}
	}

	/* Only do if it has attacks */
	if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
	{
		for (i = 0; i < 4; i++)
		{
			/* Extract the attack infomation */
			int effect = r_ptr->blow[i].effect;
			int method = r_ptr->blow[i].method;
			int d_dice = r_ptr->blow[i].d_dice;
			int d_side = r_ptr->blow[i].d_side;

			/* Hack -- no more attacks */
			if (!method) continue;

			/* Assume maximum damage*/
			atk_dam = d_dice * d_side;

			switch (method)
			{
				/*stun definitely most dangerous*/
				case RBM_PUNCH:
				case RBM_KICK:
				case RBM_BUTT:
				case RBM_CRUSH:
				{
					atk_dam *= 4;
					atk_dam /= 3;
					break;
				}
				/*cut*/
				case RBM_CLAW:
				case RBM_BITE:
				case RBM_PECK:
				{
					atk_dam *= 7;
					atk_dam /= 5;
					break;
				}
				default: 
				{
					if ((effect == GF_WOUND) || (effect == GF_BATTER))
					{
						atk_dam *= 5;
						atk_dam /= 4;
					}
					break;
				}
			}

			switch (effect)
			{
				/*other bad effects - minor*/
				case GF_EAT_GOLD:
				case GF_EAT_ITEM:
				case GF_EAT_FOOD:
				case GF_EAT_LITE:
				case GF_LOSE_CHR:
				case GF_WIND:
				{
					atk_dam += 5;
					break;
				}
				/*other bad effects - poison / disease */
				case GF_DISEASE:
				case GF_POIS:
				case GF_ICE:
				case GF_SHARD:
				{
					atk_dam *= 5;
					atk_dam /= 4;
					atk_dam += 5;
					break;
				}
				/*other bad effects - elements / sustains*/
				case GF_TERRIFY:
				case GF_ACID:
				case GF_ELEC:
				case GF_FIRE:
				case GF_COLD:
				case GF_HUNGER:
				case GF_LOSE_MANA:
				case GF_LITE:
				case GF_DARK:
				case GF_SLOW:
				{
					atk_dam += 10;
					break;
				}
				/*other bad effects - major*/
				case GF_PLASMA:
				case GF_WATER:
				case GF_SOUND:
				case GF_NEXUS:
				case GF_BLIND:
				case GF_LAVA:
				case GF_CONFUSION:
				case GF_LOSE_STR:
				case GF_LOSE_INT:
				case GF_LOSE_WIS:
				case GF_LOSE_DEX:
				case GF_EXP_10:
				case GF_HALLU:
				{
					atk_dam += 20;
					break;
				}
				/*other bad effects - major*/
				case GF_GRAVITY:
				case GF_INERTIA:
				case GF_FORCE:
				case GF_NETHER:
				case GF_CHAOS:
				case GF_DISENCHANT:
				case GF_UN_BONUS:
				case GF_UN_POWER:
				case GF_LOSE_CON:
				case GF_EXP_20:
				{
					atk_dam += 30;
					break;
				}
				/*other bad effects - major*/
				case GF_TIME:
				case GF_PARALYZE:
				case GF_LOSE_ALL:
				case GF_EXP_40:
				case GF_EXP_80:
				{
					atk_dam += 40;
					break;
				}


				/*Earthquakes*/
				case GF_SHATTER:
				{
					atk_dam += 300;
					break;
				}

				/* No damage normally */
				case GF_LITE_WEAK:
				case GF_DARK_WEAK:
				case GF_WATER_WEAK:
				case GF_SALT_WATER:
				case GF_BLIND_WEAK:
				{
					atk_dam = 5;
				}

				/*nothing special*/
				default: break;
			}


			/* Normal melee attack */
			if (method < RBM_MAX_NORMAL)
			{
				/* Keep a running total */
				melee_dam += atk_dam;
			}

			/* Hack -- aura is extra tough in melee */
			if (method == RBM_AURA) 
			{
				/* Keep a running total */
				melee_dam += 3 * atk_dam;
			}

			/* Ranged attacks can also apply spell dam */
			if (method > RBM_MIN_RANGED)
			{
				int range = MAX_SIGHT, mana = 0;
				bool must_hit = FALSE;

				/* Hack -- always allow one attack at maximum */
				if (spell_dam < atk_dam) spell_dam = atk_dam;

				/* Scale for frequency of innate attacks */
				atk_dam = atk_dam * r_ptr->freq_innate / 100;

				/* Some ranged blows can miss */
				switch(method)
				{
					case RBM_SPIT:	mana = 0; must_hit = TRUE; break;
					case RBM_GAZE:	mana = 3; range = MIN(MAX_SIGHT, r_ptr->aaf);break;
					case RBM_WAIL:  mana = 5; range = 4; break;
					case RBM_SPORE:	mana = 0; range = 3; must_hit = TRUE; break;
					case RBM_LASH:  mana = 0; range = 3; break;
					case RBM_BEG:	mana = 0; range = 4; break;
					case RBM_INSULT: mana = 0; range = 4; break;
					case RBM_SING:  mana = 0; range = 4; break;
					case RBM_TRAP:  mana = 0; range = 1; break;
					case RBM_BOULDER: mana = 0; range = 8; must_hit = TRUE; break;
					case RBM_AURA:	mana = 4; range = 2; break;
					case RBM_SELF:	mana = 3; range = 0; break;
					case RBM_ADJACENT: mana = 3; range = 1; break;
					case RBM_HANDS: mana = 4; range = 3; break;
					case RBM_MISSILE: mana = 2; range = MAX_SIGHT; break;
					case RBM_BOLT_10: mana = 5; range = MAX_SIGHT; break;
					case RBM_BOLT: mana = 4; range = MAX_SIGHT; break;
					case RBM_BEAM: mana = 6; range = 10; break;
					case RBM_BLAST: mana = 3; range = 5; break;
					case RBM_WALL: mana = 6; range = MAX_SIGHT; break;
					case RBM_BALL: mana = 4; range = MAX_SIGHT; break;
					case RBM_CLOUD: mana = 5; range = MAX_SIGHT; break;
					case RBM_STORM: mana = 6; range = MAX_SIGHT; break;
					case RBM_BREATH: mana = 0; range = 6; break;
					case RBM_AREA: mana = 3; range = (r_ptr->level / 10) + 1; break;
					case RBM_LOS: mana = 6; range = MAX_SIGHT; break;
					case RBM_LINE: mana = 4; range = MAX_SIGHT; break;
					case RBM_AIM: mana = 4; range = MAX_SIGHT; break;
					case RBM_ORB: mana = 5; range = MAX_SIGHT; break;
					case RBM_STAR: mana = 5; range = MAX_SIGHT; break;
					case RBM_SPHERE: mana = 6; range = MAX_SIGHT; break;
					case RBM_PANEL: mana = 6; range = MAX_SIGHT; break;
					case RBM_LEVEL: mana = 8; range = 255; break;
					case RBM_CROSS: mana = 4; range = MAX_SIGHT; break;
					case RBM_STRIKE: mana = 5; range = MAX_SIGHT; break;
					case RBM_EXPLODE: mana = 0; range = 1; break;
					case RBM_ARROW: mana = 0; range = 10; must_hit = TRUE; break;
					case RBM_XBOLT: mana = 0; range = 12; must_hit = TRUE; break;
					case RBM_SPIKE: mana = 0; range = 4; must_hit = TRUE; break;
					case RBM_DART: mana = 0; range = 8; must_hit = TRUE; break;
					case RBM_SHOT: mana = 0; range = 8; must_hit = TRUE; break;
					case RBM_ARC_20: mana = 6; range = 8; break;
					case RBM_ARC_30: mana = 5; range = 6; break;
					case RBM_FLASK:	mana = 0; range = 6; must_hit = TRUE; break;
				}

				/* Scale if needs to hit */
				if (must_hit && !(r_ptr->flags9 & (RF9_NEVER_MISS)))
				{
					atk_dam = atk_dam * MIN(45 + rlev * 3, 95) / 100;
				}

				/* Scale for maximum range */
				if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL)) atk_dam *= 10;
				else if (range >= 7) atk_dam *= 10;
				else if (r_ptr->flags9 & (RF9_SAME_SPEED)) atk_dam *= MIN(4 + range, 10);
				else
					atk_dam = atk_dam * (3 + range) + atk_dam * extract_energy[r_ptr->speed + (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] / (7 - range);

				/* Scale for mana requirement */
				if (mana * 10 > r_ptr->mana) atk_dam = atk_dam * r_ptr->mana / (mana * 10);

				if (spell_dam < atk_dam) spell_dam = atk_dam;
			}

		}

		/* 
		 * Apply damage over 10 rounds. We assume that the monster has to make contact first.
		 * Hack - speed has more impact on melee as has to stay in contact with player.
		 * Hack - this is except for pass wall and kill wall monsters which can always get to the player.
		 * Hack - use different values for huge monsters as they strike out to range 2.
		 */
		if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL))
				melee_dam *= 10;
		else if (r_ptr->flags3 & (RF3_HUGE))
		{
			if (r_ptr->flags9 & (RF9_SAME_SPEED)) melee_dam *= 6;
			else
				melee_dam = melee_dam * 4 + melee_dam * extract_energy[r_ptr->speed + (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] / 6;
		}
		else
		{
			if (r_ptr->flags9 & (RF9_SAME_SPEED)) melee_dam *= 5;
			else
				melee_dam = melee_dam * 3 + melee_dam * extract_energy[r_ptr->speed + (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] / 7;
		}

		/*
		 * Scale based on attack accuracy. We make a massive number of assumptions here and just use monster level.
		 */
		if (!(r_ptr->flags9 & (RF9_NEVER_MISS)))
		{
			int power = rlev + (r_ptr->flags6 & (RF6_BLESS) ? 5 : 0) + (r_ptr->flags6 & (RF6_BESERK) ? 12 : 0);

			melee_dam = melee_dam * MIN(45 + power * 3, 95) / 100;
		}

		/* Hack -- Monsters that multiply ignore the following reductions */
		if (!(r_ptr->flags2 & (RF2_MULTIPLY)))
		{
			/*Reduce damamge potential for monsters that move randomly */
			if ((r_ptr->flags1 & (RF1_RAND_25)) || (r_ptr->flags1 & (RF1_RAND_50)))
			{
				int reduce = 100;

				if (r_ptr->flags1 & (RF1_RAND_25)) reduce -= 25;
				if (r_ptr->flags1 & (RF1_RAND_50)) reduce -= 50;

				/*even moving randomly one in 8 times will hit the player*/
				reduce += (100 - reduce) / 8;

				/* adjust the melee damage*/
				melee_dam = (melee_dam * reduce) / 100;
			}

			/*monsters who can't move aren't nearly as much of a combat threat*/
			if (r_ptr->flags1 & (RF1_NEVER_MOVE))
			{
				if (r_ptr->flags6 & (RF6_TELE_TO | RF6_TELE_SELF_TO | RF6_BLINK))
				{
					/* Scale for frequency */
					melee_dam = melee_dam / 5 + 4 * melee_dam * r_ptr->freq_spell / 500;

					/* Incorporate spell failure chance */
					if (!(r_ptr->flags2 & RF2_STUPID)) melee_dam = melee_dam / 5 + 4 * melee_dam * MIN(75 + (rlev + 3) / 4, 100) / 500;
				}
				else if (r_ptr->flags2 & (RF2_HAS_AURA)) melee_dam /= 2;
				else if (r_ptr->flags2 & (RF2_INVISIBLE)) melee_dam /= 3;
				else melee_dam /= 5;
			}
		}

		/* But keep at a minimum */
		if (melee_dam < 1) melee_dam = 1;
	}

	/*
	 * Get the max damage attack
	 */

	if (dam < spell_dam) dam = spell_dam;
	if (dam < melee_dam) dam = melee_dam;

	/*
	 * Adjust for speed.  Monster at speed 120 will do double damage,
	 * monster at speed 100 will do half, etc.  Bonus for monsters who can haste self.
	 */
	dam = (dam * extract_energy[r_ptr->speed + (r_ptr->flags6 & RF6_HASTE ? 5 : 0)]) / 10;

	/*but deep in a minimum*/
	if (dam < 1) dam  = 1;

	/* We're done */
	return (dam);
}


/* Evaluate and adjust a monsters hit points for how easily the monster is damaged */
static long eval_hp_adjust(monster_race *r_ptr)
{
	long hp;
	int resists = 0;
	int ac = 0;
	int hide_bonus = 0;

	/* Get the monster base hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP)) hp = r_ptr->hdice * r_ptr->hside;
	else hp = r_ptr->hdice * (r_ptr->hside + 1) / 2;

	/* Never moves with no ranged attacks - high hit points count for less */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && !(r_ptr->freq_innate || r_ptr->freq_spell))
	{
		hp /= 2;
		if (hp < 1) hp = 1;
	}

	/* Just assume healers have more staying power */
	if (r_ptr->flags6 & RF6_HEAL) hp = (hp * 6) / 5;
	else if (r_ptr->flags6 & RF6_CURE) hp = (hp * 15) / 14;

	/* Miscellaneous improvements */
	if (r_ptr->flags2 & RF2_REGENERATE) {hp *= 10; hp /= 9;}
	if (r_ptr->flags9 & RF9_EVASIVE) 	{hp *= 3; hp /= 2;}
	if (r_ptr->flags2 & RF2_PASS_WALL) 	{hp *= 3; hp /= 2;}
	else if (r_ptr->flags6 & RF6_WRAITHFORM) {hp *= 6; hp /= 5;}

	/* Calculate hide bonus */
	if (r_ptr->flags2 & RF2_EMPTY_MIND) hide_bonus += 2;
	else
	{
		if (r_ptr->flags2 & RF2_COLD_BLOOD) hide_bonus += 1;
		if (r_ptr->flags2 & RF2_WEIRD_MIND) hide_bonus += 1;
	}

	/* Invisibility */
	if (r_ptr->flags2 & RF2_INVISIBLE)
	{
		hp = (hp * (r_ptr->level + hide_bonus + 1)) / MAX(1, r_ptr->level);
	}
	if (r_ptr->flags6 & RF6_INVIS)
	{
		hp = (hp * (r_ptr->level + hide_bonus)) / MAX(1, r_ptr->level);
	}

	/* Monsters that can teleport are a hassle, and can easily run away */
	if 	((r_ptr->flags6 & RF6_TPORT) ||
		 (r_ptr->flags6 & RF6_TELE_AWAY)||
		 (r_ptr->flags6 & RF6_TELE_LEVEL)) hp = (hp * 6) / 5;

	/* Monsters with resistances are harder to kill.
	   Therefore effective slays / brands against them are worth more. */
	if (r_ptr->flags3 & RF3_IM_ACID)	resists += 2;
	if (r_ptr->flags3 & RF3_IM_FIRE) 	resists += 2;
	if (r_ptr->flags3 & RF3_IM_COLD)	resists += 2;
	if (r_ptr->flags3 & RF3_IM_ELEC)	resists += 2;
	if (r_ptr->flags3 & RF3_IM_POIS)	resists += 2;

	/* Oppose elements */
	if (r_ptr->flags6 & RF6_OPPOSE_ELEM)
	{
		if (resists < 5)	resists = 5;
		else if (resists < 10)  resists++;
	}

	/* Hack - Immune to weapons & basic resists = tough */
	if (r_ptr->flags9 & RF9_IM_EDGED)	resists += 5;
	else if (r_ptr->flags9 & RF9_RES_EDGED)	resists += 2;
	if (r_ptr->flags9 & RF9_IM_BLUNT) 	resists += 5;
	else if (r_ptr->flags9 & RF9_RES_BLUNT) resists += 2;

	/* Bonus for multiple basic resists and weapon resists */
	if (resists >= 10) resists *= 6;
	else if (resists >= 10) resists *= 4;
	else if (resists >= 8) resists *= 3;
	else if (resists >= 6) resists *= 2;

	/* Reduce resists by vulnerabilities */
	if (r_ptr->flags3 & RF3_HURT_LITE)	resists -= 3;
	if (r_ptr->flags3 & RF3_HURT_ROCK)	resists -= 3;
	if (r_ptr->flags3 & RF3_HURT_WATER)	resists -= 1;
	if (r_ptr->flags2 & RF2_MUST_FLY)	resists -= 1;
	if (r_ptr->flags2 & RF2_MUST_SWIM)	resists -= 1;

	/* If quite resistant, reduce resists by defense holes */
	if (resists >= 5)
	{
		if (!(r_ptr->flags3 & RF3_NO_SLEEP))	resists -= 3;
		if (!(r_ptr->flags3 & RF3_NO_FEAR))	resists -= 2;
		if (!(r_ptr->flags3 & RF3_NO_CONF))	resists -= 2;
		if (!(r_ptr->flags3 & RF3_NO_STUN))	resists -= 1;
		if (!(r_ptr->flags9 & RF9_NO_SLOW))	resists -= 2;
		if (!(r_ptr->flags3 & RF9_RES_BLIND))	resists -= 2;
		if (!(r_ptr->flags9 & RF9_NO_CUTS))	resists -= 1;
		if (!(r_ptr->flags9 & RF9_RES_TPORT))	resists -= 2;

		if (resists < 0) resists = 0;
	}

	/* If quite resistant, bonus for high resists */
	if (resists >= 5)
	{
		if (r_ptr->flags9 & RF9_RES_DARK)	resists += 1;
		if (r_ptr->flags9 & RF9_RES_CHAOS)	resists += 1;
		if (r_ptr->flags9 & RF9_RES_LITE)	resists += 1;
		if (r_ptr->flags3 & RF3_RES_WATER)	resists += 1;
		if (r_ptr->flags3 & RF3_RES_NETHR)	resists += 1;
		if (r_ptr->flags3 & RF3_RES_LAVA)	resists += 1;
		if (r_ptr->flags3 & RF3_RES_NEXUS)	resists += 1;
		if (r_ptr->flags3 & RF3_RES_DISEN)	resists += 1;
	}

	/* Scale resists to ac */
	resists = resists * 25;

	/* Get the monster ac */
	ac = r_ptr->ac;

	/* Some abilities modify armour */
	if (r_ptr->flags2 & RF2_ARMOR) ac = ac * 4 / 3;
	if (r_ptr->flags6 & RF6_SHIELD) ac += 25;
	if (r_ptr->flags6 & RF6_BLESS) ac += 5;
	if (r_ptr->flags6 & RF6_BESERK) ac -= 5;

	/* Upper limit on ac */
	if (ac > 150) ac = 150;

	/* Immune to weapons */
	if (r_ptr->flags9 & RF9_IM_EDGED)	ac += 500;
	else if (r_ptr->flags9 & RF9_RES_EDGED)	ac += 200;
	if (r_ptr->flags9 & RF9_IM_BLUNT) 	ac += 500;
	else if (r_ptr->flags9 & RF9_RES_BLUNT) ac += 200;

	/* Sanity check */
	if (ac < 0) ac = 0;

	/* Easier to kill monster with magic */
	if (resists < ac)
	{
		/* Modify hit points by ac */
		hp += hp * resists / 250;
	}
	else
	{
		/* Modify hit points by ac */
		hp += hp * ac / 250;
	}

	/*boundry control*/
	if (hp < 1) hp = 1;

	return (hp);

}


/*
 * Evaluate the monster power ratings to be stored in r_info.raw
 */
errr eval_r_power(header *head)
{
	int i, j;
	byte lvl;
	long hp, av_hp, av_dam;
	long tot_hp[MAX_DEPTH];
	long dam;
	long *power;
	long tot_dam[MAX_DEPTH];
	long mon_count[MAX_DEPTH];
	monster_race *r_ptr = NULL;

	/* Reset the sum of all monster power values */
	tot_mon_power = 0;

	/* Allocate space for power */
	C_MAKE(power, z_info->r_max, long);

	/* Make sure all arrays start at zero */

	for (i = 0; i < MAX_DEPTH; i++)
	{
		tot_hp[i] = 0;
		tot_dam[i] = 0;
		mon_count[i] = 0;
	}

	/*
	 * Go through r_info and evaluate power ratings.
	 */
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Point at the "info" */
		r_ptr = (monster_race*)head->info_ptr + i;

		/* Set the current level */
		lvl = r_ptr->level;

		/* Maximum damage this monster can do in 10 game turns */
		dam = eval_max_dam(r_ptr);

		/* Adjust hit points based on resistances */
		hp = eval_hp_adjust(r_ptr);

		/* Hack -- set exp */
		if (lvl == 0) r_ptr->mexp = 0L;
		else r_ptr->mexp = (hp * dam) / (lvl * 25);

		if ((lvl) && (r_ptr->mexp < 1L)) r_ptr->mexp = 1L;

		/*
		 * Hack - at level 50 & above and 75 & above, we have to use an adjustment
		 * factor to prevent overflow.
                 */
		if (lvl >= 90)
		{
			hp /= 1000;
			dam /= 1000;
		}
		else if (lvl >= 65)
		{
			hp /= 100;
			dam /= 100;
		}
		else if (lvl >= 40)
		{
			hp /= 10;
			dam /= 10;
		}

		/* Define the power rating */
		power[i] = hp * dam;

		/* Adjust for group monsters.  Average in-level group size is 5 */
		if (r_ptr->flags1 & RF1_UNIQUE) ;

		else if (r_ptr->flags1 & RF1_FRIEND) power[i] *= 2;

		else if (r_ptr->flags1 & RF1_FRIENDS) power[i] *= 5;

		/* Adjust for multiplying monsters. This is modified by the speed,
                 * as fast multipliers are much worse than slow ones. We also adjust for
		 * ability to bypass walls or doors.
                 */
		if (r_ptr->flags2 & RF2_MULTIPLY)
		{
			if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL))
				power[i] = MAX(power[i], power[i] * extract_energy[r_ptr->speed
					+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)]);
			else if (r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR))
				power[i] = MAX(power[i], power[i] *  extract_energy[r_ptr->speed
					+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] * 3 / 2);
			else
				power[i] = MAX(power[i], power[i] * extract_energy[r_ptr->speed
					+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] / 2);
		}

		/*
		 * Update the running totals - these will be used as divisors later
		 * Total HP / dam / count for everything up to the current level
		 */
		for (j = lvl; j < (lvl == 0 ? lvl + 1: MAX_DEPTH); j++)
		{
			int count = 10;

			/*
			 * Uniques don't count towards monster power on the level.
			 */
			if (r_ptr->flags1 & RF1_UNIQUE) continue;

			/*
			 * Specifically placed monsters don't count towards monster power on the level.
			 */
			if (!(r_ptr->rarity)) continue;

			/*
			 * Hack -- provide adjustment factor to prevent overflow
			 */
			if ((j == 90) && (r_ptr->level < 90))
			{
				hp /= 10;
				dam /= 10;
			}

			if ((j == 65) && (r_ptr->level < 65))
			{
				hp /= 10;
				dam /= 10;
			}

			if ((j == 40) && (r_ptr->level < 40))
			{
				hp /= 10;
				dam /= 10;
			}

			/*
			 * Hack - if it's a group monster or multiplying monster, add several to the count
			 * so that the averages don't get thrown off
			 */

			if (r_ptr->flags1 & RF1_FRIEND) count = 20;
			else if (r_ptr->flags1 & RF1_FRIENDS) count = 50;

			if (r_ptr->flags2 & RF2_MULTIPLY)
			{
				if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL))
					count = MAX(1, extract_energy[r_ptr->speed
						+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)]) * count;
				else if (r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR))
					count = MAX(1, extract_energy[r_ptr->speed
						+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] * 3 / 2) * count;
				else
					count = MAX(1, extract_energy[r_ptr->speed
						+ (r_ptr->flags6 & RF6_HASTE ? 5 : 0)] / 2) * count;
			}

			/*
			 * Very rare monsters count less towards total monster power on the level.
			 */
			if (r_ptr->rarity > count)
			{
				hp = hp * count / r_ptr->rarity;
				dam = dam * count / r_ptr->rarity;

				count = r_ptr->rarity;
			}

			tot_hp[j] += hp;
			tot_dam[j] += dam;

			mon_count[j] += count / r_ptr->rarity;
		}

	}

	/* Apply divisors now */
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Point at the "info" */
		r_ptr = (monster_race*)head->info_ptr + i;

		/* Extract level */
		lvl = r_ptr->level;

		/* Paranoia */
		if (tot_hp[lvl] != 0 && tot_dam[lvl] != 0)
		{
			/* Divide by average HP and av damage for all in-level monsters */
			/* Note we have factored in the above 'adjustment factor' */
			av_hp = tot_hp[lvl] * 10 / mon_count[lvl];
			av_dam = tot_dam[lvl] * 10 / mon_count[lvl];

			/* XXX Justifiable paranoia - avoid divide by zero errors */
			if (av_hp > 0) power[i] = power[i] / av_hp;
			if (av_dam > 0) power[i] = power[i] / av_dam;

			/* Assign monster power */
			r_ptr->power = (s16b)power[i];

			/* Never less than 1 */
			if (r_ptr->power < 1) r_ptr->power = 1;
		}
	}

	/* Free power array */
	FREE(power);

	/* Success */
	return(0);
}


/*
 * Evaluate the ego slay ratings to be stored in e_info.raw
 */
errr eval_e_power(header *head)
{
	int i;
	ego_item_type *e_ptr = NULL;

	/* Precompute values for ego item slays for ego items */
	for (i = 0; i < z_info->e_max; i++)
	{
		/* Point at the "info" */
		e_ptr = (ego_item_type*)head->info_ptr + i;

		e_ptr->slay_power = slay_power(slay_index(e_ptr->flags1, e_ptr->flags2, e_ptr->flags3, e_ptr->flags4));
	}

	/* Success */
	return(0);
}


#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */
