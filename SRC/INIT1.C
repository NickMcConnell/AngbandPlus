/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
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


/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
int color_char_to_attr(char c)
{
	switch (c)
	{
		case 'd': return (TERM_DARK);
		case 'w': return (TERM_WHITE);
		case 's': return (TERM_SLATE);
		case 'o': return (TERM_ORANGE);
		case 'r': return (TERM_RED);
		case 'g': return (TERM_GREEN);
		case 'b': return (TERM_BLUE);
		case 'u': return (TERM_UMBER);

		case 'D': return (TERM_L_DARK);
		case 'W': return (TERM_L_WHITE);
		case 'v': return (TERM_VIOLET);
		case 'y': return (TERM_YELLOW);
		case 'R': return (TERM_L_RED);
		case 'G': return (TERM_L_GREEN);
		case 'B': return (TERM_L_BLUE);
		case 'U': return (TERM_L_UMBER);
	}

	return (-1);
}


#ifdef ALLOW_TEMPLATES


/*
 * Hack -- error tracking
 */
extern s16b error_idx;
extern s16b error_line;

/*** Helper arrays for parsing ascii template files ***/

/*
 * Room Special Flags
 */

static cptr d_info_special_flag[] =
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



static cptr d_info_level_flag[] =
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
 * Monster Blow Methods
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
	"XXX1",
	"BUTT",
	"CRUSH",
	"ENGULF",
	"XXX2",
	"CRAWL",
	"DROOL",
	"SPIT",
	"XXX3",
	"GAZE",
	"WAIL",
	"SPORE",
	"XXX4",
	"BEG",
	"INSULT",
	"MOAN",
	"XXX5",
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
 * Monster Blow Effects
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
	"CAN_TINY",
	"CAN_DIG",
	"HIDE_ITEM",
	"HIDE_DEEP",
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
        "CAN_HIDE",
        "GROUND",
        "OUTSIDE",
        "EASY_CLIMB",
        "NEED_TREE",
        "NEED_WALL",
        "TOWN",
        "BLOOD",
        "DUST",
        "SLIME",
        "TREE",
        "TREE_BIG",
        "XXX3",
        "XXX4",
        "COLLAPSE",
        "ERUPT",
        "STRIKE",
        "DYNAMIC"
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
	"FORCE_EXTRA",
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
	"MOVE_BODY",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
        "SNEAKY",
        "HAS_AURA",
        "PRIEST",
	"MAGE",
        "WARRIOR",
	"GENIUS",
	"BRAIN_7",
	"BRAIN_8"
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
	"TINY",
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
	"XXX5X3",
	"RES_NETH",
	"RES_WATE",
	"RES_PLAS",
	"RES_NEXU",
	"RES_DISE",
	"XXX6X3",
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
	"XXX7X4",
	"XXX8X4"
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
	"CAUSE_1",
	"CAUSE_2",
	"CAUSE_3",
	"CAUSE_4",
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
	"XXX1X6",
	"HEAL",
	"XXX2X6",
	"BLINK",
	"TPORT",
	"XXX3X6",
	"XXX4X6",
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
         RF7_HAS_SCALE | RF7_HAS_HEAD | RF7_HAS_BLOOD |
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
         RF7_DROP_TOOL | RF7_DROP_JUNK | RF7_DROP_MUSIC | RF7_DROP_LITE),
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
        "XXX1",
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
 *
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
 *
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

/* SF2 - timed abilities and modifying level */
/*
 *
 */

static cptr s_info_flags2[] =
{
        "AGGRAVATE",
        "CURSE_WEAPON",
        "CURSE_ARMOR",
        "CREATE_STAIR",
        "TELE_LEVEL",
        "ALTER_LEVEL",
        "GENOCIDE",
        "MASS_GENOCIDE",
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

/* SF3 - healing self, and untimed improvements */
/*
 *
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

static cptr s_info_types[] =
{
        "",
        "RECHARGE",
        "IDENT_TVAL",
        "ENCHANT_TVAL",
        "BRAND_WEAPON",
        "BRAND_ARMOR",
        "BRAND_ITEM",
        "BRAND_BOLTS",
        "WARD_GLYPH",
        "WARD_TRAP",
        "SUMMON",
        "SUMMON_RACE",
        "CREATE_RACE",
        "CREATE_KIND",
        "EARTHQUAKE",
        "DESTRUCTION",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
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
 * Initialize the "z_info" structure, by parsing an ascii "template" file
 */
errr init_z_info_txt(FILE *fp, char *buf)
{
	/* Not ready yet */
	bool okay = FALSE;


	/* Hack - just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


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
			    (v1 != z_head->v_major) ||
			    (v2 != z_head->v_minor) ||
			    (v3 != z_head->v_patch))
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


		/* Hack - Verify 'M:x:' format */
		if (buf[0] != 'M') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		if (!buf[2]) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		if (buf[3] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);


		/* Process 'F' for "Maximum f_info[] index" */
		if (buf[2] == 'F')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->f_max = max;

			/* Next... */
			continue;
		}

		/* Process 'D' for "Maximum d_info[] index" */
		if (buf[2] == 'D')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->d_max = max;

			/* Next... */
			continue;
		}


		/* Process 'K' for "Maximum k_info[] index" */
		if (buf[2] == 'K')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->k_max = max;

			/* Next... */
			continue;
		}


		/* Process 'A' for "Maximum a_info[] index" */
		if (buf[2] == 'A')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->a_max = max;

			/* Next... */
			continue;
		}


		/* Process 'E' for "Maximum e_info[] index" */
		if (buf[2] == 'E')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->e_max = max;

			/* Next... */
			continue;
		}

		/* Process 'X' for "Maximum x_info[] index" */
		if (buf[2] == 'X')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->x_max = max;

			/* Next... */
			continue;
		}



		/* Process 'R' for "Maximum r_info[] index" */
		if (buf[2] == 'R')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->r_max = max;

			/* Next... */
			continue;
		}


		/* Process 'V' for "Maximum v_info[] index" */
		if (buf[2] == 'V')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->v_max = max;

			/* Next... */
			continue;
		}

		/* Process 'T' for "Maximum t_info[] index" */
		if (buf[2] == 'T')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->t_max = max;

			/* Next... */
			continue;
		}

                /* Process 'U' for "Maximum u_info[] index" */
                if (buf[2] == 'U')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
                        z_info->u_max = max;

			/* Next... */
			continue;
		}


		/* Process 'Q' for "Maximum q_info[] index" */
		if (buf[2] == 'Q')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->q_max = max;

			/* Next... */
			continue;
		}


		/* Process 'P' for "Maximum p_info[] index" */
		if (buf[2] == 'P')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->p_max = max;

			/* Next... */
			continue;
		}


		/* Process 'C' for "Maximum c_info[] index" */
		if (buf[2] == 'C')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->c_max = max;

			/* Next... */
			continue;
		}


		/* Process 'H' for "Maximum h_info[] index" */
		if (buf[2] == 'H')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->h_max = max;

			/* Next... */
			continue;
		}

                /* Process 'W' for "Maximum w_info[] index" */
		if (buf[2] == 'W')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->w_max = max;

			/* Next... */
			continue;
		}

		/* Process 'S' for "Maximum s_info[] index" */
		if (buf[2] == 'S')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->s_max = max;

			/* Next... */
			continue;
		}

                /* Process 'Y' for "Maximum y_info[] index" */
                if (buf[2] == 'Y')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
                        z_info->y_max = max;

			/* Next... */
			continue;
		}


		/* Process 'B' for "Maximum b_info[] subindex" */
		if (buf[2] == 'B')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->b_max = max;

			/* Next... */
			continue;
		}


		/* Process 'O' for "Maximum o_list[] index" */
		if (buf[2] == 'O')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->o_max = max;

			/* Next... */
			continue;
		}


		/* Process 'M' for "Maximum m_list[] index" */
		if (buf[2] == 'M')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->m_max = max;

			/* Next... */
			continue;
		}


		/* Process 'N' for "Fake name size" */
                if (buf[2] == 'N')
		{
			long max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->fake_name_size = max;

			/* Next... */
			continue;
		}


                /* Process 'I' for "Fake text size" */
                if (buf[2] == 'I')
		{
			long max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->fake_text_size = max;

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr init_v_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	vault_type *v_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	v_head->name_size = 0;
	v_head->text_size = 0;

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
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != v_head->v_major) ||
			    (v2 != v_head->v_minor) ||
			    (v3 != v_head->v_patch))
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
			if (i >= v_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			v_ptr = &v_info[i];

			/* Hack -- Verify space */
			if (v_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!v_ptr->name) v_ptr->name = ++v_head->name_size;

			/* Append chars to the name */
			strcpy(v_name + v_head->name_size, s);

			/* Advance the index */
			v_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current v_ptr */
		if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (v_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!v_ptr->text) v_ptr->text = ++v_head->text_size;

			/* Append chars to the name */
			strcpy(v_text + v_head->text_size, s);

			/* Advance the index */
			v_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Process 'X' for "Extra info" (one line only) */
		if (buf[0] == 'X')
		{
			int typ, rat, hgt, wid;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&typ, &rat, &hgt, &wid)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;
			v_ptr->hgt = hgt;
			v_ptr->wid = wid;

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++v_head->name_size;
	++v_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}

/*
 * Grab one flag in an desc_type from a textual string
 */
static errr grab_one_room_special_flag(desc_type *d_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 8; i++)
	{
		if (streq(what, d_info_special_flag[i]))
		{
                        d_ptr->flags |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown room special flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}




/*
 * Grab one flag in an desc_type from a textual string
 */
static errr grab_one_room_level_flag(desc_type *d_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 8; i++)
	{
		if (streq(what, d_info_level_flag[i]))
		{
			d_ptr->l_flag |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown room level flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the "d_info" array, by parsing an ascii "template" file
 */
errr init_d_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s,*t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	desc_type *d_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;

	/* Prepare the "fake" stuff */
	d_head->text_size = 0;

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
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != d_head->v_major) ||
			    (v2 != d_head->v_minor) ||
			    (v3 != d_head->v_patch))
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



		/* Process 'N' for "New/Number" */
		if (buf[0] == 'N')
		{
			int prv, nxt, prc, min;

			/* Hack - get the index */
			i = error_idx + 1;

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= d_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			d_ptr = &d_info[i];

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

			/* Next... */
			continue;
		}
		/* There better be a current d_ptr */
		if (!d_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

                /* Process 'A' for "Name1" */
                if (buf[0] == 'A')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (d_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

                        /* Advance and Save the name index */
                        if (!d_ptr->name1) d_ptr->name1 = ++d_head->name_size;

			/* Append chars to the name */
                        strcpy(d_name + d_head->name_size, s);

			/* Advance the index */
                        d_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* Process 'B' for "Name2" */
                if (buf[0] == 'B')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (d_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

                        /* Advance and Save the name index */
                        if (!d_ptr->name2) d_ptr->name2 = ++d_head->name_size;

			/* Append chars to the name */
                        strcpy(d_name + d_head->name_size, s);

			/* Advance the index */
                        d_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (d_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!d_ptr->text) d_ptr->text = ++d_head->text_size;

			/* Append chars to the name */
			strcpy(d_text + d_head->text_size, s);

			/* Advance the index */
			d_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'S' for "Room Special Flags" (multiple lines) */
		if (buf[0] == 'S')
		{
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

				/* Parse this entry */
				if (0 != grab_one_room_special_flag(d_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;

		}

		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			/* Paranoia */
			if (!buf[2]) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			d_ptr->r_char = buf[2];

			/* Next... */
			continue;
		}

		/* Process 'K' for "Kind" (one line only) */
		if (buf[0] == 'K')
		{
			int kind;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
					&kind)) return (1);

			/* Save the values */
			d_ptr->tval = kind;

			/* Next... */
			continue;
		}

                /* Process 'F' for "Feature" (one line only) */
                if (buf[0] == 'F')
		{
                        int feat;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
                                        &feat)) return (1);

			/* Save the values */
                        d_ptr->feat = feat;

			/* Next... */
			continue;
		}


	/* Process 'R' for "Race flag" (once only) */
		if (buf[0] == 'R')
		{
			int n1;

			/* Set to the first field */
			s=buf+2;

			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags1[n1]; n1++)
			{
                                if (streq(s, r_info_flags1[n1])) break;
			}

			if (n1<32)
			{
                                d_ptr->r_flag = n1+1;

				continue;
			}

                	/* Analyze the race flag */
                        for (n1 = 0; r_info_flags2[n1]; n1++)
			{
                                if (streq(s, r_info_flags2[n1])) break;
			}

			if (n1<32)
			{
                                d_ptr->r_flag = n1+33;

				continue;
			}

			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags3[n1]; n1++)
			{
                                if (streq(s, r_info_flags3[n1])) break;
			}


			if (n1<32)
			{
                                d_ptr->r_flag = n1 + 65;

				continue;
			}


			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags4[n1]; n1++)
			{
                                if (streq(s, r_info_flags4[n1])) break;
			}


			if (n1<32)
                        {                         
                                d_ptr->r_flag = n1 + 97;

				continue;
			}

			/* Oops */
			msg_format("Unknown room race flag '%s'.", s);

			/* Fail */
                        return(PARSE_ERROR_GENERIC);
		}


		/* Process 'L' for "Room Level Flags" (multiple lines) */
		if (buf[0] == 'L')
		{
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

				/* Parse this entry */
				if (0 != grab_one_room_level_flag(d_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;

		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


        /* Complete the "name" and "text" size */
        ++d_head->name_size;
	++d_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);

	/* Success */
	return (0);
}

/*
 * Grab one flag in an feature_type from a textual string
 */
static errr grab_one_feat_action(feature_type *f_ptr, cptr what, int count)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, f_info_flags1[i]))
		{
			f_ptr->state[count].action=i;
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, f_info_flags2[i]))
		{
			f_ptr->state[count].action=i+32;
			return (0);
		}
	}

        /* Check flags3 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, f_info_flags3[i]))
		{
                        f_ptr->state[count].action=i+64;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown feature action '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Grab one flag in an feature_type from a textual string
 */
static errr grab_one_feat_flag(feature_type *f_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, f_info_flags1[i]))
		{
			f_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, f_info_flags2[i]))
		{
			f_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

        /* Check flags3 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, f_info_flags3[i]))
		{
                        f_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown feature flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s,*t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	feature_type *f_ptr = NULL;

	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	f_head->name_size = 0;
	f_head->text_size = 0;

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
			    (v1 != f_head->v_major) ||
			    (v2 != f_head->v_minor) ||
			    (v3 != f_head->v_patch))
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


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			int n1;

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
			if (i >= f_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			f_ptr = &f_info[i];

			/* Hack -- Verify space */
			if (f_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!f_ptr->name) f_ptr->name = ++f_head->name_size;

			/* Append chars to the name */
			strcpy(f_name + f_head->name_size, s);

			/* Advance the index */
			f_head->name_size += strlen(s);

			/* Default "mimic" */
			f_ptr->mimic = i;

			/* Default "unseen" */
                        f_ptr->unseen = i;

			/* Set default power */
			f_ptr->power=0;

			/* Set default spell */
			f_ptr->spell=0;

			/* Set default state */
                        f_ptr->defaults = i;

			/* Set other states */
			for (n1=0;n1<MAX_FEAT_STATES;n1++)
			{
				f_ptr->state[n1].action=FS_FLAGS_END;
				f_ptr->state[n1].result=i;
			}

			/* Next... */
			continue;
		}

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{

			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (f_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			/*if (!f_ptr->text)*/ f_ptr->text = ++f_head->text_size;

			/* Append chars to the name */
			strcpy(f_text + f_head->text_size, s);

			/* Advance the index */
			f_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Process 'M' for "Mimic" (one line only) */
		if (buf[0] == 'M')
		{
			int mimic;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
					&mimic)) return (1);

			/* Save the values */
			f_ptr->mimic = mimic;

			/* Next... */
			continue;
		}

                /* Process 'U' for "Unseen" (one line only) */
                if (buf[0] == 'U')
		{
                        int unseen;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
                                        &unseen)) return (1);

			/* Save the values */
                        f_ptr->unseen = unseen;

			/* Next... */
			continue;
		}

		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			int tmp;

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

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int level, rarity, priority,edge;
			

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&level, &rarity, &priority, &edge)) return (1);

			/* Save the values */
			f_ptr->level = level;
			f_ptr->rarity = rarity;
			f_ptr->priority = priority;
			f_ptr->edge = edge;


			/* Next... */
			continue;
		}

		/* Process 'K' for "States" (up to four lines + default (which cannot be last)) */
		if (buf[0] == 'K')
		{
			/* Find the next empty state slot (if any) */
			for (i = 0; i < 4; i++) if (f_ptr->state[i].action == FS_FLAGS_END) break;

			/* Oops, no more slots */
			if (i == 4) return (PARSE_ERROR_GENERIC);

			/* Analyze the first field */
			for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

			/* Terminate the field (if necessary) */
			if (*t == ':') *t++ = '\0';

			/* Is this default entry? */
			if (streq(s, "DEFAULT"))
			{

				/* Analyze result */
                                f_ptr->defaults = atoi(t);

				/* Next... */
				continue;

			}

			/* Parse this entry */
			if (0 != grab_one_feat_action(f_ptr, s, i)) return (5);

			/* Analyze result */
			f_ptr->state[i].result = atoi(t);

			/* Next... */
			continue;
		}

		/* Process 'O' for "Objects" (one line only) */
		if (buf[0] == 'O')
		{
			int k_idx;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
					&k_idx)) return (1);

			/* Save the values */
			f_ptr->k_idx = k_idx;

			/* Next... */
			continue;
		}

		/* Process 'T' for "Traps" (one line only) */
		if (buf[0] == 'T')
		{
			int n1;


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
			if (!r_info_blow_effect[n1]) return (1);

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

			/* Next... */
			continue;
		}

		/* Process 'S' for "Spell" (once only) */
		if (buf[0] == 'S')
		{
			int n1;

			/* Set to the first field */
			s=buf+2;

			/* Analyze the spell */
			for (n1 = 0; r_info_flags4[n1]; n1++)
			{
				if (streq(s, r_info_flags4[n1])) break;
			}

			if (n1<32)
			{
				f_ptr->spell = n1+96;

				continue;
			}

			/* Analyze the spell */
			for (n1 = 0; r_info_flags5[n1]; n1++)
			{
				if (streq(s, r_info_flags5[n1])) break;
			}


			if (n1<32)
			{
				f_ptr->spell = n1 + 128;

				continue;
			}


			/* Analyze the method */
			for (n1 = 0; r_info_flags6[n1]; n1++)
			{
				if (streq(s, r_info_flags6[n1])) break;
			}


			if (n1<32)
			{
				f_ptr->spell = n1 + 160;

				continue;
			}

			/* Oops */
			msg_format("Unknown feature spell '%s'.", s);

			/* Fail */
			return(5);
		}

	

		/* Process 'F' for "Feature Flags" (multiple lines) */
		if (buf[0] == 'F')
		{
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
				if (0 != grab_one_feat_flag(f_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;

		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++f_head->name_size;
	++f_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			k_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			k_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			k_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown object flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr init_k_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	object_kind *k_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	k_head->name_size = 0;
	k_head->text_size = 0;

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
			    (v1 != k_head->v_major) ||
			    (v2 != k_head->v_minor) ||
			    (v3 != k_head->v_patch))
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
			if (i >= k_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			k_ptr = &k_info[i];

			/* Hack -- Verify space */
			if (k_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!k_ptr->name) k_ptr->name = ++k_head->name_size;

			/* Append chars to the name */
			strcpy(k_name + k_head->name_size, s);

			/* Advance the index */
			k_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (k_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!k_ptr->text) k_ptr->text = ++k_head->text_size;

			/* Append chars to the name */
			strcpy(k_text + k_head->text_size, s);

			/* Advance the index */
			k_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#endif


		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

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

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int tval, sval, pval;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			k_ptr->tval = tval;
			k_ptr->sval = sval;
			k_ptr->pval = pval;

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int level, extra, wgt;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
					&level, &extra, &wgt, &cost)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			k_ptr->level = level;
			k_ptr->extra = extra;
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

			/* Next... */
			continue;
		}

                /* Process 'Y' for "Runes" (one line only) */
                if (buf[0] == 'Y')
		{
                        int runest,runesc;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                        &runest, &runesc)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        k_ptr->runest = runest;
                        k_ptr->runesc = runesc;

			/* Next... */
			continue;
		}


		/* Process 'A' for "Allocation" (one line only) */
		if (buf[0] == 'A')
		{
			int i;

			/* XXX XXX XXX Simply read each number following a colon */
			for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
			{
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

			/* Next... */
			continue;
		}

		/* Hack -- Process 'P' for "power" and such */
		if (buf[0] == 'P')
		{
			int ac, hd1, hd2, th, td, ta;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
					&ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

			k_ptr->ac = ac;
			k_ptr->dd = hd1;
			k_ptr->ds = hd2;
			k_ptr->to_h = th;
			k_ptr->to_d = td;
			k_ptr->to_a =  ta;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{
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

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++k_head->name_size;
	++k_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			a_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			a_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			a_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown artifact flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}




/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr init_a_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	artifact_type *a_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


		/* Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf+2, "%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != a_head->v_major) ||
			    (v2 != a_head->v_minor) ||
			    (v3 != a_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= a_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			a_ptr = &a_info[i];

			/* Hack -- Verify space */
			if (a_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!a_ptr->name) a_ptr->name = ++a_head->name_size;

			/* Append chars to the name */
			strcpy(a_name + a_head->name_size, s);

			/* Advance the index */
			a_head->name_size += strlen(s);

			/* Ignore everything */
                        a_ptr->flags2 |= (TR2_IGNORE_MASK);

			/* Next... */
			continue;
		}

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (a_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!a_ptr->text) a_ptr->text = ++a_head->text_size;

			/* Append chars to the name */
			strcpy(a_text + a_head->text_size, s);

			/* Advance the index */
			a_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#endif

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int tval, sval, pval;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			a_ptr->tval = tval;
			a_ptr->sval = sval;
			a_ptr->pval = pval;

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int level, rarity, wgt;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
					&level, &rarity, &wgt, &cost)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			a_ptr->level = level;
			a_ptr->rarity = rarity;
			a_ptr->weight = wgt;
			a_ptr->cost = cost;

			/* Next... */
			continue;
		}

		/* Process 'P' for "power" and such */
		if (buf[0] == 'P')
		{
			int ac, hd1, hd2, th, td, ta;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
					&ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

			a_ptr->ac = ac;
			a_ptr->dd = hd1;
			a_ptr->ds = hd2;
			a_ptr->to_h = th;
			a_ptr->to_d = td;
			a_ptr->to_a =  ta;

			/* Next... */
			continue;
		}

		/* Process 'F' for flags */
		if (buf[0] == 'F')
		{
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

			/* Next... */
			continue;
		}

		/* Process 'A' for "Activation & time" */
		if (buf[0] == 'A')
		{
                        int act, time, rand;

			/* Scan for the values */
                        if (3 != sscanf(buf + 2, "%d:%d:%d",
                                        &act, &time, &rand)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        a_ptr->activation = act;
			a_ptr->time = time;
			a_ptr->randtime = rand;

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++a_head->name_size;
	++a_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			e_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			e_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			e_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown ego-item flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}




/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr init_e_info_txt(FILE *fp, char *buf)
{
	int i;

	int cur_t = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	ego_item_type *e_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


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
			    (v1 != e_head->v_major) ||
			    (v2 != e_head->v_minor) ||
			    (v3 != e_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= e_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			e_ptr = &e_info[i];

			/* Hack -- Verify space */
			if (e_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!e_ptr->name) e_ptr->name = ++e_head->name_size;

			/* Append chars to the name */
			strcpy(e_name + e_head->name_size, s);

			/* Advance the index */
			e_head->name_size += strlen(s);

			/* Start with the first of the tval indices */
			cur_t = 0;

			/* Next... */
			continue;
		}

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (e_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!e_ptr->text) e_ptr->text = ++e_head->text_size;

			/* Append chars to the name */
			strcpy(e_text + e_head->text_size, s);

			/* Advance the index */
			e_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#endif

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int level, rarity, pad2;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
					&level, &rarity, &pad2, &cost)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			e_ptr->level = level;
			e_ptr->rarity = rarity;
			/* e_ptr->weight = wgt; */
			e_ptr->cost = cost;

			/* Next... */
			continue;
		}

                /* Process 'Y' for "Runes" (one line only) */
                if (buf[0] == 'Y')
		{
                        int runest,runesc;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                        &runest, &runesc)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        e_ptr->runest = runest;
                        e_ptr->runesc = runesc;

			/* Next... */
			continue;
		}

		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
			int slot, rating, xtra;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&slot, &rating, &xtra)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			e_ptr->slot = slot;
			e_ptr->rating = rating;
			e_ptr->xtra = xtra;

			/* Next... */
			continue;
		}

		/* Process 'T' for "Types allowed" (up to three lines) */
		if (buf[0] == 'T')
		{
			int tval, sval1, sval2;

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

			/* Next... */
			continue;
		}

		/* Hack -- Process 'C' for "creation" */
		if (buf[0] == 'C')
		{
			int th, td, ta, pv;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&th, &td, &ta, &pv)) return (PARSE_ERROR_GENERIC);

			e_ptr->max_to_h = th;
			e_ptr->max_to_d = td;
			e_ptr->max_to_a = ta;
			e_ptr->max_pval = pv;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{
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

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++e_head->name_size;
	++e_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}



/*
 * Initialize the "x_info" array, by parsing an ascii "template" file
 */
errr init_x_info_txt(FILE *fp, char *buf)
{
	int i;

        char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	flavor_type *x_ptr = NULL;


	int cur_t = 0;



	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


		/* Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf+2, "%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != x_head->v_major) ||
			    (v2 != x_head->v_minor) ||
			    (v3 != x_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= x_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			x_ptr = &x_info[i];

			/* Hack -- Verify space */
			if (x_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!x_ptr->name) x_ptr->name = ++x_head->name_size;

			/* Append chars to the name */
			strcpy(x_name + x_head->name_size, s);

			/* Advance the index */
			x_head->name_size += strlen(s);

			/* Start with the first of the tval indices */
			cur_t = 0;

			/* Next... */
			continue;
		}

		/* There better be a current x_ptr */
		if (!x_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (x_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!x_ptr->text) x_ptr->text = ++x_head->text_size;

			/* Append chars to the name */
			strcpy(x_text + x_head->text_size, s);

			/* Advance the index */
			x_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

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
			x_ptr->d_attr = tmp;
			x_ptr->d_char = sym;

			/* Next... */
			continue;
		}



		/* Process 'T' for "Types allowed" (up to five lines) */
		if (buf[0] == 'T')
		{
			int tval, sval1, sval2;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&tval, &sval1, &sval2)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			x_ptr->tval[cur_t] = (byte)tval;
			x_ptr->min_sval[cur_t] = (byte)sval1;
			x_ptr->max_sval[cur_t] = (byte)sval2;

			/* increase counter for 'possible tval' index */
			cur_t++;

			/* only five T: lines allowed */
			if (cur_t > 5) return (PARSE_ERROR_GENERIC);

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++x_head->name_size;
	++x_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}



/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race *r_ptr, cptr what)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags1[i]))
		{
			r_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags2[i]))
		{
			r_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
			r_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

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
	int i;

	/* Scan flags4 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags4[i]))
		{
			r_ptr->flags4 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags5 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags5[i]))
		{
			r_ptr->flags5 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags6 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags6[i]))
		{
			r_ptr->flags6 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}




/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr init_r_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	monster_race *r_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
	r_head->name_size = 0;
	r_head->text_size = 0;

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
			    (v1 != r_head->v_major) ||
			    (v2 != r_head->v_minor) ||
			    (v3 != r_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= r_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			r_ptr = &r_info[i];

			/* Hack -- Verify space */
			if (r_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!r_ptr->name) r_ptr->name = ++r_head->name_size;

			/* Append chars to the name */
			strcpy(r_name + r_head->name_size, s);

			/* Advance the index */
			r_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (r_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!r_ptr->text) r_ptr->text = ++r_head->text_size;

			/* Append chars to the name */
			strcpy(r_text + r_head->text_size, s);

			/* Advance the index */
			r_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

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


			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
                        int spd, hp1, hp2, aaf, ac, slp;

			/* Scan for the other values */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
					&spd, &hp1, &hp2, &aaf, &ac, &slp)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			r_ptr->speed = spd;

			r_ptr->hdice = hp1;

			r_ptr->hside = hp2;

			/* Save the values */
			r_ptr->aaf = aaf;
			r_ptr->ac = ac;
			r_ptr->sleep = slp;

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int lev, rar, pad;
			long exp;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
					&lev, &rar, &pad, &exp)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
			r_ptr->extra = pad;
			r_ptr->mexp = exp;
#if 0
                        /* Check */
                        if (r_ptr->rarity == 0) msg_format("Not encountered: %s",r_name + r_ptr->name);
#endif
			/* Next... */
			continue;
		}

		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
		{
			int n1, n2;

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

			/* Next... */
			continue;
		}

		/* Process 'F' for "Basic Flags" (multiple lines) */
		if (buf[0] == 'F')
		{
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

                        /* Carries heavy armor*/
                        if (r_ptr->flags2 & (RF2_ARMOR))
                        {
                                r_ptr->flags7 |= RF7_DROP_ARMOR;
                        }

                        /* Carries rod/staff/wand - priest and mages, but not shamans */
                        if ((r_ptr->flags2 & (RF2_MAGE | RF2_PRIEST)) && !((r_ptr->flags2 & (RF2_PRIEST)) && (r_ptr->flags2 & (RF2_MAGE)) ))
                        {
                                r_ptr->flags7 |= (RF7_DROP_RSW);
                        }

                        /* Carries writing - priest and mages */
                        if (r_ptr->flags2 & (RF2_MAGE | RF2_PRIEST))
                        {
                                r_ptr->flags7 |= (RF7_DROP_WRITING);
                        }

                        /* Death by Physical attack -- non-living monster */
                        if ((r_ptr->flags3 & (RF3_DEMON)) ||
                                         (r_ptr->flags3 & (RF3_UNDEAD)))
                        {
                                r_ptr->flags3 |= RF3_NONLIVING;
                        }

			/* Next... */
			continue;
		}

		/* Process 'S' for "Spell Flags" (multiple lines) */
		if (buf[0] == 'S')
		{
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

				/* XXX XXX XXX Hack -- Read spell frequency */
				if (1 == sscanf(s, "1_IN_%d", &i))
				{
					/* Extract a "frequency" */
					r_ptr->freq_spell = r_ptr->freq_inate = 100 / i;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
				if (0 != grab_one_spell_flag(r_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++r_head->name_size;
	++r_head->text_size;


	/* XXX XXX XXX The ghost is unused */

	/* Mega-Hack -- acquire "ghost" */
	r_ptr = &r_info[z_info->r_max-1];

	/* Get the next index */
	r_ptr->name = r_head->name_size;
	r_ptr->text = r_head->text_size;

	/* Save some space for the ghost info */
	r_head->name_size += 64;
	r_head->text_size += 64;

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


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Grab one flag in a player_race from a textual string
 */
static errr grab_one_racial_flag(player_race *pr_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			pr_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			pr_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			pr_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown player flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "p_info" array, by parsing an ascii "template" file
 */
errr init_p_info_txt(FILE *fp, char *buf)
{
	int i, j;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	player_race *pr_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


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
			    (v1 != p_head->v_major) ||
			    (v2 != p_head->v_minor) ||
			    (v3 != p_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= p_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			pr_ptr = &p_info[i];

			/* Hack -- Verify space */
			if (p_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!pr_ptr->name) pr_ptr->name = ++p_head->name_size;

			/* Append chars to the name */
			strcpy(p_name + p_head->name_size, s);

			/* Advance the index */
			p_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'S' for "Stats" (one line only) */
		if (buf[0] == 'S')
		{
			int adj;

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

			/* Next... */
			continue;
		}

		/* Process 'R' for "Racial Skills" (one line only) */
		if (buf[0] == 'R')
		{
			int dis, dev, sav, stl, srh, fos, thn, thb;

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

			/* Next... */
			continue;
		}

		/* Process 'X' for "Extra Info" (one line only) */
		if (buf[0] == 'X')
		{
			int mhp, exp, infra;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&mhp, &exp, &infra)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			pr_ptr->r_mhp = mhp;
			pr_ptr->r_exp = exp;
			pr_ptr->infra = infra;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'I' for "info" and such */
		if (buf[0] == 'I')
		{
			int hist, b_age, m_age;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&hist, &b_age, &m_age)) return (PARSE_ERROR_GENERIC);

			pr_ptr->hist = hist;
			pr_ptr->b_age = b_age;
			pr_ptr->m_age = m_age;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'I' for "info" and such */
		if (buf[0] == 'I')
		{
			int hist, b_age, m_age;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&hist, &b_age, &m_age)) return (PARSE_ERROR_GENERIC);

			pr_ptr->hist = hist;
			pr_ptr->b_age = b_age;
			pr_ptr->m_age = m_age;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'H' for "Height" */
		if (buf[0] == 'H')
		{
			int m_b_ht, m_m_ht, f_b_ht, f_m_ht;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&m_b_ht, &m_m_ht, &f_b_ht, &f_m_ht)) return (PARSE_ERROR_GENERIC);

			pr_ptr->m_b_ht = m_b_ht;
			pr_ptr->m_m_ht = m_m_ht;
			pr_ptr->f_b_ht = f_b_ht;
			pr_ptr->f_m_ht = f_m_ht;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'W' for "Weight" */
		if (buf[0] == 'W')
		{
			int m_b_wt, m_m_wt, f_b_wt, f_m_wt;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&m_b_wt, &m_m_wt, &f_b_wt, &f_m_wt)) return (PARSE_ERROR_GENERIC);

			pr_ptr->m_b_wt = m_b_wt;
			pr_ptr->m_m_wt = m_m_wt;
			pr_ptr->f_b_wt = f_b_wt;
			pr_ptr->f_m_wt = f_m_wt;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{
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

			/* Next... */
			continue;
		}

		/* Hack -- Process 'C' for class choices */
		if (buf[0] == 'C')
		{
			/* Parse every entry textually */
			for (s = buf + 2; *s; )
			{
                                int class;

				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while ((*t == ' ') || (*t == '|')) t++;
				}

                                /* Scan for the values */
                                if (1 != sscanf(s, "%d",
                                        &class)) return (PARSE_ERROR_GENERIC);


				/* Hack - Parse this entry */
                                pr_ptr->choice |= (1 << class);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++p_head->name_size;
	++p_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Initialize the "c_info" array, by parsing an ascii "template" file
 */
errr init_c_info_txt(FILE *fp, char *buf)
{
	int i, j, title=0;

	char *s;

	/* Not ready yet */     
	bool okay = FALSE;

	/* Current entry */
	player_class *pc_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


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
			    (v1 != c_head->v_major) ||
			    (v2 != c_head->v_minor) ||
			    (v3 != c_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= c_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Reset title */
			title = 0;

			/* Point at the "info" */
			pc_ptr = &c_info[i];

			/* Hack -- Verify space */
			if (c_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!pc_ptr->name) pc_ptr->name = ++c_head->name_size;

			/* Append chars to the name */
			strcpy(c_name + c_head->name_size, s);

			/* Advance the index */
                        c_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'S' for "Stats" (one line only) */
		if (buf[0] == 'S')
		{
			int adj;

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

			/* Next... */
			continue;
		}

		/* Process 'C' for "Class Skills" (one line only) */
		if (buf[0] == 'C')
		{
			int dis, dev, sav, stl, srh, fos, thn, thb;

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

			/* Next... */
			continue;
		}

		/* Process 'I' for "Improvement in Skills" (one line only) */
		if (buf[0] == 'I')
		{
			int dis, dev, sav, stl, srh, fos, thn, thb;

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

			/* Next... */
			continue;
		}


		/* Process 'X' for "Extra Info" (one line only) */
		if (buf[0] == 'X')
		{
			int mhp, exp;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
					&mhp, &exp)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			pc_ptr->c_mhp = mhp;
			pc_ptr->c_exp = exp;

			/* Next... */
			continue;
		}


		/* Process 'M' for "Magic Info" (one line only) */
		if (buf[0] == 'M')
		{
                        int stat,pow,lvl,wgt,book;

			/* Scan for the values */
                        if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
                                        &stat,&pow,&lvl,&wgt,&book)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			pc_ptr->sp_stat = stat;
			pc_ptr->sp_pow = pow;
			pc_ptr->sp_lvl = lvl;
                        pc_ptr->spell_weight = wgt;
                        pc_ptr->spell_book = book;

			/* Next... */
			continue;
		}


		/* Process 'B' for "Blows" (one line only) */
		if (buf[0] == 'B')
		{
			int num, mul, wgt;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&num, &mul, &wgt)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        pc_ptr->max_attacks = num;
                        pc_ptr->att_multiply = mul;
                        pc_ptr->min_weight = wgt;

			/* Next... */
			continue;
		}

		/* Process 'P' for "Perception" (one line only) */
		if (buf[0] == 'P')
		{
                        int turns,plus,squared,heavy;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&turns, &plus, &squared, &heavy)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			pc_ptr->id_turns = turns;
			pc_ptr->id_plus = plus;
			pc_ptr->id_squared = squared;
			pc_ptr->id_heavy = heavy;

			/* Next... */
			continue;
		}

		/* Process 'O' for "Outfitted with" (one line only) */
		if (buf[0] == 'O')
		{
			int tval[3], sval[3];

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
					&tval[0], &sval[0], &tval[1],&sval[1],
					&tval[2],&sval[2])) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			pc_ptr->outfit[0] = tval[0];
			pc_ptr->outfit[1] = sval[0];
			pc_ptr->outfit[2] = tval[1];
			pc_ptr->outfit[3] = sval[1];
			pc_ptr->outfit[4] = tval[2];
			pc_ptr->outfit[5] = sval[2];

			/* Next... */
			continue;
		}


		/* Process 'T' for "Title" */
		if (buf[0] == 'T')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (c_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Oops, no more slots */
                        if (title == PY_MAX_LEVEL/5) return (PARSE_ERROR_GENERIC);

			/* Advance and Save the text index */
			if (!pc_ptr->title[title]) pc_ptr->title[title++] = ++c_head->text_size;

                        /* Append chars to the text */
			strcpy(c_text + c_head->text_size, s);

			/* Advance the index */
			c_head->text_size += strlen(s);

			/* Next... */
			continue;
		}



		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


        /* Complete the "nam" and "text" sizes */
	++c_head->name_size;
	++c_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}

/*
 * Grab one style in a weapon style from a textual string
 */
static errr grab_one_style(weapon_style *ws_ptr, cptr what)
{
	int i;

	/* Check styles */
	for (i = 0; i < MAX_WEAP_STYLES; i++)
	{
		if (streq(what, w_info_style[i]))
		{
			ws_ptr->styles |= (1L << i);
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
static errr grab_one_benefit(weapon_style *ws_ptr, cptr what)
{
	int i;

	/* Check styles */
	for (i = 0; i < MAX_WEAP_BENEFITS; i++)
	{
		if (streq(what, w_info_benefit[i]))
		{
			ws_ptr->benefit =i;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown weapon style '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "w_info" array, by parsing an ascii "template" file
 */
errr init_w_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	weapon_style *ws_ptr = NULL;

	/* Just before the first table entry */
	i =-1;

	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


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
			    (v1 != p_head->v_major) ||
			    (v2 != p_head->v_minor) ||
			    (v3 != p_head->v_patch))
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


		/* Process 'W' for "Class/Level" */
		if (buf[0] == 'W')
		{
			int class,level;

			if (i>=z_info->w_max) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Advance the record */
			i++;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
					&class,&level)) return (PARSE_ERROR_GENERIC);

			/* Point at the "info" */
			ws_ptr = &w_info[i];

			ws_ptr->class = class;
			ws_ptr->level = level;
			ws_ptr->styles = 0;

			/* Next... */
			continue;
		}

		/* There better be a current ws_ptr */
		if (!ws_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Hack -- Process 'S' for styles */
		if (buf[0] == 'S')
		{
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
				if (0 != grab_one_style(ws_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Hack -- Process 'B' for benefits */
		if (buf[0] == 'B')
		{
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
				if (0 != grab_one_benefit(ws_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}



/*
 * Grab one flag in an spell_type from a textual string
 */
static errr grab_one_cast_flag(spell_type *s_ptr, cptr what)
{
	int i;

        /* Check flags1 */
        for (i = 0; i < 32; i++)
	{
                if (streq(what, s_info_flags1[i]))
		{
                        s_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags */
        for (i = 0; i < 32; i++)
	{
                if (streq(what, s_info_flags2[i]))
		{
                        s_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags */
        for (i = 0; i < 32; i++)
	{
                if (streq(what, s_info_flags3[i]))
		{
                        s_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown spell flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}





/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr init_s_info_txt(FILE *fp, char *buf)
{
	int i;

        char *s,*t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	spell_type *s_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
	s_head->name_size = 0;
	s_head->text_size = 0;

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
			    (v1 != s_head->v_major) ||
			    (v2 != s_head->v_minor) ||
			    (v3 != s_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= s_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			s_ptr = &s_info[i];

			/* Hack -- Verify space */
			if (s_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!s_ptr->name) s_ptr->name = ++s_head->name_size;

			/* Append chars to the name */
			strcpy(s_name + s_head->name_size, s);

			/* Advance the index */
			s_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current s_ptr */
		if (!s_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (s_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!s_ptr->text) s_ptr->text = ++s_head->text_size;

			/* Append chars to the name */
			strcpy(s_text + s_head->text_size, s);

			/* Advance the index */
			s_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'A' for "Appears in" (up to five lines) */
		if (buf[0] == 'A')
		{
			int tval,sval,slot;

			/* Find the next empty appears slot (if any) */
			for (i = 0; i < MAX_SPELL_APPEARS; i++) if (!s_ptr->appears[i].tval) break;

			/* Check bounds */
			if (i==MAX_SPELL_APPEARS) return (PARSE_ERROR_GENERIC);

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&tval,&sval,&slot)) return (PARSE_ERROR_GENERIC);

			/* Extract the damage dice and sides */
			s_ptr->appears[i].tval = tval;
			s_ptr->appears[i].sval = sval;
			s_ptr->appears[i].slot = slot;

			/* Next... */
			continue;
		}

		/* Process 'C' for "Cast by" (up to five lines) */
		if (buf[0] == 'C')
		{
			int class,level,mana,fail,min;

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

			/* Next... */
			continue;
		}


		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{
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

			/* Next... */
			continue;
		}

		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
		{
			int n1, n2;

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

			/* Next... */
			continue;
		}

                /* Process 'S' for "Spell" */
                if (buf[0] == 'S')
		{
                        int n1;

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

			/* Next... */
			continue;

                }

                /* Process 'L' for "Lasts" */
                if (buf[0] == 'L')
		{
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

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++s_head->name_size;
	++s_head->text_size;

	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);

	/* Success */
	return (0);
}

/*
 * Initialize the "y_info" array, by parsing an ascii "template" file
 */
errr init_y_info_txt(FILE *fp, char *buf)
{
	int i;

        char *s,*t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        rune_type *y_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        y_head->name_size = 0;
        y_head->text_size = 0;

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
                            (v1 != y_head->v_major) ||
                            (v2 != y_head->v_minor) ||
                            (v3 != y_head->v_patch))
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
			if (i < error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
                        if (i >= y_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        y_ptr = &y_info[i];

			/* Hack -- Verify space */
                        if (y_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
                        if (!y_ptr->name) y_ptr->name = ++y_head->name_size;

			/* Append chars to the name */
                        strcpy(y_name + y_head->name_size, s);

			/* Advance the index */
                        y_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* There better be a current y_ptr */
                if (!y_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (y_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
                        if (!y_ptr->text) y_ptr->text = ++y_head->text_size;

			/* Append chars to the name */
                        strcpy(y_text + y_head->text_size, s);

			/* Advance the index */
                        y_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

                /* Process 'Y' for "Rune flag" up to four lines */
                if (buf[0] == 'Y')
		{
			int n1;

			/* Find the next empty blow slot (if any) */
                        for (i = 0; i < 4; i++) if (!y_ptr->count[i]) break;

			/* Analyze the first field */
			for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

			/* Terminate the field (if necessary) */
			if (*t == ':') *t++ = '\0';

                        /* Get the count */
                        y_ptr->count[i] = atoi(s);

                        /* Analyze the rune flag */
                        for (n1 = 0; k_info_flags1[n1]; n1++)
			{
                                if (streq(t, k_info_flags1[n1])) break;
			}

			if (n1<32)
			{
                                y_ptr->flag[i] = n1;

				continue;
			}

                        /* Analyze the rune flag */
                        for (n1 = 0; k_info_flags2[n1]; n1++)
			{
                                if (streq(t, k_info_flags2[n1])) break;
			}

			if (n1<32)
			{
                                y_ptr->flag[i] = n1+32;

				continue;
			}

                        /* Analyze the rune flag */
                        for (n1 = 0; k_info_flags3[n1]; n1++)
			{
                                if (streq(t, k_info_flags3[n1])) break;
			}

			if (n1<32)
			{
                                y_ptr->flag[i] = n1+64;

				continue;
			}

			/* Oops */
                        msg_format("Unknown rune flag '%s'.", t);

			/* Fail */
                        return(PARSE_ERROR_GENERIC);
		}


		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
		{
			int n1, n2;

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

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
        ++y_head->name_size;
        ++y_head->text_size;

	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);

	/* Success */
	return (0);
}


/*
 * Initialize the "t_info" array, by parsing an ascii "template" file
 */
errr init_t_info_txt(FILE *fp, char *buf)
{
        int i;

        char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	town_type *t_ptr = NULL;

	/* Zone number */
	int zone = 0;

	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	t_head->name_size = 0;
	t_head->text_size = 0;

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
			    (v1 != t_head->v_major) ||
			    (v2 != t_head->v_minor) ||
			    (v3 != t_head->v_patch))
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
			if (i >= t_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			t_ptr = &t_info[i];

			/* Hack -- Verify space */
			if (t_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!t_ptr->name) t_ptr->name = ++t_head->name_size;

			/* Append chars to the name */
                        strcpy(t_name + t_head->name_size, s);

			/* Advance the index */
			t_head->name_size += strlen(s);

                        /* Reset the counters */
                        zone = 0;

			/* Next... */
			continue;
		}

		/* There better be a current t_ptr */
		if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

                        /* Hack -- Verify space */
			if (t_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!t_ptr->text) t_ptr->text = ++t_head->text_size;

			/* Append chars to the name */
                        strcpy(t_text + t_head->text_size, s);

			/* Advance the index */
			t_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#if 0
		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

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
			t_ptr->d_attr = tmp;
			t_ptr->d_char = sym;

			/* Next... */
			continue;
		}
#endif

		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			/* Paranoia */
			if (!buf[2]) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			t_ptr->r_char = buf[2];

			/* Next... */
			continue;
		}

		/* Process 'R' for "Race flag" (once only) */
		if (buf[0] == 'R')
		{
			int n1;

			/* Set to the first field */
			s=buf+2;

			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags1[n1]; n1++)
			{
                                if (streq(s, r_info_flags1[n1])) break;
			}

			if (n1<32)
			{
                                t_ptr->r_flag = n1+1;

				continue;
			}

			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags2[n1]; n1++)
			{
                                if (streq(s, r_info_flags2[n1])) break;
			}

			if (n1<32)
			{
                                t_ptr->r_flag = n1+33;

				continue;
			}

			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags3[n1]; n1++)
			{
                                if (streq(s, r_info_flags3[n1])) break;
			}


			if (n1<32)
			{
                                t_ptr->r_flag = n1 + 65;

				continue;
			}


			/* Analyze the race flag */
                        for (n1 = 0; r_info_flags4[n1]; n1++)
			{
                                if (streq(s, r_info_flags4[n1])) break;
			}


			if (n1<32)
                        {                         
                                t_ptr->r_flag = n1 + 97;

				continue;
			}

			/* Oops */
			msg_format("Unknown room race flag '%s'.", s);

			/* Fail */
                        return(PARSE_ERROR_GENERIC);
		}



		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
                        int near,distant;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                                        &near, &distant)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        t_ptr->near=near;
                        t_ptr->distant = distant;

			/* Next... */
			continue;
		}


		/* Process 'L' for "Levels" (up to four lines) */
		if (buf[0] == 'L')
		{
			int level,fill,big,small,guard;

			/* Oops, no more slots */
			if (zone == 4) return (PARSE_ERROR_GENERIC);

			/* Scan for the values */
			if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
					&level, &fill, &big, &small, &guard)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        t_ptr->zone[zone].level=level;
                        t_ptr->zone[zone].fill = fill;
                        t_ptr->zone[zone].big = big;
                        t_ptr->zone[zone].small = small;
                        t_ptr->zone[zone].guard = guard;

			/* Find the next empty zone slot (if any) */
			zone++;

			/* Next... */
			continue;
		}


		/* Process 'S' for "Stores" */
		if (buf[0] == 'S')
		{

                        int store1,store2,store3,store4,store5,store6,store7,store8;

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

			/* Find the next empty zone slot (if any) */
			zone++;

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++t_head->name_size;
	++t_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}



/*
 * Initialize the "u_info" array, by parsing an ascii "template" file
 */
errr init_u_info_txt(FILE *fp, char *buf)
{
	int i;

        int cur_t = 0;

        char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        store_type *u_ptr = NULL;

	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;

	/* Prepare the "fake" stuff */
        u_head->name_size = 0;
        u_head->text_size = 0;

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
                            (v1 != u_head->v_major) ||
                            (v2 != u_head->v_minor) ||
                            (v3 != u_head->v_patch))
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
                        if (i >= u_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        u_ptr = &u_info[i];

			/* Hack -- Verify space */
                        if (u_head->name_size + strlen(s) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
                        if (!u_ptr->name) u_ptr->name = ++u_head->name_size;

			/* Append chars to the name */
                        strcpy(u_name + u_head->name_size, s);

			/* Advance the index */
                        u_head->name_size += strlen(s);

			/* Reset the count */
			cur_t = 0;

			/* Next... */
			continue;
		}

                /* There better be a current u_ptr */
                if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (u_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
                        if (!u_ptr->text) u_ptr->text = ++u_head->text_size;

			/* Append chars to the name */
                        strcpy(u_text + u_head->text_size, s);

			/* Advance the index */
                        u_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#endif


		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

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
                        u_ptr->d_attr = tmp;
                        u_ptr->d_char = sym;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
                        int level, stval;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                        &level, &stval)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        u_ptr->level=level;
                        u_ptr->stval=stval;

			/* Next... */
			continue;
		}

                /* Process 'O' for "Offered" (up to thirty two lines) */
                if (buf[0] == 'O')
		{
                        int tval, sval, count;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
                                        &tval, &sval, &count)) return (PARSE_ERROR_GENERIC);

			/* only thirty two O: lines allowed */
                        if (cur_t >= STORE_CHOICES) return (PARSE_ERROR_GENERIC);

			/* Save the values */
                        u_ptr->tval[cur_t] = (byte)tval;
                        u_ptr->sval[cur_t] = (byte)sval;
                        u_ptr->count[cur_t] = (byte)count;

			/* increase counter for 'possible tval' index */
			cur_t++;

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
        ++u_head->name_size;
        ++u_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


/*
 * Initialize the "h_info" array, by parsing an ascii "template" file
 */
errr init_h_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	hist_type *h_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	h_head->text_size = 0;

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
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != h_head->v_major) ||
			    (v2 != h_head->v_minor) ||
			    (v3 != h_head->v_patch))
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


		/* Process 'N' for "New/Number" */
		if (buf[0] == 'N')
		{
			int prv, nxt, prc, soc;

			/* Hack - get the index */
			i = error_idx + 1;

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= h_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			h_ptr = &h_info[i];

			/* Scan for the values */
			if (4 != sscanf(buf, "N:%d:%d:%d:%d",
					&prv, &nxt, &prc, &soc)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			h_ptr->chart = prv;
			h_ptr->next = nxt;
			h_ptr->roll = prc;
			h_ptr->bonus = soc;

			/* Next... */
			continue;
		}

		/* There better be a current h_ptr */
		if (!h_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (h_head->text_size + strlen(s) + 8 > z_info->fake_text_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!h_ptr->text) h_ptr->text = ++h_head->text_size;

			/* Append chars to the name */
			strcpy(h_text + h_head->text_size, s);

			/* Advance the index */
			h_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "text" size */
	++h_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}




/*
 * Initialize the "b_info" array, by parsing an ascii "template" file
 */
errr init_b_info_txt(FILE *fp, char *buf)
{
	int i, j;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	owner_type *ot_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	b_head->name_size = 0;

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
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != b_head->v_major) ||
			    (v2 != b_head->v_minor) ||
			    (v3 != b_head->v_patch))
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
			if (j >= z_info->b_max) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Get the *real* index */
			i = (i * z_info->b_max) + j;

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= b_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			ot_ptr = &b_info[i];

			/* Hack -- Verify space */
			if (b_head->name_size + strlen(t) + 8 > z_info->fake_name_size)
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!ot_ptr->owner_name) ot_ptr->owner_name = ++b_head->name_size;

			/* Append chars to the name */
			strcpy(b_name + b_head->name_size, t);

			/* Advance the index */
			b_head->name_size += strlen(t);

			/* Next... */
			continue;
		}

		/* There better be a current ot_ptr */
		if (!ot_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int idx, gld, max, min, hgl, tol;

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

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++b_head->name_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}





/*
 * Initialize the "g_info" array, by parsing an ascii "template" file
 */
errr init_g_info_txt(FILE *fp, char *buf)
{
	int i, j;

	char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	byte *g_ptr;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	g_head->text_size = 0;

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
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != g_head->v_major) ||
			    (v2 != g_head->v_minor) ||
			    (v3 != g_head->v_patch))
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
				if (i >= g_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

				/* Save the index */
				error_idx = i;

				/* Point at the "info" */
				g_ptr = &g_info[i];

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

				/* Next... */
				continue;
			}

			/* Next... */
			continue;
		}


		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "text" size */
	++g_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}


#else   /* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif  /* ALLOW_TEMPLATES */
