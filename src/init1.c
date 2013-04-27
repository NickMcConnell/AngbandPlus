/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
 * Feature flag types
 */
static cptr f_info_flags[] =
{
	"BLOCK",
	"HALF_LOS",
	"USE_TRANS",
	"ICKY",
	"PERM",
	"OBJECT",
	"PATTERN",
	"MARK"
};


/*
 * Object script triggers
 */
static cptr k_info_triggers[] =
{
	"USE",
	"MAKE",
	"BONUS",
	"SMASH",
	"DESC",
	"TIMED",
	"HIT",
	"ATTACK",
	"ALTER",
	"SPOIL",
	NULL
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
	"CHARGE",
	"CRAWL",
	"DROOL",
	"SPIT",
	"EXPLODE",
	"GAZE",
	"WAIL",
	"SPORE",
	"XXX4",
	"BEG",
	"INSULT",
	"MOAN",
	"SHOW",
	NULL
};

/*
 * Monster Ranged Methods
 */
/*
 * Unused
static cptr r_info_ranged_method[] =
{
	"",
	"DART",
	"ARROW",
	"BOLT",
	"SHOT",
	"DAGGER",
	"AXE",
	"BOULDER",
	NULL
};
*/

/*
 * Monster Blow Effects
 */
static cptr r_info_blow_effect[] =
{
	"",
	"HURT",
	"POISON",
	"UN_BONUS",
	"UN_POWER",
	"EAT_GOLD",
	"EAT_ITEM",
	"EAT_FOOD",
	"EAT_LITE",
	"ACID",
	"ELEC",
	"FIRE",
	"COLD",
	"BLIND",
	"CONFUSE",
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
	"DISEASE",
	"TIME",
	"EXP_VAMP",
	NULL
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
	"CHAR_MIMIC",
	"ATTR_CLEAR",
	"ATTR_MULTI",
	"FORCE_DEPTH",
	"FORCE_MAXHP",
	"FORCE_SLEEP",
	"FORCE_EXTRA",
	"XXX1_1",
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
	"CAN_SPEAK",
	"REFLECTING",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"SHAPECHANGER",
	"ATTR_ANY",
	"POWERFUL",
	"XXX2X1",
	"AURA_FIRE",
	"AURA_ELEC",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"MOVE_BODY",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"BRAIN_1",
	"BRAIN_2",
	"BRAIN_3",
	"BRAIN_4",
	"BRAIN_5",
	"BRAIN_6",
	"BRAIN_7",
	"QUANTUM"
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
	"AMBERITE",
	"GOOD",
	"AURA_COLD",
	"NONLIVING",
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	"RES_TELE",
	"RES_NETH",
	"RES_WATE",
	"RES_PLAS",
	"RES_NEXU",
	"RES_DISE",
	"UNIQUE_7",
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
	"ELDRITCH_HORROR",
	"XXX3X4",
	"ROCKET",
	"ARROW",
	"XXX6X4",
	"XXX7X4",
	"XXX8X4",
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
	"BA_NUKE",
	"BR_NUKE",
	"BA_CHAO",
	"BR_DISI",
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
	"HAND_DOOM",
	"HEAL",
	"INVULNER",
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
	"ANIM_DEAD",	/* ToDo: Implement ANIM_DEAD */
	"S_KIN",
	"S_CYBER",
	"S_MONSTER",
	"S_MONSTERS",
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_HYDRA",
	"S_ANGEL",
	"S_DEMON",
	"S_UNDEAD",
	"S_DRAGON",
	"S_HI_UNDEAD",
	"S_HI_DRAGON",
	"S_AMBERITES",
	"S_UNIQUE"
};


/*
 * Monster race flags
 */
static cptr r_info_flags7[] =
{
	"AQUATIC",
	"CAN_SWIM",
	"CAN_FLY",
	"FRIENDLY",
	"SILLY",
	"LITE_1",
	"LITE_2",
	"ALWAYS_KNOWN",
	"CTH",
	"AMBER",
	"XXX7X10",
	"XXX7X11",
	"XXX7X12",
	"XXX7X13",
	"XXX7X14",
	"XXX7X15",
	"XXX7X16",
	"XXX7X17",
	"XXX7X18",
	"XXX7X19",
	"XXX7X20",
	"XXX7X21",
	"XXX7X22",
	"XXX7X23",
	"XXX7X24",
	"XXX7X25",
	"XXX7X26",
	"XXX7X27",
	"XXX7X28",
	"XXX7X29",
	"XXX7X30",
	"XXX7X31",
};

/*
 * Monster race flags
 */
static cptr r_info_flags8[] =
{
	"WILD_FOREST1",
	"WILD_FOREST2",
	"WILD_MOUNT1",
	"WILD_MOUNT2",
	"WILD_WASTE1",
	"WILD_WASTE2",
	"WILD_SWAMP1",
	"WILD_SWAMP2",
	"WILD_SHORE",
	"WILD_OCEAN",
	"WILD_GRASS",
	"WILD_TOWN",
	"DUN_DARKWATER",
	"DUN_LAIR",
	"DUN_TEMPLE",
	"DUN_TOWER",
	"DUN_RUIN",
	"DUN_GRAVE",
	"DUN_CAVERN",
	"DUN_PLANAR",
	"DUN_HELL",
	"DUN_HORROR",
	"DUN_MINE",
	"DUN_CITY",
	"DUN_XTRA1",
	"DUN_XTRA2",
	"DUN_XTRA3",
	"DUN_XTRA4",
	"DUN_XTRA5",
	"DUN_XTRA6",
	"DUN_XTRA7",
	"DUN_XTRA8"
};


/*
 * Monster race flags - Drops
 */
static cptr r_info_flags9[] =
{
	"DROP_CORPSE",
	"DROP_SKELETON",
	"XXX9X2",
	"XXX9X3",
	"XXX9X4",
	"XXX9X5",
	"XXX9X6",
	"XXX9X7",
	"XXX9X8",
	"XXX9X9",
	"XXX9X10",
	"XXX9X11",
	"XXX9X12",
	"XXX9X13",
	"XXX9X14",
	"XXX9X15",
	"XXX9X16",
	"XXX9X17",
	"XXX9X18",
	"XXX9X19",
	"XXX9X20",
	"XXX9X21",
	"XXX9X22",
	"XXX9X23",
	"XXX9X24",
	"XXX9X25",
	"XXX9X26",
	"XXX9X27",
	"XXX9X28",
	"XXX9X29",
	"XXX9X30",
	"XXX9X31",
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
	"SP",
	"STEALTH",
	"SENSE",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"BLOWS",
	"CHAOTIC",
	"VAMPIRIC",
	"SLAY_ANIMAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"KILL_DRAGON",
	"VORPAL",
	"IMPACT",
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
	"XXX1",
	"IM_POIS",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"THROW",
	"REFLECT",
	"FREE_ACT",
	"HOLD_LIFE",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_FEAR",
	"RES_LITE",
	"RES_DARK",
	"RES_BLIND",
	"RES_CONF",
	"RES_SOUND",
	"RES_SHARDS",
	"RES_NETHER",
	"RES_NEXUS",
	"RES_CHAOS",
	"RES_DISEN"
};

/*
 * Object flags
 */
static cptr k_info_flags3[] =
{
	"SH_FIRE",
	"SH_ELEC",
	"QUESTITEM",
	"XXX4",
	"NO_TELE",
	"NO_MAGIC",
	"HIDDEN_POWERS",
	"TY_CURSE",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"INSTA_ART",
	"FEATHER",
	"LITE",
	"SEE_INVIS",
	"TELEPATHY",
	"SLOW_DIGEST",
	"REGEN",
	"XTRA_MIGHT",
	"XTRA_SHOTS",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD",
	"ACTIVATE",
	"DRAIN_EXP",
	"TELEPORT",
	"AGGRAVATE",
	"BLESSED",
	"CURSED",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};

static cptr k_info_flags4[] =
{
	"LUCK_10",
	"WILD_SHOT",
	"WILD_WALK",
	"EASY_ENCHANT",
	"XXX5",
	"SH_FEAR",
	"XXX7",
	"XXX8",
	"IM_LITE",
	"IM_DARK",
	"SH_ACID",
	"SH_COLD",
	"MUTATE",
	"PATRON",
	"STRANGE_LUCK",
	"PASS_WALL",
	"GHOUL_TOUCH",
	"PSI_CRIT",
	"RETURN",
	"EXPLODE",
	"HURT_ACID",
	"HURT_ELEC",
	"HURT_FIRE",
	"HURT_COLD",
	"HURT_LITE",
	"HURT_DARK",
	"XXX27",
	"XXX28",
	"AUTO_CURSE",
	"DRAIN_STATS",
	"CANT_EAT",
	"SLOW_HEAL"
};

/*
 * Wilderness Flags
 */
static cptr w_info_flags[] =
{
	"FOREST1",
	"FOREST2",
	"MOUNT1",
	"MOUNT2",
	"WASTE1",
	"WASTE2",
	"SWAMP1",
	"SWAMP2"
};

/*
 * Monster group guild flags
 */
static cptr mg_info_guild_flags[] =
{
	"CASTLE0",
	"CASTLE1",
	"CASTLE2",
	"WARRIOR_GUILD",
	"THIEVES_GUILD",
	"RANGER_GUILD",
	"MAGE_GUILD",
	"CATHEDRAL",
	"INN",
	"ALL",
	"ALL_CASTLE",
	"ALL_GUILD"
};

/*
 * Field info-flags
 */

static cptr t_info_flags[] =
{
	"TEMP",
	"FEAT",
	"VIS",
	"MARK",
	"TRANS",
	"NO_LOOK",
	"NFT_LOOK",
	"MERGE",
	"NO_ENTER",
	"NO_MAGIC",
	"NO_OBJECT",
	"PERM",
	"IGNORE",
	"NO_MPLACE",
	"XXX13",
	"XXX14"
};

/*
 * Object script triggers
 */
static cptr t_info_triggers[] =
{
	"INIT",
	"LOAD",
	"PENTER",
	"PON",
	"MENTER",
	"MON",
	"OBDROP",
	"OBON",
	"INTER",
	"TARGET",
	"LOOK",
	"EXIT",
	"AI",
	"SPEC",
	"INTERT",
	"MENTT",
	"BUILD1",
	"BUILD2",
	"STORE1",
	"STORE2",
	"SBINIT",
	NULL
};

/*
 * Monster group dungeon flags
 */
static cptr mg_info_dungeon_types[] =
{
	"SEWER",
	"LAIR",
	"TEMPLE",
	"TOWER",
	"RUIN",
	"GRAVE",
	"CAVERN",
	"PLANAR",
	"HELL",
	"HORROR",
	"MINE",
	"CITY",
	"VANILLA",
	"DESERT",
	"SWAMP",
	"SANDY_BURROW",
	"FOREST",
	"MOUNTAIN",
	"TUNDRA",
	"PURE_CASTLE",
	"ELEMENT_FIRE",
	"ELEMENT_WATER",
	"ICE_CAVE",
	"ELEMENT_ACID",
	"SMOKE_CAVE",
	"ELEMENT_COLD",
	"ELEMENT_EARTH",
	"HEAVEN",
	"GRASSLAND",
	"ELEMENT_AIR",
	"HOUSE",
	"FLOOD_CAVE"
};

static cptr mg_info_dungeon_flags[] =
{
	"ROAD",
	"TRACK",
	"OUTSIDE",
	"FORCE_LIT",
	"SYM_2",
	"SYM_4",
	"SYM_2R",
	"SYM_4R",
	"BIG",
	"MEDIUM",
	"SMALL",
	"CENTER",
	"CASTLE",
	"SPARSE",
	"DENSE",
	"POOLS",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6",
	"XXX7",
	"XXX8",
	"XXX9",
	"XXX10",
	"XXX11",
	"XXX12",
	"XXX13",
	"XXX14",
	"XXX15",
	"XXX16",
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
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != head->v_major) ||
				(v2 != head->v_minor) || (v3 != head->v_patch))
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
		if ((err = (*parse_info_txt_line) (buf, head)) != 0)
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
	/* Unused parameter */
	(void)head;

	/* Hack - Verify 'M:x:' format */
	if (buf[0] != 'M') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (!buf[2]) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (buf[3] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);


	/* Process 'F' for "Maximum f_info[] index" */
	if (buf[2] == 'F')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->f_max = max;
	}

	/* Process 'K' for "Maximum k_info[] index" */
	else if (buf[2] == 'K')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->k_max = max;
	}

	/* Process 'A' for "Maximum a_info[] index" */
	else if (buf[2] == 'A')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->a_max = max;
	}

	/* Process 'E' for "Maximum e_info[] index" */
	else if (buf[2] == 'E')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->e_max = max;
	}

	/* Process 'R' for "Maximum r_info[] index" */
	else if (buf[2] == 'R')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->r_max = max;
	}

	/* Process 'V' for "Maximum v_info[] index" */
	else if (buf[2] == 'V')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->v_max = max;
	}

	/* Process 'O' for "Maximum o_list[] index" */
	else if (buf[2] == 'O')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->o_max = max;
	}

	/* Process 'M' for "Maximum m_list[] index" */
	else if (buf[2] == 'M')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->m_max = max;
	}

	/* Process 'N' for "Fake name size" */
	else if (buf[2] == 'N')
	{
		long max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->fake_name_size = max;
	}

	/* Process 'T' for "Fake text size" */
	else if (buf[2] == 'T')
	{
		long max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%ld", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->fake_text_size = max;
	}

	/* Process 'Q' for "Maximum number of quests" */
	else if (buf[2] == 'Q')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->q_max = max;
	}

	/* Process 'U' for "Maximum number of field types" */
	else if (buf[2] == 'U')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->t_max = max;
	}

	/* Process 'D' for "Maximum field list" */
	else if (buf[2] == 'D')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->fld_max = max;
	}

	/* Process 'G' for "Maximum region list" */
	else if (buf[2] == 'G')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->rg_max = max;
	}

	/* Process 'H' for "Maximum number of heroes" */
	else if (buf[2] == 'H')
	{
		int max;

		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->h_max = max;
	}

	/* Process 'g' for "Maximum number of monster groups" */
	else if (buf[2] == 'g')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf + 4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->mg_max = max;
	}

	/* Process 'W' for "Maximum wilderness values" */
	else if (buf[2] == 'W')
	{
		/* Hack - Verify 'M:W:x:' format */
		if (buf[5] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);


		/* Process 'N' for "Maximum wilderness tree nodes" */
		if (buf[4] == 'N')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf + 6, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->wn_max = max;
		}

		/* Process 'T' for "Maximum wilderness gen types" */
		else if (buf[4] == 'T')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf + 6, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->wt_max = max;
		}

		/* Process 'P' for "Maximum towns" */
		else if (buf[4] == 'P')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf + 6, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->wp_max = max;
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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		v_ptr = &v_info[i];

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
		s = buf + 2;

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
		if (4 != sscanf(buf + 2, "%d:%d:%d:%d",
						&typ, &rat, &hgt, &wid)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		v_ptr->typ = typ;
		v_ptr->rat = rat;
		v_ptr->hgt = hgt;
		v_ptr->wid = wid;
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
static errr grab_one_feat_flag(byte *flags, cptr names[], cptr what)
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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		f_ptr = &f_info[i];

		/* Store the name */
		if (!(f_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
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
	else if (buf[0] == 'F')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_feat_flag(&f_ptr->flags, f_info_flags, s))
			{
				return (PARSE_ERROR_INVALID_FLAG);
			}

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
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
	if (grab_one_flag(&k_ptr->flags[0], k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags[1], k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags[2], k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags[3], k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	msgf("Unknown object flag '%s'.", what);

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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		k_ptr = &k_info[i];

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
		if (3 != sscanf(buf + 2, "%d:%d:%d",
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
		if (4 != sscanf(buf + 2, "%d:%d:%d:%ld",
						&level, &extra, &wgt,
						&cost)) return (PARSE_ERROR_GENERIC);

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
		for (i = 0, s = buf + 1; s && (s[0] == ':') && s[1]; ++i)
		{
			/* Sanity check */
			if (i > 3) return (PARSE_ERROR_TOO_MANY_ALLOCATIONS);

			/* Default chance */
			k_ptr->chance[i] = 1;

			/* Store the attack damage index */
			k_ptr->locale[i] = atoi(s + 1);

			/* Find the slash */
			t = strchr(s + 1, '/');

			/* Find the next colon */
			s = strchr(s + 1, ':');

			/* If the slash is "nearby", use it */
			if (t && (!s || t < s))
			{
				int chance = atoi(t + 1);
				if (chance >= 0) k_ptr->chance[i] = chance;
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
		if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d",
						&ac, &hd1, &hd2, &th, &td,
						&ta)) return (PARSE_ERROR_GENERIC);

		k_ptr->ac = ac;
		k_ptr->dd = hd1;
		k_ptr->ds = hd2;
		k_ptr->to_h = th;
		k_ptr->to_d = td;
		k_ptr->to_a = ta;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_kind_flag(k_ptr, s))
			{
				return (PARSE_ERROR_INVALID_FLAG);
			}

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
		s = buf + 2;

		/* Store the text */
		if (!add_text(&(k_ptr->text), head, s))
		{
			msgf("Icky Description!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
		}
	}

	/* Process 'L' for "Lua script" */
	else if (buf[0] == 'L')
	{
		int n;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the trigger */
		for (n = 0; k_info_triggers[n]; n++)
		{
			if (streq(s, k_info_triggers[n])) break;
		}

		/* Invalid trigger */
		if (!k_info_triggers[n]) return (PARSE_ERROR_GENERIC);

		/* Get the text */
		s = t;

		/* Store the text */
		if (!add_text(&(k_ptr->trigger[n]), head, s))
		{
			msgf("Icky Trigger 1!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
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
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
	if (grab_one_flag(&a_ptr->flags[0], k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags[1], k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags[2], k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags[3], k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	msgf("Unknown artifact flag '%s'.", what);

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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		a_ptr = &a_info[i];

		/* Store the name */
		if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
		SET_FLAG(a_ptr, TR_IGNORE_MASK);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf + 2, "%d:%d:%d",
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
		if (4 != sscanf(buf + 2, "%d:%d:%d:%ld",
						&level, &rarity, &wgt,
						&cost)) return (PARSE_ERROR_GENERIC);

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
		if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d",
						&ac, &hd1, &hd2, &th, &td,
						&ta)) return (PARSE_ERROR_GENERIC);

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
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 !=
				grab_one_artifact_flag(a_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current k_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf + 2;

		/* Store the text */
		if (!add_text(&(a_ptr->text), head, s))
		{
			msgf("Icky Description!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
		}
	}

	/* Process 'L' for "Lua script" */
	else if (buf[0] == 'L')
	{
		int n;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the trigger */
		for (n = 0; k_info_triggers[n]; n++)
		{
			if (streq(s, k_info_triggers[n])) break;
		}

		/* Invalid trigger */
		if (!k_info_triggers[n]) return (PARSE_ERROR_GENERIC);

		/* Get the text */
		s = t;

		/* Store the text */
		if (!add_text(&(a_ptr->trigger[n]), head, s))
		{
			msgf("Icky Trigger 2!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
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
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	if (grab_one_flag(&e_ptr->flags[0], k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags[1], k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags[2], k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags[3], k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	msgf("Unknown ego-item flag '%s'.", what);

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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		e_ptr = &e_info[i];

		/* Store the name */
		if (!(e_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Start with the first of the tval indices */
		cur_t = 0;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, pad2;
		long cost;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf + 2, "%d:%d:%d:%ld",
						&level, &rarity, &pad2,
						&cost)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->level = level;
		e_ptr->rarity = rarity;
		/* e_ptr->weight = wgt; */
		e_ptr->cost = cost;
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		int slot, rating;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf + 2, "%d:%d",
						&slot, &rating)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->slot = slot;
		e_ptr->rating = rating;
	}

	/* Hack -- Process 'C' for "creation" */
	else if (buf[0] == 'C')
	{
		int th, td, ta, pv;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf + 2, "%d:%d:%d:%d",
						&th, &td, &ta, &pv)) return (PARSE_ERROR_GENERIC);

		e_ptr->max_to_h = th;
		e_ptr->max_to_d = td;
		e_ptr->max_to_a = ta;
		e_ptr->max_pval = pv;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 !=
				grab_one_ego_item_flag(e_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf + 2;

		/* Store the text */
		if (!add_text(&(e_ptr->text), head, s))
		{
			msgf("Icky Description!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
		}
	}

	/* Process 'L' for "Lua script" */
	else if (buf[0] == 'L')
	{
		int n;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Analyze the first field */
		for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

		/* Terminate the field (if necessary) */
		if (*t == ':') *t++ = '\0';

		/* Analyze the trigger */
		for (n = 0; k_info_triggers[n]; n++)
		{
			if (streq(s, k_info_triggers[n])) break;
		}

		/* Invalid trigger */
		if (!k_info_triggers[n]) return (PARSE_ERROR_GENERIC);

		/* Get the text */
		s = t;

		/* Store the text */
		if (!add_text(&(e_ptr->trigger[n]), head, s))
		{
			msgf("Icky Trigger 3!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags[0], r_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[1], r_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[2], r_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[6], r_info_flags7, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[7], r_info_flags8, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[8], r_info_flags9, what) == 0)
		return (0);

	/* Oops */
	msgf("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one (spell) flag in a monster_race from a textual string
 */
static errr grab_one_spell_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags[3], r_info_flags4, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[4], r_info_flags5, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags[5], r_info_flags6, what) == 0)
		return (0);

	/* Oops */
	msgf("Unknown monster flag '%s'.", what);

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
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 2);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num - z_info->h_max) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		r_ptr = &r_info[i];

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
		s = buf + 2;

		/* Store the text */
		if (!add_text(&(r_ptr->text), head, s))
		{
			msgf("Icky Description!!");
			message_flush();
			return (PARSE_ERROR_OUT_OF_MEMORY);
		}
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
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp1, hp2, aaf, ac, slp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d",
						&spd, &hp1, &hp2, &aaf, &ac,
						&slp)) return (PARSE_ERROR_GENERIC);

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
		if (4 != sscanf(buf + 2, "%d:%d:%d:%ld",
						&lev, &rar, &pad, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
		r_ptr->extra = pad;
		r_ptr->mexp = exp;
	}

	/* Process 'O' for "Object theme" (one line only) */
	else if (buf[0] == 'O')
	{
		int treasure, combat, magic;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf + 2, "%d:%d:%d",
						&treasure, &combat,
						&magic)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->obj_drop.treasure = (byte)treasure;
		r_ptr->obj_drop.combat = (byte)combat;
		r_ptr->obj_drop.magic = (byte)magic;

		/*
		 * Since monsters do not drop junk,
		 * this value is defined in terms of the others
		 */
		r_ptr->obj_drop.tools = 100 - (treasure + combat + magic);
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
		for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

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
		for (s = t; *t && (*t != ':'); t++) /* loop */ ;

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
		for (s = t; *t && (*t != 'd'); t++) /* loop */ ;

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
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 !=
				grab_one_basic_flag(r_ptr,
									s)) return (PARSE_ERROR_INVALID_FLAG);

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
		for (s = buf + 2; *s;)
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

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
				r_ptr->freq_spell = r_ptr->freq_inate = 100 / i;

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

	/* Success */
	return (0);
}



/*
 * Grab one flag in wild_type from a textual string
 */
static errr grab_one_wild_flag(wild_gen_data_type *w_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 8; i++)
	{
		if (streq(what, w_info_flags[i]))
		{
			w_ptr->rough_type |= (1 << i);
			return (0);
		}
	}

	/* Oops */
	msgf("Unknown wilderness flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "wild_choice_tree" and "wild_gen_data" arrays,
 *  by parsing an ascii "template" file
 */
errr init_w_info_txt(FILE *fp, char *buf)
{
	char *s, *t;

	u16b i = 0;

	/* Bounding box of entry */
	wild_bound_box_type bound;

	/* Current entry */
	wild_gen_data_type *w_ptr = NULL;

	/* Just before the first line */
	error_line = -1;

	/* The last index used */
	error_idx = -1;

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);

		/* Process 'N' for "Number" (one line only) */
		if (buf[0] == 'N')
		{
			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Check to see if there is room in array */
			if (i > z_info->wt_max - 1) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Save the index */
			error_idx = i;

			/* point to new position in array */
			w_ptr = &wild_gen_data[i];

			continue;
		}

		/* There better be a current w_ptr */
		if (!w_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Process 'M' for "Map" (one line only) */
		if (buf[0] == 'M')
		{
			int tmp;

			if (1 != sscanf(buf + 2, "%d", &tmp)) return (PARSE_ERROR_GENERIC);

			/* Paranoia */
			if ((tmp >= z_info->f_max)
				|| (tmp < 0)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			w_ptr->feat = (byte)tmp;

			/* Next... */
			continue;
		}

		/* Process 'W' for "Wilderness Info" (one line only) */
		if (buf[0] == 'W')
		{
			int hgtmin, hgtmax, popmin, popmax, lawmin, lawmax;

			/* Scan for the values */
			if (6 != sscanf(buf + 2, "%d:%d:%d:%d:%d:%d",
							&hgtmin, &hgtmax, &popmin, &popmax,
							&lawmin, &lawmax)) return (PARSE_ERROR_GENERIC);

			/* Save the values into bounds */
			bound.hgtmin = hgtmin;
			bound.hgtmax = hgtmax;

			bound.popmin = popmin;
			bound.popmax = popmax;

			bound.lawmin = lawmin;
			bound.lawmax = lawmax;

			/* Next... */
			continue;
		}

		/* Process 'T' for "Type" (one line only) */
		if (buf[0] == 'T')
		{
			int routine, chance;

			/* Scan for the values */
			if (2 != sscanf(buf + 2, "%d:%d",
							&routine, &chance)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			w_ptr->gen_routine = routine;
			w_ptr->chance = chance;

			/* Next... */
			continue;
		}

		/* Process 'F' for "Basic Flags" (multiple lines) */
		if (buf[0] == 'F')
		{
			/* Parse every entry */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|') t++;
				}

				/* Parse this entry */
				if (0 !=
					grab_one_wild_flag(w_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Process 'E' for "Extra Information" (one line only) */
		if (buf[0] == 'E')
		{
			int d0, d1, d2, d3, d4, d5, d6, d7;

			/* Scan for the values */
			if (8 != sscanf(buf + 2, "%d:%d:%d:%d:%d:%d:%d:%d",
							&d0, &d1, &d2, &d3, &d4, &d5, &d6, &d7))
				return (PARSE_ERROR_GENERIC);

			/* Save the values */
			w_ptr->data[0] = d0;
			w_ptr->data[1] = d1;
			w_ptr->data[2] = d2;
			w_ptr->data[3] = d3;
			w_ptr->data[4] = d4;
			w_ptr->data[5] = d5;
			w_ptr->data[6] = d6;
			w_ptr->data[7] = d7;

			/* Initialise if tree is empty */
			if (i == 1)
			{
				/* Initialise */
				(void)init_choice_tree(&bound, i);
			}
			else
			{
				/* Add type to decision tree */
				if (add_node_tree_root(&bound, i) == 0)
					return (PARSE_ERROR_OUT_OF_MEMORY);
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


#endif /* ALLOW_TEMPLATES */

/*
 * Grab one info-flag from a textual string
 */
static errr grab_one_info_flag(field_thaum *t_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 16; i++)
	{
		if (streq(what, t_info_flags[i]))
		{
			t_ptr->info |= (1 << i);
			return (0);
		}
	}

	/* Oops */
	msgf("Unknown field info-flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the field "thaumatergical" arrays,
 *  by parsing an ascii "template" file
 */
errr init_t_info_txt(FILE *fp, char *buf)
{
	char *s, *t;

	u16b i = 0;

	/* Current entry */
	field_thaum *t_ptr = NULL;

	/* Just before the first line */
	error_line = -1;

	/* The last index used */
	error_idx = -1;



	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);

		/* Process 'N' for "Number" (one line only) */
		if (buf[0] == 'N')
		{
			/* Length of name string */
			u16b length;

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Check to see if there is room in array */
			if (i > z_info->t_max - 1) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Save the index */
			error_idx = i;

			/* point to new position in array */
			t_ptr = &t_info[i];


			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (PARSE_ERROR_GENERIC);

			/* Name + /0 on the end */
			length = strlen(s) + 1;

			/* Make some room */
			C_MAKE(t, length, char);

			if (!t) return (PARSE_ERROR_OUT_OF_MEMORY);	/* Out of memory */

			/* Add the name */
			t_ptr->name = strcpy(t, s);
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

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0) return (PARSE_ERROR_GENERIC);

			/* Save the default values */
			t_ptr->d_attr = tmp;
			t_ptr->d_char = buf[2];

			/* Next... */
			continue;
		}

		/* Process 'W' for extra information (one line only) */
		if (buf[0] == 'W')
		{
			int priority, type, counter;

			/* Scan for the values */
			if (3 != sscanf(buf + 2, "%d:%d:%d",
							&priority, &type,
							&counter)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			t_ptr->priority = (byte)priority;
			t_ptr->type = (byte)type;
			t_ptr->count_init = (s16b)counter;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info Flags" (multiple lines) */
		if (buf[0] == 'I')
		{
			/* Parse every entry */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|') t++;
				}

				/* Parse this entry */
				if (0 !=
					grab_one_info_flag(t_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Process 'D' for "Data" (one line only) */
		if (buf[0] == 'D')
		{
			int d0, d1, d2, d3, d4, d5, d6, d7;

			/* Scan for the values */
			if (8 != sscanf(buf + 2, "%d:%d:%d:%d:%d:%d:%d:%d",
							&d0, &d1, &d2, &d3, &d4, &d5, &d6, &d7))
				return (PARSE_ERROR_GENERIC);

			/* Save the values */
			t_ptr->data_init[0] = d0;
			t_ptr->data_init[1] = d1;
			t_ptr->data_init[2] = d2;
			t_ptr->data_init[3] = d3;
			t_ptr->data_init[4] = d4;
			t_ptr->data_init[5] = d5;
			t_ptr->data_init[6] = d6;
			t_ptr->data_init[7] = d7;

			/* Next... */
			continue;
		}

		/* Process 'L' for "Lua script" */
		if (buf[0] == 'L')
		{
			int n;

			/* There better be a current t_ptr */
			if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

			/* Analyze the first field */
			for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

			/* Terminate the field (if necessary) */
			if (*t == ':') *t++ = '\0';

			/* Analyze the trigger */
			for (n = 0; t_info_triggers[n]; n++)
			{
				if (streq(s, t_info_triggers[n])) break;
			}

			/* Invalid trigger */
			if (!t_info_triggers[n]) return (PARSE_ERROR_GENERIC);

			/* Get the text */
			s = t;

			/* Store the text */
			if (t_ptr->action[n])
			{
				s16b old = t_ptr->action[n];

				t_ptr->action[n] = quark_fmt("%s\n%s", quark_str(old), s);

				quark_remove(&old);
			}
			else
			{
				t_ptr->action[n] = quark_add(s);
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);

	}

	/* Success */
	return (0);
}

/*
 * Grab one guild flag in monster_group_type from a textual string
 */
static errr grab_one_guild_flag(monster_group_type *mg_ptr, cptr what)
{
	int i;

	bool found = FALSE;

	/* Check flags */
	for (i = 0; i < 9; i++)
	{
		if (streq(what, mg_info_guild_flags[i]))
		{
			mg_ptr->flags |= (1 << i);
			found = TRUE;
		}
	}

	/* 9-11 are special cases */
	if (streq(what, mg_info_guild_flags[9]))
	{
		mg_ptr->flags |= MGF_ALL;
		found = TRUE;
	}

	if (streq(what, mg_info_guild_flags[10]))
	{
		mg_ptr->flags |= MGF_CASTLE;
		found = TRUE;
	}

	if (streq(what, mg_info_guild_flags[11]))
	{
		mg_ptr->flags |= MGF_GUILD;
		found = TRUE;
	}

	if (!found)
	{
		/* Oops */
		msgf("Unknown guild flag '%s'.", what);

		/* Error */
		return (PARSE_ERROR_GENERIC);
	}

	return(0);
}

/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_monster_flag(u32b *flags, cptr what)
{
	if (grab_one_flag(&flags[0], r_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&flags[1], r_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&flags[2], r_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&flags[3], r_info_flags4, what) == 0)
		return (0);

	if (grab_one_flag(&flags[4], r_info_flags5, what) == 0)
		return (0);

	if (grab_one_flag(&flags[5], r_info_flags6, what) == 0)
		return (0);

	if (grab_one_flag(&flags[6], r_info_flags7, what) == 0)
		return (0);

	if (grab_one_flag(&flags[7], r_info_flags8, what) == 0)
		return (0);

	if (grab_one_flag(&flags[8], r_info_flags9, what) == 0)
		return (0);


	/* Oops */
	msgf("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}

static errr grab_one_dungeon_type_flag(monster_group_type *mg_ptr, cptr what)
{
	return grab_one_flag(&mg_ptr->d_type, mg_info_dungeon_types, what);
}

static errr grab_one_dungeon_flag(monster_group_type *mg_ptr, cptr what)
{
	return grab_one_flag(&mg_ptr->d_flags, mg_info_dungeon_flags, what);
}

/*
 * Initialize the monster group arrays,
 *  by parsing an ascii "template" file
 */
errr init_mg_info_txt(FILE *fp, char *buf)
{
	char *s, *t;

	u16b i = 0;

	byte hook_num = 0;

	/* Current entry */
	monster_group_type *mg_ptr = NULL;

	/* Just before the first line */
	error_line = -1;

	/* The last index used */
	error_idx = -1;



	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (PARSE_ERROR_GENERIC);

		/* Process 'N' for "Number" (one line only) */
		if (buf[0] == 'N')
		{
			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Check to see if there is room in array */
			if (i > z_info->mg_max - 1) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Save the index */
			error_idx = i;

			/* point to new position in array */
			mg_ptr = &mg_info[i];

			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (PARSE_ERROR_GENERIC);

			/* Add the name */
			strncpy(mg_ptr->name, s, 32);

			/* Start over at hook 0. */
			hook_num = 0;

			continue;
		}

		/* Process 'G' for "Guild list" */
		if (buf[0] == 'G')
		{
			/* Parse every entry */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|') t++;
				}

				/* Parse this entry */
				if (0 !=
					grab_one_guild_flag(mg_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}
			continue;
		}

		/* Process 'H' for hook information (one line, up to 6 times) */
		if (buf[0] == 'H')
		{
			hook_rule_type *hrt_ptr;

			if (hook_num >= 6)
			{
				msgf ("Too many hooks.");
				return (PARSE_ERROR_OUT_OF_MEMORY);
			}

			hrt_ptr = &(mg_ptr->rule[hook_num++]);

			/* verify colon format */
			if (buf[3] != ':' || buf[5] != ':' || buf[7] != ':')
				return (PARSE_ERROR_GENERIC);

			/* Parse type */
			switch (buf[2])
			{
				case 'G':
					hrt_ptr->type = HRT_GRAPHIC;
					break;
				case 'F':
					hrt_ptr->type = HRT_FLAG;
					break;
				case 'N':
					hrt_ptr->type = HRT_NAME;
					break;
				case 'S':
					hrt_ptr->type = HRT_SPELL_FREQ;
					break;
				case 'X':
					hrt_ptr->type = HRT_RIDX;
					break;
				case 'B':
					hrt_ptr->type = HRT_BLOW;
					break;
				case 'H':
					hrt_ptr->type = HRT_HIT_DICE;
					break;
				case 'v':
				    hrt_ptr->type = HRT_SPEED;
					break;
				case 'D':
					hrt_ptr->type = HRT_DEPTH;
					break;
				case 'R':
					hrt_ptr->type = HRT_RARITY;
					break;
				case 'A':
					hrt_ptr->type = HRT_ARMOR;
					break;
				default:
					msgf ("Uknown hook rule type '%c'.", buf[2]);
					return (PARSE_ERROR_GENERIC);
			}

			/* Parse Include / Exclude */
			switch (buf[4])
			{
				case 'I':
					hrt_ptr->include = TRUE;
					break;
				case 'E':
					hrt_ptr->include = FALSE;
					break;
				default:
					msgf ("Expected 'E' or 'I', read '%c'.", buf[4]);
					return (PARSE_ERROR_GENERIC);
			}

			/* Parse neg */
			switch (buf[6])
			{
				case '+':
					hrt_ptr->neg = FALSE;
					break;
				case '-':
					hrt_ptr->neg = TRUE;
					break;
				default:
					msgf ("Expected '+' or '-', read '%c'.", buf[4]);
					return (PARSE_ERROR_GENERIC);
			}

			/* Parse data */
			if (buf[2] == 'F')
			{
				/* Monster flags: parse the flag descriptions, store raw data in data. */
				u32b flags[9];
				int j;

				/* Blank flags */
				for (j = 0; j < 9; flags[j++] = 0) /* loop */ ;

				/* Parse every entry */
				for (s = buf + 8; *s;)
				{
					/* Find the end of this entry */
					for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

					/* Nuke and skip any dividers */
					if (*t)
					{
						*t++ = '\0';
						while (*t == ' ' || *t == '|') t++;
					}

					/* Parse this entry */
					if (0 != grab_one_monster_flag(flags, s))
									 return (PARSE_ERROR_INVALID_FLAG);

					/* Start the next entry */
					s = t;
				}

				/* Turn flags into bytes */
				for (j = 0; j < 9; j++)
				{
					hrt_ptr->data[(4*j)] = (char) (flags[j] & 0x000000FF);
					hrt_ptr->data[(4*j)+1] = (char) ((flags[j] >> 8) & 0xFF);
					hrt_ptr->data[(4*j)+2] = (char) ((flags[j] >> 16) & 0xFF);
					hrt_ptr->data[(4*j)+3] = (char) ((flags[j] >> 24) & 0xFF);
				}

				/* Complete paranoia: test the algorithm  */
				for (j = 0; j < 9; j++)
				{
					u32b test;

					test = (u32b)hrt_ptr->data[(4*j)];
					test += ((u32b)(1 << 8) * hrt_ptr->data[(4*j)+1]);
					test += ((u32b)(1 << 16) * hrt_ptr->data[(4*j)+2]);
					test += ((u32b)(1 << 24) * hrt_ptr->data[(4*j)+3]);

					if (test != flags[j])
					{
						prtf (0, 0, "Flag to char conversion algorithm failed: %h %h", flags[j], test);
						message_flush();
					}
				}
			} /* Data for HRT_FLAG hooks */
			else if (buf[2] == 'B')
			{
				/* Monster blow: Parse the text and convert to a sequence
					MxExEx, etc., where M/B signifies method or effect, and x
					is the number of the type (not a string). */
				int j;
				int n = 0;
				bool found;

				for (s = buf + 8; *s;)
				{
					found = FALSE;

					/* Find the end of this entry */
					for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

					/* Nuke and skip any dividers */
					if (*t)
					{
						*t++ = '\0';
						while (*t == ' ' || *t == '|') t++;
					}

					/* Parse this entry */
					for (j = 0; r_info_blow_method[j]; j++)
					{
						if (streq(s, r_info_blow_method[j]))
						{
							hrt_ptr->data[n++] = 'M';
							hrt_ptr->data[n++] = (char)j;
							found = TRUE;
							break;
						}
					}

					/* Note: if there were ever an effect and a method with
					   the same name, this would end up putting them both in the
					   list.  But, (1) there aren't, and (2) even if there were,
					   this might well be the right thing to do. */
					for (j = 0; r_info_blow_effect[j]; j++)
					{
						if (streq(s, r_info_blow_effect[j]))
						{
							hrt_ptr->data[n++] = 'E';
							hrt_ptr->data[n++] = (char)j;
							found = TRUE;
							break;
						}
					}

					if (!found)
					{
						msgf ("Unknown monster blow method or effect: %s", s);
						return (PARSE_ERROR_GENERIC);
					}

					/* Continue with the next entry */
					s = t;
				}
			} /* Data for HRT_BLOW hooks */
			else
			{
				int x = atoi((buf+8));

				/* For all other hook types, we just copy the string. */
				strncpy (hrt_ptr->data, (buf+8), 128);

				/* For some hook types, we can detect problems. */
				if (x < 0)
				{
					switch(buf[2])
					{
						case 'S':
						case 'H':
						case 'v':
						case 'D':
						case 'R':
						case 'A':
							msgf ("Error: expected a number, read %s", (buf+8));
							return (PARSE_ERROR_GENERIC);
						default:
							break;
					}
				}
			}
			/* End processing 'H' for hooks */
			continue;
		}

		/* Process 'D' for dungeon type information */
		if (buf[0] == 'D')
		{
			/* Parse every entry */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|') t++;
				}

				/* Parse this entry */
				if (0 !=
					grab_one_dungeon_type_flag(mg_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}
			continue;
		}

		/* Process 'F' for dungeon flag information */
		if (buf[0] == 'F')
		{
			/* Parse every entry */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|') t++;
				}

				/* Parse this entry */
				if (0 !=
					grab_one_dungeon_flag(mg_ptr,
									   s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}
			continue;
		}

		/* Process 'L' for level information: one line only */
		if (buf[0] == 'L')
		{
			int x;
			int lmin = 1, lmax = 1, ladj = 0, nmin = 1, nmax = 1;

			if (mg_ptr->min_level)
			{
				msgf ("Duplicate L: information ignored at line %i", error_line);
				continue;
			}

			s = buf + 2;

			/* HACK: use sscanf to grab the values.  This means
			   we are not checking all the errors we could be, but
			   the code is much easier */
			x = sscanf(s, "%d:%d:%d:%d:%d", &lmin, &lmax, &ladj, &nmin, &nmax);

			mg_ptr->min_level = (byte)lmin;
			mg_ptr->max_level = (byte)lmax;
			mg_ptr->adj_level = (s16b)ladj;
			mg_ptr->min_levels = (byte)nmin;
			mg_ptr->max_levels = (byte)nmax;

			if (x < 3)
			{
				msgf ("Failed to parse L: information, read %s", s);
				return (PARSE_ERROR_GENERIC);
			}
			if (x == 3)
			{
				mg_ptr->min_levels = 1;
				mg_ptr->max_levels = 1;
			}
			if (x == 4)
			{
				mg_ptr->min_levels = MAX(1, mg_ptr->min_levels);
				mg_ptr->max_levels = mg_ptr->min_levels;
			}
			if (x == 5)
			{
				mg_ptr->min_levels = MAX(1, mg_ptr->min_levels);
				mg_ptr->max_levels = MAX(1, mg_ptr->max_levels);
			}
			/* Ensure sensible parameters */
			mg_ptr->min_levels = MIN(mg_ptr->min_levels, mg_ptr->max_levels);
			mg_ptr->max_levels = MAX(mg_ptr->min_levels, mg_ptr->max_levels);
			mg_ptr->min_level = MIN(mg_ptr->min_level, mg_ptr->max_level);
			mg_ptr->max_level = MAX(mg_ptr->min_level, mg_ptr->max_level);
		}

		/* Process 'W' for weight information: one line only */
		if (buf[0] == 'W')
		{
			if (mg_ptr->weight)
			{
				msgf ("Duplicate W: information ignored at line %i", error_line);
				continue;
			}

			mg_ptr->weight = (byte)(atoi(buf + 2));

			if (mg_ptr->weight == 0)
				msgf ("Caution: W: information gave weight 0 at line %i", error_line);
		}

	}

	/* Success */
	return (0);
}

s32b monster_auto_experience(monster_race *r_ptr, bool verbose)
{
	int danger;
	int damage = 0;
	int aggression = 100;
	s32b toughness = 0;
	int spells = 0;
	int naspells = 0;
	int ranged = 0;
	int melee = 0;
	int sfreq, schoice, achoice, emult, rmod, emod, hmod, round, amod;
	s32b exp;
	int min_d = 1;
	int min_t = 1;

	int power[4], power_base, rlev, i;
	int dam[4];
	int xhp, xhp_70;

	/* shortcut */
	if (FLAG(r_ptr, RF_WILD_TOWN)) return 0;
	/* if (FLAG(r_ptr, RF_FRIENDLY) && FLAG(r_ptr, RF_UNIQUE)) return 0;  */

	/* Effective monster level */
	rlev = ((r_ptr->hdice * 2 >= 1) ? r_ptr->level : 1);
	power_base = 3*rlev;

	/* Expected hit points; avg. of avg and max hp. */
	xhp = (FLAG(r_ptr, RF_FORCE_MAXHP) ? r_ptr->hdice*r_ptr->hside :
		   ((3*r_ptr->hdice *r_ptr->hside)+r_ptr->hdice)/4);
	xhp_70 = (70*xhp)/100;

	/* Determine melee attack power */
	for (i = 0; i < 4; i++) {
		/* Determine power for each blow */
		if (!r_ptr->blow[i].method) break;  /* no more attacks */
		switch (r_ptr->blow[i].effect)
		{
			case RBE_HURT:  case RBE_SHATTER:
				power[i] = power_base + 60;
				break;
			case RBE_LOSE_STR:  case RBE_LOSE_INT:  case RBE_LOSE_WIS:
			case RBE_LOSE_DEX:  case RBE_LOSE_CHR:  case RBE_LOSE_CON:
			case RBE_ACID:
				power[i] = power_base;
				break;
			case RBE_LOSE_ALL:  case RBE_PARALYZE:  case RBE_BLIND:
				power[i] = power_base+2;
				break;
			case RBE_POISON:  case RBE_EAT_ITEM:  case RBE_EAT_FOOD:
			case RBE_EAT_LITE: case RBE_EXP_10:  case RBE_EXP_20:
			case RBE_EXP_40: case RBE_EXP_80:  case RBE_DISEASE:
			case RBE_TIME: case RBE_EXP_VAMP:
				power[i] = power_base+5;
				break;
			case RBE_ELEC: case RBE_FIRE: case RBE_COLD:
			case RBE_CONFUSE: case RBE_TERRIFY:
				power[i] = power_base+10;
				break;
			case RBE_UN_BONUS:
				power[i] = power_base+20;
				break;
			case RBE_UN_POWER:
				power[i] = power_base+15;
				break;
			default:
				power[i] = 0;
				break;
		}
		/* determine damage for this attack */
		dam[i] = (r_ptr->blow[i].d_side ? (r_ptr->blow[i].d_dice *
										   (r_ptr->blow[i].d_side+1))/2 : 0);
		switch (r_ptr->blow[i].effect)
		{
			case RBE_POISON:
				dam[i] += 8;
				break;
			case RBE_UN_BONUS:
				dam[i] += 30;
				break;
			case RBE_UN_POWER:
				dam[i] += 15;
				break;
			case RBE_EAT_GOLD:
				dam[i] += 3;
				break;
			case RBE_EAT_ITEM:
				dam[i] += 10;
				break;
			case RBE_EAT_LITE:
				dam[i] += 3;
				break;
			case RBE_ACID:
				dam[i] += 15;
				break;
			case RBE_FIRE:
				dam[i] += 8;
				break;
			case RBE_COLD:  case RBE_ELEC:
				dam[i] += 3;
				break;
			case RBE_BLIND:
				dam[i] += 10;
				break;
			case RBE_CONFUSE:
				dam[i] += 15;
				break;
			case RBE_TERRIFY:
				dam[i] += 3;
				break;
			case RBE_PARALYZE:
				dam[i] += 45;
				break;
			case RBE_LOSE_STR:
				dam[i] += 25;
				min_d = MAX(min_d, 25);
				break;
			case RBE_LOSE_INT:  case RBE_LOSE_WIS:
				dam[i] += 10;
				break;
			case RBE_LOSE_DEX:  case RBE_LOSE_CON:
				dam[i] += 20;
				min_d = MAX(min_d, 15);
				break;
			case RBE_LOSE_CHR:
				dam[i] += 5;
				break;
			case RBE_LOSE_ALL:
				dam[i] += 60;
				min_d = MAX(min_d, 30);
				break;
			case RBE_SHATTER:
				dam[i] += 10;
				break;
			case RBE_EXP_10:
				dam[i] += 32;
				min_d = MAX(min_d, 50);
				break;
			case RBE_EXP_20:
				dam[i] += 33;
				min_d = MAX(min_d, 50);
				break;
			case RBE_EXP_40:
				dam[i] += 34;
				min_d = MAX(min_d, 50);
				break;
			case RBE_EXP_80:
				dam[i] += 36;
				min_d = MAX(min_d, 50);
				break;
			case RBE_DISEASE:
				dam[i] += 27;
				break;
			case RBE_TIME:
				dam[i] += 60;
				min_d = MAX(min_d, 100);
				break;
			case RBE_EXP_VAMP:
				dam[i] += 35;
				min_d = MAX(min_d, 50);
				break;
		}
	}

	/* calculate melee from attack powers */
	for (i = 0; i < 4; i++)
	{
		int p;
		if (!r_ptr->blow[i].method) break; /* no more attacks */
		p =MIN(120, power[i]);
		melee += (p*dam[i])/120;
	}

	/* calculate ranged damage from spell list */
	/* aggravate monsters */
	if (FLAG(r_ptr, RF_SHRIEK)) {
		spells++;
		ranged += 2;
	}

	/* fear and hallucination */
	if (FLAG(r_ptr, RF_ELDRITCH_HORROR)) {
		spells++;
		ranged += 28;
	}

	if (FLAG(r_ptr, RF_ROCKET)) {
		spells++;
		ranged += ((xhp_70 / 4) > 600 ? 600 : xhp_70/4)+250;
		min_d = MAX(min_d, 400);
	}

	if (FLAG(r_ptr, RF_ARROW)) {
		spells++;
		ranged += ((r_ptr->hdice < 4 ? 1 : r_ptr->hdice / 4)*7)/2;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BR_ACID)) {
		spells++;
		ranged += ((xhp_70 / 2) > 1200 ? 1200 : xhp_70/2)+15;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_FIRE)) {
		spells++;
		ranged += ((xhp_70 / 2) > 1200 ? 1200 : xhp_70/2)+10;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_COLD)) {
		spells++;
		ranged += ((xhp_70 / 2) > 1200 ? 1200 : xhp_70/2)+10;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_ELEC)) {
		spells++;
		ranged += ((xhp_70 / 2) > 1200 ? 1200 : xhp_70/2)+8;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_POIS)) {
		spells++;
		ranged += (5*((xhp_70 / 2) > 600 ? 600 : xhp_70/2))/4+15;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_NETH)) {
		spells++;
		ranged += (5*((xhp_70 / 4) > 450 ? 450 : xhp_70/4))/4+35;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_LITE)) {
		spells++;
		ranged += ((xhp_70 / 4) > 350 ? 350 : xhp_70/4)+10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_BR_DARK)) {
		spells++;
		ranged += ((xhp_70 / 4) > 350 ? 350 : xhp_70/4)+10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_BR_CONF)) {
		spells++;
		ranged += ((xhp_70 / 4) > 350 ? 350 : xhp_70/4)+20;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_SOUN)) {
		spells++;
		ranged += ((xhp_70 / 4) > 350 ? 350 : xhp_70/4)+10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_BR_CHAO)) {
		spells++;
		ranged += (5*((xhp_70 / 4) > 500 ? 500 : xhp_70/4))/4+20;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_DISE)) {
		spells++;
		ranged += (5*((xhp_70 / 4) > 400 ? 400 : xhp_70/4))/4+40;
		min_d = MAX(min_d, 50);
	}

	if (FLAG(r_ptr, RF_BR_NEXU)) {
		spells++;
		ranged += ((xhp_70 / 2) > 250 ? 250 : xhp_70/2)+5;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_TIME)) {
		spells++;
		ranged += (3*((xhp_70 / 2) > 150 ? 150 : xhp_70/2))/2+50;
		min_d = MAX(min_d, 50);
	}

	if (FLAG(r_ptr, RF_BR_INER)) {
		spells++;
		ranged += (5*((xhp_70 / 2) > 200 ? 200 : xhp_70/2))/4+30;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_GRAV)) {
		spells++;
		ranged += (5*((xhp_70 / 2) > 200 ? 200 : xhp_70/2))/4+30;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_SHAR)) {
		spells++;
		ranged += ((xhp_70 / 4) > 400 ? 400 : xhp_70/4)+10;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BR_PLAS)) {
		spells++;
		ranged += (3*((xhp_70 / 4) > 150 ? 150 : xhp_70/4))/2+10;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_WALL)) {
		spells++;
		ranged += (3*((xhp_70 / 4) > 200 ? 200 : xhp_70/4))/2;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_BR_MANA)) {
		spells++;
		ranged += (3*((xhp_70 / 4) > 200 ? 200 : xhp_70/4))/2;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_NUKE)) {
		spells++;
		ranged += (5*((xhp_70 / 2) > 600 ? 600 : xhp_70/2))/4+10;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BR_DISI)) {
		spells++;
		ranged += (3*((xhp_70 / 4) > 300 ? 300 : xhp_70/4))/2;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BA_NUKE)) {
		spells++;
		ranged += (3*(r_ptr->hdice * 2 + 35))/2+10;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BA_CHAO)) {
		spells++;
		ranged += (5*(r_ptr->hdice * 4 + 55))/4+20;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BA_ACID)) {
		spells++;
		ranged += (r_ptr->hdice * 7)/2+15;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BA_ELEC)) {
		spells++;
		ranged += (r_ptr->hdice * 7)/2+8;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BA_FIRE)) {
		spells++;
		ranged += (r_ptr->hdice * 7)/2+10;
		min_d = MAX(min_d, 25);
	}

	if (FLAG(r_ptr, RF_BA_COLD)) {
		spells++;
		ranged += (r_ptr->hdice * 7)/2+10;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_BA_POIS)) {
		spells++;
		ranged += 38;
	}

	if (FLAG(r_ptr, RF_BA_WATE)) {
		spells++;
		ranged += (5*(r_ptr->hdice * 9))/8;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BA_MANA)) {
		spells++;
		ranged += (3*(r_ptr->hdice * 8 + 55))/2;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BA_DARK)) {
		spells++;
		ranged += (r_ptr->hdice * 8 + 55)+10;
		min_d = MAX(min_d, 40);
	}

	/* no physical damage, but this is a kind of damage */
	if (FLAG(r_ptr, RF_DRAIN_MANA)) {
		spells++;
		ranged += 2*(r_ptr->hdice +1);
	}

	/* confusion and hallucination */
	if (FLAG(r_ptr, RF_MIND_BLAST)) {
		spells++;
		ranged += 35+36;
	}

	/* blind, confuse, paralyze, slow, hallucinate, dec int, wis, 12d15 */
	if (FLAG(r_ptr, RF_BRAIN_SMASH)) {
		spells++;
		ranged += 250;
	}

	/* curse equip + 3d8 damage */
	if (FLAG(r_ptr, RF_CAUSE_1)) {
		spells++;
		ranged += 13+15;
	}

	if (FLAG(r_ptr, RF_CAUSE_2)) {
		spells++;
		ranged += 36+15;
	}

	if (FLAG(r_ptr, RF_CAUSE_3)) {
		spells++;
		ranged += 80+15;
	}

	/* causes cuts instead of cursing */
	if (FLAG(r_ptr, RF_CAUSE_4)) {
		spells++;
		ranged += 120+10;
	}

	if (FLAG(r_ptr, RF_BO_ACID)) {
		spells++;
		ranged += (32 + (r_ptr->hdice*2)/3) + 15;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BO_ELEC)) {
		spells++;
		ranged += (18 + (r_ptr->hdice*2)/3) + 8;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BO_FIRE)) {
		spells++;
		ranged += (41 + (r_ptr->hdice*2)/3) + 10;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BO_COLD)) {
		spells++;
		ranged += (27 + (r_ptr->hdice*2)/3) + 15;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BO_NETH)) {
		spells++;
		ranged += (5*(45 + (r_ptr->hdice*3)))/4 + 15;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_BO_MANA)) {
		spells++;
		ranged += (31*(r_ptr->hdice))/2;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_BO_PLAS)) {
		spells++;
		ranged += (3*(42 + (r_ptr->hdice*2)))/2 + 10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_BO_ICEE)) {
		spells++;
		ranged += (3*(21 + (r_ptr->hdice*3)))/2 + 10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_MISSILE)) {
		spells++;
		ranged += (5*(7 + (r_ptr->hdice*2)/3))/4;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_SCARE)) {
		spells++;
		ranged += 5;
	}

	if (FLAG(r_ptr, RF_BLIND)) {
		spells++;
		ranged += 10;
	}

	if (FLAG(r_ptr, RF_CONF)) {
		spells++;
		ranged += 20;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_SLOW)) {
		spells++;
		ranged += 30;
	}

	if (FLAG(r_ptr, RF_HOLD)) {
		spells++;
		ranged += 30;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_HASTE)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 30);
	}

	/* approximated from player with 300hp */
	if (FLAG(r_ptr, RF_HAND_DOOM)) {
		spells++;
		ranged += 210+10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_HEAL)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_INVULNER)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_BLINK)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_TPORT)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_TELE_TO)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_TELE_AWAY)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_TELE_LEVEL)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_DARKNESS)) {
		spells++;
		naspells++;
	}

	if (FLAG(r_ptr, RF_TRAPS)) {
		spells++;
		ranged += 20;
	}

	if (FLAG(r_ptr, RF_FORGET)) {
		spells++;
		ranged += 25;
	}

	if (FLAG(r_ptr, RF_RAISE_DEAD)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_KIN)) {
		spells++;
		ranged += 10;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_CYBER)) {
		spells++;
		ranged += 100;
		min_d = MAX(min_d, 250);
	}

	if (FLAG(r_ptr, RF_S_MONSTER)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 10);
	}

	if (FLAG(r_ptr, RF_S_MONSTERS)) {
		spells++;
		ranged += 50;
		min_d = MAX(min_d, 20);
	}

	if (FLAG(r_ptr, RF_S_ANT)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_SPIDER)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_HOUND)) {
		spells++;
		ranged += 50;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_HYDRA)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 30);
	}

	if (FLAG(r_ptr, RF_S_ANGEL)) {
		spells++;
		ranged += 50;
		min_d = MAX(min_d, 50);
	}

	if (FLAG(r_ptr, RF_S_DEMON)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_S_UNDEAD)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 40);
	}

	if (FLAG(r_ptr, RF_S_DRAGON)) {
		spells++;
		naspells++;
		min_d = MAX(min_d, 50);
	}

	if (FLAG(r_ptr, RF_S_HI_UNDEAD)) {
		spells++;
		ranged += 100;
		min_d = MAX(min_d, 70);
	}

	if (FLAG(r_ptr, RF_S_HI_DRAGON)) {
		spells++;
		ranged += 100;
		min_d = MAX(min_d, 70);
	}

	if (FLAG(r_ptr, RF_S_AMBERITES)) {
		spells++;
		ranged += 300;
		min_d = MAX(min_d, 1200);
	}

	if (FLAG(r_ptr, RF_S_UNIQUE)) {
		spells++;
		ranged += 300;
		min_d = MAX(min_d, 1200);
	}

	/* some useful values */
	sfreq = (r_ptr->freq_inate + r_ptr->freq_spell)/2;
	achoice = (FLAG(r_ptr, RF_SMART) ? spells : (spells - naspells));
	schoice = (FLAG(r_ptr, RF_SMART) ? 1 : spells);

	emult = 100;
	if (FLAG(r_ptr, RF_RAND_25)) emult -= 25;
	if (FLAG(r_ptr, RF_RAND_50)) emult -= 50;

	/* Calculate damage */
	/* ranged first */
	if (spells) damage = (ranged * sfreq * achoice)/(100*spells);
	else damage = 0;

	/* add in melee damage */
	damage += ((100-sfreq)*melee)/100;

	/* reduce for monsters that are erratic */
	damage = (damage * emult) / 100;

	/* adjust for monster speed */
	damage = (damage*extract_energy[r_ptr->speed])/10;

	/* calculate aggressiveness */
	/* handle erratic first, then speed, then other traits. */
	if (FLAG(r_ptr, RF_RAND_25))  aggression -= 25;
	if (FLAG(r_ptr, RF_RAND_50))  aggression -= 50;
	if (FLAG(r_ptr, RF_NEVER_MOVE))  aggression -= 100;
	aggression = (aggression * extract_energy[r_ptr->speed])/10;
	/* outright bonus for being faster than a standard player */
	if (r_ptr->speed > 110) { aggression += 10;  min_d += 25; }

	/* sensory range */
	if (r_ptr->aaf <= 10)  aggression -= 10;
	else if (r_ptr->aaf >=80)  aggression += 20;
	else if (r_ptr->aaf >=50)  aggression += 10;

	/* Mimics are a little more dangerous */
	if (FLAG(r_ptr, RF_CHAR_MIMIC))  aggression += 10;

	if (FLAG(r_ptr, RF_FORCE_SLEEP)) aggression -= 10;


	/* A little harder to flee from if it's harder to see */
	if (FLAG(r_ptr, RF_COLD_BLOOD))  aggression += 3;
	if (FLAG(r_ptr, RF_EMPTY_MIND))  aggression += 3;
	if (FLAG(r_ptr, RF_WEIRD_MIND))  aggression += 2;
	if (FLAG(r_ptr, RF_LITE_1))  aggression -= 2;
	if (FLAG(r_ptr, RF_LITE_2))  aggression -= 3;
	if (FLAG(r_ptr, RF_INVISIBLE))  { aggression += 7;    min_d = MAX(min_d, 30); }



	if (FLAG(r_ptr, RF_OPEN_DOOR) || FLAG(r_ptr, RF_BASH_DOOR))
		aggression += 10;
	if (FLAG(r_ptr, RF_BASH_DOOR))  aggression += 2;

	if (FLAG(r_ptr, RF_PASS_WALL) || FLAG(r_ptr, RF_KILL_WALL))
		aggression += 30;
	if (FLAG(r_ptr, RF_KILL_WALL))  aggression += 10;

	if (FLAG(r_ptr, RF_MOVE_BODY) || FLAG(r_ptr, RF_KILL_BODY))
		aggression += 15;

	/* resistance to status effects cuts off one type of escape */
	if (FLAG(r_ptr, RF_NO_FEAR))  aggression += 5;
	if (FLAG(r_ptr, RF_NO_STUN))  aggression += 5;
	if (FLAG(r_ptr, RF_NO_CONF))  aggression += 5;
	if (FLAG(r_ptr, RF_NO_SLEEP))  aggression += 5;

	/* very slightly harder if it picks up or kills items */
	if (FLAG(r_ptr, RF_TAKE_ITEM) || FLAG(r_ptr, RF_KILL_ITEM))
		aggression += 2;

	/* can it swim, fly, walk? */
	if (FLAG(r_ptr, RF_CAN_SWIM) || FLAG(r_ptr, RF_CAN_FLY)) aggression += 5;
	if (FLAG(r_ptr, RF_CAN_FLY)) aggression += 5;
	if (FLAG(r_ptr, RF_AQUATIC)) aggression -= 25;

	/* Preventing blinks */
	if (FLAG(r_ptr, RF_TELE_TO)) aggression += 15;
	if (FLAG(r_ptr, RF_RES_TELE)) aggression += 15;


	if (aggression < 20) aggression = 20;

	/* Ready to calculate danger now */
	danger = (damage * (aggression > 400 ? 400 :
						(aggression < 20 ? 20 : aggression))) / 100;
	if (danger < min_d) danger = min_d;


	/* *********************
		Now, move on to toughness.  */

	/* Toughness formula:
		*  xhp * (AC modifier) * (Resist modifier) * (Heal modifier) * (Esc. mod)
		*      * (Summon modifier)
		*/

	/* Apply AC modifier
		* Amused: 100+100*100/100-.  But it makes sense!
		*  100+  because this is to be the percentage to use after applying the
		*        modifier.
		*  100*  because I'm calculating a percent.
		*  100/  because 100 is the asymptote for the formula.
		*  100-  for the same reason.  */
	toughness = xhp;
	min_t = (xhp / 3);
	if (r_ptr->ac >= 80) amod = 460;
	else
	{
		amod = (10000/(100-r_ptr->ac)) - 40;
	}

	toughness = (toughness * amod)/100;

	if (FLAG(r_ptr, RF_INVISIBLE)) toughness = (toughness * 110)/100;
	toughness = (toughness * (300+emult))/300;

	/* Caclulate Resist modifier.  Even the most vulnerable creature sill gets
		50%.  */
	rmod = 50;

	/* For each basic resistance, add 10%. */
	if (FLAG(r_ptr, RF_IM_ACID)) rmod += 10;
	if (FLAG(r_ptr, RF_IM_ELEC)) rmod += 10;
	if (FLAG(r_ptr, RF_IM_FIRE)) rmod += 10;
	if (FLAG(r_ptr, RF_IM_COLD)) rmod += 10;
	if (FLAG(r_ptr, RF_IM_POIS)) rmod += 10;

	/* For advanced resistances, which are rare attacks from the player,
		subtract 4%.  Skipping extremely rare types. */
	if (FLAG(r_ptr, RF_RES_NETH) || FLAG(r_ptr, RF_UNDEAD))
	{
		rmod += 4;
		if (FLAG(r_ptr, RF_UNDEAD)) rmod += 2;  /* immune */
	} else if (FLAG(r_ptr, RF_EVIL)) rmod += 2;  /* slight resistance */
 if (FLAG(r_ptr, RF_RES_NEXU) || FLAG(r_ptr, RF_BR_NEXU)) rmod += 4;
 if (FLAG(r_ptr, RF_RES_DISE)) rmod += 4;
 if (FLAG(r_ptr, RF_DEMON) || FLAG(r_ptr, RF_BR_CHAO)) rmod += 4;
 if (FLAG(r_ptr, RF_BR_SHAR)) rmod += 4;
 if (FLAG(r_ptr, RF_BR_SOUN)) rmod += 4;
 if (FLAG(r_ptr, RF_BR_CONF)) rmod += 4;
 else if (FLAG(r_ptr, RF_NO_CONF)) rmod += 2;  /* slight resistance */

 /* resistance for psi */
 if (FLAG(r_ptr, RF_EMPTY_MIND) || FLAG(r_ptr, RF_WEIRD_MIND)) rmod += 8;

 /* penalize for vulnerabilities */
 if (FLAG(r_ptr, RF_HURT_ROCK)) rmod -= 3;
 if (FLAG(r_ptr, RF_HURT_LITE)) rmod -= 6;

 /* miscellaneous stuff that might as well go here */
 if (FLAG(r_ptr, RF_GOOD) && !FLAG(r_ptr, RF_EVIL))  rmod += 2;
 if (FLAG(r_ptr, RF_REFLECTING))  rmod += 5;
 if (FLAG(r_ptr, RF_AURA_FIRE) ||
     FLAG(r_ptr, RF_AURA_COLD) ||
     FLAG(r_ptr, RF_AURA_ELEC)) { rmod += 10; min_t = MAX(min_t, 50); min_t = MAX(min_t, xhp); }
 if (FLAG(r_ptr, RF_AURA_FIRE)) rmod += 2;
 if (FLAG(r_ptr, RF_AURA_COLD)) rmod += 2;
 if (FLAG(r_ptr, RF_AURA_ELEC)) rmod += 2;

 /* apply it */
 toughness = (toughness * rmod)/100;



 /* Heal modifier */
 /* schoice != 0 if RF_HEAL present; schoice is either 1 or spells. */
 if (FLAG(r_ptr, RF_HEAL))
 {
	 toughness = (toughness * (100 + ((4*sfreq*emult)/100)/schoice))/100;
	 min_t = MAX(min_t, xhp);
 }

 hmod = 0;

 for (i = 0; i < 4; i++) {
	 int p = MAX(120, power[i]);
	 if (r_ptr->blow[i].effect == RBE_UN_POWER)  hmod += (5 * p)/120;
	 if (r_ptr->blow[i].effect == RBE_EXP_VAMP)  hmod += (10 * p)/120;
 }
 hmod = 100 + ((hmod * emult) / 100);

 if (FLAG(r_ptr, RF_REGENERATE))  { hmod += 10; min_t = MAX(min_t, xhp/2); }
 if (FLAG(r_ptr, RF_DRAIN_MANA))  hmod += (10 * emult)/100;

 toughness = (toughness * hmod)/100;

 /* Escape modifier */
 emod = 100;

 for (i = 0; i < 4; i++) {
	 int p = MAX(120, power[i]);
	 if (r_ptr->blow[i].effect == RBE_EAT_GOLD)  emod += (10 * p)/120;
	 if (r_ptr->blow[i].effect == RBE_EAT_ITEM)  emod += (10 * p)/120;
 }

 if (FLAG(r_ptr, RF_BLINK) ||
     FLAG(r_ptr, RF_TPORT) ||
     FLAG(r_ptr, RF_TELE_AWAY) ||
     FLAG(r_ptr, RF_TELE_LEVEL)) {
	 emod = emod + ((sfreq*emult)/100)/schoice;
	 min_t = MAX(min_t, (50*emult)/100);
 }

 toughness = (toughness * emod)/100;

 if (toughness < min_t) toughness = min_t;

 /* Toughness becomes less important the more one has. */
 if (toughness > 100) toughness = 100 + (toughness-100)/2;
 if (toughness > 300) toughness = 300 + (toughness-300)/3;

 /* Danger also scales down for very strong creatures. */
 if (danger > 300) danger = 300 + (danger-300)/2;
 if (danger > 1000) danger = 1000 + (danger-1000)/4;
 if (danger > 2000) danger = 2000 + (danger-3000)/6;
 /* Finally, calculate experience */

 exp = toughness * danger;

  /*
 if (verbose) msgf ("Exp: %d", exp);

   if (FLAG(r_ptr, RF_UNIQUE)) exp = (exp * 120)/100;

   if (verbose) msgf ("Unique: %d", exp);

   if (FLAG(r_ptr, RF_INVULNER)) exp = (exp * (100 + (3 * sfreq)/spells));

   if (verbose) msgf ("Invul: %d", exp);

   if (FLAG(r_ptr, RF_FRIENDS)) exp = (exp * 75) / 100;

   if (verbose) msgf ("FRIENDS: %d", exp);

   if (FLAG(r_ptr, RF_MULTIPLY)) exp = (exp * 75) / 100;

   if (verbose) msgf ("Multiply: %d", exp);
   */

 /* MEGA Hack:  Rebalance things manually */
 if (FLAG(r_ptr, RF_UNIQUE) && FLAG(r_ptr, RF_FRIENDLY)) exp = exp / 10;   /* Unique Eagles massively unbalanced */
 if (r_ptr->d_char == 'Q') exp = exp * 15; 	/* Qulthyugs got shafted */
	 if (FLAG(r_ptr, RF_MULTIPLY)) exp = exp / 3;

	 /* Hack: normalize */
	 if (r_ptr->level > 20) exp = (2*exp/3);
	 else if (r_ptr->level > 50) exp = exp/2;
	 else if (r_ptr->level > 80) exp = exp/3;


	 /* Hack: Keep things under control */
	 if (exp > (r_ptr->level)*(r_ptr->level)*(r_ptr->level)*(r_ptr->level)/6) {
		 if (verbose) msgf ("Boundary: %s: %d dm, %d ag = %d DN ... %d xhp, %d tgh",
			mon_race_name(r_ptr), damage, aggression, danger, xhp, toughness);
		 verbose = FALSE;
		 exp = (r_ptr->level)*(r_ptr->level)*(r_ptr->level)*(r_ptr->level)/6;
	 } else if (exp < (r_ptr->level -1)/2) exp = (r_ptr->level-1)/2;


	 /* round off */
	 if (exp <= 50) round = 1;
	 else if (exp <= 300) round = 5;
	 else if (exp <= 750) round = 10;
	 else if (exp <= 1500) round = 25;
	 else if (exp <= 4000) round = 50;
	 else if (exp <= 10000) round = 100;
	 else if (exp <= 100000) round = 1000;
	 else if (exp <= 1000000) round = 10000;
	 else round = 50000;

	 exp = (exp - (exp % round)) + (exp % round < round/2 ? 0 : round);

	 if (verbose) {
		 msgf ("%s: %d dm, %d ag = %d DN ... %d xhp, %d tgh",
			mon_race_name(r_ptr), damage, aggression, danger, xhp, toughness);
	 }
	 return exp;
}

static char s_info_realms[NUM_REALMS] =
{
	'L', 'S', 'N', 'C', 'D', 'J', 'A', 'I'
};

static char s_info_class[MAX_CLASS] =
{
	'Z', 'M', 'P', 'T', 'R', 'K', 'W', 'C', 'N', 'X', 'H'
};

static int s_info_char_to_realm(char c)
{
	int i;

	for (i = 0; i < NUM_REALMS; i++)
	{
		if (c == s_info_realms[i]) return (i);
	}

	return (-1);
}

static int s_info_char_to_class(char c)
{
	int i;

	for (i = 0; i < MAX_CLASS; i++)
	{
		if (c == s_info_class[i]) return (i);
	}

	return (-1);
}


/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr parse_s_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static spell_type *sp_ptr = NULL;
	static int realm;

	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf + 4, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the realm */
		realm = s_info_char_to_realm(buf[2]);

		/* Verify realm */
		if (realm < 0) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = atoi(buf + 4);

		/* Verify information */
		if (i < 0) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i > NUM_SPELLS) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Store the index */
		error_idx = (realm * NUM_SPELLS)+i;

		/* Point at the "info" */
		sp_ptr = &s_tmp_info[error_idx];

		/* Store the name */
		if (!(sp_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		char c = 0;
		int s_idx;
		int sval;

		/* There better be a current sp_ptr */
		if (!sp_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Save the values */
		i = sscanf(buf + 2, "%d:%d:%c",
						&s_idx, &sval, &c);

		/* Couldn't parse? */
		if (i < 2) return (PARSE_ERROR_GENERIC);

		sp_ptr->s_idx = s_idx;
		sp_ptr->sval = sval;

		if (i == 2) sp_ptr->realm = realm;
		else sp_ptr->realm = s_info_char_to_realm(c);
	}

	/* Hack -- Process 'C' for "cost info" */
	else if (buf[0] == 'C')
	{
		char c;
		int slevel, smana, power, cl;

		power = 0;

		/* There better be a current sp_ptr */
		if (!sp_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Save the values */
		i = sscanf(buf + 2, "%c:%d:%d:%d",
						&c, &slevel, &smana, &power);

		if (i < 3) return (PARSE_ERROR_GENERIC);

		/* Determine the class */
		cl = s_info_char_to_class(c);

		sp_ptr->info[cl].slevel = slevel;
		sp_ptr->info[cl].smana = smana;
		if (i == 4)
			sp_ptr->info[cl].power = power;
		else
			sp_ptr->info[cl].power = 0;
	}
	else
	{
		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Success */
	return (0);
}


