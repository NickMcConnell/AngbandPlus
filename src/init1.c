/* File: init1.c */

/*
 * Read the files in "/lib/edit", use them to fill in various arrays.
 *
 * Copyright (c) 2007 Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
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
 * Feature flags
 */
static cptr f_info_flags[] =
{
	"TF_LOS",
	"TF_PROJECT",
	"TF_PASSABLE",
	"TF_INTERESTING",
	"TF_PERMANENT",
	"TF_NO_SCENT",
	"TF_OBJECT",
	"TF_TRAP",
	"XXX9",
	"XX10",
	"XX11",
	"XX12",
	"XX13",
	"XX14",
	"XX15",
	"XX16",
	"TF_FLOOR",
	"TF_WALL",
	"TF_ROCK",
	"TF_GRANITE",
	"TF_DOOR_ANY",
	"TF_DOOR_CLOSED",
	"TF_SHOP",
	"XX24",
	"XX25",
	"XX26",
	"XX27",
	"XX28",
	"XX29",
	"XX30",
	"XX31",
	"XX32"
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
	"PECK",
	"XXX1",
	"BUTT",
	"CRUSH",
	"ENGULF",
	"CRAWL",
	"DROOL",
	"SPIT",
	"SLIME",
	"GAZE",
	"WAIL",
	"SPORE",
	"XXX4",
	"BEG",
	"INSULT",
	"SNEER",
	"TALK",
	"XXX5",
	NULL
};


/*
 * Monster Blow Effects
 */
static cptr r_info_blow_effect[] =
{
	"",
	"HURT",
	"WOUND",
	"BATTER",
	"SHATTER",

	"UN_BONUS",
	"UN_POWER",
	"LOSE_MANA",
	"EAT_GOLD",
	"EAT_ITEM",
	"EAT_FOOD",
	"EAT_LITE",
	"HUNGER",

	"POISON",
	"ACID",
	"ELEC",
	"FIRE",
	"COLD",

	"BLIND",
	"CONFUSE",
	"TERRIFY",
	"PARALYZE",
	"HALLU",
	"DISEASE",

	"LOSE_STR",
	"LOSE_INT",
	"LOSE_WIS",
	"LOSE_DEX",
	"LOSE_CON",
	"LOSE_CHR",
	"LOSE_LUC",
	"LOSE_ALL",

	"EXP_10",
	"EXP_20",
	"EXP_40",
	"EXP_80",

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
	"FIXED_HPS",
	"FORCE_SLEEP",
	"FORCE_EXTRA",
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
	"DROP_CHEST",
	"XXX1X1"
};

/*
 * Monster race flags
 */
static cptr r_info_flags2[] =
{
	"STUPID",
	"SMART",
	"XXX1X2",
	"PLAYER_GHOST",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"NOMISS",
	"SAMESPD",
	"EVASIVE",
	"FUNKY_DEATH",
	"CLOUD_SURROUND",
	"IS_LIT",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"XXX1",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"FLYING",
	"LOW_MANA_RUN",
	"BRAIN_1",
	"POWERFUL",
	"ARCHER",
	"MORGUL_MAGIC",
	"UDUN_MAGIC",
	"BRAIN_2"
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
	"RES_EDGED",
	"RES_BLUNT",
	"IM_EDGED",
	"IM_BLUNT",
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	"RES_PLAS",
	"RES_WATER",
	"RES_NEXUS",
	"RES_NETHR",
	"RES_CHAOS",
	"RES_DISEN",
	"RES_TPORT",
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
	"LASH",
	"BOULDER",
	"SHOT",
	"ARROW",
	"BOLT",
	"MISSL",
	"PMISSL",
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
	"BRTH_INER",
	"BRTH_GRAV",
	"BRTH_WIND",
	"BRTH_FORCE",
	"BRTH_NEXUS",
	"BRTH_NETHR",
	"BRTH_CHAOS",
	"BRTH_DISEN",
	"BRTH_TIME",
	"BRTH_MANA",
	"XXX41",
	"XXX42",
	"XXX43",
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
	"XXX51",
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
	"XXX53",
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
	"XXX3X6",
	"TELE_SELF_TO",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"XXX64",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"DRAIN_MANA",
	"XXX6",
	"CURSE",
	"MIND_BLAST",
	"XXX7",
	"WOUND",
	"HELLDARK",
	"HOLY_SMITE",
	"XXX9",
	"XX10",
	"HUNGER",
	"XX12",
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
	"XXX1",
	"XXX2",
	"S_MONSTER",
	"S_MONSTERS",
	"XXX3",
	"XXX4",
	"S_BEETLE",
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_ANIMAL",
	"XXX6",
	"XXX7",
	"S_THIEF",
	"S_BERTBILLTOM",
	"S_ORC",
	"S_ANGEL",
	"XX10",
	"XX11",
	"S_DRAGON",
	"S_HI_DRAGON",
	"XX12",
	"XX13",
	"S_DEMON",
	"S_HI_DEMON",
	"XX14",
	"XX15",
	"S_UNDEAD",
	"S_HI_UNDEAD",
	"S_WRAITH",
	"S_UNIQUE"
};





/*
 * Object flags
 */
static cptr k_info_flags_pval[] =
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
	"AWARENESS",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"INVIS",
	"DISARM",
	"DEVICE",
	"SAVE",
	"MANA",
	"LIGHT",
	"XXX5",
	"BLOWS",
	"SHOTS",
	"MIGHT",
	"XXX8",
	"XXX9",
	"XX10",
	"XX11",
	"XX12",
	"XX13",
	"XX14",
	"XX15",
	"XX16",
};

/*
 * Object flags
 */
static cptr k_info_flags1[] =
{
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	"XXX1",
	"XXX2",
	"SLAY_ANIMAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"KILL_DRAGON",
	"XXX4",
	"XXX5",
	"BRAND_ACID",
	"BRAND_ELEC",
	"BRAND_FIRE",
	"BRAND_COLD",
	"BRAND_POIS",
	"BRAND_FLAME",
	"BRAND_VENOM",
	"RETURNING",
	"VORPAL",
	"THROWING",
	"PERFECT_BALANCE",
	"TWO_HANDED_REQ",
	"TWO_HANDED_DES"
};

/*
 * Object flags
 */
static cptr k_info_flags2[] =
{
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_LITE",
	"RES_DARK",
	"RES_FEAR",
	"RES_BLIND",
	"RES_CONFU",
	"RES_SOUND",
	"RES_SHARD",
	"RES_NEXUS",
	"RES_NETHR",
	"RES_CHAOS",
	"RES_DISEN",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6",
	"EASY_ACTIVATE",
	"GLOW_WORDS",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD"
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
	"BLESSED",
	"IMPACT",
	"XXX1",
	"XXX2",
	"XXX3",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"ATTR_MULTI",
	"XXX5",
	"NO_FUEL",
	"SOULSTEAL",
	"NOMAGIC",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"DRAIN_HP",
	"XXX8",
	"XXX9",
	"XX10",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};

/*
 * Random attribute flags
 */
static cptr xtra_flags[] =
{
	"ADD_A_SUSTAIN",
	"ADD_A_HIGH_RESIST",
	"ADD_A_LOW_RESIST",
	"ADD_ANY_RESIST",
	"ADD_A_POWER",
	"MAYBE_VORPAL",
	"MAYBE_IMPACT",
	"XXX2",
	"XTRA_LIGHT_QUALITY",
	"XTRA_MISSILE_POWER",
	"ADD_DICE_SIDES",
	"CAN_BE_HEAVY",
	"XXX4",
	"XXX5",
	"HAS_NASTY_ACTIVATION",
	"HAS_NASTY_QUALITY"
};

/*
 * Essence cost flags
 */
static cptr k_info_essence_type[] =
{
	"ACID",
	"ELEC",
	"FIRE",
	"COLD",
	"POISON",
	"LIGHT",
	"DARK",
	"CONFUSION",
	"FORCE",
	"NEXUS",
	"NETHER",
	"CHAOS",
	"TIME",
	"MAGIC",
	"LIFE",
	"DEATH",
	"KNOWLEDGE",
	"SHIELD",
	"BLESSING",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
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
static bool add_text(u32b *offset, header *head, char *buf)
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

	/* Translate 7-bit encoded text into 8-bit Latin-1 text */
	xstr_trans(buf, LATIN1);

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
static u32b add_name(header *head, char *buf)
{
	u32b name_index;

	/* Hack -- Verify space */
	if (head->name_size + strlen(buf) + 8 > z_info->fake_name_size)
		return (0);

	/* Advance and save the name index */
	name_index = ++head->name_size;

	/* Translate the 7-bit encoded name into an 8-bit (Latin-1) name */
	xstr_trans(buf, LATIN1);

	/* Append chars to the names */
	strcpy(head->name_ptr + head->name_size, buf);

	/* Advance the index */
	head->name_size += strlen(buf);

	/* Return the name index */
	return (name_index);
}


/*
 * Initialize the "z_info" structure, by parsing an ascii "template" file
 */
errr parse_z_info(char *buf, header *head)
{
	z_info = (maxima*) head->info_ptr;

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

	/* Process 'Q' for "Maximum q_info[] index" */
	else if (buf[2] == 'Q')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->q_max = max;
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

	/* Process 'X' for "Maximum x_info[] index" */
	else if (buf[2] == 'X')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->x_max = max;
	}

	/* Process 'x' for "Maximum t_list[] index" */
	else if (buf[2] == 'x')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->t_max = max;
	}

	/* Process 'L' for "Maximum flavor_info[] subindex" */
	else if (buf[2] == 'L')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->flavor_max = max;
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

	/* Process 'T' for "Fake text size" */
	else if (buf[2] == 'T')
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

	/* Process 'X' for "Extra info" (one line only) */
	else if (buf[0] == 'X')
	{
		int typ, rat, hgt, wid, min_lev, max_lev;

		/* There better be a current v_ptr */
		if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
			&typ, &rat, &hgt, &wid, &min_lev, &max_lev))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Correct a value. */
		if (max_lev == 0) max_lev = MAX_DEPTH;

		/* Save the values */
		v_ptr->typ = typ;
		v_ptr->rat = rat;
		v_ptr->hgt = hgt;
		v_ptr->wid = wid;
		v_ptr->min_lev = min_lev;
		v_ptr->max_lev = max_lev;
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
static errr grab_one_u16b_flag(u16b *flags, cptr names[], cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 16; i++)
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
 * Grab one flag in a feature_type from a textual string
 */
static bool grab_one_feat_flag(feature_type *f_ptr, cptr what)
{
	if (grab_one_flag(&f_ptr->flags, f_info_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown feature_type flag '%s'.", what);

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

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_feat_flag(f_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
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

		/* Verify color */
		if ((tmp == TERM_WHITE) && (buf[4] != 'w')) return (PARSE_ERROR_UNKNOWN_COLOR);

		/* Save the values */
		f_ptr->d_attr = tmp;
		f_ptr->d_char = buf[2];
	}


	/* Process 'C' for "Special values" (one line only) */
	else if (buf[0] == 'C')
	{
		int tmp;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d", &tmp))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		f_ptr->priority = (byte)tmp;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&(f_ptr->text), head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process "!:Verify Terrain" for double-check on terrain. */
	else if ((buf[0] == '!') && (strstr(buf, "!:Verify Terrain")))
	{
		/* Scan the whole feature list (except for darkness) */
		for (i = 1; i < z_info->f_max; i++)
		{
			feature_type *f2_ptr = NULL;

			f_ptr = (feature_type*)head->info_ptr + i;

			/* This feature is not a mimic -- ignore */
			if ((f_ptr->mimic == 0) || (f_ptr->mimic == i))
			{
				continue;
			}

			f2_ptr = (feature_type*)head->info_ptr + f_ptr->mimic;

			/* Add the flags of the mimiced feature */
			f_ptr->flags |= (f2_ptr->flags);
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
 * Grab one flag in an object_kind from a textual string
 */
static bool grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
	if (grab_one_flag(&k_ptr->flags_pval, k_info_flags_pval, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown object_kind flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one extra random attribute in an object_kind from a textual string
 */
static bool grab_one_kind_xtra_flag(object_kind *k_ptr, cptr what)
{
	if (grab_one_u16b_flag(&k_ptr->xtra, xtra_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown object_kind xtra flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one essence type from a textual string
 */
static int grab_one_essence_flag(cptr what)
{
	int i;

	/* Check essences */
	for (i = 0; i < NUM_ESSENCE; i++)
	{
		if (streq(what, k_info_essence_type[i]))
		{
			return (i);
		}
	}

	/* Oops */
	msg_format("Unknown essence type '%s'.", what);

	/* Error */
	return (-1);
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

		/* Verify color */
		if ((tmp == TERM_WHITE) && (buf[4] != 'w')) return (PARSE_ERROR_UNKNOWN_COLOR);

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
		if (3 != sscanf(buf+2, "%d:%d:%d", &tval, &sval, &pval))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		k_ptr->tval = tval;
		k_ptr->sval = sval;
		k_ptr->pval = pval;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, act, wgt;
		long cost;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld", &level, &act, &wgt, &cost))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		k_ptr->level    = level;
		k_ptr->activate = (byte)act;
		k_ptr->weight   = wgt;
		k_ptr->cost     = cost;
	}

	/* Process 'A' for "Allocation" (one line only) */
	else if (buf[0] == 'A')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* XXX Simply read each number following a colon */
		for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
		{
			/* Sanity check (allow 4 allocations) */
			if (i > 3) return (PARSE_ERROR_TOO_MANY_ALLOCATIONS);

			/* Store the attack damage index */
			k_ptr->locale[i] = atoi(s+1);

			/* Find the slash */
			t = strchr(s+1, '/');

			/* Find the next colon */
			s = strchr(s+1, ':');

			/* If the slash is "nearby", use it */
			if (t && (!s || t < s))
			{
				k_ptr->chance[i] = MAX(0, atoi(t+1));
			}
		}
	}

	/* Process 'M' for "Multiple quantity" (one line only) */
	else if (buf[0] == 'M')
	{
		int prob, dice, side;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%dd%d", &prob, &dice, &side))				{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		k_ptr->gen_mult_prob = prob;
		k_ptr->gen_dice = dice;
		k_ptr->gen_side = side;
	}

	/* Hack -- Process 'C' for various "creation" values */
	else if (buf[0] == 'C')
	{
		int ac, hd1, hd2, th, td, ta;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			            &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

		k_ptr->ac   = ac;
		k_ptr->dd   = hd1;
		k_ptr->ds   = hd2;
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
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_kind_flag(k_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_kind_xtra_flag(k_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Hack -- Process 'E' for essence cost */
	else if (buf[0] == 'E')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (i = 0, s = buf + 2; *s; i++)
		{
			int num, essence_type;

			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Skip the space after the essence name */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ') t++;
			}

			/* Parse this entry */
			essence_type = grab_one_essence_flag(s);

			/* Note errors */
			if (essence_type < 0) return (PARSE_ERROR_INVALID_FLAG);

			/* Expect a number of some kind */
			if (1 != sscanf(t, "%d", &num))
				return (PARSE_ERROR_NO_ESSENCE_NUM);

			/* Hack -- advance t */
			if (num >= 10) t += 2;
			else           t += 1;

			/* Fill in essence costs */
			if (i < 4)
			{
				k_ptr->e_type[i] = essence_type;
				k_ptr->e_num[i]  = num;
			}
			else
			{
				return (PARSE_ERROR_TOO_MANY_ESSENCES);
			}

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
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
		s = buf+2;

		/* Store the text */
		if (!add_text(&(k_ptr->text), head, s))
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
 * Grab one pval-dependant flag in an artifact_type from a textual string
 */
static bool grab_one_artifact_pval_flag(artifact_type *a_ptr, cptr what, int pval)
{
	if (pval == 1)
	{
		if (grab_one_flag(&a_ptr->flags_pval1, k_info_flags_pval, what) == 0)
			return (0);
	}
	if (pval == 2)
	{
		if (grab_one_flag(&a_ptr->flags_pval2, k_info_flags_pval, what) == 0)
			return (0);
	}
	if (pval == 3)
	{
		if (grab_one_flag(&a_ptr->flags_pval3, k_info_flags_pval, what) == 0)
			return (0);
	}

	/* Oops */
	msg_format("Unknown artifact pval flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one non pval-dependant flag in an artifact_type from a textual string
 */
static bool grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
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
 * Grab one extra random attribute in an artifact_type from a textual string
 */
static bool grab_one_artifact_xtra_flag(artifact_type *a_ptr, cptr what)
{
	if (grab_one_u16b_flag(&a_ptr->xtra, xtra_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown artifact xtra flag '%s'.", what);

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
		int tval, sval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &tval, &sval))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		a_ptr->tval = tval;
		a_ptr->sval = sval;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, wgt, num;
		long cost;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values -- including max_num */
		if (5 != sscanf(buf+2, "%d:%d:%d:%ld:%d",
			&level, &rarity, &wgt, &cost, &num))
		{
			/* Assume one artifact */
			a_ptr->max_num = 1;

			/* Scan for the values -- no max num */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld", &level, &rarity, &wgt, &cost))
			{
				return (PARSE_ERROR_GENERIC);
			}
		}
		else
		{
			a_ptr->max_num = (byte)num;
		}


		/* Save the values */
		a_ptr->level =  level;
		a_ptr->rarity = rarity;
		a_ptr->weight = wgt;
		a_ptr->cost =   cost;
	}

	/* Process 'C' for various "creation" values */
	else if (buf[0] == 'C')
	{
		int ac, hd1, hd2, th, td, ta, act;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values (check for activation) */
		if (7 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d",
			&ac, &hd1, &hd2, &th, &td, &ta, &act))
		{
			/* Scan for the values (no activation) */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
				&ac, &hd1, &hd2, &th, &td, &ta))
			{
				return (PARSE_ERROR_GENERIC);
			}
		}
		else
		{
			a_ptr->activate = (byte)act;
		}

		a_ptr->ac   = ac;
		a_ptr->dd   = hd1;
		a_ptr->ds   = hd2;
		a_ptr->to_h = th;
		a_ptr->to_d = td;
		a_ptr->to_a = ta;
	}

	/* Process 'P' for "pval values" (one line only) */
	else if (buf[0] == 'P')
	{
		int pval1 = 0, pval2 = 0, pval3 = 0;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values (assume three pval values) */
		if (3 != sscanf(buf+2, "%d:%d:%d", &pval1, &pval2, &pval3))
		{
			/* Scan for the values (assume two pval values) */
			if (2 != sscanf(buf+2, "%d:%d", &pval1, &pval2))
			{
				/* Scan for the values (assume one pval value) */
				if (1 != sscanf(buf+2, "%d", &pval1))
				{
					return (PARSE_ERROR_GENERIC);
				}
			}
		}

		/* Save the pvals */
		a_ptr->pval1 = pval1;
		a_ptr->pval2 = pval2;
		a_ptr->pval3 = pval3;
	}

	/* Process '1', '2', and '3' for pval-dependant flags (one line only) */
	else if ((buf[0] == '1') || (buf[0] == '2') || (buf[0] == '3'))
	{
		int this_pval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the pval now being modified */
		if      (buf[0] == '1') this_pval = 1;
		else if (buf[0] == '2') this_pval = 2;
		else                    this_pval = 3;

		/* Verify the pval */
		if (buf[0] == '1')
		{
			if (a_ptr->pval1 == 0) return (PARSE_ERROR_NO_PVAL1);
		}
		else if (buf[0] == '2')
		{
			if (a_ptr->pval2 == 0) return (PARSE_ERROR_NO_PVAL2);
		}
		else
		{
			if (a_ptr->pval3 == 0) return (PARSE_ERROR_NO_PVAL3);
		}

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_artifact_pval_flag(a_ptr, s, this_pval))
				return (PARSE_ERROR_INVALID_FLAG);


			/* Start the next entry */
			s = t;
		}
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
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_artifact_flag(a_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_artifact_xtra_flag(a_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&(a_ptr->text), head, s))
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
 * Grab one pval-dependant flag in an ego-item_type from a textual string
 */
static bool grab_one_ego_item_pval_flag(ego_item_type *e_ptr, cptr what, int pval)
{
	if (pval == 1)
	{
		if (grab_one_flag(&e_ptr->flags_pval1, k_info_flags_pval, what) == 0)
			return (0);
	}
	else
	{
		if (grab_one_flag(&e_ptr->flags_pval2, k_info_flags_pval, what) == 0)
			return (0);
	}

	/* Oops */
	msg_format("Unknown ego-item pval flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one non pval-dependant flag in an ego-item_type from a textual string
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
 * Grab one extra random attribute in an ego-item_type from a textual string
 */
static bool grab_one_ego_item_xtra_flag(ego_item_type *e_ptr, cptr what)
{
	if (grab_one_u16b_flag(&e_ptr->xtra, xtra_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown ego-item xtra flag '%s'.", what);

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
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int level, rarity, rating;
		long cost;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			&level, &rarity, &rating, &cost))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		e_ptr->level =  level;
		e_ptr->rarity = rarity;
		e_ptr->rating = rating;
		e_ptr->cost =   cost;
	}

	/* Hack -- Process 'C' for "creation" */
	else if (buf[0] == 'C')
	{
		int th, td, ta, act;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values (check for an activation index) */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d", &th, &td, &ta, &act))
		{
			/* Scan for the values (assume no activation index) */
			if (3 != sscanf(buf+2, "%d:%d:%d", &th, &td, &ta))
			{
				return (PARSE_ERROR_GENERIC);
			}
		}
		else
		{
			/* Store the activation index */
			e_ptr->activate = (byte)act;
		}

		e_ptr->mod_to_h = th;
		e_ptr->mod_to_d = td;
		e_ptr->mod_to_a = ta;
	}

	/* Process 'T' for "Types allowed" (up to three lines) */
	else if (buf[0] == 'T')
	{
		int tval, sval1, sval2;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d", &tval, &sval1, &sval2))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		e_ptr->tval[cur_t] = (byte)tval;
		e_ptr->min_sval[cur_t] = (byte)sval1;
		e_ptr->max_sval[cur_t] = (byte)sval2;

		/* Increase counter for 'possible tval' index */
		cur_t++;

		/* Allow only a limited number of T: lines */
		if (cur_t > EGO_TVALS_MAX) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'P' for "pval values" (one line only) */
	else if (buf[0] == 'P')
	{
		int pval1 = 0, pval2 = 0;

		/* Scan for the values (assume two pval values) */
		if (2 != sscanf(buf+2, "%d:%d", &pval1, &pval2))
		{
			/* Scan for the values (assume one pval value) */
			if (1 != sscanf(buf+2, "%d", &pval1))
			{
				return (PARSE_ERROR_GENERIC);
			}
		}

		/* Save the pvals */
		e_ptr->max_pval1 = pval1;
		e_ptr->max_pval2 = pval2;
	}

	/* Process '1' and '2' for pval-dependant flags (one line only) */
	else if ((buf[0] == '1') || (buf[0] == '2'))
	{
		int this_pval;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the pval now being modified */
		if (buf[0] == '1') this_pval = 1;
		else               this_pval = 2;

		/* Verify the pval */
		if (buf[0] == '1')
		{
			if (e_ptr->max_pval1 == 0) return (PARSE_ERROR_NO_PVAL1);
		}
		else
		{
			if (e_ptr->max_pval2 == 0) return (PARSE_ERROR_NO_PVAL2);
		}

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_ego_item_pval_flag(e_ptr, s, this_pval))
				return (PARSE_ERROR_INVALID_FLAG);


			/* Start the next entry */
			s = t;
		}
	}


	/* Hack -- Process 'F' for non pval-dependant flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_ego_item_flag(e_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Parse every entry textually */
		for (s = buf + 2; *s; )
		{
			/* Find the end of this entry */
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Parse this entry */
			if (0 != grab_one_ego_item_xtra_flag(e_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

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
		s = buf+2;

		/* Store the text */
		if (!add_text(&(e_ptr->text), head, s))
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

	if (grab_one_flag(&r_ptr->flags7, r_info_flags7, what) == 0)
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

		/* Verify color */
		if ((tmp == TERM_WHITE) && (buf[4] != 'w')) return (PARSE_ERROR_UNKNOWN_COLOR);

		/* Save the values */
		r_ptr->d_attr = tmp;
		r_ptr->d_char = sym;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp, aaf, ac, slp, rng, nos;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


		/* Scan for the other values */
		if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
			&spd, &hp, &aaf, &ac, &slp, &rng, &nos))
		{
			/* Scan for the other values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
				&spd, &hp, &aaf, &ac, &slp, &rng))
			{
				/* Scan for the other values (no value for range) */
				if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
					&spd, &hp, &aaf, &ac, &slp))
				{
					return (PARSE_ERROR_GENERIC);
				}
				rng = 0;
			}

			/* Assume a noise of 20, unless specified */
			nos = 20;
		}

		/* Save the values */
		r_ptr->speed = spd;
		r_ptr->hitpoints = hp;
		r_ptr->aaf = aaf;
		r_ptr->ac = ac;
		r_ptr->sleep = slp;
		r_ptr->combat_range = rng;
		r_ptr->noise = nos;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, mana;
		long exp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld", &lev, &rar, &mana, &exp))
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		r_ptr->level =  lev;
		r_ptr->rarity = rar;
		r_ptr->mana =   mana;
		r_ptr->mexp =   exp;
	}

	/* Process 'B' for "Blows" */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < MONSTER_BLOW_MAX; i++)
		{
			if (!r_ptr->blow[i].method) break;
		}

		/* Oops, no more slots */
		if (i == MONSTER_BLOW_MAX) return (PARSE_ERROR_GENERIC);

		/* Analyze the first field */
		for (s = t = buf+2; *t && (*t != ':'); t++); /* loop */

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
		for (s = t; *t && (*t != ':'); t++); /* loop */

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
		for (s = t; *t && (*t != 'd'); t++); /* loop */

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
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while (*t == ' ' || *t == '|') t++;
			}

			/* Parse this entry */
			if (0 != grab_one_basic_flag(r_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

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
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t); /* loop */

			/* Nuke and skip any dividers */
			if (*t)
			{
				*t++ = '\0';
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* XXX Hack -- Read spell frequency */
			if ((r_ptr->freq_ranged == 0) &&
			    (1 == sscanf(s, "1_IN_%d", &i)))
			{
				/* Sanity check */
				if ((i < 1) || (i > 100))
					return (PARSE_ERROR_INVALID_SPELL_FREQ);

				/* Extract a "frequency" */
				r_ptr->freq_ranged = 100 / i;

				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Read spell power. */
			if ((r_ptr->spell_power == 0) &&
			    (1 == sscanf(s, "POW_%d", &i)))
			{
				/* Save spell power. */
				r_ptr->spell_power = i;

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
 * Initialize the "q_info" array, by parsing an ascii "template" file
 */
errr parse_q_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static quest_type *q_ptr = NULL;

	static int prev_lev = 0;

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
		if (i >= head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		q_ptr = (quest_type*)head->info_ptr + i;

		/* Store the name */
		if (!(q_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'W' for "Where/What" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, r_idx, max;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",&lev, &r_idx, &max))
			return (PARSE_ERROR_GENERIC);

		/* Check quests */
		for (i = 0; i < error_idx; i++)
		{
			/* Check for quest */
			if (lev <= prev_lev) return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		prev_lev = q_ptr->base_level = q_ptr->active_level = lev;
		q_ptr->r_idx = r_idx;
		q_ptr->type = QUEST_FIXED;


		q_ptr->max_num = max;
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
 * Initialize the "flavor_info" array, by parsing an ascii "template" file
 */
errr parse_flavor_info(char *buf, header *head)
{
	/* Current entry */
	static flavor_type *flavor_ptr;


	/* Process 'N' for "Number" */
	if (buf[0] == 'N')
	{
		int index, tval, sval, level;

		/* Scan the value -- sval and level are optional */
		if (4 != sscanf(buf, "N:%d:%d:%d:%d", &index, &tval, &sval, &level))
		{
			level = 0;

			if (3 != sscanf(buf, "N:%d:%d:%d", &index, &tval, &sval))
			{
				sval = SV_ANY;

				if (2 != sscanf(buf, "N:%d:%d", &index, &tval))
					return (PARSE_ERROR_GENERIC);
			}
		}

		/* Verify information */
		if (index <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (index >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = index;

		/* Point at the "info", using the given index */
		flavor_ptr = (flavor_type*)head->info_ptr + index;

		/* Hack -- Allow use of "-1" to mean "any sval" */
		if (sval < 0) sval = SV_ANY;

		/* Save the tval */
		flavor_ptr->tval = (byte)tval;

		/* Save the sval */
		flavor_ptr->sval = (byte)sval;

		/* Save the minimum level */
		flavor_ptr->level = (byte)level;
	}

	/* Process 'G' for "Graphics" */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current flavor_ptr */
		if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract the char */
		d_char = buf[2];

		/* Extract the attr */
		d_attr = color_char_to_attr(buf[4]);

		/* Verify color */
		if ((d_attr == TERM_WHITE) && (buf[4] != 'w')) return (PARSE_ERROR_UNKNOWN_COLOR);

		/* Save the values */
		flavor_ptr->d_attr = (byte)d_attr;
		flavor_ptr->d_char = d_char;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current flavor_ptr */
		if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[1]) return (PARSE_ERROR_GENERIC);
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Store the text */
		if (!add_text(&flavor_ptr->text, head, buf + 2))
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
 * Tval initializer
 */
static cptr tval_init_desc[] =
{
	"",
	"SKELETON",    "",            "JUNK",        "",           "SPIKE",
	"",            "CHEST",       "",            "",           "",
	"",            "",            "",            "",           "",
	"SHOT",        "ARROW",       "BOLT",        "BOW",        "DIGGING",
	"HAFTED",      "POLEARM",     "SWORD",       "",           "",
	"",            "",            "",            "",           "BOOTS",
	"GLOVES",      "HELM",        "CROWN",       "SHIELD",     "CLOAK",
	"SOFT_ARMOR",  "HARD_ARMOR",  "DRAG_ARMOR",  "LITE",       "AMULET",
	"",            "",            "",            "",           "RING",
	"",            "",            "",            "",           "",
	"",            "",            "",            "",           "STAFF",
	"",            "",            "",            "",           "",
	"",            "",            "",            "",           "WAND",
	"ROD",         "",            "",            "",           "SCROLL",
	"",            "",            "",            "",           "POTION",
	"",            "FLASK",       "",            "",           "FOOD",
	"",            "",            "",            "",           "COMPONENT",
	"PARCHMENT",  "BOTTLE",       "",            "",           "MAGIC_BOOK",
	"PRAYER_BOOK","NATURE_BOOK",  "DARK_BOOK",   "",           "",
	"",            "",            "ESSENCE",     "",           ""
};

/*
 * Shopkeeper race initializer
 */
static cptr race_init_desc[MAX_RACES] =
{
	"RACE_HUMAN",
	"RACE_ELF",
	"RACE_HOBBIT",
	"RACE_GNOME",
	"RACE_DWARF",
	"RACE_HALF_ORC",
	"RACE_HALF_TROLL",
	"RACE_DUNADAN",
	"RACE_HIGH_ELF",
	"RACE_DARK_ELF",
	"RACE_GIANT",
	"RACE_ENT"
};




/*
 * Get a tval by reading a string
 */
static byte grab_one_tval(cptr what)
{
	byte i;

	for (i = 0; i <= 100; i++)
	{
		if (streq(what, tval_init_desc[i])) return (i);
	}

	return (255);
}


/*
 * Get a store owner race by reading a string
 */
static byte grab_one_owner_race(cptr what)
{
	byte i;

	for (i = 0; i < MAX_RACES; i++)
	{
		if (streq(what, race_init_desc[i])) return (i);
	}

	return (255);
}


/*
 * Initialize various store arrays by parsing an ascii "template" file.  -LM-
 */
errr parse_store(void)
{
	int i;
	byte b;
	char str[DESC_LEN];
	int num = -1;
	s32b val;
	bool okay = FALSE;
	bool use_val;

	bool owner = FALSE;
	bool stock = FALSE;
	bool tval  = FALSE;
	bool price = FALSE;

	int array_idx = -1;
	int array_element;


	/* General buffer */
	char buf[1024];

	char *s, *t;

	FILE *fp;

	/* Get a location */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, "store.txt");

	/* Start at the front of the file */
	fp = my_fopen(buf, "r");

	/* If file doesn't exist, we just use the standard tables */
	if (!fp) return (0);


	/* Start at first line */
	error_line = 0;


	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf+2, "%d.%d.%d", &v1, &v2, &v3)) ||
			    (v1 != VERSION_MAJOR) ||
			    (v2 != VERSION_MINOR) ||
			    (v3 != VERSION_PATCH))
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

		/* Process 'G' for "Global variable" */
		if (buf[0] == 'G')
		{
			s = buf+2;

			/* Keep scanning until we find a digit */
			while (*s) if (isdigit(*s++)) break;

			/* Try to read the value as a number */
			val = atoi(s-1);

			/* We have a number */
			if (val)
			{
				/* bargain_difficulty */
				if (strstr(buf, "bargain_difficulty"))
				{
					bargain_difficulty = val;
				}
				else
				{
					return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
				}
			}
			else
			{
				return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
			}
		}

		/* Process '!' for "New Array" */
		else if (buf[0] == '!')
		{
			/* Owners array */
			if (strstr(buf, "STORE_OWNERS"))
			{
				owner = TRUE;
				stock = FALSE;
				tval  = FALSE;
				price = FALSE;
				array_idx = 0;
			}

			/* Store stock array */
			else if (strstr(buf, "STORE_STOCK"))
			{
				owner = FALSE;
				stock = TRUE;
				tval  = FALSE;
				price = FALSE;
				array_idx = 0;
			}

			/* Tval sell array */
			else if (strstr(buf, "TVAL_SELL"))
			{
				owner = FALSE;
				stock = FALSE;
				tval  = TRUE;
				price = FALSE;
				array_idx = 0;
			}

			/* Racial price modifier array */
			else if (strstr(buf, "RGOLD_ADJ"))
			{
				owner = FALSE;
				stock = FALSE;
				tval  = FALSE;
				price = TRUE;
				array_idx = 0;
			}

			/* Unknown array */
			else
			{
				return (PARSE_ERROR_UNKNOWN_ARRAY);
			}
		}

		/* Process 'N' for "Store Number" */
		else if (buf[0] == 'N')
		{
			/* Get the index */
			num = atoi(buf+2);

			/* Restart the array index count */
			array_idx = 0;
		}

		/* Process 'D' for array values */
		else if (buf[0] == 'D')
		{
			/* Require a legal array index */
			if (array_idx < 0) return (PARSE_ERROR_NO_ARRAY_SPECIFIED);

			/* Parse every entry textually */
			for (array_element = 0, s = buf + 2; *s; array_element++)
			{
				/* Skip to the next array element */
				for (; *s && strchr(" ,", *s); ++s);

				/* Try to read this value as a number */
				val = atoi(s);

				/* Value is a number */
				if ((val) || (*s == '0'))
				{
					use_val = TRUE;
				}

				/* Value is not a number */
				else
				{
					t = s;

					/* Read the value */
					for (i = 0; i < 80 && *t; i++, t++)
					{
						/* Only stop at commas.  Accept spaces, etc. */
						if (strchr(",", *t)) break;
						else str[i] = *t;
					}

					/* End the string */
					str[i] = '\0';

					use_val = FALSE;
				}

				/* Skip past this array element */
				for (; *s && (!strchr(",", *s)); ++s);


				/* Owner array */
				if (owner)
				{
					/* Require a valid store number */
					if (num < 0 || num >= MAX_STORES)
						return (PARSE_ERROR_INVALID_STORE);

					/* Handle inputs */
					if (array_element == 0)
					{
						strcpy(owners[num][array_idx].owner_name, str);
					}
					else if (array_element == 1)
					{
						owners[num][array_idx].max_cost = (u16b)val;
					}
					else if (array_element == 2)
					{
						owners[num][array_idx].max_inflate = (byte)val;
					}
					else if (array_element == 3)
					{
						owners[num][array_idx].min_inflate = (byte)val;
					}
					else if (array_element == 4)
					{
						owners[num][array_idx].haggle_per = (byte)val;
					}
					else if (array_element == 5)
					{
						owners[num][array_idx].insult_max = (byte)val;
					}
					else if (array_element == 6)
					{
						b = grab_one_owner_race(str);
						if (b >= MAX_RACES) return (PARSE_ERROR_OUT_OF_BOUNDS);

						owners[num][array_idx].owner_race = b;
					}
					else
						return (PARSE_ERROR_OUT_OF_BOUNDS);
				}

				/* Store stock array */
				else if (stock)
				{
					/* Stay within bounds of table */
					if (array_idx >= STORE_STOCK_SIZE)
					{
						return (PARSE_ERROR_OUT_OF_BOUNDS);
					}

					/* Handle inputs */
					if (array_element == 0)
					{
						store_stock[array_idx].k_idx = (s16b)val;
					}
					else if (array_element == 1)
					{
						store_stock[array_idx].prob = (byte)val;
					}
					else if (array_element == 2)
					{
						store_stock[array_idx].num = (byte)val;
					}
					else
					{
						return (PARSE_ERROR_OUT_OF_BOUNDS);
					}
				}

				/* Tval sell array */
				else if (tval)
				{
					/* Stay legal */
					if (array_idx >= MAX_STORES)
						return (PARSE_ERROR_OUT_OF_BOUNDS);

					if (array_element < 10)
					{
						if (use_val) b = 0;
						else b = grab_one_tval(str);
						if (b >= 101) return (PARSE_ERROR_OUT_OF_BOUNDS);

						tval_sell[array_idx][array_element] = b;
					}
					else
					{
						return (PARSE_ERROR_OUT_OF_BOUNDS);
					}
				}

				/* Racial price modifier array */
				else if (price)
				{
					/* Stay legal */
					if ((array_idx >= MAX_RACES) || (array_element >= MAX_RACES))
					{
						return (PARSE_ERROR_OUT_OF_BOUNDS);
					}
					rgold_adj[array_idx][array_element] = (byte)val;
				}

				/* Unknown array */
				else
				{
					return (PARSE_ERROR_UNKNOWN_ARRAY);
				}
			}

			/* Advance the array index */
			array_idx++;
		}
		else
		{
			/* Oops */
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}

	/* Close */
	(void)my_fclose(fp);

	/* Success */
	return (0);
}



#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */

