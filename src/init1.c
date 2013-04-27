/* File: init1.c */

/*
 * Copyright (c) 1997 Ben Harrison
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

#include "angband.h"
#include "tvalsval.h"

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
 * Monster Blow Methods
 */
/*
 * Monster Blow Methods
 */
static const char* const r_info_blow_method[] =
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
	"XXX5",	/* 24 */
	"", "", "", "", "", "", "", "",	/* 32 */
	"", "", "", "", "", "", "", "",	/* 40 */
	"", "", "", "", "", "", "", "",	/* 48 */
	"", "", "", "", "", "", "", "",	/* 56 */
	"", "", "", "", "", "", "", "",	/* 64 */
	"", "", "", "", "", "", "", "",	/* 72 */
	"", "", "", "", "", "", "", "",	/* 80 */
	"", "", "", "", "", "", "", "",	/* 88 */
	"", "", "", "", "", "", "", "",	/* 96 */
	"", "", "", "", "", "", "", "",	/* 104 */
	"", "", "", "", "", "", "", "",	/* 112 */
	"", "", "", "", "", "", "", "",	/* 120 */
	"", "", "", "", "", "", "", "",	/* 128 */
	"", "", "", "", "", "", "", "",	/* 136 */
	"", "", "", "", "", "", "", "",	/* 144 */
	"", "", "", "", "", "", "", "",	/* 152 */
	"", "", "", "", "", "", "", "",	/* 160 */
	"", "", "", "", "", "", "", "",	/* 168 */
	"", "", "", "", "", "", "", "",	/* 176 */
	"", "", "", "", "", "", "", "",	/* 184 */
	"", "", "", "", "", "", "", "",	/* 192 */
	"", "", "", "", "", "", "", "",	/* 200 */
	"", "", "", "", "", "", "", "",	/* 208 */
	"", "", "", "", "", "", "", "",	/* 216 */
	"", "", "", "", "", "", "", "",	/* 224 */
	"", "", "", "", "", "", "", "",	/* 232 */
	"", "", "", "", "", "", "", "",	/* 240 */
	"", "", "", "", "", "", "", "",	/* 248 */
	"", "", "",						/* 251 */
	"CLAIRVOYANT_GAZE",				/* 252 */
	"RANGED_GAZE",					/* 253 */
	"HYPEROPIC_GAZE",				/* 254 */
	"PRESBYOPIC_GAZE",				/* 255 */
	NULL
};


/*
 * Monster Blow Effects
 */
static const char* const r_info_blow_effect[] =
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
	"HALLU",
	NULL
};


/*
 * Monster race flags
 */
static const char* const r_info_flags1[] =
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
static const char* const r_info_flags2[] =
{
	"STUPID",
	"SMART",
	NULL,
	NULL,
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	NULL,
	NULL,
	"POWERFUL",
	NULL,
	NULL,
	NULL,
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
	"BRAIN_8"
};

/*
 * Monster race flags
 */
static const char* const r_info_flags3[] =
{
	"ORC",
	"TROLL",
	"GIANT",
	"DRAGON",
	"DEMON",
	"UNDEAD",
	"EVIL",
	"ANIMAL",
	NULL,
	NULL,
	NULL,
	NULL,
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	NULL,
	"RES_NETH",
	"IM_WATER",
	"RES_PLAS",
	"RES_NEXUS",
	"RES_DISE",
	NULL,
	"NO_FEAR",
	"NO_STUN",
	"NO_CONF",
	"NO_SLEEP"
};

/*
 * Monster race flags
 */
static const char* const r_info_flags4[] =
{
	"SHRIEK",
	NULL,
	NULL,
	NULL,
	"ARROW_1",
	"ARROW_2",
	"ARROW_3",
	"ARROW_4",
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
	NULL,
	NULL,
	NULL,
	"BOULDER"
};

/*
 * Monster race flags
 */
static const char* const r_info_flags5[] =
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
static const char* const r_info_flags6[] =
{
	"HASTE",
	NULL,
	"HEAL",
	NULL,
	"BLINK",
	"TPORT",
	NULL,
	NULL,
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	NULL,
	"DARKNESS",
	"TRAPS",
	"FORGET",
	NULL,
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
 * Object flags
 */
static const char* const k_info_flags1[] =
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
	"SLAY_ANIMAL",
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
static const char* const k_info_flags2[] =
{
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
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
static const char* const k_info_flags3[] =
{
	"SLOW_DIGEST",
	"FEATHER",
	"LITE",
	"REGEN",
	"TELEPATHY",
	"SEE_INVIS",
	"FREE_ACT",
	"HOLD_LIFE",
	NULL,
	NULL,
	NULL,
	NULL,
	"IMPACT",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD",
	NULL,
	NULL,
	"BLESSED",
	NULL,
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	NULL,
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};


/*
 * Activation type
 */
static const char* const a_info_act[ACT_MAX] =
{
	"ILLUMINATION",
	"MAGIC_MAP",
	"CLAIRVOYANCE",
	"PROT_EVIL",
	"DISP_EVIL",
	"HEAL1",
	"HEAL2",
	"CURE_WOUNDS",
	"HASTE1",
	"HASTE2",
	"FIRE1",
	"FIRE2",
	"FIRE3",
	"FROST1",
	"FROST2",
	"FROST3",
	"FROST4",
	"FROST5",
	"ACID1",
	"RECHARGE1",
	"SLEEP",
	"LIGHTNING_BOLT",
	"ELEC2",
	"BANISHMENT",
	"MASS_BANISHMENT",
	"IDENTIFY",
	"DRAIN_LIFE1",
	"DRAIN_LIFE2",
	"BIZZARE",
	"STAR_BALL",
	"RAGE_BLESS_RESIST",
	"PHASE",
	"TRAP_DOOR_DEST",
	"DETECT",
	"RESIST",
	"TELEPORT",
	"RESTORE_LIFE",
	"MISSILE",
	"ARROW",
	"REM_FEAR_POIS",
	"STINKING_CLOUD",
	"STONE_TO_MUD",
	"TELE_AWAY",
	"WOR",
	"CONFUSE",
	"PROBE",
	"FIREBRAND",
	"STARLIGHT",
	"MANA_BOLT",
	"BERSERKER"
};


/*
 * Class flags
 */
static const char* const c_info_flags[] =
{
	"EXTRA_SHOT",
	"BRAVERY_30",
	"BLESS_WEAPON",
	"CUMBER_GLOVE",
	"ZERO_FAIL",
	"BEAM",
	"CHOOSE_SPELLS",
	"PSEUDO_ID_HEAVY",
	"PSEUDO_ID_IMPROV",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};


/*** Initialize from ascii template files ***/

/*
 * X+NdY parser helper
 */
/* should be instantiated with an integral type */
/* actually using this is going to bump the version */
template<class T>
bool parse_X_NdY(const char* s, T& X, dice_sides& NdY)
{
	int base, ds, dd;
	
	/* Scan for the values */
	if (1 == sscanf(s, "%d", &base))
	{
		X = base;
		NdY.clear();
	}
	else if (1 == sscanf(s, "d%d", &ds))
	{
		X = 0;
		NdY.dice = 1;
		NdY.sides = ds;
	}
	else if (2 == sscanf(s, "%d+d%d", &base, &ds))
	{
		X = base;
		NdY.dice = 1;
		NdY.sides = ds;
	}
	else if (2 == sscanf(s, "%dd%d", &dd, &ds))
	{
		X = 0;
		NdY.dice = dd;
		NdY.sides = ds;
	}
	else if (3 == sscanf(s, "%d+%dd%d", &base, &dd, &ds))
	{
		X = base;
		NdY.dice = dd;
		NdY.sides = ds;
	}
	else
	{
		return FALSE;
	}
	return TRUE;
}

/* instantiate with integer-like types */
/* may not play well with unsigned long */
template<class T>
bool parse_integer_like(const char* s, T& X)
{
	long temp;

	/* Scan for the value */
	if (1 != sscanf(s, "%ld", &temp)) return FALSE;

	/* Save the value */
	X = temp;
	return TRUE;
}

template<class T,class T2>
bool parse_integer_like(const char* s, T& X, T2& X2)
{
	long temp,temp2;

	/* Scan for the value */
	if (2 != sscanf(s, "%ld:%ld", &temp, &temp2)) return FALSE;

	/* Save the value */
	X = temp;
	X2 = temp2;
	return TRUE;
}

template<class T,class T2,class T3>
bool parse_integer_like(const char* s, T& X, T2& X2, T3& X3)
{
	long temp,temp2,temp3;

	/* Scan for the value */
	if (3 != sscanf(s, "%ld:%ld:%ld", &temp, &temp2, &temp3)) return FALSE;

	/* Save the value */
	X = temp;
	X2 = temp2;
	X3 = temp3;
	return TRUE;
}

template<class T,class T2,class T3,class T4>
bool parse_integer_like(const char* s, T& X, T2& X2, T3& X3, T4& X4)
{
	long temp,temp2,temp3,temp4;

	/* Scan for the value */
	if (4 != sscanf(s, "%ld:%ld:%ld:%ld", &temp, &temp2, &temp3, &temp4)) return FALSE;

	/* Save the value */
	X = temp;
	X2 = temp2;
	X3 = temp3;
	X4 = temp4;
	return TRUE;
}

/*
 * Initialize an "*_info" array, by parsing an ascii "template" file
 */
errr init_info_txt(FILE *fp, char *buf, header *head,
                   parse_info_txt_func parse_info_txt_line)
{
	errr err;
	bool okay = FALSE;	/* Not ready yet */
	error_idx = -1;		/* Just before the first record */
	error_line = 0;		/* Just before the first line */

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
		if (buf[1] != ':') return (PARSE_ERROR_MISSING_COLON);


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
static bool add_text(u32b *offset, header *head, const char* const buf)
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
static u32b add_name(header *head, const char* const buf)
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
	maxima *z_info = (maxima*)head->info_ptr;

	/* Hack - Verify 'M:x:' format */
	if (buf[0] != 'M') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (!buf[2]) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (buf[3] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);


	/* Process 'F' for "Maximum f_info[] index" */
	if (buf[2] == 'F')
	{
		if (!parse_integer_like(buf+4,z_info->f_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'K' for "Maximum k_info[] index" */
	else if (buf[2] == 'K')
	{
		if (!parse_integer_like(buf+4,z_info->k_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'A' for "Maximum a_info[] index" */
	else if (buf[2] == 'A')
	{
		if (!parse_integer_like(buf+4,z_info->a_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'E' for "Maximum e_info[] index" */
	else if (buf[2] == 'E')
	{
		if (!parse_integer_like(buf+4,z_info->e_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'R' for "Maximum r_info[] index" */
	else if (buf[2] == 'R')
	{
		if (!parse_integer_like(buf+4,z_info->r_max)) return (PARSE_ERROR_GENERIC);
	}


	/* Process 'V' for "Maximum v_info[] index" */
	else if (buf[2] == 'V')
	{
		if (!parse_integer_like(buf+4,z_info->v_max)) return (PARSE_ERROR_GENERIC);
	}


	/* Process 'P' for "Maximum p_info[] index" */
	else if (buf[2] == 'P')
	{
		if (!parse_integer_like(buf+4,z_info->p_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'C' for "Maximum c_info[] index" */
	else if (buf[2] == 'C')
	{
		if (!parse_integer_like(buf+4,z_info->c_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'H' for "Maximum h_info[] index" */
	else if (buf[2] == 'H')
	{
		if (!parse_integer_like(buf+4,z_info->h_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'B' for "Maximum b_info[] subindex" */
	else if (buf[2] == 'B')
	{
		if (!parse_integer_like(buf+4,z_info->b_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'L' for "Maximum flavor_info[] subindex" */
	else if (buf[2] == 'L')
	{
		if (!parse_integer_like(buf+4,z_info->flavor_max)) return (PARSE_ERROR_GENERIC);
	}
	
	/* Process 'O' for "Maximum o_list[] index" */
	else if (buf[2] == 'O')
	{
		if (!parse_integer_like(buf+4,z_info->o_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'M' for "Maximum mon_list[] index" */
	else if (buf[2] == 'M')
	{
		if (!parse_integer_like(buf+4,z_info->m_max)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'N' for "Fake name size" */
	else if (buf[2] == 'N')
	{
		if (!parse_integer_like(buf+4,z_info->fake_name_size)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'T' for "Fake text size" */
	else if (buf[2] == 'T')
	{
		if (!parse_integer_like(buf+4,z_info->fake_text_size)) return (PARSE_ERROR_GENERIC);
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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		if (!(v_ptr->_name = add_name(head, s)))
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
		if (!add_text(&v_ptr->_text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'X' for "Extra info" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current v_ptr */
		if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,v_ptr->typ,v_ptr->rat,v_ptr->hgt,v_ptr->wid)) return (PARSE_ERROR_GENERIC);

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
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr parse_f_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static feature_type *f_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		if (!(f_ptr->_name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Default "mimic" */
		f_ptr->mimic = i;
	}

	/* Process 'M' for "Mimic" (one line only) */
	else if (buf[0] == 'M')
	{
		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		if (!parse_integer_like(buf+2,f_ptr->mimic)) return (PARSE_ERROR_GENERIC);
		if (f_ptr->mimic >= z_info->f_max) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
		f_ptr->d_attr = d_attr;
		f_ptr->d_char = d_char;
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
static errr grab_one_flag(u32b *flags, const char* const names[], const char* const what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (NULL!=names[i] && streq(what, names[i]))
		{
			*flags |= (1L << i);
			return (0);
		}
	}

	return (-1);
}

static errr grab_object_flag(u32b* flags,const char* const what,const char* const err_str)
{
	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(k_info_flags1));
	if (grab_one_flag(&flags[0], k_info_flags1, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(k_info_flags2));
	if (grab_one_flag(&flags[1], k_info_flags2, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(k_info_flags3));
	if (grab_one_flag(&flags[2], k_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format(err_str, what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, const char* const what)
{
	return grab_object_flag(k_ptr->flags,what,"Unknown object flag '%s'.");
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


	/* Process 'N' for "New/Name" */
	if (buf[0] == 'N')
	{
		/* Advance to the name */
		s = buf+2;

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the index */
		i = error_idx+1;

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		k_ptr = (object_kind*)head->info_ptr + i;

		/* Store the name */
		if (!(k_ptr->_name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
		k_ptr->d_attr = d_attr;
		k_ptr->d_char = d_char;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,k_ptr->obj_id.tval,k_ptr->obj_id.sval,k_ptr->pval)) return (PARSE_ERROR_GENERIC);

		{	/* XXX replicate lookup_kind code here without messages XXX */
		int k;

		/* Look for it */
		for (k = 1; k < error_idx-1; k++)
			{
			object_kind *alt_k_ptr = &((object_kind*)(k_head.info_ptr))[k];

			/* Found a match */
			if (k_ptr->obj_id == alt_k_ptr->obj_id) {plog("Items must not duplicate tval/sval"); return (PARSE_ERROR_GENERIC);};
			}
		}

		/* rod charging algorithm takes division-by-zero on pval, would be weird with negative pval */
		if (TV_ROD == k_ptr->obj_id.tval && 0 >= k_ptr->pval) {plog("Rods must have positive pval"); return (PARSE_ERROR_GENERIC);};
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,k_ptr->level,k_ptr->extra,k_ptr->weight,k_ptr->cost)) return (PARSE_ERROR_GENERIC);
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
		k_ptr->d.dice = hd1;
		k_ptr->d.sides = hd2;
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
		if (!add_text(&(k_ptr->_text), head, s))
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
static errr grab_one_artifact_flag(artifact_type *a_ptr, const char* const what)
{
	return grab_object_flag(a_ptr->flags,what,"Unknown artifact flag '%s'.");
}


/*
 * Grab one activation from a textual string
 */
static errr grab_one_activation(artifact_type *a_ptr, const char* const what)
{
	int i;

	/* Scan activations */
	for (i = 0; i < ACT_MAX; i++)
	{
		if (streq(what, a_info_act[i]))
		{
			a_ptr->activation = i;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown artifact activation '%s'.", what);

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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		if (!(a_ptr->_name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
		a_ptr->flags[2] |= (TR3_IGNORE_MASK);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,a_ptr->obj_id.tval,a_ptr->obj_id.sval,a_ptr->pval)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,a_ptr->level,a_ptr->rarity,a_ptr->weight,a_ptr->cost)) return (PARSE_ERROR_GENERIC);
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
		a_ptr->d.dice = hd1;
		a_ptr->d.sides = hd2;
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
		int time, rand;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the colon before the name */
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_MISSING_COLON);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the activation */
		if (grab_one_activation(a_ptr, buf + 2)) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (2 != sscanf(s, "%d:%d",
			            &time, &rand)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->time.base = time;
		a_ptr->time.range.dice = 1;
		a_ptr->time.range.sides = rand;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&a_ptr->_text, head, s))
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
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, const char* const what)
{
	return grab_object_flag(e_ptr->flags,what,"Unknown ego-item flag '%s'.");
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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		if (!(e_ptr->_name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Start with the first of the tval indices */
		cur_t = 0;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int pad2;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,e_ptr->level,e_ptr->rarity,pad2,e_ptr->cost)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,e_ptr->rating,e_ptr->xtra)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'T' for "Types allowed" (up to three lines) */
	else if (buf[0] == 'T')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,e_ptr->tval[cur_t],e_ptr->min_sval[cur_t],e_ptr->max_sval[cur_t])) return (PARSE_ERROR_GENERIC);

		/* Increase counter for 'possible tval' index */
		cur_t++;

		/* Allow only a limited number of T: lines */
		if (cur_t > EGO_TVALS_MAX) return (PARSE_ERROR_GENERIC);
	}

	/* Hack -- Process 'C' for "creation" */
	else if (buf[0] == 'C')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,e_ptr->max_to_h,e_ptr->max_to_d,e_ptr->max_to_a,e_ptr->max_pval)) return (PARSE_ERROR_GENERIC);
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

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&e_ptr->_text, head, s))
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
static errr grab_one_basic_flag(monster_race *r_ptr, const char* const what)
{
	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags1));
	if (grab_one_flag(&r_ptr->flags[0], r_info_flags1, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags2));
	if (grab_one_flag(&r_ptr->flags[1], r_info_flags2, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags3));
	if (grab_one_flag(&r_ptr->flags[2], r_info_flags3, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Grab one (spell) flag in a monster_race from a textual string
 */
static errr grab_one_spell_flag(monster_race *r_ptr, const char* const what)
{
	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags4));
	if (grab_one_flag(&r_ptr->spell_flags[0], r_info_flags4, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags5));
	if (grab_one_flag(&r_ptr->spell_flags[1], r_info_flags5, what) == 0)
		return (0);

	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(r_info_flags6));
	if (grab_one_flag(&r_ptr->spell_flags[2], r_info_flags6, what) == 0)
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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		if (!(r_ptr->_name = add_name(head, s)))
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
		if (!add_text(&(r_ptr->_text), head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
		r_ptr->d_attr = d_attr;
		r_ptr->d_char = d_char;
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
		r_ptr->h.dice = hp1;
		r_ptr->h.sides = hp2;
		r_ptr->aaf = aaf;
		r_ptr->ac = ac;
		r_ptr->sleep = slp;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,r_ptr->level,r_ptr->rarity,r_ptr->extra,r_ptr->mexp)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'B' for "Blows" */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < MONSTER_BLOW_MAX; i++) if (!r_ptr->blow[i].method) break;

		/* Oops, no more slots */
		if (i == MONSTER_BLOW_MAX) return (PARSE_ERROR_GENERIC);

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
		r_ptr->blow[i].method = n1;

		/* Save the effect */
		r_ptr->blow[i].effect = n2;

		/* Extract the damage dice and sides */
		r_ptr->blow[i].d.dice = atoi(s);
		r_ptr->blow[i].d.sides = atoi(t);
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
static errr grab_one_racial_flag(player_race *pr_ptr, const char* const what)
{
	return grab_object_flag(pr_ptr->flags,what,"Unknown player flag '%s'.");
}

/*
 * helper function for reading skills from config files
 */
static bool parse_skills(s16b* skills_array,char* buf)
{
	int dis, dev, sav, stl, srh, fos, thn, thb;

	/* Scan for the values */
	if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &thn, &thb)) return FALSE;

	/* Save the values */
	skills_array[SKILL_DISARM] = dis;
	skills_array[SKILL_DEVICE] = dev;
	skills_array[SKILL_SAVE] = sav;
	skills_array[SKILL_STEALTH] = stl;
	skills_array[SKILL_SEARCH] = srh;
	skills_array[SKILL_SEARCH_FREQUENCY] = fos;
	skills_array[SKILL_TO_HIT_MELEE] = thn;
	skills_array[SKILL_TO_HIT_BOW] = thb;

	return TRUE;
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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = strchr(s, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan and save the values */
		if (!parse_skills(pr_ptr->r_skills,buf)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'X' for "Extra Info" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pr_ptr->r_mhp,pr_ptr->r_exp,pr_ptr->infra)) return (PARSE_ERROR_GENERIC);
	}

	/* Hack -- Process 'I' for "info" and such */
	else if (buf[0] == 'I')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pr_ptr->hist,pr_ptr->age.b,pr_ptr->age.m)) return (PARSE_ERROR_GENERIC);
	}

	/* Hack -- Process 'H' for "Height" */
	else if (buf[0] == 'H')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pr_ptr->height[SEX_MALE].b,pr_ptr->height[SEX_MALE].m,pr_ptr->height[SEX_FEMALE].b,pr_ptr->height[SEX_FEMALE].m)) return (PARSE_ERROR_GENERIC);
	}

	/* Hack -- Process 'W' for "Weight" */
	else if (buf[0] == 'W')
	{
		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pr_ptr->weight[SEX_MALE].b,pr_ptr->weight[SEX_MALE].m,pr_ptr->weight[SEX_FEMALE].b,pr_ptr->weight[SEX_FEMALE].m)) return (PARSE_ERROR_GENERIC);
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
 * Grab one flag in a player class from a textual string
 */
static errr grab_one_class_flag(player_class *pc_ptr, const char* const what)
{
	ZAIBAND_STATIC_ASSERT(32==N_ELEMENTS(c_info_flags));
	if (grab_one_flag(&pc_ptr->flags, c_info_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown player class flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "c_info" array, by parsing an ascii "template" file
 */
errr parse_c_info(char *buf, header *head)
{
	int i, j;

	char *s, *t;

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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

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
			if (!s) return (PARSE_ERROR_MISSING_COLON);

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
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan and save the values */
		if (!parse_skills(pc_ptr->c_skills,buf)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'X' for "Extra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan and save the values */
		if (!parse_skills(pc_ptr->x_skills,buf)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pc_ptr->c_mhp,pc_ptr->c_exp,pc_ptr->sense_base,pc_ptr->sense_div)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'A' for "Attack Info" (one line only) */
	else if (buf[0] == 'A')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pc_ptr->max_attacks,pc_ptr->min_weight,pc_ptr->att_multiply)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'M' for "Magic Info" (one line only) */
	else if (buf[0] == 'M')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,pc_ptr->spell_book,pc_ptr->spell_stat,pc_ptr->spell_first,pc_ptr->spell_weight)) return (PARSE_ERROR_GENERIC);
	}

	/* Process 'B' for "Spell/Prayer book info" */
	else if (buf[0] == 'B')
	{
		int spell, level, mana, fail, exp;
		player_magic *mp_ptr;
		magic_type *spell_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
		                &spell, &level, &mana, &fail, &exp))
			return (PARSE_ERROR_GENERIC);

		/* Validate the spell index */
		if ((spell >= PY_MAX_SPELLS) || (spell < 0))
			return (PARSE_ERROR_OUT_OF_BOUNDS);

		mp_ptr = &pc_ptr->spells;
		spell_ptr = &mp_ptr->info[spell];

		/* Save the values */
		spell_ptr->slevel = level;
		spell_ptr->smana = mana;
		spell_ptr->sfail = fail;
		spell_ptr->sexp = exp;
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
		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (!parse_integer_like(buf+2,e_ptr->obj_id.tval,e_ptr->obj_id.sval,e_ptr->min,e_ptr->max)) return (PARSE_ERROR_GENERIC);

		if ((e_ptr->min > 99) || (e_ptr->max > 99))
			return (PARSE_ERROR_INVALID_ITEM_NUMBER);

		/* Next item */
		cur_equip++;

		/* Limit number of starting items */
		if (cur_equip > MAX_START_ITEMS)
			return (PARSE_ERROR_GENERIC);
	}

	/* Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
			if (0 != grab_one_class_flag(pc_ptr, s))
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
		if (!parse_integer_like(buf+2,h_ptr->chart,h_ptr->next,h_ptr->roll,h_ptr->bonus)) return (PARSE_ERROR_GENERIC);
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
		if (!s) return (PARSE_ERROR_MISSING_COLON);

		/* Nuke the colon, advance to the subindex */
		*s++ = '\0';

		/* Get the index */
		i = atoi(buf+2);

		/* Find the colon before the name */
		t = strchr(s, ':');

		/* Verify that colon */
		if (!t) return (PARSE_ERROR_MISSING_COLON);

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
		/* There better be a current ot_ptr */
		if (!ot_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (!parse_integer_like(buf+2,ot_ptr->owner_race,ot_ptr->max_cost,ot_ptr->inflate)) return (PARSE_ERROR_GENERIC);
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
			if (!s) return (PARSE_ERROR_MISSING_COLON);

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
 * Initialize the "flavor_info" array, by parsing an ascii "template" file
 */
errr parse_flavor_info(char *buf, header *head)
{
	int i;
	
	/* Current entry */
	static flavor_type *flavor_ptr;


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
		flavor_ptr = (flavor_type*)head->info_ptr + i;

		/* Save the tval */
		flavor_ptr->obj_id.tval = (byte)tval;

		/* Save the sval */
		if (result == 2)
		{
			/* Megahack - unknown sval */
			flavor_ptr->obj_id.sval = SV_UNKNOWN;
		}
		else
			flavor_ptr->obj_id.sval = (byte)sval;
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
		flavor_ptr->d_attr = d_attr;
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
		if (!add_text(&flavor_ptr->_text, head, buf + 2))
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


#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */
