/* File: init1.c */

/* Purpose: Initialization (part 1) -BEN- */

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


/*** Helper arrays for parsing ascii template files ***/

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
	"ANIM_DEAD", /* ToDo: Implement ANIM_DEAD */
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
	"XXX7X7",
	"XXX7X8",
	"XXX7X9",
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
	"NOT_FOREST1",
	"NOT_FOREST2",
	"NOT_MOUNT1",
	"NOT_MOUNT1",
	"NOT_WASTE1",
	"NOT_WASTE2",
	"NOT_SWAMP1",
	"NOT_SWAMP2",
	"WILD_SHORE",
	"WILD_OCEAN",
	"WILD_GRASS",
	"WILD_TOWN",
	"WILD_DUNGEON_01",
	"WILD_DUNGEON_02",
	"WILD_DUNGEON_03",
	"WILD_DUNGEON_04",
	"WILD_DUNGEON_05",
	"WILD_DUNGEON_06",
	"WILD_DUNGEON_07",
	"WILD_DUNGEON_08",
	"WILD_DUNGEON_09",
	"WILD_DUNGEON_10",
	"WILD_DUNGEON_11",
	"WILD_DUNGEON_12",
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
	"XXX2",
	"STEALTH",
	"SEARCH",
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
	"XXX2",
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
	"XXX7",
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
	"XXX12",
	"XXX13",
	"XXX14"
};



/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
static int color_char_to_attr(char c)
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



/*** Initialize from ascii template files ***/


/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr init_v_info_txt(FILE *fp, char *buf, bool start)
{
	int i;
	char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	vault_type *v_ptr = NULL;

	if (start)
	{
		/* Just before the first record */
		error_idx = -1;

		/* Just before the first line */
		error_line = -1;

		/* Prepare the "fake" stuff */
		v_head->name_size = 0;
		v_head->text_size = 0;
	}

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;
		if ((buf[0] == 'Q') || (buf[0] == 'T')) continue;

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
			if (v_head->name_size + strlen(s) + 8 > fake_name_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (v_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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
	if (!start)
	{
		++v_head->name_size;
		++v_head->text_size;
	}


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}



/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s;

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
			if (f_head->name_size + strlen(s) + 8 > fake_name_size) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!f_ptr->name) f_ptr->name = ++f_head->name_size;

			/* Append chars to the name */
			strcpy(f_name + f_head->name_size, s);

			/* Advance the index */
			f_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (f_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the text index */
			if (!f_ptr->text) f_ptr->text = ++f_head->text_size;

			/* Append chars to the name */
			strcpy(f_text + f_head->text_size, s);

			/* Advance the index */
			f_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

#endif

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

			/* Save the values */
			f_ptr->d_attr = tmp;
			f_ptr->d_char = buf[2];

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
			if (k_head->name_size + strlen(s) + 8 > fake_name_size)
			{
				fake_name_size += 1000;

				/* Reallocate the extra memory */
				k_info = (object_kind*)realloc(k_name, fake_name_size);
			}

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
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (k_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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
			k_ptr->weight = wgt;
			k_ptr->cost = cost;
			k_ptr->level = level;
			
			/* Note that extra is ignored here. */

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
				/* Paranoia */
				if (i >= 4) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
				
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
			k_ptr->to_a = ta;

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


		/* Hack -- Process 'V' for "Version" */
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
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= a_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			a_ptr = &a_info[i];

			/* Hack -- Verify space */
			if (a_head->name_size + strlen(s) + 8 > fake_name_size) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!a_ptr->name) a_ptr->name = ++a_head->name_size;

			/* Append chars to the name */
			strcpy(a_name + a_head->name_size, s);

			/* Advance the index */
			a_head->name_size += strlen(s);

			/* Ignore everything */
			a_ptr->flags3 |= (TR3_IGNORE_ACID);
			a_ptr->flags3 |= (TR3_IGNORE_ELEC);
			a_ptr->flags3 |= (TR3_IGNORE_FIRE);
			a_ptr->flags3 |= (TR3_IGNORE_COLD);

			/* Next... */
			continue;
		}

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (a_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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

		/* Hack -- Process 'P' for "power" and such */
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
			a_ptr->to_a = ta;

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
				if (0 != grab_one_artifact_flag(a_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

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
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= e_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			e_ptr = &e_info[i];

			/* Hack -- Verify space */
			if (e_head->name_size + strlen(s) + 8 > fake_name_size) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Advance and Save the name index */
			if (!e_ptr->name) e_ptr->name = ++e_head->name_size;

			/* Append chars to the name */
			strcpy(e_name + e_head->name_size, s);

			/* Advance the index */
			e_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (e_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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

		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
			int slot, rating;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
				&slot, &rating)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			e_ptr->slot = slot;
			e_ptr->rating = rating;

			/* Next... */
			continue;
		}

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

	/* Scan flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
			r_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags7[i]))
		{
			r_ptr->flags7 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags8 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags8[i]))
		{
			r_ptr->flags8 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags9 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
			r_ptr->flags9 |= (1L << i);
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
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= r_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			r_ptr = &r_info[i];

			/* Hack -- Verify space */
			if (r_head->name_size + strlen(s) + 8 > fake_name_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (r_head->text_size + strlen(s) + 8 > fake_text_size) return (PARSE_ERROR_OUT_OF_MEMORY);

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
			r_ptr->d_char = sym;
			r_ptr->d_attr = tmp;

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
		
		/* Process 'O' for "Object theme" (one line only) */
		if (buf[0] == 'O')
		{
			int treasure, combat, magic;

			/* Scan for the other values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
				&treasure, &combat, &magic)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			r_ptr->obj_drop.treasure = (byte) treasure;
			r_ptr->obj_drop.combat = (byte) combat;
			r_ptr->obj_drop.magic = (byte) magic;
			
			/* 
			 * Since monsters do not drop junk,
			 * this value is defined in terms of the others
			 */
			r_ptr->obj_drop.tools = 100 - (treasure + combat + magic);

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++r_head->name_size;
	++r_head->text_size;

	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);

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
	msg_format("Unknown wilderness flag '%s'.", what);

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
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Check to see if there is room in array */
			if (i > max_w_block - 1) return (PARSE_ERROR_OUT_OF_MEMORY);

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

			if (1 != sscanf(buf+2, "%d", &tmp)) return (PARSE_ERROR_GENERIC);

			/* Paranoia */
			if ((tmp >= max_f_idx) || (tmp < 0)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			w_ptr->feat = (byte) tmp;

			/* Next... */
			continue;
		}

		/* Process 'W' for "Wilderness Info" (one line only) */
		if (buf[0] == 'W')
		{
			int hgtmin, hgtmax, popmin, popmax, lawmin, lawmax;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
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
			if (2 != sscanf(buf+2, "%d:%d",
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
				if (0 != grab_one_wild_flag(w_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

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
			if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
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
				init_choice_tree(&bound, i);
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
	msg_format("Unknown field info-flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one field action flag from a textual string
 */
static errr grab_one_action_flag(field_thaum *t_ptr, char *what)
{
	int i, location;

	char *t;

	t = what;

	/* Split the string into two bits using the comma seperator */
	while (!((*t == '\0') || (*t == ',')))
	{
		/* Increment the pointer */
		t++;
	}

	/* t should point to a comma, or to a NULL */
	if (!(*t))
	{
		/* The string had no comma */
		return (PARSE_ERROR_GENERIC);
	}

	/*
	 * Hack - convert the comma to a zero
	 * so 'what' is just the location string.
	 */
	*t = 0;

	/* Move over one character to point to the function name */
	t++;

	/* Get location */
	location = atoi(what);

	/* Bounds checking */
	if ((location < 0) || (location >= FIELD_ACTION_MAX) || !(*t))
	{
		/* error */
		return (PARSE_ERROR_GENERIC);
	}

	/* Check flags */
	for (i = 0; f_action[i].func; i++)
	{
		if (streq(t, f_action[i].func))
		{
			t_ptr->action[location] = f_action[i].action;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown field info-flag '%s'.", t);

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
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Check to see if there is room in array */
			if (i > max_t_idx - 1) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Save the index */
			error_idx = i;

			/* point to new position in array */
			t_ptr = &t_info[i];


			/* Find the colon before the name */
			s = strchr(buf+2, ':');

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

			if (!t) return (PARSE_ERROR_OUT_OF_MEMORY); /* Out of memory */

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
			if (3 != sscanf(buf+2, "%d:%d:%d",
				&priority, &type, &counter)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			t_ptr->priority = (byte) priority;
			t_ptr->type = (byte) type;
			t_ptr->count_init = (s16b) counter;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info Flags" (multiple lines) */
		if (buf[0] == 'I')
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
				if (0 != grab_one_info_flag(t_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

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
			if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
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


		/* Process 'F' for "Field action Functions" (multiple lines) */
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
				if (0 != grab_one_action_flag(t_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
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








#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */


/* Random dungeon grid effects */
#define RANDOM_NONE         0x00
#define RANDOM_FEATURE      0x01
#define RANDOM_MONSTER      0x02
#define RANDOM_OBJECT       0x04
#define RANDOM_EGO          0x08
#define RANDOM_ARTIFACT     0x10
#define RANDOM_TRAP         0x20


typedef struct dungeon_grid dungeon_grid;

struct dungeon_grid
{
	int		feature;		/* Terrain feature */
	int		monster;		/* Monster */
	int		object;			/* Object */
	int		ego;			/* Ego-Item */
	int		artifact;		/* Artifact */
	int		trap;			/* Trap */
	int		cave_info;		/* Flags for CAVE_MARK, CAVE_GLOW, CAVE_ICKY, CAVE_ROOM */
	int		special;		/* Reserved for special terrain info */
	int		random;			/* Number of the random effect */
};


/*
 * Parse a sub-file of the "extra info"
 */
static errr process_dungeon_file_aux(char *buf, int init_flags)
{
	char *zz[33];

	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);

	/* Require "?:*" format */
	if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


	/* Process "%:<fname>" */
	if (buf[0] == '%')
	{
		/* Attempt to Process the given file */
		return (process_dungeon_file(buf + 2, init_flags));
	}

	/* Process "Q:<number>:<command>:... -- quest info */
	else if (buf[0] == 'Q')
	{
		int num = tokenize(buf + 2, 33, zz, 0);
		quest_type *q_ptr;

		/* Have we enough parameters? */
		if (num < 3) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

		/* Get the quest */
		q_ptr = &(quest[atoi(zz[0])]);

		/* Process "Q:<q_index>:Q:<type>:<num_mon>:<cur_num>:<max_num>:<level>:<r_idx>:<k_idx>:<flags>" -- quest info */
		if (zz[1][0] == 'Q')
		{
			if (init_flags & INIT_ASSIGN)
			{
				monster_race *r_ptr;
				artifact_type *a_ptr;

				if (num < 9) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

				q_ptr->type    = atoi(zz[2]);
				q_ptr->num_mon = atoi(zz[3]);
				q_ptr->cur_num = atoi(zz[4]);
				q_ptr->max_num = atoi(zz[5]);
				q_ptr->level   = atoi(zz[6]);
				q_ptr->r_idx   = atoi(zz[7]);
				q_ptr->k_idx   = atoi(zz[8]);

				if (num > 9)
					q_ptr->flags = atoi(zz[9]);

				r_ptr = &r_info[q_ptr->r_idx];
				if (r_ptr->flags1 & RF1_UNIQUE)
					r_ptr->flags1 |= RF1_QUESTOR;

				a_ptr = &a_info[q_ptr->k_idx];
				a_ptr->flags3 |= TR3_QUESTITEM;
			}
			return (0);
		}

		/* Process "Q:<q_index>:N:<name>" -- quest name */
		else if (zz[1][0] == 'N')
		{
			if (init_flags & (INIT_ASSIGN | INIT_SHOW_TEXT))
			{
				strcpy(q_ptr->name, zz[2]);
			}

			return (0);
		}

		/* Process "Q:<q_index>:T:<text>" -- quest description line */
		else if (zz[1][0] == 'T')
		{
			if (init_flags & INIT_SHOW_TEXT)
			{
				strcpy(quest_text[quest_text_line], zz[2]);
				quest_text_line++;
			}

			return (0);
		}
	}

	/* Process "M:<type>:<maximum>" -- set maximum values */
	else if (buf[0] == 'M')
	{
		if (tokenize(buf+2, 2, zz, 0) == 2)
		{


			/* Maximum quests */
			if (zz[0][0] == 'Q')
			{
				max_quests = atoi(zz[1]);
			}

			/* Maximum r_idx */
			else if (zz[0][0] == 'R')
			{
				max_r_idx = atoi(zz[1]);
			}

			/* Maximum k_idx */
			else if (zz[0][0] == 'K')
			{
				max_k_idx = atoi(zz[1]);
			}

			/* Maximum v_idx */
			else if (zz[0][0] == 'V')
			{
				max_v_idx = atoi(zz[1]);
			}

			/* Maximum f_idx */
			else if (zz[0][0] == 'F')
			{
				max_f_idx = atoi(zz[1]);
			}

			/* Maximum a_idx */
			else if (zz[0][0] == 'A')
			{
				max_a_idx = atoi(zz[1]);
			}

			/* Maximum e_idx */
			else if (zz[0][0] == 'E')
			{
				max_e_idx = atoi(zz[1]);
			}

			/* Maximum o_idx */
			else if (zz[0][0] == 'O')
			{
				max_o_idx = atoi(zz[1]);
			}

			/* Maximum m_idx */
			else if (zz[0][0] == 'M')
			{
				max_m_idx = atoi(zz[1]);
			}

			/* Maximum t_idx */
			else if (zz[0][0] == 'T')
			{
				max_t_idx = atoi(zz[1]);
			}

			/* Maximum fld_idx */
			else if (zz[0][0] == 'D')
			{
				max_fld_idx = atoi(zz[1]);
			}

			/* Wilderness data */
			else if (zz[0][0] == 'W')
			{
				/* Maximum wild_size */
				if (zz[0][1] == 'S')
					max_wild_size = atoi(zz[1]);

				/* Maximum wild d_tree nodes */
				if (zz[0][1] == 'N')
					max_w_node = atoi(zz[1]);

				/* Maximum wild gen types */
				if (zz[0][1] == 'T')
					max_w_block = atoi(zz[1]);

				/* Maximum towns */
				if (zz[0][1] == 'P')
					max_towns = atoi(zz[1]);
			}

			return (0);
		}
	}


	/* Failure */
	return (PARSE_ERROR_GENERIC);
}


static char tmp[8];
static cptr variant = "ZANGBAND";


/*
 * Helper function for "process_dungeon_file()"
 */
static cptr process_dungeon_file_expr(char **sp, char *fp)
{
	cptr v;

	char *b;
	char *s;

	char b1 = '[';
	char b2 = ']';

	char f = ' ';

	/* Initial */
	s = (*sp);

	/* Skip spaces */
	while (isspace(*s)) s++;

	/* Save start */
	b = s;

	/* Default */
	v = "?o?o?";

	/* Analyze */
	if (*s == b1)
	{
		const char *p;
		const char *t;

		/* Skip b1 */
		s++;

		/* First */
		t = process_dungeon_file_expr(&s, &f);

		/* Oops */
		if (!*t)
		{
			/* Nothing */
		}

		/* Function: IOR */
		else if (streq(t, "IOR"))
		{
			v = "0";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "1";
			}
		}

		/* Function: AND */
		else if (streq(t, "AND"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && streq(t, "0")) v = "0";
			}
		}

		/* Function: NOT */
		else if (streq(t, "NOT"))
		{
			v = "1";
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(t, "0")) v = "0";
			}
		}

		/* Function: EQU */
		else if (streq(t, "EQU"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && !streq(p, t)) v = "0";
			}
		}

		/* Function: LEQ */
		else if (streq(t, "LEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && (strcmp(p, t) > 0)) v = "0";
			}
		}

		/* Function: GEQ */
		else if (streq(t, "GEQ"))
		{
			v = "1";
			if (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
			while (*s && (f != b2))
			{
				p = t;
				t = process_dungeon_file_expr(&s, &f);
				if (*t && (strcmp(p, t) < 0)) v = "0";
			}
		}

		/* Oops */
		else
		{
			while (*s && (f != b2))
			{
				t = process_dungeon_file_expr(&s, &f);
			}
		}

		/* Verify ending */
		if (f != b2) v = "?x?x?";

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';
	}

	/* Other */
	else
	{
		/* Accept all printables except spaces and brackets */
		while (isprint(*s) && !strchr(" []", *s)) ++s;

		/* Extract final and Terminate */
		if ((f = *s) != '\0') *s++ = '\0';

		/* Variable */
		if (*b == '$')
		{
			/* System */
			if (streq(b+1, "SYS"))
			{
				v = ANGBAND_SYS;
			}

			/* Graphics */
			else if (streq(b+1, "GRAF"))
			{
				v = ANGBAND_GRAF;
			}

			else if (streq(b+1, "MONOCHROME"))
			{
				if (arg_monochrome)
					v = "ON";
				else
					v = "OFF";
			}

			/* Race */
			else if (streq(b+1, "RACE"))
			{
				v = rp_ptr->title;
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = cp_ptr->title;
			}

			/* First realm */
			else if (streq(b+1, "REALM1"))
			{
				v = realm_names[p_ptr->realm1];
			}

			/* Second realm */
			else if (streq(b+1, "REALM2"))
			{
				v = realm_names[p_ptr->realm2];
			}

			/* Player name */
			else if (streq(b+1, "PLAYER"))
			{
				v = player_base;
			}

			/* Town */
			else if (streq(b+1, "TOWN"))
			{
				sprintf(tmp, "%d", p_ptr->town_num);
				v = tmp;
			}

			/* Level */
			else if (streq(b+1, "LEVEL"))
			{
				sprintf(tmp, "%d", p_ptr->lev);
				v = tmp;
			}

			/* Current quest number */
			else if (streq(b+1, "QUEST_NUMBER"))
			{
				sprintf(tmp, "%d", p_ptr->inside_quest);
				v = tmp;
			}

			/* Number of last quest */
			else if (streq(b+1, "LEAVING_QUEST"))
			{
				sprintf(tmp, "%d", leaving_quest);
				v = tmp;
			}

			/* Quest status */
			else if (prefix(b+1, "QUEST"))
			{
				/* "QUEST" uses a special parameter to determine the number of the quest */
				sprintf(tmp, "%d", quest[atoi(b+6)].status);
				v = tmp;
			}

			/* Variant name */
			else if (streq(b+1, "VARIANT"))
			{
				v = variant;
			}

			/* Wilderness */
			else if (streq(b+1, "WILDERNESS"))
			{
				if (vanilla_town)
					sprintf(tmp, "NONE");
				else
					sprintf(tmp, "NORMAL");
				v = tmp;
			}
		}

		/* Constant */
		else
		{
			v = b;
		}
	}

	/* Save */
	(*fp) = f;

	/* Save */
	(*sp) = s;

	/* Result */
	return (v);
}


errr process_dungeon_file(cptr name, int init_flags)
{
	FILE *fp;

	char buf[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Count lines */
		num++;


		/* Skip "empty" lines */
		if (!buf[0]) continue;

		/* Skip "blank" lines */
		if (isspace(buf[0])) continue;

		/* Skip comments */
		if (buf[0] == '#') continue;


		/* Process "?:<expr>" */
		if ((buf[0] == '?') && (buf[1] == ':'))
		{
			char f;
			cptr v;
			char *s;

			/* Start */
			s = buf + 2;

			/* Parse the expr */
			v = process_dungeon_file_expr(&s, &f);

			/* Set flag */
			bypass = (streq(v, "0") ? TRUE : FALSE);

			/* Continue */
			continue;
		}

		/* Apply conditionals */
		if (bypass) continue;


		/* Process "%:<file>" */
		if (buf[0] == '%')
		{
			/* Process that file if allowed */
			(void)process_dungeon_file(buf + 2, init_flags);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_dungeon_file_aux(buf, init_flags);

		/* Oops */
		if (err) break;
	}

	/* Errors */
	if (err)
	{
		cptr oops;

		/* Error string */
		oops = (((err > 0) && (err < PARSE_ERROR_MAX)) ? err_str[err] : "unknown");

		/* Oops */
		msg_format("Error %d (%s) at line %d of '%s'.", err, oops, num, name);
		msg_format("Parsing '%s'.", buf);
		msg_print(NULL);
	}


	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}

