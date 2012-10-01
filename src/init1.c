/* File: init1.c */

/* Initialization of monsters, objects, artifacts, ego-items, and terrain.
 * All code to handle *_info.txt files.  Lists all monster and object flags
 * that *_info.txt files contain, translation of colors.
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


/*
 * Hack -- error tracking
 */
extern s16b error_idx;
extern s16b error_line;


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
	"REQUEST",
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
	"DROP_40",
	"DROP_60",
	"DROP_ONE",
	"DROP_TWO",
	"DROP_PLUS_5",
	"DROP_PLUS_10",
	"DROP_GOOD",
	"DROP_GREAT",
	"DROP_CHEST",
	"DROP_CHOSEN"
};

/*
 * Monster race flags
 */
static cptr r_info_flags2[] =
{
	"STUPID",
	"SMART",
	"SPEAKING",	/* From Zangband.  Was XXX1X2 */
	"PLAYER_GHOST", /* Was XXX2X2 */
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"XXX3X2",
	"XXX4X2",
	"X",
	"XXX5X2",
	"XXX7X2",
	"XXX6X2",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"MOVE_BODY",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"FLYING",	/* Was BRAIN_1 */
	"LOW_MANA_RUN",
	"BRAIN_4",
	"POWERFUL",
	"ARCHER",
	"MORGUL_MAGIC",
	"UDUN_MAGIC",
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
	"XXX1X3",
	"XXX2X3",
	"XXX3X3",
	"XXX4X3",
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
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
	"BRTH_FORCE",
	"BRTH_NEXUS",
	"BRTH_NETHR",
	"BRTH_CHAOS",
	"BRTH_DISEN",
	"BRTH_TIME",
	"XXX42",
	"XXX43",
	"XXX44",
	"XXX45",
	"XXX45"
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
	"BALL_STORM",
	"BALL_NETHR",
	"BALL_CHAOS",
	"BALL_MANA",
	"XXX51",
	"XXX52",
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
	"DISPEL",
	"XXX5",
	"MIND_BLAST",
	"BRAIN_SMASH",
	"WOUND",
	"XXX7",
	"XXX8",
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
	"XXX5",
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_ANIMAL",
	"XXX6",
	"XXX7",
	"S_THIEF",
	"S_BERTBILLTOM",
	"XXX8",
	"XXX9",
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
 * Special Player Flags
 */
static cptr player_flags_sp[] =
{
	"ARMOR_MAST","SHIELD_MAST","ARMOR_PROFICIENCY","EVASION","MAGIC_RESIST","PHASEWALK","UNLIGHT","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","ARMSMAN","FAST_ATTACK","MARKSMAN","PIERCE_SHOT",
	"MIGHTY_THROW","POWER_STRIKE","MARTIAL_ARTS","MANA_BURN","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"BEGUILE","ENHANCE_MAGIC","FAST_CAST","POWER_SIPHON","HEIGHTEN_MAGIC","SOUL_SIPHON","HARMONY","",
	"CHANNELING","ATTUNEMENT","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","ATHLETICS","CLARITY","XXX","FURY",
	"MEDITATION","REGENERATION","EXTRA_TRAP","HOLY_LIGHT","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"SWORD_SKILL","POLEARM_SKILL","HAFTED_SKILL","SLING_SKILL","BOW_SKILL","XBOW_SKILL","XXX","XXX",
	"XXX","SWORD_UNSKILL","POLEARM_UNSKILL","HAFTED_UNSKILL","SLING_UNSKILL","BOW_UNSKILL","XBOW_UNSKILL","XXX",
	"XXX","XXX","HARDY","HUNGRY","DIVINE","SHADOW","WOODEN","BEARSKIN",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"BOW_SPEED_GOOD","BOW_SPEED_GREAT","SLING_SPEED_GOOD","SLING_SPEED_GREAT","XBOW_SPEED_GOOD","XBOW_SPEED_GREAT","XXX","XXX",
	"XXX","ASSASSINATE","STRONG_SHOOT","BACKSTAB","SPREAD_ATTACKS","STRONG_BASHES","UNARMED_COMBAT","BLESS_WEAPON",
	"XXX","CHARM","DEVICE_EXPERT","STRONG_MAGIC","BEAM","XXX","LORE","HOLY",
	"RELENTLESS","PROBE","EVIL","STEAL","PSEUDO_ID_HEAVY","TRAP","WOODSMAN","XTRA_SPECIALTY",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","XXX",
	"XXX","XXX","XXX","XXX","XXX","XXX","XXX","NO_SPECIALTY"
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
	"MAGIC_MASTERY",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"MIGHT2",
	"SHOTS",
	"MIGHT1",
	"SLAY_ANIMAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"SLAY_KILL",
	"THROWING",
	"PERFECT_BALANCE",
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
	"XXX3",
	"XXX4",
	"XXX5",
	"RES_BASE_MINOR",
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
	"XXX1",
	"XXX2",
	"XXX3",
	"EASY_ACT",
	"IMPACT",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD",
	"TWO_HANDED_REQ",
	"TWO_HANDED_DES",
	"BLESSED",
	"ACTIVATE",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"XXX7",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};


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



/*** Initialize from ascii template files ***/


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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (4);

			/* Verify information */
			if (i >= v_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			v_ptr = &v_info[i];

			/* Hack -- Verify space */
			if (v_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

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
		if (!v_ptr) return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (v_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

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
			int typ, rat, hgt, wid, min_lev, max_lev;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
				&typ, &rat, &hgt, &wid, &min_lev, &max_lev)) return (1);

			/* Correct a value. */
			if (max_lev == 0) max_lev = MAX_DEPTH;

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;
			v_ptr->hgt = hgt;
			v_ptr->wid = wid;
			v_ptr->min_lev = min_lev;
			v_ptr->max_lev = max_lev;

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++v_head->name_size;
	++v_head->text_size;


	/* No version yet */
	if (!okay) return (2);


	/* Success */
	return (0);
}


/*
 * Initialize the "t_info" array, by parsing an ascii "template" file.
 * Load only the selected themed level into memory (this allows an arbi-
 * trarily large t_info.txt file). -LM-
 * Otherwise, code is essentially that of "init_v_info_txt".
 */
errr init_t_info_txt(FILE *fp, char *buf, byte chosen_level)
{
	int i;

	/* Assume we cannot start collecting data yet. */
	bool can_use = FALSE;

	char *s;

	/* Current entry */
	vault_type *t_ptr = NULL;

#ifndef NO_THEMED_LEVELS /* Themed levels and old machines don't mix. */

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
		if (buf[1] != ':') return (1);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* If correct themed level found, start processing information. */
			if (i == chosen_level) can_use = TRUE;
			else
			{
				can_use = FALSE;
				continue;
			}

			/* Verify information */
			if (i <= error_idx) return (4);

			/* Verify information */
			if (i >= t_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			t_ptr = &t_info[i];

			/* Hack -- Verify space */
			if (t_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

			/* Advance and Save the name index */
			if (!t_ptr->name) t_ptr->name = ++t_head->name_size;

			/* Append chars to the name */
			strcpy(t_name + t_head->name_size, s);

			/* Advance the index */
			t_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current t_ptr */
		if ((can_use) && (!t_ptr)) return (3);


		/* Process 'M' for special themed level feeling. */
		if (buf[0] == 'M')
		{
			/* Accept only correct themed level information. */
			if (!can_use) continue;

			/* Acquire the text */
			s = buf+2;

			/* Copy the message */
			strcpy(themed_feeling, s);

			/* Next... */
			continue;
		}


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Accept only correct themed level information. */
			if (!can_use) continue;

			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (t_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			if (!t_ptr->text) t_ptr->text = ++t_head->text_size;

			/* Append chars to the name */
			strcpy(t_text + t_head->text_size, s);

			/* Advance the index */
			t_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}

	/* Eventually, the correct themed level must be found. */
	if (!t_ptr) return (3);


	/* Complete the "name" and "text" sizes */
	++t_head->name_size;
	++t_head->text_size;

#endif

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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (4);

			/* Verify information */
			if (i >= f_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			f_ptr = &f_info[i];

			/* Hack -- Verify space */
			if (f_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

			/* Advance and Save the name index */
			if (!f_ptr->name) f_ptr->name = ++f_head->name_size;

			/* Append chars to the name */
			strcpy(f_name + f_head->name_size, s);

			/* Advance the index */
			f_head->name_size += strlen(s);

			/* Default "mimic" */
			f_ptr->mimic = i;

			/* Next... */
			continue;
		}

		/* There better be a current f_ptr */
		if (!f_ptr) return (3);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (f_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

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


		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			int tmp;

			/* Paranoia */
			if (!buf[2]) return (1);
			if (!buf[3]) return (1);
			if (!buf[4]) return (1);

			/* Extract the attr */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0) return (1);

			/* Save the values */
			f_ptr->d_attr = tmp;
			f_ptr->d_char = buf[2];

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++f_head->name_size;
	++f_head->text_size;


	/* No version yet */
	if (!okay) return (2);


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
	return (1);
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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (4);

			/* Verify information */
			if (i >= k_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			k_ptr = &k_info[i];

			/* Hack -- Verify space */
			if (k_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

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
		if (!k_ptr) return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (k_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			if (!k_ptr->text) k_ptr->text = ++k_head->text_size;

			/* Append chars to the name */
			strcpy(k_text + k_head->text_size, s);

			/* Advance the index */
			k_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Process 'G' for "Graphics" (one line only) */
		if (buf[0] == 'G')
		{
			char sym;
			int tmp;

			/* Paranoia */
			if (!buf[2]) return (1);
			if (!buf[3]) return (1);
			if (!buf[4]) return (1);

			/* Extract the char */
			sym = buf[2];

			/* Extract the attr */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0) return (1);

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
					&tval, &sval, &pval)) return (1);

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
					&level, &extra, &wgt, &cost)) return (1);

			/* Save the values */
			k_ptr->level = level;
			k_ptr->extra = extra;
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

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
					&ac, &hd1, &hd2, &th, &td, &ta)) return (1);

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
				if (0 != grab_one_kind_flag(k_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++k_head->name_size;
	++k_head->text_size;


	/* No version yet */
	if (!okay) return (2);


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
	return (1);
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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
			if (i >= a_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			a_ptr = &a_info[i];

			/* Hack -- Verify space */
			if (a_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

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
		if (!a_ptr) return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (a_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			if (!a_ptr->text) a_ptr->text = ++a_head->text_size;

			/* Append chars to the name */
			strcpy(a_text + a_head->text_size, s);

			/* Advance the index */
			a_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int tval, sval, pval;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&tval, &sval, &pval)) return (1);

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
					&level, &rarity, &wgt, &cost)) return (1);

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
			int ac, hd1, hd2, th, td, ta, act;

			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d",
					&ac, &hd1, &hd2, &th, &td, &ta, &act)) return (1);

			a_ptr->ac = ac;
			a_ptr->dd = hd1;
			a_ptr->ds = hd2;
			a_ptr->to_h = th;
			a_ptr->to_d = td;
			a_ptr->to_a =  ta;
			a_ptr->activation = act;

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
				if (0 != grab_one_artifact_flag(a_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++a_head->name_size;
	++a_head->text_size;


	/* No version yet */
	if (!okay) return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag for a set item from a textual string
 */
static errr grab_one_set_element_flag(set_element *selement_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			selement_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			selement_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			selement_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown set element flag '%s'.", what);

	/* Error */
	return (1);
}




/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr init_s_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	set_type *set_ptr = NULL;
	set_element *selement_ptr = NULL;

	/* No item in current set */
	int item_number = -1;

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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
			if (i >= s_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			set_ptr = &s_info[i];

			/* Hack -- Verify space */
			if (s_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

			/* Advance and Save the name index */
			if (!set_ptr->name) set_ptr->name = ++s_head->name_size;

			/* Append chars to the name */
			strcpy(s_name + s_head->name_size, s);

			/* Advance the index */
			s_head->name_size += strlen(s);

			/* Currently no items in this set */
			item_number = -1;

			/* No current item */
			selement_ptr = NULL;

			/* Next... */
			continue;
		}

		/* There better be a current set_ptr */
		if (!set_ptr) return (3);

		/* Process 'C' for "Count" */
		if (buf[0] == 'C')
		{
			int number;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
					&number)) return (1);

			/* Save the values */
			set_ptr->no_of_items = number;

			/* Next... */
			continue;
		}

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (s_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			if (!set_ptr->text) set_ptr->text = ++s_head->text_size;

			/* Append chars to the name */
			strcpy(s_text + s_head->text_size, s);

			/* Advance the index */
			s_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'P' for "Power" (up to 6) */
		if (buf[0] == 'P')
		{
			int a_idx, pval;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
					&a_idx, &pval)) return (1);

			/* We are on the next set item */
			item_number++;

			/* Max of 6 items per set */
			if (item_number > 5) return(3);

			/* Current set item */
			selement_ptr = &set_ptr->set_items[item_number];

			/* Save the values */
			selement_ptr->a_idx = a_idx;
			selement_ptr->pval = pval;

			/* Next... */
			continue;
		}

		/* There better be a current selement_ptr */
		if (!selement_ptr) return (3);

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
				if (0 != grab_one_set_element_flag(selement_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++a_head->name_size;
	++a_head->text_size;


	/* No version yet */
	if (!okay) return (2);


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
	return (1);
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

	static int cur_t = 0;


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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
			if (i >= e_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			e_ptr = &e_info[i];

			/* Hack -- Verify space */
			if (e_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

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
		if (!e_ptr) return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (e_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			if (!e_ptr->text) e_ptr->text = ++e_head->text_size;

			/* Append chars to the name */
			strcpy(e_text + e_head->text_size, s);

			/* Advance the index */
			e_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
			int xtra, rating;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
					&rating, &xtra)) return (1);

			/* Save the values */
			e_ptr->xtra = xtra;
			e_ptr->rating = rating;

			/* Next... */
			continue;
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

			/* Increase counter for 'possible tval' index */
			cur_t++;

			/* Allow only a limited number of T: lines */
			if (cur_t > EGO_TVALS_MAX) return (PARSE_ERROR_GENERIC);

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
					&level, &rarity, &pad2, &cost)) return (1);

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
				&th, &td, &ta, &pv)) return (1);

			/* Process various values to allow penalties. */

			if ((th < 0) && (th > -128)) e_ptr->max_to_h = 128 + ABS(th);
			else if (th < 129) e_ptr->max_to_h = th;
			else e_ptr->max_to_h = 0;

			if ((td < 0) && (td > -128)) e_ptr->max_to_d = 128 + ABS(td);
			else if (td < 129) e_ptr->max_to_d = td;
			else e_ptr->max_to_d = 0;

			if ((ta < 0) && (ta > -128)) e_ptr->max_to_a = 128 + ABS(ta);
			else if (ta < 129) e_ptr->max_to_a = ta;
			else e_ptr->max_to_a = 0;

			if ((pv < 0) && (pv > -128)) e_ptr->max_pval = 128 + ABS(pv);
			else if (pv < 129) e_ptr->max_pval = pv;
			else e_ptr->max_pval = 0;


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
				if (0 != grab_one_ego_item_flag(e_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++e_head->name_size;
	++e_head->text_size;


	/* No version yet */
	if (!okay) return (2);


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
	return (1);
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

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags7[i]))
		{
			r_ptr->flags7 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Initialize the "r_info" array, by parsing an ascii "template" file.
 * This function can also reload a specific monster, if given a racial index.
 * This is used to reinitialize player ghosts. -LM-
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
		if (buf[1] != ':') return (1);


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
				return (2);
			}

			/* Okay to proceed */
			okay = TRUE;

			/* Continue */
			continue;
		}

		/* No version yet */
		if (!okay) return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
			if (i >= r_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			r_ptr = &r_info[i];

			/* Hack -- Verify space */
			if (r_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) return (7);

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
		if (!r_ptr) return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (r_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

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
			if (!buf[2]) return (1);
			if (!buf[3]) return (1);
			if (!buf[4]) return (1);

			/* Extract the char */
			sym = buf[2];

			/* Extract the attr */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0) return (1);

			/* Save the values */
			r_ptr->d_attr = tmp;
			r_ptr->d_char = sym;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int spd, hp1, hp2, aaf, ac, slp;

			/* Scan for the other values */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
					&spd, &hp1, &hp2, &aaf, &ac, &slp)) return (1);

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
			int lev, rar, mana;
			long exp;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
					&lev, &rar, &mana, &exp)) return (1);

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
			r_ptr->mana = mana;
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
			if (i == 4) return (1);

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
			if (!r_info_blow_method[n1]) return (1);

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
			if (!r_info_blow_effect[n2]) return (1);

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
				if (0 != grab_one_basic_flag(r_ptr, s)) return (5);

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
					r_ptr->freq_ranged = 100 / i;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Read spell power. */
				if (1 == sscanf(s, "POW_%d", &i))
				{
					/* Save spell power. */
					r_ptr->spell_power = i;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
				if (0 != grab_one_spell_flag(r_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++r_head->name_size;
	++r_head->text_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}

/*
 * Grab one flag in a player_race from a textual string
 */
static errr grab_one_racial_flag(player_race *rp_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			rp_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			rp_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			rp_ptr->flags3 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown player flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Grab one special flag in a player_race from a textual string
 */
static errr grab_one_special_racial_flag(player_race *rp_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = SP_RACIAL_START; i <= SP_RACIAL_END; i++)
	{
		if (streq(what, player_flags_sp[i]))
		{
			rp_ptr->flags_special |= (1L << (i - SP_RACIAL_START));
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown player special flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "rp_info" array, by parsing an ascii "template" file
 */
errr init_rp_info_txt(FILE *fp, char *buf)
{
	int i, j;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	player_race *rp_ptr = NULL;


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
			    (v1 != rp_head->v_major) ||
			    (v2 != rp_head->v_minor) ||
			    (v3 != rp_head->v_patch))
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
			if (i >= rp_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			rp_ptr = &rp_info[i];

			/* Hack -- Verify space */
			if (rp_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE) {
				return (PARSE_ERROR_OUT_OF_MEMORY);
			}

			/* Advance and Save the name index */
			if (!rp_ptr->name) rp_ptr->name = ++rp_head->name_size;

			/* Append chars to the name */
			strcpy(rp_name + rp_head->name_size, s);

			/* Advance the index */
			rp_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current rp_ptr */
		if (!rp_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


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
				rp_ptr->r_adj[j] = adj;

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
			rp_ptr->r_dis = dis;
			rp_ptr->r_dev = dev;
			rp_ptr->r_sav = sav;
			rp_ptr->r_stl = stl;
			rp_ptr->r_srh = srh;
			rp_ptr->r_fos = fos;
			rp_ptr->r_thn = thn;
			rp_ptr->r_thb = thb;

			/* Next... */
			continue;
		}


		/* Process 'M' for "Racial Skills Extra Modifier" (one line only) */
		if (buf[0] == 'M')
		{
			int xdis, xdev, xsav, xstl, xsrh, xfos, xthn, xthb;

			/* Scan for the values */
			if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
					&xdis, &xdev, &xsav, &xstl,
					&xsrh, &xfos, &xthn, &xthb)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			rp_ptr->rx_dis = xdis;
			rp_ptr->rx_dev = xdev;
			rp_ptr->rx_sav = xsav;
			rp_ptr->rx_stl = xstl;
			rp_ptr->rx_srh = xsrh;
			rp_ptr->rx_fos = xfos;
			rp_ptr->rx_thn = xthn;
			rp_ptr->rx_thb = xthb;

			/* Next... */
			continue;
		}


		/* Process 'X' for "Extra Info" (one line only) */
		if (buf[0] == 'X')
		{
			int mhp, diff, infra;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
					&mhp, &diff, &infra)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			rp_ptr->r_mhp = mhp;
			rp_ptr->difficulty = diff;
			rp_ptr->infra = infra;

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

			rp_ptr->hist = hist;
			rp_ptr->b_age = b_age;
			rp_ptr->m_age = m_age;

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

			rp_ptr->hist = hist;
			rp_ptr->b_age = b_age;
			rp_ptr->m_age = m_age;

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

			rp_ptr->m_b_ht = m_b_ht;
			rp_ptr->m_m_ht = m_m_ht;
			rp_ptr->f_b_ht = f_b_ht;
			rp_ptr->f_m_ht = f_m_ht;

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

			rp_ptr->m_b_wt = m_b_wt;
			rp_ptr->m_m_wt = m_m_wt;
			rp_ptr->f_b_wt = f_b_wt;
			rp_ptr->f_m_wt = f_m_wt;

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
				if (0 != grab_one_racial_flag(rp_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Hack -- Process 'U' for flags */
		if (buf[0] == 'U')
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
				if (0 != grab_one_special_racial_flag(rp_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

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
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while ((*t == ' ') || (*t == '|')) t++;
				}

				/* Hack - Parse this entry */
				rp_ptr->choice |= (1 << atoi(s));

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
	++rp_head->name_size;
	++rp_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}

/*
 * Grab one special flag in a player_class from a textual string
 */
static errr grab_one_special_class_flag(player_class *cp_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = SP_CLASS_START; i <= SP_CLASS_END; i++)
	{
		if (streq(what, player_flags_sp[i]))
		{
			cp_ptr->flags_special |= (1L << (i - SP_CLASS_START));
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown player special flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one special flag in a player_class from a textual string
 */
static errr grab_one_specialty(player_class *cp_ptr, cptr what, int cur_specialty)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < SP_NO_SPECIALTY; i++)
	{
		if (streq(what, player_flags_sp[i]))
		{
			cp_ptr->specialties[cur_specialty] = i;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown player special flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the "cp_info" array, by parsing an ascii "template" file
 */
errr init_cp_info_txt(FILE *fp, char *buf)
{
	int i, j;

	char *s, *t;

	/* Current entry */
	static player_class *cp_ptr = NULL;

	static int cur_title = 0;
	static int cur_equip = 0;
	static int cur_specialty = 0;


	/* Not ready yet */
	bool okay = FALSE;

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
			    (v1 != cp_head->v_major) ||
			    (v2 != cp_head->v_minor) ||
			    (v3 != cp_head->v_patch))
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
			if (i >= cp_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			cp_ptr = &cp_info[i];

			/* Hack -- Verify space */
			if (cp_head->name_size + strlen(s) + 8 > FAKE_NAME_SIZE)
			{
				return (PARSE_ERROR_OUT_OF_MEMORY);
			}

			/* Advance and Save the name index */
			if (!cp_ptr->name) cp_ptr->name = ++cp_head->name_size;

			/* Append chars to the name */
			strcpy(cp_name + cp_head->name_size, s);

			/* Advance the index */
			cp_head->name_size += strlen(s);

			/* No titles and equipment yet */
			cur_title = 0;
			cur_equip = 0;
			cur_specialty = 0;

			/* Next... */
			continue;
		}

		/* There better be a current cp_ptr */
		if (!cp_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
				cp_ptr->c_adj[j] = adj;

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
			cp_ptr->c_dis = dis;
			cp_ptr->c_dev = dev;
			cp_ptr->c_sav = sav;
			cp_ptr->c_stl = stl;
			cp_ptr->c_srh = srh;
			cp_ptr->c_fos = fos;
			cp_ptr->c_thn = thn;
			cp_ptr->c_thb = thb;

			/* Next... */
			continue;
		}

		/* Process 'X' for level 50 "Class Skills" (one line only) */
		if (buf[0] == 'X')
		{
			int dis, dev, sav, stl, srh, fos, thn, thb;

			/* Scan for the values */
			if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
					&dis, &dev, &sav, &stl,
					&srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			cp_ptr->cx_dis = dis;
			cp_ptr->cx_dev = dev;
			cp_ptr->cx_sav = sav;
			cp_ptr->cx_stl = stl;
			cp_ptr->cx_srh = srh;
			cp_ptr->cx_fos = fos;
			cp_ptr->cx_thn = thn;
			cp_ptr->cx_thb = thb;

			/* Next... */
			continue;
		}

		/* Process 'I' other info */
		if (buf[0] == 'I')
		{
			int mhp;
			long sense_base;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%ld",
					&mhp, &sense_base)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			cp_ptr->c_mhp = mhp;
			cp_ptr->sense_base = sense_base;

			/* Next... */
			continue;
		}

		/* Process 'A' weapon info */
		if (buf[0] == 'A')
		{
			int max_1,max_50,penalty,max_penalty,bonus,max_bonus;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
					&max_1, &max_50, &penalty, &max_penalty,
					&bonus, &max_bonus)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			cp_ptr->max_1 = max_1;
			cp_ptr->max_50 = max_50;
			cp_ptr->penalty = penalty;
			cp_ptr->max_penalty = max_penalty;
			cp_ptr->bonus = bonus;
			cp_ptr->max_bonus = max_bonus;

			/* Next... */
			continue;
		}

		/* Process 'E' for "Starting Equipment" */
		if (buf[0] == 'E')
		{
			int tval, sval, min, max;

			start_item *e_ptr;

			/* Access the item */
			e_ptr = &cp_ptr->start_items[cur_equip];

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
					&tval, &sval, &min, &max)) return (PARSE_ERROR_GENERIC);

			if ((min < 0) || (max < 0) || (min > 99) || (max > 99))
				return (PARSE_ERROR_GENERIC);

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

			/* Next... */
			continue;
		}

		/* Hack -- Process 'U' for flags */
		if (buf[0] == 'U')
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
				if (0 != grab_one_special_class_flag(cp_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Process 'L' for Learnable specialties */
		if (buf[0] == 'L')
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
				if (0 != grab_one_specialty(cp_ptr, s, cur_specialty)) return (PARSE_ERROR_INVALID_FLAG);

				/* Start the next entry */
				s = t;

				/* Next title */
				cur_specialty++;

				/* Limit number of titles */
				if (cur_specialty > CLASS_SPECIALTIES)
				{
					return (PARSE_ERROR_TOO_MANY_ARGUMENTS);
				}
			}

			/* Clear unused specialties */
			for (j = cur_specialty; j < CLASS_SPECIALTIES; j++) cp_ptr->specialties[j] = SP_NO_SPECIALTY;

			/* Next... */
			continue;
		}

		/* Process 'T' for "Titles" */
		if (buf[0] == 'T')
		{
			/* Get the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (cp_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE) return (7);

			/* Advance and Save the text index */
			cp_ptr->title[cur_title] = ++cp_head->text_size;

			/* Append chars to the name */
			strcpy(cp_text + cp_head->text_size, s);

			/* Advance the index */
			cp_head->text_size += strlen(s);

			/* Next title */
			cur_title++;

			/* Limit number of titles */
			if (cur_title > PY_MAX_LEVEL / 5)
			{
				return (PARSE_ERROR_TOO_MANY_ARGUMENTS);
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}


	/* Complete the "name" and "text" sizes */
	++cp_head->name_size;
	++cp_head->text_size;


	/* No version yet */
	if (!okay) return (PARSE_ERROR_OBSOLETE_FILE);


	/* Success */
	return (0);
}






/*
 * Initialize the "ch_info" array, by parsing an ascii "template" file
 */
errr init_ch_info_txt(FILE *fp, char *buf)
{
	int i;

	/* Current entry */
	static chest_drops *ch_ptr = NULL;

	static int cur_choices = 0;

	/* Not ready yet */
	bool okay = FALSE;

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
			    (v1 != ch_head->v_major) ||
			    (v2 != ch_head->v_minor) ||
			    (v3 != ch_head->v_patch))
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
			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= ch_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			ch_ptr = &ch_info[i];

			/* No choices yet */
			cur_choices = 0;

			/* Next... */
			continue;
		}

		/* There better be a current ch_ptr */
		if (!ch_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Process 'C' for "Chest Drop" */
		if (buf[0] == 'C')
		{
			int tval, chance;

			/* Too many choices? */
			if (cur_choices >= MAX_CHEST_CHOICES) return (PARSE_ERROR_TOO_MANY_ARGUMENTS);

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
					&tval, &chance)) return (PARSE_ERROR_GENERIC);

			/* Save the values */
			ch_ptr->tval[cur_choices] = tval;
			ch_ptr->chance[cur_choices] = chance;

			/* Increment choices */
			cur_choices++;
			ch_ptr->choices = cur_choices;

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
 * Initialize the "mp_info" array, by parsing an ascii "template" file
 */
errr init_mp_info_txt(FILE *fp, char *buf)
{
	int i;

	/* Current entry */
	static player_magic *mp_ptr = NULL;

	static int cur_spell = 0;

	/* Not ready yet */
	bool okay = FALSE;

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
			    (v1 != mp_head->v_major) ||
			    (v2 != mp_head->v_minor) ||
			    (v3 != mp_head->v_patch))
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
			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= mp_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			mp_ptr = &mp_info[i];

			/* No choices yet */
			cur_spell = 0;

			/* Next... */
			continue;
		}

		/* There better be a current cp_ptr */
		if (!mp_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Process 'R' for "Realm Information" (one line only) */
		if (buf[0] == 'R')
		{
			int book, stat, realm, first, weight1, weight2;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
					&book, &stat, &realm,
					&first, &weight1, &weight2)) return (PARSE_ERROR_GENERIC);

			mp_ptr->spell_book = book;
			mp_ptr->spell_stat = stat;
			mp_ptr->spell_realm = realm;
			mp_ptr->spell_first = first;
			mp_ptr->spell_weight1 = weight1;
			mp_ptr->spell_weight2 = weight2;

			/* Next... */
			continue;
		}

		/* Process 'M' for "Magic Information" (one line only) */
		if (buf[0] == 'M')
		{
			int number;
			int start_index[11];

			/* Scan for the values */
			if (12 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
					&number, &start_index[0], &start_index[1],
					&start_index[2], &start_index[3], &start_index[4],
					&start_index[5], &start_index[6], &start_index[7],
					&start_index[8], &start_index[9], &start_index[10]))
			{
				return (PARSE_ERROR_GENERIC);
			}

			mp_ptr->spell_number = number;

			for (i = 0; i < 11; i++)
			{
				mp_ptr->book_start_index[i] = start_index[i];
			}

			/* Next... */
			continue;
		}

		/* Process 'S' for "Spell Information" */
		if (buf[0] == 'S')
		{
			int index, slevel, smana, sfail, sexp, stimecode;

			magic_type *spell = NULL;

			/* Too many choices? */
			if (cur_spell >= 64) return (PARSE_ERROR_TOO_MANY_ARGUMENTS);

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
					&index, &slevel, &smana,
					&sfail, &sexp, &stimecode)) return (PARSE_ERROR_GENERIC);

			/* find the spell to fill in */
			spell = &mp_ptr->info[cur_spell];

			/* Save the values */
			spell->index = index;
			spell->slevel = slevel;
			spell->smana = smana;
			spell->sfail = sfail;
			spell->sexp = sexp;
			spell->stimecode = stimecode;

			/* Increment choices */
			cur_spell++;

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
			if (h_head->text_size + strlen(s) + 8 > FAKE_TEXT_SIZE)
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
			if (j >= MAX_B_IDX) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Get the *real* index */
			i = (i * MAX_B_IDX) + j;

			/* Verify information */
			if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

			/* Verify information */
			if (i >= b_head->info_num) return (PARSE_ERROR_OBSOLETE_FILE);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			ot_ptr = &b_info[i];

			/* Hack -- Verify space */
			if (b_head->name_size + strlen(t) + 8 > FAKE_NAME_SIZE)
			{
				return (PARSE_ERROR_OUT_OF_MEMORY);
			}

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

			/* Changed this to a for loop from a while loop.
			 * It appears to have been setting off an odd
			 * compiler bug in Borland C for the windows
			 * version. -BR-
			 * Vanilla 2.9.3 avoids this by removing the superfluous continue
			 * at the end of the loop. I removed that here as well as
			 * keeping BR's fix -pelpel-
			 */
			for (j=MAX_P_IDX; j > 0; j--)
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

#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */



