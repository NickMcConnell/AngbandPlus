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
 * Monster Blow Methods
 */
static cptr r_info_blow_method[] =
{
	"",
	"HIT",
	"TOUCH",
	"PUNCH",
	"KICK",
	"SLASH",
	"PIERCE",
	"BLUNT",
	"LURE",
	"XXX2",
	"HOWL",
	"CLAW",
	"KISS",
	"GRAB",
	"BITE",
	"STING",
	"BUTT",
	"CRUSH",
	"ENGULF",
	"CRAWL",
	"DROOL",
	"SPIT",
	"XXX4",
	"XXX5",
	"GAZE",
	"WAIL",
	"SPORE",
	"XXX6",
	"XXX7",
	"ZAP",
	"PECK",
	"SPEAK",
	"BEG",
	"SEDUCE",
	"XXX10",
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
	"FIRE",
	"ACID",
	"ELEC",
	"COLD",
	"STEAM",
	"NETHER",
	"ETHER",
	"BLIND",
	"CONFUSE",
	"TERRIFY",
	"PARALYZE",
	"LOSE_MUS",
	"LOSE_AGI",
	"LOSE_VIG",
	"LOSE_SCH",
	"LOSE_EGO",
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
	"DROP_5",
	"DROP_10",
	"DROP_20",
	"DROP_40",
	"DROP_80",
	"DROP_1D2",
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
	"FRIENDLY",
	"HOSTILE",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"XXX1",
	"XXX2",
	"XXX3",
	"GASEOUS",
	"TRACKER",
	"IMPENT",
	"POWERFUL",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"LOW_MANA_RUN",
	"ARCHER",
	"BRAIN_1",
	"BRAIN_2",
	"BRAIN_3",
	"BRAIN_4",
	"BRAIN_5",
	"BRAIN_6"
};

/*
 * Monster race flags
 */
static cptr r_info_flags3[] =
{
	"AUTOMATA",
	"DINOSAUR",
	"CONSTRUCT",
	"ELEMENTAL",
	"DEMON",
	"UNDEAD",
	"EVIL",
	"ANIMAL",
	"ALIEN",
	"BEASTMAN",
	"PLANT",
	"HURT_LIGHT",
	"HURT_ROCK",
	"IM_FIRE",
	"IM_EARTH",
	"IM_AIR",
	"IM_WATER",
	"IM_ELEC",
	"IM_ICE",
	"IM_ACID",
	"IM_POIS",
	"IM_TIME",
	"IM_ETHER",
	"IM_SOUND",
	"IM_NETHER",
	"IM_PIERCE",
	"IM_EDGED",
	"IM_BLUNT",
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
	"ARROW",
	"GUN",
	"RIFLE",
	"SHOTGUN",
	"ROCKET",
	"MISSILE",
	"BR_FIRE",
	"BR_EARTH",
	"BR_AIR",
	"BR_WATER",
	"BR_ELEC",
	"BR_ICE",
	"BR_ACID",
	"BR_POISON",
	"BR_TIME",
	"BR_ETHER",
	"BR_SOUND",
	"BR_NETHER",
	"BR_GRAVITY",
	"BR_RAD",
	"BR_LIGHT",
	"BR_DARK",
	"CLOUD_RAD",
	"CLOUD_POISON",
	"XXX3X4",
	"XXX4X4",
	"XXX5X4",
	"XXX6X4",
	"XXX7X4",
	"XXX8X4"
};

/*
 * Monster race flags
 */
static cptr r_info_flags5[] =
{
	"BA_FIRE",
	"BA_EARTH",
	"BA_AIR",
	"BA_WATER",
	"BA_ELEC",
	"BA_ICE",
	"BA_ACID",
	"BA_POISON",
	"BA_TIME",
	"BA_ETHER",
	"BA_SOUND",
	"BA_NETHER",
	"BA_GRAVITY",
	"BA_EMP",
	"BA_RAD",
	"XXX1X5",
	"BO_FIRE",
	"BO_EARTH",
	"BO_AIR",
	"BO_WATER",
	"BO_ELEC",
	"BO_ICE",
	"BO_ACID",
	"BO_POISON",
	"BO_TIME",
	"BO_ETHER",
	"BO_SOUND",
	"BO_NETHER",
	"BO_GRAVITY",
	"BO_DARK",
	"XXX3X5",
	"XXX4X5"
};

/*
 * Monster race flags
 */
static cptr r_info_flags6[] =
{
	"HASTE",
	"CURE",
	"HEAL",
	"ADD_MANA",
	"BLINK",
	"TPORT",
	"XXX1X6",
	"XXX2X6",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"TELE_SELF_TO",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"FEAR",
	"PSI",
	"DOMINATION",
	"STUN",
	"TK",
	"FORCE",
	"CONFUSION",
	"SPIRIT",
	"ECTOPLASM",
	"BLIND",
	"SLOW",
	"HOLD",
	"DRAIN_MANA",
	"CHARGE",
	"XXX4X6",
	"XXX5X6",
	"MIRROR_IMAGE"
};

/*
 * Monster race flags
 */
static cptr r_info_flags7[] =
{
	"BE_FIRE",
	"BE_ELEC",
	"BE_WATER",
	"XXX7X4",
	"XXX7X5",
	"XXX7X6",
	"XXX7X7",
	"XXX7X8",
	"XXX7X9",
	"XXX7X10",
	"XXX7X11",
	"XXX7X12",
	"XXX7X13",
	"XXX7X14",
	"S_CUTTENCLIP",
	"S_BEASTMEN",
	"S_PLANTS",
	"S_KIN",
	"S_HI_DEMON",
	"S_MONSTER",
	"S_MONSTERS",
	"S_AUTOMATA",
	"S_SPIDER",
	"S_HOUND",
	"S_MONKEY",
	"S_ALIEN",
	"S_DEMON",
	"S_UNDEAD",
	"S_ELEMENTAL",
	"S_HI_UNDEAD",
	"S_HI_ELEMENTAL",
	"S_UNIQUE"
};

/*
 * Monster race flags
 */
static cptr r_info_flags8[] =
{
	"VUN_FIRE",
	"VUN_EARTH",
	"VUN_AIR",
	"VUN_WATER",
	"VUN_ELEC",
	"VUN_ICE",
	"VUN_ACID",
	"VUN_POIS",
	"VUN_TIME",
	"VUN_ETHER",
	"VUN_SOUND",
	"VUN_NETHER",
	"VUN_PIERCE",
	"VUN_EDGED",
	"VUN_BLUNT",
	"XXX8X16",
	"XXX8X17",
	"XXX8X18",
	"XXX8X19",
	"XXX8X20",
	"XXX8X21",
	"XXX8X22",
	"XXX8X23",
	"XXX8X24",
	"XXX8X25",
	"XXX8X26",
	"XXX8X27",
	"XXX8X28",
	"XXX8X29",
	"XXX8X30",
	"CUTTENCLIP",
	"CLOUD_SURROUND"
};

/*
 * Object flags
 */
static cptr k_info_flags_pval[] =
{
	"MUS",
	"AGI",
	"VIG",
	"SCH",
	"EGO",
	"CHR",
	"XXX6",
	"XXX7",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"MANA",
	"HEALTH",
	"LIGHT",
	"SAVE",
	"MAGIC_MASTER",
	"XXX18",
	"XXX19",
	"BLOWS",
	"SHOTS",
	"MIGHT",
	"XXX23",
	"XXX24",
	"XXX25",
	"XXX26",
	"XXX27",
	"XXX28",
	"XXX29",
	"XXX30",
	"XXX31"
};

/*
 * Object flags
 */
static cptr k_info_flags1[] =
{
	"SUST_MUS",
	"SUST_AGI",
	"SUST_VIG",
	"SUST_SCH",
	"SUST_EGO",
	"SUST_CHR",
	"XXX7",
	"XXX8",
	"SLAY_ANIMAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_AUTOMATA",
	"SLAY_DINOSAUR",
	"SLAY_CONSTRUCT",
	"SLAY_ELEMENTAL",
	"SLAY_ALIEN",
	"SLAY_BEASTMAN",
	"SLAY_CARDS",
	"XXX20",
	"XXX21",
	"XXX22",
	"XXX23",
	"XXX24",
	"BRAND_FIRE",
	"BRAND_ELEC",
	"BRAND_ICE",
	"BRAND_ACID",
	"BRAND_POISON",
	"BRAND_FORCE",
	"XXX31",
	"XXX32"
};

/*
 * Object flags
 */
static cptr k_info_flags2[] =
{
	"IMPACT",
	"CRITICAL",
	"VAMPIRIC",
	"THROW",
	"PERFECT_BALANCE",
	"VORPAL",
	"RETURN",
	"EDGED",
	"BLUNT",
	"PIERCE",
	"SLOW_DIGEST",
	"FEATHER",
	"TELEPATHY",
	"SEE_INVIS",
	"FREE_ACT",
	"WRAITH",
	"BLESSED",
	"HOLD_LIFE",
	"REFLECT",
	"INVISIBLE",
	"IMMUNE",
	"XXX22",
	"XXX23",
	"XXX24",
	"XXX25",
	"XXX26",
	"REGEN_25",
	"REGEN_50",
	"REGEN_75",
	"RES_CONFU",
	"RES_BLIND",
	"RES_FEAR"
};

/*
 * Object flags
 */
static cptr k_info_flags3[] =
{
	"SH_FIRE",
	"SH_ELEC",
	"SPINES",
	"SH_ETHER",
	"SH_XXX5",
	"SH_XXX6",
	"SH_XXX7",
	"AUTOMATA",
	"ACTIVATE",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"NO_FUEL",
	"MECHA_GEN",
	"XXX16",
	"TELEPORT",
	"EARTHQUAKE",
	"NEVER_BLOW",
	"NO_TELEPORT",
	"NO_MAGIC",
	"DRAIN_EXP",
	"DRAIN_HP",
	"DRAIN_SP",
	"DRAIN_ITEM",
	"AGGRAVATE",
	"DISRUPT_SPELL",
	"MUTABLE",
	"XXX29",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};


/*
 * Class flags
 */
static cptr c_info_flags[] =
{
	"EXTRA_SHOT",
	"BRAVERY_30",
	"BLESS_WEAPON",
	"CUMBER_GLOVE",
	"ZERO_FAIL",
	"BEAM",
	"",
	"",
	"",
	"ARCANE",
	"SECULAR",
	"POWER",
	"PSEUDO_ID1",
	"PSEUDO_ID2",
	"PSEUDO_ID3",
	"PSEUDO_ID4",
	"PSEUDO_ID_HEAVY",
	"",
	"",
	"",
	"ANARCHIST",
	"MUTABLE",
	"XXX23",
	"XXX24",
	"XXX25",
	"XXX26",
	"XXX27",
	"XXX28",
	"XXX29",
	"XXX30",
	"XXX31",
	"XXX32"
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
 * This is the limits info array
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

	/* Process 'Q' for "Maximum q_info[] index" */
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
 * This is the Vault Information array CCC
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
 * This is the terrain info array (where the hell did they get f_info?)
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
		f_ptr = &f_info[i];

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
	if (grab_one_flag(&k_ptr->flags_pval, k_info_flags_pval, what) == 0)
		return (0);

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
 * This is the object info array
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
		int ac, hd1, hd2, th, td, ta, force;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (7 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d",
			           &ac, &hd1, &hd2, &th, &td, &ta, &force)) return (PARSE_ERROR_GENERIC);

		k_ptr->ac = ac;
		k_ptr->dd = hd1;
		k_ptr->ds = hd2;
		k_ptr->to_h = th;
		k_ptr->to_d = td;
		k_ptr->to_a =  ta;
		k_ptr->force = force;
	}

	/* Process 'O' for "Oppositions" (one line only) */
	else if (buf[0] == 'O')
	{
		int fir, ear, air, wtr, elc, ice, acd, psn;
		int tim, eth, snd, nth, lit, drk, psi, tlk, spi;

		/* There better be a current pr_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (17 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &fir, &ear, &air, &wtr, &elc, &ice,
						&acd, &psn, &tim, &eth, &snd, &nth, 
						&lit, &drk, &psi, &tlk, &spi)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->res[RS_FIR] = fir;
		k_ptr->res[RS_EAR] = ear;
		k_ptr->res[RS_AIR] = air;
		k_ptr->res[RS_WTR] = wtr;
		k_ptr->res[RS_ELC] = elc;
		k_ptr->res[RS_ICE] = ice;
		k_ptr->res[RS_ACD] = acd;
		k_ptr->res[RS_PSN] = psn;
		k_ptr->res[RS_TIM] = tim;
		k_ptr->res[RS_ETH] = eth;
		k_ptr->res[RS_SND] = snd;
		k_ptr->res[RS_NTH] = nth;
		k_ptr->res[RS_LIT] = lit;
		k_ptr->res[RS_DRK] = drk;
		k_ptr->res[RS_PSI] = psi;
		k_ptr->res[RS_TLK] = tlk;
		k_ptr->res[RS_SPI] = spi;
	}
	
	/* Process 'M' for "Multiple quantity" (one line only) */
	else if (buf[0] == 'M')
	{
		int prob, dice, side;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%dd%d", &prob, &dice, &side))				
		{
			return (PARSE_ERROR_GENERIC);
		}

		/* Save the values */
		k_ptr->gen_mult_prob = prob;
		k_ptr->gen_dice = dice;
		k_ptr->gen_side = side;
	}

	/* Hack -- Process 'R' for "Ranged" */
	else if (buf[0] == 'R')
	{
		int ammo_tval, ammo_mult, num_fire, range;
		int degree;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
				&ammo_tval, &ammo_mult, &num_fire, &range, &degree)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->ammo_tval = ammo_tval;
		k_ptr->ammo_mult = ammo_mult;
		k_ptr->num_fire = num_fire;
		k_ptr->range = range;
		k_ptr->degree = degree;
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
 * This is the artifact info array 
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
		a_ptr = &a_info[i];

		/* Store the name */
		if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
#if 0
		a_ptr->flags3 |= (TR3_IGNORE_MASK);
#endif
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
			            &tval, &sval)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->tval = tval;
		a_ptr->sval = sval;
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
		int ac, hd1, hd2, th, td, ta, force;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (7 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d",
			            &ac, &hd1, &hd2, &th, &td, &ta, &force)) return (PARSE_ERROR_GENERIC);

		a_ptr->ac = ac;
		a_ptr->dd = hd1;
		a_ptr->ds = hd2;
		a_ptr->to_h = th;
		a_ptr->to_d = td;
		a_ptr->to_a = ta;
		a_ptr->force = force;
	}

	/* Process 'PV' for "pval values" (one line only) */
	else if (buf[0] == 'Y')
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
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

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

	/* Process 'O' for "Oppositions" (one line only) */
	else if (buf[0] == 'O')
	{
		int fir, ear, air, wtr, elc, ice, acd, psn;
		int tim, eth, snd, nth, lit, drk, psi, tlk, spi;

		/* There better be a current pr_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (17 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &fir, &ear, &air, &wtr, &elc, &ice,
						&acd, &psn, &tim, &eth, &snd, &nth, 
						&lit, &drk, &psi, &tlk, &spi)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->res[RS_FIR] = fir;
		a_ptr->res[RS_EAR] = ear;
		a_ptr->res[RS_AIR] = air;
		a_ptr->res[RS_WTR] = wtr;
		a_ptr->res[RS_ELC] = elc;
		a_ptr->res[RS_ICE] = ice;
		a_ptr->res[RS_ACD] = acd;
		a_ptr->res[RS_PSN] = psn;
		a_ptr->res[RS_TIM] = tim;
		a_ptr->res[RS_ETH] = eth;
		a_ptr->res[RS_SND] = snd;
		a_ptr->res[RS_NTH] = nth;
		a_ptr->res[RS_LIT] = lit;
		a_ptr->res[RS_DRK] = drk;
		a_ptr->res[RS_PSI] = psi;
		a_ptr->res[RS_TLK] = tlk;
		a_ptr->res[RS_SPI] = spi;
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
#if 0
		/* Find the colon before the name */
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		/* if (!*s) return (PARSE_ERROR_GENERIC); */

		/* Get the activation */
		/* grab_one_activation(a_ptr, buf + 2); */
#endif
		/* Scan for the values */
		if (2 != sscanf(buf + 2, "%d:%d",
			            &time, &rand)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->time = time;
		a_ptr->randtime = rand;
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
 * Initialize the "e_info" array, by parsing an ascii "template" file
 * This is the ego item info array 
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
		int th, td, ta;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			            &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

		e_ptr->max_to_h = th;
		e_ptr->max_to_d = td;
		e_ptr->max_to_a = ta;
	}

	/* Process 'O' for "Oppositions" (one line only) */
	else if (buf[0] == 'O')
	{
		int fir, ear, air, wtr, elc, ice, acd, psn;
		int tim, eth, snd, nth, lit, drk, psi, tlk, spi;

		/* There better be a current pr_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (17 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &fir, &ear, &air, &wtr, &elc, &ice,
						&acd, &psn, &tim, &eth, &snd, &nth, 
						&lit, &drk, &psi, &tlk, &spi)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		e_ptr->res[RS_FIR] = fir;
		e_ptr->res[RS_EAR] = ear;
		e_ptr->res[RS_AIR] = air;
		e_ptr->res[RS_WTR] = wtr;
		e_ptr->res[RS_ELC] = elc;
		e_ptr->res[RS_ICE] = ice;
		e_ptr->res[RS_ACD] = acd;
		e_ptr->res[RS_PSN] = psn;
		e_ptr->res[RS_TIM] = tim;
		e_ptr->res[RS_ETH] = eth;
		e_ptr->res[RS_SND] = snd;
		e_ptr->res[RS_NTH] = nth;
		e_ptr->res[RS_LIT] = lit;
		e_ptr->res[RS_DRK] = drk;
		e_ptr->res[RS_PSI] = psi;
		e_ptr->res[RS_TLK] = tlk;
		e_ptr->res[RS_SPI] = spi;
	}

	/* Process 'Y' for "pval values" (one line only) */
	else if (buf[0] == 'Y')
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
			for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

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

	if (grab_one_flag(&r_ptr->flags8, r_info_flags8, what) == 0)
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
 * This is the monster race info array
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
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp, aaf, ac, slp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
			            &spd, &hp, &aaf, &ac, &slp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->speed = spd;
		r_ptr->hitpoints = hp;
		r_ptr->aaf = aaf;
		r_ptr->ac = ac;
		r_ptr->sleep = slp;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, mana;
		long exp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			            &lev, &rar, &mana, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
		r_ptr->mana = mana;
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
 * This is the player_race info array
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
		pr_ptr = &p_info[i];

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
 * Grab one flag in a player class from a textual string
 */
static errr grab_one_class_flag(player_class *pc_ptr, cptr what)
{
	if (grab_one_flag(&pc_ptr->flags, c_info_flags, what) == 0)
		return (0);

	/* Oops */
	msg_format("Unknown player class flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}


/*
 * Initialize the "c_info" array, by parsing an ascii "template" file
 * This is the player class info array
 */
errr parse_c_info(char *buf, header *head)
{
	int i, j;

	char *s, *t;

	/* Current entry */
	static player_class *pc_ptr = NULL;

	static int cur_mtitle = 0;
	static int cur_ftitle = 0;
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
		pc_ptr = &c_info[i];

		/* Store the name */
		if (!(pc_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* No titles and equipment yet */
		cur_mtitle = 0;
		cur_ftitle = 0;
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

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int mhp, exp, sense_div, pet_upkeep_div;
		long sense_base;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%ld:%d:%d",
			            &mhp, &exp, &sense_base, &sense_div, &pet_upkeep_div))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_mhp = mhp;
		pc_ptr->c_exp = exp;
		pc_ptr->sense_base = sense_base;
		pc_ptr->sense_div = sense_div;
		pc_ptr->pet_upkeep_div = pet_upkeep_div;
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
		int spell_stat, dist;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
		                &spell_stat, &dist))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->spell_stat = spell_stat;
		pc_ptr->spell_weight = dist;
	}

	/* Process 'B' for "Spell/Prayer book info" */
	else if (buf[0] == 'B')
	{
		int book_index, handicap;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);
	
		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &book_index, &handicap))
			return (PARSE_ERROR_GENERIC);

		if (book_index >= SV_BOOK_MAX) return (PARSE_ERROR_OUT_OF_BOUNDS);

		pc_ptr->spell_book[book_index] = TRUE;
		pc_ptr->spell_handicap[book_index] = handicap;

	}

	/* Process 'T' for " Male Titles" */
	else if (buf[0] == 'T')
	{

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&pc_ptr->mtitle[cur_mtitle], head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
		
		/* Next title */
		cur_mtitle++;

		/* Limit number of titles */
		if (cur_mtitle > PY_MAX_LEVEL / 5)
			return (PARSE_ERROR_TOO_MANY_ARGUMENTS);
	}
	/* Process 'Z' for "Female Titles" */
	else if (buf[0] == 'Z')
	{

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&pc_ptr->ftitle[cur_ftitle], head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
		
		/* Next title */
		cur_ftitle++;

		/* Limit number of titles */
		if (cur_ftitle > PY_MAX_LEVEL / 5)
			return (PARSE_ERROR_TOO_MANY_ARGUMENTS);
	}

	/* Process 'E' for "Starting Equipment" */
	else if (buf[0] == 'E')
	{
		int tval, sval, pval, min, max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
			            &tval, &sval, &pval, &min, &max)) return (PARSE_ERROR_GENERIC);

		if ((min < 0) || (max < 0) || (min > 99) || (max > 99))
			return (PARSE_ERROR_INVALID_ITEM_NUMBER);

		/* Save the values */
		e_ptr->tval = tval;
		e_ptr->sval = sval;
		e_ptr->pval = pval;
		e_ptr->min = min;
		e_ptr->max = max;

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
 * This is the player history info array
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
		h_ptr = &h_info[i];

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
 * Initialize the "b_info" array, by parsing an ascii "template" file
 * This is the shop owner info array
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
		ot_ptr = &b_info[i];

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
 * This is the cost adjust info array
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
		int lev, r_idx, u_idx, max;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",&lev, &r_idx, &u_idx, &max))
			return (PARSE_ERROR_GENERIC);

		/* Check quests */
		for (i = 0; i < error_idx; i++)
		{
			/* Check for quest */
			if (lev <= prev_lev) return (PARSE_ERROR_NON_SEQUENTIAL_QUESTS);
		}

		/* Save the values */
		prev_lev = q_ptr->base_level = q_ptr->active_level = lev;

		if(r_idx)
		{
			monster_race *r_ptr = &r_info[r_idx];

			/*unique quest*/
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				q_ptr->mon_idx = r_idx;
				q_ptr->type = QUEST_FIXED_U;
			}

			/*fixed quest*/
			else 
			{
				q_ptr->mon_idx = r_idx;
				q_ptr->type = QUEST_FIXED;
			}
		}
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

#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */
