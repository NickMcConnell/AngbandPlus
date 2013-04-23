#define INIT1_C
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

/* A macro for how large the current info array can grow before it overflows. */
#define MAX_I	(int)(z_info->fake_info_size/head->info_len)


typedef struct flag_name flag_name;

struct flag_name
{
	cptr name; /* The name of the flag in the text file. */
	int set; /* The set into which the flag is to be sent. */
	u32b flag; /* The flag being set. */
};

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
    "CHARGE",  /* WAS: XXX2 */
	"CRAWL",
	"DROOL",
	"SPIT",
	"XXX3",
	"GAZE",
	"WAIL",
	"SPORE",
	"WORSHIP",
	"BEG",
	"INSULT",
	"MOAN",
    "SHOW",  /* WAS: XXX5 */
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
	"GUARDIAN",
	"MALE",
	"FEMALE",
	"CHAR_CLEAR",
	"CHAR_MULTI",
	"ATTR_CLEAR",
	"ATTR_MULTI",
	"ALWAYS_GUARD",
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
static cptr r_info_flags2[] =
{
	"STUPID",
	"SMART",
    "CAN_SPEAK", /* WAS: XXX1X2 */
    "REFLECTING",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
    "SHAPECHANGER", /*  WAS: XXX3X2 */
    "ATTR_ANY",     /*  WAS: XXX4X2 */
	"POWERFUL",
    "ELDRITCH_HORROR",
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
      "RUN_AWAY",
	"BRAIN_1",
	"BRAIN_2",
	"BRAIN_3",
	"BRAIN_4",
	"BRAIN_5",
	"BRAIN_6",
	"BRAIN_7",
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
    "GREAT_OLD_ONE", /*WAS: XXX1X3 */
    "GOOD",
	"XXX3X3",
    "NONLIVING", /*WAS: XXX4X3 */
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
	"CTHULOID",
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
	"ODD_ARTICLE",
	"XXX3X4",
    "BA_SHARD",  /* WAS: XXX4X4 */
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
    "BA_NUKE", /* WAS: XXX5X4 */
    "BR_NUKE", /* WAS: XXX6X4 */
    "BA_CHAO", /* WAS: XXX7X4 */
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
    "DREAD_CURSE",
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
    "S_REAVER",
	"S_MONSTER",
	"S_MONSTERS",
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_HYDRA",
	"S_CTHULOID",
	"S_DEMON",
	"S_UNDEAD",
	"S_DRAGON",
	"S_HI_UNDEAD",
	"S_HI_DRAGON",
	"S_GOO",
	"S_UNIQUE"
};


/*
 * Object flags
 */
static flag_name info_flags[] =
{
	{"STR", 0, TR1_STR},
	{"INT", 0, TR1_INT},
	{"WIS", 0, TR1_WIS},
	{"DEX", 0, TR1_DEX},
	{"CON", 0, TR1_CON},
	{"CHR", 0, TR1_CHR},
/*	{"XXX1", 0, TR1_XXX1}, */
/*	{"XXX2", 0, TR1_XXX2}, */
	{"STEALTH", 0, TR1_STEALTH},
	{"SEARCH", 0, TR1_SEARCH},
	{"INFRA", 0, TR1_INFRA},
	{"TUNNEL", 0, TR1_TUNNEL},
	{"SPEED", 0, TR1_SPEED},
	{"BLOWS", 0, TR1_BLOWS},
    {"CHAOTIC", 0, TR1_CHAOTIC},
    {"VAMPIRIC", 0, TR1_VAMPIRIC},
	{"SLAY_ANIMAL", 0, TR1_SLAY_ANIMAL},
	{"SLAY_EVIL", 0, TR1_SLAY_EVIL},
	{"SLAY_UNDEAD", 0, TR1_SLAY_UNDEAD},
	{"SLAY_DEMON", 0, TR1_SLAY_DEMON},
	{"SLAY_ORC", 0, TR1_SLAY_ORC},
	{"SLAY_TROLL", 0, TR1_SLAY_TROLL},
	{"SLAY_GIANT", 0, TR1_SLAY_GIANT},
	{"SLAY_DRAGON", 0, TR1_SLAY_DRAGON},
	{"KILL_DRAGON", 0, TR1_KILL_DRAGON},
	{"X15_DRAGON", 0, TR1_X15_DRAGON},
    {"VORPAL", 0, TR1_VORPAL},
	{"IMPACT", 0, TR1_IMPACT},
    {"BRAND_POIS", 0, TR1_BRAND_POIS},
	{"BRAND_ACID", 0, TR1_BRAND_ACID},
	{"BRAND_ELEC", 0, TR1_BRAND_ELEC},
	{"BRAND_FIRE", 0, TR1_BRAND_FIRE},
	{"BRAND_COLD", 0, TR1_BRAND_COLD},
	{"SUST_STR", 1, TR2_SUST_STR},
	{"SUST_INT", 1, TR2_SUST_INT},
	{"SUST_WIS", 1, TR2_SUST_WIS},
	{"SUST_DEX", 1, TR2_SUST_DEX},
	{"SUST_CON", 1, TR2_SUST_CON},
	{"SUST_CHR", 1, TR2_SUST_CHR},
	{"RAND_RESIST", 1, TR2_RAND_RESIST},
	{"RAND_POWER", 1, TR2_RAND_POWER},
	{"IM_ACID", 1, TR2_IM_ACID},
	{"IM_ELEC", 1, TR2_IM_ELEC},
	{"IM_FIRE", 1, TR2_IM_FIRE},
	{"IM_COLD", 1, TR2_IM_COLD},
	{"RAND_EXTRA", 1, TR2_RAND_EXTRA},
    {"REFLECT", 1, TR2_REFLECT},
	{"FREE_ACT", 1, TR2_FREE_ACT},
	{"HOLD_LIFE", 1, TR2_HOLD_LIFE},
	{"RES_ACID", 1, TR2_RES_ACID},
	{"RES_ELEC", 1, TR2_RES_ELEC},
	{"RES_FIRE", 1, TR2_RES_FIRE},
	{"RES_COLD", 1, TR2_RES_COLD},
	{"RES_POIS", 1, TR2_RES_POIS},
    {"RES_FEAR", 1, TR2_RES_FEAR},
	{"RES_LITE", 1, TR2_RES_LITE},
	{"RES_DARK", 1, TR2_RES_DARK},
	{"RES_BLIND", 1, TR2_RES_BLIND},
	{"RES_CONF", 1, TR2_RES_CONF},
	{"RES_SOUND", 1, TR2_RES_SOUND},
	{"RES_SHARDS", 1, TR2_RES_SHARDS},
	{"RES_NETHER", 1, TR2_RES_NETHER},
	{"RES_NEXUS", 1, TR2_RES_NEXUS},
	{"RES_CHAOS", 1, TR2_RES_CHAOS},
	{"RES_DISEN", 1, TR2_RES_DISEN},
    {"SH_FIRE", 2, TR3_SH_FIRE},
    {"SH_ELEC", 2, TR3_SH_ELEC},
	{"SHOW_ARMOUR", 2, TR3_SHOW_ARMOUR},
	{"AUTO_CURSE", 2, TR3_AUTO_CURSE},
    {"NO_TELE", 2, TR3_NO_TELE},
    {"NO_MAGIC", 2, TR3_NO_MAGIC},
    {"WRAITH", 2, TR3_WRAITH},
    {"TY_CURSE", 2, TR3_TY_CURSE},
	{"EASY_KNOW", 2, TR3_EASY_KNOW},
	{"HIDE_TYPE", 2, TR3_HIDE_TYPE},
	{"SHOW_MODS", 2, TR3_SHOW_MODS},
	{"INSTA_ART", 2, TR3_INSTA_ART},
	{"FEATHER", 2, TR3_FEATHER},
	{"LITE", 2, TR3_LITE},
	{"SEE_INVIS", 2, TR3_SEE_INVIS},
	{"TELEPATHY", 2, TR3_TELEPATHY},
	{"SLOW_DIGEST", 2, TR3_SLOW_DIGEST},
	{"REGEN", 2, TR3_REGEN},
	{"XTRA_MIGHT", 2, TR3_XTRA_MIGHT},
	{"XTRA_SHOTS", 2, TR3_XTRA_SHOTS},
	{"IGNORE_ACID", 2, TR3_IGNORE_ACID},
	{"IGNORE_ELEC", 2, TR3_IGNORE_ELEC},
	{"IGNORE_FIRE", 2, TR3_IGNORE_FIRE},
	{"IGNORE_COLD", 2, TR3_IGNORE_COLD},
	{"ACTIVATE", 2, TR3_ACTIVATE},
	{"DRAIN_EXP", 2, TR3_DRAIN_EXP},
	{"TELEPORT", 2, TR3_TELEPORT},
	{"AGGRAVATE", 2, TR3_AGGRAVATE},
	{"BLESSED", 2, TR3_BLESSED},
	{"CURSED", 2, TR3_CURSED},
	{"HEAVY_CURSE", 2, TR3_HEAVY_CURSE},
	{"PERMA_CURSE", 2, TR3_PERMA_CURSE},
};

/* A list of the flags for explosion types understood by project(). */
cptr explode_flags[] =
{
	"ELEC",
	"POIS",
	"ACID",
	"COLD",
	"FIRE",
	"","","","",
	"MISSILE",
	"ARROW",
	"PLASMA",
	"",
	"WATER",
	"LITE",
	"DARK",
	"LITE_WEAK",
	"DARK_WEAK",
	"",
	"SHARDS",
	"SOUND",
	"CONFUSION",
	"FORCE",
	"INERTIA",
	"",
	"MANA",
	"METEOR",
	"ICE",
	"",
	"CHAOS",
	"NETHER",
	"DISENCHANT",
	"NEXUS",
	"TIME",
	"GRAVITY",
	"","","","",
	"KILL_WALL",
	"KILL_DOOR",
	"KILL_TRAP",
	"","",
	"MAKE_WALL",
	"MAKE_DOOR",
	"MAKE_TRAP",
	"","","",
	"OLD_CLONE",
	"OLD_POLY",
	"OLD_HEAL",
	"OLD_SPEED",
	"OLD_SLOW",
	"OLD_CONF",
	"OLD_SLEEP",
	"OLD_DRAIN",
	"","",
	"AWAY_UNDEAD",
	"AWAY_EVIL",
	"AWAY_ALL",
	"TURN_UNDEAD",
	"TURN_EVIL",
	"TURN_ALL",
	"DISP_UNDEAD",
	"DISP_EVIL",
	"DISP_ALL",
	"DISP_DEMON",
	"DISP_LIVING",
	"SHARD",
	"NUKE",
	"MAKE_GLYPH",
	"STASIS",
	"STONE_WALL",
	"DEATH_RAY",
	"STUN",
	"HOLY_FIRE",
	"HELL_FIRE",
	"DISINTEGRATE",
	"CHARM",
	"CONTROL_UNDEAD",
	"CONTROL_ANIMAL",
	"PSI",
	"PSI_DRAIN",
	"TELEKINESIS",
	"JAM_DOOR",
	"DOMINATION",
	"DISP_GOOD",
	NULL
};

/* A list of the types of death event in order */
static cptr death_flags[] =
{
	"NOTHING",
	"MONSTER",
	"OBJECT",
	"EXPLODE",
	"COIN",
	NULL
};

/* A list of the types of coins. This can't be taken from k_info.txt because of
 * the duplicate entries. */
cptr coin_types[] =
{
	"","","COPPER", /* 482 */
	"","","SILVER", /* 485 */
	"","","","","GOLD", /* 490 */
	"","","","","","MITHRIL", /* 496 */
	"ADAMANTIUM", /* 497 */
	NULL
};

/*
 * A macro to run a given function which returns errr, and return any error
 * it gives.
 */
#define try(X) \
{ \
	errr err = X; \
	if (err != SUCCESS) return err; \
}


/*
 * A simple bounds check function for "unsigned char", etc..
 */
static errr byte_ok(int value)
{
	if (value < 0) return PARSE_ERROR_OUT_OF_BOUNDS;
	if (value > MAX_UCHAR) return PARSE_ERROR_OUT_OF_BOUNDS;
	return SUCCESS;
}



/* A macro to indicate if a character is used to terminate a name. */
#define okchar(char) (char == '\0' || char == ':')

/*
 * Return the string within an array which also exists within a string, 
 * returning (for example) 1 for the first element.
 * Returns 0 if there isn't exactly one such string.
 */
static s16b find_string(char *buf, cptr *array)
{
	u16b i, value = 0;
	char *place = 0;

	/* Find a string surrounded by ':'s. */
	for (i = 0; array[i]; i++)
	{
		if (strlen(array[i]) && strstr(buf, array[i]))
		{
			char *tmp = strstr(buf, array[i]);
			/* Check that this can't be merely a substring */
			if (!okchar(*(tmp-1))) continue;
			if (!okchar(*(tmp+strlen(array[i])))) continue;
			/* Check that this is the only possibility. */
			if (value)
			{
				msg_print("Too many suitable strings!");
				return 0;
			}
			value = i + 1;
			place = tmp;
		}
		}

	/* None there. */
	if (!value) return 0;
	/* If exactly one has been found, remove it. Leave the ':'s, though. */
	for (i = 0; i < strlen(array[value-1]); i++)
	{
		*(place+i)=' ';
	}
	return value;
}

/* A few macros for use in the 'E' case in init_r_event_txt() */

/* If one copy of chr exists within buf, return the integer immediately after it. */
#define readnum(chr) ((!strchr(buf, chr)) ? -1 : (strchr(strchr(buf, chr)+1, chr)) ? -2 : atoi(strchr(buf, chr)+1))

/* A separate routine to remove used number strings */
#define clearnum(chr) {char *s; for (s = strchr(buf, chr); *s == chr || (*s >= '0' && *s <= '9'); s++) *s=' ';}

/* A routine to set x to be the number after a given letter, and then clear it from the text string. 
 * If there are no such flags, it does nothing. If there are more than one, it returns an error.
 * THIS ASSUMES THAT ALL VALID VALUES ARE NON-NEGATIVE
 */
#define readclearnum(x, chr) if (readnum(chr) == -2) \
	{msg_format("Too many '%c's!", chr);msg_print(NULL);return ERR_PARSE;} \
	else if (readnum(chr) > -1) {x=readnum(chr); clearnum(chr);}

/*
 * A wrapper around find_string for info files.
 * As the text entries are stored as offsets in a single string rather than
 * simply as an array of strings, this first turns the former into the latter.
 * It assumes that the entries have numbers within (0,max)
 */
#define find_string_info(x_name, x_info, max, w) \
{ \
	C_TNEW(array, max, cptr); \
	for (i = 1; i < max; i++) \
		if (x_info[i].name) \
			array[i-1] = (cptr)(x_name+x_info[i].name); \
		else \
			array[i-1] = ""; \
	array[max-1] = 0; \
	w = find_string(buf, array); \
	TFREE(array); \
}

/*
 * A wrapper around find_string for a single string.
 */
static bool find_string_x(char *buf, cptr string)
{
	cptr array[2];
	array[0] = string;
	array[1] = NULL;
	return (bool)find_string(buf, array);
}

/* Find a string beginning and ending with a given character. Complain if it contains one of a set of other
 * characters. Allow \\ to be used to force the following character to be treated as plain text.
 *
 * buf is the string these characters are copied from.
 * this is the character which starts and ends this type of string.
 * all contains the characters which start and end all types of string being searched for.
 * output contains the text strings for the array in question. 
 * this_size contains the current offset of the final character in output.
 * max_size contains the maximum offset allowed.
 * offset contains the offset for this event (should be 0 initially).
 */
static errr do_get_string(char *buf, char this, cptr all, char *output, u32b *this_size, u32b max_size, u16b *offset)
{
	char *q, *r, *s = buf, *t = strchr(buf, '\0'), *last;
	bool escaped = FALSE;
	do {
		s = strchr(s+1, this);
		
	} while (s && *(s-1) == '\\');

	/* No such string. */
	if (!s) return SUCCESS;
	
	/* Go to the first character in the string itself. */
	s++;

	while (t > s && (*t != this || *(t-1) == '\\'))
	{
		t--;
	}

	/* No explicit end, so use end of buf. */
	if (t == s) t = strchr(s, '\0');
	
	/* Now copy the string to temp, being careful of \\s and rogue termination characters. */
	for (r = q = s, last = t; r < t; r++, q++)
	{
		/* Copy the character after the \\ and reset the flag. */
		if (escaped)
		{
			*q = *r;
			escaped = FALSE;
		}
		/* Accept anything from the character after a \\, but not the \\ itself. */
		else if (*r == '\\')
		{
			q--;
			escaped = TRUE;
		}
		/* Normal parsing of termination characters depends on which string is parsed first, 
		 * so reject all of them. We accept this termination character because it's unambiguous. */
		else if (strchr(all, *r) && *r != this)
		{
			printf("Unexpected termination character!");
			return ERR_PARSE;
		}
		/* Everything else is fine. */
		else
		{
			if (q != r) *q = *r;
		}
	}
	/* Make s the string of interest for now. */
	*q = '\0';
	if (max_size-(*this_size) < strlen(s))
	{
		msg_print("Not enough space for string!");
		return ERR_MEMORY;
	}
	/* Advance and save the index. */
	if (!(*offset)) (*offset) = ++(*this_size);
	/* Append characters to the text. */
	strcpy(output + (*this_size), s);

	/* Advance the index. */
	(*this_size) += strlen(s);

	/* Remove string from the buffer. */
	for (r = s-1; r <= t; r++)
	{
		*r=' ';
	}
	return SUCCESS;
}


/* Clear the \\ characters from a string */
static void clear_escapes(char *buf)
{
	char *q, *r;
	bool escaped = FALSE;
	/* Remove \\ characters from the string. */
	for (r = q = buf; *r != '\0'; q++, r++)
	{
		if (escaped)
		{
			*q = *r;
			escaped = FALSE;
		}
		else if (*r == '\\')
		{
			q--;
			escaped = TRUE;
		}
		else
		{
			if (q != r) *q = *r;
		}
	}
	*q = '\0';
}

/*
 * Initialize the "death_event" array, by parsing part of the "r_info.txt" file
 */
errr parse_r_event(char *buf, header *head, vptr *extra)
{
	int r_idx = (monster_race *)(*extra)-r_info;

	switch (*buf)
	{
		case 'N': /* New/Number/Name (extracting number) */
		{
			/* Get the number */
			(*extra) = r_info+atoi(buf+2);
			return SUCCESS;
		}

		case 'E': /* (Death) Events */
		{
			/* Current entry */
			death_event_type *d_ptr;

			int i;
			u16b temp_name_offset = 0;


			/* Nothing can be done without a valid monster */
			if (!r_idx)
			{
				msg_print("No r_idx specified!");
				return ERR_MISSING;
			}

			/* Find an unused event slot */
			if ((++error_idx) >= MAX_I)
			{
				msg_print("Too many events!");
				return ERR_MEMORY;
			}
			d_ptr = (death_event_type*)head->info_ptr+error_idx;


			/* Look for a text string to avoid matching keywords within it later.
			 * To avoid conflicts with other arbitrary text, a \\ before a character
			 * means that it can't terminate a string.
			 * This is the string which is printed when an event occurs.
			 */
			i = do_get_string(buf, '"', "^\"", head->text_ptr, &(head->text_size), z_info->fake_text_size, &(d_ptr->text));
			if (i) return i;
			/* This is a second text string, the use of which depends on the type of event.
			 * It currently only supplies a name for a randart. We haven't yet found out
			 * what type of event it is, so we save the offset to a temporary variable to
			 * deal with later.
			 */
			{
				u32b temp_name_size = head->name_size;
				i = do_get_string(buf, '^', "^\"", head->name_ptr, &(temp_name_size), z_info->fake_name_size, &temp_name_offset);
				if (i) return i;
				head->name_size = (u16b)temp_name_size;
			}
			/* Now the text strings have been removed, we remove the \\s from the file. Note that, if a name
			 * contains a \\ character, this must be listed as \\. Similarly, a " must be listed as \" and
			 * a ^ as \^. There should hopefully not be many of these.
			 */
			clear_escapes(buf);

			/* Save the monster index */
			d_ptr->r_idx = r_idx;

			/* Now find the name of an event type. */
			d_ptr->type = find_string(buf, death_flags);

			/* Check for the ONLY_ONE flag. */
			if (find_string_x(buf, "ONLY_ONE")) d_ptr->flags |= EF_ONLY_ONE;

			if (find_string_x(buf, "IF_PREV")) d_ptr->flags |= EF_IF_PREV;

			/* A single ONLY_ONE flag causes no problems, but having
			 * an IF_PREV flag without a previous entry with the same
			 * r_idx would be bad. */

			if (d_ptr->flags & EF_IF_PREV && (error_idx == 0 ||
			d_ptr->r_idx != (d_ptr-1)->r_idx))
			{
				msg_print("IF_PREV without previous entry.");
				return ERR_FLAG;
			}

			/* Look for flags compatible with the event type
			 * First parse arbitrary text strings to avoid having to
			 * deal with keywords within them.
			 *
			 * Then look for source-based strings (which should be
			 * capitalised). These will not generally have numerical
			 * alternatives.
			 *
			 * Then look for info-based strings. These will always
			 * have numerical alternatives to prevent confusion between
			 * them, and between them and source-based strings.
			 *
			 * Finally, look for other keyword-based parameters. These
			 * will use a single character to indicate them, so they
			 * must be dealt with after all text strings that might
			 * include such characters.
			 */

			switch (d_ptr->type)
			{
				case DEATH_OBJECT:
				{
					make_item_type *i_ptr = &d_ptr->par.item;
					if (find_string_x(buf, "ARTEFACT")) i_ptr->flags |= EI_ART;
					if (find_string_x(buf, "EGO")) i_ptr->flags |= EI_EGO;
					
					if (find_string_x(buf, "RANDOM"))
					{
						i_ptr->flags |= EI_RAND;
					}
					else if (i_ptr->flags & EI_ART)
					{
						find_string_info(a_name, a_info, MAX_A_IDX, i_ptr->x_idx);
					}
#ifdef ALLOW_EGO_DROP
					else if (i_ptr->flags & EI_EGO)
					{
						find_string_info(e_name, e_info, MAX_E_IDX, i_ptr->x_idx);
					}
#endif
					find_string_info(k_name, k_info, MAX_K_IDX, i_ptr->k_idx);
					if (lookup_kind(readnum('t'), readnum('s')))
					{
						byte t = 0,s = 0;
						readclearnum(t, 't');
						readclearnum(s, 's');
						i_ptr->k_idx = lookup_kind(t,s);
					}
					if (i_ptr->flags & EI_ART)
					{
						readclearnum(i_ptr->x_idx, 'a');
					}
#ifdef ALLOW_EGO_DROP
					else if (i_ptr->flags & EI_EGO)
					{
						readclearnum(i_ptr->x_idx, 'e');
					}
#endif
					readclearnum(i_ptr->min, '(');
					readclearnum(i_ptr->max, '-');

					/* Add the name of a randart, if provided. */
					if (i_ptr->flags & EI_RAND && i_ptr->flags & EI_ART)
					{
						if (temp_name_offset)
						{
							i_ptr->name = temp_name_offset;
							temp_name_offset = 0;
						}
					}
					/* Interpret no number parameter as being 1. */
					if (i_ptr->min == 0 && i_ptr->max == 0)
					{
						i_ptr->max=i_ptr->min=1;
					}

					/* Ensure that a possible x_idx field has been created for
					 * any non-random artefact or ego item */
					if (i_ptr->flags & EI_ART && ~i_ptr->flags & EI_RAND)
					{
						artifact_type *a_ptr = &a_info[i_ptr->x_idx];
						/* Ensure this is a real artefact. */
						if (!a_ptr->name)
						{
							msg_print("No valid artefact specified.");
							return ERR_PARSE;
						}
						/* Take an unstated k_idx to be that of the artefact. */
						else if (!i_ptr->k_idx)
						{
							i_ptr->k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);
						}
						/* Ensure that any stated k_idx is the right one. */
						else if (k_info[i_ptr->k_idx].tval != a_ptr->tval ||
							k_info[i_ptr->k_idx].sval != a_ptr->sval)
						{
							msg_print("Incompatible object and artefact.");
							return ERR_PARSE;
						}
					}
#ifdef ALLOW_EGO_DROP
					else if (i_ptr->flags & EI_EGO && ~i_ptr->flags & EI_RAND)
					{
						ego_item_type *e_ptr = &e_info[i_ptr->x_idx];
						/* Ensure this is a real ego item. */
						if (!e_ptr->name)
						{
							msg_print("No valid ego type specified.");
							return ERR_PARSE;
						}
						/* Ensure that the ego type is possible for this k_idx. */
					}
#endif

					/* Ensure that a possible k_idx field has been created. */
					if (k_info[i_ptr->k_idx].name == 0)
					{
						msg_print("No valid object specified.");
						return ERR_PARSE;
					}

					/* Ensure that RAND has not been used without a sensible thing
					 * to randomise. */
					if (i_ptr->flags & EI_RAND && ~i_ptr->flags & (EI_ART
#ifdef ALLOW_EGO_DROP
					| EI_EGO
#endif
					))
					{
						msg_print("Nothing valid to randomise.");
						return ERR_PARSE;
					}

					/* Prevent badly formatted ranges. */
					if (i_ptr->min > i_ptr->max || !i_ptr->min)
					{
						msg_print("Bad number parameter.");
						return ERR_FLAG;
					}
					break;
				}
				case DEATH_MONSTER:  
				{
					make_monster_type *i_ptr = &d_ptr->par.monster;
					i_ptr->strict = find_string_x(buf, "STRICT");
					find_string_info(r_name, r_info, MAX_R_IDX, i_ptr->num);
					readclearnum(i_ptr->num, 'n');
					readclearnum(i_ptr->radius, 'r');
					readclearnum(i_ptr->min, '(');
					readclearnum(i_ptr->max, '-');

					/* Interpret no number parameter as being 1. */
					if (i_ptr->min == 0 && i_ptr->max == 0)
					{
						i_ptr->max=i_ptr->min=1;
					}

					/* As the original square still has the original monster
					 * on it, parse a missing or 0 parameter as 1.
					 * Should there be a way of using the original
					 * square if requested? */
					if (!i_ptr->radius)
					{
						i_ptr->radius = 1;
					}

					/* Prevent badly formatted ranges. */
					if (i_ptr->min > i_ptr->max || !i_ptr->min)
					{
						msg_print("Bad number parameter.");
						return ERR_FLAG;
					}

					/* Prevent non-existant monster references. */
					if (!i_ptr->num || i_ptr->num >= MAX_R_IDX)
					{
						msg_print("No monster specified!");
						return ERR_FLAG;
					}
					break;
				}
				case DEATH_EXPLODE:
				{
					make_explosion_type *i_ptr = &d_ptr->par.explosion;
					i_ptr->method = find_string(buf, explode_flags);
					readclearnum(i_ptr->radius,'r');
					readclearnum(i_ptr->dice,'(');
					readclearnum(i_ptr->sides,'d');
					
					/* Require an explosion type */
					if (!i_ptr->method)
					{
						msg_print("No method indicated.");
						return ERR_FLAG;
					}
					/* Allow (d30) or (100) damage formats,
					 * but not no damage indicator at all. */
					if (!i_ptr->dice && !i_ptr->sides)
					{
						msg_print("No damage indicator.");
						return ERR_FLAG;
					}
					else if (!i_ptr->dice)
					{
						i_ptr->dice = 1;
					}
					else if (!i_ptr->sides)
					{
						i_ptr->sides = 1;
					}
					break;
				}
				case DEATH_COIN:
				{
					make_coin_type *i_ptr = &d_ptr->par.coin;
					i_ptr->metal = find_string(buf, coin_types)-1;
					if (i_ptr->metal == -1)
					{
						msg_print("No coin type!");
						return ERR_FLAG;
					}
					break;
				}
				case DEATH_NOTHING: /* No special parameters */
				break;
				default: /* i.e. no valid type specification */
				{
					msg_print("No event type specified!");
					return ERR_MISSING;
				}
			}
			/* Fill in the other general flags now the text
			 * strings have all been parsed. */
			readclearnum(d_ptr->num, 'p');
			readclearnum(d_ptr->denom,'/');

			/* Parse a missing probability entry as 1/1. */
			if (!d_ptr->num && !d_ptr->denom)
			{
				d_ptr->num=d_ptr->denom=1;
			}

			/* Complain about p0/x formats or probabilities over 1. */
			if (!d_ptr->num || d_ptr->num > d_ptr->denom)
			{
				msg_print("Bad probability specification.");
				return ERR_PARSE;
			}
			/* The type-specific text field should have either been reset, or not
			 * set in the first place. */
			if (temp_name_offset)
			{
				msg_print("Meaningless text field found.");
				return ERR_PARSE;
			}

			/* Finally check that everything has been read. */
			{
				cptr s;
				for (s = buf+2; *s != '\0'; s++)
				{
					if (!strchr(": )\"", *s))
					{
						msg_print("Uninterpreted characters!");
						return ERR_PARSE;
					}
				}
			}
			return SUCCESS;
		}

		/* Do not check for inappropriate lines, as r_info should not have
		 * been initialised correctly if there were any. This also limits
		 * the scope for synchronisation problems. */
		default:
		{
			return SUCCESS;
		}
	}
}


/*** Initialize from ascii template files ***/


/*
 * Add a text to the text-storage and store offset to it.
 *
 * Returns an error when there isn't enough space available to store
 * the text.
 */
static errr add_text(u32b *offset, header *head, cptr buf)
{
	/* Hack -- Verify space */
	if (head->text_size + strlen(buf) + 8 > z_info->fake_text_size)
		return PARSE_ERROR_OUT_OF_MEMORY;

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
	return SUCCESS;
}


/*
 * Check that a name-like string is reasonable.
 *
 * This only actually checks that CM_ACT characters are always preceded
 * by the appropriate CM_TRUE ones, i.e. that the string referred to is
 * known to exist.
 */
static errr check_string(cptr buf)
{
	cptr i;
	char require;

	/* Verify that the CM_ACT flags are legal. */
	for (i = buf, require = 0x00;*i;i++)
	{
		byte flag = 1<<find_ci(*i);
		byte action = find_cm(*i);

		/* The definition scheme used here only applies to
		 * the 28 numbers between 1 and 31 which are not multiples
		 * of 8. */
		if (~*i & 0x07 || *i & 0xE0) continue;

		switch (action)
		{
			case CM_FALSE:
			case CM_NORM:
				require &= ~flag;
				break;
			case CM_TRUE:
				require |= flag;
				break;
			case CM_ACT:
				if (~require & flag) return PARSE_ERROR_GENERIC;
				break;
			default: /* Paranoia. */
				quit_fmt("find_cm() is giving bizarre output: %d.", action);
		}
	}

	return SUCCESS;
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

	/* Check object names for incorrect flags. */
	switch (head->header_num)
	{
		case K_HEAD: case A_HEAD: case E_HEAD: case OB_HEAD: case U_HEAD:
		if (check_string(buf)) return 0;
	}

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
errr parse_z_info(char *buf, header *head, vptr UNUSED *extra)
{
	char c;
	long max;
	maxima *z = head->info_ptr;

	/* Hack - Always use record 0. */
	error_idx = 0;

	/* Verify M:x:num format. */
	if (2 != sscanf(buf, "M:%c:%ld", &c, &max)) return PARSE_ERROR_GENERIC;

	switch (c)
	{
		case 'O': z->o_max = max; break;
		case 'M': z->m_max = max; break;
		case 'B': z->oname = max; break;
		case 'D': z->mname = max; break;
		case 'N': z->fake_name_size = max; break;
		case 'T': z->fake_text_size = max; break;
		case 'I': z->fake_info_size = max; break;
		case 'R': z->ar_delay = max; break;
		default: return PARSE_ERROR_UNDEFINED_DIRECTIVE;
	}

	return SUCCESS;
}


/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr parse_f_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s;

	/* Current entry */
	feature_type *f_ptr = *extra;

	/* If this isn't the start of a record, there should already be one. */
	if (*buf != 'N' && !f_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

	/* Process 'N' for "New/Number/Name" */
	switch (*buf)
	{
		case 'N':
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
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = f_ptr = (feature_type*)head->info_ptr + i;

			/* Store the name */
			if (!(f_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Default "mimic" */
			f_ptr->mimic = i;

			return SUCCESS;
		}
		case 'M':
		{
			int mimic;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d", &mimic)) return (1);

			/* Save the values */
			f_ptr->mimic = mimic;

			return SUCCESS;
		}
		/* Process 'G' for "Graphics" (one line only) */
		case 'G':
		{
			int tmp;

			/* Paranoia */
			if (!buf[2]) return (1);
			if (!buf[3]) return (1);
			if (!buf[4]) return (1);

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);
			if (tmp < 0) return (1);

			/* Save the values */
			f_ptr->d_char = buf[2];
			f_ptr->d_attr = tmp;

			return SUCCESS;
		}
		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
}



/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr parse_v_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s;

	/* Current entry */
	vault_type *v_ptr = *extra;

	if (*buf != 'N' && !v_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;
	
	switch (*buf)
	{
		case 'N':

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
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = v_ptr = (vault_type*)head->info_ptr + i;

			/* Store the name */
			if (!(v_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

			return SUCCESS;
		}


		/* Process 'D' for "Description" */
		case 'D':
		{
			/* Acquire the text */
			s = buf+2;

			/* Store the text and continue. */
			return add_text(&(v_ptr->text), head, s);
		}


		/* Process 'X' for "Extra info" (one line only) */
		case 'X':
		{
			int typ, rat, hgt, wid;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			                &typ, &rat, &hgt, &wid)) return (1);

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;
			v_ptr->hgt = hgt;
			v_ptr->wid = wid;

			/* Next... */
			return SUCCESS;
		}

		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
}




/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_object_flag(u32b **flag, cptr what)
{
	uint i;

	/* Check flags */
	for (i = 0; i < N_ELEMENTS(info_flags); i++)
	{
		flag_name *f_ptr = info_flags+i;

		if (!flag[f_ptr->set]) continue;

		if (streq(what, f_ptr->name))
		{
			*(flag[f_ptr->set]) |= f_ptr->flag;
			return SUCCESS;
		}
	}

	/* Oops */
	msg_format("Unknown object flag '%s'.", what);

	/* Error */
	return (1);
}

static errr grab_one_kind_flag(object_kind *ptr, cptr what)
{
	u32b *f[3];
	f[0] = &(ptr->flags1);
	f[1] = &(ptr->flags2);
	f[2] = &(ptr->flags3);
	return grab_one_object_flag(f, what);
}


/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr parse_k_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, *t;

	/* Current entry */
	object_kind *k_ptr = *extra;
	
	/* If this isn't the start of a record, there should already be one. */
	if (!strchr("NT", *buf) && !k_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

	/* Process 'N' for "New/Number/Name" */
	switch (*buf)
	{
		case 'N':
	{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Paranoia -- require a name */
			if (!*++s) return (1);

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return (4);


			/* Verify information */
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = k_ptr = (object_kind*)head->info_ptr + i;

			/* Store the name */
			if (!(k_ptr->name = add_name(head, s)))
				return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Next... */
			return SUCCESS;
		}

		case 'G':
		{
			char sym, col;
			int tmp, p_id;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%c:%c:%d", &sym, &col, &p_id))
			{
				return (2);
			}

			/* Extract the attr */
			tmp = color_char_to_attr(col);

			/* Paranoia */
			if (tmp < 0) return (1);
			if (p_id < 0 || p_id > 255) return (1);

			/* Save the values */
			k_ptr->d_char = sym;
			k_ptr->d_attr = tmp;

			/* Hack - store p_id in k_ptr->u_idx until flavor_init() */
			k_ptr->u_idx = p_id;

			/* Next... */
			return SUCCESS;
		}

		case 'I':
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
			return SUCCESS;
		}

		/* Process 'W' for "More Info" (one line only) */
		case 'W':
		{
			int level, extra, wgt;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			                &level, &extra, &wgt, &cost)) return (1);

			/* Save the values */
			k_ptr->level = level;
/*			k_ptr->extra = extra;*/
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

			/* Next... */
			return SUCCESS;
		}

		/* Process 'A' for "Allocation" (one line only) */
		case 'A':
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
			return SUCCESS;
		}

		/* Hack -- Process 'P' for "power" and such */
		case 'P':
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
			return SUCCESS;
		}

		/* Hack -- Process 'F' for flags */
		case 'F':
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
			return SUCCESS;
		}
		/* Process 'D' for Description */
		case 'D':
		{
			/* Acquire the text */
			s = buf+2;

			/* Store the text and continue. */
			return add_text(&(k_ptr->text), head, s);
		}
		default:
		{
			/* Oops */
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
}



/*
 * Initialize the "o_base" array, by parsing an ascii "template" file
 */
errr parse_o_base(char *buf, header *head, vptr *extra)
{
	/* Current entry */
	o_base_type *ob_ptr = *extra;

	/* If this isn't the start of a record, there should already be one. */
	switch (*buf)
	{
		case 'N': case 'M': break; /* N and M need nothing. */
		case 'C': /* C needs an existing record. */
		if (!ob_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER; break;
		default: /* Nothing else makes sense. */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Process 'N' for "New/Number/Name" */
	switch (*buf)
	{
		case 'N':
		{
			/* Get the index */
			int i = atoi(buf+2);

			/* Find the colon before the name */
			char *s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return (PARSE_ERROR_GENERIC);

			/* Advance to the name */
			s++;

			/* Paranoia -- require a name */
			if (!*s) return (PARSE_ERROR_GENERIC);

			/* Verify index. */
			try(byte_ok(i));

			/* Advance the index */
			error_idx = i;

			/* Paranoia - there should always be space for 256 entries. */
			if (error_idx >= MAX_I) return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Point at the "info" */
			*extra = ob_ptr = (o_base_type*)head->info_ptr + error_idx;

			/* Store the name */
			if (!(ob_ptr->name = add_name(head, s)))
				return (PARSE_ERROR_OUT_OF_MEMORY);

			return SUCCESS;
		}
		/* This copies one base entry to another, so that there
		 * can be several sets of (for instance) scrolls.
		 * It must be after the original entry and not within
		 * an entry itself
		 * The latter restriction is inessential, but it allows
		 * this to be directly after the entry it mimics, which
		 * would not be possible if this didn't "finish off" the
		 * previous entry first. */
		case 'M':
		{
			int oldp_id, newp_id;
			o_base_type *ob2_ptr;

			if (2 != sscanf(buf+2, "%d:%d",
				&oldp_id, &newp_id)) return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Check for valid indices */
			try(byte_ok(oldp_id));
			try(byte_ok(newp_id));

			error_idx = newp_id;

			ob2_ptr = (o_base_type*)head->info_ptr + oldp_id;
			ob_ptr = (o_base_type*)head->info_ptr + newp_id;

			/* Check that the new entry does not already exist. */
			if (ob_ptr->name) return PARSE_ERROR_GENERIC;

			/* Check that the earlier entry exists. */
			if (!ob2_ptr->name) return PARSE_ERROR_GENERIC;

			/* Copy across. */
			ob_ptr->name = ob2_ptr->name;
			ob_ptr->cost = ob2_ptr->cost;

			/* End of entry. */
			*extra = ob_ptr = NULL;

			return SUCCESS;
		}
		case 'C':
		{
			long cost = 0;

			/* There better be a current k_ptr */
			if (!ob_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

			/* Work out the lowest cost this item might have. */
			if (!strncmp(buf+2, "default", strlen("default")))
			{
				int i;
				for (i = 0; i < z_info->k_max; i++)
				{
					object_kind *k_ptr = &k_info[i];

					/* Reject worthless items. */
					if (!k_ptr->cost) continue;

					/* Reject items with a different p_id. */
					if (k_ptr->u_idx != error_idx) continue;
		
					/* Reduce cost if necessary. */
					if (!cost || k_ptr->cost < cost) cost = k_ptr->cost;
				}
			}
			else
			{
				/* Scan for the values */
				if (1 != sscanf(buf+2, "%ld", &cost)) return (PARSE_ERROR_GENERIC);
			}

			/* Save the value */
			ob_ptr->cost = cost;

			return SUCCESS;
		}
		default: /* Never reached. */
		{
			return UNREAD_VALUE;
		}
	}
}

/*
 * Put entries in u_info for each type of flavourless description defined in
 * o_base.
 */
static errr parse_unid_flavourless(header *head)
{
	int i;

	/* Create the flavourless entries. */
	for (i = 0; i < z_info->ob_max; i++)
	{
		o_base_type *ob_ptr = &o_base[i];
		unident_type *u_ptr;

		/* No entry. */
		if (!ob_ptr->name) continue;

		/* Not flavourless. */
		if (strchr(ob_name+ob_ptr->name, CM_ACT+CI_FLAVOUR)) continue;
			
		/* Check that u_info is large enough. */
		if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

		/* Set u_ptr. */
		u_ptr = (unident_type*)head->info_ptr + error_idx;
			
		/* Set its p_id appropriately. */
		u_ptr->name = 0;
		u_ptr->p_id = i;
	}
	return SUCCESS;
}

/*
 * Parse part of the "unid_info" array from a string.
 */
errr parse_u_info(char *buf, header *head, vptr *extra)
{
	/* Current entry */
	unident_type *u_ptr = *extra;
	char *s;

	/* 
	 * Only 'N' entries can set u_ptr.
	 */
	/* There better be a current u_ptr. */
	if ((*buf != 'N') && !u_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;
		
	/* Actually parse the current field. */
	switch (buf[0])
	{
		case 'N': /* Name */
		{
			/* Advance to the name */
			s = buf+2;

			/* Paranoia -- require a name */
			if (!*s) return (PARSE_ERROR_INCORRECT_SYNTAX);

			/* Check that u_info is large enough. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Set u_ptr */
			*extra = u_ptr = (unident_type*)head->info_ptr + error_idx;
			
			/* Store the name. */
			if (!(u_ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}

		case 'G': /* Graphics */
		{
			char sym, col;
			int p_id, s_id;
			s16b i;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%c:%c:%d:%d", &sym, &col, &p_id, &s_id)) return PARSE_ERROR_GENERIC;

			/* Paranoia */
			if (color_char_to_attr(col) < 0)
			{
				msg_print("Illegal colour.");
				return (PARSE_ERROR_GENERIC);
			}
			if (!isgraph(sym))
			{
				msg_print("Illegal symbol.");
				return (PARSE_ERROR_GENERIC);
			}
			/* Extract the char */
			u_ptr->d_char = sym;

			/* Extract the attr */
			u_ptr->d_attr = color_char_to_attr(col);
			
			/* Verify indices' legality */
			try(byte_ok(p_id));
			try(byte_ok(s_id));

			/* Change the primary index to an o_base one. */
			u_ptr->p_id = p_id;
			
			/* Extract the secondary index */
			u_ptr->s_id = s_id;

			/* Verify uniqueness */
			for (i = 0; i < error_idx; i++)
			{
				unident_type *u2_ptr = (unident_type*)head->info_ptr + i;
				if (u2_ptr->p_id != u_ptr->p_id) continue;
				if (u2_ptr->s_id != u_ptr->s_id) continue;
				msg_format("Duplicated indices (%d,%d).", u_ptr->p_id, u_ptr->s_id);
				return (PARSE_ERROR_GENERIC);
			}

			return SUCCESS;
		}

		default: /* Oops */
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *ptr, cptr what)
{
	u32b *f[3];
	f[0] = &(ptr->flags1);
	f[1] = &(ptr->flags2);
	f[2] = &(ptr->flags3);
	return grab_one_object_flag(f, what);
}




/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr parse_a_info(char *buf, header *head, vptr *extra)
{
	artifact_type *a_ptr = *extra;

	int i;

	char *s, *t;

	/* Need an a_ptr before anything is written. */
	if (*buf != 'N' && !a_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N':
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
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = a_ptr = (artifact_type*)head->info_ptr + i;

			/* Store the name */
			if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

			/* Hack - Ignore all elemental attacks. */
			a_ptr->flags3 |= (TR3_IGNORE_ALL);

			/* Next... */
			return SUCCESS;
		}
		case 'I':
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
			return SUCCESS;
		}
		case 'W':
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
			return SUCCESS;
		}
		case 'P':
		{
			int ac, hd1, hd2, th, td, ta;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			                &ac, &hd1, &hd2, &th, &td, &ta)) return (1);

			a_ptr->ac = ac;
			a_ptr->dd = hd1;
			a_ptr->ds = hd2;
			a_ptr->to_h = th;
			a_ptr->to_d = td;
			a_ptr->to_a =  ta;

			/* Next... */
			return SUCCESS;
		}
		case 'F':
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
			return SUCCESS;
		}
		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static errr grab_one_ego_item_flag(ego_item_type *ptr, cptr what)
{
	u32b *f[3];
	f[0] = &(ptr->flags1);
	f[1] = &(ptr->flags2);
	f[2] = &(ptr->flags3);
	return grab_one_object_flag(f, what);
}





/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr parse_e_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, *t;

	ego_item_type *e_ptr = *extra;

	/* If this isn't the start of a record, there should already be one. */
	if (*buf != 'N' && !e_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N':
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
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = e_ptr = (ego_item_type*)head->info_ptr + i;

			/* Store the name */
			if (!(e_ptr->name = add_name(head, s)))
				return (PARSE_ERROR_OUT_OF_MEMORY);

			return SUCCESS;
		}

		/* Process 'X' for "Xtra" (one line only) */
		case 'X':
		{
			int slot, rating;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
			                &slot, &rating)) return (1);

			/* Save the values */
			e_ptr->slot = slot;
			e_ptr->rating = rating;

			return SUCCESS;
		}

		/* Process 'W' for "More Info" (one line only) */
		case 'W':
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

			return SUCCESS;
		}

		/* Hack -- Process 'C' for "creation" */
		case 'C':
		{
			int th, td, ta, pv;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			                &th, &td, &ta, &pv)) return (1);

			e_ptr->max_to_h = th;
			e_ptr->max_to_d = td;
			e_ptr->max_to_a = ta;
			e_ptr->max_pval = pv;

			return SUCCESS;
		}

		/* Hack -- Process 'F' for flags */
		case 'F':
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

			return SUCCESS;
		}

		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
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

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}




/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr parse_r_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, *t;

	monster_race *r_ptr = (monster_race *)(*extra);

	if (*buf != 'N' && !r_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;
	
	switch (*buf)
	{
		/* Process 'N' for "New/Number/Name" */
		case 'N':
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
			if (i >= MAX_I) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = r_ptr = (monster_race*)head->info_ptr + i;

			/* Store the name */
			if (!(r_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

			return SUCCESS;
		}

		/* Process 'D' for "Description" */
		case 'D':
		{
			/* Acquire the text */
			s = buf+2;

			/* Store the text and continue. */
			return add_text(&(r_ptr->text), head, s);
		}

		/* Process 'G' for "Graphics" (one line only) */
		case 'G':
		{
			int tmp;

			/* Paranoia */
			if (!buf[2]) return (1);
			if (!buf[3]) return (1);
			if (!buf[4]) return (1);

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);
			if (tmp < 0) return (1);

			/* Save the values */
			r_ptr->d_char = buf[2];
			r_ptr->d_attr = tmp;

			return SUCCESS;
		}

		/* Process 'I' for "Info" (one line only) */
		case 'I':
		{
			int spd, atspd, hp1, hp2, aaf, ac, slp;

			/* Scan for the other values */
			if (7 != sscanf(buf+2, "%d:%d:%dd%d:%d:%d:%d",
			                &spd, &atspd, &hp1, &hp2, &aaf, &ac, &slp)) return (1);

			/* Save the values */
			r_ptr->speed = spd;
			r_ptr->num_blows = atspd;
			r_ptr->hdice = hp1;
			r_ptr->hside = hp2;
			r_ptr->aaf = aaf;
			r_ptr->ac = ac;
			r_ptr->sleep = slp;

			return SUCCESS;
		}

		/* Process 'W' for "More Info" (one line only) */
		case 'W':
		{
			int lev, rar, pad;
			long exp;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			                &lev, &rar, &pad, &exp)) return (1);

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
/*			r_ptr->extra = pad;*/
			r_ptr->mexp = exp;

			return SUCCESS;
		}

		/* Process 'B' for "Blows" (up to four lines) */
		case 'B':
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

			return SUCCESS;
		}

		/* Process 'F' for "Basic Flags" (multiple lines) */
		case 'F':
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

			return SUCCESS;
		}

		/* Process 'S' for "Spell Flags" (multiple lines) */
		case 'S':
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
				if (0 != grab_one_spell_flag(r_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			return SUCCESS;
		}

		/* Ignore 'E' (death event) lines for now. */
		case 'E':
		{
			return SUCCESS;
		}

		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
		}
	}
}

/*
 * Find out if the input string starts with any of a set
 * of target strings. If it does, return the number of
 * the match. If not, return -1.
 *
 * The strings[] array should always end with a null pointer.
 */
static int is_streq(cptr in, cptr *strings)
{
	int i;
	for (i = 0; strings[i]; i++)
	{
		if (prefix(in, strings[i]))
		{
			return i;
		}
	}
	return -1;
}

/*
 * Initialize the "macro_info" array, by parsing an ascii "template" file
 */
errr parse_macro_info(char *buf, header *head, vptr *extra)
{
	char *s = buf+2;

	/* Current entry */
	init_macro_type *ptr = *extra;

	bool scope_restriction = ptr &&
		(ptr->file || ptr->pref || ptr->field);

	/* Everything else needs a record. */
	switch (*buf)
	{
		case 'Y': case 'B': case 'A': case 'M': case 'S':
		if (scope_restriction) return PARSE_ERROR_GENERIC;
		case 'F':
		if (!ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;
		case 'X':
		break;
		default: /* What is this thing? */
		return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	}

	/* Process 'X' for "From" */
	switch (*buf)
	{
		case 'X':
		{
			/* Paranoia -- require a name */
			if (!*s) return (PARSE_ERROR_GENERIC);

			/* Advance the index and check for overflow. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Point at the "info" */
			*extra = ptr = (init_macro_type*)head->info_ptr + error_idx;

			/* Store the name */
			if (!(ptr->name = add_name(head, s)))
				return (PARSE_ERROR_OUT_OF_MEMORY);

			return SUCCESS;
		}
		/* Process 'F' for "Flags" */
		case 'F':
		{
			/* If a previous 'F' field is known, add a second entry. */
			if (scope_restriction)
			{
				init_macro_type *macro2_ptr;

				/* Advance the index and check for overflow. */
				if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

				/* Point at the "info" */
				macro2_ptr = (init_macro_type*)head->info_ptr + error_idx;

				/* Copy across. */
				macro2_ptr->name = ptr->name;
				macro2_ptr->text = ptr->text;
				macro2_ptr->conv = ptr->conv;

				/* Accept the new entry. */
				*extra = ptr = macro2_ptr;
			}

			if (*s == ':' || *s == '\0')
			{
				ptr->file = 0;
			}
			else if (isdigit(*s))
			{
				int file = atoi(s);
				if (file < 0 || file > 256) return PARSE_ERROR_OUT_OF_BOUNDS;
				ptr->file = file;
			}
			else
			{
				cptr header_names[] =
				{
					"z_info",
					"macros",
					"f_info",
					"k_info",
					"u_info",
					"a_info",
					"e_info",
					"r_info",
					"r_event",
					"v_info",
					"o_base",
				};
				uint i;

				/* Look for a matching entry. */
				for (i = 0; i < N_ELEMENTS(header_names); i++)
					if (prefix(s, header_names[i])) break;
	
				if (i == N_ELEMENTS(header_names))
					return PARSE_ERROR_INVALID_FLAG;

				/* Set the file to the next number (0 is nothing) */
				ptr->file = i+1;
	
				/* Find the end of the string. */
				s += strlen(header_names[i]);
			}
			s = strchr(s, ':');
			if (s == NULL || *(++s) == '\0')
			{
				ptr->pref = ptr->field = 0;
				return SUCCESS;
			}
			else if (*s == ':')
			{
				ptr->pref = 0;
			}
			else
			{
				ptr->pref = *s;
			}
			s = strchr(s, ':');
			if (s == NULL || *(++s) == '\0')
			{
				ptr->field = 0;
			}
			else
			{
				int i = atoi(s);
				if (i < 1 || i > 255) return PARSE_ERROR_OUT_OF_BOUNDS;
				ptr->field = i;
			}
			return SUCCESS;
		}
		/* Process 'Y' for "To" (replace) */
		case 'Y':
		{
			ptr->conv = MACRO_CONV_REPLACE;

			/* Store the text */
			return add_text(&ptr->text, head, s);
		}
		
		/* Process 'B' for "To" (before) */
		case 'B':
		{
			ptr->conv = MACRO_CONV_BEFORE;

			/* Store the text */
			return add_text(&ptr->text, head, s);
		}

		/* Process 'A' for "To" (after) */
		case 'A':
		{
			ptr->conv = MACRO_CONV_AFTER;

			/* Store the text */
			return add_text(&ptr->text, head, s);
		}

		/* Process 'M' for "To" (numerical replace) */
		case 'M':
		{
			ptr->conv = MACRO_CONV_REPLACE;

			ptr->text = ++head->text_size;
			for (s--; s; s = strchr(s, ',')) 
			{
				int i = atoi(++s);
				try(byte_ok(i));

				if (head->text_size+9 > z_info->fake_text_size)
				{
					return PARSE_ERROR_OUT_OF_MEMORY;
				}
				macro_text[head->text_size++] = i;
			}
			return SUCCESS;
		}

		/* Hack - Process 'S' for "To" (special procedures) */
		case 'S':
		{
			cptr macro_spec_name[] =
			{
				"move to front",
				0
			};

			char conv_string[2]=" ";
			int i = is_streq(s, macro_spec_name);

			/* Failure */
			if (i == -1) return PARSE_ERROR_INVALID_FLAG;

			ptr->conv = MACRO_CONV_SPECIAL;

			conv_string[0] = i;

			return add_text(&ptr->text, head, conv_string);
		}
		default: /* Never reached */
		return UNREAD_VALUE;
	}
}


/*
 * Do any file-specific things which needs to be done before parsing starts.
 */
static errr init_info_txt_pre(header *head)
{
	switch (head->header_num)
	{
		/*
		 * Macros are valid for later parts of macro_info, so array references
		 * need to make sense whilst it is being read.
		 */
		case MACRO_HEAD:
		{
			macro_info = head->info_ptr;
			macro_text = head->text_ptr;
			macro_name = head->name_ptr;
			break;
		}
		case OB_HEAD:
		{
			/* Paranoia - o_base[] expects to have enough space for 256 entries. */
			if (MAX_I < 256) return PARSE_ERROR_OUT_OF_MEMORY;
			break;
		}
	}
	return SUCCESS;
}

/*
 * Find the flags shared by every object with a given p_id.
 */
static void find_ob_flags(header *head)
{
	bool ob_used[256];
	int i;
	o_base_type *obase = head->info_ptr;
	for (i = 0; i < 256; i++)
	{
		obase[i].flags1 = 0xFFFFFFFF;
		obase[i].flags2 = 0xFFFFFFFF;
		obase[i].flags3 = 0xFFFFFFFF;
		ob_used[i] = FALSE;
	}
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = k_info+i;

		/* Hack - object_kind.u_idx stores p_id during initialisation. */
		o_base_type *ob_ptr = obase+k_ptr->u_idx;

		/* Prune the flags as appropriate. */
		ob_ptr->flags1 &= k_ptr->flags1;
		ob_ptr->flags2 &= k_ptr->flags2;
		ob_ptr->flags3 &= k_ptr->flags3;

		/* Set tval on the first such object. */
		if (!ob_used[k_ptr->u_idx])
		{
			ob_used[k_ptr->u_idx] = TRUE;
			ob_ptr->tval = k_ptr->tval;
		}
		/* And unset it if the tval is unknown. */
		else if (ob_ptr->tval != k_ptr->tval)
		{
			ob_ptr->tval = TV_UNKNOWN;
		}
	}
}

#define ITFE(W,X) \
if (!(((maxima *)(head->info_ptr))->W)) \
{ \
	msg_format("Missing M:%c:* header.", X); \
	return PARSE_ERROR_MISSING_RECORD_HEADER; \
}

/*
 * Do any file-specific things which needs to be done after parsing finishes.
 */
static errr init_info_txt_final(header *head)
{
	switch (head->header_num)
	{
		/* Ensure the essential elements of z_info have been filled in. */
		case Z_HEAD:
		{
			ITFE(fake_info_size, 'I')
			ITFE(fake_text_size, 'T')
			ITFE(fake_name_size, 'N')
			ITFE(o_max, 'O')
			ITFE(m_max, 'M')
			ITFE(oname, 'N')
			ITFE(mname, 'D')
			break;
		}
		/* Add in any unflavoured objects. */
		case U_HEAD:
		{
			try(parse_unid_flavourless(head));
			break;
		}
		case K_HEAD:
		{
			/* o_base bases its defaults on k_info. */
			rebuild_raw |= 1<<OB_HEAD;
			break;
		}
		case MACRO_HEAD:
		{
			/* Force all later raw files to be rebuilt, as this changes them. */
			rebuild_raw = 0xFFFF & ~(1<<MACRO_HEAD | 1<<Z_HEAD);
			break;
		}
		case OB_HEAD:
		{
			/* Fill in object_base_type.flags*. */
			find_ob_flags(head);

			/* There should always be 256 entries. */
			error_idx = 255;

			/* The p_ids stored in u_info are derived from
			 * o_base[]. */
			rebuild_raw |= 1<<U_HEAD;
			break;
		}
	}
	return SUCCESS;
}

/* Define how many macros are available for this file. */
#define NUM_MACROS ((head->header_num == Z_HEAD) ? 0 :  \
	(head->header_num == MACRO_HEAD) ? error_idx : z_info->macros)



#define NO_VERSION	-2

static errr parse_info_line_aux(char *buf, header *head, vptr *extra)
{
	/* Then parse the resulting string. */

	/* Skip comments and blank lines */
	if (!buf[0] || (buf[0] == '#')) return SUCCESS;

	/* Verify correct "colon" format */
	if (buf[1] != ':') return (PARSE_ERROR_GENERIC);


	/* Hack -- Process 'V' for "Version" */
	if (buf[0] == 'V')
	{
		/* Scan for the values */
		if (!streq(buf+2, GAME_VERSION))

		{
			return (PARSE_ERROR_OBSOLETE_FILE);
		}

		/* Okay to proceed */
		error_idx = -1;

		/* Finished. */
		return SUCCESS;
	}

	/* No version yet */
	if (error_idx == NO_VERSION) return (PARSE_ERROR_OBSOLETE_FILE);

	/* Parse the line */
	return (*(head->parse_info_txt))(buf, head, extra);
}

/*
 * Parse a line from an ascii "template" file, applying a series of macros
 * to it as appropriate.
 *
 * This function can recurse once for every macro which acts on a given line.
 *
 * It enforces the macros in the opposite order to that in which they are given.
 */
static errr parse_info_line(char *buf, header *head, int start, vptr *extra)
{
	char buf2[1024];
	char *buf2end = buf2+1023;
	int i;

	*buf2end = '\0';

	for (i = start-1; i >= 0; i--)
	{
		init_macro_type *macro_ptr = macro_info+i;

		char *t;
		cptr s;
		int done = 0, field = (macro_ptr->field) ? 1 : 0;

		/* Bad file restriction. */
		if (macro_ptr->file && macro_ptr->file != head->header_num) continue;
	
		/* Bad prefix restriction. */
		if (macro_ptr->pref && macro_ptr->pref != buf[0]) continue;

		/* A macro may or may not modify the current string.
		 * If it does, t needs to be set to the start of the
		 * output line. */
		switch (macro_ptr->conv)
		{
			case MACRO_CONV_REPLACE:
			case MACRO_CONV_SPECIAL:
			t = buf2;
			break;
			case MACRO_CONV_BEFORE:
			case MACRO_CONV_AFTER:
			t = 0;
			break;
			default: /* Paranoia. */
			msg_format("Strange macro conversion type %d found.", macro_ptr->conv);
			return PARSE_ERROR_GENERIC;
		}

		/* Copy across, substituting and parsing. */
		for (s = buf; (!t || t < buf2end) && *s; s++)
		{
			/* Count the fields if required, starting each at the colon. */
			if (macro_ptr->field && *s == ':') field++;

			/* A match has been found. */
			if (field == macro_ptr->field && 
				prefix(s, macro_name+macro_ptr->name))
			{
				switch (macro_ptr->conv)
				{
					case MACRO_CONV_REPLACE:
					{
						cptr u = macro_text+macro_ptr->text;

						/* Remember that something happened. */
						done++;

						/* Copy as much of the string as is possible. */
						while (*u && t < buf2end) *(t++) = *(u++);

						break;
					}
					case MACRO_CONV_BEFORE:
					case MACRO_CONV_AFTER:
					{
						/* Count the matches for now. */
						done++;

						break;
					}
					case MACRO_CONV_SPECIAL:
					{
						switch (macro_text[macro_ptr->text])
						{
							case MACRO_SPEC_MOVE_TO_FRONT:
							{
								char *buf3 = C_ZNEW(strlen(buf2)+strlen(macro_name+macro_ptr->name)+1, char);
								char *start = t;

								/* Find the start of this section of
								 * buf2. */
								while (start > buf2 && *start != ':') start--;

								/* If it doesn't start at the beginning,
								 * copy the initial section across. */
								if (start > buf2)
								{
									*(start++) = '\0';
									sprintf(buf3, "%s:", buf2);
								}
								else
								{
									*buf3 = '\0';
								}

								/* Copy the string from the macro across. */
								strcat(buf3, macro_name+macro_ptr->name);

								/* Copy the rest of the string across. */
								*t = '\0';
								strcat(buf3, start);

								/* Copy back. */
								strcpy(buf2, buf3);
									
								t = strchr(buf2, '\0');
								KILL(buf3);
								done++;
							}
						}
					}
				}

				/* Don't process any part of the token again for
				 * this macro. */
				s += strlen(macro_name+macro_ptr->name)-1;

				/* Don't do anything else. */
				continue;

			}

			/* If the string is being modified, write the changed
			 * version. */
			if (t)
			{
				*(t++) = *s;
			}
		}

		/* Finish off. */
		if (t) (*t) = '\0';

		/* Nothing happened. */
		if (!done);

		else switch (macro_ptr->conv)
		{
			case MACRO_CONV_AFTER:
			{
				/* Parse the rest of this string. */
				try(parse_info_line(buf, head, i, extra));

				/* Parse the inserted string(s) fully. */
				for (i = 0; i < done; i++)
				{
					strcpy(buf, macro_text+macro_ptr->text);
					try(parse_info_line(buf, head, NUM_MACROS, extra));
				}

				/* All done. */
				return SUCCESS;
			}
			case MACRO_CONV_BEFORE:
			{
				cptr this_buf = string_make(buf);
				int j;

				/* Parse the inserted string(s) fully. */
				for (j = 0; j < done; j++)
				{
					strcpy(buf, macro_text+macro_ptr->text);
					try(parse_info_line(buf, head, NUM_MACROS, extra));
				}

				/* Parse the rest of this string. */
				strcpy(buf, this_buf);
				string_free(this_buf);
				try(parse_info_line(buf, head, i, extra));

				/* All done. */
				return SUCCESS;
			}
			case MACRO_CONV_REPLACE:
			case MACRO_CONV_SPECIAL:
			{
				strcpy(buf, buf2);
				break;
			}
		}
	}

	/* Now any macros have been processed, parse the line. */
	return parse_info_line_aux(buf, head, extra);
}

/*
 * Initialize an "*_info" array, by parsing an ascii "template" file
 */
errr init_info_txt(FILE *fp, char *buf, header *head)
{
	vptr extra = NULL;

	/* Before the version string. */
	error_idx = -2;

	/* Just before the first line */
	error_line = 0;


	/* Prepare the "fake" stuff */
	head->name_size = 0;
	head->text_size = 0;

	/* Carry out any pre-initialisation stuff. */
	try(init_info_txt_pre(head));

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		error_line++;

		if (head->header_num == Z_HEAD)
		{
			try(parse_info_line_aux(buf, head, &extra));
		}
		else
		{
			try(parse_info_line(buf, head, NUM_MACROS, &extra));
		}
	}

	/* Carry out any post-initialisation checks. */
	try(init_info_txt_final(head));

	/* Set the info size. */
	head->info_num = error_idx+1;
	head->info_size = head->info_len * head->info_num;

	/* Complete the "name" and "text" sizes */
	if (head->name_size) head->name_size++;
	if (head->text_size) head->text_size++;

	/* This doesn't need to be rebuilt. */
	rebuild_raw &= ~(head->header_num);

	/* No version yet */
	if (error_idx == NO_VERSION) return (PARSE_ERROR_OBSOLETE_FILE);

	/* All done. */
	return SUCCESS;;
}

#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */

