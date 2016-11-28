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

/*
 * Extract the first few "tokens" from a buffer
 *
 * This function uses "colon" and "slash" as the delimeter characters.
 *
 * We never extract more than "num" tokens.  The "last" token may include
 * "delimeter" characters, allowing the buffer to include a "string" token.
 *
 * We save pointers to the tokens in "tokens", and return the number found.
 *
 * Hack -- Attempt to handle the 'c' character formalism
 *
 * Hack -- An empty buffer, or a final delimeter, yields an "empty" token.
 *
 * Hack -- We will always extract at least one token
 */
static int get_tokens(char *buf, s16b num, char **tokens)
{
	int i = 0;

	char *s = buf;


	/* Process */
	while (i < num - 1)
	{
		char *t;

		/* Scan the string */
		for (t = s; *t; t++)
		{
			/* Found a delimiter */
			if ((*t == ':') || (*t == '/')) break;
		}

		/* Nothing left */
		if (!*t) break;

		/* Nuke and advance */
		*t++ = '\0';

		/* Save the token */
		tokens[i++] = s;

		/* Advance */
		s = t;
	}

	/* Save the token */
	tokens[i++] = s;

	/* Number found */
	return (i);
}
 

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
/* the following added -GSN- */	
	"INSANITY", 
	"LOSE_LUC",
	"HALLU",
	"HUNGER",
	"DEATH",
	"STONE",
	NULL
};

/* Polymorph flags */

static cptr r_poly_flags[] =
{
        "HAS_HEAD",
        "HAS_FEET",
        "HAS_HANDS",
        "HAS_TORSO",
        "HAS_FINGERS",
        "HAS_EYES",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
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
/* the following added -GSN- */	
	"NOMISS",
	"PLAYER_GHOST",
	"AURA_FEAR",
	"LAWFUL",
	"CHAOTIC",
	"DOOM",
	"NOPOLY",
	"EXTINCT" /* For ordering purposes, don't put this into r_info! */
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
	"DUNADAN", /* -GSN- changed from Amberite */
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
	"BR_WATE",
	"ARROW_5", /* -GSN- */
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
        "BO_SHAR",
        "BL_SHAR",
        "CH_SHAR",
        "BR_SKUL",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
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
	"HOLD",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
};

/*
 * Monster race flags
 */
static cptr r_info_flags6[] =
{
	"HASTE",
	"HAND_DOOM",
	"HEAL",
	"HEAL2", /* -GSN- from RAngband */
	"BLINK",
	"TPORT",
	"BR_WIND",
	"BA_WIND",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"DOOR",
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
	"S_WRAITH",
	"S_UNIQUE",
        "S_DOOM",
        "S_HI_DOOM",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
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
	"UNFAIR", /* -GSN- from Kamband */
	"RES_SLASH",
	"RES_BASH",
	"RES_STAB",
	"IM_SLASH",
	"IM_BASH",
	"IM_STAB",
	"PET",
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
	"WILD_ONLY",
	"WILD_TOWN",
	"XXX8X02",
	"WILD_SHORE",
	"WILD_OCEAN",
	"WILD_WASTE",
	"WILD_WOOD",
	"WILD_VOLCANO",
	"XXX8X08",
	"WILD_MOUNTAIN",
	"WILD_GRASS",
	"XXX8X11",
	"XXX8X12",
	"XXX8X13",
	"XXX8X14",
	"XXX8X15",
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
	"WILD_SWAMP",
	"WILD_TOO",
};


/*
 * Monster race flags - Drops
 */
static cptr r_info_flags9[] =
{
	"DROP_CORPSE",
	"DROP_SKELETON",
	"HURT_POIS",
	"HURT_ELEC",
	"HURT_ACID",
	"EAT_BLIND",
	"EAT_CONF",
	"EAT_VOMIT",
	"EAT_TELEPORT",
	"RES_STON",
	"EAT_MANA",
	"EAT_NEXUS",
	"EAT_BLINK",
	"EAT_SLEEP",
	"EAT_BERSERKER",
	"EAT_INFLATE",
	"EAT_FORGET",
	"EAT_FIERY",   
        "EAT_ACIDIC",
        "EAT_SPEED",     
        "EAT_HEAL_CONF",
        "EAT_HEAL_FEAR", 
        "EAT_HEAL_STUN",
        "EAT_GIVE_ESP",
        "EAT_FIRE_RES",
        "EAT_COLD_RES",
        "EAT_ACID_RES",  
        "EAT_ELEC_RES",
        "EAT_POIS_RES",
        "EAT_INSANITY",  
        "EAT_DRAIN_EXP",
        "EAT_POISONOUS",
        "EAT_SUPER_POISONOUS",      /* 33 */
        "EAT_GIVE_STR",             /* 34 */
        "EAT_GIVE_DEX",             /* 35 */
        "EAT_GIVE_WIS",             /* 36 */
        "EAT_GIVE_INT",             /* 37 */
        "EAT_GIVE_CON",             /* 38 */
        "EAT_GIVE_CHR",             /* 39 */
        "EAT_LOSE_STR",             /* 40 */
        "EAT_LOSE_DEX",             /* 41 */
        "EAT_LOSE_WIS",             /* 42 */
        "EAT_LOSE_INT",             /* 43 */
        "EAT_LOSE_CON",             /* 44 */
        "EAT_LOSE_CHR",             /* 45 */
        "EAT_DRAIN_MANA",           /* 46 */
        "EAT_LOSE_SPEED",           /* 47 */
        "",                         /* 48 */
        "",                         /* 49 */
        "",                         /* 50 */
        "",                         /* 51 */
        "",                         /* 52 */
        "",                         /* 53 */
        "",                         /* 54 */
        "",                         /* 55 */
        "",                         /* 56 */
        "",                         /* 57 */
        "",                         /* 58 */
        "",                         /* 59 */
        "",                         /* 60 */
        "",                         /* 61 */
        "",                         /* 62 */
        "",                         /* 63 */
        ""                          /* 64 */
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
	"INVIS",
	"LIFE",
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
	"BRAND_COLD",
        "COULD2H",                  /* 33 */
        "MUST2H",                   /* 34 */
        "",                         /* 35 */
        "",                         /* 36 */
        "",                         /* 37 */
        "",                         /* 38 */
        "",                         /* 39 */
        "",                         /* 40 */
        "",                         /* 41 */
        "",                         /* 42 */
        "",                         /* 43 */
        "",                         /* 44 */
        "",                         /* 45 */
        "",                         /* 46 */
        "",                         /* 47 */
        "",                         /* 48 */
        "",                         /* 49 */
        "",                         /* 50 */
        "",                         /* 51 */
        "",                         /* 52 */
        "",                         /* 53 */
        "",                         /* 54 */
        "",                         /* 55 */
        "",                         /* 56 */
        "",                         /* 57 */
        "",                         /* 58 */
        "",                         /* 59 */
        "",                         /* 60 */
        "",                         /* 61 */
        "",                         /* 62 */
        "",                         /* 63 */
        ""                          /* 64 */
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
	"SUST_LUC",
	"NOBREAK",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"THROWING",
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
	"RES_DISEN",
        "RES_GRAV",
        "RES_FORCE",
        "RES_INERT",
        "",                         /* 36 */
        "",                         /* 37 */
        "",                         /* 38 */
        "",                         /* 39 */
        "",                         /* 40 */
        "",                         /* 41 */
        "",                         /* 42 */
        "",                         /* 43 */
        "",                         /* 44 */
        "",                         /* 45 */
        "",                         /* 46 */
        "",                         /* 47 */
        "",                         /* 48 */
        "",                         /* 49 */
        "",                         /* 50 */
        "",                         /* 51 */
        "",                         /* 52 */
        "",                         /* 53 */
        "",                         /* 54 */
        "",                         /* 55 */
        "",                         /* 56 */
        "",                         /* 57 */
        "",                         /* 58 */
        "",                         /* 59 */
        "",                         /* 60 */
        "",                         /* 61 */
        "",                         /* 62 */
        "",                         /* 63 */
        ""                          /* 64 */
};

/*
 * Object flags
 */
static cptr k_info_flags3[] =
{
	"SH_FIRE",
	"SH_ELEC",
	"QUESTITEM",
	"WEIRD_ATTACK",
	"NO_TELE",
	"NO_MAGIC",
	"WRAITH",
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
	"PERMA_CURSE",
	"SH_COLD",
	"SH_FEAR",
        "STAT_CURSE", 
        "CLONE_CURSE",        
        "TL_CURSE",
        "GOLD_CURSE",
        "DROP_SELF_CURSE",
        "VANISH_CURSE",
        "YELL_CURSE",         
        "",                         /* 42 */
        "",                         /* 43 */
        "",                         /* 44 */
        "",                         /* 45 */
        "",                         /* 46 */
        "",                         /* 47 */
        "",                         /* 48 */
        "",                         /* 49 */
        "",                         /* 50 */
        "",                         /* 51 */
        "",                         /* 52 */
        "",                         /* 53 */
        "",                         /* 54 */
        "",                         /* 55 */
        "",                         /* 56 */
        "",                         /* 57 */
        "",                         /* 58 */
        "",                         /* 59 */
        "",                         /* 60 */
        "",                         /* 61 */
        "",                         /* 62 */
        "",                         /* 63 */
        ""                          /* 64 */
};

/*
 * Feature flags
 */
static cptr f_info_flags1[] =
{
        "NO_WALK",
        "NO_VISION",
        "CAN_LEVITATE",
        "CAN_PASS",
        "FLOOR",
        "WALL",
        "PERMANENT",
        "CAN_FLY",
        "REMEMBER",
        "NOTICE",
        "NO_NOTICE_RUNNING",
        "CAN_RUN",
	"DOOR",
        "SUPPORT_LIGHT",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1",
        "XXX1"
};

/*
 * Trap flags
 */
static cptr t_info_flags[] =
{
   "CHEST",
   "DOOR",
   "FLOOR",
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
   "LEVEL1",
   "LEVEL2",
   "LEVEL3",
   "LEVEL4",
   "XXX21",
   "XXX22",
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
			if (v_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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
			if (v_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

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
			int typ, rat, hgt, wid, min, max;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
				&typ, &rat, &hgt, &wid, &min, &max)) return (1);

			/* Correct a value. */
			if (max == 0) max = MAX_DEPTH;

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;
			v_ptr->hgt = hgt;
			v_ptr->wid = wid;
			v_ptr->min_lev = min;
			v_ptr->max_lev = max;

			/* Next... */
			continue;
		}

		/* Monster indices */
		if (buf[0] == 'M')
		{
	   	    int num;
		    char *zz[9];

		    /* Tokenize the line */
		    if ((num = get_tokens(buf+2, 9, zz)) > 1)
		    {

		    switch (num)
		    {
			/* Special */
			case 9:
				v_ptr->mon[8] = test_monster_name(zz[8]);
			case 8:
				v_ptr->mon[7] = test_monster_name(zz[7]);
			case 7:
				v_ptr->mon[6] = test_monster_name(zz[6]);
			case 6:
				v_ptr->mon[5] = test_monster_name(zz[5]);
			case 5:
				v_ptr->mon[4] = test_monster_name(zz[4]);
			case 4:
				v_ptr->mon[3] = test_monster_name(zz[3]);
			case 3:
				v_ptr->mon[2] = test_monster_name(zz[2]);
			case 2:
				v_ptr->mon[1] = test_monster_name(zz[1]);
			default:
				v_ptr->mon[0] = test_monster_name(zz[0]);
		     }
		     }

		    /* Next... */
  		    continue;
		     
		}
				     		
		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	if (!start)
	{
		++v_head->name_size;
		++v_head->text_size;
	}


	/* No version yet */
	if (!okay) return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an feature_type from a textual string
 */
static errr grab_one_feature_flag(feature_type *f_ptr, cptr what)
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

	/* Oops */
	msg_format("Unknown feature flag '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Grab one flag in an trap_type from a textual string
 */
static errr grab_one_trap_type_flag(trap_type *t_ptr, cptr what)
{
	s16b i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, t_info_flags[i]))
		{
			t_ptr->flags |= (1L << i);
			return (0);
		}
	}
	/* Oops */
	msg_format("Unknown trap_type flag '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Initialize the "tr_info" array, by parsing an ascii "template" file
 */
errr init_t_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	trap_type *t_ptr = NULL;


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
			if (i >= t_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			t_ptr = &t_info[i];

			/* Hack -- Verify space */
			if (t_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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
		if (!t_ptr) return (3);


		/* Process 'I' for "Information" */
		if (buf[0] == 'I')
		{
			int probability, another, p1valinc, difficulty;
			int minlevel;
			int dd,ds;
			char color;

			/* Scan for the values */
			if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%dd%d:%c",
					&difficulty, &probability, &another,
					&p1valinc, &minlevel, &dd, &ds,
					&color)) return (1);

			t_ptr->difficulty  = (byte)difficulty;
			t_ptr->probability = (s16b)probability;
			t_ptr->another     = (s16b)another;
			t_ptr->p1valinc     = (s16b)p1valinc;
			t_ptr->minlevel    = (byte)minlevel;
			t_ptr->dd          = (s16b)dd;
			t_ptr->ds          = (s16b)ds;
			t_ptr->color       = color_char_to_attr(color);

			/* Next... */
			continue;
		}


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (t_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
			if (!t_ptr->text) t_ptr->text = ++t_head->text_size;

			/* Append chars to the name */
			strcpy(t_text + t_head->text_size, s);

			/* Advance the index */
			t_head->text_size += strlen(s);

			/* Next... */
			continue;
		}


		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{

                        t_ptr->flags = 0;

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
                                if (0 != grab_one_trap_type_flag(t_ptr, s)) return (5);

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
	++t_head->name_size;
	++t_head->text_size;


	/* No version yet */
	if (!okay) return (2);


	/* Success */
	return (0);
}


/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

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
			if (f_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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
			if (f_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

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

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0) return (1);

			/* Save the values */
			f_ptr->d_attr = tmp;
			f_ptr->d_char = buf[2];

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{

                        f_ptr->flags1 = 0;

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
                                if (0 != grab_one_feature_flag(f_ptr, s)) return (5);

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
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			k_ptr->flags1 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			k_ptr->flags2 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			k_ptr->flags3 |= (1LL << i);
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
	int i = -1;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	object_kind *k_ptr = NULL;


	/* Prepare the "fake" stuff */
	k_head->name_size = 0;
	k_head->text_size = 0;

        error_idx = 0;
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
			s = buf+2;

			/* Verify that colon */
			if (!s) return (1);

#if 0
			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);
#endif
			i++;

			/* Verify information */
			if (i >= k_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			k_ptr = &k_info[i];

			/* Hack -- Verify space */
			if (k_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (k_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

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
			int level, mhp, wgt;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
				&level, &mhp, &wgt, &cost)) return (1);

			/* Save the values */
			k_ptr->level = level;
			k_ptr->mhp = mhp;
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

			/* Next... */
			continue;
		}

		/* Process 'S' for "Stats" (one line only) */
		if (buf[0] == 'S')
		{
			int stat[7], f;

			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
				&stat[0], &stat[1], &stat[2], &stat[3],
				&stat[4], &stat[5], &stat[6])) return (1);

			/* Save the values */
			for (f=0;f<7;f++)
			{
			   k_ptr->to_stat[f] = stat[f];
			}

			/* Next... */
			continue;
		}

		/* Process 'A' for "Allocation" (one line only) */
		if (buf[0] == 'A')
		{
			int foo;

			/* XXX XXX XXX Simply read each number following a colon */
			for (foo = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++foo)
			{
				/* Default chance */
				k_ptr->chance[foo] = 1;

				/* Store the attack damage index */
				k_ptr->locale[foo] = atoi(s+1);

				/* Find the slash */
				t = strchr(s+1, '/');

				/* Find the next colon */
				s = strchr(s+1, ':');

				/* If the slash is "nearby", use it */
				if (t && (!s || t < s))
				{
					int chance = atoi(t+1);
					if (chance > 0) k_ptr->chance[foo] = chance;
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
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			a_ptr->flags1 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			a_ptr->flags2 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			a_ptr->flags3 |= (1LL << i);
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

	/* Start the "fake" stuff */
	a_head->name_size = 0;
	a_head->text_size = 0;

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
			if (a_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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
			if (a_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
			if (!a_ptr->text) a_ptr->text = ++a_head->text_size;
			else
			{
			if (*(a_text + a_head->text_size - 1) != ' ')
			  {
			    *(a_text + a_head->text_size) = ' ';
			    *(a_text + ++a_head->text_size) = 0;
			  }
			}

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

		/* Process 'S' for "Stats" (one line only) */
		if (buf[0] == 'S')
		{
			int stat[7], f;

			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
				&stat[0], &stat[1], &stat[2], &stat[3],
				&stat[4], &stat[5], &stat[6])) return (1);

			/* Save the values */
			for (f=0;f<7;f++)
			{
			   a_ptr->to_stat[f] = stat[f];
			}

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
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			e_ptr->flags1 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			e_ptr->flags2 |= (1LL << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			e_ptr->flags3 |= (1LL << i);
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
			if (e_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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
		if (!e_ptr) return (3);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (e_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

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
				&slot, &rating)) return (1);

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

			e_ptr->max_to_h = th;
			e_ptr->max_to_d = td;
			e_ptr->max_to_a = ta;
			e_ptr->max_pval = pv;

			/* Next... */
			continue;
		}

		/* Process 'S' for "Stats" (one line only) */
		if (buf[0] == 'S')
		{
			int stat[7], f;

			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
				&stat[0], &stat[1], &stat[2], &stat[3],
				&stat[4], &stat[5], &stat[6])) return (1);

			/* Save the values */
			for (f=0;f<7;f++)
			{
			   e_ptr->max_to_stat[f] = stat[f];
			}

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
	for (i = 0; i < 64; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
			r_ptr->flags9 |= (1LL << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Grab one (poly) flag in a monster_race from a textual string
 */
static errr grab_one_poly_flag(monster_race *r_ptr, cptr what)
{
	int i;

        if (streq(what, "HAS_ALL"))
        {
  	   r_ptr->r_polyinfo |= (HAS_ALL);
	   return (0);
        }
        
	/* Scan polyinfo */
	for (i = 0; i < 16; i++)
	{
		if (streq(what, r_poly_flags[i]))
		{
			r_ptr->r_polyinfo |= (1 << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown polymorph flag '%s'.", what);

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
	for (i = 0; i < 64; i++)
	{
		if (streq(what, r_info_flags4[i]))
		{
			r_ptr->flags4 |= (1LL << i);
			return (0);
		}
	}

	/* Scan flags5 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, r_info_flags5[i]))
		{
			r_ptr->flags5 |= (1LL << i);
			return (0);
		}
	}

	/* Scan flags6 */
	for (i = 0; i < 64; i++)
	{
		if (streq(what, r_info_flags6[i]))
		{
			r_ptr->flags6 |= (1LL << i);
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
errr init_r_info_txt(FILE *fp, char *buf)
{
	int i = -1;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	monster_race *r_ptr = NULL;


	/* Just before the first record */
	error_idx = 0;

	/* Just before the first line */
	error_line = 0;


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
			s = buf+2;

			/* Verify that colon */
			if (!s) return (1);

#if 0
			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
			i = atoi(buf+2);

#endif
			i++;

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
			if (i >= r_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			r_ptr = &r_info[i];

			/* Hack -- Verify space */
			if (r_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

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

                /* Humanoids have everything in place */
                if (strchr ("hPYOTsptokzALV", r_ptr->d_char))
                     r_ptr->r_polyinfo = HAS_ALL;

                /* Golems, aliens, demons - no fingers */
                else if (strchr ("gxuU", r_ptr->d_char))
                     r_ptr->r_polyinfo = 
                     (HAS_HEAD | HAS_TORSO | HAS_HANDS | HAS_FEET);
                
                /* Dragons have head, torso, fingers */
                else if ((r_ptr->d_char == 'd') || (r_ptr->d_char == 'D'))
                    r_ptr->r_polyinfo = HAS_DRAGON;

                /* nagas, ickys, xorns, ghosts, elems have torso, head 
                   and hands (?) */
                else if (strchr ("niXGE", r_ptr->d_char))
                    r_ptr->r_polyinfo = (HAS_TORSO | HAS_HEAD | HAS_HANDS);

                /* Other animals - only torso and head */
                else if (strchr ("CJKBZMRSabcfqr", r_ptr->d_char))
                    r_ptr->r_polyinfo = (HAS_TORSO | HAS_HEAD);

                /* Other low-life forms - only torso */
                else if (strchr ("QIjmvw,$", r_ptr->d_char))
                    r_ptr->r_polyinfo = (HAS_TORSO | HAS_HEAD);

                /* Eye monsters - head only */
                else if (r_ptr->d_char == 'e')
                    r_ptr->r_polyinfo = HAS_HEAD;
                
                /* Hybrids and wights have to be examined individually */
                
		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (r_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
			if (!r_ptr->text) r_ptr->text = ++r_head->text_size;
			else
			{
			if (*(r_text + r_head->text_size - 1) != ' ')
			  {
			    *(r_text + r_head->text_size) = ' ';
			    *(r_text + ++r_head->text_size) = 0;
			  }
			}

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
			int lev, rar;
                        int noapp1, noapp2;
			long exp, wgt;

			/* Scan for the values */
			if (6 != sscanf(buf+2, "%d:%d:%ld:%ld:%dd%d",
				&lev, &rar, &wgt, &exp, &noapp1, &noapp2)) 
                                return (1);

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
			r_ptr->weight = wgt;
			r_ptr->mexp = exp;
                        r_ptr->noapp[0] = (byte) noapp1;
                        r_ptr->noapp[1] = (byte) noapp2;

			/* Next... */
			continue;
		}

		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
		{
			int n1, n2, i;

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

		/* Process 'P' for "polymorph info" (multiple lines) */
		if (buf[0] == 'P')
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
				if (0 != grab_one_poly_flag(r_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Process 'S' for "Spell Flags" (multiple lines) */
		if (buf[0] == 'S')
		{
			int foo;

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
				if (1 == sscanf(s, "1_IN_%d", &foo))
				{
					/* Extract a "frequency" */
					r_ptr->freq_spell = r_ptr->freq_inate = 100 / foo;

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


	for (i = 1; i < max_r_idx; i++)
	{
		/* Invert flag WILD_ONLY <-> RF8_DUNGEON */
		r_info[i].flags8 ^= 1L;

		/* WILD_TOO without any other wilderness flags enables all flags */
		if ((r_info[i].flags8 & RF8_WILD_TOO) && !(r_info[i].flags8 & 0x7FFFFFFE))
			r_info[i].flags8 = 0x0463;
	}

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}


#else   /* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif  /* ALLOW_TEMPLATES */


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
	int             feature;                /* Terrain feature */
	int             monster;                /* Monster */
	int             object;                 /* Object */
	int             ego;                    /* Ego-Item */
	int             artifact;               /* Artifact */
	int             trap;                   /* Trap */
	int             cave_info;              /* Flags for CAVE_MARK, CAVE_GLOW, CAVE_ICKY, CAVE_ROOM */
	int             special;                /* Reserved for special terrain info */
	int             random;                 /* Number of the random effect */
};


typedef struct wilderness_grid wilderness_grid;

struct wilderness_grid
{
	int             terrain;    /* Terrain type */
	int             town;       /* Town number */
	byte    level;          /* Level of the wilderness */
	byte    road;       /* Road */
	char    name[32];       /* Name of the town/wilderness */
};


static dungeon_grid letter[255];

static wilderness_grid w_letter[255];



/*
 * Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:
 * <artifact>:<trap>:<special>" -- info for dungeon grid
 */
static errr parse_line_feature(char *buf)
{
	int num;
	char *zz[9];


	if (init_flags & INIT_ONLY_BUILDINGS) return (0);

	/* Tokenize the line */
	if ((num = get_tokens(buf+2, 9, zz)) > 1)
	{
		/* Letter to assign */
		int index = zz[0][0];

		/* Reset the info for the letter */
		letter[index].feature = 0;
		letter[index].monster = 0;
		letter[index].object = 0;
		letter[index].ego = 0;
		letter[index].artifact = 0;
		letter[index].trap = 0;
		letter[index].cave_info = 0;
		letter[index].special = 0;
		letter[index].random = 0;

		switch (num)
		{
			/* Special */
			case 9:
				letter[index].special = atoi(zz[8]);
				/* Fall through */
			/* Trap */
			case 8:
				if (zz[7][0] == '*')
				{
					letter[index].random |= RANDOM_TRAP;

					if (zz[7][1])
					{
						zz[7]++;
						letter[index].trap = atoi(zz[7]);
					}
				}
				else
				{
					letter[index].trap = atoi(zz[7]);
				}
				/* Fall through */
			/* Artifact */
			case 7:
				if (zz[6][0] == '*')
				{
					letter[index].random |= RANDOM_ARTIFACT;

					if (zz[6][1])
					{
						zz[6]++;
						letter[index].artifact = atoi(zz[6]);
					}
				}
				else
				{
					letter[index].artifact = atoi(zz[6]);
				}
				/* Fall through */
			/* Ego-item */
			case 6:
				if (zz[5][0] == '*')
				{
					letter[index].random |= RANDOM_EGO;

					if (zz[5][1])
					{
						zz[5]++;
						letter[index].ego = atoi(zz[5]);
					}
				}
				else
				{
					letter[index].ego = atoi(zz[5]);
				}
				/* Fall through */
			/* Object */
			case 5:
				if (zz[4][0] == '*')
				{
					letter[index].random |= RANDOM_OBJECT;

					if (zz[4][1])
					{
						zz[4]++;
						letter[index].object = atoi(zz[4]);
					}
				}
				else
				{
				        letter[index].object = test_object_name(zz[4]);
				}
				/* Fall through */
			/* Monster */
			case 4:
				if (zz[3][0] == '*')
				{
					letter[index].random |= RANDOM_MONSTER;
					if (zz[3][1])
					{
						zz[3]++;
						  letter[index].monster = atoi(zz[3]);
					}
				}
				else
				{
				       letter[index].monster = test_monster_name(zz[3]);
				}
				/* Fall through */
			/* Cave info */
			case 3:
				letter[index].cave_info = atoi(zz[2]);
				/* Fall through */
			/* Feature */
			case 2:
				if (zz[1][0] == '*')
				{
					letter[index].random |= RANDOM_FEATURE;
					if (zz[1][1])
					{
						zz[1]++;
						letter[index].feature = atoi(zz[1]);
					}
				}
				else
				{
					letter[index].feature = atoi(zz[1]);
				}
				break;
		}

		return (0);
	}

	return (1);
}


/*
 * Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid
 */
static errr parse_line_building(char *buf)
{
	int i;
	char *zz[33];
	int index;
	char *s;

	/* Find the colon after the building number */
	s = strchr(buf + 2, ':');

	/* Verify that colon */
	if (!s) return (1);

	/* Nuke the colon, advance to the sub-index */
	*s++ = '\0';

	/* Paranoia -- require a sub-index */
	if (!*s) return (1);

	/* Get the building number */
	index = atoi(buf + 2);

	/* Building definition sub-index */
	switch (s[0])
	{
		/* Building name, owner, race */
		case 'N':
		{
			if (get_tokens(s + 2, 3, zz) == 3)
			{
				/* Name of the building */
				strcpy(building[index].name, zz[0]);

				/* Name of the owner */
				strcpy(building[index].owner_name, zz[1]);

				/* Race of the owner */
				strcpy(building[index].owner_race, zz[2]);

				break;
			}

			return (1);
		}

		/* Building Action */
		case 'A':
		{
			if (get_tokens(s + 2, 8, zz) >= 7)
			{
				/* Index of the action */
				int action_index = atoi(zz[0]);

				/* Name of the action */
				strcpy(building[index].act_names[action_index], zz[1]);

				/* Cost of the action for members */
				building[index].member_costs[action_index] = atol(zz[2]);

				/* Cost of the action for non-members */
				building[index].other_costs[action_index] = atol(zz[3]);

				/* Letter assigned to the action */
				building[index].letters[action_index] = zz[4][0];

				/* Action code */
				building[index].actions[action_index] = atoi(zz[5]);

				/* Action restriction */
				building[index].action_restr[action_index] = atoi(zz[6]);

				break;
			}
			return (1);
		}

		/* Building Classes */
		case 'C':
		{
			if (get_tokens(s + 2, MAX_CLASS, zz) == MAX_CLASS)
			{
				for (i = 0; i < MAX_CLASS; i++)
				{
					building[index].member_class[i] = atoi(zz[i]);
				}

				break;
			}
			return (1);
		}

		/* Building Races */
		case 'R':
		{
			if (get_tokens(s+2, MAX_RACES, zz) == MAX_RACES)
			{
				for (i = 0; i < MAX_RACES; i++)
				{
					building[index].member_race[i] = atoi(zz[i]);
				}

				break;
			}
			return (1);
		}

		/* Building Realms */
		case 'M':
		{
			if (get_tokens(s+2, MAX_REALM, zz) == MAX_REALM)
			{
				for (i = 0; i < MAX_REALM; i++)
				{
					building[index].member_realm[i+1] = atoi(zz[i]);
				}

				break;
			}
			return (1);
		}

		default:
		{
			return (1);
		}
	}

	return (0);
}


/*
 * Parse a sub-file of the "extra info"
 */
static errr process_dungeon_file_aux(char *buf, int ymin, int xmin, int ymax, int xmax, int *y, int *x)
{
	int i;

	char *zz[33];


	/* Skip "empty" lines */
	if (!buf[0]) return (0);

	/* Skip "blank" lines */
	if (isspace(buf[0])) return (0);

	/* Skip comments */
	if (buf[0] == '#') return (0);

	/* Require "?:*" format */
	if (buf[1] != ':') return (1);


	/* Process "%:<fname>" */
	if (buf[0] == '%')
	{
		/* Attempt to Process the given file */
		return (process_dungeon_file(buf + 2, ymin, xmin, ymax, xmax));
	}

	/* Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid */
	if (buf[0] == 'F')
	{
		return parse_line_feature(buf);
	}

	/* Process "D:<dungeon>" -- info for the cave grids */
	else if (buf[0] == 'D')
	{
		object_type object_type_body;

		/* Acquire the text */
		char *s = buf + 2;

		/* Length of the text */
		int len = strlen(s);

		if (init_flags & INIT_ONLY_BUILDINGS) return (0);

		for (*x = xmin, i = 0; ((*x < xmax) && (i < len)); (*x)++, s++, i++)
		{
			/* Access the grid */
			cave_type *c_ptr = &cave[*y][*x];

			int idx = s[0];

			int object_index = letter[idx].object;
			int monster_index = letter[idx].monster;
			int random = letter[idx].random;
			int artifact_index = letter[idx].artifact;
			int ego_index = letter[idx].ego;

			/* Lay down a floor */
			c_ptr->feat = letter[idx].feature;

			/* Only the features */
			if (init_flags & INIT_ONLY_FEATURES) continue;

			/* Cave info */
			c_ptr->info = letter[idx].cave_info;

			/* Create a monster */
			if (random & RANDOM_MONSTER)
			{
				monster_level = base_level + monster_index;

				place_monster(*y, *x, TRUE, TRUE);

				monster_level = base_level;
			}
			else if (monster_index)
			{
				/* Make alive again */
				if (r_info[monster_index].flags1 & RF1_UNIQUE)
				{
					r_info[monster_index].cur_num = 0;
					r_info[monster_index].max_num = 1;
				}

				/* Make alive again */
				if (r_info[monster_index].flags3 & RF3_UNIQUE_7)
				{
					if (r_info[monster_index].cur_num == 7)
						r_info[monster_index].cur_num = 6;
					r_info[monster_index].max_num = 7;
				}

				/* Place it */
				place_monster_aux(*y, *x, monster_index, TRUE, FALSE, FALSE, FALSE);
			}

			/* Object (and possible trap) */
			if ((random & RANDOM_OBJECT) && (random & RANDOM_TRAP))
			{
				object_level = base_level + object_index;

				/*
				 * Random trap and random treasure defined
				 * 25% chance for trap and 75% chance for object
				 */
				if (rand_int(100) < 75)
				{
					place_object(*y, *x, FALSE, FALSE);
				}
				else
				{
					place_trap(*y, *x);
				}

				object_level = base_level;
			}
			else if (random & RANDOM_OBJECT)
			{
				object_level = base_level + object_index;

				/* Create an out of deep object */
				if (rand_int(100) < 75)
					place_object(*y, *x, FALSE, FALSE);
				else if (rand_int(100) < 80)
					place_object(*y, *x, TRUE, FALSE);
				else
					place_object(*y, *x, TRUE, TRUE);

				object_level = base_level;
			}
			/* Random trap */
			else if (random & RANDOM_TRAP)
			{
				place_trap(*y, *x);
			}
			else if (object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				/* Create the item */
				object_prep(o_ptr, object_index);
				
				if (ego_index) o_ptr->name2 = ego_index;
				
    				if (o_ptr->tval == TV_GOLD)
				{
					coin_type = object_index - OBJ_GOLD_LIST;
					make_gold(o_ptr);
					coin_type = 0;
				}

				/* Apply magic (no messages, no artifacts) */
				apply_magic(o_ptr, base_level, FALSE, TRUE, FALSE);

				drop_near(o_ptr, -1, *y, *x);
			}

			/* Artifact */
			if (artifact_index)
			{
				/* Create the artifact */
				create_named_art(artifact_index, *y, *x);

				a_info[artifact_index].cur_num = 1;
			}

			/* Terrain special */
			c_ptr->special = letter[idx].special;
		}

		(*y)++;

		return (0);
	}

	/* Process "Q:<number>:<command>:... -- quest info */
	else if (buf[0] == 'Q')
	{
		int num = get_tokens(buf + 2, 33, zz);
		quest_type *q_ptr;

		/* Have we enough parameters? */
		if (num < 3) return (1);

		/* Get the quest */
		q_ptr = &(quest[atoi(zz[0])]);

		/* Process "Q:<q_index>:Q:<type>:<num_mon>:<cur_num>:<max_num>:<level>:<r_idx>:<k_idx>:<flags>" -- quest info */
		if (zz[1][0] == 'Q')
		{
			if (init_flags & INIT_ASSIGN)
			{
				monster_race *r_ptr;
				artifact_type *a_ptr;

				if (num < 9) return (1);

				q_ptr->type    = atoi(zz[2]);
				q_ptr->num_mon = atoi(zz[3]);
				q_ptr->cur_num = atoi(zz[4]);
				q_ptr->max_num = atoi(zz[5]);
				q_ptr->level   = atoi(zz[6]);
				q_ptr->r_idx   = test_monster_name(zz[7]);
				q_ptr->k_idx   = atoi(zz[8]);

				if (num > 9)
					q_ptr->flags  = atoi(zz[9]);

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

		/* Process "Q:<q_index>:R:<reward>" -- quest name */
		else if (zz[1][0] == 'R')
		{
			if (init_flags & (INIT_ASSIGN | INIT_SHOW_TEXT))
			{
				q_ptr->reward = test_object_name(zz[2]);
                                q_ptr->reward_num = atoi(zz[3]);
                                q_ptr->questx = atoi(zz[4]);
                                q_ptr->questy = atoi(zz[5]);
                                q_ptr->townnum = atoi(zz[6]);
                                q_ptr->next_quest = atoi(zz[7]);
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

	/* Process "W:<command>: ..." -- info for the wilderness */
	else if (buf[0] == 'W')
	{
		/* Process "W:D:<layout> */
		/* Layout of the wilderness */
		if (buf[2] == 'D')
		{
			/* Acquire the text */
			char *s = buf+4;

			/* Length of the text */
			int len = strlen(s);

			for (*x = xmin, i = 0; ((*x < xmax) && (i < len)); (*x)++, s++, i++)
			{
				int idx = s[0];

				wilderness[*y][*x].terrain = w_letter[idx].terrain;

				wilderness[*y][*x].level = w_letter[idx].level;

				wilderness[*y][*x].town = w_letter[idx].town;

				wilderness[*y][*x].road = w_letter[idx].road;

				strcpy(wilderness[*y][*x].name, w_letter[idx].name);
			}

			(*y)++;

			return (0);
		}
		/* Process "W:F:<letter>:<terrain>:<town>:<road>:<name> */
		else if (buf[2] == 'F')
		{
			int num;

			if ((num = get_tokens(buf+4, 6, zz)) > 1)
			{
				int index = zz[0][0];

				if (num > 1)
					w_letter[index].terrain = atoi(zz[1]);
				else
					w_letter[index].terrain = 0;

				if (num > 2)
					w_letter[index].level = atoi(zz[2]);
				else
					w_letter[index].level = 0;

				if (num > 3)
					w_letter[index].town = atoi(zz[3]);
				else
					w_letter[index].town = 0;

				if (num > 4)
					w_letter[index].road = atoi(zz[4]);
				else
					w_letter[index].road = 0;

				if (num > 5)
					strcpy(w_letter[index].name, zz[5]);
				else
					w_letter[index].name[0] = 0;

				return (0);
			}
		}
		/* Process "W:P:<x>:<y> - starting position in the wilderness */
		else if (buf[2] == 'P')
		{
			if ((p_ptr->wilderness_x == 0) &&
			    (p_ptr->wilderness_y == 0))
			{
				if (get_tokens(buf+4, 2, zz) == 2)
				{
					p_ptr->wilderness_x = atoi(zz[0]);
					p_ptr->wilderness_y = atoi(zz[1]);
				}
				else
				{
					return (1);
				}
			}

			return (0);
		}
	}

	/* Process "P:<y>:<x>" -- player position */
	else if (buf[0] == 'P')
	{
		if (init_flags & INIT_CREATE_DUNGEON)
		{
			if (get_tokens(buf + 2, 2, zz) == 2)
			{
				int panels_x, panels_y;

				/* Hack - Set the dungeon size */
				panels_y = (*y / SCREEN_HGT);
				if (*y % SCREEN_HGT) panels_y++;
				cur_hgt = panels_y * SCREEN_HGT;

				panels_x = (*x / SCREEN_WID);
				if (*x % SCREEN_WID) panels_x++;
				cur_wid = panels_x * SCREEN_WID;

				/* Choose a panel row */
				max_panel_rows = (cur_hgt / SCREEN_HGT) * 2 - 2;
				if (max_panel_rows < 0) max_panel_rows = 0;

				/* Choose a panel col */
				max_panel_cols = (cur_wid / SCREEN_WID) * 2 - 2;
				if (max_panel_cols < 0) max_panel_cols = 0;

				/* Assume illegal panel */
				panel_row = max_panel_rows;
				panel_col = max_panel_cols;

				/* Place player in a quest level */
				if (p_ptr->inside_quest)
				{
					py = atoi(zz[0]);
					px = atoi(zz[1]);

					/* Delete the monster (if any) */
					delete_monster(py, px);
				}
				/* Place player in the town */
				else if (!p_ptr->oldpx && !p_ptr->oldpy)
				{
					p_ptr->oldpy = atoi(zz[0]);
					p_ptr->oldpx = atoi(zz[1]);
				}
			}
		}

		return (0);
	}

	/* Process "B:<Index>:<Command>:..." -- Building definition */
	else if (buf[0] == 'B')
	{
		return parse_line_building(buf);
	}

	/* Process "M:<type>:<maximum>" -- set maximum values */
	else if (buf[0] == 'M')
	{
		if (get_tokens(buf+2, 2, zz) == 2)
		{
			/* Maximum towns */
			if (zz[0][0] == 'T')
			{
				max_towns = atoi(zz[1]);
			}

			/* Maximum quests */
			else if (zz[0][0] == 'Q')
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

			/* Maximum tr_idx */
			else if (zz[0][0] == 'U')
			{
				max_t_idx = atoi(zz[1]); 
			}

			/* Wilderness size */
			else if (zz[0][0] == 'W')
			{
				/* Maximum wild_x_size */
				if (zz[0][1] == 'X')
					max_wild_x = atoi(zz[1]);
				/* Maximum wild_y_size */
				if (zz[0][1] == 'Y')
					max_wild_y = atoi(zz[1]);
			}

			return (0);
		}
	}


	/* Failure */
	return (1);
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
				if (*t && streq(t, "1")) v = "0";
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


errr process_dungeon_file(cptr name, int ymin, int xmin, int ymax, int xmax)
{
	FILE *fp;

	char buf[1024];

	int num = -1;

	errr err = 0;

	bool bypass = FALSE;

	int x = xmin, y = ymin;


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
			(void)process_dungeon_file(buf + 2, ymin, xmin, ymax, xmax);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_dungeon_file_aux(buf, ymin, xmin, ymax, xmax, &y, &x);

		/* Oops */
		if (err) break;
	}


	/* Error */
	if (err)
	{
		/* Useful error message */
		msg_format("Error %d in line %d of file '%s'.", err, num, name);
		msg_format("Parsing '%s'", buf);
	}

	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}
