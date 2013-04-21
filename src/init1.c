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


/*
 * Hack -- error tracking
 */
extern s16b error_idx;
extern s16b error_line;


/*
 * Hack -- size of the "fake" arrays
 */
extern u16b fake_name_size;
extern u32b fake_text_size;



/*** Helper arrays for parsing ascii template files ***/

/*
 * Monster Blow Methods
 */
static cptr r_info_blow_method[] = {
	"",
	"HIT",
	"TOUCH",
	"PUNCH",
	"KICK",
	"CLAW",
	"BITE",
	"STING",
	"RIDDLE",
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
	"XXX5",
	"HALT",
	"DISGUST",
	"PROJECT",
	"EXPLODE",
	NULL
};


/*
 * Monster Blow Effects
 */
static cptr r_info_blow_effect[] = {
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
	"INSANITY",
	NULL
};


/*
 * Monster race flags
 */
static cptr r_info_flags1[] = {
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
static cptr r_info_flags2[] = {
	"STUPID",
	"SMART",
	"QUESTOR2",	/* -KMW- */
	"INNOCENT",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"INSTAPET",
	"DEEPLAVA",	/* -KMW- */
	"POWERFUL",
	"SWIM",	/* -KMW- */
	"FLY", /* -KMW- */
	"AQUATIC", /* -KMW- */
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
static cptr r_info_flags3[] = {
	"ORC",
	"TROLL",
	"GIANT",
	"DRAGON",
	"DEMON",
	"UNDEAD",
	"EVIL",
	"ANIMAL",
	"RANDOM_NAME",
	"FRIENDLY",
	"SILENT",
	"POLICE",
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"IM_POIS",
	"STAR_GOOD",
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
static cptr r_info_flags4[] = {
	"SHRIEK",
	"XXX2X4",
	"XXX3X4",
	"XXX4X4",
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
	"BR_QUAKE",
	"XXX6X4",
	"XXX7X4",
	"XXX8X4"
};

/*
 * Monster race flags
 */
static cptr r_info_flags5[] = {
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
static cptr r_info_flags6[] = {
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
	"S_UNDEAD_KOBOLD",
	"S_KOBOLD",
	"S_GOOD_UNIQUE",
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
	"S_UNIQUE"
};


/*
 * Monster race flags
 */
static cptr r_info_flags7[] = {
	"RES_STAB",
	"IMM_STAB",
	"RES_SLASH",
	"IMM_SLASH",
	"RES_BASH",
	"IMM_BASH",
	"ENDGAME",
	"UNFAIR",
	"NO_CORPSE",
	"HEADLESS",
	"HAS_SCALES",
	"HAS_WINGS",
	"HUMANOID",
	"KAMIKAZE",
	"KOBOLD",
	"XXX16X7",
	"XXX17X7",
	"XXX18X7",
	"XXX19X7",
	"XXX20X7",
	"XXX21X7",
	"XXX22X7",
	"XXX23X7",
	"XXX24X7",
	"XXX25X7",
	"XXX26X7",
	"XXX27X7",
	"XXX28X7",
	"XXX29X7",
	"XXX30X7",
	"XXX31X7",
	"XXX32X7"
};


/*
 * Object flags
 */
static cptr k_info_flags1[] = {
	"STR",
	"INT",
	"WIS",
	"DEX",
	"CON",
	"CHR",
	"EXPLODES",
	"PREFIX_NAME",
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
	"TRANSMUTE",
	"XXX6",
	"BRAND_POIS", /* from GJW -KMW- */
	"BRAND_ACID",
	"BRAND_ELEC",
	"BRAND_FIRE",
	"BRAND_COLD"
};

/*
 * Object flags
 */
static cptr k_info_flags2[] = {
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	"PLUS_TO_HIT",
	"PLUS_TO_DAM",
	"PLUS_TO_AC",
	"SPECIAL_GEN",
	"EASY_PVAL",
	"UNCURSED",
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
static cptr k_info_flags3[] = {
	"SLOW_DIGEST",
	"FEATHER",
	"LITE",
	"REGEN",
	"TELEPATHY",
	"SEE_INVIS",
	"FREE_ACT",
	"HOLD_LIFE",
	"QUESTITEM", /* -KMW- */
	"MUNCHKINISH",
	"WEIRD_ATTACK",
	"SPECIAL",
	"IMPACT",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"XXX7",
	"XXX8",
	"XXX9",
	"XXX10",
	"FLYING",
	"VAMPIRIC",
	"BLESSED",
	"ACTIVATE",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"DESC_SIMPLE",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};


/*
 * Spellbooks.
 */

static cptr s_info_books[] = {
	"MAGE",
	"PRIEST",
	"ILLUSIONIST",
	"ROGUE",
	"RANGER",
	"EVIL",
	"BARD",
	"POWER",
	"ACTIVATION",
	"NECRO",
	"ELEMENTAL",
	NULL
};

/*
 * Spell classifications.
 */

static cptr s_info_classifications[] = {
	"NORMAL",
	"SHAPE_ABOMINATION",
	"SHAPE_WOLF",
	"SHAPE_GHOST",
	"SHAPE_INSECT",
	"SHAPE_GOAT",
	"SHAPE_APE",
	"SHAPE_STATUE",
	"SHAPE_CHAOS_CLOUD",
	"SHAPE_SPARROW",
	"SHAPE_KOBOLD",
	"SHAPE_FIRE_CLOUD",
	"SHAPE_COLD_CLOUD",
	"SHAPE_DRAGON",
	"SHAPE_DEMON",
	"SHAPE_HOUND",
	"SHAPE_VAMPIRE",
	"SHAPE_QUYL",
	"SHAPE_ANGEL",
	"SHAPE_SERPENT",
	"SHAPE_MANA_BALL",
	"SHAPE_GIANT",
	"SHAPE_SPIDER",
	"SHAPE_MOLD",
	"SHAPE_ZOMBIE",
	"SHAPE_WRAITH",
	NULL
};

/*
 * Flags to ``project''.
 */

static cptr s_info_flags[32] = {
	"KILL",
	"ITEM",
	"GRID",

	"STOP",
	"BEAM",

	"ETHER",

	"VIEWABLE",
	"METEOR_SHOWER",
	"PLAYER",
	"BLAST",

	"PANEL",
	"ALL",
	"GENOCIDE",
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
	"XXX17",
	"XXX18",
	"XXX19",
	"XXX20",
	"XXX21",
	"XXX22",
	"XXX23"
};


/*
 * Spell attack types. (i.e. GF_FOO)
 */

static cptr s_info_attack_types[] = {
	"",
	"NOTHING",
	"ARROW",
	"MISSILE",
	"MANA",
	"HOLY_ORB",
	"LITE_WEAK",
	"DARK_WEAK",
	"WATER",
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
	"POIS",
	"XXX2",
	"LITE",
	"DARK",
	"XXX3",
	"CONFUSION",
	"SOUND",
	"SHARD",
	"NEXUS",
	"NETHER",
	"CHAOS",
	"DISENCHANT",
	"XXX4",
	"KILL_WALL",
	"KILL_DOOR",
	"KILL_TRAP",
	"MAKE_WALL",
	"MAKE_DOOR",
	"MAKE_TRAP",
	"MAKE_GLYPH",
	"MAKE_STAIR",
	"AWAY_UNDEAD",
	"AWAY_EVIL",
	"AWAY_ALL",
	"TURN_UNDEAD",
	"TURN_EVIL",
	"TURN_ALL",
	"DISP_UNDEAD",
	"DISP_EVIL",
	"DISP_ALL",
	"WORD_OF_DESTRUCTION",
	"CLONE",
	"POLY",
	"HEAL",
	"SPEED",
	"SLOW",
	"CONF",
	"SLEEP",
	"DRAIN",
	"QUAKE",
	"WALL_TO_CHAOS",
	"MIND_BLAST",
	"BRAIN_SMASH",
	"CHAOS_DESTRUCTION",
	"EARTHQUAKE",

	"AWAY_ALL_VERT",
	"RECALL",
	"ALTER",
	"DETECT_DOOR",
	"DETECT_TRAP",
	"DETECT_STAIR",
	"DETECT_TREASURE",
	"DETECT_GOLD",
	"DETECT_OBJECT",
	"DETECT_MAGIC",
	"DETECT_MONSTER",
	"DETECT_INVIS",
	"DETECT_EVIL",
	"DETECT_ANY",
	"HEAL_INSANITY",
	"HEAL_FEAR",
	"HEAL_CUT",
	"HEAL_STUN",
	"HEAL_POISON",
	"HEAL_DEX",
	"HEAL_CHR",
	"HEAL_STR",
	"HEAL_CON",
	"HEAL_WIS",
	"HEAL_INT",
	"HEAL_LIFE",
	"SHIELD",
	"HEROISM",
	"BLESS",
	"FOOD",
	"UNCURSE",
	"RECHARGE",
	"HEAVY_UNCURSE",
	"IDENT",
	"HEAVY_IDENT",
	"SUPER_IDENT",
	"RES_FIRE",
	"RES_COLD",
	"RES_ELEC",
	"RES_ACID",
	"RES_POIS",
	"GENOCIDE",
	"MASS_GENOCIDE",
	"ENCHANT_TO_HIT",
	"ENCHANT_TO_DAM",
	"ENCHANT_AC",
	"BRAND_AMMO",
	"BRAND_WEAPON",
	"MAKE_MONSTER",
	"MAKE_PET",
	"HEAL_CONF",
	"DETECT_GRIDS",
	"PROT_EVIL",
	"RING_OF_POWER",
	"HEAL_BLIND",
	"WAKE",
	"CURSE_ARMOR",
	"CURSE_WEAPON",
	"LEARN_RECIPE",
	"ENCHANT_EGO_ITEM",
	"ENCHANT_ARTIFACT",
	"MAKE_ITEM",
	"RUINATION",
	"DEC_STR",
	"DEC_INT",
	"DEC_WIS",
	"DEC_DEX",
	"DEC_CON",
	"DEC_CHR",
	"INFRA",
	"SEE_INVIS",
	"BERSERK",
	"HEAL_MANA",
	"INC_STR",
	"INC_INT",
	"INC_WIS",
	"INC_DEX",
	"INC_CON",
	"INC_CHR",
	"HEAL_MUTATION",
	"CAUSE_MUTATION",
	"CAUSE_INSANITY",
	"CAUSE_FEAR",
	"CAUSE_CUT",
	"CAUSE_STUN",
	"CAUSE_POISON",
	"HEAL_HALLUC",
	"CAUSE_HALLUC",
	"GAIN_EXP",
	"GLOWING_HANDS",
	"CAUSE_CONF",
	"CAUSE_BLIND",
	"RANDOM",
	"MINUS_FOOD",
	"RAISE_DEAD",
	"SOUL_PRISON",
	"REPAIR",
	"TRANSMUTE",
	"HEAVY_TRANSMUTE",
	NULL
};


/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
static int color_char_to_attr(char c)
{
	switch (c)
	{
		case 'd':
			return (TERM_DARK);
		case 'w':
			return (TERM_WHITE);
		case 's':
			return (TERM_SLATE);
		case 'o':
			return (TERM_ORANGE);
		case 'r':
			return (TERM_RED);
		case 'g':
			return (TERM_GREEN);
		case 'b':
			return (TERM_BLUE);
		case 'u':
			return (TERM_UMBER);

		case 'D':
			return (TERM_L_DARK);
		case 'W':
			return (TERM_L_WHITE);
		case 'v':
			return (TERM_VIOLET);
		case 'y':
			return (TERM_YELLOW);
		case 'R':
			return (TERM_L_RED);
		case 'G':
			return (TERM_L_GREEN);
		case 'B':
			return (TERM_L_BLUE);
		case 'U':
			return (TERM_L_UMBER);
	}

	return (-1);
}


/*
 * Simple run-length encoding.
 *
 * Note that there is no bounds checking!
 */
s16b run_length_encode(char *to, char *from)
{
	s16b len = 0;
	int i, j = 0;

	char data = from[0];
	byte count = 1;

	for (i = 1; from[i]; i++)
	{

		/* A run is broken, or run is too long. */
		if (from[i] != data || count == MAX_UCHAR)
		{

			/* Write the data. */
			to[j++] = data;
			to[j++] = count;

			data = from[i];
			count = 1;

			len += 2;

		}
		else
		{
			count++;
		}
	}

	/* Write the remainder. */
	to[j++] = data;
	to[j++] = count;

	len += 2;

	/* Append a NULL. */
	to[j++] = '\0';

	return len;
}




/*** Initialize from ascii template files ***/


/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr init_v_info_txt(FILE * fp, char *buf)
{
	int i; /* -KMW- */

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
	v_head->text2_size = 0;

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);

		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != v_head->v_major) || (v2 != v_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx)
				return (4);

			/* Verify information */
			if (i >= v_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			v_ptr = &v_info[i];

			/* Hack -- Verify space */
			if (v_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!v_ptr->name)
				v_ptr->name = ++v_head->name_size;

			/* Append chars to the name */
			strcpy(v_name + v_head->name_size, s);

			/* Advance the index */
			v_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current v_ptr */
		if (!v_ptr)
			return (3);

		/* Process monster, item and level info for special levels */

		if (buf[0] == 'Y')
		{
			int i;
			char *foo = buf + 2;
			char *bar;

			/* Fetch up to ten monster indexes */

			for (i = 0; i < 10; i++)
			{
				bar = strchr(foo, ':');

				if (bar == NULL)
				{
					v_ptr->mon[i] = atoi(foo);
					break;
				}

				bar[0] = '\0';
				v_ptr->mon[i] = atoi(foo);
				foo = bar + 1;
			}

			/* Next... */
			continue;
		}


		/* Get the quest info */

		if (buf[0] == 'Q')
		{
			int foo, bar, baz;

			if (3 != sscanf(buf + 2, "%d:%d:%d", &foo, &bar, &baz))
				return (1);

			v_ptr->q_idx = foo;
			v_ptr->q_type = bar;
			v_ptr->q_kind = baz;
			continue;
		}

		/* Get quest generation info. */
		if (buf[0] == 'G')
		{
			int foo;

			if (1 != sscanf(buf + 2, "%d", &foo))
				return 1;

			v_ptr->gen_info = foo;
			continue;
		}

		/* Get quest text */

		if (buf[0] == 'U')
		{
			s = buf + 2;

			/* Verify space */
			if (v_head->text2_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and save the text index */
			if (!v_ptr->q_text)
				v_ptr->q_text = ++v_head->text2_size;

			/* Append to the name */
			strcpy(q_text + v_head->text2_size, s);

			/* Advance the index */
			v_head->text2_size += strlen(s);

			/* Next */
			continue;
		}

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			int len;

			/* Acquire the text */
			s = buf + 2;

			/* Advance and Save the text index */
			if (!v_ptr->text)
				v_ptr->text = ++v_head->text_size;

			/* Append data using simple run-length encoding. */
			len = run_length_encode(v_text + v_head->text_size, s);

			/* Hack -- Verify space */
			if (v_head->text_size + len + 8 > fake_text_size)
				return (7);

			/* Advance the index */
			v_head->text_size += len;

			/* Next... */
			continue;
		}

		/* Process `M' for monsters/items. */
		if (buf[0] == 'M')
		{
			int len;

			/* Acquire the text */
			s = buf + 2;

			/* Advance and Save the text index */
			if (!v_ptr->m_text)
				v_ptr->m_text = ++v_head->text3_size;

			/* Append data using simple run-length encoding. */
			len = run_length_encode(vm_text + v_head->text3_size, s);

			/* Hack -- Verify space */
			if (v_head->text3_size + len + 8 > fake_text_size)
				return (7);

			/* Advance the index */
			v_head->text3_size += len;

			/* Next... */
			continue;
		}


		/* Process 'X' for "Extra info" (one line only) */
		if (buf[0] == 'X')
		{
			int typ, rat, hgt, wid;

			/* Scan for the values */
			if (4 != sscanf(buf + 2, "%d:%d:%d:%d", &typ, &rat, &hgt,
					&wid)) return (1);

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;
			v_ptr->hgt = hgt;
			v_ptr->wid = wid;

			/* Next... */
			continue;
		}


		/* Oops */
		return (6);
	}

	/* Complete the "name" and "text" sizes */
	++v_head->name_size;
	v_head->text_size += 2;
	++v_head->text2_size;

	/* No version yet */
	if (!okay)
		return (2);

	/* Success */
	return (0);
}



/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE * fp, char *buf)
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
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != f_head->v_major) || (v2 != f_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx)
				return (4);

			/* Verify information */
			if (i >= f_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			f_ptr = &f_info[i];

			/* Hack -- Verify space */
			if (f_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!f_ptr->name)
				f_ptr->name = ++f_head->name_size;

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
		if (!f_ptr)
			return (3);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf + 2;

			/* Hack -- Verify space */
			if (f_head->text_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and Save the text index */
			if (!f_ptr->text)
				f_ptr->text = ++f_head->text_size;

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
			if (1 != sscanf(buf + 2, "%d", &mimic))
				return (1);

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
			if (!buf[2])
				return (1);
			if (!buf[3])
				return (1);
			if (!buf[4])
				return (1);

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);
			if (tmp < 0)
				return (1);

			/* Save the values */
			f_ptr->f_char = buf[2];
			f_ptr->f_attr = tmp;

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
	if (!okay)
		return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind * k_ptr, cptr what)
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
errr init_k_info_txt(FILE * fp, char *buf)
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
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != k_head->v_major) || (v2 != k_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i <= error_idx)
				return (4);

			/* Verify information */
			if (i >= k_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			k_ptr = &k_info[i];

			/* Hack -- Verify space */
			if (k_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!k_ptr->name)
				k_ptr->name = ++k_head->name_size;

			/* Append chars to the name */
			strcpy(k_name + k_head->name_size, s);

			/* Advance the index */
			k_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current k_ptr */
		if (!k_ptr)
			return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf + 2;

			/* Hack -- Verify space */
			if (k_head->text_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and Save the text index */
			if (!k_ptr->text)
				k_ptr->text = ++k_head->text_size;

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
			if (!buf[2])
				return (1);
			if (!buf[3])
				return (1);
			if (!buf[4])
				return (1);

			/* Extract the char */
			sym = buf[2];

			/* Extract the attr */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0)
				return (1);

			/* Save the values */
			k_ptr->k_char = sym;
			k_ptr->k_attr = tmp;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int tval, sval, pval;

			/* Scan for the values */
			if (3 != sscanf(buf + 2, "%d:%d:%d", &tval, &sval, &pval))
				return (1);

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
			if (4 != sscanf(buf + 2, "%d:%d:%d:%ld", &level, &mhp, &wgt,
					&cost)) return (1);

			/* Save the values */
			k_ptr->level = level;
			k_ptr->mhp = mhp;
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

			/* Next... */
			continue;
		}

		/* Process 'M' for Material. */
		if (buf[0] == 'M')
		{
			int stuff;

			if (1 != sscanf(buf + 2, "%d", &stuff))
				return 1;

			k_ptr->stuff = stuff;

			continue;
		}


		/* Process 'A' for "Allocation" (one line only) */
		if (buf[0] == 'A')
		{
			int i;

			/* XXX XXX XXX Simply read each number following a colon */
			for (i = 0, s = buf + 1; s && (s[0] == ':') && s[1]; ++i)
			{
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
					if (chance > 0)
						k_ptr->chance[i] = chance;
				}
			}

			/* Next... */
			continue;
		}

		/* Process 'Z' for "actization" */
		if (buf[0] == 'Z')
		{
			int ind, trand, tstat;

			if (3 != sscanf(buf + 2, "%d:%d:%d", &ind, &trand, &tstat))
				return (1);

			k_ptr->activation = ind;
			k_ptr->timeout_rand = trand;
			k_ptr->timeout_static = tstat;

			/* Next. */
			continue;
		}

		/* Hack -- Process 'P' for "power" and such */
		if (buf[0] == 'P')
		{
			int ac, hd1, hd2, th, td, ta;

			/* Scan for the values */
			if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d", &ac, &hd1, &hd2,
					&th, &td, &ta))
				return (1);

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
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|');
					++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|')
						t++;
				}

				/* Parse this entry */
				if (0 != grab_one_kind_flag(k_ptr, s))
					return (5);

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
	if (!okay)
		return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type * a_ptr, cptr what)
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
errr init_a_info_txt(FILE * fp, char *buf)
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
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != a_head->v_major) || (v2 != a_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i < error_idx)
				return (4);

			/* Verify information */
			if (i >= a_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			a_ptr = &a_info[i];

			/* Hack -- Verify space */
			if (a_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!a_ptr->name)
				a_ptr->name = ++a_head->name_size;

			/* Append chars to the name */
			strcpy(a_name + a_head->name_size, s);

			/* Advance the index */
			a_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current a_ptr */
		if (!a_ptr)
			return (3);


		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf + 2;

			/* Hack -- Verify space */
			if (a_head->text_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and Save the text index */
			if (!a_ptr->text)
				a_ptr->text = ++a_head->text_size;

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
			if (3 != sscanf(buf + 2, "%d:%d:%d", &tval, &sval, &pval))
				return (1);

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
			if (4 != sscanf(buf + 2, "%d:%d:%d:%ld", &level, &rarity, &wgt,
					&cost)) return (1);

			/* Save the values */
			a_ptr->level = level;
			a_ptr->rarity = rarity;
			a_ptr->weight = wgt;
			a_ptr->cost = cost;

			/* Next... */
			continue;
		}

		/* Process 'Z' for "actization" */
		if (buf[0] == 'Z')
		{
			int ind, trand, tstat;

			if (3 != sscanf(buf + 2, "%d:%d:%d", &ind, &trand, &tstat))
				return (1);

			a_ptr->activation = ind;
			a_ptr->timeout_rand = trand;
			a_ptr->timeout_static = tstat;

			/* Next. */
			continue;
		}

		/* Hack -- Process 'P' for "power" and such */
		if (buf[0] == 'P')
		{
			int ac, hd1, hd2, th, td, ta;

			/* Scan for the values */
			if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d", &ac, &hd1, &hd2,
					&th, &td, &ta))
				return (1);

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
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|');
					++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while ((*t == ' ') || (*t == '|'))
						t++;
				}

				/* Parse this entry */
				if (0 != grab_one_artifact_flag(a_ptr, s))
					return (5);

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
	if (!okay)
		return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type * e_ptr, cptr what,
	bool rand)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
			if (rand)
				e_ptr->maybe_flags1 |= (1L << i);
			else
				e_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
			if (rand)
				e_ptr->maybe_flags2 |= (1L << i);
			else
				e_ptr->flags2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
			if (rand)
				e_ptr->maybe_flags3 |= (1L << i);
			else
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
errr init_e_info_txt(FILE * fp, char *buf)
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
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != e_head->v_major) || (v2 != e_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i < error_idx)
				return (4);

			/* Verify information */
			if (i >= e_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			e_ptr = &e_info[i];

			/* Hack -- Verify space */
			if (e_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!e_ptr->name)
				e_ptr->name = ++e_head->name_size;

			/* Append chars to the name */
			strcpy(e_name + e_head->name_size, s);

			/* Advance the index */
			e_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current e_ptr */
		if (!e_ptr)
			return (3);


#if 0

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf + 2;

			/* Hack -- Verify space */
			if (e_head->text_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and Save the text index */
			if (!e_ptr->text)
				e_ptr->text = ++e_head->text_size;

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
			int tval, sval;

			/* Scan for the values */
			if (2 != sscanf(buf + 2, "%d:%d", &tval, &sval))
				return (1);

			/* Save the values */
			e_ptr->tval = tval;
			e_ptr->sval = sval;

			/* Next... */
			continue;
		}


		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
			int level, rarity, rating;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf + 2, "%d:%d:%d:%ld", &level, &rarity,
					&rating, &cost))
				return (1);

			/* Save the values */
			e_ptr->level = level;
			e_ptr->rarity = rarity;
			e_ptr->rating = rating;
			e_ptr->cost = cost;

			/* Next... */
			continue;
		}

		/* Process 'M' for Material. */
		if (buf[0] == 'M')
		{
			int stuff;

			if (1 != sscanf(buf + 2, "%d", &stuff))
				return 1;

			e_ptr->stuff = stuff;

			continue;
		}

		/* Hack -- Process 'C' for "creation" */
		if (buf[0] == 'C')
		{
			int th, td, ta, pv;

			/* Scan for the values */
			if (4 != sscanf(buf + 2, "%d:%d:%d:%d", &th, &td, &ta, &pv))
				return (1);

			e_ptr->max_to_h = th;
			e_ptr->max_to_d = td;
			e_ptr->max_to_a = ta;
			e_ptr->max_pval = pv;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		/* Hack -- Process 'R' for random flags */
		if (buf[0] == 'F' || buf[0] == 'R')
		{

			bool rand = (buf[0] == 'R' ? TRUE : FALSE);

			/* Parse every entry textually */
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|');
					++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while ((*t == ' ') || (*t == '|'))
						t++;
				}

				/* Parse this entry */
				if (0 != grab_one_ego_item_flag(e_ptr, s, rand))
					return (5);

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
	if (!okay)
		return (2);


	/* Success */
	return (0);
}


/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race * r_ptr, cptr what)
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
 * Grab one (spell) flag in a monster_race from a textual string
 */
static errr grab_one_spell_flag(monster_race * r_ptr, cptr what)
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
errr init_r_info_txt(FILE * fp, char *buf)
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
	r_head->text2_size = 0;

	/* Parse */
	while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#'))
			continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':')
			return (1);


		/* Hack -- Process 'V' for "Version" */
		if (buf[0] == 'V')
		{
			int v1, v2, v3;

			/* Scan for the values */
			if ((3 != sscanf(buf + 2, "%d.%d.%d", &v1, &v2, &v3)) ||
				(v1 != r_head->v_major) || (v2 != r_head->v_minor) ||
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
		if (!okay)
			return (2);


		/* Process 'N' for "New/Number/Name" */
		if (buf[0] == 'N')
		{
			/* Find the colon before the name */
			s = strchr(buf + 2, ':');

			/* Verify that colon */
			if (!s)
				return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s)
				return (1);

			/* Get the index */
			i = atoi(buf + 2);

			/* Verify information */
			if (i < error_idx)
				return (4);

			/* Verify information */
			if (i >= r_head->info_num)
				return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			r_ptr = &r_info[i];

			/* Hack -- Verify space */
			if (r_head->name_size + strlen(s) + 8 > fake_name_size)
				return (7);

			/* Advance and Save the name index */
			if (!r_ptr->name)
				r_ptr->name = ++r_head->name_size;

			/* Append chars to the name */
			strcpy(r_name + r_head->name_size, s);

			/* Advance the index */
			r_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

		/* There better be a current r_ptr */
		if (!r_ptr)
			return (3);

		/* Process 'Y' for speech. */
		if (buf[0] == 'Y')
		{
			int foo;

			if (1 != sscanf(buf + 2, "%d", &foo))
			{
				return 1;
			}

			r_ptr->sayings_inx = foo;

			continue;
		}


		/* Process 'X' for speech. */
		if (buf[0] == 'X')
		{
			/* Acquire it. */
			s = buf + 2;

			/* Checking */
			if (r_head->text2_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Save the text index. */
			if (!r_ptr->sayings)
				r_ptr->sayings = ++r_head->text2_size;

			r_ptr->num_sayings++;

			/* Save the string. */
			strcpy(sayings_text + r_head->text2_size, s);
			r_head->text2_size += (strlen(s) + 1);

			/* Next */
			continue;
		}

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf + 2;

			/* Hack -- Verify space */
			if (r_head->text_size + strlen(s) + 8 > fake_text_size)
				return (7);

			/* Advance and Save the text index */
			if (!r_ptr->text)
				r_ptr->text = ++r_head->text_size;

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
			if (!buf[2])
				return (1);
			if (!buf[3])
				return (1);
			if (!buf[4])
				return (1);

			/* Extract the char */
			sym = buf[2];

			/* Extract the attr */
			tmp = color_char_to_attr(buf[4]);

			/* Paranoia */
			if (tmp < 0)
				return (1);

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
			if (6 != sscanf(buf + 2, "%d:%dd%d:%d:%d:%d", &spd, &hp1, &hp2,
					&aaf, &ac, &slp))
				return (1);

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
			if (4 != sscanf(buf + 2, "%d:%d:%d:%ld", &lev, &rar, &pad,
					&exp)) return (1);

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
			for (i = 0; i < 4; i++)
				if (!r_ptr->blow[i].method)
					break;

			/* Oops, no more slots */
			if (i == 4)
				return (1);

			/* Analyze the first field */
			for (s = t = buf + 2; *t && (*t != ':'); t++) /* loop */ ;

			/* Terminate the field (if necessary) */
			if (*t == ':')
				*t++ = '\0';

			/* Analyze the method */
			for (n1 = 0; r_info_blow_method[n1]; n1++)
			{
				if (streq(s, r_info_blow_method[n1]))
					break;
			}

			/* Invalid method */
			if (!r_info_blow_method[n1])
				return (1);

			/* Analyze the second field */
			for (s = t; *t && (*t != ':'); t++) /* loop */ ;

			/* Terminate the field (if necessary) */
			if (*t == ':')
				*t++ = '\0';

			/* Analyze effect */
			for (n2 = 0; r_info_blow_effect[n2]; n2++)
			{
				if (streq(s, r_info_blow_effect[n2]))
					break;
			}

			/* Invalid effect */
			if (!r_info_blow_effect[n2])
				return (1);

			/* Analyze the third field */
			for (s = t; *t && (*t != 'd'); t++) /* loop */ ;

			/* Terminate the field (if necessary) */
			if (*t == 'd')
				*t++ = '\0';

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
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|');
					++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|')
						t++;
				}

				/* Parse this entry */
				if (0 != grab_one_basic_flag(r_ptr, s))
					return (5);

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
			for (s = buf + 2; *s;)
			{
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|');
					++t) /* loop */ ;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while ((*t == ' ') || (*t == '|'))
						t++;
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
				if (0 != grab_one_spell_flag(r_ptr, s))
					return (5);

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
	++r_head->text2_size;

	/* XXX XXX XXX The ghost is unused */

	/* Mega-Hack -- acquire "ghost" */
	r_ptr = &r_info[MAX_R_IDX - 1];

	/* Acquire the next index */
	r_ptr->name = r_head->name_size;
	r_ptr->text = r_head->text_size;

	/* Save some space for the ghost info */
	r_head->name_size += 64;
	r_head->text_size += 64;

	/* Hack -- Default name/text for the ghost */
	strcpy(r_name + r_ptr->name, "Nobody, the Undefined Ghost");
	strcpy(r_text + r_ptr->text, "It seems strangely familiar...");

	/* Hack -- set the char/attr info */
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
	if (!okay)
		return (2);


	/* Success */
	return (0);
}




/*
 * Initialize the spell array. Note that this function is very different 
 * from the others:
 * a) No .raw file is made.
 * b) There is no s_info array.
 * c) It is only called when a new character is created.
 * d) Only selected entries in s_info.txt are parsed.
 */

/*
 * Note that this fn. is supposed to be equivalent to ``strsep''. 
 */

static char *grab_token(char **line, char delim)
{
	char *ret = *line;

	while (**line)
	{
		if ((**line) == delim)
		{
			(**line) = '\0';
			(*line)++;
			return ret;
		}
		(*line)++;
	}
	return ret;
}

static int grab_number(char *line, cptr arr[])
{
	int i;

	for (i = 0; arr[i] != NULL; i++)
	{
		if (strcmp(line, arr[i]) == 0)
			return i;
	}
	return -1;
}

static errr grab_one_proj_flag(proj_node * node, cptr what)
{
	int i = 0;
	int j = 0;
	char buf[80];

	/* Remove whitespace */
	while (what[i])
	{
		if (!isspace(what[i]))
		{
			buf[j] = what[i];
			j++;
		}

		i++;
	}

	buf[j] = '\0';

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(buf, s_info_flags[i]))
		{
			node->proj_flags |= (1L << i);
			return (1);
		}
	}

	/* Error */
	return (0);
}

static void s_info_error(int line, cptr name, char *buf)
{
	msg_format("Error on line %d of 's_info.txt'.", line);
	msg_format("'%s' when parsing '%s'.", name, buf);
	msg_print(NULL);

	quit("Error in 's_info.txt' file.");
}


/**********************/

s16b init_s_info_txt(byte sbook, spell * array, int max)
{
	char buf[1024];
	int line_number = 0;
	FILE *fp;

	char *token;
	char *line;

	char foo;
	int num = 0;

	spell *s_ptr = NULL;

	/* Hack -- current node for the ``F'' parameter. */
	proj_node *flag_node = NULL;
	proj_node *flag_node_prev = NULL;

	/* Hack -- current node for the ``T'' parameter. */
	proj_node *type_node = NULL;
	proj_node *type_node_prev = NULL;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, "s_info.txt");

	/* Open the file */
	fp = my_fopen(buf, "r");

	if (!fp)
		quit("Cannot open 's_info.txt' file.");


	while (0 == my_fgets(fp, buf, 1024))
	{
		line_number++;
		line = buf;

		/* Skip whitespace/comments. */
		if (!buf[0] || buf[0] == '#')
			continue;

		/* First token */
		foo = grab_token(&line, ':')[0];

		if (foo == 'V')
			continue;

		/* Process 'N' -- New spell. */
		if (foo == 'N')
		{
			int book, index;

			token = grab_token(&line, ':');
			if (line[0] == '\0')
				s_info_error(line_number, "No spell index", buf);
			book = grab_number(token, s_info_books);

			/* No such book */
			if (book < 0)
			{
				char tmp[80];

				sprintf(tmp, "No such book -- %s", token);
				s_info_error(line_number, tmp, buf);
			}

			/* Wrong book. */
			if (book != sbook)
			{
				s_ptr = NULL;
				continue;
			}

			token = grab_token(&line, ':');
			if (line[0] == '\0')
				s_info_error(line_number, "No spell name", buf);
			index = atoi(token);

			if (index >= max)
				s_info_error(line_number, "Array capacity exceeded", buf);

			s_ptr = &array[index];

			strncpy(s_ptr->name, line, 29);

			/* Safety net. */
			s_ptr->name[29] = '\0';
			s_ptr->desc[0] = '\0';


			s_ptr->unknown = TRUE;
			s_ptr->untried = TRUE;
			s_ptr->class = 0;

			/* Ugly, ugly hack -- remove the last node off the previously 
			 * initialized spell, since the last one will always be unused. */
			if (flag_node_prev != NULL)
			{
				KILL(flag_node_prev->next, proj_node);
			}

			/* Create a new projectable */
			MAKE(flag_node, proj_node);
			type_node = flag_node;
			s_ptr->proj_list = flag_node;

			/* One more spell. */
			num++;
		}


		/* Other parameters make no sense if there is no current spell. */
		if (s_ptr == NULL)
			continue;

		/* Process 'F' -- Spell flags. */

		if (foo == 'F')
		{

			/* Fill the current node with values */
			while (line[0])
			{
				token = grab_token(&line, '|');

				if (!grab_one_proj_flag(flag_node, token))
				{
					char temp[80];

					sprintf(temp, "Unknown spell flag '%s'", token);
					s_info_error(line_number, temp, buf);
				}
			}

			/* Save the current node */
			flag_node_prev = flag_node;

			/* Create a new node, if needed */
			if (flag_node->next == NULL)
			{
				MAKE(flag_node->next, proj_node);
			}

			/* Advance to the next node */
			flag_node = flag_node->next;
		}


		/* Process 'T' -- Spell type. */

		if (foo == 'T')
		{
			char temp1[80];
			char temp2[80];
			int dd = 0, ds = 0, r = 0, check;

			check =
				sscanf(line, "%[^:]:%[^:]:%dd%d:%d", temp1, temp2, &dd,
				&ds, &r);

			if (check < 2)
			{
				s_info_error(line_number,
					"Spell type must include safety flag "
					"and spell type.", buf);
			}

			if (streq(temp1, "SAFE"))
			{
				type_node->safe = TRUE;

			}
			else if (streq(temp1, "UNSAFE"))
			{
				type_node->safe = FALSE;

			}
			else
			{
				s_info_error(line_number,
					"Format is: `T:SAFE:...' or `T:UNSAFE:...'", buf);
			}

			check = grab_number(temp2, s_info_attack_types);

			if (check < 0)
			{
				char tmp[80];

				sprintf(tmp, "No such spell type -- %s", temp2);
				s_info_error(line_number, tmp, buf);
			}

			type_node->attack_kind = check;
			type_node->dam_dice = dd;
			type_node->dam_sides = ds;
			type_node->radius = r;

			/* Save the current node */
			type_node_prev = type_node;

			/* Create a new node, if needed */
			if (type_node->next == NULL)
			{
				MAKE(type_node->next, proj_node);
			}

			/* Advance to the next node */
			type_node = type_node->next;
		}



		/* Process 'P' -- Spell power. */

		if (foo == 'P')
		{
			int lev, mana;

			if (2 != sscanf(line, "%d:%d", &lev, &mana))
			{
				s_info_error(line_number,
					"Must include level and mana cost info.", buf);
			}

			s_ptr->level = lev;
			s_ptr->mana = mana;
		}

		/* Process 'D' -- spell description. */

		if (foo == 'D')
		{
			strncat(s_ptr->desc, line, 29);
		}

		/* Process 'C' -- spell classification. */
		if (foo == 'C')
		{
			int check = grab_number(line, s_info_classifications);

			if (check < 0)
			{
				char tmp[80];

				sprintf(tmp, "No such spell classification -- %s", line);
				s_info_error(line_number, tmp, buf);
			}

			s_ptr->class = check;
		}

	}


	/* Ugly, ugly hack -- remove the last node off the previously 
	 * initialized spell, since the last one will always be unused. */
	if (flag_node_prev != NULL)
	{
		KILL(flag_node_prev->next, proj_node);
	}

	/* Close the file */
	my_fclose(fp);

	return num;
}

#else /* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif /* ALLOW_TEMPLATES */
