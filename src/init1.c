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
        "XXXX_F3",
	"GOOD",
	"AURA_COLD", /* TODO: Implement aura_cold */
	"NONLIVING",
	"HURT_LITE",
	"HURT_ROCK",
        "SUSCEP_FIRE",
        "SUSCEP_COLD",
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
        "UNIQUE_4",
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
	"QUESTOR2",
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
	"XXX2X6",
	"BLINK",
	"TPORT",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"ANIM_DEAD", /* ToDo: Implement ANIM_DEAD */
        "S_BUG",
        "S_RNG",
        "XXX3X6",
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
        "PET",
        "MORTAL",
        "PLAYER_MONSTER",
        "NAZGUL",
        "XXXX",
        "LEVEL_30",
        "LEVEL_60",
        "LEVEL_100",
        "VARIAZ",
        "SEDUCE_MALES",
        "SEDUCE_FEMALES",
        "TOWNSFOLK",
        "NEVER_MOVE_FRIENDLY",
        "GUARD",
        "NEVER_BOSS",
        "NEVER_ATTACKED",        
        "DEATH_DIALOG",
        "ICE",
        "IMMORTAL",
        "RANDOM",
        "UNPLAYABLE",
        "XXX7x25",
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
        "CTHANGBAND",
        "PERNANGBAND",
        "ZANGBAND",
        "JOKEANGBAND",
        "BASEANGBAND",
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
	"WILD_SWAMP",	/* ToDo: Implement Swamp */
        "WILD_TOO",
};


/*
 * Monster race flags - Drops
 */
static cptr r_info_flags9[] =
{
	"DROP_CORPSE",
	"DROP_SKELETON",
        "XXX9X3",
        "XXX9X4",
        "HAS_EGG",
        "IMPRESED",
        "SUSCEP_ACID",
        "SUSCEP_ELEC",
        "SUSCEP_POIS",
        "KILL_TREES",
        "WYRM_PROTECT",
        "DOPPLEGANGER",
        "ONLY_DEPTH",
        "SPECIAL_GENE",
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
        "MANA",
        "SPELL",
	"STEALTH",
        "XX1X",
	"INFRA",
        "XX2X",
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
        "INVIS",
        "LIFE",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"RES_WATER",
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
	"RES_EARTH",
	"RES_RADIO",
	"RES_WIND",
	"RES_CHAOS",
	"RES_WARP"
};

/*
 * Object flags
 */
static cptr k_info_flags3[] =
{
	"SH_FIRE",
	"SH_ELEC",
	"QUESTITEM",
        "DECAY",
	"NO_TELE",
	"NO_MAGIC",
	"WRAITH",
	"XXXX",
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
 * Object flags
 */
static cptr k_info_flags4[] =
{
        "NEVER_BLOW",
        "ICE",
        "XXXX",
        "RECHARGE",
        "FLY",
        "DG_CURSE",
        "COULD2H",
        "MUST2H",
        "LEVELS",
        "CLONE",
        "SPECIAL_GENE",
        "CLIMB",
        "CRAFTED",
        "MODERATE_POWER",
        "ONLY_MALE",
        "ONLY_FEMALE",
        "ENHANCED",
        "XXXX",
        "XXXX",
        "CHARGEABLE",
        "INDESTRUCTIBLE",
        "ETERNAL",
        "SLAY_MALE",
        "SLAY_FEMALE",
        "ALWAYS_HIT",
        "LOWER_DEF",
        "LOWER_HIT",
        "RETURNING",
        "SAFETY",
        "PROTECTION",
        "XXXX",
        "PARRY"
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
        "DONT_NOTICE_RUNNING",
        "CAN_RUN",
	"DOOR",
        "SUPPORT_LIGHT",
        "CAN_CLIMB",
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
 * Dungeon flags
 */
static cptr d_info_flags1[] =
{
        "PRINCIPAL",
        "MAZE",
        "SMALLEST",
        "SMALL",
        "BIG",
        "NO_DOORS",
        "WATER_RIVER",
        "LAVA_RIVER",
        "WATER_RIVERS",
        "LAVA_RIVERS",
        "CAVE",
        "CAVERN",
        "NO_UP",
        "HOT",
        "COLD",
        "NO_DOWN",
        "FORGET",
        "UNDEAD",
        "DEMON",
        "DRAGON",
        "NO_GENERIC",
        "ICE",
        "WEIRD",
        "RANDOM_ONLY",
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
 * Wilderness feature flags
 */
static cptr wf_info_flags1[] =
{
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
			continue;
		}

		/* There better be a current v_ptr */
		if (!v_ptr) return (3);

		/* Process monster, item and level info for special levels */
		if (buf[0] == 'Y')
		{

			int mon1,mon2,mon3,mon4,mon5,mon6,mon7,mon8,mon9;
                        int mon10,item1,item2,item3,lvl, dun_type;

			/* Scan for the values */
                        if (15 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
                          &mon1,&mon2,&mon3,&mon4,&mon5,&mon6,&mon7,&mon8,&mon9,&mon10,&item1,&item2,&item3,&lvl,&dun_type)) return (1);

			/* Save the values */
                        v_ptr->mon[0] = mon1;
                        v_ptr->mon[1] = mon2;
                        v_ptr->mon[2] = mon3;
                        v_ptr->mon[3] = mon4;
                        v_ptr->mon[4] = mon5;
                        v_ptr->mon[5] = mon6;
                        v_ptr->mon[6] = mon7;
                        v_ptr->mon[7] = mon8;
                        v_ptr->mon[8] = mon9;
                        v_ptr->mon[9] = mon10;
                        v_ptr->item[0] = item1;
                        v_ptr->item[1] = item2;
                        v_ptr->item[2] = item3;
			v_ptr->lvl = lvl;
                        v_ptr->dun_type = dun_type;

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
	msg_format("Unknown object flag '%s'.", what);

	/* Error */
	return (1);
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

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        k_ptr->flags4 |= (1L << i);
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
                        int rec1 = 0;
                        int rec2 = 0;

			/* Scan for the values */
                        if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
                                &tval, &sval, &pval, &rec1, &rec2))
                                {
                                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                        &tval, &sval, &pval)) return (1);
                                }

			/* Save the values */
			k_ptr->tval = tval;
			k_ptr->sval = sval;
			k_ptr->pval = pval;
                        k_ptr->recipe1 = rec1;
                        k_ptr->recipe2 = rec2;

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

		/* Process 'B' for "Brand" (weapons & gloves) */
		if (buf[0] == 'B')
		{
			int brtyp, brrad;
			s32b brdam;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%ld:%d",
				&brtyp, &brdam, &brrad)) return (1);

			/* Save the values */
			k_ptr->brandtype = brtyp;
			k_ptr->branddam = brdam;
			k_ptr->brandrad = brrad;

			/* Next... */
			continue;
		}

		/* Resistances! */
		if (buf[0] == 'R')
		{
			int resist, resamt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &resist, &resamt)) return (1);

			/* Save the values */
                        k_ptr->resistances[resist] = resamt;

			/* Next... */
			continue;
		}

		/* Stats bonus */
		if (buf[0] == 'T')
		{
			int stat, samt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &stat, &samt)) return (1);

			/* Save the values */
                        k_ptr->statsbonus[stat] = samt;

			/* Next... */
			continue;
		}

		/* Skills bonus */
		if (buf[0] == 'K')
		{
			int skill, samt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &skill, &samt)) return (1);

			/* Save the values */
                        k_ptr->skillsbonus[skill] = samt;

			/* Next... */
			continue;
		}

		/* Misc bonus. */
		if (buf[0] == 'U')
		{
			int itype, iskill, eblows, eshots, spd, life, mana, infra, extra1, extra2, extra3, extra4, extra5, reflect, cursed;
			int sbonus, invis, light;
			s32b brdam;

			/* Scan for the values */
			if (18 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
				&itype, &iskill, &eblows, &eshots, &spd, &life, &mana, &infra, &sbonus, &invis, &light, &extra1, &extra2, &extra3, &extra4, &extra5, &reflect, &cursed)) return (1);

			/* Save the values */
			k_ptr->itemtype = itype;
			k_ptr->itemskill = iskill;
			k_ptr->extrablows = eblows;
			k_ptr->extrashots = eshots;
			k_ptr->speedbonus = spd;
			k_ptr->lifebonus = life;
			k_ptr->manabonus = mana;
			k_ptr->infravision = infra;
			k_ptr->spellbonus = sbonus;
			k_ptr->invisibility = invis;
			k_ptr->light = light;
			k_ptr->extra1 = extra1;
			k_ptr->extra2 = extra2;
			k_ptr->extra3 = extra3;
			k_ptr->extra4 = extra4;
			k_ptr->extra5 = extra5;
			k_ptr->reflect = reflect;
			k_ptr->cursed = cursed;

			/* Next... */
			continue;
		}

		/* Portralis 0.3: Artifacts Activation is back! */
		if (buf[0] == 'S')
		{
			int pos;
			char act[80];
			char sname[80];
			char tmp[80];
			char summchar;
			char c;
			int stype, spower, sspecial1, sspecial2, sspecial3, scost;

			/* Find the next empty spell */
			for (i = 0; i < 20; i++) if (!k_ptr->spell[i].type) break;

			/* Oops, no more slots */
			if (i == 20) return (1);

			/* Read the 'name' */
			pos = 2;
			c = buf[pos];
			strcpy(act, "");
			strcpy(sname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", sname, c);
				strcpy(sname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;


			/* Scan for the other values */
			if (7 != sscanf(buf+pos, "%d:%d:%d:%d:%d:%c:%d",
				&stype, &spower, &sspecial1, &sspecial2, &sspecial3, &summchar, &scost)) return (1);

			/* Save the values */
                        strcpy(k_ptr->spell[i].name, sname);
                        strcpy(k_ptr->spell[i].act, "");
			k_ptr->spell[i].type = stype;
			k_ptr->spell[i].power = spower;
			k_ptr->spell[i].special1 = sspecial1;
			k_ptr->spell[i].special2 = sspecial2;
			k_ptr->spell[i].special3 = sspecial3;
			k_ptr->spell[i].summchar = summchar;
			k_ptr->spell[i].cost = scost;		

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

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        a_ptr->flags4 |= (1L << i);
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
	int magiceffect = 0;

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

		/* Process 'B' for "Brand" (weapons & gloves) */
		if (buf[0] == 'B')
		{
			int brtyp, brrad;
			s32b brdam;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%ld:%d",
				&brtyp, &brdam, &brrad)) return (1);

			/* Save the values */
			a_ptr->brandtype = brtyp;
			a_ptr->branddam = brdam;
			a_ptr->brandrad = brrad;

			/* Next... */
			continue;
		}

		/* Resistances! */
		if (buf[0] == 'R')
		{
			int resist, resamt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &resist, &resamt)) return (1);

			/* Save the values */
                        a_ptr->resistances[resist] = resamt;

			/* Next... */
			continue;
		}

		/* Stats bonus */
		if (buf[0] == 'T')
		{
			int stat, samt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &stat, &samt)) return (1);

			/* Save the values */
                        a_ptr->statsbonus[stat] = samt;

			/* Next... */
			continue;
		}

		/* Skills bonus */
		if (buf[0] == 'K')
		{
			int skill, samt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &skill, &samt)) return (1);

			/* Save the values */
                        a_ptr->skillsbonus[skill] = samt;

			/* Next... */
			continue;
		}

		/* Misc bonus. */
		if (buf[0] == 'U')
		{
			int itype, iskill, eblows, eshots, spd, life, mana, infra, extra1, extra2, extra3, extra4, extra5, reflect, cursed;
			int sbonus, invis, light;
			s32b brdam;

			/* Scan for the values */
			if (18 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
				&itype, &iskill, &eblows, &eshots, &spd, &life, &mana, &infra, &sbonus, &invis, &light, &extra1, &extra2, &extra3, &extra4, &extra5, &reflect, &cursed)) return (1);

			/* Save the values */
			a_ptr->itemtype = itype;
			a_ptr->itemskill = iskill;
			a_ptr->extrablows = eblows;
			a_ptr->extrashots = eshots;
			a_ptr->speedbonus = spd;
			a_ptr->lifebonus = life;
			a_ptr->manabonus = mana;
			a_ptr->infravision = infra;
			a_ptr->spellbonus = sbonus;
			a_ptr->invisibility = invis;
			a_ptr->light = light;
			a_ptr->extra1 = extra1;
			a_ptr->extra2 = extra2;
			a_ptr->extra3 = extra3;
			a_ptr->extra4 = extra4;
			a_ptr->extra5 = extra5;
			a_ptr->reflect = reflect;
			a_ptr->cursed = cursed;

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

		/* Portralis 0.3: Artifacts Activation is back! */
		if (buf[0] == 'S')
		{
			int pos;
			char act[80];
			char sname[80];
			char tmp[80];
			char summchar;
			char c;
			int stype, spower, sspecial1, sspecial2, sspecial3, scost;

			/* Find the next empty spell */
			for (i = 0; i < 20; i++) if (!a_ptr->spell[i].type) break;

			/* Oops, no more slots */
			if (i == 20) return (1);

			/* Read the 'name' */
			pos = 2;
			c = buf[pos];
			strcpy(act, "");
			strcpy(sname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", sname, c);
				strcpy(sname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;


			/* Scan for the other values */
			if (7 != sscanf(buf+pos, "%d:%d:%d:%d:%d:%c:%d",
				&stype, &spower, &sspecial1, &sspecial2, &sspecial3, &summchar, &scost)) return (1);

			/* Save the values */
                        strcpy(a_ptr->spell[i].name, sname);
                        strcpy(a_ptr->spell[i].act, act);
			a_ptr->spell[i].type = stype;
			a_ptr->spell[i].power = spower;
			a_ptr->spell[i].special1 = sspecial1;
			a_ptr->spell[i].special2 = sspecial2;
			a_ptr->spell[i].special3 = sspecial3;
			a_ptr->spell[i].summchar = summchar;
			a_ptr->spell[i].cost = scost;

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

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        e_ptr->flags4 |= (1L << i);
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
	return (1);
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
			if (r_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
			if (!r_ptr->name) r_ptr->name = ++r_head->name_size;

			/* Append chars to the name */
			strcpy(r_name + r_head->name_size, s);
			sprintf(r_ptr->name_char, "%s%s", r_ptr->name_char, s);

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
			if (r_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

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
			r_ptr->d_char = sym;
			r_ptr->d_attr = tmp;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int spd, hp1, hp2, aaf, ac, slp;
			int event, xtra1, xtra2, fixlev, townum, dunum, cursed;

			/* Scan for the other values */
			if (13 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
				&spd, &hp1, &hp2, &aaf, &ac, &slp, &event, &xtra1, &xtra2, &fixlev, &townum, &dunum, &cursed)) return (1);

			/* Save the values */
			r_ptr->speed = spd;
			r_ptr->hdice = hp1;
			r_ptr->hside = hp2;
			r_ptr->aaf = aaf;
			r_ptr->ac = ac;
			r_ptr->sleep = slp;
			r_ptr->event = event;
			r_ptr->extra1 = xtra1;
			r_ptr->extra2 = xtra2;
			r_ptr->fixedlevel = fixlev;
			r_ptr->townnum = townum;
			r_ptr->dunnum = dunum;
			r_ptr->cursed = cursed;

			/* Next... */
			continue;
		}

                /* Process 'E' for "Body Parts" (one line only) */
                if (buf[0] == 'E')
		{
                        int weap, tors, fing, head, arms, legs;

			/* Scan for the other values */
                        if (BODY_MAX != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
                                &weap, &tors, &arms, &fing, &head, &legs)) return (1);

			/* Save the values */
                        r_ptr->body_parts[BODY_WEAPON] = weap;
                        r_ptr->body_parts[BODY_TORSO] = tors;
                        r_ptr->body_parts[BODY_ARMS] = arms;
                        r_ptr->body_parts[BODY_FINGER] = fing;
                        r_ptr->body_parts[BODY_HEAD] = head;
                        r_ptr->body_parts[BODY_LEGS] = legs;

			/* Next... */
			continue;
		}		

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int lev, rar, wt;
			long exp;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                                &lev, &rar, &wt, &exp)) return (1);

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
                        /* MEGA HACK */
                        if(!wt) wt = 100;
                        r_ptr->weight = wt;
			r_ptr->mexp = exp;

			/* Next... */
			continue;
		}

		/* NewAngband 1.8.0: 'B' has new functions! :) */
		if (buf[0] == 'B')
		{
			int str, dex, mind, attack, ranged, magic, blows;

			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
                                &str, &dex, &mind, &attack, &ranged, &magic, &blows)) return (1);

			/* Save the values */
			r_ptr->str = str;
			r_ptr->dex = dex;
                        r_ptr->mind = mind;
			r_ptr->skill_attack = attack;
			r_ptr->skill_ranged = ranged;
			r_ptr->skill_magic = magic;
			r_ptr->attacks = blows;

			/* Next... */
			continue;
		}

		/* NewAngband 1.8.0: Process 'A' for attacks! */
		if (buf[0] == 'A')
		{
			int pos;
			char act[80];
			char aname[80];
			char tmp[80];
			char c;
			int atype, addice, adside, aelem, aspecial1, aspecial2;

			/* Find the next empty attack */
			for (i = 0; i < 20; i++) if (!r_ptr->attack[i].type) break;

			/* Oops, no more slots */
			if (i == 20) return (1);

			/* Read the 'act' and the 'name' */
			pos = 2;
			c = buf[pos];
			strcpy(act, "");
			strcpy(aname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", act, c);
				strcpy(act, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;
			c = buf[pos];
			while (c != ':')
			{
				sprintf(tmp, "%s%c", aname, c);
				strcpy(aname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (6 != sscanf(buf+pos, "%d:%dd%d:%d:%d:%d",
				&atype, &addice, &adside, &aelem, &aspecial1, &aspecial2)) return (1);

			/* Save the values */
                        strcpy(r_ptr->attack[i].name, aname);
                        strcpy(r_ptr->attack[i].act, act);
			r_ptr->attack[i].type = atype;
			r_ptr->attack[i].ddice = addice;
			r_ptr->attack[i].dside = adside;
			r_ptr->attack[i].element = aelem;
			r_ptr->attack[i].special1 = aspecial1;
			r_ptr->attack[i].special2 = aspecial2;
			

			/* Next... */
			continue;
		}

		/* Counter! */
		if (buf[0] == 'C')
		{
			int ctype, cchance;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                                &ctype, &cchance)) return (1);

			/* Save the values */
			r_ptr->countertype = ctype;
			r_ptr->counterchance = cchance;

			/* Next... */
			continue;
		}

		/* Lives! */
		if (buf[0] == 'L')
		{
			int nlives;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
                                &nlives)) return (1);

			/* Save the values */
			r_ptr->lives = nlives;

			/* Next... */
			continue;
		}

		/* Treasure! */
		if (buf[0] == 'T')
		{
			int ttval, tsval, tchance, tmagic;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &ttval, &tsval, &tchance, &tmagic)) return (1);

			/* Save the values */
			r_ptr->treasuretval = ttval;
			r_ptr->treasuresval = tsval;
			r_ptr->treasurechance = tchance;
			r_ptr->treasuremagic = tmagic;

			/* Next... */
			continue;
		}

		/* Resistances! */
		if (buf[0] == 'R')
		{
			int resist, resamt;
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                        &resist, &resamt)) return (1);

			/* Save the values */
                        r_ptr->resistances[resist] = resamt;

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

		/* Magic! Chance to use it... */
		if (buf[0] == 'M')
		{
			int magicchance;
			int nummagic;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                                &magicchance, &nummagic)) return (1);

			/* Save the values */
			r_ptr->spellchance = magicchance;
			r_ptr->spells = nummagic;

			/* Next... */
			continue;
		}

		/* NewAngband 1.8.0: Process 'S' for spells! */
		if (buf[0] == 'S')
		{
			int pos;
			char act[80];
			char sname[80];
			char tmp[80];
			char summchar;
			char c;
			int stype, spower, sspecial1, sspecial2, sspecial3, scost;

			/* Find the next empty spell */
			for (i = 0; i < 20; i++) if (!r_ptr->spell[i].type) break;

			/* Oops, no more slots */
			if (i == 20) return (1);

			/* Read the 'act' and the 'name' */
			pos = 2;
			c = buf[pos];
			strcpy(act, "");
			strcpy(sname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", act, c);
				strcpy(act, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;
			c = buf[pos];
			while (c != ':')
			{
				sprintf(tmp, "%s%c", sname, c);
				strcpy(sname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;


			/* Scan for the other values */
			if (7 != sscanf(buf+pos, "%d:%d:%d:%d:%d:%c:%d",
				&stype, &spower, &sspecial1, &sspecial2, &sspecial3, &summchar, &scost)) return (1);

			/* Save the values */
                        strcpy(r_ptr->spell[i].name, sname);
                        strcpy(r_ptr->spell[i].act, act);
			r_ptr->spell[i].type = stype;
			r_ptr->spell[i].power = spower;
			r_ptr->spell[i].special1 = sspecial1;
			r_ptr->spell[i].special2 = sspecial2;
			r_ptr->spell[i].special3 = sspecial3;
			r_ptr->spell[i].summchar = summchar;
			r_ptr->spell[i].cost = scost;		

			/* Next... */
			continue;
		}

		/* Z is used for monster events. */
		if (buf[0] == 'Z')
		{
			int beforemelee;
			int aftermelee;
			int beforeranged;
			int afterranged;
			int beforemagic;
			int aftermagic;
			int beforemove;
			int aftermove;
			int passive;
			int takedamages;
			int evdeath;
			int evspawn;
			int evmisc;

			/* Scan for the values */
			if (13 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
                                &beforemelee, &aftermelee, &beforeranged, &afterranged, &beforemagic, &aftermagic, &beforemove, &aftermove, &passive, &takedamages, &evdeath, &evspawn, &evmisc)) return (1);

			/* Save the values */
			r_ptr->event_before_melee = beforemelee;
			r_ptr->event_after_melee = aftermelee;
			r_ptr->event_before_ranged = beforeranged;
			r_ptr->event_after_ranged = afterranged;
			r_ptr->event_before_magic = beforemagic;
			r_ptr->event_after_magic = aftermagic;
			r_ptr->event_before_move = beforemove;
			r_ptr->event_after_move = aftermove;
			r_ptr->event_passive = passive;
			r_ptr->event_take_damages = takedamages;
			r_ptr->event_death = evdeath;
			r_ptr->event_spawn = evspawn;
			r_ptr->event_misc = evmisc;

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++r_head->name_size;
	++r_head->text_size;

	/* XXX XXX XXX The ghost is unused */

	/* Mega-Hack -- acquire "ghost" */
	/*r_ptr = &r_info[max_r_idx-1];*/

	/* Acquire the next index */
	/*r_ptr->name = r_head->name_size;*/
	/*r_ptr->text = r_head->text_size;*/

	/* Save some space for the ghost info */
	/*r_head->name_size += 64;*/
	/*r_head->text_size += 64;*/

	/* Hack -- Default name/text for the ghost */
	/*strcpy(r_name + r_ptr->name, "Nobody, the Undefined Ghost");*/
	/*strcpy(r_text + r_ptr->text, "It seems strangely familiar...");*/

	/* Hack -- set the char/attr info */
	/*r_ptr->d_attr = r_ptr->x_attr = TERM_WHITE;*/
	/*r_ptr->d_char = r_ptr->x_char = 'G';*/

	/* Hack -- Try to prevent a few "potential" bugs */
	/*r_ptr->flags1 |= (RF1_UNIQUE);*/

	/* Hack -- Try to prevent a few "potential" bugs */
        /*r_ptr->flags1 |= (RF1_NEVER_MOVE | RF1_NEVER_BLOW);*/

	/* Hack -- Try to prevent a few "potential" bugs */
	/*r_ptr->hdice = r_ptr->hside = 1;*/

	/* Hack -- Try to prevent a few "potential" bugs */
	/*r_ptr->mexp = 1L;*/

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

/*
 * Grab one flag for a dungeon type from a textual string
 */
static errr grab_one_dungeon_flag(dungeon_info_type *d_ptr, cptr what)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, d_info_flags1[i]))
		{
                        d_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        msg_format("Unknown dungeon type flag '%s'.", what);

	/* Failure */
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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_monster_flag(dungeon_info_type *d_ptr, cptr what)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, r_info_flags1[i]))
		{
                        d_ptr->mflags1 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags2[i]))
		{
                        d_ptr->mflags2 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
                        d_ptr->mflags3 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, r_info_flags7[i]))
		{
                        d_ptr->mflags7 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags8 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags8[i]))
		{
                        d_ptr->mflags8 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags9 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
                        d_ptr->mflags9 |= (1L << i);
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
static errr grab_one_spell_monster_flag(dungeon_info_type *d_ptr, cptr what)
{
	int i;

	/* Scan flags4 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags4[i]))
		{
                        d_ptr->mflags4 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags5 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags5[i]))
		{
                        d_ptr->mflags5 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags6 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags6[i]))
		{
                        d_ptr->mflags6 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Initialize the "d_info" array, by parsing an ascii "template" file
 */
errr init_d_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        dungeon_info_type *d_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        d_head->name_size = 0;
        d_head->text_size = 0;

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
                            (v1 != d_head->v_major) ||
                            (v2 != d_head->v_minor) ||
                            (v3 != d_head->v_patch))
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
                        if (i >= d_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        d_ptr = &d_info[i];

			/* Hack -- Verify space */
                        if (d_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!d_ptr->name) d_ptr->name = ++d_head->name_size;

			/* Append chars to the name */
                        strcpy(d_name + d_head->name_size, s);

			/* Advance the index */
                        d_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* There better be a current d_ptr */
                if (!d_ptr) return (3);

                /* Process 'D' for "Description */
                if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (d_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
                        if (!d_ptr->text) d_ptr->text = ++d_head->text_size;

			/* Append chars to the name */
                        strcpy(d_text + d_head->text_size, s);

			/* Advance the index */
                        d_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

                /* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int min_lev, max_lev;
                        int min_plev, next, mode;
                        int min_alloc, max_chance;

			/* Scan for the values */
                        if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
                                &min_lev, &max_lev, &min_plev, &next, &mode, &min_alloc, &max_chance)) return (1);

			/* Save the values */
                        d_ptr->mindepth = min_lev;
                        d_ptr->maxdepth = max_lev;
                        d_ptr->min_plev = min_plev;
                        d_ptr->quest = next;
                        d_ptr->mode = mode;
                        d_ptr->min_m_alloc_level = min_alloc;
                        d_ptr->max_m_alloc_chance = max_chance;

                        /* Next... */
			continue;
		}

                /* Process 'L' for "fLoor type" (one line only) */
                if (buf[0] == 'L')
		{
                        int f1, f2, f3;
                        int p1, p2, p3;

			/* Scan for the values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
                                &f1, &p1, &f2, &p2, &f3, &p3)) return (1);

			/* Save the values */
                        d_ptr->floor1 = f1;
                        d_ptr->floor_percent1 = p1;
                        d_ptr->floor2 = f2;
                        d_ptr->floor_percent2 = p2;
                        d_ptr->floor3 = f3;
                        d_ptr->floor_percent3 = p3;

			/* Next... */
			continue;
		}

                /* Process 'A' for "wAll type" (one line only) */
                if (buf[0] == 'A')
		{
                        int w1, w2, w3, outer, inner;
                        int p1, p2, p3;

			/* Scan for the values */
                        if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &w1, &p1, &w2, &p2, &w3, &p3, &outer, &inner)) return (1);

			/* Save the values */
                        d_ptr->fill_type1 = w1;
                        d_ptr->fill_percent1 = p1;
                        d_ptr->fill_type2 = w2;
                        d_ptr->fill_percent2 = p2;
                        d_ptr->fill_type3 = w3;
                        d_ptr->fill_percent3 = p3;
                        d_ptr->outer_wall = outer;
                        d_ptr->inner_wall = inner;

			/* Next... */
			continue;
		}

                /* Process 'F' for "Dungeon Flags" (multiple lines) */
		if (buf[0] == 'F')
		{
                        int artif = 0, monst = 0;

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

                                /* XXX XXX XXX Hack -- Read Final Artifact */
                                if (1 == sscanf(s, "FINAL_ARTIFACT_%d", &artif))
                                {
                                        /* Extract a "Final Artifact" */
                                        d_ptr->final_artifact = artif;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

                                /* XXX XXX XXX Hack -- Read Artifact Guardian */
                                if (1 == sscanf(s, "FINAL_GUARDIAN_%d", &monst))
                                {
                                        /* Extract a "Artifact Guardian" */
                                        d_ptr->final_guardian = monst;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

                                /* XXX XXX XXX Hack -- Read Special Percentage */
                                if (1 == sscanf(s, "MONSTER_PERCENT_%d", &monst))
                                {
                                        /* Extract a "Special %" */
                                        d_ptr->special_percent = monst;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_dungeon_flag(d_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'M' for "Basic Flags" (multiple lines) */
                if (buf[0] == 'M')
		{
                        byte r_char_number = 0, r_char;

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

                                /* XXX XXX XXX Hack -- Read monster symbols */
                                if (1 == sscanf(s, "R_CHAR_%c", &r_char))
                                {
                                        /* Limited to 5 races */
                                        if(r_char_number >= 5) continue;

					/* Extract a "frequency" */
                                        d_ptr->r_char[r_char_number++] = r_char;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_basic_monster_flag(d_ptr, s)) return (5);

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
					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_spell_monster_flag(d_ptr, s)) return (5);

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
        ++d_head->name_size;
        ++d_head->text_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}

/*
 * Grab one flag for a dungeon type from a textual string
 */
static errr grab_one_wf_info_flag(wilderness_type_info *wf_ptr, cptr what)
{
	int i;

        /* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, wf_info_flags1[i]))
		{
                        wf_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Initialize the "wf_info" array, by parsing an ascii "template" file
 */
errr init_wf_info_txt(FILE *fp, char *buf)
{
	int i;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        wilderness_type_info *wf_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        wf_head->name_size = 0;
        wf_head->text_size = 0;

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
                            (v1 != wf_head->v_major) ||
                            (v2 != wf_head->v_minor) ||
                            (v3 != wf_head->v_patch))
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
                        if (i >= wf_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        wf_ptr = &wf_info[i];

			/* Hack -- Verify space */
                        if (wf_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!wf_ptr->name) wf_ptr->name = ++wf_head->name_size;

			/* Append chars to the name */
                        strcpy(wf_name + wf_head->name_size, s);

			/* Advance the index */
                        wf_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* There better be a current wf_ptr */
                if (!wf_ptr) return (3);

                /* Process 'D' for "Description */
                if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
                        if (wf_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
                        if (!wf_ptr->text) wf_ptr->text = ++wf_head->text_size;

			/* Append chars to the name */
                        strcpy(wf_text + wf_head->text_size, s);

			/* Advance the index */
                        wf_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

                /* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int entrance, level;
                        int road, feat, ter_idx;
                        char car;

			/* Scan for the values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%c",
                                &level, &entrance, &road, &feat, &ter_idx, &car)) return (1);

			/* Save the values */
                        wf_ptr->level = level;
                        wf_ptr->entrance = entrance;
                        wf_ptr->road = road;
                        wf_ptr->feat = feat;
                        wf_ptr->terrain_idx = ter_idx;

                        /* To acces it easly from the map structure */
                        wildc2i[(int)car] = error_idx;

			/* Next... */
			continue;
		}

                /* Process 'X' for "More Info" (one line only) */
                if (buf[0] == 'X')
		{
                        int terrain[MAX_WILD_TERRAIN], i;

			/* Scan for the values */
                        if (MAX_WILD_TERRAIN != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
                                &terrain[0], &terrain[1], &terrain[2],
                                &terrain[3], &terrain[4], &terrain[5],
                                &terrain[6], &terrain[7], &terrain[8],
                                &terrain[9], &terrain[10], &terrain[11],
                                &terrain[12], &terrain[13], &terrain[14],
                                &terrain[15], &terrain[16], &terrain[17])) return (1);

			/* Save the values */
                        for(i = 0; i < MAX_WILD_TERRAIN; i++)
                        {
                                wf_ptr->terrain[i] = terrain[i];
                        }

			/* Next... */
			continue;
		}

                /* Process 'F' for "Wilderness feature Flags" (multiple lines) */
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
                                if (0 != grab_one_wf_info_flag(wf_ptr, s)) return (5);

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
        ++wf_head->name_size;
        ++wf_head->text_size;

	/* No version yet */
	if (!okay) return (2);

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

static dungeon_grid letter[255];

/*
 * Parse a sub-file of the "extra info"
 */
static errr process_dungeon_file_aux(char *buf, int *yval, int *xval, int ymax, int xmax)
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
		return (process_dungeon_file(buf + 2, yval, xval, ymax, xmax));
	}

	/* Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid */
	if (buf[0] == 'F')
	{
		int num;

		if ((num = tokenize(buf+2, 9, zz)) > 1)
		{
			int index = zz[0][0];

			/* Reset the feature */
			letter[index].feature = 0;
			letter[index].monster = 0;
			letter[index].object = 0;
			letter[index].ego = 0;
			letter[index].artifact = 0;
			letter[index].trap = 0;
			letter[index].cave_info = 0;
			letter[index].special = 0;
			letter[index].random = 0;

			if (num > 1)
			{
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
			}

			if (num > 2)
				letter[index].cave_info = atoi(zz[2]);

			/* Monster */
			if (num > 3)
			{
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
					letter[index].monster = atoi(zz[3]);
				}
			}

			/* Object */
			if (num > 4)
			{
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
					letter[index].object = atoi(zz[4]);
				}
			}

			/* Ego-Item */
			if (num > 5)
			{
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
			}

			/* Artifact */
			if (num > 6)
			{
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
			}

			if (num > 7)
			{
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
					letter[index].trap = atoi(zz[7]);
			}

			if (num > 8)
				letter[index].special = atoi(zz[8]);

			return (0);
		}
	}

	/* Process "P:<x>:<y>" -- player position */
	else if (buf[0] == 'P')
	{
		if (init_flags & INIT_CREATE_DUNGEON)
		{
			if (tokenize(buf+2, 2, zz) == 2)
			{
				/* Place player in a quest level */
                                if (p_ptr->inside_quest)
				{
					py = atoi(zz[0]);
					px = atoi(zz[1]); 
				}
				/* Place player in the town */
                                else if ((p_ptr->oldpx == 0) && (p_ptr->oldpy == 0))
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
		int num;

		if ((num = tokenize(buf+2, 33, zz)) > 2)
		{
			/* Building number */
			int index = atoi(zz[0]);

			/* Building definition sub-index */
			switch (zz[1][0])
			{
				/* Building name, owner, ... */
				case 'N':
					{
						if (num > 2)
							/* Name of the building */
							strcpy(building[index].name, zz[2]);
						if (num > 3)
							/* Name of the owner */
							strcpy(building[index].owner_name, zz[3]);
						if (num > 4)
							/* Race of the owner */
							strcpy(building[index].owner_race, zz[4]);
						break;
					}
				/* Building Action */
				case 'A':
					{
						/* Index of the action */
						int action_index = atoi(zz[2]);

						if (num > 3)
							/* Name of the action */
							strcpy(building[index].act_names[action_index], zz[3]);
						if (num > 4)
							/* Cost of the action for members */
							building[index].member_costs[action_index] = atoi(zz[4]);
						if (num > 5)
							/* Cost of the action for non-members */
							building[index].other_costs[action_index] = atoi(zz[5]);
						if (num > 6)
							/* Letter assigned to the action */
							building[index].letters[action_index] = zz[6][0];
						if (num > 7)
							/* Action code */
							building[index].actions[action_index] = atoi(zz[7]);
						if (num > 8)
							/* Action restriction */
							building[index].action_restr[action_index] = atoi(zz[8]);

                                                break;
					}
				/* Building Classes */
				case 'C':
					if (num <= MAX_CLASS + 2)
					{
						for (i = 0; i < MAX_CLASS; i++)
						{
                                                        if(zz[i+1][0] == ':') building[index].member_class[i] = atoi(zz[i+2]);
						}
					}
					break;
				/* Building Races */
				case 'R':
					if (num <= MAX_RACES + 2)
					{
						for (i = 0; i < MAX_RACES; i++)
						{
                                                        if(zz[i+1][0] == ':') building[index].member_race[i] = atoi(zz[i+2]);
						}
					}
					break;
			}

			return (0);
		}
	}

	/* Process "M:<type>:<maximum>" -- set maximum values */
	else if (buf[0] == 'M')
	{
		if (tokenize(buf+2, 2, zz) == 2)
		{

			/* Maximum r_idx */
			if (zz[0][0] == 'R')
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

			/* Maximum tr_idx */
                        else if (zz[0][0] == 'W')
			{
                                max_wf_idx = atoi(zz[1]); 
			}

                        /* Maximum d_idx */
                        else if (zz[0][0] == 'D')
			{
                                max_d_idx = atoi(zz[1]);
			}

			return (0);
		}
	}

	/* Failure */
	return (1);
}


static char tmp[8];


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
				v = classes_def[p_ptr->pclass].name;
			}

			/* Player */
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

			/* Variant name */
			else if (streq(b+1, "VARIANT"))
			{
                                v = "PERNANGBAND";
			}

			/* Wilderness */
			else if (streq(b+1, "WILDERNESS"))
			{
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


errr process_dungeon_file(cptr name, int *yval, int *xval, int ymax, int xmax)
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
			(void)process_dungeon_file(buf + 2, yval, xval, ymax, xmax);

			/* Continue */
			continue;
		}


		/* Process the line */
		err = process_dungeon_file_aux(buf, yval, xval, ymax, xmax);

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

/* Init the wilderness! */
int init_wilderness()
{
	int i,j;
	int var1, var2, var3, var4;
	int wid, hgt;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	int ypos = 0;
	int xpos = 0;

	/* Clear all the wilderness array! */
	for (i = 0; i < MAX_WILD_Y; i++)
	{
		for (j = 0; j < MAX_WILD_X; j++)
		{
			wild[j][i].town = 0;
			wild[j][i].feat = 0;
		}
	}

	/* Determine the name of the file */
	sprintf(filename, "w_info.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'w_info.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse the file */
	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		if (buf[0] == 'I')
		{
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                                &wid, &hgt)) return (1);

			wild_max_x = wid;
			wild_max_y = hgt;

			/* Next... */
			continue;
		}
		if (buf[0] == 'T')
		{
			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &var1, &var2, &var3, &var4)) return (1);

			wild[var1][var2].town = var3;
			if (var4 == 1) wild[var1][var2].revive = TRUE;
			else wild[var1][var2].revive = FALSE;

			/* Next... */
			continue;
		}
		if (buf[0] == 'S')
		{
			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d",
                                &var1, &var2)) return (1);

			birth_wild_x = var1;
			birth_wild_y = var2;
			
			/* Next... */
			continue;
		}
		if (buf[0] == 'D')
		{
			int i;
			for (i = 2; i < (wid+2); i++)
			{
				if (buf[i] == '#') wild[xpos][ypos].feat = FEAT_PERM_SOLID;
				else if (buf[i] == 'X') wild[xpos][ypos].feat = 56;
				else if (buf[i] == '.') wild[xpos][ypos].feat = FEAT_FLOOR;
				else if (buf[i] == 'T') wild[xpos][ypos].feat = FEAT_TREES;
				else if (buf[i] == 'M') wild[xpos][ypos].feat = FEAT_MOUNTAIN;
				else if (buf[i] == 'w') wild[xpos][ypos].feat = FEAT_SHAL_WATER;
				else if (buf[i] == 'W') wild[xpos][ypos].feat = FEAT_DEEP_WATER;
				else if (buf[i] == 'l') wild[xpos][ypos].feat = FEAT_SHAL_LAVA;
				else if (buf[i] == 'L') wild[xpos][ypos].feat = FEAT_DEEP_LAVA;
				else if (buf[i] == ',') wild[xpos][ypos].feat = FEAT_GRASS;
				else if (buf[i] == ';') wild[xpos][ypos].feat = FEAT_DIRT;
				else if (buf[i] == '+') wild[xpos][ypos].feat = 32;
				else if (buf[i] == '<') wild[xpos][ypos].feat = 6;
				else if (buf[i] == '>') wild[xpos][ypos].feat = 7;
				else if (buf[i] == '1') wild[xpos][ypos].feat = 204;
				else if (buf[i] == '2') wild[xpos][ypos].feat = 205;
				else if (buf[i] == '3') wild[xpos][ypos].feat = 206;
				else if (buf[i] == '4') wild[xpos][ypos].feat = 207;
				else if (buf[i] == '5') wild[xpos][ypos].feat = 208;
				else if (buf[i] == '6') wild[xpos][ypos].feat = 209;
				else if (buf[i] == '7') wild[xpos][ypos].feat = 210;
				else if (buf[i] == '8') wild[xpos][ypos].feat = 211;
				else if (buf[i] == '9') wild[xpos][ypos].feat = 214;
				else if (buf[i] == '0') wild[xpos][ypos].feat = 215;
				else if (buf[i] == '-') wild[xpos][ypos].feat = 233;
				else if (buf[i] == 's') wild[xpos][ypos].feat = FEAT_SNOW;
				else if (buf[i] == 't') wild[xpos][ypos].feat = FEAT_SNOW_TREES;
				else if (buf[i] == 'G') wild[xpos][ypos].feat = FEAT_GLACIER;
				else if (buf[i] == 'x') wild[xpos][ypos].feat = FEAT_ICE_WALL;
				else if (buf[i] == 'I') wild[xpos][ypos].feat = FEAT_PERM_ICE_WALL;
				else wild[xpos][ypos].feat = FEAT_PERM_SOLID;

				xpos++;
			}
			xpos = 0;
			ypos++;

			/* Next... */
			continue;
		}
	}
	
	/* Close it */
	my_fclose(fp);
	
	/* Success */
	return (1);
}

/* Init skills names! */
int init_skills_names()
{
	int i;

	char *s, *t;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];

	/* Determine the name of the file */
	sprintf(filename, "skills.txt");

	/* Reset number of skills. */
	num_skills = 0;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'skills.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);

		/* Read skill names */
		if (buf[0] == 'N')
		{
			int pos;
			char sname[80];
			char tmp[80];
			char c;
			int snum;

			/* Read skill name */
			pos = 2;
			c = buf[pos];
			strcpy(sname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", sname, c);
				strcpy(sname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (1 != sscanf(buf+pos, "%d",
				&snum)) return (1);

			/* Save the values */
                        strcpy(skill_names[snum], sname);
			num_skills += 1;			

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}

	/* Success */
	return (0);
}

/* Init classes names! */
int init_classes()
{
	int i, x;

	char *s, *t;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	int cnum = 0;
	int currank = 0;
	int curability = 0;

	/* Clear the classes list first */
	for (x = 0; x < MAX_CLASS; x++)
	{
		classes_def[x].created = FALSE;
	}

	/* Determine the name of the file */
	sprintf(filename, "classes.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'classes.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);

		/* Read class names */
		if (buf[0] == 'N')
		{
			int pos;
			int advanced;
			char cname[80];
			char tmp[80];
			char c;

			/* Read skill name */
			pos = 2;
			c = buf[pos];
			strcpy(cname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", cname, c);
				strcpy(cname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (2 != sscanf(buf+pos, "%d:%d",
				&cnum, &advanced)) return (1);

			/* Save the values */
                        strcpy(classes_def[cnum].name, cname);
			classes_def[cnum].advanced = advanced;
			classes_def[cnum].created = TRUE;

			/* Since it's a new record, reset some stuff. */
			currank = 0;
			curability = 0;

			/* Next... */
			continue;
		}

		/* Read stats requirements */
		if (buf[0] == 'Q')
		{
			int strreq, intreq, wisreq, dexreq, conreq, chrreq;

			/* Scan for the other values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
				&strreq, &intreq, &wisreq, &dexreq, &conreq, &chrreq)) return (1);

			/* Save the values */
			classes_def[cnum].req_str = strreq;
			classes_def[cnum].req_int = intreq;
			classes_def[cnum].req_wis = wisreq;
			classes_def[cnum].req_dex = dexreq;
			classes_def[cnum].req_con = conreq;
			classes_def[cnum].req_chr = chrreq;

			/* Next... */
			continue;
		}

		/* Read required classes */
		if (buf[0] == 'R')
		{
			int reqclass, classlvl;

			/* Scan for the other values */
                        if (2 != sscanf(buf+2, "%d:%d",
				&reqclass, &classlvl)) return (1);

			/* Save the values */
			classes_def[cnum].req_classes[reqclass] = classlvl;

			/* Next... */
			continue;
		}

		/* Read required skills */
		if (buf[0] == 'L')
		{
			int reqskill, skilllvl;

			/* Scan for the other values */
                        if (2 != sscanf(buf+2, "%d:%d",
				&reqskill, &skilllvl)) return (1);

			/* Save the values */
			classes_def[cnum].req_skills[reqskill] = skilllvl;

			/* Next... */
			continue;
		}

		/* Read stats bonus */
		if (buf[0] == 'S')
		{
			int strbonus, intbonus, wisbonus, dexbonus, conbonus, chrbonus;

			/* Scan for the other values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
				&strbonus, &intbonus, &wisbonus, &dexbonus, &conbonus, &chrbonus)) return (1);

			/* Save the values */
			classes_def[cnum].str_bonus = strbonus;
			classes_def[cnum].int_bonus = intbonus;
			classes_def[cnum].wis_bonus = wisbonus;
			classes_def[cnum].dex_bonus = dexbonus;
			classes_def[cnum].con_bonus = conbonus;
			classes_def[cnum].chr_bonus = chrbonus;

			/* Next... */
			continue;
		}

		/* Read skills bonus */
		if (buf[0] == 'B')
		{
			int bonusskill, skillamt;

			/* Scan for the other values */
                        if (2 != sscanf(buf+2, "%d:%d",
				&bonusskill, &skillamt)) return (1);

			/* Save the values */
			classes_def[cnum].skills_bonus[bonusskill] = skillamt;

			/* Next... */
			continue;
		}

		/* Read ranks names */
		if (buf[0] == 'K')
		{
			int pos;
			char rnamem[80];
			char rnamef[80];
			char tmp[80];
			char c;

			/* Read male title */
			pos = 2;
			c = buf[pos];
			strcpy(rnamem, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", rnamem, c);
				strcpy(rnamem, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;
			/* Read female title */
			c = buf[pos];
			strcpy(rnamef, "");
			while (c != ';')
			{
				sprintf(tmp, "%s%c", rnamef, c);
				strcpy(rnamef, tmp);
				pos = pos + 1;
				c = buf[pos];
			}

			/* Save the values */
                        strcpy(classes_def[cnum].ranksm[currank], rnamem);
			strcpy(classes_def[cnum].ranksf[currank], rnamef);

			/* Increase the current rank. */
			currank += 1;			

			/* Next... */
			continue;
		}

		/* Abilities for the class */
		if (buf[0] == 'A')
		{
			int pos;
			char aname[80];
			char tmp[80];
			char c;
			int passive, scripted, powerid, cfeat;

			/* Read skill name */
			pos = 2;
			c = buf[pos];
			strcpy(aname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", aname, c);
				strcpy(aname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (4 != sscanf(buf+pos, "%d:%d:%d:%d",
				&passive, &scripted, &powerid, &cfeat)) return (1);

			/* Save the values */
                        strcpy(abilities_def[(cnum * 10) + curability].name, aname);
			abilities_def[(cnum * 10) + curability].abtype = passive;
			abilities_def[(cnum * 10) + curability].hardcode = scripted;
			abilities_def[(cnum * 10) + curability].powerid = powerid;
			abilities_def[(cnum * 10) + curability].combatfeat = cfeat;

			/* Advance to next ability */
			curability += 1;

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}

	/* Success */
	return (0);
}

/* Init skill feats! */
int init_feats()
{
	int i, x;

	char *s, *t;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	int curfeat = 1;

	/* Reset the feats required skills */
	for (x = 0; x < (SKILL_MAX * 10); x++)
	{
		feats_def[x].skill = -1;
	}

	/* Determine the name of the file */
	sprintf(filename, "feats.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'feats.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);

		/* Read the feats */
		if (buf[0] == 'F')
		{
			int pos;
			char fname[80];
			char tmp[80];
			char c;
			int skill, skillreq, scripted, powerid, cfeat;

			/* Read feat name */
			pos = 2;
			c = buf[pos];
			strcpy(fname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", fname, c);
				strcpy(fname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (5 != sscanf(buf+pos, "%d:%d:%d:%d:%d",
				&skill, &skillreq, &scripted, &powerid, &cfeat)) return (1);

			/* Save the values */
                        strcpy(feats_def[curfeat].name, fname);
			feats_def[curfeat].abtype = 1;
			feats_def[curfeat].skill = skill;
			feats_def[curfeat].reqskill = skillreq;
			feats_def[curfeat].hardcode = scripted;
			feats_def[curfeat].powerid = powerid;
			feats_def[curfeat].combatfeat = cfeat;

			/* Advance to next ability */
			curfeat += 1;

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}

	/* Success */
	return (0);
}

/* Init resistances! */
int init_resistances()
{
	int i, x;

	char *s, *t;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	int curres = 1;

	/* Reset the resistances */
	for (x = 0; x < MAX_RESIST; x++)
	{
		resistances_def[x].element = 0;
		resistances_def[x].magicitem = FALSE;
	}

	/* Determine the name of the file */
	sprintf(filename, "resist.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'resist.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);

		/* Read the feats */
		if (buf[0] == 'R')
		{
			int pos;
			char rname[80];
			char tmp[80];
			char c;
			int element, mitem;

			/* Read resistance name */
			pos = 2;
			c = buf[pos];
			strcpy(rname, "");
			while (c != ':')
			{
				sprintf(tmp, "%s%c", rname, c);
				strcpy(rname, tmp);
				pos = pos + 1;
				c = buf[pos];
			}
			pos = pos + 1;

			/* Scan for the other values */
                        if (2 != sscanf(buf+pos, "%d:%d",
				&element, &mitem)) return (1);

			/* Save the values */
                        strcpy(resistances_def[curres].name, rname);
			resistances_def[curres].element = element;
			if (mitem == 1) resistances_def[curres].magicitem = TRUE;
			else resistances_def[curres].magicitem = FALSE;

			/* Advance to next resistance */
			curres += 1;

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}

	/* Success */
	return (0);
}

/* Init stores inventory based on a town. */
/* This function is called everytime the player enters a town */
void init_stores_inven(int townnum)
{
	int i, j;
	int item = 0;

	char *s, *t;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	bool townfound = FALSE;

	/* Clean the inventory of all stores */
	for (i = 0; i < MAX_STORES; i++)
	{
		store_type *st_ptr = &stores[i];

		/* Set owner to 0. */
		st_ptr->owner = 0;

		for (j = 0; j < st_ptr->table_size; j++)
		{
			st_ptr->table[j] = 0;
		}
		st_ptr->table_num = 0;
	}

	/* Determine the name of the file */
	sprintf(filename, "stores.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'stores.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse */
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* Advance the line number */
		error_line++;

		/* Skip comments and blank lines */
		if (!buf[0] || (buf[0] == '#')) continue;

		/* Verify correct "colon" format */
		if (buf[1] != ':') return (1);

		/* Read class names */
		if (buf[0] == 'T')
		{
			int tnum, tnummax;

			/* Already found a town? Return. */
			if (townfound)
			{
				/* Close the file */
				my_fclose(fp);
				return;
			}

			/* Scan for the other values */
                        if (2 != sscanf(buf+2, "%d:%d",
				&tnum, &tnummax)) return (1);

			if ((townnum >= tnum && townnum <= tnummax) || (tnum == 0 && tnummax == 0)) townfound = TRUE;

			/* Next... */
			continue;
		}

		/* Item line */
		if (buf[0] == 'I')
		{
			int shopid, inum;
			store_type *st_ptr;

			if (!(townfound)) continue;

			/* Scan for the other values */
                        if (2 != sscanf(buf+2, "%d:%d",
				&shopid, &inum)) return (1);

			st_ptr = &stores[shopid];

			/* Save the values */
			st_ptr->table[st_ptr->table_num++] = inum;

			/* Next... */
			continue;
		}

		/* Close the file before returning anything. */
		my_fclose(fp);

		/* Oops */
		return (6);
	}

	/* Close the file */
	my_fclose(fp);

	/* Success */
	return (0);
}

/* Initialize vaults! */
int init_vaults()
{
	int y,x;
	int ex, ey, evtype, evinfo, evextra, evextra2, evcond, evcondval, evset, evsetval;
	int ddiag, devent, deventset, tel;
	int wid, hgt;
	int mind, maxd, typ, rarity;

	int num = 0;
	char filename[80];
	char quitmessage[80];
	FILE *fp;
	char buf[1024];
	int ypos = 0;
	int xpos = 0;

	/* Clear the classes list first */
	for (x = 0; x < MAX_VAULTS; x++)
	{
		vaults_def[x].created = FALSE;
		vaults_def[x].num = 0;
	}

	/* Determine the name of the file */
	sprintf(filename, "vaults.txt");

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, filename);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	sprintf(quitmessage, "Cannot open 'vaults.txt' file.");
	if (!fp) quit(quitmessage);

	/* Parse the file */
	/* Parse */

        while (0 == my_fgets(fp, buf, 1024))
	{
		if (buf[0] == 'N')
		{
			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d",
                                &num)) return (1);

			vaults_def[num].num = num;
			vaults_def[num].created = TRUE;

			/* Next... */
			continue;
		}
		if (buf[0] == 'I')
		{
			/* Scan for the values */
			if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
                                &wid, &hgt, &mind, &maxd, &tel, &typ, &rarity)) return (1);

			vaults_def[num].width = wid;
			vaults_def[num].height = hgt;
			vaults_def[num].mindlv = mind;
			vaults_def[num].maxdlv = maxd;
			vaults_def[num].teleport = tel;
			vaults_def[num].type = typ;
			vaults_def[num].rarity = rarity;

			/* Next... */
			continue;
		}
	}

	/* Close it */
	my_fclose(fp);

	/* Success */
	return (0);
}
