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
	"CLAW",
	"BITE",
	"STING",
	"SLASH",
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
	"XXX",
	"BEG",
	"INSULT",
	"XXX",
	"SING",
	"SHOOT",
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
	"DR_MANA",
	"SUPERHURT",
	"STONE",
	"HOLY",
	"HELL",
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
	"XXX",
	"ATTR_CLEAR",
	"ATTR_MULTI",
	"FORCE_DEPTH",
	"FORCE_MAXHP",
	"FORCE_SLEEP",
	"NO_ESCORT",
	"XXX1",
	"FRIENDS",
	"XXX",
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
	"DROP_SPECIAL",
	"XXX2"
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
	"VAMPIRE",
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
	"WALSTANIAN",
	"GARGASTAN",
	"BACRUM",
	"ZENOBIAN",
	"LODIS",
	"XXX",
	"HUMAN",
	"XXX"
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
	"TEMPLE",
	"GOOD",
	"AURA_COLD",
	"NONLIVING",
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_FIRE",
	"HURT_COLD",
	"RES_TELE",
	"XXX",
	"NO_FEAR",
	"NO_STUN",
	"NO_CONF",
	"NO_SLEEP",
	"NO_STONE",
	"ELEM_FIRE",
	"ELEM_AQUA",
	"ELEM_EARTH",
	"ELEM_WIND",
	"ELEM_MULTI",
	"ANTI_MAGIC",
	"HURT_ACID",
	"HURT_ELEC",
	"FEAR_FIELD",
};

/*
 * Monster race flags
 */
static cptr r_info_flags4[] =
{
	"SHRIEK",
	"SHIFT_ELEM",
	"DISPEL",
	"ROCKET",
	"SHOOT",
	"SHOOT_GUN",
	"XXX3",
	"XXX4",
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
	"BR_STON",
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
	"BA_LITE",
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
	"STOP_TIME",
	"SPECIAL",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"GODLY_SPEAR",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"ANIM_DEAD", /* ToDo: Implement ANIM_DEAD */
	"S_KIN",
	"XXX",
	"S_MONSTER",
	"S_MONSTERS",
	"S_ANT",
	"S_SPIDER",
	"S_HOUND",
	"S_BEAST",
	"S_ANGEL",
	"S_DEMON",
	"S_UNDEAD",
	"S_DRAGON",
	"S_HI_UNDEAD",
	"S_HI_DRAGON",
	"S_TEMPLES",
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
	"NAZGUL",
	"UNIQUE2",
	"RIDING",
	"XXX7X07",
	"HAS_LITE_1",
	"SELF_LITE_1",
	"HAS_LITE_2",
	"SELF_LITE_2",
	"GUARDIAN",
	"XXX7X13",
	"KILL_EXP",
	"LAWFUL",
	"CHAOTIC",
	"ZENOBIAN_FORCES",
	"LONG_RANGE",
	"EGG_ONLY",
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
	"WILD_SWAMP",	/* ToDo: Implement Swamp */
	"WILD_ALL",
};


/*
 * Monster race flags - Drops
 */
static cptr r_info_flags9[] =
{
	"DROP_CORPSE",
	"DROP_SKELETON",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
};


/*
 * Additional monster race flags
 */
static cptr r_info_flagsa[] =
{
	"FIRE_STORM",
	"AQUA_STORM",
	"EARTH_STORM",
	"WIND_STORM",
	"BR_PURE_FIRE",
	"BR_PURE_AQUA",
	"BR_PURE_EARTH",
	"BR_PURE_WIND",
	"PETRO_CLOUD",
	"SAND_STORM",
	"ERASE_ELEM",
	"CHANGE_ELEM",
	"SALAMANDER",
	"FENRER",
	"GNOME",
	"THUNDERBIRD",
	"IGNIS_FATUUS",
	"DARK_LORE",
	"STONE_GAZE",
	"HOLY_ORB",
	"DARK_FIRE",
	"S_ZENOBIAN",
	"S_HI_DEMON",
	"BA_DISI",
	"PURE_ELEM_BEAM",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
};


/*
 * Monster race flags - Resistances
 */
static cptr r_info_flagsr[] =
{
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_LITE",
	"RES_DARK",
	"RES_NETH",
	"RES_WATE",
	"RES_PLAS",
	"RES_SHAR",
	"RES_SOUN",
	"RES_CONF",
	"RES_CHAO",
	"RES_STON",
	"RES_DISE",
	"RES_WALL",
	"RES_INER",
	"RES_TIME",
	"RES_GRAV",
	"RES_EDGED",
	"RES_BLUNT",
	"IM_EDGED",
	"IM_BLUNT",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
};


/*
 * Object flags
 */
static cptr k_info_flags[] =
{
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"FORCE_WEAPON",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
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

	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	"RIDING",
	"SLAY_GOOD",
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
	"RES_STONE",
	"RES_CHAOS",
	"RES_DISEN",

	"SH_FIRE",
	"SH_ELEC",
	"SLAY_HUMAN",
	"SH_COLD",
	"NO_TELE",
	"NO_MAGIC",
	"DEC_MANA",
	"TY_CURSE",
	"WARNING",
	"UNHOLY",
	"SHOW_MODS",
	"SLAY_LIVING",
	"LEVITATION",
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
	"XXX",
	"EASY_SPELL",
	"WRAITH",

	"XXX",
	"FULL_NAME",
	"BALDAR",
	"RES_MAGIC",
	"EXTRA_VORPAL",
	"REGEN_MANA",
	"FEAR_FIELD",
	"FEMALE_ONLY",
	"MALE_ONLY",

	"KILL_ANIMAL",
	"KILL_EVIL",
	"KILL_UNDEAD",
	"KILL_DEMON",
	"KILL_ORC",
	"KILL_TROLL",
	"KILL_GIANT",
	"KILL_HUMAN",
	"KILL_GOOD",
	"KILL_LIVING",
	"FIXED_FLAVOR",
	"XXX",
	"XXX",
};


static cptr k_info_gen_flags[] =
{
	"INSTA_ART",
	"QUESTITEM",
	"XTRA_POWER",
	"ONE_SUSTAIN",
	"XTRA_RES_OR_POWER",
	"XTRA_H_RES",
	"XTRA_E_RES",
	"XTRA_L_RES",
	"XTRA_D_RES",
	"XTRA_RES",
	"CURSED",
	"HEAVY_CURSE",
	"PERMA_CURSE",
	"RANDOM_CURSE0",
	"RANDOM_CURSE1",
	"RANDOM_CURSE2",
	"RUNEWEAPON",
	"WALSTANIAN",
	"GARGASTAN",
	"BACRUM",
	"ZENOBIAN",
	"LODIS",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
	"XXX",
};


/*
 * Dungeon flags
 */
static cptr d_info_flags1[] =
{
	"WINNER",
	"MAZE",
	"SMALLEST",
	"BEGINNER",
	"BIG",
	"NO_DOORS",
	"WATER_RIVER",
	"LAVA_RIVER",
	"SMALLER",
	"CLOSED",
	"CAVE",
	"CAVERN",
	"VAULT",
	"XXX",
	"XXX",
	"NO_BACK",
	"XXX",
	"LAKE_WATER",
	"LAKE_LAVA",
	"LAKE_RUBBLE",
	"LAKE_TREE",
	"NO_VAULT",
	"ARENA",
	"DESTROY",
	"WILD_LITE",
	"NO_CAVE",
	"XXX",
	"XXX",
	"LITE",
	"XXX",
	"UPWARD",
	"XXX"
};


/*
 * Race flags
 */
static cptr p_info_flags[] =
{
	"DEMON",
	"UNDEAD",
	"NO_DIGEST",
	"MALE",
	"FEMALE",
	"EVOLUTION",
	"GOOD",
	"EVIL",
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
 * Class flags
 */
static cptr c_info_flags[] =
{
	"LAWFUL",
	"NEUTRAL",
	"CHAOS",
	"GOOD",
	"EVIL",
	"MALE",
	"FEMALE",
	"BIRTH",
	"REINCARNATE",
	"NO_CHANGE",
	"DEMON",
	"UNDEAD",
	"NO_DIGEST",
	"SECRET",
	"ALIEN",
	"SEE_DARK",
	"REALM_ELEM_1",
	"MONK_ARMOUR",
	"XXX19",
	"XXX20",
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
 * Class realms flags
 */
static cptr c_info_realms[] =
{
	"MAGERY",
	"FIRE",
	"AQUA",
	"EARTH",
	"WIND",
	"HOLY",
	"DEATH",
	"SYMBIOTIC",
	"WITCH",
	"DRAKONITE",
	"CRUSADE",
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
 * Add a text to the text-storage and store offset to it.
 *
 * Returns FALSE when there isn't enough space available to store
 * the text.
 */
static bool add_text(u32b *offset, header *head, cptr buf, bool normal_text)
{
	/* Hack -- Verify space */
	if (head->text_size + strlen(buf) + 8 > FAKE_TEXT_SIZE)
		return (FALSE);

	/* New text? */
	if (*offset == 0)
	{
		/* Advance and save the text index */
		*offset = ++head->text_size;
	}

	/* Additional text */
	else if (normal_text)
	{
		/*
		 * If neither the end of the last line nor
		 * the beginning of current line is not a space,
		 * fill up a space as a correct separator of two words.
		 */
		if (head->text_size > 0 &&
#ifdef JP
		    (*(head->text_ptr + head->text_size - 1) != ' ') &&
		    ((head->text_size == 1) || !iskanji(*(head->text_ptr + head->text_size - 2))) && 
		    (buf[0] != ' ') && !iskanji(buf[0])
#else
		    (*(head->text_ptr + head->text_size - 1) != ' ') &&
		    (buf[0] != ' ')
#endif
		    )
		{
			/* Append a space */
			*(head->text_ptr + head->text_size) = ' ';

			/* Advance the index */
			head->text_size++;
		}
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
 * Returns FALSE when there isn't enough space available to store
 * the name.
 */
static bool add_name(u32b *offset, header *head, cptr buf)
{
	/* Hack -- Verify space */
	if (head->name_size + strlen(buf) + 8 > FAKE_NAME_SIZE)
		return (FALSE);

	/* New name? */
	if (*offset == 0)
	{
		/* Advance and save the name index */
		*offset = ++head->name_size;
	}

	/* Append chars to the names */
	strcpy(head->name_ptr + head->name_size, buf);

	/* Advance the index */
	head->name_size += strlen(buf);

	/* Success */
	return (TRUE);
}


/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
byte color_char_to_attr(char c)
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

	return (255);
}



/*** Initialize from ascii template files ***/


/*
 * Initialize an "*_info" array, by parsing an ascii "template" file
 */
errr init_info_txt(FILE *fp, char *buf, header *head,
                   parse_info_txt_func parse_info_txt_line)
{
	errr err;

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
			/* ignore */
			continue;
		}

		/* Mega Hack -- Calculate Check Sum */
		if (buf[0] != 'N' && buf[0] != 'D')
		{
			int i;
			for (i = 0; buf[i]; i++)
			{
				head->v_extra += (byte)buf[i];
				head->v_extra ^= (1 << (i % 8));
			}
		}

		/* Parse the line */
		if ((err = (*parse_info_txt_line)(buf, head)) != 0)
			return (err);
	}


	/* Complete the "name" and "text" sizes */
	if (head->name_size) head->name_size++;
	if (head->text_size) head->text_size++;

	/* Mega-Hack -- Save some space for the "Runeweapon" info */
	if (head == &r_head)
	{
		head->name_size += MAX_NLEN * MAX_RUNEWEAPON;
		head->text_size += MAX_NLEN * 4 * MAX_RUNEWEAPON;
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
		s = my_strchr(buf+2, ':');

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
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		v_ptr = &v_info[i];

		/* Store the name */
		if (!add_name(&v_ptr->name, head, s)) return (7);
	}

	/* There better be a current v_ptr */
	else if (!v_ptr) return (3);

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* Acquire the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&v_ptr->text, head, s, FALSE)) return (7);
	}

	/* Process 'X' for "Extra info" (one line only) */
	else if (buf[0] == 'X')
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
	}

	/* Oops */
	else	return (6);

	/* Success */
	return (0);
}



/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr parse_s_info(char *buf, header *head)
{
	int i;

	/* Current entry */
	static skill_table *s_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		s_ptr = &s_info[i];
	}

	/* There better be a current s_ptr */
	else if (!s_ptr) return (3);

	/* Process 'W' for "Weapon skill gain rate" */
	else if (buf[0] == 'W')
	{
		int wt, eff;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
				&wt, &eff)) return (1);

		if (eff > 11) return (0);
		if (eff < 1) return (0);

		/* Save the values */
		s_ptr->w_eff[wt] = eff;
	}

	/* Process 'M' for "Misc. skill efficiency" */
	else if (buf[0] == 'M')
	{
		int num, eff;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
				&num, &eff)) return (1);

		if (eff > 11) return (0);
		if (eff < 1) return (0);

		/* Save the values */
		s_ptr->m_eff[num] = eff;
	}

	/* Process 'S' for "Spell skill efficiency" */
	else if (buf[0] == 'S')
	{
		int realm, eff;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
				&realm, &eff)) return (1);

		if (eff > 20) return (20);
		if (eff < 0) return (0);

		/* Save the values */
		s_ptr->s_eff[realm] = eff;
	}

	/* Oops */
	else return (6);

	/* Success */
	return (0);
}


/*
 * Initialize the "m_info" array, by parsing an ascii "template" file
 */
errr parse_m_info(char *buf, header *head)
{
	int i;

	char *s;

	/* Current entry */
	static player_magic *m_ptr = NULL;

	/* ---Hack--- */
	static int realm, magic_idx = 0, readable = 0;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		m_ptr = &m_info[i];
	}

	/* There better be a current m_ptr */
	else if (!m_ptr) return (3);

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		char *book, *stat;
		int xtra, weight;

		/* Find the colon before the name */
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		book = buf+2;

		if (streq(book, "MAGERY")) m_ptr->spell_book = TV_MAGERY_BOOK;
		else if (streq(book, "HOLY")) m_ptr->spell_book = TV_HOLY_BOOK;
		else if (streq(book, "NONE")) m_ptr->spell_book = 0;
		else return (5);

		stat = s;

		/* Find the colon before the name */
		s = my_strchr(s, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		if (streq(stat, "STR")) m_ptr->spell_stat = A_STR;
		else if (streq(stat, "INT")) m_ptr->spell_stat = A_INT;
		else if (streq(stat, "WIS")) m_ptr->spell_stat = A_WIS;
		else if (streq(stat, "DEX")) m_ptr->spell_stat = A_DEX;
		else if (streq(stat, "CON")) m_ptr->spell_stat = A_CON;
		else if (streq(stat, "CHR")) m_ptr->spell_stat = A_CHR;
		else return (5);


		/* Scan for the values */
		if (2 != sscanf(s, "%x:%d",
				(uint *)&xtra, &weight))	return (1);

		m_ptr->spell_xtra = xtra;
		m_ptr->spell_weight = weight;
	}


	/* Process 'R' for "Realm" (one line only) */
	else if (buf[0] == 'R')
	{
		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
				&realm, &readable)) return (1);

		magic_idx = 0;
	}

	else if (buf[0] == 'T')
	{
		int level, mana, fail;

		if (!readable) return (1);
		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&level, &mana, &fail)) return (1);

		m_ptr->info[realm][magic_idx].slevel = level;
		m_ptr->info[realm][magic_idx].smana = mana;
		m_ptr->info[realm][magic_idx].sfail = fail;
		magic_idx ++;
	}


	/* Oops */
	else return (6);

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
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		f_ptr = &f_info[i];

#ifdef JP
		/* Store the name */
		if (!add_name(&f_ptr->name, head, s)) return (7);
#endif
		/* Default "mimic" */
		f_ptr->mimic = i;
	}

	/* There better be a current f_ptr */
	else if (!f_ptr) return (3);

#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名としている */
	else if (buf[0] == 'E')
	{
		/* nothing to do */
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&f_ptr->name, head, s)) return (7);
	}
#endif


	/* Process 'M' for "Mimic" (one line only) */
	else if (buf[0] == 'M')
	{
		int mimic;

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
				&mimic)) return (1);

		/* Save the values */
		f_ptr->mimic = mimic;

	}


	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		byte tmp;

		/* Paranoia */
		if (!buf[2]) return (1);
		if (!buf[3]) return (1);
		if (!buf[4]) return (1);

		/* Extract the color */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp > 127) return (1);

		/* Save the values */
		f_ptr->d_attr = tmp;
		f_ptr->d_char = buf[2];

	}


	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int cur_elem, elem_buf[ELEM_NUM], offence, defence;

		/* Scan for the other values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
		    &elem_buf[ELEM_FIRE], &elem_buf[ELEM_AQUA], &elem_buf[ELEM_EARTH], &elem_buf[ELEM_WIND],
		    &offence, &defence)) return (1);

		/* Save the values */
		for (cur_elem = MIN_ELEM; cur_elem < ELEM_NUM; cur_elem++)
			f_ptr->elem[cur_elem] = elem_buf[cur_elem];
		f_ptr->to_offence = offence;
		f_ptr->to_defence = defence;
	}

	/* Oops */
	else	return (6);

	/* Success */
	return (0);
}


/*
 * Object stat bonus flags header
 */
static cptr k_info_stat_flags_header[] =
{
	"STR",
	"INT",
	"WIS",
	"DEX",
	"CON",
	"CHR",
};


/*
 * Object misc bonus flags header
 */
static cptr k_info_misc_flags_header[] =
{
	"MAGIC_MASTERY",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"BLOWS",
	"ANTI_MAGIC",
};


/*
 * Object align bonus flags header
 */
static cptr k_info_align_flags_header[] =
{
	"ALIGN_LNC",
	"ALIGN_GNE",
};


static bool grab_object_bonus(void *to_stat, void *to_misc, void *to_align, u32b flags[TR_FLAG_SIZE], int size, cptr s)
{
	char fmt_buf[32];
	int  to_bonus, i;

	/* Hack -- Read bonus to all stats */
	if (1 == sscanf(s, "ALL_STATS_%d", &to_bonus))
	{
		for (i = 0; i < A_MAX; i++)
		{
			/* Extract a bonus to stat */
			if (size == sizeof(s16b)) *((s16b *)to_stat + i) = to_bonus;
			else if (size == sizeof(byte)) *((byte *)to_stat + i) = to_bonus;
			else return FALSE;

			add_flag(flags, a_to_tr[i]);
		}

		return TRUE;
	}

	/* Hack -- Read bonus to stat */
	for (i = 0; i < A_MAX; i++)
	{
		sprintf(fmt_buf, "%s_%%d", k_info_stat_flags_header[i]);

		if (1 == sscanf(s, fmt_buf, &to_bonus))
		{
			/* Extract a bonus to stat */
			if (size == sizeof(s16b)) *((s16b *)to_stat + i) = to_bonus;
			else if (size == sizeof(byte)) *((byte *)to_stat + i) = to_bonus;
			else return FALSE;

			add_flag(flags, a_to_tr[i]);
			return TRUE;
		}
	}

	/* Hack -- Read bonus to misc */
	for (i = 0; i < OB_MAX; i++)
	{
		sprintf(fmt_buf, "%s_%%d", k_info_misc_flags_header[i]);

		if (1 == sscanf(s, fmt_buf, &to_bonus))
		{
			/* Extract a bonus to stat */
			if (size == sizeof(s16b)) *((s16b *)to_misc + i) = to_bonus;
			else if (size == sizeof(byte)) *((byte *)to_misc + i) = to_bonus;
			else return FALSE;

			add_flag(flags, ob_to_tr[i]);
			return TRUE;
		}
	}

	/* Hack -- Read bonus to align */
	for (i = 0; i < ALI_MAX; i++)
	{
		sprintf(fmt_buf, "%s_%%d", k_info_align_flags_header[i]);

		if (1 == sscanf(s, fmt_buf, &to_bonus))
		{
			/* Extract a bonus to stat */
			if (size == sizeof(s16b)) *((s16b *)to_align + i) = to_bonus;
			else if (size == sizeof(byte)) *((s16b *)to_align + i) = to_bonus;
			else return FALSE;

			add_flag(flags, ali_to_tr[i]);
			return TRUE;
		}
	}

	return FALSE;
}


/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(k_ptr->flags, i);
			return (0);
		}
	}

	/* Check gen_flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_gen_flags[i]))
		{
			k_ptr->gen_flags |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のアイテム・フラグ '%s'。", what);
#else
	msg_format("Unknown object flag '%s'.", what);
#endif


	/* Error */
	return (1);
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
#ifdef JP
		char *flavor;
#endif

		/* Find the colon before the name */
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		k_ptr = &k_info[i];

#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);

		/* Find the colon before the flavor */
		flavor = my_strchr(s, ':');

		/* Verify that colon */
		if (flavor)
		{
			/* Nuke the colon, advance to the flavor */
			*flavor++ = '\0';

			/* Store the flavor */
			if (!add_name(&k_ptr->flavor_name, head, flavor)) return (7);
		}

		/* Store the name */
		if (!add_name(&k_ptr->name, head, s)) return (7);
#endif
	}

	/* There better be a current k_ptr */
	else if (!k_ptr) return (3);


#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名としている */
	else if (buf[0] == 'E')
	{
		/* nothing to do */
	}
#else
	else if (buf[0] == 'E')
	{
		char *flavor;

		/* Acquire the Text */
		s = buf+2;

		/* Find the colon before the flavor */
		flavor = my_strchr(s, ':');

		/* Verify that colon */
		if (flavor)
		{
			/* Nuke the colon, advance to the flavor */
			*flavor++ = '\0';

			/* Store the flavor */
			if (!add_name(&k_ptr->flavor_name, head, flavor)) return (7);
		}

		/* Store the name */
		if (!add_name(&k_ptr->name, head, s)) return (7);
	}
#endif

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&k_ptr->text, head, s, TRUE)) return (7);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char sym;
		byte tmp;

		/* Paranoia */
		if (!buf[2]) return (1);
		if (!buf[3]) return (1);
		if (!buf[4]) return (1);

		/* Extract the char */
		sym = buf[2];

		/* Extract the attr */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp > 127) return (1);

		/* Save the values */
		k_ptr->d_attr = tmp;
		k_ptr->d_char = sym;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&tval, &sval, &pval)) return (1);

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

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
				&level, &extra, &wgt, &cost)) return (1);

		/* Save the values */
		k_ptr->level = level;
		k_ptr->extra = extra;
		k_ptr->weight = wgt;
		k_ptr->cost = cost;
	}

	/* Process 'A' for "Allocation" (one line only) */
	else if (buf[0] == 'A')
	{
		/* XXX XXX XXX Simply read each number following a colon */
		for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
		{
			/* Default chance */
			k_ptr->chance[i] = 1;

			/* Store the attack damage index */
			k_ptr->locale[i] = atoi(s+1);

			/* Find the slash */
			t = my_strchr(s+1, '/');

			/* Find the next colon */
			s = my_strchr(s+1, ':');

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

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
				&ac, &hd1, &hd2, &th, &td, &ta)) return (1);

		k_ptr->ac = ac;
		k_ptr->dd = hd1;
		k_ptr->ds = hd2;
		k_ptr->to_h = th;
		k_ptr->to_d = td;
		k_ptr->to_a =  ta;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
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

			if (grab_object_bonus((void *)k_ptr->to_stat, (void *)k_ptr->to_misc, (void *)k_ptr->to_align, k_ptr->flags, sizeof(s16b), s))
			{
				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_kind_flag(k_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}


	/* Oops */
	else return (6);


	/* Success */
	return (0);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(a_ptr->flags, i);
			return (0);
		}
	}

	/* Check gen_flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_gen_flags[i]))
		{
			a_ptr->gen_flags |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知の伝説のアイテム・フラグ '%s'。", what);
#else
	msg_format("Unknown artifact flag '%s'.", what);
#endif


	/* Error */
	return (1);
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
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';
#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i < error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		a_ptr = &a_info[i];

		/* Ignore everything */
		add_flag(a_ptr->flags, TR_IGNORE_ACID);
		add_flag(a_ptr->flags, TR_IGNORE_ELEC);
		add_flag(a_ptr->flags, TR_IGNORE_FIRE);
		add_flag(a_ptr->flags, TR_IGNORE_COLD);
#ifdef JP
		/* Store the name */
		if (!add_name(&a_ptr->name, head, s)) return (7);
#endif
	}

	/* There better be a current a_ptr */
	else if (!a_ptr) return (3);


#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名としている */
	else if (buf[0] == 'E')
	{
		/* nothing to do */
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&a_ptr->name, head, s)) return (7);
	}
#endif

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&a_ptr->text, head, s, TRUE)) return (7);
	}


	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&tval, &sval, &pval)) return (1);

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

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
				&level, &rarity, &wgt, &cost)) return (1);

		/* Save the values */
		a_ptr->level = level;
		a_ptr->rarity = rarity;
		a_ptr->weight = wgt;
		a_ptr->cost = cost;
	}

	/* Hack -- Process 'P' for "power" and such */
	else if (buf[0] == 'P')
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
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
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

			if (grab_object_bonus((void *)a_ptr->to_stat, (void *)a_ptr->to_misc, (void *)a_ptr->to_align, a_ptr->flags, sizeof(s16b), s))
			{
				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_artifact_flag(a_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}


	/* Oops */
	else return (6);


	/* Success */
	return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(e_ptr->flags, i);
			return (0);
		}
	}

	/* Check gen_flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_gen_flags[i]))
		{
			e_ptr->gen_flags |= (1L << i);
			return (0);
		}
	}

/* Oops */
#ifdef JP
	msg_format("未知の名のあるアイテム・フラグ '%s'。", what);
#else
	msg_format("Unknown ego-item flag '%s'.", what);
#endif


	/* Error */
	return (1);
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


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';
#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i < error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		e_ptr = &e_info[i];
#ifdef JP
		/* Store the name */
		if (!add_name(&e_ptr->name, head, s)) return (7);
#endif
	}

	/* There better be a current e_ptr */
	else if (!e_ptr) return (3);


#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名 */
	else if (buf[0] == 'E')
	{
		/* nothing to do */
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&e_ptr->name, head, s)) return (7);
	}
#endif
#if 0

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* Acquire the text */
		s = buf+2;

		/* Store the text */
		if (!add_text(&e_ptr->text, head, s, TRUE)) return (7);
	}

#endif

	/* Process 'X' for "Xtra" (one line only) */
	else if (buf[0] == 'X')
	{
		int slot, rating;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
				&slot, &rating)) return (1);

		/* Save the values */
		e_ptr->slot = slot;
		e_ptr->rating = rating;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
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
	}

	/* Hack -- Process 'C' for "creation" */
	else if (buf[0] == 'C')
	{
		int th, td, ta;

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
				&th, &td, &ta)) return (1);

		e_ptr->max_to_h = th;
		e_ptr->max_to_d = td;
		e_ptr->max_to_a = ta;
	}

	/* Hack -- Process 'F' for flags */
	else if (buf[0] == 'F')
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

			if (grab_object_bonus((void *)e_ptr->max_to_stat, (void *)e_ptr->max_to_misc, (void *)e_ptr->to_align, e_ptr->flags, sizeof(byte), s))
			{
				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_ego_item_flag(e_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Oops */
	else return (6);

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

	/* Scan flagsR */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flagsr[i]))
		{
			r_ptr->flagsr |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のモンスター・フラグ '%s'。", what);
#else
	msg_format("Unknown monster flag '%s'.", what);
#endif


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

	/* Scan flagsA */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flagsa[i]))
		{
			r_ptr->flagsa |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のモンスター・フラグ '%s'。", what);
#else
	msg_format("Unknown monster flag '%s'.", what);
#endif


	/* Failure */
	return (1);
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
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';
#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i < error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		r_ptr = &r_info[i];
#ifdef JP
		/* Store the name */
		if (!add_name(&r_ptr->name, head, s)) return (7);
#endif
	}

	/* There better be a current r_ptr */
	else if (!r_ptr) return (3);


#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名 */
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&r_ptr->E_name, head, s)) return (7);
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&r_ptr->name, head, s)) return (7);
	}
#endif
	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&r_ptr->text, head, s, TRUE)) return (7);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char sym;
		byte tmp;

		/* Paranoia */
		if (!buf[2]) return (1);
		if (!buf[3]) return (1);
		if (!buf[4]) return (1);

		/* Extract the char */
		sym = buf[2];

		/* Extract the attr */
		tmp = color_char_to_attr(buf[4]);

		/* Paranoia */
		if (tmp > 127) return (1);

		/* Save the values */
		r_ptr->d_char = sym;
		r_ptr->d_attr = tmp;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp1, hp2, aaf, ac, slp;

		/* Scan for the other values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
				&spd, &hp1, &hp2, &aaf, &ac, &slp)) return (1);

		/* Save the values */
		r_ptr->speed = spd;
		r_ptr->hdice = MAX(hp1, 1);
		r_ptr->hside = MAX(hp2, 1);
		r_ptr->aaf = aaf;
		r_ptr->ac = ac;
		r_ptr->sleep = slp;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, pad;
		long exp;
		long nextexp;
		int nextmon;

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%ld:%ld:%d",
				&lev, &rar, &pad, &exp, &nextexp, &nextmon)) return (1);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
		r_ptr->extra = pad;
		r_ptr->mexp = exp;
		r_ptr->next_exp = nextexp;
		r_ptr->next_r_idx = nextmon;
	}

	/* Process 'B' for "Blows" (up to four lines) */
	else if (buf[0] == 'B')
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
	}

	/* Process 'F' for "Basic Flags" (multiple lines) */
	else if (buf[0] == 'F')
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

			/* Hack -- Read escort monster symbols */
			if (!strncmp(s, "ESCORT_", 7))
			{
				/* Skip "ESCORT_" */
				s += 7;

				/* Read a string */
				strncpy(r_ptr->escort_char, s, sizeof(r_ptr->escort_char));

				/* Start at next entry */
				s = t;

				/* Set old-style "ESCORT" flag */
				r_ptr->flags1 |= RF1_ESCORT;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_basic_flag(r_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'S' for "Spell Flags" (multiple lines) */
	else if (buf[0] == 'S')
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
				r_ptr->freq_spell = 100 / i;

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
	}

	/* Oops */
	else return (6);


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
#ifdef JP
	msg_format("未知のダンジョン・フラグ '%s'。", what);
#else
	msg_format("Unknown dungeon type flag '%s'.", what);
#endif

	/* Failure */
	return (1);
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

	/* Scan flagsA */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flagsa[i]))
		{
			d_ptr->mflagsa |= (1L << i);
			return (0);
		}
	}

	/* Scan flagsR */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flagsr[i]))
		{
			d_ptr->mflagsr |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のモンスター・フラグ '%s'。", what);
#else
	msg_format("Unknown monster flag '%s'.", what);
#endif
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
#ifdef JP
	msg_format("未知のモンスター・フラグ '%s'。", what);
#else
	msg_format("Unknown monster flag '%s'.", what);
#endif

	/* Failure */
	return (1);
}

/*
 * Initialize the "d_info" array, by parsing an ascii "template" file
 */
errr parse_d_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static dungeon_info_type *d_ptr = NULL;


	/* Process 'N' for "New/Number/Name" */
	if (buf[0] == 'N')
	{
		/* Find the colon before the name */
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';
#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif
		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i < error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		d_ptr = &d_info[i];
#ifdef JP
		/* Store the name */
		if (!add_name(&d_ptr->name, head, s)) return (7);
#endif
	}

#ifdef JP
	else if (buf[0] == 'E') return (0);
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&d_ptr->name, head, s)) return (7);
	}
#endif

	/* Process 'D' for "Description */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&d_ptr->text, head, s, TRUE)) return (7);
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int min_lev, max_lev;
		int min_plev, mode;
		int min_alloc, max_chance;
		int obj_good, obj_great;
		int pit, nest;

		/* Scan for the values */
		if (10 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%x:%x",
				 &min_lev, &max_lev, &min_plev, &mode, &min_alloc, &max_chance, &obj_good, &obj_great, (unsigned int *)&pit, (unsigned int *)&nest)) return (1);

		/* Save the values */
		d_ptr->mindepth = min_lev;
		d_ptr->maxdepth = max_lev;
		d_ptr->min_plev = min_plev;
		d_ptr->mode = mode;
		d_ptr->min_m_alloc_level = min_alloc;
		d_ptr->max_m_alloc_chance = max_chance;
		d_ptr->obj_good = obj_good;
		d_ptr->obj_great = obj_great;
		d_ptr->pit = pit;
		d_ptr->nest = nest;
	}

	/* Process 'P' for "Place Info" */
	else if (buf[0] == 'P')
	{
		int dy, dx;

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &dy, &dx)) return (1);

		/* Save the values */
		d_ptr->dy = dy;
		d_ptr->dx = dx;
	}

	/* Process 'L' for "fLoor type" (one line only) */
	else if (buf[0] == 'L')
	{
		int f1, f2, f3;
		int p1, p2, p3;
		int tunnel;

		/* Scan for the values */
		if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
		                &f1, &p1, &f2, &p2, &f3, &p3, &tunnel)) return (1);

		/* Save the values */
		d_ptr->floor1 = f1;
		d_ptr->floor_percent1 = p1;
		d_ptr->floor2 = f2;
		d_ptr->floor_percent2 = p2;
		d_ptr->floor3 = f3;
		d_ptr->floor_percent3 = p3;
		d_ptr->tunnel_percent = tunnel;
	}

	/* Process 'A' for "wAll type" (one line only) */
	else if (buf[0] == 'A')
	{
		int w1, w2, w3, outer, inner, stream1, stream2;
		int p1, p2, p3;

		/* Scan for the values */
		if (10 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
				 &w1, &p1, &w2, &p2, &w3, &p3, &outer, &inner, &stream1, &stream2)) return (1);

		/* Save the values */
		d_ptr->fill_type1 = w1;
		d_ptr->fill_percent1 = p1;
		d_ptr->fill_type2 = w2;
		d_ptr->fill_percent2 = p2;
		d_ptr->fill_type3 = w3;
		d_ptr->fill_percent3 = p3;
		d_ptr->outer_wall = outer;
		d_ptr->inner_wall = inner;
		d_ptr->stream1 = stream1;
		d_ptr->stream2 = stream2;
	}

	/* Process 'F' for "Dungeon Flags" (multiple lines) */
	else if (buf[0] == 'F')
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

			/* XXX XXX XXX Hack -- Read Final Object */
			if (1 == sscanf(s, "FINAL_OBJECT_%d", &artif))
			{
				/* Extract a "Final Artifact" */
				d_ptr->final_object = artif;

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
			if (1 == sscanf(s, "MONSTER_DIV_%d", &monst))
			{
				/* Extract a "Special %" */
				d_ptr->special_div = monst;

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
	}

	/* Process 'M' for "Basic Flags" (multiple lines) */
	else if (buf[0] == 'M')
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

			/* Hack -- Read monster symbols */
			if (!strncmp(s, "R_CHAR_", 7))
			{
				/* Skip "R_CHAR_" */
				s += 7;

				/* Read a string */
				strncpy(d_ptr->r_char, s, sizeof(d_ptr->r_char));

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
	}

	/* Process 'S' for "Spell Flags" (multiple lines) */
	else if (buf[0] == 'S')
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
	}

	/* Oops */
	else return (6);

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
 * Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>" -- info for dungeon grid
 */
static errr parse_line_feature(char *buf)
{
	int num;
	char *zz[9];


	if (init_flags & INIT_ONLY_BUILDINGS) return (0);

	/* Tokenize the line */
	if ((num = tokenize(buf+2, 9, zz, 0)) > 1)
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
					letter[index].object = atoi(zz[4]);
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
				else if (zz[3][0] == 'c')
				{
					letter[index].monster = - atoi(zz[3]+1);
				}
				else
				{
					letter[index].monster = atoi(zz[3]);

					/* Init the fixed-questor */
					if (init_flags & INIT_QUESTOR_UNIQUES)
					{
						if (r_info[letter[index].monster].flags1 & RF1_UNIQUE)
							r_info[letter[index].monster].flags1 |= RF1_QUESTOR;
					}
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
	char *zz[37];
	int index;
	char *s;

#ifdef JP
	if (buf[2] == '$')
		return 0;
	s = buf + 2;
#else
	if (buf[2] != '$')
		return 0;
	s = buf + 3;
#endif
	/* Get the building number */
	index = atoi(s);

	/* Find the colon after the building number */
	s = my_strchr(s, ':');

	/* Verify that colon */
	if (!s) return (1);

	/* Nuke the colon, advance to the sub-index */
	*s++ = '\0';

	/* Paranoia -- require a sub-index */
	if (!*s) return (1);

	/* Building definition sub-index */
	switch (s[0])
	{
		/* Building name, owner, race */
		case 'N':
		{
			if (tokenize(s + 2, 4, zz, 0) == 4)
			{
				/* Name of the building */
				strcpy(building[index].name, zz[0]);

				/* Name of the owner */
				strcpy(building[index].owner_name, zz[1]);

				/* Race of the owner */
				strcpy(building[index].owner_race, zz[2]);

				if (zz[3][0] == '0') building[index].chaos_frame_restr = FALSE;
				else building[index].chaos_frame_restr = TRUE;

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

		/* Building Action */
		case 'A':
		{
			if (tokenize(s + 2, 7, zz, 0) >= 6)
			{
				/* Index of the action */
				int action_index = atoi(zz[0]);

				/* Name of the action */
				strcpy(building[index].act_names[action_index], zz[1]);

				/* Cost of the action */
				building[index].costs[action_index] = atoi(zz[2]);

				/* Letter assigned to the action */
				building[index].letters[action_index] = zz[3][0];

				/* Action code */
				building[index].actions[action_index] = atoi(zz[4]);

				/* Action restriction */
				building[index].action_restr[action_index] = atoi(zz[5]);

				break;
			}

			return (PARSE_ERROR_TOO_FEW_ARGUMENTS);
		}

		case 'Z':
		{
			/* Ignore scripts */
			break;
		}

		default:
		{
			return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
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

				place_monster(*y, *x, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));

				monster_level = base_level;
			}
			else if (monster_index)
			{
				int old_cur_num, old_max_num;
				bool clone = FALSE;

				if (monster_index < 0)
				{
					monster_index = -monster_index;
					clone = TRUE;
				}
				old_cur_num = r_info[monster_index].cur_num;
				old_max_num = r_info[monster_index].max_num;

				/* Make alive again */
				if (r_info[monster_index].flags1 & RF1_UNIQUE)
				{
					r_info[monster_index].cur_num = 0;
					r_info[monster_index].max_num = 1;
				}

				/* Make alive again */
				if (r_info[monster_index].flags7 & RF7_NAZGUL)
				{
					if (r_info[monster_index].cur_num == r_info[monster_index].max_num)
					{
						r_info[monster_index].max_num++;
					}
				}

				/* Place it */
				place_monster_aux(0, *y, *x, monster_index, PM_ALLOW_SLEEP);
				if (clone)
				{
					/* clone */
					m_list[hack_m_idx_ii].smart1 |= SM1_CLONED;

					/* Make alive again for real unique monster */
					r_info[monster_index].cur_num = old_cur_num;
					r_info[monster_index].max_num = old_max_num;
				}
			}

			/* Object (and possible trap) */
			if ((random & RANDOM_OBJECT) && (random & RANDOM_TRAP))
			{
				object_level = base_level + object_index;

				/*
				 * Random trap and random treasure defined
				 * 25% chance for trap and 75% chance for object
				 */
				if (randint0(100) < 75)
				{
					place_object(*y, *x, AMF_OKAY);
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
				if (randint0(100) < 75)
					place_object(*y, *x, AMF_OKAY);
				else if (randint0(100) < 80)
					place_object(*y, *x, AMF_OKAY | AMF_GOOD);
				else
					place_object(*y, *x, AMF_OKAY | AMF_GOOD | AMF_GREAT);

				object_level = base_level;
			}
			/* Random trap */
			else if (random & RANDOM_TRAP)
			{
				place_trap(*y, *x);
			}
			/* Hidden trap (or door) */
			else if (letter[idx].trap)
			{
				c_ptr->mimic = c_ptr->feat;
				c_ptr->feat = letter[idx].trap;
			}
			else if (object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

				/* Create the item */
				object_prep(o_ptr, object_index);

				if (o_ptr->tval == TV_GOLD)
				{
					coin_type = object_index - OBJ_GOLD_LIST;
					make_gold(o_ptr);
					coin_type = 0;
				}

				/* Apply magic (no messages, no artifacts) */
				apply_magic(o_ptr, base_level, AMF_GOOD);

				/* Paranoia -- proper item can be an artifact */
				if ((random & RANDOM_ARTIFACT) &&
				    (o_ptr->tval >= TV_BULLET) &&
				    (o_ptr->tval <= TV_RING))
				{
					create_artifact(o_ptr, FALSE);
				}

				(void)drop_near(o_ptr, -1, *y, *x);
			}

			/* Artifact */
			if (artifact_index)
			{
				if (a_info[artifact_index].cur_num)
				{
					int k_idx = 447;
					object_type forge;
					object_type *q_ptr = &forge;

					object_prep(q_ptr, k_idx);

					/* Drop it in the dungeon */
					(void)drop_near(q_ptr, -1, *y, *x);
				}
				else
				{
					/* Create the artifact */
					if (create_named_art(artifact_index, *y, *x))
						a_info[artifact_index].cur_num = 1;
				}
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
		int num;
		int q_idx;
		quest_type *q_ptr;
#ifdef JP
		if (buf[2] == '$')
			return 0;
		num = tokenize(buf + 2, 33, zz, 0);
#else
		if (buf[2] != '$')
			return 0;
		num = tokenize(buf + 3, 33, zz, 0);
#endif

		/* Have we enough parameters? */
		if (num < 3) return (PARSE_ERROR_TOO_FEW_ARGUMENTS);

		/* Get the quest */
		q_idx = atoi(zz[0]);
		q_ptr = &(quest[q_idx]);

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
				q_ptr->dungeon = atoi(zz[9]);

				if (num > 10)
					q_ptr->flags  = atoi(zz[10]);

				if (q_idx != QUEST_RUNEWEAPON)
				{
					r_ptr = &r_info[q_ptr->r_idx];
					if (r_ptr->flags1 & RF1_UNIQUE)
						r_ptr->flags1 |= RF1_QUESTOR;
				}
				else
				{
					q_ptr->r_idx = max_r_idx;
				}

				a_ptr = &a_info[q_ptr->k_idx];
				a_ptr->gen_flags |= TRG_QUESTITEM;
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

	/* Process "W:<command>: ..." -- info for the wilderness */
	else if (buf[0] == 'W')
	{
		return parse_line_wilderness(buf, xmin, xmax, y, x);
	}

	/* Process "P:<y>:<x>" -- player position */
	else if (buf[0] == 'P')
	{
		if (init_flags & INIT_CREATE_DUNGEON)
		{
			if (tokenize(buf + 2, 2, zz, 0) == 2)
			{
				int panels_x, panels_y;

				/* Hack - Set the dungeon size */
				panels_y = (*y / SCREEN_HGT);
				if (*y % SCREEN_HGT) panels_y++;
				cur_hgt = panels_y * SCREEN_HGT;

				panels_x = (*x / SCREEN_WID);
				if (*x % SCREEN_WID) panels_x++;
				cur_wid = panels_x * SCREEN_WID;

				/* Assume illegal panel */
				panel_row_min = cur_hgt;
				panel_col_min = cur_wid;

				/* Place player in a quest level */
				if (p_ptr->inside_quest)
				{
					int y, x;

					/* Delete the monster (if any) */
					delete_monster(py, px);

					y = atoi(zz[0]);
					x = atoi(zz[1]);

					py = y;
					px = x;
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
		if (tokenize(buf+2, 2, zz, 0) == 2)
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

			/* Maximum d_idx */
			else if (zz[0][0] == 'D')
			{
				max_d_idx = atoi(zz[1]); 
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

			/* Process 'P' for "Maximum p_info[] index" */
			else if (zz[0][0] == 'P')
			{
				max_p_idx = atoi(zz[1]);
			}

			/* Process 'C' for "Maximum c_info[] index" */
			else if (zz[0][0] == 'C')
			{
				max_c_idx = atoi(zz[1]);
			}

			/* Process 'H' for "Maximum h_info[] index" */
			else if (zz[0][0] == 'H')
			{
				max_h_idx = atoi(zz[1]);
			}

			return (0);
		}
	}


	/* Failure */
	return (1);
}


static char tmp[8];
static cptr variant = "TOband2";


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
		while (isprint(*s) && !my_strchr(" []", *s)) ++s;

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

			/* Element */
			else if (streq(b+1, "ELEM"))
			{
#ifdef JP
				v = E_elem_names[p_ptr->pelem];
#else
				v = elem_names[p_ptr->pelem];
#endif
			}

			/* Race */
			else if (streq(b+1, "RACE"))
			{
#ifdef JP
				v = p_name + rp_ptr->E_name;
#else
				v = p_name + rp_ptr->name;
#endif
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
#ifdef JP
				v = c_name + cp_ptr->E_name;
#else
				v = c_name + cp_ptr->name;
#endif
			}

			/* Player name */
			else if (streq(b+1, "PLAYER"))
			{
				static char tmp_player_name[32];
				char *pn, *tpn;
				for (pn = player_name, tpn = tmp_player_name; *pn; pn++, tpn++)
				{
#ifdef JP
					if (iskanji(*pn))
					{
						*(tpn++) = *(pn++);
						*tpn = *pn;
						continue;
					}
#endif
					*tpn = my_strchr(" []", *pn) ? '_' : *pn;
				}
				*tpn = '\0';
				v = tmp_player_name;
			}

			/* Sex */
			else if (streq(b+1, "SEX"))
			{
#ifdef JP
					v = sp_ptr->E_title;
#else
					v = sp_ptr->title;
#endif
			}

			/* Town */
			else if (streq(b+1, "TOWN"))
			{
				sprintf(tmp, "%d", p_ptr->town_num);
				v = tmp;
			}

			/* Racial level */
			else if (streq(b+1, "LEVEL"))
			{
				sprintf(tmp, "%ld", p_ptr->lev);
				v = tmp;
			}

			/* Class level */
			else if (!strncmp(b+1, "CLEVEL", 6))
			{
				char *pp = b + 1 + 6;
				char p1 = '(', p2 = ')';

				/* Current class level */
				if (!*pp)
				{
					sprintf(tmp, "%ld", p_ptr->cexp_info[p_ptr->pclass].clev);
					v = tmp;
				}

				/* Specified class level */
				else if (*pp == p1)
				{
					char class_name[40];
					char *rp = my_strchr(pp + 2, p2);

					/* Default */
					v = "0";

					pp++;
					if (rp && !*(rp + 1) && (((int)(rp - pp) + 1) < (sizeof class_name)))
					{
						int i;

						strcpy(class_name, pp);
						class_name[(int)(rp - pp)] = '\0'; /* Terminate right parentheses */

						for (i = 0; i < max_c_idx; i++)
						{
#ifdef JP
							if (streq(c_name + class_info[i].E_name, class_name))
#else
							if (streq(c_name + class_info[i].name, class_name))
#endif
							{
								sprintf(tmp, "%ld", p_ptr->cexp_info[i].clev);
								v = tmp;
								break;
							}
						}
					}
				}
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

			/* Random */
			else if (prefix(b+1, "RANDOM"))
			{
				/* "RANDOM" uses a special parameter to determine the number of the quest */
				sprintf(tmp, "%d", (int)(seed_town%atoi(b+7)));
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
				if (astral_mode)
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
	path_build(buf, sizeof(buf), ANGBAND_DIR_EDIT, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
	if (!fp) return (-1);


	/* Process the file */
	while (0 == my_fgets(fp, buf, sizeof(buf)))
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

		/* Process the line */
		err = process_dungeon_file_aux(buf, ymin, xmin, ymax, xmax, &y, &x);

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
#ifdef JP
		msg_format("'%s'を解析中。", buf);
#else
		msg_format("Parsing '%s'.", buf);
#endif

		msg_print(NULL);
	}


	/* Close the file */
	my_fclose(fp);

	/* Result */
	return (err);
}



/*
 * Grab one flag in a player race from a textual string
 */
static errr grab_one_race_flag(player_race *pr_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, p_info_flags[i]))
		{
			pr_ptr->r_flags |= (1L << i);
			return (0);
		}
	}

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(pr_ptr->flags, i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知の種族フラグ '%s'.", what);
#else
	msg_format("Unknown player race flag '%s'.", what);
#endif

	/* Error */
	return (1);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_race_runeweapon_flag(player_race *pr_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(pr_ptr->z_flags, i);
			return (0);
		}
	}

#if 0
	/* Check gen_flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_gen_flags[i]))
		{
			pr_ptr->z_gen_flags |= (1L << i);
			return (0);
		}
	}
#endif

	/* Oops */
#ifdef JP
	msg_format("未知のルーンウェポン・フラグ '%s'。", what);
#else
	msg_format("Unknown runeweapon flag '%s'.", what);
#endif


	/* Error */
	return (1);
}


/*
 * Initialize the "race_info" array, by parsing an ascii "template" file
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
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i < error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		pr_ptr = &race_info[i];

#ifdef JP
		/* Store the name */
		if (!add_name(&pr_ptr->name, head, s)) return (7);
#endif
	}

#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名 */
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&pr_ptr->E_name, head, s)) return (7);
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&pr_ptr->name, head, s)) return (7);
	}
#endif

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&pr_ptr->text, head, s, TRUE)) return (7);
	}

	/* Process 'S' for "Stats" (one line only) */
	else if (buf[0] == 'S')
	{
		int adj;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (2);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = my_strchr(s, ':');

			/* Verify that colon */
			if (!s) return (1);

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
		int dis, dev, sav, stl, srh, fos, spd, thn, thb;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (2);

		/* Scan for the values */
		if (9 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &spd, &thn, &thb)) return (1);

		/* Save the values */
		pr_ptr->r_dis = dis;
		pr_ptr->r_dev = dev;
		pr_ptr->r_sav = sav;
		pr_ptr->r_stl = stl;
		pr_ptr->r_srh = srh;
		pr_ptr->r_fos = fos;
		pr_ptr->r_spd = spd;
		pr_ptr->r_thn = thn;
		pr_ptr->r_thb = thb;
	}

	/* Process 'X' for "Extra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		int spd;

		/* There better be a current pc_ptr */
		if (!pr_ptr) return (2);

		/* Scan for the values */
		if (1 != sscanf(buf+2, "%d",
			            &spd)) return (1);

		/* Save the values */
		pr_ptr->rx_spd = spd;
	}

	/* Process 'I' for "Extra Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int mhp, msp, exp, b_age, m_age, hist, infra;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (2);

		/* Scan for the values */
		if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
			            &mhp, &msp, &exp, &b_age, &m_age, &hist, &infra)) return (1);

		/* Save the values */
		pr_ptr->r_mhp = mhp;
		pr_ptr->r_msp = msp;
		pr_ptr->r_exp = exp;
		pr_ptr->b_age = b_age;
		pr_ptr->m_age = m_age;
		pr_ptr->hist = hist;
		pr_ptr->infra = infra;
	}

	/* Hack -- Process 'H' for "Height" */
	else if (buf[0] == 'H')
	{
		int m_b_ht, m_m_ht, f_b_ht, f_m_ht;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (2);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &m_b_ht, &m_m_ht, &f_b_ht, &f_m_ht)) return (1);

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
		if (!pr_ptr) return (2);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &m_b_wt, &m_m_wt, &f_b_wt, &f_m_wt)) return (1);

		pr_ptr->m_b_wt = m_b_wt;
		pr_ptr->m_m_wt = m_m_wt;
		pr_ptr->f_b_wt = f_b_wt;
		pr_ptr->f_m_wt = f_m_wt;
	}

	/* Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current pc_ptr */
		if (!pr_ptr) return (2);

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
			if (0 != grab_one_race_flag(pr_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'Z' for runeweapon flags */
	else if (buf[0] == 'Z')
	{
		/* There better be a current pc_ptr */
		if (!pr_ptr) return (2);

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
			if (0 != grab_one_race_runeweapon_flag(pr_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Oops */
	else return (6);

	/* Success */
	return (0);
}


/*
 * Grab one flag in a player class from a textual string
 */
static errr grab_one_realm_choices_flag(player_class *pc_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, c_info_realms[i]))
		{
			pc_ptr->realm_choices |= (1L << i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のクラスフラグ '%s'.", what);
#else
	msg_format("Unknown player class flag '%s'.", what);
#endif

	/* Error */
	return (1);
}


/*
 * Grab one flag in a player class from a textual string
 */
static errr grab_one_class_flag(player_class *pc_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, c_info_flags[i]))
		{
			pc_ptr->c_flags |= (1L << i);
			return (0);
		}
	}

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(pc_ptr->flags, i);
			return (0);
		}
	}

	/* Oops */
#ifdef JP
	msg_format("未知のクラスフラグ '%s'.", what);
#else
	msg_format("Unknown player class flag '%s'.", what);
#endif

	/* Error */
	return (1);
}


/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_class_runeweapon_flag(player_class *pc_ptr, cptr what)
{
	int i;

	/* Check flags */
	for (i = 0; i < TR_FLAG_MAX; i++)
	{
		if (streq(what, k_info_flags[i]))
		{
			add_flag(pc_ptr->z_flags, i);
			return (0);
		}
	}

#if 0
	/* Check gen_flags */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_gen_flags[i]))
		{
			pc_ptr->z_gen_flags |= (1L << i);
			return (0);
		}
	}
#endif

	/* Oops */
#ifdef JP
	msg_format("未知のルーンウェポン・フラグ '%s'。", what);
#else
	msg_format("Unknown runeweapon flag '%s'.", what);
#endif


	/* Error */
	return (1);
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
		s = my_strchr(buf+2, ':');

		/* Verify that colon */
		if (!s) return (1);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

#ifdef JP
		/* Paranoia -- require a name */
		if (!*s) return (1);
#endif

		/* Get the index */
		i = atoi(buf+2);

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		pc_ptr = &class_info[i];

#ifdef JP
		/* Store the name */
		if (!add_name(&pc_ptr->name, head, s)) return (7);
#endif

		/* No titles and equipment yet */
		cur_title = 0;
		cur_equip = 0;
	}

#ifdef JP
	/* 英語名を読むルーチンを追加 */
	/* 'E' から始まる行は英語名 */
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&pc_ptr->E_name, head, s)) return (7);
	}
#else
	else if (buf[0] == 'E')
	{
		/* Acquire the Text */
		s = buf+2;

		/* Store the name */
		if (!add_name(&pc_ptr->name, head, s)) return (7);
	}
#endif

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&pc_ptr->text, head, s, TRUE)) return (7);
	}

	/* Process 'S' for "Need Stats" (one line only) */
	else if (buf[0] == 'S')
	{
		s16b adj;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = my_strchr(s, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			pc_ptr->c_need[j] = adj;

			/* Next... */
			continue;
		}
	}

	/* Process 'G' for "Gain Stats" (one line only) */
	else if (buf[0] == 'G')
	{
		s16b adj;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = my_strchr(s, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			pc_ptr->c_gain[j] = adj;

			/* Next... */
			continue;
		}
	}

	/* Process 'B' for "Reincarnate Bonus Stats" (one line only) */
	else if (buf[0] == 'B')
	{
		s16b adj;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Start the string */
		s = buf+1;

		/* For each stat */
		for (j = 0; j < A_MAX; j++)
		{
			/* Find the colon before the subindex */
			s = my_strchr(s, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the subindex */
			*s++ = '\0';

			/* Get the value */
			adj = atoi(s);

			/* Save the value */
			pc_ptr->c_bonus[j] = adj;

			/* Next... */
			continue;
		}
	}

	/* Process 'C' for "Class Skills" (one line only) */
	else if (buf[0] == 'C')
	{
		int dis, dev, sav, stl, srh, fos, spd, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Scan for the values */
		if (9 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &spd, &thn, &thb)) return (1);

		/* Save the values */
		pc_ptr->c_dis = dis;
		pc_ptr->c_dev = dev;
		pc_ptr->c_sav = sav;
		pc_ptr->c_stl = stl;
		pc_ptr->c_srh = srh;
		pc_ptr->c_fos = fos;
		pc_ptr->c_spd = spd;
		pc_ptr->c_thn = thn;
		pc_ptr->c_thb = thb;
	}

	/* Process 'X' for "Extra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		int dis, dev, sav, stl, srh, fos, spd, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Scan for the values */
		if (9 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &spd, &thn, &thb)) return (1);

		/* Save the values */
		pc_ptr->x_dis = dis;
		pc_ptr->x_dev = dev;
		pc_ptr->x_sav = sav;
		pc_ptr->x_stl = stl;
		pc_ptr->x_srh = srh;
		pc_ptr->x_fos = fos;
		pc_ptr->x_spd = spd;
		pc_ptr->x_thn = thn;
		pc_ptr->x_thb = thb;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int ac, mhp, msp, exp, w_limit, t_limit;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
			            &ac, &mhp, &msp, &exp, &w_limit, &t_limit))
			return (1);

		/* Save the values */
		pc_ptr->c_to_a = ac;
		pc_ptr->c_mhp = mhp;
		pc_ptr->c_msp = msp;
		pc_ptr->c_exp = exp;
		pc_ptr->w_limit = w_limit;
		pc_ptr->t_limit = t_limit;
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

	/* Process 'R' for "Runeweapon type" (one line only) */
	else if (buf[0] == 'R')
	{
		int tval, sval;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d",
			            &tval, &sval))
			return (1);

		/* Save the values */
		pc_ptr->tval = tval;
		pc_ptr->sval = sval;
	}

	/* Process 'T' for "Titles" */
	else if (buf[0] == 'T')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&pc_ptr->title[cur_title], head, s, TRUE)) return (7);
		
		/* Next title */
		cur_title++;

		/* Limit number of titles */
		if (cur_title > PY_MAX_LEVEL / 5) return (2);
	}

	/* Process 'M' for "Magic Info" (one line only) */
	else if (buf[0] == 'M')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

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
			if (0 != grab_one_realm_choices_flag(pc_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'O' for "Starting Equipment" */
	else if (buf[0] == 'O')
	{
		int tval, sval, min, max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &tval, &sval, &min, &max)) return (1);

		if ((min < 0) || (max < 0) || (min > 99) || (max > 99))
			return (1);

		/* Save the values */
		e_ptr->tval = tval;
		e_ptr->sval = sval;
		e_ptr->min = min;
		e_ptr->max = max;

		/* Next item */
		cur_equip++;

		/* Limit number of starting items */
		if (cur_equip > MAX_START_ITEMS)
			return (1);
	}

	/* Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

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
			if (0 != grab_one_class_flag(pc_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'Z' for runeweapon flags */
	else if (buf[0] == 'Z')
	{
		/* There better be a current pc_ptr */
		if (!pc_ptr) return (2);

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
			if (0 != grab_one_class_runeweapon_flag(pc_ptr, s)) return (5);

			/* Start the next entry */
			s = t;
		}
	}

	/* Oops */
	else return (6);

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
		int prv, nxt, prc, soc;

		/* Hack - get the index */
		i = error_idx + 1;

		/* Verify information */
		if (i <= error_idx) return (4);

		/* Verify information */
		if (i >= head->info_num) return (2);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		h_ptr = &h_info[i];

		/* Scan for the values */
		if (4 != sscanf(buf, "N:%d:%d:%d:%d",
			            &prv, &nxt, &prc, &soc)) return (1);

		/* Save the values */
		h_ptr->chart = prv;
		h_ptr->next = nxt;
		h_ptr->roll = prc;
		h_ptr->bonus = soc;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
#ifdef JP
		if (buf[2] == '$')
			return (0);
		/* Acquire the text */
		s = buf+2;
#else
		if (buf[2] != '$')
			return (0);
		/* Acquire the text */
		s = buf+3;
#endif

		/* Store the text */
		if (!add_text(&h_ptr->text, head, s, TRUE)) return (7);
	}

	/* Oops */
	else return (6);


	/* Success */
	return (0);
}


