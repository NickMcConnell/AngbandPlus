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
	"CLAW",
	"BITE",
	"STING",
	"KISS",
	"BUTT",
	"CRUSH",
	"ENGULF",
	"CRAWL",
	"DROOL",
	"SPIT",
	"GAZE",
	"WAIL",
	"SPORE",
	"BEG",
	"INSULT",
	"MOAN",
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
	"EXP_1",
	"EXP_2",
	"EXP_3",
	"EXP_4",
	"DISEASE",
	"RUST",
	"ROT",
	NULL
};


/*
 * Monster race flags
 */
static cptr r_info_flags1[32] =
{
	"UNIQUE",
	"XXX1",
	"MALE",
	"FEMALE",
	"CHAR_CLEAR",
	"ATTR_MIMIC",
	"ATTR_CLEAR",
	"ATTR_MULTI",
	"XXX2",
	"FORCE_MAXHP",
	"FORCE_SLEEP",
	"COMPANION",
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
	"DROP_MIMIC",
	"DROP_CHOSEN"
};

/*
 * Monster race flags
 */
static cptr r_info_flags2[32] =
{
	"STUPID",
	"SMART",
	"NEVER_FAIL",
	"SEE_INVIS",
	"HAS_LITE",
	"INVISIBLE",
	"COLD_BLOOD",
	"EMPTY_MIND",
	"WEIRD_MIND",
	"MULTIPLY",
	"REGENERATE",
	"POWERFUL",
	"OPEN_DOOR",
	"BASH_DOOR",
	"PASS_WALL",
	"KILL_WALL",
	"MOVE_BODY",
	"KILL_BODY",
	"TAKE_ITEM",
	"KILL_ITEM",
	"XXX1",
	"XXX2",
	"XXX3",
	"PERSON",
	"HUMANOID",
	"DRAGON",
	"DEMON",
	"UNDEAD",
	"EVIL",
	"ANIMAL",
	"PLANT",
	"CHAOTIC"
};

/*
 * Monster race flags
 */
static cptr r_info_flags3[32] =
{
	"HURT_LITE",
	"HURT_ROCK",
	"HURT_DARK",
	"XXX1",
	"NO_BLIND",
	"NO_FEAR",
	"NO_STUN",
	"NO_SLEEP",
	"NO_CUT",
	"NO_CALM",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_NETH",
	"RES_LITE",
	"RES_DARK",
	"RES_CONF",
	"RES_SOUN",
	"RES_CHAO",
	"RES_DISE",
	"RES_NEXU",
	"RES_TIME",
	"RES_INER",
	"RES_GRAV",
	"RES_SHAR",
	"RES_PLAS",
	"RES_FORCE",
	"RES_MANA",
	"RES_WATER",
	"RES_DISEASE"
};

/*
 * Monster race flags
 */
static cptr r_info_flags4[32] =
{
	"SHRIEK",
	"XXX1",
	"XXX2",
	"XXX3",
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
	"BR_FORCE",
	"BR_MANA",
	"BR_WATER",
	"BR_DISEASE",
	"XXX4",
	"XXX5"
};

/*
 * Monster race flags
 */
static cptr r_info_flags5[32] =
{
	"BA_ACID",
	"BA_ELEC",
	"BA_FIRE",
	"BA_COLD",
	"BA_POIS",
	"BA_NETH",
	"BA_WATER",
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
	"XXX1",
	"BO_NETH",
	"BO_WATER",
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
static cptr r_info_flags6[32] =
{
	"HASTE",
	"XXX1",
	"HEAL",
	"XXX2",
	"BLINK",
	"TPORT",
	"BLINK_TO",
	"XXX3",
	"TELE_TO",
	"TELE_AWAY",
	"TELE_LEVEL",
	"XXX45",
	"DARKNESS",
	"TRAPS",
	"FORGET",
	"XXX5",
	"S_KIN",
	"S_HI_DEMON",
	"S_MONSTER",
	"S_MONSTERS",
	"S_ANIMALS",
	"S_SPIDER",
	"S_HOUND",
	"S_HYDRA",
	"S_HORROR",
	"S_DEMON",
	"S_UNDEAD",
	"S_DRAGON",
	"S_HI_UNDEAD",
	"S_HI_DRAGON",
	"S_WRAITH",
	"S_UNIQUE"
};

/*
 * Class flags
 */
static cptr c_info_flags[32] =
{
	"EXTRA_SHOT",
	"BRAVERY_30",
	"BLESS_WEAPON",	
	"NO_GLOVE",	
	"POWER",		
	"EXTRA_MANA",
	"ZERO_FAIL",
	"BEAM",	
	"MYSTIC_CAST",
	"EXTRA_SPELL",	
	"PSEUDO_ID_HEAVY",	
	"CHOOSE_SPELLS",			
	"MUSIC",			
	"LORE",			
	"BETTER_CRITICAL",			
	"BETTER_SHOT",			
	"INFLUENCE",
	"XXX1",			
	"XXX2",			
	"XXX3",			
	"XXX4",			
	"XXX5",			
	"XXX6",		
	"XXX7",	
	"XXX8",	
	"XXX9",	
	"XXX10",	
	"PSEUDO_ID1",
	"PSEUDO_ID2",
	"PSEUDO_ID3",
	"PSEUDO_ID4",
	"PSEUDO_ID5"
};

/*
 * Prefix flags
 */
static cptr px_info_flags[32] =
{
	"SHOP",
	"GOOD",
	"ARTIFACT",	
	"XXX1",			
	"XXX2",			
	"XXX3",			
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
	"XXX29"
};

/*
 * Object flags
 */
static cptr k_info_flags1[32] =
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
	"MANA",
	"HEALTH",
	"MIGHT",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6",
	"XXX7",
	"XXX8",
	"XXX9",
	"XXX10",
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR"
};
	
/*
 * Object flags
 */
static cptr k_info_flags2[32] =
{
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"FREE_ACT",
	"HOLD_LIFE",
	"NO_BLIND",
	"BRAVERY",
	"RES_POIS",
	"RES_DISEASE",
	"RES_LITE",
	"RES_DARK",
	"RES_CONFU",
	"RES_SOUND",
	"RES_SHARD",
	"RES_WATER",
	"RES_NEXUS",
	"RES_NETHR",
	"RES_CHAOS",
	"RES_DISEN",
	"RES_TIME",
	"RES_MANA",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6"
};

/*
 * Object flags
 */
static cptr k_info_flags3[32] =
{
	"SLOW_DIGEST",
	"FEATHER",
	"REGEN",
	"TELEPATHY",
	"SEE_INVIS",
	"INVIS",
	"GLOW",
	"LUCK",
	"LITE1",
	"LITE2",
	"LITE3",
	"LITE4",
	"IGNORE_ELEM",
	"IGNORE_NON_ELEM",
	"IGNORE_DISEN",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"DONT_RANDOMIZE",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"XXX5",
	"DRAIN_ITEM",
	"DISRUPT",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};

/*
 * Object flags
 */
static cptr k_info_flags4[32] =
{

	"SLAY_ANIMAL",
	"SLAY_PLANT",
	"SLAY_EVIL",
	"SLAY_CHAOS",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_HUMANOID",
	"SLAY_PERSON",
	"SLAY_DRAGON",
	"KILL_DRAGON",
	"XXX1",
	"XXX2",
	"BLESSED",
	"XXX3",
	"XXX4",
	"XXX5",
	"WOUNDING",
	"TERROR",
	"XXX6",
	"XXX7",
	"IMPACT",
	"XXX8",
	"XXX9",
	"BRAND_ACID",
	"BRAND_ELEC",
	"BRAND_FIRE",
	"BRAND_COLD",
	"BRAND_POIS",
	"BRAND_LITE",
	"BRAND_DARK",
	"XXX10",
	"XXX11",
};

/*
 * Activation type
 */
static cptr k_info_act[POW_MAX] =
{
	"XXXXXX",
	"HEAL_2D10",
	"HEAL_4D10",
	"HEAL_6D10",
	"HEAL_8D10",
	"HEAL_300",
	"HEAL_500",
	"HEAL_1000",
	"HEAL_2000",
	"HEAL_CURE_6D8",
	"HEAL_CURE_300",
	"HEAL_CURE_1200",
	"LIFE",
	"RESTORE_MANA",
	"RESTORE_MANA_INT",
	"RESTORE_STR",
	"RESTORE_INT",
	"RESTORE_WIS",
	"RESTORE_DEX",
	"RESTORE_CON",
	"RESTORE_CHR",
	"RESTORE_STATS",
	"RESTORE_LEVEL",
	"RESTORE_ALL",
	"GAIN_STR",
	"GAIN_INT",
	"GAIN_WIS",
	"GAIN_DEX",
	"GAIN_CON",
	"GAIN_CHR",
	"GAIN_ALL",
	"GAIN_EXP",
	"CURE_FEAR",
	"CURE_CONFUSION",
	"CURE_DISEASE",
	"CURE_POISON_1",
	"CURE_POISON_2",
	"CURE_POIS_DISE",
	"CURE_FEAR_POIS",
	"CURE_ALL",
	"CURE_BODY",
	"CLEAR_MIND",
	"TELE_10",
	"TELE_MINOR",
	"TELE_MAJOR",
	"TELE_OTHER",
	"TELE_LEVEL",
	"TELE_CONTROL",
	"WORD_RECALL",
	"ALTER_REALITY",
	"ARROW",
	"BOLT_MISSILE_1",
	"BOLT_MISSILE_2",
	"BOLT_ELEC",
	"BOLT_FROST_1",
	"BOLT_FROST_2",
	"BOLT_ACID_1",
	"BOLT_ACID_2",
	"BOLT_FIRE_1",
	"BOLT_FIRE_2",
	"BOLT_SOUND",
	"BOLT_FORCE_1",
	"BOLT_FORCE_2",
	"BOLT_LITE",
	"BOLT_DARK",
	"BOLT_WATER",
	"BOLT_MANA",
	"BEAM_WEAK_LITE",
	"BEAM_NETHER",
	"BALL_POISON_1",
	"BALL_POISON_2",
	"BALL_ACID",
	"BALL_ELEC_1",
	"BALL_ELEC_2",
	"BALL_FIRE_1",
	"BALL_FIRE_2",
	"BALL_FROST_1",
	"BALL_FROST_2",
	"BALL_FROST_3",
	"BALL_SOUND",
	"BALL_MANA",
	"BALL_HOLY",
	"STAR_BEAM_W_LITE",
	"STAR_BALL_ELEC",
	"BANISH",
	"BLIGHT",
	"BURST_ASTRAL",
	"DRAIN_LIFE_1",
	"DRAIN_LIFE_2",
	"DRAIN_LIFE_3",
	"DISPEL_ALL",
	"DISPEL_UNDEAD_1",
	"DISPEL_UNDEAD_2",
	"DISPEL_NON_EVIL",
	"DISPEL_EVIL_3",
	"DISPEL_EVIL_4",
	"DISPEL_EVIL_5",
	"HOLY_1",
	"HOLY_2",
	"GENOCIDE",
	"MASS_GENOCIDE",
	"EARTHQUAKE",
	"DESTRUCTION",
	"LIGHT_AREA",
	"DARK_AREA",
	"DETECT_MONSTERS",
	"DETECT_EVIL",
	"DETECT_INVIS",
	"DETECT_TRAP",
	"DETECT_TREASURE",
	"DETECT_DOOR_STAIR",
	"DETECT_TRAP_DOOR",
	"DETECT_ITEM",
	"DETECT_ENCHANT",
	"DETECT_ALL",
	"ABSORB_HIT",
	"BLESS_1",
	"BLESS_2",
	"BLESS_3",
	"HEROISM",
	"RAGE_1",
	"RAGE_2",
	"RAGE_BLESS_RESIST",
	"SHIELD",
	"INVIS_1",
	"INVIS_2",
	"RESILIENCE",
	"INFRAVISION",
	"SEE_INVIS",
	"PROT_EVIL",
	"HASTE_SELF_1",
	"HASTE_SELF_2",
	"HASTE_SELF_3",
	"DISARM",
	"DEST_TRAP_DOOR_1",
	"DEST_TRAP_DOOR_2",
	"STONE_TO_MUD",
	"CREATE_DOOR",
	"CREATE_STAIR",
	"CREATE_TRAP",
	"ACQUIRE_1",
	"ACQUIRE_2",
	"AGGRAVATE",
	"AGGRAVATE_SAFE",
	"CONFUSE_MONSTER",
	"CONFUSE_ALL",
	"SLEEP_MONSTER",
	"SLEEP_ADJACENT",
	"SLEEP_ALL",
	"SLOW_MONSTER",
	"SLOW_ALL",
	"CALM_MONSTER",
	"CALM_ANIMALS",
	"CALM_NON_EVIL",
	"CALM_NON_CHAOS",
	"CALM_ALL",
	"BLIND_MONSTER",
	"SCARE_MONSTER",
	"SCARE_UNDEAD",
	"SCARE_ALL",
	"CALL_MONSTER",
	"POLY_MONSTER",
	"HEAL_MONSTER",
	"HASTE_MONSTER",
	"CLONE_MONSTER",
	"SATISFY_HUNGER",
	"RECHARGE_1",
	"RECHARGE_2",
	"RECHARGE_3",
	"RECHARGE_4",
	"IDENTIFY",
	"IDENTIFY_PACK",
	"IDENTIFY_FULL",
	"RES_FIRE",
	"RES_COLD",
	"RES_FIRE_COLD",
	"RES_ACID_ELEC",
	"RES_POISON",
	"RES_DISEASE",
	"RES_SOUND",
	"RES_ELEMENTS",
	"RES_GREATER",
	"RESISTANCE",
	"GLYPH_WARDING",
	"REMOVE_CURSE_1",
	"REMOVE_CURSE_2",
	"MAP_1",
	"MAP_2",
	"MAP_3",
	"PROBE",
	"SELF_KNOW",
	"KNOW_ALL",
	"ENCHANT_WEAPON_HIT",
	"ENCHANT_WEAPON_DAM",
	"ENCHANT_WEAPON",
	"ENCHANT_ARMOR_1",
	"ENCHANT_ARMOR_2",
	"BRAND_WEAPON_ELMNT",
	"BRAND_ARROW_ANML",
	"BRAND_ARROW_WOUND",
	"BRAND_ARROW_ELMNT",
	"BRAND_BOLT_FIRE",
	"BRAND_BOLT_LITE",
	"BRAND_SHOT_POIS",
	"BRAND_SHOT_HOLY",
	"BIZZARE",
	"CURSE_EQUIP_1",
	"CURSE_EQUIP_2",
	"SUM_MONSTER",
	"SUM_UNDEAD",
	"SUM_DRAGON",
	"NAUSEA",
	"POISON_SELF",
	"BLIND_SELF",
	"CONFUSE_SELF",
	"SCARE_SELF",
	"SLOW_SELF",
	"PARALYZE",
	"HALLUCINATE",
	"DISEASE",
	"DEFORM",
	"LOSE_STR",
	"LOSE_INT",
	"LOSE_WIS",
	"LOSE_DEX",
	"LOSE_CON",
	"LOSE_CHR",
	"LOSE_EXP",
	"RUINATION",
	"DETONATE",
	"KILL_SELF",
	"DRAGON_BLACK",
	"DRAGON_BLUE",
	"DRAGON_WHITE",
	"DRAGON_RED",
	"DRAGON_GREEN",
	"DRAGON_BRONZE",
	"DRAGON_GOLD",
	"DRAGON_SILVER",
	"DRAGON_MH",
	"DRAGON_SPIRIT",
	"DRAGON_SHADOW",
	"DRAGON_ETHER",
	"DRAGON_CHAOS",
	"DRAGON_TIME",
	"DRAGON_POWER",
	"RISK_HACK",
	"WONDER_HACK"
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
 */
errr parse_z_info(char *buf, header *head)
{
	/* Unused parameter */
	(void)head;

	/* Hack - Verify 'M/S:x:' format */
	if (!buf[2]) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
	if (buf[3] != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);

	if (buf[0] == 'M')
	{
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

		/* Process 'C' for "Maximum c_info[] index" */
		else if (buf[2] == 'C')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->c_max = max;
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

		/* Process 'X' for "Maximum px_info[] index" */
		else if (buf[2] == 'X')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->px_max = max;
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

		/* Process 'U' for "Maximum u_info[] index" */
		else if (buf[2] == 'U')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->u_max = max;
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

		/* Process 'P' for "Maximum p_info[] index" */
		else if (buf[2] == 'P')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->p_max = max;
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

		/* Process 'Q' for "Maximum q_info[] index" */
		else if (buf[2] == 'Q')
		{
			int max;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->q_max = max;
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
	}

	else if (buf[0] == 'S')
	{
		/* Process 'A' for "Minimum normal artifact" index" */
		if (buf[2] == 'A')
		{
			int min_norm;

			/* Scan for the value */
			if (1 != sscanf(buf+4, "%d", &min_norm)) return (PARSE_ERROR_GENERIC);

			/* Save the value */
			z_info->a_min_normal = min_norm;
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
	if (grab_one_flag(&k_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&k_ptr->flags4, k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown object flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one activation from a textual string
 */
static errr grab_one_activation(byte *act, cptr what)
{
	int i;

	/* Scan activations */
	for (i = 1; i < POW_MAX; i++)
	{
		if (streq(what, k_info_act[i]))
		{
			*act = i;
			return (0);
		}
	}

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown activation '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
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
		k_ptr->weight = wgt;
		k_ptr->cost = cost;
	}

	/* Process 'X' for "Even more Info" (one line only) */
	else if (buf[0] == 'X')
	{
		int material, qd, qs, breakage;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%dd%d:%d",
						&material, &qd, &qs, &breakage)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->material = material;
		k_ptr->qd = qd;
		k_ptr->qs = qs;
		k_ptr->breakage = breakage;

	}

	/* Process 'A' for "Allocation" (one line only) */
	else if (buf[0] == 'A')
	{
		int i;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* XXX XXX Simply read each number following a colon */
		for (i = 0, s = buf+1; s && (s[0] == ':') && s[1]; ++i)
		{
			if (i >= MAX_OBJ_ALLOC) return (PARSE_ERROR_TOO_MANY_ARGUMENTS);

			/* Default chance */
			k_ptr->chance[i] = 1;

			/* Store the locale */
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

	/* Process 'U' for "Activation" */
	else if (buf[0] == 'U')
	{
		byte act = 0;

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
				while ((*t == ' ') || (*t == '|')) t++;
			}

			/* Get the activation */
			grab_one_activation(&act, s);

			k_ptr->activation = act;

			/* Start the next entry */
			s = t;
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
		k_ptr->dd = hd1;
		k_ptr->ds = hd2;
		k_ptr->to_h = th;
		k_ptr->to_d = td;
		k_ptr->to_a =  ta;
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
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
	if (grab_one_flag(&a_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&a_ptr->flags4, k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown artifact flag '%s'.", what);

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
		a_ptr = &a_info[i];

		/* Store the name */
		if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
		a_ptr->flags3 |= (TR3_IGNORE_ELEM);
		a_ptr->flags3 |= (TR3_IGNORE_NON_ELEM);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pfix, pval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &tval, &sval, &pfix, &pval)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->tval = tval;
		a_ptr->sval = sval;
		a_ptr->pval = pval;

		a_ptr->prefix_idx = pfix;
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
		int ac, hd1, hd2, th, td, ta;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
			            &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

		a_ptr->ac = ac;
		a_ptr->dd = hd1;
		a_ptr->ds = hd2;
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
	
	/* Process 'U' for "Activation & time" */
	else if (buf[0] == 'U')
	{
		int time, rand;
		byte act = 0;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the colon before the name */
		s = strchr(buf + 2, ':');

		/* Verify that colon */
		if (!s) return (PARSE_ERROR_GENERIC);

		/* Nuke the colon, advance to the name */
		*s++ = '\0';

		/* Paranoia -- require a name */
		if (!*s) return (PARSE_ERROR_GENERIC);

		/* Get the activation */
		grab_one_activation(&act, buf + 2);

		/* Scan for the values */
		if (2 != sscanf(s, "%d:%d",
		                &time, &rand)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->activation = act;
		a_ptr->time = time;
		a_ptr->randtime = rand;
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
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
	if (grab_one_flag(&e_ptr->flags1, k_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags2, k_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags3, k_info_flags3, what) == 0)
		return (0);

	if (grab_one_flag(&e_ptr->flags4, k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown ego-item flag '%s'.", what);

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
		int th, td, ta, pv;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &th, &td, &ta, &pv)) return (PARSE_ERROR_GENERIC);

		e_ptr->max_to_h = th;
		e_ptr->max_to_d = td;
		e_ptr->max_to_a = ta;
		e_ptr->max_pval = pv;
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
static errr grab_one_prefix_flag(item_prefix_type *px_ptr, cptr what)
{
	if (grab_one_flag(&px_ptr->flags, px_info_flags, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown prefix flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the "px_info" array, by parsing an ascii "template" file
 */
errr parse_px_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static item_prefix_type *px_ptr = NULL;

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
		px_ptr = &px_info[i];

		/* Store the name */
		if (!(px_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'W' (one line only) */
	else if (buf[0] == 'W')
	{
		int rar, mat, cst, wgt;

		/* There better be a current r_ptr */
		if (!px_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &rar, &mat, &wgt, &cst)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		px_ptr->rarity = rar;
		px_ptr->material = mat;
		px_ptr->cost = cst;
		px_ptr->weight = wgt;
	}

	/* Process 'P' (one line only) */
	else if (buf[0] == 'P')
	{
		int th, td, dd, ds;

		/* There better be a current r_ptr */
		if (!px_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &th, &td, &dd, &ds)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		px_ptr->to_h = th;
		px_ptr->to_d = td;
		px_ptr->dd = dd;
		px_ptr->ds = ds;
	}

	/* Process 'F' for flags */
	else if (buf[0] == 'F')
	{
		/* There better be a current pc_ptr */
		if (!px_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
			if (0 != grab_one_prefix_flag(px_ptr, s))
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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_race_basic_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags1, r_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags2, r_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags3, r_info_flags3, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one (spell) flag in a monster_race from a textual string
 */
static errr grab_one_race_spell_flag(monster_race *r_ptr, cptr what)
{
	if (grab_one_flag(&r_ptr->flags4, r_info_flags4, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags5, r_info_flags5, what) == 0)
		return (0);

	if (grab_one_flag(&r_ptr->flags6, r_info_flags6, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown monster flag '%s'.", what);

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
 		if (!add_text(&r_ptr->text, head, s))
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
		int spd, hp1, hp2, aaf, ac, slp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, pad;
		long exp;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
			            &lev, &rar, &pad, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->level = lev;
		r_ptr->rarity = rar;
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
			if (0 != grab_one_race_basic_flag(r_ptr, s)) return (PARSE_ERROR_INVALID_FLAG);

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
				r_ptr->freq_spell = 100 / i;

				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_race_spell_flag(r_ptr, s))
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
 * Grab one (basic) flag in a monster_unique from a textual string
 */
static errr grab_one_unique_basic_flag(monster_unique *u_ptr, cptr what)
{
	if (grab_one_flag(&u_ptr->flags1, r_info_flags1, what) == 0)
		return (0);

	if (grab_one_flag(&u_ptr->flags2, r_info_flags2, what) == 0)
		return (0);

	if (grab_one_flag(&u_ptr->flags3, r_info_flags3, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Grab one (spell) flag in a monster_unique from a textual string
 */
static errr grab_one_unique_spell_flag(monster_unique *u_ptr, cptr what)
{
	if (grab_one_flag(&u_ptr->flags4, r_info_flags4, what) == 0)
		return (0);

	if (grab_one_flag(&u_ptr->flags5, r_info_flags5, what) == 0)
		return (0);

	if (grab_one_flag(&u_ptr->flags6, r_info_flags6, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown monster flag '%s'.", what);

	/* Failure */
	return (PARSE_ERROR_GENERIC);
}

/*
 * Initialize the "u_info" array, by parsing an ascii "template" file
 */
errr parse_u_info(char *buf, header *head)
{
	int i;

	char *s, *t;

	/* Current entry */
	static monster_unique *u_ptr = NULL;

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
		u_ptr = &u_info[i];

		/* Store the name */
		if (!(u_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'R' for "Monster Race" (one line only) */
	else if (buf[0] == 'R')
	{
		int r_idx, tmp;
		byte clr;

		/* There better be a current u_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%c", &r_idx, &clr)) return (PARSE_ERROR_GENERIC);

		/* Extract the attr */
		tmp = color_char_to_attr(clr);

		/* Paranoia */
		if (tmp < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		u_ptr->r_idx = r_idx;
		u_ptr->d_attr = tmp;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current u_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Get the text */
		s = buf+2;

		/* Store the text */
 		if (!add_text(&u_ptr->text, head, s))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int spd, hp1, hp2, aaf, ac, slp;

		/* There better be a current u_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the other values */
		if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
		                &spd, &hp1, &hp2, &aaf, &ac, &slp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		u_ptr->speed = spd;
		u_ptr->hdice = hp1;
		u_ptr->hside = hp2;
		u_ptr->aaf = aaf;
		u_ptr->ac = ac;
		u_ptr->sleep = slp;
	}

	/* Process 'W' for "More Info" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, rar, pad;
		long exp;

		/* There better be a current u_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
		                &lev, &rar, &pad, &exp)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		u_ptr->level = lev;
		u_ptr->rarity = rar;
		u_ptr->mexp = exp;
	}

	/* Process 'B' for "Blows" (up to four lines) */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current u_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < 4; i++) if (!u_ptr->blow[i].method) break;

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
		u_ptr->blow[i].method = n1;

		/* Save the effect */
		u_ptr->blow[i].effect = n2;

		/* Extract the damage dice and sides */
		u_ptr->blow[i].d_dice = atoi(s);
		u_ptr->blow[i].d_side = atoi(t);
	}

	/* Process 'F' for "Basic Flags" (multiple lines) */
	else if (buf[0] == 'F')
	{
		/* There better be a current r_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
			if (0 != grab_one_unique_basic_flag(u_ptr, s))
				return (PARSE_ERROR_INVALID_FLAG);

			/* Start the next entry */
			s = t;
		}
	}

	/* Process 'S' for "Spell Flags" (multiple lines) */
	else if (buf[0] == 'S')
	{
		/* There better be a current r_ptr */
		if (!u_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

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
				u_ptr->freq_spell = 100 / i;

				/* Start at next entry */
				s = t;

				/* Continue */
				continue;
			}

			/* Parse this entry */
			if (0 != grab_one_unique_spell_flag(u_ptr, s))
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

	if (grab_one_flag(&pr_ptr->flags4, k_info_flags4, what) == 0)
		return (0);

	/* Oops */
	message_format(MSG_GENERIC, 0, "Unknown player flag '%s'.", what);

	/* Error */
	return (PARSE_ERROR_GENERIC);
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
	
	/* Process 'R' for "Racial Skills" (one line only) */
	else if (buf[0] == 'R')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb, tht, dig, alc;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (SK_MAX != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &dis, &dev, &sav, &stl, &srh,
						&fos, &thn, &thb, &tht, &dig,
						&alc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pr_ptr->r_skill[SK_DIS] = dis;
		pr_ptr->r_skill[SK_DEV] = dev;
		pr_ptr->r_skill[SK_SAV] = sav;
		pr_ptr->r_skill[SK_STL] = stl;
		pr_ptr->r_skill[SK_SRH] = srh;
		pr_ptr->r_skill[SK_FOS] = fos;
		pr_ptr->r_skill[SK_THN] = thn;
		pr_ptr->r_skill[SK_THB] = thb;
		pr_ptr->r_skill[SK_THT] = tht;
		pr_ptr->r_skill[SK_DIG] = dig;
		pr_ptr->r_skill[SK_ALC] = alc;
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
		int hist, prfx, b_age, m_age;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
		                &hist, &prfx, &b_age, &m_age)) return (PARSE_ERROR_GENERIC);

		pr_ptr->hist = hist;
		pr_ptr->prefix = prfx;
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

	/* Hack -- Process 'U' for special abilties */
	else if (buf[0] == 'U')
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
			if (streq(s,"ANGEL")) pr_ptr->special = RACE_SPECIAL_ANGEL;
			else if (streq(s,"DEMON")) pr_ptr->special = RACE_SPECIAL_DEMON;
			else return (PARSE_ERROR_INVALID_FLAG);

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
	message_format(MSG_GENERIC, 0, "Unknown player class flag '%s'.", what);

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

	/* Process 'C' for "Class Skills" (one line only) */
	else if (buf[0] == 'C')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb, tht, dig, alc;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (SK_MAX != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &dis, &dev, &sav, &stl, &srh,
						&fos, &thn, &thb, &tht, &dig,
						&alc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_skill[SK_DIS] = dis;
		pc_ptr->c_skill[SK_DEV] = dev;
		pc_ptr->c_skill[SK_SAV] = sav;
		pc_ptr->c_skill[SK_STL] = stl;
		pc_ptr->c_skill[SK_SRH] = srh;
		pc_ptr->c_skill[SK_FOS] = fos;
		pc_ptr->c_skill[SK_THN] = thn;
		pc_ptr->c_skill[SK_THB] = thb;
		pc_ptr->c_skill[SK_THT] = tht;
		pc_ptr->c_skill[SK_DIG] = dig;
		pc_ptr->c_skill[SK_ALC] = alc;
	}

	/* Process 'X' for "Xtra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb, tht, dig, alc;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);
		
		/* Scan for the values */
		if (SK_MAX != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
		                &dis, &dev, &sav, &stl, &srh,
						&fos, &thn, &thb, &tht, &dig,
						&alc)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->x_skill[SK_DIS] = dis;
		pc_ptr->x_skill[SK_DEV] = dev;
		pc_ptr->x_skill[SK_SAV] = sav;
		pc_ptr->x_skill[SK_STL] = stl;
		pc_ptr->x_skill[SK_SRH] = srh;
		pc_ptr->x_skill[SK_FOS] = fos;
		pc_ptr->x_skill[SK_THN] = thn;
		pc_ptr->x_skill[SK_THB] = thb;
		pc_ptr->x_skill[SK_THT] = tht;
		pc_ptr->x_skill[SK_DIG] = dig;
		pc_ptr->x_skill[SK_ALC] = alc;
	}

	/* Process 'I' for "info" - 1 line only */
	else if (buf[0] == 'I')
	{
		int mhp, exp, maxattack, minweight, mult, sstat1, sstat2, dist;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
		                &mhp, &exp, &maxattack, &minweight, &mult, &sstat1, &sstat2, &dist))
						return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_mhp = mhp;
		pc_ptr->c_exp = exp;
		pc_ptr->max_attacks = maxattack;
		pc_ptr->min_weight = minweight;
		pc_ptr->att_multiply = mult;
		pc_ptr->spell_stat1 = sstat1;
		pc_ptr->spell_stat2 = sstat2;
		pc_ptr->spell_weight = dist;
	}

	/* Hack -- Process 'B' for "Spellbooks" */
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

	/* Process 'E' for "Starting Equipment" */
	else if (buf[0] == 'E')
	{
		int tval, sval, ego, min, max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
		                &tval, &sval, &ego, &min, &max)) return (PARSE_ERROR_GENERIC);

		e_ptr->tval = tval;
		e_ptr->sval = sval;
		e_ptr->ego = ego;
		e_ptr->min = min;
		e_ptr->max = max;

		/* Next item */
		cur_equip++;

		/* Limit number of starting items */
		if (cur_equip > MAX_START_ITEMS)
			return (PARSE_ERROR_GENERIC);
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
		
	else /* Process 'I' for "Info" (one line only) */
	if (buf[0] == 'I')
	{
		int race, gld, max, min, hgl, tol;

		/* There better be a current ot_ptr */
		if (!ot_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
		                &race, &gld, &max, &min, &hgl, &tol)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		ot_ptr->owner_race = race;
		ot_ptr->max_cost = gld;

#ifdef ALLOW_HAGGLE

		ot_ptr->max_inflate = max;
		ot_ptr->min_inflate = min;
		ot_ptr->haggle_per = hgl;
		ot_ptr->insult_max = tol;

#else /* ALLOW_HAGGLE */

		ot_ptr->inflate = min;

#endif /* ALLOW_HAGGLE */

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
		q_ptr = &q_info[i];

		/* Store the name */
		if (!(q_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'W' for "Where/What" (one line only) */
	else if (buf[0] == 'W')
	{
		int lev, idx, max;

		/* There better be a current q_ptr */
		if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
		                &lev, &idx, &max)) return (PARSE_ERROR_GENERIC);

		if (quest_check(lev) == QUEST_FIXED) return (PARSE_ERROR_TOO_MANY_QUESTS);

		/* Save the values */
		q_ptr->base_level = q_ptr->active_level = lev;
		q_ptr->r_idx = idx;
		q_ptr->max_num = max;
		q_ptr->type = QUEST_FIXED;
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
