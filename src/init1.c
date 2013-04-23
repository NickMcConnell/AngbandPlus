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
#define MAX_I (int)(z_info->fake_info_size/head->info_len)


/*** Helper arrays for parsing ascii template files ***/


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



typedef struct flag_name flag_name;

struct flag_name
{
	cptr name; /* The name of the flag in the text file. */
	int set; /* The set into which the flag is to be sent. */
	u32b flag; /* The flag being set. */
};

/*
 * Object flags
 *
 * Flags which are unused in the source code are simply commented out.
 * Flags which are used, but are not intended to be set via text files are
 * commented out with an explanatory comment afterwards.
 * Flags which are used in the source code, but are assigned to no monsters
 * at present are followed by an UNUSED comment.
 */
static flag_name info_flags[] =
{
	/* Object flags */

	{"STR", TR1, TR1_STR},
	{"INT", TR1, TR1_INT},
	{"WIS", TR1, TR1_WIS},
	{"DEX", TR1, TR1_DEX},
	{"CON", TR1, TR1_CON},
	{"CHR", TR1, TR1_CHR},
/* {"XXX1", TR1, TR1_XXX1}, */
/* {"XXX2", TR1, TR1_XXX2}, */
	{"STEALTH", TR1, TR1_STEALTH},
	{"SEARCH", TR1, TR1_SEARCH},
	{"INFRA", TR1, TR1_INFRA},
	{"TUNNEL", TR1, TR1_TUNNEL},
	{"SPEED", TR1, TR1_SPEED},
	{"BLOWS", TR1, TR1_BLOWS},
	{"CHAOTIC", TR1, TR1_CHAOTIC},
	{"VAMPIRIC", TR1, TR1_VAMPIRIC},
	{"SLAY_ANIMAL", TR1, TR1_SLAY_ANIMAL},
	{"SLAY_EVIL", TR1, TR1_SLAY_EVIL},
	{"SLAY_UNDEAD", TR1, TR1_SLAY_UNDEAD},
	{"SLAY_DEMON", TR1, TR1_SLAY_DEMON},
	{"SLAY_ORC", TR1, TR1_SLAY_ORC},
	{"SLAY_TROLL", TR1, TR1_SLAY_TROLL},
	{"SLAY_GIANT", TR1, TR1_SLAY_GIANT},
	{"SLAY_DRAGON", TR1, TR1_SLAY_DRAGON},
	{"KILL_DRAGON", TR1, TR1_KILL_DRAGON},
	{"X15_DRAGON", TR1, TR1_X15_DRAGON},
	{"VORPAL", TR1, TR1_VORPAL},
	{"IMPACT", TR1, TR1_IMPACT},
	{"BRAND_POIS", TR1, TR1_BRAND_POIS},
	{"BRAND_ACID", TR1, TR1_BRAND_ACID},
	{"BRAND_ELEC", TR1, TR1_BRAND_ELEC},
	{"BRAND_FIRE", TR1, TR1_BRAND_FIRE},
	{"BRAND_COLD", TR1, TR1_BRAND_COLD},
	{"SUST_STR", TR2, TR2_SUST_STR},
	{"SUST_INT", TR2, TR2_SUST_INT},
	{"SUST_WIS", TR2, TR2_SUST_WIS},
	{"SUST_DEX", TR2, TR2_SUST_DEX},
	{"SUST_CON", TR2, TR2_SUST_CON},
	{"SUST_CHR", TR2, TR2_SUST_CHR},
	{"RAND_RESIST", TR2, TR2_RAND_RESIST},
	{"RAND_POWER", TR2, TR2_RAND_POWER},
	{"IM_ACID", TR2, TR2_IM_ACID},
	{"IM_ELEC", TR2, TR2_IM_ELEC},
	{"IM_FIRE", TR2, TR2_IM_FIRE},
	{"IM_COLD", TR2, TR2_IM_COLD},
	{"RAND_EXTRA", TR2, TR2_RAND_EXTRA},
	{"REFLECT", TR2, TR2_REFLECT},
	{"FREE_ACT", TR2, TR2_FREE_ACT},
	{"HOLD_LIFE", TR2, TR2_HOLD_LIFE},
	{"RES_ACID", TR2, TR2_RES_ACID},
	{"RES_ELEC", TR2, TR2_RES_ELEC},
	{"RES_FIRE", TR2, TR2_RES_FIRE},
	{"RES_COLD", TR2, TR2_RES_COLD},
	{"RES_POIS", TR2, TR2_RES_POIS},
	{"RES_FEAR", TR2, TR2_RES_FEAR},
	{"RES_LITE", TR2, TR2_RES_LITE},
	{"RES_DARK", TR2, TR2_RES_DARK},
	{"RES_BLIND", TR2, TR2_RES_BLIND},
	{"RES_CONF", TR2, TR2_RES_CONF},
	{"RES_SOUND", TR2, TR2_RES_SOUND},
	{"RES_SHARDS", TR2, TR2_RES_SHARDS},
	{"RES_NETHER", TR2, TR2_RES_NETHER},
	{"RES_NEXUS", TR2, TR2_RES_NEXUS},
	{"RES_CHAOS", TR2, TR2_RES_CHAOS},
	{"RES_DISEN", TR2, TR2_RES_DISEN},
	{"SH_FIRE", TR3, TR3_SH_FIRE},
	{"SH_ELEC", TR3, TR3_SH_ELEC},
	{"SHOW_ARMOUR", TR3, TR3_SHOW_ARMOUR},
	{"AUTO_CURSE", TR3, TR3_AUTO_CURSE},
	{"NO_TELE", TR3, TR3_NO_TELE},
	{"NO_MAGIC", TR3, TR3_NO_MAGIC},
	{"WRAITH", TR3, TR3_WRAITH}, /* UNUSED */
	{"TY_CURSE", TR3, TR3_TY_CURSE},
	{"EASY_KNOW", TR3, TR3_EASY_KNOW},
	{"HIDE_TYPE", TR3, TR3_HIDE_TYPE},
	{"SHOW_MODS", TR3, TR3_SHOW_MODS},
	{"GOOD", TR3, TR3_GOOD},
	{"FEATHER", TR3, TR3_FEATHER},
	{"LITE", TR3, TR3_LITE},
	{"SEE_INVIS", TR3, TR3_SEE_INVIS},
	{"TELEPATHY", TR3, TR3_TELEPATHY},
	{"SLOW_DIGEST", TR3, TR3_SLOW_DIGEST},
	{"REGEN", TR3, TR3_REGEN},
	{"XTRA_MIGHT", TR3, TR3_XTRA_MIGHT},
	{"XTRA_SHOTS", TR3, TR3_XTRA_SHOTS},
	{"IGNORE_ACID", TR3, TR3_IGNORE_ACID},
	{"IGNORE_ELEC", TR3, TR3_IGNORE_ELEC},
	{"IGNORE_FIRE", TR3, TR3_IGNORE_FIRE},
	{"IGNORE_COLD", TR3, TR3_IGNORE_COLD},
	{"ACTIVATE", TR3, TR3_ACTIVATE},
	{"DRAIN_EXP", TR3, TR3_DRAIN_EXP},
	{"TELEPORT", TR3, TR3_TELEPORT},
	{"AGGRAVATE", TR3, TR3_AGGRAVATE},
	{"BLESSED", TR3, TR3_BLESSED},
	{"CURSED", TR3, TR3_CURSED},
	{"HEAVY_CURSE", TR3, TR3_HEAVY_CURSE},
	{"PERMA_CURSE", TR3, TR3_PERMA_CURSE},

	/* General monster flags */

	{"UNIQUE", RF1, RF1_UNIQUE},
/* {"GUARDIAN", RF1, RF1_GUARDIAN}, */ /* Set from q_list[]. */
	{"MALE", RF1, RF1_MALE},
	{"FEMALE", RF1, RF1_FEMALE},
	{"CHAR_CLEAR", RF1, RF1_CHAR_CLEAR},
	{"CHAR_MULTI", RF1, RF1_CHAR_MULTI},
	{"ATTR_CLEAR", RF1, RF1_ATTR_CLEAR},
	{"ATTR_MULTI", RF1, RF1_ATTR_MULTI},
/* {"ALWAYS_GUARD", RF1, RF1_ALWAYS_GUARD}, */
	{"FORCE_MAXHP", RF1, RF1_FORCE_MAXHP},
	{"FORCE_SLEEP", RF1, RF1_FORCE_SLEEP},
/* {"FORCE_EXTRA", RF1, RF1_FORCE_EXTRA}, */
/* {"FRIEND", RF1, RF1_FRIEND}, */
	{"FRIENDS", RF1, RF1_FRIENDS},
	{"ESCORT", RF1, RF1_ESCORT},
	{"ESCORTS", RF1, RF1_ESCORTS},
	{"NEVER_BLOW", RF1, RF1_NEVER_BLOW},
	{"NEVER_MOVE", RF1, RF1_NEVER_MOVE},
	{"RAND_25", RF1, RF1_RAND_25},
	{"RAND_50", RF1, RF1_RAND_50},
	{"ONLY_GOLD", RF1, RF1_ONLY_GOLD},
	{"ONLY_ITEM", RF1, RF1_ONLY_ITEM},
	{"DROP_60", RF1, RF1_DROP_60},
	{"DROP_90", RF1, RF1_DROP_90},
	{"DROP_1D2", RF1, RF1_DROP_1D2},
	{"DROP_2D2", RF1, RF1_DROP_2D2},
	{"DROP_3D2", RF1, RF1_DROP_3D2},
	{"DROP_4D2", RF1, RF1_DROP_4D2},
	{"DROP_GOOD", RF1, RF1_DROP_GOOD},
	{"DROP_GREAT", RF1, RF1_DROP_GREAT},
	{"DROP_USEFUL", RF1, RF1_DROP_USEFUL}, /* UNUSED */
/* {"DROP_CHOSEN", RF1, RF1_DROP_CHOSEN}, */
	{"STUPID", RF2, RF2_STUPID},
	{"SMART", RF2, RF2_SMART},
	{"CAN_SPEAK", RF2, RF2_CAN_SPEAK},
	{"REFLECTING", RF2, RF2_REFLECTING},
	{"INVISIBLE", RF2, RF2_INVISIBLE},
	{"COLD_BLOOD", RF2, RF2_COLD_BLOOD},
	{"EMPTY_MIND", RF2, RF2_EMPTY_MIND},
	{"WEIRD_MIND", RF2, RF2_WEIRD_MIND},
	{"MULTIPLY", RF2, RF2_MULTIPLY},
	{"REGENERATE", RF2, RF2_REGENERATE},
	{"SHAPECHANGER", RF2, RF2_SHAPECHANGER},
	{"ATTR_ANY", RF2, RF2_ATTR_ANY},
	{"POWERFUL", RF2, RF2_POWERFUL},
	{"ELDRITCH_HORROR", RF2, RF2_ELDRITCH_HORROR},
	{"AURA_FIRE", RF2, RF2_AURA_FIRE},
	{"AURA_ELEC", RF2, RF2_AURA_ELEC},
	{"OPEN_DOOR", RF2, RF2_OPEN_DOOR},
	{"BASH_DOOR", RF2, RF2_BASH_DOOR},
	{"PASS_WALL", RF2, RF2_PASS_WALL},
	{"KILL_WALL", RF2, RF2_KILL_WALL},
	{"MOVE_BODY", RF2, RF2_MOVE_BODY},
	{"KILL_BODY", RF2, RF2_KILL_BODY},
	{"TAKE_ITEM", RF2, RF2_TAKE_ITEM},
	{"KILL_ITEM", RF2, RF2_KILL_ITEM},
	{"RUN_AWAY", RF2, RF2_RUN_AWAY},
	{"ELEMENTAL", RF2, RF2_ELEMENTAL},
	{"MIMIC", RF2, RF2_MIMIC},
	{"PHANTOM", RF2, RF2_PHANTOM},
	{"CULTIST", RF2, RF2_CULTIST},
	{"SHAMAN", RF2, RF2_SHAMAN},
/* {"BRAIN_6", RF2, RF2_BRAIN_6}, */
/* {"BRAIN_7", RF2, RF2_BRAIN_7}, */
	{"ORC", RF3, RF3_ORC},
	{"TROLL", RF3, RF3_TROLL},
	{"GIANT", RF3, RF3_GIANT},
	{"DRAGON", RF3, RF3_DRAGON},
	{"DEMON", RF3, RF3_DEMON},
	{"UNDEAD", RF3, RF3_UNDEAD},
	{"EVIL", RF3, RF3_EVIL},
	{"ANIMAL", RF3, RF3_ANIMAL},
	{"GREAT_OLD_ONE", RF3, RF3_GREAT_OLD_ONE},
	{"GOOD", RF3, RF3_GOOD},
	{"PLAYER_GHOST", RF3, RF3_PLAYER_GHOST},
	{"NONLIVING", RF3, RF3_NONLIVING},
	{"HURT_LITE", RF3, RF3_HURT_LITE},
	{"HURT_ROCK", RF3, RF3_HURT_ROCK},
	{"HURT_FIRE", RF3, RF3_HURT_FIRE},
	{"HURT_COLD", RF3, RF3_HURT_COLD}, /* UNUSED */
	{"IM_ACID", RF3, RF3_IM_ACID},
	{"IM_ELEC", RF3, RF3_IM_ELEC},
	{"IM_FIRE", RF3, RF3_IM_FIRE},
	{"IM_COLD", RF3, RF3_IM_COLD},
	{"IM_POIS", RF3, RF3_IM_POIS},
	{"RES_TELE", RF3, RF3_RES_TELE},
	{"RES_NETH", RF3, RF3_RES_NETH},
	{"IM_WATER", RF3, RF3_IM_WATER},
	{"RES_PLAS", RF3, RF3_RES_PLAS},
	{"RES_NEXU", RF3, RF3_RES_NEXU},
	{"RES_DISE", RF3, RF3_RES_DISE},
	{"CTHULOID", RF3, RF3_CTHULOID},
	{"NO_FEAR", RF3, RF3_NO_FEAR},
	{"NO_STUN", RF3, RF3_NO_STUN},
	{"NO_CONF", RF3, RF3_NO_CONF},
	{"NO_SLEEP", RF3, RF3_NO_SLEEP},

	/* Monster magic flags */

	{"SHRIEK", RF4, RF4_SHRIEK},
/* {"XXX3X3", RF4, RF4_XXX2}, */
/* {"XXX3X4", RF4, RF4_XXX3}, */
	{"BA_SHARD", RF4, RF4_BA_SHARD}, /* UNUSED */
	{"ARROW_1", RF4, RF4_ARROW_1},
	{"ARROW_2", RF4, RF4_ARROW_2},
	{"ARROW_3", RF4, RF4_ARROW_3},
	{"ARROW_4", RF4, RF4_ARROW_4},
	{"BR_ACID", RF4, RF4_BR_ACID},
	{"BR_ELEC", RF4, RF4_BR_ELEC},
	{"BR_FIRE", RF4, RF4_BR_FIRE},
	{"BR_COLD", RF4, RF4_BR_COLD},
	{"BR_POIS", RF4, RF4_BR_POIS},
	{"BR_NETH", RF4, RF4_BR_NETH},
	{"BR_LITE", RF4, RF4_BR_LITE},
	{"BR_DARK", RF4, RF4_BR_DARK},
	{"BR_CONF", RF4, RF4_BR_CONF},
	{"BR_SOUN", RF4, RF4_BR_SOUN},
	{"BR_CHAO", RF4, RF4_BR_CHAO},
	{"BR_DISE", RF4, RF4_BR_DISE},
	{"BR_NEXU", RF4, RF4_BR_NEXU},
	{"BR_TIME", RF4, RF4_BR_TIME},
	{"BR_INER", RF4, RF4_BR_INER},
	{"BR_GRAV", RF4, RF4_BR_GRAV},
	{"BR_SHAR", RF4, RF4_BR_SHAR},
	{"BR_PLAS", RF4, RF4_BR_PLAS},
	{"BR_WALL", RF4, RF4_BR_WALL},
	{"BR_MANA", RF4, RF4_BR_MANA},
	{"BA_NUKE", RF4, RF4_BA_NUKE},
	{"BR_NUKE", RF4, RF4_BR_NUKE},
	{"BA_CHAO", RF4, RF4_BA_CHAO},
	{"BR_DISI", RF4, RF4_BR_DISI},
	{"BA_ACID", RF5, RF5_BA_ACID},
	{"BA_ELEC", RF5, RF5_BA_ELEC},
	{"BA_FIRE", RF5, RF5_BA_FIRE},
	{"BA_COLD", RF5, RF5_BA_COLD},
	{"BA_POIS", RF5, RF5_BA_POIS},
	{"BA_NETH", RF5, RF5_BA_NETH},
	{"BA_WATE", RF5, RF5_BA_WATE},
	{"BA_MANA", RF5, RF5_BA_MANA},
	{"BA_DARK", RF5, RF5_BA_DARK},
	{"DRAIN_MANA", RF5, RF5_DRAIN_MANA},
	{"MIND_BLAST", RF5, RF5_MIND_BLAST},
	{"BRAIN_SMASH", RF5, RF5_BRAIN_SMASH},
	{"CAUSE_1", RF5, RF5_CAUSE_1},
	{"CAUSE_2", RF5, RF5_CAUSE_2},
	{"CAUSE_3", RF5, RF5_CAUSE_3},
	{"CAUSE_4", RF5, RF5_CAUSE_4},
	{"BO_ACID", RF5, RF5_BO_ACID},
	{"BO_ELEC", RF5, RF5_BO_ELEC},
	{"BO_FIRE", RF5, RF5_BO_FIRE},
	{"BO_COLD", RF5, RF5_BO_COLD},
	{"BO_POIS", RF5, RF5_BO_POIS}, /* UNUSED */
	{"BO_NETH", RF5, RF5_BO_NETH},
	{"BO_WATE", RF5, RF5_BO_WATE},
	{"BO_MANA", RF5, RF5_BO_MANA},
	{"BO_PLAS", RF5, RF5_BO_PLAS},
	{"BO_ICEE", RF5, RF5_BO_ICEE},
	{"MISSILE", RF5, RF5_MISSILE},
	{"SCARE", RF5, RF5_SCARE},
	{"BLIND", RF5, RF5_BLIND},
	{"CONF", RF5, RF5_CONF},
	{"SLOW", RF5, RF5_SLOW},
	{"HOLD", RF5, RF5_HOLD},
	{"HASTE", RF6, RF6_HASTE},
	{"DREAD_CURSE", RF6, RF6_DREAD_CURSE},
	{"HEAL", RF6, RF6_HEAL},
/* {"XXX2X6", RF6, RF6_XXX2}, */
	{"BLINK", RF6, RF6_BLINK},
	{"TPORT", RF6, RF6_TPORT},
/* {"XXX3X6", RF6, RF6_XXX3}, */
/* {"XXX4X6", RF6, RF6_XXX4}, */
	{"TELE_TO", RF6, RF6_TELE_TO},
	{"TELE_AWAY", RF6, RF6_TELE_AWAY},
	{"TELE_LEVEL", RF6, RF6_TELE_LEVEL},
/* {"XXX5X6", RF6, RF6_XXX5}, */
	{"DARKNESS", RF6, RF6_DARKNESS},
	{"TRAPS", RF6, RF6_TRAPS},
	{"FORGET", RF6, RF6_FORGET},
	{"S_IB", RF6, RF6_S_IB},
	{"S_KIN", RF6, RF6_S_KIN},
	{"S_REAVER", RF6, RF6_S_REAVER},
	{"S_MONSTER", RF6, RF6_S_MONSTER},
	{"S_MONSTERS", RF6, RF6_S_MONSTERS},
	{"S_ANT", RF6, RF6_S_ANT},
	{"S_SPIDER", RF6, RF6_S_SPIDER},
	{"S_HOUND", RF6, RF6_S_HOUND},
	{"S_HYDRA", RF6, RF6_S_HYDRA},
	{"S_CTHULOID", RF6, RF6_S_CTHULOID},
	{"S_DEMON", RF6, RF6_S_DEMON},
	{"S_UNDEAD", RF6, RF6_S_UNDEAD},
	{"S_DRAGON", RF6, RF6_S_DRAGON},
	{"S_HI_UNDEAD", RF6, RF6_S_HI_UNDEAD},
	{"S_HI_DRAGON", RF6, RF6_S_HI_DRAGON},
	{"S_GOO", RF6, RF6_S_GOO},
	{"S_UNIQUE", RF6, RF6_S_UNIQUE},

	/* Summoning "flags". */
	{"CTHULOID", SUMMON, SUMMON_CTHULOID},
	{"DEMON", SUMMON, SUMMON_DEMON},
	{"UNDEAD", SUMMON, SUMMON_UNDEAD},
	{"DRAGON", SUMMON, SUMMON_DRAGON},
	{"GREAT_OLD_ONE", SUMMON, SUMMON_GOO},
	{"ORC", SUMMON, SUMMON_ORC},
	{"ANIMAL", SUMMON, SUMMON_ANIMAL},
	{"UNIQUE", SUMMON, SUMMON_UNIQUE},
	{"HI_UNDEAD", SUMMON, SUMMON_HI_UNDEAD},
	{"HI_DRAGON", SUMMON, SUMMON_HI_DRAGON},
	{"HOUND", SUMMON, SUMMON_HOUND},
	{"MIMIC", SUMMON, SUMMON_MIMIC},
	{"ANIMAL_RANGER", SUMMON, SUMMON_ANIMAL_RANGER},
	{"REAVER", SUMMON, SUMMON_REAVER},
	{"PHANTOM", SUMMON, SUMMON_PHANTOM},
	{"ELEMENTAL", SUMMON, SUMMON_ELEMENTAL},

	/* Dungeon flags. */
	{"TOWER", DF, DF_TOWER},
	{"START", DF, DF_START},
	{"KADATH", DF, DF_KADATH},
	{"NO_UNIQUES", DF, DF_NO_UNIQUES},
	{"NIGHTTIME", DF, DF_NIGHTTIME},

	/* Shop "flags". */
	{"GENERAL", SHOP, STORE_GENERAL},
	{"ARMOURY", SHOP, STORE_ARMOURY},
	{"WEAPON", SHOP, STORE_WEAPON},
	{"TEMPLE", SHOP, STORE_TEMPLE},
	{"ALCHEMIST", SHOP, STORE_ALCHEMIST},
	{"MAGIC", SHOP, STORE_MAGIC},
	{"BLACK", SHOP, STORE_BLACK},
	{"HOME", SHOP, STORE_HOME},
	{"LIBRARY", SHOP, STORE_LIBRARY},
	{"INN", SHOP, STORE_INN},
	{"HALL", SHOP, STORE_HALL},
	{"PAWN", SHOP, STORE_PAWN},
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
		place[i]=' ';
	}
	return value;
}

/* A few macros for use in the 'E' case in init_r_event_txt() */

/* If one copy of chr exists within buf, return the integer immediately after it. */
#define readnum(chr) \
((!strchr(buf, chr)) ? -1 : \
	(strchr(strchr(buf, chr)+1, chr)) ? -2 : \
	atoi(strchr(buf, chr)+1))

/* A separate routine to remove used number strings */
#define clearnum(chr) \
{ \
	char *s; \
	for (s = strchr(buf, chr); *s == chr || (*s >= '0' && *s <= '9'); s++) \
		*s=' '; \
}

/* A routine to set x to be the number after a given letter, and then clear it from the text string.
 * If there are no such flags, it does nothing. If there are more than one, it returns an error.
 * THIS ASSUMES THAT ALL VALID VALUES ARE NON-NEGATIVE
 */
#define readclearnum(x, chr) \
if (readnum(chr) == -2) \
{ \
	msg_format("Too many '%c's!", chr); \
	msg_print(NULL); \
	return PARSE_ERROR_GENERIC; \
} \
else if (readnum(chr) > -1) \
{ \
	x=readnum(chr); \
	clearnum(chr); \
}

/*
 * A wrapper around find_string for info files.
 * As the text entries are stored as offsets in a single string rather than
 * simply as an array of strings, this first turns the former into the latter.
 * It assumes that the entries have numbers within (0,max)
 */
#define find_string_info(x_name, x_info, max, w) \
{ \
	int i; \
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
	return 0 != find_string(buf, array);
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
			return PARSE_ERROR_GENERIC;
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
		return PARSE_ERROR_OUT_OF_MEMORY;
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
 * Given the name of a race, find it in the list of races and remove it.
 * This does, of course, rely on r_name and r_info being present and correct.
 *
 * It should be called with 0 after its last real use to free the nam array.
 */
static s16b find_monster_race(char *buf)
{
	int i;
	s16b out;
	static cptr *array = NULL;

	/* Hack - use find_monster_race(0) to remove the array. */
	if (!buf)
	{
		/* Something to do. */
		if (array)
		{
			for (i = 0; i < MAX_R_IDX; i++) FREE(array[i]);
			KILL(array);
		}
		return 0;
	}
	else if (!array)
	{
		/* Build up an array of races. */
		C_MAKE(array, MAX_R_IDX, cptr);
		for (i = 1; i < MAX_R_IDX; i++)
		{
			if (r_info[i].name)
				array[i-1] = string_make(format("%v",
					monster_desc_aux_f3, r_info+i, 1, 0));
			else
				array[i-1] = string_make("");
		}
		/* Terminate the array. */
		array[MAX_R_IDX-1] = NULL;
	}

	/* Search it for the input string. */
	out = find_string(buf, array);

	/* Return the index matched, if any. */
	return out;
}


static int find_string_ego_info(char *buf, int k_idx)
{
	C_TNEW(array, MAX_E_IDX, cptr);
	int i;
	for (i = 1; i < z_info->e_max; i++)
	{
		ego_item_type *e_ptr = e_info+i;
		if (e_ptr->name && e_ptr->max_obj >= k_idx
			&& e_ptr->min_obj <= k_idx)
		{
			array[i-1] = e_name+e_ptr->name;
		}
		else
		{
			array[i-1] = "";
		}
	}
	array[MAX_E_IDX-1] = 0;
	k_idx = find_string(buf, array);
	TFREE(array);
	return k_idx;
}

/*
 * Parse a description of an object into a make_item_type template.
 * name points to a number which indicates a name for this object, if any.
 * It will be cleared if it is used.
 */
static errr parse_object(make_item_type *i_ptr, char *buf, u16b *name)
{
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
	find_string_info(k_name, k_info, MAX_K_IDX, i_ptr->k_idx);
	if (i_ptr->flags & EI_EGO)
	{
		i_ptr->x_idx = find_string_ego_info(buf, i_ptr->k_idx);
	}
	if (readnum('k'))
	{
		readclearnum(i_ptr->k_idx, 'k');
	}
	if (i_ptr->flags & EI_ART)
	{
		readclearnum(i_ptr->x_idx, 'a');
	}
	else if (i_ptr->flags & EI_EGO)
	{
		readclearnum(i_ptr->x_idx, 'e');
	}
	readclearnum(i_ptr->min, '(');
	readclearnum(i_ptr->max, '-');

	/* Add the name of a randart, if provided. */
	if (i_ptr->flags & EI_RAND && i_ptr->flags & EI_ART)
	{
		if (*name)
		{
			i_ptr->name = *name;
			*name = 0;
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
			return PARSE_ERROR_GENERIC;
		}
		/* Take an unstated k_idx to be that of the artefact. */
		else if (!i_ptr->k_idx)
		{
			i_ptr->k_idx = a_ptr->k_idx;
		}
		/* Ensure that any stated k_idx is the right one. */
		else if (i_ptr->k_idx != a_ptr->k_idx)
		{
			msg_print("Incompatible object and artefact.");
			return PARSE_ERROR_GENERIC;
		}
	}
	else if (i_ptr->flags & EI_EGO && ~i_ptr->flags & EI_RAND)
	{
		ego_item_type *e_ptr = &e_info[i_ptr->x_idx];
		/* Ensure this is a real ego item. */
		if (!e_ptr->name)
		{
			msg_print("No valid ego type specified.");
			return PARSE_ERROR_GENERIC;
		}
		/* Ensure that the ego type is possible for this k_idx. */
	}

	/* Ensure that a possible k_idx field has been created. */
	if (k_info[i_ptr->k_idx].name == 0)
	{
		msg_print("No valid object specified.");
		return PARSE_ERROR_GENERIC;
	}

	/* Ensure that RAND has not been used without a sensible thing
	 * to randomise. */
	if (i_ptr->flags & EI_RAND &&
		~i_ptr->flags & (EI_ART | EI_EGO))
	{
		msg_print("Nothing valid to randomise.");
		return PARSE_ERROR_GENERIC;
	}

	/* Prevent badly formatted ranges. */
	if (i_ptr->min > i_ptr->max || !i_ptr->min)
	{
		msg_print("Bad number parameter.");
		return PARSE_ERROR_INVALID_FLAG;
	}

	return SUCCESS;
}

/*
 * Parse an object which is described in buf in a simple way with no parameters
 * for other functions. If there is a name for a randart, it is given as ^name^
 * and stored in the fake name array.
 */
static errr parse_object_simple(header *head, make_item_type *i_ptr, char *buf)
{
	u16b name = 0;

	try(do_get_string(buf, '^', "^", head->name_ptr, &head->name_size,
		z_info->fake_name_size, &name));

	try(parse_object(i_ptr, buf, &name));

	/* The type-specific text field should have either been reset, or not
	 * set in the first place. */
	if (name)
	{
		msg_print("Meaningless text field found.");
		return PARSE_ERROR_GENERIC;
	}

	/* Check that the whole of "buf" has been parsed. */
	for (; *buf; buf++)
	{
		if (!strchr(": )\"", *buf))
		{
			msg_print("Uninterpreted characters!");
			return PARSE_ERROR_GENERIC;
		}
	}
	return SUCCESS;
}

/*
 * Parse the description of a monster into a make_monster_type.
 */
static errr parse_monster(make_monster_type *i_ptr, char *buf)
{
	i_ptr->strict = find_string_x(buf, "STRICT");
	i_ptr->num = find_monster_race(buf);
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
		return PARSE_ERROR_INVALID_FLAG;
	}

	/* Prevent non-existant monster references. */
	if (!i_ptr->num || i_ptr->num >= MAX_R_IDX)
	{
		msg_print("No monster specified!");
		return PARSE_ERROR_INVALID_FLAG;
	}
	return SUCCESS;
}

/*
 * Parse the description of an explosion into a make_explosion_type.
 */
static errr parse_explosion(make_explosion_type *i_ptr, char *buf)
{
	cptr explode_flags[N_ELEMENTS(gf_info)+1], *s;
	gf_type *gf_ptr;

	for (gf_ptr = gf_info, s = explode_flags; gf_ptr < END_PTR(gf_info);
		gf_ptr++, s++)
	{
		*s = gf_ptr->flag;
	}

	/* Terminate the array. */
	*s = NULL;

	i_ptr->method = find_string(buf, explode_flags);
	readclearnum(i_ptr->radius,'r');
	readclearnum(i_ptr->dice,'(');
	readclearnum(i_ptr->sides,'d');

	/* Require an explosion type */
	if (!i_ptr->method)
	{
		msg_print("No method indicated.");
		return PARSE_ERROR_INVALID_FLAG;
	}
	/* Allow (d30) or (100) damage formats,
	 * but not no damage indicator at all. */
	if (!i_ptr->dice && !i_ptr->sides)
	{
		msg_print("No damage indicator.");
		return PARSE_ERROR_INVALID_FLAG;
	}
	else if (!i_ptr->dice)
	{
		i_ptr->dice = 1;
	}
	else if (!i_ptr->sides)
	{
		i_ptr->sides = 1;
	}
	return SUCCESS;
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
			u16b temp_name_offset = 0;


			/* Nothing can be done without a valid monster */
			if (!r_idx)
			{
				msg_print("No r_idx specified!");
				return PARSE_ERROR_MISSING_RECORD_HEADER;
			}

			/* Find an unused event slot */
			if ((++error_idx) >= MAX_I)
			{
				msg_print("Too many events!");
				return PARSE_ERROR_OUT_OF_MEMORY;
			}
			d_ptr = (death_event_type*)head->info_ptr+error_idx;


			/* Look for a text string to avoid matching keywords within it later.
			 * To avoid conflicts with other arbitrary text, a \\ before a character
			 * means that it can't terminate a string.
			 * This is the string which is printed when an event occurs.
			 */
			try(do_get_string(buf, '"', "^\"", head->text_ptr,
				&(head->text_size), z_info->fake_text_size, &(d_ptr->text)));

			/* This is a second text string, the use of which depends on the type of event.
			 * It currently only supplies a name for a randart. We haven't yet found out
			 * what type of event it is, so we save the offset to a temporary variable to
			 * deal with later.
			 */
			try(do_get_string(buf, '^', "^\"", head->name_ptr,
				&(head->name_size), z_info->fake_name_size, &temp_name_offset));

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
				return PARSE_ERROR_INVALID_FLAG;
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
					try(parse_object(&d_ptr->par.item, buf,
						&temp_name_offset));
					break;
				}
				case DEATH_MONSTER:
				{
					try(parse_monster(&d_ptr->par.monster, buf));
					break;
				}
				case DEATH_EXPLODE:
				{
					try(parse_explosion(&d_ptr->par.explosion, buf));
					break;
				}
				case DEATH_COIN:
				{
					make_coin_type *i_ptr = &d_ptr->par.coin;
					i_ptr->metal = find_string(buf, coin_types)-1;
					if (i_ptr->metal == -1)
					{
						msg_print("No coin type!");
						return PARSE_ERROR_INVALID_FLAG;
					}
					break;
				}
				case DEATH_NOTHING: /* No special parameters */
				break;
				default: /* i.e. no valid type specification */
				{
					msg_print("No event type specified!");
					return PARSE_ERROR_MISSING_RECORD_HEADER;
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
				return PARSE_ERROR_GENERIC;
			}
			/* The type-specific text field should have either been reset, or not
			 * set in the first place. */
			if (temp_name_offset)
			{
				msg_print("Meaningless text field found.");
				return PARSE_ERROR_GENERIC;
			}

			/* Finally check that everything has been read. */
			{
				cptr s;
				for (s = buf+2; *s != '\0'; s++)
				{
					if (!strchr(": )\"", *s))
					{
						msg_print("Uninterpreted characters!");
						return PARSE_ERROR_GENERIC;
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
		return 0;

	/* Check object names for incorrect flags. */
	switch (head->header_num)
	{
		case K_HEAD: case A_HEAD: case E_HEAD: case OB_HEAD: case U_HEAD:
		case R_HEAD:
		if (check_string(buf)) return 0;
	}

	/* Advance and save the name index */
	index = ++head->name_size;

	/* Append chars to the names */
	strcpy(head->name_ptr + head->name_size, buf);

	/* Advance the index */
	head->name_size += strlen(buf);

	/* Return the name index */
	return index;
}



/*
 * Initialize the "z_info" structure, by parsing an ascii "template" file
 */
errr parse_z_info(char *buf, header *head, vptr UNUSED *extra)
{
	char c, end[1];
	long max;
	maxima *z = head->info_ptr;

	/* Hack - Always use record 0. */
	error_idx = 0;

	/* Verify M:x:num format. */
	if (2 != sscanf(buf, "M:%c:%ld%c", &c, &max, end))
		return PARSE_ERROR_INCORRECT_SYNTAX;

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

	char *s, end[1];

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
			if (!s) return PARSE_ERROR_GENERIC;

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = f_ptr = (feature_type*)head->info_ptr + i;

			/* Store the name */
			if (!(f_ptr->name = add_name(head, s)))
			return PARSE_ERROR_OUT_OF_MEMORY;

			/* Default "mimic" */
			f_ptr->mimic = i;

			return SUCCESS;
		}
		case 'M':
		{
			int mimic;

			/* Scan for the values */
			if (1 != sscanf(buf+2, "%d%c", &mimic, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			if (mimic < 0 || mimic > 255) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Don't allow, don't explain. */
			if (mimic == error_idx) return PARSE_ERROR_GENERIC;

			/* Mimic fields override graphics ones entirely. */
			if (f_ptr->priority || f_ptr->gfx.dc || f_ptr->gfx.da)
				return PARSE_ERROR_GENERIC;

			/* Save the values */
			f_ptr->mimic = mimic;

			return SUCCESS;
		}
		/* Process 'G' for "Graphics" (one line only) */
		case 'G':
		{
			int pri;
			char sym, col;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%c:%c%c", &pri, &sym, &col, end))
			{
				return PARSE_ERROR_INCORRECT_SYNTAX;
			}

			/* Mimic fields override graphics ones entirely. */
			if (f_ptr->mimic != error_idx) return PARSE_ERROR_GENERIC;

			/* Extract and check the color */
			if (color_char_to_attr(col) < 0) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Check the priority. */
			if (pri < 0 || pri > 255) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Save the values */
			f_ptr->priority = pri;
			f_ptr->gfx.dc = sym;
			f_ptr->gfx.da = color_char_to_attr(col);

			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}



/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr parse_v_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, end[1];

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
			if (!s) return PARSE_ERROR_GENERIC;

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = v_ptr = (vault_type*)head->info_ptr + i;

			/* Store the name */
			if (!(v_ptr->name = add_name(head, s)))
			return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}


		/* Process 'D' for "Description" */
		case 'D':
		{
			/* Acquire the text */
			s = buf+2;

			i = strlen(s);

			/* Check the width (vaults must be square). */
			if (!v_ptr->wid) v_ptr->wid = i;
			else if (i != v_ptr->wid) return PARSE_ERROR_GENERIC;

			v_ptr->hgt++;

			/* Store the text and continue. */
			return add_text(&(v_ptr->text), head, s);
		}


		/* Process 'X' for "Extra info" (one line only) */
		case 'X':
		{
			int typ, rat;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d%c", &typ, &rat, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			v_ptr->typ = typ;
			v_ptr->rat = rat;

			/* Next... */
			return SUCCESS;
		}

		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}




/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_flag(u32b **flag, cptr errstr, cptr what)
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
	msg_format("Unknown %s flag '%s'.", errstr, what);

	/* Error */
	return PARSE_ERROR_GENERIC;
}

static errr grab_one_kind_flag(object_kind *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[TR1] = &(ptr->flags1);
	f[TR2] = &(ptr->flags2);
	f[TR3] = &(ptr->flags3);
	return grab_one_flag(f, "object", what);
}


/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr parse_k_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, *t, end[1];

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

			/* Verify that colon and advance */
			if (!s++) return PARSE_ERROR_GENERIC;

			/* Get the index */
			i = atoi(buf+2);

			/* Hack - negative indices really refer to the user area. */
			if (i < 0) i = OBJ_MAX_DISTRO-i;

			/* Verify information */
			if (i <= error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;


			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = k_ptr = (object_kind*)head->info_ptr + i;

			/* Store the name */
			if (!(k_ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			/* Next... */
			return SUCCESS;
		}

		case 'G':
		{
			char sym, col;
			int tmp, p_id;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%c:%c:%d%c", &sym, &col, &p_id, end))
			{
				return PARSE_ERROR_INCORRECT_SYNTAX;
			}

			/* Extract the attr */
			tmp = color_char_to_attr(col);

			/* Paranoia */
			if (tmp < 0) return PARSE_ERROR_GENERIC;
			if (p_id < 0 || p_id > 255) return PARSE_ERROR_GENERIC;

			/* Save the values */
			k_ptr->gfx.dc = sym;
			k_ptr->gfx.da = tmp;

			/* Hack - store p_id in k_ptr->u_idx until flavor_init() */
			k_ptr->u_idx = p_id;

			/* Next... */
			return SUCCESS;
		}

		case 'I':
		{
			int tval, pval, kextra;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d%c", &tval, &pval, &kextra, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			if (tval < 0 || tval > 255 || kextra < 0 || kextra > 255)
				return PARSE_ERROR_OUT_OF_BOUNDS;

			switch(tval)
			{
				case TV_BOOK: case TV_CHARM:
				{
					if (kextra > MAX_BK) return PARSE_ERROR_OUT_OF_BOUNDS;
					break;
				}
			}

			/* Save the values */
			k_ptr->tval = tval;
			k_ptr->pval = pval;
			k_ptr->extra = kextra;

			/* Next... */
			return SUCCESS;
		}

		/* Process 'W' for "More Info" (one line only) */
		case 'W':
		{
			int krating, wgt;
			long cost;

			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%ld%c", &krating, &wgt, &cost, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			k_ptr->rating = krating;
			k_ptr->weight = wgt;
			k_ptr->cost = cost;

			/* Next... */
			return SUCCESS;
		}

		/* Process 'A' for "Allocation" (one line only) */
		case 'A':
		{
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
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d%c",
				&hd1, &hd2, &th, &td, &ac, &ta, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

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
				if (0 != grab_one_kind_flag(k_ptr, s)) return PARSE_ERROR_INVALID_FLAG;

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
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}



/*
 * Initialize the "o_base" array, by parsing an ascii "template" file
 */
errr parse_o_base(char *buf, header *head, vptr *extra)
{
	char end[1];

	/* Current entry */
	o_base_type *ob_ptr = *extra;

	/* If this isn't the start of a record, there should already be one. */
	switch (*buf)
	{
		case 'N': case 'M': break; /* N and M need nothing. */
		case 'C': /* C needs an existing record. */
		if (!ob_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER; break;
		default: /* Nothing else makes sense. */
		return PARSE_ERROR_UNDEFINED_DIRECTIVE;
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
			if (!s) return PARSE_ERROR_GENERIC;

			/* Advance to the name */
			s++;

			/* Verify index. */
			try(byte_ok(i));

			/* Advance the index */
			error_idx = i;

			/* Paranoia - there should always be space for 256 entries. */
			if (error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Point at the "info" */
			*extra = ob_ptr = (o_base_type*)head->info_ptr + error_idx;

			/* Store the name */
			if (!(ob_ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

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

			if (2 != sscanf(buf+2, "%d:%d%c", &oldp_id, &newp_id, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

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
			if (!ob_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

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
				if (1 != sscanf(buf+2, "%ld%c", &cost, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;
			}

			/* Save the value */
			ob_ptr->cost = cost;

			return SUCCESS;
		}
		default: /* Never reached. */
		{
			return PARSE_ERROR_GENERIC;
		}
	}
}

static errr parse_unid_flavourless_aux(header *head, int p_id)
{
	/* Find the next element in u_info. */
	unident_type *u_ptr = (unident_type*)head->info_ptr + (++error_idx);

	/* Check that u_info is large enough. */
	if (error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

	/* Set the fields as required. */
	u_ptr->name = 0;
	u_ptr->p_id = p_id;
	u_ptr->gfx.da = TERM_DARK;
	u_ptr->gfx.dc = ' ';

	return SUCCESS;
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

		/* No entry. */
		if (!ob_ptr->name) continue;

		/* Not flavourless. */
		if (strchr(ob_name+ob_ptr->name, CM_ACT+CI_FLAVOUR)) continue;

		/* Add an entry. */
		try(parse_unid_flavourless_aux(head, i));
	}

	/* Hack - there must always be at least one flavourless entry. */
	if (((unident_type*)(head->info_ptr))[error_idx].name)
	{
		/* Add a dummy entry. */
		try(parse_unid_flavourless_aux(head, 0));
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
	char *s, end[1];

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
			if (4 != sscanf(buf+2, "%c:%c:%d:%d%c",
				&sym, &col, &p_id, &s_id, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Paranoia */
			if (color_char_to_attr(col) < 0)
			{
				msg_print("Illegal colour.");
				return PARSE_ERROR_GENERIC;
			}
			if (!ISGRAPH(sym))
			{
				msg_print("Illegal symbol.");
				return PARSE_ERROR_GENERIC;
			}
			/* Extract the char */
			u_ptr->gfx.dc = sym;

			/* Extract the attr */
			u_ptr->gfx.da = color_char_to_attr(col);

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
				return PARSE_ERROR_GENERIC;
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
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[TR1] = &(ptr->flags1);
	f[TR2] = &(ptr->flags2);
	f[TR3] = &(ptr->flags3);
	return grab_one_flag(f, "object", what);
}




/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr parse_a_info(char *buf, header *head, vptr *extra)
{
	artifact_type *a_ptr = *extra;

	int i;

	char *s, *t, end[1];

	/* Need an a_ptr before anything is written. */
	if (*buf != 'N' && !a_ptr) return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N':
		{
			/* Find the colon before the name */
			s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return PARSE_ERROR_GENERIC;

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = a_ptr = (artifact_type*)head->info_ptr + i;

			/* Store the name */
			if (!(a_ptr->name = add_name(head, s)))
			return PARSE_ERROR_OUT_OF_MEMORY;

			/* Next... */
			return SUCCESS;
		}
		case 'I':
		{
			int k_idx, pval;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d%c", &k_idx, &pval, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Test the values */
			if (k_idx < 0 || k_idx >= MAX_K_IDX || !k_info[k_idx].name ||
				(pval < -32768 || pval > 32767))
			{
				return PARSE_ERROR_OUT_OF_BOUNDS;
			}

			/* Save the values */
			a_ptr->k_idx = k_idx;
			a_ptr->pval = pval;

			/* Next... */
			return SUCCESS;
		}
		case 'W':
		{
			int level, level2, rarity, wgt;
			long cost;

			/* Scan for the values */
			if (5 != sscanf(buf+2, "%d:%d:%d:%d:%ld%c",
				&level, &level2, &rarity, &wgt, &cost, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			a_ptr->level = level;
			a_ptr->level2 = level2;
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
			if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d%c",
				&hd1, &hd2, &th, &td, &ac, &ta, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

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
				if (0 != grab_one_artifact_flag(a_ptr, s)) return PARSE_ERROR_INVALID_FLAG;

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static errr grab_one_ego_item_flag(ego_item_type *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[TR1] = &(ptr->flags1);
	f[TR2] = &(ptr->flags2);
	f[TR3] = &(ptr->flags3);
	return grab_one_flag(f, "object", what);
}





/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr parse_e_info(char *buf, header *head, vptr *extra)
{
	char end[1];
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
			if (!s) return PARSE_ERROR_GENERIC;

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = e_ptr = (ego_item_type*)head->info_ptr + i;

			/* Store the name */
			if (!(e_ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}

		/* Process 'X' for "Xtra" (one line only) */
		case 'X':
		{
			int r,s;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%d%c", &r, &s, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			e_ptr->rating = r;
			e_ptr->special = s;

			return SUCCESS;
		}

		/* Process 'W' for "More Info" (one line only) */
		case 'W':
		{
			int chance;
			long cost;

			/* Scan for the values */
			if (2 != sscanf(buf+2, "%d:%ld%c", &chance, &cost, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			e_ptr->chance = chance;
			e_ptr->cost = cost;

			return SUCCESS;
		}

		/* Process 'P' for "potential power". */
		case 'P':
		{
			int th, td, ta, pv;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%d%c", &th, &td, &ta, &pv, end))
				return PARSE_ERROR_INCORRECT_SYNTAX;

			e_ptr->max_to_h = th;
			e_ptr->max_to_d = td;
			e_ptr->max_to_a = ta;
			e_ptr->max_pval = pv;

			return SUCCESS;
		}

		/* Process 'O' for object range. */
		case 'O':
		{
			int min, max;
			switch (sscanf(buf+2, "%d:%d%c", &min, &max, end))
			{
				case 1:
				{
					max = min;
					/* Fall through. */
				}
				case 2:
				{
					/* Check for sanity. */
					if (!min || max >= z_info->k_max || max < min)
						return PARSE_ERROR_OUT_OF_BOUNDS;

					/* Set stuff. */
					e_ptr->min_obj = min;
					e_ptr->max_obj = max;
					return SUCCESS;
				}
				default:
				{
					return PARSE_ERROR_INCORRECT_SYNTAX;
				}
			}
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
				if (0 != grab_one_ego_item_flag(e_ptr, s)) return PARSE_ERROR_INVALID_FLAG;

				/* Start the next entry */
				s = t;
			}

			return SUCCESS;
		}

		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}


static errr grab_one_basic_flag(monster_race *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[RF1] = &(ptr->flags1);
	f[RF2] = &(ptr->flags2);
	f[RF3] = &(ptr->flags3);
	return grab_one_flag(f, "monster", what);
}

static errr grab_one_spell_flag(monster_race *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[RF4] = &(ptr->flags4);
	f[RF5] = &(ptr->flags5);
	f[RF6] = &(ptr->flags6);
	return grab_one_flag(f, "monster", what);
}




/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr parse_r_info(char *buf, header *head, vptr *extra)
{
	int i;

	char *s, *t, end[1];

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
			if (!s) return PARSE_ERROR_GENERIC;

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i <= error_idx) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

			/* Verify information */
			if (i >= MAX_I) return PARSE_ERROR_OBSOLETE_FILE;

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			*extra = r_ptr = (monster_race*)head->info_ptr + i;

			/* Store the name */
			if (!(r_ptr->name = add_name(head, s)))
			return PARSE_ERROR_OUT_OF_MEMORY;

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
			if (!buf[2]) return PARSE_ERROR_GENERIC;
			if (!buf[3]) return PARSE_ERROR_GENERIC;
			if (!buf[4]) return PARSE_ERROR_GENERIC;

			/* Extract the color */
			tmp = color_char_to_attr(buf[4]);
			if (tmp < 0) return PARSE_ERROR_GENERIC;

			/* Save the values */
			r_ptr->gfx.dc = buf[2];
			r_ptr->gfx.da = tmp;

			return SUCCESS;
		}

		/* Process 'I' for "Info" (one line only) */
		case 'I':
		{
			int spd, atspd, hp1, hp2, aaf, ac, slp;

			/* Scan for the other values */
			if (7 != sscanf(buf+2, "%d:%d:%dd%d:%d:%d:%d%c",
				&spd, &atspd, &hp1, &hp2, &aaf, &ac, &slp, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			r_ptr->speed = spd+110;
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
			if (4 !=
				sscanf(buf+2, "%d:%d:%d:%ld%c", &lev, &rar, &pad, &exp, end))
					return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Save the values */
			r_ptr->level = lev;
			r_ptr->rarity = rar;
/* r_ptr->extra = pad;*/
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
			if (i == 4) return PARSE_ERROR_GENERIC;

			/* Analyze the first field */
			for (s = t = buf+2; *t && (*t != ':'); t++) /* loop */;

			/* Terminate the field (if necessary) */
			if (*t == ':') *t++ = '\0';

			/* Analyze the method */
			for (n1 = 0; n1 < NUM_BLOW_METHODS; n1++)
			{
				if (streq(s, blow_methods[n1].flagname)) break;
			}

			/* Invalid method */
			if (!blow_methods[n1].flagname) return PARSE_ERROR_GENERIC;

			/* "No blow" is denoted by 0, so increase the index of the first
			 * method to 1. */
			n1++;

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
			if (!r_info_blow_effect[n2]) return PARSE_ERROR_GENERIC;

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
				if (0 != grab_one_basic_flag(r_ptr, s)) return PARSE_ERROR_INVALID_FLAG;

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
					/* Sanity check */
					if ((i < 1) || (i > 100))
						return PARSE_ERROR_INVALID_SPELL_FREQ;

					/* Extract a "frequency" */
					r_ptr->freq_spell = r_ptr->freq_inate = 100 / i;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
				if (0 != grab_one_spell_flag(r_ptr, s)) return PARSE_ERROR_INVALID_FLAG;

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
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}



/*
 * Grab one value from a textual string
 *
 * This is slightly more complicated as the returned value will be 16 bits
 * long rather than 32.
 */
static errr grab_one_summon_flag(u32b *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[SUMMON] = ptr;
	return grab_one_flag(f, "summon", what);
}


/*
 * Grab one dungeon flag from a textual string
 */
static errr grab_one_dungeon_flag(u32b *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[DF] = ptr;
	return grab_one_flag(f, "dungeon", what);
}

/*
 * Initialize the "dun_defs" array, by parsing an ascii "template" file
 */
errr parse_dun_defs(char *buf, header *head, vptr *extra)
{
	/* Current entry */
	dun_type *ptr = *extra;

	/* Only N can start a record. */
	if (!ptr && *buf != 'N') return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N': /* N:symbol:shortname... */
		{
			/* Check the format. */
			if (buf[3] != ':') return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Check that dun_defs is large enough. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Set ptr */
			*extra = ptr = (dun_type*)head->info_ptr + error_idx;

			/* Store the symbol. */
			ptr->sym = buf[2];

			/* Store the name. */
			if (!(ptr->shortname = add_name(head, buf+4)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}
		case 'D': /* D:name... */
		{
			/* Avoid multiple descriptions. */
			if (ptr->name) return PARSE_ERROR_TOO_MANY_ARGUMENTS;

			/* Store the name. */
			if (!(ptr->name = add_name(head, buf+2)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}
		case 'I': /* I:min-max:bias... */
		{
			char t[1024] = "";
			int min, max, i;

			/* Scan for values. */
			i = sscanf(buf+2, "%d-%d:%c%1023[^\n]", &min, &max, t, t+1);

			/* Check the format. */
			if (i < 2) return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Hack - use 0-0 to indicate a town with no dungeon (the entry can
			 * still be necessary because of the town bias). */
			if (!min && !max)
			{
			}
			/* Check the level range for sanity. */
			else if (min <= 0 || max > 127 || min > max)
			{
				return PARSE_ERROR_OUT_OF_BOUNDS;
			}
			else
			{
				ptr->offset = min-1;
				ptr->max_level = max-min+1;
			}

			/* Notice whether t contains a character or a string. */
			if (i == 4)
			{
				u32b f = 0;
				/* Grab a pre-defined summon type. */
				try(grab_one_summon_flag(&f, t));
				ptr->bias |= f & ~(SUMMON_NO_UNIQUES);
			}
			else if (i == 3)
			{
				/* Grab a character to summon. */
				ptr->bias |= SUMMON_CHAR(*t);
			}

			return SUCCESS;
		}
		case 'F': /* F:TOWER */
		{
			char *s, *t;
			/* Parse every entry textually */
			for (s = buf + 2; *s; )
			{
				u32b f = 0;
				/* Find the end of this entry */
				for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) /* loop */;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|' || *t == ':') t++;
				}

				/* Parse this entry */
				try(grab_one_dungeon_flag(&f, s));

				/* Hack - DF_NO_UNIQUES affects the summon type, not the flags. */
				if (f == DF_NO_UNIQUES)
				{
					ptr->bias |= SUMMON_NO_UNIQUES;
				}
				else
				{
					ptr->flags |= f;
				}

				/* Start the next entry */
				s = t;
			}

			return SUCCESS;
		}
		case 'Q': case 'T': case 'S': /* Ignore other valid flags. */
		{
			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}


/*
 * Grab one shop flag from a textual string
 */
static errr grab_one_shop_type(u32b *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[SHOP] = ptr;
	return grab_one_flag(f, "shop", what);
}

/*
 * Initialize the "town_defs" array, by parsing an ascii "template" file
 */
errr parse_town_defs(char *buf, header *head, vptr *extra)
{
	/* Current entry */
	town_type *ptr = *extra;

	/* Only N can start a record. */
	if (!ptr && *buf != 'N') return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N': /* New */
		{
			/* Check that dun_defs is large enough. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Set ptr */
			*extra = ptr = (town_type*)head->info_ptr + error_idx;

			/* Hack - clear the shop table. */
			C_BSET(ptr->store, 99, MAX_STORES_PER_TOWN, byte);

			return SUCCESS;
		}
		case 'T': /* House price, town description. */
		{
			char s[1024];
			long i;
			if (2 != sscanf(buf+2, "%ld:%1023[^\n]", &i, s))
			{
				return PARSE_ERROR_INCORRECT_SYNTAX;
			}

			/* Check that the price is reasonable. */
			if (i < 0) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Copy the price across. */
			ptr->house_price = i;

			/* Store the name. */
			if (!(ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}
		case 'S':
		{
			char *s, *t;

			int i;
			/* Find the first unset store. */
			for (i = 0; i < MAX_STORES_PER_TOWN; i++)
			{
				if (ptr->store[i] == 99) break;
			}
			if (i == MAX_STORES_PER_TOWN) return PARSE_ERROR_TOO_MANY_ARGUMENTS;

			/* Parse every entry textually */
			for (s = buf + 2; *s; )
			{
				u32b shop = 0;
				/* Find the end of this entry */
				for (t = s; *t && !strchr(" |:", *t); t++) /* loop */;

				/* Avoid overflow. */
				if (i >= MAX_STORES_PER_TOWN) return PARSE_ERROR_TOO_MANY_ARGUMENTS;

				/* Nuke and skip any dividers */
				if (*t)
				{
					*t++ = '\0';
					while (*t == ' ' || *t == '|' || *t == ':') t++;
				}

				/* Find a shop. */
				try(grab_one_shop_type(&shop, s));

				/* Remember the shop. */
				ptr->store[i++] = shop;

				/* Start the next entry */
				s = t;
			}

			/* Remember how many were seen. */
			ptr->numstores = i;

			/* Next... */
			return SUCCESS;
		}
		case 'D': case 'I': case 'Q': case 'F': /* Ignore other valid flags. */
		{
			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}

/*
 * Initialize the "q_list" array, by parsing an ascii "template" file
 */
errr parse_q_list(char *buf, header *head, vptr *extra)
{
	/* Find the current dungeon. */
	dun_type *ptr = *extra;
	int n = (ptr) ? ptr - dun_defs : -1;

	/* Only N can start a record. */
	if (n == -1 && *buf != 'N') return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N': /* New */
		{
			/* Check that this doesn't somehow overflow the dungeon table. */
			if (++n >= z_info->dungeons) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Set the index */
			*extra = dun_defs+n;

			return SUCCESS;
		}
		case 'Q':
		{
			int lev, num, mon;
			char t[1024];
			quest_type *q_ptr;

			/* Scan for values. */
			if (sscanf(buf+2, "%d:%d:%1023[^\n]", &lev, &num, t) < 3)
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Check that the values are reasonable. */
			if (lev <= ptr->offset || lev > ptr->offset+ptr->max_level ||
				num < 1 || num > 255) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Check that q_list is large enough. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			q_ptr = (quest_type*)(head->info_ptr)+error_idx;

			/* Find the monster specified. */
			mon = find_monster_race(buf);

			/* Check that a monster was found. */
			if (mon < 1 || mon >= MAX_R_IDX) return PARSE_ERROR_INVALID_FLAG;

			/* Only one unique can exist. */
			if ((r_info[mon].flags1 & RF1_UNIQUE) && num > 1)
			{
				return PARSE_ERROR_GENERIC;
			}

			/* Set everything. */
			q_ptr->r_idx = mon;
			q_ptr->level = lev - ptr->offset;
			q_ptr->dungeon = n;
			q_ptr->max_num = num;
			q_ptr->known = TRUE; /* Fixed quests are known from the start. */

			return SUCCESS;
		}
		case 'D': case 'I': case 'T': case 'S': case 'F':
		{
			/* Ignore other valid flags. */
			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}

/*
 * Initialize the "owners" array, by parsing an ascii "template" file
 */
errr parse_s_info(char *buf, header *head, vptr *extra)
{
	char end[1];
	owner_type *ptr = *extra;

	/* Only N can start a record. */
	if (!ptr && *buf != 'N') return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		case 'N':
		{
			/* Check that there is enough space. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Set ptr */
			*extra = ptr = (owner_type*)head->info_ptr + error_idx;

			/* Store the name. */
			if (!(ptr->name = add_name(head, buf+2)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}
		case 'C':
		{
			long cost, linf, uinf, haggle, insult;

			/* Scan for values. */
			if (sscanf(buf+2, "%ld:%ld:%ld:%ld:%ld%c", &cost, &uinf, &linf,
				&haggle, &insult, end) != 5)
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Check the numbers are reasonable. */
			if (cost < 0 || cost > 32767 ||
				linf < 0 || linf > 255 ||
				uinf < 0 || uinf > 255 ||
				haggle < 0 || haggle > 255 ||
				insult < 0 || insult > 255)
			{
				return PARSE_ERROR_OUT_OF_BOUNDS;
			}

			/* Copy the numbers across. */
			ptr->max_cost = cost;
			ptr->max_inflate = uinf;
			ptr->min_inflate = linf;
			ptr->haggle_per = haggle;
			ptr->insult_max = insult;

			return SUCCESS;
		}
		case 'I':
		{
			char races[1024], towns[1024];
			int shop, race, town, p;

			/* Scan for values. */
			p = sscanf(buf+2, "%d:%1023[^:]:%1023[^\n]", &shop, races, towns);

			/* The town is optional. */
			if (p < 2) return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Interpret the strings. */
			for (race = 0; race < MAX_RACES; race++)
				if (!strcmp(races, race_info[race].title)) break;

			if (p < 3)
			{
				town = TOWN_NONE;
			}
			else
			{
				for (town = 0; town < MAX_TOWNS; town++)
					if (!strcmp(towns, dun_name+dun_defs[town].shortname)) break;
			}

			/* Check the numbers are reasonable. */
			if (town == MAX_TOWNS || race == MAX_RACES ||
				shop < 0 || shop >= MAX_STORE_TYPES)
			{
				return PARSE_ERROR_OUT_OF_BOUNDS;
			}

			/* Copy the numbers across. */
			ptr->shop_type = shop;
			ptr->owner_race = race;
			ptr->town = town;

			return SUCCESS;
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
		}
	}
}

/*
 * Set the skill in "ptr" containing "name" as a substring to "v".
 */
static errr parse_template_skill(player_template *tp_ptr, cptr name, long v)
{
	player_skill *sk_ptr;
	int skill = -1;

	/* Find the one skill with a name containing "name", or fail. */
	FOR_ALL_IN(skill_set, sk_ptr)
	{
		if (!strstr(sk_ptr->name, name)) continue;
		if (skill >= 0) return PARSE_ERROR_INVALID_FLAG;
		skill = sk_ptr - skill_set;
	}
	if (skill < 0) return PARSE_ERROR_INVALID_FLAG;

	/* Bounds check (should this become a signed char?). */
	if (ABS(v) > MAX_SHORT) return PARSE_ERROR_OUT_OF_BOUNDS;

	/* Copy to tp_ptr. */
	tp_ptr->skill[skill] = v;

	return SUCCESS;
}

/*
 * Initialize the "player templates" array, by parsing an ascii "template" file.
 */
errr parse_template(char *buf, header *head, vptr *extra)
{
	char end[1];
	player_template *ptr = *extra;
	long p[6];
	int i;

	/* Only N can start a record. */
	if (!ptr && *buf != 'N') return PARSE_ERROR_MISSING_RECORD_HEADER;

	switch (*buf)
	{
		/* Process 'N' for "New/Number/Name" */
		case 'N':
		{
			/* Get the index */
			int i = atoi(buf+2);

			/* Find the colon before the name */
			char *s = strchr(buf+2, ':');

			/* Verify that colon */
			if (!s) return PARSE_ERROR_GENERIC;

			/* Advance to the name */
			s++;

			/* Verify index. */
			try(byte_ok(i));

			/* Advance the index */
			error_idx = i;

			/* Paranoia - there should always be space for 256 entries. */
			if (error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Point at the "info" */
			*extra = ptr = (player_template*)head->info_ptr + error_idx;

			/* Store the name */
			if (!(ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

			return SUCCESS;
		}
		/* Process 'X' for "Extra info" */
		case 'X':
		{
			/* Read. */
			if (sscanf(buf+2, "%ld%c", p, end) != 1)
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Bounds check. */
			if (*p < 0 || *p > 255) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Copy. */
			ptr->choices = *p;

			return SUCCESS;
		}
		/* Process 'S' for "Stat bonuses" */
		case 'S':
		{
			assert(A_MAX == 6); /* Give up sscanf? Never! */

			/* Read. */
			if (sscanf(buf+2, "%ld:%ld:%ld:%ld:%ld:%ld%c",
				p, p+1, p+2, p+3, p+4, p+5, end) != 6)
				return PARSE_ERROR_INCORRECT_SYNTAX;

			for (i = 0; i < A_MAX; i++)
			{
				/* Bounds check. */
				if (ABS(p[i]) > MAX_SHORT) return PARSE_ERROR_OUT_OF_BOUNDS;

				/* Copy. */
				ptr->c_adj[i] = p[i];
			}

			return SUCCESS;
		}
		/* Process 'A' for "Artefact effects" */
		case 'A':
		{
			/* Read. */
			if (sscanf(buf+2, "%ld:%ld:%ld%c", p, p+1, p+2, end) != 3)
				return PARSE_ERROR_INCORRECT_SYNTAX;

			/* Bounds check. */
			if (p[0] < 0 || p[0] > 255) return PARSE_ERROR_OUT_OF_BOUNDS;
			if (p[1] < 0 || p[1] > 255) return PARSE_ERROR_OUT_OF_BOUNDS;
			if (p[2] < 0 || p[2] > 255) return PARSE_ERROR_OUT_OF_BOUNDS;

			/* Copy. */
			ptr->art1_bias = p[0];
			ptr->art2_bias = p[1];
			ptr->art2_chance = p[2];

			return SUCCESS;
		}
		/* Process 'K' for "Skill bonuses" */
		case 'K':
		{
			/* Parse each argument in turn (except for the last one). */
			char tmp[SKILL_NAME_LEN];
			assert(SKILL_NAME_LEN >= 128); /* Hmm... */
			for (buf++; sscanf(buf, ":%128[^=:]=%ld:%c", tmp, p, end) == 3;
				buf = strchr(buf+1, ':'))
			{
				try(parse_template_skill(ptr, tmp, *p));
			}
			/* Parse the final argument, or fail. */
			if (sscanf(buf, ":%128[^=:]=%ld%c", tmp, p, end) == 2)
			{
				try(parse_template_skill(ptr, tmp, *p));
			}
			else
			{
				return PARSE_ERROR_INCORRECT_SYNTAX;
			}

			return SUCCESS;
		}
		/* Process 'E' for "Initial equipment" */
		case 'E':
		{
			for (i = 0; i < MAX_TPL_ITEMS; i++)
			{
				/* Parse the object and finish. */
				if (!ptr->items[i].k_idx)
				{
					 return parse_object_simple(head, ptr->items+i, buf+2);
				}
			}
			return PARSE_ERROR_TOO_MANY_ARGUMENTS;
		}
		/* Process 'O' for "Alternate initial equipment" */
		case 'O':
		{
			for (i = 0; i < MAX_TPL_ITEMS; i++)
			{
				if (!ptr->items[i].k_idx) break;
			}

			/* No equipment has been described yet. */
			if (!i) return PARSE_ERROR_MISSING_RECORD_HEADER;

			/* Find the backup for the last item given. */
			i += MAX_TPL_ITEMS-1;

			/* Already defined. */
			if (ptr->items[i].k_idx) return PARSE_ERROR_TOO_MANY_ARGUMENTS;

			/* Parse the object and finish. */
			return parse_object_simple(head, ptr->items+i, buf+2);
		}
		default:
		{
			return PARSE_ERROR_UNDEFINED_DIRECTIVE;
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
		return PARSE_ERROR_UNDEFINED_DIRECTIVE;
	}

	/* Process 'X' for "From" */
	switch (*buf)
	{
		case 'X':
		{
			/* Advance the index and check for overflow. */
			if (++error_idx >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Point at the "info" */
			*extra = ptr = (init_macro_type*)head->info_ptr + error_idx;

			/* Store the name */
			if (!(ptr->name = add_name(head, s)))
				return PARSE_ERROR_OUT_OF_MEMORY;

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
			else if (ISDIGIT(*s))
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
					"d_dun",
					"d_town",
					"d_quest",
					"s_info",
					"template",
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
		{
			return PARSE_ERROR_GENERIC;
		}
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
		case EVENT_HEAD: case Q_HEAD:
		{
			/* Find the monster name array. */
			find_monster_race(0);
			break;
		}
		case R_HEAD:
		{
			/* Ensure that various indices mentioned in the game exist. */

			/* get_rnd_q_monster */
			int max = MON_MEPHISTOPHELES_LORD_OF_HELL-1;

			/* do_cmd_suicide */
			max = MAX(max, MON_SUICIDE);

			/* take_hit */
			max = MAX(max, MON_FATAL_POLYMORPH);

			/* place_ghost */
			max = MAX(max, MON_PLAYER_GHOST);

			/* Paranoia - check that there's space for the required monsters. */
			if (max >= MAX_I) return PARSE_ERROR_OUT_OF_MEMORY;

			/* Check that r_info is as big as the game expects. */
			if (error_idx < max) error_idx = max;

			break;
		}
		case F_HEAD:
		{
			feature_type *finfo = head->info_ptr;
			int i;

			/* Check that the array is large enough for the game. */
			if (error_idx < LAST_FEAT) error_idx = LAST_FEAT;

			for (i = 0; i <= error_idx; i++)
			{
				/* Avoid out of bounds features, now the size is known. */
				if (finfo[i].mimic > error_idx)
				{
					msg_format("Feature %d mimics a non-existent feature.");
					return PARSE_ERROR_OUT_OF_BOUNDS;
				}
				/* Forbid features which mimic mimics as pointless
				 * obfuscation. */
				if (finfo[finfo[i].mimic].mimic != finfo[i].mimic)
				{
					msg_format("Feature %d cannot be a mimic, "
						"as feature %d already mimics it.", finfo[i].mimic, i);
					return PARSE_ERROR_GENERIC;
				}
			}
			break;
		}
	}
	return SUCCESS;
}

/* Define how many macros are available for this file. */
static int PURE num_macros(const header *head)
{
	switch (head->header_num)
	{
		case Z_HEAD: return 0;
		case MACRO_HEAD: return error_idx;
		default: return z_info->macros;
	}
}

#define NUM_MACROS num_macros(head)


#define NO_VERSION -2

static errr parse_info_line_aux(char *buf, header *head, vptr *extra)
{
	/* Then parse the resulting string. */

	/* Skip comments and blank lines */
	if (!buf[0] || (buf[0] == '#')) return SUCCESS;

	/* Verify correct "colon" format */
	if (buf[1] != ':') return PARSE_ERROR_GENERIC;


	/* Hack -- Process 'V' for "Version" */
	if (buf[0] == 'V')
	{
		/* Scan for the values */
		if (!streq(buf+2, GAME_VERSION))

		{
			return PARSE_ERROR_OBSOLETE_FILE;
		}

		/* Okay to proceed */
		error_idx = -1;

		/* Finished. */
		return SUCCESS;
	}

	/* No version yet */
	if (error_idx == NO_VERSION) return PARSE_ERROR_OBSOLETE_FILE;

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
static errr parse_info_line(char *buf, header *head, int initmacro, vptr *extra)
{
	char buf2[1024];
	char *buf2end = buf2+1023;
	int i;

	WIPE(buf2, buf2);

	for (i = initmacro-1; i >= 0; i--)
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
				FREE(this_buf);
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
	error_idx = NO_VERSION;

	/* Prepare the "fake" stuff */
	head->name_size = 0;
	head->text_size = 0;

	/* Carry out any pre-initialisation stuff. */
	try(init_info_txt_pre(head));

	/* Parse */
	for (error_line = 1; !my_fgets(fp, buf, 1024); error_line++)
	{
		try(parse_info_line(buf, head, NUM_MACROS, &extra));
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
	if (error_idx == NO_VERSION) return PARSE_ERROR_OBSOLETE_FILE;

	/* All done. */
	return SUCCESS;;
}

#else /* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif /* ALLOW_TEMPLATES */

