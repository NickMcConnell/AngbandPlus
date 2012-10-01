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
	"XXX5",
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
	"HALLU",
	NULL
};


typedef struct flag_name flag_name;

struct flag_name
{
	cptr name; /* The name of the flag in the text file. */
	int set; /* The set into which the flag is to be sent. */
	u32b flag; /* The flag being set. */
};


#define TR1 0
#define TR2 1
#define TR3 2
#define RF1 3
#define RF2 4
#define RF3 5
#define RF4 6
#define RF5 7
#define RF6 8
#define CF1 9
#define MAX_FLAG_SETS	10



/*
 * Monster race flags for the race_info_flags1 structure
 */
static flag_name info_flags[] =
{

/*
 * Monster race flags 2
 */

	{"UNIQUE", RF1, RF1_UNIQUE},
	{"QUESTOR", RF1, RF1_QUESTOR},
	{"MALE", RF1, RF1_MALE},
	{"FEMALE", RF1, RF1_FEMALE},
	{"CHAR_CLEAR", RF1, RF1_CHAR_CLEAR},
	{"CHAR_MULTI", RF1, RF1_CHAR_MULTI},
	{"ATTR_CLEAR", RF1, RF1_ATTR_CLEAR},
	{"ATTR_MULTI", RF1, RF1_ATTR_MULTI},
	{"FORCE_DEPTH", RF1, RF1_FORCE_DEPTH},
	{"FORCE_MAXHP", RF1, RF1_FORCE_MAXHP},
	{"FORCE_SLEEP", RF1, RF1_FORCE_SLEEP},
	{"FORCE_EXTRA", RF1, RF1_FORCE_EXTRA},
	{"FRIEND", RF1, RF1_FRIEND},
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
	{"DROP_CHEST", RF1, RF1_DROP_CHEST},
	{"DROP_CHOSEN", RF1, RF1_DROP_CHOSEN},

	/*RF1 uber-flags*/
	{"DROP_UP_TO_10", RF1, RF1_DROP_UP_TO_10},
	{"DROP_UP_TO_12", RF1, RF1_DROP_UP_TO_12},
	{"DROP_UP_TO_14", RF1, RF1_DROP_UP_TO_14},
	{"DROP_UP_TO_16", RF1, RF1_DROP_UP_TO_16},
	{"DROP_UP_TO_18", RF1, RF1_DROP_UP_TO_18},
	{"DROP_UP_TO_20", RF1, RF1_DROP_UP_TO_20},


/*
 * Monster race flags 2
 */

	{"STUPID", RF2, RF2_STUPID},
	{"SMART", RF2, RF2_SMART},
	{"HAS_LITE", RF2, RF2_HAS_LITE},
	{"RF2XXX2", RF2, RF2_RF2XXX2},
	{"INVISIBLE", RF2, RF2_INVISIBLE},
	{"COLD_BLOOD", RF2, RF2_COLD_BLOOD},
	{"EMPTY_MIND", RF2, RF2_EMPTY_MIND},
	{"WEIRD_MIND", RF2, RF2_WEIRD_MIND},
	{"MULTIPLY", RF2, RF2_MULTIPLY},
	{"REGENERATE", RF2, RF2_REGENERATE},
	{"RF2XXX3", RF2, RF2_RF2XXX3},
	{"RF2XXX4", RF2, RF2_RF2XXX4},
	{"POWERFUL", RF2, RF2_POWERFUL},
	{"RF2XXX5", RF2, RF2_RF2XXX5},
	{"RF2XXX7", RF2, RF2_RF2XXX7},
	{"RF2XXX6", RF2, RF2_RF2XXX6},
	{"OPEN_DOOR", RF2, RF2_OPEN_DOOR},
	{"BASH_DOOR", RF2, RF2_BASH_DOOR},
	{"PASS_WALL", RF2, RF2_PASS_WALL},
	{"KILL_WALL", RF2, RF2_KILL_WALL},
	{"MOVE_BODY", RF2, RF2_MOVE_BODY},
	{"KILL_BODY", RF2, RF2_KILL_BODY},
	{"TAKE_ITEM", RF2, RF2_TAKE_ITEM},
	{"KILL_ITEM", RF2, RF2_KILL_ITEM},
	{"BRAIN_1", RF2, RF2_BRAIN_1},
	{"BRAIN_2", RF2, RF2_BRAIN_2},
	{"BRAIN_3", RF2, RF2_BRAIN_3},
	{"BRAIN_4", RF2, RF2_BRAIN_4},
	{"BRAIN_5", RF2, RF2_BRAIN_5},
	{"BRAIN_6", RF2, RF2_BRAIN_6},
	{"BRAIN_7", RF2, RF2_BRAIN_7},
	{"BRAIN_8", RF2, RF2_BRAIN_8},


/*
 * Monster race flags 3
 */

	{"ORC", RF3, RF3_ORC},
	{"TROLL", RF3, RF3_TROLL},
	{"GIANT", RF3, RF3_GIANT},
	{"DRAGON", RF3, RF3_DRAGON},
	{"DEMON", RF3, RF3_DEMON},
	{"UNDEAD", RF3, RF3_UNDEAD},
	{"EVIL", RF3, RF3_EVIL},
	{"ANIMAL", RF3, RF3_ANIMAL},
	{"RF3XXX1", RF3, RF3_RF3XXX1},
	{"RF3XXX2", RF3, RF3_RF3XXX2},
	{"RF3XXX3", RF3, RF3_RF3XXX3},
	{"RF3XXX4", RF3, RF3_RF3XXX4},
	{"HURT_LITE", RF3, RF3_HURT_LITE},
	{"HURT_ROCK", RF3, RF3_HURT_ROCK},
	{"HURT_FIRE", RF3, RF3_HURT_FIRE},
	{"HURT_COLD", RF3, RF3_HURT_COLD},
	{"IM_ACID", RF3, RF3_IM_ACID},
	{"IM_ELEC", RF3, RF3_IM_ELEC},
	{"IM_FIRE", RF3, RF3_IM_FIRE},
	{"IM_COLD", RF3, RF3_IM_COLD},
	{"IM_POIS", RF3, RF3_IM_POIS},
	{"RF3XXX5", RF3, RF3_RF3XXX5},
	{"RES_NETH", RF3, RF3_RES_NETH},
	{"RES_WATE", RF3, RF3_RES_WATE},
	{"RES_PLAS", RF3, RF3_RES_PLAS},
	{"RES_NEXU", RF3, RF3_RES_NEXU},
	{"RES_DISE", RF3, RF3_RES_DISE},
	{"RF3XXX6", RF3, RF3_RF3XXX6},
	{"NO_FEAR", RF3, RF3_NO_FEAR},
	{"NO_STUN", RF3, RF3_NO_STUN},
	{"NO_CONF", RF3, RF3_NO_CONF},
	{"NO_SLEEP", RF3, RF3_NO_SLEEP},

	/*RF3 uber-flags*/
	{"IM_ELEM", RF3, RF3_IM_ELEM},
	{"IM_ALL", RF3, RF3_IM_ALL},
	{"NO_CHARM", RF3, RF3_NO_CHARM},
/*
 * Monster race flags 4
 */


	{"SHRIEK", RF4, RF4_SHRIEK},
	{"RF4XXX2", RF4, RF4_RF4XXX2},
	{"RF4XXX3", RF4, RF4_RF4XXX3},
	{"RF4XXX4", RF4, RF4_RF4XXX4},
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
	{"RF4XXX5", RF4, RF4_RF4XXX5},
	{"RF4XXX6", RF4, RF4_RF4XXX6},
	{"RF4XXX7", RF4, RF4_RF4XXX7},
	{"BOULDER", RF4, RF4_BOULDER},

	/*RF4 uber-flags*/
	{"BR_ELEM", RF4, RF4_BR_ELEM},
	{"BR_POWER", RF4, RF4_BR_POWER},
	{"BR_ALL", RF4, RF4_BR_ALL},

/*
 * Monster race flags 5
 */


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
	{"BO_POIS", RF5, RF5_BO_POIS},
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


/*
 * Monster race flags
 */

	{"HASTE", RF6, RF6_HASTE},
	{"RF6XXX1", RF6, RF6_RF6XXX1},
	{"HEAL", RF6, RF6_HEAL},
	{"RF6XXX2", RF6, RF6_RF6XXX2},
	{"BLINK", RF6, RF6_BLINK},
	{"TPORT", RF6, RF6_TPORT},
	{"RF6XXX3", RF6, RF6_RF6XXX3},
	{"RF6XXX4", RF6, RF6_RF6XXX4},
	{"TELE_TO", RF6, RF6_TELE_TO},
	{"TELE_AWAY", RF6, RF6_TELE_AWAY},
	{"TELE_LEVEL", RF6, RF6_TELE_LEVEL},
	{"RF6XXX5", RF6, RF6_RF6XXX5},
	{"DARKNESS", RF6, RF6_DARKNESS},
	{"TRAPS", RF6, RF6_TRAPS},
	{"FORGET", RF6, RF6_FORGET},
	{"RF6XXX6", RF6, RF6_RF6XXX6},
	{"S_KIN", RF6, RF6_S_KIN},
	{"S_HI_DEMON", RF6, RF6_S_HI_DEMON},
	{"S_MONSTER", RF6, RF6_S_MONSTER},
	{"S_MONSTERS", RF6, RF6_S_MONSTERS},
	{"S_ANIMAL", RF6, RF6_S_ANIMAL},
	{"S_SPIDER", RF6, RF6_S_SPIDER},
	{"S_HOUND", RF6, RF6_S_HOUND},
	{"S_HYDRA", RF6, RF6_S_HYDRA},
	{"S_AINUR", RF6, RF6_S_AINUR},
	{"S_DEMON", RF6, RF6_S_DEMON},
	{"S_UNDEAD", RF6, RF6_S_UNDEAD},
	{"S_DRAGON", RF6, RF6_S_DRAGON},
	{"S_HI_UNDEAD", RF6, RF6_S_HI_UNDEAD},
	{"S_HI_DRAGON", RF6, RF6_S_HI_DRAGON},
	{"S_WRAITH", RF6, RF6_S_WRAITH},
	{"S_UNIQUE", RF6, RF6_S_UNIQUE},

/*
 * Object flags 1
 */

	{"STR", TR1, TR1_STR},
	{"INT", TR1, TR1_INT},
	{"WIS", TR1, TR1_WIS},
	{"DEX", TR1, TR1_DEX},
	{"CON", TR1, TR1_CON},
	{"CHR", TR1, TR1_CHR},
	{"TR1XXX1",	TR1, TR1_TR1XXX1},
	{"TR1XXX2",	TR1, TR1_TR1XXX2},
	{"STEALTH", TR1, TR1_STEALTH},
	{"SEARCH", TR1, TR1_SEARCH},
	{"INFRA", TR1, TR1_INFRA},
	{"TUNNEL", TR1, TR1_TUNNEL},
	{"SPEED", TR1, TR1_SPEED},
	{"BLOWS", TR1, TR1_BLOWS},
	{"SHOTS", TR1, TR1_SHOTS},
	{"MIGHT", TR1, TR1_MIGHT},
	{"SLAY_ANIMAL", TR1, TR1_SLAY_ANIMAL},
	{"SLAY_EVIL", TR1, TR1_SLAY_EVIL},
	{"SLAY_UNDEAD", TR1, TR1_SLAY_UNDEAD},
	{"SLAY_DEMON", TR1, TR1_SLAY_DEMON},
	{"SLAY_ORC", TR1, TR1_SLAY_ORC},
	{"SLAY_TROLL", TR1, TR1_SLAY_TROLL},
	{"SLAY_GIANT", TR1, TR1_SLAY_GIANT},
	{"SLAY_DRAGON", TR1, TR1_SLAY_DRAGON},
	{"KILL_DRAGON", TR1, TR1_KILL_DRAGON},
	{"KILL_DEMON", TR1, TR1_KILL_DEMON},
	{"KILL_UNDEAD", TR1, TR1_KILL_UNDEAD},
	{"BRAND_POIS", TR1, TR1_BRAND_POIS},
	{"BRAND_ACID", TR1, TR1_BRAND_ACID},
	{"BRAND_ELEC", TR1, TR1_BRAND_ELEC},
	{"BRAND_FIRE", TR1, TR1_BRAND_FIRE},
	{"BRAND_COLD", TR1, TR1_BRAND_COLD},

	/*TR1 Uber-flags*/
	{"ALL_STATS", TR1, TR1_ALL_STATS},

/*
 * Object flags 2
 */

	{"SUST_STR", TR2, TR2_SUST_STR},
	{"SUST_INT", TR2, TR2_SUST_INT},
	{"SUST_WIS", TR2, TR2_SUST_WIS},
	{"SUST_DEX", TR2, TR2_SUST_DEX},
	{"SUST_CON", TR2, TR2_SUST_CON},
	{"SUST_CHR", TR2, TR2_SUST_CHR},
	{"TR2XXX1", TR2, TR2_TR2XXX1},
	{"TR2XXX2", TR2, TR2_TR2XXX2},
	{"TR2XXX3", TR2, TR2_TR2XXX3},
	{"TR2XXX4", TR2, TR2_TR2XXX4},
	{"TR2XXX5", TR2, TR2_TR2XXX5},
	{"TR2XXX6", TR2, TR2_TR2XXX6},
	{"IM_ACID", TR2, TR2_IM_ACID},
	{"IM_ELEC", TR2, TR2_IM_ELEC},
	{"IM_FIRE", TR2, TR2_IM_FIRE},
	{"IM_COLD", TR2, TR2_IM_COLD},
	{"RES_ACID", TR2, TR2_RES_ACID},
	{"RES_ELEC", TR2, TR2_RES_ELEC},
	{"RES_FIRE", TR2, TR2_RES_FIRE},
	{"RES_COLD", TR2, TR2_RES_COLD},
	{"RES_POIS", TR2, TR2_RES_POIS},
	{"RES_FEAR", TR2, TR2_RES_FEAR},
	{"RES_LITE", TR2, TR2_RES_LITE},
	{"RES_DARK", TR2, TR2_RES_DARK},
	{"RES_BLIND", TR2, TR2_RES_BLIND},
	{"RES_CONFU", TR2, TR2_RES_CONFU},
	{"RES_SOUND", TR2, TR2_RES_SOUND},
	{"RES_SHARD", TR2, TR2_RES_SHARD},
	{"RES_NEXUS", TR2, TR2_RES_NEXUS},
	{"RES_NETHR", TR2, TR2_RES_NETHR},
	{"RES_CHAOS", TR2, TR2_RES_CHAOS},
	{"RES_DISEN", TR2, TR2_RES_DISEN},

	/*TR2 Uber-flags*/
	{"SUST_STATS", TR2, TR2_SUST_STATS},
	{"RESISTANCE", TR2, TR2_RESISTANCE},

/*
 * Object flags 3
 */

	{"SLOW_DIGEST", TR3, TR3_SLOW_DIGEST},
	{"FEATHER", TR3, TR3_FEATHER},
	{"LITE", TR3, TR3_LITE},
	{"REGEN", TR3, TR3_REGEN},
	{"TELEPATHY", TR3, TR3_TELEPATHY},
	{"SEE_INVIS", TR3, TR3_SEE_INVIS},
	{"FREE_ACT", TR3, TR3_FREE_ACT},
	{"HOLD_LIFE", TR3, TR3_HOLD_LIFE},
	{"NEVER_PICKUP", TR3, TR3_NEVER_PICKUP},
	{"TR3XXX2", TR3, TR3_TR3XXX2},
	{"TR3XXX3", TR3, TR3_TR3XXX3},
	{"TR3XXX4", TR3, TR3_TR3XXX4},
	{"IMPACT", TR3, TR3_IMPACT},
	{"TELEPORT", TR3, TR3_TELEPORT},
	{"AGGRAVATE", TR3, TR3_AGGRAVATE},
	{"DRAIN_EXP", TR3, TR3_DRAIN_EXP},
	{"IGNORE_ACID", TR3, TR3_IGNORE_ACID},
	{"IGNORE_ELEC", TR3, TR3_IGNORE_ELEC},
	{"IGNORE_FIRE", TR3, TR3_IGNORE_FIRE},
	{"IGNORE_COLD", TR3, TR3_IGNORE_COLD},
	{"THROWING", TR3, TR3_THROWING},
	{"PERFECT_BALANCE", TR3, TR3_PERFECT_BALANCE},
	{"BLESSED", TR3, TR3_BLESSED},
	{"ACTIVATE", TR3, TR3_ACTIVATE},
	{"INSTA_ART", TR3, TR3_INSTA_ART},
	{"EASY_KNOW", TR3, TR3_EASY_KNOW},
	{"HIDE_TYPE", TR3, TR3_HIDE_TYPE},
	{"SHOW_MODS", TR3, TR3_SHOW_MODS},
	{"TR3XXX7", TR3, TR3_TR3XXX7},
	{"LIGHT_CURSE", TR3, TR3_LIGHT_CURSE},
	{"HEAVY_CURSE", TR3, TR3_HEAVY_CURSE},
	{"PERMA_CURSE",  TR3, TR3_PERMA_CURSE},

	{"IGNORE_ALL",  TR3, TR3_IGNORE_ALL},

/*
 * Class flags
 */

	{"EXTRA_SHOT",  CF1, CF_EXTRA_SHOT},
	{"BRAVERY_30",  CF1, CF_BRAVERY_30},
	{"BLESS_WEAPON",  CF1, CF_BLESS_WEAPON},
	{"CUMBER_GLOVE",  CF1, CF_CUMBER_GLOVE},
	{"ZERO_FAIL",  CF1, CF_ZERO_FAIL},
	{"BEAM",  CF1, CF_BEAM},
	{"CHOOSE_SPELLS",  CF1, CF_CHOOSE_SPELLS},
	{"PSEUDO_ID_HEAVY",  CF1, CF_PSEUDO_ID_HEAVY},
	{"PSEUDO_ID_IMPROV",  CF1, CF_PSEUDO_ID_IMPROV},
	{"ROGUE_COMBAT",  CF1, CF_ROGUE_COMBAT},
	{"EXTRA_ARROW",  CF1, CF_EXTRA_ARROW},
	{"SET_TRAPS",  CF1, CF_SET_TRAPS},
	{"EXTRA_ATTACK",  CF1, CF_EXTRA_ATTACK},
	{"CFXXX14",  CF1, CF_CFXXX14},
	{"CFXXX15",  CF1, CF_CFXXX15},
	{"CFXXX16",  CF1, CF_CFXXX16},
	{"CFXXX17",  CF1, CF_CFXXX17},
	{"CFXXX18",  CF1, CF_CFXXX18},
	{"CFXXX19",  CF1, CF_CFXXX19},
	{"CFXXX20",  CF1, CF_CFXXX20},
	{"CFXXX21",  CF1, CF_CFXXX21},
	{"CFXXX22",  CF1, CF_CFXXX22},
	{"CFXXX23",  CF1, CF_CFXXX23},
	{"CFXXX24",  CF1, CF_CFXXX24},
	{"CFXXX21",  CF1, CF_CFXXX25},
	{"CFXXX25",  CF1, CF_CFXXX26},
	{"CFXXX26",  CF1, CF_CFXXX27},
	{"CFXXX27",  CF1, CF_CFXXX28},
	{"CFXXX28",  CF1, CF_CFXXX29},
	{"CFXXX29",  CF1, CF_CFXXX29},
	{"CFXXX30",  CF1, CF_CFXXX30},
	{"CFXXX31",  CF1, CF_CFXXX31},
	{"CFXXX32",  CF1, CF_CFXXX32}
};






/*
 * Activation type
 */
static cptr a_info_act[ACT_MAX] =
{
	"ILLUMINATION",
	"MAGIC_MAP",
	"CLAIRVOYANCE",
	"PROT_EVIL",
	"DISP_EVIL",
	"HEAL1",
	"HEAL2",
	"CURE_WOUNDS",
	"HASTE1",
	"HASTE2",
	"FIRE1",
	"FIRE2",
	"FIRE3",
	"FROST1",
	"FROST2",
	"FROST3",
	"FROST4",
	"FROST5",
	"ACID1",
	"RECHARGE1",
	"SLEEP",
	"LIGHTNING_BOLT",
	"ELEC2",
	"BANISHMENT",
	"MASS_BANISHMENT",
	"IDENTIFY",
	"DRAIN_LIFE1",
	"DRAIN_LIFE2",
	"BIZZARE",
	"STAR_BALL",
	"RAGE_BLESS_RESIST",
	"PHASE",
	"TRAP_DOOR_DEST",
	"DETECT",
	"RESIST",
	"TELEPORT",
	"RESTORE_LIFE",
	"MISSILE",
	"ARROW",
	"REM_FEAR_POIS",
	"STINKING_CLOUD",
	"STONE_TO_MUD",
	"TELE_AWAY",
	"WOR",
	"CONFUSE",
	"PROBE",
	"FIREBRAND",
	"STARLIGHT",
	"MANA_BOLT",
	"BERSERKER"
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
	maxima *z_info = head->info_ptr;

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

	/* Process 'L' for "Maximum flavor_info[] subindex" */
	else if (buf[2] == 'L')
	{
		int max;

		/* Scan for the value */
		if (1 != sscanf(buf+4, "%d", &max)) return (PARSE_ERROR_GENERIC);

		/* Save the value */
		z_info->flavor_max = max;
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

	/* Process 'M' for "Maximum mon_list[] index" */
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
		v_ptr = (vault_type*)head->info_ptr + i;

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

		/* Check for maximum vault sizes */
		if ((v_ptr->typ == 7) && ((v_ptr->wid > 33) || (v_ptr->hgt > 22)))
			return (PARSE_ERROR_VAULT_TOO_BIG);

		if ((v_ptr->typ == 8) && ((v_ptr->wid > 66) || (v_ptr->hgt > 44)))
			return (PARSE_ERROR_VAULT_TOO_BIG);
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
		f_ptr = (feature_type*)head->info_ptr + i;

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
		char d_char;
		int d_attr;

		/* There better be a current f_ptr */
		if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract d_char */
		d_char = buf[2];

		/* If we have a longer string than expected ... */
		if (buf[5])
		{
			/* Advance "buf" on by 4 */
			buf += 4;

			/* Extract the colour */
			d_attr = color_text_to_attr(buf);
		}
		else
		{
			/* Extract the attr */
			d_attr = color_char_to_attr(buf[4]);
		}

		/* Paranoia */
		if (d_attr < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		f_ptr->d_attr = d_attr;
		f_ptr->d_char = d_char;
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
			return 0;
		}
	}

	/* Oops */
	msg_format("Unknown %s flag '%s'.", errstr, what);

	/* Error */
	return (-1);
}


/*
 * Grab one flag in an object_kind from a textual string
 */
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
		k_ptr = (object_kind*)head->info_ptr + i;

		/* Store the name */
		if (!(k_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'G' for "Graphics" (one line only) */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current k_ptr */
		if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract d_char */
		d_char = buf[2];

		/* If we have a longer string than expected ... */
		if (buf[5])
		{
			/* Advance "buf" on by 4 */
			buf += 4;

			/* Extract the colour */
			d_attr = color_text_to_attr(buf);
		}
		else
		{
			/* Extract the attr */
			d_attr = color_char_to_attr(buf[4]);
		}

		/* Paranoia */
		if (d_attr < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		k_ptr->d_attr = d_attr;
		k_ptr->d_char = d_char;
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
 * Grab one activation from a textual string
 */
static errr grab_one_activation(artifact_type *a_ptr, cptr what)
{
	int i;

	/* Scan activations */
	for (i = 0; i < ACT_MAX; i++)
	{
		if (streq(what, a_info_act[i]))
		{
			a_ptr->activation = i;
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown artifact activation '%s'.", what);

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
		a_ptr = (artifact_type*)head->info_ptr + i;

		/* Store the name */
		if (!(a_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);

		/* Ignore everything */
		a_ptr->flags3 |= (TR3_IGNORE_MASK);
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int tval, sval, pval;

		/* There better be a current a_ptr */
		if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (3 != sscanf(buf+2, "%d:%d:%d",
			            &tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

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

	/* Process 'A' for "Activation & time" */
	else if (buf[0] == 'A')
	{
		int ptime, prand;

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
		if (grab_one_activation(a_ptr, buf + 2)) return (PARSE_ERROR_GENERIC);

		/* Scan for the values */
		if (2 != sscanf(s, "%d:%d",
			            &ptime, &prand)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		a_ptr->time = ptime;
		a_ptr->randtime = prand;
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
static bool grab_one_ego_item_flag(ego_item_type *ptr, cptr what)
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
		e_ptr = (ego_item_type*)head->info_ptr + i;

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
		int rating, xtra;

		/* There better be a current e_ptr */
		if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (2 != sscanf(buf+2, "%d:%d", &rating, &xtra))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
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

		/* Increase counter for 'possible tval' index */
		cur_t++;

		/* Allow only a limited number of T: lines */
		if (cur_t > EGO_TVALS_MAX) return (PARSE_ERROR_GENERIC);
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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_flag(monster_race *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[RF1] = &(ptr->flags1);
	f[RF2] = &(ptr->flags2);
	f[RF3] = &(ptr->flags3);
	return grab_one_flag(f, "monster", what);
}



/*
 * Grab one (spell) flag in a monster_race from a textual string
 */
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
		r_ptr = (monster_race*)head->info_ptr + i;

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
		char d_char;
		int d_attr;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract d_char */
		d_char = buf[2];

		/* If we have a longer string than expected ... */
		if (buf[5])
		{
			/* Advance "buf" on by 4 */
			buf += 4;

			/* Extract the colour */
			d_attr = color_text_to_attr(buf);
		}
		else
		{
			/* Extract the attr */
			d_attr = color_char_to_attr(buf[4]);
		}

		/* Paranoia */
		if (d_attr < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		r_ptr->d_attr = d_attr;
		r_ptr->d_char = d_char;
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
		r_ptr->extra = pad;
		r_ptr->mexp = exp;
	}

	/* Process 'B' for "Blows" */
	else if (buf[0] == 'B')
	{
		int n1, n2;

		/* There better be a current r_ptr */
		if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Find the next empty blow slot (if any) */
		for (i = 0; i < MONSTER_BLOW_MAX; i++) if (!r_ptr->blow[i].method) break;

		/* Oops, no more slots */
		if (i == MONSTER_BLOW_MAX) return (PARSE_ERROR_GENERIC);

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
			if (1 == sscanf(s, "1_IN_%d", &i))
			{
				/* Sanity check */
				if ((i < 1) || (i > 100))
					return (PARSE_ERROR_INVALID_SPELL_FREQ);

				/* Extract a "frequency" */
				r_ptr->freq_spell = r_ptr->freq_innate = 100 / i;

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

#if 0
	/* XXX XXX XXX The ghost is unused */

	/* Mega-Hack -- acquire "ghost" */
	r_ptr = &r_info[z_info->r_max-1];

	/* Get the next index */
	r_ptr->name = head->name_size;
	r_ptr->text = head->text_size;

	/* Save some space for the ghost info */
	head->name_size += 64;
	head->text_size += 64;

	/* Hack -- Default name/text for the ghost */
	strcpy(r_name + r_ptr->name, "Nobody, the Undefined Ghost");
	strcpy(r_text + r_ptr->text, "It seems strangely familiar...");

	/* Hack -- set the attr/char info */
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
#endif

	/* Success */
	return (0);
}


/*
 * Grab one flag in a player_race from a textual string
 */
static errr grab_one_racial_flag(player_race *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[TR1] = &(ptr->flags1);
	f[TR2] = &(ptr->flags2);
	f[TR3] = &(ptr->flags3);
	return grab_one_flag(f, "player", what);
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
		pr_ptr = (player_race*)head->info_ptr + i;

		/* Store the name */
		if (!(pr_ptr->name = add_name(head, s)))
			return (PARSE_ERROR_OUT_OF_MEMORY);
	}

	/* Process 'S' for "Stats" (one line only) */
	else if (buf[0] == 'S')
	{
		int adj;

		/* There better be a current pr_ptr */
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
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pr_ptr */
		if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pr_ptr->r_dis = dis;
		pr_ptr->r_dev = dev;
		pr_ptr->r_sav = sav;
		pr_ptr->r_stl = stl;
		pr_ptr->r_srh = srh;
		pr_ptr->r_fos = fos;
		pr_ptr->r_thn = thn;
		pr_ptr->r_thb = thb;
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
static errr grab_one_class_flag(player_class *ptr, cptr what)
{
	u32b *f[MAX_FLAG_SETS];
	C_WIPE(f, MAX_FLAG_SETS, sizeof(u32b*));
	f[CF1] = &(ptr->flags);
	return grab_one_flag(f, "player", what);
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
		pc_ptr = (player_class*)head->info_ptr + i;

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
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_dis = dis;
		pc_ptr->c_dev = dev;
		pc_ptr->c_sav = sav;
		pc_ptr->c_stl = stl;
		pc_ptr->c_srh = srh;
		pc_ptr->c_fos = fos;
		pc_ptr->c_thn = thn;
		pc_ptr->c_thb = thb;
	}

	/* Process 'X' for "Extra Skills" (one line only) */
	else if (buf[0] == 'X')
	{
		int dis, dev, sav, stl, srh, fos, thn, thb;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
			            &dis, &dev, &sav, &stl,
			            &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->x_dis = dis;
		pc_ptr->x_dev = dev;
		pc_ptr->x_sav = sav;
		pc_ptr->x_stl = stl;
		pc_ptr->x_srh = srh;
		pc_ptr->x_fos = fos;
		pc_ptr->x_thn = thn;
		pc_ptr->x_thb = thb;
	}

	/* Process 'I' for "Info" (one line only) */
	else if (buf[0] == 'I')
	{
		int mhp, exp, sense_div;
		long sense_base;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%ld:%d",
			            &mhp, &exp, &sense_base, &sense_div))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->c_mhp = mhp;
		pc_ptr->c_exp = exp;
		pc_ptr->sense_base = sense_base;
		pc_ptr->sense_div = sense_div;
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
		int spell_book, spell_stat, spell_first, spell_weight;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
		                &spell_book, &spell_stat,
		                &spell_first, &spell_weight))
			return (PARSE_ERROR_GENERIC);

		/* Save the values */
		pc_ptr->spell_book = spell_book;
		pc_ptr->spell_stat = spell_stat;
		pc_ptr->spell_first = spell_first;
		pc_ptr->spell_weight = spell_weight;
	}

	/* Process 'B' for "Spell/Prayer book info" */
	else if (buf[0] == 'B')
	{
		int spell, level, mana, fail, exp;
		player_magic *mp_ptr;
		magic_type *spell_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Scan for the values */
		if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
		                &spell, &level, &mana, &fail, &exp))
			return (PARSE_ERROR_GENERIC);

		/* Validate the spell index */
		if ((spell >= PY_MAX_SPELLS) || (spell < 0))
			return (PARSE_ERROR_OUT_OF_BOUNDS);

		mp_ptr = &pc_ptr->spells;
		spell_ptr = &mp_ptr->info[spell];

		/* Save the values */
		spell_ptr->slevel = level;
		spell_ptr->smana = mana;
		spell_ptr->sfail = fail;
		spell_ptr->sexp = exp;
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

	/* Process 'E' for "Starting Equipment" */
	else if (buf[0] == 'E')
	{
		int tval, sval, min, max;

		start_item *e_ptr;

		/* There better be a current pc_ptr */
		if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Access the item */
		e_ptr = &pc_ptr->start_items[cur_equip];

		/* Scan for the values */
		if (4 != sscanf(buf+2, "%d:%d:%d:%d",
			            &tval, &sval, &min, &max)) return (PARSE_ERROR_GENERIC);

		if ((min < 0) || (max < 0) || (min > 99) || (max > 99))
			return (PARSE_ERROR_INVALID_ITEM_NUMBER);

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
		h_ptr = (hist_type*)head->info_ptr + i;

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
		ot_ptr = (owner_type*)head->info_ptr + i;

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
			g_ptr = (byte*)head->info_ptr + i;

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

		q_ptr->mon_idx = r_idx;

		if(r_idx)
		{
			monster_race *r_ptr = &r_info[r_idx];

			/*unique quest*/
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				q_ptr->type = QUEST_FIXED_U;
			}

			/*fixed quest*/
			else q_ptr->type = QUEST_FIXED;

			q_ptr->max_num = max;
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
 * Initialize the "flavor_info" array, by parsing an ascii "template" file
 */
errr parse_flavor_info(char *buf, header *head)
{
	int i;

	/* Current entry */
	static flavor_type *flavor_ptr;


	/* Process 'N' for "Number" */
	if (buf[0] == 'N')
	{
		int tval, sval;
		int result;

		/* Scan the value */
		result = sscanf(buf, "N:%d:%d:%d", &i, &tval, &sval);

		/* Either two or three values */
		if ((result != 2) && (result != 3)) return (PARSE_ERROR_GENERIC);

		/* Verify information */
		if (i <= error_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

		/* Verify information */
		if (i >= head->info_num) return (PARSE_ERROR_TOO_MANY_ENTRIES);

		/* Save the index */
		error_idx = i;

		/* Point at the "info" */
		flavor_ptr = (flavor_type*)head->info_ptr + i;

		/* Save the tval */
		flavor_ptr->tval = (byte)tval;

		/* Save the sval */
		if (result == 2)
		{
			/* Megahack - unknown sval */
			flavor_ptr->sval = SV_UNKNOWN;
		}
		else
			flavor_ptr->sval = (byte)sval;
	}

	/* Process 'G' for "Graphics" */
	else if (buf[0] == 'G')
	{
		char d_char;
		int d_attr;

		/* There better be a current flavor_ptr */
		if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[2]) return (PARSE_ERROR_GENERIC);
		if (!buf[3]) return (PARSE_ERROR_GENERIC);
		if (!buf[4]) return (PARSE_ERROR_GENERIC);

		/* Extract d_char */
		d_char = buf[2];

		/* If we have a longer string than expected ... */
		if (buf[5])
		{
			/* Advance "buf" on by 4 */
			buf += 4;

			/* Extract the colour */
			d_attr = color_text_to_attr(buf);
		}
		else
		{
			/* Extract the attr */
			d_attr = color_char_to_attr(buf[4]);
		}

		/* Paranoia */
		if (d_attr < 0) return (PARSE_ERROR_GENERIC);

		/* Save the values */
		flavor_ptr->d_attr = d_attr;
		flavor_ptr->d_char = d_char;
	}

	/* Process 'D' for "Description" */
	else if (buf[0] == 'D')
	{
		/* There better be a current flavor_ptr */
		if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

		/* Paranoia */
		if (!buf[1]) return (PARSE_ERROR_GENERIC);
		if (!buf[2]) return (PARSE_ERROR_GENERIC);

		/* Store the text */
		if (!add_text(&flavor_ptr->text, head, buf + 2))
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


#else	/* ALLOW_TEMPLATES */

#ifdef MACINTOSH
static int i = 0;
#endif

#endif	/* ALLOW_TEMPLATES */
