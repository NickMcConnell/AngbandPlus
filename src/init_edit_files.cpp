#include "src/init.h"

/* File: was init1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/user_macros.h"


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
 * The code could actually be removed and placed into a "stand-alone"
 * program, but that feels a little silly, especially considering some
 * of the platforms that we currently support.
 */




/*** Helper arrays for parsing ascii template files ***/

#define BLOW_METHOD_MAX     26

/*
 * Monster Blow Methods
 */
static QString r_info_blow_method[BLOW_METHOD_MAX] =
{
    "",
    "HIT",
    "TOUCH",
    "PUNCH",
    "KICK",
    "CLAW",
    "BITE",
    "STING",
    "PECK",
    "BREATHE",
    "BUTT",
    "CRUSH",
    "ENGULF",
    "CRAWL",
    "DROOL",
    "SPIT",
    "SLIME",
    "GAZE",
    "WAIL",
    "SPORE",
    "TRAMPLE",
    "BEG",
    "INSULT",
    "XXX5",
    "XXX6",
    NULL
};

#define BLOW_EFFECT_MAX  36

/*
 * Monster Blow Effects
 */
static QString r_info_blow_effect[BLOW_EFFECT_MAX] =
{
    "",
    "HURT",
    "WOUND",
    "BATTER",
    "SHATTER",
    "UN_BONUS",
    "UN_POWER",
    "LOSE_MANA",
    "EAT_GOLD",
    "EAT_ITEM",
    "EAT_FOOD",
    "EAT_LIGHT",
    "HUNGER",
    "POISON",
    "ACID",
    "ELEC",
    "FIRE",
    "COLD",
    "BLIND",
    "CONFUSE",
    "TERRIFY",
    "PARALYZE",
    "HALLU",
    "DISEASE",
    "LOSE_STR",
    "LOSE_INT",
    "LOSE_WIS",
    "LOSE_DEX",
    "LOSE_CON",
    "LOSE_CHR",
    "LOSE_ALL",
    "EXP_10",
    "EXP_20",
    "EXP_40",
    "EXP_80",
    NULL
};

enum num_flag_sets
{
    SF1 = 1,
    TR1,
    TR2,
    TR3,
    TN1,
    CF1,
    RN1,
    RF1,
    RF2,
    RF3,
    RF4,
    RF5,
    RF6,
    RF7,
    FF1,
    FF2,
    FF3,
    MAX_FLAG_SETS
};

typedef struct flag_name flag_name;


struct flag_name
{
    QString name; /* The name of the flag in the text file. */
    num_flag_sets set; /* The set into which the flag is to be sent. */
    u32b flag; /* The flag being set. */
};






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
    {"CHAR_MIMIC", RF1, RF1_CHAR_MIMIC},
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
    {"HAS_LIGHT", RF2, RF2_HAS_LIGHT},
    {"RF2XXX2", RF2, RF2_RF2XXX2},
    {"INVISIBLE", RF2, RF2_INVISIBLE},
    {"COLD_BLOOD", RF2, RF2_COLD_BLOOD},
    {"EMPTY_MIND", RF2, RF2_EMPTY_MIND},
    {"WEIRD_MIND", RF2, RF2_WEIRD_MIND},
    {"MULTIPLY", RF2, RF2_MULTIPLY},
    {"REGENERATE", RF2, RF2_REGENERATE},
    {"SPECIAL", RF2, RF2_SPECIAL},
    {"EVASIVE", RF2, RF2_EVASIVE},
    {"CLOUD_SURROUND", RF2, RF2_CLOUD_SURROUND},
    {"RF2XXX5", RF2, RF2_RF2XXX5},
    {"PLAYER_GHOST", RF2, RF2_PLAYER_GHOST},
    {"STAY_NATIVE", RF2, RF2_STAY_NATIVE},
    {"OPEN_DOOR", RF2, RF2_OPEN_DOOR},
    {"BASH_DOOR", RF2, RF2_BASH_DOOR},
    {"PASS_WALL", RF2, RF2_PASS_WALL},
    {"KILL_WALL", RF2, RF2_KILL_WALL},
    {"RF2XXX8", RF2, RF2_RF2XXX8},
    {"KILL_BODY", RF2, RF2_KILL_BODY},
    {"TAKE_ITEM", RF2, RF2_TAKE_ITEM},
    {"KILL_ITEM", RF2, RF2_KILL_ITEM},
    {"BRAIN_1", RF2, RF2_BRAIN_1},
    {"LOW_MANA_RUN", RF2, RF2_LOW_MANA_RUN},
    {"BRAIN_2", RF2, RF2_BRAIN_2},
    {"POWERFUL", RF2, RF2_POWERFUL},
    {"RF2XXX1", RF2, RF2_RF2XXX1},
    {"RF2XXX9", RF2, RF2_RF2XXX9},
    {"RF2XX10", RF2, RF2_RF2XX10},
    {"BRAIN_3", RF2, RF2_BRAIN_3},



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
    {"FLYING", RF3, RF3_FLYING},
    {"RF3XXX2", RF3, RF3_RF3XXX2},
    {"HURT_POIS", RF3, RF3_HURT_POIS},
    {"HURT_ACID", RF3, RF3_HURT_ACID},
    {"HURT_LIGHT", RF3, RF3_HURT_LIGHT},
    {"HURT_ROCK", RF3, RF3_HURT_ROCK},
    {"HURT_FIRE", RF3, RF3_HURT_FIRE},
    {"HURT_COLD", RF3, RF3_HURT_COLD},
    {"IM_ACID", RF3, RF3_IM_ACID},
    {"IM_ELEC", RF3, RF3_IM_ELEC},
    {"IM_FIRE", RF3, RF3_IM_FIRE},
    {"IM_COLD", RF3, RF3_IM_COLD},
    {"IM_POIS", RF3, RF3_IM_POIS},
    {"RES_CHAOS", RF3, RF3_RES_CHAOS},
    {"RES_NETHR", RF3, RF3_RES_NETHR},
    {"RES_WATER", RF3, RF3_RES_WATER},
    {"RES_PLAS", RF3, RF3_RES_PLAS},
    {"RES_NEXUS", RF3, RF3_RES_NEXUS},
    {"RES_DISEN", RF3, RF3_RES_DISEN},
    {"NO_SLOW", RF3, RF3_NO_SLOW},
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
    {"LASH", RF4, RF4_LASH},
    {"BOULDER", RF4, RF4_BOULDER},
    {"SHOT", RF4, RF4_SHOT},
    {"ARROW", RF4, RF4_ARROW},
    {"BOLT", RF4, RF4_BOLT},
    {"MISSL", RF4, RF4_MISSL},
    {"PMISSL", RF4, RF4_PMISSL},
    {"BRTH_ACID", RF4, RF4_BRTH_ACID},
    {"BRTH_ELEC", RF4, RF4_BRTH_ELEC},
    {"BRTH_FIRE", RF4, RF4_BRTH_FIRE},
    {"BRTH_COLD", RF4, RF4_BRTH_COLD},
    {"BRTH_POIS", RF4, RF4_BRTH_POIS},
    {"BRTH_PLAS", RF4, RF4_BRTH_PLAS},
    {"BRTH_LIGHT", RF4, RF4_BRTH_LIGHT},
    {"BRTH_DARK", RF4, RF4_BRTH_DARK},
    {"BRTH_CONFU", RF4, RF4_BRTH_CONFU},
    {"BRTH_SOUND", RF4, RF4_BRTH_SOUND},
    {"BRTH_SHARD", RF4, RF4_BRTH_SHARD},
    {"BRTH_INER", RF4, RF4_BRTH_INER},
    {"BRTH_GRAV", RF4, RF4_BRTH_GRAV},
    {"RF4_XX1X", RF4, RF4_RF4XX1X},
    {"BRTH_FORCE", RF4, RF4_BRTH_FORCE},
    {"BRTH_NEXUS", RF4, RF4_BRTH_NEXUS},
    {"BRTH_NETHR", RF4, RF4_BRTH_NETHR},
    {"BRTH_CHAOS", RF4, RF4_BRTH_CHAOS},
    {"BRTH_DISEN", RF4, RF4_BRTH_DISEN},
    {"BRTH_TIME", RF4, RF4_BRTH_TIME},
    {"BRTH_MANA", RF4, RF4_BRTH_MANA},
    {"RF4XXX1", RF4, RF4_RF4XXX1},
    {"RF4XXX2", RF4, RF4_RF4XXX2},
    {"RF4XXX3", RF4, RF4_RF4XXX3},

    /*RF4 uber-flags*/
    {"BRTH_ELEM", RF4, RF4_BRTH_ELEM},
    {"BRTH_POWER", RF4, RF4_BRTH_POWER},
    {"BRTH_ALL", RF4, RF4_BRTH_ALL},


/*
 * Monster race flags 5
 */


    {"BALL_ACID", RF5, RF5_BALL_ACID},
    {"BALL_ELEC", RF5, RF5_BALL_ELEC},
    {"BALL_FIRE", RF5, RF5_BALL_FIRE},
    {"BALL_COLD", RF5, RF5_BALL_COLD},
    {"BALL_POIS", RF5, RF5_BALL_POIS},
    {"BALL_LIGHT", RF5, RF5_BALL_LIGHT},
    {"BALL_DARK", RF5, RF5_BALL_DARK},
    {"BALL_CONFU", RF5, RF5_BALL_CONFU},
    {"BALL_SOUND", RF5, RF5_BALL_SOUND},
    {"BALL_SHARD", RF5, RF5_BALL_SHARD},
    {"BALL_METEOR", RF5,RF5_BALL_METEOR},
    {"BALL_STORM", RF5, RF5_BALL_STORM},
    {"BALL_NETHR", RF5, RF5_BALL_NETHR},
    {"BALL_CHAOS", RF5, RF5_BALL_CHAOS},
    {"BALL_MANA", RF5, RF5_BALL_MANA},
    {"BALL_WATER", RF5, RF5_BALL_WATER},
    {"BOLT_ACID", RF5, RF5_BOLT_ACID},
    {"BOLT_ELEC", RF5, RF5_BOLT_ELEC},
    {"BOLT_FIRE", RF5, RF5_BOLT_FIRE},
    {"BOLT_COLD", RF5, RF5_BOLT_COLD},
    {"BOLT_POIS", RF5, RF5_BOLT_POIS},
    {"BOLT_PLAS", RF5, RF5_BOLT_PLAS},
    {"BOLT_ICE", RF5, RF5_BOLT_ICE},
    {"BOLT_WATER", RF5, RF5_BOLT_WATER},
    {"BOLT_NETHR", RF5, RF5_BOLT_NETHR},
    {"BOLT_MANA", RF5, RF5_BOLT_MANA},
    {"BOLT_GRAV", RF5, RF5_BOLT_GRAV},
    {"BEAM_ELEC", RF5, RF5_BEAM_ELEC},
    {"BEAM_ICE", RF5, RF5_BEAM_ICE},
    {"BEAM_NETHR", RF5, RF5_BEAM_NETHR},
    {"BEAM_LAVA", RF5, RF5_BEAM_LAVA},
    {"HOLY_ORB", RF5, RF5_HOLY_ORB},


/*
 * Monster race flags 6
 */

    {"HASTE", RF6, RF6_HASTE},
    {"ADD_MANA", RF6, RF6_ADD_MANA},
    {"HEAL", RF6, RF6_HEAL},
    {"CURE", RF6, RF6_CURE},
    {"BLINK", RF6, RF6_BLINK},
    {"TPORT", RF6, RF6_TPORT},
    {"RF6_XXX1", RF6, RF6_RF6XXX1},
    {"TELE_SELF_TO", RF6, RF6_TELE_SELF_TO},
    {"TELE_TO", RF6, RF6_TELE_TO},
    {"TELE_AWAY", RF6, RF6_TELE_AWAY},
    {"TELE_LEVEL", RF6, RF6_TELE_LEVEL},
    {"RF6_RF6XXX1", RF6, RF6_RF6XXX1},
    {"DARKNESS", RF6, RF6_DARKNESS},
    {"TRAPS", RF6, RF6_TRAPS},
    {"RF6XXX3", RF6, RF6_RF6XXX3},
    {"DRAIN_MANA", RF6, RF6_DRAIN_MANA},
    {"RF6XXX4", RF6, RF6_RF6XXX4},
    {"RF6XXX5", RF6, RF6_RF6XXX5},
    {"MIND_BLAST", RF6, RF6_MIND_BLAST},
    {"BRAIN_SMASH", RF6, RF6_BRAIN_SMASH},
    {"WOUND", RF6, RF6_WOUND},
    {"RF6XXX6", RF6, RF6_RF6XXX6},
    {"RF6XXX7", RF6, RF6_RF6XXX7},
    {"RF6XXX8", RF6, RF6_RF6XXX8},
    {"RF6XXX9", RF6, RF6_RF6XXX9},
    {"HUNGER", RF6, RF6_HUNGER},
    {"RF6XX11", RF6, RF6_RF6XX11},
    {"SCARE", RF6, RF6_SCARE},
    {"BLIND", RF6, RF6_BLIND},
    {"CONF", RF6, RF6_CONF},
    {"SLOW", RF6, RF6_SLOW},
    {"HOLD", RF6, RF6_HOLD},


/*
 * Monster summon flags 7
 */

    {"S_KIN", RF7, RF7_S_KIN},
    {"RF7XXX1", RF7, RF7_RF7XXX1},
    {"RF7XXX2", RF7, RF7_RF7XXX2},
    {"S_MONSTER", RF7, RF7_S_MONSTER},
    {"S_MONSTERS", RF7, RF7_S_MONSTERS},
    {"RF7XXX3", RF7, RF7_RF7XXX3},
    {"RF7XXX4", RF7, RF7_RF7XXX4},
    {"RF7XXX5", RF7, RF7_RF7XXX5},
    {"S_ANT", RF7, RF7_S_ANT},
    {"S_SPIDER", RF7, RF7_S_SPIDER},
    {"S_HOUND", RF7, RF7_S_HOUND},
    {"S_ANIMAL", RF7, RF7_S_ANIMAL},
    {"S_HYDRA", RF7, RF7_S_HYDRA},
    {"RF7XXX7", RF7, RF7_RF7XXX7},
    {"S_THIEF", RF7, RF7_S_THIEF},
    {"S_BERTBILLTOM", RF7, RF7_S_BERTBILLTOM},
    {"RF7XXX7", RF7, RF7_RF7XXX7},
    {"S_AINU", RF7, RF7_S_AINU},
    {"RF7XX10", RF7, RF7_RF7XX10},
    {"RF7XX11", RF7, RF7_RF7XX11},
    {"S_DRAGON", RF7, RF7_S_DRAGON},
    {"S_HI_DRAGON", RF7, RF7_S_HI_DRAGON},
    {"RF7XX12", RF7, RF7_RF7XX12},
    {"RF7XX13", RF7, RF7_RF7XX13},
    {"S_DEMON", RF7, RF7_S_DEMON},
    {"S_HI_DEMON", RF7, RF7_S_HI_DEMON},
    {"RF7XX14", RF7, RF7_RF7XX14},
    {"S_UNIQUE", RF7, RF7_S_UNIQUE},
    {"S_HI_UNIQUE", RF7, RF7_S_HI_UNIQUE},
    {"S_UNDEAD", RF7, RF7_S_UNDEAD},
    {"S_HI_UNDEAD", RF7, RF7_S_HI_UNDEAD},
    {"S_WRAITH", RF7, RF7_S_WRAITH},

/*
 * Monster native flags
 */

    {"N_LAVA", RN1, RN1_N_LAVA},
    {"N_ICE", RN1, RN1_N_ICE},
    {"N_OIL", RN1, RN1_N_OIL},
    {"N_FIRE", RN1, RN1_N_FIRE},
    {"N_SAND", RN1, RN1_N_SAND},
    {"N_FOREST", RN1, RN1_N_FOREST},
    {"N_WATER", RN1, RN1_N_WATER},
    {"N_ACID", RN1, RN1_N_ACID},
    {"N_MUD", RN1, RN1_N_MUD},
    {"RNXX1_2", RN1, RN1_RNXX1_2},
    {"RNXX1_1", RN1, RN1_RNXX1_1},
    {"RNXXX_0", RN1, RN1_RNXXX_0},
    {"RNXXX_1", RN1, RN1_RNXXX_1},
    {"RNXXX_2", RN1, RN1_RNXXX_2},
    {"RNXXX_3", RN1, RN1_RNXXX_3},
    {"RNXXX_4", RN1, RN1_RNXXX_4},

/*object_flags*/

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
    {"BRAND_ACID", TR1, TR1_BRAND_ACID},
    {"BRAND_ELEC", TR1, TR1_BRAND_ELEC},
    {"BRAND_FIRE", TR1, TR1_BRAND_FIRE},
    {"BRAND_COLD", TR1, TR1_BRAND_COLD},
    {"BRAND_POIS", TR1, TR1_BRAND_POIS},

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
    {"IM_ACID", TR2, TR2_IM_ACID},
    {"IM_ELEC", TR2, TR2_IM_ELEC},
    {"IM_FIRE", TR2, TR2_IM_FIRE},
    {"IM_COLD", TR2, TR2_IM_COLD},
    {"IM_POIS", TR2, TR2_IM_POIS},
    {"RES_ACID", TR2, TR2_RES_ACID},
    {"RES_ELEC", TR2, TR2_RES_ELEC},
    {"RES_FIRE", TR2, TR2_RES_FIRE},
    {"RES_COLD", TR2, TR2_RES_COLD},
    {"RES_POIS", TR2, TR2_RES_POIS},
    {"RES_FEAR", TR2, TR2_RES_FEAR},
    {"RES_LIGHT", TR2, TR2_RES_LIGHT},
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
    {"LIGHT", TR3, TR3_LIGHT},
    {"REGEN", TR3, TR3_REGEN},
    {"TELEPATHY", TR3, TR3_TELEPATHY},
    {"SEE_INVIS", TR3, TR3_SEE_INVIS},
    {"FREE_ACT", TR3, TR3_FREE_ACT},
    {"HOLD_LIFE", TR3, TR3_HOLD_LIFE},
    {"NEVER_PICKUP", TR3, TR3_NEVER_PICKUP},
    {"IRONMAN_ONLY", TR3, TR3_IRONMAN_ONLY},
    {"STORE_ONLY", TR3, TR3_STORE_ONLY},
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
 * Native flags 1
 */

    {"NATIVE_LAVA", TN1, TN1_NATIVE_LAVA},
    {"NATIVE_ICE", TN1, TN1_NATIVE_ICE},
    {"NATIVE_OIL", TN1, TN1_NATIVE_OIL},
    {"NATIVE_FIRE", TN1, TN1_NATIVE_FIRE},
    {"NATIVE_SAND", TN1, TN1_NATIVE_SAND},
    {"NATIVE_FOREST", TN1, TN1_NATIVE_FOREST},
    {"NATIVE_WATER", TN1, TN1_NATIVE_WATER},
    {"NATIVE_ACID", TN1, TN1_NATIVE_ACID},
    {"NATIVE_MUD", TN1, TN1_NATIVE_MUD},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},
    {"NATIVE_UNUSED", TN1, TN1_NATIVE_UNUSED},

/*
 * Sold in Store flags 1
 */

    {"GENERAL_STORE", SF1, SF1_GENERAL_STORE},
    {"ARMORY", SF1, SF1_ARMORY},
    {"WEAPONSMITH", SF1, SF1_WEAPONSMITH},
    {"TEMPLE", SF1, SF1_TEMPLE},
    {"ALCHEMIST", SF1, SF1_ALCHEMIST},
    {"MAGIC_SHOP", SF1, SF1_MAGIC_SHOP},
    {"BLACK_MARKET", SF1, SF1_BLACK_MARKET},
    {"BOOKSHOP", SF1, SF1_BOOKSHOP},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},
    {"STORE_UNUSED", SF1, SF1_STORE_UNUSED},

/* Terrain flags */

    {"LOS", FF1, FF1_LOS},
    {"PROJECT", FF1, FF1_PROJECT},
    {"MOVE", FF1, FF1_MOVE},
    {"PLACE", FF1, FF1_PLACE},
    {"DROP", FF1, FF1_DROP},
    {"SECRET", FF1, FF1_SECRET},
    {"NOTICE",	FF1, FF1_NOTICE},
    {"REMEMBER",	FF1, FF1_REMEMBER},
    {"CAN_OPEN", FF1, FF1_CAN_OPEN},
    {"CAN_CLOSE", FF1, FF1_CAN_CLOSE},
    {"CAN_BASH", FF1, FF1_CAN_BASH},
    {"CAN_SPIKE", FF1, FF1_CAN_SPIKE},
    {"CAN_DISARM", FF1, FF1_CAN_DISARM},
    {"SHOP", FF1, FF1_SHOP},
    {"CAN_TUNNEL", FF1, FF1_CAN_TUNNEL},
    {"F1XXX_1", FF1, FF1_F1XXX_1},
    {"HAS_GOLD", FF1, FF1_HAS_GOLD},
    {"F1XXX_2", FF1, FF1_F1XXX_2},
    {"DOOR", FF1, FF1_DOOR},
    {"TRAP", FF1, FF1_TRAP},
    {"STAIRS", FF1, FF1_STAIRS},
    {"GLYPH", FF1, FF1_GLYPH},
    {"LESS", FF1, FF1_LESS},
    {"MORE", FF1, FF1_MORE},
    {"RUN", FF1, FF1_RUN},
    {"FLOOR", FF1, FF1_FLOOR},
    {"WALL", FF1, FF1_WALL},
    {"PERMANENT", FF1, FF1_PERMANENT},
    {"INNER", FF1, FF1_INNER},
    {"OUTER", FF1, FF1_OUTER},
    {"SOLID", FF1, FF1_SOLID},
    {"HIT_TRAP", FF1, FF1_HIT_TRAP},

    /*FF1 Uber-flags*/


/*
 * Terrain flags 2
 */

    {"BRIDGE", FF2, FF2_BRIDGE},
    {"RIVER", FF2, FF2_RIVER},
    {"LAKE", FF2, FF2_LAKE},
    {"F2XXX_1", FF2, FF2_F2XXX_1},
    {"COVERED", FF2, FF2_COVERED},
    {"GLOW", FF2, FF2_GLOW},
    {"F2XXX_2", FF2, FF2_F2XXX_2},
    {"EFFECT", FF2, FF2_EFFECT},
    {"F2XXX_3", FF2, FF2_F2XXX_3},
    {"F2XXX_14", FF2, FF2_F2XXX_14},
    {"F2XXX_15", FF2, FF2_F2XXX_15},
    {"F2XXX_13", FF2, FF2_F2XXX_13},
    {"HURT_ROCK", FF2, FF2_HURT_ROCK},
    {"HURT_FIRE", FF2, FF2_HURT_FIRE},
    {"HURT_COLD", FF2, FF2_HURT_COLD},
    {"HURT_ACID", FF2, FF2_HURT_ACID},
    {"HURT_ELEC", FF2, FF2_HURT_ELEC},
    {"HURT_WATER", FF2, FF2_HURT_WATER},
    {"F2XXX_4", FF2, FF2_F2XXX_4},
    {"F2XXX_5", FF2, FF2_F2XXX_5},
    {"F2XXX_6", FF2, FF2_F2XXX_6},
    {"F2XXX_7", FF2, FF2_F2XXX_7},
    {"CAN_FLY", FF2, FF2_CAN_FLY},
    {"F2XXX_8", FF2, FF2_F2XXX_8},
    {"F2XXX_9", FF2, FF2_F2XXX_9},
    {"F2XXX_10", FF2, FF2_F2XXX_10},
    {"TRAP_PASSIVE", FF2, FF2_TRAP_PASSIVE},
    {"TRAP_SMART", FF2, FF2_TRAP_SMART},
    {"TRAP_MON", FF2, FF2_TRAP_MON},
    {"F2XXX_11", FF2, FF2_F2XXX_11},
    {"F2XXX_12", FF2, FF2_F2XXX_12},
    {"SHAFT", FF2, FF2_SHAFT},

    /*FF2 Uber-flags*/


/*
 * Terrain flags 3
 */

    {"LAVA", FF3, FF3_LAVA},
    {"ICE", FF3, FF3_ICE},
    {"OIL", FF3, FF3_OIL},
    {"FIRE", FF3, FF3_FIRE},
    {"SAND", FF3, FF3_SAND},
    {"FOREST", FF3, FF3_FOREST},
    {"WATER", FF3, FF3_WATER},
    {"ACID", FF3, FF3_ACID},
    {"MUD", FF3, FF3_MUD},
    {"F3XXX_0", FF3, FF3_F3XXX_0},
    {"F3XXX_1", FF3, FF3_F3XXX_1},
    {"F3XXX_2", FF3, FF3_F3XXX_2},
    {"F3XXX_3", FF3, FF3_F3XXX_3},
    {"F3XXX_4", FF3, FF3_F3XXX_4},
    {"F3XXX_5", FF3, FF3_F3XXX_5},
    {"F3XXX_6", FF3, FF3_F3XXX_6},
    {"HURT_BOIL_WATER", FF3, FF3_HURT_BOIL_WATER},
    {"HURT_POIS", FF3, FF3_HURT_POIS},
    {"DOOR_JAMMED", FF3, FF3_DOOR_JAMMED},
    {"DOOR_CLOSED", FF3, FF3_DOOR_CLOSED},
    {"DOOR_OPEN", FF3, FF3_DOOR_OPEN},
    {"DOOR_BROKEN", FF3, FF3_DOOR_BROKEN},
    {"DOOR_LOCKED", FF3, FF3_DOOR_LOCKED},
    {"PICK_TRAP", FF3, FF3_PICK_TRAP},
    {"PICK_DOOR", FF3, FF3_PICK_DOOR},
    {"F3XXX_17", FF3, FF3_F3XXX_17},
    {"F3XXX_18", FF3, FF3_F3XXX_18},
    {"F3XXX_19", FF3, FF3_F3XXX_19},
    {"TREE", FF3, FF3_TREE},
    {"F3XXX_22", FF3, FF3_F3XXX_22},
    {"F3XXX_21", FF3, FF3_F3XXX_21},
    {"DYNAMIC", FF3, FF3_DYNAMIC},

    /*FFE Uber-Flags*/



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
    {"BRIGAND_COMBAT",  CF1, CF_BRIGAND_COMBAT},
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
static QString a_info_act[ACT_MAX] =
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
    "IDENTIFY_FULLY",
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
    "BERSERKER",
    "RES_ACID",
    "RES_ELEC",
    "RES_FIRE",
    "RES_COLD",
    "RES_POIS"
};



/*** Initialize from ascii template files ***/
/*
 *Read a line of 'N: serial number : name'.
 * Assumes the N: has already been stripped away
 */
int process_n_line(QString read_from_line, QString *save_to_string, int *array_index, int max_index)
{
    int num_index;

    // Isolate the name
    int colon_marker = read_from_line.lastIndexOf(':');

    QString actual_line_name = read_from_line;

    /* Isolate the name */
    actual_line_name.remove(0, colon_marker + 1);

    // Make sure we have a name
    if (!actual_line_name.length()) return (PARSE_ERROR_GENERIC);

    // Strip the name down to the index
    read_from_line.truncate(colon_marker);

    QTextStream line_string (&read_from_line);

    // Turn 'i' into the index#
    line_string >> num_index;

    /* Verify information */
    if (num_index <= last_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

    /* Verify information */
    if (num_index >= max_index) return (PARSE_ERROR_TOO_MANY_ENTRIES);

    /* Save the index so we can remember where we are next time*/
    last_idx = *array_index = num_index;

    // Add the actual name
    *save_to_string = actual_line_name;

    return (0);
}

// Process 8 integers from a string assumes v1:v2:v3:v4:v5:v6:v7 format
static int process_8_ints(QString line_info, int *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, int *v8)
{
    int fake_value = -25437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value, x4 = fake_value, x5 = fake_value, x6 = fake_value, x7 = fake_value, x8 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4 >> x5 >> x6 >> x7 >> x8;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x5 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x6 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x7 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x8 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;
    *v5 = x5;
    *v6 = x6;
    *v7 = x7;
    *v8 = x8;

    // Success
    return (0);
}

// Process 7 integers from a string assumes v1:v2:v3:v4:v5:v6:v7 format
static int process_7_ints(QString line_info, int *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7)
{
    int fake_value = -25437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value, x4 = fake_value, x5 = fake_value, x6 = fake_value, x7 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4 >> x5 >> x6 >> x7;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x5 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x6 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x7 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;
    *v5 = x5;
    *v6 = x6;
    *v7 = x7;

    // Success
    return (0);
}

// Process 6 integers from a string assumes v1:v2:v3:v4:v5:v6 format
static int process_6_ints(QString line_info, int *v1, int *v2, int *v3, int *v4, int *v5, int *v6)
{
    int fake_value = -25437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value, x4 = fake_value, x5 = fake_value, x6 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4 >> x5 >> x6;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x5 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x6 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;
    *v5 = x5;
    *v6 = x6;

    // Success
    return (0);
}

// Process 5 integers from a string assumes v1:v2:v3:v4:v5 format
static int process_5_ints(QString line_info, int *v1, int *v2, int *v3, int *v4, int *v5)
{
    int fake_value = -27437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value, x4 = fake_value, x5 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4 >> x5;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x5 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;
    *v5 = x5;

    // Success
    return (0);
}



// Process 4 integers from a string assumes v1:v2:v3:v4 format
int process_4_ints(QString line_info, int *v1, int *v2, int *v3, int *v4)
{
    int fake_value = -27437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value, x4 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;

    // Success
    return (0);
}

// Process 4 integers from a string assumes v1:v2:v3:v4 format
static int process_3_ints_1_long(QString line_info, int *v1, int *v2, int *v3, long *v4)
{
    int fake_value = -27437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value;
    long x4 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3 >> x4;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x4 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;
    *v4 = x4;

    // Success
    return (0);
}

// Process 3 integers from a string assumes v1:v2:v3 format
static int process_3_ints(QString line_info, int *v1, int *v2, int *v3)
{
    int fake_value = -27437;
    int x1 = fake_value, x2 = fake_value, x3 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2 >> x3;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x3 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;
    *v3 = x3;

    // Success
    return (0);
}

// Process 2 integers from a string assumes v1:v2 format
static int process_2_ints(QString line_info, int *v1, int *v2)
{
    int fake_value = -27437;
    int x1 = fake_value, x2 = fake_value;

    // Get rid of all the colons
    line_info.replace(QString(":"), QString(" "));
    line_info.replace(QString("d"), QString(" "));
    line_info.replace(QString("/"), QString(" "));

    QTextStream line_string (&line_info);

    line_string >> x1 >> x2;

    /* Make sure everything was read properly */

    if (x1 == fake_value) return (PARSE_ERROR_GENERIC);
    if (x2 == fake_value) return (PARSE_ERROR_GENERIC);

    // Assume all values read properly.  Assume success.
    *v1 = x1;
    *v2 = x2;

    // Success
    return (0);
}


// Helper function for parse Z info (example, returns a value of 35 from QString F:35)
// The bool trim is to cut the first two characters, depending on if the line is sent with 'M:" or not.
static int process_single_value(QString line_info, bool trim)
{
    int value = 0;

    // If needed, get rid of the prefix and create a line string
    if (trim) line_info.remove(0,2);

    QTextStream line_string (&line_info);

    line_string >> value;

    return (value);
}


/*
 * Verify version number of an edit file.
 */
static int verify_version(QString read_line)
{

    int v1 = 0, v2 = 0, v3 = 0;

    // Get rid of the periods
    read_line.replace(QString("."), QString(" "));

    QTextStream text_stream(&read_line);

    text_stream >> v1 >> v2 >> v3;

    /* Verify the values */
    if ((v1 != VERSION_MAJOR) ||
        (v2 != VERSION_MINOR) ||
        (v3 != VERSION_PATCH))
    {


            return (PARSE_ERROR_OBSOLETE_FILE);
    }

    /* Success */
    return (0);
}

// Process G:{G:{Character}:{U}Color line.  Assumes G: has been chopped.
static QChar process_graphics_line(QString line_info, int *error_return, QColor *color_return, byte *color_slot)
{
    QChar symbol;
    QColor color;
    int count;

    *error_return = 0;

    /* Extract d_char and remove (including the colon) */
    symbol = line_info.at(0);
    line_info.remove(0,2);

    // The color can be entered as text or in RGB format.
    count = line_info.count(':');

    if (!line_info.length())
    {
        *error_return = PARSE_ERROR_MISSING_FIELD;
        return ('a');
    }

    /* Color entered in RGB format */
    if (count == 2)
    {
        int red, green, blue;

        process_3_ints(line_info, &red, &green, &blue);

        // Verify the colors are in the right ranget
        if ((red < 0) || (red > 255) ||
            (green < 0) || (green > 255) ||
            (blue < 0) ||  (blue > 255))
        {
            *error_return = PARSE_ERROR_GENERIC;

        }

        else
        {
            *color_slot = COLOR_CUSTOM;
            color.setRgb(red, green, blue, 255);
        }

    }
    else if (!count)//color entered by name.
    {
        bool color_found = FALSE;
        for (int i = 0; i < MAX_COLORS; i++)
        {
            // Not a match.
            if (!operator==(line_info, preset_colors[i].color_name)) continue;

            //load the color and finish.
            color_found = TRUE;
            color.setRgb(preset_colors[i].red, preset_colors[i].green, preset_colors[i].blue, 255);
            *color_slot = i;
            break;
        }

        if (!color_found) *error_return = PARSE_ERROR_GENERIC;
    }
    else *error_return = PARSE_ERROR_GENERIC;

    // success
    *color_return = color;
    return (symbol);
}

// Process the D: Descriptive Text line
static QString process_description(QString text_add, QString text_current)
{
    // Store the text.  Is this the first entry or are we appending?
    if (!text_current.length()) text_current = text_add;
    else text_current.append(text_add);

    return (text_current);
}



/*
 * Initialize the "z_info" structure, by parsing an ascii "template" file
 */
int parse_z_info(QString line_info)
{
    /* Skip comments and blank lines */
    if ((line_info.isNull()) || (line_info.isEmpty()) || (line_info.startsWith('#'))) return (0);

    /* Hack - Verify proper format */
    if (line_info.length() < 3) return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
    if (line_info.at(1) != ':') return (PARSE_ERROR_UNDEFINED_DIRECTIVE);

    QChar command = line_info.at(0);

    if (command == 'V')
    {
        // get rid of the V: prefix
        line_info.remove(0,2);

        return (verify_version(line_info));
    }

    /* Process 'F' for "Maximum f_info[] index" */
    else if (command == 'F')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->f_max = max;
    }

    /* Process 'K' for "Maximum k_info[] index" */
    else if (command == 'K')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->k_max = max;
    }

    /* Process 'A' for "Maximum a_info[] index" */
    else if (command == 'A')
    {
        int art_special_max = 0, art_normal_max = 0, art_random_max = 0;

        // Get rid of the prefix and all the colons
        line_info.remove(0,2);

        if (process_3_ints(line_info, &art_special_max, &art_normal_max, &art_random_max) > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        z_info->art_spec_max = art_special_max;
        z_info->art_norm_max = art_normal_max + art_special_max;
        z_info->art_rand_max = z_info->art_norm_max + art_random_max;

        /*that final slot is for a set quest artifact*/
        z_info->art_max = art_special_max + art_normal_max + art_random_max + 1;
    }

    /* Process 'E' for "Maximum e_info[] index" */
    else if (command == 'E')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->e_max = max;
    }

    /* Process 'G' for "Maximum t_info[] index" */
    else if (command == 'G')
    {

        int maintainer_ghost_max = 0, player_ghost_max = 0;

        // Get rid of the prefix and all the colons
        line_info.remove(0,2);

        if (process_2_ints(line_info, &maintainer_ghost_max, &player_ghost_max) > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        z_info->ghost_player_max = player_ghost_max;
        z_info->ghost_maint_max = maintainer_ghost_max;
        z_info->ghost_template_max = maintainer_ghost_max + player_ghost_max;
    }

    /* Process 'R' for "Maximum r_info[] index" */
    else if (command == 'R')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->r_max = max;

    }


    /* Process 'T' for "Maximum v_info[] index" */
    else if (command == 'T')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->v_max = max;
    }


    /* Process 'P' for "Maximum p_info[] index" */
    else if (command == 'P')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
       z_info->p_max = max;
    }

    /* Process 'C' for "Maximum c_info[] index" */
    else if (command == 'C')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->c_max = max;
    }

    /* Process 'X' for "Maximum x_info[] index" */
    else if (command == 'X')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->x_max = max;
    }

    /* Process 'H' for "Maximum h_info[] index" */
    else if (command == 'H')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->h_max = max;
    }

    /* Process 'B' for "Maximum b_info[] subindex" */
    else if (command == 'B')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->b_max = max;
    }

    /* Process 'Q' for "Maximum q_info[] index" */
    else if (command == 'Q')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->q_max = max;
    }

    /* Process 'L' for "Maximum flavor_info[] subindex" */
    else if (command == 'L')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
         z_info->flavor_max = max;
    }

    /* Process 'O' for "Maximum o_list[] index" */
    else if (command == 'O')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->o_max = max;
    }

    /* Process 'M' for "Maximum mon_list[] index" */
    else if (command == 'M')
    {
        int max = process_single_value(line_info, TRUE);

        /* Verify values */
        if (!max) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        z_info->m_max = max;
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
int parse_v_info(QString line_info)
{
    /* Current entry */
    vault_type *v_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             v_ptr =& v_info[last_idx];
        }
    }

    if (command == 'V')
    {
        return (verify_version(line_info));
    }

    /* Process 'N' for "New/Number/Name" */
    else  if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->v_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        v_ptr =& v_info[index_entry];

        // Add the info
        v_ptr->vault_name = name_entry;
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current v_ptr */
        if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        // Make sure we have a name
        if (!line_info.length()) return (PARSE_ERROR_GENERIC);

        //Store the text
        v_ptr->vault_text = process_description (line_info, v_ptr->vault_text);

    }

    /* Process 'X' for "Extra info" (one line only) */
    else if (command == 'X')
    {
        int typ = 0, rat = 0, hgt = 0, wid = 0;

        /* There better be a current v_ptr */
        if (!v_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

         if (process_4_ints(line_info, &typ, &rat, &hgt, &wid) > 0) return (PARSE_ERROR_GENERIC);

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
 * Starting with a flag, find out where it goes, and mark it.
 */
static QString find_one_flag(QString flag_line, int *return_slot, QString entry_type, num_flag_sets min_flag, num_flag_sets max_flag)
{
    u16b i;
    QString single_flag;

    // Step one, isolate a single flag, and cut it off from the string
    // Find the rightmost flag splitter
    int splitter = flag_line.lastIndexOf('|');

    single_flag = flag_line;

    // No splitter found, must be only one flag remaining
    if (splitter == -1)
    {
        //Clear the flag_line so we move onto the next line
        flag_line.clear();
    }
    // Handle a line with multiple lines remaining
    else
    {
        // Cut the rightmost flag off from the rest and work with it
        single_flag.remove(0, splitter);

        // Cut off the rightmost flag off for next time.
        flag_line.truncate(splitter);
        flag_line.chop(1);
    }

    // Strip it down to the basic flag
    single_flag.remove(QChar(' '));
    single_flag.remove(QChar('|'));

    // Step 2 - find the matching flag and return
    // Check flags
    for (i = 0; i < N_ELEMENTS(info_flags); i++)
    {
        flag_name *flag_ptr =& info_flags[i];
        // Make sure we are in the right set
        if (flag_ptr->set < min_flag) continue;
        if (flag_ptr->set > max_flag) continue;

        if (single_flag.compare(flag_ptr->name, Qt::CaseInsensitive) == 0)
        {
            *return_slot = i;
            //Found it
            return (flag_line);
        }
    }

    // report error
    pop_up_message_box(QString(QObject::tr("Entry is %1 Unknown %2 flag '%3'.")) .arg(last_idx) .arg(entry_type) .arg(single_flag));

    *return_slot = -1;

    //Found it
    return (flag_line);
}



/*
 * Grab an action in an feature_type from a textual string
 *
 * IMPORTANT!!! Each set of feature flags (FF1, FF2 and FF3) must be COMPLETE
 * (32 entries, even with unused flags) and their ordering must match the
 * ordering in defines.h -DG-
 */
static int grab_one_feature_action(feature_type *f_ptr, QString what, int count)
{
    int ffx_index = 0;
    flag_name *flag_ptr;

    u16b i;

    for (i = 0; i < N_ELEMENTS(info_flags); i++)
    {

        flag_ptr =& info_flags[i];

        if ((flag_ptr->set == FF1) || (flag_ptr->set == FF2) ||
            (flag_ptr->set == FF3))
        {
            if (operator==(what, flag_ptr->name))
            {
                f_ptr->state[count].fs_action = ffx_index;
                return 0;
            }
            ffx_index++;
        }

    }

    // Couldn't find the flag
    pop_up_message_box(QString(QObject::tr("last index %1 Unknown feature action flag '%2'")) .arg (last_idx) .arg(what));

    return -1;
}

/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
int parse_f_info(QString line_info)
{
    /* Current entry */
    feature_type *f_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    int i;

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             f_ptr =& f_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;

        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->f_max);

        if (is_error) return (is_error);

        f_ptr =& f_info[index_entry];
        f_ptr->f_name = name_entry;

        /* Default "mimic" */
        f_ptr->f_mimic = index_entry;

        /* Default "state change" -- if not specified */
        f_ptr->defaults = index_entry;

        /* Default "states" */
        for (i = 0; i < MAX_FEAT_STATES; i++) f_ptr->state[i].fs_action = FS_FLAGS_END;
    }

    /* Process 'M' for "Mimic" (one line only) */
    else if (command == 'M')
    {
        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        int f_mimic = process_single_value(line_info, FALSE);

        /* Verify values */
        if (!f_mimic) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        f_ptr->f_mimic = (u16b)f_mimic;
    }

    /* Process 'G' for "Graphics" (one line only) */
    else if (command == 'G')
    {
        QChar d_char;
        QColor d_color;
        byte color_slot;
        int error;

        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        d_char = process_graphics_line(line_info, &error, &d_color, &color_slot);

        /* Paranoia */
        if (error > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        f_ptr->color_num = color_slot;
        f_ptr->d_color = d_color;
        f_ptr->d_char = d_char;
    }

    /* Hack -- Process 'F' for flags */
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while (flag_line.length() > 0)
        {
            int slot;

            flag_line = find_one_flag(flag_line, &slot, "terrain", FF1, FF3);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == FF1) f_ptr->f_flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == FF2) f_ptr->f_flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == FF3) f_ptr->f_flags3 |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Process 'E' for "Edge */
    else if (command == 'E')
    {
        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        int edge = process_single_value(line_info, FALSE);

        /* Verify values */
        if (!edge) return (PARSE_ERROR_GENERIC);

        /* Save the value */
        f_ptr->f_edge = edge;
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (command == 'W')
    {
        int level = 0, rarity = 0, power = 0;

        /* There better be a current v_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

         if (process_3_ints(line_info, &level, &rarity, & power) > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        f_ptr->f_level = level;
        f_ptr->f_rarity = rarity;
        f_ptr->f_power = power;
    }

    /* Process 'C' for damage and movement info */
    else if (command == 'C')
    {
        int dam_non_native = -100, native_energy_move = -100, non_native_energy_move = -100;
        int native_to_hit_adj = -100, non_native_to_hit_adj = -100, stealth_adj = -100;

        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

         if (process_6_ints(line_info, &dam_non_native, &native_energy_move,
                            &non_native_energy_move, &native_to_hit_adj,
                            &non_native_to_hit_adj, &stealth_adj) > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        f_ptr->dam_non_native = dam_non_native;
        f_ptr->native_energy_move = native_energy_move;
        f_ptr->non_native_energy_move = non_native_energy_move;
        f_ptr->native_to_hit_adj = native_to_hit_adj;
        f_ptr->non_native_to_hit_adj = non_native_to_hit_adj;
        f_ptr->f_stealth_adj = stealth_adj;
    }

    /* Process 'X' for "Effects Info" (one line for effects only) effects will not have the W or C lines*/
    else if (command == 'X')
    {
        int level = -100, rarity = -100, power = -100, damage = -100, gf_type = -100, timeout_set = -100, timeout_rand = -100;

        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_7_ints(line_info, &level, &rarity, &power, & damage, &gf_type, & timeout_set, &timeout_rand)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        f_ptr->f_level = level;
        f_ptr->f_rarity = rarity;
        f_ptr->f_power = power;
        f_ptr->x_damage = (byte)damage;
        f_ptr->x_gf_type = (byte)gf_type;
        f_ptr->x_timeout_set = (byte)timeout_set;
        f_ptr->x_timeout_rand = (byte)timeout_rand;

    }

    /* Process 'K' for "States" (up to four lines + default (which cannot be last)) */
    else if (command == 'K')
    {
        int change_feat = 0, change_power = 0;
        int  count, splitter, i;
        QString action;

        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Find the next empty state slot (if any) */
        for (i = 0; i < MAX_FEAT_STATES; i++) if (f_ptr->state[i].fs_action == FS_FLAGS_END) break;

        /* Oops, no more slots */
        if (i == MAX_FEAT_STATES) return (PARSE_ERROR_GENERIC);

        action = line_info;

        // Do we have one or two colons?
        count = line_info.count(':');

        /* We need a second field (transition result) */
        if (!count) return (PARSE_ERROR_GENERIC);

        // Separate the action from the numbers
        splitter = action.indexOf(':');
        action.truncate(splitter);
        line_info.remove(0, (splitter + 1));

        if (count == 1) change_feat = process_single_value(line_info, FALSE);
        else process_2_ints(line_info, &change_feat, &change_power);

        if (!change_feat) return (PARSE_ERROR_INVALID_FEATURE_TRANSITION);

        /* Is this default entry? */
        if (action.contains("DEFAULT"))
        {
            /* Analyze result */
            f_ptr->defaults = change_feat;

        }
        else
        {
            /* Reset */
            f_ptr->state[i].fs_action = 0;

            /* Parse this entry */
            if (grab_one_feature_action(f_ptr, action, i)) return (PARSE_ERROR_INVALID_FLAG);

            /* Analyze result */
            f_ptr->state[i].fs_result = change_feat;

            if (count == 2) f_ptr->state[i].fs_power = change_power;

            /*
             * If we don't have a transition power we just copy
             * the default power
             */
            else f_ptr->state[i].fs_power = f_ptr->f_power;
        }
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current f_ptr */
        if (!f_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        f_ptr->f_text = process_description (line_info, f_ptr->f_text);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
int parse_k_info(QString line_info)
{
    /* Current entry */
    static object_kind *k_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             k_ptr =& k_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->k_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        k_ptr =& k_info[index_entry];

        // Add the info
        k_ptr->k_name = name_entry;
    }

    /* Process 'G' for "Graphics" (one line only) */
    else if (command == 'G')
    {        
        QChar d_char;
        QColor d_color;
        byte color_slot;
        int error;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        d_char = process_graphics_line(line_info, &error, &d_color, &color_slot);

        /* Paranoia */
        if (error > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        k_ptr->color_num = color_slot;
        k_ptr->d_color = d_color;
        k_ptr->d_char = d_char;
    }

    /* Process 'I' for "Info" (one line only) */
    else if (command == 'I')
    {
        int tval = -100, sval = -100, pval = -100;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints(line_info, &tval, &sval, &pval))  return (PARSE_ERROR_GENERIC);

        /* Save the values */
        k_ptr->tval = tval;
        k_ptr->sval = sval;
        k_ptr->pval = pval;
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (command == 'W')
    {
        int level, extra, wgt;
        long cost;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints_1_long(line_info, &level, &extra, &wgt, &cost))  return (PARSE_ERROR_GENERIC);

        /* Save the values */
        k_ptr->k_level = level;
        k_ptr->extra = extra;
        k_ptr->weight = wgt;
        k_ptr->cost = cost;
    }

    /* Process 'A' for "Allocation" (one line only) */
    else if (command == 'A')
    {
        int depth_1 = -1, rarity_1 = -1, depth_2 = -1, rarity_2 = -1, depth_3 = -1, rarity_3 = -1,

        // How many slashes do we have?
        count = line_info.count('/');

        // needs to be between 1 and 3
        if ((count == 0) || (count > 3)) return PARSE_ERROR_TOO_MANY_ALLOCATIONS;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (count == 1)
        {
            if (process_2_ints(line_info, &depth_1, &rarity_1)) return (PARSE_ERROR_GENERIC);

            k_ptr->locale[0] = depth_1;
            k_ptr->chance[0] = rarity_1;

        }
        else if (count == 2)
        {
            if (process_4_ints(line_info, &depth_1, &rarity_1, &depth_2, &rarity_2)) return (PARSE_ERROR_GENERIC);

            k_ptr->locale[0] = depth_1;
            k_ptr->chance[0] = rarity_1;
            k_ptr->locale[1] = depth_2;
            k_ptr->chance[1] = rarity_2;
        }
        else // (count == 3)
        {
            if (process_6_ints(line_info, &depth_1, &rarity_1, &depth_2, &rarity_2, &depth_3, &rarity_3)) return (PARSE_ERROR_GENERIC);

            k_ptr->locale[0] = depth_1;
            k_ptr->chance[0] = rarity_1;
            k_ptr->locale[1] = depth_2;
            k_ptr->chance[1] = rarity_2;
            k_ptr->locale[2] = depth_3;
            k_ptr->chance[2] = rarity_3;
        }
    }

    /* Hack -- Process 'P' for "power" and such */
    else if (command == 'P')
    {
        int ac = -100, hd1 = -100, hd2 = -100, th = -100, td = -100, ta = -100;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_6_ints(line_info, &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

        k_ptr->ac = ac;
        k_ptr->dd = hd1;
        k_ptr->ds = hd2;
        k_ptr->to_h = th;
        k_ptr->to_d = td;
        k_ptr->to_a =  ta;
    }

    /* Hack -- Process 'F' for flags */
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while (flag_line.length() > 0)
        {
            int slot;

            flag_line = find_one_flag(flag_line, &slot, "object_kind", SF1, TN1);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == TR1)         k_ptr->k_flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == TR2)    k_ptr->k_flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == TR3)    k_ptr->k_flags3 |= fl_ptr->flag;
            else if (fl_ptr->set == TN1)    k_ptr->k_native |= fl_ptr->flag;
            else if (fl_ptr->set == SF1)    k_ptr->k_store  |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current k_ptr */
        if (!k_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        k_ptr->k_text = process_description (line_info, k_ptr->k_text);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Initialize the "t_info" array, by parsing an ascii "template" file
 */
int parse_t_info(QString line_info)
{
    /* Current entry */
    static ghost_template *t_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             t_ptr =& t_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;

        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->ghost_template_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        t_ptr =& t_info[index_entry];

        // Add the info
        t_ptr->t_name = name_entry;
    }

    /* Process 'I' for "Info" (one line only) */
    else if (command == 'I')
    {
        int t_gender, t_race, t_class, t_depth;

        /* There better be a current k_ptr */
        if (!t_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        // Get rid of all the colons and d
        if (process_4_ints(line_info, &t_gender, &t_race, &t_class, & t_depth))return (PARSE_ERROR_GENERIC);

        /* Save the values */
        t_ptr->t_gender = t_gender;
        t_ptr->t_race = t_race;
        t_ptr->t_class = t_class;
        t_ptr->t_depth = t_depth;
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Grab one activation from a textual string
 */
static int grab_one_activation(artifact_type *a_ptr, QString what)
{
    int i;

    /* Scan activations */
    for (i = 0; i < ACT_MAX; i++)
    {
        if (operator==(what, a_info_act[i]))
        {
            a_ptr->activation = i;
            return (0);
        }
    }

    pop_up_message_box(QString(QObject::tr("Unknown artifact activation '%1'")) .arg(what));

    /* Error */
    return (PARSE_ERROR_GENERIC);
}



/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
int parse_a_info(QString line_info)
{
    /* Current entry */
    static artifact_type *a_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             a_ptr =& a_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;

        // Note we are oimiting entries to art_norm_max to leave room for in-game created randarts
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->art_norm_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        a_ptr =& a_info[index_entry];

        // Add the info
        a_ptr->a_name = name_entry;

        /* Ignore everything */
        a_ptr->a_flags3 |= (TR3_IGNORE_MASK);

    }

    /* Process 'I' for "Info" (one line only) */
    else if (command == 'I')
    {
        int tval = -100, sval = -100, pval = -100;

        /* There better be a current a_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints(line_info, &tval, &sval, &pval)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        a_ptr->tval = tval;
        a_ptr->sval = sval;
        a_ptr->pval = pval;
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (command == 'W')
    {
        int level, rarity, wgt;
        long cost;

        /* There better be a current k_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints_1_long(line_info, &level, &rarity, &wgt, &cost)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        a_ptr->a_level = level;
        a_ptr->a_rarity = rarity;
        a_ptr->weight = wgt;
        a_ptr->cost = cost;
    }

    /* Process 'P' for "power" and such */
    else if (command == 'P')
    {
        int ac = -100, hd1 = -100, hd2 = -100, th = -100, td = -100, ta = -100;

        /* There better be a current a_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_6_ints(line_info, &ac, &hd1, &hd2, &th, &td, &ta)) return (PARSE_ERROR_GENERIC);

        a_ptr->ac = ac;
        a_ptr->dd = hd1;
        a_ptr->ds = hd2;
        a_ptr->to_h = th;
        a_ptr->to_d = td;
        a_ptr->to_a = ta;
    }

    /* Process 'F' for flags */
    else if (command == 'F')
    {
        /* There better be a current a_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        QString flag_line = line_info;

        while (flag_line.length() > 0)
        {
            int slot;

            flag_line = find_one_flag(flag_line, &slot, "artifact", TR1, TN1);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == TR1)         a_ptr->a_flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == TR2)    a_ptr->a_flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == TR3)    a_ptr->a_flags3 |= fl_ptr->flag;
            else if (fl_ptr->set == TN1)    a_ptr->a_native |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Process 'A' for "Activation & time" */
    else if (command == 'A')
    {
        int ptime = 0, prand = 0;
        int  count, splitter;
        QString action;

        /* There better be a current a_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        action = line_info;

        // Do we have two colons?
        count = line_info.count(':');

        /* We need 3 fields */
        if (count != 2) return (PARSE_ERROR_GENERIC);

        // Separate the action from the numbers
        splitter = action.indexOf(':');
        action.truncate(splitter);
        line_info.remove(0, (splitter + 1));

        if (process_2_ints(line_info, &ptime, &prand)) return (PARSE_ERROR_GENERIC);

        /* Parse this entry */
        if (grab_one_activation(a_ptr, action)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        a_ptr->time = ptime;
        a_ptr->randtime = prand;
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current a_ptr */
        if (!a_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        a_ptr->a_text = process_description (line_info, a_ptr->a_text);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Add a name to the probability tables
 */
static int build_prob(QString name, names_type *n_ptr)
{
    // eliminate spaces
    name.remove(QChar(' '));

    int length = name.length();

    int c_prev, c_cur, c_next;

    // Make sure there is an actual name
    if (!length)	return PARSE_ERROR_GENERIC;    

    c_prev = c_cur = S_WORD;

    for (int i = 0; i < length; i++)
    {
        QChar character = name.at(i);

        if (character.isLetter())
        {
            character = character.toLower();

            c_next = letter_to_number(character);
            n_ptr->lprobs[c_prev][c_cur][c_next]++;
            n_ptr->ltotal[c_prev][c_cur]++;
            c_prev = c_cur;
            c_cur = c_next;
        }
    }

    n_ptr->lprobs[c_prev][c_cur][E_WORD]++;
    n_ptr->ltotal[c_prev][c_cur]++;

    return 0;
}

/*
 * Initialize the "n_info" array, by parsing an ascii "template" file
 */
int parse_n_info(QString line_info)
{
    names_type *n_ptr = n_info;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info.at(0) == '#') return (0);
    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    if (command == 'N')
    {
        return build_prob (line_info, n_ptr);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
    }

    else     return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
}

static int find_current_curt(const ego_item_type *e_ptr)
{
    for (int i = 0;  i < EGO_TVALS_MAX; i++)
    {
        if (e_ptr->tval[i] > 0) continue;
        if (e_ptr->min_sval[i] > 0) continue;
        if (e_ptr->max_sval[i] > 0) continue;

        // Success
        return i;
    }

    return EGO_TVALS_MAX;
}


/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
int parse_e_info(QString line_info)
{
    /* Current entry */
    static ego_item_type *e_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             e_ptr =& e_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->e_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        e_ptr =& e_info[index_entry];

        // Add the info
        e_ptr->e_name = name_entry;
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (command == 'W')
    {
        int level, rarity, pad2;
        long cost;

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints_1_long(line_info, &level, &rarity, &pad2, &cost)) return (PARSE_ERROR_GENERIC);

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Save the values */
        e_ptr->level = level;
        e_ptr->rarity = rarity;
        /* e_ptr->weight = wgt; */
        e_ptr->cost = cost;
    }

    /* Process 'X' for "Xtra" (one line only) */
    else if (command == 'X')
    {
        int e_rating, xtra;

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_2_ints(line_info, &e_rating, &xtra)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        e_ptr->rating = e_rating;
        e_ptr->xtra = xtra;
    }

    /* Process 'T' for "Types allowed" (up to three lines) */
    else if (command == 'T')
    {
        int tval, sval1, sval2;

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        int cur_t = find_current_curt(e_ptr);

        /* Allow only a limited number of T: lines */
        if (cur_t >= EGO_TVALS_MAX) return (PARSE_ERROR_GENERIC);

        if (process_3_ints(line_info, &tval, &sval1, &sval2)) return (PARSE_ERROR_GENERIC);

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
    else if (command == 'C')
    {
        int th, td, ta, pv;

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_4_ints(line_info, &th, &td, &ta, &pv)) return (PARSE_ERROR_GENERIC);

        e_ptr->max_to_h = th;
        e_ptr->max_to_d = td;
        e_ptr->max_to_a = ta;
        e_ptr->max_pval = pv;
    }

    /* Hack -- Process 'F' for flags */
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while(flag_line.length() > 0)
        {

            int slot;

            flag_line = find_one_flag(flag_line, &slot, "ego_item", TR1, TN1);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == TR1)         e_ptr->e_flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == TR2)    e_ptr->e_flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == TR3)    e_ptr->e_flags3 |= fl_ptr->flag;
            else if (fl_ptr->set == TN1)    e_ptr->e_native |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current e_ptr */
        if (!e_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        e_ptr->e_text = process_description (line_info, e_ptr->e_text);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
int parse_r_info(QString line_info)
{
    int i;

    /* Current entry */
    static monster_race *r_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {

        if (last_idx > -1)
        {
            /* Point at the "info" */
             r_ptr =& r_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;

        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->r_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        r_ptr =& r_info[index_entry];

        // Add the info
        r_ptr->r_name_full = name_entry;
    }

    /* Process 'A' for "Abbreviated Name" */
    else if (command == 'A')
    {
        if (!line_info.length()) return (PARSE_ERROR_GENERIC);

        r_ptr->r_name_short = line_info;
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        r_ptr->r_text = process_description (line_info, r_ptr->r_text);
    }

    /* Process 'G' for "Graphics" (one line only) */
    else if (command == 'G')
    {
        QChar d_char;
        QColor d_color;
        byte color_slot;
        int error;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        d_char = process_graphics_line(line_info, &error, &d_color, &color_slot);

        /* Paranoia */
        if (error > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        r_ptr->color_num = color_slot;
        r_ptr->d_color = d_color;
        r_ptr->d_char = d_char;
    }

    /* Process 'I' for "Info" (one line only) */
    else if (command == 'I')
    {
        int spd = -100, hp1 = -100, hp2 = -100, aaf = -100, ac = -100, slp = -100;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_6_ints(line_info, &spd, &hp1, &hp2, &aaf, &ac, &slp))return (PARSE_ERROR_GENERIC);

        /* Save the values */
        r_ptr->r_speed = spd;
        r_ptr->hdice = hp1;
        r_ptr->hside = hp2;
        r_ptr->aaf = aaf;
        r_ptr->ac = ac;
        r_ptr->sleep = slp;
    }

    /* Process 'W' for "More Info" (one line only) */
    else if (command == 'W')
    {
        int lev, rar, mana;
        long exp;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints_1_long(line_info, &lev, &rar, &mana, &exp)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        r_ptr->level = lev;
        r_ptr->rarity = rar;
        r_ptr->mana = mana;
        r_ptr->mexp = exp;
    }

    /* Process 'B' for "Blows" */
    else if (command == 'B')
    {
        int n1, n2, splitter, count, dd, ds;
        bool has_method = FALSE, has_damage_dice = FALSE;

        QString blow, method, damage_dice;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Find the next empty blow slot (if any) */
        for (i = 0; i < MONSTER_BLOW_MAX; i++) if (!r_ptr->blow[i].method) break;

        /* Oops, no more slots */
        if (i == MONSTER_BLOW_MAX) return (PARSE_ERROR_GENERIC);

        //We have 3 scenarios to parse... a simple blow method, a blow method + attack, and method/attack with damage dice.
        // Do we have zero, one or two colons?
        count = line_info.count(':');

        blow = line_info;
        method = line_info;

        // We at least have a method
        if (count)
        {
            // Separate the action from the method
            splitter = blow.indexOf(':');
            blow.truncate(splitter);
            method.remove(0, splitter+1);
            has_method = TRUE;

            // We have damage dice as well.
            if (count == 2)
            {
                // Separate the blow from the damage dice.
                splitter = method.indexOf(':');
                damage_dice = method;
                method.truncate(splitter);
                damage_dice.remove(0, splitter+1);
                has_damage_dice = TRUE;
            }
        }

        if (blow.isEmpty()) n1 = 0;

        /* Analyze the blow */
        else for (n1 = 1; n1 < BLOW_METHOD_MAX; n1++)
        {
            QString blow_method = r_info_blow_method[n1];

            if (operator==(blow, r_info_blow_method[n1])) break;
        }

        /* Invalid method */
        if (n1 == BLOW_METHOD_MAX) return (PARSE_ERROR_GENERIC);

        if (has_method)
        {
            if (blow.isEmpty()) n2 = 0;

            /* Analyze effect */
            else for (n2 = 1; BLOW_EFFECT_MAX; n2++)
            {
                if (operator==(method, r_info_blow_effect[n2])) break;
            }

            /* Invalid effect */
            if (n2 == BLOW_EFFECT_MAX) return (PARSE_ERROR_GENERIC);
        }

        if (has_damage_dice)
        {
            if (process_2_ints(damage_dice, &dd, &ds)) return (PARSE_ERROR_GENERIC);
        }


        /* Save the method */
        r_ptr->blow[i].method = n1;

        /* Save the effect */
        if (has_method) r_ptr->blow[i].effect = n2;

        /* Extract the damage dice and sides */
        if (has_damage_dice)
        {
            r_ptr->blow[i].d_dice = dd;
            r_ptr->blow[i].d_side = ds;
        }
    }

    /* Process 'F' for "Basic Flags" (multiple lines) */
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while(flag_line.length() > 0)
        {

            int slot;

            flag_line = find_one_flag(flag_line, &slot, "monster flag", RN1, RF3);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == RF1)         r_ptr->flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == RF2)    r_ptr->flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == RF3)    r_ptr->flags3 |= fl_ptr->flag;
            else if (fl_ptr->set == RN1)    r_ptr->r_native |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Process 'S' for "Spell Flags" (multiple lines) */
    else if (command == 'S')
    {
        QString flag_line = line_info;

        /* There better be a current r_ptr */
        if (!r_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        // For some reason spellpower and power
        if (flag_line.contains("SPELL_PCT"))
        {
            int spell_pct, spell_power;

            QString spell_capability = flag_line;
            //get rid of all the letters
            spell_capability.remove(QChar('S'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('P'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('E'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('L'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('_'));
            spell_capability.remove(QChar('C'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('T'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('O'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('W'), Qt::CaseInsensitive);
            spell_capability.remove(QChar('|'));

            QTextStream final_percent (&spell_capability);
            final_percent >> spell_pct >> spell_power;
            r_ptr->freq_ranged = spell_pct;
            r_ptr->spell_power = spell_power;
        }

        else while(flag_line.length() > 0)
        {

            int slot;

            flag_line = find_one_flag(flag_line, &slot, "spell_flag", RF4, RF7);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == RF4)         r_ptr->flags4 |= fl_ptr->flag;
            else if (fl_ptr->set == RF5)    r_ptr->flags5 |= fl_ptr->flag;
            else if (fl_ptr->set == RF6)    r_ptr->flags6 |= fl_ptr->flag;
            else if (fl_ptr->set == RF7)    r_ptr->flags7 |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Initialize the "p_info" array, by parsing an ascii "template" file
 */
int parse_p_info(QString line_info)
{
    /* Current entry */
    static player_race *pr_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             pr_ptr =& p_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->p_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        pr_ptr =& p_info[index_entry];

        // Add the info
        pr_ptr->pr_name = name_entry;
    }

    /* Process 'S' for "Stats" (one line only) */
    else if (command == 'S')
    {
        int str, intell, wis, dex, con, chr;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_6_ints(line_info, &str, &intell, &wis, &dex, &con, &chr))return (PARSE_ERROR_GENERIC);

        pr_ptr->r_adj[A_STR] = str;
        pr_ptr->r_adj[A_INT] = intell;
        pr_ptr->r_adj[A_WIS] = wis;
        pr_ptr->r_adj[A_DEX] = dex;
        pr_ptr->r_adj[A_CON] = con;
        pr_ptr->r_adj[A_CHR] = chr;
    }

    /* Process 'R' for "Racial Skills" (one line only) */
    else if (command == 'R')
    {
        int dis, dev, sav, stl, srh, fos, thn, thb;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_8_ints(line_info, &dis, &dev, &sav, &stl, &srh, &fos, &thn, &thb))return (PARSE_ERROR_GENERIC);

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
    else if (command == 'X')
    {
        int mhp, exp, infra;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_3_ints(line_info, &mhp, &exp, &infra))return (PARSE_ERROR_GENERIC);

        /* Save the values */
        pr_ptr->r_mhp = mhp;
        pr_ptr->r_exp = exp;
        pr_ptr->infra = infra;
    }

    /* Hack -- Process 'I' for "info" and such */
    else if (command == 'I')
    {
        int hist, b_age, m_age;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_3_ints(line_info, &hist, &b_age, &m_age))return (PARSE_ERROR_GENERIC);

        pr_ptr->hist = hist;
        pr_ptr->b_age = b_age;
        pr_ptr->m_age = m_age;
    }

    /* Hack -- Process 'H' for "Height" */
    else if (command == 'H')
    {
        int m_b_ht, m_m_ht, f_b_ht, f_m_ht;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_4_ints(line_info, &m_b_ht, &m_m_ht, &f_b_ht, &f_m_ht))return (PARSE_ERROR_GENERIC);

        pr_ptr->m_b_ht = m_b_ht;
        pr_ptr->m_m_ht = m_m_ht;
        pr_ptr->f_b_ht = f_b_ht;
        pr_ptr->f_m_ht = f_m_ht;
    }

    /* Hack -- Process 'W' for "Weight" */
    else if (command == 'W')
    {
        int m_b_wt, m_m_wt, f_b_wt, f_m_wt;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);


        /* Scan for the values */
        if (process_4_ints(line_info, &m_b_wt, &m_m_wt, &f_b_wt, &f_m_wt))return (PARSE_ERROR_GENERIC);

        pr_ptr->m_b_wt = m_b_wt;
        pr_ptr->m_m_wt = m_m_wt;
        pr_ptr->f_b_wt = f_b_wt;
        pr_ptr->f_m_wt = f_m_wt;
    }

    /* Hack -- Process 'F' for flags */
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while(flag_line.length() > 0)
        {
            int slot;

            flag_line = find_one_flag(flag_line, &slot, "player_race", TR1, TN1);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == TR1)         pr_ptr->pr_flags1 |= fl_ptr->flag;
            else if (fl_ptr->set == TR2)    pr_ptr->pr_flags2 |= fl_ptr->flag;
            else if (fl_ptr->set == TR3)    pr_ptr->pr_flags3 |= fl_ptr->flag;
            else if (fl_ptr->set == TN1)    pr_ptr->pr_native |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    /* Hack -- Process 'C' for class choices */
    else if (command == 'C')
    {
        /* There better be a current pr_ptr */
        if (!pr_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        // Use bit commands to add the various acceptable races.  Would have to re-do this if we add more than 10 classes.
        if (line_info.contains('0')) pr_ptr->choice |= (1 << 0);
        if (line_info.contains('1')) pr_ptr->choice |= (1 << 1);
        if (line_info.contains('2')) pr_ptr->choice |= (1 << 2);
        if (line_info.contains('3')) pr_ptr->choice |= (1 << 3);
        if (line_info.contains('4')) pr_ptr->choice |= (1 << 4);
        if (line_info.contains('5')) pr_ptr->choice |= (1 << 5);
        if (line_info.contains('6')) pr_ptr->choice |= (1 << 6);
        if (line_info.contains('7')) pr_ptr->choice |= (1 << 7);
        if (line_info.contains('8')) pr_ptr->choice |= (1 << 8);
        if (line_info.contains('9')) pr_ptr->choice |= (1 << 9);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
 * Initialize the "c_info" array, by parsing an ascii "template" file
 */
int parse_c_info(QString line_info)
{
    /* Current entry */
    static player_class *pc_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);


    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             pc_ptr =& c_info[last_idx];
        }
    }

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             pc_ptr =& c_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->c_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        pc_ptr =& c_info[index_entry];

        // Add the info
        pc_ptr->cl_name = name_entry;

        /* No titles and equipment yet */
        cur_title = 0;
        cur_equip = 0;
    }

    /* Process 'S' for "Stats" (one line only) */
    else if (command == 'S')
    {
        int str, intell, wis, dex, con, chr;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_6_ints(line_info, &str, &intell, &wis, &dex, &con, &chr))return (PARSE_ERROR_GENERIC);

        pc_ptr->c_adj[A_STR] = str;
        pc_ptr->c_adj[A_INT] = intell;
        pc_ptr->c_adj[A_WIS] = wis;
        pc_ptr->c_adj[A_DEX] = dex;
        pc_ptr->c_adj[A_CON] = con;
        pc_ptr->c_adj[A_CHR] = chr;
    }

    /* Process 'C' for "Class Skills" (one line only) */
    else if (command == 'C')
    {
        int dis, dev, sav, stl, srh, fos, thn, thb;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_8_ints(line_info, &dis, &dev, &sav, &stl, &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

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
    else if (command == 'X')
    {
        int dis, dev, sav, stl, srh, fos, thn, thb;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        if (process_8_ints(line_info, &dis, &dev, &sav, &stl, &srh, &fos, &thn, &thb)) return (PARSE_ERROR_GENERIC);

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
    else if (command == 'I')
    {
        int mhp, exp, sense_base;
        long sense_div;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_3_ints_1_long(line_info, &mhp, &exp, &sense_base, &sense_div)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        pc_ptr->c_mhp = mhp;
        pc_ptr->c_exp = exp;
        pc_ptr->sense_base = sense_base;
        pc_ptr->sense_div = sense_div;
    }

    /* Process 'A' for "Attack Info" (one line only) */
    else if (command == 'A')
    {
        int max_attacks, min_weight, att_multiply;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_3_ints(line_info, &max_attacks, &min_weight, &att_multiply)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        pc_ptr->max_attacks = max_attacks;
        pc_ptr->min_weight = min_weight;
        pc_ptr->att_multiply = att_multiply;
    }

    /* Process 'M' for "Magic Info" (one line only) */
    else if (command == 'M')
    {
        int spell_book, spell_first, spell_weight;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_3_ints(line_info, &spell_book, &spell_first, &spell_weight)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        pc_ptr->spell_book = spell_book;
        pc_ptr->spell_first = spell_first;
        pc_ptr->spell_weight = spell_weight;
    }

    /* Process 'B' for "Spell/Prayer book info" */
    else if (command == 'B')
    {
        int spell, level, mana, fail, exp;
        player_magic *mp_ptr;
        magic_type *spell_ptr;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_5_ints(line_info, &spell, &level, &mana, &fail, &exp)) return (PARSE_ERROR_GENERIC);

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
    else if (command == 'T')
    {
        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Limit number of titles */
        if (cur_title > z_info->max_titles)
            return (PARSE_ERROR_TOO_MANY_ARGUMENTS);

        /* Store the text */
        pc_ptr->cl_title[cur_title] = line_info;

        /* Next title */
        cur_title++;
    }

    /* Process 'E' for "Starting Equipment" */
    else if (command == 'E')
    {
        int tval, sval, min, max;

        start_item *e_ptr;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Access the item */
        e_ptr = &pc_ptr->start_items[cur_equip];

        /* Scan for the values */
        if (process_4_ints(line_info, &tval, &sval, &min, &max)) return (PARSE_ERROR_GENERIC);

        if ((min < 0) || (max < 0) || (min > OBJ_MAX_STACK) || (max > OBJ_MAX_STACK))
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
    else if (command == 'F')
    {
        QString flag_line = line_info;

        /* There better be a current pc_ptr */
        if (!pc_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        while(flag_line.length() > 0)
        {

            int slot;

            flag_line = find_one_flag(flag_line, &slot, "player_class", TN1, CF1);

            // Error
            if (slot == -1) return PARSE_ERROR_INVALID_FLAG;

            // Point to the flag info
            flag_name *fl_ptr =& info_flags[slot];

            // Add the appropriate flag
            if (fl_ptr->set == TN1)         pc_ptr->c_native |= fl_ptr->flag;
            else if (fl_ptr->set == CF1)    pc_ptr->flags    |= fl_ptr->flag;

            //found an inalid flag
            else return PARSE_ERROR_INVALID_FLAG;
        }
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
int parse_h_info(QString line_info)
{
    int i;

    /* Current entry */
    static hist_type *h_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             h_ptr =& h_info[last_idx];
        }
    }


    /* Process 'N' for "New/Number" */
    if (command == 'N')
    {
        int prv, nxt, prc, soc;

        /* Hack - get the index */
        i = last_idx + 1;

        /* Verify information */
        if (i >= z_info->h_max) return (PARSE_ERROR_TOO_MANY_ENTRIES);

        /* Save the index */
        last_idx = i;

        /* Point at the "info" */
        h_ptr =& h_info[i];

        /* Scan for the values */
        if (process_4_ints(line_info, &prv, &nxt, &prc, &soc)) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        h_ptr->chart = prv;
        h_ptr->next = nxt;
        h_ptr->roll = prc;
        h_ptr->bonus = soc;
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current h_ptr */
        if (!h_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        h_ptr->h_text = process_description (line_info, h_ptr->h_text);
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
int parse_b_info(QString line_info)
{

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        /* Get the index */
        shop_idx = process_single_value(line_info, FALSE);
        owner_idx = 0;

        return 0;
    }

    /* Process 'S' for "Owner" */
    else if (command == 'S')
    {
        owner_type *ot_ptr;
        int splitter;
        int max_cost;

        if ((shop_idx * z_info->b_max) + owner_idx >= (MAX_STORES * z_info->b_max))
            return PARSE_ERROR_TOO_MANY_ENTRIES;

        ot_ptr =&  b_info[(shop_idx * z_info->b_max) + owner_idx];

        // Isolate the name
        splitter = line_info.lastIndexOf(':');

        QString shop_owner = line_info;

        /* Isolate the name */
        shop_owner.remove(0, splitter);
        shop_owner.remove(QChar(':'));

        // Make sure we have a name
        if (!shop_owner.length()) return (PARSE_ERROR_GENERIC);

        // Just leave the index
        line_info.truncate(splitter);

        QTextStream line_string (&line_info);

        // Turn 'i' into the index#
        line_string >> max_cost;

        // Add the info
        ot_ptr->owner_name = shop_owner;

        ot_ptr->max_cost = max_cost;

        owner_idx++;
        return 0;
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
    }

    /* Oops */
    return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
}



/*
 * Initialize the "q_info" array, by parsing an ascii "template" file
 */
int parse_q_info(QString line_info)
{
    int i;

    /* Current entry */
    static quest_type *q_ptr = NULL;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);

    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             q_ptr =& q_info[last_idx];
        }
    }

    /* Process 'N' for "New/Number/Name" */
    if (command == 'N')
    {
        int index_entry;
        QString name_entry;
        int is_error = process_n_line(line_info, &name_entry, &index_entry, z_info->q_max);

        if (is_error) return (is_error);

        /* Point at the "info" */
        q_ptr =& q_info[index_entry];

        // Add the info
        q_ptr->name = name_entry;
    }

    /* Process 'W' for "Where/What" (one line only) */
    else if (command == 'W')
    {
        int lev, r_idx, u_idx, max;

        /* There better be a current q_ptr */
        if (!q_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Scan for the values */
        if (process_4_ints(line_info, &lev, &r_idx, &u_idx, &max)) return (PARSE_ERROR_GENERIC);

        /* Check quests */
        for (i = 0; i < last_idx; i++)
        {
            /* Check for quest */
            if (lev <= prev_lev) return (PARSE_ERROR_NON_SEQUENTIAL_QUESTS);
        }

        /* Save the values */
        prev_lev = q_ptr->base_level = lev;

        q_ptr->mon_idx = r_idx;

        if(r_idx)
        {
            q_ptr->q_type = QUEST_PERMANENT;

            q_ptr->q_max_num = max;
        }
    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
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
int parse_flavor_info(QString line_info)
{
    int i;

    /* Current entry */
    static flavor_type *flavor_ptr;

    /* Skip comments and blank lines */
    if (line_info.isNull() || line_info.isEmpty() || line_info[0] == '#') return (0);

    /* Verify correct "colon" format */
    if (line_info[1] != ':') return (PARSE_ERROR_GENERIC);

    // Get the commmand and remove the command prefix (ex. N:)
    QChar command = line_info.at(0);
    line_info.remove(0,2);


    // First check if we need to point to a current entry.
    if (command != 'N')
    {
        if (last_idx > -1)
        {
            /* Point at the "info" */
             flavor_ptr =& flavor_info[last_idx];
        }
    }

    /* Process 'N' for "Number" */
    if (command == 'N')
    {
        int tval, sval;

        int count = line_info.count(':');

        /* Either two or three values */
        if ((count != 1) && (count != 2)) return (PARSE_ERROR_GENERIC);

        if (count == 2)
        {
            if (process_3_ints(line_info, &i, &tval, &sval))return (PARSE_ERROR_GENERIC);
        }
        else //(count == 1)
        {
            if (process_2_ints(line_info, &i, &tval))return (PARSE_ERROR_GENERIC);
        }

        /* Verify information */
        if (i <= last_idx) return (PARSE_ERROR_NON_SEQUENTIAL_RECORDS);

        /* Verify information */
        if (i >= z_info->flavor_max) return (PARSE_ERROR_TOO_MANY_ENTRIES);

        /* Save the index */
        last_idx = i;

        /* Point at the "info" */
        flavor_ptr =& flavor_info[i];

        /* Save the tval */
        flavor_ptr->tval = (byte)tval;

        /* Save the sval */
        if (count == 2)
        {

            flavor_ptr->sval = (byte)sval;
        }
        /* Megahack - unknown sval */
        else flavor_ptr->sval = SV_UNKNOWN;
    }

    /* Process 'G' for "Graphics" */
    else if (command == 'G')
    {
        QChar d_char;
        QColor d_color;
        byte color_slot;
        int error;

        /* There better be a current flavor_ptr */
        if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        d_char = process_graphics_line(line_info, &error, &d_color, &color_slot);

        /* Paranoia */
        if (error > 0) return (PARSE_ERROR_GENERIC);

        /* Save the values */
        flavor_ptr->color_num = color_slot;
        flavor_ptr->d_color= d_color;
        flavor_ptr->d_char = d_char;
    }

    /* Process 'D' for "Description" */
    else if (command == 'D')
    {
        /* There better be a current flavor_ptr */
        if (!flavor_ptr) return (PARSE_ERROR_MISSING_RECORD_HEADER);

        /* Verify values */
        if (line_info.length() == 0) return (PARSE_ERROR_GENERIC);

        //Store the text
        flavor_ptr->text = process_description (line_info, flavor_ptr->text);

    }

    else if (command == 'V')
    {
        return (verify_version(line_info));
    }

    else
    {
        /* Oops */
        return (PARSE_ERROR_UNDEFINED_DIRECTIVE);
    }

    /* Success */
    return (0);
}



