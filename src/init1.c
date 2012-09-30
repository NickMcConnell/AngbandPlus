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
 * Monster Blow Methods
 */
static cptr r_info_blow_method[] =
{
        "*",
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
        "*",
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
        "INSANITY",
        "HALLU",
        "PARASITE",
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
        "DEATH_ORB",
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
        "DRAGONRIDER",
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
        "MULTIPLY",
        "S_ANIMAL",
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
        "S_ANIMALS",
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
        "S_DRAGONRIDER",  /* DG : Summon DragonRider */
	"S_KIN",
        "S_HI_DEMON",
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
        "SPIDER",
        "NAZGUL",
        "DG_CURSE",
        "POSSESSOR",
        "NO_DEATH",
        "NO_TARGET",
        "AI_ANNOY",
        "AI_SPECIAL",
        "NEUTRAL",
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
        "NO_CUT",
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
        "HAS_LITE",
        "MIMIC",
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
        "NEVER_GENE",
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
cptr k_info_flags1[] =
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
	"BRAND_COLD"
};

/*
 * Object flags
 */
cptr k_info_flags2[] =
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
        "SENS_FIRE",
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
	"RES_DISEN"
};

/*
 * Trap flags
 */
cptr k_info_flags2_trap[] =
{
        "AUTOMATIC_5",
        "AUTOMATIC_99",
        "KILL_GHOST",
        "TELEPORT_TO",
        "ONLY_DRAGON",
        "ONLY_DEMON",
        "XXX3",
        "XXX3",
        "ONLY_ANIMAL",
        "ONLY_UNDEAD",
        "ONLY_EVIL",
        "XXX3",
	"XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
        "XXX3",
};


/*
 * Object flags
 */
cptr k_info_flags3[] =
{
	"SH_FIRE",
	"SH_ELEC",
        "AUTO_CURSE",
        "DECAY",
	"NO_TELE",
	"NO_MAGIC",
	"WRAITH",
	"TY_CURSE",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"INSTA_ART",
	"FEATHER",
        "LITE1",
	"SEE_INVIS",
        "NORM_ART",
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
cptr k_info_flags4[] =
{
        "NEVER_BLOW",
        "PRECOGNITION",
        "BLACK_BREATH",
        "RECHARGE",
        "FLY",
        "DG_CURSE",
        "COULD2H",
        "MUST2H",
        "LEVELS",
        "CLONE",
        "SPECIAL_GENE",
        "CLIMB",
        "FAST_CAST",
        "CAPACITY",
        "CHARGING",
        "CHEAPNESS",
        "FOUNTAIN",
        "ANTIMAGIC_50",
        "ANTIMAGIC_30",
        "ANTIMAGIC_20",
        "ANTIMAGIC_10",
        "EASY_USE",
        "IM_NETHER",
        "RECHARGED",
        "ULTIMATE",
        "AUTO_ID",
        "LITE2",
        "LITE3",
        "FUEL_LITE",
        "ART_EXP",
        "CURSE_NO_DROP",
        "NO_RECHARGE"
};

/*
 * Object flags
 */
cptr k_info_flags5[] =
{
        "TEMPORARY",
        "DRAIN_MANA",
        "DRAIN_HP",
        "KILL_DEMON",
        "KILL_UNDEAD",
        "CRIT",
        "ATTR_MULTI",
        "WOUNDING",
        "FULL_NAME",
        "LUCK",
        "IMMOVABLE",
	"XXX8X22",
	"XXX8X22",
	"XXX8X02",
	"XXX8X02",
	"XXX8X02",
	"XXX8X02",
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
	"XXX8X02",
	"XXX8X22",
};

/*
 * ESP flags
 */
cptr esp_flags[] =
{
        "ESP_ORC",
        "ESP_TROLL",
        "ESP_DRAGON",
        "ESP_GIANT",
        "ESP_DEMON",
        "ESP_UNDEAD",
        "ESP_EVIL",
        "ESP_ANIMAL",
        "ESP_DRAGONRIDER",
        "ESP_GOOD",
        "ESP_NONLIVING",
        "ESP_UNIQUE",
        "ESP_SPIDER",
	"XXX8X02",
	"XXX8X02",
	"XXX8X02",
	"XXX8X02",
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
	"XXX8X02",
        "ESP_ALL",
};

/* Specially handled properties for ego-items */

static cptr ego_flags[] =
{
        "SUSTAIN",
        "OLD_RESIST",
        "ABILITY",
        "R_ELEM",
        "R_LOW",
        "R_HIGH",
        "R_ANY",
        "R_DRAGON",
        "SLAY_WEAP",
        "DAM_DIE",
        "DAM_SIZE",
        "PVAL_M1",
        "PVAL_M2",
        "PVAL_M3",
        "PVAL_M5",
        "AC_M1",
        "AC_M2",
        "AC_M3",
        "AC_M5",
        "TH_M1",
        "TH_M2",
        "TH_M3",
        "TH_M5",
        "TD_M1",
        "TD_M2",
        "TD_M3",
        "TD_M5",
        "R_P_ABILITY",
        "R_STAT",
        "R_STAT_SUST",
        "R_IMMUNITY",
        "LIMIT_BLOWS"
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
        "TUNNELABLE",
        "WEB",
        "ATTR_MULTI",
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
        "FORCE_DOWN",
        "FORGET",
        "NO_DESTROY",
        "SAND_VEIN",
        "CIRCULAR_ROOMS",
        "EMPTY",
        "DAMAGE_FEAT",
        "FLAT",
        "TOWER",
        "RANDOM_TOWNS",
        "DOUBLE",
        "LIFE_LEVEL",
        "EVOLVE",
        "ADJUST_LEVEL_1",
        "ADJUST_LEVEL_2",
        "NO_RECALL",
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
 * Stores flags
 */
static cptr st_info_flags1[] =
{
        "DEPEND_LEVEL",
        "SHALLOW_LEVEL",
        "MEDIUM_LEVEL",
        "DEEP_LEVEL",
        "RARE",
        "VERY_RARE",
        "COMMON",
        "ALL_ITEM",
        "RANDOM",
        "FORCE_LEVEL",
        "MUSEUM",
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
 * Race flags
 */
cptr rp_info_flags1[] =
{
        "TP",
        "PASS_TREE",
        "RESIST_BLACK_BREATH",
        "NO_STUN",
        "XTRA_MIGHT_BOW",
        "XTRA_MIGHT_XBOW",
        "XTRA_MIGHT_SLING",
        "AC_LEVEL",
        "HURT_LITE",
        "VAMPIRE",
        "UNDEAD",
        "NO_CUT",
        "CORRUPT",
        "NO_FOOD",
        "NO_GOD",
	"ZERO_FAIL",
	"NO_GLOVES",
	"BLESS_WEAPON",
	"BEAM",
        "MONK_SPECIAL",
        "ANTIMAGIC",
        "MOLD_FRIEND",
        "GOD_FRIEND",
        "HACK_MIMIC",
        "INNATE_SPELLS",
        "ANTAGONIC_REALMS",
        "HACK_WEAPON",
        "BACKSTAB",
        "SPREAD_BLOWS",
        "CHAOS_GOD",
        "FULL_ILLUSION",
        "HAS_MANA"
};

/*
 * Race flags
 */
cptr rp_info_flags2[] =
{
        "PET_EXP",
        "ASTRAL",
        "BLADE_SPECIAL",
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
 * Dungeon effect types (used in E:damage:frequency:type entry in d_info.txt)
 */
static struct
{
        cptr name;
        int feat;
} d_info_dtypes[] =
{
        {"ELEC", GF_ELEC},
        {"POISON", GF_POIS},
        {"ACID", GF_ACID},
        {"COLD", GF_COLD},
        {"FIRE", GF_FIRE},
        {"MISSILE", GF_MISSILE},
        {"ARROW", GF_ARROW},
        {"PLASMA", GF_PLASMA},
        {"WATER", GF_WATER},
        {"LITE", GF_LITE},
        {"DARK", GF_DARK},
        {"LITE_WEAK", GF_LITE_WEAK},
        {"LITE_DARK", GF_DARK_WEAK},
        {"SHARDS", GF_SHARDS},
        {"SOUND", GF_SOUND},
        {"CONFUSION", GF_CONFUSION},
        {"FORCE", GF_FORCE},
        {"INERTIA", GF_INERTIA},
        {"MANA", GF_MANA},
        {"METEOR", GF_METEOR},
        {"ICE", GF_ICE},
        {"CHAOS", GF_CHAOS},
        {"NETHER", GF_NETHER},
        {"DISENCHANT", GF_DISENCHANT},
        {"NEXUS", GF_NEXUS},
        {"TIME", GF_TIME},
        {"GRAVITY", GF_GRAVITY},
        {"ROCKET", GF_ROCKET},
        {"NUKE", GF_NUKE},
        {"HOLY_FIRE", GF_HOLY_FIRE},
        {"HELL_FIRE", GF_HELL_FIRE},
        {"DISINTEGRATE", GF_DISINTEGRATE},
        {"DESTRUCTION", GF_DESTRUCTION},
        {"RAISE", GF_RAISE},
        {NULL, 0}
};


/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
int color_char_to_attr(char c)
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
 * Grab one race flag from a textual string
 */
static bool unknown_shut_up = FALSE;
static errr grab_one_class_flag(s32b *choice, cptr what)
{
	int i;
	cptr s;

        /* Scan classes flags */
        for (i = 0; i < max_c_idx && (s = class_info[i].title + c_name); i++)
	{
                if (streq(what, s))
		{
                        (choice[i / 32]) |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        if (!unknown_shut_up) msg_format("Unknown class flag '%s'.", what);

	/* Failure */
	return (1);
}
static errr grab_one_race_allow_flag(s32b *choice, cptr what)
{
	int i;
	cptr s;

        /* Scan classes flags */
        for (i = 0; i < max_rp_idx && (s = race_info[i].title + rp_name); i++)
	{
                if (streq(what, s))
		{
                        (choice[i / 32]) |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        if (!unknown_shut_up) msg_format("(1)Unknown race flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Grab one realm from a textual string
 */
static errr grab_one_player_realm_flag(s32b *realms, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 1; i < MAX_REALM; i++)
	{
                if (streq(what, realm_names[i][0]))
		{
                        (realms[(i - 1) / 32]) |= (1L << (i - 1));
			return (0);
		}
	}

	/* Oops */
        if (!unknown_shut_up) msg_format("Unknown realm name '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Grab one flag from a textual string
 */
static errr grab_one_player_race_flag(s32b *f1, s32b *f2, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, rp_info_flags1[i]))
		{
                        (*f1) |= (1L << i);
			return (0);
		}
        }

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, rp_info_flags2[i]))
		{
                        (*f2) |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        msg_format("(2)Unknown race flag '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_race_kind_flag(s32b *f1, s32b *f2, s32b *f3, s32b *f4, s32b *f5, s32b *esp, cptr what)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
                        (*f1) |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
                        (*f2) |= (1L << i);
			return (0);
		}
	}

        /* Check flags2 -- traps*/
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags2_trap[i]))
		{
                        (*f3) |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
                        (*f3) |= (1L << i);
			return (0);
		}
	}

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        (*f4) |= (1L << i);
			return (0);
		}
	}

        /* Check flags5 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags5[i]))
		{
                        (*f5) |= (1L << i);
			return (0);
		}
	}

        /* Check esp_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, esp_flags[i]))
		{
                        (*esp) |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown object flag '%s'.", what);

	/* Error */
	return (1);
}

/*
 * Initialize the "player" arrays, by parsing an ascii "template" file
 */
errr init_player_info_txt(FILE *fp, char *buf)
{
        int i = 0, powers = 0, lev = 1, tit_idx = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        player_race *rp_ptr = NULL;
        player_race_mod *rmp_ptr = NULL;
        player_class *c_ptr = NULL;
        meta_class_type *mc_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        rp_head->name_size = 0;
        rp_head->text_size = 0;
        rmp_head->name_size = 0;
        rmp_head->text_size = 0;
        c_head->name_size = 0;
        c_head->text_size = 0;

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
                            (v1 != rp_head->v_major) ||
                            (v2 != rp_head->v_minor) ||
                            (v3 != rp_head->v_patch))
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

                /* Reinit error_idx */
                if (buf[0] == 'I')
                {
                        error_idx = -1;
                        continue;
                }

                /* Process 'H' for "History" */
                if (buf[0] == 'H')
		{
                        int idx;
                        char *zz[6];

			/* Scan for the values */
                        if (tokenize(buf+2, 6, zz, ':', ':') != 6) return (1);

                        idx = atoi(zz[0]);
                        bg[idx].roll = atoi(zz[1]);
                        bg[idx].chart = atoi(zz[2]);
                        bg[idx].next = atoi(zz[3]);
                        bg[idx].bonus = atoi(zz[4]);

                        bg[idx].info = ++rp_head->text_size;

                        /* Append chars to the name */
                        strcpy(rp_text + rp_head->text_size, zz[5]);

                        /* Advance the index */
                        rp_head->text_size += strlen(zz[5]);

                        /* Next... */
			continue;
		}


		/* Process 'N' for "New/Number/Name" */
                if ((buf[0] == 'R') && (buf[2] == 'N'))
		{
			/* Find the colon before the name */
                        s = strchr(buf+4, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
                        i = atoi(buf+4);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
                        if (i >= rp_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        rp_ptr = &race_info[i];

			/* Hack -- Verify space */
                        if (rp_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!rp_ptr->title) rp_ptr->title = ++rp_head->name_size;

			/* Append chars to the name */
                        strcpy(rp_name + rp_head->name_size, s);

			/* Advance the index */
                        rp_head->name_size += strlen(s);

                        rp_ptr->powers[0] = rp_ptr->powers[1] = rp_ptr->powers[2] = rp_ptr->powers[3] = -1;
                        powers = 0;
                        lev = 1;

			/* Next... */
			continue;
		}

		/* Process 'D' for "Description" */
                if ((buf[0] == 'R') && (buf[2] == 'D'))
		{
			/* Acquire the text */
                        s = buf+4;

			/* Hack -- Verify space */
                        if (rp_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
                        if (!rp_ptr->desc)
                        {
                                rp_ptr->desc = ++rp_head->text_size;

                                /* Append chars to the name */
                                strcpy(rp_text + rp_head->text_size, s);

                                /* Advance the index */
                                rp_head->text_size += strlen(s);
                        }
                        else
                        {
                                /* Append chars to the name */
                                strcpy(rp_text + rp_head->text_size, format("\n%s", s));

                                /* Advance the index */
                                rp_head->text_size += strlen(s) + 1;
                        }

			/* Next... */
			continue;
		}

                /* Process 'E' for "body parts" */
                if ((buf[0] == 'R') && (buf[2] == 'E'))
		{
                        int s[BODY_MAX], z;

			/* Scan for the values */
                        if (BODY_MAX != sscanf(buf+4, "%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5])) return (1);

                        for (z = 0; z < BODY_MAX; z++)
                                rp_ptr->body_parts[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'R' for "flag level" */
                if ((buf[0] == 'R') && (buf[2] == 'R'))
		{
                        int s[2];

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%d",
                                &s[0], &s[1])) return (1);

                        lev = s[0];
                        rp_ptr->opval[lev] = s[1];

                        /* Next... */
			continue;
		}

                /* Process 'S' for "Stats" */
                if ((buf[0] == 'R') && (buf[2] == 'S'))
		{
                        int s[7], z;

			/* Scan for the values */
                        if (7 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6])) return (1);

                        rp_ptr->luck = s[6];
                        for (z = 0; z < 6; z++)
                                rp_ptr->r_adj[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'Z' for "powers" */
                if ((buf[0] == 'R') && (buf[2] == 'Z'))
		{
                        int i;

			/* Acquire the text */
                        s = buf+4;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        rp_ptr->powers[powers++] = i;

			/* Next... */
			continue;
		}

                /* Process 'K' for "sKills" */
                if ((buf[0] == 'R') && (buf[2] == 'K'))
		{
                        int s[8];

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        rp_ptr->r_dis = s[0];
                        rp_ptr->r_dev = s[1];
                        rp_ptr->r_sav = s[2];
                        rp_ptr->r_stl = s[3];
                        rp_ptr->r_srh = s[4];
                        rp_ptr->r_fos = s[5];
                        rp_ptr->r_thn = s[6];
                        rp_ptr->r_thb = s[7];

                        /* Next... */
			continue;
		}

                /* Process 'M' for "Mods" */
                if ((buf[0] == 'R') && (buf[2] == 'M'))
		{
                        int s[10];

			/* Scan for the values */
                        if (10 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7], &s[8], &s[9])) return (1);

                        rp_ptr->b_age = s[0];
                        rp_ptr->m_age = s[1];
                        rp_ptr->m_b_ht = s[2];
                        rp_ptr->m_m_ht = s[3];
                        rp_ptr->m_b_wt = s[4];
                        rp_ptr->m_m_wt = s[5];
                        rp_ptr->f_b_ht = s[6];
                        rp_ptr->f_m_ht = s[7];
                        rp_ptr->f_b_wt = s[8];
                        rp_ptr->f_m_wt = s[9];

                        /* Next... */
			continue;
		}

                /* Process 'P' for "xtra" */
                if ((buf[0] == 'R') && (buf[2] == 'P'))
		{
                        int s[4];

			/* Scan for the values */
                        if (4 != sscanf(buf+4, "%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3])) return (1);

                        rp_ptr->r_mhp = s[0];
                        rp_ptr->r_exp = s[1];
                        rp_ptr->infra = s[2];
                        rp_ptr->chart = s[3];

                        /* Next... */
			continue;
		}

                /* Process 'G' for "Player flags" (multiple lines) */
                if ((buf[0] == 'R') && (buf[2] == 'G'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_player_race_flag(&rp_ptr->flags1, &rp_ptr->flags2, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'F' for "level Flags" (multiple lines) */
                if ((buf[0] == 'R') && (buf[2] == 'F'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_race_kind_flag(&rp_ptr->oflags1[lev], &rp_ptr->oflags2[lev], &rp_ptr->oflags3[lev], &rp_ptr->oflags4[lev], &rp_ptr->oflags5[lev], &rp_ptr->oesp[lev], s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'C' for "Class choice flags" (multiple lines) */
                if ((buf[0] == 'R') && (buf[2] == 'C'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_class_flag(rp_ptr->choice, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Process 'N' for "New/Number/Name" */
                if ((buf[0] == 'S') && (buf[2] == 'N'))
		{
			/* Find the colon before the name */
                        s = strchr(buf+4, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
                        i = atoi(buf+4);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
                        if (i >= rmp_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        rmp_ptr = &race_mod_info[i];

			/* Hack -- Verify space */
                        if (rmp_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!rmp_ptr->title) rmp_ptr->title = ++rmp_head->name_size;

			/* Append chars to the name */
                        strcpy(rmp_name + rmp_head->name_size, s);

			/* Advance the index */
                        rmp_head->name_size += strlen(s);

                        rmp_ptr->powers[0] = rmp_ptr->powers[1] = rmp_ptr->powers[2] = rmp_ptr->powers[3] = -1;
                        powers = 0;
                        lev = 1;

			/* Next... */
			continue;
		}

		/* Process 'D' for "Description" */
                if ((buf[0] == 'S') && (buf[2] == 'D'))
		{
			/* Acquire the text */
                        s = buf+6;

                        if (buf[4] == 'A') rmp_ptr->place = TRUE;
                        else  rmp_ptr->place = FALSE;

			/* Hack -- Verify space */
                        if (rmp_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
                        if (!rmp_ptr->desc)
                        {
                                rmp_ptr->desc = ++rmp_head->text_size;

                                /* Append chars to the name */
                                strcpy(rmp_text + rmp_head->text_size, s);

                                /* Advance the index */
                                rmp_head->text_size += strlen(s);
                        }
                        else
                        {
                                /* Append chars to the name */
                                strcpy(rmp_text + rmp_head->text_size, format("\n%s", s));

                                /* Advance the index */
                                rmp_head->text_size += strlen(s) + 1;
                        }

			/* Next... */
			continue;
		}

                /* Process 'E' for "body parts" */
                if ((buf[0] == 'S') && (buf[2] == 'E'))
		{
                        int s[BODY_MAX], z;

			/* Scan for the values */
                        if (BODY_MAX != sscanf(buf+4, "%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5])) return (1);

                        for (z = 0; z < BODY_MAX; z++)
                                rmp_ptr->body_parts[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'R' for "flag level" */
                if ((buf[0] == 'S') && (buf[2] == 'R'))
		{
                        int s[2];

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%d",
                                &s[0], &s[1])) return (1);

                        lev = s[0];
                        rmp_ptr->opval[lev] = s[1];

                        /* Next... */
			continue;
		}

                /* Process 'S' for "Stats" */
                if ((buf[0] == 'S') && (buf[2] == 'S'))
		{
                        int s[8], z;

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        rmp_ptr->mana = s[7];
                        rmp_ptr->luck = s[6];
                        for (z = 0; z < 6; z++)
                                rmp_ptr->r_adj[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'Z' for "powers" */
                if ((buf[0] == 'S') && (buf[2] == 'Z'))
		{
                        int i;

			/* Acquire the text */
                        s = buf+4;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        rmp_ptr->powers[powers++] = i;

			/* Next... */
			continue;
		}

                /* Process 'K' for "sKills" */
                if ((buf[0] == 'S') && (buf[2] == 'K'))
		{
                        int s[8];

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        rmp_ptr->r_dis = s[0];
                        rmp_ptr->r_dev = s[1];
                        rmp_ptr->r_sav = s[2];
                        rmp_ptr->r_stl = s[3];
                        rmp_ptr->r_srh = s[4];
                        rmp_ptr->r_fos = s[5];
                        rmp_ptr->r_thn = s[6];
                        rmp_ptr->r_thb = s[7];

                        /* Next... */
			continue;
		}

                /* Process 'M' for "Mods" */
                if ((buf[0] == 'S') && (buf[2] == 'M'))
		{
                        int s[10];

			/* Scan for the values */
                        if (10 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7], &s[8], &s[9])) return (1);

                        rmp_ptr->b_age = s[0];
                        rmp_ptr->m_age = s[1];
                        rmp_ptr->m_b_ht = s[2];
                        rmp_ptr->m_m_ht = s[3];
                        rmp_ptr->m_b_wt = s[4];
                        rmp_ptr->m_m_wt = s[5];
                        rmp_ptr->f_b_ht = s[6];
                        rmp_ptr->f_m_ht = s[7];
                        rmp_ptr->f_b_wt = s[8];
                        rmp_ptr->f_m_wt = s[9];

                        /* Next... */
			continue;
		}

                /* Process 'P' for "xtra" */
                if ((buf[0] == 'S') && (buf[2] == 'P'))
		{
                        int s[3];

			/* Scan for the values */
                        if (3 != sscanf(buf+4, "%d:%d:%d",
                                &s[0], &s[1], &s[2])) return (1);

                        rmp_ptr->r_mhp = s[0];
                        rmp_ptr->r_exp = s[1];
                        rmp_ptr->infra = s[2];

                        /* Next... */
			continue;
		}

                /* Process 'G' for "Player flags" (multiple lines) */
                if ((buf[0] == 'S') && (buf[2] == 'G'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_player_race_flag(&rmp_ptr->flags1, &rmp_ptr->flags2, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'F' for "level Flags" (multiple lines) */
                if ((buf[0] == 'S') && (buf[2] == 'F'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_race_kind_flag(&rmp_ptr->oflags1[lev], &rmp_ptr->oflags2[lev], &rmp_ptr->oflags3[lev], &rmp_ptr->oflags4[lev], &rmp_ptr->oflags5[lev], &rmp_ptr->oesp[lev], s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'A' for "Allowed races" (multiple lines) */
                if ((buf[0] == 'S') && (buf[2] == 'A'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_race_allow_flag(rmp_ptr->choice, s)) return (5);

				/* Start the next entry */
				s = t;
                        }

			/* Next... */
			continue;
		}

                /* Process 'C' for "Class choice flags" (multiple lines) */
                if ((buf[0] == 'S') && (buf[2] == 'C'))
		{
                        s32b choice[2] = {0, 0}, z;

			/* Parse every entry */
                        for (s = buf + 6; *s; )
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
                                if (0 != grab_one_class_flag(choice, s)) return (5);

				/* Start the next entry */
				s = t;
			}

                        for (z = 0; z < 2; z++)
                        {
                                if (buf[4] == 'A') rmp_ptr->pclass[z] |= choice[z];
                                else rmp_ptr->mclass[z] |= choice[z];
                        }

			/* Next... */
			continue;
		}

		/* Process 'N' for "New/Number/Name" */
                if ((buf[0] == 'C') && (buf[2] == 'N'))
		{
			/* Find the colon before the name */
                        s = strchr(buf+4, ':');

			/* Verify that colon */
			if (!s) return (1);

			/* Nuke the colon, advance to the name */
			*s++ = '\0';

			/* Paranoia -- require a name */
			if (!*s) return (1);

			/* Get the index */
                        i = atoi(buf+4);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
                        if (i >= c_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        c_ptr = &class_info[i];

			/* Hack -- Verify space */
                        if (c_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!c_ptr->title) c_ptr->title = ++c_head->name_size;

			/* Append chars to the name */
                        strcpy(c_name + c_head->name_size, s);

			/* Advance the index */
                        c_head->name_size += strlen(s);

                        c_ptr->powers[0] = c_ptr->powers[1] = c_ptr->powers[2] = c_ptr->powers[3] = -1;
                        powers = 0;
                        lev = 1;
                        c_ptr->obj_num = 0;
                        tit_idx = 0;

			/* Next... */
			continue;
		}

		/* Process 'D' for "Description" */
                if ((buf[0] == 'C') && (buf[2] == 'D'))
		{
                        /* Acquire the text */
                        s = buf + 6;

			/* Hack -- Verify space */
			if (c_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

                        switch (buf[4])
                        {
                                case '0':
                                        /* Advance and Save the text index */
                                        if (!c_ptr->desc)
                                        {
                                                c_ptr->desc = ++c_head->text_size;

                                                /* Append chars to the name */
                                                strcpy(c_text + c_head->text_size, s);

                                                /* Advance the index */
                                                c_head->text_size += strlen(s);
                                        }
                                        else
                                        {
                                                /* Append chars to the name */
                                                strcpy(c_text + c_head->text_size, format("\n%s", s));

                                                /* Advance the index */
                                                c_head->text_size += strlen(s) + 1;
                                        }
                                        break;
                                case '1':
                                        /* Advance and Save the text index */
                                        c_ptr->titles[tit_idx++] = ++c_head->text_size;

                                        /* Append chars to the name */
                                        strcpy(c_text + c_head->text_size, s);

                                        /* Advance the index */
                                        c_head->text_size += strlen(s);
                                        break;
                                case '2':
                                        /* Advance and Save the text index */
                                        c_ptr->magic_desc = ++c_head->text_size;

                                        /* Append chars to the name */
                                        strcpy(c_text + c_head->text_size, s);

                                        /* Advance the index */
                                        c_head->text_size += strlen(s);
                                        break;
                                default:
                                        return (6);
                                        break;
                        }

			/* Next... */
			continue;
                }

                /* Process 'O' for "Object birth" */
                if ((buf[0] == 'C') && (buf[2] == 'O'))
		{
                        int s[4];

			/* Scan for the values */
                        if (4 != sscanf(buf+4, "%d:%d:%dd%d",
                                &s[0], &s[1], &s[2], &s[3])) return (1);

                        c_ptr->obj_tval[c_ptr->obj_num] = s[0];
                        c_ptr->obj_sval[c_ptr->obj_num] = s[1];
                        c_ptr->obj_dd[c_ptr->obj_num] = s[2];
                        c_ptr->obj_ds[c_ptr->obj_num++] = s[3];

                        /* Next... */
			continue;
		}

                /* Process 'E' for "body parts" */
                if ((buf[0] == 'C') && (buf[2] == 'E'))
		{
                        int s[BODY_MAX], z;

			/* Scan for the values */
                        if (BODY_MAX != sscanf(buf+4, "%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5])) return (1);

                        for (z = 0; z < BODY_MAX; z++)
                                c_ptr->body_parts[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'R' for "flag level" */
                if ((buf[0] == 'C') && (buf[2] == 'R'))
		{
                        int s[2];

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%d",
                                &s[0], &s[1])) return (1);

                        lev = s[0];
                        c_ptr->opval[lev] = s[1];

                        /* Next... */
			continue;
		}

                /* Process 'C' for "Stats" */
                if ((buf[0] == 'C') && (buf[2] == 'S'))
		{
                        int s[8], z;

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        c_ptr->mana = s[6];
                        c_ptr->extra_blows = s[7];
                        for (z = 0; z < 6; z++)
                                c_ptr->c_adj[z] = s[z];

                        /* Next... */
			continue;
		}

                /* Process 'Z' for "powers" */
                if ((buf[0] == 'C') && (buf[2] == 'Z'))
		{
                        int i;

			/* Acquire the text */
                        s = buf+4;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        c_ptr->powers[powers++] = i;

			/* Next... */
			continue;
		}

                /* Process 'K' for "sKills" */
                if ((buf[0] == 'C') && (buf[2] == 'K'))
		{
                        int s[8];

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        c_ptr->c_dis = s[0];
                        c_ptr->c_dev = s[1];
                        c_ptr->c_sav = s[2];
                        c_ptr->c_stl = s[3];
                        c_ptr->c_srh = s[4];
                        c_ptr->c_fos = s[5];
                        c_ptr->c_thn = s[6];
                        c_ptr->c_thb = s[7];

                        /* Next... */
			continue;
		}

                /* Process 'x' for "Xtra skills" */
                if ((buf[0] == 'C') && (buf[2] == 'X'))
		{
                        int s[8];

			/* Scan for the values */
                        if (8 != sscanf(buf+4, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &s[0], &s[1], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7])) return (1);

                        c_ptr->x_dis = s[0];
                        c_ptr->x_dev = s[1];
                        c_ptr->x_sav = s[2];
                        c_ptr->x_stl = s[3];
                        c_ptr->x_srh = s[4];
                        c_ptr->x_fos = s[5];
                        c_ptr->x_thn = s[6];
                        c_ptr->x_thb = s[7];

                        /* Next... */
			continue;
		}

                /* Process 'Y' for "pets" */
                if ((buf[0] == 'C') && (buf[2] == 'Y'))
		{
                        int s[2];

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%d",
                                &s[0], &s[1])) return (1);

                        c_ptr->max_companion = s[0];
                        c_ptr->max_companion_plus = s[1];

                        /* Next... */
			continue;
		}

                /* Process 'M' for "Magic" */
                if ((buf[0] == 'C') && (buf[2] == 'M'))
		{
                        int s;
                        char c;

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%c",
                                        &s, &c)) return (1);

                        c_ptr->magic_key = s;
                        c_ptr->magic_disrupt = (c == 'T') ? TRUE : FALSE;

                        /* Next... */
			continue;
		}

                /* Process 'P' for "xtra" */
                if ((buf[0] == 'C') && (buf[2] == 'P'))
		{
                        int s[2];

			/* Scan for the values */
                        if (2 != sscanf(buf+4, "%d:%d",
                                &s[0], &s[1])) return (1);

                        c_ptr->c_mhp = s[0];
                        c_ptr->c_exp = s[1];

                        /* Next... */
			continue;
		}

                /* Process 'C' for "sensing" */
                if ((buf[0] == 'C') && (buf[2] == 'C'))
		{
                        s32b s[3];
                        char h, m;

			/* Scan for the values */
                        if (5 != sscanf(buf+4, "%c:%c:%ld:%ld:%ld",
                                &h, &m, &s[0], &s[1], &s[2])) return (1);

                        c_ptr->sense_heavy = (h == 'H') ? TRUE: FALSE;
                        c_ptr->sense_heavy_magic = (m == 'H') ? TRUE: FALSE;
                        c_ptr->sense_base = s[0];
                        c_ptr->sense_pl = s[1];
                        c_ptr->sense_plus = s[2];

                        /* Next... */
			continue;
		}

                /* Process 'B' for "blows" */
                if ((buf[0] == 'C') && (buf[2] == 'B'))
		{
                        int s[3];

			/* Scan for the values */
                        if (3 != sscanf(buf+4, "%d:%d:%d",
                                &s[0], &s[1], &s[2])) return (1);

                        c_ptr->blow_num = s[0];
                        c_ptr->blow_wgt = s[1];
                        c_ptr->blow_mul = s[2];

                        /* Next... */
			continue;
		}

                /* Process 'G' for "Player flags" (multiple lines) */
                if ((buf[0] == 'C') && (buf[2] == 'G'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_player_race_flag(&c_ptr->flags1, &c_ptr->flags2, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'F' for "level Flags" (multiple lines) */
                if ((buf[0] == 'C') && (buf[2] == 'F'))
		{
			/* Parse every entry */
                        for (s = buf + 4; *s; )
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
                                if (0 != grab_one_race_kind_flag(&c_ptr->oflags1[lev], &c_ptr->oflags2[lev], &c_ptr->oflags3[lev], &c_ptr->oflags4[lev], &c_ptr->oflags5[lev], &c_ptr->oesp[lev], s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'A' for "mAgic" (multiple lines) */
                if ((buf[0] == 'C') && (buf[2] == 'A' && ((buf[4] == '1') || (buf[4] == '2'))))
		{
			/* Parse every entry */
                        for (s = buf + 6; *s; )
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
                                if (0 != grab_one_player_realm_flag((buf[4] == '1') ? c_ptr->Mrealm_choices : c_ptr->mrealm_choices, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'A' for "mAgic" */
                if ((buf[0] == 'C') && (buf[2] == 'A')  && (buf[4] == '3'))
		{
                        int s[9];
                        char c[4];
                        int i;

                        c[3] = 0;
			/* Scan for the values */
                        if (11 != sscanf(buf+6, "%d:%c%c%c:%d:%d:%d:%d:%d:%d:%d",
                                         &s[0], &c[0], &c[1], &c[2], &s[2], &s[3], &s[4], &s[5], &s[6], &s[7], &s[8])) return (1);

                        /* Find it in the list */
                        for (i = 0; i < 6; i++)
                        {
                                if (!stricmp(c, stat_names[i])) break;
                        }

                        if (i == 6) return (6);

                        c_ptr->spell_book = s[0];
                        c_ptr->spell_stat = i;
                        c_ptr->spell_lev = s[2];
                        c_ptr->spell_fail = s[3];
                        c_ptr->spell_mana = s[4];
                        c_ptr->spell_first = s[5];
                        c_ptr->spell_weight = s[6];
                        c_ptr->max_spell_level = s[7];
                        c_ptr->magic_max_spell = s[8];

                        /* Next... */
			continue;
		}

		/* Process 'N' for "New/Number/Name" */
                if ((buf[0] == 'M') && (buf[2] == 'N'))
                {
                        /* Find the colon before the name */
                        s = strchr(buf+4, ':');

			/* Verify that colon */
                        if (!s) return (1);

                        /* Nuke the colon, advance to the name */
                        *s++ = '\0';

                        /* Paranoia -- require a name */
                        if (!*s) return (1);

                        /* Get the index */
                        i = atoi(buf+4);

                        /* Verify information */
                        if (i < error_idx) return (4);

                        /* Verify information */
                        if (i >= max_mc_idx) return (2);

                        /* Save the index */
                        error_idx = i;

                        /* Point at the "info" */
                        mc_ptr = &meta_class_info[i];

			/* Append chars to the name */
                        strcpy(mc_ptr->name, s + 2);
                        mc_ptr->color = color_char_to_attr(s[0]);
                        for (powers = 0; powers < max_c_idx; powers++)
                                mc_ptr->classes[powers] = -1;
                        powers = 0;

                        /* Next... */
                        continue;
                }

                /* Process 'C' for "Classes" */
                if ((buf[0] == 'M') && (buf[2] == 'C'))
                {
                        int i;

			/* Acquire the text */
                        s = buf + 4;

                        /* Find it in the list */
                        for (i = 0; i < max_c_idx; i++)
                        {
                                if (!stricmp(s, class_info[i].title + c_name)) break;
                        }

                        if (i == max_c_idx) return (6);

                        mc_ptr->classes[powers++] = i;

			/* Next... */
			continue;
		}

                /* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
        ++rp_head->name_size;
        ++rp_head->text_size;
        ++rmp_head->name_size;
        ++rmp_head->text_size;
        ++c_head->name_size;
        ++c_head->text_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}


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
        u32b default_desc = 0, default_tunnel = 0, default_block = 0;

	/* Current entry */
	feature_type *f_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Prepare the "fake" stuff */
	f_head->name_size = 0;
	f_head->text_size = 0;

        /* Add some fake descs */
        default_desc = ++f_head->text_size;
        strcpy(f_text + f_head->text_size, "a wall blocking your way");
        f_head->text_size += strlen("a wall blocking your way");

        default_tunnel = ++f_head->text_size;
        strcpy(f_text + f_head->text_size, "You cannot tunnel through that.");
        f_head->text_size += strlen("You cannot tunnel through that.");

        default_block = ++f_head->text_size;
        strcpy(f_text + f_head->text_size, "a wall blocking your way");
        f_head->text_size += strlen("a wall blocking your way");

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
                        f_ptr->text = default_desc;
                        f_ptr->block = default_desc;
                        f_ptr->tunnel = default_tunnel;
                        f_ptr->block = default_block;

			/* Next... */
			continue;
		}

		/* There better be a current f_ptr */
		if (!f_ptr) return (3);


                /* Process 'D' for "Descriptions" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
                        s = buf+4;

			/* Hack -- Verify space */
			if (f_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

                        switch (buf[2])
                        {
                                case '0':
                                        /* Advance and Save the text index */
                                        f_ptr->text = ++f_head->text_size;
                                        break;
                                case '1':
                                        /* Advance and Save the text index */
                                        f_ptr->tunnel = ++f_head->text_size;
                                        break;
                                case '2':
                                        /* Advance and Save the text index */
                                        f_ptr->block = ++f_head->text_size;
                                        break;
                                default:
                                        return (6);
                                        break;
                        }

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

                /* Process 'S' for "Shimmer" (one line only) */
                if (buf[0] == 'S')
		{
                        char s0, s1, s2, s3, s4, s5, s6;

			/* Scan for the values */
                        if (7 != sscanf(buf+2, "%c:%c:%c:%c:%c:%c:%c",
                                &s0, &s1, &s2, &s3, &s4, &s5, &s6)) return (1);

			/* Save the values */
                        f_ptr->shimmer[0] = color_char_to_attr(s0);
                        f_ptr->shimmer[1] = color_char_to_attr(s1);
                        f_ptr->shimmer[2] = color_char_to_attr(s2);
                        f_ptr->shimmer[3] = color_char_to_attr(s3);
                        f_ptr->shimmer[4] = color_char_to_attr(s4);
                        f_ptr->shimmer[5] = color_char_to_attr(s5);
                        f_ptr->shimmer[6] = color_char_to_attr(s6);

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

                /* Process 'E' for "Effects" (up to four lines) -SC- */
                if (buf[0] == 'E')
                {
                        int side, dice, freq, type;
                        cptr tmp;
  
                        /* Find the next empty blow slot (if any) */
                        for (i = 0; i < 4; i++) if ((!f_ptr->d_side[i]) &&
                                                    (!f_ptr->d_dice[i])) break;
  
                        /* Oops, no more slots */
                        if (i == 4) return (1);
  
                        /* Scan for the values */
                          if (4 != sscanf(buf+2, "%dd%d:%d:%d",
                                  &dice, &side, &freq, &type))
                        {
                                int j;
                         
                                if (3 != sscanf(buf+2, "%dd%d:%d",
                                                &dice, &side, &freq)) return (1);
                         
                                tmp = buf+2;
                                for (j = 0; j < 2; j++)
                                {
                                        tmp = strchr(tmp, ':');
                                        if (tmp == NULL) return(1);
                                        tmp++;
                                }
                         
                                j = 0;
  
                                while (d_info_dtypes[j].name != NULL)
                                        if (strcmp(d_info_dtypes[j].name, tmp) == 0)
                                        {
                                                f_ptr->d_type[i] = d_info_dtypes[j].feat;
                                                break;
                                        }
                                        else j++;
                         
                                if (d_info_dtypes[j].name == NULL) return(1);
                        }
                        else
                                f_ptr->d_type[i] = type;
  
                        freq *= 10;
                        /* Save the values */
                        f_ptr->d_side[i] = side;
                        f_ptr->d_dice[i] = dice;
                        f_ptr->d_frequency[i] = freq;
  
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

        /* Check flags2 -- traps*/
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags2_trap[i]))
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

        /* Check flags5 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags5[i]))
		{
                        k_ptr->flags5 |= (1L << i);
			return (0);
		}
	}

        /* Check esp_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, esp_flags[i]))
		{
                        k_ptr->esp |= (1L << i);
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

                        /* Needed hack */
                        k_ptr->esp = 0;
                        k_ptr->power = -1;

			/* Next... */
			continue;
		}

		/* There better be a current k_ptr */
		if (!k_ptr) return (3);

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
                        int tval, sval, pval, pval2 = 0;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &tval, &sval, &pval, &pval2))
                        {
                                if (3 != sscanf(buf+2, "%d:%d:%d",
                                        &tval, &sval, &pval))
                                        return (1);
                        }

			/* Save the values */
			k_ptr->tval = tval;
			k_ptr->sval = sval;
			k_ptr->pval = pval;
                        k_ptr->pval2 = pval2;

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

                /* Process 'T' for "arTifact Info" (one line only) */
                if (buf[0] == 'T')
		{
                        int btval, bsval;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                &btval, &bsval)) return (1);

			/* Save the values */
                        k_ptr->btval = btval;
                        k_ptr->bsval = bsval;

			/* Next... */
			continue;
		}

                /* Process 'Z' for "Granted power" */
                if (buf[0] == 'Z')
		{
                        int i;

			/* Acquire the text */
			s = buf+2;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        k_ptr->power = i;

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

        /* Check flags2 -- traps*/
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags2_trap[i]))
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

        /* Check flags5 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags5[i]))
		{
                        a_ptr->flags5 |= (1L << i);
			return (0);
		}
	}

        /* Check esp_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, esp_flags[i]))
		{
                        a_ptr->esp |= (1L << i);
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

                        /* Needed hack */
                        a_ptr->esp = 0;
                        a_ptr->power = -1;

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
                                &tval, &sval, &pval))
                        {
                                return (1);
                        }

			/* Save the values */
                        a_ptr->tval = tval;
                        a_ptr->sval = sval;
                        a_ptr->pval = pval;

                        /* Verify */
                        if (!lookup_kind(tval, sval)) return (6);

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

                /* Process 'Z' for "Granted power" */
                if (buf[0] == 'Z')
		{
                        int i;

			/* Acquire the text */
			s = buf+2;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        a_ptr->power = i;

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
 * Initialize the "set_info" array, by parsing an ascii "template" file
 */
errr init_set_info_txt(FILE *fp, char *buf)
{
        int i;
        int cur_art = 0, cur_num = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
	set_type *set_ptr = NULL;


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
                        int z, y;

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
			if (i >= set_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
			set_ptr = &set_info[i];

			/* Hack -- Verify space */
			if (set_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
			if (!set_ptr->name) set_ptr->name = ++set_head->name_size;

			/* Append chars to the name */
			strcpy(set_name + set_head->name_size, s);

			/* Advance the index */
			set_head->name_size += strlen(s);

                        /* Needed hack */
                        set_ptr->num = 0;
                        set_ptr->num_use = 0;
                        for (z = 0; z < 6; z++)
                        {
                                set_ptr->arts[z].a_idx = 0;
                                set_ptr->arts[z].present = FALSE;
                                for (y = 0; y < 6; y++)
                                {
                                        set_ptr->arts[z].flags1[y] = 0;
                                        set_ptr->arts[z].flags2[y] = 0;
                                        set_ptr->arts[z].flags3[y] = 0;
                                        set_ptr->arts[z].flags4[y] = 0;
                                        set_ptr->arts[z].flags5[y] = 0;
                                        set_ptr->arts[z].esp[y] = 0;
                                        set_ptr->arts[z].pval[y] = 0;
                                }
                        }

			/* Next... */
			continue;
		}

		/* There better be a current set_ptr */
		if (!set_ptr) return (3);

		/* Process 'D' for "Description" */
		if (buf[0] == 'D')
		{
			/* Acquire the text */
			s = buf+2;

			/* Hack -- Verify space */
			if (set_head->text_size + strlen(s) + 8 > fake_text_size) return (7);

			/* Advance and Save the text index */
			if (!set_ptr->desc) set_ptr->desc = ++set_head->text_size;

			/* Append chars to the name */
			strcpy(set_text + set_head->text_size, s);

			/* Advance the index */
			set_head->text_size += strlen(s);

			/* Next... */
			continue;
		}

		/* Process 'P' for "Power" (up to 6) */
		if (buf[0] == 'P')
		{
                        int a_idx, num, pval;
                        int z;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &a_idx, &num, &pval))
                        {
                                return (1);
                        }

                        for (z = 0; z < set_ptr->num; z++)
                                if (set_ptr->arts[z].a_idx == a_idx) break;
                        if (z == set_ptr->num)
                        {
                                set_ptr->num++;
                                set_ptr->arts[z].a_idx = a_idx;
                        }

			/* Save the values */
                        set_ptr->arts[z].pval[num - 1] = pval;
                        cur_art = z;
                        cur_num = num - 1;

			/* Next... */
			continue;
		}

		/* Process 'F' for flags */
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
                                if (0 != grab_one_race_kind_flag(&set_ptr->arts[cur_art].flags1[cur_num],
                                                                 &set_ptr->arts[cur_art].flags2[cur_num],
                                                                 &set_ptr->arts[cur_art].flags3[cur_num],
                                                                 &set_ptr->arts[cur_art].flags4[cur_num],
                                                                 &set_ptr->arts[cur_art].flags5[cur_num],
                                                                 &set_ptr->arts[cur_art].esp[cur_num],
                                                                 s)) return (5);

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
	++set_head->name_size;
	++set_head->text_size;


	/* No version yet */
	if (!okay) return (2);


	/* Success */
	return (0);
}


/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what, int n)
{
	int i;

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
                        e_ptr->flags1[n] |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
                        e_ptr->flags2[n] |= (1L << i);
			return (0);
		}
	}

        /* Check flags2 -- traps */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags2_trap[i]))
		{
                        e_ptr->flags2[n] |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
                        e_ptr->flags3[n] |= (1L << i);
			return (0);
		}
	}

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        e_ptr->flags4[n] |= (1L << i);
			return (0);
		}
	}

        /* Check flags5 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags5[i]))
		{
                        e_ptr->flags5[n] |= (1L << i);
			return (0);
		}
	}

        /* Check esp_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, esp_flags[i]))
		{
                        e_ptr->esp[n] |= (1L << i);
			return (0);
		}
	}

        /* Check ego_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, ego_flags[i]))
		{
                        e_ptr->fego[n] |= (1L << i);
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
        int i, cur_r = -1, cur_t = 0, j;

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

                        /* Needed hack */
                        e_ptr->power = -1;
                        cur_r = -1;
                        cur_t = 0;

                        for (j = 0; j < 6; j++)
                        {
                                e_ptr->tval[j] = 255;
                        }
                        for (j = 0; j < 5; j++)
                        {
                                e_ptr->rar[j] = 0;
                                e_ptr->flags1[j] = 0;
                                e_ptr->flags2[j] = 0;
                                e_ptr->flags3[j] = 0;
                                e_ptr->flags4[j] = 0;
                                e_ptr->flags5[j] = 0;
                                e_ptr->esp[j] = 0;
                        }

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

                /* Process 'T' for "Tval/Sval" (up to 5 lines) */
                if (buf[0] == 'T')
		{
                        int tv, minsv, maxsv;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &tv, &minsv, &maxsv)) return (1);

			/* Save the values */
                        e_ptr->tval[cur_t] = tv;
                        e_ptr->min_sval[cur_t] = minsv;
                        e_ptr->max_sval[cur_t] = maxsv;

                        cur_t++;

			/* Next... */
			continue;
		}

                /* Process 'R' for "flags rarity" (up to 5 lines) */
                if (buf[0] == 'R')
		{
                        int rar;

			/* Scan for the values */
                        if (1 != sscanf(buf+2, "%d",
                                &rar)) return (1);

                        cur_r++;

			/* Save the values */
                        e_ptr->rar[cur_r] = rar;

			/* Next... */
			continue;
		}

		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
			int slot, rating;
                        char pos;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%c:%d:%d",
                                &pos, &slot, &rating)) return (1);

			/* Save the values */
                        /* e_ptr->slot = slot; */
			e_ptr->rating = rating;
                        e_ptr->before = (pos == 'B')?TRUE:FALSE;

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int level, rarity, rarity2;
			long cost;

			/* Scan for the values */
			if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
                                &level, &rarity, &rarity2, &cost)) return (1);

			/* Save the values */
			e_ptr->level = level;
			e_ptr->rarity = rarity;
                        e_ptr->mrarity = rarity2;
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

                /* Process 'Z' for "Granted power" */
                if (buf[0] == 'Z')
		{
                        int i;

			/* Acquire the text */
			s = buf+2;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        e_ptr->power = i;

			/* Next... */
			continue;
		}

		/* Hack -- Process 'F' for flags */
		if (buf[0] == 'F')
		{
                        if (cur_r == -1) return (6);

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
                                if (0 != grab_one_ego_item_flag(e_ptr, s, cur_r)) return (5);

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
 * Grab one flag in a randart_part_type from a textual string
 */
static bool grab_one_randart_item_flag(randart_part_type *ra_ptr, cptr what, char c)
{
        int i;
        s32b *f1, *f2, *f3, *f4, *f5, *esp;

        if (c == 'F')
        {
                f1 = &ra_ptr->flags1;
                f2 = &ra_ptr->flags2;
                f3 = &ra_ptr->flags3;
                f4 = &ra_ptr->flags4;
                f5 = &ra_ptr->flags5;
                esp = &ra_ptr->esp;
        }
        else
        {
                f1 = &ra_ptr->aflags1;
                f2 = &ra_ptr->aflags2;
                f3 = &ra_ptr->aflags3;
                f4 = &ra_ptr->aflags4;
                f5 = &ra_ptr->aflags5;
                esp = &ra_ptr->aesp;
        }

	/* Check flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags1[i]))
		{
                        *f1 |= (1L << i);
			return (0);
		}
	}

	/* Check flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags2[i]))
		{
                        *f2 |= (1L << i);
			return (0);
		}
	}

        /* Check flags2 -- traps */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags2_trap[i]))
		{
                        *f2 |= (1L << i);
			return (0);
		}
	}

	/* Check flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, k_info_flags3[i]))
		{
                        *f3 |= (1L << i);
			return (0);
		}
	}

        /* Check flags4 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags4[i]))
		{
                        *f4 |= (1L << i);
			return (0);
		}
	}

        /* Check flags5 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, k_info_flags5[i]))
		{
                        *f5 |= (1L << i);
			return (0);
		}
	}

        /* Check esp_flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, esp_flags[i]))
		{
                        *esp |= (1L << i);
			return (0);
		}
	}

        /* Check ego_flags */
        if (c == 'F')
        {
                for (i = 0; i < 32; i++)
                {
                        if (streq(what, ego_flags[i]))
                        {
                                ra_ptr->fego |= (1L << i);
                                return (0);
                        }
                }
        }

	/* Oops */
	msg_format("Unknown ego-item flag '%s'.", what);

	/* Error */
	return (1);
}




/*
 * Initialize the "ra_info" array, by parsing an ascii "template" file
 */
errr init_ra_info_txt(FILE *fp, char *buf)
{
        int i, cur_t = 0, j, cur_g = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        randart_part_type *ra_ptr = NULL;


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
                            (v1 != ra_head->v_major) ||
                            (v2 != ra_head->v_minor) ||
                            (v3 != ra_head->v_patch))
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


		/* Process 'G' for "General" (up to 30 lines) */
		if (buf[0] == 'G')
		{
                        int chance, dd, ds, plus;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%dd%d:%d",
                                &chance, &dd, &ds, &plus)) return (1);

			/* Save the values */
                        ra_gen[cur_g].chance = chance;
                        ra_gen[cur_g].dd = dd;
                        ra_gen[cur_g].ds = ds;
                        ra_gen[cur_g].plus = plus;
                        cur_g++;

			/* Next... */
			continue;
		}

                /* Process 'N' for "New/Number" */
		if (buf[0] == 'N')
		{
			/* Get the index */
			i = atoi(buf+2);

			/* Verify information */
			if (i < error_idx) return (4);

			/* Verify information */
                        if (i >= ra_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        ra_ptr = &ra_info[i];

                        /* Needed hack */
                        ra_ptr->power = -1;
                        cur_t = 0;

                        for (j = 0; j < 20; j++)
                        {
                                ra_ptr->tval[j] = 255;
                        }
                        ra_ptr->flags1 = 0;
                        ra_ptr->flags2 = 0;
                        ra_ptr->flags3 = 0;
                        ra_ptr->flags4 = 0;
                        ra_ptr->flags5 = 0;
                        ra_ptr->esp = 0;
                        ra_ptr->fego = 0;

			/* Next... */
			continue;
		}

                /* There better be a current ra_ptr */
                if (!ra_ptr) return (3);

                /* Process 'T' for "Tval/Sval" (up to 5 lines) */
                if (buf[0] == 'T')
		{
                        int tv, minsv, maxsv;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &tv, &minsv, &maxsv)) return (1);

			/* Save the values */
                        ra_ptr->tval[cur_t] = tv;
                        ra_ptr->min_sval[cur_t] = minsv;
                        ra_ptr->max_sval[cur_t] = maxsv;

                        cur_t++;

			/* Next... */
                        continue;
		}

		/* Process 'X' for "Xtra" (one line only) */
		if (buf[0] == 'X')
		{
                        int power, max;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                &power, &max)) return (1);

			/* Save the values */
                        ra_ptr->value = power;
                        ra_ptr->max = max;

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int level, rarity, rarity2;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &level, &rarity, &rarity2)) return (1);

			/* Save the values */
                        ra_ptr->level = level;
                        ra_ptr->rarity = rarity;
                        ra_ptr->mrarity = rarity2;

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

                        ra_ptr->max_to_h = th;
                        ra_ptr->max_to_d = td;
                        ra_ptr->max_to_a = ta;
                        ra_ptr->max_pval = pv;

			/* Next... */
			continue;
		}

                /* Process 'Z' for "Granted power" */
                if (buf[0] == 'Z')
		{
                        int i;

			/* Acquire the text */
			s = buf+2;

                        /* Find it in the list */
                        for (i = 0; i < power_max; i++)
                        {
                                if (!stricmp(s, powers_type[i].name)) break;
                        }

                        if (i == power_max) return (6);

                        ra_ptr->power = i;

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
                                if (0 != grab_one_randart_item_flag(ra_ptr, s, 'F')) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Hack -- Process 'A' for antagonic flags */
		if (buf[0] == 'A')
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
                                if (0 != grab_one_randart_item_flag(ra_ptr, s, 'A')) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


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

			/* Advance the index */
			r_head->name_size += strlen(s);

                        /* HACK -- Those ones HAVE to have a set default value */
                        r_ptr->drops.treasure = OBJ_GENE_TREASURE;
                        r_ptr->drops.combat = OBJ_GENE_COMBAT;
                        r_ptr->drops.magic = OBJ_GENE_MAGIC;
                        r_ptr->drops.tools = OBJ_GENE_TOOL;
                        r_ptr->freq_inate = r_ptr->freq_spell = 0;

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

                        /* Mega debugging hack */
                        if (weap > arms) quit(format("monster %d, %d weapon(s), %d arm(s) !", error_idx, weap, arms));

			/* Next... */
			continue;
		}

                /* Process 'O' for "Object type" (one line only) */
                if (buf[0] == 'O')
		{
                        int treasure, combat, magic, tools;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &treasure, &combat, &magic, &tools)) return (1);

			/* Save the values */
                        r_ptr->drops.treasure = treasure;
                        r_ptr->drops.combat = combat;
                        r_ptr->drops.magic = magic;
                        r_ptr->drops.tools = tools;

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

		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
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
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
	++r_head->name_size;
	++r_head->text_size;


	/* Mega-Hack -- acquire "ghost" */
	r_ptr = &r_info[max_r_idx-1];

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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_ego_flag(monster_ego *re_ptr, cptr what, bool add)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags1[i]))
		{
                        if (add)
                                re_ptr->mflags1 |= (1L << i);
                        else
                                re_ptr->nflags1 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags2[i]))
		{
                        if (add)
                                re_ptr->mflags2 |= (1L << i);
                        else
                                re_ptr->nflags2 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
                        if (add)
                                re_ptr->mflags3 |= (1L << i);
                        else
                                re_ptr->nflags3 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags7[i]))
		{
                        if (add)
                                re_ptr->mflags7 |= (1L << i);
                        else
                                re_ptr->nflags7 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags8 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags8[i]))
		{
                        if (add)
                                re_ptr->mflags8 |= (1L << i);
                        else
                                re_ptr->nflags8 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags9 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
                        if (add)
                                re_ptr->mflags9 |= (1L << i);
                        else
                                re_ptr->nflags9 |= (1L << i);
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
static errr grab_one_spell_ego_flag(monster_ego *re_ptr, cptr what, bool add)
{
	int i;

	/* Scan flags4 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags4[i]))
		{
                        if (add)
                                re_ptr->mflags4 |= (1L << i);
                        else
                                re_ptr->nflags4 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags5 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags5[i]))
		{
                        if (add)
                                re_ptr->mflags5 |= (1L << i);
                        else
                                re_ptr->nflags5 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags6 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags6[i]))
		{
                        if (add)
                                re_ptr->mflags6 |= (1L << i);
                        else
                                re_ptr->nflags6 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}


/* Values in re_info can be fixed, added, substracted or percented */
static byte monster_ego_modify(char c)
{
        switch (c)
        {
                case '+': return MEGO_ADD;
                case '-': return MEGO_SUB;
                case '=': return MEGO_FIX;
                case '%': return MEGO_PRC;
                default:
                {
                        msg_format("Unknown monster ego value modifier %c.", c);
                        return MEGO_ADD;
                }
        }
}

/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_ego_flag(monster_ego *re_ptr, cptr what, bool must)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags1[i]))
		{
                        if (must) re_ptr->flags1 |= (1L << i);
                        else re_ptr->hflags1 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags2[i]))
		{
                        if (must) re_ptr->flags2 |= (1L << i);
                        else re_ptr->hflags2 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
                        if (must) re_ptr->flags3 |= (1L << i);
                        else re_ptr->hflags3 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags7[i]))
		{
                        if (must) re_ptr->flags7 |= (1L << i);
                        else re_ptr->hflags7 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags8 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags8[i]))
		{
                        if (must) re_ptr->flags8 |= (1L << i);
                        else re_ptr->hflags8 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags9 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
                        if (must) re_ptr->flags9 |= (1L << i);
                        else re_ptr->hflags9 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
	msg_format("Unknown monster flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Initialize the "re_info" array, by parsing an ascii "template" file
 */
errr init_re_info_txt(FILE *fp, char *buf)
{
        int i, j;

        byte blow_num = 0;
        int r_char_number = 0, nr_char_number = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        monster_ego *re_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        re_head->name_size = 0;
        re_head->text_size = 0;

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
                            (v1 != re_head->v_major) ||
                            (v2 != re_head->v_minor) ||
                            (v3 != re_head->v_patch))
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
                        if (i >= re_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        re_ptr = &re_info[i];

			/* Hack -- Verify space */
                        if (re_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!re_ptr->name) re_ptr->name = ++re_head->name_size;

			/* Append chars to the name */
                        strcpy(re_name + re_head->name_size, s);

			/* Advance the index */
                        re_head->name_size += strlen(s);

                        /* Some inits */
                        blow_num = 0;
                        r_char_number = 0;
                        nr_char_number = 0;
                        for (j = 0; j < 5; j++) re_ptr->r_char[j] = 0;
                        for (j = 0; j < 5; j++) re_ptr->nr_char[j] = 0;
                        for (j = 0; j < 4; j++)
                        {
                                re_ptr->blow[j].method = 0;
                                re_ptr->blow[j].effect = 0;
                                re_ptr->blow[j].d_dice = 0;
                                re_ptr->blow[j].d_side = 0;
                                re_ptr->blowm[j][0] = MEGO_ADD;
                                re_ptr->blowm[j][1] = MEGO_ADD;
                        }

			/* Next... */
			continue;
		}

                /* There better be a current re_ptr */
                if (!re_ptr) return (3);

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
                        if (buf[2] != '*') sym = buf[2];
                        else sym = MEGO_CHAR_ANY;

			/* Extract the attr */
                        if (buf[4] != '*') tmp = color_char_to_attr(buf[4]);
                        else tmp = MEGO_CHAR_ANY;

			/* Paranoia */
			if (tmp < 0) return (1);

			/* Save the values */
                        re_ptr->d_char = sym;
                        re_ptr->d_attr = tmp;

			/* Next... */
			continue;
		}

		/* Process 'I' for "Info" (one line only) */
		if (buf[0] == 'I')
		{
			int spd, hp1, hp2, aaf, ac, slp;
                        char mspd, mhp1, mhp2, maaf, mac, mslp;

			/* Scan for the other values */
                        if (12 != sscanf(buf+2, "%c%d:%c%dd%c%d:%c%d:%c%d:%c%d",
                                &mspd, &spd, &mhp1, &hp1, &mhp2, &hp2, &maaf, &aaf, &mac, &ac, &mslp, &slp)) return (1);

			/* Save the values */
                        re_ptr->speed = (spd << 2) + monster_ego_modify(mspd);
                        re_ptr->hdice = (hp1 << 2) + monster_ego_modify(mhp1);
                        re_ptr->hside = (hp2 << 2) + monster_ego_modify(mhp2);
                        re_ptr->aaf = (aaf << 2) + monster_ego_modify(maaf);
                        re_ptr->ac = (ac << 2) + monster_ego_modify(mac);
                        re_ptr->sleep = (slp << 2) + monster_ego_modify(mslp);

			/* Next... */
			continue;
		}

		/* Process 'W' for "More Info" (one line only) */
		if (buf[0] == 'W')
		{
                        int lev, rar, wt;
                        char mlev, mwt, mexp, pos;
			long exp;

			/* Scan for the values */
                        if (8 != sscanf(buf+2, "%c%d:%d:%c%d:%c%ld:%c",
                                &mlev, &lev, &rar, &mwt, &wt, &mexp, &exp, &pos)) return (1);

			/* Save the values */
                        re_ptr->level = (lev << 2) + monster_ego_modify(mlev);
                        re_ptr->rarity = rar;
                        re_ptr->weight = (wt << 2) + monster_ego_modify(mwt);
                        re_ptr->mexp = (exp << 2) + monster_ego_modify(mexp);
                        re_ptr->before = (pos == 'B')?TRUE:FALSE;

			/* Next... */
			continue;
		}

		/* Process 'B' for "Blows" (up to four lines) */
		if (buf[0] == 'B')
		{
                        int n1, n2, dice, side;
                        char mdice, mside;

			/* Oops, no more slots */
                        if (blow_num == 4) return (1);

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

			/* Save the method */
                        re_ptr->blow[blow_num].method = n1;

			/* Save the effect */
                        re_ptr->blow[blow_num].effect = n2;

			/* Extract the damage dice and sides */
                        if (4 != sscanf(t, "%c%dd%c%d",
                                &mdice, &dice, &mside, &side)) return (1);

                        re_ptr->blow[blow_num].d_dice = dice;
                        re_ptr->blow[blow_num].d_side = side;
                        re_ptr->blowm[blow_num][0] = monster_ego_modify(mdice);
                        re_ptr->blowm[blow_num][1] = monster_ego_modify(mside);
                        blow_num++;

			/* Next... */
			continue;
		}

                /* Process 'F' for "Flags monster must have" (multiple lines) */
                if (buf[0] == 'F')
		{
                        char r_char;

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
                                        re_ptr->r_char[r_char_number++] = r_char;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_ego_flag(re_ptr, s, TRUE)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'H' for "Flags monster must not have" (multiple lines) */
                if (buf[0] == 'H')
		{
                        char r_char;

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
                                        if(nr_char_number >= 5) continue;

					/* Extract a "frequency" */
                                        re_ptr->nr_char[nr_char_number++] = r_char;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_ego_flag(re_ptr, s, FALSE)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'M' for "Basic Monster Flags" (multiple lines) */
                if (buf[0] == 'M')
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
                                if (0 != grab_one_basic_ego_flag(re_ptr, s, TRUE)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'O' for "Basic Monster -Flags" (multiple lines) */
                if (buf[0] == 'O')
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

                                /* XXX XXX XXX Hack -- Read no flags */
                                if (!strcmp(s, "MF_ALL"))
				{
                                        /* No flags */
                                        re_ptr->nflags1 = re_ptr->nflags2 = re_ptr->nflags3 = re_ptr->nflags7 = re_ptr->nflags8 = re_ptr->nflags9 = 0xFFFFFFFF;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_basic_ego_flag(re_ptr, s, FALSE)) return (5);

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
					/* Extract a "frequency" */
                                        re_ptr->freq_spell = re_ptr->freq_inate = 100 / i;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_spell_ego_flag(re_ptr, s, TRUE)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
                }

                /* Process 'T' for "Spell -Flags" (multiple lines) */
                if (buf[0] == 'T')
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

                                /* XXX XXX XXX Hack -- Read no flags */
                                if (!strcmp(s, "MF_ALL"))
				{
                                        /* No flags */
                                        re_ptr->nflags4 = re_ptr->nflags5 = re_ptr->nflags6 = 0xFFFFFFFF;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_spell_ego_flag(re_ptr, s, FALSE)) return (5);

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
        ++re_head->name_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
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
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_monster_flag(dungeon_info_type *d_ptr, cptr what, byte rule)
{
	int i;

	/* Scan flags1 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, r_info_flags1[i]))
		{
                        d_ptr->rules[rule].mflags1 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags2 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags2[i]))
		{
                        d_ptr->rules[rule].mflags2 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags3 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags3[i]))
		{
                        d_ptr->rules[rule].mflags3 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags7 */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, r_info_flags7[i]))
		{
                        d_ptr->rules[rule].mflags7 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags8 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags8[i]))
		{
                        d_ptr->rules[rule].mflags8 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags9 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags9[i]))
		{
                        d_ptr->rules[rule].mflags9 |= (1L << i);
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
static errr grab_one_spell_monster_flag(dungeon_info_type *d_ptr, cptr what, byte rule)
{
	int i;

	/* Scan flags4 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags4[i]))
		{
                        d_ptr->rules[rule].mflags4 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags5 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags5[i]))
		{
                        d_ptr->rules[rule].mflags5 |= (1L << i);
			return (0);
		}
	}

	/* Scan flags6 */
	for (i = 0; i < 32; i++)
	{
		if (streq(what, r_info_flags6[i]))
		{
                        d_ptr->rules[rule].mflags6 |= (1L << i);
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
        int i, j;

        byte rule_num = 0;

        byte r_char_number = 0;

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

                        /* HACK -- Those ones HAVE to have a set default value */
                        d_ptr->ix = -1;
                        d_ptr->iy = -1;
                        d_ptr->ox = -1;
                        d_ptr->oy = -1;
                        rule_num = -1;
                        r_char_number = 0;
                        for (j = 0; j < 5; j++)
                        {
                                int k;

                                d_ptr->rules[j].mode = DUNGEON_MODE_NONE;
                                d_ptr->rules[j].percent = 0;

                                for (k = 0; k < 5; k++) d_ptr->rules[j].r_char[k] = 0;
                        }

                        /* HACK -- Those ones HAVE to have a set default value */
                        d_ptr->objs.treasure = OBJ_GENE_TREASURE;
                        d_ptr->objs.combat = OBJ_GENE_COMBAT;
                        d_ptr->objs.magic = OBJ_GENE_MAGIC;
                        d_ptr->objs.tools = OBJ_GENE_TOOL;

			/* Next... */
			continue;
		}

                /* There better be a current d_ptr */
                if (!d_ptr) return (3);

                /* Process 'D' for "Description */
                if (buf[0] == 'D')
		{
                        /* Acquire short name */
                        d_ptr->short_name[0] = buf[2];
                        d_ptr->short_name[1] = buf[3];
                        d_ptr->short_name[2] = buf[4];

			/* Acquire the text */
                        s = buf + 6;

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
                        int min_plev, next;
                        int min_alloc, max_chance;

			/* Scan for the values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
                                &min_lev, &max_lev, &min_plev, &next, &min_alloc, &max_chance)) return (1);

			/* Save the values */
                        d_ptr->mindepth = min_lev;
                        d_ptr->maxdepth = max_lev;
                        d_ptr->min_plev = min_plev;
                        d_ptr->next = next;
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
			int i;

			/* Scan for the values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
                                &f1, &p1, &f2, &p2, &f3, &p3)) {
				/* Scan for the values - part ii*/
				if (3 != sscanf(buf+2, "%d:%d:%d", &p1, &p2,
						&p3)) return (1);
				
				/* Save the values */
				d_ptr->floor_percent1[1] = p1;
				d_ptr->floor_percent2[1] = p2;
				d_ptr->floor_percent3[1] = p3;
				
				continue;
			}
			
			/* Save the values */
			d_ptr->floor1 = f1;
			d_ptr->floor2 = f2;
			d_ptr->floor3 = f3;
			
			for (i=0; i<2; i++)
			{
				d_ptr->floor_percent1[i] = p1;
				d_ptr->floor_percent2[i] = p2;
				d_ptr->floor_percent3[i] = p3;
			}
			
			/* Next... */
			continue;
		}

                /* Process 'O' for "Object type" (one line only) */
                if (buf[0] == 'O')
		{
                        int treasure, combat, magic, tools;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &treasure, &combat, &magic, &tools)) return (1);

			/* Save the values */
                        d_ptr->objs.treasure = treasure;
                        d_ptr->objs.combat = combat;
                        d_ptr->objs.magic = magic;
                        d_ptr->objs.tools = tools;

			/* Next... */
			continue;
		}

                /* Process 'A' for "wAll type" (one line only) */
                if (buf[0] == 'A')
		{
                        int w1, w2, w3, outer, inner;
                        int p1, p2, p3;
			int i;

			/* Scan for the values */
                        if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
                                &w1, &p1, &w2, &p2, &w3, &p3, &outer, &inner))
			{
				/* Scan for the values - part ii*/
				if (3 != sscanf(buf+2, "%d:%d:%d", &p1, &p2,
						&p3)) return (1);
				
				/* Save the values */
				d_ptr->fill_percent1[1] = p1;
				d_ptr->fill_percent2[1] = p2;
				d_ptr->fill_percent3[1] = p3;
				continue;
			}

			/* Save the values */
                        d_ptr->fill_type1 = w1;
                        d_ptr->fill_type2 = w2;
                        d_ptr->fill_type3 = w3;
			
			for (i=0; i<2; i++)
			{
				d_ptr->fill_percent1[i] = p1;
				d_ptr->fill_percent2[i] = p2;
				d_ptr->fill_percent3[i] = p3;
			}
			
                        d_ptr->outer_wall = outer;
                        d_ptr->inner_wall = inner;

			/* Next... */
			continue;
		}

                /* Process 'E' for "Effects" (up to four lines) -SC- */
                if (buf[0] == 'E')
                {
                        int side, dice, freq, type;
                        cptr tmp;
  
                        /* Find the next empty blow slot (if any) */
                        for (i = 0; i < 4; i++) if ((!d_ptr->d_side[i]) &&
                                                    (!d_ptr->d_dice[i])) break;
  
                        /* Oops, no more slots */
                        if (i == 4) return (1);
  
                        /* Scan for the values */
                          if (4 != sscanf(buf+2, "%dd%d:%d:%d",
                                  &dice, &side, &freq, &type))
                        {
                                int j;
                         
                                if (3 != sscanf(buf+2, "%dd%d:%d",
                                                &dice, &side, &freq)) return (1);
                         
                                tmp = buf+2;
                                for (j = 0; j < 2; j++)
                                {
                                        tmp = strchr(tmp, ':');
                                        if (tmp == NULL) return(1);
                                        tmp++;
                                }
                         
                                j = 0;
  
                                while (d_info_dtypes[j].name != NULL)
                                        if (strcmp(d_info_dtypes[j].name, tmp) == 0)
                                        {
                                                d_ptr->d_type[i] = d_info_dtypes[j].feat;
                                                break;
                                        }
                                        else j++;
                         
                                if (d_info_dtypes[j].name == NULL) return(1);
                        }
                        else
                                d_ptr->d_type[i] = type;
  
                        freq *= 10;
                        /* Save the values */
                        d_ptr->d_side[i] = side;
                        d_ptr->d_dice[i] = dice;
                        d_ptr->d_frequency[i] = freq;
  
                        /* Next... */
                        continue;
                }

                /* Process 'F' for "Dungeon Flags" (multiple lines) */
		if (buf[0] == 'F')
		{
                        int artif = 0, monst = 0, obj = 0;
                        int ix = -1, iy = -1, ox = -1, oy = -1;

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

                                /* XXX XXX XXX Hack -- Read dungeon in/out coords */
                                if (4 == sscanf(s, "WILD_%d_%d__%d_%d", &ix, &iy, &ox, &oy))
                                {
                                        d_ptr->ix = ix;
                                        d_ptr->iy = iy;
                                        d_ptr->ox = ox;
                                        d_ptr->oy = oy;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

                                /* XXX XXX XXX Hack -- Read Final Object */
                                if (1 == sscanf(s, "FINAL_OBJECT_%d", &obj))
                                {
                                        /* Extract a "Final Artifact" */
                                        d_ptr->final_object = obj;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
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

				/* Parse this entry */
                                if (0 != grab_one_dungeon_flag(d_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'R' for "monster generation Rule" (up to 5 lines) */
                if (buf[0] == 'R')
		{
                        int percent, mode;
                        int z, y, lims[5];

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%d:%d",
                                &percent, &mode)) return (1);

			/* Save the values */
                        r_char_number = 0;
                        rule_num++;

                        d_ptr->rules[rule_num].percent = percent;
                        d_ptr->rules[rule_num].mode = mode;

                        /* Lets remap the flat percents */
                        lims[0] = d_ptr->rules[0].percent;
                        for (y = 1; y <= rule_num; y++)
                        {
                                lims[y] = lims[y - 1] + d_ptr->rules[y].percent;
                        }
                        for (z = 0; z < 100; z++)
                        {
                                for (y = rule_num; y >= 0; y--)
                                {
                                        if (z < lims[y]) d_ptr->rule_percents[z] = y;
                                }
                        }

			/* Next... */
			continue;
		}

                /* Process 'M' for "Basic Flags" (multiple lines) */
                if (buf[0] == 'M')
		{
                        byte r_char;

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
                                        d_ptr->rules[rule_num].r_char[r_char_number++] = r_char;

					/* Start at next entry */
					s = t;

					/* Continue */
					continue;
				}

				/* Parse this entry */
                                if (0 != grab_one_basic_monster_flag(d_ptr, s, rule_num)) return (5);

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

				/* Parse this entry */
                                if (0 != grab_one_spell_monster_flag(d_ptr, s, rule_num)) return (5);

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
 * Grab one race flag from a textual string
 */
static errr grab_one_race_flag(owner_type *ow_ptr, int state, cptr what)
{
	int i;
	cptr s;

        /* Scan race flags */
        unknown_shut_up = TRUE;
        if (!grab_one_race_allow_flag(ow_ptr->races[state], what))
        {
                unknown_shut_up = FALSE;
                return (0);
        }

        /* Scan classes flags */
        if (!grab_one_class_flag(ow_ptr->classes[state], what))
        {
                unknown_shut_up = FALSE;
                return (0);
        }

        /* Scan realms flags */
        if (!grab_one_player_realm_flag(ow_ptr->realms[state], what))
        {
                unknown_shut_up = FALSE;
                return (0);
        }

	/* Oops */
        unknown_shut_up = FALSE;
        msg_format("Unknown race/class/realm flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Grab one store flag from a textual string
 */
static errr grab_one_store_flag(store_info_type *st_ptr, cptr what)
{
	int i;

        /* Scan store flags */
	for (i = 0; i < 32; i++)
	{
                if (streq(what, st_info_flags1[i]))
		{
                        st_ptr->flags1 |= (1L << i);
			return (0);
		}
	}

	/* Oops */
        msg_format("Unknown store flag '%s'.", what);

	/* Failure */
	return (1);
}

/*
 * Initialize the "st_info" array, by parsing an ascii "template" file
 */
errr init_st_info_txt(FILE *fp, char *buf)
{
        int i = 0, item_idx = 0;

	char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        store_info_type *st_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        st_head->name_size = 0;
        st_head->text_size = 0;

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
                            (v1 != st_head->v_major) ||
                            (v2 != st_head->v_minor) ||
                            (v3 != st_head->v_patch))
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
                        if (i >= st_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        st_ptr = &st_info[i];

			/* Hack -- Verify space */
                        if (st_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!st_ptr->name) st_ptr->name = ++st_head->name_size;

			/* Append chars to the name */
                        strcpy(st_name + st_head->name_size, s);

			/* Advance the index */
                        st_head->name_size += strlen(s);

                        /* We are ready for a new set of objects */
                        item_idx = 0;

			/* Next... */
			continue;
		}

                /* There better be a current st_ptr */
                if (!st_ptr) return (3);

                /* Process 'I' for "Items" (multiple lines) */
                if (buf[0] == 'I')
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
                        st_ptr->table[item_idx][1] = atoi(buf+2);

			/* Append chars to the name */
                        st_ptr->table[item_idx++][0] = test_item_name(s);

                        st_ptr->table_num = item_idx;

			/* Next... */
			continue;
		}

                /* Process 'T' for "Tval/sval" */
                if (buf[0] == 'T')
		{
                        int tv1, sv1, rar1;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &rar1, &tv1, &sv1)) return (1);

			/* Get the index */
                        st_ptr->table[item_idx][1] = rar1;
                        /* Hack -- 256 as a sval means all possible items */
                        st_ptr->table[item_idx++][0] = (sv1 < 256)?lookup_kind(tv1, sv1):tv1 + 10000;

                        st_ptr->table_num = item_idx;

                        /* Next... */
			continue;
		}

                /* Process 'G' for "Graphics" one line only) */
                if (buf[0] == 'G')
		{
                        char c, a;
                        int attr;

			/* Scan for the values */
                        if (2 != sscanf(buf+2, "%c:%c",
                                &c, &a)) return (1);

			/* Extract the color */
                        attr = color_char_to_attr(a);

			/* Paranoia */
                        if (attr < 0) return (1);

			/* Save the values */
			st_ptr->d_char = c;
			st_ptr->d_attr = attr;

                        /* Next... */
			continue;
		}

                /* Process 'A' for "Actions" (one line only) */
                if (buf[0] == 'A')
		{
                        int a1, a2, a3, a4, a5, a6;

			/* Scan for the values */
                        if (6 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d",
                                &a1, &a2, &a3, &a4, &a5, &a6)) return (1);

			/* Save the values */
                        st_ptr->actions[0] = a1;
                        st_ptr->actions[1] = a2;
                        st_ptr->actions[2] = a3;
                        st_ptr->actions[3] = a4;
                        st_ptr->actions[4] = a5;
                        st_ptr->actions[5] = a6;

                        /* Next... */
                        continue;
		}

                /* Process 'F' for "store Flags" (multiple lines) */
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
                                if (0 != grab_one_store_flag(st_ptr, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}

                /* Process 'O' for "Owners" (one line only) */
                if (buf[0] == 'O')
		{
                        int a1, a2, a3, a4;

			/* Scan for the values */
                        if (4 != sscanf(buf+2, "%d:%d:%d:%d",
                                &a1, &a2, &a3, &a4)) return (1);

			/* Save the values */
                        st_ptr->owners[0] = a1;
                        st_ptr->owners[1] = a2;
                        st_ptr->owners[2] = a3;
                        st_ptr->owners[3] = a4;

                        /* Next... */
			continue;
		}

                /* Process 'W' for "Extra info" (one line only) */
                if (buf[0] == 'W')
		{
                        int max_obj;

			/* Scan for the values */
                        if (1 != sscanf(buf+2, "%d",
                                &max_obj)) return (1);

			/* Save the values */
                        if (max_obj > STORE_INVEN_MAX) max_obj = STORE_INVEN_MAX;
                        st_ptr->max_obj = max_obj;

                        /* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
        ++st_head->name_size;
        ++st_head->text_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}

/*
 * Initialize the "ba_info" array, by parsing an ascii "template" file
 */
errr init_ba_info_txt(FILE *fp, char *buf)
{
        int i = 0;

        char *s;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        store_action_type *ba_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        ba_head->name_size = 0;
        ba_head->text_size = 0;

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
                            (v1 != ba_head->v_major) ||
                            (v2 != ba_head->v_minor) ||
                            (v3 != ba_head->v_patch))
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
                        if (i >= ba_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        ba_ptr = &ba_info[i];

			/* Hack -- Verify space */
                        if (ba_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!ba_ptr->name) ba_ptr->name = ++ba_head->name_size;

			/* Append chars to the name */
                        strcpy(ba_name + ba_head->name_size, s);

			/* Advance the index */
                        ba_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* There better be a current ba_ptr */
                if (!ba_ptr) return (3);

                /* Process 'C' for "Costs" (one line only) */
                if (buf[0] == 'C')
		{
                        int ch, cn, cl;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &ch, &cn, &cl)) return (1);

			/* Save the values */
                        ba_ptr->costs[STORE_HATED] = ch;
                        ba_ptr->costs[STORE_NORMAL] = cn;
                        ba_ptr->costs[STORE_LIKED] = cl;

                        /* Next... */
			continue;
		}

                /* Process 'I' for "Infos" (one line only) */
                if (buf[0] == 'I')
		{
                        int act, act_res;
                        char letter;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%c",
                                &act, &act_res, &letter)) return (1);

			/* Save the values */
                        ba_ptr->action = act;
                        ba_ptr->action_restr = act_res;
                        ba_ptr->letter = letter;

                        /* Next... */
			continue;
		}

		/* Oops */
		return (6);
	}


	/* Complete the "name" and "text" sizes */
        ++ba_head->name_size;
        ++ba_head->text_size;

	/* No version yet */
	if (!okay) return (2);

	/* Success */
	return (0);
}

/*
 * Initialize the "ow_info" array, by parsing an ascii "template" file
 */
errr init_ow_info_txt(FILE *fp, char *buf)
{
        int i;

        char *s, *t;

	/* Not ready yet */
	bool okay = FALSE;

	/* Current entry */
        owner_type *ow_ptr = NULL;


	/* Just before the first record */
	error_idx = -1;

	/* Just before the first line */
	error_line = -1;


	/* Start the "fake" stuff */
        ow_head->name_size = 0;
        ow_head->text_size = 0;

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
                            (v1 != ow_head->v_major) ||
                            (v2 != ow_head->v_minor) ||
                            (v3 != ow_head->v_patch))
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
                        if (i >= ow_head->info_num) return (2);

			/* Save the index */
			error_idx = i;

			/* Point at the "info" */
                        ow_ptr = &ow_info[i];

			/* Hack -- Verify space */
                        if (ow_head->name_size + strlen(s) + 8 > fake_name_size) return (7);

			/* Advance and Save the name index */
                        if (!ow_ptr->name) ow_ptr->name = ++ow_head->name_size;

			/* Append chars to the name */
                        strcpy(ow_name + ow_head->name_size, s);

			/* Advance the index */
                        ow_head->name_size += strlen(s);

			/* Next... */
			continue;
		}

                /* There better be a current ow_ptr */
                if (!ow_ptr) return (3);


                /* Process 'C' for "Costs" (one line only) */
                if (buf[0] == 'C')
		{
                        int ch, cn, cl;

			/* Scan for the values */
                        if (3 != sscanf(buf+2, "%d:%d:%d",
                                &ch, &cn, &cl)) return (1);

			/* Save the values */
                        ow_ptr->costs[STORE_HATED] = ch;
                        ow_ptr->costs[STORE_NORMAL] = cn;
                        ow_ptr->costs[STORE_LIKED] = cl;

                        /* Next... */
			continue;
		}

                /* Process 'I' for "Info" (multiple lines line only) */
                if (buf[0] == 'I')
		{
                        int cost, max_inf, min_inf, haggle, insult;

			/* Scan for the values */
                        if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
                                &cost, &max_inf, &min_inf, &haggle, &insult)) return (1);

			/* Save the values */
                        ow_ptr->max_cost = cost;
                        ow_ptr->max_inflate = max_inf;
                        ow_ptr->min_inflate = min_inf;
                        ow_ptr->haggle_per = haggle;
                        ow_ptr->insult_max = insult;

                        /* Next... */
			continue;
		}
                
                /* Process 'L' for "Liked races/classes/realms" (multiple lines) */
                if (buf[0] == 'L')
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
                                if (0 != grab_one_race_flag(ow_ptr, STORE_LIKED, s)) return (5);

				/* Start the next entry */
				s = t;
			}

			/* Next... */
			continue;
		}
                /* Process 'H' for "Hated races/classes/realms" (multiple lines) */
                if (buf[0] == 'H')
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
                                if (0 != grab_one_race_flag(ow_ptr, STORE_HATED, s)) return (5);

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
        ++ow_head->name_size;
        ++ow_head->text_size;

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

                        /* To acces it easily from the map structure */
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
        int             bx, by;                 /* For between gates */
        int             mimic;                  /* Mimiced features */
        bool            ok;
        bool            defined;
};
static bool meta_sleep = TRUE;

static dungeon_grid letter[255];

/*
 * Parse a sub-file of the "extra info"
 */
bool process_dungeon_file_full = FALSE;
static errr process_dungeon_file_aux(char *buf, int *yval, int *xval, int xvalstart, int ymax, int xmax)
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
                return (process_dungeon_file(buf + 2, yval, xval, ymax, xmax, FALSE));
	}

        /* Process "N:<sleep>" */
        if (buf[0] == 'N')
	{
		int num;

                if ((num = tokenize(buf + 2, 1, zz, ':', '/')) > 0)
		{
                        meta_sleep = atoi(zz[0]);
                }

		return (0);
        }

        /* Process "F:<letter>:<terrain>:<cave_info>:<monster>:<object>:<ego>:<artifact>:<trap>:<special>:<mimic>" -- info for dungeon grid */
	if (buf[0] == 'F')
	{
		int num;

                if ((num = tokenize(buf+2, 10, zz, ':', '/')) > 1)
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
                        letter[index].mimic = 0;
                        letter[index].ok = TRUE;
                        letter[index].defined = TRUE;

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

                        if (num > 9)
                        {
                                letter[index].mimic = atoi(zz[9]);
                        }

			return (0);
		}
	}

	/* Process "D:<dungeon>" -- info for the cave grids */
	else if (buf[0] == 'D')
	{
		int x;

		object_type object_type_body;

		/* Acquire the text */
		char *s = buf+2;

		/* Length of the text */
		int len = strlen(s);

		int y = *yval;
                *xval = xvalstart;
		for (x = *xval, i = 0; ((x < xmax) && (i < len)); x++, s++, i++)
		{
			/* Access the grid */
			cave_type *c_ptr = &cave[y][x];

			int idx = s[0];

			int object_index = letter[idx].object;
			int monster_index = letter[idx].monster;
			int random = letter[idx].random;
			int artifact_index = letter[idx].artifact;

                        if (!letter[idx].ok) msg_format("Warning '%c' not defined but used.", idx);

                        if (init_flags & INIT_GET_SIZE) continue;

                        /* use the plasma generator wilderness */
                        if (((!dun_level) || (!letter[idx].defined)) && (idx == ' ')) continue;

			/* Lay down a floor */
			c_ptr->feat = letter[idx].feature;
                        c_ptr->mimic = letter[idx].mimic;

			/* Cave info */
			c_ptr->info = letter[idx].cave_info;

			/* Create a monster */
			if (random & RANDOM_MONSTER)
			{
				int level = monster_level;
					
				monster_level = quest[p_ptr->inside_quest].level + monster_index;

                                place_monster(y, x, meta_sleep, FALSE);

				monster_level = level;
			}
			else if (monster_index)
			{
				/* Place it */
                                m_allow_special[monster_index] = TRUE;
                                place_monster_aux(y, x, monster_index, meta_sleep, FALSE, MSTATUS_ENEMY);
                                m_allow_special[monster_index] = FALSE;
			}

			/* Object (and possible trap) */
			if ((random & RANDOM_OBJECT) && (random & RANDOM_TRAP))
			{
				int level = object_level;

				object_level = quest[p_ptr->inside_quest].level;

				/*
				 * Random trap and random treasure defined
				 * 25% chance for trap and 75% chance for object
				 */
				if (rand_int(100) < 75)
				{
					place_object(y, x, FALSE, FALSE);
				}
				else
				{
					place_trap(y, x);
				}

				object_level = level;
			}
			else if (random & RANDOM_OBJECT)
			{
				/* Create an out of deep object */
				if (object_index)
				{
					int level = object_level;

					object_level = quest[p_ptr->inside_quest].level + object_index;
					if (rand_int(100) < 75)
						place_object(y, x, FALSE, FALSE);
					else if (rand_int(100) < 80)
						place_object(y, x, TRUE, FALSE);
					else
						place_object(y, x, TRUE, TRUE);

					object_level = level;
				}
				else if (rand_int(100) < 75)
				{
					place_object(y, x, FALSE, FALSE);
				}
				else if (rand_int(100) < 80)
				{
					place_object(y, x, TRUE, FALSE);
				}
				else
				{
					place_object(y, x, TRUE, TRUE);
				}
			}
			/* Random trap */
			else if (random & RANDOM_TRAP)
			{
				place_trap(y, x);
			}
			else if (object_index)
			{
				/* Get local object */
				object_type *o_ptr = &object_type_body;

                                k_allow_special[object_index] = TRUE;

				/* Create the item */
				object_prep(o_ptr, object_index);

				/* Apply magic (no messages, no artifacts) */
                                apply_magic(o_ptr, dun_level, FALSE, TRUE, FALSE);

                                k_allow_special[object_index] = FALSE;

				drop_near(o_ptr, -1, y, x);
			}

			/* Artifact */
			if (artifact_index)
			{
				int I_kind = 0;

				artifact_type *a_ptr = &a_info[artifact_index];

				object_type forge;

				/* Get local object */
				object_type *q_ptr = &forge;

                                a_allow_special[artifact_index] = TRUE;

				/* Wipe the object */
				object_wipe(q_ptr);

				/* Acquire the "kind" index */
				I_kind = lookup_kind(a_ptr->tval, a_ptr->sval);

				/* Create the artifact */
				object_prep(q_ptr, I_kind);

				/* Save the name */
				q_ptr->name1 = artifact_index;

				/* Extract the fields */
				q_ptr->pval = a_ptr->pval;
				q_ptr->ac = a_ptr->ac;
				q_ptr->dd = a_ptr->dd;
				q_ptr->ds = a_ptr->ds;
				q_ptr->to_a = a_ptr->to_a;
				q_ptr->to_h = a_ptr->to_h;
				q_ptr->to_d = a_ptr->to_d;
				q_ptr->weight = a_ptr->weight;

				random_artifact_resistance(q_ptr);

				a_info[artifact_index].cur_num = 1;

                                a_allow_special[artifact_index] = FALSE;

				/* Drop the artifact */
				drop_near(q_ptr, -1, y, x);
			}

			/* Terrain special */
                        if (letter[idx].special == -1)
                        {
                                if (!letter[idx].bx)
                                {
                                        letter[idx].bx = x;
                                        letter[idx].by = y;
                                }
                                else
                                {
                                        c_ptr->special = (letter[idx].by << 8) + letter[idx].bx;
                                        cave[letter[idx].by][letter[idx].bx].special = (y << 8) + x;
                                }
                        }
                        else
                        {
                                c_ptr->special = letter[idx].special;
                        }
		}
                if ((process_dungeon_file_full) && (*xval < x)) *xval = x;
		(*yval)++;

		return (0);
	}

	/* Process "W:<command>: ..." -- info for the wilderness */
	else if (buf[0] == 'W')
	{
		/* Process "W:D:<layout> */
		/* Layout of the wilderness */
		if (buf[2] == 'D')
		{
                        int x;
                        char i;

			/* Acquire the text */
                        char *s = buf+4;

			int y = *yval;

                        for(x = 0; x < max_wild_x; x++)
                        {
                                if (1 != sscanf(s + x, "%c", &i)) return (1);
                                wild_map[y][x].feat = wildc2i[(int)i];
                        }

			(*yval)++;

			return (0);
		}
                /* Process "M:<plus>:<line>" -- move line lines */
                else if (buf[2] == 'M')
                {
                        if (tokenize(buf+4, 2, zz, ':', '/') == 2)
                        {
                                if (atoi(zz[0]))
                                {
                                        (*yval) += atoi(zz[1]);
                                }
                                else
                                {
                                        (*yval) -= atoi(zz[1]);
                                }
                        }
                        else
                        {
                                return (1);
			}
                        return (0);
                }
		/* Process "W:P:<x>:<y> - starting position in the wilderness */
		else if (buf[2] == 'P')
		{
			if ((p_ptr->wilderness_x == 0) &&
			    (p_ptr->wilderness_y == 0))
			{
                                if (tokenize(buf+4, 2, zz, ':', '/') == 2)
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
                /* Process "W:E:<dungeon>:<y>:<x> - entrance to the dungeon <dungeon> */
                else if (buf[2] == 'E')
		{
                        if (tokenize(buf+4, 3, zz, ':', '/') == 3)
                        {
                                wild_map[atoi(zz[1])][atoi(zz[2])].entrance = 1000 + atoi(zz[0]);
                        }
                        else
                        {
                                return (1);
			}

			return (0);
		}
	}

	/* Process "P:<x>:<y>" -- player position */
	else if (buf[0] == 'P')
	{
		if (init_flags & INIT_CREATE_DUNGEON)
		{
                        if (tokenize(buf+2, 2, zz, ':', '/') == 2)
			{
				/* Place player in a quest level */
                                if (p_ptr->inside_quest || (init_flags & INIT_POSITION))
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

	/* Process "M:<type>:<maximum>" -- set maximum values */
	else if (buf[0] == 'M')
	{
                if (tokenize(buf+2, 3, zz, ':', '/') >= 2)
		{
			/* Maximum towns */
			if (zz[0][0] == 'T')
			{
				max_towns = atoi(zz[1]); 
			}

                        /* Maximum real towns */
                        if (zz[0][0] == 't')
			{
                                max_real_towns = atoi(zz[1]); 
			}

			/* Maximum r_idx */
			else if (zz[0][0] == 'R')
			{
				max_r_idx = atoi(zz[1]); 
			}

                        /* Maximum re_idx */
                        else if (zz[0][0] == 'r')
			{
                                max_re_idx = atoi(zz[1]); 
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

                        /* Maximum ra_idx */
                        else if (zz[0][0] == 'Z')
			{
                                max_ra_idx = atoi(zz[1]); 
			}

			/* Maximum o_idx */
			else if (zz[0][0] == 'O')
			{
				max_o_idx = atoi(zz[1]); 
			}

                        /* Maximum player types */
                        else if (zz[0][0] == 'P')
			{
                                if (zz[1][0] == 'R')
                                {
                                        max_rp_idx = atoi(zz[2]); 
                                }
                                else if (zz[1][0] == 'S')
                                {
                                        max_rmp_idx = atoi(zz[2]); 
                                }
                                else if (zz[1][0] == 'C')
                                {
                                        max_c_idx = atoi(zz[2]);
                                }
                                else if (zz[1][0] == 'M')
                                {
                                        max_mc_idx = atoi(zz[2]);
                                }
                                else if (zz[1][0] == 'H')
                                {
                                        max_bg_idx = atoi(zz[2]); 
                                }
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

                        /* Maximum wf_idx */
                        else if (zz[0][0] == 'W')
			{
                                max_wf_idx = atoi(zz[1]); 
			}

                        /* Maximum ba_idx */
                        else if (zz[0][0] == 'B')
			{
                                max_ba_idx = atoi(zz[1]); 
			}

                        /* Maximum st_idx */
                        else if (zz[0][0] == 'S')
			{
                                max_st_idx = atoi(zz[1]); 
			}

                        /* Maximum set_idx */
                        else if (zz[0][0] == 's')
			{
                                max_set_idx = atoi(zz[1]);
			}

                        /* Maximum ow_idx */
                        else if (zz[0][0] == 'N')
			{
                                max_ow_idx = atoi(zz[1]); 
			}

                        /* Maximum wilderness x size */
                        else if (zz[0][0] == 'X')
			{
                                max_wild_x = atoi(zz[1]); 
			}

                        /* Maximum wilderness y size */
                        else if (zz[0][0] == 'Y')
			{
                                max_wild_y = atoi(zz[1]); 
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
                                v = rp_ptr->title + rp_name;
			}

                        /* Race Mod */
                        else if (streq(b+1, "RACEMOD"))
			{
                                v = rmp_ptr->title + rmp_name;
			}

			/* Class */
			else if (streq(b+1, "CLASS"))
			{
				v = cp_ptr->title + c_name;
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

                        /* DAYTIME status */
                        else if (prefix(b+1, "DAYTIME"))
			{
                                sprintf(tmp, "%d", ((bst(HOUR, turn) >= 6) && (bst(HOUR, turn) < 18)) ? 1 : 0);
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
                                v = "PERNANGBAND";
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


errr process_dungeon_file(cptr name, int *yval, int *xval, int ymax, int xmax, bool init)
{
	FILE *fp;

	char buf[1024];

        int num = -1, i;

	errr err = 0;

	bool bypass = FALSE;

        /* Save the start since it ought to be modified */
        int xmin = *xval;

        if (init)
        {
                meta_sleep = TRUE;
                for (i = 0; i < 255; i++)
                {
                        letter[i].defined = FALSE;
                        if (i == ' ') letter[i].ok = TRUE;
                        else letter[i].ok = FALSE;
                        letter[i].bx = 0;
                        letter[i].by = 0;
                }
        }

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_EDIT, name);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* No such file */
        if (!fp)
        {
                msg_format("Cannot find file %s at %s", name, buf);
                return (-1);
        }

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
                        (void)process_dungeon_file(buf + 2, yval, xval, ymax, xmax, FALSE);

			/* Continue */
			continue;
		}


		/* Process the line */
                err = process_dungeon_file_aux(buf, yval, xval, xmin, ymax, xmax);

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
