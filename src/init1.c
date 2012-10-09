/* Files: init1.c */

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
 * Hack -- size of the "fake" arrays
 */
extern u16b fake_name_size;
extern u16b fake_text_size;

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
   "DROP_CHOSEN",
   "RF1_XXX33",
   "RF1_XXX34",
   "RF1_XXX35",
   "RF1_XXX36",
   "RF1_XXX37",
   "RF1_XXX38",
   "RF1_XXX39",
   "RF1_XXX40",
   "RF1_XXX41",
   "RF1_XXX42",
   "RF1_XXX43",
   "RF1_XXX44",
   "RF1_XXX45",
   "RF1_XXX46",
   "RF1_XXX47",
   "RF1_XXX48",
   "RF1_XXX49",
   "RF1_XXX50",
   "RF1_XXX51",
   "RF1_XXX52",
   "RF1_XXX53",
   "RF1_XXX54",
   "RF1_XXX55",
   "RF1_XXX56",
   "RF1_XXX57",
   "RF1_XXX58",
   "RF1_XXX59",
   "RF1_XXX60",
   "RF1_XXX61",
   "RF1_XXX62",
   "RF1_XXX63",
   "RF1_XXX64"
};

/*
 * Monster race flags
 */
static cptr r_info_flags2[] =
{
   "STUPID",
   "SMART",
   "XXX1X2",
   "XXX2X2",
   "INVISIBLE",
   "COLD_BLOOD",
   "EMPTY_MIND",
   "WEIRD_MIND",
   "MULTIPLY",
   "REGENERATE",
   "XXX3X2",
   "XXX4X2",
   "POWERFUL",
   "XXX5X2",
   "XXX7X2",
   "BUILD_WALL",
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
   "BRAIN_8",
   "RF2_XXX33",
   "RF2_XXX34",
   "RF2_XXX35",
   "RF2_XXX36",
   "RF2_XXX37",
   "RF2_XXX38",
   "RF2_XXX39",
   "RF2_XXX40",
   "RF2_XXX41",
   "RF2_XXX42",
   "RF2_XXX43",
   "RF2_XXX44",
   "RF2_XXX45",
   "RF2_XXX46",
   "RF2_XXX47",
   "RF2_XXX48",
   "RF2_XXX49",
   "RF2_XXX50",
   "RF2_XXX51",
   "RF2_XXX52",
   "RF2_XXX53",
   "RF2_XXX54",
   "RF2_XXX55",
   "RF2_XXX56",
   "RF2_XXX57",
   "RF2_XXX58",
   "RF2_XXX59",
   "RF2_XXX60",
   "RF2_XXX61",
   "RF2_XXX62",
   "RF2_XXX63",
   "RF2_XXX64"
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
   "WYRM",
   "XXX2X3",
   "XXX3X3",
   "XXX4X3",
   "HURT_LITE",
   "HURT_ROCK",
   "HURT_FIRE",
   "HURT_COLD",
   "IM_ACID",
   "IM_ELEC",
   "IM_FIRE",
   "IM_COLD",
   "IM_POIS",
   "NO_FLOOR",
   "RES_NETH",
   "RES_WATE",
   "RES_PLAS",
   "RES_NEXU",
   "RES_DISE",
   "XXX6X3",
   "NO_FEAR",
   "NO_STUN",
   "NO_CONF",
   "NO_SLEEP",
   "RF3_XXX33",
   "RF3_XXX34",
   "RF3_XXX35",
   "RF3_XXX36",
   "RF3_XXX37",
   "RF3_XXX38",
   "RF3_XXX39",
   "RF3_XXX40",
   "RF3_XXX41",
   "RF3_XXX42",
   "RF3_XXX43",
   "RF3_XXX44",
   "RF3_XXX45",
   "RF3_XXX46",
   "RF3_XXX47",
   "RF3_XXX48",
   "RF3_XXX49",
   "RF3_XXX50",
   "RF3_XXX51",
   "RF3_XXX52",
   "RF3_XXX53",
   "RF3_XXX54",
   "RF3_XXX55",
   "RF3_XXX56",
   "RF3_XXX57",
   "RF3_XXX58",
   "RF3_XXX59",
   "RF3_XXX60",
   "RF3_XXX61",
   "RF3_XXX62",
   "RF3_XXX63",
   "RF3_XXX64"
};

/*
 * Monster race flags
 */
static cptr r_info_flags4[] =
{
   "SHRIEK",
   "XXX2X4",
   "XXX3X4",
   "ARROW_1",
   "ARROW_2",
   "ARROW_3",
   "ARROW_4",
   "ARROW_5",
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
   "BR_PFIRE",
   "BOLT_1",
   "BOLT_2",
   "BOLT_3",
   "BOLT_4",
   "BOLT_5",
   "SHOT_1",
   "SHOT_2",
   "SHOT_3",
   "SHOT_4",
   "SHOT_5",
   "RF4_XXX40",
   "RF4_XXX41",
   "RF4_XXX42",
   "RF4_XXX43",
   "RF4_XXX44",
   "RF4_XXX45",
   "RF4_XXX46",
   "RF4_XXX47",
   "RF4_XXX48",
   "RF4_XXX49",
   "RF4_XXX50",
   "RF4_XXX51",
   "RF4_XXX52",
   "RF4_XXX53",
   "RF4_XXX54",
   "RF4_XXX55",
   "RF4_XXX56",
   "RF4_XXX57",
   "RF4_XXX58",
   "RF4_XXX59",
   "RF4_XXX60",
   "RF4_XXX61",
   "RF4_XXX62",
   "RF4_XXX63",
   "RF4_XXX64"
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
   "RF5_XXX33",
   "RF5_XXX34",
   "RF5_XXX35",
   "RF5_XXX36",
   "RF5_XXX37",
   "RF5_XXX38",
   "RF5_XXX39",
   "RF5_XXX40",
   "RF5_XXX41",
   "RF5_XXX42",
   "RF5_XXX43",
   "RF5_XXX44",
   "RF5_XXX45",
   "RF5_XXX46",
   "RF5_XXX47",
   "RF5_XXX48",
   "RF5_XXX49",
   "RF5_XXX50",
   "RF5_XXX51",
   "RF5_XXX52",
   "RF5_XXX53",
   "RF5_XXX54",
   "RF5_XXX55",
   "RF5_XXX56",
   "RF5_XXX57",
   "RF5_XXX58",
   "RF5_XXX59",
   "RF5_XXX60",
   "RF5_XXX61",
   "RF5_XXX62",
   "RF5_XXX63",
   "RF5_XXX64"
};

/*
 * Monster race flags
 */
static cptr r_info_flags6[] =
{
   "HASTE",
   "HEAL",
   "BLINK",
   "TPORT",
   "TELE_TO",
   "TELE_AWAY",
   "TELE_LEVEL",
   "DARKNESS",
   "TRAPS",
   "FORGET",
   "S_TROLLS",
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
   "S_XXX1",
   "S_XXX2",
   "S_XXX3",
   "S_NUMBER2",
   "S_NUMBER4",
   "S_NUMBER8",
   "S_NUMBER16",
   "RF6_XXX33",
   "RF6_XXX34",
   "RF6_XXX35",
   "RF6_XXX36",
   "RF6_XXX37",
   "RF6_XXX38",
   "RF6_XXX39",
   "RF6_XXX40",
   "RF6_XXX41",
   "RF6_XXX42",
   "RF6_XXX43",
   "RF6_XXX44",
   "RF6_XXX45",
   "RF6_XXX46",
   "RF6_XXX47",
   "RF6_XXX48",
   "RF6_XXX49",
   "RF6_XXX50",
   "RF6_XXX51",
   "RF6_XXX52",
   "RF6_XXX53",
   "RF6_XXX54",
   "RF6_XXX55",
   "RF6_XXX56",
   "RF6_XXX57",
   "RF6_XXX58",
   "RF6_XXX59",
   "RF6_XXX60",
   "RF6_XXX61",
   "RF6_XXX62",
   "RF6_XXX63",
   "RF6_XXX64"
};

static cptr corpse_gives_flags[32] =
{
   "STR",              /*  1 */
   "DEX",              /*  2 */
   "INT",              /*  3 */
   "WIS",              /*  4 */
   "CON",              /*  5 */
   "CHR",              /*  6 */
   "BLINDNESS",        /*  7 */
   "CONFUSION",        /*  8 */
   "VOMIT",            /*  9 */
   "TELEPORT",         /* 10 */
   "PEARL",            /* 11 */
   "POISON",           /* 12 */
   "BADPOISON",        /* 13 */
   "PARALYZE",         /* 14 */
   "MANA",             /* 15 */
   "SCREAM",           /* 16 */
   "THROAT",           /* 17 */
   "NEXUS",            /* 18 */
   "BLINK",            /* 19 */
   "SPEED",            /* 20 */
   "FIRE",             /* 21 */
   "ACID",             /* 22 */
   "ELEC",             /* 23 */
   "COLD",             /* 24 */
   "BADTHROAT",        /* 25 */
   "SLEEP",            /* 26 */
   "BERSERKER",        /* 27 */
   "INFLATE",          /* 28 */
   "FORGET",           /* 29 */
   "BADSCREAM",        /* 30 */
   "",                 /* 31 */
   ""                  /* 32 */
};

static cptr corpse_takes_flags[32] =
{
   "STR",                       /*  1 */
   "DEX",                       /*  2 */
   "INT",                       /*  3 */
   "WIS",                       /*  4 */
   "CON",                       /*  5 */
   "CHR",                       /*  6 */
   "BLINDNESS",                 /*  7 */
   "CONFUSION",                 /*  8 */
   "POISON",                    /*  9 */
   "BADPOISON",                 /* 10 */
   "PARALYZE",                  /* 11 */
   "MANA",                      /* 12 */
   "SPEED",                     /* 13 */
   "ACID",                      /* 14 */
   "FIRE",                      /* 15 */
   "ELEC",                      /* 16 */
   "COLD"                       /* 17 */
   "",                          /* 18 */
   "",                          /* 19 */
   "",                          /* 20 */
   "",                          /* 21 */
   "",                          /* 22 */
   "",                          /* 23 */
   "",                          /* 24 */
   "",                          /* 25 */
   "",                          /* 26 */
   "",                          /* 27 */
   "",                          /* 28 */
   "",                          /* 29 */
   "",                          /* 30 */
   "",                          /* 31 */
   ""                           /* 32 */
};

/*
 * Object flags
 */
static cptr k_info_flags1[64] =
{
   "STR1",                     /*  1 */
   "INT1",                     /*  2 */
   "WIS1",                     /*  3 */
   "DEX1",                     /*  4 */
   "CON1",                     /*  5 */
   "CHR1",                     /*  6 */
   "LITE1",                    /*  7 */
   "STEALTH1",                 /*  8 */
   "SEARCH1",                  /*  9 */
   "INFRA1",                   /* 10 */
   "TUNNEL1",                  /* 11 */
   "SPEED1",                   /* 12 */
   "BLOWS1",                   /* 13 */
   "MIGHT1",                   /* 14 */
   "SHOTS1",                   /* 15 */
   "MAGIC1",                   /* 16 */
   "VAMPIRIC1",                /* 17 */
   "STR2",                     /* 18 */
   "INT2",                     /* 19 */
   "WIS2",                     /* 20 */
   "DEX2",                     /* 21 */
   "CON2",                     /* 22 */
   "CHR2",                     /* 23 */
   "LITE2",                    /* 24 */
   "STEALTH2",                 /* 25 */
   "SEARCH2",                  /* 26 */
   "INFRA2",                   /* 27 */
   "TUNNEL2",                  /* 28 */
   "SPEED2",                   /* 29 */
   "BLOWS2",                   /* 30 */
   "MIGHT2",                   /* 31 */
   "SHOTS2",                   /* 32 */
   "MAGIC2",                   /* 33 */
   "VAMPIRIC2",                /* 34 */
   "SLAY_ANIMAL",              /* 35 */
   "KILL_ANIMAL",              /* 36 */
   "SLAY_EVIL",                /* 37 */
   "KILL_EVIL",                /* 38 */
   "SLAY_UNDEAD",              /* 39 */
   "KILL_UNDEAD",              /* 40 */
   "SLAY_DEMON",               /* 41 */
   "KILL_DEMON",               /* 42 */
   "SLAY_ORC",                 /* 43 */
   "KILL_ORC",                 /* 44 */
   "SLAY_TROLL",               /* 45 */
   "KILL_TROLL",               /* 46 */
   "SLAY_GIANT",               /* 47 */
   "KILL_GIANT",               /* 48 */
   "SLAY_DRAGON",              /* 49 */
   "KILL_DRAGON",              /* 50 */
   "IMPACT",                   /* 51 */
   "XXX1",                     /* 52 */
   "BRAND_ACID",               /* 53 */
   "BRAND_ELEC",               /* 54 */
   "BRAND_FIRE",               /* 55 */
   "BRAND_COLD",               /* 56 */
   "XXX2",                     /* 57 */
   "XXX3",                     /* 58 */
   "XXX4",                     /* 59 */
   "XXX5",                     /* 60 */
   "XXX6",                     /* 61 */
   "XXX7",                     /* 62 */
   "XXX8",                     /* 63 */
   "XXX9"                      /* 64 */
};

/*
 * Object flags
 */
static cptr k_info_flags2[64] =
{
   "SUST_STR",                 /* 01 0x0000000000000001LL */
   "SUST_INT",                 /* 02 0x0000000000000002LL */
   "SUST_WIS",                 /* 03 0x0000000000000004LL */
   "SUST_DEX",                 /* 04 0x0000000000000008LL */
   "SUST_CON",                 /* 05 0x0000000000000010LL */
   "SUST_CHR",                 /* 06 0x0000000000000020LL */
   "",                         /* 07 0x0000000000000040LL */
   "",                         /* 08 0x0000000000000080LL */
   "IM_ACID",                  /* 09 0x0000000000000100LL */
   "IM_ELEC",                  /* 10 0x0000000000000200LL */
   "IM_FIRE",                  /* 11 0x0000000000000400LL */
   "IM_COLD",                  /* 12 0x0000000000000800LL */
   "",                         /* 13 0x0000000000001000LL */
   "",                         /* 14 0x0000000000002000LL */
   "FREE_ACT",                 /* 15 0x0000000000004000LL */
   "HOLD_LIFE",                /* 16 0x0000000000008000LL */
   "RES_ACID",                 /* 17 0x0000000000010000LL */
   "RES_ELEC",                 /* 18 0x0000000000020000LL */
   "RES_FIRE",                 /* 19 0x0000000000040000LL */
   "RES_COLD",                 /* 20 0x0000000000080000LL */
   "RES_POIS",                 /* 21 0x0000000000100000LL */
   "RES_FEAR",                 /* 22 0x0000000000200000LL */
   "RES_LITE",                 /* 23 0x0000000000400000LL */
   "RES_DARK",                 /* 24 0x0000000000800000LL */
   "RES_BLIND",                /* 25 0x0000000001000000LL */
   "RES_CONF",                 /* 26 0x0000000002000000LL */
   "RES_SOUND",                /* 27 0x0000000004000000LL */
   "RES_SHARDS",               /* 28 0x0000000008000000LL */
   "RES_NETHER",               /* 29 0x0000000010000000LL */
   "RES_NEXUS",                /* 30 0x0000000020000000LL */
   "RES_CHAOS",                /* 31 0x0000000040000000LL */
   "RES_DISEN",                /* 32 0x0000000080000000LL */
   "",                         /* 33 */
   "",                         /* 34 */
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
static cptr k_info_flags3[64] =
{
   "",                        /* 01 0x0000000000000001LL */
   "",                        /* 02 0x0000000000000002LL */
   "",                        /* 03 0x0000000000000004LL */
   "",                        /* 04 0x0000000000000008LL */
   "",                        /* 05 0x0000000000000010LL */
   "",                        /* 06 0x0000000000000020LL */
   "",                        /* 07 0x0000000000000040LL */
   "NOFLAVOR",                /* 08 0x0000000000000080LL */
   "EASY_KNOW",               /* 09 0x0000000000000100LL */
   "HIDE_TYPE",               /* 10 0x0000000000000200LL */
   "SHOW_MODS",               /* 11 0x0000000000000400LL */
   "INSTA_ART",               /* 12 0x0000000000000800LL */
   "FEATHER",                 /* 13 0x0000000000001000LL */
   "SOMELITE",                /* 14 0x0000000000002000LL */
   "SEE_INVIS",               /* 15 0x0000000000004000LL */
   "TELEPATHY",               /* 16 0x0000000000008000LL */
   "SLOW_DIGEST",             /* 17 0x0000000000010000LL */
   "REGEN",                   /* 18 0x0000000000020000LL */
   "COULD2H",                 /* 19 0x0000000000040000LL */
   "MUST2H",                  /* 20 0x0000000000080000LL */
   "IGNORE_ACID",             /* 21 0x0000000000100000LL */
   "IGNORE_ELEC",             /* 22 0x0000000000200000LL */
   "IGNORE_FIRE",             /* 23 0x0000000000400000LL */
   "IGNORE_COLD",             /* 24 0x0000000000800000LL */
   "ACTIVATE",                /* 25 0x0000000001000000LL */
   "DRAIN_EXP",               /* 26 0x0000000002000000LL */
   "TELEPORT",                /* 27 0x0000000004000000LL */
   "AGGRAVATE",               /* 28 0x0000000008000000LL */
   "BLESSED",                 /* 29 0x0000000010000000LL */
   "CURSED",                  /* 30 0x0000000020000000LL */
   "HEAVY_CURSE",             /* 31 0x0000000040000000LL */
   "PERMA_CURSE",             /* 32 0x0000000080000000LL */
   "",                        /* 33 */
   "",                        /* 34 */
   "",                        /* 35 */
   "",                        /* 36 */
   "",                        /* 37 */
   "",                        /* 38 */
   "",                        /* 39 */
   "",                        /* 40 */
   "",                        /* 41 */
   "",                        /* 42 */
   "",                        /* 43 */
   "",                        /* 44 */
   "",                        /* 45 */
   "",                        /* 46 */
   "",                        /* 47 */
   "",                        /* 48 */
   "",                        /* 49 */
   "",                        /* 50 */
   "",                        /* 51 */
   "",                        /* 52 */
   "",                        /* 53 */
   "",                        /* 54 */
   "",                        /* 55 */
   "",                        /* 56 */
   "",                        /* 57 */
   "",                        /* 58 */
   "",                        /* 59 */
   "",                        /* 60 */
   "",                        /* 61 */
   "",                        /* 62 */
   "",                        /* 63 */
   ""                         /* 64 */
};

/*
 * Trap flags
 */
static cptr t_info_flags[] =
{
   "CHEST",
   "DOOR",
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
 * vault option flags
 */
static cptr v_info_options[] =
{
   "GOING_DOWN",               /*  1 0x00000001L */
   "GOING_UP",                 /*  2 0x00000002L */
   "LIGHT10",                  /*  3 0x00000004L */
   "LIGHT25",                  /*  4 0x00000008L */
   "LIGHT50",                  /*  5 0x00000010L */
   "BOOSTOBJ1",                /*  6 0x00000020L */
   "BOOSTOBJ2",                /*  7 0x00000040L */
   "BOOSTOBJ4",                /*  9 0x00000080L */
   "BOOSTOBJ8",                /* 10 0x00000100L */
   "RACE_ALL",                 /* 11 0x00000200L */
   "RACE_MAX4",                /* 12 0x00000400L */
   "RACE_MAX8",                /* 13 0x00000800L */
   "RACE_MAX16",               /* 14 0x00001000L */
   "RACE_MAX32",               /* 15 0x00002000L */
   "RACE_SOME",                /* 16 0x00004000L */
   "MIRROR_X",                 /* 15 0x00008000L */
   "MIRROR_Y",                 /* 16 0x00010000L */
   "",                         /* 17 0x00020000L */
   "",                         /* 18 0x00040000L */
   "",                         /* 19 0x00080000L */
   "",                         /* 20 0x00100000L */
   "",                         /* 21 0x00200000L */
   "",                         /* 22 0x00400000L */
   "",                         /* 23 0x00800000L */
   "",                         /* 24 0x01000000L */
   "",                         /* 25 0x02000000L */
   "",                         /* 26 0x04000000L */
   "",                         /* 27 0x08000000L */
   "",                         /* 28 0x10000000L */
   "",                         /* 29 0x20000000L */
   "",                         /* 30 0x40000000L */
   ""                          /* 31 0x80000000L */
};

/*
 * Spell caster types
 */
static cptr spell_info_class_flags[MAX_CLASS] =
{
   "WARRIOR",
   "MAGE",
   "PRIEST",
   "ROGUE",
   "RANGER",
   "PALADIN",
   "WARMAGE",
   "HIGHPRST"
};

static cptr spell_info_scale_flags[5] =
{
   "SMALL",
   "AVG",
   "LARGE",
   "SUPER"
};

static cptr spell_info_type_flags[MAX_SPELL_TYPES] =
{
   "ATTACK_NAT",
   "ATTACK_DRK",
   "ESCAPE",
   "HEAL",
   "SENSE",
   "CHANGE_OTHER",
   "CHANGE_ITEM",
   "CHANGE_SELF",
   "CHANGE_WORLD"
};

/*
 * Convert a "color letter" into an "actual" color
 * The colors are: dwsorgbuDWvyRGBU, as shown below
 */
static s16b color_char_to_attr(char c)
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
      case 'u': return (TERM_BROWN);

      case 'D': return (TERM_L_DARK);
      case 'W': return (TERM_L_WHITE);
      case 'v': return (TERM_VIOLET);
      case 'y': return (TERM_YELLOW);
      case 'R': return (TERM_L_RED);
      case 'G': return (TERM_L_GREEN);
      case 'B': return (TERM_L_BLUE);
      case 'U': return (TERM_L_BROWN);
   }

   return (-1);
}

/*** Initialize from ascii template files ***/

static s16b init_test_version(char *buf)
{
   int v1, v2, v3;

   /* Scan for the values */
   if ((3 != sscanf(buf, "V:%d.%d.%d", &v1, &v2, &v3)) ||
       ((byte)v1 != VERSION_MAJOR) ||
       ((byte)v2 != VERSION_MINOR) ||
       ((byte)v3 != VERSION_PATCH))
   {
       return (2);
   }
   return(0);
}

/*
 * to parse lines like X:<hgt>3:<wid>4:<level>5
 * which are much more readable, this routine removes all <....> pairs from
 * buf.
 */
static s16b clean_buf(char *buf)
{
   s16b i, j, k1, k2;
   i=0;
   while (buf[i]!=0)
   {
      if (buf[i]=='<')
      {
         j=i;

         while (buf[j]!='>')
         {
            j++;
            if (buf[j]==0)
            {
               break;
            }
         }
         if (buf[j]==0)
         {
            return 10;
         }

         k1=i;
         k2=j+1;
         while (TRUE)
         {
            buf[k1]=buf[k2];
            if (buf[k2]==0) break;
            k1++;
            k2++;
         }

      }
      i++;
   }
   return 0;
}


/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static void test_one_monster_flag(u64b *flags, s16b *flag_set, cptr what)
{
   s16b i;

   /* Scan flags1 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags1[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=1;
         return;
      }
   }

   /* Scan flags2 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags2[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=2;
         return;
      }
   }

   /* Scan flags3 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags3[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=3;
         return;
      }
   }

   /* Scan flags4 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags4[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=4;
         return;
      }
   }

   /* Scan flags5 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags5[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=5;
         return;
      }
   }

   /* Scan flags6 */
   for (i = 0; i < 64; i++)
   {
      if (streq(what, r_info_flags6[i]))
      {
         (*flags) |= (1L << i);
         (*flag_set)=6;
         return;
      }
   }


   /* Oops */
   msg_format("Unknown monster flag '%s'.", what);

   /* Failure */
   (*flag_set)=0;

   return;
}


/*
 * Grab one monster_flag for vault-building
 */
static errr grab_one_vault_flag(vault_type *v_ptr, cptr what)
{
   s16b i = 0;
   u64b my_flags = 0LL;

   test_one_monster_flag(&my_flags, &i, what);
   switch(i)
   {
      case 1: v_ptr->flags1 |= my_flags;
              return(0);
      case 2: v_ptr->flags2 |= my_flags;
              return(0);
      case 3: v_ptr->flags3 |= my_flags;
              return(0);
      case 4: v_ptr->flags4 |= my_flags;
              return(0);
      case 5: v_ptr->flags5 |= my_flags;
              return(0);
      case 6: v_ptr->flags6 |= my_flags;
              return(0);
      default: msg_format("Illegal vault flag '%s'.", what);
   }

   /* Oops */
   msg_format("Unknown vault flag '%s'.", what);

   /* Failure */
   return (1);
}

static errr grab_one_vault_nflag(vault_type *v_ptr, cptr what)
{
   s16b i = 0;
   u64b my_flags = 0LL;

   test_one_monster_flag(&my_flags, &i, what);
   switch(i)
   {
      case 1: v_ptr->nflags1 |= my_flags;
              return(0);
      case 2: v_ptr->nflags2 |= my_flags;
              return(0);
      case 3: v_ptr->nflags3 |= my_flags;
              return(0);
      case 4: v_ptr->nflags4 |= my_flags;
              return(0);
      case 5: v_ptr->nflags5 |= my_flags;
              return(0);
      case 6: v_ptr->nflags6 |= my_flags;
              return(0);
      default: msg_format("Illegal vault flag '%s'.", what);
   }

   /* Oops */
   msg_format("Unknown vault flag '%s'.", what);

   /* Failure */
   return (1);
}


static s16b init_v_info_flags(vault_type *v_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while (*t == ' ' || *t == '|') t++;
      }

      /* Parse this entry */
      if (0 != grab_one_vault_flag(v_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static s16b init_v_info_nflags(vault_type *v_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while (*t == ' ' || *t == '|') t++;
      }

      /* Parse this entry */
      if (0 != grab_one_vault_nflag(v_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

/*
 * Grab one vault option flag in an vault type from a textual string
 */
static errr grab_one_vault_option(vault_type *v_ptr, cptr what)
{
   s16b i=0;

   /* Check flags1 */
   for (i = 0; i<32; i++)
   {
      if (streq(what, v_info_options[i]))
      {
         v_ptr->options |= (1LL << i);
         return (0);
      }
   }

   /* Oops */
   msg_format("Unknown vault option '%s'.", what);

   /* Error */
   return (1);
}


static s16b init_v_info_options(vault_type *v_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while (*t == ' ' || *t == '|') t++;
      }

      /* Parse this entry */
      if (0 != grab_one_vault_option(v_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}


static s16b init_v_info_extra(vault_type *v_ptr, char *buf)
{
   int typ, id, rat, hgt, wid, min_lev, max_lev, sublevel;

   /* Scan for the values */
   if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d:%d",
       &typ, &id, &rat, &hgt, &wid, &min_lev, &max_lev, &sublevel)) return (1);

   /* Save the values */
   v_ptr->typ = (s16b)typ;
   v_ptr->id  = (byte)id;
   v_ptr->rat = (s16b)rat;
   v_ptr->hgt = (s16b)hgt;
   v_ptr->wid = (s16b)wid;
   v_ptr->min_lev = (s16b)min_lev;
   v_ptr->max_lev = (s16b)max_lev;
   v_ptr->sublevel = (byte)sublevel;
   return(0);
}

/*
 * Initialize the "v_info" array, by parsing an ascii "template" file
 */
errr init_v_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   vault_type *v_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = buf+2;                 /* Find the colon before the name */
         if (!s) return (1);        /* Paranoia -- require a name */
         i++;                       /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_V_IDX) return (8);   /* Verify information */
         v_ptr = &v_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > v_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!v_ptr->name) v_ptr->name = ++name_size;

         strcpy(v_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

      /* There better be a current v_ptr */
      if (!v_ptr) return (3);

      /* Process 'D' for "Description" */
      if (buf[0] == 'D')
      {
          s = buf+2;   /* Acquire the text */

          dlog(DEBUGTEMPL,"v_info line (width %d) before >%s<\n", v_ptr->wid, s);
          while (strlen(s) < v_ptr->wid)
          {
             strcat(s, " ");
          }
          dlog(DEBUGTEMPL,"v_info line (width %d) after  >%s<\n", v_ptr->wid, s);
          /* Hack -- Verify space */
          if (text_size + strlen(s) > v_text_size - 8) return (7);
          if (!v_ptr->text)
          {
             v_ptr->text = ++text_size; /* Advance and Save the text index */
          }

          strcpy(v_text + text_size, s); /* Append chars to the name */
          text_size += strlen(s);
          continue;
      }


      switch (buf[0])
      {
         /* Process 'X' for "Extra info" (one line only) */
         case 'X': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_v_info_extra(v_ptr, buf);
                   break;
         /* Process 'F' for flags */
         case 'F': ret=init_v_info_flags(v_ptr, buf);
                   break;
         /* Process 'B' for nflags */
         case 'B': ret=init_v_info_nflags(v_ptr, buf);
                   break;
         /* Process 'O' for options */
         case 'O': ret=init_v_info_options(v_ptr, buf);
                   break;
         default:  return (6);
      }
      if (ret) return(ret);
   }

   ++name_size;
   ++text_size;

   v_number = i+1;

   if (error_line == 0) return(10);

   dlog(DEBUGALWAYS,"Angband: guessed vault array size at %d, found %d\n",
                    MAX_V_IDX, v_number);
   dlog(DEBUGALWAYS,"Angband: guessed vault name array size at %ld, found %ld\n",
              v_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed vault text array size at %ld, found %ld\n",
              v_text_size, text_size);
   v_name_size = name_size;
   v_text_size = text_size;

   /* No version yet */

   if (!okay) return (2);

   write_info_header("v_info.hdr", v_number, v_name_size, v_text_size);
   write_info_data("v_info.raw", (char*)v_info, v_number, sizeof(vault_type),
                   v_name, v_name_size, v_text, v_text_size);

   /* Success */
   return (0);
}

static s16b init_f_info_info(feature_type *f_ptr, char *buf)
{
   int mtyp, styp;

   /* Scan for the values */
   if (2 != sscanf(buf+2, "%d:%d",
       &mtyp, &styp)) return (1);

   /* Save the values */
   f_ptr->mtyp = (s16b)mtyp;
   f_ptr->styp = (s16b)styp;
   return(0);
}

static s16b init_f_info_priority(feature_type *f_ptr, char *buf)
{
   int prior;

   /* Scan for the values */
   if (1 != sscanf(buf+2, "%d",
       &prior)) return (1);

   /* Save the values */
   f_ptr->priority = (byte)prior;
   return(0);
}

static s16b init_f_info_mimic(feature_type *f_ptr, char *buf)
{
   int mim_m, mim_s;

   /* Scan for the values */
   if (2 != sscanf(buf+2, "%d:%d",
                   &mim_m, &mim_s)) return (1);

   /* Save the values */
   f_ptr->mim_m = (s16b)mim_m;
   f_ptr->mim_s = (s16b)mim_s;
   return(0);
}

static s16b init_f_info_flags(feature_type *f_ptr, char *buf)
{
   s16b i=0;

   /* Paranoia */
   if (!buf[2]) return (1);

   /* init */
   f_ptr->flags = 0L;

   for (i=2;i<strlen(buf);i++)
   {
      switch (buf[i])
      {
         case 'L' : f_ptr->flags |= CAVE_LIGHT;
                    break;
         case 'M' : f_ptr->flags |= CAVE_MIMIC;
                    break;
         case 'W' : f_ptr->flags |= CAVE_WALK;
                    break;
         case 'Z' : f_ptr->flags |= CAVE_MAGIC;
                    break;
         case 'S' : f_ptr->flags |= CAVE_SWIM;
                    break;
         case 'N' : f_ptr->flags |= CAVE_NOTICE;
      }
   }
   return(0);
}

static s16b init_f_info_graphics(feature_type *f_ptr, char *buf)
{
   s16b tmp;

   /* Paranoia */
   if (!buf[2]) return (1);
   if (!buf[3]) return (1);
   if (!buf[4]) return (1);

   tmp = color_char_to_attr(buf[4]);
   if (tmp < 0) return (1);

   /* Save the values */
   f_ptr->d_char = buf[2];
   f_ptr->d_attr = tmp;

   /* jk - make sure it's initialized */

   f_ptr->x_char = buf[2];
   f_ptr->x_attr = tmp;
   return(0);
}

/*
 * Initialize the "f_info" array, by parsing an ascii "template" file
 */
errr init_f_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret = 0;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   feature_type *f_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == (ret=my_fgets(fp, buf, 1024)))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
          s = buf+2;                 /* Find the colon before the name */
          if (!s) return (1);        /* Verify that colon */
          i++;                       /* Get the index */
          error_idx = i;             /* Save the index */
          if (i >= MAX_F_IDX) return (8);   /* Verify information */
          f_ptr = &f_info[i];        /* Point at the "info" */

          /* Hack -- Verify space */
          if (name_size + strlen(s) > f_name_size - 8) return (7);
          /* Advance and Save the name index */
          if (!f_ptr->name) f_ptr->name = ++name_size;

          strcpy(f_name + name_size, s);  /* Append chars to the name */
          name_size += strlen(s);         /* Advance the index */
          continue;                       /* Next... */
      }

      /* There better be a current f_ptr */
      if (!f_ptr) return (3);
      switch (buf[0])
      {
         case 'D': break;

         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_f_info_info(f_ptr, buf);
                   break;
         /* Process 'P' for "Priority" (one line only) */
         case 'P': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_f_info_priority(f_ptr, buf);
                   break;
         /* Process 'M' for "Mimic" (one line only) */
         case 'M': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_f_info_mimic(f_ptr, buf);
                   break;
         /* Process 'F' for "FLAGS" (one line only) */
         case 'F': ret=init_f_info_flags(f_ptr, buf);
                   break;
         /* Process 'G' for "Graphics" (one line only) */
         case 'G': ret=init_f_info_graphics(f_ptr, buf);
                   break;

         default:  return(6);
      }
      if (ret) return(ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;
   f_number = i + 1;

   if (error_line == 0) return(10);

   dlog(DEBUGALWAYS,"Angband: guessed feature array size at %d, found %d\n",
                    MAX_F_IDX, f_number);
   dlog(DEBUGALWAYS,"Angband: guessed feature name array size at %ld, found %ld\n",
              f_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed feature text array size at %ld, found %ld\n",
              f_text_size, text_size);
   f_name_size = name_size;
   f_text_size = text_size;

   write_info_header("f_info.hdr", f_number, f_name_size, f_text_size);
   write_info_data("f_info.raw", (char*)f_info, f_number, sizeof(feature_type),
                   f_name, f_name_size, f_text, f_text_size);

   /* Success */
   return (0);
}

/*
 * Grab one flag in an object_kind from a textual string
 */
static errr grab_one_kind_flag(object_kind *k_ptr, cptr what)
{
   s16b i=0;

   /* Check flags1 */
   for (i = 0; i<64; i++)
   {
      if (k_info_flags1[i][0] == '\0') continue;

      if (streq(what, k_info_flags1[i]))
      {
         k_ptr->flags1 |= (1LL << i);
         return (0);
      }
   }

   /* Check flags2 */
   for (i = 0; i<64; i++)
   {
      if (k_info_flags2[i][0] == '\0') continue;

      if (streq(what, k_info_flags2[i]))
      {
         k_ptr->flags2 |= (1LL << i);
         return (0);
      }
   }

   /* Check flags3 */
   for (i = 0; i<64; i++)
   {
      if (k_info_flags3[i][0] == '\0') continue;

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

static s16b init_k_info_graphics(object_kind *k_ptr, char *buf)
{
   char sym;
   s16b tmp;

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
   k_ptr->d_char = sym;
   k_ptr->d_attr = tmp;
   return(0);
}

static s16b init_k_info_info(object_kind *k_ptr, char *buf)
{
   int tval, sval, p1val, p2val;

   /* Scan for the values */
   if (4 != sscanf(buf+2, "%d:%d:%d:%d",
       &tval, &sval, &p1val, &p2val)) return (1);

   /* Save the values */
   k_ptr->tval = (s16b)tval;
   k_ptr->sval = (s16b)sval;
   k_ptr->p1val = (s16b)p1val;
   k_ptr->p2val = (s16b)p2val;
   return(0);
}

static s16b init_k_info_more(object_kind *k_ptr, char *buf)
{
   int level, extra, wgt;
   long cost;

   /* Scan for the values */
   if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
       &level, &extra, &wgt, &cost)) return (1);

   /* Save the values */
   k_ptr->level  = (s16b)level;
   k_ptr->extra  = (s16b)extra;
   k_ptr->weight = (s16b)wgt;
   k_ptr->cost   = cost;
   return(0);
}

static s16b init_k_info_allocation(object_kind *k_ptr, char *buf)
{

   s16b i=0;
   char *s, *t;

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

       if (t && (!s || t < s))
       {
           s16b chance = atoi(t+1);
           if (chance > 0) k_ptr->chance[i] = chance;
       }
   }
   return(0);
}

static s16b init_k_info_power(object_kind *k_ptr, char *buf)
{

   int ac, hd1, hd2, th, td, ta;

   /* Scan for the values */
   if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
       &ac, &hd1, &hd2, &th, &td, &ta)) return (1);

   k_ptr->ac   = (s16b)ac;
   k_ptr->dd   = (s16b)hd1;
   k_ptr->ds   = (s16b)hd2;
   k_ptr->to_h = (s16b)th;
   k_ptr->to_d = (s16b)td;
   k_ptr->to_a = (s16b)ta;
   return(0);
}

static s16b init_k_info_flags(object_kind *k_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry textually */
   for (s = buf + 2; *s; )
   {
       /* Find the end of this entry */
       for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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
   return(0);
}

/*
 * Initialize the "k_info" array, by parsing an ascii "template" file
 */
errr init_k_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   object_kind *k_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = buf+2;                 /* Find the colon before the name */
         if (!s) return (1);        /* Verify that colon */
         i++;                       /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_K_IDX) return (8);   /* Verify information */
         k_ptr = &k_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > k_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!k_ptr->name) k_ptr->name = ++name_size;

         strcpy(k_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

      /* There better be a current k_ptr */
      if (!k_ptr) return (3);

      /* Process 'D' for "Description" */
      if (buf[0] == 'D')
      {
         s = buf+2;   /* Acquire the text */

         /* Hack -- Verify space */
         if (text_size + strlen(s) > k_text_size - 8) return (7);

         if (!k_ptr->text)
            k_ptr->text = ++text_size; /* Advance and Save the text index */
         else
         /* pop in a space if necessary */
         {
            if (*(k_text + text_size - 1) != ' ')
            {
               *(k_text + text_size) = ' ';
               *(k_text + ++text_size) = 0;
            }
         }

         strcpy(k_text + text_size, s); /* Append chars to the name */
         text_size += strlen(s);
         continue;
      }
      switch (buf[0])
      {
         /* Process 'G' for "Graphics" (one line only) */
         case 'G': ret=init_k_info_graphics(k_ptr, buf);
                   break;
         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_k_info_info(k_ptr, buf);
                   break;
         /* Process 'W' for "More Info" (one line only) */
         case 'W': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_k_info_more(k_ptr, buf);
                   break;
         /* Process 'A' for "Allocation" (one line only) */
         case 'A': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_k_info_allocation(k_ptr, buf);
                   break;
         /* Hack -- Process 'P' for "power" and such */
         case 'P': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_k_info_power(k_ptr, buf);
                   break;
         /* Hack -- Process 'F' for flags */
         case 'F': ret=init_k_info_flags(k_ptr, buf);
                   break;
         default:  return(6);
      }
      if (ret) return(ret);
   }
   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;
   k_number = i + 1;

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed object array size at %d, found %d\n",
                    MAX_K_IDX, k_number);
   dlog(DEBUGALWAYS,"Angband: guessed object name array size at %ld, found %ld\n",
              k_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed object text array size at %ld, found %ld\n",
              k_text_size, text_size);
   k_name_size = name_size;
   k_text_size = text_size;

   write_info_header("k_info.hdr", k_number, k_name_size, k_text_size);
   write_info_data("k_info.raw", (char*)k_info, k_number, sizeof(object_kind),
                   k_name, k_name_size, k_text, k_text_size);

   /* Success */
   return (0);
}

/*
 * Grab one flag in an artifact_type from a textual string
 */
static errr grab_one_artifact_flag(artifact_type *a_ptr, cptr what)
{
   s16b i;

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

static s16b init_a_info_info(artifact_type *a_ptr, char *buf)
{
   int tval, sval, p1val, p2val;

   /* Scan for the values */
   if (4 != sscanf(buf+2, "%d:%d:%d:%d",
       &tval, &sval, &p1val, &p2val)) return (1);

   /* Save the values */
   a_ptr->tval = (s16b)tval;
   a_ptr->sval = (s16b)sval;
   a_ptr->p1val = (s16b)p1val;
   a_ptr->p2val = (s16b)p2val;
   return(0);
}

static s16b get_a_info_more_info(artifact_type *a_ptr, char *buf)
{
   int level, rarity, wgt;
   long cost;

   /* Scan for the values */
   if (4 != sscanf(buf+2, "%d:%d:%d:%ld",
       &level, &rarity, &wgt, &cost)) return (1);

   /* Save the values */
   a_ptr->level  = (s16b)level;
   a_ptr->rarity = (s16b)rarity;
   a_ptr->weight = (s16b)wgt;
   a_ptr->cost = cost;
   return(0);
}

static s16b get_a_info_power(artifact_type *a_ptr, char *buf)
{
   int ac, hd1, hd2, th, td, ta;

   /* Scan for the values */
   if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
       &ac, &hd1, &hd2, &th, &td, &ta)) return (1);

   a_ptr->ac   = (s16b)ac;
   a_ptr->dd   = (s16b)hd1;
   a_ptr->ds   = (s16b)hd2;
   a_ptr->to_h = (s16b)th;
   a_ptr->to_d = (s16b)td;
   a_ptr->to_a = (s16b)ta;
   return(0);
}

static s16b get_a_info_flags(artifact_type *a_ptr, char *buf)
{
   char *s, *t;

   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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
   return (0);
}

/*
 * Initialize the "a_info" array, by parsing an ascii "template" file
 */
errr init_a_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   artifact_type *a_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = strchr(buf+2, ':');    /* Find the colon before the name */
         *s++ = '\0';               /* Nuke the colon, advance to the name */
         /* Paranoia -- require a name */
         if (!*s) return (1);       /* Verify that colon */
         i = atoi(buf+2);           /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_A_IDX) return (8);   /* Verify information */
         a_ptr = &a_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > a_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!a_ptr->name) a_ptr->name = ++name_size;

         strcpy(a_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

      /* There better be a current a_ptr */
      if (!a_ptr) return (3);

      /* Process 'D' for "Description" */
      if (buf[0] == 'D')
      {
         s = buf+2;   /* Acquire the text */

         /* Hack -- Verify space */
         if (text_size + strlen(s) > a_text_size - 8) return (7);

         if (!a_ptr->text)
            a_ptr->text =
            ++text_size; /* Advance and Save the text index */
         else
         /* pop in a space if necessary */
         {
            if (*(a_text + text_size - 1) != ' ')
            {
               *(a_text + text_size) = ' ';
               *(a_text + ++text_size) = 0;
            }
         }

         strcpy(a_text + text_size, s); /* Append chars to the name */
         text_size += strlen(s);
         continue;
      }
      switch (buf[0])
      {
         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=init_a_info_info(a_ptr, buf);
                   break;
         /* Process 'W' for "More Info" (one line only) */
         case 'W': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_a_info_more_info(a_ptr, buf);
                   break;
         /* Hack -- Process 'P' for "power" and such */
         case 'P': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_a_info_power(a_ptr, buf);
                   break;
         /* Hack -- Process 'F' for flags */
          case 'F': ret=get_a_info_flags(a_ptr, buf);
                   break;
          default: return (6);
      }
      if (ret) return(ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;
   a_number = i + 1;

   /* Hack -- extract the "ignore" flags */
   for (i = 0; i < a_number; i++)
   {
      a_ptr = &a_info[i];

      /* Skip non-artifacts */
      if (!a_ptr->name) continue;

      /* Ignore everything */
      a_ptr->flags3 |= (TR3_IGNORE_ACID);
      a_ptr->flags3 |= (TR3_IGNORE_ELEC);
      a_ptr->flags3 |= (TR3_IGNORE_FIRE);
      a_ptr->flags3 |= (TR3_IGNORE_COLD);
   }

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed artifact array size at %d, found %d\n",
                    MAX_A_IDX, a_number);
   dlog(DEBUGALWAYS,"Angband: guessed artifact name array size at %ld, found %ld\n",
              a_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed artifact text array size at %ld, found %ld\n",
              a_text_size, text_size);
   a_name_size = name_size;
   a_text_size = text_size;

   write_info_header("a_info.hdr", a_number, a_name_size, a_text_size);
   write_info_data("a_info.raw", (char*)a_info, a_number, sizeof(artifact_type),
                   a_name, a_name_size, a_text, a_text_size);

   /* Success */
   return (0);
}

/*
 * Grab one flag in a ego-item_type from a textual string
 */
static bool grab_one_ego_item_flag(ego_item_type *e_ptr, cptr what)
{
   s16b i;
   /* Check flags1 */
   for (i = 0; i<64; i++)
   {
      if (streq(what, k_info_flags1[i]))
      {
         e_ptr->flags1 |= (1LL << i);
         return (0);
      }
   }

   /* Check flags2 */
   for (i = 0; i<64; i++)
   {
      if (streq(what, k_info_flags2[i]))
      {
         e_ptr->flags2 |= (1LL << i);
         return (0);
      }
   }

   /* Check flags3 */
   for (i = 0; i<64; i++)
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

static s16b get_e_info_xtra(ego_item_type *e_ptr, char *buf)
{
   int slot, rating,weightfactor;

   /* Scan for the values */
   /* jk - weightfactor included */
   if (3 != sscanf(buf+2, "%d:%d:%d",
       &slot, &rating, &weightfactor)) return (1);

   /* Save the values */
   e_ptr->slot = (s16b)slot;
   e_ptr->rating = (s16b)rating;
   e_ptr->weightfactor = (s16b)weightfactor;
   return(0);
}

static s16b get_e_info_more_info(ego_item_type *e_ptr, char *buf)
{
   int level, rarity;
   long cost;

   /* Scan for the values */
   if (3 != sscanf(buf+2, "%d:%d:%ld",
       &level, &rarity, &cost)) return (1);

   /* Save the values */
   e_ptr->level = (s16b)level;
   e_ptr->rarity = (s16b)rarity;
   e_ptr->cost = cost;
   return(0);
}

static s16b get_e_info_attr1(ego_item_type *e_ptr, char *buf)
{
   int th, td, ta, p1v, p2v, dd, ds;

   /* Scan for the values */
   if (7 != sscanf(buf+2, "%d:%d:%d:%d:%d:%d:%d",
       &th, &td, &ta, &p1v, &p2v, &dd, &ds)) return (1);

   e_ptr->max_to_h = (s16b)th;
   e_ptr->max_to_d = (s16b)td;
   e_ptr->max_to_a = (s16b)ta;
   e_ptr->max_p1val = (s16b)p1v;
   e_ptr->max_p2val = (s16b)p2v;
   e_ptr->ds = (byte)dd;
   e_ptr->dd = (byte)ds;
   return(0);
}

static s16b get_e_info_attr2(ego_item_type *e_ptr, char *buf)
{
   int th, td, ta, p1v, p2v;

   /* Scan for the values */
   if (5 != sscanf(buf+2, "%d:%d:%d:%d:%d",
       &th, &td, &ta, &p1v, &p2v)) return (1);

   e_ptr->min_to_h = (s16b)th;
   e_ptr->min_to_d = (s16b)td;
   e_ptr->min_to_a = (s16b)ta;
   e_ptr->min_p1val = (s16b)p1v;
   e_ptr->min_p2val = (s16b)p2v;
   return(0);
}


static s16b get_e_info_flags(ego_item_type *e_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry textually */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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
   return(0);
}

/*
 * Initialize the "e_info" array, by parsing an ascii "template" file
 */
errr init_e_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   ego_item_type *e_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = strchr(buf+2, ':');    /* Find the colon before the name */
         *s++ = '\0';               /* Nuke the colon, advance to the name */
         /* Paranoia -- require a name */
         if (!*s) return (1);       /* Verify that colon */
         i = atoi(buf+2);           /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_E_IDX) return (8);   /* Verify information */
         e_ptr = &e_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > e_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!e_ptr->name) e_ptr->name = ++name_size;

         strcpy(e_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

      /* There better be a current e_ptr */
      if (!e_ptr) return (3);

      /* Process 'D' for "Description" */
      if (buf[0] == 'D')
      {
         s = buf+2;   /* Acquire the text */

         /* Hack -- Verify space */
         if (text_size + strlen(s) > e_text_size - 8) return (7);

         if (!e_ptr->text)
            e_ptr->text = ++text_size; /* Advance and Save the text index */
         else
         /* pop in a space if necessary */
         {
            if (*(e_text + text_size - 1) != ' ')
            {
               *(e_text + text_size) = ' ';
               *(e_text + ++text_size) = 0;
            }
         }

         strcpy(e_text + text_size, s); /* Append chars to the name */
         text_size += strlen(s);
         continue;
      }
      switch (buf[0])
      {
         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_e_info_xtra(e_ptr, buf);
                   break;
         /* Process 'W' for "More Info" (one line only) */
         case 'W': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_e_info_more_info(e_ptr, buf);
                   break;
         /* Hack -- Process 'C' for "creation" */
         case 'C': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_e_info_attr1(e_ptr, buf);
                   break;
         /* Hack -- get minimum values for some attributes */
         case 'M': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_e_info_attr2(e_ptr, buf);
                   break;
         /* Hack -- Process 'F' for flags */
         case 'F': ret=get_e_info_flags(e_ptr, buf);
                   break;
         default:  return (6);
      }
      if (ret) return(ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;
   e_number = i + 1;

   /* Hack -- extract the "ignore" flags */
   for (i = 0; i < e_number; i++)
   {
      e_ptr = &e_info[i];

      /* Skip non-artifacts */
      if (!e_ptr->name) continue;

      /* Ignore when resistant */
      if (e_ptr->flags2 & TR2_RES_ACID) e_ptr->flags3 |= (TR3_IGNORE_ACID);
      if (e_ptr->flags2 & TR2_RES_ELEC) e_ptr->flags3 |= (TR3_IGNORE_ELEC);
      if (e_ptr->flags2 & TR2_RES_FIRE) e_ptr->flags3 |= (TR3_IGNORE_FIRE);
      if (e_ptr->flags2 & TR2_RES_COLD) e_ptr->flags3 |= (TR3_IGNORE_COLD);

      /* Ignore when immune */
      if (e_ptr->flags2 & TR2_IM_ACID) e_ptr->flags3 |= (TR3_IGNORE_ACID);
      if (e_ptr->flags2 & TR2_IM_ELEC) e_ptr->flags3 |= (TR3_IGNORE_ELEC);
      if (e_ptr->flags2 & TR2_IM_FIRE) e_ptr->flags3 |= (TR3_IGNORE_FIRE);
      if (e_ptr->flags2 & TR2_IM_COLD) e_ptr->flags3 |= (TR3_IGNORE_COLD);
   }

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed ego-item array size at %d, found %d\n",
                    MAX_E_IDX, e_number);
   dlog(DEBUGALWAYS,"Angband: guessed ego-item name array size at %ld, found %ld\n",
              e_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed ego-item text array size at %ld, found %ld\n",
              e_text_size, text_size);
   e_name_size = name_size;
   e_text_size = text_size;

   write_info_header("e_info.hdr", e_number, e_name_size, e_text_size);
   write_info_data("e_info.raw", (char*)e_info, e_number, sizeof(ego_item_type),
                   e_name, e_name_size, e_text, e_text_size);

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

static s16b get_t_info_flags(trap_type *t_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry textually */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

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
   return(0);
}

static s16b get_t_info_integers(trap_type *t_ptr, char *buf)
{
   int probability, another, p1valinc, difficulty, minlevel;
   int dd,ds;
   char color;

   /* Scan for the values */
   if (8 != sscanf(buf+2, "%d:%d:%d:%d:%d:%dd%d:%c",
       &difficulty, &probability, &another, &p1valinc, &minlevel,
       &dd,&ds,&color)) return (1);

   t_ptr->difficulty  = (byte)difficulty;
   t_ptr->probability = (s16b)probability;
   t_ptr->another     = (s16b)another;
   t_ptr->p1valinc     = (s16b)p1valinc;
   t_ptr->minlevel    = (byte)minlevel;
   t_ptr->dd          = (s16b)dd;
   t_ptr->ds          = (s16b)ds;
   t_ptr->color       = color_char_to_attr(color);

   return(0);
}


/*
 * Initialize the "t_info" array, by parsing an ascii "template" file
 */
errr init_t_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   trap_type *t_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = strchr(buf+2, ':');    /* Find the colon before the name */
         *s++ = '\0';               /* Nuke the colon, advance to the name */
         /* Paranoia -- require a name */
         if (!*s) return (1);       /* Verify that colon */
         i = atoi(buf+2);           /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_T_IDX) return (8);  /* Verify information */
         t_ptr = &t_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > t_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!t_ptr->name) t_ptr->name = ++name_size;

         strcpy(t_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

       /* There better be a current e_ptr */
       if (!t_ptr) return (3);

      /* Process 'D' for "Description" */
      if (buf[0] == 'D')
      {
         s = buf+2;   /* Acquire the text */

         /* Hack -- Verify space */
         if (text_size + strlen(s) > t_text_size - 8) return (7);

         if (!t_ptr->text)
            t_ptr->text = ++text_size; /* Advance and Save the text index */
         else
         /* pop in a space if necessary */
         {
            if (*(t_text + text_size - 1) != ' ')
            {
               *(t_text + text_size) = ' ';
               *(t_text + ++text_size) = 0;
            }
         }

         strcpy(t_text + text_size, s); /* Append chars to the name */
         text_size += strlen(s);
         continue;
      }
      switch (buf[0])
      {
         /* Hack -- Process 'F' for flags */
         case 'F': ret=get_t_info_flags(t_ptr, buf);
                   break;
         /* Hack -- Process 'I' for "integers" - or whatever  */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret=get_t_info_integers(t_ptr, buf);
                   break;
         default:  return (6);
      }
      if (ret) return(ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;
   t_number = i + 1;

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed trap array size at %d, found %d\n",
                    MAX_T_IDX, t_number);
   dlog(DEBUGALWAYS,"Angband: guessed trap name array size at %ld, found %ld\n",
              t_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed trap text array size at %ld, found %ld\n",
              t_text_size, text_size);
   t_name_size = name_size;
   t_text_size = text_size;

   write_info_header("t_info.hdr", t_number, t_name_size, t_text_size);
   write_info_data("t_info.raw", (char*)t_info, t_number, sizeof(trap_type),
                   t_name, t_name_size, t_text, t_text_size);

   /* Success */
   return (0);
}

/*
 * Grab one (basic) flag in a monster_race from a textual string
 */
static errr grab_one_basic_monster_flag(monster_race *r_ptr, cptr what)
{
   s16b i = 0;
   u64b my_flags = 0LL;

   test_one_monster_flag(&my_flags, &i, what);
   switch(i)
   {
      case 1: r_ptr->flags1 |= my_flags;
              return(0);
      case 2: r_ptr->flags2 |= my_flags;
              return(0);
      case 3: r_ptr->flags3 |= my_flags;
              return(0);
      default: msg_format("Illegal monster race flag '%s'.", what);
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
{
   s16b i = 0;
   u64b my_flags = 0LL;

   test_one_monster_flag(&my_flags, &i, what);
   switch(i)
   {
      case 4: r_ptr->flags4 |= my_flags;
              return(0);
      case 5: r_ptr->flags5 |= my_flags;
              return(0);
      case 6: r_ptr->flags6 |= my_flags;
              return(0);
      default: msg_format("Illegal monster spell flag '%s'.", what);
   }

   /* Oops */
   msg_format("Unknown monster flag '%s'.", what);

   /* Failure */
   return (1);
}

   /* Oops */
   msg_format("Unknown monster flag '%s'.", what);

   /* Failure */
   return (1);
}

static errr add_one_corpse_gives_flag_alw(monster_race *r_ptr, cptr what)
{
   s16b i;

   /* Check flags1 */
   for (i = 0; i < 32; i++)
   {
      if (streq(what, corpse_gives_flags[i]))
      {
         r_ptr->corpse_gives_alw |= (1L << i);
         return (0);
      }
   }
   msg_format("Unknown corpse_gives_alw flag '%s'.", what);

   /* Error */
   return (1);
}

static errr add_one_corpse_gives_flag_smt(monster_race *r_ptr, cptr what)
{
   s16b i;

   /* Check flags1 */
   for (i = 0; i < 32; i++)
   {
      if (streq(what, corpse_gives_flags[i]))
      {
         r_ptr->corpse_gives_smt |= (1L << i);
         return (0);
      }
   }
   msg_format("Unknown corpse_gives_smt flag '%s'.", what);

   /* Error */
   return (1);
}

static errr add_one_corpse_takes_flag_alw(monster_race *r_ptr, cptr what)
{
   s16b i;

   /* Check flags1 */
   for (i = 0; i < 32; i++)
   {
      if (streq(what, corpse_takes_flags[i]))
      {
         r_ptr->corpse_takes_alw |= (1L << i);
         return (0);
      }
   }
   msg_format("Unknown corpse_takes_alw flag '%s'.", what);

   /* Error */
   return (1);
}

static errr add_one_corpse_takes_flag_smt(monster_race *r_ptr, cptr what)
{
   s16b i;

   /* Check flags1 */
   for (i = 0; i < 32; i++)
   {
      if (streq(what, corpse_takes_flags[i]))
      {
         r_ptr->corpse_takes_smt |= (1L << i);
         return (0);
      }
   }
   msg_format("Unknown corpse_takes_smt flag '%s'.", what);

   /* Error */
   return (1);
}

static errr process_corpse_gives_alw(monster_race *r_ptr, char *what)
{
   char *s, *t;

   /* Parse every entry */
   for (s = what; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while ((*t == ' ') || (*t == '|')) t++;
      }

      /* Parse this entry */
      if (0 != add_one_corpse_gives_flag_alw(r_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static errr process_corpse_gives_smt(monster_race *r_ptr, char *what)
{
   char *s, *t;

   /* Parse every entry */
   for (s = what; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while ((*t == ' ') || (*t == '|')) t++;
      }

      /* Parse this entry */
      if (0 != add_one_corpse_gives_flag_smt(r_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static errr process_corpse_takes_alw(monster_race *r_ptr, char *what)
{
   char *s, *t;

   /* Parse every entry */
   for (s = what; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while ((*t == ' ') || (*t == '|')) t++;
      }

      /* Parse this entry */
      if (0 != add_one_corpse_takes_flag_alw(r_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static errr process_corpse_takes_smt(monster_race *r_ptr, char *what)
{
   char *s, *t;

   /* Parse every entry */
   for (s = what; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while ((*t == ' ') || (*t == '|')) t++;
      }

      /* Parse this entry */
      if (0 != add_one_corpse_takes_flag_smt(r_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static s16b init_r_info_graphics(monster_race *r_ptr, char *buf)
{
   char sym;
   s16b tmp;

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
   return (0);
}

static s16b init_r_info_info(monster_race *r_ptr, char *buf)
{
   int spd, hp1, hp2, aaf, ac, slp;

   /* Scan for the other values */
   if (6 != sscanf(buf+2, "%d:%dd%d:%d:%d:%d",
       &spd, &hp1, &hp2, &aaf, &ac, &slp)) return (1);

   /* Save the values */
   r_ptr->speed = (s16b)spd;
   r_ptr->hdice = (s16b)hp1;
   r_ptr->hside = (s16b)hp2;
   r_ptr->aaf = (s16b)aaf;
   r_ptr->ac = (s16b)ac;
   r_ptr->sleep = (s16b)slp;
   return (0);
}

static s16b init_r_info_more(monster_race *r_ptr, char *buf)
{
   int lev, rar, max_generations;
   long exp;

   /* Scan for the values */
   if (4 != sscanf(buf+2, "%d:%d:%ld:%d",
       &lev, &rar, &exp, &max_generations)) return (1);

   /* Save the values */
   r_ptr->level = (s16b)lev;
   r_ptr->rarity = (s16b)rar;
   r_ptr->mexp = exp;
   r_ptr->max_gen = (byte)max_generations;
   return (0);
}

static s16b init_r_info_blows(monster_race *r_ptr, char *buf)
{
   s16b n1, n2, i;
   char *s, *t;

   /* Find the next empty blow slot (if any) */
   for (i = 0; i < 4; i++) if (!r_ptr->blow[i].method) break;

   /* Oops, no more slots */
   if (i == 4) return (1);

   /* Analyze the first field */
   for (s = t = buf+2; *t && (*t != ':'); t++) ;

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
   for (s = t; *t && (*t != ':'); t++) ;

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
   for (s = t; *t && (*t != 'd'); t++) ;

   /* Terminate the field (if necessary) */
   if (*t == 'd') *t++ = '\0';

   /* Save the method */
   r_ptr->blow[i].method = n1;

   /* Save the effect */
   r_ptr->blow[i].effect = n2;

   /* Extract the damage dice and sides */
   r_ptr->blow[i].d_dice = atoi(s);
   r_ptr->blow[i].d_side = atoi(t);
   return (0);
}

static s16b init_r_info_flags(monster_race *r_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while (*t == ' ' || *t == '|') t++;
      }

      /* Parse this entry */
      if (0 != grab_one_basic_monster_flag(r_ptr, s)) return (5);

      /* Start the next entry */
      s = t;
   }
   return (0);
}

static s16b init_r_info_corpse(monster_race *r_ptr, char *buf)
{
   int chance, nutrition, weight, chance_gives, chance_takes;
   int chance_bones;
   long spoiling;
   s16b err = 0;

   switch(buf[6])
   {
      case 'F': if (7 != sscanf(buf+8, "%d:%d:%d:%ld:%d:%d:%d",
                                &chance,
                                &nutrition,
                                &weight,
                                &spoiling,
                                &chance_gives,
                                &chance_takes,
                                &chance_bones)) return (1);

                r_ptr->corpse_chance = (s16b) chance;
                r_ptr->corpse_nutrition = (s16b) nutrition;
                r_ptr->corpse_weight = (s16b) weight;
                r_ptr->corpse_spoiling = (s32b) spoiling;
                r_ptr->corpse_chance_bones = (s16b) chance_bones;
                r_ptr->corpse_chance_gives = (byte) chance_gives;
                r_ptr->corpse_chance_takes = (byte) chance_takes;

                break;
      case 'G': if (buf[7]=='A')
                   err=process_corpse_gives_alw(r_ptr, buf+9);
                else if (buf[7]=='S')
                   err=process_corpse_gives_smt(r_ptr, buf+9);
                else
                   quit(format("Unknown corpse flag type in %s in r_info.txt", buf));
                break;
      case 'T': if (buf[7]=='A')
                   err=process_corpse_takes_alw(r_ptr, buf+9);
                else if (buf[7]=='S')
                   err=process_corpse_takes_smt(r_ptr, buf+9);
                else
                   quit(format("Unknown corpse flag type in %s in r_info.txt", buf));
                break;
      default:  quit(format("Unknown corpse flag type in %s in r_info.txt", buf));
                break;
   }
   return (err);
}

static s16b init_r_info_spells(monster_race *r_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
      int fr;

      /* Find the end of this entry */
      for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

      /* Nuke and skip any dividers */
      if (*t)
      {
         *t++ = '\0';
         while ((*t == ' ') || (*t == '|')) t++;
      }

      /* XXX XXX XXX Hack -- Read spell frequency */
      if (1 == sscanf(s, "1_IN_%d", &fr))
      {
         /* Extract a "frequency" */
         r_ptr->freq_spell = r_ptr->freq_inate = 100 / (s16b)fr;

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
   return (0);
}

/*
 * Initialize the "r_info" array, by parsing an ascii "template" file
 */
errr init_r_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* Current entry */
   monster_race *r_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == my_fgets(fp, buf, 1024))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = buf+2;                 /* Find the colon before the name */
         if (!s) return (1);        /* Verify that colon */
         i++;                       /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_R_IDX) return (8);   /* Verify information */
         r_ptr = &r_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > r_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!r_ptr->name) r_ptr->name = ++name_size;

         strcpy(r_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */

         continue;                       /* Next... */
      }

      /* There better be a current r_ptr */
      if (!r_ptr) return (3);

      /* Process 'D' for "Description" */

      if (buf[0] == 'D')
      {
          s = buf+2;   /* Acquire the text */

          /* Hack -- Verify space */
          if (text_size + strlen(s) > r_text_size - 8) return (7);

          if (!r_ptr->text)
             r_ptr->text = ++text_size; /* Advance and Save the text index */
          else
          {
               if (*(r_text + text_size - 1) != ' ')
               {
                  *(r_text + text_size) = ' ';
                  *(r_text + ++text_size) = 0;
               }
          }

          strcpy(r_text + text_size, s); /* Append chars to the name */
          text_size += strlen(s);
          continue;
      }
      switch (buf[0])
      {
         /* Process 'G' for "Graphics" (one line only) */
         case 'G': ret = init_r_info_graphics(r_ptr, buf);
                   break;
         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_r_info_info(r_ptr, buf);
                   break;
         /* Process 'W' for "More Info" (one line only) */
         case 'W': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_r_info_more(r_ptr, buf);
                   break;
         /* Process 'B' for "Blows" (up to four lines) */
         case 'B': ret = init_r_info_blows(r_ptr, buf);
                   break;
         /* Process 'F' for "Basic Flags" (multiple lines) */
         case 'F': ret = init_r_info_flags(r_ptr, buf);
                   break;
         /* Process 'C' for "Corpse Flags" (multiple lines) */
         case 'C': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_r_info_corpse(r_ptr, buf);
                   break;
         /* Process 'S' for "Spell Flags" (multiple lines) */
         case 'S': ret = init_r_info_spells(r_ptr, buf);
                   break;
         default:  return (6);
      }
      if (ret) return (ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;

   r_number = i + 1;
   r_number_total = i + 1 + MAX_GHOSTS;

   /* clear the ghosts */
   for (i=r_number ; i < r_number_total; i++)
   {
      (void)WIPE(&r_info[i], monster_race);
   }

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed monster array size at %d, found %d total %d\n",
                    MAX_R_IDX, r_number, r_number_total);
   dlog(DEBUGALWAYS,"Angband: guessed monster name array size at %ld, found %ld total %ld\n",
              r_name_size, name_size, r_text_size_total);
   dlog(DEBUGALWAYS,"Angband: guessed monster text array size at %ld, found %ld total %ld\n",
              r_text_size, text_size, r_text_size_total);
   r_name_size = name_size;
   r_text_size = text_size;
   r_name_size_total = r_name_size + MAX_GHOSTS * 32;
   r_text_size_total = r_text_size + MAX_GHOSTS * 240;

   /* wipe the ghost slots */
   for (i=r_name_size; i<r_name_size_total; i++)
   {
      *(r_name + i) = 0;
   }
   for (i=r_text_size; i<r_text_size_total; i++)
   {
      *(r_text + i) = 0;
   }

   /* we write two headers, read init2.c: init_r_info() for comments */
   write_info_header("r_info.hdr", r_number, r_name_size, r_text_size);
   write_info_header("r_info_t.hdr", r_number_total, r_name_size_total, r_text_size_total);
   write_info_data("r_info.raw", (char*)r_info, r_number_total, sizeof(monster_race),
                   r_name, r_name_size_total, r_text, r_text_size_total);
   return (0);
}

static s16b init_s_info_info(spell_type *s_ptr, char *buf)
{
   int level,mana,chance,minfail;

   /* Scan for the other values */
   if (4 != sscanf(buf+2, "%d:%d:%d:%d",
       &level, &mana, &chance, &minfail)) return (1);

   /* Save the values */
   s_ptr->level = (s16b)level;
   s_ptr->mana = (s16b)mana;
   s_ptr->chance = (s16b)chance;
   s_ptr->minfail = (s16b)minfail;
   return (0);
}

static s16b init_s_info_chars(spell_type *s_ptr, char *buf)
{
   char chars[8];
   char colors[8];
   bool do_chars = TRUE;
   s16b i,j;

   /* Scan for the other values */
   i=0;
   j=0;
   while (buf[2+i])
   {

      if (buf[2+i]==':')
      {
         do_chars=FALSE;
         j=-1;
      }
      else
      {
         if (do_chars)
         {
            chars[j]=buf[2+i];
            chars[j+1]=0;
         }
         else
         {
            colors[j]=buf[2+i];
            colors[j+1]=0;
         }
      }
      i++;
      j++;
      if (j==8) return(1);
   }

   if (strlen(chars)!=strlen(colors)) return(1);

   /* Save the values */
   strcpy(s_ptr->chr, chars);
   for (i=0; i<strlen(colors);i++)
   {
      s_ptr->col[i]=color_char_to_attr(colors[i]);
   }
   return (0);
}


static errr add_spell_class_flag(spell_type *s_ptr, cptr what)
{
   s16b i;

   for (i = 0; i < MAX_CLASS; i++)
   {
      if (streq(what, spell_info_class_flags[i]))
      {
         s_ptr->sclass |= (1L << i);
         return (0);
      }
   }
   msg_format("Unknown spell class flag '%s'.", what);

   /* Error */
   return (1);
}

static errr add_spell_scale_flag(spell_type *s_ptr, cptr what)
{
   s16b i;

   for (i = 0; i < MAX_SPELL_SCALES; i++)
   {
      if (streq(what, spell_info_scale_flags[i]))
      {
         s_ptr->scale = i;
         return (0);
      }
   }
   msg_format("Unknown spell class flag '%s'.", what);

   /* Error */
   return (1);
}

static errr add_spell_type_flag(spell_type *s_ptr, cptr what)
{
   s16b i;

   for (i = 0; i < MAX_SPELL_TYPES; i++)
   {
      if (streq(what, spell_info_type_flags[i]))
      {
         s_ptr->type=i;
         return (0);
      }
   }
   msg_format("Unknown spell type flag '%s'.", what);

   /* Error */
   return (1);
}

static s16b init_s_info_class(spell_type *s_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
       /* Find the end of this entry */
       for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

       /* Nuke and skip any dividers */
       if (*t)
       {
           *t++ = '\0';
           while (*t == ' ' || *t == '|') t++;
       }

       /* Parse this entry */
       if (0 != add_spell_class_flag(s_ptr, s)) return (5);

       /* Start the next entry */
       s = t;
   }
   return (0);
}

static s16b init_s_info_scale(spell_type *s_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
       /* Find the end of this entry */
       for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

       /* Nuke and skip any dividers */
       if (*t)
       {
           *t++ = '\0';
           while (*t == ' ' || *t == '|') t++;
       }

       /* Parse this entry */
       if (0 != add_spell_scale_flag(s_ptr, s)) return (5);

       /* Start the next entry */
       s = t;
   }
   return (0);
}

static s16b init_s_info_type(spell_type *s_ptr, char *buf)
{
   char *s, *t;

   /* Parse every entry */
   for (s = buf + 2; *s; )
   {
       /* Find the end of this entry */
       for (t = s; *t && (*t != ' ') && (*t != '|'); ++t) ;

       /* Nuke and skip any dividers */
       if (*t)
       {
           *t++ = '\0';
           while (*t == ' ' || *t == '|') t++;
       }

       /* Parse this entry */
       if (0 != add_spell_type_flag(s_ptr, s)) return (5);

       /* Start the next entry */
       s = t;
   }
   return (0);
}

/*
 * Initialize the "s_info" array, by parsing an ascii "template" file
 */
errr init_s_info_txt(FILE *fp, char *buf)
{
   s16b i = -1, ret;

   char *s;

   /* Not ready yet */
   bool okay = FALSE;

   /* return-code from fgets */

   /* Current entry */
   spell_type *s_ptr = NULL;

   /* Start the "fake" stuff */
   u16b name_size = 0;
   u16b text_size = 0;

   error_idx = 0;
   error_line = 0;

   /* Parse */
   while (0 == (ret=my_fgets(fp, buf, 1024)))
   {
      /* Advance the line number */
      error_line++;

      /* Skip comments and blank lines */
      if (!buf[0] || (buf[0] == '#')) continue;

      /* Hack -- Process 'V' for "Version" */
      if (buf[0] == 'V')
      {
         ret=init_test_version(buf);
         if (ret) return(ret);
         okay = TRUE;
         continue;
      }

      /* No version yet */
      if (!okay) return (2);

      /* Process 'N' for "New/Number/Name" */
      if (buf[0] == 'N')
      {
         s = strchr(buf+2, ':');    /* Find the colon before the name */
         *s++ = '\0';               /* Nuke the colon, advance to the name */
         /* Paranoia -- require a name */
         if (!*s) return (1);       /* Verify that colon */
         i = atoi(buf+2);           /* Get the index */
         error_idx = i;             /* Save the index */
         if (i >= MAX_S_IDX) return (8);   /* Verify information */
         s_ptr = &s_info[i];        /* Point at the "info" */

         /* Hack -- Verify space */
         if (name_size + strlen(s) > s_name_size - 8) return (7);
         /* Advance and Save the name index */
         if (!s_ptr->name) s_ptr->name = ++name_size;

         strcpy(s_name + name_size, s);  /* Append chars to the name */
         name_size += strlen(s);         /* Advance the index */
         continue;                       /* Next... */
      }

      /* There better be a current r_ptr */
      if (!s_ptr) return (3);

      /* Process 'D' for "Description" */

      if (buf[0] == 'D')
      {
         s = buf+2;   /* Acquire the text */

         /* Hack -- Verify space */
         if (text_size + strlen(s) > s_text_size - 8) return (7);

         if (!s_ptr->text)
            s_ptr->text = ++text_size; /* Advance and Save the text index */
         else
         /* pop in a space if necessary */
         {
            if (*(s_text + text_size - 1) != ' ')
            {
               *(s_text + text_size) = ' ';
               *(s_text + ++text_size) = 0;
            }
         }

         strcpy(s_text + text_size, s); /* Append chars to the name */
         text_size += strlen(s);
         continue;
      }

      switch (buf[0])
      {
         /* Process 'I' for "Info" (one line only) */
         case 'I': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_s_info_info(s_ptr, buf);
                   break;
         /* Process 'W' for "Who - class info" (one line only) */
         case 'W': ret = init_s_info_class(s_ptr, buf);
                   break;
         /* Process 'S' for "scale info" (one line only) */
         case 'S': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_s_info_scale(s_ptr, buf);
                   break;
         /* Process 'T' for "type info" (one line only) */
         case 'T': ret=clean_buf(buf);
                   if (ret) return (ret);
                   ret = init_s_info_type(s_ptr, buf);
                   break;
         /* Process 'C' for "char info" (one line only) */
         case 'C': ret = init_s_info_chars(s_ptr, buf);
                   break;
         default:  return (6);
      }
      if (ret) return (ret);
   }

   /* Complete the "name" and "text" sizes */
   ++name_size;
   ++text_size;

   s_number = i + 1;

   if (error_line == 0) return(10);

   /* No version yet */
   if (!okay) return (2);

   dlog(DEBUGALWAYS,"Angband: guessed spell array size at %d, found %d\n",
                    MAX_S_IDX, s_number);
   dlog(DEBUGALWAYS,"Angband: guessed spell name array size at %ld, found %ld\n",
              s_name_size, name_size);
   dlog(DEBUGALWAYS,"Angband: guessed spell text array size at %ld, found %ld\n",
              s_text_size, text_size);
   s_name_size = name_size;
   s_text_size = text_size;

   write_info_header("s_info.hdr", s_number, s_name_size, s_text_size);
   write_info_data("s_info.raw", (char*)s_info, s_number, sizeof(spell_type),
                   s_name, s_name_size, s_text, s_text_size);

   return (0);
}
