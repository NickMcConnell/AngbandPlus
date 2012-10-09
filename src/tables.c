/* File: tables.c */

/* Purpose: Angband Tables */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


debug_flag_type debug_flag[32] =
{
   { "SAVE",  "savefile handling in general" },                     /* 0x00000001L */
   { "SAVE2", "savefile handling (detailed) **" },                  /* 0x00000002L */
   { "TRAPS", "trap handling" },                                    /* 0x00000004L */
   { "ITEMS", "item handling" },                                    /* 0x00000008L */
   { "MOVES", "character movement" },                               /* 0x00000010L */
   { "LIGHT", "lighting effects **" },                              /* 0x00000020L */
   { "GENER", "dungeon generation" },                               /* 0x00000040L */
   { "GENER2", "dungeon generation (detailed) **" },                /* 0x00000080L */
   { "STORE", "store handling"},                                    /* 0x00000100L */
   { "PREF", "preference file handling" },                          /* 0x00000200L */
   { "MESG", "message handling **" },                               /* 0x00000400L */
   { "FLOW", "program flow in general"},                            /* 0x00000800L */
   { "ARENA", "arena handling" },                                   /* 0x00001000L */
   { "PROJ", "projecting computations" },                           /* 0x00002000L */
   { "LOS", "line-of-sight computations **" },                      /* 0x00004000L */
   { "FIGHT", "fighting in general" },                              /* 0x00008000L */
   { "MACRO", "macro handling" },                                   /* 0x00010000L */
   { "GHOST", "ghost handling" },                                   /* 0x00020000L */
   { "ALLOC", "array allocation handling" },                        /* 0x00040000L */
   { "TEMPL", "template parsing" },                                 /* 0x00080000L */
   { "MONST", "monster birth" },                                    /* 0x00100000L */
   { "MONAI", "monster AI and movement **" },                       /* 0x00200000L */
   { "BIRTH", "character birth" },                                  /* 0x00400000L */
   { "CMPRS", "savefile compression" },                             /* 0x00800000L */
   { "MSGS",  "send all messages also to debuglog" },               /* 0x01000000L */
   { "NONE1", "reserved" },                                         /* 0x02000000L */
   { "KEYS",  "debug key-presses & macro-handling" },               /* 0x04000000L */
   { "NONE1", "reserved" },                                         /* 0x08000000L */
   { "NONE1", "reserved" },                                         /* 0x10000000L */
   { "NONE1", "reserved" },                                         /* 0x20000000L */
   { "NONE1", "reserved" },                                         /* 0x40000000L */
   { "EXTRA", "temporary debug messages" }                          /* 0x80000000L */
};

/*
 * Global array for looping through the "keypad directions"
 */
s16b ddd[9] = { 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/* jk */
cptr dirstr[10] =
{
   "below you 1",             /* 0 */
   "to the south-west",       /* 1 */
   "to the south",            /* 2 */
   "to the south-east",       /* 3 */
   "to the west",             /* 4 */
   "below you 2",             /* 5 */
   "to the east",             /* 6 */
   "to the north-west",       /* 7 */
   "to the north",            /* 8 */
   "to the north-east"        /* 9 */
};


/* jk */
/* about the knowledge of traps - meant as that you %s, knowstr */
cptr knowstr[10] = { "have never studied before",      /* 0 */
                     "vaguely remember",               /* 1 */
                     "distinctly remember",            /* 2 */
                     "remember quite well",            /* 3 */
                     "have seen many times before",    /* 4 */
                     "have examined",                  /* 5 */
                     "have studied",                   /* 5 */
                     "have examined in detail",        /* 7 */
                     "have studied in detail",         /* 8 */
                     "understand completely" };        /* 9 */

cptr slidingstr[10] = { "You slide in another direction.",
                        "You can't turn on your slippery feet.",
                        "Your feet slip.",
                        "You have no grip.",
                        "Your feet have a life of their own.",
                        "The floor is very smooth here.",
                        "You try to walk, but you keep sliding.",
                        "You feet are coated in ice.",
                        "You can't walk on these feet.",
                        "Your feet slip away."};

cptr arena_welcome [7][4] =
{
  {  "Why come here at all for so little money?",
     "You won't get the crowd's support this way.",
     "Don't you need the money?",
     "Did I hear that correctly? Miser!" },
  {  "That's a bit cowardly.",
     "Being rich has it's advantages.",
     "Money doesn't stink, you know.",
     "Promise me next time will be different." },
  {  "If I say jump, do you ask how high?",
     "Thinking is difficult, not?",
     "Mr Standard, welcome to our establishment",
     "That's not original" },
  {  "There's that money look in your eyes.",
     "Do you really think you can do this?",
     "Does your mother know about this?",
     "HaHo! Enter Mr. Hero!" },
  {  "I see. A bit short of money, no?",
     "You can come back a second time!",
     "I can pay that, don't you worry now.",
     "Let me think - do I still have an adequate opponent?" },
  {  "You must be a very special someone...",
     "I don't know if I have any opponents equal to the task.",
     "Let me check my safe first.",
     "Would you like that in coins or notes?"},
  {  "I can't pay that!",
     "You look much too wimpy for that amount!",
     "If you want to fight Ancient Wyrms of Chaos, I'd suggest the dungeon!",
     "If you want to fight Ancient Balrogs in great numbers, try the dungeon!"}
};

cptr arena_leave_items [7][4] =
{
  {  "Do you also clean homes?",
     "You sorry miser - you're not leaving me anything!",
     "Please, mighty warrior, consider my hungry children?",
     "You young lout - you may have won, but you have bad manners!" },
  {  "What do I need this low-valued crap for?",
     "Please keep my arena free of rubble in the future!",
     "I can understand why you left this, you miser.",
     "What a laugh - I haven't seen such bad quality in years...." },
  {  "I see you didn't need this?",
     "Being rich has it's advantages, obviously.",
     "What am I going to do with another one of these?",
     "Hey! I'm the arena-master, not some rubbish-remover!" },
  {  "Why leave these items?",
     "You have no need of this?",
     "Really now, no true warrior would leave this behind!",
     "You must already have everything!" },
  {  "Perhaps you should have identified this!",
     "Are you sure you won't regret this?",
     "Didn't you need this fine gift?",
     "You may not need it, but I do!" },
  {  "I think you made a mistake here!",
     "Hahahaha - you should have checked, you really should have!",
     "Oh boy, wait until I tell this to my friends!",
     "You do know what do left don't you, stupid?" },
  {  "You must be a very special someone.",
     "Wow - even a king wouldn't leave this.",
     "I'm not sure the black market has enough money to buy this.....",
     "Aaah - it's the fabled lost artifact of the old tales..."}
};


/*
 * Global arrays for converting "keypad direction" into offsets
 */
/*               0  1  2  3  4  5  6  7  8  9 */
/*                  SW S  SE E     W  NW N  NE */

s16b ddx[10] = { 0,-1, 0, 1,-1, 0, 1,-1, 0, 1 };
s16b ddy[10] = { 0, 1, 1, 1, 0, 0, 0,-1,-1,-1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 */
s16b ddx_ddd[9] = { 0, 0, 1, -1, 1, -1, 1, -1, 0 };
s16b ddy_ddd[9] = { 1, -1, 0, 0, 1, 1, -1, -1, 0 };

/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
char hexsym[16] =
{
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};

/*
 * Object flag names
 */
cptr object_flag_names[3][64] =
{
   {
      "Add Str",                        /* 0x0000000000000001LL */
      "Add Int",                        /* 0x0000000000000002LL */
      "Add Wis",                        /* 0x0000000000000004LL */
      "Add Dex",                        /* 0x0000000000000008LL */
      "Add Con",                        /* 0x0000000000000010LL */
      "Add Chr",                        /* 0x0000000000000020LL */
      NULL,                             /* 0x0000000000000040LL */
      "Add Stl",                        /* 0x0000000000000080LL */
      "Add Srch",                       /* 0x0000000000000100LL */
      "Add Infr",                       /* 0x0000000000000200LL */
      "Add Tun.",                       /* 0x0000000000000400LL */
      "Add Spd",                        /* 0x0000000000000800LL */
      "Extr Att",                       /* 0x0000000000001000LL */
      "Ex Might",                       /* 0x0000000000002000LL */
      "Ex Shots",                       /* 0x0000000000004000LL */
      "Add Magc",                       /* 0x0000000000008000LL */
      "Slay Anm",                       /* 0x0000000000010000LL */
      "Kill Anm,",                      /* 0x0000000000020000LL */
      "Slay Evil",                      /* 0x0000000000040000LL */
      "Kill Evil",                      /* 0x0000000000080000LL */
      "Slay Und",                       /* 0x0000000000100000LL */
      "Kill Und",                       /* 0x0000000000200000LL */
      "Slay Demn",                      /* 0x0000000000400000LL */
      "Kill Demn",                      /* 0x0000000000800000LL */
      "Slay Orc",                       /* 0x0000000001000000LL */
      "Kill Orc",                       /* 0x0000000002000000LL */
      "Slay Trol",                      /* 0x0000000004000000LL */
      "Kill Trol",                      /* 0x0000000008000000LL */
      "Slay Gnt",                       /* 0x0000000010000000LL */
      "Kill Gnt",                       /* 0x0000000020000000LL */
      "Slay Drag",                      /* 0x0000000040000000LL */
      "Kill Drag",                      /* 0x0000000080000000LL */
      "Vampiric",                       /* 0x0000000100000000LL */
      "Impact",                         /* 0x0000000200000000LL */
      NULL,                             /* 0x0000000400000000LL */
      "Acid Brand",                     /* 0x0000000800000000LL */
      "Elec Brand",                     /* 0x0000001000000000LL */
      "Fire Brand",                     /* 0x0000002000000000LL */
      "Cold Brand",                     /* 0x0000004000000000LL */
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
   },
   {
      "Sust Str",
      "Sust Int",
      "Sust Wis",
      "Sust Dex",
      "Sust Con",
      "Sust Chr",
      NULL,
      NULL,
      "Imm Acid",
      "Imm Elec",
      "Imm Fire",
      "Imm Cold",
      NULL,
      NULL,
      "Free Act",
      "Hold Life",
      "Res Acid",
      "Res Elec",
      "Res Fire",
      "Res Cold",
      "Res Pois",
      "Res Fear",
      "Res Lite",
      "Res Dark",
      "Res Blind",
      "Res Conf",
      "Res Sound",
      "Res Shard",
      "Res Neth",
      "Res Nexus",
      "Res Chaos",
      "Res Disen",
      "Vampiric.",
      "Impact",
      NULL,
      "Acid Brand",
      "Elec Brand",
      "Fire Brand",
      "Cold Brand",
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL
   },
   {
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        "Easy Know",
        "Hide Type",
        "Show Mods",
        "Insta Art",
        "Feather",
        "Lite",
        "See Invis",
        "Telepathy",
        "Digestion",
        "Regen",
        NULL,
        NULL,
        "Ign Acid",
        "Ign Elec",
        "Ign Fire",
        "Ign Cold",
        "Activate",
        "Drain Exp",
        "Teleport",
        "Aggravate",
        "Blessed",
        "Cursed",
        "Hvy Curse",
        "Prm Curse",
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL
    }
};



/*
 * Stat Table (INT/WIS) -- Number of half-spells per level
 */
byte adj_mag_study[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    1   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    2   /* 12 */,
    2   /* 13 */,
    2   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    2   /* 18/00-18/09 */,
    2   /* 18/10-18/19 */,
    2   /* 18/20-18/29 */,
    2   /* 18/30-18/39 */,
    2   /* 18/40-18/49 */,
    3   /* 18/50-18/59 */,
    3   /* 18/60-18/69 */,
    3   /* 18/70-18/79 */,
    3   /* 18/80-18/89 */,
    4   /* 18/90-18/99 */,
    4   /* 18/100-18/109 */,
    4   /* 18/110-18/119 */,
    5   /* 18/120-18/129 */,
    5   /* 18/130-18/139 */,
    5   /* 18/140-18/149 */,
    5   /* 18/150-18/159 */,
    5   /* 18/160-18/169 */,
    5   /* 18/170-18/179 */,
    5   /* 18/180-18/189 */,
    5   /* 18/190-18/199 */,
    5   /* 18/200-18/209 */,
    5   /* 18/210-18/219 */,
    5   /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- extra half-mana-points per level
 */
byte adj_mag_mana[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    2   /* 9 */,
    2   /* 10 */,
    2   /* 11 */,
    2   /* 12 */,
    2   /* 13 */,
    2   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    3   /* 18/00-18/09 */,
    3   /* 18/10-18/19 */,
    3   /* 18/20-18/29 */,
    3   /* 18/30-18/39 */,
    3   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    5   /* 18/70-18/79 */,
    6   /* 18/80-18/89 */,
    7   /* 18/90-18/99 */,
    8   /* 18/100-18/109 */,
    9   /* 18/110-18/119 */,
    10  /* 18/120-18/129 */,
    11  /* 18/130-18/139 */,
    12  /* 18/140-18/149 */,
    13  /* 18/150-18/159 */,
    14  /* 18/160-18/169 */,
    15  /* 18/170-18/179 */,
    16  /* 18/180-18/189 */,
    16  /* 18/190-18/199 */,
/* jk - this was 16 all the way */
    16  /* 18/200-18/209 */,
    16  /* 18/210-18/219 */,
    17  /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- Minimum failure rate (percentage)
 */
byte adj_mag_fail[] =
{
    99  /* 3 */,
    99  /* 4 */,
    99  /* 5 */,
    99  /* 6 */,
    99  /* 7 */,
    50  /* 8 */,
    30  /* 9 */,
    20  /* 10 */,
    15  /* 11 */,
    12  /* 12 */,
    11  /* 13 */,
    10  /* 14 */,
    9   /* 15 */,
    8   /* 16 */,
    7   /* 17 */,
    6   /* 18/00-18/09 */,
    6   /* 18/10-18/19 */,
    5   /* 18/20-18/29 */,
    5   /* 18/30-18/39 */,
    5   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    4   /* 18/70-18/79 */,
    4   /* 18/80-18/89 */,
    3   /* 18/90-18/99 */,
    3   /* 18/100-18/109 */,
    2   /* 18/110-18/119 */,
    2   /* 18/120-18/129 */,
    2   /* 18/130-18/139 */,
    2   /* 18/140-18/149 */,
    1   /* 18/150-18/159 */,
    1   /* 18/160-18/169 */,
    1   /* 18/170-18/179 */,
    1   /* 18/180-18/189 */,
    1   /* 18/190-18/199 */,
    0   /* 18/200-18/209 */,
    0   /* 18/210-18/219 */,
    0   /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- Various things
 */
byte adj_mag_stat[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    1   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    1   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    3   /* 18/00-18/09 */,
    3   /* 18/10-18/19 */,
    3   /* 18/20-18/29 */,
    3   /* 18/30-18/39 */,
    3   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    5   /* 18/70-18/79 */,
    6   /* 18/80-18/89 */,
    7   /* 18/90-18/99 */,
    8   /* 18/100-18/109 */,
    9   /* 18/110-18/119 */,
    10  /* 18/120-18/129 */,
    11  /* 18/130-18/139 */,
    12  /* 18/140-18/149 */,
    13  /* 18/150-18/159 */,
    14  /* 18/160-18/169 */,
    15  /* 18/170-18/179 */,
    16  /* 18/180-18/189 */,
    17  /* 18/190-18/199 */,
    18  /* 18/200-18/209 */,
    19  /* 18/210-18/219 */,
    20  /* 18/220+ */
};


/*
 * Stat Table (CHR) -- payment percentages
 */
byte adj_chr_gold[] =
{
    130 /* 3 */,
    125 /* 4 */,
    122 /* 5 */,
    120 /* 6 */,
    118 /* 7 */,
    116 /* 8 */,
    114 /* 9 */,
    112 /* 10 */,
    110 /* 11 */,
    108 /* 12 */,
    106 /* 13 */,
    104 /* 14 */,
    103 /* 15 */,
    102 /* 16 */,
    101 /* 17 */,
    100 /* 18/00-18/09 */,
    99  /* 18/10-18/19 */,
    98  /* 18/20-18/29 */,
    97  /* 18/30-18/39 */,
    96  /* 18/40-18/49 */,
    95  /* 18/50-18/59 */,
    94  /* 18/60-18/69 */,
    93  /* 18/70-18/79 */,
    92  /* 18/80-18/89 */,
    91  /* 18/90-18/99 */,
    90  /* 18/100-18/109 */,
    89  /* 18/110-18/119 */,
    88  /* 18/120-18/129 */,
    87  /* 18/130-18/139 */,
    86  /* 18/140-18/149 */,
    85  /* 18/150-18/159 */,
    84  /* 18/160-18/169 */,
    83  /* 18/170-18/179 */,
    82  /* 18/180-18/189 */,
    81  /* 18/190-18/199 */,
    80  /* 18/200-18/209 */,
    80  /* 18/210-18/219 */,
    80  /* 18/220+ */
};


/*
 * Stat Table (INT) -- Magic devices
 */
byte adj_int_dev[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    1   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    1   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    3   /* 18/00-18/09 */,
    3   /* 18/10-18/19 */,
    4   /* 18/20-18/29 */,
    4   /* 18/30-18/39 */,
    5   /* 18/40-18/49 */,
    5   /* 18/50-18/59 */,
    6   /* 18/60-18/69 */,
    6   /* 18/70-18/79 */,
    7   /* 18/80-18/89 */,
    7   /* 18/90-18/99 */,
    8   /* 18/100-18/109 */,
    9   /* 18/110-18/119 */,
    10  /* 18/120-18/129 */,
    11  /* 18/130-18/139 */,
    12  /* 18/140-18/149 */,
    13  /* 18/150-18/159 */,
    14  /* 18/160-18/169 */,
    15  /* 18/170-18/179 */,
    16  /* 18/180-18/189 */,
    17  /* 18/190-18/199 */,
    18  /* 18/200-18/209 */,
    19  /* 18/210-18/219 */,
    20  /* 18/220+ */
};


/*
 * Stat Table (WIS) -- Saving throw
 */
byte adj_wis_sav[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    1   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    1   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    3   /* 18/00-18/09 */,
    3   /* 18/10-18/19 */,
    3   /* 18/20-18/29 */,
    3   /* 18/30-18/39 */,
    3   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    4   /* 18/60-18/69 */,
    5   /* 18/70-18/79 */,
    5   /* 18/80-18/89 */,
    6   /* 18/90-18/99 */,
    7   /* 18/100-18/109 */,
    8   /* 18/110-18/119 */,
    9   /* 18/120-18/129 */,
    10  /* 18/130-18/139 */,
    11  /* 18/140-18/149 */,
    12  /* 18/150-18/159 */,
    13  /* 18/160-18/169 */,
    14  /* 18/170-18/179 */,
    15  /* 18/180-18/189 */,
    16  /* 18/190-18/199 */,
    17  /* 18/200-18/209 */,
    18  /* 18/210-18/219 */,
    19  /* 18/220+ */
};


/*
 * Stat Table (DEX) -- disarming
 */
byte adj_dex_dis[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    0   /* 8 */,
    0   /* 9 */,
    0   /* 10 */,
    0   /* 11 */,
    0   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    1   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    4   /* 18/00-18/09 */,
    4   /* 18/10-18/19 */,
    4   /* 18/20-18/29 */,
    4   /* 18/30-18/39 */,
    5   /* 18/40-18/49 */,
    5   /* 18/50-18/59 */,
    5   /* 18/60-18/69 */,
    6   /* 18/70-18/79 */,
    6   /* 18/80-18/89 */,
    7   /* 18/90-18/99 */,
    8   /* 18/100-18/109 */,
    8   /* 18/110-18/119 */,
    8   /* 18/120-18/129 */,
    8   /* 18/130-18/139 */,
    8   /* 18/140-18/149 */,
    9   /* 18/150-18/159 */,
    9   /* 18/160-18/169 */,
    9   /* 18/170-18/179 */,
    9   /* 18/180-18/189 */,
    9   /* 18/190-18/199 */,
    10  /* 18/200-18/209 */,
    10  /* 18/210-18/219 */,
    10  /* 18/220+ */
};


/*
 * Stat Table (INT) -- disarming
 */
byte adj_int_dis[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    1   /* 8 */,
    1   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    1   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    2   /* 15 */,
    2   /* 16 */,
    2   /* 17 */,
    3   /* 18/00-18/09 */,
    3   /* 18/10-18/19 */,
    3   /* 18/20-18/29 */,
    4   /* 18/30-18/39 */,
    4   /* 18/40-18/49 */,
    5   /* 18/50-18/59 */,
    6   /* 18/60-18/69 */,
    7   /* 18/70-18/79 */,
    8   /* 18/80-18/89 */,
    9   /* 18/90-18/99 */,
    10  /* 18/100-18/109 */,
    10  /* 18/110-18/119 */,
    11  /* 18/120-18/129 */,
    12  /* 18/130-18/139 */,
    13  /* 18/140-18/149 */,
    14  /* 18/150-18/159 */,
    15  /* 18/160-18/169 */,
    16  /* 18/170-18/179 */,
    17  /* 18/180-18/189 */,
    18  /* 18/190-18/199 */,
    19  /* 18/200-18/209 */,
    19  /* 18/210-18/219 */,
    19  /* 18/220+ */
};


/*
 * Stat Table (DEX) -- bonus to ac (plus 128)
 */
byte adj_dex_ta[] =
{
    128 + -4    /* 3 */,
    128 + -3    /* 4 */,
    128 + -2    /* 5 */,
    128 + -1    /* 6 */,
    128 + 0     /* 7 */,
    128 + 0     /* 8 */,
    128 + 0     /* 9 */,
    128 + 0     /* 10 */,
    128 + 0     /* 11 */,
    128 + 0     /* 12 */,
    128 + 0     /* 13 */,
    128 + 0     /* 14 */,
    128 + 1     /* 15 */,
    128 + 1     /* 16 */,
    128 + 1     /* 17 */,
    128 + 2     /* 18/00-18/09 */,
    128 + 2     /* 18/10-18/19 */,
    128 + 2     /* 18/20-18/29 */,
    128 + 2     /* 18/30-18/39 */,
    128 + 2     /* 18/40-18/49 */,
    128 + 3     /* 18/50-18/59 */,
    128 + 3     /* 18/60-18/69 */,
    128 + 3     /* 18/70-18/79 */,
    128 + 4     /* 18/80-18/89 */,
    128 + 5     /* 18/90-18/99 */,
    128 + 6     /* 18/100-18/109 */,
    128 + 7     /* 18/110-18/119 */,
    128 + 8     /* 18/120-18/129 */,
    128 + 9     /* 18/130-18/139 */,
    128 + 9     /* 18/140-18/149 */,
    128 + 10    /* 18/150-18/159 */,
    128 + 11    /* 18/160-18/169 */,
    128 + 12    /* 18/170-18/179 */,
    128 + 13    /* 18/180-18/189 */,
    128 + 14    /* 18/190-18/199 */,
    128 + 15    /* 18/200-18/209 */,
    128 + 15    /* 18/210-18/219 */,
    128 + 15    /* 18/220+ */
};


/*
 * Stat Table (STR) -- bonus to dam (plus 128)
 */
byte adj_str_td[] =
{
    128 + -2    /* 3 */,
    128 + -2    /* 4 */,
    128 + -1    /* 5 */,
    128 + -1    /* 6 */,
    128 + 0     /* 7 */,
    128 + 0     /* 8 */,
    128 + 0     /* 9 */,
    128 + 0     /* 10 */,
    128 + 0     /* 11 */,
    128 + 0     /* 12 */,
    128 + 0     /* 13 */,
    128 + 0     /* 14 */,
    128 + 0     /* 15 */,
    128 + 1     /* 16 */,
    128 + 2     /* 17 */,
    128 + 2     /* 18/00-18/09 */,
    128 + 2     /* 18/10-18/19 */,
    128 + 3     /* 18/20-18/29 */,
    128 + 3     /* 18/30-18/39 */,
    128 + 3     /* 18/40-18/49 */,
    128 + 3     /* 18/50-18/59 */,
    128 + 3     /* 18/60-18/69 */,
    128 + 4     /* 18/70-18/79 */,
    128 + 5     /* 18/80-18/89 */,
    128 + 5     /* 18/90-18/99 */,
    128 + 6     /* 18/100-18/109 */,
    128 + 7     /* 18/110-18/119 */,
    128 + 8     /* 18/120-18/129 */,
    128 + 9     /* 18/130-18/139 */,
    128 + 10    /* 18/140-18/149 */,
    128 + 11    /* 18/150-18/159 */,
    128 + 12    /* 18/160-18/169 */,
    128 + 13    /* 18/170-18/179 */,
    128 + 14    /* 18/180-18/189 */,
    128 + 15    /* 18/190-18/199 */,
    128 + 16    /* 18/200-18/209 */,
    128 + 18    /* 18/210-18/219 */,
    128 + 20    /* 18/220+ */
};


/*
 * Stat Table (DEX) -- bonus to hit (plus 128)
 */
byte adj_dex_th[] =
{
    128 + -3    /* 3 */,
    128 + -2    /* 4 */,
    128 + -2    /* 5 */,
    128 + -1    /* 6 */,
    128 + -1    /* 7 */,
    128 + 0     /* 8 */,
    128 + 0     /* 9 */,
    128 + 0     /* 10 */,
    128 + 0     /* 11 */,
    128 + 0     /* 12 */,
    128 + 0     /* 13 */,
    128 + 0     /* 14 */,
    128 + 0     /* 15 */,
    128 + 1     /* 16 */,
    128 + 2     /* 17 */,
    128 + 3     /* 18/00-18/09 */,
    128 + 3     /* 18/10-18/19 */,
    128 + 3     /* 18/20-18/29 */,
    128 + 3     /* 18/30-18/39 */,
    128 + 3     /* 18/40-18/49 */,
    128 + 4     /* 18/50-18/59 */,
    128 + 4     /* 18/60-18/69 */,
    128 + 4     /* 18/70-18/79 */,
    128 + 4     /* 18/80-18/89 */,
    128 + 5     /* 18/90-18/99 */,
    128 + 6     /* 18/100-18/109 */,
    128 + 7     /* 18/110-18/119 */,
    128 + 8     /* 18/120-18/129 */,
    128 + 9     /* 18/130-18/139 */,
    128 + 9     /* 18/140-18/149 */,
    128 + 10    /* 18/150-18/159 */,
    128 + 11    /* 18/160-18/169 */,
    128 + 12    /* 18/170-18/179 */,
    128 + 13    /* 18/180-18/189 */,
    128 + 14    /* 18/190-18/199 */,
    128 + 15    /* 18/200-18/209 */,
    128 + 15    /* 18/210-18/219 */,
    128 + 15    /* 18/220+ */
};


/*
 * Stat Table (STR) -- bonus to hit (plus 128)
 */
byte adj_str_th[] =
{
    128 + -3    /* 3 */,
    128 + -2    /* 4 */,
    128 + -1    /* 5 */,
    128 + -1    /* 6 */,
    128 + 0     /* 7 */,
    128 + 0     /* 8 */,
    128 + 0     /* 9 */,
    128 + 0     /* 10 */,
    128 + 0     /* 11 */,
    128 + 0     /* 12 */,
    128 + 0     /* 13 */,
    128 + 0     /* 14 */,
    128 + 0     /* 15 */,
    128 + 0     /* 16 */,
    128 + 0     /* 17 */,
    128 + 1     /* 18/00-18/09 */,
    128 + 1     /* 18/10-18/19 */,
    128 + 1     /* 18/20-18/29 */,
    128 + 1     /* 18/30-18/39 */,
    128 + 1     /* 18/40-18/49 */,
    128 + 1     /* 18/50-18/59 */,
    128 + 1     /* 18/60-18/69 */,
    128 + 2     /* 18/70-18/79 */,
    128 + 3     /* 18/80-18/89 */,
    128 + 4     /* 18/90-18/99 */,
    128 + 5     /* 18/100-18/109 */,
    128 + 6     /* 18/110-18/119 */,
    128 + 7     /* 18/120-18/129 */,
    128 + 8     /* 18/130-18/139 */,
    128 + 9     /* 18/140-18/149 */,
    128 + 10    /* 18/150-18/159 */,
    128 + 11    /* 18/160-18/169 */,
    128 + 12    /* 18/170-18/179 */,
    128 + 13    /* 18/180-18/189 */,
    128 + 14    /* 18/190-18/199 */,
    128 + 15    /* 18/200-18/209 */,
    128 + 15    /* 18/210-18/219 */,
    128 + 15    /* 18/220+ */
};


/*
 * Stat Table (STR) -- weight limit in deca-pounds
 */
byte adj_str_wgt[] =
{
     5  /* 3 */,
     6  /* 4 */,
     7  /* 5 */,
     8  /* 6 */,
     9  /* 7 */,
    10  /* 8 */,
    11  /* 9 */,
    12  /* 10 */,
    13  /* 11 */,
    14  /* 12 */,
    15  /* 13 */,
    16  /* 14 */,
    17  /* 15 */,
    18  /* 16 */,
    19  /* 17 */,
    20  /* 18/00-18/09 */,
    22  /* 18/10-18/19 */,
    24  /* 18/20-18/29 */,
    26  /* 18/30-18/39 */,
    28  /* 18/40-18/49 */,
    30  /* 18/50-18/59 */,
    30  /* 18/60-18/69 */,
    30  /* 18/70-18/79 */,
    30  /* 18/80-18/89 */,
    30  /* 18/90-18/99 */,
    30  /* 18/100-18/109 */,
    30  /* 18/110-18/119 */,
    30  /* 18/120-18/129 */,
    30  /* 18/130-18/139 */,
    30  /* 18/140-18/149 */,
    30  /* 18/150-18/159 */,
    30  /* 18/160-18/169 */,
    30  /* 18/170-18/179 */,
    30  /* 18/180-18/189 */,
    30  /* 18/190-18/199 */,
    30  /* 18/200-18/209 */,
    30  /* 18/210-18/219 */,
    30  /* 18/220+ */
};


/*
 * Stat Table (STR) -- weapon weight limit in pounds
 */
byte adj_str_hold[] =
{
     4  /* 3 */,
     5  /* 4 */,
     6  /* 5 */,
     7  /* 6 */,
     8  /* 7 */,
    10  /* 8 */,
    12  /* 9 */,
    14  /* 10 */,
    16  /* 11 */,
    18  /* 12 */,
    20  /* 13 */,
    22  /* 14 */,
    24  /* 15 */,
    26  /* 16 */,
    28  /* 17 */,
    30  /* 18/00-18/09 */,
    30  /* 18/10-18/19 */,
    35  /* 18/20-18/29 */,
    40  /* 18/30-18/39 */,
    45  /* 18/40-18/49 */,
    50  /* 18/50-18/59 */,
    55  /* 18/60-18/69 */,
    60  /* 18/70-18/79 */,
    65  /* 18/80-18/89 */,
    70  /* 18/90-18/99 */,
    80  /* 18/100-18/109 */,
    80  /* 18/110-18/119 */,
    80  /* 18/120-18/129 */,
    80  /* 18/130-18/139 */,
    80  /* 18/140-18/149 */,
    90  /* 18/150-18/159 */,
    90  /* 18/160-18/169 */,
    90  /* 18/170-18/179 */,
    90  /* 18/180-18/189 */,
    90  /* 18/190-18/199 */,
   100  /* 18/200-18/209 */,
   100  /* 18/210-18/219 */,
   100  /* 18/220+ */
};


/*
 * Stat Table (STR) -- digging value
 */
byte adj_str_dig[] =
{
      0 /* 3 */,
      0 /* 4 */,
      1 /* 5 */,
      2 /* 6 */,
      3 /* 7 */,
      4 /* 8 */,
      4 /* 9 */,
      5 /* 10 */,
      5 /* 11 */,
      6 /* 12 */,
      6 /* 13 */,
      7 /* 14 */,
      7 /* 15 */,
      8 /* 16 */,
      8 /* 17 */,
      9 /* 18/00-18/09 */,
     10 /* 18/10-18/19 */,
     12 /* 18/20-18/29 */,
     15 /* 18/30-18/39 */,
     20 /* 18/40-18/49 */,
     25 /* 18/50-18/59 */,
     30 /* 18/60-18/69 */,
     35 /* 18/70-18/79 */,
     40 /* 18/80-18/89 */,
     45 /* 18/90-18/99 */,
     50 /* 18/100-18/109 */,
     55 /* 18/110-18/119 */,
     60 /* 18/120-18/129 */,
     65 /* 18/130-18/139 */,
     70 /* 18/140-18/149 */,
     75 /* 18/150-18/159 */,
     80 /* 18/160-18/169 */,
     85 /* 18/170-18/179 */,
     90 /* 18/180-18/189 */,
     95 /* 18/190-18/199 */,
    100 /* 18/200-18/209 */,
    100 /* 18/210-18/219 */,
    100 /* 18/220+ */
};


/*
 * Stat Table (STR) -- help index into the "blow" table
 */
byte adj_str_blow[] =
{
      3 /* 3 */,
      4 /* 4 */,
      5 /* 5 */,
      6 /* 6 */,
      7 /* 7 */,
      8 /* 8 */,
      9 /* 9 */,
     10 /* 10 */,
     11 /* 11 */,
     12 /* 12 */,
     13 /* 13 */,
     14 /* 14 */,
     15 /* 15 */,
     16 /* 16 */,
     17 /* 17 */,
     20 /* 18/00-18/09 */,
     30 /* 18/10-18/19 */,
     40 /* 18/20-18/29 */,
     50 /* 18/30-18/39 */,
     60 /* 18/40-18/49 */,
     70 /* 18/50-18/59 */,
     80 /* 18/60-18/69 */,
     90 /* 18/70-18/79 */,
    100 /* 18/80-18/89 */,
    110 /* 18/90-18/99 */,
    120 /* 18/100-18/109 */,
    130 /* 18/110-18/119 */,
    140 /* 18/120-18/129 */,
    150 /* 18/130-18/139 */,
    160 /* 18/140-18/149 */,
    170 /* 18/150-18/159 */,
    180 /* 18/160-18/169 */,
    190 /* 18/170-18/179 */,
    200 /* 18/180-18/189 */,
    210 /* 18/190-18/199 */,
    220 /* 18/200-18/209 */,
    230 /* 18/210-18/219 */,
    240 /* 18/220+ */
};


/*
 * Stat Table (DEX) -- index into the "blow" table
 */
byte adj_dex_blow[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    0   /* 8 */,
    0   /* 9 */,
    1   /* 10 */,
    1   /* 11 */,
    1   /* 12 */,
    1   /* 13 */,
    1   /* 14 */,
    1   /* 15 */,
    1   /* 16 */,
    1   /* 17 */,
    1   /* 18/00-18/09 */,
    2   /* 18/10-18/19 */,
    2   /* 18/20-18/29 */,
    2   /* 18/30-18/39 */,
    2   /* 18/40-18/49 */,
    3   /* 18/50-18/59 */,
    3   /* 18/60-18/69 */,
    4   /* 18/70-18/79 */,
    4   /* 18/80-18/89 */,
    5   /* 18/90-18/99 */,
    6   /* 18/100-18/109 */,
    7   /* 18/110-18/119 */,
    8   /* 18/120-18/129 */,
    9   /* 18/130-18/139 */,
    10  /* 18/140-18/149 */,
    11  /* 18/150-18/159 */,
    12  /* 18/160-18/169 */,
    14  /* 18/170-18/179 */,
    16  /* 18/180-18/189 */,
    18  /* 18/190-18/199 */,
    20  /* 18/200-18/209 */,
    20  /* 18/210-18/219 */,
    20  /* 18/220+ */
};


/*
 * Stat Table (DEX) -- chance of avoiding "theft" and "falling"
 */
byte adj_dex_safe[] =
{
    0   /* 3 */,
    1   /* 4 */,
    2   /* 5 */,
    3   /* 6 */,
    4   /* 7 */,
    5   /* 8 */,
    5   /* 9 */,
    6   /* 10 */,
    6   /* 11 */,
    7   /* 12 */,
    7   /* 13 */,
    8   /* 14 */,
    8   /* 15 */,
    9   /* 16 */,
    9   /* 17 */,
    10  /* 18/00-18/09 */,
    10  /* 18/10-18/19 */,
    15  /* 18/20-18/29 */,
    15  /* 18/30-18/39 */,
    20  /* 18/40-18/49 */,
    25  /* 18/50-18/59 */,
    30  /* 18/60-18/69 */,
    35  /* 18/70-18/79 */,
    40  /* 18/80-18/89 */,
    45  /* 18/90-18/99 */,
    50  /* 18/100-18/109 */,
    60  /* 18/110-18/119 */,
    70  /* 18/120-18/129 */,
    80  /* 18/130-18/139 */,
    90  /* 18/140-18/149 */,
    100 /* 18/150-18/159 */,
    100 /* 18/160-18/169 */,
    100 /* 18/170-18/179 */,
    100 /* 18/180-18/189 */,
    100 /* 18/190-18/199 */,
    100 /* 18/200-18/209 */,
    100 /* 18/210-18/219 */,
    100 /* 18/220+ */
};


/*
 * Stat Table (CON) -- base regeneration rate
 */
byte adj_con_fix[] =
{
    0   /* 3 */,
    0   /* 4 */,
    0   /* 5 */,
    0   /* 6 */,
    0   /* 7 */,
    0   /* 8 */,
    0   /* 9 */,
    0   /* 10 */,
    0   /* 11 */,
    0   /* 12 */,
    0   /* 13 */,
    1   /* 14 */,
    1   /* 15 */,
    1   /* 16 */,
    1   /* 17 */,
    2   /* 18/00-18/09 */,
    2   /* 18/10-18/19 */,
    2   /* 18/20-18/29 */,
    2   /* 18/30-18/39 */,
    2   /* 18/40-18/49 */,
    3   /* 18/50-18/59 */,
    3   /* 18/60-18/69 */,
    3   /* 18/70-18/79 */,
    3   /* 18/80-18/89 */,
    3   /* 18/90-18/99 */,
    4   /* 18/100-18/109 */,
    4   /* 18/110-18/119 */,
    5   /* 18/120-18/129 */,
    6   /* 18/130-18/139 */,
    6   /* 18/140-18/149 */,
    7   /* 18/150-18/159 */,
    7   /* 18/160-18/169 */,
    8   /* 18/170-18/179 */,
    8   /* 18/180-18/189 */,
    8   /* 18/190-18/199 */,
    9   /* 18/200-18/209 */,
    9   /* 18/210-18/219 */,
    9   /* 18/220+ */
};


/*
 * Stat Table (CON) -- extra half-hitpoints per level (plus 128)
 */
byte adj_con_mhp[] =
{
    128 + -5    /* 3 */,
    128 + -3    /* 4 */,
    128 + -2    /* 5 */,
    128 + -1    /* 6 */,
    128 + 0     /* 7 */,
    128 + 0     /* 8 */,
    128 + 0     /* 9 */,
    128 + 0     /* 10 */,
    128 + 0     /* 11 */,
    128 + 0     /* 12 */,
    128 + 0     /* 13 */,
    128 + 0     /* 14 */,
    128 + 1     /* 15 */,
    128 + 1     /* 16 */,
    128 + 2     /* 17 */,
    128 + 3     /* 18/00-18/09 */,
    128 + 4     /* 18/10-18/19 */,
    128 + 4     /* 18/20-18/29 */,
    128 + 4     /* 18/30-18/39 */,
    128 + 4     /* 18/40-18/49 */,
    128 + 5     /* 18/50-18/59 */,
    128 + 6     /* 18/60-18/69 */,
    128 + 7     /* 18/70-18/79 */,
    128 + 8     /* 18/80-18/89 */,
    128 + 9     /* 18/90-18/99 */,
    128 + 10    /* 18/100-18/109 */,
    128 + 11    /* 18/110-18/119 */,
    128 + 12    /* 18/120-18/129 */,
    128 + 13    /* 18/130-18/139 */,
    128 + 14    /* 18/140-18/149 */,
    128 + 15    /* 18/150-18/159 */,
    128 + 16    /* 18/160-18/169 */,
    128 + 18    /* 18/170-18/179 */,
    128 + 20    /* 18/180-18/189 */,
    128 + 22    /* 18/190-18/199 */,
    128 + 25    /* 18/200-18/209 */,
    128 + 25    /* 18/210-18/219 */,
    128 + 25    /* 18/220+ */
};


/*
 * This table is used to help calculate the number of blows the player can
 * make in a single round of attacks (one player turn) with a normal weapon.
 *
 * This number ranges from a single blow/round for weak players to up to six
 * blows/round for powerful warriors.
 *
 * Note that certain artifacts and ego-items give "bonus" blows/round.
 *
 * First, from the player class info table, we extract some values.
 *
 * To get "P", we look up the relevant "adj_str_blow[]" (see above),
 * multiply it by "mul", and then divide it by "div", rounding down.
 *
 * To get "D", we look up the relevant "adj_dex_blow[]" (see above),
 * note especially column 6 (DEX 18/101) and 11 (DEX 18/150).
 *
 * The player gets "blows_table[P][D]" blows/round, as shown below,
 * up to a maximum of "num" blows/round, plus any "bonus" blows/round.
 */
byte blows_table[12][12] =
{

/* P/D         0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11+ */

/* 0  */    {  1,   1,   1,   1,   1,   1,   2,   2,   2,   2,   2,   3 },
/* jk - the last 5, at [1,11] was a 4, but now a priest with 18*** str & dex */
/* can wield a 40.0 pound weapon for 5 blows a round (mace of disruption) */
/* 1  */    {  1,   1,   1,   1,   2,   2,   3,   3,   3,   4,   4,   5 },
/* 2  */    {  1,   1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5 },
/* 3  */    {  1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5,   5 },
/* 4  */    {  1,   2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5 },
/* 5  */    {  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },
/* 6  */    {  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },
/* 7  */    {  2,   3,   3,   4,   4,   4,   5,   5,   5,   5,   5,   6 },
/* 8  */    {  3,   3,   3,   4,   4,   4,   5,   5,   5,   5,   6,   6 },
/* 9  */    {  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   6,   6 },
/* 10 */    {  3,   3,   4,   4,   4,   4,   5,   5,   5,   6,   6,   6 },
/* 11+ */   {  3,   3,   4,   4,   4,   4,   5,   5,   6,   6,   6,   6 },

};

/* jk - what objects are allowable in what store */
/* pairs of tval, sval, chance. sval == -1 means all svals acceptable */
/*                              sval == -2 only of priestly flavor    */
/*                              sval == -3 only of mage flavor        */
/*                              sval == -4 only Holy Avenger/Blessed  */
/* chance: chance of x in 100 that such an item must be present       */
store_table_type store_accepts[MAX_STORES][STORE_CHOICES] =
{
   {
      /* store 0 - general store */
      { TV_FOOD, SV_FOOD_RATION,         90 },
      { TV_FOOD, SV_FOOD_BISCUIT,         0 },
      { TV_FOOD, SV_FOOD_JERKY,           0 },
      { TV_FOOD, SV_FOOD_PINT_OF_WINE,    0 },
      { TV_FOOD, SV_FOOD_PINT_OF_ALE,     0 },
      { TV_FOOD, SV_FOOD_WAYBREAD,        0 },
      { TV_DIGGING, -1,                  10 },
      { TV_CLOAK, -1,                    40 },
      { TV_POTION, SV_POTION_WATER,       0 },
      { TV_POTION, SV_POTION_APPLE_JUICE, 0 },
      { TV_SPIKE, -1,                     0 },
      { TV_FLASK, 0,                     90 },
      { TV_LITE, -1,                     90 },
      { 0, 0, 0 }
   },

   {
      /* 1 Armoury */
      { TV_BOOTS, -1,                     0 },
      { TV_HELM, -1,                      0 },
      { TV_CROWN, -1,                     0 },
      { TV_SOFT_ARMOR, -1,                0 },
      { TV_HARD_ARMOR, -1,                0 },
      { TV_DRAG_ARMOR, -1,                0 },
      { TV_GLOVES, -1,                    0 },
      { TV_SHIELD, -1,                    0 },
      { 0, 0, 0 }
   },

   {
      /* 2 Weaponsmith */
      { TV_SWORD, -1,                     0 },
      { TV_POLEARM, -1,                   0 },
      { TV_HAFTED, -1,                    0 },
      { TV_BOW, -1,                       0 },
      { TV_SHOT, -1,                      0 },
      { TV_ARROW, -1,                    50 },
      { TV_BOLT, -1,                     50 },
      { 0, 0, 0 }
   },

   {
      /* 3 Temple */
      { TV_HAFTED, -1,                    0 },
      { TV_SWORD, -4,                     0 },
      { TV_POLEARM, -4,                   0 },
      { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL,     50 },
      { TV_SCROLL, -1,                    0 },
      { TV_POTION, -1,                    0 },
      { TV_SPELL, -2,                     0 },
      { TV_BOOK, -1,                    100 },
      { 0, 0, 0 }
   },

   {
      /* 4 Alchemy shop */
      { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL,     50 },
      { TV_SCROLL, -1,                    0 },
      { TV_POTION, -1,                    0 },
      { 0, 0, 0 }
   },

   {
      /* 5 Magic-User store */
      { TV_RING,-1,                       0 },
      { TV_AMULET, -1,                    0 },
      { TV_WAND, -1,                      0 },
      { TV_STAFF, -1,                     0 },
      { TV_SPELL, -3,                     0 },
      { TV_BOOK, -1,                     50 },
      { TV_ROD, -1,                      50 },
      { 0, 0, 0 }
   },

   {
      /* 6 Black Market */
      { -1, -1, 0 },
      { 0, 0, 0 }
   },

   {
      /* 7 Home */
      { -1, -1, 0 },
      { 0, 0, 0 }
   },

   {
      /* 8 unused */
      { -1, -1, 0 },
      { 0, 0, 0 }
   },

   {
      /* 9 unused */
      { -1, -1, 0 },
      { 0, 0, 0 }
   },

   {
      /* 10 General Digger Store */
      { TV_DIGGING, -1,                   0 },
      { 0, 0 }
   },

   {
      /* 11 General Light Store */
      { TV_FLASK, -1,                     0 },
      { TV_LITE, -1,                      0 },
      { 0, 0 }
   },

   {
      /* 12 General Food Store */
      { TV_FOOD, -1,                      0 },
      { 0, 0 }
   },

   {
      /* 13 General Cloaks Store */
      { TV_CLOAK, -1,                     0 },
      { 0, 0 }
   },

   {
      /* 14 Armoury Boots Store */
      { TV_BOOTS, -1,                     0 },
      { 0, 0 }
   },

   {
      /* 15 Armoury Gloves & Shields Store */
      { TV_GLOVES, -1,                    0 },
      { TV_SHIELD, -1,                    0 },
      { 0, 0 }
   },

   {
      /* 16 Armoury Crowns and Helmets Store */
      { TV_HELM, -1,                      0 },
      { TV_CROWN, -1,                     0 },
      { 0, 0 }
   },

   {
      /* 17 Armoury Soft Armour Store */
      { TV_SOFT_ARMOR, -1,                0 },
      { TV_CLOAK, -1,                     0 },
      { 0, 0 }
   },

   {
      /* 18 Armoury Hard Armour Store */
      { TV_HARD_ARMOR, -1,                0 },
      { TV_DRAG_ARMOR, -1,                0 },
      { 0, 0 }
   },

   {
      /* 19 Weapon Shot Store */
      { TV_ARROW, -1,                     0 },
      { TV_BOLT, -1,                      0 },
      { TV_SHOT, -1,                      0 },
      { TV_BOW, -1,                       0 },
      { 0, 0 }
   },

   {
      /* 20 Weapon Hafted Store */
      { TV_HAFTED, -1,                    0 },
      { 0, 0 }
   },

   {
      /* 21 Weapon Sword Store */
      { TV_SWORD, -1,                     0 },
      { 0, 0 }
   },

   {
      /* 22 Weapon Polearm Store */
      { TV_POLEARM, -1,                   0 },
      { 0, 0 }
   },

   {
      /* 23 Temple Spell Store */
      { TV_SPELL, -2,                     0 },
      { TV_BOOK, -1,                      0 },
      { 0, 0 }
   },

   {
      /* 24 Temple Hafted Store */
      { TV_HAFTED, -1,                    0 },
      { TV_SWORD, -4,                     0 },
      { TV_POLEARM, -4,                   0 },
      { 0, 0 }
   },

   {
      /* 25 Alchemist Scroll Store */
      { TV_SCROLL, -1,                    0 },
      { 0, 0 }
   },

   {
      /* 26 Alchemist Potion Store */
      { TV_POTION, -1,                    0 },
      { 0, 0, 0 }
   },

   {
      /* 27 Magic Spell Store */
      { TV_SPELL, -3,                     0 },
      { TV_BOOK, -1,                      0 },
      { 0, 0 }
   },

   {
      /* 28 Magic Ring Store */
      { TV_RING, -1,                      0 },
      { TV_AMULET, -1,                    0 },
      { 0, 0 }
   },

   {
      /* 29 Magic Wand Store */
      { TV_WAND, -1,                      0 },
      { TV_STAFF, -1,                     0 },
      { TV_ROD, -1,                       0 },
      { 0, 0 }
   }
};

/*
 * Store owners (exactly four "possible" owners per store, chosen randomly)
 * { name, purse, max greed, min greed, haggle_per, tolerance, race, unused }
 */
owner_type owners[MAX_STORES][MAX_OWNERS] =
{
  {
    /* General store */
    { "Bilbo the Friendly",     20000L,    170,  108,   5,  15,  RACE_HOBBIT},
    { "Rincewind the Chicken",  20000L,    175,  108,   4,  12,  RACE_HUMAN},
    { "Sultan the Midget",      30000L,    170,  107,   5,  15,  RACE_GNOME},
    { "Lyar-el the Comely",     30000L,    165,  107,   6,  18,  RACE_ELF},
    { "Wladislaw the Patient",  25000L,    170,  106,   9,  25,  RACE_DWARF},
    { "Balimba the Third",      20000L,    170,  108,   5,  15,  RACE_HOBBIT},
    { "Carlos the Chopper",     20000L,    175,  108,   4,  12,  RACE_HUMAN},
    { "Ricardo the Rich",       40000L,    170,  107,   5,  15,  RACE_GNOME},
    { "Magnorx the mettlesome", 30000L,    165,  107,   4,  12,  RACE_ELF},
    { "Wladislaw the Patient",  25000L,    170,  106,   9,  25,  RACE_DWARF},
    { "Carreras the Good",      20000L,    170,  108,   5,  15,  RACE_HUMAN},
    { "Hi-Hat the Second",      20000L,    175,  108,   4,  12,  RACE_HOBBIT},
    { "Melania the Maiden",     30000L,    170,  107,   5,  15,  RACE_ELF},
    { "Gabba the Greedy",       30000L,    165,  107,   6,  18,  RACE_HALF_ORC},
    { "Morten the Moody",       25000L,    170,  106,   4,  15,  RACE_DWARF},
    { "Savannah the Savage",    30000L,    170,  108,   5,  12,  RACE_HALF_TROLL},
    { "Bad Barney the Beast",   20000L,    175,  108,   4,  12,  RACE_HUMAN},
    { "Mya the Magnificent",    30000L,    170,  107,   5,  15,  RACE_GNOME},
    { "Stephanopoulos",         30000L,    165,  107,   6,  18,  RACE_ELF},
    { "Fred the Faster",        25000L,    170,  106,   9,  15,  RACE_DWARF},
  },
  {
    /* Armoury */
    { "Kon-Dar the Ugly",        50000L,  210,  115,   5,   7,  RACE_HALF_ORC},
    { "Darg-Low the Grim",       70000L,  190,  111,   4,   9,  RACE_HUMAN},
    { "Decado the Handsome",     90000L,  200,  112,   4,  10,  RACE_DUNADAN},
    { "Mauglin the Grumpy",      90000L,  200,  112,   4,   5,  RACE_DWARF},
    { "Frederick the Fast",      50000L,  210,  115,   5,   7,  RACE_HALF_TROLL},
    { "One-Hand the Fearsome",   70000L,  190,  111,   4,   9,  RACE_HUMAN},
    { "One-Eye the Old",         80000L,  200,  112,   4,  10,  RACE_DUNADAN},
    { "Tall Tom the Thumper",    90000L,  200,  112,   4,   5,  RACE_DWARF},
    { "Small Tom the Trasher",   50000L,  210,  115,   5,   7,  RACE_DWARF},
    { "Grimaldi the Gazer",      50000L,  190,  111,   4,   9,  RACE_HUMAN},
    { "Sam Short",               70000L,  200,  112,   4,  10,  RACE_DUNADAN},
    { "Heraclitis the First",    80000L,  200,  112,   4,   5,  RACE_DWARF},
    { "Xena from Xanthos",       50000L,  210,  115,   5,   7,  RACE_HALF_ORC},
    { "Walt the Waist-Cleaver",  80000L,  190,  111,   4,   9,  RACE_HUMAN},
    { "Herman Hard-Helmet",      50000L,  200,  112,   4,  10,  RACE_DUNADAN},
    { "Mahalia the Abusive",     70000L,  200,  112,   4,   5,  RACE_DWARF},
    { "Karl No-Beard",           50000L,  210,  115,   5,   7,  RACE_HALF_ORC},
    { "Kahondra the Killer",     80000L,  190,  111,   4,   9,  RACE_HUMAN},
    { "Xomaka the Third",        80000L,  200,  112,   4,  10,  RACE_DUNADAN},
    { "Annaca the Ancient",      90000L,  200,  112,   4,   5,  RACE_DWARF},
  },
  {
    /* Weapon Smith */
    { "Ithyl-Mak the Beastly",   40000L,  210,  115,   6,   6,  RACE_HALF_TROLL},
    { "Arndal Beast-Slayer",     60000L,  185,  110,   5,   9,  RACE_HALF_ELF},
    { "Tarl Beast-Master",       90000L,  190,  115,   5,   7,  RACE_HOBBIT},
    { "Oglign Dragon-Slayer",    40000L,  195,  112,   4,   8,  RACE_DWARF},
    { "Pernelle the Poisoner",   60000L,  210,  115,   6,   6,  RACE_HALF_ELF},
    { "Drax Dragonmaster",       90000L,  185,  110,   5,   9,  RACE_HALF_TROLL},
    { "Tarl Troll-Hurter",       40000L,  190,  115,   5,   7,  RACE_HOBBIT},
    { "Otha Ogre-Fiend",         60000L,  195,  112,   4,   8,  RACE_DWARF},
    { "Majal Mumak-Seeker",      90000L,  210,  115,   6,   6,  RACE_HALF_TROLL},
    { "Ehus Undead-Finder",      40000L,  185,  110,   5,   9,  RACE_HALF_ELF},
    { "Orly the Obnoxious",      60000L,  190,  115,   5,   7,  RACE_HOBBIT},
    { "Appleby the Lazy",        90000L,  195,  112,   4,   8,  RACE_HOBBIT},
    { "Andri the Hound-Crusher", 40000L,  210,  115,   6,   6,  RACE_ELF},
    { "Large Leo Lamehand",      60000L,  185,  110,   5,   9,  RACE_HALF_ORC},
    { "Zyhpoi Neverfail",        90000L,  190,  115,   5,   7,  RACE_HOBBIT},
    { "Erljan the Unstoppable",  40000L,  195,  112,   4,   8,  RACE_HUMAN},
    { "Hanson the Hard",         60000L,  210,  115,   6,   6,  RACE_HALF_ORC},
    { "Jarl the Hunter",         90000L,  185,  110,   5,   9,  RACE_HALF_ELF},
    { "Wump the Ancient",        40000L,  190,  115,   5,   7,  RACE_DUNADAN},
    { "Pallox the Protector",    60000L,  195,  112,   4,   8,  RACE_DWARF},
  },
  {
    /* Temple */
    { "Ludwig the Humble",       50000L,  175,  109,   6,  15,  RACE_HUMAN},
    { "Gunnar the Paladin",      60000L,  185,  110,   5,  23,  RACE_HUMAN},
    { "Delilah the Pure",        70000L,  180,  107,   6,  20,  RACE_ELF},
    { "Bosk the Wise",           50000L,  185,  109,   5,  15,  RACE_DWARF},
    { "Wilvert the Wannabee",    60000L,  175,  109,   6,  15,  RACE_HUMAN},
    { "Gobba the Godly",         70000L,  185,  110,   5,  23,  RACE_HUMAN},
    { "Simon the Silent",        50000L,  180,  107,   6,  20,  RACE_ELF},
    { "Thom the Thin",           60000L,  185,  109,   5,  15,  RACE_HUMAN},
    { "Borzhov Long-Beard",      70000L,  175,  109,   6,  15,  RACE_DWARF},
    { "Kamarsky the Cleric",     50000L,  185,  110,   5,  23,  RACE_HUMAN},
    { "Andoplho the Amateur",    60000L,  180,  107,   6,  20,  RACE_ELF},
    { "Ben Big-Belly",           70000L,  185,  109,   5,  15,  RACE_DWARF},
    { "Morten the Mage-Bane",    50000L,  175,  109,   6,  15,  RACE_HUMAN},
    { "Andrusz the Avid",        60000L,  185,  110,   5,  23,  RACE_HUMAN},
    { "Urlik the Praying",       70000L,  180,  107,   6,  20,  RACE_ELF},
    { "Holy Helen the Steady",   50000L,  185,  109,   5,  15,  RACE_DWARF},
    { "Amadeui the Aimable",     60000L,  175,  109,   6,  15,  RACE_HUMAN},
    { "Murat the Morose",        70000L,  185,  110,   5,  23,  RACE_HUMAN},
    { "Jurjua the Clever",       50000L,  180,  107,   6,  20,  RACE_ELF},
    { "Aihjal the Adept",        60000L,  185,  109,   5,  15,  RACE_DWARF},
  },

  /* Alchemy Shop */
  {
    { "Mauser the Chemist",      50000L,  190,  111,   5,   8,  RACE_HALF_ELF},
    { "Wizzle the Chaotic",      60000L,  190,  110,   6,   8,  RACE_HOBBIT},
    { "Ga-nat the Greedy",       70000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Sasha the Slender",       50000L,  220,  111,   4,   9,  RACE_ELF},
    { "Sartix the Smart",        60000L,  190,  111,   5,   8,  RACE_HALF_ELF},
    { "Abraham the Awoken",      70000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Piuz the Sleepy",         50000L,  220,  111,   4,   9,  RACE_ELF},
    { "Catharine the Small",     60000L,  190,  111,   5,   8,  RACE_HALF_ELF},
    { "Laurie the Librarian",    70000L,  190,  110,   6,   8,  RACE_HOBBIT},
    { "Hulyo with the Glasses",  50000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Pol the Powerfull",       60000L,  220,  111,   4,   9,  RACE_ELF},
    { "Tommy the Trickster",     70000L,  190,  111,   5,   8,  RACE_HALF_ELF},
    { "Regilio the Randomizer",  50000L,  190,  110,   6,   8,  RACE_HOBBIT},
    { "Halef the Helpless",      60000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Maximilian the Mixer",    70000L,  220,  111,   4,   9,  RACE_ELF},
    { "Adrim Wooden-Leg",        50000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Boris the Smelly",        60000L,  190,  111,   5,   8,  RACE_HALF_ELF},
    { "Wassily the Watery",      70000L,  190,  110,   6,   8,  RACE_HOBBIT},
    { "Flopux the Fast",         50000L,  200,  116,   6,   9,  RACE_GNOME},
    { "Balthazar the Brewer",    60000L,  220,  111,   4,   9,  RACE_ELF},
  },
  {
    /* Magic Shop */
    { "Ariel the Sorceress",     30000L,  200,  110,   7,   8,  RACE_HALF_ELF},
    { "Buggerby the Great",      40000L,  215,  113,   6,  10,  RACE_GNOME},
    { "Inglorian the Mage",      50000L,  200,  110,   7,  10,  RACE_HUMAN},
    { "Luthien Starshine",       30000L,  175,  110,   5,  11,  RACE_HIGH_ELF},
    { "Fyorx Fumble-Fingers",    40000L,  200,  110,   7,   8,  RACE_HALF_ELF},
    { "Wildramus the Wizard",    50000L,  215,  113,   6,  10,  RACE_GNOME},
    { "Drax Elf-Like",           30000L,  200,  110,   7,  10,  RACE_DWARF},
    { "Sheamus the Westerner",   40000L,  175,  110,   5,  11,  RACE_DUNADAN},
    { "Mercurion the Mercyful",  50000L,  200,  110,   7,   8,  RACE_HALF_ELF},
    { "Gaston Big-Nose",         30000L,  215,  113,   6,  10,  RACE_GNOME},
    { "Fratsox Flabbergasted",   40000L,  200,  110,   7,  10,  RACE_HUMAN},
    { "Mohammus the Mighty",     50000L,  175,  110,   5,  11,  RACE_HIGH_ELF},
    { "Syri Sparkling-Fingers",  30000L,  200,  110,   7,   8,  RACE_HALF_ELF},
    { "Hergi Three-Eye",         40000L,  215,  113,   6,  10,  RACE_GNOME},
    { "Ceffeux the Celt",        50000L,  200,  110,   7,  10,  RACE_HUMAN},
    { "Theodorix the Theorist",  30000L,  175,  110,   5,  11,  RACE_HIGH_ELF},
    { "Anastasia the Animator",  40000L,  200,  110,   7,   8,  RACE_HALF_ELF},
    { "Klaze Klaze-Son",         50000L,  215,  113,   6,  10,  RACE_GNOME},
    { "Hulya Know-It-All",       30000L,  200,  110,   7,  10,  RACE_HUMAN},
    { "Dimbleby Disdain",        40000L,  175,  110,   5,  11,  RACE_HIGH_ELF},
  },
  {
    /* Black Market */
    { "Lo-Hak the Awful",       100000L,  250,  150,  10,   5,  RACE_HALF_TROLL},
    { "Histor the Goblin",      120000L,  250,  150,  10,   5,  RACE_HALF_ORC},
    { "Durwin the Shifty",      140000L,  250,  150,  10,   5,  RACE_HUMAN},
    { "Drago the Fair",         160000L,  250,  150,  10,   5,  RACE_ELF},
    { "Harry the Handler",      100000L,  250,  150,  10,   5,  RACE_HALF_TROLL},
    { "Bernard Buy-All",        120000L,  250,  150,  10,   5,  RACE_HALF_ORC},
    { "Marnix the Mean",        140000L,  250,  150,  10,   5,  RACE_HUMAN},
    { "Leo Lazy-Bones",         160000L,  250,  150,  10,   5,  RACE_ELF},
    { "Pernelle the Persistent",100000L,  250,  150,  10,   5,  RACE_HALF_TROLL},
    { "Edmund the Sheep",       120000L,  250,  150,  10,   5,  RACE_HALF_ORC},
    { "Lupus the Wayfarer",     140000L,  250,  150,  10,   5,  RACE_HUMAN},
    { "Polnur the Easterner",   160000L,  250,  150,  10,   5,  RACE_ELF},
    { "Rex Ham-Bone",           100000L,  250,  150,  10,   5,  RACE_HALF_TROLL},
    { "Dirshovich the Bearded", 120000L,  250,  150,  10,   5,  RACE_HALF_ORC},
    { "Jean-Claude the Nice",   140000L,  250,  150,  10,   5,  RACE_HUMAN},
    { "Richard the Stern",      160000L,  250,  150,  10,   5,  RACE_ELF},
    { "Padd the Heavy Handed",  100000L,  250,  150,  10,   5,  RACE_HALF_ELF},
    { "Jon the Cold",           120000L,  250,  150,  10,   5,  RACE_DUNADAN},
    { "Emilio the Empty-Handed",140000L,  250,  150,  10,   5,  RACE_HUMAN},
    { "Lex the Ever-Last",      160000L,  250,  150,  10,   5,  RACE_ELF},
  },
  {
    /* Home */
    { "Your home",              0,      100,  100,   0,  99,  99},
    { "Your home",              0,      100,  100,   0,  99,  99},
    { "Your home",              0,      100,  100,   0,  99,  99},
    { "Your home",              0,      100,  100,   0,  99,  99}
  }
};

/*
 * This table allows quick conversion from "speed" to "energy"
 * The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
 * Note that table access is *much* quicker than computation.
 *
 * Note that the table has been changed at high speeds.  From
 * "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
 * at speeds above "Fast (+30)", one approaches an asymptotic
 * effective limit of 50 energy per turn.  This means that it
 * is relatively easy to reach "Fast (+30)" and get about 40
 * energy per turn, but then speed becomes very "expensive",
 * and you must get all the way to "Fast (+50)" to reach the
 * point of getting 45 energy per turn.  After that point,
 * furthur increases in speed are more or less pointless,
 * except to balance out heavy inventory.
 *
 * Note that currently the fastest monster is "Fast (+30)".
 *
 * It should be possible to lower the energy threshhold from
 * 100 units to 50 units, though this may interact badly with
 * the (compiled out) small random energy boost code.  It may
 * also tend to cause more "clumping" at high speeds.
 */
byte extract_energy[200] =
{
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* Slow */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* S-50 */     1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
/* S-40 */     2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
/* S-30 */     2,  2,  2,  2,  2,  2,  2,  3,  3,  3,
/* S-20 */     3,  3,  3,  3,  3,  4,  4,  4,  4,  4,
/* S-10 */     5,  5,  5,  5,  6,  6,  7,  7,  8,  9,
/* Norm */    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
/* F+10 */    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
/* F+20 */    30, 31, 32, 33, 34, 35, 36, 36, 37, 37,
/* F+30 */    38, 38, 39, 39, 40, 40, 40, 41, 41, 41,
/* F+40 */    42, 42, 42, 43, 43, 43, 44, 44, 44, 44,
/* F+50 */    45, 45, 45, 45, 45, 46, 46, 46, 46, 46,
/* F+60 */    47, 47, 47, 47, 47, 48, 48, 48, 48, 48,
/* F+70 */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
/* Fast */    49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
};

/*
 * Base experience levels, may be adjusted up for race and/or class
 */
s32b player_exp[PY_MAX_LEVEL] =
{
        10,             25,             45,             70,
        100,            140,            200,            280,
        380,            500,            650,            850,
        1100,           1400,           1800,           2300,
        2900,           3600,           4400,           5400,
        6800,           8400,           10200,          12500,
        17500,          25000,          35000L,         50000L,
        75000L,         100000L,        150000L,        200000L,
        275000L,        350000L,        450000L,        550000L,
        700000L,        850000L,        1000000L,       1250000L,
        1500000L,       1800000L,       2100000L,       2400000L,
        2700000L,       3000000L,       3500000L,       4000000L,
        4500000L,       5000000L
};


/*
 * random birth preferences:
 *
 * this is needed for letting a random char choose teachers that are
 * useful to him.
 */
byte teach_class_pref[MAX_CLASS][MAX_CLASS] =
{
   /* warrior mage priest rogue ranger paladin war-mage highprst gladiatr */
   {  10,     2,   1,     5,    8,     9,      4,       1,       9 }, /* warrior  */
   {  2,      10,  4,     7,    7,     3,      8,       9,       1 }, /* mage     */
   {  1,      4,   10,    5,    4,     8,      3,       9,       1 }, /* priest   */
   {  5,      7,   5,     10,   8,     4,      6,       2,       5 }, /* rogue    */
   {  8,      7,   4,     8,    10,    5,      8,       2,       6 }, /* ranger   */
   {  9,      2,   8,     4,    5,     10,     4,       5,       8 }, /* paladin  */
   {  4,      8,   3,     6,    8,     4,      10,      4,       7 }, /* warmage  */
   {  1,      9,   9,     2,    2,     5,      4,       10,      1 }, /* highprst */
   {  9,      1,   1,     5,    6,     8,      7,       1,       10}  /* gladiatr */
};

byte teach_race_pref[MAX_RACES][MAX_CLASS] =
{
   /* warrior mage priest rogue ranger paladin war-mage highprst gladiatr */
   {  5,      5,   5,     5,    5,     5,      5,       5,       5 }, /* human      */
   {  4,      6,   5,     7,    6,     4,      6,       6,       5 }, /* half-elf   */
   {  3,      7,   6,     7,    6,     2,      6,       7,       3 }, /* elf        */
   {  3,      8,   5,     9,    9,     1,      6,       3,       3 }, /* hobbit     */
   {  3,      8,   8,     8,    4,     5,      3,       8,       3 }, /* gnome      */
   {  8,      2,   8,     2,    3,     7,      2,       5,       8 }, /* dwarf      */
   {  8,      2,   6,     6,    4,     3,      5,       2,       8 }, /* half-orc   */
   {  9,      4,   4,     2,    7,     7,      5,       2,       9 }, /* half-troll */
   {  7,      7,   7,     7,    7,     7,      7,       7,       7 }, /* dunadain   */
   {  9,      9,   5,     9,    9,     2,      9,       7,       9 }, /* high-elf   */
   {  6,      7,   8,     2,    8,     6,      6,       8,       5 }  /* druedain   */
};

byte teach_stat_pref[MAX_CLASS][4] =
{
   { A_STR, A_DEX, A_CON, 111   }, /* warrior  */
   { A_INT, A_DEX, A_CON, A_STR }, /* mage     */
   { A_WIS, A_DEX, A_CON, 111   }, /* priest   */
   { A_DEX, A_INT, A_STR, A_CON }, /* rogue    */
   { A_DEX, A_INT, A_CON, A_STR }, /* ranger   */
   { A_STR, A_WIS, A_DEX, A_CON }, /* paladin  */
   { A_STR, A_INT, A_DEX, A_CON }, /* warmage  */
   { A_WIS, A_INT, A_CON, 111   }, /* highprst */
   { A_STR, A_DEX, A_CON, 111   }  /* gladiatr */
};


/*
 * Player Sexes
 *
 *      Title,
 *      Winner
 */
player_sex sex_info[MAX_SEXES] =
{
        {
                "Female",
                "Queen"
        },

        {
                "Male",
                "King"
        }
};

/*
 * Player Race Information:
 *    Title,
 *    {STR,INT,WIS,DEX,CON,CHR}
 *    r_dis, r_dev, r_sav, r_stl, r_srh, r_pcp, r_thn, r_thb, tactic
 *    hitdie, exp base,
 *    Age (Base, Mod),
 *    Male (Hgt, Wgt),
 *    Female (Hgt, Wgt)
 *    infra
 */
/* jk - each race can now play each class - as a test */
player_race race_info[MAX_RACES] =
{
  /* name        STRINTWISDEXCONCHR  D  D  S  S  S  P   T   T  T  HD  E   A   A height                     infra */
  /*                                 I  E  A  T  R  C   H   H  A  II  X   G   G +weight                          */
  /*                                 S  V  V  L  H  P   N   B  C  TE  P   E   E [male      ][female      ]       */
{  "Human",     { 0, 0, 0, 0, 0, 0}, 0, 0, 0, 0, 0,10,  0,  0,4+0,10,100, 14, 6,72, 6,180,25,66, 4,150, 20, 0},
{  "Half-Elf",  {-1, 1, 0, 1,-1, 1}, 2, 3, 3, 1, 6,11, -1,  5,4+0, 9,110, 24,16,66, 6,130,15,62, 6,100, 10, 2},
{  "Elf",       {-1, 2, 1, 1,-2, 1}, 5, 6, 6, 1, 8,12, -5, 15,4+0, 8,120, 75,75,60, 4,100, 6,54, 4, 80,  6, 3},
{  "Hobbit",    {-2, 2, 1, 3, 2, 1},15,18,18, 4,12,15,-10, 20,4-1, 7,110, 21,12,36, 3, 60, 3,33, 3, 50,  3, 4},
{  "Gnome",     {-1, 2, 0, 2, 1,-2},10,12,12, 3, 6,13, -8, 12,4-2, 8,125, 50,40,42, 3, 90, 6,39, 3, 75,  3, 4},
{  "Dwarf",     { 2,-3, 2,-2, 2,-3}, 2, 9, 9,-1, 7,10, 15,  0,4+2,11,120, 35,15,48, 3,150,10,46, 3,120, 10, 5},
{  "Half-Orc",  { 2,-1, 0, 0, 1,-4},-3,-3,-3,-1, 0, 7, 12, -5,4+3,10,110, 11, 4,66, 1,150, 5,62, 1,120,  5, 3},
{  "Half-Troll",{ 4,-4,-2,-4, 3,-6},-5,-8,-8,-2,-1, 5, 20,-10,4+4,12,120, 20,10,96,10,250,50,84, 8,225, 40, 3},
{  "Dunadain",  { 1, 2, 2, 2, 3, 2}, 4, 5, 5, 2, 3,13, 15, 10,4+1,10,180, 50,20,82, 5,190,20,78, 6,180, 15, 0},
{  "High-Elf",  { 1, 3,-1, 3, 1, 5}, 4,20,20, 3, 3,14, 10, 25,4+0,10,200,100,30,90,10,190,20,82,10,180, 15, 4},
{  "Druedain",  { 3, 2, 3,-3, 6,-4}, 2,10,20, 5,12,15,  0, -5,4+0,10,100, 11, 5,45, 3,150,10,43, 3,150, 10, 20}
};

/*
 * Player Classes.
 *
 *                       Title,
 *                       {STR,INT,WIS,DEX,CON,CHR}, spell_stat, spell_encumbrance, spell_level,
 *   STARTING QUALITIES: c_dis, c_dev, c_sav, c_stl, c_srh, c_pcp, c_thn, c_thb, tactic,
 *   PER 10 LEVELS INCR: x_dis, x_dev, x_sav, x_stl, x_srh, x_pcp, x_thn, x_thb,
 *                       HD, Exp, num, wgt, mul
 */
player_class class_info[MAX_CLASS] =
{
/*                 S  I  W  D  C  C   s  spl spl  [ starting qualities]  t [ /10 level incr  ]  h e  n w  m   */
/*                 T  N  I  E  O  H   p  enc  lv  d  d  s  s s  p  t  t  a d  d  s  s s p t  t  i x  u g  u   */
/*                 R  T  S  X  N  R   e           i  e  a  t r  c  h  h  c i  e  a  t r c h  h  t p  m t  l   */
/*                                    l           s  v  v  l h  p  n  b  t s  v  v  l h p n  b  d e           */
/*                                    l                                  c                      c r           */
{ "Warrior",     { 5,-2,-2, 2, 2,-1}, 0,   0, 99, 25,18,18,1,14, 2,70,55,7,10, 7,10,0,0,0,45,45,9, 0,6,30,5 },
{ "Mage",        {-5, 3, 0, 1,-2, 1}, 1, 300,  1, 30,36,30,2,16,20,34,20,1, 7,13, 9,0,0,0,15,15,0,30,4,40,2 },
{ "Priest",      {-1,-3, 3,-1, 0, 2}, 2, 350,  1, 25,30,32,2,16, 8,48,35,2, 7,10,12,0,0,0,20,20,2,20,5,35,3 },
{ "Rogue",       { 2, 1,-2, 3, 1,-1}, 1, 350,  5, 45,32,28,5,32,24,60,66,4,15,10,10,0,0,0,40,30,6,25,5,30,3 },
{ "Ranger",      { 2, 2, 0, 1, 1, 1}, 1, 400,  3, 30,32,28,3,24,16,56,72,5, 8,10,10,0,0,0,30,45,4,30,5,35,4 },
{ "Paladin",     { 3,-3, 1, 0, 2, 2}, 2, 400,  1, 20,24,25,1,12, 2,68,40,6, 7,10,11,0,0,0,35,30,6,35,5,30,4 },
{ "War-Mage",    { 2, 1,-3, 2, 0,-1}, 1, 300,  1, 30,24,24,2,15,12,45,38,4, 9,10,10,0,0,0,30,30,5,35,5,35,4 },
{ "High Priest", {-5, 3, 3, 0,-1, 2}, 2, 350,  1, 30,45,32,2,16,20, 0, 0,0, 7,10,10,0,0,0, 0, 0,5,15,0, 0,0 },
{ "Gladiator",   { 5,-2,-2, 2, 2,-1}, 0,   0, 99, 25,14,18,1,14, 2,70,45,7,10, 7,10,0,0,0,45,45,9, 0,6,30,5 }
};

/* jk - to hit, to dam, to ac, to stealth, to disarm, to saving throw */
/* this concept is taken from Adom, where Thomas Biskup thought it out, */
/* as far as I know - thanks, Thomas!, for a great game. */
tactic_info_type tactic_info[9] =
{
/*     hit  dam   ac stl  dis  sav */
     { -14, -13, +15, +3, +15, +14, "coward"},           /* 4-4 */
     {  -8,  -8, +11, +2,  +9,  +9, "meek"},             /* 4-3 */
     {  -4,  -4,  +7, +1,  +5,  +5, "wary"},             /* 4-2 */
     {  -2,  -2,  +3, +1,  +2,  +2, "careful"},          /* 4-1 */
     {   0,   0,   0,  0,   0,   0, "normal"},           /* 4+0 */
     {   2,   3,  -2, -1,  -2,  -3, "confident"},        /* 4+1 */
     {   3,   5,  -5, -2,  -5,  -7, "aggressive"},       /* 4+2 */
     {   5,   7, -10, -3, -11, -12, "furious"},          /* 4+3 */
     {   8,  12, -25, -5, -18, -18, "berserker"}         /* 4+4 */
};

move_info_type move_info[9] =
{
/*    speed, searching, stealth, perception */
    { -10,     17,        8,      28, "crawling"},
    {  -8,     12,        6,      21, "very slow"},
    {  -6,      8,        5,      15, "slow"},
    {  -3,      4,        3,       8, "leisurely"},
    {   0,      0,        0,       0, "normal"},
    {   1,     -4,       -1,      -4, "brisk"},
    {   2,     -6,       -2,      -8, "fast"},
    {   3,    -10,       -3,     -14, "very fast"},
    {   4,    -16,       -5,     -20, "running"}
};

/*
 * Class titles for the player.
 *
 * The player gets a new title every five levels, so each class
 * needs only ten titles total.
 */
cptr player_title[MAX_CLASS][PY_MAX_LEVEL/5] =
{
   /* Warrior */
   {
       "Rookie",
       "Soldier",
       "Mercenary",
       "Veteran",
       "Swordsman",
       "Champion",
       "Hero",
       "Baron",
       "Duke",
       "Lord",
   },

   /* Mage */
   {
       "Novice",
       "Apprentice",
       "Trickster",
       "Illusionist",
       "Spellbinder",
       "Evoker",
       "Conjurer",
       "Warlock",
       "Sorcerer",
       "Mage Lord",
   },

   /* Priest */
   {
       "Believer",
       "Acolyte",
       "Adept",
       "Curate",
       "Canon",
       "Lama",
       "Patriarch",
       "Priest",
       "Head Priest",
       "Priest Lord",
   },

   /* Rogues */
   {
       "Vagabond",
       "Cutpurse",
       "Robber",
       "Burglar",
       "Filcher",
       "Sharper",
       "Low Thief",
       "High Thief",
       "Master Thief",
       "Assassin",
   },

   /* Rangers */
   {
       "Runner",
       "Strider",
       "Scout",
       "Courser",
       "Tracker",
       "Guide",
       "Pathfinder",
       "Low Ranger",
       "High Ranger",
       "Ranger Lord",
   },

   /* Paladins */
   {
       "Gallant",
       "Keeper",
       "Protector",
       "Defender",
       "Warder",
       "Knight",
       "Guardian",
       "Low Paladin",
       "High Paladin",
       "Paladin Lord",
   },

   /* War-Mages */
   {
       "Novice",
       "Apprentice",
       "Protector",
       "Magic Guard",
       "Spellcaster",
       "Evil Hunter",
       "Conjurer",
       "Warlock",
       "Sorcerer",
       "War Lord",
   },

   /* High Priest */
   {
       "Novice",
       "Apprentice",
       "Acolyte",
       "Adept",
       "Spellbinder",
       "Curate",
       "Lama",
       "Conjurer",
       "High Mage",
       "Priest Lord",
   },

   /* Gladiator */
   {
       "Rookie",
       "Apprentice",
       "Entertainer",
       "Fighter",
       "Team Fighter",
       "Team Leader",
       "Gladiator",
       "Veteran",
       "Tactician",
       "Champion"
   }

};



/*
 * Hack -- the "basic" color names (see "TERM_xxx")
 */
cptr color_names[16] =
{
   "Dark",
   "White",
   "Slate",
   "Orange",
   "Red",
   "Green",
   "Blue",
   "Umber",
   "Light Dark",
   "Light Slate",
   "Violet",
   "Yellow",
   "Light Red",
   "Light Green",
   "Light Blue",
   "Light Umber",
};


/*
 * Hack -- the "basic" sound names (see "SOUND_xxx")
 */
cptr sound_names[SOUND_MAX] =
{
   "",
   "hit",
   "miss",
   "flee",
   "drop",
   "kill",
   "level",
   "death",
};

/*
 * Certain "screens" always use the main screen, including News, Birth,
 * Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
 *
 * Later, special flags may allow sub-windows to "steal" stuff from the
 * main window, including File dump (help), File dump (artifacts, uniques),
 * Character screen, Small scale map, Previous Messages, Store screen, etc.
 */
cptr window_flag_desc[32] =
{
   "Display inven/equip",
   "Display equip/inven",
   "Display player (basic)",
   "Display player (extra)",
   "Display Last Kills",
   "",
   "Display messages",
   "Display overhead view",
   "Display monster recall",
   "Display object recall (NOT WORKING!)",
   "",
   "Display snap-shot",
   "",
   "",
   "Display borg messages",
   "Display borg status",
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
 * Abbreviations of healthy stats
 */
cptr stat_names[6] =
{
    "STR: ", "INT: ", "WIS: ", "DEX: ", "CON: ", "CHR: "
};

/*
 * Abbreviations of damaged stats
 */
cptr stat_names_reduced[6] =
{
    "Str: ", "Int: ", "Wis: ", "Dex: ", "Con: ", "Chr: "
};

/*
 * The different flavors. ? signifies an unused flavor, so it is recognizable
 * if used by accident
 * NOTE: EACH CLASS MUST HAVE AT LEAST ONE ? record at the end!!
 */
flavor_type flavor[MAX_FLAVORS] =
{
/* entry 0-79: rings */
/*    0 */    { "Adamantite",       TERM_L_GREEN },
/*    1 */    { "Alexandrite",      TERM_GREEN },
/*    2 */    { "Amber",            TERM_L_BLUE },
/*    3 */    { "Amethyst",         TERM_VIOLET },
/*    4 */    { "Aquamarine",       TERM_L_BLUE },
/*    5 */    { "Azurite",          TERM_L_BLUE },
/*    6 */    { "Beryl",            TERM_L_GREEN },
/*    7 */    { "Black Tourmaline", TERM_SLATE },
/*    8 */    { "Bloodstone",       TERM_RED },
/*    9 */    { "Bluish Topaz",     TERM_L_BLUE },
/*   10 */    { "Brown Garnet",     TERM_BROWN },
/*   11 */    { "Brown Titanite",   TERM_BROWN },
/*   12 */    { "Brown Topaz",      TERM_BROWN },
/*   13 */    { "Calcite",          TERM_WHITE },
/*   14 */    { "Carnelian",        TERM_RED },
/*   15 */    { "Corundum",         TERM_SLATE },
/*   16 */    { "Diamond",          TERM_WHITE },
/*   17 */    { "Emerald",          TERM_GREEN },
/*   18 */    { "Fluorite",         TERM_L_GREEN },
/*   19 */    { "Granite",          TERM_L_WHITE },
/*   20 */    { "Green Garnet",     TERM_GREEN },
/*   21 */    { "Green Topaz",      TERM_L_GREEN },
/*   22 */    { "Green Tourmaline", TERM_GREEN },
/*   23 */    { "Grey Titanite",    TERM_SLATE },
/*   24 */    { "Jade",             TERM_L_GREEN },
/*   25 */    { "Jasper",           TERM_BROWN },
/*   26 */    { "Jet",              TERM_L_DARK },
/*   27 */    { "Lapis Lazuli",     TERM_BLUE },
/*   28 */    { "Malachite",        TERM_GREEN },
/*   29 */    { "Marble",           TERM_WHITE },
/*   30 */    { "Moonstone",        TERM_L_WHITE },
/*   31 */    { "Morganite",        TERM_VIOLET },
/*   32 */    { "Onyx",             TERM_L_RED },
/*   33 */    { "Opal",             TERM_L_WHITE },
/*   34 */    { "Pearl",            TERM_WHITE },
/*   35 */    { "Pink Topaz",       TERM_VIOLET },
/*   36 */    { "Pink Tourmaline",  TERM_VIOLET },
/*   37 */    { "Quartz",           TERM_L_WHITE },
/*   38 */    { "Quartzite",        TERM_L_WHITE },
/*   39 */    { "Red Garnet",       TERM_RED },
/*   40 */    { "Red Titanite",     TERM_RED },
/*   41 */    { "Rhodonite",        TERM_L_RED },
/*   42 */    { "Ruby",             TERM_RED },
/*   43 */    { "Sapphire",         TERM_BLUE },
/*   44 */    { "Tiger Eye",        TERM_YELLOW },
/*   45 */    { "Tortoise Shell",   TERM_BROWN },
/*   46 */    { "Turquoise",        TERM_L_BLUE },
/*   47 */    { "Violet Garnet",    TERM_VIOLET },
/*   48 */    { "Yellow Garnet",    TERM_YELLOW },
/*   49 */    { "Yellow Titanite",  TERM_YELLOW },
/*   50 */    { "Yellow Topaz",     TERM_YELLOW },
/*   51 */    { "Zircon",           TERM_L_BROWN },
/*   52 */    { "?",                -1 },
/*   53 */    { "?",                -1 },
/*   54 */    { "?",                -1 },
/*   55 */    { "?",                -1 },
/*   56 */    { "?",                -1 },
/*   57 */    { "?",                -1 },
/*   58 */    { "?",                -1 },
/*   59 */    { "?",                -1 },
/*   60 */    { "?",                -1 },
/*   61 */    { "?",                -1 },
/*   62 */    { "?",                -1 },
/*   63 */    { "?",                -1 },
/*   64 */    { "?",                -1 },
/*   65 */    { "?",                -1 },
/*   66 */    { "?",                -1 },
/*   67 */    { "?",                -1 },
/*   68 */    { "?",                -1 },
/*   69 */    { "?",                -1 },
              /* Amulets (adjectives and colors) */
/*   70 */    { "Amber",          TERM_YELLOW },
/*   71 */    { "Obsidian",       TERM_L_DARK },
/*   72 */    { "Tortoise Shell", TERM_BROWN },
/*   73 */    { "Copper",         TERM_L_BROWN },
/*   74 */    { "Driftwood",      TERM_L_BROWN },
/*   75 */    { "Bone",           TERM_WHITE },
/*   76 */    { "Golden",         TERM_YELLOW },
/*   77 */    { "Coral",          TERM_WHITE },
/*   78 */    { "Brass",          TERM_L_BROWN },
/*   79 */    { "Azure",          TERM_L_BLUE },
/*   80 */    { "Agate",          TERM_L_WHITE },
/*   81 */    { "Bronze",         TERM_L_BROWN },
/*   82 */    { "Crystal",        TERM_WHITE },
/*   83 */    { "Ivory",          TERM_WHITE },
/*   84 */    { "Pewter",         TERM_SLATE },
/*   85 */    { "Silver",         TERM_L_WHITE },
/*   86 */    { "?",                -1},
/*   87 */    { "?",                -1},
/*   88 */    { "?",                -1},
/*   89 */    { "?",                -1},
/*   90 */    { "?",                -1},
/*   91 */    { "?",                -1},
/*   92 */    { "?",                -1},
/*   93 */    { "?",                -1},
/*   94 */    { "?",                -1},
/*   95 */    { "?",                -1},
/*   96 */    { "?",                -1},
/*   97 */    { "?",                -1},
/*   98 */    { "?",                -1},
/*   99 */    { "?",                -1},
              /* Staffs (adjectives and colors) */
/*  100 */    { "Aspen",      TERM_L_BROWN },
/*  101 */    { "Balsa",      TERM_L_BROWN },
/*  102 */    { "Banyan",     TERM_L_BROWN },
/*  103 */    { "Birch",      TERM_L_BROWN },
/*  104 */    { "Cedar",      TERM_L_RED },
/*  105 */    { "Cottonwood", TERM_L_BROWN },
/*  106 */    { "Cypress",    TERM_BROWN },
/*  107 */    { "Dogwood",    TERM_BROWN },
/*  108 */    { "Elm",        TERM_L_BROWN },
/*  109 */    { "Eucalyptus", TERM_L_BROWN },
/*  110 */    { "Hemlock",    TERM_WHITE },
/*  111 */    { "Hickory",    TERM_WHITE },
/*  112 */    { "Ironwood",   TERM_BROWN },
/*  113 */    { "Locust",     TERM_BROWN },
/*  114 */    { "Mahogany",   TERM_BROWN },
/*  115 */    { "Maple",      TERM_WHITE },
/*  116 */    { "Mulberry",   TERM_BROWN },
/*  117 */    { "Oak",        TERM_BROWN },
/*  118 */    { "Pine",       TERM_YELLOW },
/*  119 */    { "Redwood",    TERM_L_RED },
/*  120 */    { "Robinia",    TERM_GREEN },
/*  121 */    { "Rosewood",   TERM_BROWN },
/*  122 */    { "Spruce",     TERM_WHITE },
/*  123 */    { "Sycamore",   TERM_WHITE },
/*  124 */    { "Teak",       TERM_BROWN },
/*  125 */    { "Walnut",     TERM_BROWN },
/*  126 */    { "Mistletoe",  TERM_BROWN },
/*  127 */    { "Hawthorn",   TERM_BROWN },
/*  128 */    { "Bamboo",     TERM_YELLOW },
/*  129 */    { "Silver",     TERM_WHITE },
/*  130 */    { "Runed",      TERM_VIOLET },
/*  131 */    { "Golden",     TERM_YELLOW },
/*  132 */    { "Ashen",      TERM_WHITE },
/*  133 */    { "Gnarled",    TERM_WHITE },
/*  134 */    { "Ivory",      TERM_WHITE },
/*  135 */    { "Willow",     TERM_WHITE },
/*  136 */    { "?",                -1},
/*  137 */    { "?",                -1},
/*  138 */    { "?",                -1},
/*  139 */    { "?",                -1},
/*  140 */    { "?",                -1},
/*  141 */    { "?",                -1},
/*  142 */    { "?",                -1},
/*  143 */    { "?",                -1},
/*  144 */    { "?",                -1},
/*  145 */    { "?",                -1},
/*  146 */    { "?",                -1},
/*  147 */    { "?",                -1},
/*  148 */    { "?",                -1},
/*  149 */    { "?",                -1},
              /* Wands (adjectives and colors) */
/*  150 */    { "Aluminum",        TERM_L_BLUE },
/*  151 */    { "Aluminum-Plated", TERM_L_BLUE },
/*  152 */    { "Brass",           TERM_L_BROWN },
/*  153 */    { "Bronze",          TERM_L_BROWN },
/*  154 */    { "Cast Iron",       TERM_L_DARK },
/*  155 */    { "Chrome",          TERM_WHITE },
/*  156 */    { "Copper",          TERM_L_BROWN },
/*  157 */    { "Copper-Plated",   TERM_L_BROWN  },
/*  158 */    { "Glass",           TERM_ORANGE },
/*  159 */    { "Gold",            TERM_YELLOW },
/*  160 */    { "Gold-Plated",     TERM_YELLOW },
/*  161 */    { "Iron",            TERM_SLATE },
/*  162 */    { "Ivory",           TERM_WHITE },
/*  163 */    { "Lead",            TERM_SLATE },
/*  164 */    { "Lead-Plated",     TERM_SLATE },
/*  165 */    { "Magnesium",       TERM_L_WHITE },
/*  166 */    { "Mithril",         TERM_L_BLUE },
/*  167 */    { "Mithril-Plated",  TERM_L_BLUE },
/*  168 */    { "Molybdenum",      TERM_L_WHITE },
/*  169 */    { "Nickel",          TERM_L_BROWN },
/*  170 */    { "Nickel-Plated",   TERM_L_BROWN },
/*  171 */    { "Pewter",          TERM_SLATE },
/*  172 */    { "Platinum",        TERM_WHITE },
/*  173 */    { "Runed",           TERM_BROWN },
/*  174 */    { "Rusty",           TERM_RED },
/*  175 */    { "Silver",          TERM_L_WHITE },
/*  176 */    { "Silver-Plated",   TERM_L_WHITE },
/*  177 */    { "Steel",           TERM_L_WHITE },
/*  178 */    { "Steel-Plated",    TERM_L_WHITE },
/*  179 */    { "Tin",             TERM_L_WHITE },
/*  180 */    { "Tin-Plated",      TERM_L_WHITE },
/*  181 */    { "Titanium",        TERM_WHITE },
/*  182 */    { "Tungsten",        TERM_WHITE },
/*  183 */    { "Zinc",            TERM_L_WHITE },
/*  184 */    { "Zinc-Plated",     TERM_L_WHITE },
/*  185 */    { "Zirconium",       TERM_L_WHITE },
/*  186 */    { "?",                -1},
/*  187 */    { "?",                -1},
/*  188 */    { "?",                -1},
/*  189 */    { "?",                -1},
/*  190 */    { "?",                -1},
/*  191 */    { "?",                -1},
/*  192 */    { "?",                -1},
/*  193 */    { "?",                -1},
/*  194 */    { "?",                -1},
/*  195 */    { "?",                -1},
/*  196 */    { "?",                -1},
/*  197 */    { "?",                -1},
/*  198 */    { "?",                -1},
/*  199 */    { "?",                -1},
              /* Rods (adjectives and colors) */
/*  200 */    { "Aluminum",        TERM_L_BLUE },
/*  201 */    { "Aluminum-Plated", TERM_L_BLUE },
/*  202 */    { "Brass",           TERM_L_BROWN },
/*  203 */    { "Bronze",          TERM_L_BROWN },
/*  204 */    { "Cast Iron",       TERM_L_DARK },
/*  205 */    { "Chrome",          TERM_WHITE },
/*  206 */    { "Copper",          TERM_L_BROWN },
/*  207 */    { "Copper-Plated",   TERM_L_BROWN  },
/*  208 */    { "Glass",           TERM_ORANGE },
/*  209 */    { "Gold",            TERM_YELLOW },
/*  210 */    { "Gold-Plated",     TERM_YELLOW },
/*  211 */    { "Iron",            TERM_SLATE },
/*  212 */    { "Iron Studded",    TERM_SLATE },
/*  213 */    { "Ivory",           TERM_WHITE },
/*  214 */    { "Lead",            TERM_SLATE },
/*  215 */    { "Lead-Plated",     TERM_SLATE },
/*  216 */    { "Magnesium",       TERM_L_WHITE },
/*  217 */    { "Mithril",         TERM_L_BLUE },
/*  218 */    { "Mithril-Plated",  TERM_L_BLUE },
/*  219 */    { "Molybdenum",      TERM_L_WHITE },
/*  220 */    { "Nickel",          TERM_L_BROWN },
/*  221 */    { "Nickel-Plated",   TERM_L_BROWN },
/*  222 */    { "Pewter",          TERM_SLATE },
/*  223 */    { "Platinum",        TERM_WHITE },
/*  224 */    { "Platinum-Plated", TERM_WHITE },
/*  225 */    { "Runed",           TERM_BROWN },
/*  226 */    { "Rusty",           TERM_RED },
/*  227 */    { "Silver",          TERM_L_WHITE },
/*  228 */    { "Silver-Plated",   TERM_L_WHITE },
/*  229 */    { "Steel",           TERM_L_WHITE },
/*  230 */    { "Steel-Plated",    TERM_L_WHITE },
/*  231 */    { "Tin",             TERM_L_WHITE },
/*  232 */    { "Tin-Plated",      TERM_L_WHITE },
/*  233 */    { "Titanium",        TERM_WHITE },
/*  234 */    { "Tungsten",        TERM_WHITE },
/*  235 */    { "Zinc",            TERM_L_WHITE },
/*  236 */    { "Zinc-Plated",     TERM_L_WHITE },
/*  237 */    { "Zirconium",       TERM_L_WHITE },
/*  238 */    { "Ruby-Decorated",  TERM_RED },
/*  239 */    { "Topaz-Decorated", TERM_RED },
/*  240 */    { "Blue-Stained",    TERM_L_BLUE },
/*  241 */    { "Red-Stained",     TERM_RED },
/*  242 */    { "Yellow-Striped",  TERM_YELLOW },
/*  243 */    { "Green-Dotted",    TERM_GREEN },
/*  244 */    { "Green-Stained",   TERM_L_GREEN },
/*  245 */    { "Dull Black",      TERM_L_DARK },
/*  246 */    { "Grey Colored",    TERM_SLATE },
/*  247 */    { "Glowing",         TERM_ORANGE },
/*  248 */    { "Blue-Striped",    TERM_BLUE },
/*  249 */    { "Ironwood",        TERM_BROWN },
/*  250 */    { "?",                -1},
/*  251 */    { "?",                -1},
/*  252 */    { "?",                -1},
/*  253 */    { "?",                -1},
/*  254 */    { "?",                -1},
/*  255 */    { "?",                -1},
/*  256 */    { "?",                -1},
/*  257 */    { "?",                -1},
/*  258 */    { "?",                -1},
/*  259 */    { "?",                -1},
/*  260 */    { "?",                -1},
/*  261 */    { "?",                -1},
/*  262 */    { "?",                -1},
/*  263 */    { "?",                -1},
/*  264 */    { "?",                -1},
/*  265 */    { "?",                -1},
/*  266 */    { "?",                -1},
/*  267 */    { "?",                -1},
/*  268 */    { "?",                -1},
/*  269 */    { "?",                -1},
              /* food (adjectives and colors) */
/*  270 */    { "Black Spotted",   TERM_L_DARK },
/*  271 */    { "Black Striped",   TERM_L_DARK },
/*  272 */    { "Black",           TERM_L_DARK },
/*  273 */    { "Blue Spotted",    TERM_BLUE },
/*  274 */    { "Blue Striped",    TERM_BLUE },
/*  275 */    { "Blue",            TERM_BLUE },
/*  276 */    { "Bright White",    TERM_WHITE },
/*  277 */    { "Brown Spotted",   TERM_BROWN },
/*  278 */    { "Brown Striped",   TERM_BROWN },
/*  279 */    { "Brown",           TERM_BROWN },
/*  280 */    { "Dark Blue",       TERM_BLUE },
/*  281 */    { "Dark Green",      TERM_GREEN },
/*  282 */    { "Dark Red",        TERM_RED },
/*  283 */    { "Deep Green",      TERM_GREEN },
/*  284 */    { "Dirty",           TERM_SLATE },
/*  285 */    { "Dried",           TERM_WHITE },
/*  286 */    { "Dusty",           TERM_SLATE },
/*  287 */    { "Filthy",          TERM_SLATE },
/*  288 */    { "Furry",           TERM_L_WHITE },
/*  289 */    { "Glowing",         TERM_YELLOW },
/*  290 */    { "Green Slimy",     TERM_L_GREEN },
/*  291 */    { "Green Spotted",   TERM_L_GREEN },
/*  292 */    { "Green Striped",   TERM_GREEN },
/*  293 */    { "Green",           TERM_GREEN },
/*  294 */    { "Grey Spotted",    TERM_SLATE },
/*  295 */    { "Grey Striped",    TERM_SLATE },
/*  296 */    { "Grey",            TERM_SLATE },
/*  297 */    { "Light Blue",      TERM_L_BLUE },
/*  298 */    { "Light Green",     TERM_L_GREEN },
/*  299 */    { "Mildewed",        TERM_WHITE },
/*  300 */    { "Mouldy",          TERM_SLATE },
/*  301 */    { "Muddy",           TERM_BROWN },
/*  302 */    { "Red Spotted",     TERM_RED },
/*  303 */    { "Red Striped",     TERM_RED },
/*  304 */    { "Red",             TERM_RED },
/*  305 */    { "Rotten",          TERM_BROWN },
/*  306 */    { "Slimy",           TERM_SLATE },
/*  307 */    { "Splotchy",        TERM_L_WHITE },
/*  308 */    { "Spongy",          TERM_YELLOW },
/*  309 */    { "Tan",             TERM_L_BROWN },
/*  310 */    { "Violet Spotted",  TERM_VIOLET },
/*  311 */    { "Violet",          TERM_VIOLET },
/*  312 */    { "White Spotted",   TERM_WHITE },
/*  313 */    { "White",           TERM_WHITE },
/*  314 */    { "Wrinkled",        TERM_BROWN },
/*  315 */    { "Yellow And Red",  TERM_YELLOW },
/*  316 */    { "Yellow Spotted",  TERM_YELLOW },
/*  317 */    { "Yellow Striped",  TERM_YELLOW },
/*  318 */    { "Yellow",          TERM_YELLOW },
/*  319 */    { "?",               -1 },
/*  320 */    { "?",               -1 },
/*  321 */    { "?",               -1 },
/*  322 */    { "?",               -1 },
/*  323 */    { "?",               -1 },
/*  324 */    { "?",               -1 },
/*  325 */    { "?",               -1 },
/*  326 */    { "?",               -1 },
/*  327 */    { "?",               -1 },
/*  328 */    { "?",               -1 },
/*  329 */    { "?",               -1 },
/*  330 */    { "?",               -1 },
/*  331 */    { "?",               -1 },
/*  332 */    { "?",               -1 },
/*  333 */    { "?",               -1 },
/*  334 */    { "?",               -1 },
/*  335 */    { "?",               -1 },
/*  336 */    { "?",               -1 },
/*  337 */    { "?",               -1 },
/*  338 */    { "?",               -1 },
/*  339 */    { "?",               -1 },
              /* potions */
/*  340 */    { "Clear",             TERM_WHITE },
/*  341 */    { "Azure",             TERM_L_BLUE },
/*  342 */    { "Brown",             TERM_BROWN },
/*  343 */    { "Cloudy",            TERM_WHITE },
/*  344 */    { "Blue Speckled",     TERM_BLUE },
/*  345 */    { "Blue Swirly",       TERM_BLUE },
/*  346 */    { "Blue",              TERM_BLUE },
/*  347 */    { "Brown Speckled",    TERM_BROWN },
/*  348 */    { "Bubbling",          TERM_L_WHITE },
/*  349 */    { "Chartreuse",        TERM_L_GREEN },
/*  350 */    { "Clotted Red",       TERM_RED },
/*  351 */    { "Coagulated Crimson",TERM_RED },
/*  352 */    { "Copper Speckled",   TERM_L_BROWN },
/*  353 */    { "Crimson",           TERM_RED },
/*  354 */    { "Cyan",              TERM_L_BLUE },
/*  355 */    { "Dark Blue",         TERM_BLUE },
/*  356 */    { "Dark Green",        TERM_GREEN },
/*  357 */    { "Dark Red",          TERM_RED },
/*  358 */    { "Gloopy Green",      TERM_GREEN },
/*  359 */    { "Gold Speckled",     TERM_YELLOW },
/*  360 */    { "Gold",              TERM_YELLOW },
/*  361 */    { "Green Speckled",    TERM_GREEN },
/*  362 */    { "Green",             TERM_GREEN },
/*  363 */    { "Grey Speckled",     TERM_SLATE },
/*  364 */    { "Grey",              TERM_SLATE },
/*  365 */    { "Hazy",              TERM_L_WHITE },
/*  366 */    { "Icky Green",        TERM_GREEN },
/*  367 */    { "Icky Green",        TERM_GREEN },
/*  368 */    { "Indigo",            TERM_VIOLET },
/*  369 */    { "Light Blue",        TERM_L_BLUE },
/*  370 */    { "Light Brown",       TERM_L_BROWN },
/*  371 */    { "Light Green",       TERM_L_GREEN },
/*  372 */    { "Magenta",           TERM_RED },
/*  373 */    { "Metallic Blue",     TERM_BLUE },
/*  374 */    { "Metallic Brown",    TERM_BROWN },
/*  375 */    { "Metallic Green",    TERM_GREEN },
/*  376 */    { "Metallic Grey",     TERM_SLATE },
/*  377 */    { "Metallic Orange",   TERM_ORANGE },
/*  378 */    { "Metallic Purple",   TERM_VIOLET },
/*  379 */    { "Metallic Red",      TERM_RED },
/*  380 */    { "Metallic Yellow",   TERM_YELLOW },
/*  381 */    { "Misty",             TERM_L_WHITE },
/*  382 */    { "Oily Yellow",       TERM_YELLOW },
/*  383 */    { "Orange Speckled",   TERM_ORANGE },
/*  384 */    { "Orange",            TERM_ORANGE },
/*  385 */    { "Pink Speckled",     TERM_L_RED },
/*  386 */    { "Pink",              TERM_L_RED },
/*  387 */    { "Puce",              TERM_VIOLET },
/*  388 */    { "Pungent",           TERM_L_RED },
/*  389 */    { "Purple Speckled",   TERM_VIOLET },
/*  390 */    { "Purple",            TERM_VIOLET },
/*  391 */    { "Red Speckled",      TERM_RED },
/*  392 */    { "Red",               TERM_RED },
/*  393 */    { "Shimmering",        TERM_MULTI },
/*  394 */    { "Silver Speckled",   TERM_L_WHITE },
/*  395 */    { "Smoky",             TERM_L_DARK },
/*  396 */    { "Tangerine",         TERM_ORANGE },
/*  397 */    { "Vermilion",         TERM_RED },
/*  398 */    { "Violet Speckled",   TERM_VIOLET },
/*  399 */    { "Violet",            TERM_VIOLET },
/*  400 */    { "Viscous Blue",      TERM_BLUE },
/*  401 */    { "Viscous Brown",     TERM_BROWN },
/*  402 */    { "Viscous Clear",     TERM_WHITE },
/*  403 */    { "Viscous Green",     TERM_GREEN },
/*  404 */    { "Viscous Grey",      TERM_SLATE },
/*  405 */    { "Viscous Orange",    TERM_ORANGE },
/*  406 */    { "Viscous Pink",      TERM_L_RED },
/*  407 */    { "Viscous White",     TERM_WHITE },
/*  408 */    { "Viscous Yellow",    TERM_YELLOW },
/*  409 */    { "White",             TERM_WHITE },
/*  410 */    { "Yellow Speckled",   TERM_YELLOW },
/*  411 */    { "Yellow",            TERM_YELLOW },
/*  412 */    { "?",                -1},
/*  413 */    { "?",                -1},
/*  414 */    { "?",                -1},
/*  415 */    { "?",                -1},
/*  416 */    { "?",                -1},
/*  417 */    { "?",                -1},
/*  418 */    { "?",                -1},
/*  419 */    { "?",                -1},
/*  420 */    { "?",                -1},
/*  421 */    { "?",                -1},
/*  422 */    { "?",                -1},
/*  423 */    { "?",                -1},
/*  424 */    { "?",                -1},
/*  425 */    { "?",                -1},
/*  426 */    { "?",                -1},
/*  427 */    { "?",                -1},
/*  428 */    { "?",                -1},
/*  429 */    { "?",                -1},
               /* scrolls - are filled in later... */
/*  430-439 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  440-449 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  450-459 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  460-469 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  470-479 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  480-489 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"?",TERM_WHITE},
/*  490-499 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"?",TERM_WHITE},
/*  500-509 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"?",TERM_WHITE},
               /* spells - like scrolls, but 250 in use */
/*  510-519 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  520-529 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  530-539 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  540-549 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  550-559 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  560-569 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  570-579 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  580-589 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  590-599 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  600-609 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  610-619 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  620-629 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  630-639 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  640-649 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  650-659 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"?",TERM_WHITE},
/*  660-669 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  670-679 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  680-689 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  690-699 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  700-709 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  710-719 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  720-729 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  730-739 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  740-749 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  760-769 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  770-779 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  780-789 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  790-799 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
/*  800-809 */ {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               {"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},{"*",TERM_WHITE},
               /* bolt/beam graphics horizontal */
/*  810 */     { "-", TERM_DARK   },
/*  811 */     { "-", TERM_WHITE   },
/*  812 */     { "-", TERM_SLATE   },
/*  813 */     { "-", TERM_ORANGE  },
/*  814 */     { "-", TERM_RED     },
/*  815 */     { "-", TERM_GREEN   },
/*  816 */     { "-", TERM_BLUE    },
/*  817 */     { "-", TERM_BROWN   },
/*  818 */     { "-", TERM_L_DARK  },
/*  819 */     { "-", TERM_L_WHITE },
/*  820 */     { "-", TERM_VIOLET  },
/*  821 */     { "-", TERM_YELLOW  },
/*  822 */     { "-", TERM_L_RED   },
/*  823 */     { "-", TERM_L_GREEN },
/*  824 */     { "-", TERM_L_BLUE  },
/*  825 */     { "-", TERM_L_BROWN },
               /* bolt/beam graphics vertical */
/*  826 */     { "-", TERM_DARK   },
/*  827 */     { "-", TERM_WHITE   },
/*  828 */     { "-", TERM_SLATE   },
/*  829 */     { "-", TERM_ORANGE  },
/*  830 */     { "-", TERM_RED     },
/*  831 */     { "-", TERM_GREEN   },
/*  832 */     { "-", TERM_BLUE    },
/*  833 */     { "-", TERM_BROWN   },
/*  834 */     { "-", TERM_L_DARK  },
/*  835 */     { "-", TERM_L_WHITE },
/*  836 */     { "-", TERM_VIOLET  },
/*  837 */     { "-", TERM_YELLOW  },
/*  838 */     { "-", TERM_L_RED   },
/*  839 */     { "-", TERM_L_GREEN },
/*  840 */     { "-", TERM_L_BLUE  },
/*  841 */     { "-", TERM_L_BROWN },
               /* bolt/beam graphics diagonal1 */
/*  842 */     { "/", TERM_DARK   },
/*  843 */     { "/", TERM_WHITE   },
/*  844 */     { "/", TERM_SLATE   },
/*  845 */     { "/", TERM_ORANGE  },
/*  846 */     { "/", TERM_RED     },
/*  847 */     { "/", TERM_GREEN   },
/*  848 */     { "/", TERM_BLUE    },
/*  849 */     { "/", TERM_BROWN   },
/*  850 */     { "/", TERM_L_DARK  },
/*  851 */     { "/", TERM_L_WHITE },
/*  852 */     { "/", TERM_VIOLET  },
/*  853 */     { "/", TERM_YELLOW  },
/*  854 */     { "/", TERM_L_RED   },
/*  855 */     { "/", TERM_L_GREEN },
/*  856 */     { "/", TERM_L_BLUE  },
/*  857 */     { "/", TERM_L_BROWN },
               /* bolt/beam graphics diagonal2 */
/*  858 */     { "\\", TERM_DARK   },
/*  859 */     { "\\", TERM_WHITE   },
/*  860 */     { "\\", TERM_SLATE   },
/*  861 */     { "\\", TERM_ORANGE  },
/*  862 */     { "\\", TERM_RED     },
/*  863 */     { "\\", TERM_GREEN   },
/*  864 */     { "\\", TERM_BLUE    },
/*  865 */     { "\\", TERM_BROWN   },
/*  866 */     { "\\", TERM_L_DARK  },
/*  867 */     { "\\", TERM_L_WHITE },
/*  868 */     { "\\", TERM_VIOLET  },
/*  869 */     { "\\", TERM_YELLOW  },
/*  860 */     { "\\", TERM_L_RED   },
/*  871 */     { "\\", TERM_L_GREEN },
/*  872 */     { "\\", TERM_L_BLUE  },
/*  873 */     { "\\", TERM_L_BROWN },
               /* ball graphics */
/*  874 */     { "*", TERM_DARK   },
/*  875 */     { "*", TERM_WHITE   },
/*  876 */     { "*", TERM_SLATE   },
/*  877 */     { "*", TERM_ORANGE  },
/*  878 */     { "*", TERM_RED     },
/*  879 */     { "*", TERM_GREEN   },
/*  880 */     { "*", TERM_BLUE    },
/*  881 */     { "*", TERM_BROWN   },
/*  882 */     { "*", TERM_L_DARK  },
/*  883 */     { "*", TERM_L_WHITE },
/*  884 */     { "*", TERM_VIOLET  },
/*  885 */     { "*", TERM_YELLOW  },
/*  886 */     { "*", TERM_L_RED   },
/*  887 */     { "*", TERM_L_GREEN },
/*  888 */     { "*", TERM_L_BLUE  },
/*  889 */     { "*", TERM_L_BROWN }
};


cptr syllables[MAX_SYLLABLES] =
{
  "a",   "ab",  "abb", "afs", "ag",  "aks", "ala", "an",  "ankh","app",
  "arg", "arze","ash", "aus", "ban", "bac", "bam", "bar", "bat", "bek",
  "bie", "bin", "bir", "bit", "bjor","blu", "bot", "bra", "bre", "bu",
              "byt", "comp","con", "cos", "cra", "cre", "dalf","daa", "dab", "dan",
  "den", "der", "doe", "dok", "eep", "el",  "eng", "er",  "ere", "erk",
  "esh", "evs", "fa",  "faz", "fid", "flit","for", "fri", "fu",  "gan",
  "gar", "glen","glu", "gop", "gre", "gva", "ha",  "he",  "hyd", "i",
  "ing", "ion", "ip",  "ish", "it",  "ite", "iv",  "jo",  "jry", "jzu",
  "kho", "khum","kli", "klis","kvi", "la",  "lech","lri", "man", "mar",
  "me",  "mi",  "mic", "mik", "mon", "mung","mur", "naa", "nag", "nej",
  "nelg","nep", "ner", "nes", "nif", "nilp","nis", "nih", "nin", "o",
  "od",  "ood", "orff","org", "orn", "ox",  "oxy", "oz",  "pay", "pet",
  "ple", "plu", "po",  "pot", "prok","qiy", "qsu", "re",  "rea", "rhov",
  "rhuy","ri",  "rli", "ro",  "rog", "rok", "rol", "sa",  "san", "sat",
  "see", "sef", "seh", "sev", "sjii","shu", "ski", "sna", "sne", "snik",
  "sno", "snyf","so",  "sol", "sri", "sruq","sta", "sun", "ta",  "tab",
  "tchi","tem", "ther","ti",  "tich","tox", "trol","tue", "turs","u",
  "ulk", "um",  "un",  "uni", "uqq", "ur",  "uzch","val", "viv", "vly",
  "vom", "wah", "wed", "werg","wex", "whon","wun", "x",   "xaai","xch",
  "yerg","yp",  "uuy", "zun", "acha","ergo","hiji","qwet","tri", "blaa"
};

/*
 * Hold the titles of scrolls, 6 to 14 characters each
 * Also keep an array of scroll colors (always WHITE for now)
 */


/*
 * Available Options
 *
 * Option Screen Sets:
 *
 *      Set 1: User Interface
 *      Set 2: Disturbance
 *      Set 3: Inventory
 *      Set 4: Game Play
 *      Set 5: Efficiency
 *      Set 6: Special
 *
 *      Set 255: Cheating
 *
 * XXX XXX XXX Note that the "cheating options" can only be set in
 * the "do_cmd_prefs()" function, since they must set the special
 * "I am a cheater" flags.
 */
option_type options[] =
{

  /*** User-Interface - page 1 ***/

  /* variable                 default page name */
  { &rogue_like_commands,       FALSE, 1, "Rogue-like commands" },
  { &quick_messages,            TRUE,  1, "Quick messages" },
  { &other_query_flag,          FALSE, 1, "Prompt for various information" },
#ifdef MAKE_DIST
  { &display_coords,            FALSE, 1, "Display coordinates on screen" },
#else
  { &display_coords,            TRUE,  1, "Display coordinates on screen" },
#endif
  { &print_experience_advance,  TRUE,  1, "Print experience needed to advance" },
  { &auto_open,                 TRUE,  1, "Auto open doors when colliding" },
  { &use_old_target,            FALSE, 1, "Use old target by default" },
  { &always_pickup,             TRUE,  1, "Pick things up by default" },
  { &pick_up_gold,              TRUE,  1, "Auto-pick up any gold you see" },
  { &pick_up_absorbable,        TRUE,  1, "Auto-pick up any absorbable items you see"},
  { &always_repeat,             TRUE,  1, "Repeat obvious commands" },
  { &depth_in_feet,             TRUE,  1, "Show dungeon level in feet" },
  { &show_health_bar,           TRUE,  1, "Display Monster Health Bar" },
  { &ring_bell,                 FALSE, 1, "Audible bell (on errors, etc)" },
  { &use_color,                 TRUE,  1, "Use color if possible (slow)" },
  { &smooth_scroll_panels,      FALSE, 1, "Smooth scroll panels (very slow)" },
  { &number_hit_messages,       FALSE, 1, "Use numbered messages for each attack-hit-message" },
  { &show_key_help,             TRUE,  1, "Show help text when unknown command entered" },

  /*** Disturbance  - page 2 ***/

  { &find_ignore_stairs,        TRUE,  2, "Run past stairs" },
  { &find_ignore_doors,         TRUE,  2, "Run through open doors" },
  { &find_cut,                  TRUE,  2, "Run past known corners" },
  { &find_examine,              TRUE,  2, "Run into potential corners" },
  { &disturb_move,              TRUE,  2, "Disturb whenever any monster moves" },
  { &disturb_near,              TRUE,  2, "Disturb whenever viewable monster moves" },
  { &disturb_enter,             TRUE,  2, "Disturb whenever viewable monster appears" },
  { &disturb_leave,             TRUE,  2, "Disturb whenever viewable monster disappears" },
  { &disturb_panel,             TRUE,  2, "Disturb whenever map panel changes" },
  { &disturb_other,             TRUE,  2, "Disturb whenever various things happen" },
  { &alert_hitpoint,            FALSE, 2, "Alert user to critical hitpoints" },
  { &alert_failure,             FALSE, 2, "Alert user to various failures" },
  { &corpse_messages,           TRUE,  2, "Alert user to corpses decomposing" },
  { &fear_messages,             TRUE,  2, "Alert user to monster fears" },
  { &drop_messages,             TRUE,  2, "Alert user to items dropping" },
  { &ask_before_traps,          TRUE,  2, "Ask before touching a known trap" },

    /*** Inventory - page 3 ***/

  { &show_inven_weight,         TRUE,  3, "Show weights in inventory list" },
  { &show_equip_weight,         TRUE,  3, "Show weights in equipment list" },
  { &show_store_weight,         TRUE,  3, "Show weights in stores" },
  { &show_floor_weight,         TRUE,  3, "Show weights on floor" },
  { &show_equip_label,          TRUE,  3, "Show labels in equipment list" },
  { &stack_allow_items,         TRUE,  3, "Allow weapons and armor to stack" },
  { &stack_allow_corpses,       TRUE,  3, "Allow corpses to stack even if they don't have the same age" },
  { &stack_force_notes,         FALSE, 3, "Over-ride inscriptions when stacking" },
  { &stack_force_costs,         FALSE, 3, "Over-ride discounts when stacking" },
  { &no_haggle_flag,            TRUE,  3, "Disable haggling in stores" },
  { &shuffle_owners,            TRUE,  3, "Shuffle store owners" },
  { &color_known_items,         TRUE,  3, "Describe known items with their color" },
  { &carry_cursed_flag,         TRUE,  3, "Never auto-pickup known cursed items" },
  { &kill_cursed_floor,         FALSE, 3, "Kill cursed items on floor when we find them" },
  { &kill_cursed_pack,          TRUE,  3, "Kill cursed items in pack without confirmation" },
  { &pickup_add_to_ammo,        TRUE,  3, "Add picked-up items to ammo automatically"},
  { &ask_for_other_ammo,        TRUE,  3, "Ask what ammo to use before firing"},
  { &stack_ignore_logs,         TRUE,  3, "Ignore where items came from when stacking"},
  { &stacking_wipes_logs,       TRUE,  3, "Remember how stacked items were gotten"},
  { &show_spell_numbers,        FALSE, 3, "Show spell numbers in books (for macros)"},
  { &show_full_name_on_destroy, TRUE,  3, "Show the full name of any items you've destroyed"},

    /*** Game-Play - page 4 ***/

  { &view_perma_grids,          TRUE,  4, "Map remembers all perma-lit grids" },
  { &view_torch_grids,          TRUE,  4, "Map remembers all torch-lit grids" },
  { &scum_always,               FALSE, 4, "Auto-scum for good levels (always)" },
  { &scum_sometimes,            FALSE, 4, "Auto-scum for good levels (sometimes)" },
  { &scum_verygood,             FALSE, 4, "Scum for very good levels when scumming" },
  { &dungeon_align,             TRUE,  4, "Generate dungeons with aligned rooms" },
  { &dungeon_connected,         TRUE,  4, "Make sure all rooms in the dungeon are connected (slow)" },
  { &generate_large_levels,     TRUE,  4, "Generate levels up to 462 x 120. Otherwise 198 x 60" },
  { &flow_by_sound,             FALSE, 4, "Monsters chase current location (v.slow)" },
  { &flow_by_smell,             FALSE, 4, "Monsters chase recent locations (v.slow)" },
  { &track_follow,              FALSE, 4, "Monsters follow the player (beta)" },
  { &track_target,              FALSE, 4, "Monsters target the player (beta)" },
  { &smart_learn,               TRUE,  4, "Monsters learn from their mistakes" },
  { &smart_cheat,               FALSE, 4, "Monsters exploit players weaknesses" },
  { &monster_flee_exits,        TRUE,  4, "Monsters flee toward exits from a room (slow)" },
  { &monster_know_exits,        TRUE,  4, "Monsters flee toward exits they can't see" },
  { &auto_target_only_monster,  TRUE,  4, "Auto-target the only monster in sight" },
  { &create_corpses,            TRUE,  4, "Create corpses on monster death" },
  { &good_store_items,          TRUE,  4, "Use extra CPU cycles in creating store items" },

#if 0
for the future!!
  { &note_kill_unique,          TRUE,  7, "Take a note after killing a unique monster" },
  { &note_id_artifact,          TRUE,  7, "Take a note after identifying an artifact" },
  { &note_leave_level,          TRUE,  7, "Take a note after leaving a level" },
  { &note_low_hp,               TRUE,  7, "Take a note after dipping below 10% hp after level 10" },
  { &note_1000turns,            TRUE,  7, "Take a note every 1000 turns" },
  { &note_10000turns,           TRUE,  7, "Take a note every 10000 turns" },
#endif

    /*** Efficiency - page 5 ***/

  { &view_reduce_lite,          FALSE, 5, "Reduce lite-radius when running" },
  { &view_reduce_view,          FALSE, 5, "Reduce view-radius in town" },
  { &optimize_display,          FALSE, 5, "Optimize visual display" },
  { &optimize_various,          FALSE, 5, "Optimize message recall" },
  { &flush_failure,             TRUE,  5, "Flush input on various failures" },
  { &flush_disturb,             TRUE,  5, "Flush input whenever disturbed" },
  { &fresh_before,              TRUE,  5, "Flush output before every command" },
  { &fresh_after,               TRUE,  5, "Flush output after every command" },
  { &fresh_message,             TRUE,  5, "Flush output after every message" },
  { &save_messages,             TRUE,  5, "Save/append messages to disk" },
  { &compress_savefile,         TRUE,  5, "Compress saved levels on disk" },
#ifdef MAKE_DIST
  { &kill_savefile_interrupt,   TRUE,  5, "Kill savefiles on ^C interrupt" },
#else
  { &kill_savefile_interrupt,   FALSE,  5, "Kill savefiles on ^C interrupt" },
#endif
  { &remove_levelfiles,         TRUE,  5, "Remove saved levels when killed" },
  { &hilite_player,             TRUE,  5, "Hilite the player with the cursor" },
  { &view_yellow_lite,          TRUE,  5, "Draw lit terrain in yellow (slow)" },
  { &view_bright_lite,          TRUE,  5, "Draw viewable terrain brightly (slow)" },
#ifdef MAKE_DIST
  { &view_all_squares,          FALSE, 5, "Look at terrain features" },
#else
  { &view_all_squares,          TRUE,  5, "Look at terrain features" },
#endif
  { &view_granite_lite,         TRUE,  5, "Use special colors for lit wall grids (slow)" },

    /*** Special - page 6 ***/

  { &use_mirror_debug,          FALSE, 6, "Show debug messages in mirror window" },
  { &use_mirror_around,         FALSE, 6, "Show overhead map in mirror window" },
  { &use_mirror_recent,         FALSE, 6, "Show monster info in mirror window" },
  { &use_mirror_normal,         FALSE, 6, "Show inven/equip in mirror window" },
  { &use_mirror_choose,         FALSE, 6, "Show item choices in mirror window" },
  { &use_mirror_spells,         FALSE, 6, "Show spell choices in mirror window" },
  { &use_recall_recent,         TRUE,  6, "Show monster info in recall window" },
  { &use_choice_normal,         TRUE,  6, "Show inven/equip in choice window" },
  { &use_choice_choose,         TRUE,  6, "Show item choices in choice window" },
  { &use_choice_spells,         TRUE,  6, "Show spell choices in choice window" },
  { &show_choose_prompt,        FALSE, 6, "Show prompt in choice window" },
  { &show_choose_info,          FALSE, 6, "Show info in choice window" },
  { &show_choose_label,         FALSE, 6, "Show labels in choice window" },
  { &show_choose_weight,        FALSE, 6, "Show weights in choice window" },
  { &recall_show_desc,          TRUE,  6, "Show descriptions in recall window" },
  { &recall_show_kill,          TRUE,  6, "Show kill counts in recall window" },
  { &show_unkilled,             FALSE, 6, "Show unkilled monsters in kill list" },

    /*** Cheating - page 0 ***/
  { &cheat_peek,                FALSE, 0, "Peek into object creation" },
  { &cheat_hear,                FALSE, 0, "Peek into monster creation" },
  { &cheat_room,                FALSE, 0, "Peek into dungeon creation" },
  { &cheat_xtra,                FALSE, 0, "Peek into other things" },
  { &cheat_know,                FALSE, 0, "Know complete monster info" },
  { &cheat_live,                FALSE, 0, "Allow player to be healed in town on death" },
  { &cheat_mode,                FALSE, 0, "Allow player to see info about un-id'ed items in mode screens" },
  { &cheat_spell_info,          FALSE, 0, "Allow player to view spell info" },
#ifdef MAKE_DIST
  { &cheat_hitpoints,           FALSE, 0, "Allow player to have unlimited hitpoints" },
#else
  { &cheat_hitpoints,           TRUE,  0, "Allow player to have unlimited hitpoints" },
#endif
  { &cheat_numeric_skills,      FALSE, 0, "Allow player to see numeric skill-values" },

    /*** End of Table ***/

    { NULL,                     FALSE, 0, NULL }
};


