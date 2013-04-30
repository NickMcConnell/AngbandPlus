/*
 * File: calcs.c
 * Purpose: Player status calculation
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "../s-angband.h"
#include "../../common/tvalsval.h"
#include "../monster/mon-msg.h"
#include "../monster/mon-spell.h"
#include "../monster/mon-util.h"
#include "../netserver.h"
#include "../object/pval.h"
#include "../s-spells.h"
#include "../squelch.h"


/*
 * Stat Table (CHR) -- payment percentages
 */
const byte adj_chr_gold[STAT_RANGE] =
{
    143 /* 3 */,
    137 /* 4 */,
    134 /* 5 */,
    132 /* 6 */,
    129 /* 7 */,
    127 /* 8 */,
    123 /* 9 */,
    122 /* 10 */,
    121 /* 11 */,
    118 /* 12 */,
    116 /* 13 */,
    113 /* 14 */,
    113 /* 15 */,
    112 /* 16 */,
    111 /* 17 */,
    110 /* 18/00-18/09 */,
    108 /* 18/10-18/19 */,
    107 /* 18/20-18/29 */,
    106 /* 18/30-18/39 */,
    105 /* 18/40-18/49 */,
    104 /* 18/50-18/59 */,
    103 /* 18/60-18/69 */,
    102 /* 18/70-18/79 */,
    101 /* 18/80-18/89 */,
    100 /* 18/90-18/99 */,
    99  /* 18/100-18/109 */,
    97  /* 18/110-18/119 */,
    96  /* 18/120-18/129 */,
    95  /* 18/130-18/139 */,
    94  /* 18/140-18/149 */,
    93  /* 18/150-18/159 */,
    92  /* 18/160-18/169 */,
    91  /* 18/170-18/179 */,
    90  /* 18/180-18/189 */,
    90  /* 18/190-18/199 */,
    90  /* 18/200-18/209 */,
    90  /* 18/210-18/219 */,
    90  /* 18/220+ */
};


/*
 * Stat Table (INT) -- Magic devices
 */
static const byte adj_int_dev[STAT_RANGE] =
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
    6   /* 18/100-18/109 */,
    7   /* 18/110-18/119 */,
    7   /* 18/120-18/129 */,
    8   /* 18/130-18/139 */,
    8   /* 18/140-18/149 */,
    9   /* 18/150-18/159 */,
    9   /* 18/160-18/169 */,
    10  /* 18/170-18/179 */,
    10  /* 18/180-18/189 */,
    11  /* 18/190-18/199 */,
    11  /* 18/200-18/209 */,
    12  /* 18/210-18/219 */,
    13  /* 18/220+ */
};


/*
 * Stat Table (WIS) -- Saving throw
 */
static const byte adj_wis_sav[STAT_RANGE] =
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
static const byte adj_dex_dis[STAT_RANGE] =
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
static const byte adj_int_dis[STAT_RANGE] =
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
static const byte adj_dex_ta[STAT_RANGE] =
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
static const byte adj_str_td[STAT_RANGE] =
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
static const byte adj_dex_th[STAT_RANGE] =
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
static const byte adj_str_th[STAT_RANGE] =
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
static const byte adj_str_wgt[STAT_RANGE] =
{
    5   /* 3 */,
    6   /* 4 */,
    7   /* 5 */,
    8   /* 6 */,
    9   /* 7 */,
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
const byte adj_str_hold[STAT_RANGE] =
{
    4   /* 3 */,
    5   /* 4 */,
    6   /* 5 */,
    7   /* 6 */,
    8   /* 7 */,
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
    100 /* 18/200-18/209 */,
    100 /* 18/210-18/219 */,
    100 /* 18/220+ */
};


/*
 * Stat Table (STR) -- digging value
 */
static const byte adj_str_dig[STAT_RANGE] =
{
    0   /* 3 */,
    0   /* 4 */,
    1   /* 5 */,
    2   /* 6 */,
    3   /* 7 */,
    4   /* 8 */,
    4   /* 9 */,
    5   /* 10 */,
    5   /* 11 */,
    6   /* 12 */,
    6   /* 13 */,
    7   /* 14 */,
    7   /* 15 */,
    8   /* 16 */,
    8   /* 17 */,
    9   /* 18/00-18/09 */,
    10  /* 18/10-18/19 */,
    12  /* 18/20-18/29 */,
    15  /* 18/30-18/39 */,
    20  /* 18/40-18/49 */,
    25  /* 18/50-18/59 */,
    30  /* 18/60-18/69 */,
    35  /* 18/70-18/79 */,
    40  /* 18/80-18/89 */,
    45  /* 18/90-18/99 */,
    50  /* 18/100-18/109 */,
    55  /* 18/110-18/119 */,
    60  /* 18/120-18/129 */,
    65  /* 18/130-18/139 */,
    70  /* 18/140-18/149 */,
    75  /* 18/150-18/159 */,
    80  /* 18/160-18/169 */,
    85  /* 18/170-18/179 */,
    90  /* 18/180-18/189 */,
    95  /* 18/190-18/199 */,
    100 /* 18/200-18/209 */,
    100 /* 18/210-18/219 */,
    100 /* 18/220+ */
};


/*
 * Stat Table (STR) -- help index into the "blow" table
 */
const byte adj_str_blow[STAT_RANGE] =
{
    3   /* 3 */,
    4   /* 4 */,
    5   /* 5 */,
    6   /* 6 */,
    7   /* 7 */,
    8   /* 8 */,
    9   /* 9 */,
    10  /* 10 */,
    11  /* 11 */,
    12  /* 12 */,
    13  /* 13 */,
    14  /* 14 */,
    15  /* 15 */,
    16  /* 16 */,
    17  /* 17 */,
    20  /* 18/00-18/09 */,
    30  /* 18/10-18/19 */,
    40  /* 18/20-18/29 */,
    50  /* 18/30-18/39 */,
    60  /* 18/40-18/49 */,
    70  /* 18/50-18/59 */,
    80  /* 18/60-18/69 */,
    90  /* 18/70-18/79 */,
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
static const byte adj_dex_blow[STAT_RANGE] =
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
    2   /* 17 */,
    2   /* 18/00-18/09 */,
    2   /* 18/10-18/19 */,
    3   /* 18/20-18/29 */,
    3   /* 18/30-18/39 */,
    4   /* 18/40-18/49 */,
    4   /* 18/50-18/59 */,
    5   /* 18/60-18/69 */,
    5   /* 18/70-18/79 */,
    6   /* 18/80-18/89 */,
    6   /* 18/90-18/99 */,
    7   /* 18/100-18/109 */,
    7   /* 18/110-18/119 */,
    8   /* 18/120-18/129 */,
    8   /* 18/130-18/139 */,
    8   /* 18/140-18/149 */,
    9   /* 18/150-18/159 */,
    9   /* 18/160-18/169 */,
    9   /* 18/170-18/179 */,
    10  /* 18/180-18/189 */,
    10  /* 18/190-18/199 */,
    11  /* 18/200-18/209 */,
    11  /* 18/210-18/219 */,
    11  /* 18/220+ */
};


/*
 * Stat Table (DEX) -- chance of avoiding "theft" and "falling"
 */
const byte adj_dex_safe[STAT_RANGE] =
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
const byte adj_con_fix[STAT_RANGE] =
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
 * Stat Table (CON) -- extra 1/100th hitpoints per level
 */
static const int adj_con_mhp[STAT_RANGE] =
{
    -250    /* 3 */,
    -150    /* 4 */,
    -100    /* 5 */,
     -75    /* 6 */,
     -50    /* 7 */,
     -25    /* 8 */,
     -10    /* 9 */,
      -5    /* 10 */,
       0    /* 11 */,
       5    /* 12 */,
      10    /* 13 */,
      25    /* 14 */,
      50    /* 15 */,
      75    /* 16 */,
     100    /* 17 */,
     150    /* 18/00-18/09 */,
     175    /* 18/10-18/19 */,
     200    /* 18/20-18/29 */,
     225    /* 18/30-18/39 */,
     250    /* 18/40-18/49 */,
     275    /* 18/50-18/59 */,
     300    /* 18/60-18/69 */,
     350    /* 18/70-18/79 */,
     400    /* 18/80-18/89 */,
     450    /* 18/90-18/99 */,
     500    /* 18/100-18/109 */,
     550    /* 18/110-18/119 */,
     600    /* 18/120-18/129 */,
     650    /* 18/130-18/139 */,
     700    /* 18/140-18/149 */,
     750    /* 18/150-18/159 */,
     800    /* 18/160-18/169 */,
     900    /* 18/170-18/179 */,
    1000    /* 18/180-18/189 */,
    1100    /* 18/190-18/199 */,
    1250    /* 18/200-18/209 */,
    1250    /* 18/210-18/219 */,
    1250    /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- Number of half-spells per level
 */
static const int adj_mag_study[] =
{
      0 /* 3 */,
      0 /* 4 */,
     10 /* 5 */,
     20 /* 6 */,
     30 /* 7 */,
     40 /* 8 */,
     50 /* 9 */,
     60 /* 10 */,
     70 /* 11 */,
     80 /* 12 */,
     85 /* 13 */,
     90 /* 14 */,
     95 /* 15 */,
    100 /* 16 */,
    105 /* 17 */,
    110 /* 18/00-18/09 */,
    115 /* 18/10-18/19 */,
    120 /* 18/20-18/29 */,
    130 /* 18/30-18/39 */,
    140 /* 18/40-18/49 */,
    150 /* 18/50-18/59 */,
    160 /* 18/60-18/69 */,
    170 /* 18/70-18/79 */,
    180 /* 18/80-18/89 */,
    190 /* 18/90-18/99 */,
    200 /* 18/100-18/109 */,
    210 /* 18/110-18/119 */,
    220 /* 18/120-18/129 */,
    230 /* 18/130-18/139 */,
    240 /* 18/140-18/149 */,
    250 /* 18/150-18/159 */,
    250 /* 18/160-18/169 */,
    250 /* 18/170-18/179 */,
    250 /* 18/180-18/189 */,
    250 /* 18/190-18/199 */,
    250 /* 18/200-18/209 */,
    250 /* 18/210-18/219 */,
    250 /* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- extra half-mana-points per level
 */
static const int adj_mag_mana[] =
{
      0 /* 3 */,
     10 /* 4 */,
     20 /* 5 */,
     30 /* 6 */,
     40 /* 7 */,
     50 /* 8 */,
     60 /* 9 */,
     70 /* 10 */,
     80 /* 11 */,
     90 /* 12 */,
    100 /* 13 */,
    110 /* 14 */,
    120 /* 15 */,
    130 /* 16 */,
    140 /* 17 */,
    150 /* 18/00-18/09 */,
    160 /* 18/10-18/19 */,
    170 /* 18/20-18/29 */,
    180 /* 18/30-18/39 */,
    190 /* 18/40-18/49 */,
    200 /* 18/50-18/59 */,
    225 /* 18/60-18/69 */,
    250 /* 18/70-18/79 */,
    300 /* 18/80-18/89 */,
    350 /* 18/90-18/99 */,
    400 /* 18/100-18/109 */,
    450 /* 18/110-18/119 */,
    500 /* 18/120-18/129 */,
    550 /* 18/130-18/139 */,
    600 /* 18/140-18/149 */,
    650 /* 18/150-18/159 */,
    700 /* 18/160-18/169 */,
    750 /* 18/170-18/179 */,
    800 /* 18/180-18/189 */,
    800 /* 18/190-18/199 */,
    800 /* 18/200-18/209 */,
    800 /* 18/210-18/219 */,
    800 /* 18/220+ */
};


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypassed until the character is created.
 */
static void calc_spells(struct player *p)
{
    int i, j, k, levels;
    int num_allowed, num_known, num_forgotten;
    int percent_spells;
    const magic_type *s_ptr;
    s16b old_spells;
    const char *pp = ((p->clazz->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

    /* Hack -- must be literate */
    if (!p->clazz->spell_book) return;

    /* Save the new_spells value */
    old_spells = p->new_spells;

    /* Determine the number of spells allowed */
    levels = p->lev - p->clazz->spell_first + 1;

    /* Hack -- no negative spells */
    if (levels < 0) levels = 0;

    /* Number of 1/100 spells per level */
    percent_spells = adj_mag_study[p->state.stat_ind[p->clazz->spell_stat]];

    /* Extract total allowed spells (rounded up) */
    num_allowed = (((percent_spells * levels) + 50) / 100);

    /* Assume none known */
    num_known = 0;
    num_forgotten = 0;

    /* Count the number of spells we know */
    for (j = 0; j < PY_MAX_SPELLS; j++)
    {
        /* Count known spells */
        if (p->spell_flags[j] & PY_SPELL_LEARNED) num_known++;
        if (p->spell_flags[j] & PY_SPELL_FORGOTTEN) num_forgotten++;
    }

    /* See how many spells we must forget or may learn */
    p->new_spells = num_allowed - num_known;

    /* Forget spells which are too hard */
    for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
    {
        /* Efficiency -- all done */
        if (num_known == 0) break;

        /* Access the spell */
        j = p->spell_order[i];

        /* Skip non-spells */
        if (j >= 99) continue;

        /* Get the spell */
        s_ptr = &p->clazz->spells.info[j];

        /* Skip spells we are allowed to know */
        if (s_ptr->slevel <= p->lev) continue;

        /* Is it known? */
        if (p->spell_flags[j] & PY_SPELL_LEARNED)
        {
            /* Mark as forgotten */
            p->spell_flags[j] |= PY_SPELL_FORGOTTEN;

            /* No longer known */
            p->spell_flags[j] &= ~PY_SPELL_LEARNED;

            /* Message */
            msg(p, "You have forgotten the %s of %s.", pp,
                get_spell_name(p->clazz->spell_book, j));

            /* One more can be learned */
            p->new_spells++;
            num_known--;
            num_forgotten++;
        }
    }

    /* Forget spells if we know too many spells */
    for (i = PY_MAX_SPELLS; i >= 0; i--)
    {
        /* Stop when possible */
        if (p->new_spells >= 0) break;

        /* Efficiency -- all done */
        if (num_known == 0) break;

        /* Get the (i+1)th spell learned */
        j = p->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) continue;

        /* Forget it (if learned) */
        if (p->spell_flags[j] & PY_SPELL_LEARNED)
        {
            /* Mark as forgotten */
            p->spell_flags[j] |= PY_SPELL_FORGOTTEN;

            /* No longer known */
            p->spell_flags[j] &= ~PY_SPELL_LEARNED;

            /* Message */
            msg(p, "You have forgotten the %s of %s.", pp,
                get_spell_name(p->clazz->spell_book, j));

            /* One more can be learned */
            p->new_spells++;
            num_known--;
            num_forgotten++;
        }
    }

    /* Check for spells to remember */
    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        /* None left to remember */
        if (p->new_spells <= 0) break;

        /* Efficiency -- all done */
        if (num_forgotten == 0) break;

        /* Get the next spell we learned */
        j = p->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) break;

        /* Access the spell */
        s_ptr = &p->clazz->spells.info[j];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p->lev) continue;

        /* First set of spells */
        if (p->spell_flags[j] & PY_SPELL_FORGOTTEN)
        {
            /* No longer forgotten */
            p->spell_flags[j] &= ~PY_SPELL_FORGOTTEN;

            /* Known once more */
            p->spell_flags[j] |= PY_SPELL_LEARNED;

            /* Message */
            msg(p, "You have remembered the %s of %s.", pp,
                get_spell_name(p->clazz->spell_book, j));

            /* One less can be learned */
            p->new_spells--;
            num_forgotten--;
        }
    }      

    /* Assume no spells available */
    k = 0;

    /* Count spells that can be learned */
    for (j = 0; j < PY_MAX_SPELLS; j++)
    {
        /* Access the spell */
        s_ptr = &p->clazz->spells.info[j];

        /* Skip spells we cannot remember */
        if ((s_ptr->slevel > p->lev) || (s_ptr->slevel == 0)) continue;

        /* Skip spells we already know */
        if (p->spell_flags[j] & PY_SPELL_LEARNED) continue;

        /* Count it */
        k++;
    }

    /* Cannot learn more spells than exist */
    if (p->new_spells > k) p->new_spells = k;

    /* Hack -- wait for creation */
    if (!p->alive) return;

    /* Hack -- delay messages after character creation */
    if (p->delayed_display)
    {
        /* Message if needed */
        if (p->new_spells)
        {
            /* Message */
            msg(p, "You can learn %d more %s%s.", p->new_spells, pp, PLURAL(p->new_spells));
        }

        /* Redraw Study Status */
        p->redraw |= (PR_STUDY);

        return;
    }

    /* Spell count changed */
    if (old_spells != p->new_spells)
    {
        /* Message if needed */
        if (p->new_spells)
        {
            /* Message */
            msg(p, "You can learn %d more %s%s.", p->new_spells, pp, PLURAL(p->new_spells));
        }

        /* Redraw Study Status */
        p->redraw |= (PR_STUDY);
    }
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(struct player *p)
{
    int msp, levels, cur_wgt, max_wgt;
    object_type *o_ptr;
    bool old_cumber_glove = p->cumber_glove;
    bool old_cumber_armor = p->cumber_armor;
    bitflag f[OF_SIZE];

    /* Shapechangers get arbitrary mana */
    if (player_has(p, PF_MONSTER_SPELLS))
    {
        /* Arbitrary value (should be enough) */
        msp = 2 * p->lev;
    }

    /* Hack -- Must be literate */
    else if (!p->clazz->spell_book)
    {
        p->msp = 0;
        p->csp = 0;
        p->csp_frac = 0;
        return;
    }

    /* Extract "effective" player level */
    else
    {
        levels = (p->lev - p->clazz->spell_first) + 1;
        if (levels > 0)
        {
            msp = 1;
            msp += adj_mag_mana[p->state.stat_ind[p->clazz->spell_stat]] * levels / 100;
        }
        else
        {
            levels = 0;
            msp = 0;
        }
    }

    /* Get the gloves */
    o_ptr = &p->inventory[INVEN_HANDS];

    /* Examine the gloves */
    object_flags(o_ptr, f);

    /* Process gloves for those disturbed by them */
    if (player_has(p, PF_CUMBER_GLOVE))
    {
        /* Assume player is not encumbered by gloves */
        p->cumber_glove = FALSE;

        /* Normal gloves hurt mage-type spells */
        if (o_ptr->kind && !of_has(f, OF_FREE_ACT) && !of_has(f, OF_SPELLS_OK) &&
            !(of_has(f, OF_DEX) && (o_ptr->pval[which_pval(o_ptr, OF_DEX)] > 0)))
        {
            /* Encumbered */
            p->cumber_glove = TRUE;

            /* Reduce mana */
            msp = (3 * msp) / 4;
        }
    }

    /* Assume player not encumbered by armor */
    p->cumber_armor = FALSE;

    /* Weigh the armor */
    cur_wgt = 0;
    cur_wgt += p->inventory[INVEN_BODY].weight;
    cur_wgt += p->inventory[INVEN_HEAD].weight;
    cur_wgt += p->inventory[INVEN_ARM].weight;
    cur_wgt += p->inventory[INVEN_OUTER].weight;
    cur_wgt += p->inventory[INVEN_HANDS].weight;
    cur_wgt += p->inventory[INVEN_FEET].weight;

    /* Determine the weight allowance */
    max_wgt = p->clazz->spell_weight;

    /* Heavy armor penalizes mana */
    if (((cur_wgt - max_wgt) / 10) > 0)
    {
        /* Encumbered */
        p->cumber_armor = TRUE;

        /* Reduce mana */
        msp -= ((cur_wgt - max_wgt) / 10);
    }

    /* Sorcerors and elementalists get extra mana */
    if (player_has(p, PF_EXTRA_MANA)) msp = (5 * msp) / 4;

    /* Extra mana capacity */
    if (of_has(f, OF_MANA))
    {
        /* 1 pval = 10% more mana */
        msp = ((10 + o_ptr->pval[which_pval(o_ptr, OF_MANA)]) * msp) / 10;
    }

    /* Meditation increase mana at the cost of hp */
    if (p->timed[TMD_MEDITATE]) msp = (3 * msp) / 2;

    /* Mana can never be negative */
    if (msp < 0) msp = 0;

    /* Maximum mana has changed */
    if (p->msp != msp)
    {
        int old_num = get_player_num(p);

        /* Player has no mana now */
        if (!msp) player_clear_timed(p, TMD_MANASHIELD, TRUE);

        /* Save new limit */
        p->msp = msp;

        /* Enforce new limit */
        if (p->csp >= msp)
        {
            p->csp = msp;
            p->csp_frac = 0;
        }

        /* Hack -- Redraw picture */
        redraw_picture(p, old_num);

        /* Display mana later */
        p->redraw |= (PR_MANA);
    }

    /* Hack -- wait for creation */
    if (!p->alive) return;

    /* Hack -- delay messages after character creation */
    if (p->delayed_display)
    {
        /* Message */
        if (p->cumber_glove)
            msg(p, "Your covered hands feel unsuitable for spellcasting.");
        if (p->cumber_armor)
            msg(p, "The weight of your armor encumbers your movement.");

        return;
    }

    /* Take note when "glove state" changes */
    if (old_cumber_glove != p->cumber_glove)
    {
        /* Message */
        if (p->cumber_glove)
            msg(p, "Your covered hands feel unsuitable for spellcasting.");
        else
            msg(p, "Your hands feel more suitable for spellcasting.");
    }

    /* Take note when "armor state" changes */
    if (old_cumber_armor != p->cumber_armor)
    {
        /* Message */
        if (p->cumber_armor)
            msg(p, "The weight of your armor encumbers your movement.");
        else
            msg(p, "You feel able to move more freely.");
    }
}


/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(struct player *p)
{
    long bonus;
    int mhp;

    /* Get "1/100th hitpoint bonus per level" value */
    bonus = adj_con_mhp[p->state.stat_ind[A_CON]];

    /* Calculate hitpoints */
    mhp = p->player_hp[p->lev - 1] + (bonus * p->lev / 100);

    /* Always have at least one hitpoint per level */
    if (mhp < p->lev + 1) mhp = p->lev + 1;

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        monster_race *r_ptr = &r_info[p->r_idx];

        mhp = mhp * 3 / 5 + (1500 * r_ptr->avg_hp) / (r_ptr->avg_hp + 4500);
    }

    /* Meditation increase mana at the cost of hp */
    if (p->timed[TMD_MEDITATE]) mhp = mhp * 3 / 5;

    /* New maximum hitpoints */
    if (p->mhp != mhp)
    {
        int old_num = get_player_num(p);

        /* Save new limit */
        p->mhp = mhp;

        /* Enforce new limit */
        if (p->chp >= mhp)
        {
            p->chp = mhp;
            p->chp_frac = 0;
        }

        /* Hack -- Redraw picture */
        redraw_picture(p, old_num);

        /* Display hitpoints (later) */
        p->redraw |= (PR_HP);
    }
}


/*
 * Calculate and set the current light radius.
 *
 * Note that a cursed light source no longer emits light.
 */
static void calc_torch(struct player *p)
{
    int i;
    s16b old_light = p->cur_light;
    s16b new_light = 0;

    /* Ascertain lightness if outside of the dungeon */
    if ((p->depth <= 0) && is_daytime())
    {
        /* Notice changes in the "light radius" */
        if (old_light)
        {
            /* Update the visuals */
            p->cur_light = 0;
            p->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
        }

        return;
    }

    /* Examine all wielded objects */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        bitflag f[OF_SIZE];
        int amt = 0;
        object_type *o_ptr = &p->inventory[i];

        /* Skip empty slots */
        if (!o_ptr->kind) continue;

        /* Extract the flags */
        object_flags(o_ptr, f);

        /* Light radius is now a pval */
        if (of_has(f, OF_LIGHT)) amt = o_ptr->pval[which_pval(o_ptr, OF_LIGHT)];

        /* Cursed objects emit no light */
        if (of_has(f, OF_LIGHT_CURSE)) amt = 0;

        /* Lights without fuel provide no light */
        if ((o_ptr->tval == TV_LIGHT) && !of_has(f, OF_NO_FUEL) && (o_ptr->timeout == 0)) amt = 0;

        /* Alter p_ptr->cur_light if reasonable */
        new_light += amt;
    }

    /* Limit light */
    new_light = MIN(new_light, 5);
    new_light = MAX(new_light, 0);

    /* Notice changes in the "light radius" */
    if (old_light != new_light)
    {
        /* Update the visuals */
        p->cur_light = new_light;
        p->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
    }
}


/*
 * This table is used to help calculate the number of blows the player can
 * make in a single round of attacks (one player turn) with a normal weapon.
 *
 * This number ranges from a single blow/round for weak players to up to six
 * blows/round for powerful warriors.
 *
 * Note that certain artifacts and ego-items give "bonus" blows/round.
 *
 * First, from the player class, we extract some values:
 *
 *   Warrior     --> num = 6; mul = 5; div = MAX(30, weapon_weight);
 *   Mage        --> num = 4; mul = 2; div = MAX(40, weapon_weight);
 *   Priest      --> num = 4; mul = 3; div = MAX(35, weapon_weight);
 *   Rogue       --> num = 5; mul = 4; div = MAX(30, weapon_weight);
 *   Ranger      --> num = 5; mul = 4; div = MAX(35, weapon_weight);
 *   Paladin     --> num = 5; mul = 5; div = MAX(30, weapon_weight);
 *   Sorceror    --> num = 1; mul = 2; div = MAX(40, weapon_weight);
 *   Unbeliever  --> num = 6; mul = 5; div = MAX(30, weapon_weight);
 *   Archer      --> num = 2; mul = 3; div = MAX(35, weapon_weight);
 *   Monk        --> irrelevant (barehanded damage)
 *   Telepath    --> num = 4; mul = 3; div = MAX(35, weapon_weight);
 *   Necromancer --> num = 4; mul = 2; div = MAX(40, weapon_weight);
 *   Shapechangr --> num = 5; mul = 4; div = MAX(35, weapon_weight);
 *   Elemntalist --> num = 3; mul = 2; div = MAX(40, weapon_weight);
 *
 * To get "P", we look up the relevant "adj_str_blow[]" (see above),
 * multiply it by "mul", and then divide it by "div", rounding down.
 *
 * To get "D", we look up the relevant "adj_dex_blow[]" (see above).
 *
 * Then we look up the energy cost of each blow using "blows_table[P][D]".
 * The player gets blows/round equal to 100/this number, up to a maximum of
 * "num" blows/round, plus any "bonus" blows/round.
 */
static const byte blows_table[12][12] =
{
    /* P/D */
    /* 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11+ */

    /* 0  */
    {  100, 100, 95,  85,  75,  60,  50,  42,  35,  30,  25,  23 },

    /* 1  */
    {  100, 95,  85,  75,  60,  50,  42,  35,  30,  25,  23,  21 },

    /* 2  */
    {  95,  85,  75,  60,  50,  42,  35,  30,  26,  23,  21,  20 },

    /* 3  */
    {  85,  75,  60,  50,  42,  36,  32,  28,  25,  22,  20,  19 },

    /* 4  */
    {  75,  60,  50,  42,  36,  33,  28,  25,  23,  21,  19,  18 },

    /* 5  */
    {  60,  50,  42,  36,  33,  30,  27,  24,  22,  21,  19,  17 },

    /* 6  */
    {  50,  42,  36,  33,  30,  27,  25,  23,  21,  20,  18,  17 },

    /* 7  */
    {  42,  36,  33,  30,  28,  26,  24,  22,  20,  19,  18,  17 },

    /* 8  */
    {  36,  33,  30,  28,  26,  24,  22,  21,  20,  19,  17,  16 },

    /* 9  */
    {  35,  32,  29,  26,  24,  22,  21,  20,  19,  18,  17,  16 },

    /* 10 */
    {  34,  30,  27,  25,  23,  22,  21,  20,  19,  18,  17,  16 },

    /* 11+ */
    {  33,  29,  26,  24,  22,  21,  20,  19,  18,  17,  16,  15 }
};


/*
 * Calculate the blows a player would get, wielding a weapon of specified weight.
 *
 * Note: state->num_blows is now 100x the number of blows
 */
static int calc_blows(struct player *p, s16b weight, player_state *state)
{
    int blows;
    int str_index, dex_index;
    int div;
    int blow_energy;

    /* Enforce a minimum "weight" (tenth pounds) */
    div = ((weight < p->clazz->min_weight)? p->clazz->min_weight: weight);

    /* Get the strength vs weight */
    str_index = (adj_str_blow[state->stat_ind[A_STR]] * p->clazz->att_multiply / div);

    /* Maximal value */
    if (str_index > 11) str_index = 11;

    /* Index by dexterity */
    dex_index = MIN(adj_dex_blow[state->stat_ind[A_DEX]], 11);

    /* Use the blows table to get energy per blow */
    blow_energy = blows_table[str_index][dex_index];

    blows = MIN(10000 / blow_energy, 100 * p->clazz->max_attacks);

    /* Require at least one blow */
    return MAX(blows, 100);
}


/*
 * Computes current weight limit.
 */
int weight_limit(player_state *state)
{
    int i;

    /* Weight limit based only on strength */
    i = adj_str_wgt[state->stat_ind[A_STR]] * 100;

    /* Return the result */
    return (i);
}


/*
 * Computes weight remaining before burdened.
 */
int weight_remaining(struct player *p)
{
    int i;

    /* Weight limit based only on strength */
    i = 60 * adj_str_wgt[p->state.stat_ind[A_STR]] - p->total_weight - 1;

    /* Return the result */
    return (i);
}


static int getAvgDam(monster_race *r_ptr)
{
    int m, tot = 0;

    for (m = 0; m < MONSTER_BLOW_MAX; m++)
    {
        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;

        /* Extract the attack info */
        tot += r_ptr->blow[m].d_dice * (r_ptr->blow[m].d_side + 1);
    }

    return (tot / (2 * MONSTER_BLOW_MAX));
}


/*
 * Computes extra ac for monks wearing very light or no armour at all.
 *
 * o_ptr -- the armor part to check
 * bonus -- ac bonus for this armor part when wearing no armor
 * k_min -- threshold for light armor (half bonus)
 * k_max -- threshold for heavy armor (no bonus)
 * level -- player level
 */
static int monk_get_extra_ac(const object_type *o_ptr, int bonus, const object_kind *k_min,
    const object_kind *k_max, int level)
{
    int min, max, extra_ac = bonus * level / 50;

    /* No armor: full bonus */
    if (!o_ptr->kind) return extra_ac;

    /* No capacity: no bonus */
    if (!k_min || !k_max) return 0;

    min = k_min->weight;
    max = k_min->weight + (k_max->weight - k_min->weight) * level / 50;

    /* Light armor: half bonus */
    if (o_ptr->weight <= min) return extra_ac / 2;

    /* Heavy armor: no bonus */
    if ((min >= max) || (o_ptr->weight >= max)) return 0;

    /* Partial bonus */
    return (extra_ac * (max - o_ptr->weight) / (max - min) / 2);
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * If id_only is true, calc_bonuses() will only use the known
 * information of objects; thus it returns what the player _knows_
 * the character state to be.
 */
void calc_bonuses(struct player *p, object_type inventory[], player_state *state, bool id_only)
{
    int i, j, hold;
    int extra_blows = 0;
    int extra_shots = 0;
    int extra_might = 0;
    object_type *o_ptr;
    bitflag f[OF_SIZE], f2[OF_SIZE];
    bitflag collect_f[OF_SIZE];
    bool unencumbered_monk = monk_armor_ok(p, inventory);
    byte cumber_shield = 0;
    monster_race *r_ptr = NULL;

    create_mask(f2, FALSE, OFT_ESP, OFT_MAX);

    if (p->r_idx) r_ptr = &r_info[p->r_idx];

    /*** Reset ***/

    /* Set various defaults */
    state->speed = 110;
    state->num_blows = 100;
    state->num_shots = 1;
    state->ammo_tval = TV_ROCK;
    state->ammo_mult = 1;

    /*** Extract race/class info ***/

    /* Base infravision (purely racial) */
    state->see_infra = p->race->infra;

    /* Base skills */
    for (i = 0; i < SKILL_MAX; i++)
        state->skills[i] = p->race->r_skills[i] + p->clazz->c_skills[i];

    /*** Analyze player ***/

    /* Extract the player flags */
    player_flags(p, collect_f, inventory);

    /* Ent: penalty to speed (polymorphed Ents get only half penalty) */
    if (player_has(p, PF_GIANT))
    {
        if (p->r_idx)
            state->speed -= 1;
        else
            state->speed -= 2;
    }

    /* Unencumbered monks get nice abilities */
    if (unencumbered_monk)
    {
        /* Faster every 10 levels */
        state->speed += (p->lev) / 10;
    }

    /* Ghost */
    if (p->ghost) state->see_infra += 3;

    /* Rogues get speed bonus */
    if (player_has(p, PF_SPEED_BONUS) && (p->lev >= 5))
        state->speed += (((p->lev - 5) / 15) + 1);

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        state->to_a += r_ptr->ac / 2;
        state->dis_to_a += r_ptr->ac / 2;
        state->to_d += getAvgDam(r_ptr);
        state->dis_to_d += getAvgDam(r_ptr);
        state->speed += (r_ptr->speed - 110) / 2;

        /* Fruit bat mode: get double speed bonus */
        if (OPT_P(p, birth_fruit_bat)) state->speed += (r_ptr->speed - 110) / 2;
    }

    /*** Analyze equipment ***/

    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->kind) continue;

        /* Extract the item flags */
        if (id_only)
            object_flags_known(o_ptr, f, object_flavor_is_aware(p, o_ptr));
        else
            object_flags(o_ptr, f);

        of_union(collect_f, f);

        /* Affect stats */
        if (of_has(f, OF_STR))
            state->stat_add[A_STR] += o_ptr->pval[which_pval(o_ptr, OF_STR)];
        if (of_has(f, OF_INT))
            state->stat_add[A_INT] += o_ptr->pval[which_pval(o_ptr, OF_INT)];
        if (of_has(f, OF_WIS))
            state->stat_add[A_WIS] += o_ptr->pval[which_pval(o_ptr, OF_WIS)];
        if (of_has(f, OF_DEX))
            state->stat_add[A_DEX] += o_ptr->pval[which_pval(o_ptr, OF_DEX)];
        if (of_has(f, OF_CON))
            state->stat_add[A_CON] += o_ptr->pval[which_pval(o_ptr, OF_CON)];
        if (of_has(f, OF_CHR))
            state->stat_add[A_CHR] += o_ptr->pval[which_pval(o_ptr, OF_CHR)];

        /* Affect stealth */
        if (of_has(f, OF_STEALTH))
            state->skills[SKILL_STEALTH] += o_ptr->pval[which_pval(o_ptr, OF_STEALTH)];

        /* Affect searching ability (factor of five) */
        if (of_has(f, OF_SEARCH))
            state->skills[SKILL_SEARCH] += o_ptr->pval[which_pval(o_ptr, OF_SEARCH)] * 5;

        /* Affect searching frequency (factor of five) */
        if (of_has(f, OF_SEARCH))
            state->skills[SKILL_SEARCH_FREQUENCY] += o_ptr->pval[which_pval(o_ptr, OF_SEARCH)] * 5;

        /* Affect infravision */
        if (of_has(f, OF_INFRA))
            state->see_infra += o_ptr->pval[which_pval(o_ptr, OF_INFRA)];

        /* Affect digging (factor of 20) */
        if (of_has(f, OF_TUNNEL))
            state->skills[SKILL_DIGGING] += o_ptr->pval[which_pval(o_ptr, OF_TUNNEL)] * 20;

        /* Affect speed */
        if (of_has(f, OF_SPEED))
            state->speed += o_ptr->pval[which_pval(o_ptr, OF_SPEED)];

        /* Affect blows */
        if (of_has(f, OF_BLOWS))
            extra_blows += o_ptr->pval[which_pval(o_ptr, OF_BLOWS)];

        /* Affect shots */
        if (of_has(f, OF_SHOTS))
            extra_shots += o_ptr->pval[which_pval(o_ptr, OF_SHOTS)];

        /* Affect Might */
        if (of_has(f, OF_MIGHT))
            extra_might += o_ptr->pval[which_pval(o_ptr, OF_MIGHT)];

        /* Shield encumberance */
        if (of_has(f, OF_TWO_HANDED)) cumber_shield++;
        if ((i == INVEN_ARM) && cumber_shield) cumber_shield++;

        /* Modify the base armor class */
        state->ac += o_ptr->ac;

        /* The base armor class is always known */
        state->dis_ac += o_ptr->ac;

        /* Apply the bonuses to armor class */
        if (!id_only || object_is_known(p, o_ptr))
            state->to_a += o_ptr->to_a;

        /* Apply the mental bonuses to armor class, if known */
        if (object_defence_plusses_are_visible(p, o_ptr))
            state->dis_to_a += o_ptr->to_a;

        /* Hack -- do not apply "weapon" bonuses */
        if (i == INVEN_WIELD) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (i == INVEN_BOW) continue;

        /* Apply the bonuses to hit/damage */
        if (!id_only || object_is_known(p, o_ptr))
        {
            state->to_h += o_ptr->to_h;
            state->to_d += o_ptr->to_d;

            /* Unencumbered monks get double bonuses from gloves */
            if (unencumbered_monk && (i == INVEN_HANDS))
            {
                state->to_h += o_ptr->to_h;
                state->to_d += o_ptr->to_d;
            }
        }

        /* Apply the mental bonuses to hit/damage, if known */
        if (object_attack_plusses_are_visible(p, o_ptr))
        {
            state->dis_to_h += o_ptr->to_h;
            state->dis_to_d += o_ptr->to_d;

            /* Unencumbered monks get double bonuses from gloves */
            if (unencumbered_monk && (i == INVEN_HANDS))
            {
                state->dis_to_h += o_ptr->to_h;
                state->dis_to_d += o_ptr->to_d;
            }
        }
    }

    /*** Update all flags ***/

    for (i = 0; i < OF_MAX; i++)
    {
        if (of_has(collect_f, i)) of_on(state->flags, i);
    }

    /* ESP flags */
    if (check_state_aux(p, state->flags, OF_ESP_ALL))
    {
        of_diff(state->flags, f2);
        of_on(state->flags, OF_ESP_ALL);
    }

    /*** Handle stats ***/

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        if (rf_has(r_ptr->flags, RF_STUPID)) state->stat_add[A_INT] -= 2;
        if (rf_has(r_ptr->flags, RF_SMART)) state->stat_add[A_INT] += 2;
        if (r_ptr->freq_spell == 33) state->stat_add[A_INT] += 1;
        if (r_ptr->freq_spell == 50) state->stat_add[A_INT] += 3;
        if (r_ptr->freq_spell == 100) state->stat_add[A_INT] += 5;
    }

    /* Adrenaline effects (part 1) */
    if (p->timed[TMD_ADRENALINE])
    {
        s16b fx = (p->timed[TMD_ADRENALINE] - 1) / 20;

        /* Increase strength, dexterity, constitution */
        state->stat_add[A_STR] += fx;
        state->stat_add[A_DEX] += (fx + 1) / 2;
        state->stat_add[A_CON] += fx;
    }

    /* Elemental harmony effects (part 1) */
    if (p->timed[TMD_HARMONY])
    {
        s16b fx = (p->timed[TMD_HARMONY] - 1) / 20;

        /* Increase strength, dexterity, constitution */
        state->stat_add[A_STR] += fx;
        state->stat_add[A_DEX] += (fx + 1) / 2;
        state->stat_add[A_CON] += fx;
    }

    /* Calculate stats */
    for (i = 0; i < A_MAX; i++)
    {
        int add, top, use, ind, r_adj;

        /* Extract modifier */
        add = state->stat_add[i];

        /* Polymorphed players only get half adjustment from race */
        r_adj = p->race->r_adj[i];
        if (p->r_idx)
        {
            if (r_adj > 0) r_adj = (r_adj + 1) / 2;
            else if (r_adj < 0) r_adj = (r_adj - 1) / 2;
        }

        /* Modify the stats for race and class */
        add += (r_adj + p->clazz->c_adj[i]);

        /* Extract the new "stat_top" value for the stat */
        top = modify_stat_value(p->stat_max[i], add);

        /* Save the new value */
        state->stat_top[i] = top;

        /* Extract the new "stat_use" value for the stat */
        use = modify_stat_value(p->stat_cur[i], add);

        /* Save the new value */
        state->stat_use[i] = use;

        /* Values: n/a */
        if (use <= 3) ind = 0;

        /* Values: 3, 4, ..., 17 */
        else if (use <= 18) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (37);

        my_assert((0 <= ind) && (ind < STAT_RANGE));

        /* Save the new index */
        state->stat_ind[i] = ind;
    }

    /* Unencumbered monks get extra ac for wearing very light or no armour at all */
    if (unencumbered_monk)
    {
        object_kind *k_min, *k_max;
        int extra_ac;

        /* Soft armor */
        k_min = lookup_kind(TV_SOFT_ARMOR, SV_ROBE);
        k_max = lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL);
        extra_ac = monk_get_extra_ac(&inventory[INVEN_BODY], 54, k_min, k_max, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;

        /* Cloaks */
        k_max = lookup_kind(TV_CLOAK, SV_FUR_CLOAK);
        extra_ac = monk_get_extra_ac(&inventory[INVEN_OUTER], 12, k_max, k_max, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;

        /* No bonus for wearing a shield */
        extra_ac = monk_get_extra_ac(&inventory[INVEN_ARM], 12, NULL, NULL, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;

        /* Caps and crowns */
        k_max = lookup_kind(TV_CROWN, SV_JEWEL_ENCRUSTED_CROWN);
        extra_ac = monk_get_extra_ac(&inventory[INVEN_HEAD], 12, k_max, k_max, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;

        /* Gloves */
        k_max = lookup_kind(TV_GLOVES, SV_SET_OF_CAESTUS);
        extra_ac = monk_get_extra_ac(&inventory[INVEN_HANDS], 18, k_max, k_max, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;

        /* Leather boots */
        k_max = lookup_kind(TV_BOOTS, SV_PAIR_OF_LEATHER_BOOTS);
        extra_ac = monk_get_extra_ac(&inventory[INVEN_FEET], 12, k_max, k_max, p->lev);
        state->to_a += extra_ac;
        state->dis_to_a += extra_ac;
    }

    /*** Temporary flags ***/

    /* Apply temporary "stun" */
    if (p->timed[TMD_STUN] > 50)
    {
        state->to_h -= 20;
        state->dis_to_h -= 20;
        state->to_d -= 20;
        state->dis_to_d -= 20;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 8 / 10;
    }
    else if (p->timed[TMD_STUN])
    {
        state->to_h -= 5;
        state->dis_to_h -= 5;
        state->to_d -= 5;
        state->dis_to_d -= 5;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 9 / 10;
    }

    /* Adrenaline effects (part 2) */
    if (p->timed[TMD_ADRENALINE])
    {
        s16b fx = (p->timed[TMD_ADRENALINE] - 1) / 20;

        if (fx >= 2)
        {
            state->to_d += 8;
            state->dis_to_d += 8;
        }
        if (fx >= 3) extra_blows++;
        if (fx == 4) state->speed += 10;
    }

    /* Elemental harmony effects (part 2) */
    if (p->timed[TMD_HARMONY])
    {
        s16b fx = (p->timed[TMD_ADRENALINE] - 1) / 20;

        if (fx >= 2)
        {
            state->to_a += 50;
            state->dis_to_a += 50;
        }
        if (fx >= 3)
        {
            p->timed[TMD_OPP_ACID] = -1;
            p->timed[TMD_OPP_ELEC] = -1;
            p->timed[TMD_OPP_FIRE] = -1;
            p->timed[TMD_OPP_COLD] = -1;
            p->timed[TMD_OPP_POIS] = -1;
        }
        else
        {
            player_clear_timed(p, TMD_OPP_ACID, TRUE);
            player_clear_timed(p, TMD_OPP_ELEC, TRUE);
            player_clear_timed(p, TMD_OPP_FIRE, TRUE);
            player_clear_timed(p, TMD_OPP_COLD, TRUE);
            player_clear_timed(p, TMD_OPP_POIS, TRUE);
        }
        if (fx == 4) state->speed += 10;
    }

    /* Invulnerability */
    if (p->timed[TMD_INVULN])
    {
        state->to_a += 100;
        state->dis_to_a += 100;
    }

    /* Temporary blessing */
    if (p->timed[TMD_BLESSED])
    {
        state->to_a += 5;
        state->dis_to_a += 5;
        state->to_h += 10;
        state->dis_to_h += 10;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 105 / 100;
    }

    /* Temporary shield */
    if (p->timed[TMD_SHIELD])
    {
        state->to_a += 50;
        state->dis_to_a += 50;
    }

    /* Temporary stoneskin */
    if (p->timed[TMD_STONESKIN])
    {
        state->to_a += 40;
        state->dis_to_a += 40;
        state->speed -= 5;
    }

    /* Temporary "Hero" */
    if (p->timed[TMD_HERO])
    {
        state->to_h += 12;
        state->dis_to_h += 12;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 105 / 100;
    }

    /* Temporary "Berserk" */
    if (p->timed[TMD_SHERO])
    {
        state->to_h += 24;
        state->dis_to_h += 24;
        state->to_a -= 10;
        state->dis_to_a -= 10;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 9 / 10;
    }

    /* Temporary "fast" */
    if (p->timed[TMD_FAST] || p->timed[TMD_SPRINT]) state->speed += 10;

    /* Temporary "slow" */
    if (p->timed[TMD_SLOW]) state->speed -= 10;

    /* Temporary infravision boost */
    if (p->timed[TMD_SINFRA]) state->see_infra += 5;

    /* Temporary fear */
    if (p->timed[TMD_TERROR] > p->timed[TMD_AFRAID])
        p->timed[TMD_AFRAID] = p->timed[TMD_TERROR];
    if (p->timed[TMD_TERROR]) state->speed += 5;

    /* Fear can come from item flags too */
    if (check_state_aux(p, state->flags, OF_AFRAID))
    {
        state->to_h -= 20;
        state->dis_to_h -= 20;
        state->to_a += 8;
        state->dis_to_a += 8;
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 95 / 100;
    }

    /* Confusion */
    if (p->timed[TMD_CONFUSED])
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 75 / 100;

    /* Amnesia */
    if (p->timed[TMD_AMNESIA])
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 8 / 10;

    /* Poison */
    if (p->timed[TMD_POISONED])
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 95 / 100;

    /* Hallucination */
    if (p->timed[TMD_IMAGE])
        state->skills[SKILL_DEVICE] = state->skills[SKILL_DEVICE] * 8 / 10;

    /*** Analyze weight ***/

    /* Extract the current weight (in tenth pounds) */
    j = p->total_weight;

    /* Cap the weight */
    if (j > (1 << 14)) j = (1 << 14);

    /* Extract the "weight limit" (in tenth pounds) */
    i = weight_limit(state);

    /* Apply "encumbrance" from weight */
    if (j > i / 2) state->speed -= ((j - (i / 2)) / (i / 10));

    /* Bloating slows the player down (a little) */
    if (p->food >= PY_FOOD_MAX) state->speed -= 10;

    /* Searching slows the player down */
    if (p->searching)
    {
        state->speed -= 10;

        /* Adding "stealth mode" for rogues */
        if (player_has(p, PF_STEALTH_MODE))
            state->skills[SKILL_STEALTH] *= 3;
    }

    /* Sanity check on extreme speeds */
    if (state->speed < 0) state->speed = 0;
    if (state->speed > 199) state->speed = 199;

    /*** Apply modifier bonuses ***/

    /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
    state->to_a += ((int)(adj_dex_ta[state->stat_ind[A_DEX]]) - 128);
    state->to_d += ((int)(adj_str_td[state->stat_ind[A_STR]]) - 128);
    state->to_h += ((int)(adj_dex_th[state->stat_ind[A_DEX]]) - 128);
    state->to_h += ((int)(adj_str_th[state->stat_ind[A_STR]]) - 128);

    /* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
    state->dis_to_a += ((int)(adj_dex_ta[state->stat_ind[A_DEX]]) - 128);
    state->dis_to_d += ((int)(adj_str_td[state->stat_ind[A_STR]]) - 128);
    state->dis_to_h += ((int)(adj_dex_th[state->stat_ind[A_DEX]]) - 128);
    state->dis_to_h += ((int)(adj_str_th[state->stat_ind[A_STR]]) - 128);

    /*** Modify skills ***/

    /* Affect Skill -- disarming (DEX and INT) */
    state->skills[SKILL_DISARM] += adj_dex_dis[state->stat_ind[A_DEX]];
    state->skills[SKILL_DISARM] += adj_int_dis[state->stat_ind[A_INT]];

    /* Affect Skill -- magic devices (INT) */
    state->skills[SKILL_DEVICE] += adj_int_dev[state->stat_ind[A_INT]];

    /* Affect Skill -- saving throw (WIS) */
    state->skills[SKILL_SAVE] += adj_wis_sav[state->stat_ind[A_WIS]];

    /* Affect Skill -- digging (STR) */
    state->skills[SKILL_DIGGING] += adj_str_dig[state->stat_ind[A_STR]];

    /* Handle polymorphed players */
    if (p->r_idx && rf_has(r_ptr->flags, RF_KILL_WALL))
        state->skills[SKILL_DIGGING] = 2000;

    /* Elementalists get instant tunnel at level 50 */
    if (player_has(p, PF_ELEMENTAL_SPELLS) && (p->lev == 50))
        state->skills[SKILL_DIGGING] = 2000;

    /* Affect Skills (Level, by Class) */
    for (i = 0; i < SKILL_MAX; i++)
        state->skills[i] += (p->clazz->x_skills[i] * p->lev / 10);

    /* Monks get stealth bonus */
    if (player_has(p, PF_MARTIAL_ARTS) && !check_state_aux(p, state->flags, OF_AGGRAVATE))
        state->skills[SKILL_STEALTH] += (p->lev / 10);

    /* Handle polymorphed players */
    if (p->r_idx)
    {
        if (r_ptr->extra <= 50) state->skills[SKILL_STEALTH] += 2;
        else if (r_ptr->extra <= 100) state->skills[SKILL_STEALTH] += 1;
        else if (r_ptr->extra <= 150) state->skills[SKILL_STEALTH] += 0;
        else if (r_ptr->extra <= 450) state->skills[SKILL_STEALTH] -= 1;
        else if (r_ptr->extra <= 2000) state->skills[SKILL_STEALTH] -= 2;
        else if (r_ptr->extra <= 10000) state->skills[SKILL_STEALTH] -= 3;
        else state->skills[SKILL_STEALTH] -= 4;
    }

    /* Limit Skill -- digging from 1 up */
    if (state->skills[SKILL_DIGGING] < 1) state->skills[SKILL_DIGGING] = 1;

    /* Limit Skill -- stealth from 0 to 30 */
    if (state->skills[SKILL_STEALTH] > 30) state->skills[SKILL_STEALTH] = 30;
    if (state->skills[SKILL_STEALTH] < 0) state->skills[SKILL_STEALTH] = 0;

    /* Apply Skill -- Extract noise from stealth */
    state->noise = (1L << (30 - state->skills[SKILL_STEALTH]));

    /* Obtain the "hold" value */
    hold = adj_str_hold[state->stat_ind[A_STR]];

    /*** Analyze current bow ***/

    /* Examine the "current bow" */
    o_ptr = &inventory[INVEN_BOW];

    /* Assume not heavy */
    state->heavy_shoot = FALSE;

    /* It is hard to hold a heavy bow */
    if (hold < o_ptr->weight / 10)
    {
        /* Hard to wield a heavy bow */
        state->to_h += 2 * (hold - o_ptr->weight / 10);
        state->dis_to_h += 2 * (hold - o_ptr->weight / 10);

        /* Heavy Bow */
        state->heavy_shoot = TRUE;
    }

    /* Analyze launcher */
    if (o_ptr->kind)
    {
        /* Analyze the launcher */
        switch (o_ptr->sval)
        {
            /* Sling and ammo */
            case SV_SLING:
            {
                state->ammo_tval = TV_SHOT;
                state->ammo_mult = 2;
                break;
            }

            /* Short Bow and Arrow */
            case SV_SHORT_BOW:
            {
                state->ammo_tval = TV_ARROW;
                state->ammo_mult = 2;
                break;
            }

            /* Long Bow and Arrow */
            case SV_LONG_BOW:
            {
                state->ammo_tval = TV_ARROW;
                state->ammo_mult = 3;
                break;
            }

            /* Light Crossbow and Bolt */
            case SV_LIGHT_XBOW:
            {
                state->ammo_tval = TV_BOLT;
                state->ammo_mult = 3;
                break;
            }

            /* Heavy Crossbow and Bolt */
            case SV_HEAVY_XBOW:
            {
                state->ammo_tval = TV_BOLT;
                state->ammo_mult = 4;
                break;
            }
        }

        /* Apply special flags */
        if (!state->heavy_shoot)
        {
            /* Extra shots */
            state->num_shots += extra_shots;

            /* Extra might */
            state->ammo_mult += extra_might;

            /* Hack -- Rangers love Bows */
            if (player_has(p, PF_EXTRA_SHOT) && (state->ammo_tval == TV_ARROW))
            {
                /* Extra shot at level 20 */
                if (p->lev >= 20) state->num_shots++;

                /* Extra shot at level 40 */
                if (p->lev >= 40) state->num_shots++;
            }

            /* Archers are powerful with missile launchers */
            if (player_has(p, PF_EXTRA_SHOTS))
            {
                if (state->ammo_tval == TV_SHOT)
                {
                    /* Up to 5 extra shots with a sling */
                    state->num_shots += p->lev * 5 / 50;
                }
                else if (state->ammo_tval == TV_ARROW)
                {
                    /* Up to 4 extra shots with a bow */
                    state->num_shots += p->lev * 4 / 50;
                }
                else if (state->ammo_tval == TV_BOLT)
                {
                    /* Up to 3 extra shots with a crossbow */
                    state->num_shots += p->lev * 3 / 50;
                }
            }
        }
    }

    /* Monks are good rock throwers */
    if (player_has(p, PF_MARTIAL_ARTS) && (state->ammo_tval == TV_ROCK))
    {
        /* Extra shot at levels 15, 30 and 45 */
        state->num_shots += p->lev / 15;
    }

    /* Archers are great rock throwers */
    if (player_has(p, PF_EXTRA_SHOTS) && (state->ammo_tval == TV_ROCK))
    {
        /* Up to 6 extra shots when throwing rocks */
        state->num_shots += p->lev * 6 / 50;
    }

    /* Handle polymorphed players */
    if (p->r_idx && (rsf_has(r_ptr->spell_flags, RSF_ARROW_X) ||
        rsf_has(r_ptr->spell_flags, RSF_ARROW_1) || rsf_has(r_ptr->spell_flags, RSF_ARROW_2) ||
        rsf_has(r_ptr->spell_flags, RSF_ARROW_3) || rsf_has(r_ptr->spell_flags, RSF_ARROW_4)))
    {
        state->num_shots++;
    }

    /* Temporary "Farsight" */
    if (p->timed[TMD_FARSIGHT])
    {
        int bonus = (p->lev - 7) / 10;

        state->to_h += bonus;
        state->dis_to_h += bonus;
        state->see_infra += bonus;
    }
    if (p->timed[TMD_ZFARSIGHT])
        state->see_infra += p->lev / 4;

    /*** Analyze weapon ***/

    /* Examine the "main weapon" */
    o_ptr = &inventory[INVEN_WIELD];

    /* Assume not heavy */
    state->heavy_wield = FALSE;

    /* It is hard to hold a heavy weapon */
    if (hold < o_ptr->weight / 10)
    {
        /* Hard to wield a heavy weapon */
        state->to_h += 2 * (hold - o_ptr->weight / 10);
        state->dis_to_h += 2 * (hold - o_ptr->weight / 10);

        /* Heavy weapon */
        state->heavy_wield = TRUE;
    }

    /* Non-object means barehanded attacks */
    if (!o_ptr->kind) {my_assert(o_ptr->weight == 0);}

    /* Monks get special barehanded attacks */
    if (player_has(p, PF_MARTIAL_ARTS))
    {
        /* Up to 7 extra blows */
        extra_blows = (p->lev - 1) * 100 / 7 + 100 * extra_blows;

        /* Encumbered monks only get half the extra blows */
        if (!unencumbered_monk) extra_blows /= 2;

        /* Add in the "bonus blows" */
        state->num_blows += extra_blows;

        /* Unencumbered monks get a bonus tohit/todam */
        if (unencumbered_monk)
        {
            state->to_h += p->lev * 2 / 5;
            state->to_d += p->lev * 2 / 5;

            state->dis_to_h += p->lev * 2 / 5;
            state->dis_to_d += p->lev * 2 / 5;
        }
    }

    /* Normal weapons */
    else if (!state->heavy_wield)
    {
        object_type *o_ptr2 = &inventory[INVEN_TOOL];

        /* Calculate number of blows */
        state->num_blows = calc_blows(p, o_ptr->weight, state) + 100 * extra_blows;

        /* Boost digging skill by weapon weight if no digger equipped */
        if (!o_ptr2->kind || (o_ptr2->tval != TV_DIGGING))
            state->skills[SKILL_DIGGING] += (o_ptr->weight / 10);
    }

    /* Assume okay */
    state->icky_wield = FALSE;

    /* Priest weapon penalty for non-blessed edged weapons */
    if (player_has(p, PF_BLESS_WEAPON) && !check_state_aux(p, state->flags, OF_BLESSED) &&
        ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
    {
        /* Reduce the real bonuses */
        /* Really reduce them (a penalty of -2 is a joke...) */
        if (state->to_h > 0) state->to_h = 3 * state->to_h / 5;
        state->to_h -= 2;
        if (state->to_d > 0) state->to_d = 3 * state->to_d / 5;
        state->to_d -= 2;

        /* Reduce the mental bonuses */
        if (state->dis_to_h > 0) state->dis_to_h = 3 * state->dis_to_h / 5;
        state->dis_to_h -= 2;
        if (state->dis_to_d > 0) state->dis_to_d = 3 * state->dis_to_d / 5;
        state->dis_to_d -= 2;

        /* Icky weapon */
        state->icky_wield = TRUE;
    }

    /* Assume no shield encumberance */
    state->cumber_shield = FALSE;

    /* It is hard to wield a two-handed weapon with a shield */
    if (cumber_shield == 2)
    {
        /* Hard to wield a two-handed weapon with a shield */
        if (state->to_h > 0) state->to_h = 2 * state->to_h / 3;
        state->to_h -= 2;
        if (state->dis_to_h > 0) state->dis_to_h = 2 * state->dis_to_h / 3;
        state->dis_to_h -= 2;

        /* Shield encumberance */
        state->cumber_shield = TRUE;
    }

    /* Boost digging skill by digger weight */
    o_ptr = &inventory[INVEN_TOOL];
    if (o_ptr->kind && (o_ptr->tval == TV_DIGGING))
        state->skills[SKILL_DIGGING] += (o_ptr->weight / 10);
}


/*
 * Calculate bonuses, and print various things on changes
 */
static void update_bonuses(struct player *p)
{
    int i, flag;
    player_state *state = &p->state;
    player_state old = p->state;
    int old_show_tofhit, show_tofhit;
    int old_show_tomhit, show_tomhit;
    int old_show_toshit, show_toshit;
    int old_show_tofdam, show_tofdam;
    int old_show_tomdam, show_tomdam;
    int old_show_tosdam, show_tosdam;
    bitflag f[OF_SIZE];

    /* Save the old hit/damage bonuses */
    get_plusses(p, &old_show_tofhit, &old_show_tofdam, &old_show_tomhit, &old_show_tomdam,
        &old_show_toshit, &old_show_tosdam);

    /*** Calculate bonuses ***/

    WIPE(state, player_state);
    calc_bonuses(p, p->inventory, state, FALSE);

    /*** Notice changes ***/

    /* Analyze stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Notice changes */
        if (state->stat_top[i] != old.stat_top[i])
        {
            /* Redisplay the stats later */
            p->redraw |= (PR_STATS);
        }

        /* Notice changes */
        if (state->stat_use[i] != old.stat_use[i])
        {
            /* Redisplay the stats later */
            p->redraw |= (PR_STATS);
        }

        /* Notice changes */
        if (state->stat_ind[i] != old.stat_ind[i])
        {
            /* Change in CON affects Hitpoints */
            if (i == A_CON) p->update |= (PU_HP);

            /* Change in INT may affect Mana/Spells */
            else if (i == A_INT)
            {
                if (p->clazz->spell_stat == A_INT)
                {
                    p->update |= (PU_MANA | PU_SPELLS);

                    /* Redraw */
                    p->redraw |= PR_SPELL;
                }
            }

            /* Change in WIS may affect Mana/Spells */
            else if (i == A_WIS)
            {
                if (p->clazz->spell_stat == A_WIS)
                {
                    p->update |= (PU_MANA | PU_SPELLS);

                    /* Redraw */
                    p->redraw |= PR_SPELL;
                }
            }
        }
    }

    /* Hack -- Telepathy Change */
    create_mask(f, FALSE, OFT_ESP, OFT_MAX);
    for (flag = of_next(f, FLAG_START); flag != FLAG_END; flag = of_next(f, flag + 1))
    {
        if (check_state_aux(p, state->flags, flag) != check_state_aux(p, old.flags, flag))
            p->update |= (PU_MONSTERS);
    }

    /* Hack -- See Invis Change */
    if (check_state_aux(p, state->flags, OF_SEE_INVIS) != check_state_aux(p, old.flags, OF_SEE_INVIS))
        p->update |= (PU_MONSTERS);

    /* Redraw speed (if needed) */
    if (state->speed != old.speed) p->redraw |= (PR_SPEED);

    /* Redraw armor (if needed) */
    if ((state->dis_ac != old.dis_ac) || (state->dis_to_a != old.dis_to_a))
    {
        /* Redraw */
        p->redraw |= (PR_ARMOR);
    }

    /* Redraw plusses to hit/damage if necessary */
    get_plusses(p, &show_tofhit, &show_tofdam, &show_tomhit, &show_tomdam, &show_toshit,
        &show_tosdam);
    if ((show_tofhit != old_show_tofhit) || (show_tofdam != old_show_tofdam) ||
        (show_tomhit != old_show_tomhit) || (show_tomdam != old_show_tomdam) ||
        (show_toshit != old_show_toshit) || (show_tosdam != old_show_tosdam))
    {
        /* Redraw plusses */
        p->redraw |= (PR_PLUSSES);
    }

    /* Hack -- wait for creation */
    if (!p->alive) return;

    /* Send skills and weight */
    Send_skills(p);
    Send_weight(p, p->total_weight, weight_remaining(p));

    /* Hack -- delay messages after character creation */
    if (p->delayed_display)
    {
        /* Message */
        if (state->heavy_shoot)
            msg(p, "You have trouble wielding such a heavy bow.");
        if (state->heavy_wield)
            msg(p, "You have trouble wielding such a heavy weapon.");
        if (state->icky_wield)
            msg(p, "You do not feel comfortable with your weapon.");
        if (state->cumber_shield)
            msg(p, "You have trouble wielding your weapon with a shield.");

        return;
    }

    /* Take note when "heavy bow" changes */
    if (old.heavy_shoot != state->heavy_shoot)
    {
        /* Message */
        if (state->heavy_shoot)
            msg(p, "You have trouble wielding such a heavy bow.");
        else if (p->inventory[INVEN_BOW].kind)
            msg(p, "You have no trouble wielding your bow.");
        else
            msg(p, "You feel relieved to put down your heavy bow.");
    }

    /* Take note when "heavy weapon" changes */
    if (old.heavy_wield != state->heavy_wield)
    {
        /* Message */
        if (state->heavy_wield)
            msg(p, "You have trouble wielding such a heavy weapon.");
        else if (p->inventory[INVEN_WIELD].kind)
            msg(p, "You have no trouble wielding your weapon.");
        else
            msg(p, "You feel relieved to put down your heavy weapon.");
    }

    /* Take note when "illegal weapon" changes */
    if (old.icky_wield != state->icky_wield)
    {
        /* Message */
        if (state->icky_wield)
            msg(p, "You do not feel comfortable with your weapon.");
        else if (p->inventory[INVEN_WIELD].kind)
            msg(p, "You feel comfortable with your weapon.");
        else
            msg(p, "You feel more comfortable after removing your weapon.");
    }

    /* Take note when "shield encumberance" changes */
    if (old.cumber_shield != state->cumber_shield)
    {
        /* Message */
        if (state->cumber_shield)
            msg(p, "You have trouble wielding your weapon with a shield.");
        else if (p->inventory[INVEN_WIELD].kind)
            msg(p, "You have no trouble wielding your weapon.");
        else
            msg(p, "You feel more comfortable after removing your weapon.");
    }
}


/*** Generic "deal with" functions ***/


/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(struct player *p)
{
    /* Nothing to do */
    if (!p->notice) return;
    if (p->notice & PN_WAIT) return;

    /* Deal with squelch stuff */
    if (p->notice & PN_SQUELCH)
    {
        p->notice &= ~(PN_SQUELCH);
        squelch_drop(p);
    }

    /* Combine the pack */
    if (p->notice & PN_COMBINE)
    {
        p->notice &= ~(PN_COMBINE);
        combine_pack(p);
    }

    /* Reorder the pack */
    if (p->notice & PN_REORDER)
    {
        p->notice &= ~(PN_REORDER);
        reorder_pack(p);
    }

    /* Sort the quiver */
    if (p->notice & PN_SORT_QUIVER)
    {
        p->notice &= ~(PN_SORT_QUIVER);
        sort_quiver(p);
    }

    /* Dump the monster messages */
    if (p->notice & PN_MON_MESSAGE)
    {
        p->notice &= ~(PN_MON_MESSAGE);

        /* Make sure this comes after all of the monster messages */
        if (p->size_mon_msg > 0) flush_all_monster_messages(p);
    }
}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(struct player *p)
{
    /* Nothing to do */
    if (!p->update) return;

    if (p->update & PU_BONUS)
    {
        p->update &= ~(PU_BONUS);
        update_bonuses(p);
    }

    if (p->update & PU_TORCH)
    {
        p->update &= ~(PU_TORCH);
        calc_torch(p);
    }

    if (p->update & PU_HP)
    {
        p->update &= ~(PU_HP);
        calc_hitpoints(p);
    }

    if (p->update & PU_MANA)
    {
        p->update &= ~(PU_MANA);
        calc_mana(p);
    }

    if (p->update & PU_SPELLS)
    {
        p->update &= ~(PU_SPELLS);
        calc_spells(p);
    }

    /* Character is not ready yet, no screen updates */
    if (!p->alive) return;

    if (p->update & PU_FORGET_VIEW)
    {
        p->update &= ~(PU_FORGET_VIEW);
        forget_view(p);
    }

    if (p->update & PU_UPDATE_VIEW)
    {
        p->update &= ~(PU_UPDATE_VIEW);
        update_view(p);
    }

    if (p->update & PU_FORGET_FLOW)
    {
        p->update &= ~(PU_FORGET_FLOW);
        cave_forget_flow(p);
    }

    if (p->update & PU_UPDATE_FLOW)
    {
        p->update &= ~(PU_UPDATE_FLOW);
        cave_update_flow(p, cave_get(p->depth));
    }

    if (p->update & PU_DISTANCE)
    {
        p->update &= ~(PU_DISTANCE);
        p->update &= ~(PU_MONSTERS);
        update_monsters(p->depth, TRUE);
        update_players();
    }

    if (p->update & PU_MONSTERS)
    {
        p->update &= ~(PU_MONSTERS);
        update_monsters(p->depth, FALSE);
        update_players();
    }
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw"
 */
void handle_stuff(struct player *p)
{
    /* Hack -- Delay updating */
    if (p->new_level_flag) return;

    update_stuff(p);
    redraw_stuff(p);
}


/*
 * Handle "p_ptr->notice", "p_ptr->update" and "p_ptr->redraw"
 */
void refresh_stuff(int Ind)
{
    player_type *p_ptr = player_get(Ind);

    /* Hack -- delay updating */
    if (p_ptr->new_level_flag) return;

    /* Notice stuff */
    notice_stuff(p_ptr);

    /* Handle stuff */
    handle_stuff(p_ptr);
}


/* Monks cannot use heavy armor */
bool monk_armor_ok(struct player *p, object_type *inven)
{
    u16b monk_arm_wgt = 0;

    if (!player_has(p, PF_MARTIAL_ARTS)) return FALSE;

    /* Weight the armor */
    monk_arm_wgt += inven[INVEN_BODY].weight;
    monk_arm_wgt += inven[INVEN_HEAD].weight;
    monk_arm_wgt += inven[INVEN_ARM].weight;
    monk_arm_wgt += inven[INVEN_OUTER].weight;
    monk_arm_wgt += inven[INVEN_HANDS].weight;
    monk_arm_wgt += inven[INVEN_FEET].weight;

    /* Little bonus for kings because of the crown (20 lbs) */
    if (p->total_winner) return (monk_arm_wgt <= 350);

    return (monk_arm_wgt <= (100 + (p->lev * 4)));
}
