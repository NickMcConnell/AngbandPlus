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




/*
 * Global array for looping through the "keypad directions"
 */
s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/*
 * Global arrays for converting "keypad direction" into offsets
 */
s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 */
s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };



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
 *    Warrior --> num = 6; mul = 5; div = MAX(30, weapon_weight);
 *    Mage    --> num = 4; mul = 2; div = MAX(40, weapon_weight);
 *    Priest  --> num = 5; mul = 3; div = MAX(35, weapon_weight);
 *    Rogue   --> num = 5; mul = 3; div = MAX(30, weapon_weight);
 *    Ranger  --> num = 5; mul = 4; div = MAX(35, weapon_weight);
 *    Paladin --> num = 5; mul = 4; div = MAX(30, weapon_weight);
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
	/* P/D */
	/* 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11+ */

	/* 0  */
	{  1,   1,   1,   1,   1,   1,   2,   2,   2,   2,   2,   3 },

	/* 1  */
	{  1,   1,   1,   1,   2,   2,   3,   3,   3,   4,   4,   4 },

	/* 2  */
	{  1,   1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5 },

	/* 3  */
	{  1,   2,   2,   3,   3,   4,   4,   4,   5,   5,   5,   5 },

	/* 4  */
	{  1,   2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5 },

	/* 5  */
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 6  */
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 7  */
	{  2,   3,   3,   4,   4,   4,   5,   5,   5,   5,   5,   6 },

	/* 8  */
	{  3,   3,   3,   4,   4,   4,   5,   5,   5,   5,   6,   6 },

	/* 9  */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   6,   6 },

	/* 10 */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   6,   6,   6 },

	/* 11+ */
	{  3,   3,   4,   4,   4,   4,   5,   5,   6,   6,   6,   6 },
};


s16b arena_monsters[MAX_ARENA_MONS] =
{
	 30,	 43,	102,	118,	126,	149,	173,
	183,	188,	191,	216,	230,	238,	244,
	255,	262,	293,	297,	321,	349,	372,
	401,	415,	454,	464,	485,	538,	631,
	641
};



/*
 * Store owners (exactly four "possible" owners per store, chosen randomly)
 * { name, purse, max greed, min greed, haggle_per, tolerance, race, unused }
 */
owner_type owners[MAX_STORES][MAX_OWNERS] =
{
	{
		/* General store */
                { "Shopkeeper",       300,    170, 108,  5, 15, RACE_HUMAN},
		{ "Shopkeeper",       300,    175, 108,  4, 12, RACE_HUMAN},
		{ "Shopkeeper",       300,    170, 107,  5, 15, RACE_HALF_ELF},
		{ "Shopkeeper",       300,    165, 107,  6, 18, RACE_HUMAN},
	},
	{
		/* Armoury */
                { "Shopkeeper",           30000,   210, 115,  5,  7, RACE_HUMAN},
                { "Shopkeeper",          30000,  190, 111,  4,  9, RACE_DEVLING},
                { "Shopkeeper",        30000,  200, 112,  4, 10, RACE_HUMAN},
		{ "Shopkeeper",          30000,  200, 112,  4,  5, RACE_HUMAN},
	},
	{
		/* Weapon Smith */
                { "Arnold the Beastly",      30000,   210, 115,  6,  6, RACE_BARBARIAN},
                { "Arndal Beast-Slayer",     30000,  185, 110,  5,  9, RACE_HALF_ELF},
                { "Eddie Beast-Master",      30000,  190, 115,  5,  7, RACE_HALF_ORC},
                { "Oglign Dragon-Slayer",    30000,  195, 112,  4,  8, RACE_DWARF},
	},
	{
		/* Temple */
                { "Ludwig the Humble",          30000,   175, 109,  6, 15, RACE_DWARF},
                { "Gunnar the Paladin",         30000,  185, 110,  5, 23, RACE_HALF_TROLL},
		{ "Torin the Chosen",           30000,  180, 107,  6, 20, RACE_HIGH_ELF},
		{ "Sarastro the Wise",          30000,  185, 109,  5, 15, RACE_HUMAN},
	},
	{
		/* Alchemist */
		{ "Mauser the Chemist",         30000,  190, 111,  5,  8, RACE_HALF_ELF},
		{ "Wizzle the Chaotic",         30000,  190, 110,  6,  8, RACE_HOBBIT},
		{ "Midas the Greedy",           30000,  200, 116,  6,  9, RACE_GNOME},
		{ "Ja-Far the Alchemist",       30000,  220, 111,  4,  9, RACE_ELF},
	},
	{
		/* Magic Shop */
                { "Durg the Sorcerer",        30000,  200, 110,  7,  8, RACE_HALF_ELF},
		{ "Buggerby the Great",		30000,	215, 113,  6, 10, RACE_GNOME},
		{ "Inglorian the Mage",		30000,	200, 110,  7, 10, RACE_HUMAN},
		{ "Luthien Starshine",		30000,	175, 110,  5, 11, RACE_HIGH_ELF},
	},
	{
		/* Black Market */
		{ "Gary Gygaz",                 30000,  250, 150, 10,  5, RACE_HALF_TROLL},
		{ "Histor the Goblin",          30000,  250, 150, 10,  5, RACE_HALF_ORC},
		{ "Quark the Ferengi",          30000,  250, 150, 10,  5, RACE_DWARF},
		{ "Topi the Fair(?)",           30000,  250, 150, 10,  5, RACE_HUMAN},
	},
	{
		/* Home */
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99}
	},

	{
		/* Bookstore */
		{ "Dolaf the Greedy", 30000, 175, 108, 4, 12, RACE_HUMAN},
		{ "Odnar the Sage", 30000, 120, 105, 6, 16, RACE_HIGH_ELF},
		{ "Gandar the Neutral", 30000, 120, 110, 7, 19, RACE_DARK_ELF},
		{ "Ro-sha the Patient", 30000, 140, 105, 6, 12, RACE_ELF},
        },

	{
                /* Licialhyd Store */
                { "Shopkeeper",       30000,    170, 108,  5, 15, RACE_HUMAN},
		{ "Shopkeeper",       30000,    175, 108,  4, 12, RACE_HUMAN},
		{ "Shopkeeper",       30000,    170, 107,  5, 15, RACE_HALF_ELF},
		{ "Shopkeeper",       30000,    165, 107,  6, 18, RACE_HUMAN},
        },

	{
                /* Rare Items Shop */
                { "Uor The Fierce", 30000, 175, 108, 4, 6, RACE_HALF_TROLL},
                { "Kelg The Assasin", 30000, 120, 105, 6, 11, RACE_HALF_ELF},
                { "Gerard The Just", 30000, 120, 110, 7, 36, RACE_HUMAN},
                { "Swarg The Heavy", 30000, 140, 105, 6, 12, RACE_HALF_ORC},
        },

	{
                /* Rods and Crystals shop */
                { "Minda The Beautiful", 30000, 160, 107, 4, 6, RACE_ELF},
                { "Jonhatan The Trickster", 30000, 140, 106, 6, 11, RACE_HUMAN},
                { "Gorg The Ogre Magi", 30000, 180, 115, 7, 36, RACE_HALF_OGRE},
                { "Vincent The Thinker", 30000, 150, 103, 6, 12, RACE_GNOME},
	},

	{
                /* Music Store */
                { "Shopkeeper",       30000,    170, 108,  5, 15, RACE_HUMAN},
		{ "Shopkeeper",       30000,    175, 108,  4, 12, RACE_HUMAN},
		{ "Shopkeeper",       30000,    170, 107,  5, 15, RACE_HALF_ELF},
		{ "Shopkeeper",       30000,    165, 107,  6, 18, RACE_HUMAN},
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
        1,
        3,
        5,
        7,
        10,
        14,
        20,
        28,
        38,
        50,
        65,
        85,
        110,
        140,
        180,
        230,
        290,
        360,
        440,
        540,
        680,
        840,
        1020,
        1250,
        1750,
        2500,
        3500L,
        5000L,
        7500L,
        10000L,
        15000L,
        20000L,
        27500L,
        35000L,
        45000L,
        55000L,
        70000L,
        85000L,
        100000L,
        125000L,
        150000L,
        180000L,
        210000L,
        240000L,
        270000L,
        300000L,
        350000L,
        400000L,
        450000L,
        500000L,
        550000L,
        600000L,
        650000L,
        700000L,
        750000L,
        800000L,
        850000L,
        900000L,
        950000L,
        1000000L,
        1050000L,
        1100000L,
        1150000L,
        1200000L,
        1250000L,
        1300000L,
        1350000L,
        1400000L,
        1450000L,
        1500000L,
        1550000L,
        1600000L,
        1650000L,
        1700000L,
        1750000L,
        1800000L,
        1850000L,
        1900000L,
        1950000L,
        2000000L,
        2050000L,
        2100000L,
        2150000L,
        2200000L,
        2250000L,
        2300000L,
        2350000L,
        2400000L,
        2450000L,
        2500000L,
        2550000L,
        2600000L,
        2650000L,
        2700000L,
        2750000L,
        2800000L,
        2850000L,
        2900000L,
        2950000L,
        3000000L,
	3050000L,
        3100000L,
        3150000L,
        3200000L,
        3250000L,
        3300000L,
        3350000L,
        3400000L,
        3450000L,
        3500000L,
        3550000L,
        3600000L,
        3650000L,
        3700000L,
        3750000L,
        3800000L,
        3850000L,
        3900000L,
        3950000L,
        4000000L,
        4050000L,
        4100000L,
        4150000L,
        4200000L,
        4250000L,
        4300000L,
        4350000L,
        4400000L,
        4450000L,
        4500000L,
        4550000L,
        4600000L,
        4650000L,
        4700000L,
        4750000L,
        4800000L,
        4850000L,
        4900000L,
        4950000L,
        5000000L,
        5050000L,
        5100000L,
        5150000L,
        5200000L,
        5250000L,
        5300000L,
        5350000L,
        5400000L,
        5450000L,
        5500000L,
        5550000L,
        5600000L,
        5650000L,
        5700000L,
        5750000L,
        5800000L,
        5850000L,
        5900000L,
        5950000L,
        6000000L,
        6050000L,
        6100000L,
        6150000L,
        6200000L,
        6250000L,
        6300000L,
        6350000L,
        6400000L,
        6450000L,
        6500000L,
        6550000L,
        6600000L,
        6650000L,
        6700000L,
        6750000L,
        6800000L,
        6850000L,
        6900000L,
        6950000L,
        7000000L,
        7050000L,
        7100000L,
        7150000L,
        7200000L,
        7250000L,
        7300000L,
        7350000L,
        7400000L,
        7450000L,
        7500000L,
        7550000L,
        7000000L,
        7650000L,
        7700000L,
        7750000L,
        7800000L,
        7850000L,
        7900000L,
        7950000L,
        8000000L
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
        },
        {
                "Neuter",
                "Ruler"
        }
};


/*
 * Player Races
 *
 *      Title,
 *      {STR,INT,WIS,DEX,CON,CHR},
 *      r_dis, r_dev, r_sav, r_stl, r_srh, r_fos, r_thn, r_thb,
 *      hitdie, exp base,
 *      Age (Base, Mod),
 *      Male (Hgt, Wgt),
 *      Female (Hgt, Wgt)
 *      infra,
 *      class-choices
 */
player_race race_info[MAX_RACES] =
{
	{
		"Human",
		{  0,  0,  0,  0,  0,  0 },
		0,  0,  0,  0,  0,  10,  0,  0,
                12,  1000,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
		0,
                0x7FF,
	},
	{
		"Half-Elf",
                { 0,  0,  0,  0, 0,  0 },
		2,  3,  3,  1, 6,  11, -1,  5,
                11,  1000,
		24, 16,
		66,  6, 130, 15,
		62,  6, 100, 10,
		2,
                0x7FF,
	},
	{
		"Elf",
                { 0,  0,  0,  0, 0,  0 },
		5,  6,  6,  2, 8,  12, -5, 15,
                10,  1000,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		3,
                0x75F,

	},
	{
		"Dwarf",
                {  0, 0,  0, 0,  0, 0 },
		2,  9,  10,  -1,  7,  10, 15,  0,
                13,  1000,
		35, 15,
		48,  3, 150, 10,
		46,  3, 120, 10,
		5,
                0x005,        
	},
	{
		"Gnome",
                { 0,  0,  0,  0,  0, 0 },
		10, 12, 12,  3, 6,  13, -8, 12,
                10,  1000,
		50, 40,
		42,  3, 90,  6,
		39,  3, 75,  3,
		4,
                0x60F,
	},
	{
		"Kobold",
                { 0, 0, 0, 0, 0, 0 },
		-2, -3, -2, -1, 1, 8, 10, -8,
                11, 1000,
		11,  3,
                42,  3, 70,  3,
                39,  3, 70,  3,
		3,
                0x009,
	},
	{
                "Devling",
                {  0,  0,  0,  0,  0,  0 },
                5,  5,  5,  2,  10,  10,  5,  5,
                6,  1000,
                2,  5,
                42,  3, 70,  3,
                39,  3, 70,  3,
		0,
                0x7FF,
	},
	{
                "Celestial",
                {  0,  0,  0,  0,  0,  0 },
                10,  10,  10,  2,  2,  10,  25,  25,
                12,  1000,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
                0,
                0x7FF,
	},
	
        {
                "Demon",
                {  0,  0,  0,  0,  0,  0 },
                6,  10,  10,  -5,  18,  10,  20,  0,
                12,  1000,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
                3,
                0x7DF,
        },

        {
                "Zulgor",
                {  0,  0,  0,  0,  0,  0 },
                12,  10,  14,  1,  22,  10,  20,  0,
                18,  1000,
                14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
                3,
                0x7DF,
        },

	{
                "Monster",
                {  0,  0,  0,  0,  0,  0 },
                30,  30,  30,  5,  30,  10,  30,  30,
                1,  1000,
                5,  10,
		72,  6, 180, 25,
		66,  4, 150, 20,
		0,
                0x7FF,
	},

};


/* Player Races
 *
 *      Title,
 *      {STR,INT,WIS,DEX,CON,CHR},
 *      r_dis, r_dev, r_sav, r_stl, r_srh, r_fos, r_thn, r_thb,
 *      hitdie, exp base,
 *      Age (Base, Mod),
 *      Male (Hgt, Wgt),
 *      Female (Hgt, Wgt)
 *      infra,
 *      class-choices
 */


/*
 * Player Classes
 *
 *      Title,
 *      {STR,INT,WIS,DEX,CON,CHR},
 *      c_dis, c_dev, c_sav, c_stl, c_srh, c_fos, c_thn, c_thb,
 *      x_dis, x_dev, x_sav, x_stl, x_srh, x_fos, x_thn, x_thb,
 *      HD, Exp
 */
player_class class_info[MAX_CLASS] =
{
	{
                "Apprentice",
                { 0, 0, 0, 0, 0, 0},
                10, 10, 10, 0,  10, 0, 30, 30,
                2, 2,  2, 0,  0,  2,  10, 10,
                2,  0
	},
        
	{
		"Warrior",
		{ 5, -2, -2, 2, 2, -1},
		25, 18, 18, 1,  14, 2, 70, 55,
                12, 7,  10, 0,  0,  0,  45, 45,
		9,  0
	},

	{
		"Fighter",
		{ 5, -2, -2, 2, 2, -1},
		25, 18, 18, 1,  14, 2, 70, 55,
                12, 7,  10, 0,  0,  0,  45, 45,
		9,  0
	},

	{
		"Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 34, 20,
		7,  13, 9,  0,  0,  0,  15, 15,
		0, 30
	},

	{
		"Priest",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 48, 35,
		7,  10, 12, 0,  0,  0, 20, 20,
		2, 20
	},

	{
		"Rogue",
		{ 2, 1, -2, 3, 1, -1},
		45, 32, 28, 5, 32, 24, 60, 66,
		15, 10, 10, 0,  0,  0, 40, 30,
		6, 25
	},

	{
		"Ranger",
		{ 2, 2, 0, 1, 1, 1},
		30, 32, 28, 3,  24, 16, 56, 72,
		8,  10, 10, 0,  0,  0,  30, 45,
		4, 30
	},

	{
		"Paladin",
		{ 3, -3, 1, 0, 2, 2},
		20, 24, 26, 1,  12, 2, 68, 40,
		7,  10, 11, 0,  0,  0,  35, 30,
		6, 35
	},

	{
		"Monk",
		{ 2, -1, 1, 3, 2, 1},
		45, 32, 28, 5, 32, 24, 64, 60,
		15, 11, 10, 0,  0,  0, 40, 30,
		6, 40
	},

	{
                "Archer",
                { 2, 1, 0, 2, 1, 1},
                30, 32, 28, 3,  24, 16, 56, 82,
		8,  10, 10, 0,  0,  0,  30, 45,
		4, 30
	},

        /* Advanced Classes */

	{
	        "High-Mage",
        	{-5, 4, 0, 0, -2, 1},
		30, 36, 30, 2,  16, 20, 34, 20,
		7,  13, 9,  0,  0,  0,  15, 15,
		0, 30
	},

	{
                "Element Lord",
		{ 2, 2, 0, 1, 0, 1},
		30, 30, 28, 2,  18, 16, 50, 25,
		7,  10,  9, 0,  0,  0,  20, 20,
		4, 50
	},
	{
                "Monster Mage",
		{ 2, 2, 0, 1, 1, 1},
                30, 25, 32, 0,  15, 16, 56, 30,
                8,  6, 11, 0,  0,  0,  30, 10,
		4, 30
	},

	{
                "Defender",
		{ 5, -2, -2, 2, 2, -1},
		25, 18, 18, 1,  14, 2, 70, 55,
                12, 7,  10, 0,  0,  0,  45, 45,
		9,  0
	},

        {
                "Justice W",
                {3, 4, 6, 2, 3, 6},
                25, 36, 40, 0,  20, 20, 60, -10,
                5,  16, 17,  0,  0,  0,  20, 0,
                8, 150
	},
	{
                "High Monk",
                { 3, 6, 3, 4, 5, 1},
                30, 40, 42, 3,  27, 15, 150, -10,
                8,  12, 21, 0,  0,  0,  50, 0,
                6, 150
	},

	{
                "Soul Guardian",
                {-1, 3, 3, -1, 0, 3},
		25, 30, 32, 2,  16, 8, 48, 35,
		7,  10, 12, 0,  0,  0, 20, 20,
		2, 20
	},
	{
                "S. Stalker",
                { 2, 4, 0, 5, 1, 1},
                35, 28, 28, 5, 28, 24, 75, 68,
                12, 10, 10, 0,  0,  0, 42, 33,
                6, 120
	},


	{
                "Alchemist",
                {-2, 3, 4, 2, -2, 0},
                30, 60, 10, 2,  26, 20, 34, 30,
                7,  30, 9,  0,  0,  0,  15, 15,
                0, 40
	},

	{
                "Possessor",
                { 1, -2, -2, 1, 1, 0},
                25, 18, 18, 1,  14, 2, 30, 25,
                12, 7,  10, 0,  0,  0,  25, 25,
                9,  40
	},

	{
                "Sorceror",
                { -5, 6, 4, 2, -5, 1},
                35, 48, 38, 3, 22, 12, 10, 15,
                22, 17, 20, 0,  0,  0, 15, 15,
                4,  60
	},

	{
                "Necromancer",
                {-3, 3, -1, 3, -1, -5},
                16, 40, 42, 3,  10, 8, 35, 20,
                9,  16, 3, 0,  0,  0, 10, 5,
                1, 40
	},
	
	{
                "Magi Warrior",
                { 3, 4, 3, 4, 3, 3},
                35, 45, 40, 5, 30, 12, 72, 67,
                22, 17, 20, 0,  0,  0, 40, 40,
                8,  0
	},

	{
                "Berserker",
                { 20, -20, -20, 4, 20, -7},
                0, -2000, -50, 0, 0, 0, 150, -2000,
                0, 0, 20, 0,  0,  0, 80, 0,
                12, -20
	},

	{
                "Dark Lord",
                { 5, 7, -5, 3, 3, -20},
                25, 30, 40, 4, 25, 10, 60, 50,
                16, 12, 20, 0,  0,  0, 30, 30,
                8,  2000
	},

	{
                "Mana Maiden",
                { -20, 20, 20, 0, -20, 20},
                40, 100, 80, 1,  34, 16, -20, 10,
                12, 50, 40, 0,  0,  0,  0, 2,
                3, 400
	},

	{
                "Battle Master",
                { 10, -5, -5, 5, 10, 0},
                27, 32, 23, 5,  16, 8, 120, 100,
                14, 6,  11, 0,  0,  0,  80, 50,
                20,  0
	},

	{
                "Felisan",
                { 0, 0, 0, 0, 0, 0},
                32, 36, 42, 3,  40, 16, -10, 200,
                10,  11, 21, 0,  0,  0,  2, 60,
                6, 150
	},

	{
                "Fire Lord",
                { 6, 2, -5, 2, 2, -1},
                25, 18, 18, 0,  20, 2, 80, 50,
                12, 7,  10, 0,  0,  0,  50, 40,
                12,  200
	},
	{
                "Valkyrie",
                { 8, 5, 5, 8, 5, 6},
                30, 30, 26, 0,  25, 2, 100, 80,
                14, 12,  15, 0,  0,  0, 70, 40,
                20,  3000
	},
	{
                "Commander",
                {5, 4, 6, 2, 3, 15},
                32, 35, 30, 0,  20, 10, 80, 50,
                6,  14, 12,  0,  0,  0,  50, 20,
                10, 300
	},
	{
                "Samurai",
                { 6, 2, 2, 2, 2, 2},
                27, 21, 25, 1,  23, 2, 80, 40,
                13, 8,  12, 0,  0,  0,  50, 30,
                12,  200
	},
	{
                "Ice Lord",
                { 6, 2, -5, 2, 2, -1},
                25, 20, 18, 0,  20, 2, 80, 50,
                12, 8,  10, 0,  0,  0,  50, 40,
                12,  200
	},
	{
                "Psychic",
                {-1, 4, 4, 0, 0, 1},
		25, 30, 32, 2,  16, 8, 48, 35,
		7,  10, 12, 0,  0,  0, 20, 20,
		2, 20
	},
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
 * Certain "screens" always use the main screen, including News, Birth,
 * Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
 *
 * Later, special flags may allow sub-windows to "steal" stuff from the
 * main window, including File dump (help), File dump (artifacts, uniques),
 * Character screen, Small scale map, Previous Messages, Store screen, etc.
 *
 * The "ctrl-i" (tab) command flips the "Display inven/equip" and "Display
 * equip/inven" flags for all windows.
 *
 * The "ctrl-g" command (or pseudo-command) should perhaps grab a snapshot
 * of the main screen into any interested windows.
 */
cptr window_flag_desc[32] =
{
	"Display inven/equip",
	"Display equip/inven",
	"Display spell list",
	"Display character",
	NULL,
	NULL,
	"Display messages",
	"Display overhead view",
	"Display monster recall",
	"Display object recall",
	NULL,
	"Display snap-shot",
	NULL,
	NULL,
	"Display borg messages",
	"Display borg status",
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
};


/*
 * Available Options
 *
 * Option Screen Sets:
 *
 *      Set 1: User Interface
 *      Set 2: Disturbance
 *      Set 3: Inventory
 *      Set 4: Game Play
 *      Set 5: PernAngband
 *      Set 6: Birth
 *
 * Note that bits 28-31 of set 0 are currently unused.
 */
option_type option_info[] =
{
	/*** User-Interface ***/

	{ &rogue_like_commands,         FALSE,  1,      0, 0,
	"rogue_like_commands",          "Rogue-like commands" },

	{ &quick_messages,              TRUE,  1,      0, 1,
	"quick_messages",               "Activate quick messages" },

	{ &other_query_flag,            FALSE,  1,      0, 2,
	"other_query_flag",             "Prompt for various information" },

	{ &carry_query_flag,            FALSE,  1,      0, 3,
	"carry_query_flag",             "Prompt before picking things up" },

	{ &use_old_target,              FALSE,  1,      0, 4,
	"use_old_target",               "Use old target by default" },

	{ &always_pickup,               TRUE,   1,      0, 5,
	"always_pickup",                "Pick things up by default" },

	{ &always_repeat,               TRUE,   1,      0, 6,
	"always_repeat",                "Repeat obvious commands" },

	{ &depth_in_feet,               FALSE,  1,      0, 7,
	"depth_in_feet",                "Show dungeon level in feet" },

	{ &stack_force_notes,           TRUE,  1,      0, 8,
	"stack_force_notes",            "Merge inscriptions when stacking" },

	{ &stack_force_costs,           FALSE,  1,      0, 9,
	"stack_force_costs",            "Merge discounts when stacking" },

	{ &show_labels,                 TRUE,   1,      0, 10,
	"show_labels",                  "Show labels in object listings" },

	{ &show_weights,                TRUE,  1,      0, 11,
	"show_weights",                 "Show weights in object listings" },

	{ &show_inven_graph,            FALSE,  1,      2, 0,
	"show_inven_graph",             "Show graphics in inventory list" },

	{ &show_equip_graph,            FALSE,  1,      2, 1,
	"show_equip_graph",             "Show graphics in equipment list" },

	{ &show_store_graph,            FALSE,  1,      2, 2,
	"show_store_graph",             "Show graphics in stores" },

	{ &show_choices,                TRUE,  1,      0, 12,
	"show_choices",                 "Show choices in certain sub-windows" },

	{ &show_details,                TRUE,  1,      0, 13,
	"show_details",                 "Show details in certain sub-windows" },

	{ &ring_bell,                   FALSE,   1,      0, 14,
	"ring_bell",                    "Audible bell (on errors, etc)" },
	/* Changed to default to FALSE -- it's so extremely annoying!!! -TY */

	{ &use_color,                   TRUE,   1,      0, 15,
	"use_color",                    "Use color if possible (slow)" },


	/*** Disturbance ***/

	{ &find_ignore_stairs,          FALSE,   2,      0, 16,
	"find_ignore_stairs",           "Run past stairs" },

	{ &find_ignore_doors,           TRUE,   2,      0, 17,
	"find_ignore_doors",            "Run through open doors" },

	{ &find_cut,                    FALSE,   2,      0, 18,
	"find_cut",                     "Run past known corners" },

	{ &find_examine,                TRUE,   2,      0, 19,
	"find_examine",                 "Run into potential corners" },

	{ &disturb_move,                TRUE,   2,      0, 20,
	"disturb_move",                 "Disturb whenever any monster moves" },

	{ &disturb_near,                TRUE,   2,      0, 21,
	"disturb_near",                 "Disturb whenever viewable monster moves" },

	{ &disturb_panel,               TRUE,   2,      0, 22,
	"disturb_panel",                "Disturb whenever map panel changes" },

	{ &disturb_state,               TRUE,   2,      0, 23,
	"disturb_state",                "Disturb whenever player state changes" },

	{ &disturb_minor,               TRUE,   2,      0, 24,
	"disturb_minor",                "Disturb whenever boring things happen" },

	{ &disturb_other,               TRUE,   5,      0, 25,
	"disturb_other",                "Disturb whenever random things happen" },

	{ &alert_hitpoint,              FALSE,  2,      0, 26,
	"alert_hitpoint",               "Alert user to critical hitpoints" },

	{ &alert_failure,               FALSE,  2,      0, 27,
	"alert_failure",                "Alert user to various failures" },

	{ &last_words,                  TRUE,   5,      0, 28,
	"last_words",                   "Get last words when the character dies" },

	{ &speak_unique,                TRUE,   5,      0, 29,
	"speak_unique",                 "Allow shopkeepers and uniques to speak" },

	{ &small_levels,                TRUE,   5,      0, 30,
	"small_levels",                 "Allow unusually small dungeon levels" },

	{ &empty_levels,                TRUE,   5,      0, 31,
        "empty_levels",                 "Allow empty 'arena' levels" },

	/*** Game-Play ***/

        { &auto_haggle,                 TRUE,  3,      1, 0,
	"auto_haggle",                  "Auto-haggle in stores" },

        { &auto_scum,                   TRUE,  3,      1, 1,
	"auto_scum",                    "Auto-scum for good levels" },

	{ &stack_allow_items,           TRUE,   3,      1, 2,
	"stack_allow_items",            "Allow weapons and armor to stack" },

	{ &stack_allow_wands,           TRUE,   3,      1, 3,
	"stack_allow_wands",            "Allow wands/staffs/rods to stack" },

	{ &expand_look,                 FALSE,  3,      1, 4,
	"expand_look",                  "Expand the power of the look command" },

	{ &expand_list,                 FALSE,  3,      1, 5,
	"expand_list",                  "Expand the power of the list commands" },

	{ &view_perma_grids,            TRUE,   3,      1, 6,
	"view_perma_grids",             "Map remembers all perma-lit grids" },

	{ &view_torch_grids,            FALSE,  3,      1, 7,
	"view_torch_grids",             "Map remembers all torch-lit grids" },

	{ &dungeon_align,               TRUE,   3,      1, 8,
	"dungeon_align",                "Generate dungeons with aligned rooms" },

	{ &dungeon_stair,               TRUE,   3,      1, 9,
	"dungeon_stair",                "Generate dungeons with connected stairs" },

	{ &flow_by_sound,               FALSE,  3,      1, 10,
	"flow_by_sound",                "Monsters chase current location (v.slow)" },

	{ &flow_by_smell,               FALSE,  3,      1, 11,
	"flow_by_smell",                "Monsters chase recent locations (v.slow)" },

	{ &center_player,               FALSE,   3,      1, 12,
	"center_player",               "Center the screen around the player."},

         { &player_symbols,              FALSE,   5,      1, 13,
	"player_symbols",               "Use special symbols for the player char"},

/***** Space to use *****/

	{ &smart_learn,                 FALSE,  3,      1, 14,
	"smart_learn",                  "Monsters learn from their mistakes" },

	{ &smart_cheat,                 FALSE,  3,      1, 15,
	"smart_cheat",                  "Monsters exploit players weaknesses" },


	/*** Efficiency ***/

	{ &view_reduce_lite,            FALSE,  4,      1, 16,
	"view_reduce_lite",             "Reduce lite-radius when running" },

	{ &view_reduce_view,            FALSE,  4,      1, 17,
	"view_reduce_view",             "Reduce view-radius in town" },

	{ &avoid_abort,                 FALSE,  4,      1, 18,
	"avoid_abort",                  "Avoid checking for user abort" },

	{ &avoid_other,                 FALSE,  4,      1, 19,
	"avoid_other",                  "Avoid processing special colors" },

	{ &flush_failure,               TRUE,   4,      1, 20,
	"flush_failure",                "Flush input on various failures" },

	{ &flush_disturb,               FALSE,  4,      1, 21,
	"flush_disturb",                "Flush input whenever disturbed" },

	{ &flush_command,               FALSE,  4,      1, 22,
	"flush_command",                "Flush input before every command" },

	{ &fresh_before,                TRUE,   4,      1, 23,
	"fresh_before",                 "Flush output before every command" },

	{ &fresh_after,                 FALSE,  4,      1, 24,
	"fresh_after",                  "Flush output after every command" },

	{ &fresh_message,               FALSE,  4,      1, 25,
	"fresh_message",                "Flush output after every message" },

	{ &compress_savefile,           TRUE,   4,      1, 26,
	"compress_savefile",            "Compress messages in savefiles" },

        { &hilite_player,               FALSE,  4,      1, 27,
	"hilite_player",                "Hilite the player with the cursor" },

        { &view_yellow_lite,            FALSE,  4,      1, 28,
	"view_yellow_lite",             "Use special colors for torch-lit grids" },

        { &view_bright_lite,            FALSE,  4,      1, 29,
	"view_bright_lite",             "Use special colors for 'viewable' grids" },

        { &view_granite_lite,           FALSE,   4,      1, 30,
	"view_granite_lite",            "Use special colors for wall grids (slow)" },

        { &view_special_lite,           FALSE,  4,      1, 31,
	"view_special_lite",            "Use special colors for floor grids (slow)" },

	{ &skip_mutations,              FALSE, 5, 5, 0,
	"skip_mutations",               "Skip mutations in 'C'haracter Display" },

        { &plain_descriptions,          TRUE, 5, 5, 1,
	"plain_descriptions",           "Plain object descriptions" },

	{ &quest_scaling,               FALSE, 5, 5, 2,
	"quest_scaling",                "Scale enemies in quests to match your level" },

        { &auto_destroy,                TRUE, 5, 5, 3,
	"auto_destroy",                 "No query to destroy known worthless items" },

        { &very_fast_messages,          FALSE, 5,5, 4,
	"very_fast_messages",           "Very fast messages(auto-clear '-more-' prompts)" },

	{ &confirm_stairs,              FALSE, 5, 5, 5,
	"confirm_stairs",               "Prompt before exiting a dungeon level" },

	{ &disturb_pets,                FALSE, 5, 5, 6,
	"disturb_pets",                 "Disturb when visible pets move" },

#ifdef ALLOW_EASY_OPEN
        { &easy_open,               TRUE, 5,5,7,
	"easy_open",                "Automatically open doors" },
#endif /* ALLOW_EASY_OPEN */

#ifdef ALLOW_EASY_DISARM
        { &easy_disarm,                  TRUE, 5,5,8,
	"easy_disarm",                   "Automatically disarm traps" },
#endif /* ALLOW_EASY_DISARM */

        { &water_levels,                  TRUE, 5,5,9,
        "water_levels",                   "Allow generation of flooded levels" },

        { &flavored_attacks,              TRUE, 5,5,10,
        "flavored_attacks",               "Show silly messages when fighting" },

        { &always_small_level,            FALSE, 5,5,11,
        "always_small_level",             "Always make small levels" },

        { &no_pickup_corpse,              TRUE, 5,5,12,
        "no_pickup_corpse",               "Don't pick up the monster's corpses" },

        { &easy_tunnel,                   FALSE, 5,5,13,
        "easy_tunnel",                    "Automaticaly tunnel walls" },

        /*** Birth Options ***/


	/*** Stacking ***/

	{ &testing_stack,               TRUE,  255, 7, 30,
	"testing_stack",                "Allow objects to stack on floor" },

	{ &testing_carry,               TRUE,  255, 7, 31,
	"testing_carry",                "Allow monsters to carry objects" },


	/*** End of Table ***/

	{ NULL,                         0, 0, 0, 0,
	NULL,                           NULL }
};


/*
 * Random artifact activations.
 */
activation activation_info[MAX_T_ACT] = {
  {"death",0,127},
  {"ruination",0,128},
  {"destruction",0,129},
  {"stupidity",0,130},
  {"weakness",0,131},
  {"unhealth",0,132},
  {"ugliness",0,133},
  {"clumsiness",0,134},
  {"naivete",0,135},
  {"stat loss",0,136},
  {"huge stat loss",0,137},
  {"experience loss",0,138},
  {"huge experience loss",0,139},
  {"teleportation",1000,125},
  {"monster summoning",5,140},
  {"paralyzation",0,141},
  {"hallucination",100,142},
  {"poisoning",0,143},
  {"hunger",0,144},
  {"stun",0,145},
  {"cuts",0,146},
  {"paranoia",0,147},
  {"confusion",0,148},
  {"blindness",0,149},
  {"pet summoning",1010,150},
  {"cure paralyzation",5000,151},
  {"cure hallucination",1000,152},
  {"cure poison",1000,83},
  {"cure hunger",1000,154},
  {"cure stun",1000,154},
  {"cure cut",1000,155},
  {"cure fear",1000,156},
  {"cure confusion",1000,157},
  {"cure blindness",1000,158},
  {"cure light wounds",500,81},
  {"cure serious wounds",750,82},
  {"cure critical wounds",1000,86},
  {"curing",1100,160},
  {"genocide",5000,57},
  {"mass genocide",10000,58},
  {"restoration",2000,85},
  {"light",1000,111},
  {"darkness",0,161},
  {"teleportation",1000,125},
  {"level teleportation",500,162},
  {"acquirement",30000,163},
  {"something weird",50,164},
  {"aggravation",0,165},
  {"mutation",100,166},
  {"cure insanity",2000,167},
  {"cure mutation",2000,168},
  {"light absortion",800,169},
};
