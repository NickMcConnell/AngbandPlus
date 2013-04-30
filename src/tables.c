/* File: tables.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Large pieces of the code have been copied from Oangband by Leon Marrik */

#include "angband.h"

/*
 * Global array for looping through the "keypad directions".
 */
s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/*
 * Global arrays for converting "keypad direction" into "offsets".
 */
s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]".
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
 * Stat Table (STR) -- bonus to hit (plus 128)
 */
byte adj_str_th[A_RANGE] =
{
	128 + -3	/* 0 */,
	128 + -2	/* 1 */,
	128 + -1	/* 2 */,
	128 + -1	/* 3 */,
	128 + 0		/* 4 */,
	128 + 0		/* 5 */,
	128 + 0		/* 6 */,
	128 + 0		/* 7 */,
	128 + 0		/* 8 */,
	128 + 0		/* 9 */,
	128 + 0		/* 10 */,
	128 + 0		/* 11 */,
	128 + 0		/* 12 */,
	128 + 0		/* 13 */,
	128 + 0		/* 14 */,
	128 + 0		/* 15 */,
	128 + 1		/* 16 */,
	128 + 1		/* 17 */,
	128 + 2		/* 18 */,
	128 + 2		/* 19 */,
	128 + 2		/* 20 */,
	128 + 3		/* 21 */,
	128 + 3		/* 22 */,
	128 + 3		/* 23 */,
	128 + 4		/* 24 */,
	128 + 4		/* 25 */,
	128 + 5		/* 26 */,
	128 + 5		/* 27 */,
	128 + 6		/* 28 */,
	128 + 6		/* 29 */,
	128 + 7		/* 30 */
};

/*
 * Stat Table (STR) -- weight limit in deca-pounds
 */
byte adj_str_wgt[A_RANGE] =
{
	5	/* 0 */,
	6	/* 1 */,
	7	/* 2 */,
	8	/* 3 */,
	9	/* 4 */,
	10	/* 5 */,
	11	/* 6 */,
	12	/* 7 */,
	13	/* 8 */,
	14	/* 9 */,
	15	/* 10 */,
	16	/* 11 */,
	17	/* 12 */,
	18	/* 13 */,
	19	/* 14 */,
	20	/* 15 */,
	24	/* 16 */,
	28	/* 17 */,
	30	/* 18 */,
	30	/* 19 */,
	30	/* 20 */,
	30	/* 21 */,
	30	/* 22 */,
	30	/* 23 */,
	30	/* 24 */,
	30	/* 25 */,
	30	/* 26 */,
	30	/* 27 */,
	30	/* 28 */,
	30	/* 29 */,
	30	/* 30 */
};

/*
 * Stat Table (STR) -- weapon weight limit in pounds
 */
byte adj_str_hold[A_RANGE] =
{
	1		/* 0 */,
	1		/* 1 */,
	2		/* 2 */,
	3		/* 3 */,
	4		/* 4 */,
	5		/* 5 */,
	6		/* 6 */,
	7		/* 7 */,
	8		/* 8 */,
	9		/* 9 */,
	10		/* 10 */,
	12		/* 11 */,
	14		/* 12 */,
	15		/* 13 */,
	18		/* 14 */,
	19		/* 15 */,
	20		/* 16 */,
	21		/* 17 */,
	22		/* 18 */,
	23		/* 19 */,
	24		/* 20 */,
	25		/* 21 */,
	26		/* 22 */,
	27		/* 23 */,
	28		/* 24 */,
	29		/* 25 */,
	30		/* 26 */,
	35		/* 27 */,
	40		/* 28 */,
	45		/* 29 */,
	90		/* 30 */
};

/*
 * Stat Table (STR) -- digging value
 */
byte adj_str_dig[A_RANGE] =
{
	0		/* 0 */,
	0		/* 1 */,
	1		/* 2 */,
	2		/* 3 */,
	3		/* 4 */,
	4		/* 5 */,
	4		/* 6 */,
	5		/* 7 */,
	5		/* 8 */,
	6		/* 9 */,
	6		/* 10 */,
	7		/* 11 */,
	7		/* 12 */,
	8		/* 13 */,
	8		/* 14 */,
	9		/* 15 */,
	12		/* 16 */,
	20		/* 17 */,
	30		/* 18 */,
	40		/* 19 */,
	50		/* 20 */,
	60		/* 21 */,
	70		/* 22 */,
	75		/* 23 */,
	80		/* 24 */,
	85		/* 25 */,
	90		/* 26 */,
	95		/* 27 */,
	100		/* 28 */,
	100		/* 29 */,
	100		/* 30 */
};

/*
 * Stat Table (STR) -- used for calcuating throw range and bashing strength
 */
byte adj_str_blow[A_RANGE] =
{
	3		/* 0 */,
	4		/* 1 */,
	5		/* 2 */,
	6		/* 3 */,
	7		/* 4 */,
	8		/* 5 */,
	9		/* 6 */,
	10		/* 7 */,
	11		/* 8 */,
	12		/* 9 */,
	13		/* 10 */,
	14		/* 11 */,
	15		/* 12 */,
	16		/* 13 */,
	17		/* 14 */,
	20		/* 15 */,
	40		/* 16 */,
	60		/* 17 */,
	80		/* 18 */,
	100 	/* 19 */,
	120 	/* 20 */,
	140 	/* 21 */,
	160 	/* 22 */,
	170 	/* 23 */,
	180 	/* 24 */,
	190 	/* 25 */,
	200 	/* 26 */,
	210 	/* 27 */,
	220 	/* 28 */,
	230 	/* 29 */,
	240 	/* 30 */
};

/*
 * Stat Table (STR) -- armor weight limit
 */
byte adj_str_armor[A_RANGE] =
{
	0	/* 0 */,
	1	/* 1 */,
	2	/* 2 */,
	3	/* 3 */,
	4	/* 4 */,
	5	/* 5 */,
	6	/* 6 */,
	7	/* 7 */,
	9	/* 8 */,
	11	/* 9 */,
	13	/* 10 */,
	16  	/* 11 */,
	18	/* 12 */,
	20	/* 13 */,
	22	/* 14 */,
	24	/* 15 */,
	26	/* 16 */,
	27	/* 17 */,
	28	/* 18 */,
	29	/* 19 */,
	30	/* 20 */,
	31	/* 21 */,
	32	/* 22 */,
	34	/* 23 */,
	36	/* 24 */,
	38	/* 25 */,
	40	/* 26 */,
	45	/* 27 */,
	50	/* 28 */,
	55	/* 29 */,
	60	/* 30 */
};

/*
 * Stat Table (STR) - damage sides for unarmed combat
 */
byte adj_str_unarmed[A_RANGE] =
{
	1	/* 0 */,
	1	/* 1 */,
	1	/* 2 */,
	1	/* 3 */,
	1	/* 4 */,
	1	/* 5 */,
	1	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	2	/* 9 */,
	2	/* 10 */,
	2   /* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	3	/* 15 */,
	3	/* 16 */,
	3	/* 17 */,
	3	/* 18 */,
	4	/* 19 */,
	4	/* 20 */,
	5	/* 21 */,
	5	/* 22 */,
	6	/* 23 */,
	7	/* 24 */,
	8	/* 25 */,
	9	/* 26 */,
	10	/* 27 */,
	12	/* 28 */,
	15	/* 29 */,
	20	/* 30 */
};

/*
 * Stat Table (INT) -- disarming
 */
byte adj_int_dis[A_RANGE] =
{
	0	/* 0 */,
	1	/* 1 */,
	2	/* 2 */,
	3	/* 3 */,
	4	/* 4 */,
	5	/* 5 */,
	6	/* 6 */,
	7	/* 7 */,
	8	/* 8 */,
	9	/* 9 */,
	10	/* 10 */,
	11	/* 11 */,
	12	/* 12 */,
	13	/* 13 */,
	14	/* 14 */,
	15	/* 15 */,
	16	/* 16 */,
	17	/* 17 */,
	18	/* 18 */,
	19	/* 19 */,
	20	/* 20 */,
	21	/* 21 */,
	22	/* 22 */,
	23	/* 23 */,
	24	/* 24 */,
	25	/* 25 */,
	26	/* 26 */,
	27	/* 27 */,
	28	/* 28 */,
	29	/* 29 */,
	30	/* 30 */
};

/*
 * Stat Table (INT) -- alchemy
 */
byte adj_int_alc[A_RANGE] =
{
	0	/* 0 */,
	2	/* 1 */,
	4	/* 2 */,
	6	/* 3 */,
	8	/* 4 */,
	10	/* 5 */,
	12	/* 6 */,
	14	/* 7 */,
	16	/* 8 */,
	18	/* 9 */,
	20	/* 10 */,
	21	/* 11 */,
	22	/* 12 */,
	24	/* 13 */,
	27	/* 14 */,
	28	/* 15 */,
	31	/* 16 */,
	34	/* 17 */,
	37	/* 18 */,
	40	/* 19 */,
	43	/* 20 */,
	46	/* 21 */,
	49	/* 22 */,
	52	/* 23 */,
	55	/* 24 */,
	58	/* 25 */,
	61	/* 26 */,
	64	/* 27 */,
	67	/* 28 */,
	70	/* 29 */,
	73	/* 30 */
};

/*
 * Stat Table (INT) -- mapping
 */
byte adj_int_map[A_RANGE] =
{
	0		/* 0 */,
	2		/* 1 */,
	4		/* 2 */,
	6		/* 3 */,
	8		/* 4 */,
	10		/* 5 */,
	12		/* 6 */,
	14		/* 7 */,
	16		/* 8 */,
	19		/* 9 */,
	22		/* 10 */,
	25		/* 11 */,
	29		/* 12 */,
	33		/* 13 */,
	37		/* 14 */,
	39		/* 15 */,
	41		/* 16 */,
	43		/* 17 */,
	45		/* 18 */,
	47		/* 19 */,
	49		/* 20 */,
	51		/* 21 */,
	52		/* 22 */,
	53		/* 23 */,
	54		/* 24 */,
	55		/* 25 */,
	56		/* 26 */,
	57		/* 27 */,
	58		/* 28 */,
	59		/* 29 */,
	60		/* 30 */
};

/*
 * Stat Table (WIS) -- Saving throw
 */
byte adj_wis_sav[A_RANGE] =
{
	0	/* 0 */,
	0	/* 1 */,
	0	/* 2 */,
	0	/* 3 */,
	1	/* 4 */,
	1	/* 5 */,
	2	/* 6 */,
	2	/* 7 */,
	3	/* 8 */,
	3	/* 9 */,
	4	/* 10 */,
	4	/* 11 */,
	5	/* 12 */,
	6	/* 13 */,
	7	/* 14 */,
	8	/* 15 */,
	9	/* 16 */,
	10	/* 17 */,
	12	/* 18 */,
	13	/* 19 */,
	14	/* 20 */,
	16	/* 21 */,
	17	/* 22 */,
	18	/* 23 */,
	20	/* 24 */,
	21	/* 25 */,
	22	/* 26 */,
	24	/* 27 */,
	25	/* 28 */,
	26	/* 29 */,
	28	/* 30 */
};

/*
 * Stat Table (WIS) -- perception
 */
byte adj_wis_per[A_RANGE] =
{
	0		/* 0 */,
	2		/* 1 */,
	4		/* 2 */,
	6		/* 3 */,
	8		/* 4 */,
	10		/* 5 */,
	12		/* 6 */,
	14		/* 7 */,
	16		/* 8 */,
	19		/* 9 */,
	22		/* 10 */,
	25		/* 11 */,
	29		/* 12 */,
	33		/* 13 */,
	37		/* 14 */,
	39		/* 15 */,
	41		/* 16 */,
	43		/* 17 */,
	45		/* 18 */,
	47		/* 19 */,
	49		/* 20 */,
	51		/* 21 */,
	52		/* 22 */,
	53		/* 23 */,
	54		/* 24 */,
	55		/* 25 */,
	56		/* 26 */,
	57		/* 27 */,
	58		/* 28 */,
	59		/* 29 */,
	60		/* 30 */
};

/*
 * Stat Table (DEX) -- disarming
 */
byte adj_dex_dis[A_RANGE] =
{
	0	/* 0 */,
	0	/* 1 */,
	1	/* 2 */,
	1	/* 3 */,
	2	/* 4 */,
	2	/* 5 */,
	3	/* 6 */,
	3	/* 7 */,
	4	/* 8 */,
	4	/* 9 */,
	5	/* 10 */,
	5	/* 11 */,
	6	/* 12 */,
	6	/* 13 */,
	7	/* 14 */,
	7	/* 15 */,
	8	/* 16 */,
	8	/* 17 */,
	9	/* 18 */,
	9	/* 19 */,
	10	/* 20 */,
	10	/* 21 */,
	11	/* 22 */,
	11	/* 23 */,
	12	/* 24 */,
	12	/* 25 */,
	13	/* 26 */,
	13	/* 27 */,
	14	/* 28 */,
	14	/* 29 */,
	15	/* 30 */
};

/*
 * Stat Table (DEX) -- bonus to ac (plus 128)
 */
byte adj_dex_ta[A_RANGE] =
{
	128 + -4	/* 0 */,
	128 + -3	/* 1 */,
	128 + -2	/* 2 */,
	128 + -1	/* 3 */,
	128 + 0		/* 4 */,
	128 + 0		/* 5 */,
	128 + 0		/* 6 */,
	128 + 0		/* 7 */,
	128 + 0		/* 8 */,
	128 + 0		/* 9 */,
	128 + 0		/* 10 */,
	128 + 0		/* 11 */,
	128 + 1		/* 12 */,
	128 + 1		/* 13 */,
	128 + 1		/* 14 */,
	128 + 2		/* 15 */,
	128 + 2		/* 16 */,
	128 + 2		/* 17 */,
	128 + 3		/* 18 */,
	128 + 4		/* 19 */,
	128 + 6		/* 20 */,
	128 + 8		/* 21 */,
	128 + 9		/* 22 */,
	128 + 10	/* 23 */,
	128 + 11	/* 24 */,
	128 + 12	/* 25 */,
	128 + 13	/* 26 */,
	128 + 14	/* 27 */,
	128 + 15	/* 28 */,
	128 + 15	/* 29 */,
	128 + 15	/* 30 */
};

/*
 * Stat Table (DEX) -- bonus to hit (plus 128)
 */
byte adj_dex_th[A_RANGE] =
{
	128 + -3	/* 0 */,
	128 + -2	/* 1 */,
	128 + -1	/* 2 */,
	128 + -1	/* 3 */,
	128 + -1	/* 4 */,
	128 + 0		/* 5 */,
	128 + 0		/* 6 */,
	128 + 0		/* 7 */,
	128 + 0		/* 8 */,
	128 + 0		/* 9 */,
	128 + 0		/* 10 */,
	128 + 0		/* 11 */,
	128 + 0		/* 12 */,
	128 + 0		/* 13 */,
	128 + 1		/* 14 */,
	128 + 1		/* 15 */,
	128 + 1		/* 16 */,
	128 + 2		/* 17 */,
	128 + 2		/* 18 */,
	128 + 2		/* 19 */,
	128 + 3		/* 20 */,
	128 + 3		/* 21 */,
	128 + 4		/* 22 */,
	128 + 4		/* 23 */,
	128 + 5		/* 24 */,
	128 + 5		/* 25 */,
	128 + 6		/* 26 */,
	128 + 6		/* 27 */,
	128 + 7		/* 28 */,
	128 + 7		/* 29 */,
	128 + 8		/* 30 */
};


/*
 * Stat Table (DEX) -- jumping
 */
byte adj_dex_mob[A_RANGE] =
{
	0		/* 0 */,
	2		/* 1 */,
	4		/* 2 */,
	6		/* 3 */,
	8		/* 4 */,
	10		/* 5 */,
	12		/* 6 */,
	14		/* 7 */,
	16		/* 8 */,
	18		/* 9 */,
	20		/* 10 */,
	22		/* 11 */,
	25		/* 12 */,
	27		/* 13 */,
	30		/* 14 */,
	32		/* 15 */,
	35		/* 16 */,
	37		/* 17 */,
	40		/* 18 */,
	42		/* 19 */,
	45		/* 20 */,
	47		/* 21 */,
	50		/* 22 */,
	52		/* 23 */,
	55		/* 24 */,
	57		/* 25 */,
	60		/* 26 */,
	62		/* 27 */,
	65		/* 28 */,
	67		/* 29 */,
	70		/* 30 */
};

/*
 * Stat Table (DEX) -- chance of avoiding "theft" and "falling"
 */
byte adj_dex_safe[A_RANGE] =
{
	0	/* 0 */,
	1	/* 1 */,
	2	/* 2 */,
	3	/* 3 */,
	4	/* 4 */,
	5	/* 5 */,
	5	/* 6 */,
	6	/* 7 */,
	6	/* 8 */,
	7	/* 9 */,
	7	/* 10 */,
	8	/* 11 */,
	9	/* 12 */,
	10	/* 13 */,
	12	/* 14 */,
	14	/* 15 */,
	18	/* 16 */,
	22	/* 17 */,
	30	/* 18 */,
	40	/* 19 */,
	50	/* 20 */,
	70	/* 21 */,
	80	/* 22 */,
	90	/* 23 */,
	100	/* 24 */,
	100	/* 25 */,
	100	/* 26 */,
	100	/* 27 */,
	100	/* 28 */,
	100	/* 29 */,
	100	/* 30 */
};

/*
 * Stat Table (DEX) -- max number of blows you get in melee
 */
byte adj_dex_blows[A_RANGE] =
{
	1	/* 0 */,
	1	/* 1 */,
	1	/* 2 */,
	1	/* 3 */,
	1	/* 4 */,
	1	/* 5 */,
	1	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	2	/* 9 */,
	2	/* 10 */,
	2	/* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	3	/* 14 */,
	3	/* 15 */,
	3	/* 16 */,
	3	/* 17 */,
	4	/* 18 */,
	4	/* 19 */,
	4	/* 20 */,
	5	/* 21 */,
	5	/* 22 */,
	5	/* 23 */,
	6	/* 24 */,
	6	/* 25 */,
	6	/* 26 */,
	7	/* 27 */,
	7	/* 28 */,
	7	/* 29 */,
	7	/* 30 */
};

/*
 * Stat Table (CON) -- base regeneration rate
 */
byte adj_con_fix[A_RANGE] =
{
	0	/* 0 */,
	0	/* 1 */,
	0	/* 2 */,
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	0	/* 7 */,
	0	/* 8 */,
	0	/* 9 */,
	0	/* 10 */,
	1	/* 11 */,
	1	/* 12 */,
	1	/* 13 */,
	1	/* 14 */,
	2	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
	3	/* 18 */,
	3	/* 19 */,
	4	/* 20 */,
	5	/* 21 */,
	6	/* 22 */,
	7	/* 23 */,
	7	/* 24 */,
	8	/* 25 */,
	8	/* 26 */,
	8	/* 27 */,
	9	/* 28 */,
	9	/* 29 */,
	9	/* 30 */
};

/*
 * Stat Table (CON) -- extra quarter-hitpoints per level (plus 128)
 */
byte adj_con_mhp[A_RANGE] =
{
	128 + -12	/* 0 */,
	128 + -8	/* 1 */,
	128 + -6	/* 2 */,
	128 + -4	/* 3 */,
	128 + -2	/* 4 */,
	128 + -1	/* 5 */,
	128 + 0		/* 6 */,
	128 + 0		/* 7 */,
	128 + 0		/* 8 */,
	128 + 0		/* 9 */,
	128 + 0		/* 10 */,
	128 + 1		/* 11 */,
	128 + 2		/* 12 */,
	128 + 3		/* 13 */,
	128 + 4		/* 14 */,
	128 + 6		/* 15 */,
	128 + 8		/* 16 */,
	128 + 9		/* 17 */,
	128 + 12	/* 18 */,
	128 + 15	/* 19 */,
	128 + 19	/* 20 */,
	128 + 23	/* 21 */,
	128 + 27	/* 22 */,
	128 + 30	/* 23 */,
	128 + 33	/* 24 */,
	128 + 36	/* 25 */,
	128 + 40	/* 26 */,
	128 + 44	/* 27 */,
	128 + 48	/* 28 */,
	128 + 50	/* 29 */,
	128 + 50	/* 30 */
};

/*
 * Stat Table (CHR) -- Magic devices
 * (the gaps mirror the blows per turn table)
 */
byte adj_chr_dev[A_RANGE] =
{
	0	/* 0 */,
	1	/* 1 */,
	2	/* 2 */,
	3	/* 3 */,
	4	/* 4 */,
	5	/* 5 */,
	6	/* 6 */,
	7	/* 7 */,
	8	/* 8 */,
	10	/* 9 */,
	11	/* 10 */,
	12	/* 11 */,
	13	/* 12 */,
	14	/* 13 */,
	16	/* 14 */,
	17	/* 15 */,
	18	/* 16 */,
	19	/* 17 */,
	20	/* 18 */,
	22	/* 19 */,
	23	/* 20 */,
	24	/* 21 */,
	25	/* 22 */,
	26	/* 23 */,
	28	/* 24 */,
	29	/* 25 */,
	30	/* 26 */,
	32	/* 27 */,
	33	/* 28 */,
	34	/* 29 */,
	36	/* 30 */	
};

/*
 * Stat Table (CHR) -- Spell & Device Range
 */
byte adj_chr_range[A_RANGE] =
{
	0	/* 0 */,
	1	/* 1 */,
	2	/* 2 */,
	3	/* 3 */,
	4	/* 4 */,
	5	/* 5 */,
	5	/* 6 */,
	6	/* 7 */,
	6	/* 8 */,
	7	/* 9 */,
	7	/* 10 */,
	8	/* 11 */,
	8	/* 12 */,
	9	/* 13 */,
	9	/* 14 */,
	10	/* 15 */,
	10	/* 16 */,
	11	/* 17 */,
	11	/* 18 */,
	12	/* 19 */,
	12	/* 20 */,
	13	/* 21 */,
	13	/* 22 */,
	14	/* 23 */,
	14	/* 24 */,
	15	/* 25 */,
	15	/* 26 */,
	16	/* 27 */,
	16	/* 28 */,
	17	/* 29 */,
	17	/* 30 */	
};

/*
 * Stat Table (CHR) -- payment percentages
 */
byte adj_chr_gold[A_RANGE] =
{
	150	/* 0 */,
	144	/* 1 */,
	140	/* 2 */,
	137	/* 3 */,
	134	/* 4 */,
	131	/* 5 */,
	128	/* 6 */,
	125	/* 7 */,
	122	/* 8 */,
	119	/* 9 */,
	116	/* 10 */,
	113	/* 11 */,
	110	/* 12 */,
	107	/* 13 */,
	104	/* 14 */,
	101	/* 15 */,
	98	/* 16 */,
	95	/* 17 */,
	92	/* 18 */,
	89	/* 19 */,
	85	/* 20 */,
	82	/* 21 */,
	79	/* 22 */,
	76	/* 23 */,
	73	/* 24 */,
	70	/* 25 */,
	67	/* 26 */,
	64	/* 27 */,
	61	/* 28 */,
	58	/* 29 */,
	55	/* 30 */
};

/*
 * Stat Table (CHR) -- monster calmed recovery
 */
byte adj_chr_calm[A_RANGE] =
{
	15	/* 0 */,
	12	/* 1 */,
	10	/* 2 */,
	 8	/* 3 */,
	 8	/* 4 */,
	 7	/* 5 */,
	 7	/* 6 */,
	 6	/* 7 */,
	 6	/* 8 */,
	 5	/* 9 */,
	 5	/* 10 */,
	 5	/* 11 */,
	 4	/* 12 */,
	 4	/* 13 */,
	 4	/* 14 */,
	 4	/* 15 */,
	 3	/* 16 */,
	 3	/* 17 */,
	 2	/* 18 */,
	 2	/* 19 */,
	 1	/* 20 */,
	 1	/* 21 */,
	 1	/* 22 */,
	 1	/* 23 */,
	 1	/* 24 */,
	 1	/* 25 */,
	 1	/* 26 */,
	 1	/* 27 */,
	 1	/* 28 */,
	 1	/* 29 */,
	 1	/* 30 */
};

/*
 * Stat Table (INT/WIS) -- Number of spells at level 50 (33 for mystics)
 */
byte adj_mag_study[A_RANGE] =
{
	0		/* 0 */,
	0		/* 1 */,
	0		/* 2 */,
	0		/* 3 */,
	0		/* 4 */,
	8		/* 5 */,
	16		/* 6 */,
	24		/* 7 */,
	32		/* 8 */,
	40		/* 9 */,
	42		/* 10 */,
	44		/* 11 */,
	46		/* 12 */,
	48		/* 13 */,
	50		/* 14 */,
	52		/* 15 */,
	54		/* 16 */,
	56		/* 17 */,
	58		/* 18 */,
	63		/* 19 */,
	68		/* 20 */,
	73		/* 21 */,
	78		/* 22 */,
	83		/* 23 */,
	88		/* 24 */,
	94		/* 25 */,
	100		/* 26 */,
	106		/* 27 */,
	112		/* 28 */,
	118		/* 29 */,
	124		/* 30 */
};

/*
 * Stat Table (spell stat) -- extra mana at level 50 divided by 2.
 */
byte adj_mag_mana[A_RANGE] =
{
	0   	/* 0 */,
	0   	/* 1 */,
	0   	/* 2 */,
	1   	/* 3 */,
	8   	/* 4 */,
	15  	/* 5 */,
	21  	/* 6 */,
	25  	/* 7 */,
	28  	/* 8 */,
	32  	/* 9 */,
	35  	/* 10 */,
	37  	/* 11 */,
	39  	/* 12 */,
	41  	/* 13 */,
	43  	/* 14 */,
	45  	/* 15 */,
	48  	/* 16 */,
	52  	/* 17 */,
	60  	/* 18 */,
	75  	/* 19 */,
	100 	/* 20 */,
	125 	/* 21 */,
	150 	/* 22 */,
	162 	/* 23 */,
	175 	/* 24 */,
	187 	/* 25 */,
	200 	/* 26 */,
	205 	/* 27 */,
	210 	/* 28 */,
	215 	/* 29 */,
	220 	/* 30 */
};

/*
 * Stat Table (spell stat) -- extra half-mana-points per level for classes with extra mana
 */
byte adj_mag_extra_mana[A_RANGE] =
{
	0		/* 0 */,
	0		/* 1 */,
	0		/* 2 */,
	0		/* 3 */,
	0		/* 4 */,
	0		/* 5 */,
	0		/* 6 */,
	5		/* 7 */,
	10		/* 8 */,
	15		/* 9 */,
	20		/* 10 */,
	25		/* 11 */,
	27		/* 12 */,
	29		/* 13 */,
	31		/* 14 */,
	32		/* 15 */,
	34		/* 16 */,
	36		/* 17 */,
	38		/* 18 */,
	40		/* 19 */,
	47		/* 20 */,
	60		/* 21 */,
	75		/* 22 */,
	80		/* 23 */,
	85		/* 24 */,
	90		/* 25 */,
	95		/* 26 */,
	100		/* 27 */,
	105 		/* 28 */,
	110		/* 29 */,
	120		/* 30 */
};

/*
 * Stat Table (INT/WIS) -- Minimum failure rate (percentage)
 */
byte adj_mag_fail[A_RANGE] =
{
	99		/* 0 */,
	99		/* 1 */,
	99		/* 2 */,
	99		/* 3 */,
	99		/* 4 */,
	50		/* 5 */,
	30		/* 6 */,
	20		/* 7 */,
	15		/* 8 */,
	12		/* 9 */,
	11		/* 10 */,
	10		/* 11 */,
	9		/* 12 */,
	8		/* 13 */,
	7		/* 14 */,
	6		/* 15 */,
	5		/* 16 */,
	5		/* 17 */,
	4		/* 18 */,
	4		/* 19 */,
	3		/* 20 */,
	2		/* 21 */,
	2		/* 22 */,
	1		/* 23 */,
	1		/* 24 */,
	1		/* 25 */,
	1		/* 26 */,
	1		/* 27 */,
	0		/* 28 */,
	0		/* 29 */,
	0		/* 30 */
};

/*
 * Stat Table (INT/WIS) -- Various things
 */
byte adj_mag_stat[A_RANGE] =
{
	0		/* 0 */,
	0		/* 1 */,
	0		/* 2 */,
	0		/* 3 */,
	0		/* 4 */,
	1		/* 5 */,
	1		/* 6 */,
	1		/* 7 */,
	1		/* 8 */,
	1		/* 9 */,
	1		/* 10 */,
	1		/* 11 */,
	2		/* 12 */,
	2		/* 13 */,
	2		/* 14 */,
	3		/* 15 */,
	3		/* 16 */,
	3		/* 17 */,
	4		/* 18 */,
	6		/* 19 */,
	8		/* 20 */,
	10		/* 21 */,
	12		/* 22 */,
	13		/* 23 */,
	14		/* 24 */,
	15		/* 25 */,
	16		/* 26 */,
	17		/* 27 */,
	18		/* 28 */,
	19		/* 29 */,
	20		/* 30 */
};

/*
 * Maximum amount of blows does a weapon allows, by weight
 */
byte weapon_wgt_blows[30] = { 7,7,6,6,5,5,4,4,3,3,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 };

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
 * Effect of stealth on invisibility.
 */
byte invis_chance[31] =
{
	10, /*  0 */
	12, /*  1 */
	15, /*  2 */
	18, /*  3 */
	19, /*  4 */
	20, /*  5 */
	21, /*  6 */
	22, /*  7 */
	25, /*  8 */
	30, /*  9 */
	35, /* 10 */
	40, /* 11 */
	45, /* 12 */
	50, /* 13 */
	55, /* 14 */
	60, /* 15 */
	65, /* 16 */
	75, /* 17 */
	85, /* 18 */
	90, /* 19 */
	90, /* 20 */
	90, /* 21 */
	90, /* 22 */
	90, /* 23 */
	90, /* 24 */
	90, /* 25 */
	90, /* 26 */
	90, /* 27 */
	90, /* 28 */
	90, /* 29 */
	90  /* 30 */
};

/*
 * Base experience levels, may be adjusted up for race and/or class
 */
s32b player_exp[PY_MAX_LEVEL] =
{
	2 * 1.30,	/* 1 */
	8 * 1.30,	/* 2 */
	20 * 1.30,	/* 3 */
	46 * 1.30,	/* 4 */
	77 * 1.30,	/* 5 */
	123 * 1.25,	/* 6 */
	198 * 1.25,	/* 7 */
	308 * 1.25,	/* 8 */
	502 * 1.25,	/* 9 */
	715 * 1.25,	/* 10 */
	1001 * 1.2,	/* 11 */
	1403 * 1.2,	/* 12 */
	1936 * 1.2,	/* 13 */
	2618 * 1.2,	/* 14 */
	3564 * 1.2,	/* 15 */
	5080 * 1.15,	/* 16 */
	6699 * 1.15,	/* 17 */
	8756 * 1.15,	/* 18 */
	11132 * 1.15,	/* 19 */
	14256 * 1.15,	/* 20 */
	19448 * 1.10,	/* 21 */
	24948 * 1.10,	/* 22 */
	31416 * 1.10,	/* 23 */
	39875 * 1.10,	/* 24 */
	57750 * 1.10,	/* 25 */
	85250 * 1.05,	/* 26 */
	123200L * 1.05,	/* 27 */
	181500L * 1.05,	/* 28 */
	280500L * 1.05,	/* 29 */
	385000L * 1.05,	/* 30 */
	594000L * 1.0,	/* 31 */
	814000L * 1.0,	/* 32 */
	1149500L * 1.0,	/* 33 */
	1501500L * 1.0,	/* 34 */
	1980000L * 1.0,	/* 35 */
	2480500L * 0.95,	/* 36 */
	3234000L * 0.95,	/* 37 */
	4020500L * 0.95,	/* 38 */
	4840000L * 0.95,	/* 39 */
	6187500L * 0.95,	/* 40 */
	7590000L * 0.90,	/* 41 */
	9306000L * 0.90,	/* 42 */
	11088000L * 0.90,	/* 43 */
	12936000L * 0.90,	/* 44 */
	14850000L * 0.90,	/* 45 */
	16830000L * 0.85,	/* 46 */
	20020000L * 0.85,	/* 47 */
	23850000L * 0.85,	/* 48 */
	26730000L * 0.85,	/* 49 */
	30250000L * 0.85	/* 50 */
};

/*
 * Player Sexes
 *
 *	Title,
 *	Winner
 */
player_sex sex_info[SEX_MAX] =
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

player_race_special race_special_info[2][RACE_SPECIAL_LEVELS] =
/* Title, stat bonuses, skill bonuses, flag1, flag2, flag3, extra */
{
/* Angels and Demons - each line is a 5-level bracket. Extra is the index of racial ability */
	{ /*Angel*/
		{ "Cherub",		{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			0x00000000L, 0x00000000L, 0x00000000L, POW_DETECT_EVIL, 50},
		{ "Seraph",		{0, 0, 0, 0, 1, 0}, {0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0},
			0x10000000L, 0x00000000L, 0x00000040L, POW_LIGHT_AREA_1, 100},
		{ "Deva",		{2, 0, 1, 0, 2, 1},	{0, 0, 3, 3, 0, 0, 1, 0, 0, 0, 0},
			0x10000000L, 0x00000004L, 0x00000042L, POW_BEAM_WEAK_LITE, 50},
		{ "Deva",		{2, 0, 1, 0, 2, 1}, {0, 0, 3, 3, 0, 0, 1, 0, 0, 0, 0},
			0x10000000L, 0x00000004L, 0x00000042L, POW_BEAM_WEAK_LITE, 50},
		{ "Planetar",	{2, 1, 1, 0, 2, 1},	{0, 0, 6, 7, 0, 0, 1, 0, 0, 0, 0},
			0x90000000L, 0x00000004L, 0x00000052L, POW_BEAM_WEAK_LITE, 25},
		{ "Planetar",	{2, 1, 1, 0, 2, 1},	{0, 0, 6, 7, 0, 0, 1, 0, 0, 0, 0},
			0x90000000L, 0x00000004L, 0x00000052L, POW_BEAM_WEAK_LITE, 25},
		{ "Archon",		{3, 1, 2, 0, 3, 2}, {0, 0, 9,11, 0, 0, 2, 0, 0, 0, 0},
			0x90000000L, 0x00000005L, 0x00000052L, POW_BALL_HOLY_1, 15},
		{ "Archon",		{3, 1, 2, 0, 3, 2},	{0, 0, 9,11, 0, 0, 2, 0, 0, 0, 0},
			0x90000000L, 0x00000105L, 0x00000052L, POW_BALL_HOLY_1, 15},
		{ "Solar",		{3, 2, 2, 0, 3, 2},	{0, 0,12,16, 0, 0, 2, 0, 0, 0, 0},
			0x90000000L, 0x00000105L, 0x00000057L, POW_PROT_EVIL_1, 200},
		{ "Solar",		{3, 2, 2, 0, 3, 2}, {0, 0,12,16, 0, 0, 2, 0, 0, 0, 0},
			0x90000000L, 0x00000105L, 0x00000057L, POW_PROT_EVIL_1, 200},
		{ "Archangel",	{4, 2, 3, 0, 4, 3},	{0, 0,16,22, 0, 0, 3, 1, 0, 0, 0},
			0x90000000L, 0x00000107L, 0x00000057L, POW_PROT_EVIL_1, 150}
	},
	{ /*Demon*/
		{ "Lemure",		{0, 0, 0, 0, 0, 0} ,{0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0},
			0x00000000L, 0x00000000L, 0x00800000L, 0, 0},
		{ "Quasit",		{1, 0, 0, 1, 1, 0} ,{0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0},
			0x00000000L, 0x00000004L, 0x00800000L, 0, 0},
		{ "Imp",		{2, 2, 0, 2, 2,-1} ,{0, 0, 2, 0,  0, 0, 0, 0, 0, 0, 0},
			0x00000000L, 0x00000004L, 0x00800000L, 0, 0},
		{ "Tengu",		{3, 3, 0, 3, 2,-1} ,{0, 0, 4, 1, -1, 0, 0, 0, 0, 0, 0},
			0x00000000L, 0x0000000CL, 0x00800000L, POW_TELE_10, 15},
		{ "Bodak",      {3, 3, 0, 3, 3,-1} ,{0, 0, 6, 3, -2, 0, 1, 0, 0, 0, 0},
			0x04000000L, 0x0000000CL, 0x00800000L, POW_BOLT_FIRE_1, 30},
		{ "Vrock",      {3, 3, 1, 3, 4,-1} ,{0, 0, 8, 5, -4, 0, 2, 1, 0, 0, 0},
			0x04000000L, 0x0000000CL, 0x00800000L, POW_BOLT_FIRE_1, 25},
		{ "Hezrou",		{4, 3, 1, 3, 5,-2} ,{0, 0,10, 7, -5, 0, 2, 1, 1, 0, 0},
			0x44000000L, 0x0000000CL, 0x00800000L, POW_BOLT_FIRE_1, 20},
		{ "Glabrezu",   {4, 3, 1, 3, 5,-2} ,{0, 0,13, 9, -7, 0, 3, 1, 1, 0, 0},
			0x44000000L, 0x0000000CL, 0x00800000L, POW_BALL_FIRE_1, 30},
		{ "Nalfeshnee", {5, 4, 2, 3, 6,-2} ,{0, 0,16,11, -8, 0, 3, 1, 1, 0, 0},
			0x44000000L, 0x0000000CL, 0x00800004L, POW_BALL_FIRE_1, 25},
		{ "Pit Fiend",	{5, 4, 2, 3, 6,-3} ,{0, 0,19,13, -9, 0, 4, 2, 2, 0, 0},
			0x44000000L, 0x0000010CL, 0x00800004L, POW_BALL_FIRE_1, 20},
		{ "Balrog",		{6, 5, 2, 4, 7,-4} ,{0, 0,22,15,-10, 0, 4, 2, 2, 0, 0},
			0x44000000L, 0x0000010CL, 0x00800004L, POW_BALL_PLASMA, 30}
	}	
};

spell_book instruments[SV_MUSIC_MAX] =
{
	{
		/* Lyre (sval 0) */
		0,
		{
			{ POW_LIGHT_AREA_2,		"Ballad of Light",				 1,  1, 25 },
			{ POW_HEROISM,			"Ballad of Heroism",			 5,  6, 60 },
			{ POW_RAGE_1,			"Ballad of Rage",				15, 20, 60 },
			{ POW_RES_SOUND,		"Ballad of Sound Deflection",	18, 30, 75 },
			{ POW_DEST_TRAP_DOOR_2,	"Ballad of Unbarring",			22, 20, 60 },
			{ POW_RESISTANCE,		"Ballad of Resistance",			33, 30, 75 },
			{ POW_RESILIENCE,		"Ballad of Resilience",			50, 80, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Horn (sval 1) */
		0,
		{
			{ POW_BOLT_SOUND,		"Stunning Note",				 2,  2, 26 },
			{ POW_BOLT_FORCE_1,		"Crushing Note",				 5,  5, 40 },
			{ POW_STONE_TO_MUD,		"Bring Down Walls",				10, 10, 60 },
			{ POW_EARTHQUAKE,		"Call Earthquake",				20, 25, 60 },
			{ POW_DESTRUCTION,		"Tone of Destruction",			40, 45, 75 },
			{ POW_MASS_GENOCIDE,	"Tone of Death",				50,100, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Flute (sval 2) */
		0,
		{
			{ POW_HEAL_2,			"Lesser Healing Melody",		 4,  5, 20 },

			{ POW_POLY_MONSTER,		"Changing Melody",				12, 10, 40 },
			{ POW_HEAL_3,			"Greater Healing Melody",		15, 14, 40 },
			{ POW_CURE_POIS_DISE,	"Curing Melody",				30, 50, 90 },
			{ POW_WORD_RECALL,		"Recall Melody",				40, 75, 80 },
			{ POW_INVIS_2,			"Unseen Melody",				45, 45, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Lute (sval 3) */
		0,
		{
			{ POW_TELE_10,			"Dance of Dislocation",			 5,  3, 20 },
			{ POW_CONFUSE_ALL,		"Dance of Confusion",			10, 30, 30 },
			{ POW_BALL_SOUND,		"Dance of Damaging",			18, 15, 40 },
			{ POW_TELE_MAJOR,		"Dance of Teleportation",		25, 15, 50 },
			{ POW_TELE_LEVEL,		"Dance of Teleport Level",		40, 50, 60 },
			{ POW_ALTER_REALITY,	"Dance of Alter Reality",		50, 50, 70 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Drum (sval 4) */
		0,
		{
			{ POW_AGGRAVATE_SAFE,	"Aggravation Tempo",			 1,  1, 10 },
			{ POW_SCARE_ALL,		"Fear Tempo",					 8, 15, 30 },
			{ POW_HASTE_SELF_1,		"Fast Tempo",					15, 15, 40 },
			{ POW_SLOW_ALL,			"Slow Tempo",					28, 40, 60 },
			{ POW_TELE_OTHER_BEAM,	"Away Tempo",					32, 25, 60 },
			{ POW_BANISH,			"Banish Tempo",					45, 40, 70 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Harp (sval 5) */
		0,
		{
			{ POW_CALM_MONSTER,		"Song of Calmness",				 1,  3, 20 },
			{ POW_HARPOON,		"Song of Attraction",			 4,  6, 20 },
			{ POW_CALM_ANIMALS,		"Song of Soothing the Beast",	 8, 15, 60 },
			{ POW_CALM_NON_EVIL,	"Song of Peace",				15, 25, 80 },
			{ POW_CALM_ALL,			"Song of Great Peace",			25, 45, 90 },
			{ POW_HEAL_4,			"Song of Health",				40, 50, 85 },
			{ POW_DISPEL_EVIL_4,	"Song of Holyness",				50,	30, 75 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Pipes of pan (sval 6) */
		0,
		{
			{ POW_MUSIC_LYRE,		"Sounds of the Lyre",			25,  0,  5 },
			{ POW_MUSIC_HORN,		"Sounds of the Horn",			25,  0,  5 },
			{ POW_MUSIC_FLUTE,		"Sounds of the Flute",			25,  0,  5 },
			{ POW_MUSIC_LUTE,		"Sounds of the Lute",			25,  0,  5 },
			{ POW_MUSIC_DRUM,		"Sounds of the Drum",			25,  0,  5 },
			{ POW_MUSIC_HARP,		"Sounds of the Harp",			25,  0,  5 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	}
};

byte sub_spell_idx[MAX_SUB_TYPE][2] =
{
	{ 0, 0},
	{ 1, 5},	/* Fire, acid, elec, frost bolts */
	{ 6, 9},	/* Magic missile */
	{10,14},	/* Fire, acid, elec, frost, poison balls */
	{15,19},	/* Poison bolt */
	{20,22},	/* Mana, Nexus Bolt */
	{23,24},	/* Blizzard */
	{25,26},	/* Hell Storm */
	{27,28}		/* Elemental Storm */
};

sub_spell_type sub_spell_list[MAX_SUB_SPELL] = 
{
	{  0,  0, 0,  0, 0, 0, 0, 0 },
	/* Fire, acid, elec, frost bolts */
	{  1,  4, 3,  0, 0, 0,10, 0 },
	{  2,  7, 3,  0, 0, 4,10, 1 },
	{  5, 12, 3,  0, 0, 8,10, 4 },
	{ 10, 19, 4,  0, 0,16,10,14 },
	{ 20, 36, 4,  0, 0,32,10,24 },
	/* Magic missile */
	{  1,  4, 4,  0, 0, 0,10, 0 },
	{  3,  8, 4,  0, 0,10,10, 5 },
	{  5, 12, 4,  0, 0,20,10,10 },
	{  7, 16, 4,  0, 0,40,10,15 },
	/* Fire, acid, elec, frost, poison balls */
	{ 10,  0, 0, 45, 2, 0, 0, 0 },
	{ 20,  0, 0, 80, 2,20, 0, 5 },
	{ 40,  0, 0,150, 2,35, 0,10 },
	{ 50,  0, 0,150, 3,55, 0,15 },
	{ 60,  0, 0,200, 3,55, 0,20 },
	/* Poison bolt */
	{  5,  8, 6,  0, 0, 0, 8, 0 },
	{ 10, 15, 6,  0, 0,10, 8, 5 },
	{ 15, 21, 6,  0, 0,20, 8,10 },
	{ 20, 27, 6,  0, 0,40, 8,15 },
	{ 25, 33, 6,  0, 0,45, 8,20 },
	/* Mana, Nexus bolt */
	{  5, 11, 5,  0, 0, 0, 8, 0 },
	{ 15, 22, 6,  0, 0,10, 8, 5 },
	{ 30, 33, 7,  0, 0,20, 8,10 },
	/* Blizzard */
	{ 30,  0, 0,115, 3, 0, 0, 0 },
	{ 55,  0, 0,115, 4,48, 0,10 },
	/* Hell Storm */
	{ 35,  0, 0,140, 3, 0, 0, 0 },
	{ 65,  0, 0,140, 4,35, 0,10 },
	/* Elemental Storm */
	{ 40,  0, 0,175, 3, 0, 0, 0 },
	{ 75,  0, 0,175, 4,17, 0,10 }
};

spell_book books[SV_BOOK_MAX] = 
{
	{
		/* Esoteric Energies (sval 0) */
		SBF_MAGIC,
		{
			{ POW_BOLT_FIRE_X,		"Fire Bolt",				 1, -1, 28 },
			{ POW_TELE_10,			"Phase Door",  				 1,  1, 30 },
			{ POW_LIGHT_AREA_2,		"Light Area",				 1,  2, 32 },
			{ POW_DETECT_MONSTERS,		"Detect Monsters",			 2,  2, 29 },
			{ POW_SLEEP_MONSTER,		"Sleep Monster",			 2,  2, 32 },
			{ POW_BOLT_ELEC_X,		"Lightning Bolt",			 2, -1, 35 },
			{ POW_BALL_POISON,		"Stinking Cloud",			 3,  3, 33 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Winter Witchcraft (sval 1) */
		SBF_MAGIC,
		{
			{ POW_BOLT_COLD_X,		"Frost Bolt",				 1, -1, 28 },
			{ POW_TELE_10,			"Phase Door",  				 1,  1, 30 },
			{ POW_LIGHT_AREA_2,		"Light Area",				 1,  2, 32 },
			{ POW_DETECT_MONSTERS,		"Detect Monsters",			 2,  2, 29 },
			{ POW_SLEEP_MONSTER,		"Sleep Monster",			 2,  2, 32 },
			{ POW_BOLT_ACID_X,		"Acid Bolt",				 2, -1, 35 },
			{ POW_BALL_POISON,		"Stinking Cloud",			 3,  3, 33 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Sorcerous Adjurations (sval 2) */
		SBF_MAGIC,
		{
			{ POW_INFRAVISION,		"Infravision",				4,  2, 41 },
			{ POW_CONFUSE_MONSTER,		"Confuse Monster",			4,  3, 32 },
			{ POW_SHIELD,			"Shield",				5,  7, 66 },
			{ POW_TELE_MAJOR,		"Teleport Self",			7,  6, 41 },
			{ POW_BEAM_WEAK_LITE,		"Spear of Light",			7,  7, 41 },
			{ POW_SLEEP_ALL,		"Sleep All",				8,  7, 51 },
			{ POW_PHLOGISTON,		"Phlogiston",				9,  8, 51 },
			/* { POW_BOLT_MISSILE_X,	"Magic Missile",			5, -2, 25 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }

		}
	},
	{
		/* Blaze of Thaumaturgy (sval 3) */
		SBF_MAGIC,
		{
			{ POW_FLAMING_HANDS,		"Burning Hands",			10, 3, 42 },
			{ POW_BOLT_ACID_X,		"Acid Bolt",				10, -1, 42 },
			{ POW_SLOW_MONSTER,		"Slow Monster",				11,  7, 56 },
			{ POW_BALL_FIRE_X,		"Fire Ball",				11, -3, 37 },
			{ POW_RAY_MANA,			"Mana Ray",				12, 6, 37 },
			{ POW_GLYPH_LESSER,		"Barrier",				12, 11, 92 },
			{ POW_BALL_ELEC_X,		"Lightning Ball",			13, -3, 42 },
			/* { POW_CREATE_DOOR,		"Create Doors",				11, 12, 35 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Magick of the Rimewind (sval 4) */
		SBF_MAGIC,
		{
			{ POW_ICY_HANDS,		"Claws of Winter",			10, 3, 42 },
			{ POW_BOLT_ELEC_X,		"Lightning Bolt",			10, -1, 42 },
			{ POW_SLOW_MONSTER,		"Slow Monster",				11,  7, 56 },
			{ POW_BALL_COLD_X,		"Frost Ball",				11, -3, 37 },
			{ POW_RAY_MANA,			"Mana Ray",				12, 6, 37 },
			{ POW_GLYPH_LESSER,		"Barrier",				12, 11, 92 },
			{ POW_BALL_ACID_X,		"Acid Ball",				13, -3, 42 },
			/* { POW_CREATE_DOOR,		"Create Doors",				11, 12, 35 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{		
		/* Evocations of Power (sval 5) */
		SBF_MAGIC,
		{
			{ POW_EARTHBIND,		"Earthbind All",			16, 12, 51 },
			{ POW_RECHARGE_1,		"Recharge Item",			17, 12, 81 },
			{ POW_BLIND_MONSTER,		"Blind Monster",			19, 12, 66 },
			{ POW_CURSE_ALL,		"Curse All",				21, 25, 56 },
			{ POW_TELE_OTHER,		"Teleport Other",			23,  8, 66 },
			{ POW_DRAGONSLAYER,		"Dragonslayer",				25, 12, 81 },
			{ POW_HASTE_SELF_1,		"Haste Self",				30, 12, 71 },
			/* { POW_SLOW_ALL,		"Slow All",			19, 20, 33 }, */
			/* { POW_MAGIC_LOCK,		"Magic Lock",				21, 25, 50 }, */
			/* { POW_EARTHQUAKE,		"Earthquake",				25, 18, 60 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Resistance of Scarabtarices (sval 6) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_RES_FIRE,			"Resist Fire",				 5,  5, 56 },
			{ POW_RES_COLD,			"Resist Cold",				 5,  5, 56 },
			{ POW_RES_ACID,			"Resist Acid",				 5,  5, 56 },
			{ POW_RES_ELEC,			"Resist Electricity",		 5,  5, 50 },
			{ POW_RES_POISON,		"Resist Poison",			10, 10, 81 },
			{ POW_RES_DISEASE,		"Resist Disease",			10, 10, 81 },
			{ POW_RES_LITE_DARK,	"Resist Light & Darkness",	12, 15, 80 },
			{ POW_RESISTANCE,		"Resistance",				20, 25, 91 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Mordenkainen's Escapes (sval 7) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_ABSORB_HIT,		"Absorb Hit",				 5,  5, 31 },
			{ POW_CREATE_STAIR,		"Create Stairs",			10, 15, 46 },
			{ POW_TELE_LEVEL,		"Teleport Level",			15, 17, 66 },
			{ POW_TELE_CONTROL,		"Dimension Door",			20, 25, 76 },
			{ POW_CREATE_WALL,		"Create Walls",				30, 40, 86 },
/*			{ POW_WORD_RECALL,		"Word of Recall",			35, 25, 75 },		*/
			{ POW_DESTRUCTION,		"Word of Destruction",		40, 21, 84 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Kelek's Grimoire of Power (sval 8) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_DETECT_EVIL,		"Detect Evil",				 5,  5, 56 },
			{ POW_BLIGHT,			"Blight",					10, 13, 66 },
			{ POW_POLY_MONSTER,		"Polymorph Other",			15,  7, 51 },
			{ POW_PROBE_MONSTER,	"Probe Monster",			20, 12, 86 },
			{ POW_GENOCIDE,			"Genocide",					35, 50, 76 },
			{ POW_HYPERCHARGE,		"Hypercharge",				40,150, 86 },
			{ POW_MASS_GENOCIDE,	"Mass Genocide",			45, 75, 86 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Raal's Tome of Destruction (sval 9) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_BOLT_POISON_X,	"Poison Bolt",				 5, -4, 56 },
			{ POW_BOLT_MANA_X,		"Mana Bolt",				10, -5, 56 },
			{ POW_BOLT_NEXUS_X,		"Nexus Bolt",				10, -5, 56 },
			{ POW_BALL_POISON_X,	"Cloud Kill",				15, -3, 66 },
			{ POW_BALL_COLD_ELEC_X,	"Blizzard",					25, -6, 91 },
			{ POW_BALL_FIRE_ACID_X,	"Hell Storm",				30, -7, 87 },
			{ POW_BALL_ELEM_X,		"Elemental Storm",			35, -8, 94 },
			{ POW_BALL_MANA,		"Mana Storm",				40, 50, 97 },
			{ POW_BALL_ANNIHILATION,"Sphere of Annihilation",	50, 75, 74 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Tenser's transformations (sval 10) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_HEROISM,			"Heroism",					 5,  5, 56 },
			{ POW_STABILITY,		"Stability",				10, 18, 41 },
			{ POW_INVIS_2,			"Temporary Invisibility",	20, 35, 66 },
			{ POW_RES_CHAOS_NEXUS,	"Resist Chaos & Nexus",		30, 25, 41 },
			{ POW_RESILIENCE,		"Globe of Resilience",		45, 70, 81 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Lore of the Hunter (sval 11) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_BRAND_AMMO_ANML,	"Hunter's Arrows",			18, 25, 66 },
			{ POW_CALM_ANIMALS,		"Calm Animals",				23, 35, 61 },
			{ POW_BRAND_AMMO_WOUND, "Sharpen Arrows",			28, 25, 76 },
			{ POW_HASTE_SELF_1,		"Haste Self",				33, 12, 92 },
			{ POW_BRAND_AMMO_ELMNT, "Elemental Arrows",			43, 40, 86 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Novice's Handbook (sval 12) */
		SBF_PRAYER,
		{
			{ POW_BLESS_1,			"Bless",			 1,  2, 26 },
			{ POW_HEAL_1,			"Cure Light Wounds",		 1,  2, 21 },
			{ POW_DETECT_EVIL,		"Detect Evil",			 2,  1, 16 },
			{ POW_LIGHT_AREA_2,		"Call Light",			 3,  2, 31 },
			{ POW_CALM_MONSTER,		"Calm Monster",			 3,  3, 33 },
			{ POW_CURE_TAINT,		"Cleanse Taint",		 3,  1, 34 },
			{ POW_TURN_UNLIFE,		"Turn Unlife",			 3,  1, 34 },
			/* { POW_PHLOGISTON,		"Divine Flame",			 4,  4, 31 }, */
			/* { POW_BOLDNESS,			"Boldness",			 2,  2, 25 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Words of Wisdom (sval 13) */
		SBF_PRAYER,
		{
			{ POW_CURE_POISON,		"Neutralize Poison",		 4,  3, 44 },
			{ POW_SCARE_MONSTER,		"Scare Monster",		 5,  4, 35 },
			{ POW_TELE_MINOR,		"Portal",			 5,  4, 36 },
			{ POW_HEAL_2,			"Cure Serious Wounds",		 5,  4, 38 },
			{ POW_BLESS_2,			"Chant",			 6,  4, 40 },
			{ POW_RES_FIRE_COLD,		"Resist Heat and Cold",		 7,  5, 44 },
			{ POW_REMOVE_CURSE_1,		"Remove Curse",			 7,  6, 44 },
			/* { POW_SLEEP_ADJACENT,	"Sanctuary",			 6,  5, 36 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Chants and Blessings (sval 14) */
		SBF_PRAYER,
		{
			{ POW_BALL_HOLY_2,		"Orb of Draining",		 9,  8, 44 },
			{ POW_HEAL_3,			"Cure Critical Wounds",		 9,  7, 46 },
			{ POW_CURE_DISEASE,		"Cure Disease",			10,  7, 46 },
			{ POW_GLYPH_HOLY,		"Holy Sigil",			10, 2, 56 },
			{ POW_SEE_INVIS,		"Sense Invisible",		11,  8, 48 },
			{ POW_PROT_EVIL_2,		"Protection from Evil",		11,  8, 48 },
			{ POW_EARTHQUAKE,		"Earthquake",			13,  9, 61 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
 		/* Exorcism and Dispelling (sval 15) */
		SBF_PRAYER,
		{
			{ POW_CALM_NON_EVIL,		"Soothing Words",		16, 8, 86 },
			{ POW_DISPEL_UNDEAD_1,		"Dispel Undead",		16, 10, 56 },
			{ POW_DISPEL_DEMON,		"Dispel Demons",		17, 12, 61 },
			{ POW_PROT_CHAOS_2,		"Protection from Chaos",	20,  8, 48 },
			{ POW_DISPEL_EVIL_3,		"Dispel Evil",			25, 17, 76 },
			{ POW_GLYPH_WARDING,		"Glyph of Warding",		33, 5, 92 },
			{ POW_HOLY_2,			"Holy Word",			39, 32, 97 },
			/* { POW_BLESS_3,		"Prayer",					15, 14, 50 }, */
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Ethereal openings (sval 16) */
		(SBF_PRAYER | SBF_GOOD),
		{ 
			{ POW_TELE_10,			"Blink",					 3,  3, 56 },
			{ POW_TELE_MAJOR,		"Teleport Self",			10, 10, 56 },
			{ POW_TELE_OTHER,		"Teleport Other",			20, 15, 86 },
			{ POW_TELE_LEVEL,		"Teleport Level",			30, 40, 81 },
/*			{ POW_WORD_RECALL,		"Word of Recall",			35, 50, 75 },		*/
			{ POW_ALTER_REALITY,	"Alter Reality",			40, 60, 79 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Godly Insights (sval 17) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_DETECT_MONSTERS,	"Detect Monsters",			 3,  3, 56 },
			{ POW_DETECT_ALL,		"Detection",				10,  8, 86 },
			{ POW_PROBE_ALL,		"Probing",					25, 20, 86 },
			{ POW_MAP_2,			"Clairvoyance",				35, 50, 86 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Purifications and Healing (sval 18) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_HEAL_4,			"Cure Mortal Wounds",		17,  7, 66 },
			{ POW_HEAL_5,			"Healing",					30, 50, 86 },
			{ POW_RESTORE_STATS,	"Restoration",				35, 70, 92 },
			{ POW_RESTORE_LEVEL,	"Remembrance",				35, 70, 92 },
			{ POW_CALM_ALL,			"Holy Peace",				45, 50, 86 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Holy Infusions (sval 19) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_DEST_TRAP_DOOR_2,	"Unbarring Ways",			 5,  6, 56 },
			{ POW_RECHARGE_2,		"Recharging",				15, 20, 86 },
			{ POW_REMOVE_CURSE_2,	"Dispel Curse",				25, 40, 86 },
			{ POW_ENCHANT_WEAPON_2,	"Enchant Weapon",			35, 50, 86 },
			{ POW_ENCHANT_ARMOR_2,	"Enchant Armour",			37, 60, 91 },
			{ POW_BRAND_AMMO_HOLY,	"Sanctify Ammunition",		45, 95, 91 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Wrath of God (sval 20) */
		(SBF_PRAYER | SBF_GOOD),
		{  
			{ POW_DISPEL_UNDEAD_2,	"Dispel Undead",			15,  7, 76 },
			{ POW_DISPEL_EVIL_4,	"Dispel Evil",				20, 10, 81 },
			{ POW_BANISH,			"Banishment",				25, 25, 86 },
			{ POW_DESTRUCTION,		"Word of Destruction",		35, 35, 86 },
			{ POW_DRAIN_LIFE_3,		"Annihilation",				45, 60, 81 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Seven steps to transcendence (sval 21) */
		(SBF_MYSTIC | SBF_GOOD),
		{
			{ POW_CLEAR_MIND,		"Clarity of Mind",			 5,  5, 16 },
			{ POW_TELE_CONTROL,		"Mastery of Space",			10, 25, 66 },
			{ POW_RES_ELEMENTS,		"Power over Elements",		15, 30, 76 },
			{ POW_DETECT_ALL,		"Purity of Vision",			20, 35, 86 },
			{ POW_CURE_BODY,		"Control of the Body",		30, 40, 92 },
			{ POW_RES_GREATER,		"One with the World",		40, 45,102 },
			{ POW_RESILIENCE,		"Mind over Matter",			50, 50,112 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Teachings of the Ninth Master (sval 22) */
		(SBF_MYSTIC | SBF_GOOD),
		{
			{ POW_ABSORB_HIT,		"Absorb Hit",				 5,  5, 36 },
			{ POW_BOLT_MANA_X,		"Mana Bolt",				10, -5, 56 },
			{ POW_INVIS_2,			"Vanish from Sight",		20, 20, 66 },
			{ POW_BANISH,			"Banish Enemies",			25, 30, 86 },
			{ POW_HEAL_4,			"Heal",						30, 20, 66 },
			{ POW_HASTE_SELF_2,		"Essence of Speed",			35, 60, 56 },
			{ POW_ALTER_REALITY,	"Alter Reality",			40, 80, 81 },
			{ POW_GLYPH_WARDING,	"Glyph of Warding",			45, 60, 92 },
			{ POW_BURST_ASTRAL,		"Astral Burst",				50, 80, 56 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
			/* The Necronomicon (sval 23) */
		(SBF_NECRONOM | SBF_GOOD | SBF_ARTIFACT),
		{ 
			{ POW_DARK_AREA,		"Unlight Area",				 1,  1, 21 },
			{ POW_DISPEL_NON_EVIL,	"Wave of Evil",				25, 15,134 },
			{ POW_BEAM_NETHER,		"Ray of Evil",				40, 50, 99 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Codex of Ultimate Wisdom (sval 24) */
		(SBF_CODEX | SBF_GOOD | SBF_ARTIFACT),
		{
			{ POW_IDENTIFY,			"Perception",				 1, 15, 56 },
			{ POW_IDENTIFY_PACK,	"Greater Perception",		20, 20, 92 },
			{ POW_IDENTIFY_FULL,	"Revelation",				50,100, 78 },
			{ POW_MAP_2,			"Clairvoyance",				50,100, 16 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Mathemagical Calculations (sval 25) */
		(SBF_MATHEMAGIC | SBF_GOOD | SBF_ARTIFACT),
		{
			{ POW_SPELL_INFLUENCE,	"Enchance Influence",		20, 35, 82 },
			{ POW_SPELL_DURATION,	"Optimize Duration",		30, 50, 82 },
			{ POW_SPELL_DAMAGE,		"Augment Damage",			40, 75, 82 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	}

};

/*
 * Resistance names 
 */
cptr resist_names[RS_MAX] =
{
	"acid",
	"electricity",
	"fire",
	"cold",
	"water",
	"poison",
	"disease",
	"light",
	"dark",
	"sound",
	"shards",
	"nexus",
	"nether",
	"chaos",
	"disenchantment",
	"time",
	"mana"
};

/*
 * Resistance names 
 */
cptr resist_names_short[RS_MAX] =
{
	"Acid ",
	"Elec ",
	"Fire ",
	"Cold ",
	"Water",
	"Poisn",
	"Dises",
	"Light",
	"Dark ",
	"Sound",
	"Shard",
	"Nexus",
	"Nethr",
	"Chaos",
	"Disen",
	"Time ",
	"Mana "
};

/*
 * Resistance maximums
 */
res_cap resist_caps[RS_MAX] =
{
	{55, 88},
	{55, 88},
	{55, 88},
	{55, 88},
	{50, 66},
	{55, 88},
	{55, 88},
	{50, 66},
	{50, 66},
	{40, 50},
	{40, 50},
	{40, 50},
	{40, 50},
	{40, 50},
	{40, 50},
	{40, 50},
	{40, 50}
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
cptr stat_names[A_MAX] =
{
	"STR: ", "MEM: ", "WIS: ", "DEX: ", "CON: ", "PRE: "
};

/*
 * Abbreviations of damaged stats
 */
cptr stat_names_reduced[A_MAX] =
{
	"STR: ", "MEM: ", "WIS: ", "DEX: ", "CON: ", "PRE: "
};

/*
 * Certain "screens" always use the main screen, including News, Birth,
 * Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
 *
 * Later, special flags may allow sub-windows to "steal" stuff from the
 * main window, including File dump (help), File dump (artifacts, uniques),
 * Character screen, Small scale map, Previous Messages, Store screen, etc.
 */
cptr window_flag_desc[16] =
{
	"Display inven/equip",
	"Display equip/inven",
	"Display player (basic info)",
	"Display player (flags & resists)",
	"Display current player condition",
	"Display visible monsters",
	"Display messages",
	"Display overhead view",
	"Display monster recall",
	"Display object recall",
	/* "Display room descriptions", */
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};

/*
 * Options -- definitions 
 */
option_type options[OPT_NORMAL] =
{
	{"rogue_like_commands",	"Rogue-like commands",						FALSE},
	{"quick_messages",		"Activate quick messages",					TRUE },
	{"floor_query_flag",	"Prompt for floor item selection",			FALSE},
	{"carry_query_flag",	"Prompt before picking things up",			TRUE },
	{"carry_heavy_query",	"Verify before picking up heavy objects",	TRUE },
	{"use_old_target",		"Use old target by default",				FALSE},
	{"always_pickup",		"Pick things up by default",				TRUE },
	{"always_repeat",		"Repeat obvious commands",					TRUE },
	{"depth_in_feet",		"Show dungeon level in feet",				FALSE},
	{"stack_force_notes",	"Merge inscriptions when stacking",			TRUE },
	{"stack_force_costs",	"Merge discounts when stacking",			TRUE },
	{"show_labels",			"Show labels in equipment listings",		TRUE },
	{"show_weights",		"Show weights in all object listings",		TRUE },
	{"ring_bell",			"Audible bell (on errors, etc)",			TRUE },
	{"show_flavors",		"Show flavors in object descriptions",		TRUE },
	{"run_ignore_stairs",	"When running, ignore stairs",				FALSE },
	{"run_ignore_doors",	"When running, ignore doors",				FALSE },
	{"run_cut_corners",		"When running, cut corners",				TRUE },
	{"disturb_move",		"Disturb whenever any monster moves",		TRUE },
	{"disturb_near",		"Disturb whenever viewable monster moves",	TRUE },
	{"disturb_panel",		"Disturb whenever map panel changes",		TRUE },
	{"disturb_state",		"Disturb whenever player state changes",	TRUE },
	{"disturb_minor",		"Disturb whenever boring things happen",	TRUE },
	{"verify_destroy",		"Verify destruction of objects",			TRUE },
	{"use_command",			"Allow unified use command",				FALSE},
	{"expand_look",			"Expand the power of the look command",		TRUE },
	{"expand_list",			"Expand the power of the list commands",	TRUE },
	{"view_perma_grids",	"Map remembers all perma-lit grids",		TRUE },
	{"view_torch_grids",	"Map remembers all torch-lit grids",		TRUE},
	{"dungeon_align",		"Generate dungeons with aligned rooms",		TRUE },
	{"view_reduce_lite",	"Reduce lite-radius when running",			FALSE},
	{"hidden_player",		"Hide player symbol when running",			FALSE},
	{"avoid_abort",			"Avoid checking for user abort",			FALSE},
	{"avoid_other",			"Avoid processing special colors",			FALSE},
	{"flush_failure",		"Flush input on various failures",			TRUE },
	{"flush_disturb",		"Flush input whenever disturbed",			FALSE},
	{"fresh_before",		"Flush output before every command",		TRUE },
	{"fresh_after",			"Flush output after various things",		FALSE},
	{"compress_savefile",	"Compress messages in savefiles",			TRUE },
	{"hilite_player",		"Hilite the player with the cursor",		FALSE},
	{"view_yellow_lite",	"Use special colors for torch lite",		TRUE },
	{"view_bright_lite",	"Use special colors for field of view",		TRUE },
	{"view_granite_lite",	"Use special colors for wall grids",		FALSE},
	{"view_special_lite",	"Use special colors for floor grids",		TRUE },
	{"easy_direction",		"Open/Disarm/Close without direction",		FALSE},
	{"easy_alter",			"Open/Disarm doors/traps on movement",		FALSE},
	{"easy_floor",			"Display floor stacks in a list",   		FALSE},
	{"show_piles",			"Show stacks as white '&'",					TRUE},
	{"center_player",		"Center map continuously",					FALSE},
	{"run_avoid_center",	"Avoid centering while running",			FALSE},
	{"scroll_target",		"Scroll map while targetting",				TRUE },
	{"auto_more",			"Automatically clear '-more-' prompts",		FALSE},
	{"view_monster_lite",	"Allow monsters to have light radius",		TRUE },
	{"verify_leave_quest",	"Verify before descending from quest level",TRUE },
	{"always_show_lists",	"Always show lists in item/spell selection",TRUE },
	{"display_room_desc",	"Display room descriptions",				FALSE},
	{"display_insc_msg",	"Display inscriptions in messages",			FALSE},
	{"display_recharge_msg","Display messages upon recharge",			TRUE },
	{"inscribe_unique",		"Auto-inscribe unique drops",				FALSE},
	{"spellbook_menu",		"Alternate spellbook interface",			FALSE},
	{"show_trap_piles",		"Show objects on traps as red '&'",			TRUE },
	{"view_player_color",	"Use special colors for the player",		TRUE },
	{"allow_prefix_colors",	"Allow prefixes to determine object color",	TRUE }
};

option_type options_birth[OPT_BIRTH] =
{
	{"birth_point_based",		"Allow purchase of stats using points",		FALSE},
	{"birth_auto_roller",		"Allow specification of minimal stats",		FALSE},
	{"birth_preserve",			"Preserve artifacts when leaving level",	TRUE},
	{"birth_ironman",			"Restrict the use of stairs/recall",		FALSE},
	{"birth_no_stores",			"Restrict the use of stores/home",			FALSE},
	{"birth_no_artifacts",		"Restrict creation of artifacts",			FALSE},
	{"birth_autoscum",			"Generate better (harder) levels",			FALSE},
	{"birth_no_feelings",		"No level feelings",						FALSE},
	{"birth_start_kit",			"Pre-shop for some basic items",			FALSE},
	{"birth_smart_packs",		"Monsters act smarter in groups",			FALSE},
	{"birth_smart_cheat",		"Monsters exploit players weaknesses",		FALSE},
	{"birth_flow_by_sound",		"Monsters chase current location",			TRUE },
	{"birth_flow_by_smell",		"Monsters chase recent locations",			TRUE },
	{"birth_random_hp",			"Generate hitpoints randomly",				FALSE },
	{"birth_force_small_lev",	"All levels will be generated as small",	TRUE},
	{"birth_easy_mode",			"Easy mode",								FALSE},
	{"birth_nightmare_mode",	"Nightmare mode",							FALSE},
	{"birth_retain_squelch",	"Retain squelch settings",					FALSE},
	{"birth_weighted_roller",	"Allow specification of stat weighings",	FALSE},
};

option_type options_cheat[OPT_CHEAT] =
{
	{"cheat_peek",			"Peek into object creation",				FALSE},
	{"cheat_hear",			"Peek into monster creation",				FALSE},
	{"cheat_room",			"Peek into dungeon creation",				FALSE},
	{"cheat_know",			"Know complete monster info",				FALSE},
	{"cheat_live",			"Allow player to avoid death",				FALSE},
	{"cheat_no_save",		"No automatic saves upon death", 			FALSE},
	{"cheat_debug",			"Allow access to debug mode",	 			FALSE},
	{"cheat_wizard",		"Activate wizard mode",						FALSE},
	{"cheat_no_respawn",	"No respawning monsters",					FALSE}
};

option_type options_squelch[OPT_SQUELCH] =
{
	{"squelch_junk",		"Squelch anything worth 0 gold",			FALSE},
	{"auto_squelch",		"Automatically destroy squelched items",	FALSE}
};

/*
 * Option screen interface
 */
byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** User-Interface ***/

	{
		OPT_rogue_like_commands,
		OPT_use_command,
		OPT_quick_messages,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_carry_heavy_query,
		/* OPT_use_old_target, */
		OPT_always_pickup,
		OPT_always_repeat,
		OPT_verify_destroy,
		OPT_easy_direction,
		OPT_easy_alter,
		OPT_easy_floor,
		OPT_verify_leave_quest,
		OPT_spellbook_menu,
		OPT_always_show_lists,
		OPT_NONE
	},

	/*** Display ***/

	{
		OPT_depth_in_feet,
		OPT_show_labels,
		OPT_show_weights,
		OPT_show_flavors,
		OPT_show_piles,
		OPT_show_trap_piles,
		OPT_display_insc_msg,
		OPT_hilite_player,
		OPT_view_yellow_lite,
		OPT_view_bright_lite,
		OPT_view_granite_lite,
		OPT_view_special_lite,
		OPT_view_monster_lite,
		OPT_view_player_color,
		OPT_allow_prefix_colors,
		OPT_NONE
	},

	/*** Disturbance ***/

	{
		OPT_run_ignore_stairs,
		OPT_run_ignore_doors,
		OPT_run_cut_corners,
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_panel,
		OPT_disturb_state,
		OPT_disturb_minor,
		OPT_display_recharge_msg,
		OPT_auto_more,
		OPT_ring_bell,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Game-Play ***/

	{
		OPT_expand_look,
		OPT_expand_list,
 		OPT_center_player,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		OPT_dungeon_align,
		OPT_stack_force_notes,
		OPT_stack_force_costs,
		OPT_scroll_target,
		/* OPT_display_room_desc, */
		OPT_inscribe_unique,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Efficiency ***/

	{
		OPT_view_reduce_lite,
		OPT_hidden_player,
		OPT_avoid_abort,
		OPT_avoid_other,
		OPT_flush_failure,
		OPT_flush_disturb,
		OPT_fresh_before,
		OPT_fresh_after,
		OPT_compress_savefile,
 		OPT_run_avoid_center,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
	},

	/*** Cheat ***/

	{
#ifdef ALLOW_DEBUG
		OPT_cheat_debug,
#else /* ALLOW_DEBUG */
		OPT_NONE,
#endif /* ALLOW_DEBUG */
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_know,
		OPT_cheat_live,
		OPT_cheat_no_save,
		/* OPT_cheat_no_respawn, */
		OPT_cheat_wizard,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Birth ***/

	{
		OPT_birth_random_hp,
		OPT_birth_retain_squelch,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Difficulty ***/

	{
		OPT_birth_easy_mode,
		OPT_birth_nightmare_mode,
		OPT_birth_preserve,
		OPT_birth_no_stores,
		OPT_birth_no_feelings,
		OPT_birth_no_artifacts,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Monster AI ***/

	{
		OPT_birth_smart_packs,
		OPT_birth_smart_cheat,
#ifdef MONSTER_FLOW
		OPT_birth_flow_by_sound,
		OPT_birth_flow_by_smell,
#else /* MONSTER_FLOW */
		OPT_NONE,
		OPT_NONE,
#endif /* MONSTER_FLOW */
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	}
};


cptr inscrip_text[MAX_INSCRIP] =
{
	NULL,
	"terrible",
	"worthless",
	"cursed",
	"broken",
	"average",
	"good",
	"excellent",
	"special",
	"uncursed",
	"indestructible"
};
