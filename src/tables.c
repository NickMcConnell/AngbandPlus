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
	128 + 1		/* 15 */,
	128 + 1		/* 16 */,
	128 + 1		/* 17 */,
	128 + 1		/* 18 */,
	128 + 3		/* 19 */,
	128 + 5		/* 20 */,
	128 + 7		/* 21 */,
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
 * Stat Table (STR) -- bonus to dam (plus 128)
 */
byte adj_str_td[A_RANGE] =
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
	128 + 1		/* 13 */,
	128 + 2		/* 14 */,
	128 + 2		/* 15 */,
	128 + 3		/* 16 */,
	128 + 3		/* 17 */,
	128 + 4		/* 18 */,
	128 + 4		/* 19 */,
	128 + 5		/* 20 */,
	128 + 5		/* 21 */,
	128 + 7		/* 22 */,
	128 + 8 	/* 23 */,
	128 + 9		/* 24 */,
	128 + 10	/* 25 */,
	128 + 11	/* 26 */,
	128 + 12	/* 27 */,
	128 + 13	/* 28 */,
	128 + 14	/* 29 */,
	128 + 15	/* 30 */
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
	2		/* 0 */,
	3		/* 1 */,
	4		/* 2 */,
	5		/* 3 */,
	6		/* 4 */,
	7		/* 5 */,
	8		/* 6 */,
	9		/* 7 */,
	10		/* 8 */,
	11		/* 9 */,
	13		/* 10 */,
	15		/* 11 */,
	17		/* 12 */,
	19		/* 13 */,
	21		/* 14 */,
	23		/* 15 */,
	25		/* 16 */,
	27		/* 17 */,
	30		/* 18 */,
	35		/* 19 */,
	40		/* 20 */,
	50		/* 21 */,
	60		/* 22 */,
	70		/* 23 */,
	80		/* 24 */,
	90		/* 25 */,
	90		/* 26 */,
	90		/* 27 */,
	100		/* 28 */,
	100		/* 29 */,
	100		/* 30 */
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
 * Stat Table (STR) -- help index into the "blow" table
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
 * Stat Table (INT) -- Magic devices
 */
byte adj_int_dev[A_RANGE] =
{
	0	/* 0 */,
	0	/* 1 */,
	0	/* 2 */,
	0	/* 3 */,
	0	/* 4 */,
	1	/* 5 */,
	1	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	3	/* 15 */,
	4	/* 16 */,
	5	/* 17 */,
	6	/* 18 */,
	7	/* 19 */,
	8	/* 20 */,
	10	/* 21 */,
	12	/* 22 */,
	13	/* 23 */,
	14	/* 24 */,
	15	/* 25 */,
	16	/* 26 */,
	17	/* 27 */,
	18	/* 28 */,
	19	/* 29 */,
	20	/* 30 */
};

/*
 * Stat Table (INT) -- disarming
 */
byte adj_int_dis[A_RANGE] =
{
	0	/* 0 */,
	0	/* 1 */,
	0	/* 2 */,
	0	/* 3 */,
	0	/* 4 */,
	1	/* 5 */,
	1	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	3	/* 15 */,
	3	/* 16 */,
	4	/* 17 */,
	6	/* 18 */,
	8	/* 19 */,
	10	/* 20 */,
	11	/* 21 */,
	13	/* 22 */,
	14	/* 23 */,
	15	/* 24 */,
	16	/* 25 */,
	17	/* 26 */,
	18	/* 27 */,
	19	/* 28 */,
	19	/* 29 */,
	19	/* 30 */
};

/*
 * Stat Table (INT) -- alchemy
 */
byte adj_int_alc[A_RANGE] =
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
	0	/* 11 */,
	0	/* 12 */,
	1	/* 13 */,
	1	/* 14 */,
	2	/* 15 */,
	3	/* 16 */,
	4	/* 17 */,
	5	/* 18 */,
	6	/* 19 */,
	7	/* 20 */,
	8	/* 21 */,
	9	/* 22 */,
	9	/* 23 */,
	10	/* 24 */,
	10	/* 25 */,
	11	/* 26 */,
	13	/* 27 */,
	13	/* 28 */,
	15	/* 29 */,
	17	/* 30 */
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
	0	/* 4 */,
	1	/* 5 */,
	1	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	3	/* 15 */,
	3	/* 16 */,
	3	/* 17 */,
	4	/* 18 */,
	5	/* 19 */,
	7	/* 20 */,
	9	/* 21 */,
	11	/* 22 */,
	12	/* 23 */,
	13	/* 24 */,
	14	/* 25 */,
	15	/* 26 */,
	16	/* 27 */,
	17	/* 28 */,
	18	/* 29 */,
	19	/* 30 */
};

/*
 * Stat Table (DEX) -- disarming
 */
byte adj_dex_dis[A_RANGE] =
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
	1	/* 10 */,
	1	/* 11 */,
	1	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	4	/* 15 */,
	4	/* 16 */,
	5	/* 17 */,
	5	/* 18 */,
	6	/* 19 */,
	8	/* 20 */,
	8	/* 21 */,
	8	/* 22 */,
	9	/* 23 */,
	9	/* 24 */,
	9	/* 25 */,
	9	/* 26 */,
	9	/* 27 */,
	10	/* 28 */,
	10	/* 29 */,
	10	/* 30 */
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
	128 + -2	/* 2 */,
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
	128 + 1		/* 13 */,
	128 + 2		/* 14 */,
	128 + 3		/* 15 */,
	128 + 3		/* 16 */,
	128 + 3		/* 17 */,
	128 + 4		/* 18 */,
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
	8	/* 12 */,
	9	/* 13 */,
	9	/* 14 */,
	10	/* 15 */,
	15	/* 16 */,
	20	/* 17 */,
	30	/* 18 */,
	40	/* 19 */,
	50	/* 20 */,
	70	/* 21 */,
	90	/* 22 */,
	100	/* 23 */,
	100	/* 24 */,
	100	/* 25 */,
	100	/* 26 */,
	100	/* 27 */,
	100	/* 28 */,
	100	/* 29 */,
	100	/* 30 */
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
 * Stat Table (CON) -- extra half-hitpoints per level (plus 128)
 */
byte adj_con_mhp[A_RANGE] =
{
	128 + -6	/* 0 */,
	128 + -4	/* 1 */,
	128 + -3	/* 2 */,
	128 + -2	/* 3 */,
	128 + -1	/* 4 */,
	128 + 0		/* 5 */,
	128 + 0		/* 6 */,
	128 + 0		/* 7 */,
	128 + 0		/* 8 */,
	128 + 0		/* 9 */,
	128 + 0		/* 10 */,
	128 + 0		/* 11 */,
	128 + 1		/* 12 */,
	128 + 1		/* 13 */,
	128 + 2		/* 14 */,
	128 + 3		/* 15 */,
	128 + 4		/* 16 */,
	128 + 4		/* 17 */,
	128 + 6		/* 18 */,
	128 + 8		/* 19 */,
	128 + 10	/* 20 */,
	128 + 12	/* 21 */,
	128 + 14	/* 22 */,
	128 + 15	/* 23 */,
	128 + 16	/* 24 */,
	128 + 18	/* 25 */,
	128 + 20	/* 26 */,
	128 + 22	/* 27 */,
	128 + 25	/* 28 */,
	128 + 25	/* 29 */,
	128 + 25	/* 30 */
};

#ifdef ALLOW_HAGGLE

/*
 * Stat Table (CHR) -- payment percentages
 */
byte adj_chr_gold[A_RANGE] =
{
	130	/* 0 */,
	125	/* 1 */,
	122	/* 2 */,
	120	/* 3 */,
	118	/* 4 */,
	116	/* 5 */,
	114	/* 6 */,
	112	/* 7 */,
	110	/* 8 */,
	108	/* 9 */,
	106	/* 10 */,
	104	/* 11 */,
	103	/* 12 */,
	102	/* 13 */,
	101	/* 14 */,
	100	/* 15 */,
	98	/* 16 */,
	96	/* 17 */,
	94	/* 18 */,
	92	/* 19 */,
	90	/* 20 */,
	88	/* 21 */,
	86	/* 22 */,
	85	/* 23 */,
	84	/* 24 */,
	83	/* 25 */,
	82	/* 26 */,
	81	/* 27 */,
	80	/* 28 */,
	80	/* 29 */,
	80	/* 30 */
};

#else /* ALLOW_HAGGLE */

/*
 * Stat Table (CHR) -- payment percentages
 */
byte adj_chr_gold[A_RANGE] =
{
	140	/* 0 */,
	135	/* 1 */,
	132	/* 2 */,
	130	/* 3 */,
	128	/* 4 */,
	126	/* 5 */,
	124	/* 6 */,
	122	/* 7 */,
	120	/* 8 */,
	118	/* 9 */,
	116	/* 10 */,
	114	/* 11 */,
	113	/* 12 */,
	112	/* 13 */,
	111	/* 14 */,
	110	/* 15 */,
	108	/* 16 */,
	106	/* 17 */,
	104	/* 18 */,
	102	/* 19 */,
	100	/* 20 */,
	98	/* 21 */,
	96	/* 22 */,
	95	/* 23 */,
	93	/* 24 */,
	91	/* 25 */,
	89	/* 26 */,
	87	/* 27 */,
	83	/* 28 */,
	80	/* 29 */,
	80	/* 30 */
};

#endif 

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
	10		/* 5 */,
	20		/* 6 */,
	30		/* 7 */,
	40		/* 8 */,
	45		/* 9 */,
	50		/* 10 */,
	52		/* 11 */,
	54		/* 12 */,
	56		/* 13 */,
	58		/* 14 */,
	59		/* 15 */,
	62		/* 16 */,
	64		/* 17 */,
	70		/* 18 */,
	85		/* 19 */,
	95		/* 20 */,
	105		/* 21 */,
	115		/* 22 */,
	120		/* 23 */,
	122		/* 24 */,
	124		/* 25 */,
	126		/* 26 */,
	128		/* 27 */,
	130		/* 28 */,
	132		/* 29 */,
	134		/* 30 */
};

/*
 * Stat Table (spell stat) -- extra mana at level 50 divided by 2.
 */
byte adj_mag_mana[A_RANGE] =
{
	0   	/* 0 */,
	0   	/* 1 */,
	0   	/* 2 */,
	0   	/* 3 */,
	0   	/* 4 */,
	10  	/* 5 */,
	15  	/* 6 */,
	17  	/* 7 */,
	20  	/* 8 */,
	22  	/* 9 */,
	25  	/* 10 */,
	27  	/* 11 */,
	30  	/* 12 */,
	31  	/* 13 */,
	33  	/* 14 */,
	35  	/* 15 */,
	40  	/* 16 */,
	47  	/* 17 */,
	55  	/* 18 */,
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
	105 	/* 28 */,
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
 * These tables are used to help calculate the number of blows the player can
 * make in a single round of attacks (one player turn) with a normal weapon.
 */

/* 
 * The first table compares the weapon weight (/10) with the player's strength. 
 */
byte wght_str_blows[30][A_RANGE] = 
{
/* W/S */

/*  0 */ {5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5},
/*  1 */ {4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5},
/*  2 */ {3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5},
/*  3 */ {3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5},
/*  4 */ {2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5},
/*  5 */ {2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5},
/*  6 */ {1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5},
/*  7 */ {1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5},
/*  8 */ {1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5},
/*  9 */ {1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,5},
/* 10 */ {0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4},
/* 11 */ {0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4},
/* 12 */ {0,0,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4},
/* 13 */ {0,0,0,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4},
/* 14 */ {0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4},
/* 15 */ {0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4},
/* 16 */ {0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4},
/* 17 */ {0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4},
/* 18 */ {0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4},
/* 19 */ {0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4},
/* 20 */ {0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4},
/* 21 */ {0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4},
/* 22 */ {0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4},
/* 23 */ {0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4},
/* 24 */ {0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4},
/* 25 */ {0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4},
/* 26 */ {0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3},
/* 27 */ {0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3},
/* 28 */ {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3},
/* 29 */ {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3}
};

/* 
 * The next table is then used to calculate the actual number of attacks.
 *
 * This number ranges from a single blow/round for weak players to up to six
 * blows/round for powerful warriors.
 *
 * Note that certain artifacts and ego-items give "bonus" blows/round.
 *
 * To get "P", we use the result of the previous table (modified by class).
 *
 * To get "D", we use dexterity
 *
 * The player gets "blows_table[P][D]" blows/round, as shown below,
 * up to a maximum of "num" blows/round, plus any "bonus" blows/round.
 */
byte blows_table[10][A_RANGE] =
{
/* P/D */
                         
/* 0 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3},
/* 1 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3},
/* 2 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3},
/* 3 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4},
/* 4 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,3,3,4,4,4,4,4},
/* 5 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4},
/* 6 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5},
/* 7 */ {1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,5,5},
/* 8 */ {1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6},
/* 9 */ {1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6}
};

/*
 * The max plusses per item weight, in increments of 1 pound
 */
byte max_item_plus[30] =
{
	3,   3,  4,  4,  5,  5,  6,  6,  7,  7, 
	8,   9,  9, 10, 10, 11, 11, 12, 12, 12,
	13, 13, 13, 14, 14, 14, 15, 15, 15, 15
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
	10,
	25,
	45,
	70,
	100,
	140,
	200,
	280,
	380,
	500,
	650,
	850,
	1100,
	1400,
	1800,
	2300,
	2900,
	3600,
	4400,
	5400,
	6800,
	8400,
	10200,
	12500,
	17500,
	25000,
	35000L,
	50000L,
	75000L,
	100000L,
	150000L,
	200000L,
	275000L,
	350000L,
	450000L,
	550000L,
	700000L,
	850000L,
	1000000L,
	1250000L,
	1500000L,
	1800000L,
	2100000L,
	2400000L,
	2700000L,
	3000000L,
	3500000L,
	4000000L,
	4500000L,
	5000000L
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
		{ "Cherub",		{0,0,0,0,0,0}	,{0,0, 0, 0,0,0,0, 0,0,0,0,0},0x00000000L,0x00000000L,0x00000000L,1},
		{ "Seraph",		{0,0,0,0,1,0}	,{0,0, 1, 1,0,0,0, 0,0,0,0,0},0x10000000L,0x00000000L,0x00000040L,2},
		{ "Deva",		{2,0,1,0,2,1}	,{0,0, 3, 3,0,0,0, 1,0,0,0,0},0x10000000L,0x00000004L,0x00000042L,3},
		{ "Deva",		{2,0,1,0,2,1}	,{0,0, 3, 3,0,0,0, 1,0,0,0,0},0x10000000L,0x00000004L,0x00000042L,3},
		{ "Planetar",	{2,1,1,0,2,1}	,{0,0, 6, 7,0,0,0, 3,0,0,0,0},0x90000000L,0x00000004L,0x00000052L,3},
		{ "Planetar",	{2,1,1,0,2,1}	,{0,0, 6, 7,0,0,0, 3,0,0,0,0},0x90000000L,0x00000004L,0x00000052L,3},
		{ "Archon",		{3,1,2,0,3,2}	,{0,0, 9,11,0,0,0, 4,1,0,0,0},0x90000000L,0x00000005L,0x00000052L,4},
		{ "Archon",		{3,1,2,0,3,2}	,{0,0, 9,11,0,0,0, 4,1,0,0,0},0x90000000L,0x00000005L,0x00000052L,4},
		{ "Solar",		{3,2,2,0,3,2}	,{0,0,12,16,0,0,0, 5,2,0,0,0},0x90000000L,0x00000005L,0x00000057L,5},
		{ "Solar",		{3,2,2,0,3,2}	,{0,0,12,16,0,0,0, 5,2,0,0,0},0x90000000L,0x00000005L,0x00000057L,5},
		{ "Archangel",	{4,2,3,0,4,3}	,{0,0,16,22,0,0,0, 8,3,0,0,0},0x90000000L,0x00000007L,0x00000057L,5}
	},
	{ /*Demon*/
		{ "Lemure",		{0,0,0,0, 0, 0}	,{0,0, 0, 0,  0,0,0, 0, 0, 0,0,0},0x00000000L,0x00000000L,0x00000000L,0},
		{ "Quasit",		{1,0,0,1, 1, 0}	,{0,0, 0, 0,  0,0,0, 0, 0, 0,0,0},0x00000000L,0x00000004L,0x00000000L,0},
		{ "Imp",		{2,2,0,2, 2,-1}	,{0,0, 2, 0,  0,0,0, 0, 0, 0,0,0},0x00000000L,0x00000004L,0x00000000L,0},
		{ "Tengu",		{3,3,0,3, 2,-1}	,{0,0, 4, 1, -1,0,0, 1, 1, 0,0,0},0x00000000L,0x0000000CL,0x00000000L,6},
		{ "Bodak",      {3,3,0,3, 3,-1}	,{0,0, 6, 3, -2,0,0, 3, 1, 1,0,0},0x04000000L,0x0000000CL,0x00000000L,7},
		{ "Vrock",      {3,3,1,3, 4,-1}	,{0,0, 8, 5, -4,0,0, 5, 2, 2,0,0},0x04000000L,0x0000000CL,0x00000000L,7},
		{ "Hezrou",		{4,3,1,3, 5,-2}	,{0,0,10, 7, -5,0,0, 6, 3, 2,0,0},0x44000000L,0x0000000CL,0x00000000L,7},
		{ "Glabrezu",   {4,3,1,3, 5,-2}	,{0,0,13, 9, -7,0,0, 7, 3, 3,0,0},0x44000000L,0x0000000CL,0x00000000L,8},
		{ "Nalfeshnee", {5,4,2,3, 6,-2}	,{0,0,16,11, -8,0,0, 8, 4, 3,0,0},0x44000000L,0x0000000CL,0x00000004L,8},
		{ "Pit Fiend",	{5,4,2,3, 6,-3}	,{0,0,19,13, -9,0,0, 9, 5, 4,0,0},0x44000000L,0x0000000CL,0x00000004L,8},
		{ "Balrog",		{6,5,2,4, 7,-4}	,{0,0,22,15,-10,0,0,10, 5, 4,0,0},0x44000000L,0x0000000CL,0x00000004L,9}
	}	
};

spell_book instruments[SV_MUSIC_MAX] =
{
	{
		/* Lyre (sval 0) */
		0,
		{
			{ POW_LIGHT_AREA,		"Ballad of Light",				 1,  1, 25 },
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
			{ POW_HEAL_1,			"Lesser Healing Melody",		 2,  2, 20 },
			{ POW_POLY_MONSTER,		"Changing Melody",				15, 10, 40 },
			{ POW_HEAL_3,			"Greater Healing Melody",		20, 14, 40 },
			{ POW_CURE_POIS_DISE,	"Curing Melody",				35, 50, 90 },
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
			{ POW_TELE_OTHER,		"Away Tempo",					32, 25, 60 },
			{ POW_BANISH,			"Banish Tempo",					45, 40, 70 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Harp (sval 5) */
		0,
		{
			{ POW_CALM_MONSTER,		"Song of Calmness",				 1,  4, 20 },
			{ POW_CALM_ANIMALS,		"Song of Soothing the Beast",	10, 30, 60 },
			{ POW_CALM_NON_EVIL,	"Song of Peace",				20, 45, 80 },
			{ POW_CALM_ALL,			"Song of Great Peace",			30, 65, 90 },
			{ POW_HEAL_4,			"Song of Health",				45, 85, 85 },
			{ POW_DISPEL_EVIL_3,	"Song of Holyness",				50,	30, 75 },
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
	{  1,  4, 3,  0, 0, 0, 9, 0 },
	{  2,  7, 3,  0, 0, 4, 9, 1 },
	{  5, 12, 3,  0, 0, 8, 9, 4 },
	{ 10, 19, 4,  0, 0,16, 9,14 },
	{ 20, 36, 4,  0, 0,32, 9,24 },
	/* Magic missile */
	{  1,  4, 4,  0, 0, 0, 9, 0 },
	{  3,  8, 4,  0, 0,10, 9, 5 },
	{  5, 12, 4,  0, 0,20, 9,10 },
	{  7, 16, 4,  0, 0,40, 9,15 },
	/* Fire, acid, elec, frost, poison balls */
	{ 10,  0, 0, 45, 2, 0, 0, 0 },
	{ 20,  0, 0, 80, 2,20, 0, 5 },
	{ 40,  0, 0,150, 2,35, 0,10 },
	{ 50,  0, 0,150, 3,55, 0,15 },
	{ 60,  0, 0,200, 3,55, 0,20 },
	/* Poison bolt */
	{  5,  8, 6,  0, 0, 0, 6, 0 },
	{ 10, 15, 6,  0, 0,10, 6, 5 },
	{ 15, 21, 6,  0, 0,20, 6,10 },
	{ 20, 27, 6,  0, 0,40, 6,15 },
	{ 25, 33, 6,  0, 0,45, 6,20 },
	/* Mana, Nexus bolt */
	{  5, 11, 5,  0, 0, 0, 6, 0 },
	{ 15, 22, 6,  0, 0,10, 6, 5 },
	{ 30, 33, 7,  0, 0,20, 6,10 },
	/* Blizzard */
	{ 30,  0, 0,120, 3, 0, 0, 0 },
	{ 55,  0, 0,120, 4,48, 0,10 },
	/* Hell Storm */
	{ 35,  0, 0,150, 3, 0, 0, 0 },
	{ 65,  0, 0,150, 4,35, 0,10 },
	/* Elemental Storm */
	{ 40,  0, 0,180, 3, 0, 0, 0 },
	{ 75,  0, 0,180, 4,17, 0,10 }
};

spell_book books[SV_BOOK_MAX] = 
{
	{
		/* Elementary Incantations (sval 0) */
		SBF_MAGIC,
		{
			{ POW_BOLT_ACID_X,		"Acid Bolt",				 1, -1, 22 },
			{ POW_BOLT_ELEC_X,		"Lightning Bolt",			 1, -1, 22 },
			{ POW_TELE_10,			"Phase Door",  				 1,  1, 24 },
			{ POW_LIGHT_AREA,		"Light Area",				 1,  2, 26 },
			{ POW_BOLT_FIRE_X,		"Fire Bolt",				 2, -1, 22 },
			{ POW_BOLT_COLD_X,		"Frost Bolt",				 2, -1, 22 },
			{ POW_DETECT_MONSTERS,	"Detect Monsters",			 2,  2, 23 },
			{ POW_BALL_POISON,		"Stinking Cloud",			 3,  3, 27 },
			{ POW_DETECT_TRAP_DOOR,	"Find Traps, Doors & Stairs",3,  4, 25 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Sorcerous Adjurations (sval 1) */
		SBF_MAGIC,
		{
			{ POW_CONFUSE_MONSTER,	"Confuse Monster",			 4,  3, 26 },
			{ POW_SLEEP_MONSTER,	"Sleep Monster",			 4,  5, 26 },
			{ POW_INFRAVISION,		"Infravision",				 4,  4, 35 },
			{ POW_BOLT_MISSILE_X,	"Magic Missile",			 5, -2, 25 },
			{ POW_SHIELD,			"Shield",					 5,  7, 60 },
			{ POW_TELE_MAJOR,		"Teleport Self",			 7,  6, 35 },
			{ POW_BEAM_WEAK_LITE,	"Spear of Light",			 7,  7, 35 },
			{ POW_STONE_TO_MUD,		"Turn Stone to Mud",		 9,  7, 44 },
			{ POW_SATISFY_HUNGER,	"Satisfy Hunger",			 9,  8, 45 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Thaumaturgic Conjurations (sval 2) */
		SBF_MAGIC,
		{
			{ POW_SLEEP_ALL,		"Sleep All",				11,  9, 45 },
			{ POW_CREATE_DOOR,		"Create Doors",				11, 13, 35 },
			{ POW_SLOW_MONSTER,		"Slow Monster",				11,  9, 50 },
			{ POW_IDENTIFY,			"Identify Item",			11,  8, 75 },
			{ POW_BALL_FIRE_X,		"Fire Ball",				13, -3, 33 },
			{ POW_BALL_COLD_X,		"Frost Ball",				13, -3, 33 },
			{ POW_BALL_ACID_X,		"Acid Ball",				14, -3, 33 },
			{ POW_BALL_ELEC_X,		"Lightning Ball",			14, -3, 33 },
			{ POW_DETECT_ENCHANT,	"Detect Enchantment",		15, 10, 40 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{		
		/* Evocations of Power (sval 3) */
		SBF_MAGIC,
		{
			{ POW_GLYPH_LESSER,		"Barrier",					17, 35, 90 },
			{ POW_RECHARGE_1,		"Recharge Item",			17, 12, 75 },
			{ POW_BLIND_MONSTER,	"Blind Monster",			19, 12, 60 },
			{ POW_SLOW_ALL,			"Slow All",					19, 20, 33 },
			{ POW_MAGIC_LOCK,		"Magic Lock",				21, 25, 50 },
			{ POW_IDENTIFY_PACK,	"Identify Pack",			21, 24, 80 },
			{ POW_TELE_OTHER,		"Teleport Others",			23, 12, 60 },
			{ POW_EARTHQUAKE,		"Earthquake",				25, 18, 60 },
			{ POW_HASTE_SELF_1,		"Haste Self",				30, 12, 65 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Resistance of Scarabtarices (sval 4) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_RES_FIRE,			"Resist Fire",				 5,  5, 50 },
			{ POW_RES_COLD,			"Resist Cold",				 5,  5, 50 },
			{ POW_RES_ACID,			"Resist Acid",				 5,  5, 50 },
			{ POW_RES_ELEC,			"Resist Electricity",		 5,  5, 50 },
			{ POW_RES_POISON,		"Resist Poison",			10, 10, 75 },
			{ POW_RES_DISEASE,		"Resist Disease",			10, 10, 75 },
			{ POW_RES_LITE_DARK,	"Resist Light & Darkness",	12, 15, 80 },
			{ POW_RESISTANCE,		"Resistance",				20, 25, 85 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Mordenkainen's Escapes (sval 5) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_ABSORB_HIT,		"Absorb Hit",				 5,  5, 25 },
			{ POW_CREATE_STAIR,		"Create Stairs",			10, 15, 40 },
			{ POW_TELE_LEVEL,		"Teleport Level",			15, 17, 60 },
			{ POW_TELE_CONTROL,		"Dimension Door",			20, 25, 70 },
			{ POW_CREATE_WALL,		"Create Walls",				30, 40, 80 },
			{ POW_WORD_RECALL,		"Word of Recall",			35, 25, 75 },
			{ POW_DESTRUCTION,		"Word of Destruction",		40, 21, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Kelek's Grimoire of Power (sval 6) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_DETECT_EVIL,		"Detect Evil",				 5,  5, 50 },
			{ POW_BLIGHT,			"Blight",					10, 13, 60 },
			{ POW_POLY_MONSTER,		"Polymorph Other",			15,  7, 45 },
			{ POW_PROBE_MONSTER,	"Probe Monster",			20, 12, 80 },
			{ POW_GENOCIDE,			"Genocide",					35, 50, 70 },
			{ POW_MASS_GENOCIDE,	"Mass Genocide",			45, 75, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Raal's Tome of Destruction (sval 7) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_BOLT_POISON_X,	"Poison Bolt",				 5, -4, 50 },
			{ POW_BOLT_MANA_X,		"Mana Bolt",				10, -5, 50 },
			{ POW_BOLT_NEXUS_X,		"Nexus Bolt",				10, -5, 50 },
			{ POW_BALL_POISON_X,	"Cloud Kill",				15, -3, 60 },
			{ POW_BALL_COLD_ELEC_X,	"Blizzard",					25, -6, 85 },
			{ POW_BALL_FIRE_ACID_X,	"Hell Storm",				30, -7, 85 },
			{ POW_BALL_ELEM_X,		"Elemental Storm",			35, -8, 90 },
			{ POW_BALL_MANA,		"Mana Storm",				40, 50, 95 },
			{ POW_BALL_ANNIHILATION,"Sphere of Annihilation",	50, 75, 70 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Tenser's transformations (sval 8) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_HEROISM,			"Heroism",					 5,  5, 50 },
			{ POW_STABILITY,		"Stability",				10, 18, 35 },
			{ POW_INVIS_2,			"Temporary Invisibility",	20, 35, 60 },
			{ POW_RES_CHAOS_NEXUS,	"Resist Chaos & Nexus",		30, 25, 35 },
			{ POW_RESILIENCE,		"Globe of Resilience",		45, 70, 75 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Lore of the Hunter (sval 9) */
		(SBF_MAGIC | SBF_GOOD),
		{
			{ POW_BRAND_ARROW_ANML,	"Hunter's Arrows",			18, 25, 60 },
			{ POW_CALM_ANIMALS,		"Calm Animals",				23, 35, 55 },
			{ POW_BRAND_ARROW_WOUND,"Sharpen Arrows",			28, 25, 70 },
			{ POW_HASTE_SELF_1,		"Haste Self",				33, 12, 90 },
			{ POW_BRAND_ARROW_ELMNT,"Elemental Arrows",			43, 40, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Novice's Handbook (sval 10) */
		SBF_PRAYER,
		{
			{ POW_DETECT_EVIL,		"Detect Evil",				 1,  1, 10 },
			{ POW_HEAL_1,			"Cure Light Wounds",		 1,  2, 15 },
			{ POW_BLESS_1,			"Bless",					 1,  2, 20 },
			{ POW_CURE_FEAR,		"Remove Fear",				 1,  2, 25 },
			{ POW_LIGHT_AREA,		"Call Light",				 3,  2, 25 },
			{ POW_DETECT_TRAP,		"Find Traps",				 3,  3, 27 },
			{ POW_DETECT_DOOR_STAIR,"Detect Doors/Stairs",		 3,  3, 27 },
			{ POW_CURE_POISON_1,	"Slow Poison",				 3,  3, 28 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Words of Wisdom (sval 11) */
		SBF_PRAYER,
		{
			{ POW_SCARE_MONSTER,	"Scare Monster",			 5,  4, 29 },
			{ POW_TELE_MINOR,		"Portal",					 5,  4, 30 },
			{ POW_HEAL_2,			"Cure Serious Wounds",		 5,  4, 32 },
			{ POW_BLESS_2,			"Chant",					 5,  5, 34 },
			{ POW_SLEEP_ADJACENT,	"Sanctuary",				 7,  5, 36 },
			{ POW_SATISFY_HUNGER,	"Satisfy Hunger",			 7,  5, 38 },
			{ POW_REMOVE_CURSE_1,	"Remove Curse",				 7,  6, 38 },
			{ POW_RES_FIRE_COLD,	"Resist Heat and Cold",		 7,  7, 38 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Chants and Blessings (sval 12) */
		SBF_PRAYER,
		{
			{ POW_CURE_POISON_2,	"Neutralize Poison",		 9,  6, 38 },
			{ POW_BALL_HOLY,		"Orb of Draining",			 9,  7, 38 },
			{ POW_HEAL_3,			"Cure Critical Wounds",		 9,  7, 40 },
			{ POW_CURE_DISEASE,		"Cure Disease",				10,  7, 40 },
			{ POW_GLYPH_HOLY,		"Holy Sigil",				10, 15, 50 },
			{ POW_SEE_INVIS,		"Sense Invisible",			11,  8, 42 },
			{ POW_PROT_EVIL,		"Protection from Evil",		11,  8, 42 },
			{ POW_EARTHQUAKE,		"Earthquake",				13,  9, 55 },
			{ POW_MAP_1,			"Sense Surroundings",		13, 10, 45 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
 		/* Exorcism and Dispelling (sval 13) */
		SBF_PRAYER,
		{
			{ POW_SCARE_UNDEAD,		"Turn Undead",				15, 12, 50 },
			{ POW_BLESS_3,			"Prayer",					15, 14, 50 },
			{ POW_DISPEL_UNDEAD_1,	"Dispel Undead",			17, 14, 55 },
			{ POW_DISPEL_EVIL_3,	"Dispel Evil",				25, 20, 70 },
			{ POW_GLYPH_WARDING,	"Glyph of Warding",			33, 55, 90 },
			{ POW_HOLY_2,			"Holy Word",				39, 32, 95 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Ethereal openings (sval 14) */
		(SBF_PRAYER | SBF_GOOD),
		{ 
			{ POW_TELE_10,			"Blink",					 3,  3, 50 },
			{ POW_TELE_MAJOR,		"Teleport Self",			10, 10, 50 },
			{ POW_TELE_OTHER,		"Teleport Other",			20, 20, 80 },
			{ POW_TELE_LEVEL,		"Teleport Level",			30, 40, 75 },
			{ POW_WORD_RECALL,		"Word of Recall",			35, 50, 75 },
			{ POW_ALTER_REALITY,	"Alter Reality",			40, 60, 75 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Godly Insights (sval 15) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_DETECT_MONSTERS,	"Detect Monsters",			 3,  3, 50 },
			{ POW_DETECT_ALL,		"Detection",				10,  8, 80 },
			{ POW_IDENTIFY,			"Perception",				20, 20, 80 },
			{ POW_PROBE_ALL,		"Probing",					25, 20, 80 },
			{ POW_MAP_2,			"Clairvoyance",				35, 50, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Purifications and Healing (sval 16) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_HEAL_4,			"Cure Mortal Wounds",		17,  7, 60 },
			{ POW_HEAL_5,			"Healing",					30, 50, 80 },
			{ POW_RESTORE_STATS,	"Restoration",				35, 70, 90 },
			{ POW_RESTORE_LEVEL,	"Remembrance",				35, 70, 90 },
			{ POW_CALM_NON_EVIL,	"Holy Peace",				45, 50, 80 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Holy Infusions (sval 17) */
		(SBF_PRAYER | SBF_GOOD),
		{
			{ POW_DEST_TRAP_DOOR_2,	"Unbarring Ways",			 5,  6, 50 },
			{ POW_RECHARGE_2,		"Recharging",				15, 20, 80 },
			{ POW_REMOVE_CURSE_2,	"Dispel Curse",				25, 40, 80 },
			{ POW_ENCHANT_WEAPON,	"Enchant Weapon",			35, 50, 80 },
			{ POW_ENCHANT_ARMOR_2,	"Enchant Armour",			37, 60, 85 },
			{ POW_BRAND_SHOT_HOLY,	"Sanctify Shots",			45, 95, 85 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* Wrath of God (sval 18) */
		(SBF_PRAYER | SBF_GOOD),
		{  
			{ POW_DISPEL_UNDEAD_2,	"Dispel Undead",			15,  7, 70 },
			{ POW_DISPEL_EVIL_4,	"Dispel Evil",				20, 10, 75 },
			{ POW_BANISH,			"Banishment",				25, 25, 80 },
			{ POW_DESTRUCTION,		"Word of Destruction",		35, 35, 80 },
			{ POW_DRAIN_LIFE_3,		"Annihilation",				45, 60, 75 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		0,
		{
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Seven steps to transcendence (sval 20) */
		(SBF_MYSTIC | SBF_GOOD),
		{
			{ POW_CLEAR_MIND,		"Clarity of Mind",			 5,  5, 10 },
			{ POW_TELE_CONTROL,		"Mastery of Space",			10, 25, 60 },
			{ POW_RES_ELEMENTS,		"Power over Elements",		15, 30, 70 },
			{ POW_DETECT_ALL,		"Purity of vision",			20, 35, 80 },
			{ POW_CURE_BODY,		"Control of the Body",		30, 40, 90 },
			{ POW_RES_GREATER,		"One with the world",		40, 45,100 },
			{ POW_RESILIENCE,		"Mind over Matter",			50, 50,110 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Teachings of the Ninth Master (sval 21) */
		(SBF_MYSTIC | SBF_GOOD),
		{
			{ POW_POLY_MONSTER,		"Change Other",				 5, 10, 45 },
			{ POW_BOLT_MANA,		"Mana Bolt",				10,  5, 50 },
			{ POW_INVIS_2,			"Vanish from Sight",		20, 20, 60 },
			{ POW_BANISH,			"Banish Enemies",			25, 30, 80 },
			{ POW_HEAL_4,			"Heal",						30, 20, 60 },
			{ POW_HASTE_SELF_2,		"Essence of Speed",			35, 60, 50 },
			{ POW_ALTER_REALITY,	"Alter Reality",			40, 80, 75 },
			{ POW_GLYPH_WARDING,	"Glyph of Warding",			45, 60, 90 },
			{ POW_BURST_ASTRAL,		"Astral Burst",				50, 80, 50 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
			/* The Necronomicon (sval 22) */
		(SBF_NECRONOM | SBF_GOOD | SBF_ARTIFACT),
		{ 
			{ POW_DARK_AREA,		"Unlight Area",				 1,  1, 15 },
			{ POW_DISPEL_NON_EVIL,	"Wave of Evil",				25, 15,132 },
			{ POW_BEAM_NETHER,		"Ray of Evil",				40, 50, 97 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }
		}
	},
	{
		/* The Codex of Ultimate Wisdom (sval 23) */
		(SBF_CODEX | SBF_GOOD | SBF_ARTIFACT),
		{
			{ POW_IDENTIFY,			"Perception",				 1, 15, 50 },
			{ POW_IDENTIFY_PACK,	"Greater Perception",		20, 20, 90 },
			{ POW_IDENTIFY_FULL,	"Revelation",				50,100, 72 },
			{ POW_MAP_2,			"Clairvoyance",				50,100, 10 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 },
			{ 0, NULL, 99,  0, 0 }, { 0, NULL, 99,  0, 0 }
		}
	}
};

/*
 * Resistance names 
 */
cptr resist_names[RS_MAX] =
{
	"acid",
	"electricty",
	"fire",
	"cold",
	"water",
	"poison",
	"disease",
	"light",
	"dark",
	"confusion",
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
	"Confu",
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
	"STR: ", "INT: ", "WIS: ", "DEX: ", "CON: ", "CHR: "
};

/*
 * Abbreviations of damaged stats
 */
cptr stat_names_reduced[A_MAX] =
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
	"Display room descriptions",
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
	{"run_ignore_stairs",	"When running, ignore stairs",				TRUE },
	{"run_ignore_doors",	"When running, ignore doors",				TRUE },
	{"run_cut_corners",		"When running, cut corners",				TRUE },
	{"disturb_move",		"Disturb whenever any monster moves",		TRUE },
	{"disturb_near",		"Disturb whenever viewable monster moves",	TRUE },
	{"disturb_panel",		"Disturb whenever map panel changes",		TRUE },
	{"disturb_state",		"Disturb whenever player state changes",	TRUE },
	{"disturb_minor",		"Disturb whenever boring things happen",	TRUE },
	{"alert_failure",		"Alert user to various failures",			FALSE},
	{"verify_destroy",		"Verify destruction of objects",			TRUE },
	{"use_command",			"Allow unified use command",				FALSE},
	{"expand_look",			"Expand the power of the look command",		TRUE },
	{"expand_list",			"Expand the power of the list commands",	TRUE },
	{"view_perma_grids",	"Map remembers all perma-lit grids",		TRUE },
	{"view_torch_grids",	"Map remembers all torch-lit grids",		FALSE},
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
	{"show_piles",			"Show stacks using special attr/char",		FALSE},
	{"center_player",		"Center map continuously",					FALSE},
	{"run_avoid_center",	"Avoid centering while running",			FALSE},
	{"scroll_target",		"Scroll map while targetting",				TRUE },
	{"auto_more",			"Automatically clear '-more-' prompts",		FALSE},
	{"view_monster_lite",	"Allow monsters to have light radius",		TRUE },
	{"verify_leave_quest",	"Verify before descending from quest level",TRUE },
	{"auto_haggle",			"Auto-haggle in stores",					TRUE },
	{"display_room_desc",	"Display room descriptions",				FALSE},
	{"display_insc_msg",	"Display inscriptions in messages",			FALSE},
	{"display_recharge_msg","Display messages upon recharge",			TRUE },
	{"inscribe_unique",		"Auto-inscribe unique drops",				FALSE},
	{"spellbook_menu",		"Alternate spellbook interface",			FALSE},
	{"trap_under_object",	"Traps are hidden by objects",				TRUE }
};

option_type options_birth[OPT_BIRTH] =
{
	{"birth_point_based",		"Allow purchase of stats using points",		FALSE},
	{"birth_auto_roller",		"Allow specification of minimal stats",		FALSE},
	{"birth_preserve",			"Preserve artifacts when leaving level",	TRUE },
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
	{"birth_random_hp",			"Generate hitpoints randomly",				TRUE },
	{"birth_force_small_lev",	"All levels will be generated as small",	FALSE},
	{"birth_easy_mode",			"Easy mode",								FALSE},
	{"birth_nightmare_mode",	"Nightmare mode",							FALSE},
	{"birth_retain_squelch",	"Retain squelch settings",					FALSE}
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
	{"squelch_junk",		"Squelch anything worth 0 gold",			TRUE},
	{"auto_squelch",		"Automatically destroy squelched items",	FALSE}
};

/*
 * Option screen interface
 *
 * Note the special significance given to the constant "255".
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
		OPT_use_old_target,
		OPT_always_pickup,
		OPT_always_repeat,
		OPT_verify_destroy,
		OPT_easy_direction,
		OPT_easy_alter,
		OPT_easy_floor,
		OPT_verify_leave_quest,
		OPT_spellbook_menu,
		255
	},

	/*** Display ***/

	{
		OPT_depth_in_feet,
		OPT_show_labels,
		OPT_show_weights,
		OPT_show_flavors,
		OPT_show_piles,
		OPT_display_insc_msg,
		OPT_hilite_player,
		OPT_view_yellow_lite,
		OPT_view_bright_lite,
		OPT_view_granite_lite,
		OPT_view_special_lite,
		OPT_view_monster_lite,
		OPT_trap_under_object,
		255,
		255,
		255
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
		OPT_alert_failure,
		OPT_display_recharge_msg,
		OPT_auto_more,
		OPT_ring_bell,
		255,
		255,
		255,
		255
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
		OPT_display_room_desc,
#ifdef ALLOW_HAGGLE
		OPT_auto_haggle,
#else /* ALLOW_HAGGLE */
		255,
#endif /* ALLOW_HAGGLE */
		OPT_inscribe_unique,
		255,
		255,
		255,
		255
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
		255,
		255,
		255,
		255,
		255,
		255,
	},

	/*** Cheat ***/

	{
#ifdef ALLOW_DEBUG
		OPT_cheat_debug,
#else /* ALLOW_DEBUG */
		255,
#endif /* ALLOW_DEBUG */
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_know,
		OPT_cheat_live,
		OPT_cheat_no_save,
		OPT_cheat_no_respawn,
		OPT_cheat_wizard,
		255,
		255,
		255,
		255,
		255,
		255,
		255
	},

	/*** Birth ***/

	{
		OPT_birth_point_based,
		OPT_birth_auto_roller,
		OPT_birth_random_hp,
		OPT_birth_start_kit,
		OPT_birth_retain_squelch,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255
	},

	/*** Difficulty ***/

	{
		OPT_birth_easy_mode,
		OPT_birth_nightmare_mode,
		OPT_birth_autoscum,
		OPT_birth_preserve,
		OPT_birth_ironman,
		OPT_birth_no_stores,
		OPT_birth_no_feelings,
		OPT_birth_no_artifacts,
		OPT_birth_force_small_lev,
		255,
		255,
		255,
		255,
		255,
		255,
		255
	},

	/*** Monster AI ***/

	{
		OPT_birth_smart_packs,
		OPT_birth_smart_cheat,
#ifdef MONSTER_FLOW
		OPT_birth_flow_by_sound,
		OPT_birth_flow_by_smell,
#else /* MONSTER_FLOW */
		255,
		255,
#endif /* MONSTER_FLOW */
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255,
		255
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
