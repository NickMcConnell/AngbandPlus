/* File: tables.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
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

#include "angband.h"




/*
 * Global array for looping through the "keypad directions".
 */
const s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/*
 * Global arrays for converting "keypad direction" into "offsets".
 */
const s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

const s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]".
 */
const s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

const s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };


/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
const char hexsym[16] =
{
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};

const byte moria_class_level_adj[MORIA_MAX_CLASS][MORIA_MAX_LEV_ADJ] =
{
/*	       bth    bthb   device  disarm   save/misc hit  */
/* Warrior */ {	4,	4,	2,	2,	3 },
/* Mage    */ { 2,	2,	4,	3,	3 },
/* Priest  */ { 2,	2,	4,	3,	3 },
/* Rogue   */ { 3,	4,	3,	4,	3 },
/* Ranger  */ { 3,	4,	3,	3,	3 },
/* Paladin */ { 3,	3,	3,	2,	3 }
};

/* used to calculate the number of blows the player gets in combat */
const byte moria_blows_table[MORIA_MAX_STR_ADJ][MORIA_MAX_DEX_ADJ] =
{
/* STR/W:	   9  18  67 107 117 118   : DEX */
/* <2 */	{  1,  1,  1,  1,  1,  1 },
/* <3 */	{  1,  1,  1,  1,  2,  2 },
/* <4 */	{  1,  1,  1,  2,  2,  3 },
/* <5 */	{  1,  1,  2,  2,  3,  3 },
/* <7 */	{  1,  2,  2,  3,  3,  4 },
/* <9 */	{  1,  2,  2,  3,  4,  4 },
/* >9 */	{  2,  2,  3,  3,  4,  4 }
};


/*
 * Stat Table (INT/WIS) -- Number of half-spells per level
 */
const int adj_mag_study[] =
{
	  0	/* 3 */,
	  0	/* 4 */,
	 10	/* 5 */,
	 20	/* 6 */,
	 30	/* 7 */,
	 40	/* 8 */,
	 50	/* 9 */,
	 60	/* 10 */,
	 70	/* 11 */,
	 80	/* 12 */,
	 85	/* 13 */,
	 90	/* 14 */,
	 95	/* 15 */,
	100	/* 16 */,
	105	/* 17 */,
	110	/* 18/00-18/09 */,
	115	/* 18/10-18/19 */,
	120	/* 18/20-18/29 */,
	130	/* 18/30-18/39 */,
	140	/* 18/40-18/49 */,
	150	/* 18/50-18/59 */,
	160	/* 18/60-18/69 */,
	170	/* 18/70-18/79 */,
	180	/* 18/80-18/89 */,
	190	/* 18/90-18/99 */,
	200	/* 18/100-18/109 */,
	210	/* 18/110-18/119 */,
	220	/* 18/120-18/129 */,
	230	/* 18/130-18/139 */,
	240	/* 18/140-18/149 */,
	250	/* 18/150-18/159 */,
	250	/* 18/160-18/169 */,
	250	/* 18/170-18/179 */,
	250	/* 18/180-18/189 */,
	250	/* 18/190-18/199 */,
	250	/* 18/200-18/209 */,
	250	/* 18/210-18/219 */,
	250	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- extra half-mana-points per level (divided by 100)
 */
const int adj_mag_mana[] =
{
	  0	/* 3 */,
	 10	/* 4 */,
	 20	/* 5 */,
	 30	/* 6 */,
	 40	/* 7 */,
	 50	/* 8 */,
	 60	/* 9 */,
	 70	/* 10 */,
	 80	/* 11 */,
	 90	/* 12 */,
	100	/* 13 */,
	110	/* 14 */,
	120	/* 15 */,
	130	/* 16 */,
	140	/* 17 */,
	150	/* 18/00-18/09 */,
	160	/* 18/10-18/19 */,
	170	/* 18/20-18/29 */,
	180	/* 18/30-18/39 */,
	190	/* 18/40-18/49 */,
	200	/* 18/50-18/59 */,
	225	/* 18/60-18/69 */,
	250	/* 18/70-18/79 */,
	300	/* 18/80-18/89 */,
	350	/* 18/90-18/99 */,
	400	/* 18/100-18/109 */,
	450	/* 18/110-18/119 */,
	500	/* 18/120-18/129 */,
	550	/* 18/130-18/139 */,
	600	/* 18/140-18/149 */,
	650	/* 18/150-18/159 */,
	700	/* 18/160-18/169 */,
	750	/* 18/170-18/179 */,
	800	/* 18/180-18/189 */,
	800	/* 18/190-18/199 */,
	800	/* 18/200-18/209 */,
	800	/* 18/210-18/219 */,
	800	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- Minimum failure rate (percentage)
 */
const byte adj_mag_fail[] =
{
	99	/* 3 */,
	99	/* 4 */,
	99	/* 5 */,
	99	/* 6 */,
	99	/* 7 */,
	50	/* 8 */,
	30	/* 9 */,
	20	/* 10 */,
	15	/* 11 */,
	12	/* 12 */,
	11	/* 13 */,
	10	/* 14 */,
	9	/* 15 */,
	8	/* 16 */,
	7	/* 17 */,
	6	/* 18/00-18/09 */,
	6	/* 18/10-18/19 */,
	5	/* 18/20-18/29 */,
	5	/* 18/30-18/39 */,
	5	/* 18/40-18/49 */,
	4	/* 18/50-18/59 */,
	4	/* 18/60-18/69 */,
	4	/* 18/70-18/79 */,
	4	/* 18/80-18/89 */,
	3	/* 18/90-18/99 */,
	3	/* 18/100-18/109 */,
	2	/* 18/110-18/119 */,
	2	/* 18/120-18/129 */,
	2	/* 18/130-18/139 */,
	2	/* 18/140-18/149 */,
	1	/* 18/150-18/159 */,
	1	/* 18/160-18/169 */,
	1	/* 18/170-18/179 */,
	1	/* 18/180-18/189 */,
	1	/* 18/190-18/199 */,
	0	/* 18/200-18/209 */,
	0	/* 18/210-18/219 */,
	0	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- failure rate adjustment
 */
const int adj_mag_stat[] =
{
	-5	/* 3 */,
	-4	/* 4 */,
	-3	/* 5 */,
	-3	/* 6 */,
	-2	/* 7 */,
	-1	/* 8 */,
	 0	/* 9 */,
	 0	/* 10 */,
	 0	/* 11 */,
	 0	/* 12 */,
	 0	/* 13 */,
	 1	/* 14 */,
	 2	/* 15 */,
	 3	/* 16 */,
	 4	/* 17 */,
	 5	/* 18/00-18/09 */,
	 6	/* 18/10-18/19 */,
	 7	/* 18/20-18/29 */,
	 8	/* 18/30-18/39 */,
	 9	/* 18/40-18/49 */,
	10	/* 18/50-18/59 */,
	11	/* 18/60-18/69 */,
	12	/* 18/70-18/79 */,
	15	/* 18/80-18/89 */,
	18	/* 18/90-18/99 */,
	21	/* 18/100-18/109 */,
	24	/* 18/110-18/119 */,
	27	/* 18/120-18/129 */,
	30	/* 18/130-18/139 */,
	33	/* 18/140-18/149 */,
	36	/* 18/150-18/159 */,
	39	/* 18/160-18/169 */,
	42	/* 18/170-18/179 */,
	45	/* 18/180-18/189 */,
	48	/* 18/190-18/199 */,
	51	/* 18/200-18/209 */,
	54	/* 18/210-18/219 */,
	57	/* 18/220+ */
};


/*
 * Stat Table (CHR) -- payment percentages
 */
const byte adj_chr_gold[] =
{
	143	/* 3 */,
	137	/* 4 */,
	134	/* 5 */,
	132	/* 6 */,
	129	/* 7 */,
	127	/* 8 */,
	123	/* 9 */,
	122	/* 10 */,
	121	/* 11 */,
	118	/* 12 */,
	116	/* 13 */,
	113	/* 14 */,
	113	/* 15 */,
	112	/* 16 */,
	111	/* 17 */,
	110	/* 18/00-18/09 */,
	108	/* 18/10-18/19 */,
	107	/* 18/20-18/29 */,
	106	/* 18/30-18/39 */,
	105	/* 18/40-18/49 */,
	104	/* 18/50-18/59 */,
	103	/* 18/60-18/69 */,
	102	/* 18/70-18/79 */,
	101	/* 18/80-18/89 */,
	100	/* 18/90-18/99 */,
	99	/* 18/100-18/109 */,
	97	/* 18/110-18/119 */,
	96	/* 18/120-18/129 */,
	95	/* 18/130-18/139 */,
	94	/* 18/140-18/149 */,
	93	/* 18/150-18/159 */,
	92	/* 18/160-18/169 */,
	91	/* 18/170-18/179 */,
	90	/* 18/180-18/189 */,
	90	/* 18/190-18/199 */,
	90	/* 18/200-18/209 */,
	90	/* 18/210-18/219 */,
	90	/* 18/220+ */
};

/*
 * Stat Table (CHR) -- bonus to charm
 *
 * Boosted the bonuses significantly
 * as a part in making charms better. -AR
 * Notice the unlinear progression
 * of the bonuses.
 */
const s16b adj_chr_charm[] =
{
	- 10/* 3 */,
	- 8	/* 4 */,
	- 6	/* 5 */,
	- 3	/* 6 */,
	- 2	/* 7 */,
	- 1	/* 8 */,
	0	/* 9 */,
	0	/* 10 */,
	0	/* 11 */,
	0	/* 12 */,
	0	/* 13 */,
	0	/* 14 */,
	0	/* 15 */,
	1	/* 16 */,
	1	/* 17 */,
	2	/* 18/00-18/09 */,
	2	/* 18/10-18/19 */,
	3	/* 18/20-18/29 */,
	3	/* 18/30-18/39 */,
	4	/* 18/40-18/49 */,
	4	/* 18/50-18/59 */,
	5	/* 18/60-18/69 */,
	5	/* 18/70-18/79 */,
	6	/* 18/80-18/89 */,
	6	/* 18/90-18/99 */,
	7	/* 18/100-18/109 */,
	7	/* 18/110-18/119 */,
	8	/* 18/120-18/129 */,
	8	/* 18/130-18/139 */,
	9	/* 18/140-18/149 */,
	9	/* 18/150-18/159 */,
	10	/* 18/160-18/169 */,
	10	/* 18/170-18/179 */,
	11	/* 18/180-18/189 */,
	11	/* 18/190-18/199 */,
	12	/* 18/200-18/209 */,
	13	/* 18/210-18/219 */,
	15	/* 18/220+ */
};


/*
 * Stat Table (INT) -- Magic devices
 */
const byte adj_int_dev[] =
{
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	0	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	1	/* 12 */,
	1	/* 13 */,
	1	/* 14 */,
	2	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
	3	/* 18/00-18/09 */,
	3	/* 18/10-18/19 */,
	4	/* 18/20-18/29 */,
	4	/* 18/30-18/39 */,
	5	/* 18/40-18/49 */,
	5	/* 18/50-18/59 */,
	6	/* 18/60-18/69 */,
	6	/* 18/70-18/79 */,
	7	/* 18/80-18/89 */,
	7	/* 18/90-18/99 */,
	8	/* 18/100-18/109 */,
	9	/* 18/110-18/119 */,
	10	/* 18/120-18/129 */,
	11	/* 18/130-18/139 */,
	12	/* 18/140-18/149 */,
	13	/* 18/150-18/159 */,
	14	/* 18/160-18/169 */,
	15	/* 18/170-18/179 */,
	16	/* 18/180-18/189 */,
	17	/* 18/190-18/199 */,
	18	/* 18/200-18/209 */,
	19	/* 18/210-18/219 */,
	20	/* 18/220+ */
};


/*
 * Stat Table (WIS) -- Saving throw
 */
const byte adj_wis_sav[] =
{
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	0	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	1	/* 12 */,
	1	/* 13 */,
	1	/* 14 */,
	2	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
	3	/* 18/00-18/09 */,
	3	/* 18/10-18/19 */,
	3	/* 18/20-18/29 */,
	3	/* 18/30-18/39 */,
	3	/* 18/40-18/49 */,
	4	/* 18/50-18/59 */,
	4	/* 18/60-18/69 */,
	5	/* 18/70-18/79 */,
	5	/* 18/80-18/89 */,
	6	/* 18/90-18/99 */,
	7	/* 18/100-18/109 */,
	8	/* 18/110-18/119 */,
	9	/* 18/120-18/129 */,
	10	/* 18/130-18/139 */,
	11	/* 18/140-18/149 */,
	12	/* 18/150-18/159 */,
	13	/* 18/160-18/169 */,
	14	/* 18/170-18/179 */,
	15	/* 18/180-18/189 */,
	16	/* 18/190-18/199 */,
	17	/* 18/200-18/209 */,
	18	/* 18/210-18/219 */,
	19	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- disarming
 */
const byte adj_dex_dis[] =
{
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
	1	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
	4	/* 18/00-18/09 */,
	4	/* 18/10-18/19 */,
	4	/* 18/20-18/29 */,
	4	/* 18/30-18/39 */,
	5	/* 18/40-18/49 */,
	5	/* 18/50-18/59 */,
	5	/* 18/60-18/69 */,
	6	/* 18/70-18/79 */,
	6	/* 18/80-18/89 */,
	7	/* 18/90-18/99 */,
	8	/* 18/100-18/109 */,
	8	/* 18/110-18/119 */,
	8	/* 18/120-18/129 */,
	8	/* 18/130-18/139 */,
	8	/* 18/140-18/149 */,
	9	/* 18/150-18/159 */,
	9	/* 18/160-18/169 */,
	9	/* 18/170-18/179 */,
	9	/* 18/180-18/189 */,
	9	/* 18/190-18/199 */,
	10	/* 18/200-18/209 */,
	10	/* 18/210-18/219 */,
	10	/* 18/220+ */
};


/*
 * Stat Table (INT) -- disarming
 */
const byte adj_int_dis[] =
{
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	0	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	1	/* 12 */,
	1	/* 13 */,
	1	/* 14 */,
	2	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
	3	/* 18/00-18/09 */,
	3	/* 18/10-18/19 */,
	3	/* 18/20-18/29 */,
	4	/* 18/30-18/39 */,
	4	/* 18/40-18/49 */,
	5	/* 18/50-18/59 */,
	6	/* 18/60-18/69 */,
	7	/* 18/70-18/79 */,
	8	/* 18/80-18/89 */,
	9	/* 18/90-18/99 */,
	10	/* 18/100-18/109 */,
	10	/* 18/110-18/119 */,
	11	/* 18/120-18/129 */,
	12	/* 18/130-18/139 */,
	13	/* 18/140-18/149 */,
	14	/* 18/150-18/159 */,
	15	/* 18/160-18/169 */,
	16	/* 18/170-18/179 */,
	17	/* 18/180-18/189 */,
	18	/* 18/190-18/199 */,
	19	/* 18/200-18/209 */,
	19	/* 18/210-18/219 */,
	19	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- bonus to ac (plus 128)
 */
const byte adj_dex_ta[] =
{
	128 + -4	/* 3 */,
	128 + -3	/* 4 */,
	128 + -2	/* 5 */,
	128 + -1	/* 6 */,
	128 + 0	/* 7 */,
	128 + 0	/* 8 */,
	128 + 0	/* 9 */,
	128 + 0	/* 10 */,
	128 + 0	/* 11 */,
	128 + 0	/* 12 */,
	128 + 0	/* 13 */,
	128 + 0	/* 14 */,
	128 + 1	/* 15 */,
	128 + 1	/* 16 */,
	128 + 1	/* 17 */,
	128 + 2	/* 18/00-18/09 */,
	128 + 2	/* 18/10-18/19 */,
	128 + 2	/* 18/20-18/29 */,
	128 + 2	/* 18/30-18/39 */,
	128 + 2	/* 18/40-18/49 */,
	128 + 3	/* 18/50-18/59 */,
	128 + 3	/* 18/60-18/69 */,
	128 + 3	/* 18/70-18/79 */,
	128 + 4	/* 18/80-18/89 */,
	128 + 5	/* 18/90-18/99 */,
	128 + 6	/* 18/100-18/109 */,
	128 + 7	/* 18/110-18/119 */,
	128 + 8	/* 18/120-18/129 */,
	128 + 9	/* 18/130-18/139 */,
	128 + 9	/* 18/140-18/149 */,
	128 + 10	/* 18/150-18/159 */,
	128 + 11	/* 18/160-18/169 */,
	128 + 12	/* 18/170-18/179 */,
	128 + 13	/* 18/180-18/189 */,
	128 + 14	/* 18/190-18/199 */,
	128 + 15	/* 18/200-18/209 */,
	128 + 15	/* 18/210-18/219 */,
	128 + 15	/* 18/220+ */
};


/*
 * Stat Table (STR) -- bonus to dam (plus 128)
 */
const byte adj_str_td[] =
{
	128 + -2	/* 3 */,
	128 + -2	/* 4 */,
	128 + -1	/* 5 */,
	128 + -1	/* 6 */,
	128 + 0	/* 7 */,
	128 + 0	/* 8 */,
	128 + 0	/* 9 */,
	128 + 0	/* 10 */,
	128 + 0	/* 11 */,
	128 + 0	/* 12 */,
	128 + 0	/* 13 */,
	128 + 0	/* 14 */,
	128 + 0	/* 15 */,
	128 + 1	/* 16 */,
	128 + 2	/* 17 */,
	128 + 2	/* 18/00-18/09 */,
	128 + 2	/* 18/10-18/19 */,
	128 + 3	/* 18/20-18/29 */,
	128 + 3	/* 18/30-18/39 */,
	128 + 3	/* 18/40-18/49 */,
	128 + 3	/* 18/50-18/59 */,
	128 + 3	/* 18/60-18/69 */,
	128 + 4	/* 18/70-18/79 */,
	128 + 5	/* 18/80-18/89 */,
	128 + 5	/* 18/90-18/99 */,
	128 + 6	/* 18/100-18/109 */,
	128 + 7	/* 18/110-18/119 */,
	128 + 8	/* 18/120-18/129 */,
	128 + 9	/* 18/130-18/139 */,
	128 + 10	/* 18/140-18/149 */,
	128 + 11	/* 18/150-18/159 */,
	128 + 12	/* 18/160-18/169 */,
	128 + 13	/* 18/170-18/179 */,
	128 + 14	/* 18/180-18/189 */,
	128 + 15	/* 18/190-18/199 */,
	128 + 16	/* 18/200-18/209 */,
	128 + 18	/* 18/210-18/219 */,
	128 + 20	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- bonus to hit (plus 128)
 */
const byte adj_dex_th[] =
{
	128 + -3	/* 3 */,
	128 + -2	/* 4 */,
	128 + -2	/* 5 */,
	128 + -1	/* 6 */,
	128 + -1	/* 7 */,
	128 + 0	/* 8 */,
	128 + 0	/* 9 */,
	128 + 0	/* 10 */,
	128 + 0	/* 11 */,
	128 + 0	/* 12 */,
	128 + 0	/* 13 */,
	128 + 0	/* 14 */,
	128 + 0	/* 15 */,
	128 + 1	/* 16 */,
	128 + 2	/* 17 */,
	128 + 3	/* 18/00-18/09 */,
	128 + 3	/* 18/10-18/19 */,
	128 + 3	/* 18/20-18/29 */,
	128 + 3	/* 18/30-18/39 */,
	128 + 3	/* 18/40-18/49 */,
	128 + 4	/* 18/50-18/59 */,
	128 + 4	/* 18/60-18/69 */,
	128 + 4	/* 18/70-18/79 */,
	128 + 4	/* 18/80-18/89 */,
	128 + 5	/* 18/90-18/99 */,
	128 + 6	/* 18/100-18/109 */,
	128 + 7	/* 18/110-18/119 */,
	128 + 8	/* 18/120-18/129 */,
	128 + 9	/* 18/130-18/139 */,
	128 + 9	/* 18/140-18/149 */,
	128 + 10	/* 18/150-18/159 */,
	128 + 11	/* 18/160-18/169 */,
	128 + 12	/* 18/170-18/179 */,
	128 + 13	/* 18/180-18/189 */,
	128 + 14	/* 18/190-18/199 */,
	128 + 15	/* 18/200-18/209 */,
	128 + 15	/* 18/210-18/219 */,
	128 + 15	/* 18/220+ */
};


/*
 * Stat Table (STR) -- bonus to hit (plus 128)
 */
const byte adj_str_th[] =
{
	128 + -3	/* 3 */,
	128 + -2	/* 4 */,
	128 + -1	/* 5 */,
	128 + -1	/* 6 */,
	128 + 0	/* 7 */,
	128 + 0	/* 8 */,
	128 + 0	/* 9 */,
	128 + 0	/* 10 */,
	128 + 0	/* 11 */,
	128 + 0	/* 12 */,
	128 + 0	/* 13 */,
	128 + 0	/* 14 */,
	128 + 0	/* 15 */,
	128 + 0	/* 16 */,
	128 + 0	/* 17 */,
	128 + 1	/* 18/00-18/09 */,
	128 + 1	/* 18/10-18/19 */,
	128 + 1	/* 18/20-18/29 */,
	128 + 1	/* 18/30-18/39 */,
	128 + 1	/* 18/40-18/49 */,
	128 + 1	/* 18/50-18/59 */,
	128 + 1	/* 18/60-18/69 */,
	128 + 2	/* 18/70-18/79 */,
	128 + 3	/* 18/80-18/89 */,
	128 + 4	/* 18/90-18/99 */,
	128 + 5	/* 18/100-18/109 */,
	128 + 6	/* 18/110-18/119 */,
	128 + 7	/* 18/120-18/129 */,
	128 + 8	/* 18/130-18/139 */,
	128 + 9	/* 18/140-18/149 */,
	128 + 10	/* 18/150-18/159 */,
	128 + 11	/* 18/160-18/169 */,
	128 + 12	/* 18/170-18/179 */,
	128 + 13	/* 18/180-18/189 */,
	128 + 14	/* 18/190-18/199 */,
	128 + 15	/* 18/200-18/209 */,
	128 + 15	/* 18/210-18/219 */,
	128 + 15	/* 18/220+ */
};


/*
 * Stat Table (STR) -- weight limit in deca-pounds
 */
const byte adj_str_wgt[] =
{
	5	/* 3 */,
	6	/* 4 */,
	7	/* 5 */,
	8	/* 6 */,
	9	/* 7 */,
	10	/* 8 */,
	11	/* 9 */,
	12	/* 10 */,
	13	/* 11 */,
	14	/* 12 */,
	15	/* 13 */,
	16	/* 14 */,
	17	/* 15 */,
	18	/* 16 */,
	19	/* 17 */,
	20	/* 18/00-18/09 */,
	22	/* 18/10-18/19 */,
	24	/* 18/20-18/29 */,
	26	/* 18/30-18/39 */,
	28	/* 18/40-18/49 */,
	30	/* 18/50-18/59 */,
	30	/* 18/60-18/69 */,
	30	/* 18/70-18/79 */,
	30	/* 18/80-18/89 */,
	30	/* 18/90-18/99 */,
	30	/* 18/100-18/109 */,
	30	/* 18/110-18/119 */,
	30	/* 18/120-18/129 */,
	30	/* 18/130-18/139 */,
	30	/* 18/140-18/149 */,
	30	/* 18/150-18/159 */,
	30	/* 18/160-18/169 */,
	30	/* 18/170-18/179 */,
	30	/* 18/180-18/189 */,
	30	/* 18/190-18/199 */,
	30	/* 18/200-18/209 */,
	30	/* 18/210-18/219 */,
	30	/* 18/220+ */
};


/*
 * Stat Table (STR) -- weapon weight limit in pounds
 */
const byte adj_str_hold[] =
{
	4	/* 3 */,
	5	/* 4 */,
	6	/* 5 */,
	7	/* 6 */,
	8	/* 7 */,
	10	/* 8 */,
	12	/* 9 */,
	14	/* 10 */,
	16	/* 11 */,
	18	/* 12 */,
	20	/* 13 */,
	22	/* 14 */,
	24	/* 15 */,
	26	/* 16 */,
	28	/* 17 */,
	30	/* 18/00-18/09 */,
	30	/* 18/10-18/19 */,
	35	/* 18/20-18/29 */,
	40	/* 18/30-18/39 */,
	45	/* 18/40-18/49 */,
	50	/* 18/50-18/59 */,
	55	/* 18/60-18/69 */,
	60	/* 18/70-18/79 */,
	65	/* 18/80-18/89 */,
	70	/* 18/90-18/99 */,
	80	/* 18/100-18/109 */,
	80	/* 18/110-18/119 */,
	80	/* 18/120-18/129 */,
	80	/* 18/130-18/139 */,
	80	/* 18/140-18/149 */,
	90	/* 18/150-18/159 */,
	90	/* 18/160-18/169 */,
	90	/* 18/170-18/179 */,
	90	/* 18/180-18/189 */,
	90	/* 18/190-18/199 */,
	100	/* 18/200-18/209 */,
	100	/* 18/210-18/219 */,
	100	/* 18/220+ */
};


/*
 * Stat Table (STR) -- digging value
 */
const byte adj_str_dig[] =
{
	0	/* 3 */,
	0	/* 4 */,
	1	/* 5 */,
	2	/* 6 */,
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
	9	/* 18/00-18/09 */,
	10	/* 18/10-18/19 */,
	12	/* 18/20-18/29 */,
	15	/* 18/30-18/39 */,
	20	/* 18/40-18/49 */,
	25	/* 18/50-18/59 */,
	30	/* 18/60-18/69 */,
	35	/* 18/70-18/79 */,
	40	/* 18/80-18/89 */,
	45	/* 18/90-18/99 */,
	50	/* 18/100-18/109 */,
	55	/* 18/110-18/119 */,
	60	/* 18/120-18/129 */,
	65	/* 18/130-18/139 */,
	70	/* 18/140-18/149 */,
	75	/* 18/150-18/159 */,
	80	/* 18/160-18/169 */,
	85	/* 18/170-18/179 */,
	90	/* 18/180-18/189 */,
	95	/* 18/190-18/199 */,
	100	/* 18/200-18/209 */,
	100	/* 18/210-18/219 */,
	100	/* 18/220+ */
};


/*
 * Stat Table (STR) -- help index into the "blow" table
 */
const byte adj_str_blow[] =
{
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
const byte adj_dex_blow[] =
{
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
	1	/* 13 */,
	1	/* 14 */,
	1	/* 15 */,
	1	/* 16 */,
	1	/* 17 */,
	1	/* 18/00-18/09 */,
	2	/* 18/10-18/19 */,
	2	/* 18/20-18/29 */,
	2	/* 18/30-18/39 */,
	2	/* 18/40-18/49 */,
	3	/* 18/50-18/59 */,
	3	/* 18/60-18/69 */,
	4	/* 18/70-18/79 */,
	4	/* 18/80-18/89 */,
	5	/* 18/90-18/99 */,
	6	/* 18/100-18/109 */,
	7	/* 18/110-18/119 */,
	8	/* 18/120-18/129 */,
	9	/* 18/130-18/139 */,
	10	/* 18/140-18/149 */,
	11	/* 18/150-18/159 */,
	12	/* 18/160-18/169 */,
	14	/* 18/170-18/179 */,
	16	/* 18/180-18/189 */,
	18	/* 18/190-18/199 */,
	20	/* 18/200-18/209 */,
	20	/* 18/210-18/219 */,
	20	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- chance of avoiding "theft" and "falling"
 */
const byte adj_dex_safe[] =
{
	0	/* 3 */,
	1	/* 4 */,
	2	/* 5 */,
	3	/* 6 */,
	4	/* 7 */,
	5	/* 8 */,
	5	/* 9 */,
	6	/* 10 */,
	6	/* 11 */,
	7	/* 12 */,
	7	/* 13 */,
	8	/* 14 */,
	8	/* 15 */,
	9	/* 16 */,
	9	/* 17 */,
	10	/* 18/00-18/09 */,
	10	/* 18/10-18/19 */,
	15	/* 18/20-18/29 */,
	15	/* 18/30-18/39 */,
	20	/* 18/40-18/49 */,
	25	/* 18/50-18/59 */,
	30	/* 18/60-18/69 */,
	35	/* 18/70-18/79 */,
	40	/* 18/80-18/89 */,
	45	/* 18/90-18/99 */,
	50	/* 18/100-18/109 */,
	60	/* 18/110-18/119 */,
	70	/* 18/120-18/129 */,
	80	/* 18/130-18/139 */,
	90	/* 18/140-18/149 */,
	95	/* 18/150-18/159 */,
	97	/* 18/160-18/169 */,
	99	/* 18/170-18/179 */,
	100	/* 18/180-18/189 */,
	100	/* 18/190-18/199 */,
	100	/* 18/200-18/209 */,
	100	/* 18/210-18/219 */,
	100	/* 18/220+ */
};


/*
 * Stat Table (CON) -- base regeneration rate
 */
const byte adj_con_fix[] =
{
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
	0	/* 13 */,
	1	/* 14 */,
	1	/* 15 */,
	1	/* 16 */,
	1	/* 17 */,
	2	/* 18/00-18/09 */,
	2	/* 18/10-18/19 */,
	2	/* 18/20-18/29 */,
	2	/* 18/30-18/39 */,
	2	/* 18/40-18/49 */,
	3	/* 18/50-18/59 */,
	3	/* 18/60-18/69 */,
	3	/* 18/70-18/79 */,
	3	/* 18/80-18/89 */,
	3	/* 18/90-18/99 */,
	4	/* 18/100-18/109 */,
	4	/* 18/110-18/119 */,
	5	/* 18/120-18/129 */,
	6	/* 18/130-18/139 */,
	6	/* 18/140-18/149 */,
	7	/* 18/150-18/159 */,
	7	/* 18/160-18/169 */,
	8	/* 18/170-18/179 */,
	8	/* 18/180-18/189 */,
	8	/* 18/190-18/199 */,
	9	/* 18/200-18/209 */,
	9	/* 18/210-18/219 */,
	9	/* 18/220+ */
};


/*
 * Stat Table (CON) -- extra half-hitpoints per level (divided by 100)
 */
const int adj_con_mhp[] =
{
	-250	/* 3 */,
	-150	/* 4 */,
	-100	/* 5 */,
	 -75	/* 6 */,
	 -50	/* 7 */,
	 -25	/* 8 */,
	 -10	/* 9 */,
	  -5	/* 10 */,
	   0	/* 11 */,
	   5	/* 12 */,
	  10	/* 13 */,
	  25	/* 14 */,
	  50	/* 15 */,
	  75	/* 16 */,
	 100	/* 17 */,
	 150	/* 18/00-18/09 */,
	 175	/* 18/10-18/19 */,
	 200	/* 18/20-18/29 */,
	 225	/* 18/30-18/39 */,
	 250	/* 18/40-18/49 */,
	 275	/* 18/50-18/59 */,
	 300	/* 18/60-18/69 */,
	 350	/* 18/70-18/79 */,
	 400	/* 18/80-18/89 */,
	 450	/* 18/90-18/99 */,
	 500	/* 18/100-18/109 */,
	 550	/* 18/110-18/119 */,
	 600	/* 18/120-18/129 */,
	 650	/* 18/130-18/139 */,
	 700	/* 18/140-18/149 */,
	 750	/* 18/150-18/159 */,
	 800	/* 18/160-18/169 */,
	 900	/* 18/170-18/179 */,
	1000	/* 18/180-18/189 */,
	1100	/* 18/190-18/199 */,
	1250	/* 18/200-18/209 */,
	1250	/* 18/210-18/219 */,
	1250	/* 18/220+ */

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
const byte blows_table[12][12] =
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
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 6  */
	{  2,   2,   3,   3,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 7  */
	{  2,   3,   3,   4,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 8  */
	{  3,   3,   3,   4,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 9  */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 10 */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   5,   5 },

	/* 11+ */
	{  3,   3,   4,   4,   4,   4,   5,   5,   5,   5,   5,   5 },
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

const byte extract_energy_nppmoria[6] =
{
	2,  /* 9 speed - slow + temporary slow */
	5,  /* 10 speed - slow */
	10, /* 11 speed - normal speed */
	20, /* 12 speed - +1 */
	40, /* 13 speed - +2 */
	80, /* 14 speed - +3  */
};

const byte extract_energy_nppangband[200] =
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

const s32b player_exp_nppmoria[PY_MAX_LEVEL_MORIA] =
{
      10,			/* Experience needed to gain level 2 */
      25,			/* Experience needed to gain level 3 */
      45,			/* Experience needed to gain level 4 */
      70,			/* Experience needed to gain level 5 */
      100,			/* Experience needed to gain level 6 */
      140,			/* Experience needed to gain level 7 */
      200,			/* Experience needed to gain level 8 */
      280,			/* Experience needed to gain level 9 */
      380,			/* Experience needed to gain level 10 */
      500,			/* Experience needed to gain level 11 */
      650,			/* Experience needed to gain level 12 */
      850,	 		/* Experience needed to gain level 13 */
      1100,	   		/* Experience needed to gain level 14 */
      1400,     	/* Experience needed to gain level 15 */
      1800,     	/* Experience needed to gain level 16 */
      2300,			/* Experience needed to gain level 17 */
      2900,    		/* Experience needed to gain level 18 */
      3600,    		/* Experience needed to gain level 19 */
      4400,    		/* Experience needed to gain level 20 */
      5400,	 		/* Experience needed to gain level 21 */
      6800,	   		/* Experience needed to gain level 22 */
      8400,    		/* Experience needed to gain level 23 */
      10200,    	/* Experience needed to gain level 24 */
      12500,		/* Experience needed to gain level 25 */
      17500,   		/* Experience needed to gain level 26 */
      25000,  		/* Experience needed to gain level 27 */
      35000L,  		/* Experience needed to gain level 28 */
      50000L,   	/* Experience needed to gain level 29 */
      75000L,		/* Experience needed to gain level 30 */
      100000L,  	/* Experience needed to gain level 31 */
      150000L,  	/* Experience needed to gain level 32 */
      200000L,		/* Experience needed to gain level 33 */
      300000L, 		/* Experience needed to gain level 34 */
      400000L, 		/* Experience needed to gain level 35 */
      500000L, 		/* Experience needed to gain level 36 */
      750000L, 		/* Experience needed to gain level 37 */
      1500000L, 	/* Experience needed to gain level 38 */
      2500000L, 	/* Experience needed to gain level 39 */
      5000000L, 	/* Experience needed to gain level 40 */
      10000000L		/* Level 40 */
};


/*
 * Base experience levels, may be adjusted up for race and/or class
 */
const s32b player_exp_nppangband[PY_MAX_LEVEL] =
{
		10,			/* Experience needed to gain level 2 */
		25,			/* Experience needed to gain level 3 */
		45,			/* Experience needed to gain level 4 */
		70,			/* Experience needed to gain level 5 */
		100,		/* Experience needed to gain level 6 */
		140,		/* Experience needed to gain level 7 */
		200,		/* Experience needed to gain level 8 */
		280,		/* Experience needed to gain level 9 */
		380,		/* Experience needed to gain level 10 */
		500,		/* Experience needed to gain level 11 */
		650,		/* Experience needed to gain level 12 */
		850,		/* Experience needed to gain level 13 */
		1100,		/* Experience needed to gain level 14 */
		1400,		/* Experience needed to gain level 15 */
		1800,		/* Experience needed to gain level 16 */
		2300,		/* Experience needed to gain level 17 */
		2900,		/* Experience needed to gain level 18 */
		3600,		/* Experience needed to gain level 19 */
		4400,		/* Experience needed to gain level 20 */
		5400,		/* Experience needed to gain level 21 */
		6800,		/* Experience needed to gain level 22 */
		8400,		/* Experience needed to gain level 23 */
		10200,		/* Experience needed to gain level 24 */
		12500,		/* Experience needed to gain level 25 */
		17500,		/* Experience needed to gain level 26 */
		25000,		/* Experience needed to gain level 27 */
		35000L,		/* Experience needed to gain level 28 */
		50000L,		/* Experience needed to gain level 29 */
		75000L,		/* Experience needed to gain level 30 */
		100000L,	/* Experience needed to gain level 31 */
		150000L,	/* Experience needed to gain level 32 */
		200000L,	/* Experience needed to gain level 33 */
		275000L,	/* Experience needed to gain level 34 */
		350000L,	/* Experience needed to gain level 35 */
		450000L,	/* Experience needed to gain level 36 */
		550000L,	/* Experience needed to gain level 37 */
		700000L,	/* Experience needed to gain level 38 */
		850000L,	/* Experience needed to gain level 39 */
		1000000L,	/* Experience needed to gain level 40 */
		1250000L,	/* Experience needed to gain level 41 */
		1500000L,	/* Experience needed to gain level 42 */
		1800000L,	/* Experience needed to gain level 43 */
		2100000L,	/* Experience needed to gain level 44 */
		2400000L,	/* Experience needed to gain level 45 */
		2700000L,	/* Experience needed to gain level 46 */
		3000000L,	/* Experience needed to gain level 47 */
		3500000L,	/* Experience needed to gain level 48 */
		4000000L,	/* Experience needed to gain level 49 */
		4500000L,	/* Experience needed to gain level 50 */
		5000000L	/* Level 50 */
};


/*
 * Player Sexes
 *
 *	Title,
 *	Winner
 */
const player_sex sex_info[MAX_SEXES] =
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
 * Spells in each book (mage spells, priest and druid spells)
 */
const s16b spell_list_nppmoria_mage[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK] =
{
	/* Beginners-Magick */
	{
		SPELL_MAGIC_MISSILE,
		SPELL_DETECT_MONSTERS,
		SPELL_PHASE_DOOR,
		SPELL_LIGHT_AREA,
		SPELL_CURE_LIGHT_WOUNDS,
		SPELL_FIND_TRAPS_DOORS,
		SPELL_STINKING_CLOUD,
		-1,
		-1,
	},

	/* Magick I */
	{
		SPELL_CONFUSE_MONSTER,
		SPELL_LIGHTNING_BOLT,
		SPELL_TRAP_DOOR_DESTRUCTION,
		SPELL_SLEEP_MONSTER,
		SPELL_CURE_POISON,
		SPELL_TELEPORT_SELF,
		SPELL_REMOVE_CURSE,
		SPELL_FROST_BOLT,
		SPELL_TURN_STONE_TO_MUD,
	},

	/* Magick II */
	{
		SPELL_CREATE_FOOD,
	   	SPELL_RECHARGE_ITEM_I,
	   	SPELL_SLEEP_II,
		SPELL_POLYMORPH_OTHER,
		SPELL_IDENTIFY,
		SPELL_MASS_SLEEP,
		SPELL_FIRE_BOLT,
		SPELL_SLOW_MONSTER,
		-1,
	},

	/* The Mages' Guide to Power */
	{
		SPELL_FROST_BALL,
		SPELL_RECHARGE_ITEM_II,
		SPELL_TELEPORT_OTHER,
		SPELL_HASTE_SELF,
		SPELL_FIRE_BALL,
		SPELL_WORD_OF_DESTRUCTION,
		SPELL_BANISHMENT,
		-1,
		-1,
	},
};

const s16b spell_list_nppmoria_priest[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK] =
{
	/*** Priest spell books ***/

	/*Beginner's Handbook*/
	{
		PRAYER_DETECT_EVIL,
		PRAYER_CURE_LIGHT_WOUNDS,
		PRAYER_BLESS,
		PRAYER_REMOVE_FEAR,
		PRAYER_CALL_LIGHT,
		PRAYER_FIND_TRAPS,
		PRAYER_DETECT_DOORS_STAIRS,
		PRAYER_SLOW_POISON,
		-1,
	},

	/*Words of Wisdom*/
	{
		PRAYER_CONFUSE_MONSTER,
		PRAYER_PORTAL,
		PRAYER_CURE_SERIOUS_WOUNDS,
		PRAYER_CHANT,
		PRAYER_SANCTUARY,
		PRAYER_CREATE_FOOD,
		PRAYER_REMOVE_CURSE,
		PRAYER_RESIST_HEAT_COLD,
		-1,
	},

	/*Chants and Blessings*/
	{
		PRAYER_NEUTRALIZE_POISON,
		PRAYER_ORB_OF_DRAINING,
		PRAYER_CURE_CRITICAL_WOUNDS,
		PRAYER_SENSE_INVISIBLE,
		PRAYER_PROTECTION_FROM_EVIL,
		PRAYER_EARTHQUAKE,
		PRAYER_SENSE_SURROUNDINGS,
		PRAYER_CURE_MORTAL_WOUNDS,
		PRAYER_TURN_UNDEAD,

	},

	/*Exorcism and Dispelling*/
	{
		PRAYER_PRAYER,
		PRAYER_DISPEL_UNDEAD,
		PRAYER_HEAL,
		PRAYER_DISPEL_EVIL,
		PRAYER_GLYPH_OF_WARDING,
		PRAYER_HOLY_WORD,
		-1,
		-1,
		-1,
	},
};



/*
 * Spells in each book (mage spells, priest and druid spells)
 */
const s16b spell_list_nppangband_mage[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK] =
{

	/* Magic for Beginners */
	{
		SPELL_MAGIC_MISSILE,
		SPELL_DETECT_MONSTERS,
		SPELL_PHASE_DOOR,
		SPELL_LIGHT_AREA,
		SPELL_TREASURE_DETECTION,
		SPELL_CURE_LIGHT_WOUNDS,
		SPELL_OBJECT_DETECTION,
		SPELL_FIND_TRAPS_DOORS,
		SPELL_STINKING_CLOUD,
	},

	/* Conjurings and Tricks */
	{
		SPELL_CONFUSE_MONSTER,
		SPELL_SHOCK_WAVE,
		SPELL_TRAP_DOOR_DESTRUCTION,
		SPELL_CURE_POISON,
		SPELL_SLEEP_MONSTER,
		SPELL_TELEPORT_SELF,
		SPELL_SPEAR_OF_LIGHT,
		SPELL_ICE_BOLT,
		SPELL_WAIL_OF_THE_BANSHEE,
	},

	/* Incantations and Illusions */
	{
		SPELL_SATISFY_HUNGER,
    	SPELL_RECHARGE_ITEM_I,
		SPELL_TURN_STONE_TO_MUD,
		SPELL_SHARD_STORM,
		SPELL_POLYMORPH_OTHER,
		SPELL_IDENTIFY,
		SPELL_DETECT_INVISIBLE,
		SPELL_HURRICANE,
		SPELL_SLOW_MONSTER,
	},

	/* Sorcery and Evocations */
	{
		SPELL_CALL_LIGHTNING,
		SPELL_TELEPORT_OTHER,
		SPELL_HASTE_SELF,
		SPELL_MASS_SLEEP,
		SPELL_WATER_BOLT,
		SPELL_DETECT_ENCHANTMENT,
		SPELL_MASS_IDENTIFY,
		-1,
		-1,
	},

	/* Resistances of Scarabtarices */
	{
		SPELL_RESIST_COLD,
		SPELL_RESIST_FIRE,
		SPELL_RESIST_POISON,
		SPELL_RESISTANCE,
		SPELL_SHIELD,
		-1,
		-1,
		-1,
		-1,
	},

	/* Raal's Tome of Destruction */
	{
		SPELL_NOVA,
		SPELL_REND_SOUL,
		SPELL_PRISMATIC_SPRAY,
		SPELL_CLOUD_KILL,
		SPELL_ICE_STORM,
		SPELL_PLASMA_BOLT,
		SPELL_METEOR_STORM,
		SPELL_RIFT,
		-1,
	},

	/* Mordenkainen's Escapes */
	{
		SPELL_DOOR_CREATION,
		SPELL_STAIR_CREATION,
		SPELL_TELEPORT_LEVEL,
		SPELL_WORD_OF_RECALL,
		SPELL_RUNE_OF_PROTECTION,
		SPELL_FLIGHT,
		-1,
		-1,
		-1,
	},

	/* Tenser's transformations */
	{
		SPELL_BERSERKER,
		SPELL_ENCHANT_ARMOR,
		SPELL_ENCHANT_WEAPON,
		SPELL_RECHARGE_ITEM_II,
		SPELL_ELEMENTAL_BRAND,
		-1,
		-1,
		-1,
		-1,
	},

	/* Kelek's Grimoire of Power */
	{
		SPELL_EARTHQUAKE,
		SPELL_BEDLAM,
		SPELL_BANISHMENT,
		SPELL_WORD_OF_DESTRUCTION,
		SPELL_MASS_BANISHMENT,
		SPELL_DARKNESS_STORM,
		SPELL_MANA_BOLT,
		SPELL_MANA_STORM,
		-1,
	},
};

/*** Priest spell books ***/
const s16b spell_list_nppangband_priest[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK] =
{
		/*Beginner's Handbook*/
	{
		PRAYER_DETECT_EVIL,
		PRAYER_CURE_LIGHT_WOUNDS,
		PRAYER_BLESS,
		PRAYER_REMOVE_FEAR,
		PRAYER_CALL_LIGHT,
		PRAYER_FIND_TRAPS_DOORS_STAIRS,
		PRAYER_SHOCK_BOLT,
		PRAYER_SLOW_POISON,
		-1,
	},

	/*Words of Wisdom*/
	{
		PRAYER_SCARE_MONSTER,
		PRAYER_PORTAL,
		PRAYER_CURE_SERIOUS_WOUNDS,
		PRAYER_CHANT,
		PRAYER_SANCTUARY,
		PRAYER_SATISFY_HUNGER,
		PRAYER_REMOVE_CURSE,
		PRAYER_RESIST_HEAT_COLD,
		-1,
	},

	/*Chants and Blessings*/
	{
		PRAYER_NEUTRALIZE_POISON,
		PRAYER_CURE_CRITICAL_WOUNDS,
		PRAYER_SENSE_INVISIBLE,
		PRAYER_PROTECTION_FROM_EVIL,
		PRAYER_EARTHQUAKE,
		PRAYER_SENSE_SURROUNDINGS,
		PRAYER_CURE_MORTAL_WOUNDS,
		PRAYER_TURN_UNDEAD,
		PRAYER_ORB_OF_DRAINING,
	},

	/*Exorcism and Dispelling*/
	{
		PRAYER_PRAYER,
		PRAYER_SUN_BEAM,
		PRAYER_HEAL,
		PRAYER_DISPEL_EVIL,
		PRAYER_GLYPH_OF_WARDING,
		PRAYER_HOLY_WORD,
		-1,
		-1,
		-1,
	},

	/*Ethereal Openings*/
	{
		PRAYER_BLINK,
		PRAYER_UNBARRING_WAYS,
		PRAYER_TELEPORT_SELF,
		PRAYER_TELEPORT_OTHER,
		PRAYER_TELEPORT_LEVEL,
		PRAYER_WORD_OF_RECALL,
		PRAYER_ALTER_REALITY,
		-1,
		-1,
	},

	/*Godly Insights*/
	{
		PRAYER_DETECT_MONSTERS,
		PRAYER_DETECTION,
		PRAYER_RECHARGING,
		PRAYER_PERCEPTION,
		PRAYER_PROBING,
		PRAYER_CLAIRVOYANCE,
		PRAYER_MASS_IDENTIFY,
		-1,
		-1,
	},

	/*Purifications and Healing*/
	{
		PRAYER_CURE_SERIOUS_WOUNDS2,
		PRAYER_CURE_MORTAL_WOUNDS2,
		PRAYER_HEALING,
		PRAYER_RESTORATION,
		PRAYER_REMEMBRANCE,
		-1,
		-1,
		-1,
		-1,
	},

	/*Holy Infusions*/
	{
		PRAYER_DISPEL_CURSE,
		PRAYER_ENCHANT_WEAPON,
		PRAYER_ENCHANT_ARMOUR,
		PRAYER_ELEMENTAL_BRAND,
		-1,
		-1,
		-1,
		-1,
		-1,
	},

	/*Wrath of God*/
	{
		PRAYER_SUN_BURST,
		PRAYER_DISPEL_EVIL2,
		PRAYER_BANISH_EVIL,
		PRAYER_WORD_OF_DESTRUCTION,
		PRAYER_JUDGEMENT_OF_MANDOS,
		-1,
		-1,
		-1,
		-1,
	},
};

/*** Druid spell books ***/
const s16b spell_list_nppangband_druid[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK] =
{
	/*Call of the Wild*/
	{
		DRUID_ACID_BOLT,
		DRUID_CURE_LIGHT_WOUNDS,
		DRUID_DETECT_LIFE,
		DRUID_CALL_LIGHT,
		DRUID_FIND_TRAPS_DOORS,
		DRUID_SLOW_POISON,
		DRUID_POISON_CLOUD,
		DRUID_NATURAL_ESCAPE,
		DRUID_BARKSKIN,
	},

	/* Environmental Adjurations */
	{
		DRUID_NOURISHMENT,
		DRUID_TURN_STONE_TO_MUD,
		DRUID_FROST_BEAM,
		DRUID_CURE_POISON,
		DRUID_TRAP_DOOR_DESTRUCTION,
		DRUID_RESIST_HEAT_COLD,
		DRUID_SPEAR_OF_LIGHT,
		DRUID_FIRE_BEAM,
		DRUID_STERILIZE,
	},

	/* Commanding Nature */
	{
		DRUID_EXTINGUISH,
		DRUID_CLEAR_AREA,
		DRUID_CURE_CRITICAL_WOUNDS,
		DRUID_IDENTIFY,
		DRUID_CLEAR_AIR,
		DRUID_DETECT_TERRAIN,
		DRUID_EARTHQUAKE,
		DRUID_LIFE_DRAIN_BURST,
		DRUID_CREATE_ELEMENTS,
	},

	/* Lore of Engagement */
	{
		DRUID_ELEMENTAL_BRAND,
		DRUID_FROST_BALL,
		DRUID_HEAL,
		DRUID_DISPEL_LIFE,
		DRUID_FIRE_BALL,
		DRUID_DRAIN_LIFE_ARC,
		DRUID_MASS_IDENTIFY,
		-1,
		-1,
	},

	/* Radagast's Protections */
	{
		DRUID_RESIST_ELEC,
		DRUID_RESIST_ACID,
		DRUID_RESIST_POISON,
		DRUID_RESISTANCE,
		DRUID_HASTE_SELF,
		DRUID_GLACIER,
		-1,
		-1,
		-1,
	},

	/* Melian's Reformations */
	{
		DRUID_FLICKER,
		DRUID_WORD_OF_RECALL,
		DRUID_HEALING,
		DRUID_RESTORATION,
		DRUID_REMEMBRANCE,
		DRUID_SANDSTORM,
		-1,
		-1,
		-1,
	},

	/* Arda's Habitats */
	{
		DRUID_NATIVE_SAND,
		DRUID_NATIVE_MUD,
		DRUID_NATIVE_WATER,
		DRUID_NATIVE_OIL,
		DRUID_NATIVE_LAVA,
		DRUID_CHANNEL_LIGHTNING,
		-1,
		-1,
		-1,
	},

	/* Natural Infusions */
	{
		DRUID_DISPEL_CURSE,
		DRUID_RECHARGE_ITEM,
		DRUID_BRAND_AMMUNITION,
		DRUID_ENCHANT_ARMOUR,
		DRUID_BRAND_WEAPON,
		-1,
		-1,
		-1,
		-1,
	},

	/* Nature's Fury */
	{
		DRUID_WATER_CHAIN,
		DRUID_CALL_HUORNS,
		DRUID_MASTER_ELEMENTS,
		DRUID_STEAL_POWERS,
		-1,
		-1,
		-1,
		-1,
		-1,
	},
};




/*
 * Each chest has a certain set of traps, determined by pval
 * Each chest has a "pval" from 1 to the chest level (max 55)
 * If the "pval" is negative then the trap has been disarmed
 * The "pval" of a chest determines the quality of its treasure
 * Note that disarming a trap on a chest also removes the lock.
 */
const byte chest_traps[64] =
{
	0,					/* 0 == empty */
	(CHEST_POISON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),			/* 5 == best small wooden */
	0,
	(CHEST_POISON),
	(CHEST_POISON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_POISON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_SUMMON | CHEST_LOSE_CON),			/* 15 == best large wooden */
	0,
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_PARALYZE),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_SUMMON | CHEST_PARALYZE),
	(CHEST_PARALYZE),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_EXPLODE),			/* 25 == best small iron */
	0,
	(CHEST_POISON | CHEST_LOSE_STR),
	(CHEST_POISON | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_PARALYZE),
	(CHEST_POISON | CHEST_SUMMON),
	(CHEST_SUMMON),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),	/* 35 == best large iron */
	0,
	(CHEST_SUMMON | CHEST_EXPLODE),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_EXPLODE),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_POISON | CHEST_PARALYZE),	/* 45 == best small steel */
	0,
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_POISON | CHEST_PARALYZE),	/* 55 == best large steel */
	(CHEST_EXPLODE | CHEST_PARALYZE | CHEST_LOSE_STR),
	(CHEST_EXPLODE | CHEST_PARALYZE | CHEST_LOSE_CON),
	(CHEST_EXPLODE | CHEST_PARALYZE | CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_EXPLODE | CHEST_SUMMON | CHEST_PARALYZE),
	(CHEST_EXPLODE | CHEST_SUMMON | CHEST_PARALYZE),
	(CHEST_EXPLODE | CHEST_SUMMON | CHEST_PARALYZE),
	(CHEST_EXPLODE | CHEST_SUMMON | CHEST_PARALYZE),
	(CHEST_EXPLODE | CHEST_SUMMON | CHEST_PARALYZE),
};

/*
 * Array of feeling strings
 */
cptr feeling_themed_level[LEV_THEME_TAIL] =
{
	"creeping coin",
	"orc",
	"troll",
	"ogre",
	"hound",
	"giant",
	"young dragon",
	"acid dragon",
	"fire dragon",
	"electric dragon",
	"cold dragon",
	"poison dragon",
	"chromatic dragon",
	"dragon",
	"ancient dragon",
	"jelly",
	"lesser cave dweller",
	"animal",
	"humanoid",
	"minor demon",
	"demon",
	"major demon",
	"cave dweller",
	"undead",
	"elemental dragons",
	"servants of the Valar"
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
	"LightDark",
	"LightSlate",
	"Violet",
	"Yellow",
	"LightRed",
	"LightGreen",
	"LightBlue",
	"LightUmber",
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
 * Full stat names
 */
cptr stat_names_full[A_MAX] =
{
	"strength",
	"intelligence",
	"wisdom",
	"dexterity",
	"constitution",
	"charisma"
};



/*
 * Certain "screens" always use the main screen, including News, Birth,
 * Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
 *
 * Later, special flags may allow sub-windows to "steal" stuff from the
 * main window, including File dump (help), File dump (artifacts, uniques),
 * Character screen, Small scale map, Previous Messages, Store screen, etc.
 */
const char *window_flag_desc[32] =
{
	"Display inven/equip",
	"Display equip/inven",
	"Display player (basic)",
	"Display player (extra)",
	"Display player (compact)",
	"Display map view",
	"Display messages",
	"Display overhead view",
	"Display monster recall",
	"Display object recall",
	"Display monster list",
	"Display status",
	"Display item list",
	"Display feature recall",
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
};


/*
 * Options -- definitions
 */
option_entry options[OPT_MAX] =
{
	{"rogue_like_commands",	"Rogue-like commands",					FALSE},		/* OPT_rogue_like_commands */
	{"quick_messages",		"Activate quick messages",				TRUE},		/* OPT_quick_messages */
	{"use_sound",			"Use Sound", 							FALSE},		/* OPT_use_sound */
	{"carry_query_flag",	"Prompt before picking things up",		FALSE},		/* OPT_carry_query_flag */
	{"use_old_target",		"Use old target by default",			FALSE},		/* OPT_use_old_target */
	{"always_pickup",		"Pick things up by default",			TRUE},		/* OPT_always_pickup */
	{"floor_query_flag",	"Prompt before using floor items",		TRUE},		/* OPT_floor_query_flag */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"stack_force_notes",	"Merge inscriptions when stacking",		FALSE},		/* OPT_stack_force_notes */
	{"stack_force_costs",	"Merge discounts when stacking",		FALSE},		/* OPT_stack_force_costs */
	{"expand_inscribe",     "Expand the power of the inscribe commands", TRUE},	/* OPT_expand_inscribe */
	{"disturb_detect",		"Disturb whenever leaving trap detected area",TRUE},/* OPT_disturb_detect */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"ring_bell",			"Audible bell (on errors, etc)",		TRUE},		/* OPT_ring_bell */
	{"show_flavors",		"Show flavors in object descriptions",	TRUE},		/* OPT_flavors */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"disturb_move",		"Disturb whenever any monster moves",	TRUE},		/* OPT_disturb_move */
	{"disturb_near",		"Disturb whenever viewable monster moves",TRUE},	/* OPT_disturb_near */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"disturb_state",		"Disturb whenever player state changes",TRUE},		/* OPT_disturb_state */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"verify_destroy",		"Verify destruction of objects",		TRUE},		/* OPT_verify_destroy */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"auto_scum",			"Auto-scum for good levels",			FALSE},		/* OPT_auto_scum */
	{"allow_themed_levels","Allow the generation of themed levels",  TRUE},		/* OPT_allow_themed_levels */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{NULL,					NULL,									FALSE},		/* xxx */
	{"view_perma_grids",	"Map remembers all perma-lit grids",	TRUE},		/* OPT_view_perma_grids */
	{"view_torch_grids",	"Map remembers all torch-lit grids",	FALSE},		/* OPT_view_torch_grids */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{"smart_cheat",			"Monsters exploit players weaknesses",	FALSE},	/* OPT_smart_cheat */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{"flush_failure",		"Flush input on various failures",		TRUE},	/* OPT_flush_failure */
	{"flush_disturb",		"Flush input whenever disturbed",		TRUE},	/* OPT_flush_disturb */
	{NULL,				NULL,										FALSE},	/* xxx flush_command */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,					NULL,	FALSE},/* xxx */
	{NULL,				NULL,									FALSE},
	{NULL,				NULL,									FALSE},
	{"hilight_player",		"Hilight the player with the cursor",	FALSE},	/* OPT_hilight_player */
	{"view_yellow_light",	"Use special colors for torch light",	FALSE},	/* OPT_view_yellow_light */
	{"view_bright_light",	"Use special colors for field of view",	FALSE},	/* OPT_view_bright_light */
	{"view_granite_light",	"Use special colors for wall grids",	FALSE},	/* OPT_view_granite_light */
	{"view_special_light",	"Use special colors for floor grids",	FALSE},	/* OPT_view_special_light */
	{"easy_open",			"Open/Disarm/Close without direction",	TRUE},	/* OPT_easy_open */
	{"easy_alter",			"Open/Disarm doors/traps on movement",	TRUE},	/* OPT_easy_alter */
	{NULL,				NULL,								FALSE},/* xxx */
	{"show_piles",			"Show stacks using special attr/char",	FALSE},	/* OPT_show_piles */
	{"center_player",		"Center map continuously (very slow)",	FALSE},	/* OPT_center_player */
	{"animate_flicker",    	"Shimmer multi-colored things",  		TRUE}, /* OPT_animate_flicker */
	{"xchars_to_file",		"Allow accents in output files",		FALSE},	/* OPT_xchars_to_file */
	{"auto_more",			"Automatically clear '-more-' prompts",	FALSE},	/* OPT_auto_more */
	{NULL,				NULL,								FALSE},/* xxx */
	{"auto_display_lists",	"Automatically display drop-down lists", TRUE},	/* OPT_auto_display_lists */
	{"hp_changes_color",	"Player color indicates low hit points",FALSE},	/* OPT_hp_changes_color*/
	{"verify_leave_quest",	"Verify before leaving a quest level",TRUE},/* OPT_verify_leave_quest*/
	{"mark_squelch_items",	"Items marked for squelch appear as dot",FALSE},/* OPT_mark_squelch_items */
	{"mouse_movement",      "Allow mouse clicks to move the player",       TRUE }, /*OPT_mouse_movement*/
	{"mouse_buttons",       "Show mouse status line buttons",              TRUE }, /*OPT_mouse_buttons*/
	{"notify_recharge",		"Notify on object recharge", 	FALSE},/* OPT_notify_recharge */
	{NULL,				NULL,								FALSE},/* xxx */
	{NULL,				NULL,							FALSE},/* xxx */
	{NULL,				NULL,							FALSE},/* xxx */
	{NULL,				NULL,							FALSE},/* xxx */
	{NULL,				NULL,							FALSE},/* xxx */
	{NULL,				NULL,							FALSE},/* xxx */
	{NULL,NULL,	FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{"birth_no_selling",    	"Birth: Items always sell for 0 gold",          FALSE}, /* OPT_birth_no_selling */
	{"birth_maximize",			"Birth: Maximize effect of race/class bonuses",	TRUE},	/* OPT_birth_maximize */
	{"birth_preserve",			"Birth: Preserve artifacts when leaving level",	TRUE},	/* OPT_birth_preserve */
	{"birth_ironman",			"Birth: Restrict the use of stairs/recall",		FALSE},	/* OPT_birth_ironman */
	{"birth_no_stores",			"Birth: Restrict the use of stores/home",		FALSE},	/* OPT_birth_no_stores */
	{"birth_no_artifacts",		"Birth: Restrict creation of artifacts",		FALSE},	/* OPT_birth_no_artifacts */
	{"birth_rand_artifacts",	"Birth: Randomize some of the artifacts",		FALSE},	/* OPT_birth_rand_artifacts */
	{"birth_no_stacking",		"Birth: Never stack objects on the floor",		FALSE},	/* OPT_birth_no_stacking */
	{"birth_take_notes",		"Birth: Have notes written to a file",			TRUE},	/* OPT_birth_auto_notes */
	{"birth_force_small_lev",	"Birth: All levels will be generated as small",	FALSE},	/* OPT_birth_force_small_lev */
	{"birth_connected_stairs",  "Birth: Generate connected stairs",       		TRUE},	/* OPT_birth_connected_stairs */
	{"birth_no_quests",			"Birth: Disable quests",						FALSE},	/* OPT_birth_no_quests*/
	{"birth_no_player ghosts",	"Birth: Disable player ghosts",					FALSE},	/* OPT_birth_no_player ghosts*/
	{"birth_no_store_services",	"Birth: Disable store services",				FALSE},	/* OPT_birth_no_store_services*/
	{"birth_no_xtra_artifacts", "Birth: Disable extra artifacts",				FALSE},	/* OPT_birth_no_xtra_artifacts*/
	{"birth_money",             "Birth: Start with more money and no equipment",FALSE },/* OPT_birth_money */
	{"birth_simple_dungeons",   "Birth: Prevent unusual terrains or dungeons",	FALSE },/* OPT_birth_birth_simple_dungeons */
	{"birth_swap_weapons",   	"Birth: Replace bow slot with swap weapon slot",	FALSE },/* OPT_birth_swap_weapons */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{"cheat_peek",			"Cheat: Peek into object creation",				FALSE},	/* OPT_cheat_peek */
	{"cheat_hear",			"Cheat: Peek into monster creation",			FALSE},	/* OPT_cheat_hear */
	{"cheat_room",			"Cheat: Peek into dungeon creation",			FALSE},	/* OPT_cheat_room */
	{"cheat_xtra",			"Cheat: Peek into something else",				FALSE},	/* OPT_cheat_xtra */
	{"cheat_know",			"Cheat: Know complete monster info",			FALSE},	/* OPT_cheat_know */
	{"cheat_live",			"Cheat: Allow player to avoid death",			FALSE},	/* OPT_cheat_live */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{"adult_no_selling",    	"Adult: Items always sell for 0 gold",          FALSE}, /* OPT_adult_no_selling */
	{"adult_maximize",			"Adult: Maximize effect of race/class bonuses",	TRUE},	/* OPT_adult_maximize */
	{"adult_preserve",			"Adult: Preserve artifacts when leaving level",	TRUE},	/* OPT_adult_preserve */
	{"adult_ironman",			"Adult: Restrict the use of stairs/recall",		FALSE},	/* OPT_adult_ironman */
	{"adult_no_stores",			"Adult: Restrict the use of stores/home",		FALSE},	/* OPT_adult_no_stores */
	{"adult_no_artifacts",		"Adult: Restrict creation of artifacts",		FALSE},	/* OPT_adult_no_artifacts */
	{"adult_rand_artifacts",	"Adult: Randomize some of the artifacts",		FALSE},	/* OPT_adult_rand_artifacts */
	{"adult_no_stacking",		"Adult: Never stack objects on the floor",		FALSE},	/* OPT_adult_no_stacking */
	{"adult_take_notes",		"Adult: Have notes to written to a file",		TRUE},	/* OPT_adult_auto_notes */
	{"adult_force_small_lev",	"Adult: All levels generated small",			FALSE},	/* OPT_adult_force_small_lev*/
	{"adult_conected_stairs",   "Adult: Generate connected stairs",       		TRUE},	/* OPT_adult_connected_stairs */
	{"adult_no_quests",			"Adult: Disable quests",						FALSE},	/* OPT_adult_no_quests*/
	{"adult_no_player_ghosts",	"Adult: Disable player ghosts",					FALSE},	/* OPT_adult_no_player ghosts*/
	{"adult_no_store_services",	"Adult: Disable store services",				FALSE},	/* OPT_adult_no_store_services*/
	{"adult_no_xtra_artifacts",	"Adult: Disable extra artifacts",				FALSE},	/* OPT_adult_no_xtra_artifacts*/
	{"adult_birth_money",      	"Adult: Start with more money and no equipment",FALSE },/* OPT_adult_birth_money*/
	{"adult_simple_dungeons",   "Adult: Prevent unusual terrains or dungeons",	FALSE },/* OPT_adult_birth_simple_dungeons */
	{"adult_swap_weapons",   	"Adult: Replace bow slot with swap weapon slot",	FALSE },/* OPT_adult_swap_weapons */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{"score_peek",			"Score: Peek into object creation",				FALSE},	/* OPT_score_peek */
	{"score_hear",			"Score: Peek into monster creation",			FALSE},	/* OPT_score_hear */
	{"score_room",			"Score: Peek into dungeon creation",			FALSE},	/* OPT_score_room */
	{"score_xtra",			"Score: Peek into something else",				FALSE},	/* OPT_score_xtra */
	{"score_know",			"Score: Know complete monster info",			FALSE},	/* OPT_score_know */
	{"score_live",			"Score: Allow player to avoid death",			FALSE},	/* OPT_score_live */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE},/* xxx */
	{NULL,NULL,FALSE} /* xxx */
};

/*
 * Option screen interface
 */
const byte option_page_nppangband[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** Interface/Gameplay ***/

	{
		OPT_use_sound,
		OPT_rogue_like_commands,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_use_old_target,
		OPT_always_pickup,
		OPT_stack_force_notes,
		OPT_stack_force_costs,
		OPT_auto_display_lists,
		OPT_easy_open,
		OPT_easy_alter,
		OPT_expand_inscribe,
		OPT_mouse_movement,
		OPT_mouse_buttons,
		OPT_auto_scum,
		OPT_allow_themed_levels,
		OPT_smart_cheat,
		OPT_xchars_to_file,
		OPT_NONE,
		OPT_NONE
	},

	/*** Display ***/

	{
		OPT_hp_changes_color,
		OPT_hilight_player,
		OPT_center_player,
		OPT_show_piles,
		OPT_show_flavors,
		OPT_view_yellow_light,
		OPT_view_bright_light,
		OPT_view_granite_light,
		OPT_view_special_light,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		OPT_mark_squelch_items,
		OPT_animate_flicker,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/* Warning */

	{
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_detect,
		OPT_disturb_state,
		OPT_quick_messages,
		OPT_auto_more,
		OPT_ring_bell,
		OPT_flush_failure,
		OPT_flush_disturb,
		OPT_verify_destroy,
		OPT_notify_recharge,
		OPT_verify_leave_quest,
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
		OPT_birth_maximize,
		OPT_birth_rand_artifacts,
		OPT_birth_money,
		OPT_birth_take_notes,
		OPT_birth_force_small_lev,
		OPT_birth_ironman,
		OPT_birth_no_stores,
		OPT_birth_no_artifacts,
		OPT_birth_no_stacking,
		OPT_birth_preserve,
		OPT_birth_connected_stairs,
		OPT_birth_no_quests,
		OPT_birth_no_player_ghosts,
		OPT_birth_no_store_services,
		OPT_birth_no_xtra_artifacts,
		OPT_birth_no_selling,
		OPT_birth_simple_dungeons,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Cheat ***/

	{
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_xtra,
		OPT_cheat_know,
		OPT_cheat_live,
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
	}
};

/*
 * Option screen interface
 */
const byte option_page_nppmoria[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** Interface/Gameplay ***/

	{
		OPT_use_sound,
		OPT_rogue_like_commands,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_use_old_target,
		OPT_always_pickup,
		OPT_stack_force_notes,
		OPT_stack_force_costs,
		OPT_auto_display_lists,
		OPT_easy_open,
		OPT_easy_alter,
		OPT_expand_inscribe,
		OPT_mouse_movement,
		OPT_mouse_buttons,
		OPT_smart_cheat,
		OPT_xchars_to_file,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/*** Display ***/

	{
		OPT_hp_changes_color,
		OPT_hilight_player,
		OPT_center_player,
		OPT_show_piles,
		OPT_show_flavors,
		OPT_view_yellow_light,
		OPT_view_bright_light,
		OPT_view_granite_light,
		OPT_view_special_light,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		OPT_mark_squelch_items,
		OPT_animate_flicker,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
	},

	/* Warning */

	{
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_detect,
		OPT_disturb_state,
		OPT_quick_messages,
		OPT_auto_more,
		OPT_ring_bell,
		OPT_flush_failure,
		OPT_flush_disturb,
		OPT_verify_destroy,
		OPT_notify_recharge,
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

	/*** Birth ***/

	{
		OPT_birth_money,
		OPT_birth_take_notes,
		OPT_birth_ironman,
		OPT_birth_no_stores,
		OPT_birth_no_stacking,
		OPT_birth_no_selling,
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

	/*** Cheat ***/

	{
		OPT_cheat_peek,
		OPT_cheat_hear,
		OPT_cheat_room,
		OPT_cheat_xtra,
		OPT_cheat_know,
		OPT_cheat_live,
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
	"good",
	"excellent",
	"special",
	"uncursed",
	"indestructible"
};



/*
 * First column is Mana Cost
 * Second column is multiplier * spellpower (breaths handled separately)
 * Third column is a divider to we can have damage like
 * spellpower * 5/2 using only integer math.
 * 4th column is damage variance
 * 5th column is Optimal Ranges for various spells.
 * 6 is optimal for Breath Weapons, Beams, and Arcs.
 * 3 is hard maximum for Lash/Spit.
 * 0 indicates no range limitation for other spells.
 *
 * This range is considered a preference if d_range in spell_desire is > 0.
 * It is a hard limit if d_range = 0.
 */

/*{Mana_cost,dam_mult,dam_div,dam_var,best_range}*/
byte spell_info_RF4[32][5]=
{
	{1,     0,     0,     0,     0},        /* RF4_SHIEIK */
	{0,     0,     0,     0,     0},        /* RF4_LASH */
	{0,     3,     1,     4,     4},        /* RF4_BOULDER */
	{0,     4,     1,     4,     4},        /* RF4_SHOT */
	{0,     4,     1,     4,     4},        /* RF4_ARROW */
	{0,     4,     1,     4,     4},        /* RF4_BOLT */
	{0,     3,     1,     4,     4},        /* RF4_MISSL */
	{0,     3,     1,     6,     6},        /* RF4_PMISSLE */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_ACID */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_ELEC */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_FIRE */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_COLD */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_POIS */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_PLAS */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_LIGHT */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_DARK */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_CONFU */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_SOUND */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_SHARD */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_INER */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_GRAV */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_FORCE */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_NEXUS */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_NETHR */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_CHAOS */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_DISEN */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_TIME */
	{0,     0,     0,     0,     0},        /* RF4_BRTH_MANA */
	{0,     0,     0,     0,     0},        /* RF4_XXX3 */
	{0,     0,     0,     0,     0},        /* RF4_XXX4 */
	{0,     0,     0,     0,     0},        /* RF4_XXX5 */
	{0,     0,     0,     0,     0}        /* RF4_XXX6 */
};

 /*{Mana_cost,dam_mult,dam_div,dam_var,best_range}*/
byte spell_info_RF5[32][5]=
{
	{4,     4,     1,     6,     6},        /* RF5_BALL_ACID */
	{4,     4,     1,     6,     6},        /* RF5_BALL_ELEC */
	{4,     4,     1,     6,     6},        /* RF5_BALL_FIRE */
	{4,     4,     1,     6,     6},        /* RF5_BALL_COLD */
	{4,     3,     1,     6,     6},        /* RF5_BALL_POIS */
	{5,     3,     1,     6,     6},        /* RF5_BALL_LIGHT */
	{5,     3,     1,     6,     6},        /* RF5_BALL_DARK */
	{6,     3,     1,     6,     6},        /* RF5_BALL_CONFU */
	{4,     2,     1,     6,     6},        /* RF5_BALL_SOUND */
	{4,     3,     1,     6,     6},        /* RF5_BALL_SHARD */
	{6,     5,     2,     6,     6},        /* RF5_BALL_METEOR */
	{5,     3,     1,     4,     4},        /* RF5_BALL_STORM */
	{6,     3,     1,     6,     6},        /* RF5_BALL_NETHR */
	{7,     3,     1,     4,     4},        /* RF5_BALL_CHAOS */
	{7,     3,     1,     8,     8},        /* RF5_BALL_MANA */
	{7,     3,     1,     6,     6},        /* RF5_BALL_WATER */
	{4,     4,     1,     6,     6},        /* RF5_BOLT_ACID */
	{4,     4,     1,     6,     6},        /* RF5_BOLT_ELEC */
	{4,     4,     1,     6,     6},        /* RF5_BOLT_FIRE */
	{4,     4,     1,     6,     6},        /* RF5_BOLT_COLD */
	{4,     3,     1,     6,     6},        /* RF5_BOLT_POIS */
	{5,     3,     1,     6,     6},        /* RF5_BOLT_PLAS */
	{5,     3,     1,     6,     6},        /* RF5_BOLT_ICE */
	{5,     3,     1,     6,     6},        /* RF5_BOLT_WATER */
	{5,     3,     1,     6,     6},        /* RF5_BOLT_NETHR */
	{5,     2,     1,     6,     6},        /* RF5_BOLT_MANA */
	{5,     2,     1,     6,     6},        /* RF5_BOLT_GRAV */
	{6,     3,     1,     6,     6},        /* RF5_BEAM_ELEC */
	{6,     4,     1,     6,     6},        /* RF5_BEAM_ICE */
	{6,     3,     1,     6,     6},        /* RF5_BEAM_NETHR */
	{6,     5,     2,     6,     6},        /* RF5_BEAM_LAVA */
	{5,     5,     2,     8,     8}         /* RF5_HOLY_ORB */
};

 /*{Mana_cost,dam_mult,dam_div,dam_var,best_range}*/
byte spell_info_RF6[32][5]=
{
	{6,     0,     0,     0,     0},        /* RF6_HASTE */
	{0,     0,     0,     0,     0},        /* RF6_ADD_MANA */
	{3,     0,     0,     0,     0},        /* RF6_HEAL */
	{3,     0,     0,     0,     0},        /* RF6_CURE */
	{3,     0,     0,     0,     0},        /* RF6_BLINK */
	{8,     0,     0,     0,     0},        /* RF6_TPORT */
	{0,     0,     0,     0,     0},        /* RF6_XXX1 */
	{4,     0,     0,     0,     0},        /* RF6_TELE_SELF_TO */
	{4,     0,     0,     0,     0},        /* RF6_TELE_TO */
	{8,     0,     0,     0,     0},        /* RF6_TELE_AWAY */
	{8,     0,     0,     0,     0},        /* RF6_TELE_LEVEL */
	{0,     0,     0,     0,     0},        /* RF6_XXX2 */
	{1,     0,     0,     0,     0},        /* RF6_DARKNESS */
	{2,     0,     0,     0,     0},        /* RF6_TRAPS */
	{0,     0,     0,     0,     0},        /* RF6_XXX3 */
	{2,     0,     0,     0,     0},        /* RF6_DRAIN_MANA */
	{0,     0,     0,     0,     0},        /* RF6_XXX4 */
	{0,     0,     0,     0,     0},        /* RF6_XXX5 */
	{3,     3,     2,     6,     6},        /* RF6_MIND_BLAST */
	{4,     5,     2,     6,     6},        /* RF6_BRAIN_SMASH */
	{4,     5,     2,     6,     6},        /* RF6_WOUND */
	{0,     0,     0,     0,     0},        /* RF6_XXX6 */
	{0,     0,     0,     0,     0},        /* RF6_XXX7 */
	{0,     0,     0,     0,     0},        /* RF6_XXX8 */
	{0,     0,     0,     0,     0},        /* RF6_XXX9 */
	{2,     0,     0,     0,     0},        /* RF6_HUNGER */
	{0,     0,     0,     0,     0},        /* RF6_XX11 */
	{1,     0,     0,     0,     0},        /* RF6_SCARE */
	{3,     0,     0,     0,     0},        /* RF6_BLIND */
	{4,     0,     0,     0,     0},        /* RF6_CONF */
	{5,     0,     0,     0,     0},        /* RF6_SLOW */
	{6,     0,     0,     0,     0}        /* RF6_HOLD */
};

 /*{Mana_cost,dam_mult,dam_div,dam_var,best_range}*/
byte spell_info_RF7[32][5]=
{
	{12,    0,     0,     0,     0},        /* RF7_S_KIN */ /* Summon - 6 */
	{0,     0,     0,     0,     0},        /* RF7_XXX1 */
	{0,     0,     0,     0,     0},        /* RF7_XXX2 */
	{10,    0,     0,     0,     0},        /* RF7_S_MONSTER */ /* Summon - 1 */
	{15,    0,     0,     0,     0},        /* RF7_S_MONSTERS */ /* Summon - 8 */
	{0,     0,     0,     0,     0},        /* RF7_XXX3 */
	{0,     0,     0,     0,     0},        /* RF7_XXX4 */
	{0,     0,     0,     0,     0},        /* RF7_XXX5 */
	{10,    0,     0,     0,     0},        /* RF7_S_ANT */ /* Summon - 6 */
	{12,    0,     0,     0,     0},        /* RF7_S_SPIDER */ /* Summon - 6 */
	{14,    0,     0,     0,     0},        /* RF7_S_HOUND */ /* Summon - 6 */
	{15,    0,     0,     0,     0},        /* RF7_S_ANIMAL */ /* Summon - 6 */
	{15,    0,     0,     0,     0},        /* RF7_S_HYDRA */
	{0,     0,     0,     0,     0},        /* RF7_XXX7 */
	{15,    0,     0,     0,     0},        /* RF7_S_THIEF */ /* Summon - 6 */
	{5,     0,     0,     0,     0},        /* RF7_S_BERTBILLTOM */ /* Summon - 2 */
	{0,     0,     0,     0,     0},        /* RF7_XXX8 */
	{15,    0,     0,     0,     0},        /* RF7_S_AINU */
	{0,     0,     0,     0,     0},        /* RF7_XX10 */
	{0,     0,     0,     0,     0},        /* RF7_XX11 */
	{14,    0,     0,     0,     0},        /* RF7_S_DRAGON */ /* Summon - 1 */
	{20,    0,     0,     0,     0},        /* RF7_S_HI_DRAGON */ /* Summon - 8 */
	{0,     0,     0,     0,     0},        /* RF7_XX12 */
	{0,     0,     0,     0,     0},        /* RF7_XX13 */
	{14,    0,     0,     0,     0},        /* RF7_S_DEMON */ /* Summon - 1 / 2-3 */
	{20,    0,     0,     0,     0},        /* RF7_S_HI_DEMON */ /* Summon - 8 */
	{0,     0,     0,     0,     0},        /* RF7_XX14 */
	{15,    0,     0,     0,     0},        /* RF7_S_UNIQUE */ /* Summon - 8 */
	{20,    0,     0,     0,     0},        /* RF7_S_HI_UNIQUE */ /* Summon - 8 */
	{12,    0,     0,     0,     0},        /* RF7_S_UNDEAD */ /* Summon - 1 */
	{20,    0,     0,     0,     0},        /* RF7_S_HI_UNDEAD */ /* Summon - 8 */
	{20,    0,     0,     0,     0}        /* RF7_S_WRAITH */ /* Summon - 8 */

};

/*
 * d_base:     base desirability for AI.
 * d_summ:     desriability for AI per monster level
 *                  times 0-3 based on number of clear spaces
 * d_hurt:     desirability for AI per monster spell power
 *                  times 0-3 based on damage taken
 * d_mana:     desirability for AI per monster spell power
 *                  times 0-2 based on mana shortage
 * d_esc:      desirability for AI per monster level
 *                  times 0-3 based on fear, and damage taken
 * d_tact:     desirability for AI per monster level, modified
 *                  times 0-3 based on proximity, min_range, and best_range
 * d_res:      category of 'resistability' checked by monster AI
 *                 for purposes of desirability.
 * d_range:    % of spell desirability retained for each step past 'range'
 */

byte spell_desire_RF4[32][8] =
{
/*     d_base	  d_hurt    d_esc	 d_res				    */
/*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 30,  0,   0,   5,	0,   0,	   0	  ,  100}, /* RF4_SHRIEK    */
	{ 40,  0,   0,   5,	0,   0,	   0	  ,    0}, /* RF4_LASH	    */
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_BOULDER   */
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_SHOT	    */
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_ARROW	    */
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_BOLT	    */
	{ 40,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_MISSL	    */
	{ 40,  0,   0,   5,	0,   0, LRN_PARCH ,  100}, /* RF4_PMISSL    */
	{ 75,  0,   0,   5,	0,   0, LRN_ACID  ,   90}, /* RF4_BRTH_ACID */
	{ 75,  0,   0,   5,	0,   0, LRN_ELEC  ,   90}, /* RF4_BRTH_ELEC */
	{ 75,  0,   0,   5,	0,   0, LRN_FIRE  ,   90}, /* RF4_BRTH_FIRE */
	{ 75,  0,   0,   5,	0,   0, LRN_COLD  ,   90}, /* RF4_BRTH_COLD */
	{ 65,  0,   0,   5,	0,   0, LRN_POIS  ,   90}, /* RF4_BRTH_POIS */
	{ 65,  0,   0,   5,	0,   0, LRN_PLAS  ,   90}, /* RF4_BRTH_PLAS */
	{ 65,  0,   0,   5,	0,   0, LRN_LIGHT  ,   90}, /* RF4_BRTH_LIGHT */
	{ 65,  0,   0,   5,	0,   0, LRN_DARK  ,   90}, /* RF4_BRTH_DARK */
	{ 65,  0,   0,   5,	0,   0, LRN_CONFU ,   90}, /* RF4_BRTH_CONFU*/
	{ 65,  0,   0,   5,	0,   0, LRN_SOUND ,   90}, /* RF4_BRTH_SOUND*/
	{ 65,  0,   0,   5,	0,   0, LRN_SHARD ,   90}, /* RF4_BRTH_SHARD*/
	{ 65,  0,   0,   5,	0,   0,	   0	  ,   90}, /* RF4_BRTH_INER */
	{ 65,  0,   0,   5,	0,   0, LRN_SOUND2,   90}, /* RF4_BRTH_GRAV */
	{  0,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XX1X */
	{ 65,  0,   0,   5,	0,   0, LRN_SOUND2,   90}, /* RF4_BRTH_FORCE*/
	{ 65,  0,   0,   5,	0,   0, LRN_NEXUS ,   90}, /* RF4_BRTH_NEXUS*/
	{ 65,  0,   0,   5,	0,   0, LRN_NETHR ,   90}, /* RF4_BRTH_NETHR*/
	{ 65,  0,   0,   5,	0,   0, LRN_CHAOS ,   90}, /* RF4_BRTH_CHAOS*/
	{ 65,  0,   0,   5,	0,   0, LRN_DISEN ,   90}, /* RF4_BRTH_DISEN*/
	{ 65,  0,   0,   5,	0,   0,	   0	  ,   90}, /* RF4_BRTH_TIME */
	{ 65,  0,   0,   5,	0,   0,	   0	  ,   90}, /* RF4_BRTH_MANA */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX1 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX2 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}  /* RF4_XXX3 */
};

byte spell_desire_RF5[32][8] =
{
/*     d_base	  d_hurt    d_esc	 d_res				    */
/*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 50,  0,   0,   0,	0,   0, LRN_ACID  ,  100}, /* RF5_BALL_ACID */
	{ 50,  0,   0,   0,	0,   0, LRN_ELEC  ,  100}, /* RF5_BALL_ELEC */
	{ 50,  0,   0,   0,	0,   0, LRN_FIRE  ,  100}, /* RF5_BALL_FIRE */
	{ 50,  0,   0,   0,	0,   0, LRN_COLD  ,  100}, /* RF5_BALL_COLD */
	{ 50,  0,   0,   0,	0,   0, LRN_POIS  ,  100}, /* RF5_BALL_POIS */
	{ 40,  0,   0,   0,	0,   0, LRN_LIGHT  ,  100}, /* RF5_BALL_LIGHT */
	{ 40,  0,   0,   0,	0,   0, LRN_DARK  ,  100}, /* RF5_BALL_DARK */
	{ 40,  0,   0,   0,	0,   0, LRN_CONFU ,  100}, /* RF5_BALL_CONFU*/
	{ 40,  0,   0,   0,	0,   0, LRN_SOUND ,  100}, /* RF5_BALL_SOUND*/
	{ 40,  0,   0,   0,	0,   0, LRN_SHARD ,  100}, /* RF5_BALL_SHARD*/
	{ 40,  0,   0,   0,	0,   0, 	0     ,  100}, /* RF5_BALL_METEOR */
	{ 40,  0,   0,   0,	0,   0, LRN_STORM ,  100}, /* RF5_BALL_STORM*/
	{ 40,  0,   0,   0,	0,   0, LRN_NETHR ,  100}, /* RF5_BALL_NETHR*/
	{ 40,  0,   0,   0,	0,   0, LRN_CHAOS ,  100}, /* RF5_BALL_CHAOS*/
	{ 40,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_BALL_MANA */
	{ 40,  0,   0,   0,	0,   0, LRN_WATER ,  100}, /* RF5_BALL_WATER*/
	{ 40,  0,   0,   0,	0,   0, LRN_ACID  ,  100}, /* RF5_BOLT_ACID */
	{ 40,  0,   0,   0,	0,   0, LRN_ELEC  ,  100}, /* RF5_BOLT_ELEC */
	{ 40,  0,   0,   0,	0,   0, LRN_FIRE  ,  100}, /* RF5_BOLT_FIRE */
	{ 40,  0,   0,   0,	0,   0, LRN_COLD  ,  100}, /* RF5_BOLT_COLD */
	{ 40,  0,   0,   0,	0,   0, LRN_POIS  ,  100}, /* RF5_BOLT_POIS */
	{ 50,  0,   0,   0,	0,   0, LRN_PLAS  ,  100}, /* RF5_BOLT_PLAS */
	{ 50,  0,   0,   0,	0,   0, LRN_ICE	  ,  100}, /* RF5_BOLT_ICE  */
	{ 35,  0,   0,   0,	0,   0, LRN_WATER ,  100}, /* RF5_BOLT_WATER*/
	{ 35,  0,   0,   0,	0,   0, LRN_NETHR ,  100}, /* RF5_BOLT_NETHR*/
	{ 30,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_BOLT_MANA */
	{ 45,  0,   0,   0,	0,   0,    0  	  ,  100}, /* RF5_BOLT_GRAV */
	{ 50,  0,   0,   0,	0,   0, LRN_ELEC  ,   90}, /* RF5_BEAM_ELEC */
	{ 50,  0,   0,   0,	0,   0, LRN_ICE	  ,   90}, /* RF5_BEAM_ICE  */
	{ 50,  0,   0,   0,	0,   0, LRN_NETHR ,   90}, /* RF5_BEAM_NETHR*/
	{ 0,   0,   0,   0,	0,   0,	 LRN_LAVA ,  100}, /* RF5_BEAM_LAVA*/
	{ 60,  0,   0,   0,	0,   0,	   0	  ,  100}  /* RF5_HOLY_ORB */
};


byte spell_desire_RF6[32][8] =
{
/*     d_base	  d_hurt    d_esc	 d_res				    */
/*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 50,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_HASTE	    */
	{ 15,  0,   0,  25,	0,   0,	   0  ,  100}, /* RF6_ADD_MANA  */
	{ 10,  0,   30,  0,	0,   0,	   0	  ,  100}, /* RF6_HEAL	    */
	{ 50,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_CURE	    */
	{ 27,  0,   0,   0,	10,  15,   0	  ,  100}, /* RF6_BLINK	    */
	{  3,  0,   0,   0,	20,  10,   0	  ,  100}, /* RF6_TPORT	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX1	    */
	{ 30,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_TELE_SELF_TO*/
	{ 30,  0,   0,   0,	0,   10,   0	  ,  100}, /* RF6_TELE_TO   */
	{  3,  0,   0,   0,	20,  10,   0	  ,  100}, /* RF6_TELE_AWAY */
	{  3,  0,   0,   0,	20,  10,LRN_NEXUS_SAVE,	   100}, /* RF6_TELE_LEVEL */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX3	    */
	{ 20,  0,   0,   0,	5,   0,	   0	  ,  100}, /* RF6_DARKNESS  */
	{ 25,  0,   0,   0,	5,   0,	   0	  ,  100}, /* RF6_TRAPS	    */
	{  0,  0,   0,   0,	0,   0,    0      ,  100}, /* RF6_XXX3    */
	{ 25,  0,   0,   15,	0,   0, LRN_MANA  ,  100}, /* RF6_DRAIN_MANA*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX4	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX5	    */
	{ 30,  0,   0,   0,	0,   0, LRN_SAVE  ,  100}, /* RF6_MIND_BLAST*/
	{ 40,  0,   0,   0,	0,   0, LRN_SAVE  ,  100}, /* RF6_BRAIN_SMASH*/
	{ 40,  0,   0,   0,	0,   0, LRN_SAVE  ,  100}, /* RF6_WOUND	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX6	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX7	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX8	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX9	    */
	{ 25,  0,   0,   0,	0,   0,	 LRN_SAVE ,  100}, /* RF6_HUNGER    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XX11	    */
	{ 25,  0,   0,   0,	0,   0, LRN_FEAR_SAVE,	  100}, /* RF6_SCARE	 */
	{ 30,  0,   0,   0,	0,   0, LRN_BLIND_SAVE,	   100}, /* RF6_BLIND	  */
	{ 30,  0,   0,   0,	0,   0, LRN_CONFU_SAVE,	   100}, /* RF6_CONF	  */
	{ 40,  0,   0,   0,	0,   0, LRN_FREE_SAVE,	  100}, /* RF6_SLOW	 */
	{ 35,  0,   0,   0,	0,   0, LRN_FREE_SAVE,	  100} /* RF6_HOLD	*/
};

byte spell_desire_RF7[32][8] =
{
 /*     d_base	  d_hurt    d_esc	 d_res				    */
 /*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_KIN	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX1	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX2	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_MONSTER */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_MONSTERS*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX3	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX4	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX5	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_ANT	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_SPIDER  */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HOUND   */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_ANIMAL  */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_RF7XXX7    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_THIEF   */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_BERTBILLTOM*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX8	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_AINU    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX10	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX11	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DRAGON  */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DRAGON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX12	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX13	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DEMON   */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DEMON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX14	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_UNIQUE  */
	{ 0,   18,  0,   0,	0,   0,	   0  	  ,  100}, /* RF7_S_HI_UNIQUE */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_UNDEAD  */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_UNDEAD*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}  /* RF7_S_WRAITH  */
};





/*
 * Character translations and definitions.  -JG-
 *
 * Upper case and lower case equivalents of a given ISO Latin-1 character.
 * A character's general "type"; types may be combined.
 *
 * Note that this table assumes use of the standard Angband extended fonts.
 *
 * Notice the accented characters in positions 191+.  If they don't appear
 * correct to you, then you are viewing this table in a non-Latin-1 font.
 * Individual ports are responsible for translations from the game's
 * internal Latin-1 to their system's character encoding(s) (unless ASCII).
 */
const byte char_tables[256][CHAR_TABLE_SLOTS] =
{
/* TO_UPPER, TO_LOWER, CHAR TYPE */
    {   0,     0,           0L },               /*        Empty      */
    {   1,     1,  CHAR_SYMBOL },               /*        Solid      */
    {   2,     2,  CHAR_SYMBOL },               /* Mostly solid      */
    {   3,     3,  CHAR_SYMBOL },               /* Wall pattern      */
    {   4,     4,  CHAR_SYMBOL },               /*    Many dots      */
    {   5,     5,  CHAR_SYMBOL },               /*  Medium dots      */
    {   6,     6,  CHAR_SYMBOL },               /*     Few dots      */
    {   7,     7,  CHAR_SYMBOL },               /*     Tiny dot      */
    {   8,     8,  CHAR_SYMBOL },               /*    Small dot      */
    {   9,     9,  CHAR_SYMBOL },               /*   Medium dot      */
    {  10,    10,  CHAR_SYMBOL },               /*    Large dot      */
    {  11,    11,  CHAR_SYMBOL },               /*       Rubble      */
    {  12,    12,  CHAR_SYMBOL },               /*     Treasure      */
    {  13,    13,  CHAR_SYMBOL },               /*  Closed door      */
    {  14,    14,  CHAR_SYMBOL },               /*    Open Door      */
    {  15,    15,  CHAR_SYMBOL },               /*  Broken door      */
    {  16,    16,  CHAR_SYMBOL },               /*       Pillar      */
    {  17,    17,  CHAR_SYMBOL },               /*        Water      */
    {  18,    18,  CHAR_SYMBOL },               /*         Tree      */
    {  19,    19,  CHAR_SYMBOL },               /*    Fire/lava      */
    {  20,    20,  CHAR_SYMBOL },               /*   Pit/portal      */
    {  22,    22,           0L },               /*       Unused      */
    {  22,    22,           0L },               /*       Unused      */
    {  23,    23,           0L },               /*       Unused      */
    {  24,    24,           0L },               /*       Unused      */
    {  25,    25,           0L },               /*       Unused      */
    {  26,    26,           0L },               /*       Unused      */
    {  27,    27,           0L },               /*       Unused      */
    {  28,    28,           0L },               /*       Unused      */
    {  29,    29,           0L },               /*       Unused      */
    {  30,    30,           0L },               /*       Unused      */
    {  31,    31,           0L },               /*       Unused      */

    {  32,    32,   CHAR_BLANK },               /*        Space      */
    {  33,    33,   CHAR_PUNCT },               /*            !      */
    {  34,    34,   CHAR_PUNCT },               /*            "      */
    {  35,    35,   CHAR_PUNCT },               /*            #      */
    {  36,    36,   CHAR_PUNCT },               /*            $      */
    {  37,    37,   CHAR_PUNCT },               /*            %      */
    {  38,    38,   CHAR_PUNCT },               /*            &      */
    {  39,    39,   CHAR_PUNCT },               /*            '      */
    {  40,    40,   CHAR_PUNCT },               /*            (      */
    {  41,    41,   CHAR_PUNCT },               /*            )      */
    {  42,    42,   CHAR_PUNCT },               /*            *      */
    {  43,    43,   CHAR_PUNCT },               /*            +      */
    {  44,    44,   CHAR_PUNCT },               /*            ,      */
    {  45,    45,   CHAR_PUNCT },               /*            -      */
    {  46,    46,   CHAR_PUNCT },               /*            .      */
    {  47,    47,   CHAR_PUNCT },               /*            /      */

    {  48,    48,   CHAR_DIGIT },               /*            0      */
    {  49,    49,   CHAR_DIGIT },               /*            1      */
    {  50,    50,   CHAR_DIGIT },               /*            2      */
    {  51,    51,   CHAR_DIGIT },               /*            3      */
    {  52,    52,   CHAR_DIGIT },               /*            4      */
    {  53,    53,   CHAR_DIGIT },               /*            5      */
    {  54,    54,   CHAR_DIGIT },               /*            6      */
    {  55,    55,   CHAR_DIGIT },               /*            7      */
    {  56,    56,   CHAR_DIGIT },               /*            8      */
    {  57,    57,   CHAR_DIGIT },               /*            9      */
    {  58,    58,   CHAR_DIGIT },               /*            :      */
    {  59,    59,   CHAR_DIGIT },               /*            ;      */

    {  60,    60,   CHAR_PUNCT },               /*            <      */
    {  61,    61,   CHAR_PUNCT },               /*            =      */
    {  62,    62,   CHAR_PUNCT },               /*            >      */
    {  63,    63,   CHAR_PUNCT },               /*            ?      */
    {  64,    64,   CHAR_PUNCT },               /*            @      */
    {  65,    97,   CHAR_UPPER | CHAR_VOWEL },  /*            A      */
    {  66,    98,   CHAR_UPPER },               /*            B      */
    {  67,    99,   CHAR_UPPER },               /*            C      */
    {  68,   100,   CHAR_UPPER },               /*            D      */
    {  69,   101,   CHAR_UPPER | CHAR_VOWEL },  /*            E      */
    {  70,   102,   CHAR_UPPER },               /*            F      */
    {  71,   103,   CHAR_UPPER },               /*            G      */
    {  72,   104,   CHAR_UPPER },               /*            H      */
    {  73,   105,   CHAR_UPPER | CHAR_VOWEL },  /*            I      */
    {  74,   106,   CHAR_UPPER },               /*            J      */
    {  75,   107,   CHAR_UPPER },               /*            K      */
    {  76,   108,   CHAR_UPPER },               /*            L      */
    {  77,   109,   CHAR_UPPER },               /*            M      */
    {  78,   110,   CHAR_UPPER },               /*            N      */
    {  79,   111,   CHAR_UPPER | CHAR_VOWEL },  /*            O      */
    {  80,   112,   CHAR_UPPER },               /*            P      */
    {  81,   113,   CHAR_UPPER },               /*            Q      */
    {  82,   114,   CHAR_UPPER },               /*            R      */
    {  83,   115,   CHAR_UPPER },               /*            S      */
    {  84,   116,   CHAR_UPPER },               /*            T      */
    {  85,   117,   CHAR_UPPER | CHAR_VOWEL },  /*            U      */
    {  86,   118,   CHAR_UPPER },               /*            V      */
    {  87,   119,   CHAR_UPPER },               /*            W      */
    {  88,   120,   CHAR_UPPER },               /*            X      */
    {  89,   121,   CHAR_UPPER },               /*            Y      */
    {  90,   122,   CHAR_UPPER },               /*            Z      */

    {  91,    91,   CHAR_PUNCT },               /*            [      */
    {  92,    92,   CHAR_PUNCT },               /*            \      */
    {  93,    93,   CHAR_PUNCT },               /*            ]      */
    {  94,    94,   CHAR_PUNCT },               /*            ^      */
    {  95,    95,   CHAR_PUNCT },               /*            _      */
    {  96,    96,   CHAR_PUNCT },               /*            `      */
    {  65,    97,   CHAR_LOWER | CHAR_VOWEL },  /*            a      */
    {  66,    98,   CHAR_LOWER },               /*            b      */
    {  67,    99,   CHAR_LOWER },               /*            c      */
    {  68,   100,   CHAR_LOWER },               /*            d      */
    {  69,   101,   CHAR_LOWER | CHAR_VOWEL },  /*            e      */
    {  70,   102,   CHAR_LOWER },               /*            f      */
    {  71,   103,   CHAR_LOWER },               /*            g      */
    {  72,   104,   CHAR_LOWER },               /*            h      */
    {  73,   105,   CHAR_LOWER | CHAR_VOWEL },  /*            i      */
    {  74,   106,   CHAR_LOWER },               /*            j      */
    {  75,   107,   CHAR_LOWER },               /*            k      */
    {  76,   108,   CHAR_LOWER },               /*            l      */
    {  77,   109,   CHAR_LOWER },               /*            m      */
    {  78,   110,   CHAR_LOWER },               /*            n      */
    {  79,   111,   CHAR_LOWER | CHAR_VOWEL },  /*            o      */
    {  80,   112,   CHAR_LOWER },               /*            p      */
    {  81,   113,   CHAR_LOWER },               /*            q      */
    {  82,   114,   CHAR_LOWER },               /*            r      */
    {  83,   115,   CHAR_LOWER },               /*            s      */
    {  84,   116,   CHAR_LOWER },               /*            t      */
    {  85,   117,   CHAR_LOWER | CHAR_VOWEL },  /*            u      */
    {  86,   118,   CHAR_LOWER },               /*            v      */
    {  87,   119,   CHAR_LOWER },               /*            w      */
    {  88,   120,   CHAR_LOWER },               /*            x      */
    {  89,   121,   CHAR_LOWER },               /*            y      */
    {  90,   122,   CHAR_LOWER },               /*            z      */
    { 123,   123,   CHAR_PUNCT },               /*            {    */
    { 124,   124,   CHAR_PUNCT },               /*            |      */
    { 125,   125,   CHAR_PUNCT },               /*            }      */
    { 126,   126,   CHAR_PUNCT },               /*            ~      */
    { 127,   127,  CHAR_SYMBOL },               /* Wall pattern      */

    { 128,   128,           0L },               /*       Unused      */
    { 129,   129,           0L },               /*       Unused      */
    { 130,   130,           0L },               /*       Unused      */
    { 131,   131,           0L },               /*       Unused      */
    { 132,   132,           0L },               /*       Unused      */
    { 133,   133,           0L },               /*       Unused      */
    { 134,   134,           0L },               /*       Unused      */
    { 135,   135,           0L },               /*       Unused      */
    { 136,   136,           0L },               /*       Unused      */
    { 137,   137,           0L },               /*       Unused      */
    { 138,   138,           0L },               /*       Unused      */
    { 139,   139,           0L },               /*       Unused      */
    { 140,   140,           0L },               /*       Unused      */
    { 141,   141,           0L },               /*       Unused      */
    { 142,   142,           0L },               /*       Unused      */
    { 143,   143,           0L },               /*       Unused      */
    { 144,   144,           0L },               /*       Unused      */
    { 145,   145,           0L },               /*       Unused      */
    { 146,   146,           0L },               /*       Unused      */
    { 147,   147,           0L },               /*       Unused      */
    { 148,   148,           0L },               /*       Unused      */
    { 149,   149,           0L },               /*       Unused      */
    { 150,   150,           0L },               /*       Unused      */
    { 151,   151,           0L },               /*       Unused      */
    { 152,   152,           0L },               /*       Unused      */
    { 153,   153,           0L },               /*       Unused      */
    { 154,   154,           0L },               /*       Unused      */
    { 155,   155,           0L },               /*       Unused      */
    { 156,   156,           0L },               /*       Unused      */
    { 157,   157,           0L },               /*       Unused      */
    { 158,   158,           0L },               /*       Unused      */
    { 159,   159,           0L },               /*       Unused      */
    { 160,   160,           0L },               /*       Unused      */

    { 161,   161,   CHAR_PUNCT },               /*       iexcl   ¡   */
    { 162,   162,   CHAR_PUNCT },               /*        euro   ¢   */
    { 163,   163,   CHAR_PUNCT },               /*       pound   £   */
    { 164,   164,   CHAR_PUNCT },               /*      curren   ¤   */
    { 165,   165,   CHAR_PUNCT },               /*         yen   ¥   */
    { 166,   166,   CHAR_PUNCT },               /*      brvbar   ¦   */
    { 167,   167,   CHAR_PUNCT },               /*        sect   §   */
    { 168,   168,  CHAR_SYMBOL },               /*  Bolt - vert      */
    { 169,   169,  CHAR_SYMBOL },               /*  Bolt - horz      */
    { 170,   170,  CHAR_SYMBOL },               /*  Bolt -rdiag      */
    { 171,   171,  CHAR_SYMBOL },               /*  Bolt -ldiag      */
    { 172,   172,  CHAR_SYMBOL },               /*  Spell-cloud      */
    { 173,   173,  CHAR_SYMBOL },               /*   Spell-elec      */
    { 174,   174,  CHAR_SYMBOL },               /*  Spell-explo      */

    { 175,   175,           0L },               /*       Unused      */
    { 176,   176,           0L },               /*       Unused      */
    { 177,   177,           0L },               /*       Unused      */
    { 178,   178,           0L },               /*       Unused      */
    { 179,   179,           0L },               /*       Unused      */
    { 180,   180,           0L },               /*       Unused      */
    { 181,   181,           0L },               /*       Unused      */
    { 182,   182,           0L },               /*       Unused      */
    { 183,   183,           0L },               /*       Unused      */
    { 184,   184,           0L },               /*       Unused      */
    { 185,   185,           0L },               /*       Unused      */
    { 186,   186,           0L },               /*       Unused      */
    { 187,   187,           0L },               /*       Unused      */
    { 188,   188,           0L },               /*       Unused      */
    { 189,   189,           0L },               /*       Unused      */
    { 190,   190,           0L },               /*       Unused      */

    { 191,   191,   CHAR_PUNCT },               /*      iquest   ¿   */
    { 192,   224,   CHAR_UPPER | CHAR_VOWEL },  /*      Agrave   À   */
    { 193,   225,   CHAR_UPPER | CHAR_VOWEL },  /*      Aacute   Á   */
    { 194,   226,   CHAR_UPPER | CHAR_VOWEL },  /*       Acirc   Â   */
    { 195,   227,   CHAR_UPPER | CHAR_VOWEL },  /*      Atilde   Ã   */
    { 196,   228,   CHAR_UPPER | CHAR_VOWEL },  /*        Auml   Ä   */
    { 197,   229,   CHAR_UPPER | CHAR_VOWEL },  /*       Aring   Å   */
    { 198,   230,   CHAR_UPPER | CHAR_VOWEL },  /*       Aelig   Æ   */
    { 199,   231,   CHAR_UPPER },               /*      Ccedil   Ç   */
    { 200,   232,   CHAR_UPPER | CHAR_VOWEL },  /*      Egrave   È   */
    { 201,   233,   CHAR_UPPER | CHAR_VOWEL },  /*      Eacute   É   */
    { 202,   234,   CHAR_UPPER | CHAR_VOWEL },  /*       Ecirc   Ê   */
    { 203,   235,   CHAR_UPPER | CHAR_VOWEL },  /*        Euml   Ë   */
    { 204,   236,   CHAR_UPPER | CHAR_VOWEL },  /*      Igrave   Ì   */
    { 205,   237,   CHAR_UPPER | CHAR_VOWEL },  /*      Iacute   Í   */
    { 206,   238,   CHAR_UPPER | CHAR_VOWEL },  /*       Icirc   Î   */
    { 207,   239,   CHAR_UPPER | CHAR_VOWEL },  /*        Iuml   Ï   */
    { 208,   240,   CHAR_UPPER },               /*         ETH   Ð   */
    { 209,   241,   CHAR_UPPER },               /*      Ntilde   Ñ   */
    { 210,   242,   CHAR_UPPER | CHAR_VOWEL },  /*      Ograve   Ò   */
    { 211,   243,   CHAR_UPPER | CHAR_VOWEL },  /*      Oacute   Ó   */
    { 212,   244,   CHAR_UPPER | CHAR_VOWEL },  /*       Ocirc   Ô   */
    { 213,   245,   CHAR_UPPER | CHAR_VOWEL },  /*      Otilde   Õ   */
    { 214,   246,   CHAR_UPPER | CHAR_VOWEL },  /*        Ouml   Ö   */
    { 215,   215,           0L },               /*       Unused      */
    { 216,   248,   CHAR_UPPER | CHAR_VOWEL },  /*      Oslash   Ø   */
    { 217,   249,   CHAR_UPPER | CHAR_VOWEL },  /*      Ugrave   Ù   */
    { 218,   250,   CHAR_UPPER | CHAR_VOWEL },  /*      Uacute   Ú   */
    { 219,   251,   CHAR_UPPER | CHAR_VOWEL },  /*       Ucirc   Û   */
    { 220,   252,   CHAR_UPPER | CHAR_VOWEL },  /*        Uuml   Ü   */
    { 221,   253,   CHAR_UPPER },               /*      Yacute   Ý   */
    { 222,   254,   CHAR_UPPER },               /*       THORN   Þ   */
    { 223,   223,   CHAR_LOWER },               /*       szlig   ß   */

    { 192,   224,   CHAR_LOWER | CHAR_VOWEL },  /*      agrave   à   */
    { 193,   225,   CHAR_LOWER | CHAR_VOWEL },  /*      aacute   á   */
    { 194,   226,   CHAR_LOWER | CHAR_VOWEL },  /*       acirc   â   */
    { 195,   227,   CHAR_LOWER | CHAR_VOWEL },  /*      atilde   ã   */
    { 196,   228,   CHAR_LOWER | CHAR_VOWEL },  /*        auml   ä   */
    { 197,   229,   CHAR_LOWER | CHAR_VOWEL },  /*       aring   å   */
    { 198,   230,   CHAR_LOWER | CHAR_VOWEL },  /*       aelig   æ   */
    { 199,   231,   CHAR_LOWER },               /*      ccedil   ç   */
    { 200,   232,   CHAR_LOWER | CHAR_VOWEL },  /*      egrave   è   */
    { 201,   233,   CHAR_LOWER | CHAR_VOWEL },  /*      eacute   é   */
    { 202,   234,   CHAR_LOWER | CHAR_VOWEL },  /*       ecirc   ê   */
    { 203,   235,   CHAR_LOWER | CHAR_VOWEL },  /*        euml   ë   */
    { 204,   236,   CHAR_LOWER | CHAR_VOWEL },  /*      igrave   ì   */
    { 205,   237,   CHAR_LOWER | CHAR_VOWEL },  /*      iacute   í   */
    { 206,   238,   CHAR_LOWER | CHAR_VOWEL },  /*       icirc   î   */
    { 207,   239,   CHAR_LOWER | CHAR_VOWEL },  /*        iuml   ï   */
    { 208,   240,   CHAR_LOWER },               /*         eth   ð   */
    { 209,   241,   CHAR_LOWER },               /*      ntilde   ñ   */
    { 210,   242,   CHAR_LOWER | CHAR_VOWEL },  /*      ograve   ò   */
    { 211,   243,   CHAR_LOWER | CHAR_VOWEL },  /*      oacute   ó   */
    { 212,   244,   CHAR_LOWER | CHAR_VOWEL },  /*       ocirc   ô   */
    { 213,   245,   CHAR_LOWER | CHAR_VOWEL },  /*      otilde   õ   */
    { 214,   246,   CHAR_LOWER | CHAR_VOWEL },  /*        ouml   ö   */
    { 247,   247,           0L },               /*       Unused      */
    { 216,   248,   CHAR_LOWER | CHAR_VOWEL },  /*      oslash   ø   */
    { 217,   249,   CHAR_LOWER | CHAR_VOWEL },  /*      ugrave   ù   */
    { 218,   250,   CHAR_LOWER | CHAR_VOWEL },  /*      uacute   ú   */
    { 219,   251,   CHAR_LOWER | CHAR_VOWEL },  /*       ucirc   û   */
    { 220,   252,   CHAR_LOWER | CHAR_VOWEL },  /*        uuml   ü   */
    { 221,   253,   CHAR_LOWER },               /*      yacute   ý   */
    { 222,   254,   CHAR_LOWER },               /*       thorn   þ   */
    { 121,   255,   CHAR_LOWER },               /*        yuml   ÿ   */
};





/*
 * Translate from encodes to extended 8-bit characters and back again.
 */
const xchar_type latin1_encode[] =
{
    { "`A", 192 },  { "'A", 193 },  { "^A", 194 },  { "~A", 195 },
    { "\"A", 196 },  { "*A", 197 },  { ",C", 199 },  { "`E", 200 },
    { "'E", 201 },  { "^E", 202 }, { "\"E", 203 },  { "`I", 204 },
    { "'I", 205 },  { "^I", 206 }, { "\"I", 207 },  { "~N", 209 },
    { "`O", 210 },  { "'O", 211 },  { "^O", 212 },  { "~O", 213 },
	{ "\"O", 214 },  { "/O", 216 },  { "`U", 217 },  { "'U", 218 },
    { "^U", 219 }, { "\"U", 220 },  { "'Y", 221 },  { "`a", 224 },
    { "'a", 225 },  { "^a", 226 },  { "~a", 227 }, { "\"a", 228 },
    { "*a", 229 },  { ",c", 231 },  { "`e", 232 },  { "'e", 233 },
    { "^e", 234 }, { "\"e", 235 },  { "`i", 236 },  { "'i", 237 },
    { "^i", 238 }, { "\"i", 239 },  { "~n", 241 },  { "`o", 242 },
    { "'o", 243 },  { "^o", 244 },  { "~o", 245 }, { "\"o", 246 },
    { "/o", 248 },  { "`u", 249 },  { "'u", 250 },  { "^u", 251 },
    { "\"u", 252 },  { "'y", 253 }, { "\"y", 255 },

    { "iexcl", 161 }, { "euro", 162 }, { "pound", 163 }, { "curren", 164 },
    { "yen", 165 },   { "brvbar", 166 }, { "sect", 167 }, { "Agrave", 192 },
    { "Aacute", 193 }, { "Acirc", 194 }, { "Atilde", 195 }, { "Auml", 196 },
    { "Aring", 197 }, { "Aelig", 198 }, { "Ccedil", 199 }, { "Egrave", 200 },
    { "Eacute", 201 }, { "Ecirc", 202 }, { "Euml", 203 }, { "Igrave", 204 },
    { "Iacute", 205 }, { "Icirc", 206 }, { "Iuml", 207 }, { "ETH", 208 },
    { "Ntilde", 209 }, { "Ograve", 210 }, { "Oacute", 211 }, { "Ocirc", 212 },
    { "Otilde", 213 }, { "Ouml", 214 }, { "Oslash", 216 }, { "Ugrave", 217 },
    { "Uacute", 218 }, { "Ucirc", 219 }, { "Uuml", 220 }, { "Yacute", 221 },
    { "THORN", 222 }, { "szlig", 223 }, { "agrave", 224 }, { "aacute", 225 },
    { "acirc", 226 }, { "atilde", 227 }, { "auml", 228 }, { "aring", 229 },
    { "aelig", 230 }, { "ccedil", 231 }, { "egrave", 232 }, { "eacute", 233 },
    { "ecirc", 234 }, { "euml", 235 }, { "igrave", 236 }, { "iacute", 237 },
    { "icirc", 238 }, { "iuml", 239 }, { "eth", 240 },   { "ntilde", 241 },
    { "ograve", 242 }, { "oacute", 243 }, { "ocirc", 244 }, { "otilde", 245 },
    { "ouml", 246 }, { "oslash", 248 }, { "ugrave", 249 }, { "uacute", 250 },
    { "ucirc", 251 }, { "uuml", 252 }, { "yacute", 253 }, { "thorn", 254 },
    { "yuml", 255 },   { "\0", 0 }
};



cptr squelch_status[SQUELCH_OPT_MAX] =
{
	"Never Squelch",
    "Never Pickup",
    "Always Pickup",
    "Always Squelch",
};

const byte squelch_status_color[SQUELCH_OPT_MAX] =
{
	TERM_YELLOW,
	TERM_L_GREEN,
	TERM_L_UMBER,
    TERM_L_RED,
};

/*
 * 11x25 room, or 9x23 of room after the permanent rock.
 * Walls open over time.  Values of 10 are the permanent borders of the cave.
 */
const byte arena_level_map[ARENA_LEVEL_HGT][ARENA_LEVEL_WID] =
{
	{10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10},
	{10, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9,10},
	{10, 9,10, 8, 5, 5, 6, 5, 4, 5, 4, 5, 2, 5, 4, 5, 4, 5, 6, 5, 5, 7,10, 9,10},
	{10, 9, 7, 7, 9, 6, 4, 3, 3, 4, 1, 2, 3, 2, 1, 4, 3, 3, 4, 6, 9, 7, 7, 9,10},
	{10, 9, 8, 8, 6, 5, 4, 5, 3, 4, 1,10, 1,10, 1, 4, 3, 5, 4, 5, 6, 8, 8, 9,10},
	{10,10, 8, 7, 8, 5, 4, 2, 1, 1, 0, 0, 0, 0, 0, 1, 1, 2, 4, 5, 8, 7, 8,10,10},
	{10, 9, 8, 8, 6, 5, 4, 5, 3, 4, 1,10, 1,10, 1, 4, 3, 5, 4, 5, 6, 8, 8, 9,10},
	{10, 9, 7, 7, 9, 6, 4, 3, 3, 4, 1, 2, 3, 2, 1, 4, 3, 3, 4, 6, 9, 7, 7, 9,10},
	{10, 9,10, 8, 5, 5, 6, 5, 4, 5, 4, 5, 2, 5, 4, 5, 4, 5, 6, 5, 5, 8,10, 9,10},
	{10, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9,10},
	{10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}
};

const byte pit_room_maps[MAX_PIT_PATTERNS][PIT_HEIGHT][PIT_WIDTH] =
{
	/* Original Pit with one hard monster in center */
	{
		{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
		{0,1,1,2,2,3,3,4,5,5,5,4,3,3,2,2,1,1,0},
		{0,1,1,2,2,3,3,4,6,7,6,4,3,3,2,2,1,1,0},
		{0,1,1,2,2,3,3,4,5,5,5,4,3,3,2,2,1,1,0},
		{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	},
	/* One top monster with tougher escorts */
	{
		{0,0,1,1,1,2,2,3,3,4,3,2,2,2,1,1,1,0,0},
		{0,1,2,3,3,4,4,5,5,6,5,5,4,4,3,3,2,1,0},
		{1,2,3,4,4,5,5,6,6,7,6,6,5,5,4,4,3,2,1},
		{0,1,2,3,3,4,4,5,5,6,5,5,4,4,3,3,2,1,0},
		{0,0,1,1,1,2,2,3,3,4,3,2,2,2,1,1,1,0,0},
	},
	/* Two top monsters with toughest escorts */
	{
		{0,0,0,1,1,1,2,2,2,3,2,2,2,1,1,1,0,0,0},
		{0,1,3,4,5,6,5,4,3,3,3,4,5,6,5,4,3,1,0},
		{0,2,3,4,5,7,5,4,3,3,3,4,5,7,5,4,3,2,0},
		{0,1,3,4,5,6,5,4,3,3,3,4,5,6,5,4,3,1,0},
		{0,0,0,1,1,1,2,2,2,3,2,2,2,1,1,1,0,0,0},
	}

};

/* Some useful constants */
cptr standard_home_letters =   "abcfmnoqrtuvyz13456790AB";
cptr roguelike_home_letters =  "acfhmnoqruvyz13456790ABD";
cptr standard_equip_letters =  "abcdefghijklmnopqrstuvw";
cptr roguelike_equip_letters = "acdefgimopqrstuwvxzABCD";

const brands_structure brands_info_nppangband[10] =
{
	{TR1_BRAND_POIS, 3, RF3_IM_POIS, 0L, 1, 1, 1, "resist poison"},
	{TR1_BRAND_ACID, 3, RF3_IM_ACID, ELEMENT_ACID, 4, 5, 1, "resist acid"},
	{TR1_BRAND_ELEC, 3, RF3_IM_ELEC, (ELEMENT_WATER | ELEMENT_BWATER), 4, 5, 1, "resist electricity"},
	{TR1_BRAND_FIRE, 3, RF3_IM_FIRE, (ELEMENT_LAVA), 5, 5, 1, "resist fire"},
	{TR1_BRAND_FIRE, 3, RF3_IM_FIRE, (ELEMENT_FIRE | ELEMENT_BWATER), 4, 4, 1, "resist fire"},
	{TR1_BRAND_FIRE, 3, RF3_IM_FIRE, (ELEMENT_WATER), 1, 1, 2, "resist fire"},
	{TR1_BRAND_COLD, 3, RF3_IM_COLD, (ELEMENT_ICE), 5, 5, 1, "resist cold"},
	{TR1_BRAND_COLD, 3, RF3_IM_COLD, (ELEMENT_WATER), 4, 4, 1, "resist cold"},
	{TR1_BRAND_COLD, 3, RF3_IM_COLD, (ELEMENT_LAVA), 1, 1, 2, "resist cold"},
	{TR1_BRAND_COLD, 3, RF3_IM_COLD, (ELEMENT_BMUD | ELEMENT_BWATER), 1, 1, 2,"resist cold"}
};


const slays_structure slays_info_nppangband[11] =
{
	{TR1_SLAY_ANIMAL, 2, RF3_ANIMAL, 	"animals"},
	{TR1_SLAY_EVIL, 2, RF3_EVIL, 		"evil creatures"},
	{TR1_SLAY_UNDEAD, 3, RF3_UNDEAD,	"the undead"},
	{TR1_SLAY_DEMON, 3, RF3_DEMON, 		"demons"},
	{TR1_SLAY_ORC, 3, RF3_ORC, 			"orcs"},
	{TR1_SLAY_TROLL, 3, RF3_TROLL, 		"trolls"},
	{TR1_SLAY_GIANT, 3, RF3_GIANT, 		"giants"},
	{TR1_SLAY_DRAGON, 3, RF3_DRAGON, 	"dragons"},
	{TR1_KILL_DRAGON, 5, RF3_DRAGON, 	"dragons"},
	{TR1_KILL_DEMON, 5, RF3_DEMON, 		"demons"},
	{TR1_KILL_UNDEAD, 5, RF3_UNDEAD,	"the undead"},

};

/*
 * This table is a hack, as it works differently than brands info for Angband.
 * Only creatures who are succeptible to the element take extra damage.
 */
const slays_structure brands_info_nppmoria[4] =
{
	{TR1_BRAND_FIRE, 2, RF3_HURT_FIRE, "are susceptible to fire"},
	{TR1_BRAND_COLD, 2, RF3_HURT_COLD, "are susceptible to cold"},
	{TR1_BRAND_ACID, 2, RF3_HURT_ACID, "are susceptible to acid"},
	{TR1_BRAND_POIS, 2, RF3_HURT_POIS, "are susceptible to poison"},
};


const slays_structure slays_info_nppmoria[4] =
{
	{TR1_SLAY_ANIMAL, 2, RF3_ANIMAL, 	"animals"},
	{TR1_SLAY_EVIL, 2, RF3_EVIL, 		"evil creatures"},
	{TR1_SLAY_UNDEAD, 3, RF3_UNDEAD,	"the undead"},
	{TR1_SLAY_DRAGON, 4, RF3_DRAGON, 	"dragons"},
};



const mon_susceptibility_struct mon_suscept[4] =
{
	{TR1_BRAND_FIRE, RF3_HURT_FIRE, "fire"},
	{TR1_BRAND_COLD, RF3_HURT_COLD, "cold"},
	{TR1_BRAND_ACID, RF3_HURT_ACID, "acid"},
	{TR1_BRAND_POIS, RF3_HURT_COLD, "poison"},
};
