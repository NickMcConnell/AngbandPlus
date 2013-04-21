/* File: tables.c */

/* Purpose: Angband Tables */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
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
* Stat Table (INT/WIS) -- Number of half-spells per level
*/
byte adj_mag_study[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	1       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	2       /* 12 */,
	2       /* 13 */,
	2       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	2       /* 18/00-18/09 */,
	2       /* 18/10-18/19 */,
	2       /* 18/20-18/29 */,
	2       /* 18/30-18/39 */,
	2       /* 18/40-18/49 */,
	3       /* 18/50-18/59 */,
	3       /* 18/60-18/69 */,
	3       /* 18/70-18/79 */,
	3       /* 18/80-18/89 */,
	4       /* 18/90-18/99 */,
	4       /* 18/100-18/109 */,
	4       /* 18/110-18/119 */,
	5       /* 18/120-18/129 */,
	5       /* 18/130-18/139 */,
	5       /* 18/140-18/149 */,
	5       /* 18/150-18/159 */,
	5       /* 18/160-18/169 */,
	5       /* 18/170-18/179 */,
	5       /* 18/180-18/189 */,
	5       /* 18/190-18/199 */,
	5       /* 18/200-18/209 */,
	6       /* 18/210-18/219 */,
	6       /* 18/220+ */
};


/*
* Stat Table (INT/WIS) -- extra half-mana-points per level
*/
byte adj_mag_mana[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	2       /* 9 */,
	2       /* 10 */,
	2       /* 11 */,
	2       /* 12 */,
	2       /* 13 */,
	2       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	3       /* 18/00-18/09 */,
	3       /* 18/10-18/19 */,
	3       /* 18/20-18/29 */,
	3       /* 18/30-18/39 */,
	3       /* 18/40-18/49 */,
	4       /* 18/50-18/59 */,
	4       /* 18/60-18/69 */,
	5       /* 18/70-18/79 */,
	6       /* 18/80-18/89 */,
	7       /* 18/90-18/99 */,
	8       /* 18/100-18/109 */,
	9       /* 18/110-18/119 */,
	10      /* 18/120-18/129 */,
	11      /* 18/130-18/139 */,
	12      /* 18/140-18/149 */,
	13      /* 18/150-18/159 */,
	14      /* 18/160-18/169 */,
	15      /* 18/170-18/179 */,
	16      /* 18/180-18/189 */,
	16      /* 18/190-18/199 */,
	17      /* 18/200-18/209 */,
	17      /* 18/210-18/219 */,
	18      /* 18/220+ */
};


/*
* Stat Table (INT/WIS) -- Minimum failure rate (percentage)
*/
byte adj_mag_fail[] =
{
	99      /* 3 */,
	99      /* 4 */,
	99      /* 5 */,
	99      /* 6 */,
	99      /* 7 */,
	50      /* 8 */,
	30      /* 9 */,
	20      /* 10 */,
	15      /* 11 */,
	12      /* 12 */,
	11      /* 13 */,
	10      /* 14 */,
	9       /* 15 */,
	8       /* 16 */,
	7       /* 17 */,
	6       /* 18/00-18/09 */,
	6       /* 18/10-18/19 */,
	5       /* 18/20-18/29 */,
	5       /* 18/30-18/39 */,
	5       /* 18/40-18/49 */,
	4       /* 18/50-18/59 */,
	4       /* 18/60-18/69 */,
	4       /* 18/70-18/79 */,
	4       /* 18/80-18/89 */,
	3       /* 18/90-18/99 */,
	3       /* 18/100-18/109 */,
	2       /* 18/110-18/119 */,
	2       /* 18/120-18/129 */,
	2       /* 18/130-18/139 */,
	2       /* 18/140-18/149 */,
	1       /* 18/150-18/159 */,
	1       /* 18/160-18/169 */,
	1       /* 18/170-18/179 */,
	1       /* 18/180-18/189 */,
	1       /* 18/190-18/199 */,
	0       /* 18/200-18/209 */,
	0       /* 18/210-18/219 */,
	0       /* 18/220+ */
};


/*
* Stat Table (INT/WIS) -- Various things
*/
byte adj_mag_stat[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	1       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	1       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	3       /* 18/00-18/09 */,
	3       /* 18/10-18/19 */,
	3       /* 18/20-18/29 */,
	3       /* 18/30-18/39 */,
	3       /* 18/40-18/49 */,
	4       /* 18/50-18/59 */,
	4       /* 18/60-18/69 */,
	5       /* 18/70-18/79 */,
	6       /* 18/80-18/89 */,
	7       /* 18/90-18/99 */,
	8       /* 18/100-18/109 */,
	9       /* 18/110-18/119 */,
	10      /* 18/120-18/129 */,
	11      /* 18/130-18/139 */,
	12      /* 18/140-18/149 */,
	13      /* 18/150-18/159 */,
	14      /* 18/160-18/169 */,
	15      /* 18/170-18/179 */,
	16      /* 18/180-18/189 */,
	17      /* 18/190-18/199 */,
	18      /* 18/200-18/209 */,
	19      /* 18/210-18/219 */,
	20      /* 18/220+ */
};


/*
* Stat Table (CHA) -- payment percentages
*/
byte adj_cha_gold[] =
{
	130     /* 3 */,
	125     /* 4 */,
	122     /* 5 */,
	120     /* 6 */,
	118     /* 7 */,
	116     /* 8 */,
	114     /* 9 */,
	112     /* 10 */,
	110     /* 11 */,
	108     /* 12 */,
	106     /* 13 */,
	104     /* 14 */,
	103     /* 15 */,
	102     /* 16 */,
	101     /* 17 */,
	100     /* 18/00-18/09 */,
	99      /* 18/10-18/19 */,
	98      /* 18/20-18/29 */,
	97      /* 18/30-18/39 */,
	96      /* 18/40-18/49 */,
	95      /* 18/50-18/59 */,
	94      /* 18/60-18/69 */,
	93      /* 18/70-18/79 */,
	92      /* 18/80-18/89 */,
	91      /* 18/90-18/99 */,
	90      /* 18/100-18/109 */,
	89      /* 18/110-18/119 */,
	88      /* 18/120-18/129 */,
	87      /* 18/130-18/139 */,
	86      /* 18/140-18/149 */,
	85      /* 18/150-18/159 */,
	84      /* 18/160-18/169 */,
	83      /* 18/170-18/179 */,
	82      /* 18/180-18/189 */,
	81      /* 18/190-18/199 */,
	80      /* 18/200-18/209 */,
	79      /* 18/210-18/219 */,
	78      /* 18/220+ */
};


/*
* Stat Table (INT) -- Magic devices
*/
byte adj_int_dev[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	1       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	1       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	3       /* 18/00-18/09 */,
	3       /* 18/10-18/19 */,
	4       /* 18/20-18/29 */,
	4       /* 18/30-18/39 */,
	5       /* 18/40-18/49 */,
	5       /* 18/50-18/59 */,
	6       /* 18/60-18/69 */,
	6       /* 18/70-18/79 */,
	7       /* 18/80-18/89 */,
	7       /* 18/90-18/99 */,
	8       /* 18/100-18/109 */,
	9       /* 18/110-18/119 */,
	10      /* 18/120-18/129 */,
	11      /* 18/130-18/139 */,
	12      /* 18/140-18/149 */,
	13      /* 18/150-18/159 */,
	14      /* 18/160-18/169 */,
	15      /* 18/170-18/179 */,
	16      /* 18/180-18/189 */,
	17      /* 18/190-18/199 */,
	18      /* 18/200-18/209 */,
	19      /* 18/210-18/219 */,
	20      /* 18/220+ */
};


/*
* Stat Table (WIS) -- Saving throw
*/
byte adj_wis_sav[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	1       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	1       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	3       /* 18/00-18/09 */,
	3       /* 18/10-18/19 */,
	3       /* 18/20-18/29 */,
	3       /* 18/30-18/39 */,
	3       /* 18/40-18/49 */,
	4       /* 18/50-18/59 */,
	4       /* 18/60-18/69 */,
	5       /* 18/70-18/79 */,
	5       /* 18/80-18/89 */,
	6       /* 18/90-18/99 */,
	7       /* 18/100-18/109 */,
	8       /* 18/110-18/119 */,
	9       /* 18/120-18/129 */,
	10      /* 18/130-18/139 */,
	11      /* 18/140-18/149 */,
	12      /* 18/150-18/159 */,
	13      /* 18/160-18/169 */,
	14      /* 18/170-18/179 */,
	15      /* 18/180-18/189 */,
	16      /* 18/190-18/199 */,
	17      /* 18/200-18/209 */,
	18      /* 18/210-18/219 */,
	19      /* 18/220+ */
};


/*
* Stat Table (DEX) -- disarming
*/
byte adj_dex_dis[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	0       /* 8 */,
	0       /* 9 */,
	0       /* 10 */,
	0       /* 11 */,
	0       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	1       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	4       /* 18/00-18/09 */,
	4       /* 18/10-18/19 */,
	4       /* 18/20-18/29 */,
	4       /* 18/30-18/39 */,
	5       /* 18/40-18/49 */,
	5       /* 18/50-18/59 */,
	5       /* 18/60-18/69 */,
	6       /* 18/70-18/79 */,
	6       /* 18/80-18/89 */,
	7       /* 18/90-18/99 */,
	8       /* 18/100-18/109 */,
	8       /* 18/110-18/119 */,
	8       /* 18/120-18/129 */,
	8       /* 18/130-18/139 */,
	8       /* 18/140-18/149 */,
	9       /* 18/150-18/159 */,
	9       /* 18/160-18/169 */,
	9       /* 18/170-18/179 */,
	9       /* 18/180-18/189 */,
	9       /* 18/190-18/199 */,
	10      /* 18/200-18/209 */,
	10      /* 18/210-18/219 */,
	10      /* 18/220+ */
};


/*
* Stat Table (INT) -- disarming
*/
byte adj_int_dis[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	1       /* 8 */,
	1       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	1       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	2       /* 15 */,
	2       /* 16 */,
	2       /* 17 */,
	3       /* 18/00-18/09 */,
	3       /* 18/10-18/19 */,
	3       /* 18/20-18/29 */,
	4       /* 18/30-18/39 */,
	4       /* 18/40-18/49 */,
	5       /* 18/50-18/59 */,
	6       /* 18/60-18/69 */,
	7       /* 18/70-18/79 */,
	8       /* 18/80-18/89 */,
	9       /* 18/90-18/99 */,
	10      /* 18/100-18/109 */,
	10      /* 18/110-18/119 */,
	11      /* 18/120-18/129 */,
	12      /* 18/130-18/139 */,
	13      /* 18/140-18/149 */,
	14      /* 18/150-18/159 */,
	15      /* 18/160-18/169 */,
	16      /* 18/170-18/179 */,
	17      /* 18/180-18/189 */,
	18      /* 18/190-18/199 */,
	19      /* 18/200-18/209 */,
	19      /* 18/210-18/219 */,
	20      /* 18/220+ */
};


/*
* Stat Table (DEX) -- bonus to ac (plus 128)
*/
byte adj_dex_ta[] =
{
	128 + -4        /* 3 */,
	128 + -3        /* 4 */,
	128 + -2        /* 5 */,
	128 + -1        /* 6 */,
	128 + 0 /* 7 */,
	128 + 0 /* 8 */,
	128 + 0 /* 9 */,
	128 + 0 /* 10 */,
	128 + 0 /* 11 */,
	128 + 0 /* 12 */,
	128 + 0 /* 13 */,
	128 + 0 /* 14 */,
	128 + 1 /* 15 */,
	128 + 1 /* 16 */,
	128 + 1 /* 17 */,
	128 + 2 /* 18/00-18/09 */,
	128 + 2 /* 18/10-18/19 */,
	128 + 2 /* 18/20-18/29 */,
	128 + 2 /* 18/30-18/39 */,
	128 + 2 /* 18/40-18/49 */,
	128 + 3 /* 18/50-18/59 */,
	128 + 3 /* 18/60-18/69 */,
	128 + 3 /* 18/70-18/79 */,
	128 + 4 /* 18/80-18/89 */,
	128 + 5 /* 18/90-18/99 */,
	128 + 6 /* 18/100-18/109 */,
	128 + 7 /* 18/110-18/119 */,
	128 + 8 /* 18/120-18/129 */,
	128 + 9 /* 18/130-18/139 */,
	128 + 9 /* 18/140-18/149 */,
	128 + 10        /* 18/150-18/159 */,
	128 + 11        /* 18/160-18/169 */,
	128 + 12        /* 18/170-18/179 */,
	128 + 13        /* 18/180-18/189 */,
	128 + 14        /* 18/190-18/199 */,
	128 + 15        /* 18/200-18/209 */,
	128 + 15        /* 18/210-18/219 */,
	128 + 16        /* 18/220+ */
};


/*
* Stat Table (STR) -- bonus to dam (plus 128)
*/
byte adj_str_td[] =
{
	128 + -2        /* 3 */,
	128 + -2        /* 4 */,
	128 + -1        /* 5 */,
	128 + -1        /* 6 */,
	128 + 0 /* 7 */,
	128 + 0 /* 8 */,
	128 + 0 /* 9 */,
	128 + 0 /* 10 */,
	128 + 0 /* 11 */,
	128 + 0 /* 12 */,
	128 + 0 /* 13 */,
	128 + 0 /* 14 */,
	128 + 0 /* 15 */,
	128 + 1 /* 16 */,
	128 + 2 /* 17 */,
	128 + 2 /* 18/00-18/09 */,
	128 + 2 /* 18/10-18/19 */,
	128 + 3 /* 18/20-18/29 */,
	128 + 3 /* 18/30-18/39 */,
	128 + 3 /* 18/40-18/49 */,
	128 + 3 /* 18/50-18/59 */,
	128 + 3 /* 18/60-18/69 */,
	128 + 4 /* 18/70-18/79 */,
	128 + 5 /* 18/80-18/89 */,
	128 + 5 /* 18/90-18/99 */,
	128 + 6 /* 18/100-18/109 */,
	128 + 7 /* 18/110-18/119 */,
	128 + 8 /* 18/120-18/129 */,
	128 + 9 /* 18/130-18/139 */,
	128 + 10        /* 18/140-18/149 */,
	128 + 11        /* 18/150-18/159 */,
	128 + 12        /* 18/160-18/169 */,
	128 + 13        /* 18/170-18/179 */,
	128 + 14        /* 18/180-18/189 */,
	128 + 15        /* 18/190-18/199 */,
	128 + 16        /* 18/200-18/209 */,
	128 + 18        /* 18/210-18/219 */,
	128 + 20        /* 18/220+ */
};


/*
* Stat Table (DEX) -- bonus to hit (plus 128)
*/
byte adj_dex_th[] =
{
	128 + -3        /* 3 */,
	128 + -2        /* 4 */,
	128 + -2        /* 5 */,
	128 + -1        /* 6 */,
	128 + -1        /* 7 */,
	128 + 0 /* 8 */,
	128 + 0 /* 9 */,
	128 + 0 /* 10 */,
	128 + 0 /* 11 */,
	128 + 0 /* 12 */,
	128 + 0 /* 13 */,
	128 + 0 /* 14 */,
	128 + 0 /* 15 */,
	128 + 1 /* 16 */,
	128 + 2 /* 17 */,
	128 + 3 /* 18/00-18/09 */,
	128 + 3 /* 18/10-18/19 */,
	128 + 3 /* 18/20-18/29 */,
	128 + 3 /* 18/30-18/39 */,
	128 + 3 /* 18/40-18/49 */,
	128 + 4 /* 18/50-18/59 */,
	128 + 4 /* 18/60-18/69 */,
	128 + 4 /* 18/70-18/79 */,
	128 + 4 /* 18/80-18/89 */,
	128 + 5 /* 18/90-18/99 */,
	128 + 6 /* 18/100-18/109 */,
	128 + 7 /* 18/110-18/119 */,
	128 + 8 /* 18/120-18/129 */,
	128 + 9 /* 18/130-18/139 */,
	128 + 9 /* 18/140-18/149 */,
	128 + 10        /* 18/150-18/159 */,
	128 + 11        /* 18/160-18/169 */,
	128 + 12        /* 18/170-18/179 */,
	128 + 13        /* 18/180-18/189 */,
	128 + 14        /* 18/190-18/199 */,
	128 + 15        /* 18/200-18/209 */,
	128 + 15        /* 18/210-18/219 */,
	128 + 16        /* 18/220+ */
};


/*
* Stat Table (STR) -- bonus to hit (plus 128)
*/
byte adj_str_th[] =
{
	128 + -3        /* 3 */,
	128 + -2        /* 4 */,
	128 + -1        /* 5 */,
	128 + -1        /* 6 */,
	128 + 0 /* 7 */,
	128 + 0 /* 8 */,
	128 + 0 /* 9 */,
	128 + 0 /* 10 */,
	128 + 0 /* 11 */,
	128 + 0 /* 12 */,
	128 + 0 /* 13 */,
	128 + 0 /* 14 */,
	128 + 0 /* 15 */,
	128 + 0 /* 16 */,
	128 + 0 /* 17 */,
	128 + 1 /* 18/00-18/09 */,
	128 + 1 /* 18/10-18/19 */,
	128 + 1 /* 18/20-18/29 */,
	128 + 1 /* 18/30-18/39 */,
	128 + 1 /* 18/40-18/49 */,
	128 + 1 /* 18/50-18/59 */,
	128 + 1 /* 18/60-18/69 */,
	128 + 2 /* 18/70-18/79 */,
	128 + 3 /* 18/80-18/89 */,
	128 + 4 /* 18/90-18/99 */,
	128 + 5 /* 18/100-18/109 */,
	128 + 6 /* 18/110-18/119 */,
	128 + 7 /* 18/120-18/129 */,
	128 + 8 /* 18/130-18/139 */,
	128 + 9 /* 18/140-18/149 */,
	128 + 10        /* 18/150-18/159 */,
	128 + 11        /* 18/160-18/169 */,
	128 + 12        /* 18/170-18/179 */,
	128 + 13        /* 18/180-18/189 */,
	128 + 14        /* 18/190-18/199 */,
	128 + 15        /* 18/200-18/209 */,
	128 + 15        /* 18/210-18/219 */,
	128 + 16        /* 18/220+ */
};


/*
* Stat Table (STR) -- weight limit in deca-pounds
*/
byte adj_str_wgt[] =
{
	5       /* 3 */,
	6       /* 4 */,
	7       /* 5 */,
	8       /* 6 */,
	9       /* 7 */,
	10      /* 8 */,
	11      /* 9 */,
	12      /* 10 */,
	13      /* 11 */,
	14      /* 12 */,
	15      /* 13 */,
	16      /* 14 */,
	17      /* 15 */,
	18      /* 16 */,
	19      /* 17 */,
	20      /* 18/00-18/09 */,
	22      /* 18/10-18/19 */,
	24      /* 18/20-18/29 */,
	26      /* 18/30-18/39 */,
	28      /* 18/40-18/49 */,
	30      /* 18/50-18/59 */,
	31      /* 18/60-18/69 */,
	31      /* 18/70-18/79 */,
	32      /* 18/80-18/89 */,
	32      /* 18/90-18/99 */,
	33      /* 18/100-18/109 */,
	33      /* 18/110-18/119 */,
	34      /* 18/120-18/129 */,
	34      /* 18/130-18/139 */,
	35      /* 18/140-18/149 */,
	35      /* 18/150-18/159 */,
	36      /* 18/160-18/169 */,
	36      /* 18/170-18/179 */,
	37      /* 18/180-18/189 */,
	37      /* 18/190-18/199 */,
	38      /* 18/200-18/209 */,
	38      /* 18/210-18/219 */,
	39      /* 18/220+ */
};


/*
* Stat Table (STR) -- weapon weight limit in pounds
*/
byte adj_str_hold[] =
{
	4       /* 3 */,
	5       /* 4 */,
	6       /* 5 */,
	7       /* 6 */,
	8       /* 7 */,
	10      /* 8 */,
	12      /* 9 */,
	14      /* 10 */,
	16      /* 11 */,
	18      /* 12 */,
	20      /* 13 */,
	22      /* 14 */,
	24      /* 15 */,
	26      /* 16 */,
	28      /* 17 */,
	30      /* 18/00-18/09 */,
	30      /* 18/10-18/19 */,
	35      /* 18/20-18/29 */,
	40      /* 18/30-18/39 */,
	45      /* 18/40-18/49 */,
	50      /* 18/50-18/59 */,
	55      /* 18/60-18/69 */,
	60      /* 18/70-18/79 */,
	65      /* 18/80-18/89 */,
	70      /* 18/90-18/99 */,
	80      /* 18/100-18/109 */,
	80      /* 18/110-18/119 */,
	80      /* 18/120-18/129 */,
	80      /* 18/130-18/139 */,
	80      /* 18/140-18/149 */,
	90      /* 18/150-18/159 */,
	90      /* 18/160-18/169 */,
	90      /* 18/170-18/179 */,
	90      /* 18/180-18/189 */,
	90      /* 18/190-18/199 */,
	100     /* 18/200-18/209 */,
	100     /* 18/210-18/219 */,
	100     /* 18/220+ */
};


/*
* Stat Table (STR) -- digging value
*/
byte adj_str_dig[] =
{
	0       /* 3 */,
	0       /* 4 */,
	1       /* 5 */,
	2       /* 6 */,
	3       /* 7 */,
	4       /* 8 */,
	4       /* 9 */,
	5       /* 10 */,
	5       /* 11 */,
	6       /* 12 */,
	6       /* 13 */,
	7       /* 14 */,
	7       /* 15 */,
	8       /* 16 */,
	8       /* 17 */,
	9       /* 18/00-18/09 */,
	10      /* 18/10-18/19 */,
	12      /* 18/20-18/29 */,
	15      /* 18/30-18/39 */,
	20      /* 18/40-18/49 */,
	25      /* 18/50-18/59 */,
	30      /* 18/60-18/69 */,
	35      /* 18/70-18/79 */,
	40      /* 18/80-18/89 */,
	45      /* 18/90-18/99 */,
	50      /* 18/100-18/109 */,
	55      /* 18/110-18/119 */,
	60      /* 18/120-18/129 */,
	65      /* 18/130-18/139 */,
	70      /* 18/140-18/149 */,
	75      /* 18/150-18/159 */,
	80      /* 18/160-18/169 */,
	85      /* 18/170-18/179 */,
	90      /* 18/180-18/189 */,
	95      /* 18/190-18/199 */,
	100     /* 18/200-18/209 */,
	100     /* 18/210-18/219 */,
	100     /* 18/220+ */
};


/*
* Stat Table (STR) -- help index into the "blow" table
*/
byte adj_str_blow[] =
{
	3       /* 3 */,
	4       /* 4 */,
	5       /* 5 */,
	6       /* 6 */,
	7       /* 7 */,
	8       /* 8 */,
	9       /* 9 */,
	10      /* 10 */,
	11      /* 11 */,
	12      /* 12 */,
	13      /* 13 */,
	14      /* 14 */,
	15      /* 15 */,
	16      /* 16 */,
	17      /* 17 */,
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
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	0       /* 8 */,
	0       /* 9 */,
	1       /* 10 */,
	1       /* 11 */,
	1       /* 12 */,
	1       /* 13 */,
	1       /* 14 */,
	1       /* 15 */,
	1       /* 16 */,
	1       /* 17 */,
	1       /* 18/00-18/09 */,
	2       /* 18/10-18/19 */,
	2       /* 18/20-18/29 */,
	2       /* 18/30-18/39 */,
	2       /* 18/40-18/49 */,
	3       /* 18/50-18/59 */,
	3       /* 18/60-18/69 */,
	4       /* 18/70-18/79 */,
	4       /* 18/80-18/89 */,
	5       /* 18/90-18/99 */,
	6       /* 18/100-18/109 */,
	7       /* 18/110-18/119 */,
	8       /* 18/120-18/129 */,
	9       /* 18/130-18/139 */,
	10      /* 18/140-18/149 */,
	11      /* 18/150-18/159 */,
	12      /* 18/160-18/169 */,
	14      /* 18/170-18/179 */,
	16      /* 18/180-18/189 */,
	18      /* 18/190-18/199 */,
	20      /* 18/200-18/209 */,
	20      /* 18/210-18/219 */,
	20      /* 18/220+ */
};


/*
* Stat Table (DEX) -- chance of avoiding "theft" and "falling"
*/
byte adj_dex_safe[] =
{
	0       /* 3 */,
	1       /* 4 */,
	2       /* 5 */,
	3       /* 6 */,
	4       /* 7 */,
	5       /* 8 */,
	5       /* 9 */,
	6       /* 10 */,
	6       /* 11 */,
	7       /* 12 */,
	7       /* 13 */,
	8       /* 14 */,
	8       /* 15 */,
	9       /* 16 */,
	9       /* 17 */,
	10      /* 18/00-18/09 */,
	10      /* 18/10-18/19 */,
	15      /* 18/20-18/29 */,
	15      /* 18/30-18/39 */,
	20      /* 18/40-18/49 */,
	25      /* 18/50-18/59 */,
	30      /* 18/60-18/69 */,
	35      /* 18/70-18/79 */,
	40      /* 18/80-18/89 */,
	45      /* 18/90-18/99 */,
	50      /* 18/100-18/109 */,
	60      /* 18/110-18/119 */,
	70      /* 18/120-18/129 */,
	80      /* 18/130-18/139 */,
	90      /* 18/140-18/149 */,
	100     /* 18/150-18/159 */,
	100     /* 18/160-18/169 */,
	100     /* 18/170-18/179 */,
	100     /* 18/180-18/189 */,
	100     /* 18/190-18/199 */,
	100     /* 18/200-18/209 */,
	100     /* 18/210-18/219 */,
	100     /* 18/220+ */
};


/*
* Stat Table (CON) -- base regeneration rate
*/
byte adj_con_fix[] =
{
	0       /* 3 */,
	0       /* 4 */,
	0       /* 5 */,
	0       /* 6 */,
	0       /* 7 */,
	0       /* 8 */,
	0       /* 9 */,
	0       /* 10 */,
	0       /* 11 */,
	0       /* 12 */,
	0       /* 13 */,
	1       /* 14 */,
	1       /* 15 */,
	1       /* 16 */,
	1       /* 17 */,
	2       /* 18/00-18/09 */,
	2       /* 18/10-18/19 */,
	2       /* 18/20-18/29 */,
	2       /* 18/30-18/39 */,
	2       /* 18/40-18/49 */,
	3       /* 18/50-18/59 */,
	3       /* 18/60-18/69 */,
	3       /* 18/70-18/79 */,
	3       /* 18/80-18/89 */,
	3       /* 18/90-18/99 */,
	4       /* 18/100-18/109 */,
	4       /* 18/110-18/119 */,
	5       /* 18/120-18/129 */,
	6       /* 18/130-18/139 */,
	6       /* 18/140-18/149 */,
	7       /* 18/150-18/159 */,
	7       /* 18/160-18/169 */,
	8       /* 18/170-18/179 */,
	8       /* 18/180-18/189 */,
	8       /* 18/190-18/199 */,
	9       /* 18/200-18/209 */,
	9       /* 18/210-18/219 */,
	9       /* 18/220+ */
};


/*
* Stat Table (CON) -- extra half-hitpoints per level (plus 128)
*/
byte adj_con_mhp[] =
{
	128 + -5        /* 3 */,
	128 + -3        /* 4 */,
	128 + -2        /* 5 */,
	128 + -1        /* 6 */,
	128 + 0 /* 7 */,
	128 + 0 /* 8 */,
	128 + 0 /* 9 */,
	128 + 0 /* 10 */,
	128 + 0 /* 11 */,
	128 + 0 /* 12 */,
	128 + 0 /* 13 */,
	128 + 0 /* 14 */,
	128 + 1 /* 15 */,
	128 + 1 /* 16 */,
	128 + 2 /* 17 */,
	128 + 3 /* 18/00-18/09 */,
	128 + 4 /* 18/10-18/19 */,
	128 + 4 /* 18/20-18/29 */,
	128 + 4 /* 18/30-18/39 */,
	128 + 4 /* 18/40-18/49 */,
	128 + 5 /* 18/50-18/59 */,
	128 + 6 /* 18/60-18/69 */,
	128 + 7 /* 18/70-18/79 */,
	128 + 8 /* 18/80-18/89 */,
	128 + 9 /* 18/90-18/99 */,
	128 + 10        /* 18/100-18/109 */,
	128 + 11        /* 18/110-18/119 */,
	128 + 12        /* 18/120-18/129 */,
	128 + 13        /* 18/130-18/139 */,
	128 + 14        /* 18/140-18/149 */,
	128 + 15        /* 18/150-18/159 */,
	128 + 16        /* 18/160-18/169 */,
	128 + 18        /* 18/170-18/179 */,
	128 + 20        /* 18/180-18/189 */,
	128 + 22        /* 18/190-18/199 */,
	128 + 25        /* 18/200-18/209 */,
	128 + 26        /* 18/210-18/219 */,
	128 + 27        /* 18/220+ */
};


/*
* This table is used to help calculate the number of blows the player can
* make in a single round of attacks (one player turn) with a normal weapon.
*
* This number ranges from a single blow/round for weak players to up to six
* blows/round for powerful warriors.
*
* Note that certain artefacts and ego-items give "bonus" blows/round.
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



/*
* Store owners (exactly four "possible" owners per store, chosen randomly)
* { name, purse, max greed, min greed, haggle_per, tolerance, race }
*/
owner_type owners[MAX_STORES][MAX_OWNERS] =
{
	{
		/* Store 0 (General Store)*/
		{ "Wirt the Weasel",         250,    170, 108,  5, 15, HUMAN}, /* Diablo II + Diablo I */
		{ "Atma the Tragic",       500,    175, 108,  4, 12, HUMAN}, /* Diablo II ( should be an Inn Owner, but I will not put in a hack on a hack for her */
		{ "Elzix the Scoundrel",          750,    170, 107,  5, 15, HUMAN}, /* Diablo II  */
		{ "Ogden the Innkeeper",        1000,    165, 107,  6, 18, HUMAN},  /* Diablo I  */
		{ "Wirt the Weasel",         250,    170, 108,  5, 15, HUMAN}, /* Diablo II + Diablo I */
		{ "Atma the Tragic",       500,    175, 108,  4, 12, HUMAN}, /* Diablo II ( should be an Inn Owner, but I will not put in a hack on a hack for her */
		{ "Elzix the Scoundrel",          750,    170, 107,  5, 15, HUMAN}, /* Diablo II  */
		{ "Ogden the Innkeeper",        1000,    165, 107,  6, 18, HUMAN},  /* Diablo I  */
		{ "Wirt the Weasel",         250,    170, 108,  5, 15, HUMAN}, /* Diablo II + Diablo I */
		{ "Atma the Tragic",       500,    175, 108,  4, 12, HUMAN}, /* Diablo II ( should be an Inn Owner, but I will not put in a hack on a hack for her */
		{ "Elzix the Scoundrel",          750,    170, 107,  5, 15, HUMAN}, /* Diablo II  */
		{ "Ogden the Innkeeper",        1000,    165, 107,  6, 18, HUMAN},  /* Diablo I  */
		{ "Wirt the Weasel",         250,    170, 108,  5, 15, HUMAN}, /* Diablo II + Diablo I */
		{ "Atma the Tragic",       500,    175, 108,  4, 12, HUMAN}, /* Diablo II ( should be an Inn Owner, but I will not put in a hack on a hack for her */
		{ "Elzix the Scoundrel",          750,    170, 107,  5, 15, HUMAN}, /* Diablo II  */
		{ "Ogden the Innkeeper",        1000,    165, 107,  6, 18, HUMAN},  /* Diablo I  */
	},
	{
		/* Store 1 (Armoury) */
		{ "Flavie, Rogue Guardian",           10000,   210, 115,  5,  7, HUMAN},  /* Diablo II */
		{ "Flava, Holy Knight",          15000,  190, 111,  4,  9, HUMAN},  /* Diablo II, act 2 */
		{ "Hratli, Sorcer",            25000,  200, 112,  4, 10, HUMAN}, /* Diablo II, act 3 */
		{ "Feradach Dathi",          30000,  200, 112,  4,  5, HUMAN},  /* Irish King, enchanted his armor with a speed bonus ;) What a guy */
		{ "Flavie, Rogue Guardian",           10000,   210, 115,  5,  7, HUMAN},  /* Diablo II */
		{ "Flava, Holy Knight",          15000,  190, 111,  4,  9, HUMAN},  /* Diablo II, act 2 */
		{ "Hratli, Sorcer",            25000,  200, 112,  4, 10, HUMAN}, /* Diablo II, act 3 */
		{ "Feradach Dathi",          30000,  200, 112,  4,  5, HUMAN},  /* Irish King, enchanted his armor with a speed bonus ;) What a guy */
		{ "Flavie, Rogue Guardian",           10000,   210, 115,  5,  7, HUMAN},  /* Diablo II */
		{ "Flava, Holy Knight",          15000,  190, 111,  4,  9, HUMAN},  /* Diablo II, act 2 */
		{ "Hratli, Sorcer",            25000,  200, 112,  4, 10, HUMAN}, /* Diablo II, act 3 */
		{ "Feradach Dathi",          30000,  200, 112,  4,  5, HUMAN},  /* Irish King, enchanted his armor with a speed bonus ;) What a guy */
		{ "Flavie, Rogue Guardian",           10000,   210, 115,  5,  7, HUMAN},  /* Diablo II */
		{ "Flava, Holy Knight",          15000,  190, 111,  4,  9, HUMAN},  /* Diablo II, act 2 */
		{ "Hratli, Sorcer",            25000,  200, 112,  4, 10, HUMAN}, /* Diablo II, act 3 */
		{ "Feradach Dathi",          30000,  200, 112,  4,  5, HUMAN},  /* Irish King, enchanted his armor with a speed bonus ;) What a guy */
	},
	{
		/* Store 2 (Weaponsmith)*/
		{ "Charsi, Rogue Blacksmith",      10000,   210, 115,  6,  6, HUMAN}, /* Diablo II,  */
		{ "Kashya, Rogue Captain",        15000,  185, 110,  5,  9, HUMAN},  /* Diablo II */
		{ "Geglash the Barbarian",         25000,  190, 115,  5,  7, NORDIC}, /* Diablo II, act 2*/
		{ "Lady of the Lake",       30000,  195, 112,  4,  8, FAE}, /*  She was in the business of selling *magical* blades */
		{ "Natalya, Hunter of Evil",      10000,   210, 115,  6,  6, HUMAN},  /* Diablo II, act 3*/
		{ "Larzuk the Barbarian",        15000,  185, 110,  5,  9, NORDIC}, /* Diablo II, act 5*/
		{ "Qual-Kehk the Troubled",         25000,  190, 115,  5,  7, HUMAN}, /* Diablo II, act 5*/
		{ "Griswold, Blacksmith",       30000,  195, 112,  4,  8, DWARF}, /* Diablo I */
		{ "Charsi, Rogue Blacksmith",      10000,   210, 115,  6,  6, HUMAN}, /* Diablo II,  */
		{ "Kashya, Rogue Captain",        15000,  185, 110,  5,  9, HUMAN},  /* Diablo II */
		{ "Geglash the Barbarian",         25000,  190, 115,  5,  7, NORDIC}, /* Diablo II, act 2*/
		{ "Lady of the Lake",       30000,  195, 112,  4,  8, FAE}, /*  She was in the business of selling *magical* blades */
		{ "Natalya, Hunter of Evil",      10000,   210, 115,  6,  6, HUMAN},  /* Diablo II, act 3*/
		{ "Larzuk the Barbarian",        15000,  185, 110,  5,  9, NORDIC}, /* Diablo II, act 5*/
		{ "Qual-Kehk the Troubled",         25000,  190, 115,  5,  7, HUMAN}, /* Diablo II, act 5*/
		{ "Griswold, Blacksmith",       30000,  195, 112,  4,  8, DWARF}, /* Diablo I */
	},
		{
		/* Store 3 (Temple)*/
		{ "Akara, Rogue Priestess",         10000,   175, 109,  6, 15, HUMAN}, /*Diablo II, The spiritual leader of the Sisters of the Sightless Eye*/
		{ "Lord Jerhyn",         15000,  185, 110,  5, 23, HUMAN}, /* Diablo II, act 2 */
		{ "Ormus, Healer",           25000,  180, 107,  6, 20, HUMAN},  /* Diablo II, act 3 */
		{ "Asenath the Holy",          30000,  185, 109,  5, 15, HUMAN}, /* Diablo II, act 3 */
		{ "Pepin the Healer",         10000,   175, 109,  6, 15, HUMAN}, /* Diablo I  */
		{ "Julian the Theurgist",         15000,  185, 110,  5, 23, HUMAN},  /* Theurgists are cool ? */
		{ "Roger Bacon",           25000,  180, 107,  6, 20, HUMAN},  /* Yah, those Franciscans eh ! */
		{ "Abraham ben Samuel Abulafia",          30000,  185, 109,  5, 15, HUMAN},  /* Go kabbalah's */
		{ "Akara, Rogue Priestess",         10000,   175, 109,  6, 15, HUMAN}, /*Diablo II, The spiritual leader of the Sisters of the Sightless Eye*/
		{ "Lord Jerhyn",         15000,  185, 110,  5, 23, HUMAN}, /* Diablo II, act 2 */
		{ "Ormus, Healer",           25000,  180, 107,  6, 20, HUMAN},  /* Diablo II, act 3 */
		{ "Asenath the Holy",          30000,  185, 109,  5, 15, HUMAN}, /* Diablo II, act 3 */
		{ "Pepin the Healer",         10000,   175, 109,  6, 15, HUMAN}, /* Diablo I  */
		{ "Julian the Theurgist",         15000,  185, 110,  5, 23, HUMAN},  /* Theurgists are cool ? */
		{ "Roger Bacon",           25000,  180, 107,  6, 20, HUMAN},  /* Yah, those Franciscans eh ! */
		{ "Abraham ben Samuel Abulafia",          30000,  185, 109,  5, 15, HUMAN},  /* Go kabbalah's */
	},
	{
		/* Store 4 (Alchemist)*/
		{ "Lysander",				10000,  190, 111,  5,  8, HUMAN },  /* Diablo II, act 2 */
		{ "Alkor",         10000,  190, 110,  6,  8, HUMAN},  /* Diablo II, act 3 */
		{ "Jamella, Gatekeeper",           15000,  200, 116,  6,  9, HUMAN}, /* Diablo II, act 4 */
		{ "Anya, the Fated",       15000,  220, 111,  4,  9, HUMAN},  /* Diablo II, act 5 */
		{ "Malah, town Nurse",         10000,  190, 111,  5,  8, DWARF}, /* Diablo II, act 5 */
		{ "Twardowski, Physician",         10000,  190, 110,  6,  8, HUMAN},  /*  Famous Alchemist  */
		{ "Parmigianino",           15000,  200, 116,  6,  9, HUMAN},  /*  Girolamo Francesco Maria Mazzola (11 January 1503- 24 August 1540) */
		{ "Nicholas Flamel",       15000,  220, 111,  4,  9, FAE},  /* Okay, this is _not_ the NF of Harry Potter ;) */
		{ "Lysander",				10000,  190, 111,  5,  8, HUMAN},  /* Diablo II, act 2 */
		{ "Alkor",         10000,  190, 110,  6,  8, HUMAN},  /* Diablo II, act 3 */
		{ "Jamella, Gatekeeper",           15000,  200, 116,  6,  9, HUMAN}, /* Diablo II, act 4 */
		{ "Anya, the Fated",       15000,  220, 111,  4,  9, HUMAN},  /* Diablo II, act 5 */
		{ "Malah, town Nurse",         10000,  190, 111,  5,  8, DWARF}, /* Diablo II, act 5 */
		{ "Twardowski, Physician",         10000,  190, 110,  6,  8, HUMAN},  /*  Famous Alchemist  */
		{ "Parmigianino",           15000,  200, 116,  6,  9, HUMAN},  /*  Girolamo Francesco Maria Mazzola (11 January 1503- 24 August 1540) */
		{ "Nicholas Flamel",       15000,  220, 111,  4,  9, FAE},  /* Okay, this is _not_ the NF of Harry Potter ;) */
	},
	{
		/* Store 5 (Magic)*/
		{ "Merlin the Sage",       30000,  175, 110,  5, 11,  DEVILSPAWN},		/*Merlin was the son of an incubus*/
    	{ "Lady of the Lake",        15000,  200, 110,  7,  8, FAE},	     /*  She was in the business of selling *magical* blades */
 		{ "Drognan",        15000,  200, 110,  7,  8, HUMAN},  /* Diablo II */
		{ "Adria the Witch",         20000,  215, 113,  6, 10, HUMAN},  /* Diablo I */
		{ "Witch of Endor",       30000,  200, 110,  7, 10, HUMAN}, /* Hebrew Bible */
		{ "Gyges of Lydia",       30000,  175, 110,  5, 11, HUMAN}, /* king said to possess magical artifacts */
		{ "Iannes and Mambres",         20000,  215, 113,  6, 10, DEVILSPAWN}, /* magicians at Pharaoh's court mentioned in the New Testament */
		{ "Simon the Sorcerer",       30000,  200, 110,  7, 10, DEVILSPAWN}, /* Simon Magus, not the computer game */
		{ "Merlin the Sage",       30000,  175, 110,  5, 11, DEVILSPAWN},		/*Merlin was the son of an incubus*/
    	{ "Lady of the Lake",        15000,  200, 110,  7,  8, FAE},	     /*  She was in the business of selling *magical* blades */
 		{ "Drognan",        15000,  200, 110,  7,  8, HUMAN},  /* Diablo II */
		{ "Adria the Witch",         20000,  215, 113,  6, 10, HUMAN},  /* Diablo I */
		{ "Witch of Endor",       30000,  200, 110,  7, 10, HUMAN}, /* Hebrew Bible */
		{ "Gyges of Lydia",       30000,  175, 110,  5, 11, HUMAN}, /* king said to possess magical artifacts */
		{ "Iannes and Mambres",         20000,  215, 113,  6, 10, ELDER}, /* magicians at Pharaoh's court mentioned in the New Testament */
		{ "Simon the Sorcerer",       30000,  200, 110,  7, 10, DEVILSPAWN}, /* Simon Magus, not the computer game */
	},
	{
		/* Store 6 (Black Market)*/
		{ "Apollonius of Tyana", 20000,  250, 150, 10,  5, HUMAN},  /* Year 2 */
		{ "Lady of the Lake",          20000,  250, 150, 10,  5, FAE},  /*  She was pretty shady, in the business of selling *magical* *blades* */
		{ "Gheed - Caravan Trader",          30000,  250, 150, 10,  5, HUMAN}, /* Diablo II */
		{ "Warriv - Caravan Leader",           30000,  250, 150, 10,  5, HUMAN}, /* Diablo II */
		{ "Greiz - Mercenary Captain",             20000,  250, 150, 10,  5, HUMAN}, /* Diablo II, act 2 */
		{ "Asheara, Iron Wolf",          20000,  250, 150, 10,  5, HUMAN}, /* Diablo II, act 3 */
		{ "Lucius Apuleius",          30000,  250, 150, 10,  5, HUMAN}, /* A Discourse on Magic ;) */
		{ "Elymas",           30000,  250, 150, 10,  5, HUMAN}, /* Bibilic Jewish Magician */
		{ "Apollonius of Tyana", 20000,  250, 150, 10,  5, HUMAN},  /* Year 2 */
		{ "Lady of the Lake",          20000,  250, 150, 10,  5, FAE},  /*  She was pretty shady, in the business of selling *magical* *blades* */
		{ "Gheed - Caravan Trader",          30000,  250, 150, 10,  5, HUMAN}, /* Diablo II */
		{ "Warriv - Caravan Leader",           30000,  250, 150, 10,  5, HUMAN}, /* Diablo II */
		{ "Greiz - Mercenary Captain",             20000,  250, 150, 10,  5, HUMAN}, /* Diablo II, act 2 */
		{ "Asheara, Iron Wolf",          20000,  250, 150, 10,  5, HUMAN}, /* Diablo II, act 3 */
		{ "Lucius Apuleius",          30000,  250, 150, 10,  5, HUMAN}, /* A Discourse on Magic ;) */
		{ "Elymas",           30000,  250, 150, 10,  5, HUMAN}, /* Bibilic Jewish Magician */	
    },
	{
		/* Store 7 (Home)*/
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
		{ "Your home",                          0,      100, 100,  0, 99, 99},
	},
	{
		/*Eliezer of Worms*/
		/* Store 8 (Bookstore)*/
		{ "Johann Weyer", 15000, 175, 108, 4, 12, HUMAN}, /* Pseudomonarchia Daemonum (between February 24, 1515 and February 24, 1516 Ð February 24, 1588)*/
		{ "Ludovico Antonio Muratori", 20000, 120, 105, 6, 16, HUMAN},  /* Ludovico Antonio Muratori (1672 - 1750) was an Italian historian */
		{ "Johannes Trithemius", 25000, 120, 110, 7, 19, HUMAN}, /* Steganographia  (1 February 1462 - 13 December 1516) */
		{ "Abra-Melin", 30000, 140, 105, 6, 12, HUMAN}, /* The Book of the Sacred Magic of Abramelin the Mage  - 1458 */
		{ "Alibek the Egyptian", 15000, 175, 108, 4, 12, HUMAN}, /* The Grand Grimoir - 13th century */
		{ "John Dee", 20000, 120, 105, 6, 16, HUMAN}, /* Wrote books in the enochian language - (July 13, 1527 Ð 1608 or 1609)*/
		{ "Heinrich Cornelius Agrippa", 25000, 120, 110, 7, 19, HUMAN}, /* Libri tres de occulta philosophia - born of noble birth in Cologne September 14, 1486, died in Grenoble February 18, 1535) */
		{ "Peter de Abano", 30000, 140, 105, 6, 12, HUMAN}, /* Heptameron - (c. 1250-1316)  */
		{ "Hypatia of Alexandria", 15000, 175, 108, 4, 12, HUMAN}, /* philosopher, mathematician, and teacher,  (c. 370 - 415) */
		{ "Eratosthenes", 20000, 120, 105, 6, 16, HUMAN}, /* Famous Librarian - (276 BC - 194 BC)*/
		{ "Gaius Suetonius Tranquillus", 25000, 120, 110, 7, 19, HUMAN}, /* Roman Librarian -  (69 or 70 AD - after 130 AD  */
		{ "Zenodotus", 30000, 140, 105, 6, 12, HUMAN},  /* He was the first superintendent of the library at Alexandria - 280 BC. */
		{ "Flavius Magnus Aurelius Cassiodorus Senator", 15000, 175, 108, 4, 12, HUMAN}, /* Owner of east,west,greek,roman, goth works */
		{ "Dionysius Exiguus", 20000, 120, 105, 6, 16, HUMAN}, /* Inventor of Anno Domini */
		{ "Callimachus", 25000, 120, 110, 7, 19, HUMAN}, /*  His Pinakes (tablets), in 120 books, a critical and chronologically arranged catalogue of the library, laid the foundation of a history of Greek literature. (ca. 305 BC- ca. 240 BC)  */
		{ "Demetrius Phalereus", 30000, 140, 105, 6, 12, HUMAN}, /* 40,000 to 700,000 books and was initially organized by Demetrius Phalereus. - 3rd century BC, */
	},
	{
		/* Store 9 (Inn), This shop has a hack that prevents (Human to show), shop is identified by nymber*/
		{ "The George and Dragon", 15000, 175, 108, 4, 12, HUMAN}, /*London*/
		{ "The Tabard Inn", 20000, 120, 105, 6, 16, HUMAN},  /* London */
 		{ "The Cat and The Fiddle", 25000, 120, 110, 7, 19, HUMAN},  /* Hampshire */
 		{ "The Old Bell", 30000, 140, 105, 6, 12, HUMAN},  /* Malmesbury */
		{ "The Weary Friar", 15000, 175, 108, 4, 12, HUMAN}, /* Cornwall */
		{ "The Fighting Cocks", 20000, 120, 105, 6, 16, HUMAN}, /* St Albans */
		{ "The Trip to Jerusalem", 25000, 120, 110, 7, 19, HUMAN}, /* Nottingham */
		{ "The Bull & Bladder", 30000, 140, 105, 6, 12, HUMAN}, /* Birmingham */
		{ "The George and Dragon", 15000, 175, 108, 4, 12, HUMAN}, /*London*/
		{ "The Tabard Inn", 20000, 120, 105, 6, 16, HUMAN},  /* London */
 		{ "The Cat and The Fiddle", 25000, 120, 110, 7, 19, HUMAN},  /* Hampshire */
 		{ "The Old Bell", 30000, 140, 105, 6, 12, HUMAN},  /* Malmesbury */
		{ "The Weary Friar", 15000, 175, 108, 4, 12, HUMAN}, /* Cornwall */
		{ "The Fighting Cocks", 20000, 120, 105, 6, 16, HUMAN}, /* St Albans */
		{ "The Trip to Jerusalem", 25000, 120, 110, 7, 19, HUMAN}, /* Nottingham */
		{ "The Bull & Bladder", 30000, 140, 105, 6, 12, HUMAN}, /* Birmingham */
	},
	{
		/* Store 10 (Hall)*/
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
		{ "Hall of Records",                          0,      100, 100,  0, 99, 99},
	},
	{
		/* Store 11 (Pawnbroker)* Yah, I just doubled the entries, enterprising souls can find some more known trader states */
		{ "Joris the Fleming", 2000, 100, 100, 4, 12, HUMAN},  /*Famous traders ;) */
		{ "Giovanni the Venetian", 4000, 100, 100, 6, 16, HUMAN}, /* Mercantile Italian city-state */
		{ "Hiram of Tyre", 5000, 100, 100, 7, 19, HUMAN},  /*Phoenicia was an ancient civilization in the north of ancient Canaan, trading nation,, furnished architects, workmen and cedar timbers for the temple of his ally Solomon at Jerusalem.*/
		{ "Yosef the Radhanite", 10000, 100, 100, 6, 12, HUMAN}, /* Jewish Trading clan */
		{ "Nicolasthe Genoan", 2000, 100, 100, 4, 12, HUMAN},  /*mercantile Italian city-stat*/
		{ "Alfonso the Pisan", 4000, 100, 100, 6, 16, HUMAN}, /*mercantile Italian city-state*/
		{ "Eduardo the Amalfian", 5000, 100, 100, 7, 19, HUMAN},  /*mercantile Italian city-state*/
		{ "Jan the Dutch", 10000, 100, 100, 6, 12, HUMAN}, /* Vikings also traded when they couldnt steal */
		{ "Joris the Fleming", 2000, 100, 100, 4, 12, HUMAN},  /*Famous traders ;) */
		{ "Giovanni the Venetian", 4000, 100, 100, 6, 16, HUMAN}, /* Mercantile Italian city-state */
		{ "Hiram of Tyre", 5000, 100, 100, 7, 19, HUMAN},  /*Phoenicia was an ancient civilization in the north of ancient Canaan, trading nation,, furnished architects, workmen and cedar timbers for the temple of his ally Solomon at Jerusalem.*/
		{ "Yosef the Radhanite", 10000, 100, 100, 6, 12, HUMAN}, /* Jewish Trading clan */
		{ "Nicolasthe Genoan", 2000, 100, 100, 4, 12, HUMAN},  /*mercantile Italian city-stat*/
		{ "Alfonso the Pisan", 4000, 100, 100, 6, 16, HUMAN}, /*mercantile Italian city-state*/
		{ "Eduardo the Amalfian", 5000, 100, 100, 7, 19, HUMAN},  /*mercantile Italian city-state*/
		{ "Cain the Elder", 10000, 100, 100, 6, 12, HUMAN}, /* Diablo II */
	},
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

/* This table has been inverted for Hellband. The new values
* are 1000/x where x is the old value. This gives the same spread
* but subtracting extract_energy[n] each move and always adding
* 10 per turn, rather than adding extract_energy[n] each turn and
* always subtracting 100.
*
* This has been done to allow the seperating out of movement speed
* and attack speed.
*/
u16b extract_energy[200] =
{
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* S-50 */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* S-40 */     500,  500,  500,  500,  500,  500,  500,  500,  500,  500,
/* S-30 */     500,  500,  500,  500,  500,  500,  500,  333,  333,  333,
/* S-20 */     333,  333,  333,  333,  333,  250,  250,  250,  250,  250,
/* S-10 */     200,  200,  200,  200,  167,  167,  143,  143,  125,  111,
/* Norm */    100, 91, 83, 77, 71, 67, 63, 59, 56, 53,
/* F+10 */    50, 48, 45, 43, 42, 40, 38, 37, 36, 34,
/* F+20 */    33, 32, 31, 30, 29, 29, 28, 28, 27, 27,
/* F+30 */    26, 26, 26, 26, 25, 25, 25, 24, 24, 24,
/* F+40 */    24, 24, 24, 23, 23, 23, 23, 23, 23, 23,
/* F+50 */    22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
/* F+60 */    21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
/* F+70 */    20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
/* Fast */    20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
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
*      Title,
*      Winner
*/
player_sex sex_info[MAX_SEXES] =
{
	{ "Lady",      "Lady", "Dame" },
	{ "Gentleman", "Sir",  "Lord" }
};


/*
* Player Races
*
*      Title,
*      {STR,INT,WIS,DEX,CON,CHA},
*      r_dis(arm), r_dev(ices), r_sav(ingthrow), r_stl(stealth), r_srh(search), r_fos(frequency of search), r_thn, r_thb,
*      hitdie, exp base,
*      Age (Base, Mod),			
*      Male (Hgt, Wgt),
*      Female (Hgt, Wgt)
*      infra,
*      class-choices
*/
player_race race_info[COUNT_SUBRACES] = { 	
/*     Name					 STRINTWISDEXCONCHA    dis  dev sav stl sh  shf th thshoot  hd  exp  age1   2   hgt1  2  whgt1 2  hght1 2 whgt1   2  infra  choices	rations undead  f3rles  !light*/	
   {  "Florentian",			{ 0, 0, 0, 0, 0, 0 }, 	 0,  0,  0,  0,  0, 10,  0,  0, 	10, 100,  14,   6,   72,  6, 180, 25,  66,  4, 150,  20,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All */
   {  "Gypsy",				{ 0, 0, 0, 1, 0, 1 }, 	 5,  5,  3,  3,  0, 10,  0,  0, 	10, 120,  14,   6,   72,  6, 180, 25,  66,  4, 150,  20,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All */	
   {  "Nordic",				{ 0,+1,+1, 0, 0, 0 },	 2,-10,  2,  1,  1,  7, 12, 10,		11, 120,  14,   8,   82,  5, 200, 20,  78,  6, 190,  15,  0,  0x1FFF, 	TRUE,	FALSE,	TRUE,	FALSE,}, /* All */
   {  "Atlantian",			{-1, 3, 2, 2,-2, 1 }, 	 5, 15, 20,  3,  8, 12, -5, 10, 	 9, 150,  75,  75,   60,  4, 100,  6,  54,  4,  80,   6,  5,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All */
   {  "Dwarf descendant",	{ 1, 1,-1,-1, 1,-1 }, 	 2,  9, 10, -1,  7, 10, 15,  0, 	11, 125,  35,  15,   48,  3, 150, 10,  46,  3, 120,  10,  5,  0x1FFF,   TRUE,	FALSE,	FALSE,	FALSE,}, /* Warrior, Priest and Druid */
   {  "Elf descendant",		{-1, 1, 1, 1,-1, 1 }, 	 5,  6,  6,  2,  8, 12, -5, 15, 	 8, 120,  75,  75,   60,  4, 100,  6,  54,  4,  80,   6,  3,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Ogre descendant",	{ 3,-1,-1,-1, 3,-1 }, 	-3, -5, -5, -2, -1,  5, 20,  0, 	12, 130,  40,  10,   92, 10, 255, 60,  80,  8, 235,  60,  3,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Troll descendant",	{ 4,-3,-2,-3, 3,-2 }, 	-5, -8, -8, -2, -1,  5, 20,-10, 	12, 137,  20,  10,   96, 10, 250, 50,  84,  8, 225,  40,  3,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Giant descendant",	{ 4,-2,-2,-2, 3,-3 }, 	-6, -8, -6, -2, -1,  5, 25,  5, 	13, 150,  40,  10,  100, 10, 255, 65,  80, 10, 240,  64,  3,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Titan descendant",	{ 5, 1, 1,-2, 3, 1 }, 	-5,  5,  2, -2,  1,  8, 25,  0, 	14, 255, 100,  30,  111, 11, 255, 86,  99, 11, 250,  86,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Nephilim",			{ 1, 2, 2, 2, 3, 2 }, 	 4,  5,  5,  2,  3, 13, 15, 10, 	10, 225,  50,  50,   82,  5, 190, 20,  78,  6, 180,  15,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /*All*/   
   {  "Human",				{ 1, 2, 2, 2, 3, 2 }, 	 4,  5,  5,  2,  3, 13, 15, 10, 	10, 225,  50,  50,   82,  5, 190, 20,  78,  6, 180,  15,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /*This should never happen*/
   {  "Seelie Fae",			{-4, 3, 3, 3,-2, 2 }, 	10, 10, 10,  4, 10, 10,-12,  0, 	 7, 175,  50,  25,   32,  2,  75,  2,  29,  2,  65,   2,  4,  0x1E5E, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Gnome",				{-1, 2, 0, 2, 1,-2 }, 	10, 12, 12,  3,  6, 13, -8, 12, 	 8, 135,  50,  40,   42,  3,  90,  6,  39,  3,  75,   3,  4,  0x1E5E, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Leprechaun",			{-4, 3, 3, 4,-4, 7 }, 	 2,  4, 10, 15,  5, 15, -5, -5, 	 7, 100,  14,   3,   50,  3,  90,  6,  50,  3,  75,   3,  2,  0x1E5E, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Kobold",				{ 1,-1, 0, 1, 0,-4 }, 	-2, -3, -2, -1,  1,  8, 10, -8, 	 9, 125,  11,   3,   60,  1, 130,  5,  55,  1, 100,   5,  3,  0x0009, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Devilspawn",			{ 2,-2,-1,-1, 2,-4 }, 	-5, -2, -1, -1, -1,  5, 12,  5, 	11, 140,  14,   6,   65,  6, 150, 20,  61,  6, 120,  15,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All but Ranger and Paladin */
   {  "Imp",				{-1,-1,-1, 1, 2,-3 }, 	-3,  2, -1,  1, -1, 10,  5, -5, 	10, 110,  13,   4,   68,  1, 150,  5,  64,  1, 120,   5,  3,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Succubus",			{ 0, 2, 0, 2, 0, 4 }, 	-5, -2, -1, -1, -1,  5, 12,  5, 	11, 160,  14,   6,   65,  6, 150, 20,  61,  6, 120,  15,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All but Ranger and Paladin */
   {  "Lili",				{ 0, 0, 0, 0, 4, 4 }, 	-5, -2, -1, -1, -1,  5, 12,  5, 	11, 160,  14,   6,   65,  6, 150, 20,  61,  6, 120,  15,  0,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All but Ranger and Paladin */
   {  "Elder",				{ 1, 3, 2, 3, 1, 5 }, 	 4, 20, 20,  4,  3, 14, 10, 25, 	10, 200, 100,  30,   90, 10, 190, 20,  82, 10, 180,  15,  4,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Elder Guardian",     { 4,-5,-5, 0, 4,-4 }, 	-5, -5, 10, -1, -1,  8, 20,  0, 	12, 200,   1, 100,   66,  1, 200,  6,  62,  1, 180,   6,  4,  0x0001, 	FALSE,	FALSE,	TRUE,	FALSE,}, 
   {  "Elder Horror",		{-3, 4, 4, 0,-2,-5 }, 	10, 25, 15,  2,  5, 12,-10, -5, 	 9, 140, 100,  25,   68,  6, 142, 15,  63,  6, 112,  10,  4,  0x1746, 	TRUE,	FALSE,	FALSE,	FALSE,},
   {  "Vampire",			{ 3, 3,-1,-1, 1, 2 }, 	 4, 10, 10,  4,  1,  8,  5,  0, 	11, 200, 100,  30,   72,  6, 180, 25,  66,  4, 150,  20,  5,  0x1FFF, 	FALSE,	TRUE,	FALSE,	TRUE,}, /* All */
   {  "Werewolf",			{ 3, 3,-1,-1, 1, 2 }, 	 4, 10, 10,  4,  1,  8,  5,  0, 	11, 200, 100,  30,   72,  6, 180, 25,  66,  4, 150,  20,  5,  0x1FFF, 	TRUE,	FALSE,	FALSE,	FALSE,}, /* All */
   {  "Skeleton",			{ 0,-2,-2, 0, 1,-4 }, 	-5, -5,  5, -1, -1,  8, 10,  0, 	10, 145, 100,  35,   72,  6,  50,  5,  66,  4,  50,   5,  2,  0x1F0F, 	FALSE,	FALSE,	FALSE,	FALSE,},
   {  "Mummy",				{ 2,-6,-6, 1, 4,-5 }, 	-5, -5,  8, -1, -1,  5, 15,  0, 	13, 135, 100,  30,   72,  6, 100, 25,  66,  4, 100,  20,  2,  0x0001, 	FALSE,	TRUE,	FALSE,	FALSE,}, /*Only warrior*/
   {  "Spectre",			{-5, 4, 4, 2,-3,-6 }, 	10, 25, 20,  5,  5, 14,-15, -5, 	 7, 180, 100,  30,   72,  6, 100, 25,  66,  4, 100,  20,  5,  0x1F4E,   FALSE,	TRUE,	FALSE,	FALSE,},/* Mage, Priest, Rogue, Warrior-Mage, Mystic */
}; 

player_race sign_info[COUNT_SIGNS] = { 	
/*     Name					 STRINTWISDEXCONCHA    dis  dev sav stl sh  shf th thshoot  hd  exp  age1   2   hgt1  2  whgt1 2  hght1 2 whgt1   2  infra  choices rations	undead, f3rles	!light*/	
	{  "Free",				{ 0, 0, 0, 0, 0, 0 }, 	 0,  0,  0,  0,  0,  0,  0,  0, 	0,   0,  0,   0,   0,  0,   0,  0,   0,  0,   0,   0,  0,  0x1FFF,		TRUE,	FALSE,	FALSE,	FALSE,}, /* All */	
	{  "Draconian",			{ 2, 1, 1, 1, 2,-3 }, 	-2,  5,  3,  0,  1, 10,  5,  5, 	0, 150, 75,  33,   0,  0,   0,  0,   0,  0,   0,   0,  2,  0x1FFF,		TRUE,	FALSE,	FALSE,	FALSE,}, /* All but Rogue, Paladin and Hell Knight */	
	{  "Serpens",			{ 0, 0, 0, 0, 0, 0 }, 	 0,  0,  0, 10,  0, 10,  0,  0, 	0,   0,  0,   0,   0,  0,   0,  0,   0,  0,   0,   0,  0,  0x1FFF,		TRUE,	FALSE,	FALSE,	FALSE,}, /* All */	
	{  "Plutus",			{ 0, 0, 0, 0, 0, 0 }, 	 0, 10,  0,  0,  0, 10,  0,  0, 	0,   0,  0,   0,   0,  0,   0,  0,   0,  0,   0,   0,  0,  0x1FFF,		TRUE,	FALSE,	FALSE,	FALSE,}, /* All */	
    {  "Morui",				{ 2,-1,-1, 1, 2,-2 }, 	10,  5,  5,  0, -1, 10,  5,  5, 	2,  35,  6,   0,   0,  0,   0,  0,   0,  0,   0,   0,  2,  0x1FFF,		TRUE,	FALSE,	FALSE,	FALSE,},
};


/*  The description should be no longer than 20 characters if you want it to look pretty in the 'U'se menu */
/*  The description should be able to work with 'You can ...' if you want it to look pretty with self knowledge */
U_power racial_powers[] = {
	/*Race             desc5678901234567890   lev  cost cost/level  stat   extra_info		power*/
	{ DWARF			, "detect traps & doors",	5,	5,		0,		A_WIS,	NULL,			0,},
	{ GNOME			, "teleport",				5,	5,		0,		A_INT,	NULL,			0,},
	{ LEPRECHAUN	, "teleport",				5,	5,		5,		A_INT,	NULL,			0,},
	{ TROLL			, "berserk",				10,	12,		0,		A_WIS,	NULL,			0,},
	{ NORDIC		, "berserk",				8,	10,		0,		A_WIS,	NULL,			0,},
	{ NEPHILIM		, "use divine powers",		30,	50,		0,		A_INT,	NULL,			0,},
	{ OGRE			, "set a trap",				25,	35,		0,		A_INT,	NULL,			0,},
	{ GIANT			, "bash a wall",			20,	10,		0,		A_STR,	NULL,			0,},
	{ TITAN			, "probe",					25,	20,		0,		A_INT,	NULL,			0,},		
	{ SPECTRE		, "scare",					4,	3,		0,		A_INT,	NULL,			0,},
	{ KOBOLD		, "spit poison",			12,	8,		0,		A_DEX,	"dam lvl",		0},
	{ ATLANTIAN		, "conjure a  missile",		2,	2,		0,		A_INT,	NULL,			0,},
	{ HORROR		, "mind blast",				15,	12,		0,		A_INT,	"dam lvl",		0},
	{ IMP			, "blast fire",				9,	15,		0,		A_WIS,	"dam lvl",		0},
	{ GUARDIAN		, "cast stone skin",		20,	15,		0,		A_CON,	"dur 30+d20",	0},
	{ SKELETON		, "restore life",			30,	30,		0,		A_WIS,	NULL,			0,},
	{ MUMMY			, "restore life",			30,	30,		0,		A_WIS,	NULL,			0,},				
	{ VAMPIRE		, "drain life",				2,	1,		3,		A_CON,	NULL,			0,},
	{ FAE			, "throw faerie dust",		12,	12,		0,		A_INT,	NULL,			0,},	
	{ 0				, NULL,						0,	0,		0,		0	,	NULL,			0,},/*Famous NULL record*/	
	
};

/*  The description should be no longer than 20 characters if you want it to look pretty in the 'U'se menu */
/*  The description should be able to work with 'You can ...' if you want it to look pretty with self knowledge */
U_power sign_powers[] = {
	/*Sign                 desc5678901234567890                   lev  cost cost/level   stat   extra_info		power*/
	{ SIGN_PLUTUS		, "detect doors & traps",	5,	5,		0,		A_WIS,	NULL,			ABILITY_DETECT_DOOR_TRAPS,},
	{ SIGN_MORUI		, "spit acid",				9,	9,		0,		A_DEX,	NULL,			ABILITY_SPIT_ACID,},
	{ SIGN_DRACO		, "breathe the elements",	1,	0,		1,		A_CON,	"dam 2*lvl",	ABILITY_BREATHE_ELEMENTS,},		
	{ 0					, NULL,						0,	0,		0,		0	,	NULL,			0,},/*Famous NULL record*/	
};

/*  The description should be no longer than 20 characters if you want it to look pretty in the 'U'se menu */
/*  Another table is used for self knowledge */
U_power freak_powers[] = {
	/*Sign                 desc5678901234567890   lev  cost cost/level   stat   extra_info		power*/
	{ COR1_SPIT_ACID	, "spit acid",				9,	9,		0,		A_DEX,	"dam lvl",		COR1_SPIT_ACID,},
	{ COR1_BR_FIRE		, "fire breath",			20,	0,		1,		A_CON,	"dam 2*lvl",	COR1_BR_FIRE,},
	{ COR1_HYPN_GAZE	, "hypnotic gaze",			12,	12,		0,		A_CHA,	NULL,			COR1_HYPN_GAZE,},
	{ COR1_TELEKINES	, "telekinesis",			9,	9,		0,		A_WIS,	NULL,			COR1_TELEKINES,},
	{ COR1_VTELEPORT	, "teleport",				7,	7,		0,		A_WIS,	NULL,			COR1_VTELEPORT,},
	{ COR1_MIND_BLST	, "mind blast",				5,	3,		0,		A_WIS,	NULL,			COR1_MIND_BLST,},
	{ COR1_SLIME		, "hellslime",				15,	15,		0,		A_CON,	NULL,			COR1_SLIME,},
	{ COR1_VAMPIRISM	, "vampiric drain",			13,	0,		1,		A_CON,	NULL,			COR1_VAMPIRISM,},
	{ COR1_SMELL_MET	, "sense metal",			3,	2,		0,		A_INT,	NULL,			COR1_SMELL_MET,},	
	{ COR1_SMELL_MON	, "sense monsters",			5,	4,		0,		A_INT,	NULL,			COR1_SMELL_MON,},		
	{ COR1_BLINK		, "blink",					3,	3,		0,		A_WIS,	NULL,			COR1_BLINK,},
	{ COR1_EAT_ROCK		, "eat rock",				8,	12,		0,		A_CON,	NULL,			COR1_EAT_ROCK,},
	{ COR1_SWAP_POS		, "swap position",			15,	12,		0,		A_DEX,	NULL,			COR1_SWAP_POS,},
	{ COR1_SHRIEK		, "shriek",					4,	4,		0,		A_CON,	NULL,			COR1_SHRIEK,},
	{ COR1_ILLUMINE		, "illuminate",				3,	2,		0,		A_INT,	NULL,			COR1_ILLUMINE,},
	{ COR1_DET_CURSE	, "detect curses",			7,	14,		0,		A_WIS,	NULL,			COR1_DET_CURSE,},
	{ COR1_BERSERK		, "berserk",				8,	8,		0,		A_STR,	NULL,			COR1_BERSERK,},
	{ COR1_POLYMORPH	, "polymorph",				18,	20,		0,		A_CON,	NULL,			COR1_POLYMORPH,},
	{ COR1_MIDAS_TCH	, "midas touch",			10,	5,		0,		A_INT,	NULL,			COR1_MIDAS_TCH,},
	{ COR1_GROW_MOLD	, "grow mold",				1,	6,		0,		A_CON,	NULL,			COR1_GROW_MOLD,},
	{ COR1_RESIST		, "resist elements",		10,	12,		0,		A_CON,	NULL,			COR1_RESIST,},	
	{ COR1_EARTHQUAKE	, "earthquake",				12,	12,		0,		A_STR,	NULL,			COR1_EARTHQUAKE,},		
	{ COR1_EAT_MAGIC	, "eat magic",				17,	1,		0,		A_WIS,	NULL,			COR1_EAT_MAGIC,},			
	{ COR1_WEIGH_MAG	, "weigh magic",			6,	6,		0,		A_INT,	NULL,			COR1_WEIGH_MAG,},				
	{ COR1_STERILITY	, "sterilize",				20,	40,		0,		A_CHA,	NULL,			COR1_STERILITY,},
	{ COR1_PANIC_HIT	, "panic hit",				10,	12,		0,		A_DEX,	NULL,			COR1_PANIC_HIT,},
	{ COR1_DAZZLE		, "dazzle",					7,	15,		0,		A_CHA,	NULL,			COR1_DAZZLE,},	
	{ COR1_EYE_BEAM		, "eye beams",				7,	10,		0,		A_WIS,	NULL,			COR1_EYE_BEAM,},
	{ COR1_RECALL		, "recall",					17,	50,		0,		A_INT,	NULL,			COR1_RECALL,},	
	{ COR1_BANISH		, "banish evil",			25,	25,		0,		A_WIS,	NULL,			COR1_BANISH,},		
	{ COR1_COLD_TOUCH	, "cold touch",				2,	2,		0,		A_CON,	NULL,			COR1_COLD_TOUCH,},			
	{ COR1_LAUNCHER		, "cold touch",				2,	2,		0,		A_CON,	NULL,			3,}, /* XXX_XXX_XXX Hack! COR1_LAUNCHER counts as negative... */			
	{ 0					, NULL,						0,	0,		0,		0	,	NULL,			0,},/*Famous NULL record*/	
};

corruption_type corruptions[COUNT_CORRUPTIONS]={
	{ 1,	 COR1_SPIT_ACID, 	"You gain the ability to spit acid.",						4,"You lose the ability to spit acid.",                       " You can spit acid (dam lvl).",                           },
	{ 1,	 COR1_BR_FIRE,		"You gain the ability to breathe fire.",					3,"You lose the ability to breathe fire.",                    " You can breathe fire (dam lvl * 2).",                    },
	{ 1,	 COR1_HYPN_GAZE,	"Your eyes look mesmerizing...",							2,"Your eyes look uninteresting.",                            " Your gaze is hypnotic.",                                 },
	{ 1,	 COR1_TELEKINES,	"You gain the ability to move objects telekinetically.",	2,"You lose the ability to move objects telekinetically.",    " You are telekinetic.",                                   },
	{ 1,	 COR1_VTELEPORT,	"You gain the power of teleportation at will.",				3,"You lose the power of teleportation at will.",             " You can teleport at will.",                              },
	{ 1,	 COR1_MIND_BLST,	"You gain the power of Mind Blast.",						2,"You lose the power of Mind Blast.",                        " You can Mind Blast your enemies.",                       },
	{ 1,	 COR1_SLIME,		"You gain the power to summon hellslime.",					2,"You lose the power to summon hellslime.",                  " You can summon waves of hellslime.",                     },
	{ 1,	 COR1_VAMPIRISM,	"You become vampiric.",										2,"You are no longer vampiric.",                              " You can drain life from a foe like a vampire.",          },
	{ 1,	 COR1_SMELL_MET,	"You smell a metallic odor.",								3,"You no longer smell a metallic odor.",                     " You can smell nearby precious metal.",                   },
	{ 1,	 COR1_SMELL_MON,	"You smell filthy monsters.",								4,"You no longer smell filthy monsters.",                     " You can smell nearby monsters.",                         },
	{ 1,	 COR1_BLINK,		"You gain the power of minor teleportation.",				3,"You lose the power of minor teleportation.",               " You can teleport yourself short distances.",             },
	{ 1,	 COR1_EAT_ROCK,		"The walls look delicious.",								2,"The walls look unappetizing.",                             " You can consume solid rock.",                            },
	{ 1,	 COR1_SWAP_POS,		"You feel like walking a mile in someone else's shoes.",	2,"You feel like staying in your own shoes.",                 " You can switch locations with another being.",           },
	{ 1,	 COR1_SHRIEK,		"Your vocal cords get much tougher.",						3,"Your vocal cords get much weaker.",                        " You can emit a horrible shriek.",                        },
	{ 1,	 COR1_ILLUMINE,		"You can light up rooms with your presence.",				3,"You can no longer light up rooms with your presence.",     " You can emit bright light.",                             },
	{ 1,	 COR1_DET_CURSE,	"You can feel evil magics.",								2,"You can no longer feel evil magics.",                      " You can feel the danger of evil magic.",                 },
	{ 1,	 COR1_BERSERK,		"You feel a controlled rage.",								3,"You no longer feel a controlled rage.",                    " You can drive yourself into a berserk frenzy.",          },
	{ 1,	 COR1_POLYMORPH,	"Your body seems mutable.",									1,"Your body seems stable.",                                  " You can polymorph yourself at will.",                    },
	{ 1,	 COR1_MIDAS_TCH,	"You gain the Midas touch.",								2,"You lose the Midas touch.",                                " You can turn ordinary items to gold.",                   },
	{ 1,	 COR1_GROW_MOLD,	"You feel a sudden affinity for mold.",						1,"You feel a sudden dislike for mold.",                      " You can cause mold to grow near you.",                   },
	{ 1,	 COR1_RESIST,		"You feel like you can protect yourself.",					3,"You feel like you might be vulnerable.",                   " You can harden yourself to the ravages of the elements.",},
	{ 1,	 COR1_EARTHQUAKE,	"You gain the ability to wreck the dungeon.",				3,"You lose the ability to wreck the dungeon.",               " You can bring down the dungeon around your ears.",       },
	{ 1,	 COR1_EAT_MAGIC,	"Your magic items look delicious.",							1,"Your magic items no longer look delicious.",               " You can consume magic energy for your own use.",         },
	{ 1,	 COR1_WEIGH_MAG,	"You feel you can better understand the magic around you.",	2,"You no longer sense magic.",                               " You can feel the strength of the magics affecting you.", },
	{ 1,	 COR1_STERILITY,	"You can give everything around you a headache.",			1,"You hear a massed sigh of relief.",                        " You can cause mass impotence.",                          },
	{ 1,	 COR1_PANIC_HIT,	"You suddenly understand how thieves feel.",				2,"You no longer feel jumpy.",                                " You can run for your life after hitting something.",     },
	{ 1,	 COR1_DAZZLE,		"You gain the ability to emit dazzling lights.",			3,"You lose the ability to emit dazzling lights.",            " You can emit confusing, blinding lights.",               },
	{ 1,	 COR1_EYE_BEAM,		"Your eyes burn for a moment.",								3,"Your eyes burn for a moment, then feel soothed.",          " Your eyes can fire beams of light.",                     },
	{ 1,	 COR1_RECALL,		"You feel briefly homesick, but it passes.",				2,"You feel briefly homesick.",                               " You can travel between town and the depths.",            },
	{ 1,	 COR1_BANISH,		"You feel a holy wrath fill you.",							1,"You no longer feel a holy wrath.",                         " You can send evil creatures directly to Hell.",          },
	{ 1,	 COR1_COLD_TOUCH,	"Your hands get very cold.",								2,"Your hands warm up.",                                      " You can freeze things with a touch.",                    },
	{ 1,	 COR1_LAUNCHER,		"Your throwing arm feels much stronger.",					2,"Your throwing arm feels much weaker.",                     " You can hurl objects with great force.",                 },
	{ 2,	 COR2_BERS_RAGE,	"You become subject to fits of berserk rage!",				1,"You are no longer subject to fits of berserk rage!",       " You are subject to berserker fits.",                     },
	{ 2,	 COR2_COWARDICE,	"You become an incredible coward!",							1,"You are no longer an incredible coward!",                  " You are subject to cowardice.",                          },
	{ 2,	 COR2_RTELEPORT,	"Your position seems very uncertain...",					1,"Your position seems more certain.",                        " You are teleporting randomly.",                          },
	{ 2,	 COR2_FORGET,		"You are subject to fits of amnesia!",						1,"Your amnesia clears up!",                                  " You are subject to fits of amnesia.",                    },
	{ 2,	 COR2_HALLU,		"You are afflicted by a hallucinatory insanity!",			1,"You are no longer afflicted by a hallucinatory insanity!", " You have a hallucinatory insanity.",                     },
	{ 2,	 COR2_BRIMSTONE,	"You become a conduit for the fires of hell.",				1,"You are no longer a conduit to hell fires.",               " You are occasionally surrounded with brimstone.",        },
	{ 2,	 COR2_POISON_FANGS, "You grow poison fangs!",									2,"You lose your poison fangs!",                              " You have poison fangs (poison, 3d7).",                   },
	{ 2,	 COR2_CLAWS,		"Your fingernails grow into razor sharp claws!",			2,"Your fingernails return to normal!",                       " You have razor sharp claws (dam. 2d6)..",                },
	{ 2,	 COR2_LARGE_HORNS,	"You grow large horns on your head!",						2,"Your horns shrivel and fall off!",                         " You have large horns on your head (dam. 2d4).",          },
	{ 2,	 COR2_ATT_DEMON,	"You start attracting demons.",								2,"You stop attracting demons.",                              " You attract demons",                                     },
	{ 2,	 COR2_PROD_MANA,	"You start producing magical energy uncontrollably.",		1,"You stop producing magical energy uncontrollably.",        " You are producing magical energy uncontrollably",        },
	{ 2,	 COR2_SPEED_FLUX,	"You become manic-depressive.",								2,"You are no longer manic-depressive.",                      " You move faster or slower randomly.",                    },
	{ 2,	 COR2_BANISH_ALL,	"You feel a terrifying power lurking behind you.",			2,"You no longer feel a terrifying power lurking behind you."," You sometimes cause nearby creatures to vanish.",        },
	{ 2,	 COR2_EAT_LIGHT,	"You feel a strange kinship with the night.",				1,"You feel the world's a brighter place.",                   " You sometimes feed off of the light around you.",        },
	{ 2,	 COR2_SMALL_HORNS,	"You grow small horns on your head.",						2,"Your horns shrivel and fall off.",                         " You have small horns on your head (dam 1d4).",           },
	{ 2,	 COR2_ATT_ANIMAL,	"You start attracting animals.",							1,"You stop attracting animals.",                             " You attract animals.",                                   },
	{ 2,	 COR2_SPINES,		"Bony spines grow from your elbows and knees.",				1,"Your spines vanish from your joints.",                     " You have bony spines on your joints (dam 2d5).",         },
	{ 2,	 COR2_RAW_CHAOS,	"You feel the universe is less stable around you.",			1,"You feel the universe is more stable around you.",         " You occasionally are surrounded with raw chaos.",        },
	{ 2,	 COR2_NORMALITY,	"You feel strangely normal.",								3,"You feel normally strange.",                               " You may be corrupted, but you're recovering.",           },
	{ 2,	 COR2_WRAITH,		"You start to fade in and out of the physical world.",		1,"You are firmly in the physical world.",                    " You fade in and out of physical reality.",               },
	{ 2,	 COR2_POLY_WOUND,	"You feel forces of chaos entering your old scars.",		1,"You feel forces of chaos departing your old scars.",       " Your health is subject to chaotic forces.",              },
	{ 2,	 COR2_WASTING,		"You suddenly contract a horrible wasting disease.",		1,"You are cured of the horrible wasting disease!",           " You have a horrible wasting disease.",                   },
	{ 2,	 COR2_ATT_DRAGON,	"You start attracting dragons.",							1,"You stop attracting dragons.",                             " You attract dragons.",                                   },
	{ 2,	 COR2_WEIRD_MIND,	"Your thoughts suddenly take off in strange directions.",	2,"Your thoughts return to boring paths.",                    " Your mind randomly expands and contracts.",              },
	{ 2,	 COR2_NAUSEA,		"Your stomach starts to roil nauseously.",					1,"Your stomach stops roiling.",                              " You have a seriously upset stomach.",                    },
	{ 2,	 COR2_PATRON,		"You attract the notice of an infernal patron!",			2,"You lose the attention of the infernal patron.",           " An infernal patron gives you gifts.",                    },
	{ 2,	 COR2_INTUITION,	"You gain an intuition into the world.",					1,"You lose your strange intuition.",                         " You notice when miracles change the world.",             },
	{ 2,	 COR2_WARNING,		"You suddenly feel paranoid.",								2,"You no longer feel paranoid.",                             " You receive warnings about your foes.",                  },
	{ 2,	 COR2_INVULN,		"You are blessed with fits of invulnerability.",			1,"You are no longer blessed with fits of invulnerability.",  " You occasionally feel invincible.",                      },
	{ 2,	 COR2_SP_TO_HP,		"You are subject to fits of magical healing.",				2,"You are no longer subject to fits of magical healing.",    " Your blood sometimes rushes to your muscles.",           },
	{ 2,	 COR2_HP_TO_SP,		"You are subject to fits of painful clarity.",				1,"You are no longer subject to fits of painful clarity.",    " Your blood sometimes rushes to your head.",              },
	{ 2,	 COR2_DISARM,		"Your feet grow to four times their former size.",			1,"Your feet shrink to their former size.",                   " You occasionally stumble and drop things.",              },
	{ 3,	 COR3_HYPER_STR,	"You turn into a superhuman he-man!",						3,"Your muscles revert to normal.",                           " You are superhumanly strong (+4 STR).",                  },
	{ 3,	 COR3_PUNY,			"Your muscles wither away...",								3,"Your muscles revert to normal.",                           " You are puny (-4 STR).",                                 },
	{ 3,	 COR3_HYPER_INT,	"Your brain throbs with ideas!",							3,"Your brain reverts to normal.",                            " You are a genius (+4 INT/WIS).",                         },
	{ 3,	 COR3_IDIOTIC,		"Your brain withers away...",								3,"Your brain reverts to normal",                             " You are an idiot (-4 INT/WIS).",                         },
	{ 3,	 COR3_RESILIENT,	"You become extraordinarily resilient.",					2,"You become ordinarily resilient again.",                   " You are very resilient (+4 CON).",                       },
	{ 3,	 COR3_XTRA_FAT,		"You become sickeningly fat!",								2,"You benefit from a miracle diet!",                         " You are extremely fat (+2 CON, -2 speed).",              },
	{ 3,	 COR3_ALBINO,		"You turn into an albino! You feel frail...",				2,"You are no longer an albino!",                             " You are albino (-4 CON).",                               },
	{ 3,	 COR3_FLESH_ROT,	"Your flesh is afflicted by a rotting disease!",			3,"Your flesh is no longer afflicted by a rotting disease!",  " Your flesh is rotting (-2 CON, -1 CHA).",                },
	{ 3,	 COR3_SILLY_VOI,	"Your voice turns into a hoarse rasp!",						2,"Your voice returns to normal.",                            " Your voice is a hoarse rasp (-4 CHA).",                  },
	{ 3,	 COR3_FORKED_TONGUE, "Your tongue becomes forked!",								2,"Your tongue returns to normal.",                           " You have a forked tongue (-1 CHA).",                     },
	{ 3,	 COR3_ILL_NORM,		"You start projecting a reassuring image.",					1,"You stop projecting a reassuring image.",                  " Your appearance is masked with illusion.",               },
	{ 3,	 COR3_GLOW_EYES,	"Your eyes start to glow!",									3,"Your eyes stop glowing!",                                  " You have glowing eyes (+15 search).",                    },
	{ 3,	 COR3_MAGIC_RES,	"You become resistant to magic.",							2,"You become susceptible to magic again.",                   " You are resistant to magic.",                            },
	{ 3,	 COR3_STENCH,		"You start smelling like the dead!",						3,"You stop smelling so bad!",                                " You have a stench of the grave about you (-3 stealth).", },
	{ 3,	 COR3_INFRAVIS,		"Your infravision is improved.",							3,"Your infravision is degraded.",                            " You have remarkable infravision (+3).",                  },
	{ 3,	 COR3_GOAT_LEGS,	"Your legs become cloven hooved!",							2,"Your hooves return to normal feet!",                       " You have goat legs with cloven hooves (+3 speed).",      },
	{ 3,	 COR3_SHORT_LEG,	"Your legs turn into short stubs!",							2,"Your legs lengthen to normal.",                            " Your legs are short stubs (-3 speed).",                  },
	{ 3,	 COR3_ELEC_TOUC,	"Electricity starts running through you!",					2,"Electricity stops running through you.",                   " Electricity is running through your veins.",             },
	{ 3,	 COR3_FIRE_BODY,	"Your body is enveloped in flames!",						2,"Your body is no longer enveloped in flames.",              " Your body is enveloped in flames.",                      },
	{ 3,	 COR3_WART_SKIN,	"Disgusting warts appear everywhere on you!",				3,"Your warts disappear!",                                    " Your skin is covered with warts (-2 CHA, +5 AC).",       },
	{ 3,	 COR3_SCALES,		"Your skin turns into black scales!",						3,"Your scales vanish!",                                      " Your skin has turned into scales (-1 CHA, +10 AC).",     },
	{ 3,	 COR3_IRON_SKIN,	"Your skin turns to steel!",								2,"Your skin reverts to flesh!",                              " Your skin is made of steel (-1 DEX, +25 AC).",           },
	{ 3,	 COR3_WINGS,		"You grow a pair of wings.",								2,"Your wings fall off.",                                     " You have wings.",                                        },
	{ 3,	 COR3_FEARLESS,		"You become completely fearless.",							3,"You begin to feel fear again.",                            " You are completely fearless.",                           },
	{ 3,	 COR3_REGEN,		"You start regenerating.",									2,"You stop regenerating.",                                   " You are regenerating.",                                  },
	{ 3,	 COR3_ESP,			"You develop a telepathic ability!",						2,"You lose your telepathic ability!",                        " You are telepathic.",                                    },
	{ 3,	 COR3_LIMBER,		"Your muscles become limber.",								3,"Your muscles stiffen.",                                    " Your body is very limber (+3 DEX).",                     },
	{ 3,	 COR3_ARTHRITIS,	"Your joints suddenly hurt.",								3,"Your joints stop hurting.",                                " Your joints ache constantly (-3 DEX).",                  },
	{ 3,	 COR3_RES_TIME,		"You feel immortal.",										1,"You feel all too mortal.",                                 " You are protected from the ravages of time.",            },
	{ 3,	 COR3_VULN_ELEM,	"You feel strangely exposed.",								1,"You feel less exposed.",                                   " You are susceptible to damage from the elements.",       },
	{ 3,	 COR3_MOTION,		"You move with new assurance.",								3,"You move with less assurance.",                            " Your movements are precise and forceful (+1 STL).",      },
	{ 3,	 COR3_SUS_STATS,	"You feel like you can recover from anything.",				2,"You no longer feel like you can recover from anything.",   " Your body resists serious damage.",                      },
};

opposed_corruptions_type opposed_corruptions[] = {
	{	3,	COR3_PUNY,			3, COR3_HYPER_STR,		"You no longer feel super-strong!"		},
	{	3,	COR3_HYPER_STR,		3, COR3_PUNY,			"You no longer feel puny!"				},			
	{	3,	COR3_IDIOTIC,		3, COR3_HYPER_INT,		"You are no longer a genius."			},
	{	3,	COR3_HYPER_INT,		3, COR3_IDIOTIC,		"You are no longer an idiot."			},		
	{	3,	COR3_IRON_SKIN,		3, COR3_SCALES,			"You lose your scales."					},					
	{	3,	COR3_IRON_SKIN,		3, COR3_FLESH_ROT,		"Your flesh rots no longer."			},					
	{	3,	COR3_IRON_SKIN,		3, COR3_WART_SKIN,		"You lose your warts."					},	
	{	3,	COR3_SCALES,		3, COR3_IRON_SKIN,		"Your skin is no longer made of steel."	},
	{	3,	COR3_FLESH_ROT,		3, COR3_IRON_SKIN,		"Your skin is no longer made of steel."	},
	{	3,	COR3_WART_SKIN,		3, COR3_IRON_SKIN,		"Your skin is no longer made of steel."	},			
	{	3,	COR3_FEARLESS,		2, COR3_FEARLESS,		"You are no longer cowardly."			},
	{	2,	COR2_COWARDICE,		3, COR2_COWARDICE,		"You no longer feel fearless."			},			
	{	3,	COR3_FLESH_ROT,		3, COR3_REGEN,			"You stop regenerating."				},
	{	3,	COR3_REGEN,			3, COR3_FLESH_ROT,		"You flesh stops rotting."				},
	{	3,	COR3_LIMBER,		3, COR3_ARTHRITIS,		"Your joints stop hurting."				},
	{	3,	COR3_ARTHRITIS,		3, COR3_LIMBER,			"You no longer feel limber."			},
	{	2,	COR2_LARGE_HORNS,	2, COR2_SMALL_HORNS,	"Your horns grow larger."				},			
	{	2,	COR2_SMALL_HORNS,	2, COR2_LARGE_HORNS,	"Your horns shrink."					},
	{	0,	0,					0, 0,					NULL									}, /*Famous NULL record*/	
};


/*
* Player Classes
*
*      Title,
*      {STR,INT,WIS,DEX,CON,CHA},
*      c_dis, c_dev, c_sav, c_stl, c_srh, c_fos, c_thn, c_thb,
*      x_dis, x_dev, x_sav, x_stl, x_srh, x_fos, x_thn, x_thb,
*      HD, Exp
*/
player_class class_info[MAX_CLASS] =
{
	{		"Warrior",		{ 5,-2,-2, 2, 2,-1},	25,  18,  18,  1,  14,  2,   70,  55,	12,  7,   10,  0,  0,  0,  45,  45,  9,  0	},
	{		"Mage",			{-5, 3, 0, 1,-2, 1},	30,  36,  30,  2,  16,  20,  34,  20,	7,   13,  9,   0,  0,  0,  15,  15,	 0,  30	},
	{		"Priest",		{-1,-3, 3,-1, 0, 2},	25,  30,  32,  2,  16,  8,   48,  35,	7,   10,  12,  0,  0,  0,  20,  20,	 2,  20	},
	{		"Rogue",		{ 2, 1,-2, 3, 1,-1},	45,  32,  28,  5,  32,  24,  60,  66,	15,  10,  10,  0,  0,  0,  40,  30,	 6,  25	},
	{		"Ranger",		{ 2, 2, 0, 1, 1, 1},	30,  32,  28,  3,  24,  16,  56,  72,	8,   10,  10,  0,  0,  0,  30,  45,	 4,  30	},
	{		"Paladin",		{ 3,-3, 1, 0, 2, 2},	20,  24,  26,  1,  12,  2,   68,  40,	7,   10,  11,  0,  0,  0,  35,  30,	 6,  35	},
	{		"Warrior-Mage",	{ 2, 2, 0, 1, 0, 1},	30,  30,  28,  2,  18,  16,  50,  25,	7,   10,  9,   0,  0,  0,  20,  20,	 4,  50	},
	{		"Hell Knight",	{ 2, 1, 0, 1, 2,-2},	20,  25,  25,  1,  14,  12,  65,  40,	7,   11,  10,  0,  0,  0,  34,  29,	 6,  35	},
	{		"Mystic",		{ 2,-1, 1, 3, 2, 1},	45,  32,  28,  5,  32,  24,  64,  60,	15,  11,  10,  0,  0,  0,  40,  30,	 6,  40	},
	{		"Mindcrafter",	{-1, 0, 3,-1,-1, 2},   	30,  30,  30,  3,  22,  16,  50,  40,	10,  10,  10,  0,  0,  0,  20,  30,	 2,  25	},/* note: spell stat is Wis */
	{		"High-Mage",	{-5, 4, 0, 0,-2, 1},	30,  36,  30,  2,  16,  20,  34,  20,	7,   13,  9,   0,  0,  0,  15,  15,	 0,  30	},
	{		"Druid",		{-1,-3, 4,-2, 0, 3},	25,  30,  32,  2,  16,  8,   48,  35,	7,   10,  12,  0,  0,  0,  20,  20,	 2,  20	},
	{		"Warlock",		{-5, 4, 0, 0,-2, 1},	30,  36,  30,  2,  16,  20,  34,  20,	7,   13,  9,   0,  0,  0,  15,  15,	 0,  30	},
};

/*
 *   Spell Book
 *   Spell Xtra
 *   Spell Stat,
 *   Spell Type,
 *   Spell Level,
 *   Spell Encumbrance,
 *   Array of skill per realm
 */

class_magic realms_info[] = {
	/*Class Name		Book				Xtra    Stat,	Type,	Level,	Encumbrance,  MIRACLES	SORCERY NATURE	CHAOS	DEATH	TAROT	CHARMS	SOMATIC DEMONIC*/
	/*Warrior     */{	0,					0,		A_STR,	0,		99,		0,			{ NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		},},
	/*Mage        */{	TV_SORCERY_BOOK,	0,		A_INT,	0,		1,		300,		{ SAME,		SAME,	SAME,	SAME,	SAME,	SAME,	SAME,	SAME,	SAME,	},},
	/*Priest      */{	TV_MIRACLES_BOOK,	0,		A_WIS,	1,		1,		350,		{ SUPER,	WORSE,	SAME,	WORSE,	SUPER,	WORSE,	WORSE,	WORSE,	POOR,	},},
	/*Rogue       */{	TV_SORCERY_BOOK,	0,		A_INT,	0,		5,		350,		{ POOR,		SAME,	POOR,	SAME,	BETTER,	BETTER,	BETTER,	WORSE,	WORSE,	},},
	/*Ranger      */{	TV_MIRACLES_BOOK,	0,		A_INT,	0,		3,		400,		{ NO,		NO,		BETTER,	WORSE,	POOR,	WORSE,	WORSE,	WORSE,	NO,		},},
	/*Paladin     */{	TV_MIRACLES_BOOK,	0,		A_WIS,	1,		1,		400,		{ BETTER,	NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		},},
	/*Warrior-Mage*/{	TV_SORCERY_BOOK,	0,		A_INT,	0,		1,		350,		{ WORSE,	WORSE,	WORSE,	WORSE,	WORSE,	WORSE,	SUPER,	WORSE,	WORSE,  },},
	/*Hell Knight */{	TV_MIRACLES_BOOK,	0,		A_INT,	1,		2,		400,		{ NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		BETTER, },},
	/*Mystic      */{	TV_MIRACLES_BOOK,	0,		A_WIS,	0,		1,		300,		{ NO,		NO,		NO,		NO,		NO,		NO,		NO,		SUPER,	NO,		},},
	/*Mindcrafter */{	TV_MIRACLES_BOOK,	0,		A_WIS,	0,		99,		300,		{ NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		NO,		},},
	/*High Mage   */{	TV_SORCERY_BOOK,	0,		A_INT,	0,		1,		300,		{ BETTER,	BETTER,	BETTER,	BETTER,	BETTER,	BETTER,	BETTER,	BETTER,	BETTER, },},
	/*Druid       */{	TV_MIRACLES_BOOK,	0,		A_WIS,	1,		1,		350,		{ NO,		NO,		SUPER,	NO,		NO,		NO,		NO,		NO,		NO,		},},
	/*Warlock     */{	TV_SORCERY_BOOK,	0,		A_INT,	0,		1,		300,		{ WORSE,	WORSE,	WORSE,	BETTER,	WORSE,	WORSE,	WORSE,	WORSE,	BETTER, },},
};

/*
 * This table contains the progression of the spells, the center column simply goes up from 1 to 50
 * from left to right you see the worst progression to the best progression
 * technically I could have left out the middle column, but it makes it now easier to wrap your brain
 * around it.
 * So example : if a mage gets Miracle FOO on level 15 , a palladin or a high-mage would get it on 14 and a priest on level 7
 * a warlock woudl get it on level 18. A thief would get it on level 20.
 */
byte spell_skill_level[50][5]={
	/* POOR WORSE MAGE BETTER SUPER */
	{  3,   2,    1,   1,     1, },
	{  5,   4,    2,   2,     1, },
	{  5,   4,    3,   2,     1, },
	{  5,   5,    4,   3,     1, },
	{  7,   6,    5,   4,     3, },
	{  8,   7,    6,   4,     3, },
	{  9,   8,    7,   5,     3, },
	{  9,   8,    7,   5,     3, },
	{  11,  10,   9,   5,     5, },
	{  12,  10,   10,  6,     5, },
	{  14,  14,   11,  9,     5, },    
	{  14,  14,   12,  9,     5, },    
	{  17,  16,   13,  12,    7, },
	{  17,  16,   14,  12,    7, },
	{  20,  18,   15,  14,    7  },
	{  20,  18,   16,  14,    7, },
	{  24,  20,   17,  15,    9, },
	{  28,  22,   18,  15,    9, },
	{  30,  24,   19,  17,    10,},
	{  33,  26,   21,  19,    10,},
	{  33,  26,   22,  19,    10,},
	{  35,  28,   23,  21,    11,},
	{  35,  28,   23,  21,    11,},
	{  36,  28,   24,  21,    11,},
	{  36,  28,   25,  20,    15,},
	{  37,  28,   26,  20,    15,},
	{  37,  29,   27,  20,    15,},
	{  37,  29,   28,  24,    15,},
	{  38,  33,   29,  25,    17,},    
	{  38,  33,   30,  25,    17,},
	{  38,  33,   31,  25,    17,},
	{  38,  33,   32,  25,    17,},
	{  40,  34,   33,  30,    18,},
	{  40,  36,   34,  30,    18,},
	{  44,  42,   35,  33,    25,},
	{  44,  42,   36,  33,    25,},
	{  45,  43,   37,  33,    25,},
	{  45,  43,   38,  33,    25,},
	{  45,  44,   39,  35,    33,},
	{  46,  44,   40,  35,    33,},
	{  46,  44,   41,  40,    35,},
	{  47,  45,   42,  40,    35,},
	{  47,  45,   43,  40,    35,},
	{  48,  46,   44,  41,    38,},
	{  48,  46,   45,  42,    38,},
	{  49,  47,   46,  40,    38,},
	{  49,  48,   47,  41,    39,},
	{  50,  48,   48,  45,    40,},
	{  50,  49,   49,  47,    42,},
	{  50,  50,   50,  49,    45,},
};


/*
 * This table contains the cost of the spells, the center column simply goes up from 1 to 25 
 * and then shows the delta's for costs higher than 25
 * from left to right you see the worst progression to the best progression
 * technically I could have left out the middle column, but it makes it now easier to wrap your brain
 * around it.
 * So example : if a Miracle Spell cost 13 for mage, it would cost 9 for a palladin or a high-mage, 4 for a priest,
 * 15 for a warlock. 21 for a thief
 * Example 2 explaining last line : A miracle spell costing 100 for a mage , is 90 for high=mage and 
 */
int spell_skill_mana[26][5] = {
	/* POOR WORSE MAGE BETTER SUPER */
	{  2,   2,    1,   1,     1,}, 
	{  4,   4,    2,   2,     2,},
	{  5,   5,    3,   3,     2,},
	{  6,   5,    4,   3,     2,},
	{  8,   6,    5,   3,     2,},
	{  8,   7,    6,   4,     2,},
	{  11,  8,    7,   4,     3,},
	{  12,  9,    8,   5,     4,},
	{  12,  9,    9,   5,     4,},
	{  12,  9,    10,  5,     4,},
	{  20,  14,   11,  8,     4,},
	{  20,  14,   12,  8,     4,},
	{  21,  15,   13,  9,     4,},
	{  23,  16,   14,  10,    5,},
	{  23,  16,   15,  10,    5,},
	{  23,  16,   16,  10,    5,},
	{  28,  20,   17,  15,    6,},
	{  29,  22,   18,  15,    7,},
	{  30,  24,   19,  15,    8,},
	{  32,  26,   20,  15,    8,},
	{  33,  26,   21,  15,    10,},
	{  33,  27,   22,  15,    10,},
	{  34,  27,   23,  15,    12,},
	{  34,  28,   24,  15,    12,},
	{  35,  28,   25,  24,    14,}, 
	{  10,  5,    0,  -10    -15,},   
};


u32b fake_spell_flags[4]=
{
0x000000ff,
0x0000ff00,
0x00ff0000,
0xff000000,
};

u16b realm_choices[MAX_CLASS]=
{
/* Warrior */		(CH_NONE),
/* Mage */			(CH_MIRACLES | CH_SORCERY | CH_NATURE | CH_CHAOS | CH_DEATH | CH_TAROT | CH_CHARMS | CH_SOMATIC | CH_DEMONIC),
/* Priest */		(CH_NATURE | CH_CHAOS | CH_TAROT | CH_CHARMS | CH_SOMATIC | CH_DEMONIC),
/* Rogue */			(CH_SORCERY | CH_DEATH | CH_TAROT | CH_CHARMS | CH_DEMONIC ),
/* Ranger */		(CH_CHAOS | CH_DEATH | CH_TAROT | CH_CHARMS | CH_SOMATIC | CH_DEMONIC),
/* Paladin */		(CH_MIRACLES | CH_DEATH),
/* Warrior-Mage */  (CH_MIRACLES | CH_NATURE | CH_CHAOS | CH_DEATH | CH_TAROT | CH_CHARMS | CH_SORCERY | CH_SOMATIC | CH_DEMONIC),
/* Hell Knight */	(CH_DEMONIC),
/* Mystic */        (CH_SOMATIC),
/* Mindcrafter */   (CH_NONE),
/* High Mage */     (CH_MIRACLES | CH_SORCERY | CH_NATURE | CH_CHAOS | CH_DEATH | CH_TAROT | CH_CHARMS | CH_SOMATIC | CH_DEMONIC),
/* Druid */         (CH_NATURE),
/* Warlock */  (CH_MIRACLES | CH_SORCERY | CH_NATURE | CH_DEATH | CH_TAROT | CH_CHARMS | CH_SOMATIC),
};

cptr realm_names [] =
{
"no magic",
"Miracles",
"Sorcery",
"Nature",
"Chaos",
"Death",
"Tarot",
"Charms",
"Somatic",
"Demonic"
};


spell_type spells[MAX_REALM][32] = 
{
	/*Miracles */
	{
	{ 1, 1, 30, 4,      "Detect Evil",                  NULL,  "Detects all evil creatures that are nearby.",},    
	{ 3, 2, 35, 4,      "Cure Light Wounds",            "heal: 2d10",  "Cures you of 2d10 damage, and reduces bleeding.",},
	{ 4, 3, 35, 4,      "Bless",                        "dur: 12+1d12",  "Blesses you, giving you a +5 bonus to armour class and a +10 bonus to hit.",},
	{ 5, 5, 35, 4,      "Remove Fear",                  NULL,  "Stops you from being afraid.  ",},
	{ 7, 7, 35, 4,      "Call Light",                   "dam: 2d(PLEV / 2); rad: (PLEV/10)+1",  "Lights the area that you are in with a permanent light - possibly damaging some creatures",},
	{ 9, 8, 40, 4,      "Detect Traps and Secret Doors",NULL,  "Finds all traps, stairs and secret doors in your surrounding area.",},
	{ 12, 12, 40, 3,    "Cure Medium Wounds",           "heal: 4d10",  "Cures you of 4d10 damage, and reduces bleeding.",},
	{ 15, 14, 45, 3,    "Satisfy Hunger",               NULL,  "Removes all hunger, leaving you comfortably full.",},
		
	{ 16, 16, 45, 4,    "Remove Curse",                 NULL,  "Removes minor curses from carried objects, allowing them to be taken off.",},
	{ 17, 17, 50, 4,    "Cure Poison",                  NULL,  "Removes all poison from your system.",},
	{ 18, 18, 50, 4,    "Cure Critical Wounds",         "heal: 8d10",  "Heals 8d10 damage, and completely stops bleeding or stunning.",},
	{ 19, 19, 50, 4,    "Sense Unseen",                 "dur: 24+1d24",  "Allows you to see invisible creatures for a short while.",},
	{ 20, 20, 50, 4,    "Holy Orb",                     "dam: 3d6+PLEV+ORB",  "Fires a damaging ball of holy fire. ",},
	{ 23, 23, 50, 4,    "Protection from Evil",         "dur: 1d25+3*PLEV",  "Puts up a barrier around yourself that has a chance of stopping attacks from evil creatures.",},
	{ 30, 30, 55, 5,    "Healing",                      "heal: 300",  "Heals 300 damage and completely stops bleeding and stunning.",},
	{ 35, 70, 75, 5,    "Glyph of Warding",             NULL,  "Places a rune on the floor that many creatures will be unable to pass",},
		
	{ 26, 30, 50, 75,   "Exorcism",                     "dam: PLEV",  "Banishes demons and undead, and scares other evil creatures.",},
	{ 28, 25, 70, 150,  "Dispel Curse",                 NULL,  "Removes more powerful curses from carried objects.",},
	{ 33, 33, 60, 75,   "Dispel Undead & Demons",       "dam: PLEV*3",  "Triple strength banishment against undead and demonic creatures.",},
	{ 35, 35, 60, 75,   "Day of the Dove",              "charm: PLEV*2",  "Befriends all nearby monsters.",},
	{ 35, 35, 70, 75,   "Dispel Evil",                  "dam: PLEV*4",  "Quadruple strength banishment against all evil creatures.",},
	{ 35, 55, 80, 115,  "Banish",                       "dam: 100",  "Teleports away all nearby evil creatures to elsewhere on the level.",},
	{ 39, 40, 80, 125,  "Holy Word",                    "dam: PLEV*4; heal: 1000",  "Banishes evil, heals 1000 damage, removes bleeding, fear, poison and stunning.",},
	{ 46, 70, 80, 150,  "Warding True",                 NULL,  "Surrounds you with Glyphs of Warding.",},
		
	{ 9, 9, 50, 40,     "Heroism",                      "dur: 1d25+25; heal: 10",  "Gives you a +12 bonus to hit, 10 extra hit points and protection from fear.",},
	{ 25, 25, 50, 50,   "Prayer",                       "dur: 1d48+48",  "Like Bless, except that it lasts longer.",},
	{ 35, 85, 80, 115,  "Bless Weapon",                 NULL,  "Makes a weapon blessed - and thus usable by priests. This also removes any curse on the weapon. It is dangerous to use this spell on artifacts, as it may damage them.",},
	{ 42, 100, 80, 225, "Restoration",                  NULL,  "Removes any drain or damage from any of your abilities, and removes any experience drain.",},
	{ 45, 90, 80, 115,  "Healing True",                 "heal: 2000",  "Heals 2000 damage, all bleeding and all stunning.",},
	{ 48, 50, 80, 100,  "Holy Vision",                  NULL,  "Fully identifies an item",},
	{ 49, 100, 80, 250, "Divine Intervention",          "heal: 300; dam: PLEV*4",  "This banishes, stuns, confuses, scares, slows and paralyses monsters that surround you, as well as healing you and hasting you to allow you a retreat.",},
	{ 50, 100, 80, 250, "Holy Invulnerability",         "dur: 7+1d7",  "Makes you completely immune to damage for a short period of time.",}, 
	},
	/*Sorcery*/
	{                                                                 
	{ 1, 1, 23, 4,      "Detect Monsters",              NULL,  "Detects the presence of all monsters in the nearby area.",},
	{ 1, 2, 24, 4,      "Phase Door",                   "range: 10",  "Teleports you to a random location within 10o'",},
	{ 3, 3, 25, 1,      "Detect Doors and Traps",       NULL,  "Reveals any doors, stairs and traps that are in the nearby area.",},
	{ 3, 3, 30, 1,      "Light Area",                   "dam: 2d(PLEV / 2); rad: (PLEV / 10) + 1",  "Creates a permanent light in your surrounding area, possibly damaging creatures.",},
	{ 4, 4, 30, 1,      "Confuse Monster",              "strength: ( PLEV * 3) / 2",  "Confuses a nearby (non-undead) monster. ",},
	{ 5, 5, 35, 5,      "Teleport",                     "range : PLEV*5",  "Teleports you to a random location on the level.",},
	{ 6, 5, 30, 4,      "Sleep Monster",                NULL,  "Sends a nearby monster to sleep.",},
	{ 7, 7, 75, 9,      "Recharging",                   NULL,  "Recharges a wand, rod or staff.",},
		
	{ 9, 7, 75, 8,      "Magic Mapping",                NULL,  "Reveals the layout of your nearby surroundings.",},
	{ 10, 7, 75, 8,     "Identify",                     NULL,  "Identifies the basic abilities of an item.",},
	{ 11, 7, 75, 7,     "Slow Monster",                 NULL,  "Slows down the movement and attacks of a nearby monster.",},
	{ 13, 7, 50, 6,     "Mass Sleep",                   NULL,  "Sends all nearby monsters to sleep.",},
	{ 18, 12, 60, 8,    "Teleport Away",                NULL,  "Teleports a nearby monster away from you, to a random place on the level.",},
	{ 22, 12, 60, 8,    "Haste Self",                   "dur: 1d(20+PLEV)+PLEV",  "Speeds your movement for a limited duration. ",},
	{ 28, 20, 70, 15,   "Detection True",               NULL,  "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones).",},
	{ 33, 30, 75, 20,   "Identify True",                NULL,  "Fully identifies an item.",},
		
	{ 3, 3, 25, 15,     "Detect Objects and Treasure",  NULL,  "Reveals the location of all objects and money. ",},
	{ 10, 10, 70, 40,   "Detect Enchantment",           NULL,  "Reveals the location of magical items.",},
	{ 10, 10, 80, 40,   "Charm Monster",                NULL,  "Makes a nearby monster friendly.",},
	{ 12, 12, 80, 40,   "Dimension Door",               "range: PLEV+2",  "Teleports you to a location of your choice on the current level.",},
	{ 14, 10, 60, 25,   "Sense Minds",                  "dur: 20+1d30",  "Gives you ESP for a short duration.",},
	{ 20, 18, 85, 50,   "Self Knowledge",               NULL,  "Reveals knowledge about yourself and your abilities (including those given by items).",},
	{ 20, 18, 60, 25,   "Teleport Level",               NULL,  "Teleports you completely off the level - either to the level below or the level above.",},
	{ 25, 25, 75, 19,   "Word of Recall",               "delay: 15+1d21",  "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored.",},
		
	{ 10, 10, 40, 20,   "Stasis",                       NULL,  "Freezes a monster in time, preventing it from taking any actions for a while.",},
	{ 25, 25, 75, 70,   "Telekinesis",                  "weight: PLEV*15/10",  "Picks up an object from a distance. ",},
	{ 25, 30, 95, 160,  "Explosive Rune",               "dam: 7d7+PLEV/2",  "Leaves a rune on the floor that will explode when a creature walks over it.",},
	{ 30, 40, 80, 120,  "Clairvoyance",                 "dur: 25+1d30",  "Reveals the layout of the entire level, and gives you ESP for a short duration.",},
	{ 40, 80, 95, 200,  "Enchant Weapon",               NULL,  "Increases the magical bonuses on your weapon.",},
	{ 40, 100, 95, 200, "Enchant Armour",               NULL,  "Increases the magical bonuses on a piece of your armour.",},
	{ 42, 50, 90, 175,  "Alchemy",                      NULL,  "Turns an item to gold, giving you approximately a third of the amount you would be able to get by selling it.",},
	{ 45, 70, 75, 250,  "Globe of Invulnerability",     "dur: 8+1d8",  "Makes you completely immune to damage for a short while.",}, 
	},
	/*Nature*/
	{
	{ 1, 1, 23, 4,      "Detect Creatures",             NULL,  "Reveals the presence of all nearby creatures.",},
	{ 3, 3, 25, 3,      "First Aid",                    "heal: 2d8",  "Cures you of 2d8 damage, and reduces your bleeding.",},
	{ 3, 3, 25, 1,      "Detect Doors and Traps",       NULL,  "Reveals the location of any nearby secret doors, traps or stairs.",},
	{ 4, 4, 35, 4,      "Foraging",                     NULL,  "Produces enough food to fill you up.",},
	{ 4, 4, 50, 5,      "Daylight",                     "dam: 2d(PLEV/2)",  "Lights your immediate area with a permanent light, possibly damaging creatures.",},
	{ 4, 5, 50, 5,      "Animal Taming",                "charm: PLEV",  "Makes an animal your friend.",},
	{ 5, 5, 50, 5,      "Resist Environment",           "dur: 20+1d20",  "Gives you a temporary resistance to cold, fire and electricity.",},
	{ 5, 5, 35, 4,      "Cure Cuts & Poison",			NULL,  "Removes all poison and bleeding.",},
		
	{ 5, 5, 40, 6,      "Stone to Mud",                 NULL,  "Turns a square of rock into loose mud, effectively removing it.",},
	{ 5, 5, 30, 6,      "Lightning Bolt",               "dam: (3+((PLEV-5)/4))d8",  "Fires a bolt of lightning doing electricity damage.",},
	{ 7, 6, 45, 6,      "Nature Awareness",             NULL,  "Reveals the layout of the local area, as well as finding secret doors, stairs and traps. Also reveals the location of any nearby monsters.",},
	{ 7, 6, 40, 6,      "Frost Bolt",                   "dam: (5+((PLEV-5)/4))d8",  "Fires a bolt frost doing cold damage.",},
	{ 9, 6, 30, 5,      "Ray of Sunlight",              NULL,  "Creates a beam of permanent light, possibly damaging creatures.",},
	{ 19, 12, 55, 8,    "Entangle",                     NULL,  "Causes vines to grow from the floor, slowing all nearby monsters.",},
	{ 25, 25, 90, 50,   "Summon Animal",                NULL,  "Summons an animal ally to help you.",},
	{ 40, 100, 95, 50,  "Herbal Healing",               "heal: 1000",  "Cures 1000 damage, and removes bleeding, poison and stunning.",},
		
	{ 7, 7, 20, 28,     "Door Building",                NULL,  "Makes a door to protect you from anything which might be following you.",},
	{ 9, 12, 40, 44,    "Stair Building",               NULL,  "Creates a staircase at your feet for quick escape.",},
	{ 10, 12, 75, 120,  "Stone Skin",                   "dur: 1d20+30",  "Adds 50 to your armour class for a short while.",},
	{ 15, 20, 85, 60,   "Resistance True",              "dur: 1d20+20",  "Gives you a temporary resistance to acid, cold, fire, electricity and poison.",},
	{ 30, 30, 90, 100,  "Animal Friendship",            "charm: PLEV*2",  "Makes all nearby animals your friends.",},
	{ 37, 40, 90, 200,  "Stone Tell",                   NULL,  "Fully identifies an item.",},
	{ 38, 45, 75, 200,  "Wall of Stone",                NULL,  "Creates a wall of stone to protect you from anything which might be following you.",},
	{ 40, 90, 90, 250,  "Protect from Corrosion",       NULL,  "Permanently protects an item from acid damage.",},
		
	{ 20, 18, 60, 25,   "Earthquake",                   "rad: 10",  "Creates a large earthquake centered on your location, strong enough to collapse the roof. You are protected from the effects of this spell, safe in the epicenter.",},
	{ 23, 23, 80, 50,   "Whirlwind Attack",             NULL,  "Allows you to attack all adjacent creatures in melee.",},
	{ 25, 25, 75, 29,   "Blizzard",                     "dam: 70+PLEV;rad: (PLEV/12)+1",  "Creates a ball of frost, dealing cold damage.",},
	{ 30, 27, 75, 35,   "Lightning Storm",              "dam: 90+PLEV;rad: (PLEV/12)+1",  "Creates a ball of lightning, doing electricity damage.",},
	{ 35, 30, 85, 65,   "Whirlpool",                    "dam: 100+PLEV;rad: (PLEV/12)+1",  "Creates a whirling ball of water, doing elemental water damage.",},
	{ 37, 35, 90, 100,  "Call Sunlight",                "dam: 150",  "Creates a ball of elemental light, and also lights the whole level permanently.",},
	{ 40, 90, 95, 250,  "Elemental Branding",           NULL,  "Permanently engulfs the striking surface of a weapon in an element, causing it to do more damage.",},
	{ 40, 75, 65, 150,  "Nature's Wrath",               "dam: 4*PLEV+100+PLEV",  "Severely damages all nearby creatures and causes an earthquake.",},
	},
	/*Chaos*/
	{
	{ 1, 1, 20, 4,      "Magic Missile",                "dam: (3+(PLEV - 1) / 5)d4", "Creates a physical missile which strikes a nearby target.",},
	{ 1, 2, 22, 4,      "Trap / Door Destruction",      NULL, "Destroys any adjacent doors or traps.",},
	{ 2, 2, 25, 4,      "Flash of Light",               "dam: (2d(PLEV / 2))d((PLEV / 10) + 1)", "Permanently lights the nearby area, possibly damaging creatures.",},
	{ 5, 5, 30, 1,      "Touch of Confusion",           NULL, "Creates an aura around your hands that will confuse the next creature you attack.",},
	{ 9, 6, 50, 1,      "Mana Burst",                   "dam: 3d5+PLEV+ORB", "Fires a ball of raw magical (not elemental) energy.",},
	{ 13, 9, 45, 6,     "Fire Bolt",                    "dam: (8+((PLEV-5)/4))d8", "Fires a bolt of flame that does fire damage.",},
	{ 14, 9, 45, 6,     "Fist of Force",                "dam: (8+((PLEV-5)/4))d8", "Fires a bolt of pure force, that can disintegrate what it touches.",},
	{ 15, 9, 35, 5,     "Teleport Self",                NULL, "Teleports you to a random place on the level.",},
		
	{ 17, 10, 25, 5,    "Wonder",                       NULL, "Creates a random (but often useful) chaotic effect. The more powerful you are, the more useful the effect is likely to be.",},
	{ 19, 12, 45, 9,    "Chaos Bolt",                   "dam: (10+((PLEV-5)/4))d8", "Fires a bolt of raw chaos.",},
	{ 21, 13, 45, 10,   "Sonic Boom",                   "dam: PLEV+45", "Fires a ball of sound.",},
	{ 23, 15, 50, 11,   "Doom Bolt",                    "dam: (11+((PLEV-5)/4))d8", "Fires a stream of raw magic.",},
	{ 25, 16, 50, 12,   "Fire Ball",                    "dam: PLEV+55; rad:2", "Fires a ball of flame, doing fire damage.",},
	{ 25, 18, 60, 8,    "Teleport Other",               NULL, "Teleports the targeted creature to somewhere random on the level.",},
	{ 30, 20, 80, 15,   "Word of Destruction",          "rad:15", "Causes the roof to collapse around you, much like the effects of an earthquake.",},
	{ 35, 40, 85, 40,   "Invoke Primal Chaos",          "dam: PLEV+66", "Fires a ball of raw chaos.",},
		
	{ 11, 7, 45, 9,     "Polymorph Other",              NULL, "Turns the target into a random creature.",},
	{ 15, 15, 80, 35,   "Chain Lightning",              "dam: (5+(PLEV/10))d8", "Fires bolts of lightning in eight directions, doing electricity damage.",},
	{ 16, 14, 80, 35,   "Arcane Binding",               NULL, "Recharges a wand, staff or rod.",},
	{ 25, 25, 85, 100,   "Disintegrate",                "dam: PLEV+80; rad: 3+PLEV/40", "Fires a powerful beam of disintegrating energy.",},
	{ 30, 25, 85, 150,  "Alter Reality",                NULL, "Completely alters reality, placing you in a different (but similar depth) dungeon level.",},
	{ 42, 50, 85, 250,  "Polymorph Self",               NULL, "Invokes chaos into your own body, warping it in a random way.",},
	{ 45, 90, 80, 250,  "Chaos Branding",               NULL, "Invokes chaos into your weapon, changing it.",},
	{ 47, 100, 90, 250, "Summon Demon",                 NULL, "Summons a friendly demon to help you.",},
		
	{ 20, 20, 66, 8,    "Beam of Gravity",              "dam: (9+((PLEV-5)/4))d8", "Fires a beam of elemental gravity.",},
	{ 35, 32, 85, 35,   "Meteor Swarm",                 "dam: (3*PLEV)/2;each: -1", "Makes a number of meteors appear and explode in your vicinity.",},
	{ 37, 34, 75, 40,   "Flame Strike",                 "dam: 150+2*PLEV", "Fires an intense ball of flame, doing massive amounts of fire damage.",},
	{ 41, 42, 85, 100,  "Call Primal Chaos",            "dam: 75*1d2", "Fires a random beam or ball of energy.",},
	{ 43, 44, 80, 150,  "Shard Ball",                   "dam: 120+PLEV; rad:2", "Fires a ball of sharp shards of metal and stone.",},
	{ 45, 48, 85, 200,  "Mana Storm",                   "dam: 300+PLEV*2; rad: 4", "Fires a ball of extreme magical energy.",},
	{ 47, 75, 80, 200,  "Breathe Primal Chaos",         "dam: CHP; rad: 2", "Makes you breathe a huge gout of raw chaos.",},
	{ 49, 100, 85, 250, "Call the Void",                "dam: 175; times: 3", "Invokes an extremely destructive series of explosions around yourself that might be very dangerous in an enclosed area.",},    
	},
	/*Death*/
	{
	{ 1, 1, 25, 4,      "Detect Unlife",                NULL,  "Reveals the location of any nearby undead or demonic creatures.",},
	{ 2, 2, 25, 4,      "Malediction",                  "dam:  (3 + ((PLEV-1)/5))d3",  "Fires a bolt of hell fire that may also scare, confuse or stun creatures.",},
	{ 2, 2, 25, 4,      "Detect Evil",                  NULL,  "Reveals the location of any nearby evil creatures.",},
	{ 3, 3, 27, 3,      "Stinking Cloud",               "dam: 	10 + (PLEV / 2)",  "Fires a ball of poison gas.",},
	{ 5, 5, 30, 4,      "Black Sleep",                  NULL,  "Sends a nearby monster to sleep.",},
	{ 7, 10, 75, 6,     "Resist Poison",                "dur: 1d20+20",  "Gives you a temporary resistance to poison.",},
	{ 9, 9, 30, 4,      "Horrify",                      "scare: PLEV",  "Scares and stuns a nearby monster.",},
	{ 10, 10, 30, 4,    "Enslave Undead",               "charm: PLEV",  "Makes an undead your friend.",},
		
	{ 12, 12, 40, 5,    "Orb of Entropy",               "dam: 3d6+ORB",  "Fires a life draining ball of energy.",},
	{ 13, 12, 30, 4,    "Nether Bolt",                  "dam: (6+((PLEV-5)/4))d8",  "Fires a bolt of nether.",},
	{ 18, 15, 50, 10,   "Terror",                       "scare: PLEV+30",  "Scares away all nearby monsters.",},
	{ 23, 20, 60, 16,   "Vampiric Drain",               "dam: PLEV + 1d(PLEV) * (PLEV/10+1)",  "Drains the life from a nearby creature, both healing you and sating your hunger.",},
	{ 30, 75, 50, 30,   "Poison Branding",              NULL,  "Permanently coats your weapon with poison.",},
	{ 33, 35, 60, 16,   "Dispel Good",                  "dam: PLEV*4",  "Banishes good creatures.",},
	{ 37, 25, 95, 25,   "Genocide",                     NULL,  "Destroys all creatures of a chosen type on a level. Unique creatures are teleported off the level, rather than destroyed. You get no experience for monsters killed with Genocide.",},
	{ 45, 50, 95, 150,  "Restore Life",                 NULL,  "Restores any drained experience you have.",},
		
	{ 10, 20, 80, 180,  "Berserk",                      "dur: 25+1d25; heal:30",  "Sends you berserk giving you 30 extra hit points, +24 to hit and -10 to armour class.",},
	{ 10, 15, 80, 30,   "Invoke Spirits",               NULL,  "Has a random (but usually beneficial) effect. The higher level you are, the more likely it is to be useful.",},
	{ 11, 11, 30, 15,   "Dark Bolt",                    "dam: (4+((PLEV-5)/4))d8",  "Fires a bolt of darkness.",},
	{ 30, 25, 75, 50,   "Battle Frenzy",                "dur: 1d25+25",  "Sends you berserk and also hastes you for a short while.",},
	{ 33, 35, 60, 125,  "Vampirism True",               "dam: 100; heal:100",  "Drains a large amount of life from a target and heals you by the amount drained.",},
	{ 33, 90, 70, 90,   "Vampiric Branding",            NULL,  "Permanently turns your weapon into a life-draining vampiric blade.",},
	{ 40, 40, 70, 200,  "Darkness Storm",               "dam:120; rad:4",  "Fires a ball of darkness.",},
	{ 40, 75, 80, 100,  "Mass Genocide",                NULL,  "Destroys all non-unique creatures on the level. You get no experience for monsters killed with Mass Genocide.",},
		
	{ 20, 20, 75, 50,   "Death Ray",                    "dam : PLEV",  "Fires a ray that will kill almost any living creature.",},
	{ 25, 66, 95, 250,  "Raise the Dead",               NULL,  "Creates undead servants to help you.",},
	{ 30, 40, 95, 250,  "Esoteria",                     NULL,  "Identifies (with varying accuracy) an item you are carrying.",},
	{ 33, 35, 70, 40,   "Word of Death",                "dam: PLEV*3",  "Kills or damages all nearby living creatures.",},
	{ 37, 35, 80, 70,   "Evocation",                    "dam: PLEV*4",  "Banishes, teleports away, and scares all nearby living creatures.",},
	{ 42, 120, 95, 250, "Hellfire",                     "dam: 666",  "Fires a huge ball of hell fire.",},
	{ 45, 100, 90, 250, "Omnicide",                     NULL,  "Destroys all non-unique creatures on the level, absorbing their essence as spell points. You get no experience for monsters killed with Omnicide.",},
	{ 47, 100, 90, 250, "Wraithform",                   "dur: 1d(PLEV/2)+1d(PLEV/2)",  "Temporarily makes you intangible so that you can walk through walls.",},
	},
	/*Tarot*/
	{
	{ 1, 1, 50, 3,      "Shift",						"range: 10",  "Teleports you to a random nearby location.",},
	{ 3, 3, 50, 4,      "The Challenge",				"dam: (3+((PLEV-1)/5))d3",  "Harms a nearby monster with psychic energy.",},
	{ 5, 5, 75, 8,      "Hopes & Fears",				"random: -1",  "Invokes a random (but usually beneficial) effect from a Tarot card.",},
	{ 6, 6, 80, 8,      "Restack",						NULL,  "Resets the depth that you will go to with Recall.",},
	{ 7, 7, 40, 4,      "Fool's Journey",				"range: PLEV*4",  "Teleports you to a random location on the level.",},
	{ 9, 9, 60, 6,      "Sleight of Hand",				"range: PLEV+2",  "Teleports you to a nearby location that you specify.",},
	{ 14, 12, 60, 6,    "The High Priestess",			"dur: 25+1d30",  "Gives you temporary ESP.",},
	{ 17, 15, 60, 5,    "The Chariot",					"range: PLEV",  "Teleports a creature to a random place on the level.",},
		
	{ 20, 20, 80, 8,    "The Wheel of Fortune",			"weight:  PLEV*15/10",  "Teleports a nearby object to your hand.",},
	{ 23, 5,  50, 5,    "Temperance",					"dur: 20+1d20",  "Gives you a temporary resistance to cold, fire and electricity.",},
	{ 28, 24, 60, 8,    "King of Swords",				NULL,  "Summons a demon to serve you.",},
	{ 30, 10, 80, 40,   "The Lover",					NULL,  "Makes a nearby human friendly.",},
	{ 33, 28, 80, 12,   "Elements of The Minchiate",	NULL,  "Summons a fire elemental to help you",},
	{ 35, 30, 70, 10,   "The Hermit",					NULL,  "Teleports you off the level you are on, onto either the one above or the one below.",},
	{ 40, 35, 80, 15,   "Search for the Self",			"delay: 15+1d21",  "Teleports you back to the town, or - if you are in the town - down to the deepest dungeon level that you have yet visited.",},
	{ 42, 40, 70, 12,   "Shuffle",						NULL,  "Teleports all nearby monsters to elsewhere on the level.",},
		
	{ 15, 15, 80, 20,   "Ink Blot",						NULL,  "Summons a bizarre creature to help you.",},
	{ 24, 24, 70, 25,   "The Star",						"heal: 150",  "Heals 150 damage and completely stops bleeding or stunning.",},
	{ 26, 26, 70, 30,   "Fortitude",					"dur: PLEV",  "Blesses and also hastes you for a short while.",},
	{ 30, 30,  70, 35,  "The Emperor",					"charm: PLEV*2",  "Makes a nearby monster friendly.",},
	{ 35, 70, 80, 100,  "Branding of the Minchiate",	NULL,  "Brands your weapon with fire, cold and electricity",},
	{ 40, 100, 90, 250, "Tarot Ascension",				NULL,  "Attunes you with the planes, altering your body.",},
	{ 42, 50, 50, 75,   "Death",						"dispel: PLEV*3",  "Banishes all nearby living creatures.",},
	{ 45, 100, 90, 200, "The Devil",					NULL,  "Summons devils to help you.",},
		
	{ 30, 30, 60, 50,   "Read The Lay",					NULL,  "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones).",},
	{ 35, 50, 90, 100,  "The Magician",					NULL,  "Fully identifies an item.",},
	{ 36, 80, 80, 150,  "Patter",						"dam: 200; rad:5",  "Confuse surrounding monsters.",},
	{ 39, 80, 80, 150,  "The Tower",					"dam: 200; rad:7",  "Surrounding monsters get hit with fire and lightning. Walls will crumble.",},
	{ 42, 100, 80, 200, "Lay of The Celtic Cross",		NULL,  "Summons multiple monsters to help you",},
	{ 47, 100, 80, 150, "Ten of Pentacles",				NULL,  "Summons Higher Demons to help you.",},
	{ 48, 100, 80, 200, "The Traitor",					NULL,  "Summons Fallen Angels.",},
	{ 49, 100, 80, 220, "Justice",						"dam : PLEV*3",  "Banishes all evil, undead, demons, devils and fallen angels.",},
	},
	/*Charms*/
	{
	{ 1, 1, 20, 4,      "Zap",							"dam: (3 + ((PLEV-1)/5))d3",  "Fires a bolt of electricity.",},
	{ 1, 1, 33, 5,      "Wizard Lock",					NULL,  "Locks a nearby door.",},
	{ 1, 1, 33, 4,      "Detect Invisibility",			NULL,  "Reveals the location of nearby invisible creatures.",},
	{ 2, 1, 33, 5,      "Detect Monsters",				NULL,  "Reveals the location of nearby creatures.",},
	{ 2, 2, 33, 5,      "Blink",						"range: 4",  "Teleports you to a random nearby location.",},
	{ 4, 4, 40, 6,      "Light Area",					"dam: 2d(PLEV/2)",  "Permanently lights the nearby area, possibly damaging creatures.",},
	{ 5, 5, 33, 7,      "Trap & Door Destruction",		NULL,  "Destroys any adjacent doors or traps.",},
	{ 6, 5, 44, 5,      "Cure Light Wounds",			"heal: 2d8",  "Heals 2d8 damage and reduces bleeding.",},
		
	{ 7, 6, 40, 7,      "Detect Doors & Traps",			NULL,  "Detects all nearby secret doors, traps and stairs.",},
	{ 8, 8, 60, 7,      "Phlogiston",					NULL,  "Provides extra fuel for a light source.",},
	{ 9, 8, 50, 6,      "Detect Treasure",				NULL,  "Detects nearby money and seams of treasure.",},
	{ 9, 9, 50, 6,      "Detect Enchantment",			NULL,  "Detects nearby magical items.",},
	{ 9, 9, 50, 6,      "Detect Objects",				NULL,  "Reveals the location of nearby objects.",},
	{ 11, 10, 50, 6,    "Cure Poison",					NULL,  "Removes all poison from your system.",},
	{ 12, 12, 50, 5,    "Resist Cold",					"dur: 1d20+20",  "Provides a temporary resistance to cold.",},
	{ 13, 12, 50, 5,    "Resist Fire",					"dur: 1d20+20",  "Provides a temporary resistance to fire.",},
		
	{ 14, 12, 50, 5,    "Resist Lightning",				"dur: 1d20+20",  "Provides a temporary resistance to electricity.",},
	{ 15, 12, 50, 5,    "Resist Acid",					"dur: 1d20+20",  "Provides a temporary resistance to acid.",},
	{ 16, 14, 33, 6,    "Cure Medium Wounds",			"heal: 4d8",  "Cures 4d8 damage and severely reduces bleeding.",},
	{ 18, 15, 50, 8,    "Teleport",						"range: PLEV*5",  "Teleports you to a random location on the level.",},
	{ 20, 16, 60, 9,    "Stone to Mud",					NULL,  "Turns a square of rock into loose mud, effectively removing it.",},
	{ 23, 18, 60, 9,    "Ray of Light",					"dam: 6d8",  "Creates a beam of permanent light, possibly damaging creatures.",},
	{ 25, 20, 70, 12,   "Satisfy Hunger",				NULL,  "Removes all hunger, leaving you comfortably full.",},
	{ 25, 20, 60, 13,   "See Invisible",				"dur: 1d24+24",  "Allows you to see invisible creatures for a short while.",},
		
	{ 28, 25, 70, 30,   "Recharging",					NULL,  "Recharges a wand, staff or rod.",},
	{ 35, 35, 80, 25,   "Teleport Level",				NULL,  "Teleports you off the level you are on, onto either the one above or the one below.",},
	{ 38, 30, 60, 25,   "Identify",						NULL,  "Identifies the basic abilities of an item.",},
	{ 40, 30, 70, 25,   "Teleport Away",				"range: PLEV",  "Teleports a creature to a random place on the level.",},
	{ 41, 30, 66, 30,   "Elemental Ball",				"dam: 75+PLEV;rad: 2",  "Fires a ball of a random element (fire, cold, acid of electricity).",},
	{ 42, 30, 80, 40,   "Detection",					NULL,  "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones).",},
	{ 45, 50, 70, 50,   "Word of Recall",				"delay: 15+1d21",  "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored.",},
	{ 49, 100, 80, 200, "Clairvoyance",					"dur: 1d30+25",  "Gives you temporary ESP.",},  
	},
	/*Somatic*/
	{                                            
	{ 1, 1, 23, 4,      "Cure Light Wounds",			"heal: 2d10",  "Heals 2d10 damage and reduces bleeding.",},
	{ 1, 2, 24, 4,      "Shift",						"range: 10",  "Teleports you to a random nearby location.",},
	{ 3, 3, 25, 1,      "Embrace Fear",					NULL,  "Stops you being afraid.",},
	{ 3, 3, 30, 1,      "Bat's Sense",					NULL,  "Reveals the layout of your nearby surroundings.",},
	{ 4, 4, 30, 1,      "Eagle's Vision",				NULL,  "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones).",},
	{ 5, 5, 35, 5,      "Mind Vision",					"dur: 1d30+25",  "Gives you temporary ESP.",},
	{ 6, 5, 30, 4,      "Cure Medium Wounds",			"heal: 4d10",  "Cures 4d10 damage and severely reduces bleeding.",},
	{ 7, 7, 75, 9,      "Satisfy Hunger",				NULL,  "Removes all hunger, leaving you comfortably full.",},
		
	{ 9, 7, 75, 8,      "Burn Resistance",				"dur: 1d20+20",  "Gives you temporary resistance to fire, acid and electricity.",},
	{ 10, 7, 75, 8,     "Detoxify",						NULL,  "Removes all poison from your system.",},
	{ 11, 7, 75, 7,     "Cure Critical Wounds",			"heal: 8d10",  "Heals 8d10 damage, and completely stops bleeding or stunning.",},
	{ 13, 7, 50, 6,     "See Invisible",				"dur: 1d24+24",  "Allows you to see invisible creatures for a short while.",},
	{ 18, 12, 60, 8,    "Teleport",						"range: PLEV*3",  "Teleports you to a random location on the level.",},
	{ 22, 12, 60, 8,    "Haste",						"dur: 1d(PLEV+20)+PLEV",  "Temporarily speeds up your movement.",},
	{ 28, 20, 70, 15,   "Healing",						"heal: 300",  "Heals 300 damage and completely stops bleeding and stunning.",},
	{ 33, 30, 75, 20,   "Resist True",					"dur: 1d25+25",  "Gives you a temporary resistance to acid, cold, fire, electricity and poison.",},
		
	{ 3, 3, 25, 15,     "Horrific Visage",				"scare: PLEV",  "Scares and stuns a nearby monster.",},
	{ 10, 10, 70, 40,   "See Magic",					NULL,  "Detects nearby magical items.",},
	{ 10, 10, 80, 40,   "Stone Skin",					"dur: 1d20+30",  "Adds 50 to your armour class for a short while.",},
	{ 12, 12, 80, 40,   "Move Body",					"range: PLEV+2",  "Teleports you to a nearby location that you specify.",},
	{ 14, 10, 60, 25,   "Corrupt Body",					NULL,  "Warps your body in a random manner.",},
	{ 20, 18, 85, 50,   "Know Self",					NULL,  "Reveals knowledge about yourself and your abilities (including those given by items).",},
	{ 20, 18, 60, 25,   "Teleport Level",				NULL,  "Teleports you off the level you are on, onto either the one above or the one below.",},
	{ 25, 25, 75, 19,   "Word of Recall",				"delay: 15+1d21",  "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored.",},
		
	{ 10, 10, 40, 20,   "Heroism",						"dur: 1d25+25; heal:10",  "Makes you feel heroic, giving you a +12 bonus to hit and 10 extra hit points.Also removes all fear.",},
	{ 25, 25, 75, 70,   "Wraithform",					"dur: 1d(PLEV/2)+1d(PLEV/2)",  "Temporarily makes you intangible so that you can walk through walls.",},
	{ 25, 30, 95, 160,  "Attune to Magic",				NULL,  "Fully identifies an item.",},
	{ 30, 40, 80, 120,  "Restore Body",					NULL,  "Heals damage to all six ability scores.",},
	{ 40, 80, 95, 200,  "Healing True",					"heal: 2000",  "Heals 2000 damage, all bleeding and all stunning.",},
	{ 40, 100, 95, 200, "Hypnotic Eyes",				"charm: PLEV",  "Makes a monster your friend.",},
	{ 42, 50, 90, 175,  "Restore Soul",					NULL,  "Restores any drained experience.",},
	{ 45, 70, 75, 250,  "Invulnerability",				"dur: 1d7+7",  "Makes you completely immune to damage for a short while.",},    
	},
	/*Demonic*/
	{
	{ 1,  3,  25, 4,    "Unholy Strength",				"dur: 1d25+25;dam: (PLEV/10)*5+5",  "+12 bonus to hit, 10 extra hit points, protection from fear at the cost of hitpoints",}, 
	{ 1,  1,  30, 4,    "Sense Evil",					NULL,  "Detects all evil creatures that are nearby.",},
	{ 2,  2,  20, 4,    "Scorch",						"dam: (3+(PLEV - 1) / 5)d4",  "Fires a bolt of flame that does fire damage.",},
	{ 4,  2,  30, 4,    "Perilous Shadows",				"dam: (3+(PLEV - 1) / 5)d4",  "Fires a bolt of darkness that does darkness damage.",},
	{ 12, 9,  35, 5,    "Teleport",						"range: 75",  "Teleports you to a random place on the level.",},
	{ 14, 9,  45, 6,    "Disintegrate",					"dam: (8+((PLEV-5)/4))d8",  "Fires a bolt of pure force, that can disintegrate what it touches.",},
	{ 25, 30, 95, 10,   "Demonic Sigil",				"dam: 7d7+PLEV/2",  "Leaves a rune on the floor that will explode when a creature walks over it.",},
	{ 25, 30, 95, 10,   "Hecate's Radiance",			"charm: (PLEV/10)+1",  "Lights up your surrounding, causing monster to be charmed, confused or affraid.",},
		
	{10, 20, 80, 15,    "Abaddon's Rage",				"dur: 1d25+25",  "+ 5 AC, +22 to hit, 10 extra hit points, protection from fear",},   
	{15,  1, 15, 15,    "Mind Leech",					NULL,  "Converts all your mana into physical health",},
	{15,  1, 15, 15,    "Blood Leech",					NULL,  "Converts your health into mana, potentially leaving you with 1 hit point",},
	{35, 70, 75, 5,     "Glyph of Warding",				NULL,  "Places a rune on the floor that many creatures will be unable to pass",},
	{23, 23, 50, 4,     "Protection from Evil",			"dur: 1d25+3*PLEV",  "Puts up a barrier around yourself that has a chance of stopping attacks from evil creatures.",},
	{37, 100, 80, 150,  "Summon Demons",				NULL,  "Summons Higher Demons to help you.",},
	{37, 100, 80, 150,  "Summon the Fallen",			NULL,  "Summons Fallen Angels to help you.",},
	{28, 20, 70, 15,    "Balm of the Cocytus",			"heal: 300; drain: -1; con: -1",  "Heals 300 damage and completely stops bleeding and stunning at the cost of your constitution.",},
		
	{ 20, 18, 60, 25,   "Araqiel's Wrath",				"rad: 8",  "Creates a large earthquake centered on your location, strong enough to collapse the roof. You are protected from the effects of this spell, safe in the epicenter.",},
	{ 42, 100, 80, 200, "Kokabiel's Call",				NULL,  "Summons multiple spirits to help you.",},                             
	{ 30, 40, 80, 120,  "Baraquiel's Guile",			NULL,  "Detects and pseudo-id's all magical items on the floor",},
	{ 30, 25, 75, 50,   "Sariel's Ire",					"dur: 1d50+25",  "+12 bonus to hit, 10 extra hit points, protection from fear while in a magical shell.",},               
	{ 33, 35, 60, 125,  "Azazel's Rule",				NULL,  "Charm all goats on the entire level",},
	{ 40, 40, 70, 200,  "Danel's Deluge",				"dam: 3d6+PLEV+PLEV/3",  "Radiate strong sunlight.",},
	{ 40, 75, 80, 100,  "Amaros' Grief",				"dam: PLEV*4",  "Banish all Demons, Devils and Fallen Angels",},
	{ 40, 40, 80, 120,  "Teachings of Kasyade",			NULL,  "Light up the entire level"},    
		
	{ 12, 12, 40, 5,    "Orb of Impending Doom",		"dam: 3d6+ORB+PLEV",  "Fires a damaging ball of hellfire.",},     
	{ 15, 5, 50, 5,     "Temperance",					"dur: 1d50+50",  "Resist fire and cold.",},
	{ 46, 70, 80, 150,  "True Warding",					NULL,  "Surrounds you with Glyphs of Warding.",},
	{ 30, 20, 80, 15,   "Word of Destruction",			"rad: 15",  "Causes the roof to collapse around you, much like the effects of an earthquake.",},
	{ 40, 100, 95, 200, "Gift of Malphas ",				NULL,  "Not done yet",},     
	{ 40, 100, 95, 200, "Kiss of Lillith",				"charm: PLEV*4",  "Quadriple charm attack on all monsters around you.",},
	{ 40, 100, 95, 200, "Behemoth's Call",				"rad: 16",  "Flood the dungeon with its items and monsters around you.",},
	{ 47, 75, 80, 200,  "Chaos Rift",					"dam: MHP;rad: 2",  "Creates a huge rift causing Primal Chaos damage."},
	}
};

/*
* Each chest has a certain set of traps, determined by pval
* Each chest has a "pval" from 1 to the chest level (max 55)
* If the "pval" is negative then the trap has been disarmed
* The "pval" of a chest determines the quality of its treasure
* Note that disarming a trap on a chest also removes the lock.
*/
byte chest_traps[64] =
{
0,                                      /* 0 == empty */
(CHEST_POISON),
(CHEST_LOSE_STR),
(CHEST_LOSE_CON),
(CHEST_LOSE_STR),
(CHEST_LOSE_CON),                       /* 5 == best small wooden */
0,
(CHEST_POISON),
(CHEST_POISON),
(CHEST_LOSE_STR),
(CHEST_LOSE_CON),
(CHEST_POISON),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_SUMMON),                 /* 15 == best large wooden */
0,
(CHEST_LOSE_STR),
(CHEST_LOSE_CON),
(CHEST_PARALYZE),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_SUMMON),
(CHEST_PARALYZE),
(CHEST_LOSE_STR),
(CHEST_LOSE_CON),
(CHEST_EXPLODE),                        /* 25 == best small iron */
0,
(CHEST_POISON | CHEST_LOSE_STR),
(CHEST_POISON | CHEST_LOSE_CON),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_PARALYZE),
(CHEST_POISON | CHEST_SUMMON),
(CHEST_SUMMON),
(CHEST_EXPLODE),
(CHEST_EXPLODE | CHEST_SUMMON), /* 35 == best large iron */
0,
(CHEST_SUMMON),
(CHEST_EXPLODE),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_POISON | CHEST_PARALYZE),
(CHEST_EXPLODE),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_POISON | CHEST_PARALYZE),        /* 45 == best small steel */
0,
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR),
(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_CON),
(CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_POISON | CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR | CHEST_LOSE_CON),
(CHEST_POISON | CHEST_PARALYZE),
(CHEST_POISON | CHEST_PARALYZE),        /* 55 == best large steel */
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
(CHEST_EXPLODE | CHEST_SUMMON),
};


/*
* Class titles for the player.
*
* The player gets a new title every five levels, so each class
* needs only ten titles total.
*/
cptr player_title[MAX_CLASS][PY_MAX_LEVEL/5] =
{
/* Warrior */		{"Rookie",		"Soldier",		"Mercenary",	"Veteran",		"Swordsman",	"Champion",		"Hero",			"Baron",			"Duke",				"Lord",},
/* Mage*/			{"Neophyte",	"Trickster",	"Green Mage",	"Spellbinder",	"High Mage",	"High Mage",	"Magister",		"Magus",			"Ipsissimus",		"Arch Mage",},
/* Priest */		{"Believer",	"Acolyte",		"Adept",		"Curate",		"Canon",		"Priest",		"High Priest",	"Cardinal",			"Inquisitor",		"Heir",},
/* Rogues */		{"Cutpurse",	"Robber",		"Burglar",		"Filcher",		"Sharper",		"Low Thief",	"High Thief",	"Master Thief",		"Assassin",			"Guildmaster",},
/* Rangers */		{"Runner",		"Strider",		"Scout",		"Courser",		"Tracker",		"Guide",		"Pathfinder",	"Low Ranger",		"High Ranger",		"Ranger Lord",},
/* Paladins */		{"Gallant",		"Keeper",		"Protector",	"Defender",		"Warder",		"Knight",		"Guardian",		"Low Paladin",		"High Paladin",		"Paladin Lord",},
/* Warrior-Mage */	{"Novice",		"Apprentice",	"Journeyman",	"Veteran",		"Enchanter",	"Champion",		"Mage-Hero",	"Baron Mage",		"Battlemage",		"Wizard Lord",},
/* Hell Knight */	{"Apprentice",	"Initiate",		"Hell's Slave",	"Hell's Guard",	"Hell Knight",	"Hell Knight",	"Hell Knight",	"Hell Knight",		"Master Knight",	"First Knight",},
/* Mystic */		{"Initiate",	"Brother",		"Disciple",		"Immaculate",	"Master",		"Soft Master",	"Hard Master",	"Flower Master",	"Dragon Master",	"Grand Master",},
/* Mindcrafter */	{"Trainee",		"Acolyte",		"Adept",		"Immaculate",	"Contemplator",	"Mentalist",	"Psychic",		"Psionicist",		"Esper",			"Mindmaster",},
/* High Mage*/		{"Neophyte",	"Trickster",	"Mage",			"Spellbinder",	"High Mage",	"High Mage",	"Magister",		"Magus",			"Ipsissimus",		"Arch Mage",},
/* Druid */			{"Neophyte",	"Initiate",		"Adept",		"Lesser Druid",	"Druid",		"Druid",		"Oak Druid",	"Great Druid",		"Grand Druid",		"Arch Druid",},
/* Warlock */		{"Apprentice",	"Initiate",		"Oath Breaker",	"Hell's Guard",	"Warlock",		"Warlock",		"Warlock",		"Warlock",			"Master Warlock",	"First Warlock",},
};

/*
* Hack -- the "basic" colour names (see "TERM_xxx")
*/
cptr colour_names[16] =
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
"STR: ", "INT: ", "WIS: ", "DEX: ", "CON: ", "CHA: "
};

/*Yes, this is braindead, one day I will fix it*/
cptr stats_short[6] =
{
	"STR", "INT", "WIS", "DEX", "CON", "CHA"
};

/*
* Abbreviations of damaged stats
*/
cptr stat_names_reduced[6] =
{
"str: ", "int: ", "wis: ", "dex: ", "con: ", "cha: "
};


/*
* Certain "screens" always use the main screen, including News, Birth,
* Dungeon, Tomb-stone, High-scores, Macros, Colors, Visuals, Options.
*
* Later, special flags may allow sub-windows to "steal" stuff from the
* main window, including File dump (help), File dump (artefacts, uniques),
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
"Display visible monsters",
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
*/
option_type option_info[] =
{
/*** User-Interface ***/

{ &rogue_like_commands, FALSE,	1,      0, 0,			"rogue_like_commands",          "Rogue-like commands" },
{ &quick_messages,		TRUE,	1,      0, 1,			"quick_messages",               "Activate quick messages" },
{ &other_query_flag,    FALSE,  1,      0, 2,			"other_query_flag",             "Prompt for item confirmation" },
{ &carry_query_flag,    FALSE,  4,      0, 3,			"carry_query_flag",             "Prompt before picking things up" },
{ &use_old_target,      FALSE,  3,      0, 4,			"use_old_target",               "Use old target by default" },
{ &always_pickup,       TRUE,   4,      0, 5,			"always_pickup",                "Pick things up by default" },
{ &always_repeat,       TRUE,   1,      0, 6,			"always_repeat",                "Repeat obvious commands" },
{ &depth_in_feet,       FALSE,  1,      0, 7,			"depth_in_feet",                "Show dungeon level in feet" },
{ &stack_force_notes,   TRUE,	4,      0, 8,			"stack_force_notes",			"Merge inscriptions when stacking" },
{ &stack_force_costs,   TRUE,	4,      0, 9,			"stack_force_costs",			"Merge discounts when stacking" },
{ &show_labels,         TRUE,   4,      0, 10,			"show_labels",                  "Show labels in object listings" },
{ &show_weights,        TRUE,	4,      0, 11,			"show_weights",                 "Show weights in object listings" },
{ &show_choices,        TRUE,	1,      0, 12,			"show_choices",                 "Show choices in certain sub-windows" },
{ &show_details,        TRUE,	1,      0, 13,			"show_details",                 "Show description in monster details" },
{ &ring_bell,           FALSE,  1,      0, 14,			"ring_bell",                    "Audible bell (on errors, etc)" },
{ &use_colour,          TRUE,   1,      0, 15,			"use_colour",                   "Use colour if possible" },

/*** Disturbance ***/

{ &find_ignore_stairs,  FALSE,	2,      0, 16,			"find_ignore_stairs",			"Run past stairs" },
{ &find_ignore_doors,   TRUE,   2,      0, 17,			"find_ignore_doors",			"Run through open doors" },
{ &find_cut,            TRUE,   2,      0, 18,			"find_cut",                     "Run past known corners" },
{ &find_examine,        TRUE,   2,      0, 19,			"find_examine",                 "Run into potential corners" },
{ &disturb_move,        FALSE,  2,      0, 20,			"disturb_move",                 "Disturb whenever any monster moves" },
{ &disturb_near,		TRUE,   2,      0, 21,			"disturb_near",                 "Disturb whenever viewable monster moves" },
{ &disturb_panel,		TRUE,   2,      0, 22,			"disturb_panel",                "Disturb whenever map panel changes" },
{ &disturb_state,		TRUE,   2,      0, 23,			"disturb_state",                "Disturb whenever player state changes" },
{ &disturb_minor,		FALSE,  2,      0, 24,			"disturb_minor",                "Disturb whenever boring things happen" },
{ &disturb_other,		TRUE,   2,      0, 25,			"disturb_other",                "Disturb whenever random things happen" },
{ &alert_hitpoint,		TRUE,	2,      0, 26,			"alert_hitpoint",               "Alert user to critical hitpoints" },
{ &alert_failure,		FALSE,  2,      0, 27,			"alert_failure",                "Alert user to various failures" },
{ &small_levels,		TRUE,   6,      0, 28,			"small_levels",					"Allow unusually small dungeon levels" },
{ &empty_levels,		TRUE,   6,      0, 29,			"empty_levels",					"Allow empty 'arena' levels" },

/*** Game-Play ***/

{ &auto_haggle,			TRUE,  6,      1, 0,			"auto_haggle",                  "Auto-haggle in stores" },
{ &auto_scum,			TRUE,  6,      1, 1,			"auto_scum",                    "Auto-scum for good levels" },
{ &stack_allow_items,   TRUE,  4,      1, 2,			"stack_allow_items",			"Allow weapons and armour to stack" },
{ &stack_allow_wands,   TRUE,  4,      1, 3,			"stack_allow_wands",			"Allow wands/staffs/rods to stack" },
{ &expand_look,			TRUE,  6,      1, 4,			"expand_look",                  "Expand the power of the look command" },
{ &expand_list,			TRUE,  6,      1, 5,			"expand_list",                  "Expand the power of the list commands" },
{ &view_perma_grids,    TRUE,  6,      1, 6,			"view_perma_grids",             "Map remembers all perma-lit grids" },
{ &view_torch_grids,    TRUE,  6,      1, 7,			"view_torch_grids",             "Map remembers all torch-lit grids" },
{ &dungeon_align,		TRUE,  6,      1, 8,			"dungeon_align",                "Generate dungeons with aligned rooms" },
{ &dungeon_stair,		TRUE,  6,      1, 9,			"dungeon_stair",                "Generate dungeons with connected stairs" },
{ &dungeon_small,		FALSE, 6,      1, 10,			"dungeon_small",                "Always generate small dungeons" },
{ &flow_by_sound,		FALSE, 3,      1, 11,			"flow_by_sound",                "Monsters chase current location" },
{ &flow_by_smell,		TRUE,  3,      1, 12,			"flow_by_smell",                "Monsters chase recent locations" },
{ &player_symbols,		TRUE,  1,      1, 13,			"player_symbols",				"Use special symbols for the player char"},
{ &equippy_chars,		TRUE,  4,      1, 14,			"equippy_chars",				"Display 'equippy' chars" },
{ &smart_learn,			TRUE,  3,      1, 15,			"smart_learn",                  "Monsters learn from their mistakes" },
{ &smart_cheat,			FALSE, 3,      1, 16,			"smart_cheat",                  "Monsters exploit players weaknesses" },
{ &easy_open,			TRUE,  6,      1, 17,			"easy_open",                    "Open and close automatically" },
{ &easy_disarm,			TRUE,  6,      1, 18,			"easy_disarm",                  "Disarm traps automatically" },

/*** Efficiency ***/

{ &view_reduce_lite,    FALSE,  5,      1, 19,			"view_reduce_lite",             "Reduce lite-radius when running" },
{ &view_reduce_view,    FALSE,  5,      1, 20,			"view_reduce_view",             "Reduce view-radius in town" },
{ &avoid_abort,			FALSE,  5,      1, 21,			"avoid_abort",                  "Avoid checking for user abort" },
{ &avoid_other,			FALSE,  5,      1, 22,			"avoid_other",                  "Avoid processing special colours" },
{ &flush_failure,		TRUE,   5,      1, 23,			"flush_failure",                "Flush input on various failures" },
{ &flush_disturb,		FALSE,  5,      1, 24,			"flush_disturb",                "Flush input whenever disturbed" },
{ &flush_command,		FALSE,  5,      1, 25,			"flush_command",                "Flush input before every command" },
{ &fresh_before,		TRUE,   5,      1, 26,			"fresh_before",                 "Refresh screen before every command" },
{ &fresh_after,			FALSE,  5,      1, 27,			"fresh_after",                  "Refresh screen after every command" },
{ &fresh_message,		FALSE,  5,      1, 28,			"fresh_message",                "Refresh screen after every message" },
{ &compress_savefile,   TRUE,   5,      1, 29,			"compress_savefile",			"Compress messages in savefiles" },
{ &hilite_player,		FALSE,	1,      1, 30,			"hilite_player",                "Hilite the player with the cursor" },
{ &view_yellow_lite,    TRUE,	5,		1, 31,			"view_yellow_lite",             "Use special colours for torch-lit grids" },
{ &view_bright_lite,    TRUE,   5,      2, 1,			"view_bright_lite",             "Use special colours for 'viewable' grids" },
{ &view_granite_lite,   TRUE,   5,      2, 2,			"view_granite_lite",			"Use special colours for wall grids" },
{ &view_special_lite,   TRUE,   5,		2, 3,			"view_special_lite",			"Use special colours for floor grids" },
{ &skip_corruptions,	FALSE,  6,		2, 4,			"skip_corruptions",				"Skip corruptions in 'C'haracter Display" },
{ &plain_descriptions,	FALSE,  4,		2, 5,			"plain_descriptions",			"Plain object descriptions" },
{ &stupid_monsters,		FALSE,  3,		2, 6,			"stupid_monsters",				"Monsters behave stupidly" },
{ &auto_destroy,		TRUE,   4,		2, 7,			"auto_destroy",					"No query to destroy known worthless items" },
{ &wear_confirm,        TRUE,   4,		2, 8,			"confirm_wear",					"Confirm to wear/wield known cursed items" },
{ &confirm_stairs,      FALSE,  1,		2, 9,			"confirm_stairs",				"Prompt before exiting a dungeon level" },
{ &disturb_allies,		FALSE,  2,		2, 10,			"disturb_allies",				"Disturb when visible allies move" },
{ &multi_stair,			FALSE,  6,		2, 11,			"multi_stair",					"Stairs can be longer than one level" },
{ &rand_unbiased,		TRUE,   5,		2,12,			"rand_unbiased",				"Random numbers have bias removed"},
{ &unify_commands,		FALSE,  4,		2,13,			"unify_commands",				"Use a single 'u'se command for all objects"},
{ &testing_stack,		TRUE,   4,		2, 14,			"testing_stack",                "Allow objects to stack on floor" },
{ &monsters_carry,		TRUE,   3,		2, 15,			"monsters_carry",               "Allow monsters to carry objects" },
{ &centre_view,			TRUE,   1,		2, 16,			"centre_view",					"Centre view around player" },
{ &no_centre_run,		FALSE,  5,		2, 17,			"no_centre_run",                "Do not centre view whilst running" },
{ &maximise_mode,		TRUE,   7,		2, 18,			"maximise_mode",                "Include race/class bonuses in stat calcs" },
{ &preserve_mode,		TRUE,   7,		2, 19,			"preserve_mode",                "Artifacts are not lost if you never saw them" },
{ &use_autoroller,		FALSE,  7,		2, 20,			"use_autoroller",               "Stats are rolled repeatedly with minima" },
{ &spend_points,		TRUE,   7,		2, 21,			"spend_points",					"Stats are not rolled, points are spent on them" },
{ &ironman_shop,		FALSE,  7,		2, 22,			"ironman_shop",					"Shops (except for libraries) are locked" },
{ &apply_k_storebought,	FALSE,  8,		2, 23,			"inscribe_shop",				"Apply {!k} on storebought items" },	
{ &apply_k_discover,	FALSE,  8,		2, 24,			"inscribe_id",					"Apply {!k} on discovery of new item types" },	
{ &sanity_store,		TRUE,   9,		2, 25,			"sanity_store",					"Keep storebought items" },	
{ &sanity_speed,		TRUE,   9,		2, 26,			"sanity_speed",					"Keep items giving speed bonuses" },	
{ &sanity_immune,		TRUE,   9,		2, 27,			"sanity_immune",				"Keep items giving immunity" },	
{ &sanity_telepathy,	TRUE,   9,		2, 28,			"sanity_telepathy",				"Keep items giving telepathy" },	
{ &sanity_high_resist,	TRUE,   9,		2, 29,			"sanity_high_resists",			"Keep items giving high resists" },	
{ &sanity_stat,			TRUE,   9,		2, 30,			"sanity_stat",					"Keep items boosting stats" },	
{ &sanity_verbose,		TRUE,   9,		2, 31,			"sanity_verbose",				"Inform when sanity checks are used" },	
{ &sanity_id,			TRUE,   9,		3, 1,			"sanity_id",					"Keep unknown consumables" },			
{ &sanity_realm,		TRUE,   9,		3, 0,			"sanity_realm",					"Keep books from own realm" },		
{ &sanity_price,		TRUE,   9,		3, 1,			"sanity_price",					"Keep items worth more than x gold" },	
{ &reverse_xp,          TRUE,   1,		3, 2,			"reverse_xp",					"Show xp untill next level" },		
	

/*** End of Table ***/

{ NULL,                 0, 0, 0, 0,	NULL,                   NULL }
};


cptr evil_patron_shorts[MAX_PATRON] =
{
"Abaddon",
"Asmodeus",
"Astoreth",
"Baal",
"Balaam",

"Belial",
"Belphegor",
"Bifrons",
"Dispater",
"Hauras",

"Kobal",
"Mephistopheles",
"Nergal",
"Pazzuzzu",
"Sargatanas",

"Lucifer"
};

cptr evil_patron_longs[MAX_PATRON] =
{
"Abaddon, Leader of Hell's Army",
"Asmodeus, Prince of Lust",
"Astoreth, Treasurer of Hell",
"Baal, Duke of Sloth",
"Balaam the Prophet",

"Belial, Lord of Darkness",
"Belphegor the Temptress",
"Bifrons, Prince of Sorcery",
"Dispater of the Underworld",
"Hauras, Lord of Fire",

"Kobal, the Dark Entertainer",
"Mephistopheles the Dealmaker",
"Nergal the Inquisitor",
"Pazzuzzu, Lord of the Air",
"Sargatanas, Brigadeer of Infernal Spirits",

"Lucifer Morningstar"
};

int evil_stats[MAX_PATRON] =
{
A_CON,  /* Abaddon */
A_CON,  /* Asmodeus */
A_STR,  /* Astoreth */
A_STR,  /* Baal */
A_STR,  /* Balaam */

A_INT,  /* Belial */
A_STR,  /* Belphegor */
A_INT,  /* Bifrons */
A_CON,  /* Dispater */
A_CHA,  /* Hauras */

-1,     /* Kobal */
A_STR,  /* Mephistopheles */
A_CHA,  /* Nergal */
A_CON,  /* Pazzuzzu */
A_INT,  /* Sargatanas */

A_STR,  /* Lucifer */
};




int chaos_rewards[MAX_PATRON][20] =
{

/* Thed, Mother of Devilspawn: */
{
REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL,
REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF,
REW_POLY_SLF, REW_POLY_SLF, REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP,
REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL, REW_AUGM_ABL
},

/* Ragnaglar the Unclean: */
{
REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_H_SUMMON, REW_SUMMON_M,
REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_WND,
REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_ABL, REW_SER_UNDE,
REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GOOD_OBS
},

/* Cacodemon, Spawn of the Devil: */
{
REW_WRATH, REW_WRATH, REW_HURT_LOT, REW_PISS_OFF, REW_H_SUMMON,
REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_DESTRUCT, REW_SER_UNDE,
REW_GENOCIDE, REW_MASS_GEN, REW_MASS_GEN, REW_DISPEL_C, REW_GOOD_OBJ,
REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL
},

/* Malia, Mistress of Disease: */
{
REW_WRATH, REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL,
REW_IGNORE, REW_IGNORE, REW_SER_UNDE, REW_DESTRUCT, REW_GENOCIDE,
REW_MASS_GEN, REW_MASS_GEN, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL,
REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL
},

/* Pochnargo the Mutator: */
{
REW_TY_CURSE, REW_TY_CURSE, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL,
REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND, REW_POLY_WND,
REW_GENOCIDE, REW_DISPEL_C, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS,
REW_GAIN_ABL, REW_CHAOS_WP, REW_GAIN_EXP, REW_AUGM_ABL, REW_GOOD_OBS
},


/* Thanatar the Assassin: */
{
REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_H_SUMMON, REW_H_SUMMON,
REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF,
REW_POLY_SLF, REW_SER_DEMO, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL,
REW_CHAOS_WP, REW_DO_HAVOC, REW_GOOD_OBJ, REW_GREA_OBJ, REW_GREA_OBS
},

/* Gbaji the Deceiver: */
{
REW_TY_CURSE, REW_HURT_LOT, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL,
REW_SUMMON_M, REW_LOSE_EXP, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND,
REW_SER_UNDE, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_GAIN_EXP,
REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GREA_OBS, REW_AUGM_ABL
},


/* Vivamort the Vampire Lord: */
{
REW_WRATH, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_EXP, REW_H_SUMMON,
REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF,
REW_POLY_SLF, REW_MASS_GEN, REW_SER_DEMO, REW_HEAL_FUL, REW_CHAOS_WP,
REW_CHAOS_WP, REW_GOOD_OBJ, REW_GAIN_EXP, REW_GREA_OBJ, REW_AUGM_ABL
},

/* Krarsht, the Hungry One: */
{
REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_CURSE_WP, REW_RUIN_ABL,
REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND,
REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS, REW_HEAL_FUL, REW_GAIN_EXP,
REW_GAIN_ABL, REW_CHAOS_WP, REW_GOOD_OBS, REW_GREA_OBJ, REW_AUGM_ABL
},

/* Wakboth, the Devil: */
{
REW_WRATH, REW_CURSE_AR, REW_CURSE_WP, REW_CURSE_WP, REW_CURSE_AR,
REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF,
REW_POLY_WND, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_AUGM_ABL,
REW_GOOD_OBJ, REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_GREA_OBS
},

/* Bagog, the Scorpion Queen: */
{
REW_WRATH, REW_SER_DEMO, REW_CURSE_WP, REW_CURSE_AR, REW_LOSE_EXP,
REW_GAIN_ABL, REW_LOSE_ABL, REW_POLY_WND, REW_POLY_SLF, REW_IGNORE,
REW_DESTRUCT, REW_MASS_GEN, REW_CHAOS_WP, REW_GREA_OBJ, REW_HURT_LOT,
REW_AUGM_ABL, REW_RUIN_ABL, REW_H_SUMMON, REW_GREA_OBS, REW_AUGM_ABL
},

/* Gark the Calm: */
{
REW_WRATH, REW_HURT_LOT, REW_HURT_LOT, REW_H_SUMMON, REW_H_SUMMON,
REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_SER_MONS, REW_SER_DEMO,
REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GOOD_OBJ,
REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_GREA_OBJ, REW_GREA_OBS
},

/* Ikadz, Lord of Torture: */
{
REW_WRATH, REW_PISS_OFF, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL,
REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_SER_DEMO,
REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_EXP,
REW_GAIN_EXP, REW_CHAOS_WP, REW_GAIN_ABL, REW_GREA_OBJ, REW_AUGM_ABL
},

/* Kajabor: */
{
REW_WRATH, REW_PISS_OFF, REW_HURT_LOT, REW_RUIN_ABL, REW_LOSE_ABL,
REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF,
REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_ABL,
REW_GAIN_ABL, REW_SER_UNDE, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL
},

/* Krjalk, the Traitor: */
{
REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL,
REW_LOSE_EXP, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_SLF,
REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_CHAOS_WP, REW_GREA_OBJ,
REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP, REW_GAIN_EXP, REW_AUGM_ABL
},

/* Ompalam, Lord of Slavery: */
{
REW_WRATH, REW_HURT_LOT, REW_PISS_OFF, REW_LOSE_ABL, REW_LOSE_EXP,
REW_IGNORE,   REW_IGNORE,   REW_DISPEL_C, REW_DO_HAVOC, REW_DO_HAVOC,
REW_POLY_SLF, REW_POLY_SLF, REW_GAIN_EXP, REW_GAIN_ABL, REW_GAIN_ABL,
REW_SER_MONS, REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_GOOD_OBS
}
};

martial_arts ma_blows[MAX_MA] =
{
#ifdef VERBOSE_MARTIAL_ARTS
{ "You punch %s.",                          1, 0, 1, 4, 0 },
{ "You kick %s.",                           2, 0, 1, 6, 0 },
{ "You strike %s.",                         3, 0, 1, 7, 0 },
{ "You hit %s with your knee.",             5, 5, 2, 3, MA_KNEE },
{ "You hit %s with your elbow.",            7, 5, 1, 8, 0 },
{ "You butt %s.",                           9, 10, 2, 5, 0 },
{ "You kick %s.",                           11, 10, 3, 4, MA_SLOW },
{ "You uppercut %s.",                       13, 12, 4, 4, 6 },
{ "You double-kick %s.",                    16, 15, 5, 4, 8 },
{ "You hit %s with a Cat's Claw.",          20, 20, 5, 5, 0 },
{ "You hit %s with a jump kick.",           25, 25, 5, 6, 10 },
{ "You hit %s with an Eagle's Claw.",       29, 25, 6, 6, 0 },
{ "You hit %s with a circle kick.",         33, 30, 6, 8, 10 },
{ "You hit %s with an Iron Fist.",          37, 35, 8, 8, 10 },
{ "You hit %s with a flying kick.",         41, 35, 8, 10, 12 },
{ "You hit %s with a Dragon Fist.",       45, 35, 10, 10, 16 },
{ "You hit %s with a Crushing Blow.",         48, 35, 10, 12, 18 },
#else
{ "You punch %s.",                          1, 0, 1, 4, 0 },
{ "You kick %s.",                           2, 0, 1, 6, 0 },
{ "You strike %s.",                         3, 0, 1, 7, 0 },
{ "You knee %s.",             5, 5, 2, 3, MA_KNEE },
{ "You hit %s.",            7, 5, 1, 8, 0 },
{ "You butt %s.",                           9, 10, 2, 5, 0 },
{ "You kick %s.",                           11, 10, 3, 4, MA_SLOW },
{ "You uppercut %s.",                       13, 12, 4, 4, 6 },
{ "You double-kick %s.",                    16, 15, 5, 4, 8 },
{ "You hit %s.",          20, 20, 5, 5, 0 },
{ "You kick %s.",           25, 25, 5, 6, 10 },
{ "You hit %s.",       29, 25, 6, 6, 0 },
{ "You kick %s.",         33, 30, 6, 8, 10 },
{ "You punch %s.",          37, 35, 8, 8, 10 },
{ "You kick %s.",         41, 35, 8, 10, 12 },
{ "You punch %s.",       45, 35, 10, 10, 16 },
{ "You punch %s.",       48, 35, 10, 12, 18 },
#endif
};


mindcraft_power mindcraft_powers[MAX_MINDCRAFT_POWERS] = {
/* Level gained,  cost,  %fail,  name */
{ 1,   1,  15, "Precognition" },       /* Det. monsters/traps */
{ 2,   1,  20, "Neural Blast" },     /* ~MM */
{ 3,   2,  25, "Minor Displacement" }, /* Phase/dimension door */
{ 7,   6,  35, "Major Displacement" }, /* Tele. Self / All */
{ 9,   7,  50, "Domination" },
{ 11,  7,  30, "Pulverise" },      /* Telekinetic "bolt" */
{ 13, 12,  50, "Character Armour" },   /* Psychic/physical defenses */
{ 15, 12,  60, "Psychometry" },
{ 18, 10,  45, "Mind Wave" },          /* Ball -> LOS */
{ 23, 15,  50, "Adrenaline Channeling" },
{ 25, 10,  40, "Psychic Drain" },      /* Convert enemy HP to mana */
{ 28, 20,  45, "Telekinetic Wave" },   /* Ball -> LOS */
};


/* Class Sub-Names based on realm */
cptr class_sub_name[MAX_CLASS][MAX_REALM+1] =
{

/* Warrior Default = No Magic*/
/*	Default			Miracles		Sorcery		Nature		Chaos			Death			Tarot			Charms			Somatic        Demonic*/
{	"Warrior",		"-",			"-",		"-",		"-",			"-",			"-",			"-",			"-",            "-" },
{	"Mage",			"Mage",			"Mage",		"Mage",		"Mage",			"Mage",			"Mage",			"Mage",			"Mage",			"Mage"},
{	"Priest",		"Priest",		"-",		"-",		"-",			"Pagan",		"-",			"-",			"-",			"-"	},
{	"Rogue",		"-",			"Burglar",	"-",		"-",			"Assassin",		"Card Sharp",	"Thief",		"-",			"-"	},
{	"Ranger",		"-",			"-",		"Ranger",	"-",			"-",			"-",			"-",			"-",			"-"	},
{	"Paladin",		"Paladin",		"-",		"-",		"-",			"Death Knight", "-",			"-",			"-",			"-"	},
{	"Warrior-Mage", "-",			"-",		"-",		"-",			"-",			"-",			"Warrior-Mage", "-",			"-"	},
{	"Hell Knight",	"-",			"-",		"-",		"-",            "-",			"-",			"-",			"-",			"Hell Knight"	},
{	"Mystic",		"-",			"-",		"-",		"-",			"-",			"-",			"-",			"Mystic",		"-"},
{	"Mindcrafter",	"-",			"-",		"-",		"-",			"-",			"-",			"-",			"-",			"-"         },
{	"High-Mage",	"Vivimancer",	"Sorceror", "Naturist", "Warlock",		"Necromancer",	"Seer",			"Hedge Wizard",	"Zen Master",	"Warlock"   },
{	"Druid",		"-",			"-",		"Druid",	"-",			"-",			"-",			"-",			"-",			"-"	},
{   "Warlock",		"-",			"-",		"-",		"-",            "-",            "-",			"-",			"-",			"Warlock"	},
};

timed_type timed[] = 
{/*	  s16b *timer;			  cptr status					cptr gain;									cptr lose;									u32b redraw;		u32b update;*/
	{ &(p_body.fast),	      "hastened",					"You feel time slowing down!",				"You regain normal speed",					0,					PU_BONUS, },
	{ &(p_body.slow),	      "slowed",						"You feel yourself moving slower!",			"You feel yourself speed up.",				0,					PU_BONUS, },
	{ &(p_body.blind),	      "blind",						"You are blind!",							"You can see again.",						PR_MAP|PR_BLIND,	PU_UN_VIEW|PU_UN_LITE|PU_VIEW|PU_LITE|PU_MONSTERS, }, /*PW_OVERHEAD*/
	{ &(p_body.paralyzed),	  "paralyzed",					"You are paralyzed!",						"You can move again.",						PR_STATE,			0, }, 
	{ &(p_body.confused),	  "confused",					"You are confused!",						"You feel less confused now.",				PR_CONFUSED,		0, }, 
	{ &(p_body.afraid),	      "terrified",					"You are terrified!",						"You feel bolder now.",						PR_AFRAID,			0, },
	{ &(p_body.image),	      "hallucinating",				"Oh, wow! Everything looks so cosmic now!", "You can see clearly again.",				PR_MAP,				PU_MONSTERS, }, /*PW_OVERHEAD*/
	{ &(p_body.poisoned),	  "poisoned",					"You are poisoned!",                        "You are no longer poisoned.",				PR_POISONED,		0, },
	{ &(p_body.cut),	      "cut",						"You are cut!",								"You are no longer bleeding.",				PR_CUT,				0, }, /*Call to custom code */
	{ &(p_body.stun),	      "stunned",					"You are stunned!",							"You are no longer stunned.",				PR_STUN,			PU_BONUS, }, /*Call to custom code */
	{ &(p_body.protevil),	  "protected from evil",		"You feel safe from evil!",                 "You no longer feel safe from evil.",		0,					0, }, 
	{ &(p_body.invuln),	      "invulnerable",				"Invulnerability!",                         "The invulnerability wears off.",			PR_MAP,				PU_BONUS|PU_MONSTERS, }, /*PW_OVERHEAD*/
	{ &(p_body.hero),	      "heroic",						"You feel like a hero!",                    "The heroism wears off.",					0,					PU_BONUS|PU_HP, },
	{ &(p_body.shero),	      "raging",						"You feel the surge of a cold rage!",       "Your rage wears down.",					0,					PU_BONUS|PU_HP, },
	{ &(p_body.shield),		  "shielded",					"Your are shielded.",						"Your shield dissipates.",					0,					PU_BONUS, },
	{ &(p_body.blessed),	  "blessed",					"You feel righteous!",                      "The prayer has expired.",					0,					PU_BONUS, },
	{ &(p_body.tim_invis),	  "seeing invisible",			"Your eyes feel very sensitive!",           "Your eyes feel less sensitive.",			0,					PU_BONUS|PU_MONSTERS, },
	{ &(p_body.tim_infra),	  "seeing infra-red",			"Your eyes begin to tingle!",               "Your eyes stop tingling.",					0,					PU_BONUS|PU_MONSTERS, },
	{ &(p_body.magic_shell),  "anti-magical",				"You feel shielded from magic.",            "You no longer feel shielded from magic.",  0,					PU_BONUS, },
	{ &(p_body.oppose_acid),  "resistant to acid",			"You feel resistant to acid!",              "You feel less resistant to acid.",         0,					0, },
	{ &(p_body.oppose_elec),  "resistant to electricity",	"You feel resistant to electricity!",       "You feel less resistant to electricity.",  0,					0, },
	{ &(p_body.oppose_fire),  "resistant to fire",			"You feel resistant to fire!",              "You feel less resistant to fire.",         0,					0, },
	{ &(p_body.oppose_cold),  "resistant to cold",			"You feel resistant to cold!",              "You feel less resistant to cold.",         0,					0, },
	{ &(p_body.oppose_pois),  "resistant to poison",		"You feel resistant to poison!",            "You feel less resistant to poison.",       0,					0, },
	{ &(p_body.tim_esp),      "psychic",					"You feel your consciousness expand!",      "Your consciousness contracts again.",      0,					PU_BONUS|PU_MONSTERS, },
	{ &(p_body.wraith_form),  "incorporeal",				"You become a ghastly wraith-being!",       "You are no longer a wraith.",              PR_MAP,				PU_BONUS|PU_MONSTERS, },   /*PW_OVERHEAD*/
};

menu_type menu_info[10][10] =
{
	{
    {"Magic/Special", '1', FALSE},
    {"Action", '2', FALSE},
    {"Items(use)", '3', FALSE},
    {"Items(other)", '4', FALSE},
    {"Equip", '5', FALSE},
    {"Door/Box", '6', FALSE},
    {"Informations", '7', FALSE},
    {"Options", '8', FALSE},
    {"Other commands", '9', FALSE},
    {"Documentation (?)", '?', TRUE},
	},
    
	{
    {"Use", 'm', TRUE},
    {"See tips", 'b', TRUE},
    {"Study", 'G', TRUE},
    {"Special abilities", 'U', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
    
	{
    {"Rest", 'R', TRUE},
    {"Disarm a trap", 'D', TRUE},
    {"Search", 's', TRUE},
    {"Look", 'l', TRUE},
    {"Target", '*', TRUE},
    {"Dig", 'T', TRUE},
    {"Go up stairs", '<', TRUE},
    {"Go down stairs", '>', TRUE},
    {"Search mode ON/OFF", 'S', TRUE},
    {"", 0, FALSE},        
	},
    
	{
    {"Read a scroll", 'r', TRUE},
    {"Drink a potion", 'q', TRUE},
    {"Use a staff", 'u', TRUE},
    {"Aim a wand", 'a', TRUE},
    {"Zap a rod", 'z', TRUE},
    {"Activate an equipment", 'A', TRUE},
    {"Eat", 'E', TRUE},
    {"Fire missile weapon", 'f', TRUE},
    {"Throw an item", 'v', TRUE},
    {"", 0, FALSE}
	},
    
	{
    {"Get items", 'g', TRUE},
    {"Drop an item", 'd', TRUE},
    {"Destroy an item", 'k', TRUE},
    {"Inscribe an item", '{', TRUE},
    {"Uninscribe an item", '}', TRUE},
    {"Info about an item", 'I', TRUE},
    {"Inventory list", 'i', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
    
	{
    {"Wear", 'w', TRUE},
    {"Take off", 't', TRUE},
    {"Refuel", 'F', TRUE},
    {"Equipment list", 'e', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
    
	{
    {"Open", 'o', TRUE},
    {"Close", 'c', TRUE},
    {"Bash a door", 'B', TRUE},
    {"Jam a door", 'j', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
    
	{
    {"Full map", 'M', TRUE},
    {"Map", 'L', TRUE},
    {"Level feeling", KTRL('F'), TRUE},
    {"Character status", 'C', TRUE},
    {"Identify symbol", '/', TRUE},
    {"Show prev messages", KTRL('P'), TRUE},
    {"Current time", KTRL('T'), TRUE},
    {"Various informations", '~', TRUE},
    {"Play record menu", '|', TRUE},
    {"", 0, FALSE}
	},
    
	{
    {"Set options", '=', TRUE},
    {"Interact with macros", '@', TRUE},
    {"Interact w/ visuals", '%', TRUE},
    {"Interact with colors", '&', TRUE},
    {"Enter a user pref", '\"', TRUE},
    {"Reload auto-pick pref", '$', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
        
	{
    {"Save and quit", KTRL('X'), TRUE},
    {"Save", KTRL('S'), TRUE},
    {"Documentation", '?', TRUE},
    {"Redraw", KTRL('R'), TRUE},
    {"Take note", ':', TRUE},
    {"Dump screen dump", ')', TRUE},
    {"Load screen dump", '(', TRUE},
    {"Version info", 'V', TRUE},
    {"", 0, FALSE},
    {"", 0, FALSE}
	},
};


/* All possible squelch states described */
cptr squelch_strings[] = {
	"Keep all",
	"Keep good and better",
	"Keep excellent and better",
	"Keep great",		
	"Keep unknown",	
	"Keep only dungeon books",				
	"Keep closed",						
	"Keep none",				
	"Keep only artefacts",
	NULL,};
