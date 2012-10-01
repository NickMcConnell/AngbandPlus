/* File: tables.c */

/* Mapping of directions, what classes are legal for what races.  Stat 
 * effects, blows per round, store owners, speed-to-energy, base exp level 
 * cost, player sexes, races, classes, spell table (for each class), spell 
 * names by index, conversion of +to_d to Deadliness, traps on chests, 
 * class names, color names, stat abbrevs, window flags, options and their
 * default values.  Druid blows.  
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
{  0, -1,  0,  1, -1,  0,  1, -1,  0,  1 };

s16b ddy[10] =
{  0,  1,  1,  1,  0,  0,  0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 */
s16b ddx_ddd[9] =
{  0,  0,  1, -1,  1, -1,  1, -1,  0 };

s16b ddy_ddd[9] =
{  1, -1,  0,  0,  1,  1, -1, -1,  0 };

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
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	1	/* 7 */,
	1	/* 8 */,
	1	/* 9 */,
	1	/* 10 */,
	1	/* 11 */,
	2	/* 12 */,
	2	/* 13 */,
	2	/* 14 */,
	2	/* 15 */,
	2	/* 16 */,
	2	/* 17 */,
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
	3	/* 18/100-18/109 */,
	3	/* 18/110-18/119 */,
	3	/* 18/120-18/129 */,
	3	/* 18/130-18/139 */,
	3	/* 18/140-18/149 */,
	3	/* 18/150-18/159 */,
	3	/* 18/160-18/169 */,
	3	/* 18/170-18/179 */,
	3	/* 18/180-18/189 */,
	3	/* 18/190-18/199 */,
	3	/* 18/200-18/209 */,
	3	/* 18/210-18/219 */,
	3	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- extra tenth-mana-points per level.
 */
byte adj_mag_mana[] =
{
	0	/* 3 */,
	0	/* 4 */,
	0	/* 5 */,
	0	/* 6 */,
	5	/* 7 */,
	6	/* 8 */,
	7	/* 9 */,
	8	/* 10 */,
	9	/* 11 */,
	9	/* 12 */,
	10	/* 13 */,
	10	/* 14 */,
	10	/* 15 */,
	11	/* 16 */,
	11	/* 17 */,
	12	/* 18/00-18/09 */,
	13	/* 18/10-18/19 */,
	14	/* 18/20-18/29 */,
	15	/* 18/30-18/39 */,
	17	/* 18/40-18/49 */,
	20	/* 18/50-18/59 */,
	22	/* 18/60-18/69 */,
	25	/* 18/70-18/79 */,
	30	/* 18/80-18/89 */,
	35	/* 18/90-18/99 */,
	40	/* 18/100-18/109 */,
	45	/* 18/110-18/119 */,
	50	/* 18/120-18/129 */,
	55	/* 18/130-18/139 */,
	60	/* 18/140-18/149 */,
	65	/* 18/150-18/159 */,
	70	/* 18/160-18/169 */,
	74	/* 18/170-18/179 */,
	77	/* 18/180-18/189 */,
	79	/* 18/190-18/199 */,
	81	/* 18/200-18/209 */,
	83	/* 18/210-18/219 */,
	85	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- Minimum failure rate (percentage)
 */
byte adj_mag_fail[] =
{
	99	/* 3 */,
	99	/* 4 */,
	99	/* 5 */,
	99	/* 6 */,
	50	/* 7 */,
	40	/* 8 */,
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
 * Stat Table (INT/WIS) -- Reduction of failure rate
 */
byte adj_mag_stat[] =
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
	6	/* 18/80-18/89 */,
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
 * Stat Table (CHR) -- payment percentage of normal.  Effect of CHR 
 * increased because of changes elsewhere. -LM-
 */
byte adj_chr_gold[] =
{
	150	/* 3 */,
	143	/* 4 */,
	138	/* 5 */,
	132	/* 6 */,
	127	/* 7 */,
	123	/* 8 */,
	120	/* 9 */,
	117	/* 10 */,
	114	/* 11 */,
	112	/* 12 */,
	110	/* 13 */,
	108	/* 14 */,
	106	/* 15 */,
	104	/* 16 */,
	102	/* 17 */,
	100	/* 18/00-18/09 */,
	99	/* 18/10-18/19 */,
	98	/* 18/20-18/29 */,
	97	/* 18/30-18/39 */,
	96	/* 18/40-18/49 */,
	95	/* 18/50-18/59 */,
	94	/* 18/60-18/69 */,
	93	/* 18/70-18/79 */,
	92	/* 18/80-18/89 */,
	91	/* 18/90-18/99 */,
	90	/* 18/100-18/109 */,
	89	/* 18/110-18/119 */,
	88	/* 18/120-18/129 */,
	87	/* 18/130-18/139 */,
	86	/* 18/140-18/149 */,
	85	/* 18/150-18/159 */,
	84	/* 18/160-18/169 */,
	83	/* 18/170-18/179 */,
	82	/* 18/180-18/189 */,
	81	/* 18/190-18/199 */,
	80	/* 18/200-18/209 */,
	80	/* 18/210-18/219 */,
	80	/* 18/220+ */
};


/*
 * Stat Table (INT) -- Magic devices
 */
byte adj_int_dev[] =
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
byte adj_wis_sav[] =
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
 * Stat Table (DEX) -- disarming (also getting out of pits)
 */
byte adj_dex_dis[] =
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
byte adj_int_dis[] =
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
byte adj_dex_ta[] =
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
 * Stat Table (STR) -- bonus to Deadliness (plus 128).  To compensate
 * for changes elsewhere, STR now has a larger effect. -LM-
 */
byte adj_str_td[] =
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
	128 + 3	/* 18/00-18/09 */,
	128 + 4	/* 18/10-18/19 */,
	128 + 4	/* 18/20-18/29 */,
	128 + 5	/* 18/30-18/39 */,
	128 + 6	/* 18/40-18/49 */,
	128 + 7	/* 18/50-18/59 */,
	128 + 8	/* 18/60-18/69 */,
	128 + 9	/* 18/70-18/79 */,
	128 + 10	/* 18/80-18/89 */,
	128 + 11	/* 18/90-18/99 */,
	128 + 12	/* 18/100-18/109 */,
	128 + 13	/* 18/110-18/119 */,
	128 + 14	/* 18/120-18/129 */,
	128 + 15	/* 18/130-18/139 */,
	128 + 16	/* 18/140-18/149 */,
	128 + 17	/* 18/150-18/159 */,
	128 + 18	/* 18/160-18/169 */,
	128 + 19	/* 18/170-18/179 */,
	128 + 20	/* 18/180-18/189 */,
	128 + 21	/* 18/190-18/199 */,
	128 + 22	/* 18/200-18/209 */,
	128 + 23	/* 18/210-18/219 */,
	128 + 25	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- bonus to Skill (plus 128.  To compensate for
 * changes elsewhere, DEX now has a larger effect. -LM-
 */
byte adj_dex_th[] =
{
	128 + -4	/* 3 */,
	128 + -3	/* 4 */,
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
	128 + 1	/* 15 */,
	128 + 2	/* 16 */,
	128 + 3	/* 17 */,
	128 + 3	/* 18/00-18/09 */,
	128 + 3	/* 18/10-18/19 */,
	128 + 4	/* 18/20-18/29 */,
	128 + 4	/* 18/30-18/39 */,
	128 + 4	/* 18/40-18/49 */,
	128 + 5	/* 18/50-18/59 */,
	128 + 5	/* 18/60-18/69 */,
	128 + 6	/* 18/70-18/79 */,
	128 + 6	/* 18/80-18/89 */,
	128 + 7	/* 18/90-18/99 */,
	128 + 8	/* 18/100-18/109 */,
	128 + 9	/* 18/110-18/119 */,
	128 + 10	/* 18/120-18/129 */,
	128 + 11	/* 18/130-18/139 */,
	128 + 12	/* 18/140-18/149 */,
	128 + 13	/* 18/150-18/159 */,
	128 + 14	/* 18/160-18/169 */,
	128 + 15	/* 18/170-18/179 */,
	128 + 16	/* 18/180-18/189 */,
	128 + 17	/* 18/190-18/199 */,
	128 + 18	/* 18/200-18/209 */,
	128 + 19	/* 18/210-18/219 */,
	128 + 20	/* 18/220+ */
};


/*
 * Stat Table (STR) -- weight limit in deca-pounds
 */
byte adj_str_wgt[] =
{
	6	/* 3 */,
	7	/* 4 */,
	8	/* 5 */,
	9	/* 6 */,
	10	/* 7 */,
	11	/* 8 */,
	12	/* 9 */,
	13	/* 10 */,
	14	/* 11 */,
	15	/* 12 */,
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
byte adj_str_hold[] =
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
byte adj_str_dig[] =
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
byte adj_str_blow[] =
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
byte adj_dex_blow[] =
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
	3	/* 18/40-18/49 */,
	3	/* 18/50-18/59 */,
	4	/* 18/60-18/69 */,
	4	/* 18/70-18/79 */,
	5	/* 18/80-18/89 */,
	5	/* 18/90-18/99 */,
	6	/* 18/100-18/109 */,
	7	/* 18/110-18/119 */,
	8	/* 18/120-18/129 */,
	9	/* 18/130-18/139 */,
	9	/* 18/140-18/149 */,
	10	/* 18/150-18/159 */,
	10	/* 18/160-18/169 */,
	11	/* 18/170-18/179 */,
	11	/* 18/180-18/189 */,
	11	/* 18/190-18/199 */,
	11	/* 18/200-18/209 */,
	11	/* 18/210-18/219 */,
	11	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- Used for number of shots per round
 */
byte adj_dex_shots[] =
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
	0	/* 14 */,
	0	/* 15 */,
	0	/* 16 */,
	0	/* 17 */,
	1	/* 18/00-18/09 */,
	1	/* 18/10-18/19 */,
	2	/* 18/20-18/29 */,
	3	/* 18/30-18/39 */,
	4	/* 18/40-18/49 */,
	5	/* 18/50-18/59 */,
	6	/* 18/60-18/69 */,
	7	/* 18/70-18/79 */,
	8	/* 18/80-18/89 */,
	9	/* 18/90-18/99 */,
	10	/* 18/100-18/109 */,
	11	/* 18/110-18/119 */,
	12	/* 18/120-18/129 */,
	13	/* 18/130-18/139 */,
	14	/* 18/140-18/149 */,
	15	/* 18/150-18/159 */,
	16	/* 18/160-18/169 */,
	17	/* 18/170-18/179 */,
	18	/* 18/180-18/189 */,
	19	/* 18/190-18/199 */,
	20	/* 18/200-18/209 */,
	20	/* 18/210-18/219 */,
	20	/* 18/220+ */
};


/*
 * Stat Table (DEX) -- chance of avoiding "theft" and "falling".  Modified to 
 * make both theft and security from theft less of a sure thing by LM.
 */
byte adj_dex_safe[] =
{
	5	/* 3 */,
	5	/* 4 */,
	6	/* 5 */,
	6	/* 6 */,
	7	/* 7 */,
	7	/* 8 */,
	8	/* 9 */,
	8	/* 10 */,
	9	/* 11 */,
	10	/* 12 */,
	12	/* 13 */,
	14	/* 14 */,
	16	/* 15 */,
	18	/* 16 */,
	20	/* 17 */,
	22	/* 18/00-18/09 */,
	25	/* 18/10-18/19 */,
	27	/* 18/20-18/29 */,
	30	/* 18/30-18/39 */,
	32	/* 18/40-18/49 */,
	35	/* 18/50-18/59 */,
	40	/* 18/60-18/69 */,
	45	/* 18/70-18/79 */,
	50	/* 18/80-18/89 */,
	55	/* 18/90-18/99 */,
	60	/* 18/100-18/109 */,
	65	/* 18/110-18/119 */,
	70	/* 18/120-18/129 */,
	75	/* 18/130-18/139 */,
	80	/* 18/140-18/149 */,
	82	/* 18/150-18/159 */,
	85	/* 18/160-18/169 */,
	87	/* 18/170-18/179 */,
	90	/* 18/180-18/189 */,
	92	/* 18/190-18/199 */,
	95	/* 18/200-18/209 */,
	95	/* 18/210-18/219 */,
	95	/* 18/220+ */
};


/*
 * Stat Table (CON) -- base regeneration rate
 */
byte adj_con_fix[] =
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
 * Stat Table (CON) -- extra half-hitpoints per level (plus 128).
 * Because monsters don't breath as powerfully now, I have reduced the
 * effect of this stat. -LM-
 */
byte adj_con_mhp[] =
{
	128 + -5	/* 3 */,
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
	128 + 2	/* 17 */,
	128 + 3	/* 18/00-18/09 */,
	128 + 4	/* 18/10-18/19 */,
	128 + 4	/* 18/20-18/29 */,
	128 + 4	/* 18/30-18/39 */,
	128 + 4	/* 18/40-18/49 */,
	128 + 5	/* 18/50-18/59 */,
	128 + 5	/* 18/60-18/69 */,
	128 + 6	/* 18/70-18/79 */,
	128 + 6	/* 18/80-18/89 */,
	128 + 7	/* 18/90-18/99 */,
	128 + 8	/* 18/100-18/109 */,
	128 + 9	/* 18/110-18/119 */,
	128 + 9	/* 18/120-18/129 */,
	128 + 10	/* 18/130-18/139 */,
	128 + 11	/* 18/140-18/149 */,
	128 + 12	/* 18/150-18/159 */,
	128 + 12	/* 18/160-18/169 */,
	128 + 13	/* 18/170-18/179 */,
	128 + 14	/* 18/180-18/189 */,
	128 + 15	/* 18/190-18/199 */,
	128 + 17	/* 18/200-18/209 */,
	128 + 18	/* 18/210-18/219 */,
	128 + 20	/* 18/220+ */
};


/*
 * Stat Table (DEX) evasion max bonus from DEX.
 */
byte adj_dex_evas[] =
{
	25	/* 3 */,
	25	/* 4 */,
	25	/* 5 */,
	25	/* 6 */,
	25	/* 7 */,
	25	/* 8 */,
	25	/* 9 */,
	25	/* 10 */,
	26	/* 11 */,
	27	/* 12 */,
	28	/* 13 */,
	29	/* 14 */,
	30	/* 15 */,
	31	/* 16 */,
	32	/* 17 */,
	33	/* 18/00-18/09 */,
	33	/* 18/10-18/19 */,
	34	/* 18/20-18/29 */,
	34	/* 18/30-18/39 */,
	35	/* 18/40-18/49 */,
	35	/* 18/50-18/59 */,
	35	/* 18/60-18/69 */,
	36	/* 18/70-18/79 */,
	36	/* 18/80-18/89 */,
	36	/* 18/90-18/99 */,
	37	/* 18/100-18/109 */,
	37	/* 18/110-18/119 */,
	37	/* 18/120-18/129 */,
	38	/* 18/130-18/139 */,
	38	/* 18/140-18/149 */,
	38	/* 18/150-18/159 */,
	39	/* 18/160-18/169 */,
	39	/* 18/170-18/179 */,
	39	/* 18/180-18/189 */,
	40	/* 18/190-18/199 */,
	40	/* 18/200-18/209 */,
	40	/* 18/210-18/219 */,
	40	/* 18/220+ */
};


/*
 * This table is used to help calculate the number of blows the player 
 * can make in a single round of attacks (one player turn) with a 
 * weapon that is not too heavy to wield effectively.
 *
 * The player gets "blows_table[P][D]" blows/round, as shown below.
 *
 * To get "P", we look up the relevant "adj_str_blow[]" (see above),
 * multiply it by 6, and then divide it by the effective weapon 
 * weight (in deci-pounds), rounding down.
 *
 * To get "D", we look up the relevant "adj_dex_blow[]" (see above).
 *
 * (Some interesting calculations)
 * The character cannot get five blows with any weapon greater than 24
 * lb, and cannot get six with any weapon greater than 14.4 lb.
 */
byte blows_table[12][12] =
{
  /*  <- Dexterity factor -> */
  /* 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11+ */

  {  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3 }, /*  0         */
  {  2,  2,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3 }, /*  1    ^    */
  {  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  4 }, /*  2    |    */
  {  2,  2,  2,  2,  3,  3,  3,  3,  3,  4,  4,  4 }, /*  3         */
  {  2,  2,  2,  3,  3,  3,  3,  3,  4,  4,  4,  4 }, /*  4  Ratio  */
  {  2,  2,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4 }, /*  5  of STR */
  {  2,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  5 }, /*  6  over   */
  {  2,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5 }, /*  7  weight */
  {  2,  3,  3,  3,  4,  4,  4,  4,  4,  5,  5,  5 }, /*  8         */
  {  2,  3,  3,  3,  4,  4,  4,  4,  5,  5,  5,  5 }, /*  9    |    */
  {  2,  3,  3,  3,  4,  4,  4,  5,  5,  5,  5,  6 }, /* 10    V    */
  {  2,  3,  3,  4,  4,  4,  5,  5,  5,  6,  6,  6 }  /* 11+        */
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
 * Experience levels
 */
s32b player_exp[PY_MAX_LEVEL] =
{
	10,		/* 1 */
	25,
	45,
	70,
	105,
	150,
	210,
	290,
	390,
	530,		/* 10 */
	720,
	960,
	1270,
	1700,
	2250,
	3000,
	4000,
	5300,
	6900,
	9100,		/* 20 */
	12100,
	16000,
	21000,
	27500,
	36500,
	48500,
	64000,
	84500L,
	111000L,
	144000L,	/* 30 */
	183000L,
	230000L,
	285000L,
	350000L,
	430000L,
	525000L,
	640000L,
	780000L,
	945000L,
	1140000L,	/* 40 */
	1380000L,
	1660000L,
	2000000L,
	2400000L,
	2900000L,
	3500000L,
	4200000L,
	5000000L,
	6000000L,
	7200000L	/* 50 */
};


/*
 * Player Sexes
 *
 *	Title,
 *	Winner
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
 * Player Classes
 *
 *	Title,
 *	{STR,INT,WIS,DEX,CON,CHR},
 *	 c_dis,  c_dev,  c_sav,  c_stl,  c_srh,  c_fos,  c_thn,  c_thb,
 *	cx_dis, cx_dev, cx_sav, cx_stl, cx_srh, cx_fos, cx_thn, cx_thb,
 *
 *	Note: Modifier values are the total level bonus that a 50th 
 *    level character would get for that skill.  See xtra1.c for how 
 *    this works.
 *
 *	HD, Exp
 */
player_class class_info[MAX_CLASS] =
{
	{
		"Warrior",
		{ 5, -2, -2, 2, 2, -1},
		18, 18, 18, 1, 14,  5, 25, 25,
		50, 35, 50, 0,  0,  0,100, 80,
		9
	},

	{	
		"Mage",
		{-4, 3, 0, 1, -2, 1},
		14, 36, 30,  3, 16, 12, 10, 15,
		30, 65, 45, -3,  0,  0, 25, 25,
		0
	},

	{
		"Priest",
		{-1, -3, 3, -1, 0, 2},
		15, 32, 32,  2, 16,  8, 16, 15,
		30, 50, 60,  0,  0,  0, 50, 25,
		2
	},

		/* A high-level rogue sees and disarms *everything*. */
	{
		"Rogue",
		{ 2, 1, -2, 3, 1, -5},
		45, 32, 28,  6, 36, 28, 15, 29,
		90, 50, 50,  0, 40, 40, 70, 60,
		5
	},

	{
		"Ranger",
		{ 1, 0, 1, 2, 1, 0},
		22, 26, 28, 4, 24, 16, 15, 30,
		50, 40, 50, 0,  0,  0, 65, 95,
		5
	},

	{
		"Paladin",
		{ 2, -3, 1, 0, 1, 2},
		14, 16, 25, 1, 12,  2, 19, 15,
		30, 30, 55, 0,  0,  0, 76, 20,
		7
	},

	{
		"Druid",
		{ -2, -2, 3, 1, 0, 1},
		22, 26, 30, 4, 24, 16, 12, 21,
		40, 45, 60, 0,  0,  0, 30, 40,
		0
	},

	{
		"Necromancer",
		{ -2, 3, -2, 1, 0, -2},
		14, 32, 25,  2, 14,  6, 10, 15,
		30, 50, 40,  0,  0,  0, 30, 30,
		0
	},

	{
		"Assassin",
		{ 2, 1, -3, 2, 0, -2},
		35, 26, 20,  5, 30, 22, 20, 29,
		70, 50, 40,  0, 10, 10, 85, 80,
		4
	}
};


/*
 * Bonuses to close combat skill based on class, weapon, and 
 * weapon weight. -BR-
 */
player_weapon weapon_info[MAX_CLASS] =
{
	{
		/* Warrior */
		150,		/* Max Weight (decipounds) at level 1 */
		450,		/* Max Weight (decipounds) at level 50 */
		17,		/* Penalty per 10 pounds over */
		10,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Mage */
		60,		/* Max Weight (decipounds) at level 1 */
		160,		/* Max Weight (decipounds) at level 50 */
		33,		/* Penalty per 10 pounds over */
		30,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Priest */
		120,		/* Max Weight (decipounds) at level 1 */
		220,		/* Max Weight (decipounds) at level 50 */
		25,		/* Penalty per 10 pounds over */
		25,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		TRUE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Rogue */
		100,		/* Max Weight (decipounds) at level 1 */
		200,		/* Max Weight (decipounds) at level 50 */
		33,		/* Penalty per 10 pounds over */
		25,		/* Max Penalty */
		17,		/* Bonus per 10 pounds under */
		15,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Ranger */
		120,		/* Max Weight (decipounds) at level 1 */
		250,		/* Max Weight (decipounds) at level 50 */
		20,		/* Penalty per 10 pounds over */
		20,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Paladin */
		150,		/* Max Weight (decipounds) at level 1 */
		450,		/* Max Weight (decipounds) at level 50 */
		17,		/* Penalty per 10 pounds over */
		10,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		TRUE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Druid */
		50,		/* Max Weight (decipounds) at level 1 */
		120,		/* Max Weight (decipounds) at level 50 */
		33,		/* Penalty per 10 pounds over */
		30,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		TRUE		/* Bare-handed skill */
	},

	{
		/* Necromancer */
		60,		/* Max Weight (decipounds) at level 1 */
		160,		/* Max Weight (decipounds) at level 50 */
		33,		/* Penalty per 10 pounds over */
		30,		/* Max Penalty */
		0,		/* Bonus per 10 pounds under */
		0,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	},

	{
		/* Assassin */
		100,		/* Max Weight (decipounds) at level 1 */
		200,		/* Max Weight (decipounds) at level 50 */
		33,		/* Penalty per 10 pounds over */
		25,		/* Max Penalty */
		22,		/* Bonus per 10 pounds under */
		20,		/* Max Bonus */
		FALSE,		/* Edged Weapon Penalty */
		FALSE		/* Bare-handed skill */
	}
};


/*
 * Specialty abilities available by class -BR-
 */
byte specialty_info [MAX_CLASS][CLASS_SPECIALTIES] =
{
	{
		/* Warrior */
		SP_ARMOR_MAST,
		SP_ARMSMAN,
		SP_ATHLETICS,
		SP_MIGHTY_THROW,
		SP_FAST_ATTACK,
		SP_FURY,
		SP_MARKSMAN,
		SP_PIERCE_SHOT,
		SP_REGENERATION,
		SP_SHIELD_MAST,
		SP_EVASION,
		SP_MARTIAL_ARTS,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Mage */
		SP_ARMOR_PROFICIENCY,
		SP_BEGUILE,
		SP_CLARITY,
		SP_ENHANCE_MAGIC,
		SP_FAST_CAST,
		SP_MEDITATION,
		SP_POWER_SIPHON,
		SP_MAGIC_RESIST,
		SP_PHASEWALK,
		SP_HEIGHTEN_MAGIC,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Priest */
		SP_CLARITY,
		SP_ENHANCE_MAGIC,
		SP_FAST_CAST,
		SP_MEDITATION,
		SP_ARMSMAN,
		SP_PHASEWALK,
		SP_HOLY_LIGHT,
		SP_HARMONY,
		SP_FURY,
		SP_HEIGHTEN_MAGIC,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Rogue */
		SP_ARMSMAN,
		SP_ATHLETICS,
		SP_EVASION,
		SP_FAST_ATTACK,
		SP_MARKSMAN,
		SP_REGENERATION,
		SP_MIGHTY_THROW,
		SP_PHASEWALK,
		SP_UNLIGHT,
		SP_EXTRA_TRAP,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Ranger */
		SP_ARMSMAN,
		SP_ATHLETICS,
		SP_EVASION,
		SP_FAST_ATTACK,
		SP_FURY,
		SP_MARKSMAN,
		SP_PIERCE_SHOT,
		SP_REGENERATION,
		SP_MIGHTY_THROW,
		SP_SHIELD_MAST,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Paladin */
		SP_ARMOR_MAST,
		SP_ARMSMAN,
		SP_ATHLETICS,
		SP_FAST_ATTACK,
		SP_FURY,
		SP_REGENERATION,
		SP_SHIELD_MAST,
		SP_ENHANCE_MAGIC,
		SP_MAGIC_RESIST,
		SP_HOLY_LIGHT,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Druid */
		SP_ARMOR_PROFICIENCY,
		SP_BEGUILE,
		SP_CLARITY,
		SP_FAST_CAST,
		SP_MEDITATION,
		SP_ENHANCE_MAGIC,
		SP_REGENERATION,
		SP_HARMONY,
		SP_POWER_STRIKE,
		SP_HEIGHTEN_MAGIC,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Necromancer */
		SP_UNLIGHT,
		SP_ARMOR_PROFICIENCY,
		SP_BEGUILE,
		SP_CLARITY,
		SP_ENHANCE_MAGIC,
		SP_FAST_CAST,
		SP_MEDITATION,
		SP_MAGIC_RESIST,
		SP_SOUL_SIPHON,
		SP_HEIGHTEN_MAGIC,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	},

	{
		/* Assassin */
		SP_ARMSMAN,
		SP_ATHLETICS,
		SP_EVASION,
		SP_FAST_ATTACK,
		SP_MARKSMAN,
		SP_PIERCE_SHOT,
		SP_REGENERATION,
		SP_MIGHTY_THROW,
		SP_UNLIGHT,
		SP_FURY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY,
		SP_NO_SPECIALTY
	}
};


/*
 * The spell information table.  Each class has a list of spells, 
 * organized into spellbooks. -LM-
 *
 *    Spell Book Tval, Spell Stat, Spell Realm, Level of first spell, 
 *    Max armour weight that avoids mana penalties, Additional armour
 *    weight to eliminate all mana.
 *
 *    Number of spells (max is 64), array (not spell) index of first 
 *    spell in each of the nine books, listed by sval.
 *
 *    Array of Spells, listed by spellbook.  The index controls spell 
 *    name (see next table) and effects (see cmd5.c).  Max index = 255.
 *     { Index, Lev, Mana, Fail, Exp/Lev }
 */
player_magic magic_info[MAX_CLASS] =
{
	{
		/*** Warrior ***/

		0,   A_STR,   REALM_NONE,   99,   0,   1,

		/* Zero spells.  Spellbooks contain 0 spells each. */
		0,	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},

		{
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0}
		}
	},

	{
		/*** Mage ***/

		TV_MAGIC_BOOK,   A_INT,   REALM_SORCERY,   1,   250,  600,

		/* 59 spells in nine books. */
		59,	{ 0, 9, 18, 26, 33, 38, 43, 48, 53, 59, 59 },

		{
			/* Magic for Beginners (sval 0) */
			{   0,   1,  1, 22,   2}, /* Magic Missile */
			{   1,	 1,  1, 23,   2}, /* Detect Monsters */
			{   2,	 2,  1, 24,   2}, /* Phase Door */
			{   3,	 3,  2, 25,   2}, /* Light Area */
			{   4,	 3,  1, 25,   2}, /* Combat Poison */
			{   5,	 4,  2, 25,   3}, /* Cure Light Wounds */
			{  69,	 5,  3, 25,   3},       /* Find Traps */
			{  70,	 5,  3, 25,   3},       /* Detect Doors/Stairs */
			{   8,	 6,  3, 27,   4},	/* Stinking Cloud */

			/* Conjurings and Tricks (sval 1) */
			{   9,	 7,  3, 30,   5},	/* Confuse Monster */
			{  10,	 7,  3, 30,   6},	/* Lightning Bolt */
			{  11,	 9,  3, 30,   6},	/* Door Destruction */
			{  12,	 9,  4, 30,   6},	/* Sleep Monster */
			{  13,	11,  5, 35,   6},	/* Cure Poison */
			{  14,	11,  5, 35,   6},	/* Teleport Self */
			{  15,	12,  5, 30,   7},	/* Spear of Light */
			{  16,	13,  5, 40,   9},	/* Recharge Item I */
			{  17,	13,  5, 44,   7},	/* Cone of Cold */

			/* Incantations and Illusions (sval 2) */
			{  18,	14,  8, 45,   7},	/* Satisfy Hunger */
			{  19,	15,  7, 45,   7},	/* Magic Disarm */
			{  20,	15,  7, 45,   7},	/* Polymorph Other */
			{  21,	17, 12, 75,   8},	/* Identify */
			{  22,	17,  7, 50,   8},	/* Sleep Monsters */
			{  23,	19,  7, 50,   8},	/* Fire Bolt */
			{  24,	19,  7, 50,   8},	/* Slow Monster */
			{  25,	20,  2, 50,   8},	/* Tap magical energy */

			/* Sorcery and Evocations (sval 3) */
			{  26,	22, 12, 75,   9},	/* Frost Ball */
			{  27,	23, 10, 75,  11},	/* Recharge Item II */
			{  28,	25, 12, 60,  11},	/* Teleport Other */
			{  29,	25, 12, 65,  11},	/* Haste Self */
			{  30,	29, 16, 65,  12},	/* Fire Ball */
			{  31,	35, 30, 80,  15},	/* Hold Monsters */
			{  32,	37, 30, 80,  15},	/* Word of Destruction */

			/* Resistance of Scarabtarices (sval 4) */
			{  33,	21,  5, 50,  20},	/* Resist Fire */
			{  34,	21,  5, 50,  20},	/* Resist Cold */
			{  35,	21,  5, 50,  20},	/* Resist Acid */
			{  36,	27, 11, 75,  30},	/* Resist Poison */
			{  37,	35, 22, 85,  50},	/* Resistance */

			/* Mordenkainen's Escapes (sval 5) */
			{  38,	12,  4, 20,  16},	/* Door Creation */
			{  39,	16,  8, 40,  20},	/* Stair Creation */
			{  40,	20, 11, 60,  20},	/* Teleport Level */
			{  41,	25, 15, 60,  25},	/* Word of Recall */
			{  42,	43, 20, 65,  75},	/* Dimension Door */

			/* Kelek's Grimoire of Power (sval 6) */
			{  43,	 5,  2, 50,  10},	/* Detect Evil */
			{  44,	15,  6, 70,  12},	/* Detect Enchantment */
			{  45,	25, 10, 85,  20},	/* Earthquake */
			{  46,	35, 25, 65,  50},	/* Beguiling */
			{  47,	37, 21, 70,  55},	/* Starburst */

			/* Tenser's transformations... (sval 7) */
			{  48,	18,  0, 50,  22},	/* Clear Mind */
			{  49,	30,  8, 75,  30},	/* Shield */
			{  50,	38, 15, 85,  50},	/* Recharge Item III */
			{  51,	40, 18, 50,  55},	/* Essence of Speed */
			{  52,	44, 24, 75,  90},	/* Strengthen Defenses */

			/* Raal's Tome of Destruction (sval 8) */
			{  53,	16,  6, 55,  20},	/* Acid Bolt */
			{  54,	21, 10, 65,  17},	/* Cloud Kill */
			{  55,	27, 15, 70,  22},	/* Acid Ball */
			{  56,	33, 23, 70,  35},	/* Ice Storm */
			{  57,	39, 33, 70,  60},	/* Meteor Swarm */
			{  58,	46, 42, 60, 120},	/* Mana Storm */

			{   0, 	99,  0,  0,   0}, 
			{   0, 	99,  0,  0,   0}, 
			{   0, 	99,  0,  0,   0}, 
			{   0, 	99,  0,  0,   0}, 
			{   0, 	99,  0,  0,   0}
		}
	},

	{
		/*** Priest ***/

		TV_PRAYER_BOOK,  A_WIS,  REALM_PIETY,  1,  350,  800,

		/* 58 prayers in 9 books. */
		58, { 0, 8, 16, 25, 31, 37, 42, 47, 53, 58, 58 },

		{
			/* Novice's Handbook (sval 0) */
			{  64,	 1,  1, 10,   2}, /* Detect Evil */
			{  65,	 1,  1, 15,   2}, /* Cure Light Wounds */
			{  66,	 3,  2, 20,   2}, /* Bless */
			{  67,	 3,  2, 25,   2}, /* Remove Fear */
			{  68,	 3,  2, 25,   2}, /* Call Light */
			{  69,	 5,  4, 25,   3}, /* Find Traps */
			{  70,	 5,  4, 25,   3}, /* Detect Doors/Stairs */
			{  71,	 6,  1, 30,   3}, /* Slow Poison */

			/* Words of Wisdom (sval 1) */
			{  72,	 7,  4, 28,   4}, /* Cure Serious Wounds */
			{  73,	 7,  2, 29,   4}, /* Scare Monster */
			{  74,	 8,  5, 32,   4}, /* Portal */
			{  75,	 9,  4, 34,   5}, /* Chant */
			{  76,	 9,  4, 36,   5}, /* Sanctuary */
			{  77,	11,  4, 38,   5}, /* Satisfy Hunger */
			{  78,	13,  5, 38,   5}, /* Remove Curse */
			{  79,	13,  6, 38,   5}, /* Resist Heat and Cold */

			/* Chants and Blessings (sval 2) */
			{  80,	14,  6, 38,   5}, /* Neutralize Poison */
			{  81,	15,  7, 38,   6}, /* Orb of Draining */
			{  82,	15,  7, 40,   6}, /* Sense Invisible */
			{  83,	17,  7, 42,   6}, /* Protection from Evil */
			{  84,	17,  8, 42,   7}, /* Cure Mortal Wounds */
			{  85,	19,  8, 55,   7}, /* Earthquake */
			{  86,	19,  9, 45,   7}, /* Sense Surroundings */
			{  87,	21,  9, 45,   7}, /* Turn Undead */
			{  88,	21,  9, 50,   8}, /* Prayer */

			/* Exorcism and Dispelling (sval 3) */
			{  89,	23, 11, 50,   8}, /* Dispel Undead */
			{  90,	25, 16, 60,  11}, /* Heal */
			{  91,	27, 18, 70,  15}, /* Dispel Evil */
			{  92,	32, 22, 75,  20}, /* Sacred Shield */
			{  93,	37, 40, 90,  29}, /* Glyph of Warding */
			{  94,	40, 40, 90,  35}, /* Holy Word */

			/* Ethereal openings (sval 4) */
			{  95,	 3,  2, 50,  15}, /* Blink */
			{  96,	10,  7, 50,  20}, /* Teleport Self */
			{  97,	20, 20, 80,  25}, /* Teleport Other */
			{  98,	30, 30, 75,  40}, /* Teleport Level */
			{  99,	35, 40, 75,  65}, /* Word of Recall */
			{ 100,	40, 40, 75,  90}, /* Alter Reality */

			/* Godly Insights... (sval 5) */
			{ 101,	 2,  2, 50,  12}, /* Detect Monsters */
			{ 102,	10,  8, 80,  16}, /* Detection */
			{ 104,	28, 20, 80,  30}, /* Perception */
			{ 103,	38, 20, 80,  40}, /* Probing */
			{ 105,	42, 50, 80,  70}, /* Clairvoyance */

			/* Purifications and Healing (sval 6) */
			{ 106,	36, 30, 50,  65}, /* Banishment */
			{ 107,	38, 40, 60,  70}, /* Healing */
			{ 108,	38, 50, 80,  70}, /* Sacred Knowledge */
			{ 109,	40, 60, 90,  90}, /* Restoration */
			{ 110,	40, 60, 90,  90}, /* Remembrance */

			/* Holy Infusions (sval 7) */
			{ 111,	 5,  2, 40,  20}, /* Unbarring Ways */
			{ 112,	15, 30, 80,  25}, /* Recharging */
			{ 113,	25, 30, 80,  40}, /* Dispel Curse */
			{ 114,	31, 18, 70,  55}, /* Disarm Trap */
			{ 115,	35, 22, 80,  70}, /* Holding */
			{ 116,	39, 40, 85,  80}, /* Enchant Weapon or Armour */

			/* Wrath of God (sval 8) */
			{ 117,	17, 14, 70,  25}, /* Ball of Light */
			{ 118,	21, 16, 75,  30}, /* Holy Lance */
			{ 119,	29, 25, 80,  40}, /* Word of Destruction */
			{ 120,	40, 30, 80,  65}, /* Annihilation */
			{ 121,	48, 45, 70, 150}, /* Call on Varda */

			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}
		}
	},

	{
		/*** Rogue ***/

		TV_MAGIC_BOOK,  A_INT,  REALM_SORCERY,  5,  300,  1000,

		/* 34 spells in five books */
		34,	{ 0, 8, 15, 22, 22, 22, 28, 28, 34, 34, 34 },

		{
			/* Magic for Beginners (sval 0) */
			{   1,	 5,  1, 40,   1},	/* Detect Monsters */
			{   2,	 6,  1, 45,   1},	/* Phase Door */
			{ 141,	 6,  2, 50,   1},	/* Detect Trap/Doors */
			{   3,	 7,  3, 65,   1},	/* Light Area */
			{   4,	 7,  1, 60,   1},	/* Combat Poison */
			{   5,	 9,  2, 65,   2},	/* Cure Light Wounds */
			{   6,	 9,  2, 65,   2},	/* Detect Treasure */
			{   7,	10,  2, 70,   2},	/* Detect Objects */

			/* Conjurings and Tricks (sval 1) */
			{  11,	13,  2, 40,   2},	/* Door Destruction */
			{  12,	15,  5, 70,   2},	/* Sleep Monster */
			{  13,	16,  5, 70,   2},	/* Cure Poison */
			{  14,	17,  4, 50,   2},	/* Teleport Self */
			{  59,	19,  3, 60,   2},	/* Hit and Run */
			{  19,	21,  2, 40,   2},	/* Magic Disarm */
			{   8,	23,  4, 70,   3},	/* Stinking Cloud */

			/* Incantations and Illusions (sval 2) */
			{  44,	25,  2, 50,   3},	/* Detect Enchantment */
			{  20,	27,  7, 50,   3},	/* Polymorph Other */
			{  21,	28,  7, 50,   4},	/* Identify */
			{  16,	32,  7, 50,   4},	/* Recharge Item I */
			{  24,	34,  7, 60,   4},	/* Slow Monster */
			{  18,	37,  9, 60,   5},	/* Satisfy Hunger */
			{  29,	39, 12, 60,   5},	/* Haste Self */

			/* Sorcery and Evocations (sval 3) */

			/* Resistance of Scarabtarices (sval 4) */

			/* Mordenkainen's Escapes (sval 5) */
			{  38,	26,  7, 20,   6},	/* Door Creation */
			{  39,	30,  8, 40,   6},	/* Stair Creation */
			{  40,	34, 11, 60,   6},	/* Teleport Level */
			{ 251,	36, 12, 70,   7}, /* Slip into the Shadows */
			{  41,	40, 15, 60,  10},	/* Word of Recall */
			{  28,	42, 16, 85,   3},	/* Teleport Other */

			/* Kelek's Grimoire of Power (sval 6) */

			/* Tenser's Transformations... (sval 7) */
			{  82,	30,  4, 40,  10},	/* Sense Invisible */
			{  86,	35, 15, 50,  15},	/* Probing */
			{  49,	37, 15, 60,  16},	/* Shield */
			{  27,	40, 22, 60,  20},	/* Recharge Item II */
			{  37,	42, 25, 60,  21},	/* Resistance */
			{  60,	47, 25, 50,  25},	/* Day of Misrule */

			/* Raal's Tome of Destruction (sval 8) */

			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0}
		}
	},

	{
		/*** Ranger ***/

		TV_DRUID_BOOK,  A_WIS,  REALM_NATURE,  3,  300,  1000,

		/* 33 techniques in 5 books. */
		33,	{ 0, 8, 15, 22, 22, 22, 28, 28, 33, 33, 33 },

		{
			/* Call of the Wild (sval 0) */
			{ 128,	 3,  1, 30,   1}, /* detect life */
			{ 130,	 3,  1, 35,   1}, /* foraging */
			{ 129,	 5,  2, 35,   1}, /* call light */
			{ 141,	 5,  4, 45,   1}, /* detect trap/doors */
			{ 132,	 7,  1, 35,   1}, /* combat poison */
			{ 134,	 7,  2, 30,   2}, /* door destruction */
			{ 131,	 9,  4, 60,   2}, /* blink */
			{ 156,	 9,  6, 60,   1}, /* disarm trap */

			/* Communion with Nature (sval 1) */
			{ 137,	10,  5, 40,   2}, /* Cure poison */
			{ 139,	11,  5, 45,   3}, /* sleep creature */
			{ 140,	13,  5, 40,   3}, /* frighten creature */
			{ 187,	13,  3, 40,   3}, /* Creature Knowledge */
			{  86,	15,  5, 45,   3}, /* sense surroundings */
			{ 142,	16,  5, 60,   3}, /* snuff small life */
			{ 144,	18,  8, 70,   4}, /* heroism */

			/* Gifts of Nature (sval 2) */
			{ 149,	21,  6, 50,   4}, /* resist poison */
			{ 151,	23,  7, 50,   5}, /* resist fire & cold */
			{ 152,	25,  7, 50,   6}, /* detect all */
			{ 154,	27,  8, 55,   8}, /* resist acid & lightning */
			{ 157,	35, 15, 70,  13}, /* identify */
			{ 158,	41, 45, 75,  16}, /* create athelas */
			{ 153,	46,  1, 70,  24}, /* natural vitality */

			/* Book of Combat (sval 3) */

			/* Radagast's Shapeshifts (sval 4) */

			/* Melian's Lore (sval 5) */
			{ 166,	22,  1, 25,   6}, /* detect evil */
			{ 135,	25, 10, 40,  10}, /* turn stone to mud */
			{ 223,	31,  6, 50,  13}, /* probing */
			{ 169,	32,  5, 65,  15}, /* sight beyond sight */
			{ 147,	35, 15, 70,  19}, /* teleport monster */
			{ 170,	45, 20, 70,  34}, /* herbal healing */

			/* Primal Forces (sval 6) */

			/* Bombadil's Songs (sval 7) */
			{ 167,	27, 10, 40,  25}, /* song of frightening */
			{ 176,	27, 10, 40,  25}, /* song of lulling */
			{ 177,	30, 15, 55,  30}, /* song of protection */
			{ 178,	35, 20, 60,  35}, /* song of dispelling */
			{ 180,	48, 60, 60,  40}, /* song of renewal */

			/* Spirit of Yavanna (sval 8) */

			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}

		}
	},

	{
		/*** Paladin ***/

		TV_PRAYER_BOOK,  A_WIS,  REALM_PIETY,  3,  400,  1200,

		/* 40 prayers in 6 books. */
		40, { 0, 8, 16, 16, 25, 25, 30, 35, 35, 40, 40 },

		{
			/* Novice's Handbook (sval 0) */
			{  64,	 3,  1, 30,   1}, /* Detect Evil */
			{  66,	 4,  1, 35,   1}, /* Bless */
			{  67,	 5,  1, 35,   1}, /* Remove Fear */
			{  68,	 7,  2, 35,   1}, /* Call Light */
			{  65,	 8,  2, 35,   2}, /* Cure Light Wounds */
			{  71,	 9,  2, 40,   2}, /* Slow Poison */
			{  151,	10,  4, 45,   2}, /* Resist Heat and Cold */
			{  73,	11,  4, 40,   2}, /* Scare Monster */

			/* Words of Wisdom (sval 1) */
			{  75,	11,  4, 45,   2}, /* Chant */
			{  95,	13,  4, 50,   3}, /* Blink */
			{ 154,	13,  4, 40,   3}, /* Resist Acid & Lightning */
			{  80,	15,  6, 50,   3}, /* Neutralize Poison */
			{  83,	17,  6, 50,   3}, /* Protection from Evil */
			{  69,	18,  6, 70,   3}, /* Find Traps */
			{  70,	18,  6, 70,   3}, /* Detect Doors/Stairs */
			{  84,	20,  9, 50,   4}, /* Cure Mortal Wounds */

			/* Chants and Blessings (sval 2) */

			/* Exorcism and Dispelling (sval 3) */
			{  77,	21,  8, 45,   4}, /* Satisfy Hunger */
			{  81,	22,  6, 45,   4}, /* Orb of Draining */
			{  78,	23, 11, 45,   4}, /* Remove Curse */
			{  74,	23,  6, 50,   4}, /* Portal */
			{  82,	25,  8, 50,   5}, /* Sense Invisible */
			{  88,	27,  6, 50,   6}, /* Prayer */
			{  92,	30, 10, 80,   7}, /* Sacred Shield */
			{ 122,	36, 10, 50,  10}, /* Elemental Infusion */
			{  90,	41, 10, 60,  16}, /* Heal */

			/* Ethereal openings (sval 4) */

			/* Godly Insights... (sval 5) */
			{ 101,	30,  3, 50,  12}, /* Detect Monsters */
			{ 102,	35,  9, 70,  20}, /* Detection */
			{  86,	37,  9, 50,  24}, /* Sense Surroundings */
			{ 104,	43, 20, 70,  26}, /* Perception */
			{ 103,	45, 20, 70,  41}, /* Probing */

			/* Purifications and Healing (sval 6) */
			{ 114,	28,  4, 50,  10}, /* Disarm Trap */
			{ 123,	36,  7, 50,  23}, /* Sanctify for Battle */
			{ 107,	41, 19, 65,  34}, /* Healing */
			{ 109,	46, 40, 70,  45}, /* Restoration */
			{ 110,	46, 40, 70,  45}, /* Remembrance */

			/* Holy Infusions (sval 7) */

			/* Wrath of God (sval 8) */
			{ 117,	37, 10, 70,  20}, /* Ball of Light */
			{ 124,	40, 10, 50,  29}, /* Horn of Wrath */
			{ 118,	42, 12, 75,  37}, /* Holy Lance */
			{ 119,	45, 45, 70,  40}, /* Word of Destruction */
			{ 120,	49, 25, 60,  70}, /* Annihilation */

			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0}
		}
	},

	{
		/*** Druid ***/

		TV_DRUID_BOOK,  A_WIS,  REALM_NATURE,  1,  250,  600,

		/* 58 techniques in 9 books. */
		58,	{ 0, 9, 17, 25, 32, 37, 42, 47, 52, 58, 58 },

		{
			/* Call of the Wild (sval 0) */
			{ 128,	 1,  1, 22,   2}, /* detect life */
			{ 129,	 1,  1, 23,   2}, /* call light */
			{ 130,	 2,  1, 24,   2}, /* foraging */
			{ 132,	 3,  1, 25,   2}, /* combat poison */
			{ 131,	 3,  2, 25,   2}, /* blink */
			{ 133,	 4,  2, 25,   3}, /* lightning spark */
			{ 134,	 5,  3, 25,   3}, /* door destruction */
			{ 135,	 5,  3, 25,   3}, /* turn stone to mud */
			{ 136,	 6,  4, 27,   4}, /* ray of sunlight */

			/* Communion with Nature (sval 1) */
			{ 137,	 7,  4, 30,   5}, /* Cure poison */
			{ 138,	 7,  5, 30,   5}, /* frost bolt */
			{ 139,	 9,  5, 30,   5}, /* sleep creature */
			{ 140,	 9,  5, 30,   6}, /* frighten creature */
			{ 141,	11,  5, 30,   6}, /* detect trap/doors */
			{ 142,	11,  6, 35,   6}, /* cease small life */
			{ 143,	13,  6, 35,   6}, /* fire bolt */
			{ 145,	13,  9, 44,   7}, /* remove curse */

			/* Gifts of Nature (sval 2) */
			{ 146,	15,  8, 45,   7}, /* acid bolt */
			{ 147,	15,  8, 75,   8}, /* teleport monster */
			{ 148,	17, 10, 45,   8}, /* poison bolt */
			{ 149,	17,  5, 45,   8}, /* resist poison */
			{ 150,	19,  8, 75,   8}, /* earthquake */
			{ 151,	19,  7, 50,   8}, /* resist fire & cold */
			{ 152,	21, 14, 50,   8}, /* detect all */
			{ 153,	21,  1, 50,   8}, /* natural vitality */

			/* Book of Combat (sval 3) */
			{ 154,	22,  9, 55,   9}, /* resist acid & lightning */
			{ 155,	23, 13, 90,  11}, /* wither foe */
			{ 156,	23, 14, 60,   9}, /* disarm trap */
			{ 157,	25, 20, 85,  11}, /* identify */
			{ 158,	29, 50, 65,  12}, /* create athelas */
			{ 159,	33, 20, 80,  15}, /* raging storm */
			{ 160,	37, 22, 80,  26}, /* thunderclap */

			/* Radagast's Shapeshifts (sval 4) */
			{ 161,	15,  4, 50,  15}, /* become mouse */
			{ 162,	15,  7, 50,  15}, /* become ferret */
			{ 163,	18,  9, 50,  20}, /* become hound */
			{ 164,	20, 16, 75,  25}, /* become gazelle */
			{ 165,	25, 20, 85,  32}, /* become lion */

			/* Melian's Lore (sval 5) */
			{ 166,	20,  2, 20,  16}, /* detect evil */
			{ 167,	24,  8, 40,  20}, /* song of frightening */
			{ 168,	20, 10, 60,  20}, /* sense surroundings */
			{ 169,	25, 16, 60,  25}, /* sight beyond sight */
			{ 170,	39, 28, 60,  60}, /* herbal healing */

			/* Primal Forces (sval 6) */
			{ 171,	26, 12, 50,  40}, /* blizzard */
			{ 172,	35, 15, 70,  50}, /* trigger tsunami */
			{ 173,	38, 20, 85,  60}, /* volcanic eruption */
			{ 174,	40, 22, 60,  70}, /* molten lightning */
			{ 175,	45, 26, 60,  80}, /* starburst. */

			/* Bombadil's Songs (sval 7) */
			{ 176,	18,  9, 20,  35}, /* song of lulling */
			{ 177,	30,  9, 35,  50}, /* song of protection */
			{ 178,	30, 24, 40,  50}, /* song of dispelling */
			{ 179,	45, 40, 55,  80}, /* song of warding */
			{ 180,	45, 70, 65,  80}, /* song of renewal */

			/* Spirit of Yavanna (sval 8) */
			{ 181,	21,  9, 50,  20}, /* time blast */
			{ 182,	25, 18, 60,  17}, /* essence of speed */
			{ 183,	25, 20, 70,  22}, /* infusion */
			{ 184,	34, 25, 70,  27}, /* become elder ent */
			{ 185,	39, 28, 70,  40}, /* regain life */
			{ 186,	49, 40, 60, 140}, /* intervention of Yavanna */

			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}
		}
	},

	{
		/*** Necromancer ***/

		TV_NECRO_BOOK,  A_INT,  REALM_NECROMANTIC,  1,  200,  600,

		/* 58 rituals in 9 books. */
		58, { 0, 8, 16, 24, 30, 36, 42, 47, 53, 58, 58 },
		{
			/* Beginner's Curses (sval 0) */
			{ 192,	 1,  1, 22,   2}, /* magic bolt */
			{ 193,	 1,  1, 23,   2}, /* detect evil */
			{ 194,	 3,  1, 24,   2}, /* enhanced infravision */
			{ 195,	 3,  2, 25,   2}, /* break curse */
			{ 196,	 4,  3, 25,   3}, /* slow monster */
			{ 197,	 5,  3, 25,   3}, /* sleep monster */
			{ 198,	 5,  3, 25,   3}, /* horrify */
			{ 199,	 6,  3, 85,   5}, /* become bat */

			/* Dark Initiation (sval 1) */
			{ 200,	 7,  3, 27,   4}, /* door destruction */
			{ 201,	 8,  3, 30,   4}, /* dark bolt */
			{ 202,	 9,  6, 30,   4}, /* noxious fumes */
			{ 203,	 9,  3, 20,   4}, /* turn undead */
			{ 204,	11,  5, 30,   4}, /* turn evil */
			{ 205,	11,  4, 35,   5}, /* cure poison */
			{ 206,	12,  6, 45,   5}, /* dispel undead */
			{ 207,	14,  8, 50,   5}, /* dispel evil */

			/* Blood Novitiate (sval 2) */
			{ 208,	14,  5, 40,   6}, /* see invisible */
			{ 209,	15,  5, 75,   7}, /* shadow shifting */
			{ 210,	17,  8, 75,   6}, /* detect traps */
			{ 211,	17,  8, 75,   6}, /* detect doors/stairs */
			{ 212,	17,  8, 45,   6}, /* sleep monsters */
			{ 213,	19,  8, 45,   7}, /* slow monsters */
			{ 215,	21,  7, 50,   7}, /* death bolt */
			{ 216,	21,  6, 50,   8}, /* resist poison */

			/* Frightful Realms (sval 3) */
			{ 217,	23, 11, 50,   8}, /* Dispel Demons */
			{ 218,	23, 11, 55,   9}, /* dark spear */
			{ 219,	24, 11, 60,  10}, /* mana bolt */
			{ 221,	28, 16, 65,  12}, /* dark ball */
			{ 222,	33, 21, 70,  16}, /* stench of death */
			{ 220,	43, 25, 70,  21}, /* genocide */

			/* Mysteries of Angmar (sval 4) */
			{ 214,	19,  5, 50,  15}, /* detect magic */
			{ 223,	19, 10, 40,  15}, /* probing */
			{ 224,	19, 10, 40,  15}, /* shadow mapping */
			{ 250,	21, 10, 55,  23}, /* Mental Awareness */
			{ 225,	23, 18, 80,  28}, /* identify */
			{ 226,	27, 18, 50,  31}, /* shadow warping */

			/* Unholy Protection (sval 5) */
			{ 229,	25,  6, 40,  30}, /* heal any wound */
			{ 228,	30, 12, 50,  25}, /* resist acid and cold */
			{ 230,	36, 12, 60,  40}, /* protection from evil */
			{ 231,	38, 12, 60,  60}, /* black blessing */
			{ 232,	43, 20, 70,  90}, /* banish evil */
			{ 233,	47, 35, 70, 140}, /* shadow barrier */

			/* Life Force Mastery (sval 6) */
			{ 234,	31, 12, 75,  30}, /* detect all monsters */
			{ 235,	36, 15, 50,  35}, /* strike at life */
			{ 236,	38, 22, 60,  40}, /* orb of death */
			{ 237,	43, 25, 70,  75}, /* dispel life */
			{ 238,	46, 36, 70, 120}, /* vampiric drain */

			/* Metamorphoses(sval 7) */
			{ 239,	18,  9, 40,  20}, /* recharging */
			{ 240,	32, 12, 50,  35}, /* become werewolf */
			{ 241,	34, 12, 50,  40}, /* dispel curse */
			{ 242,	40, 16, 50,  60}, /* become vampire */
			{ 243,	40, 22, 50,  60}, /* haste self */
			{ 244,	44, 40, 70,  90}, /* prepare black breath */

			/* Necronomicon(sval 8) */
			{ 245,	31, 24, 45,  30}, /* word of destruction */
			{ 246,	36, 30, 50,  35}, /* teleport other */
			{ 247,	38, 32, 60,  40}, /* smash undead */
			{ 248,	42, 40, 70,  75}, /* bind undead */
			{ 249,	47, 40, 70, 120}, /* darkness storm */

			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}, 
			{   0,	99,  0,  0,   0}
		}
	},

	{
		/*** Assassin ***/

		TV_NECRO_BOOK,  A_INT,  REALM_NECROMANTIC,  3,  300, 1000,

		/* 29 rituals in 4 books. */
		29, { 0, 9, 9, 17, 17, 23, 23, 23, 29, 29, 29 },

		{
			{ 193,	 3,  1, 23,   1}, /* detect evil */
			{ 210,	 3,  1, 25,   1}, /* detect traps */
			{ 211,	 3,  1, 25,   1}, /* detect doors/stairs */
			{ 194,	 5,  1, 25,   1}, /* enhanced infravision */
			{ 192,	 5,  1, 65,   1}, /* magic bolt */
			{ 196,	 7,  4, 25,   2}, /* slow monster */
			{ 197,	 7,  4, 25,   2}, /* sleep monster */
			{ 198,	 9,  4, 25,   2}, /* horrify */
			{ 200,	11,  2, 25,   2}, /* door destruction */

			/* Dark Initiation (sval 1) */

			/* Blood Novitiate (sval 2) */
			{ 202,	13,  5, 30,   3}, /* noxious fumes */
			{ 205,	15,  3, 35,   3}, /* cure poison */
			{ 208,	17,  3, 40,   4}, /* see invisible */
			{ 209,	20,  4, 65,   4}, /* shadow shifting */
			{ 214,	23,  4, 50,   5}, /* detect magic */
			{ 216,	27,  7, 50,   5}, /* resist poison */
			{ 251,	33,  9, 60,   7}, /* slip into the shadows */
			{ 199,	38,  9, 65,   9}, /* become bat */

			/* Frightful Realms (sval 3) */

			/* Mysteries of Angmar (sval 4) */
			{ 234,	14,  4, 45,   7}, /* detect all monsters */
			{ 231,	18,  4, 45,  11}, /* black blessing */
			{ 223,	25,  8, 55,  14}, /* probing */
			{ 224,	29,  9, 60,  21}, /* shadow mapping */
			{ 227,	35, 22, 70,  27}, /* poison ammo */
			{ 225,	40, 25, 70,  25}, /* identify */

			/* Unholy Protection (sval 5) */

			/* Life Force Mastery (sval 6) */

			/* Metamorphoses(sval 7) */
			{  84,	26,  3, 50,  19}, /* Cure Mortal Wounds */
			{ 226,	33,  8, 50,  23}, /* shadow warping */
			{ 239,	36, 14, 50,  26}, /* recharging */
			{ 240,	40, 18, 50,  30}, /* become werewolf */
			{ 253,	44, 25, 60,  35}, /* rebalance weapon */
			{ 252,	48, 30, 60,  40}, /* bloodwrath */

			/* Necronomicon(sval 8) */

			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}, { 0, 99,  0, 0, 0},
			{ 0, 99,  0, 0, 0}
		}
	}
};



/*
 * Names of the spells (mage spells, priestly prayers, Nature magics,
 * and Necromantic spells).  Spells are listed by index (see table above 
 * for index assignments, and cmd5.c for the effects asociated with each 
 * index) -LM-
 */
cptr spell_names[255] =
{
	/* Magic for Beginners (sval 0) */
	"Magic Missile",						/* index 0 */
	"Detect Monsters",
	"Phase Door",
	"Light Area",
	"Combat Poison",
	"Cure Light Wounds",
	"Search for Treasure",			/* Rogue spell. */
	"Detect Objects",				/* Rogue spell. */
	"Stinking Cloud",

	/* Conjurings and Tricks (sval 1) */
	"Confuse Monster",					/* index 9 */
	"Lightning Bolt",
	"Door Destruction",
	"Sleep Monster",
	"Cure Poison",
	"Teleport Self",
	"Spear of Light",
	"Recharge Item I",
	"Cone of Cold",

	/* Incantations and Illusions (sval 2) */
	"Satisfy Hunger",						/* index 18 */
	"Magic Disarm",
	"Polymorph Other",
	"Identify",
	"Sleep Monsters",
	"Fire Bolt",
	"Slow Monster",
	"Tap Magical Energy",

	/* Sorcery and Evocations (sval 3) */
	"Frost Ball",						/* index 26 */
	"Recharge Item II",
	"Teleport Other",
	"Haste Self",
	"Fire Ball",
	"Hold Monsters",
	"Word of Destruction",

	/* Resistance of Scarabtarices (sval 4) */
	"Resist Fire",						/* index 33 */
	"Resist Cold",
	"Resist Acid",
	"Resist Poison",
	"Resistance",

	/* Mordenkainen's Escapes (sval 5) */
	"Door Creation",						/* index 38 */
	"Stair Creation",
	"Teleport Level",
	"Word of Recall",
	"Dimension Door",

	/* Kelek's Grimoire of Power (sval 6) */
	"Detect Evil",						/* index 43 */
	"Detect Enchantment",
	"Earthquake",
	"Beguiling",
	"Starburst",

	/* Tenser's transformations... (sval 7) */
	"Clear Mind",						/* index 48 */
	"Shield",
	"Recharge Item III",
	"Essence of Speed",
	"Strengthen Defenses",

	/* Raal's Tome of Destruction (sval 8) */
	"Acid Bolt",						/* index 53 */
	"Cloud Kill",
	"Acid Ball",
	"Ice Storm",
	"Meteor Swarm",
	"Mana Storm",

	"Hit and Run",							/* index 59 */
	"Day of Misrule",
	"(blank)",
	"(blank)",
	"(blank)",



	/* Novice's Handbook (sval 0) */
	"Detect Evil",						/* index 64 */
	"Cure Light Wounds",
	"Bless",
	"Remove Fear",
	"Call Light",
	"Find Traps",
	"Detect Doors/Stairs",
	"Slow Poison",

	/* Words of Wisdom (sval 1) */
	"Cure Serious Wounds",					/* index 72 */
	"Scare Monster",
	"Portal",
	"Chant",
	"Sanctuary",
	"Satisfy Hunger",
	"Remove Curse",
	"Resist Heat and Cold",

	/* Chants and Blessings (sval 2) */
	"Neutralize Poison",					/* index 80 */
	"Orb of Draining",
	"Sense Invisible",
	"Protection from Evil",
	"Cure Mortal Wounds",
	"Earthquake",
	"Sense Surroundings",
	"Turn Undead",
	"Prayer",

	/* Exorcism and Dispelling (sval 3) */
	"Dispel Undead",						/* index 89 */
	"Heal",
	"Dispel Evil",
	"Sacred Shield",
	"Glyph of Warding",
	"Holy Word",

	/* Ethereal openings (sval 4) */
	"Blink",							/* index 95 */
	"Teleport Self",
	"Teleport Other",
	"Teleport Level",
	"Word of Recall",
	"Alter Reality",

	/* Godly Insights... (sval 5) */
	"Detect Monsters",					/* index 101 */
	"Detection",
	"Probing",
	"Perception",
	"Clairvoyance",

	/* Purifications and Healing (sval 6) */
	"Banishment",						/* index 106 */
	"Healing",
	"Sacred Knowledge",
	"Restoration",
	"Remembrance",

	/* Holy Infusions (sval 7) */
	"Unbarring Ways",						/* index 111 */
	"Recharging",
	"Dispel Curse",
	"Disarm Trap",
	"Holding",
	"Enchant Weapon or Armour",

	/* Wrath of God (sval 8) */
	"Ball of Light",						/* index 117 */
	"Holy Lance",
	"Word of Destruction",
	"Annihilation",
	"Call on Varda",

	"Elemental Infusion",							/* index 122 */
	"Sanctify for Battle",
	"Horn of Wrath",
	"(blank)",
	"(blank)",
	"(blank)",



	/* Call of the Wild (sval 0) */
	"Detect Life",						/* index 128 */
	"Call Light",
	"Foraging",
	"Blink",
	"Combat Poison",
	"Lightning Spark",
	"Door Destruction",
	"Turn Stone to Mud",
	"Ray of Sunlight",

	/* Communion with Nature (sval 1) */
	"Cure Poison",						/* index 137 */
	"Frost Bolt",
	"Sleep Creature",
	"Frighten Creature",
	"Detect Traps/Doors",
	"Snuff Small Life",
	"Fire Bolt",
	"Heroism",
	"Remove Curse",

	/* Gifts of Nature (sval 2) */
	"Acid Bolt",						/* index 146 */
	"Teleport Monster",
	"Poison Bolt",
	"Resist Poison",
	"Earthquake",
	"Resist Fire & Cold",
	"Detect All",
	"Natural Vitality",

	/* Book of Combat (sval 3) */
	"Resist Acid & Lightning",				/* index 154 */
	"Wither Foe",
	"Disarm Trap",
	"Identify",
	"Create Athelas",
	"Raging Storm",
	"Thunderclap",

	/* Radagast's Shapeshifts (sval 4) */
	"Become Mouse",						/* index 161 */
	"Become Ferret",
	"Become Hound",
	"Become Gazelle",
	"Become Lion",

	/* Melian's Lore (sval 5) */
	"Detect Evil",						/* index 166 */
	"Song of Frightening",
	"Sense Surroundings",
	"Sight beyond Sight",
	"Herbal Healing",

	/* Primal Forces (sval 6) */
	"Blizzard",							/* index 171 */
	"Trigger Tsunami",
	"Volcanic Eruption",
	"Molten Lightning",
	"Starburst",

	/* Bombadil's Songs (sval 7) */
	"Song of Lulling",					/* index 176 */
	"Song of Protection",
	"Song of Dispelling",
	"Song of Warding",
	"Song of Renewal",

	/* Spirit of Yavanna (sval 8) */
	"Timeblast",						/* index 181 */
	"Essence of Speed",
	"Infusion",
	"Become Elder Ent",
	"Regain Life",
	"Intervention of Yavanna",

	"Creature Knowledge",					/* index 187 */
	"(blank)",
	"(blank)",
	"(blank)",
	"(blank)",



	/* Beginner's Curses (sval 0) */
	"Magic Bolt",						/* index 192 */
	"Detect Evil",
	"Enhanced Infravision",
	"Break Curse",
	"Slow Monster",
	"Sleep Monster",
	"Horrify",
	"Become Bat",

	/* Dark Initiation (sval 1) */
	"Door Destruction",					/* index 200 */
	"Dark Bolt",
	"Noxious Fumes",
	"Turn Undead",
	"Turn Evil",
	"Cure Poison",
	"Dispel Undead",
	"Dispel Evil",

	/* Blood Novitiate (sval 2) */
	"See Invisible",						/* index 208 */
	"Shadow Shifting",
	"Detect Traps",
	"Detect Doors/Stairs",
	"Sleep Monsters",
	"Slow Monsters",
	"Detect Magic",
	"Death Bolt",
	"Resist Poison",

	/* Frightful Realms (sval 3) */
	"Exorcise Demons",					/* index 217 */
	"Dark Spear",
	"Mana Bolt",
	"Genocide",
	"Dark Ball",
	"Stench of Death",

	/* Mysteries of Angmar (sval 4) */
	"Probing",							/* index 223 */
	"Shadow Mapping",
	"Identify",
	"Shadow Warping",
	"Poison Ammo",

	/* Unholy Protection (sval 5) */
	"Resist Acid and Cold",					/* index 229 */
	"Cure Any Wound",
	"Protection From Evil",
	"Black Blessing",
	"Banish Evil",
	"Shadow Barrier",

	/* Life Force Mastery (sval 6) */
	"Detect All Monsters",					/* index 234 */
	"Strike at Life",
	"Orb of Death",
	"Dispel Life",
	"Vampiric Drain",

	/* Metamorphoses (sval 7) */
	"Recharging",						/* index 239 */
	"Become Werewolf",
	"Dispel Curse",
	"Become Vampire",
	"Haste Self",
	"Prepare Black Breath",

	/* Necronomicon (sval 8) */
	"Word of Destruction",					/* index 245 */
	"Teleport Other",
	"Smash Undead",
	"Bind Undead",
	"Darkness Storm",

	"Mental Awareness",							/* index 250 */
	"Slip into the Shadows",
	"Bloodwrath",
	"Rebalance Weapon",
	"(blank)"
};


/*
 * Conversion of plusses to Deadliness to a percentage added to damage.  
 * Much of this table is not intended ever to be used, and is included 
 * only to handle possible inflation elsewhere. -LM-
 */
byte deadliness_conversion[151] = 
{
	  0, 
	  5,  10,  14,  18,  22,  26,  30,  33,  36,  39, 
	 42,  45,  48,  51,  54,  57,  60,  63,  66,  69, 
	 72,  75,  78,  81,  84,  87,  90,  93,  96,  99, 
	102, 104, 107, 109, 112, 114, 117, 119, 122, 124, 
	127, 129, 132, 134, 137, 139, 142, 144, 147, 149, 
	152, 154, 157, 159, 162, 164, 167, 169, 172, 174, 
	176, 178, 180, 182, 184, 186, 188, 190, 192, 194, 
	196, 198, 200, 202, 204, 206, 208, 210, 212, 214, 
	216, 218, 220, 222, 224, 226, 228, 230, 232, 234, 
	236, 238, 240, 242, 244, 246, 248, 250, 251, 253, 

	255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
	255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
	255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
	255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 
	255, 255, 255, 255, 255, 255, 255, 255, 255, 255
};

/*
 * Each chest has a certain set of traps, determined by pval (which 
 * also controls the quality of treasure).
 * Table revised for Oangband.
 */
/* Note that traps can actually be 4 entries past the nominal "best" comment */
int chest_traps[100] =
{
	0,					/* 0 == empty */
	(CHEST_POISON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	0,
	(CHEST_POISON),
	(CHEST_POISON),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_POISON),
	(CHEST_SCATTER),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_SUMMON),			/* 15 == best small wooden */
	0,
	(CHEST_LOSE_STR),
	(CHEST_SCATTER),
	(CHEST_PARALYZE),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_SUMMON),
	(CHEST_PARALYZE),
	(CHEST_LOSE_STR),
	(CHEST_LOSE_CON),
	(CHEST_EXPLODE),			/* 25 == best large wooden */
	0,
	(CHEST_E_SUMMON),
	(CHEST_POISON | CHEST_LOSE_CON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_POISON | CHEST_SUMMON),
	(CHEST_E_SUMMON),
	(CHEST_EXPLODE),
	(CHEST_BIRD_STORM),	
	0,
	(CHEST_SUMMON),
	(CHEST_EXPLODE),
	(CHEST_E_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON), /* 40 == best small iron */
	(CHEST_BIRD_STORM),
	(CHEST_EXPLODE),
	(CHEST_BIRD_STORM),
	(CHEST_E_SUMMON),
	(CHEST_BIRD_STORM),
	0,
	(CHEST_E_SUMMON),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_STR),
	(CHEST_E_SUMMON),	/* 50 == best large iron */
	(CHEST_BIRD_STORM),
	(CHEST_E_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_BIRD_STORM),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_E_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_SCATTER),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_H_SUMMON),	/* 65 == best small steel */
	(CHEST_BIRD_STORM),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_LOSE_CON),
	(CHEST_BIRD_STORM),
	(CHEST_SCATTER),
	(CHEST_H_SUMMON),
	(CHEST_POISON | CHEST_PARALYZE | CHEST_SCATTER),
	(CHEST_H_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),	/* 75 == best large steel */
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_EXPLODE | CHEST_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_H_SUMMON | CHEST_SCATTER),
	(CHEST_H_SUMMON),
	(CHEST_BIRD_STORM),
	(CHEST_LOSE_STR | CHEST_LOSE_CON),
	(CHEST_H_SUMMON),	/* 95 == best small jeweled */
	(CHEST_H_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_E_SUMMON),
	(CHEST_H_SUMMON),
	(CHEST_RUNES_OF_EVIL),
	(CHEST_POISON | CHEST_PARALYZE),
	(CHEST_H_SUMMON),
	(CHEST_RUNES_OF_EVIL),
	(CHEST_H_SUMMON),
	(CHEST_RUNES_OF_EVIL),	/* 95 == best large jeweled */
	(CHEST_RUNES_OF_EVIL),
	(CHEST_H_SUMMON),
	(CHEST_RUNES_OF_EVIL),
	(CHEST_RUNES_OF_EVIL | CHEST_EXPLODE),
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
		"Theurgist",
		"Canon",
		"Lama",
		"Patriarch",
		"High Priest",
		"Priest Lord",
	},

	/* Rogue */
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
		"Guildmaster",
	},

	/* Ranger */
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

	/* Paladin */
	{
		"Gallant",
		"Keeper",
		"Protector",
		"Defender",
		"Warder",
		"Knight",
		"Guardian",
		"Crusader",
		"Paladin",
		"Paladin Lord",
	},

	/* Druid */
	{
		"Wanderer",
		"Tamer",
		"Nurturer",
		"Gardener",
		"Creator",
		"Earth Warder",
		"Windrider",
		"Stormwielder",
		"High Mystic",
		"Mystic Lord",
	},

	/* Necromancer */
	{
		"Acolyte",
		"Curser",
		"Dark Student",
		"Initiate",
		"Slavemaster",
		"Summoner",
		"Controller",
		"Commander",
		"Dark Master",
		"Night Lord",
	},
	/* Assassin */
	{
		"Trainee",
		"Myrmidon",
		"Initiate",
		"Knifer",
		"Bladesman",
		"Hashishin",
		"Black Dagger",
		"Shadowstrike",
		"Assassinator",
		"Death Lord",
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
 */
cptr window_flag_desc[32] =
{
	"Display inven/equip",
	"Display equip/inven",
	"Display player (basic)",
	"Display player (extra)",
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
 * Options -- textual names (where defined)
 */
cptr option_text[OPT_MAX] =
{
	"rogue_like_commands",		/* OPT_rogue_like_commands */	   /*0*/
	"quick_messages",			/* OPT_quick_messages */
	"floor_query_flag",			/* OPT_floor_query_flag */
	"carry_query_flag",			/* OPT_carry_query_flag */
	"use_old_target",			/* OPT_use_old_target */
	"always_pickup",			/* OPT_always_pickup */
	"always_repeat",			/* OPT_always_repeat */
	"depth_in_feet",			/* OPT_depth_in_feet */
	"stack_force_notes",		/* OPT_stack_force_notes */
	"stack_force_costs",		/* OPT_stack_force_costs */
	"show_labels",				/* OPT_show_labels */
	"show_weights",				/* OPT_show_weights */
	"show_choices",				/* OPT_show_choices */
	"show_details",				/* OPT_show_details */
	"use_metric",				/* OPT_metric */
	"show_flavors",				/* OPT_flavors */
	"run_ignore_stairs",		/* OPT_run_ignore_stairs */
	"run_ignore_doors",			/* OPT_run_ignore_doors */
	"run_cut_corners",			/* OPT_run_cut_corners */
	"run_use_corners",			/* OPT_run_use_corners */
	"disturb_move",				/* OPT_disturb_move */  /*20*/
	"disturb_near",				/* OPT_disturb_near */
	"disturb_panel",			/* OPT_disturb_panel */
	"disturb_state",			/* OPT_disturb_state */
	"disturb_minor",			/* OPT_disturb_minor */
	"disturb_other",			/* OPT_disturb_other */
	"alert_hitpoint",			/* OPT_alert_hitpoint */
	"alert_failure",			/* OPT_alert_failure */
	"verify_destroy",			/* OPT_verify_destroy */
	"verify_special",			/* OPT_verify_special */
	"ring_bell",				/* OPT_ring_bell */
	"verify_destroy_junk",		/* OPT_verify_destroy_junk */
	"auto_haggle",				/* OPT_auto_haggle */
	"auto_scum",				/* OPT_auto_scum */
	"easy_open",				/* OPT_easy_open   -TNB- */
	"easy_disarm",				/* OPT_easy_disarm   -TNB- */
	"expand_look",				/* OPT_expand_look */
	"expand_list",				/* OPT_expand_list */
	"view_perma_grids",			/* OPT_view_perma_grids */
	"view_torch_grids",			/* OPT_view_torch_grids */
	"auto_more",				/* OPT_auto_more */
	"dungeon_stair",			/* OPT_dungeon_stair */
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"smart_cheat",				/* OPT_smart_cheat */
	"view_reduce_lite",			/* OPT_view_reduce_lite */
	"hidden_player",			/* OPT_hidden_player */
	"avoid_abort",				/* OPT_avoid_abort */
	"avoid_other",				/* OPT_avoid_other */
	"flush_failure",			/* OPT_flush_failure */
	"flush_disturb",			/* OPT_flush_disturb */
	"center_player",			/* OPT_center_player */
	"fresh_before",				/* OPT_fresh_before */
	"fresh_after",				/* OPT_fresh_after */
	"center_running",			/* OPT_center_running */
	"compress_savefile",		/* OPT_compress_savefile */
	"hilite_player",			/* OPT_hilite_player */
	"view_yellow_lite",			/* OPT_view_yellow_lite */  /*60*/
	"view_bright_lite",			/* OPT_view_bright_lite */
	"view_granite_lite",		/* OPT_view_granite_lite */
	"view_special_lite",			/* OPT_view_special_lite */
	NULL,
	NULL,
	NULL,		
	"show_piles",				/* OPT_show_piles */
	NULL,		
	"show_detect",				/* OPT_show_detect */
	"disturb_trap_detect",		/* OPT_disturb_trap_detect */
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL, /*80*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL, /*100*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL, /*120*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,
	NULL,
	"birth_point_based",		      /* OPT_birth_point_based */
	"birth_auto_roller",		/* OPT_birth_auto_roller */
	NULL,	 
	"birth_preserve",		/* OPT_birth_preserve */	   
	NULL,	 
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL, /*140*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		
	NULL,		
	NULL,		
	NULL,		
	"cheat_peak",				/* OPT_cheat_peek */ /*160*/
	"cheat_hear",				/* OPT_cheat_hear */
	"cheat_room",				/* OPT_cheat_room */
	"cheat_xtra",				/* OPT_cheat_xtra */
	"cheat_know",				/* OPT_cheat_know */
	"cheat_live",				/* OPT_cheat_live */
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL, /*180*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,
	"adult_point_based",		      /* OPT_adult_point_based */
	"adult_auto_roller",		/* OPT_adult_auto_roller */	     
	NULL,
	"adult_preserve",		/* OPT_adult_preserve */
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*220*/
	NULL,	
	NULL,	
	NULL,	
	"score_peek",				/* OPT_score_peek */
	"score_hear",				/* OPT_score_hear */
	"score_room",				/* OPT_score_room */
	"score_xtra",				/* OPT_score_xtra */
	"score_know",				/* OPT_score_know */
	"score_live",				/* OPT_score_live */
	NULL,	
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*240*/
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL
};


/*
 * Options -- descriptions (where defined)
 */
cptr option_desc[OPT_MAX] =
{
	"Rogue-like commands",				/* OPT_rogue_like_commands */ /*0*/
	"Activate quick messages",					/* OPT_quick_messages */
	"Prompt for floor item selection",			/* OPT_floor_query_flag */
	"Prompt before picking things up",			/* OPT_carry_query_flag */
	"Use old target by default",				/* OPT_use_old_target */
	"Pick things up by default",				/* OPT_always_pickup */
	"Repeat obvious commands",					/* OPT_always_repeat */
	"Show dungeon level in feet (or meters)",	/* OPT_depth_in_feet */
	"Merge inscriptions when stacking",			/* OPT_stack_force_notes */
	"Merge discounts when stacking",			/* OPT_stack_force_costs */
	"Show labels in equipment listings",		/* OPT_show_labels */
	"Show weights in all object listings",		/* OPT_show_weights */
	"Show choices in inven/equip windows",		/* OPT_show_choices */
	"Show details in monster descriptions",		/* OPT_show_details */
	"Use metric (SI) measurements",				/* OPT_metric */
	"Show flavors in object descriptions",		/* OPT_show_flavors */
	"When running, ignore stairs",				/* OPT_run_ignore_stairs */
	"When running, ignore doors",				/* OPT_run_ignore_doors */
	"When running, cut corners",				/* OPT_run_cut_corners */
	"When running, use corners",				/* OPT_run_use_corners */
	"Disturb whenever any monster moves",		/* OPT_disturb_move */	/*20*/
	"Disturb whenever viewable monster moves",	/* OPT_disturb_near */
	"Disturb whenever map panel changes",		/* OPT_disturb_panel */
	"Disturb whenever player state changes",	/* OPT_disturb_state */
	"Disturb whenever boring things happen",	/* OPT_disturb_minor */
	"Disturb whenever various things happen",	/* OPT_disturb_other */
	"Alert user to critical hitpoints",			/* OPT_alert_hitpoint */
	"Alert user to various failures",			/* OPT_alert_failure */
	"Verify destruction of objects",			/* OPT_verify_destroy */
	"Verify use of special commands",			/* OPT_verify_special */
	"Audible bell (on errors, etc)",			/* OPT_ring_bell */
	"Verify destruction of worthless objects",	/* OPT_verify_destroy_junk */
	"Auto-haggle in stores",					/* OPT_auto_haggle */
	"Auto-scum for good levels",				/* OPT_auto_scum */
	"Open and close doors automatically",		/* OPT_easy_open  -TBN- */
	"Disarm traps automatically",				/* OPT_easy_disarm   -TNB- */
	"Expand the power of the look command",		/* OPT_expand_look */
	"Expand the power of the list commands",	/* OPT_expand_list */
	"Map remembers all perma-lit grids",		/* OPT_view_perma_grids */
	"Map remembers all torch-lit grids",		/* OPT_view_torch_grids */
	"Automatically clear '-more-' prompts",		/* OPT_auto_more */	
	"Generate dungeons with connected stairs",	/* OPT_dungeon_stair */
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"Monsters exploit players weaknesses",		/* OPT_smart_cheat */
	"Reduce light radius when running",			/* OPT_view_reduce_lite */
	"Hide player symbol when running",			/* OPT_hidden_player */
	"Avoid checking for user abort",			/* OPT_avoid_abort */
	"Avoid processing special colors",			/* OPT_avoid_other */
	"Flush input on various failures",			/* OPT_flush_failure */
	"Flush input whenever disturbed",			/* OPT_flush_disturb */
	"Keep the player centered (slow)",			/* OPT_center_player */
	"Flush output before every command",		/* OPT_fresh_before */
	"Flush output after various things",		/* OPT_fresh_after */
	"Keep player centered while running (slow)",/* OPT_center_running */
	"Compress messages in savefiles",			/* OPT_compress_savefile */
	"Highlight the player with the cursor",		/* OPT_hilite_player */
	"Use special colors for torch lite",		/* OPT_view_yellow_lite */ /*60*/
	"Use special colors for field of view",		/* OPT_view_bright_lite */
	"Use special colors for wall grids",		/* OPT_view_granite_lite */
	"Use special colors for floor grids",		/* OPT_view_special_lite */
	NULL,
	NULL,
	NULL,		
	"Show stacks using special attr/char",		/* OPT_show_piles */
	NULL,		
	"Show region affected by using detection spells", /* OPT_show_detect */
	"Disturb when leaving last trap detect area", /* OPT_disturb_trap_detect */
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,  /*80*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,		
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,  /*100*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,  /*120*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,
	NULL,
	"Birth: Use point based character generation",		 /* OPT_birth_point_based */
	"Birth: Use Autoroller if rolling for stats",		 /* OPT_birth_auto_roller */
	NULL,
	"Birth: No special feelings/artifacts preserved",  /* OPT_birth_preserve */	 
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,	 
	NULL,
	NULL,
	NULL, /*140*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,
	NULL,
	NULL,
	NULL,		
	"Cheat: Peek into object creation",		/* OPT_cheat_peek */ /*160*/
	"Cheat: Peek into monster creation",		/* OPT_cheat_hear */
	"Cheat: Peek into dungeon creation",		/* OPT_cheat_room */
	"Cheat: Peek into something else",		/* OPT_cheat_xtra */
	"Cheat: Know complete monster info",		/* OPT_cheat_know */
	"Cheat: Allow player to avoid death",		/* OPT_cheat_live */
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL, /*180*/
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,		NULL,		NULL,		NULL,		NULL,
	NULL,
	"Adult: Use point based character generation",		 /* OPT_adult_point_based */
	"Adult: Use Autoroller if rolling for stats",		 /* OPT_adult_auto_roller */
	NULL,
	"Adult: Artifacts preserved & no special feelings",  /* OPT_adult_preserve */	   
	NULL,
	NULL,
	NULL,
	NULL, 
	NULL, 
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*220*/
	NULL,
	NULL,
	NULL,
	"Score: Peek into object creation",			/* OPT_score_peek */
	"Score: Peek into monster creation",		/* OPT_score_hear */
	"Score: Peek into dungeon creation",		/* OPT_score_room */
	"Score: Peek into something else",			/* OPT_score_xtra */
	"Score: Know complete monster info",		/* OPT_score_know */
	"Score: Allow player to avoid death",		/* OPT_score_live */
	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*240*/
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL
};


/*
 * Options -- normal values
 */
bool option_norm[OPT_MAX] =
{
	FALSE,		/* OPT_rogue_like_commands */	/*0*/
	FALSE,		/* OPT_quick_messages */
	TRUE,		/* OPT_floor_query_flag */
	FALSE,		/* OPT_carry_query_flag */
	FALSE,		/* OPT_use_old_target */
	TRUE,		/* OPT_always_pickup */
	TRUE,		/* OPT_always_repeat */
	TRUE,		/* OPT_depth_in_feet */
	TRUE,		/* OPT_stack_force_notes */
	FALSE,		/* OPT_stack_force_costs */
	TRUE,		/* OPT_show_labels */
	TRUE,		/* OPT_show_weights */
	TRUE,		/* OPT_show_choices */
	TRUE,		/* OPT_show_details */
	FALSE,		/* OPT_metric */
	TRUE,		/* OPT_show_flavors */
	TRUE,		/* OPT_run_ignore_stairs */
	TRUE,		/* OPT_run_ignore_doors */
	TRUE,		/* OPT_run_cut_corners */
	TRUE,		/* OPT_run_use_corners */
	TRUE,		/* OPT_disturb_move */ /*20*/
	TRUE,		/* OPT_disturb_near */
	TRUE,		/* OPT_disturb_panel */
	TRUE,		/* OPT_disturb_state */
	TRUE,		/* OPT_disturb_minor */
	FALSE,		/* OPT_disturb_other */
	TRUE,		/* OPT_alert_hitpoint */
	FALSE,		/* OPT_alert_failure */
	TRUE,		/* OPT_verify_destroy */
	TRUE,		/* OPT_verify_special */
	TRUE,		/* OPT_ring_bell */
	TRUE,		/* OPT_verify_destroy_junk */
	TRUE,		/* OPT_auto_haggle */
	FALSE,		/* OPT_auto_scum */
	TRUE,		/* OPT_easy_open */
	FALSE,		/* OPT_easy_disarm */
	TRUE,		/* OPT_expand_look */
	FALSE,		/* OPT_expand_list */
	TRUE,		/* OPT_view_perma_grids */
	TRUE,		/* OPT_view_torch_grids */
	FALSE,		/* OPT_auto_more */
	TRUE,		/* OPT_dungeon_stair */
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,		/* OPT_smart_cheat */
	FALSE,		/* OPT_view_reduce_lite */
	FALSE,		/* OPT_hidden_player */
	FALSE,		/* OPT_avoid_abort */
	FALSE,		/* OPT_avoid_other */
	TRUE,		/* OPT_flush_failure */
	FALSE,		/* OPT_flush_disturb */
	FALSE,		/* OPT_center_player */
	TRUE,		/* OPT_fresh_before */
	FALSE,		/* OPT_fresh_after */
	FALSE,		/* OPT_center_running */
	FALSE,		/* OPT_compress_savefile */
	FALSE,		/* OPT_hilite_player */
	TRUE,		/* OPT_view_yellow_lite */	 /*60*/
	TRUE,		/* OPT_view_bright_lite */
	FALSE,		/* OPT_view_granite_lite */
	TRUE,		/* OPT_view_special_lite */
	FALSE,
	FALSE,
	FALSE,		
	FALSE,		/* OPT_show_piles */
	FALSE,		
	TRUE,			  /* OPT_show_detect */
	TRUE,		/* OPT_disturb_trap_detect */
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,	/*80*/
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,	/*100*/
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,	/*120*/
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,
	FALSE,
	TRUE,		 /* OPT_birth_point_based */
	TRUE,		 /* OPT_birth_auto_roller */
	FALSE,
	FALSE,	/* OPT_birth_preserve */      
	FALSE,
	FALSE,
	FALSE,
	FALSE,
	FALSE,	  
	FALSE,	  
	FALSE,
	FALSE,
	FALSE, /*140*/
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		
	FALSE,		/* OPT_cheat_peek */  /*160*/
	FALSE,		/* OPT_cheat_hear */
	FALSE,		/* OPT_cheat_room */
	FALSE,		/* OPT_cheat_xtra */
	FALSE,		/* OPT_cheat_know */
	FALSE,		/* OPT_cheat_live */
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE, /*180*/
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,		FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,
	TRUE,	/* OPT_adult_point_based */
	TRUE,	/* OPT_adult_auto_roller */
	FALSE,
	FALSE,	/* OPT_adult_preserve */      
	FALSE,
	FALSE,
	FALSE,
	FALSE, 
	FALSE, 
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE, /*220*/
	FALSE,
	FALSE,
	FALSE,
	FALSE,		/* OPT_score_peek */
	FALSE,		/* OPT_score_hear */
	FALSE,		/* OPT_score_room */
	FALSE,		/* OPT_score_xtra */
	FALSE,		/* OPT_score_know */
	FALSE,		/* OPT_score_live */
	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE, /*240*/
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE,
	FALSE,	FALSE,	FALSE,	FALSE,	FALSE
};


/*
 * Option screen interface
 */
byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER] =
{
	/*** User-Interface ***/

	{
		OPT_rogue_like_commands,
		OPT_quick_messages,
		OPT_floor_query_flag,
		OPT_carry_query_flag,
		OPT_use_old_target,
		OPT_always_pickup,
		OPT_always_repeat,
		OPT_depth_in_feet,
		OPT_stack_force_notes,
		OPT_stack_force_costs,
		OPT_show_labels,
		OPT_show_weights,
		OPT_show_choices,
		OPT_show_details,
		OPT_metric,
		OPT_show_flavors,
		OPT_show_detect,
		255,
		255,
		255
	},

	/*** Disturbance ***/

	{
		OPT_run_ignore_stairs,
		OPT_run_ignore_doors,
		OPT_run_cut_corners,
		OPT_run_use_corners,
		OPT_disturb_move,
		OPT_disturb_near,
		OPT_disturb_panel,
		OPT_disturb_trap_detect,
		OPT_disturb_state,
		OPT_disturb_minor,
		OPT_disturb_other,
		OPT_alert_hitpoint,
		OPT_alert_failure,
		OPT_verify_destroy,
		OPT_verify_destroy_junk,
		OPT_verify_special,
		OPT_ring_bell,
		OPT_auto_more,
		255,
		255
	},

	/*** Game-Play ***/

	{
		OPT_auto_haggle,
		OPT_auto_scum,
		OPT_easy_open,
		OPT_easy_disarm,
		OPT_expand_look,
		OPT_expand_list,
		OPT_view_perma_grids,
		OPT_view_torch_grids,
		255,
		OPT_dungeon_stair,
		255,
		255,
		255,
		255,
		255,
		OPT_smart_cheat,
		255,
		255,
		255,
		255
	},

	/*** Efficiency and Lighting ***/

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
		OPT_hilite_player,
		OPT_view_yellow_lite,
		OPT_view_bright_lite,
		OPT_view_granite_lite,
		OPT_view_special_lite,
		OPT_center_player,
		OPT_center_running,
		OPT_show_piles,
		255,
		255,
		255
	},
	/*** Birth ***/

	{
		OPT_birth_point_based, 
		OPT_birth_auto_roller,	      
		OPT_birth_preserve, 
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
		255,
		255,
		255,
		255,
		255,
		255
	},

	/*** Cheat ***/

	{
	  OPT_cheat_peek,
	  OPT_cheat_hear,
	  OPT_cheat_room,
	  OPT_cheat_xtra,
	  OPT_cheat_know,
	  OPT_cheat_live,
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
	  255,
	  255,
	  255
	}
};

/* Table of Druid blows. -LM- */
druid_blows d_blow[NUM_D_BLOWS] =
{
	{ "punch",			 1, 5 },
	{ "kick",			 2, 4 },
	{ "knee",			 1,12 },
	{ "chop",			 2, 7 },
	{ "uppercut",		 3, 6 },
	{ "boot",			 3, 9 },
	{ "bang on",		 6, 4 },
	{ "slam",			 4, 9 },
	{ "grapple with",	13, 3 },
	{ "hammer",		 9, 6 },
	{ "head butt",		 3,24 },
	{ "strangle",		 8,10 },
	{ "roundhouse kick",	 5,19 },
	{ "assault",		10,11 },
	{ "crush",			11,11 },
	{ "double-kick",		21, 6 },
	{ "thunderclap belt",	 8,19 },
	{ "blizzard gouge",	14,11 },
	{ "tsunami whirl",	 7,26 },
	{ "stormwind chop",	10,22 }
};

cptr feel_text[FEEL_MAX] =
{
	NULL,		 /* FEEL_NONE */
	"broken",	 /* FEEL_BROKEN */
	"terrible",	 /* FEEL_TERRIBLE */
	"worthless",	 /* FEEL_WORTHLESS */
	"cursed",	 /* FEEL_CURSED */
	"uncursed",	 /* FEEL_UNCURSED */
	"average",	 /* FEEL_AVERAGE */
	"good",		 /* FEEL_GOOD */
	"excellent",	 /* FEEL_EXCELLENT */
	"special",	 /* FEEL_SPECIAL */
};

byte mana_cost_RF4[32]=
{
	1,			/* RF4_SHIEIK */
	0,			/* RF4_LASH */
	0,			/* RF4_BOULDER */
	0,			/* RF4_SHOT */
	0,			/* RF4_ARROW */
	0,			/* RF4_BOLT */
	0,			/* RF4_MISSL */
	0,			/* RF4_PMISSLE */
	0,			/* RF4_BRTH_ACID */
	0,			/* RF4_BRTH_ELEC */
	0,			/* RF4_BRTH_FIRE */
	0,			/* RF4_BRTH_COLD */
	0,			/* RF4_BRTH_POIS */
	0,			/* RF4_BRTH_PLAS */
	0,			/* RF4_BRTH_LITE */
	0,			/* RF4_BRTH_DARK */
	0,			/* RF4_BRTH_CONFU */
	0,			/* RF4_BRTH_SOUND */
	0,			/* RF4_BRTH_SHARD */
	0,			/* RF4_BRTH_INER */
	0,			/* RF4_BRTH_GRAV */
	0,			/* RF4_BRTH_FORCE */
	0,			/* RF4_BRTH_NEXUS */
	0,			/* RF4_BRTH_NETHR */
	0,			/* RF4_BRTH_CHAOS */
	0,			/* RF4_BRTH_DISEN */
	0,			/* RF4_BRTH_TIME */
	0,			/* RF4_XXX2 */
	0,			/* RF4_XXX3 */
	0,			/* RF4_XXX4 */
	0,			/* RF4_XXX5 */
	0			/* RF4_XXX6 */
};

byte mana_cost_RF5[32]=
{
	4,			/* RF5_BALL_ACID */
	4,			/* RF5_BALL_ELEC */
	4,			/* RF5_BALL_FIRE */
	4,			/* RF5_BALL_COLD */
	4,			/* RF5_BALL_POIS */
	5, 			/* RF5_BALL_LITE */
	5, 			/* RF5_BALL_DARK */
	6, 			/* RF5_BALL_CONFU */
	4, 			/* RF5_BALL_SOUND */
	4, 			/* RF5_BALL_SHARD */
	5, 			/* RF5_BALL_STORM */
	6, 			/* RF5_BALL_NETHR */
	7, 			/* RF5_BALL_CHAOS */
	5, 			/* RF5_BALL_MANA */
	0, 			/* RF5_XXX1 */
	0, 			/* RF5_XXX2 */
	4, 			/* RF5_BOLT_ACID */
	4, 			/* RF5_BOLT_ELEC */
	4, 			/* RF5_BOLT_FIRE */
	4, 			/* RF5_BOLT_COLD */
	4, 			/* RF5_BOLT_POIS */
	4, 			/* RF5_BOLT_PLAS */
	4, 			/* RF5_BOLT_ICE */
	5, 			/* RF5_BOLT_WATER */
	5, 			/* RF5_BOLT_NETHER */
	4, 			/* RF5_BOLT_MANA */
	0, 			/* RF5_XXX3 */
	3, 			/* RF5_BEAM_ELEC */
	6, 			/* RF5_BEAM_ICE */
	7, 			/* RF5_BEAM_NETHER */
	7, 			/* RF5_ARC__HFIRE */
	5  			/* RF5_ARC__FORCE */
};

byte mana_cost_RF6[32]=
{
	6, 			/* RF6_HASTE */
	0, 			/* RF6_ADD_MANA */
	0, 			/* RF6_HEAL */
	3, 			/* RF6_CURE */
	1, 			/* RF6_BLINK */	
	6, 			/* RF6_TPORT */
	0, 			/* RF6_XXX1 */
	4, 			/* RF6_TELE_SELF_TO */
	4, 			/* RF6_TELE_TO */
	8, 			/* RF6_TELE_AWAY */
	8, 			/* RF6_TELE_LEVEL */
	0, 			/* RF6_XXX3 */
	1, 			/* RF6_DARKNESS */
	2, 			/* RF6_TRAPS */
	6, 			/* RF6_FORGET */
	0, 			/* RF6_DRAIN_MANA */
	5, 			/* RF6_DISPEL */ 
	0, 			/* RF6_XXX5 */ 
	3, 			/* RF6_MIND_BLAST */
	4, 			/* RF6_BRAIN_SMASH */
	4, 			/* RF6_WOUND */
	0, 			/* RF6_XXX6 */
	0, 			/* RF6_XXX7 */ 
	0, 			/* RF6_XXX8 */
	0, 			/* RF6_XXX9 */
	2, 			/* RF6_HUNGER */ 
	0, 			/* RF6_XX11 */ 
	1, 			/* RF6_SCARE */ 
	3, 			/* RF6_BLIND */ 
	4, 			/* RF6_CONF */ 
	5, 			/* RF6_SLOW */ 
	6  			/* RF6_HOLD */ 
};

byte mana_cost_RF7[32]=
{
	12,			/* RF7_S_KIN */ /* Summon - 6 */
	0, 			/* RF7_XXX1 */
	0, 			/* RF7_XXX2 */
	10,			/* RF7_S_MONSTER */ /* Summon - 1 */
	15,		/* RF7_S_MONSTERS */ /* Summon - 8 */
	0,			/* RF7_XXX3 */
	0, 			/* RF7_XXX4 */
	0, 			/* RF7_XXX5 */
	10, 			/* RF7_S_ANT */ /* Summon - 6 */
	12,			/* RF7_S_SPIDER */ /* Summon - 6 */
	14,			/* RF7_S_HOUND */ /* Summon - 6 */
	15,			/* RF7_S_ANIMAL */ /* Summon - 6 */
	0, 			/* RF7_XXX6 */
	0, 			/* RF7_XXX7 */
	15,			/* RF7_S_THIEF */ /* Summon - 6 */
	5, 			/* RF7_S_BERTBILLTOM */ /* Summon - 2 */
	0, 			/* RF7_XXX8 */
	0, 			/* RF7_XXX9 */
	0, 			/* RF7_XX10 */
	0, 			/* RF7_XX11 */
	14,			/* RF7_S_DRAGON */ /* Summon - 1 */
	20,			/* RF7_S_HI_DRAGON */ /* Summon - 8 */
	0, 			/* RF7_XX12 */
	0, 			/* RF7_XX13 */
	14,			/* RF7_S_DEMON */ /* Summon - 1 / 2-3 */
	20,			/* RF7_S_HI_DEMON */ /* Summon - 8 */
	0, 			/* RF7_XX14 */
	0, 			/* RF7_XX15 */
	12,			/* RF7_S_UNDEAD */ /* Summon - 1 */
	20,			/* RF7_S_HI_UNDEAD */ /* Summon - 8 */
	20,			/* RF7_S_WRAITH */ /* Summon - 8 */
	20 			/* RF7_S_UNIQUE */ /* Summon - 8 */
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
	{ 35,  0,   0,   5,	0,   0, LRN_ARCH  ,  100}, /* RF4_MISSL	    */
	{ 40,  0,   0,   5,	0,   0, LRN_PARCH ,  100}, /* RF4_PMISSL    */
	{ 65,  0,   0,   5,	0,   0, LRN_ACID  ,   90}, /* RF4_BRTH_ACID */
	{ 65,  0,   0,   5,	0,   0, LRN_ELEC  ,   90}, /* RF4_BRTH_ELEC */
	{ 65,  0,   0,   5,	0,   0, LRN_FIRE  ,   90}, /* RF4_BRTH_FIRE */
	{ 65,  0,   0,   5,	0,   0, LRN_COLD  ,   90}, /* RF4_BRTH_COLD */
	{ 65,  0,   0,   5,	0,   0, LRN_POIS  ,   90}, /* RF4_BRTH_POIS */
	{ 65,  0,   0,   5,	0,   0, LRN_PLAS  ,   90}, /* RF4_BRTH_PLAS */
	{ 50,  0,   0,   5,	0,   0, LRN_LITE  ,   90}, /* RF4_BRTH_LITE */
	{ 50,  0,   0,   5,	0,   0, LRN_DARK  ,   90}, /* RF4_BRTH_DARK */
	{ 50,  0,   0,   5,	0,   0, LRN_CONFU ,   90}, /* RF4_BRTH_CONFU*/
	{ 50,  0,   0,   5,	0,   0, LRN_SOUND ,   90}, /* RF4_BRTH_SOUND*/
	{ 50,  0,   0,   5,	0,   0, LRN_SHARD ,   90}, /* RF4_BRTH_SHARD*/
	{ 50,  0,   0,   5,	0,   0,	   0	  ,   90}, /* RF4_BRTH_INER */
	{ 50,  0,   0,   5,	0,   0, LRN_SOUND2,   90}, /* RF4_BRTH_GRAV */
	{ 50,  0,   0,   5,	0,   0, LRN_SOUND2,   90}, /* RF4_BRTH_FORCE*/
	{ 50,  0,   0,   5,	0,   0, LRN_NEXUS ,   90}, /* RF4_BRTH_NEXUS*/
	{ 50,  0,   0,   5,	0,   0, LRN_NETHR ,   90}, /* RF4_BRTH_NETHR*/
	{ 50,  0,   0,   5,	0,   0, LRN_CHAOS ,   90}, /* RF4_BRTH_CHAOS*/
	{ 50,  0,   0,   5,	0,   0, LRN_DISEN ,   90}, /* RF4_BRTH_DISEN*/
	{ 50,  0,   0,   5,	0,   0,	   0	  ,   90}, /* RF4_BRTH_TIME */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX2 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX3 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX4 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF4_XXX5 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100} /* RF4_XXX6 */
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
	{ 40,  0,   0,   0,	0,   0, LRN_LITE  ,  100}, /* RF5_BALL_LITE */
	{ 40,  0,   0,   0,	0,   0, LRN_DARK  ,  100}, /* RF5_BALL_DARK */
	{ 40,  0,   0,   0,	0,   0, LRN_CONFU ,  100}, /* RF5_BALL_CONFU*/
	{ 40,  0,   0,   0,	0,   0, LRN_SOUND ,  100}, /* RF5_BALL_SOUND*/
	{ 40,  0,   0,   0,	0,   0, LRN_SHARD ,  100}, /* RF5_BALL_SHARD*/
	{ 40,  0,   0,   0,	0,   0, LRN_STORM ,  100}, /* RF5_BALL_STORM*/
	{ 40,  0,   0,   0,	0,   0, LRN_NETHR ,  100}, /* RF5_BALL_NETHR*/
	{ 40,  0,   0,   0,	0,   0, LRN_CHAOS ,  100}, /* RF5_BALL_CHAOS*/
	{ 40,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_BALL_MANA */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_XXX1 */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_XXX2 */
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
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF5_XXX3	    */
	{ 50,  0,   0,   0,	0,   0, LRN_ELEC  ,   90}, /* RF5_BEAM_ELEC */
	{ 50,  0,   0,   0,	0,   0, LRN_ICE	  ,   90}, /* RF5_BEAM_ICE  */
	{ 50,  0,   0,   0,	0,   0, LRN_NETHR ,   90}, /* RF5_BEAM_NETHR*/
	{ 50,  0,   0,   0,	0,   0, LRN_FIRE  ,   95}, /* RF5_ARC__HFIRE*/
	{ 40,  0,   0,   0,	0,   0,	   0	  ,   90} /* RF5_ARC__FORCE*/
};

byte spell_desire_RF6[32][8] =
{
/*     d_base	  d_hurt    d_esc	 d_res				    */
/*	     d_summ	d_mana	  d_tact	   d_range		    */
	{ 50,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_HASTE	    */
	{ 15,  0,   0,   25,	0,   0,	   0	  ,  100}, /* RF6_ADD_MANA  */
	{ 10,  0,   20,  0,	0,   0,	   0	  ,  100}, /* RF6_HEAL	    */
	{ 50,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_CURE	    */
	{ 27,  0,   0,   0,	10,  15,   0	  ,  100}, /* RF6_BLINK	    */
	{ 10,  0,   0,   0,	20,  10,   0	  ,  100}, /* RF6_TPORT	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX1	    */
	{ 30,  0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_TELE_SELF_TO*/
	{ 30,  0,   0,   0,	0,   10,   0	  ,  100}, /* RF6_TELE_TO   */
	{ 10,  0,   0,   0,	20,  10,   0	  ,  100}, /* RF6_TELE_AWAY */
	{ 10,  0,   0,   0,	20,  10,LRN_NEXUS_SAVE,	   100}, /* RF6_TELE_LEVEL */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF6_XXX3	    */
	{ 20,  0,   0,   0,	5,   0,	   0	  ,  100}, /* RF6_DARKNESS  */
	{ 25,  0,   0,   0,	5,   0,	   0	  ,  100}, /* RF6_TRAPS	    */
	{ 25,  0,   0,   0,	5,   0, LRN_SAVE  ,  100}, /* RF6_FORGET    */
	{ 25,  0,   0,   15,	0,   0, LRN_MANA  ,  100}, /* RF6_DRAIN_MANA*/
	{ 20,  0,   0,   10,	0,   0,	   0      ,  100}, /* RF6_DISPEL    */
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
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX6	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX7	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_THIEF   */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_BERTBILLTOM*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX8	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XXX9	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX10	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX11	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DRAGON  */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DRAGON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX12	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX13	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DEMON   */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DEMON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX14	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX15	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_UNDEAD  */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_UNDEAD*/
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_WRAITH  */
	{ 0,   18,  0,   0,	0,   0,	   0	  ,  100}  /* RF7_S_UNIQUE  */
};

/*
 * Optimal Ranges for various spells.
 * 6 is optimal for Breath Weapons, Beams, and Arcs.
 * 3 is optimal for Lash/Spit.
 * 0 indicates no range limitation for other spells.
 *
 * This range is considered a preference if d_range in spell_desire is > 0.
 * It is a hard limit if d_range = 0.
 */
byte spell_range_RF4[32] =
{
	0,3,0,0,0,0,0,0,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,0,0,0,0,0
};

byte spell_range_RF5[32] =
{
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,6,6,6,6
};

byte spell_range_RF6[32] =
{
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 
};

byte spell_range_RF7[32] =
{
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

/*
 * Names array for specialty abilites.
 * The order here must match both the definition numbers in defines.h and
 * the order of specialty_tips in info.c
 */
cptr specialty_names[255]=
{
	"Armor Mastery",			/* Defense */
	"Shield Mastery",
	"Armor Proficiency",
	"Evasion",
	"Magic Resistance",
	"Phasewalking",
	"Unlight",
	"","","","","","","","","","","","","",
	"Armsman",				/* Physical Offense */
	"Fast Attacking",
	"Marksman",
	"Piercing Shot",
	"Mighty Throw",
	"Power Strike",
	"Martial Arts",
	"","","","","","","","","","","","","",
	"Beguile",				/* Magic and Mana */
	"Enhance Magic",
	"Fast Casting",
	"Power Siphon",
	"Heighten Magic",
	"Soul Siphon",
	"Harmony",
	"","","","","","","","","","","","","",
	"Athletics",				/* Others */
	"Clarity",
	"",
	"Fury",
	"Meditation",
	"Regeneration",
	"Extra Trap",
	"Holy Light",
	"","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","",""
};
