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
 * Player resistance levels.
 *
 * The interesting range is Base-1 (vulnerable) to Base+10 (immune).
 */
int extract_resistance[60] =
{
        /* -20  */    -30, -30, -30, -30, -30, -30, -30, -30, -30, -30,
        /* -10  */    -30, -30, -30, -30, -30, -30, -30, -30, -30, -30,
        /* Base */      0,  25,  45,  60,  70,  75,  80,  85,  90,  95,
        /* +10  */    100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
        /* +20  */    100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
        /* +30  */    100, 100, 100, 100, 100, 100, 100, 100, 100, 100
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
 * Names of the spells (mage spells, priestly prayers, Nature magics,
 * and Necromantic spells).  Spells are listed by index (see table above
 * for index assignments, and cmd5.c for the effects asociated with each
 * index) -LM-
 */
cptr spell_names[287] =
{
        /* Magic for Beginners (sval 0) */
        "Magic Missile",                                                /* index 0 */
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

        "Hit and Run",                                                  /* index 59 */
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
        "Ball of Light",                                                /* index 117 */
        "Holy Lance",
        "Word of Destruction",
        "Annihilation",
        "Call on Varda",

        "Elemental Infusion",                                                   /* index 122 */
        "Sanctify for Battle",
        "Horn of Wrath",
        "Purifying Strike",                                          /* index 125 */
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
        "Acid Bolt",                                            /* index 146 */
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
        "Become Mouse",                                         /* index 161 */
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
        "Timeblast",                                            /* index 181 */
        "Essence of Speed",
        "Infusion",
        "Become Elder Ent",
        "Regain Life",
        "Intervention of Yavanna",

        "Creature Knowledge",                                   /* index 187 */
        "",
        "",
        "",
        "",



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
        "Probing",                                                      /* index 223 */
        "Shadow Mapping",
        "Identify",
        "Shadow Warping",
        "",

        /* Unholy Protection (sval 5) */
        "Resist Acid and Cold",                                 /* index 229 */
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
    
	"Mental Awareness",						/* index 250 */
        "Slip into the Shadows",
        "Bloodwrath",
        "Rebalance Weapon",
        "",

        /* Special: Archery type spells */
        "Stone Volley",                                                 /* index 255 */
        "",
        "",
        "",
        "",
        "",                                                     /* index 260 */
        "",
        "",
        "Deadeye Shot",
        "Rain of Arrows",
        "Storm Shot",                                                   /* index 265 */
        "Dragonslayer",
        "Scourging Shot",
        "",
        "",
        "",                                                     /* index 270 */
        "Vile Dart",
        "Poison Shot",
        "",
        "",
        "",                                                     /* index 275 */
        "",
        "",
        "",
        "",
        "",                                                     /* index 280 */
        "",
        "",
        "",
        "",
        "",                                                     /* index 285 */
        ""
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
 * The next two tables are useful for generating list of items of
 * different types.  "group_item" comes from wizard1.c, which is where
 * it was previously used.
 */

/* 
 * Index into "grouper" for general item types.
 *
 * Must be synced to non-NULL entries in group_item.
 *
 * This is a little silly - the right solution is to initialize based
 * on the group_item table at load time.
 */
int new_group_index[] =
{ 0,  3,  4,  8, 11, 17, 18, 19, 20, 21,
  22, 23, 24, 25, 26, 27, 28, 29, 30, 36,
  -1};

/*
 * The basic items categorized by type
 */
grouper group_item[] =
{
	{ TV_SHOT,		"Ammo" },
	{ TV_ARROW,		  NULL },
	{ TV_BOLT,		  NULL },
    
	{ TV_BOW,		"Bows" },
    
	{ TV_SWORD,		"Weapons" },
	{ TV_POLEARM,	  NULL },
	{ TV_HAFTED,	  NULL },
	{ TV_DIGGING,	  NULL },
    
	{ TV_SOFT_ARMOR,	"Armour (Body)" },
	{ TV_HARD_ARMOR,	  NULL },
	{ TV_DRAG_ARMOR,	  NULL },
    
	{ TV_CLOAK,		"Armour (Misc)" },
	{ TV_SHIELD,	  NULL },
	{ TV_HELM,		  NULL },
	{ TV_CROWN,		  NULL },
	{ TV_GLOVES,	  NULL },
	{ TV_BOOTS,		  NULL },
    
	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },
	{ TV_SCROLL,	"Scrolls" },
	{ TV_POTION,	"Potions" },
	{ TV_FOOD,		"Food" },
	{ TV_ROD,		"Rods" },
	{ TV_WAND,		"Wands" },
	{ TV_STAFF,		"Staffs" },
    
	{ TV_MAGIC_BOOK,	"Books (Mage)" },
	{ TV_PRAYER_BOOK,	"Books (Priest)" },
	{ TV_DRUID_BOOK,	"Stones (Druid)" },
	{ TV_NECRO_BOOK,	"Books (Necro)" },
    
	{ TV_CHEST,		"Chests" },
    
	{ TV_SPIKE,		"Various" },
	{ TV_LITE,		  NULL },
	{ TV_FLASK,		  NULL },
	{ TV_JUNK,		  NULL },
	{ TV_BOTTLE,	  NULL },
	{ TV_SKELETON,	  NULL },
    
	{ 0, "" }
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
	"Display monster list",
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
        "rogue_like_commands",          /* OPT_rogue_like_commands */     /*0*/
        "quick_messages",                       /* OPT_quick_messages */
	"floor_query_flag",                 /* OPT_floor_query_flag */
	"carry_query_flag",                 /* OPT_carry_query_flag */
        "use_old_target",                       /* OPT_use_old_target */
        "always_pickup",                        /* OPT_always_pickup */
        "always_repeat",                        /* OPT_always_repeat */
        "depth_in_feet",                        /* OPT_depth_in_feet */
        "stack_force_notes",            /* OPT_stack_force_notes */
        "stack_force_costs",            /* OPT_stack_force_costs */
        "show_labels",                          /* OPT_show_labels */
	"show_weights",			/* OPT_show_weights */
	"show_choices",			/* OPT_show_choices */
	"show_details",			/* OPT_show_details */
	"use_metric",			/* OPT_metric */
	"show_flavors",			/* OPT_flavors */
	"run_ignore_stairs",		/* OPT_run_ignore_stairs */
	"run_ignore_doors",			/* OPT_run_ignore_doors */
	"run_cut_corners",			/* OPT_run_cut_corners */
	"run_use_corners",			/* OPT_run_use_corners */
	"disturb_move",			/* OPT_disturb_move */  /*20*/
	"disturb_near",			/* OPT_disturb_near */
	"disturb_panel",			/* OPT_disturb_panel */
	"disturb_state",			/* OPT_disturb_state */
	"disturb_minor",			/* OPT_disturb_minor */
	"disturb_other",			/* OPT_disturb_other */
	"alert_hitpoint",			/* OPT_alert_hitpoint */
	"alert_failure",			/* OPT_alert_failure */
	"verify_destroy",			/* OPT_verify_destroy */
	"verify_special",			/* OPT_verify_special */
	"ring_bell",			/* OPT_ring_bell */
	"verify_destroy_junk",		/* OPT_verify_destroy_junk */
	"auto_haggle",			/* OPT_auto_haggle */
	"auto_scum",			/* OPT_auto_scum */
	"easy_open",			/* OPT_easy_open   -TNB- */
	"easy_disarm",			/* OPT_easy_disarm   -TNB- */
	"expand_look",			/* OPT_expand_look */
	"expand_list",			/* OPT_expand_list */
	"view_perma_grids",			/* OPT_view_perma_grids */
	"view_torch_grids",			/* OPT_view_torch_grids */
        "auto_more",                            /* OPT_auto_more */
        "dungeon_stair",                        /* OPT_dungeon_stair */
        "strong_squelch",                       /* OPT_strong_squelch */
	"bottom_status",                    /* OPT_bottom_status */
	"mouse_buttons",                    /* OPT_mouse_buttons */
	"show_menus",                       /* OPT_show_menus */
	"xchars_to_file",                   /* OPT_xchars_to_file */
        "smart_cheat",                          /* OPT_smart_cheat */
        "view_reduce_lite",                     /* OPT_view_reduce_lite */
        "hidden_player",                        /* OPT_hidden_player */
	"avoid_abort",			/* OPT_avoid_abort */
	"avoid_other",			/* OPT_avoid_other */
	"flush_failure",			/* OPT_flush_failure */
	"flush_disturb",			/* OPT_flush_disturb */
	"center_player",			/* OPT_center_player */
	"fresh_before",			/* OPT_fresh_before */
	"fresh_after",			/* OPT_fresh_after */
	"center_running",			/* OPT_center_running */
	"compress_savefile",		/* OPT_compress_savefile */
	"hilite_player",			/* OPT_hilite_player */
	"view_yellow_lite",		     /* OPT_view_yellow_lite */  /*60*/
	"view_bright_lite",			/* OPT_view_bright_lite */
        "view_granite_lite",            /* OPT_view_granite_lite */
        "view_special_lite",                    /* OPT_view_special_lite */
        "squelch_worthless",            /* OPT_squelch_worthless */
	"easy_more",                        /* OPT_easy_more */
	"query_floor",			/* OPT_query_floor */
        "show_piles",                           /* OPT_show_piles */
        "hp_changes_color",              /* OPT_hp_changes_color */
        "show_detect",                          /* OPT_show_detect */
        "disturb_trap_detect",          /* OPT_disturb_trap_detect */
	"show_lists",                       /* OPT_show_lists */
	"hide_squelchable",                 /* OPT_hide_squelchable */
	"auto_squelch",                     /* OPT_auto_squelch */
	NULL,               NULL,           NULL,
        NULL,           NULL,           NULL,           NULL,           NULL, /*80*/
        NULL,           NULL,           NULL,           NULL,           NULL,
        NULL,           NULL,           NULL,           NULL,           NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL, /*100*/
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
        NULL,           NULL,           NULL,           NULL,           NULL, /*120*/
        NULL,           NULL,           NULL,           NULL,           NULL,
        NULL,
        "birth_point_based",                  /* OPT_birth_point_based */
        "birth_auto_roller",            /* OPT_birth_auto_roller */
        NULL,
        "birth_preserve",               /* OPT_birth_preserve */
	"birth_small_device",	      /* OPT_birth_small_device */
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
	NULL,
	NULL, /*140*/
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,		
	NULL,		
	NULL,		
	NULL,		
	"cheat_peak",			/* OPT_cheat_peek */ /*160*/
	"cheat_hear",			/* OPT_cheat_hear */
	"cheat_room",			/* OPT_cheat_room */
	"cheat_xtra",			/* OPT_cheat_xtra */
	"cheat_know",			/* OPT_cheat_know */
	"cheat_live",			/* OPT_cheat_live */
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL, /*180*/
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
        NULL,
        "adult_point_based",                  /* OPT_adult_point_based */
        "adult_auto_roller",            /* OPT_adult_auto_roller */
        NULL,
        "adult_preserve",               /* OPT_adult_preserve */
	"adult_small_device",      	/* OPT_adult_small_device */	     
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,   NULL,   NULL,   NULL,   NULL,
        NULL,   NULL,   NULL,   NULL,   NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*220*/
	NULL,	
	NULL,	
	NULL,	
	"score_peek",			/* OPT_score_peek */
	"score_hear",			/* OPT_score_hear */
	"score_room",			/* OPT_score_room */
	"score_xtra",			/* OPT_score_xtra */
	"score_know",			/* OPT_score_know */
	"score_live",			/* OPT_score_live */
	NULL,	
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*240*/
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,   NULL
};


/*
 * Options -- descriptions (where defined)
 */
cptr option_desc[OPT_MAX] =
{
        "Rogue-like commands",                          /* OPT_rogue_like_commands */ /*0*/
        "Activate quick messages",                                      /* OPT_quick_messages */
	"Prompt for floor item selection",      /* OPT_floor_query_flag */
	"Prompt before picking things up",      /* OPT_carry_query_flag */
        "Use old target by default",                            /* OPT_use_old_target */
        "Pick things up by default",                            /* OPT_always_pickup */
        "Repeat obvious commands",                                      /* OPT_always_repeat */
        "Show dungeon level in feet (or meters)",       /* OPT_depth_in_feet */
        "Merge inscriptions when stacking",                     /* OPT_stack_force_notes */
        "Merge discounts when stacking",                        /* OPT_stack_force_costs */
        "Show labels in equipment listings",            /* OPT_show_labels */
	"Show weights in all object listings",  /* OPT_show_weights */
	"Show choices in inven/equip windows",  /* OPT_show_choices */
	"Show details in monster descriptions", /* OPT_show_details */
	"Use metric (SI) measurements",	    /* OPT_metric */
	"Show flavors in object descriptions",  /* OPT_show_flavors */
	"When running, ignore stairs",	    /* OPT_run_ignore_stairs */
	"When running, ignore doors",	    /* OPT_run_ignore_doors */
	"When running, cut corners",	    /* OPT_run_cut_corners */
	"When running, use corners",	    /* OPT_run_use_corners */
	"Disturb whenever any monster moves",   /* OPT_disturb_move */	/*20*/
	"Disturb whenever viewable monster moves", /* OPT_disturb_near */
	"Disturb whenever map panel changes",   /* OPT_disturb_panel */
	"Disturb whenever player state changes",  /* OPT_disturb_state */
	"Disturb whenever boring things happen",  /* OPT_disturb_minor */
	"Disturb whenever various things happen", /* OPT_disturb_other */
	"Alert user to critical hitpoints",	    /* OPT_alert_hitpoint */
	"Alert user to various failures",	    /* OPT_alert_failure */
	"Verify destruction of objects",	    /* OPT_verify_destroy */
	"Verify use of special commands",	    /* OPT_verify_special */
	"Audible bell (on errors, etc)",	    /* OPT_ring_bell */
	"Verify destruction of worthless objects", /* OPT_verify_destroy_junk */
	"Auto-haggle in stores",		    /* OPT_auto_haggle */
	"Auto-scum for good levels",	    /* OPT_auto_scum */
	"Open and close doors automatically",   /* OPT_easy_open  -TBN- */
	"Disarm traps automatically",	    /* OPT_easy_disarm   -TNB- */
	"Expand the power of the look command", /* OPT_expand_look */
	"Expand the power of the list commands", /* OPT_expand_list */
	"Map remembers all perma-lit grids",    /* OPT_view_perma_grids */
	"Map remembers all torch-lit grids",    /* OPT_view_torch_grids */
        "Automatically clear '-more-' prompts",         /* OPT_auto_more */
        "Generate dungeons with connected stairs",      /* OPT_dungeon_stair */
        "Auto-squelched items are immediately destroyed",       /* OPT_strong_squelch */
	"Status is shown at the bottom of the screen", /* OPT_bottom_status */
	"Buttons for common mouse commands are shown", /* OPT_mouse_buttons */
	"Enter key brings up command menu",     /* OPT_show_menus */
	"Allow accents in output files",        /* OPT_xchars_to_file */
        "Monsters exploit players weaknesses",          /* OPT_smart_cheat */
        "Reduce light radius when running",                     /* OPT_view_reduce_lite */
        "Hide player symbol when running",                      /* OPT_hidden_player */
	"Avoid checking for user abort",	    /* OPT_avoid_abort */
	"Avoid processing special colors",	    /* OPT_avoid_other */
	"Flush input on various failures",	    /* OPT_flush_failure */
	"Flush input whenever disturbed",	    /* OPT_flush_disturb */
	"Keep the player centered (slow)",	    /* OPT_center_player */
	"Flush output before every command",    /* OPT_fresh_before */
	"Flush output after various things",    /* OPT_fresh_after */
	"Keep player centered while running (slow)",  /* OPT_center_running */
	"Compress messages in savefiles",	    /* OPT_compress_savefile */
	"Highlight the player with the cursor", /* OPT_hilite_player */
	"Use special colors for torch lite",    /* OPT_view_yellow_lite */ /*60*/
	"Use special colors for field of view", /* OPT_view_bright_lite */
        "Use special colors for wall grids",            /* OPT_view_granite_lite */
        "Use special colors for floor grids",           /* OPT_view_special_lite */
	"Squelch worthless items automatically",/* OPT_squelch_worthless */
	"Minimise '-more-' prompts",            /* OPT_easy_more */
	"Display things before picking them up",			/* OPT_query_floor */
        "Show stacks using special attr/char",          /* OPT_show_piles */
        "Player symbol changes color with damage", /* OPT_hp_changes_color */
        "Show region affected by using detection spells", /* OPT_show_detect */
        "Disturb when leaving last trap detect area", /* OPT_disturb_trap_detect */
	"Automatically show lists for commands", /* OPT_show_lists */
	"Hide items set as squelchable",            /* OPT_hide_squelchable */
	"Destroy items marked as squelch automatically",    /* OPT_auto_squelch */
	NULL,               NULL,           NULL,
        NULL,           NULL,           NULL,           NULL,           NULL,  /*80*/
        NULL,           NULL,           NULL,           NULL,           NULL,
        NULL,           NULL,           NULL,           NULL,           NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,  /*100*/
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
        NULL,           NULL,           NULL,           NULL,           NULL,  /*120*/
        NULL,           NULL,           NULL,           NULL,           NULL,
        NULL,
        "Birth: Use point based character generation",           /* OPT_birth_point_based */
        "Birth: Use Autoroller if rolling for stats",            /* OPT_birth_auto_roller */
        NULL,
        "Birth: No special feelings/artifacts preserved",  /* OPT_birth_preserve */
	"Birth: View and spell distances halved", /* OPT_birth_small_device */
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
	"Cheat: Peek into monster creation",	/* OPT_cheat_hear */
	"Cheat: Peek into dungeon creation",	/* OPT_cheat_room */
	"Cheat: Peek into something else",		/* OPT_cheat_xtra */
	"Cheat: Know complete monster info",	/* OPT_cheat_know */
	"Cheat: Allow player to avoid death",	/* OPT_cheat_live */
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL, /*180*/
	NULL,	NULL,		NULL,		NULL,		NULL,
	NULL,	NULL,		NULL,		NULL,		NULL,
        NULL,
        "Adult: Use point based character generation",           /* OPT_adult_point_based */
        "Adult: Use Autoroller if rolling for stats",            /* OPT_adult_auto_roller */
        NULL,
        "Adult: Artifacts preserved & no special feelings",  /* OPT_adult_preserve */
	"Adult: View and spell distances halved",      /* OPT_adult_small_device */
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,   NULL,   NULL,   NULL,   NULL,
        NULL,   NULL,   NULL,   NULL,   NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL, /*220*/
	NULL,
	NULL,
	NULL,
	"Score: Peek into object creation",		/* OPT_score_peek */
	"Score: Peek into monster creation",	/* OPT_score_hear */
	"Score: Peek into dungeon creation",	/* OPT_score_room */
	"Score: Peek into something else",		/* OPT_score_xtra */
	"Score: Know complete monster info",	/* OPT_score_know */
	"Score: Allow player to avoid death",	/* OPT_score_live */
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
        FALSE,          /* OPT_rogue_like_commands */   /*0*/
	TRUE,               /* OPT_quick_messages */
	TRUE,               /* OPT_floor_query_flag */
	FALSE,              /* OPT_carry_query_flag */
        FALSE,          /* OPT_use_old_target */
        TRUE,           /* OPT_always_pickup */
        TRUE,           /* OPT_always_repeat */
        TRUE,           /* OPT_depth_in_feet */
        TRUE,           /* OPT_stack_force_notes */
        FALSE,          /* OPT_stack_force_costs */
        TRUE,           /* OPT_show_labels */
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
        FALSE,          /* OPT_auto_more */
        TRUE,           /* OPT_dungeon_stair */
        FALSE,          /* OPT_strong_squelch */
	FALSE,              /* OPT_bottom_status */
	TRUE,               /* OPT_mouse_buttons */
	TRUE,               /* OPT_show_menus */
	FALSE,              /* OPT_xchars_to_file */
        FALSE,          /* OPT_smart_cheat */
        FALSE,          /* OPT_view_reduce_lite */
        FALSE,          /* OPT_hidden_player */
	FALSE,		/* OPT_avoid_abort */
	FALSE,		/* OPT_avoid_other */
	TRUE,		/* OPT_flush_failure */
	FALSE,		/* OPT_flush_disturb */
	FALSE,		/* OPT_center_player */
	TRUE,		/* OPT_fresh_before */
	FALSE,		/* OPT_fresh_after */
        FALSE,          /* OPT_center_running */
        FALSE,          /* OPT_compress_savefile */
        FALSE,          /* OPT_hilite_player */
        TRUE,           /* OPT_view_yellow_lite */       /*60*/
        TRUE,           /* OPT_view_bright_lite */
        FALSE,          /* OPT_view_granite_lite */
        TRUE,           /* OPT_view_special_lite */
        FALSE,          /* OPT_squelch_worthless */
	FALSE,              /* OPT_easy_more */
        FALSE,          /* OPT_query_floor */
        FALSE,          /* OPT_show_piles */
        TRUE,           /* OPT_hp_changes_color */
        TRUE,                     /* OPT_show_detect */
        TRUE,           /* OPT_disturb_trap_detect */
	FALSE,              /* OPT_show_lists */
	FALSE,              FALSE,          FALSE,          FALSE,
        FALSE,          FALSE,          FALSE,          FALSE,          FALSE,  /*80*/
        FALSE,          FALSE,          FALSE,          FALSE,          FALSE,
        FALSE,          FALSE,          FALSE,          FALSE,          FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,	/*100*/
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,	/*120*/
        FALSE,          FALSE,          FALSE,          FALSE,          FALSE,
        FALSE,
        FALSE,
        TRUE,            /* OPT_birth_point_based */
        TRUE,            /* OPT_birth_auto_roller */
        FALSE,
        TRUE,   /* OPT_birth_preserve */
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,
	FALSE,
	FALSE, /*140*/
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		
	FALSE,		/* OPT_cheat_peek */  /*160*/
	FALSE,		/* OPT_cheat_hear */
	FALSE,		/* OPT_cheat_room */
	FALSE,		/* OPT_cheat_xtra */
	FALSE,		/* OPT_cheat_know */
	FALSE,		/* OPT_cheat_live */
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE, /*180*/
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
	FALSE,	FALSE,		FALSE,		FALSE,		FALSE,
        FALSE,
        TRUE,   /* OPT_adult_point_based */
        TRUE,   /* OPT_adult_auto_roller */
        FALSE,
        TRUE,   /* OPT_adult_preserve */
        FALSE,  /* OPT_adult_small_device */
        FALSE,
        FALSE,
        FALSE,
        FALSE,
        FALSE,  FALSE,  FALSE,  FALSE,  FALSE,
        FALSE,  FALSE,  FALSE,  FALSE,  FALSE,
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
                OPT_always_repeat,
                OPT_show_labels,
                OPT_show_weights,
                OPT_show_choices,
                OPT_show_details,
                OPT_metric,
                OPT_show_flavors,
                OPT_show_detect,
		OPT_show_menus,
		OPT_hp_changes_color,
		OPT_mouse_buttons,
		OPT_bottom_status,
		OPT_squelch_worthless,
		OPT_hide_squelchable,
		OPT_xchars_to_file,
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
		OPT_easy_more,
		OPT_NONE,
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
                OPT_depth_in_feet,
                OPT_stack_force_notes,
                OPT_stack_force_costs,
                OPT_dungeon_stair,
		OPT_query_floor,
		OPT_always_pickup,
		OPT_always_repeat,
                OPT_smart_cheat,
		OPT_show_lists,
		OPT_NONE,
		OPT_NONE,
		OPT_NONE,
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
		OPT_NONE,
		OPT_NONE,
		OPT_NONE
        },
        /*** Birth ***/

        {
                OPT_birth_preserve,
                OPT_birth_small_device,
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

/* Table of Druid blows. -LM- */
druid_blows d_blow[NUM_D_BLOWS] =
{
	{ "punch",		  1, 5 },
	{ "kick",		  2, 4 },
	{ "knee",		  1,12 },
	{ "chop",		  2, 7 },
	{ "uppercut",	  3, 6 },
	{ "boot",		  3, 9 },
	{ "bang on",	  6, 4 },
	{ "slam",		  4, 9 },
	{ "grapple with",	 13, 3 },
	{ "hammer",		  9, 6 },
	{ "head butt",	  3,24 },
	{ "strangle",	  8,10 },
	{ "roundhouse kick",  5,19 },
	{ "assault",	 10,11 },
	{ "crush",		 11,11 },
	{ "double-kick",	 21, 6 },
	{ "thunderclap belt", 8,19 },
	{ "blizzard gouge",	 14,11 },
	{ "tsunami whirl",	  7,26 },
	{ "stormwind chop",	 10,22 }
};

cptr feel_text[FEEL_MAX] =
{
	NULL,	 /* FEEL_NONE */
	"broken",	 /* FEEL_BROKEN */
	"terrible",	 /* FEEL_TERRIBLE */
	"worthless", /* FEEL_WORTHLESS */
        "cursed",        /* FEEL_CURSED */
        "uncursed",      /* FEEL_UNCURSED */
        "average",       /* FEEL_AVERAGE */
	"good",      /* FEEL_GOOD_STRONG */
        "excellent",     /* FEEL_EXCELLENT */
        "special",       /* FEEL_SPECIAL */
	"good",      /* FEEL_GOOD_WEAK */
};

const grouper object_text_order [] =
{
        {TV_SWORD,              "Sword"                 },
        {TV_POLEARM,            "Polearm"               },
        {TV_HAFTED,             "Hafted Weapon" },
        {TV_BOW,                "Bow"                   },
        {TV_ARROW,              "Ammunition"    },
        {TV_BOLT,               NULL                    },
        {TV_SHOT,               NULL                    },
        {TV_SHIELD,             "Shield"                },
        {TV_CROWN,              "Crown"                 },
        {TV_HELM,               "Helm"                  },
        {TV_GLOVES,             "Gloves"                },
        {TV_BOOTS,              "Boots"                 },
        {TV_CLOAK,              "Cloak"                 },
        {TV_DRAG_ARMOR,         "Dragon Scale Mail" },
        {TV_HARD_ARMOR,         "Hard Armor"    },
        {TV_SOFT_ARMOR,         "Soft Armor"    },
        {TV_RING,               "Ring"                  },
        {TV_AMULET,             "Amulet"                },
        {TV_LITE,               "Lite"                  },
        {TV_POTION,             "Potion"                },
        {TV_SCROLL,             "Scroll"                },
        {TV_WAND,               "Wand"                  },
        {TV_STAFF,              "Staff"                 },
        {TV_ROD,                "Rod"                   },
        {TV_PRAYER_BOOK,        "Priest Book"   },
        {TV_MAGIC_BOOK,         "Magic Book"    },
        {TV_DRUID_BOOK,         "Stone of Lore" },
        {TV_NECRO_BOOK,         "Necromantic Tome" },
        {TV_SPIKE,              "Spike"                 },
        {TV_DIGGING,            "Digger"                },
        {TV_FOOD,               "Food"                  },
        {TV_FLASK,              "Flask"                 },
        {TV_JUNK,               "Junk"                  },
        {0,                     NULL                    }
};

byte mana_cost_RF4[32]=
{
	1,                  /* RF4_SHREIK */
        0,                      /* RF4_LASH */
        0,                      /* RF4_BOULDER */
        0,                      /* RF4_SHOT */
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
        0,                      /* RF4_BRTH_CHAOS */
        0,                      /* RF4_BRTH_DISEN */
        0,                      /* RF4_BRTH_TIME */
        0,                      /* RF4_XXX2 */
        0,                      /* RF4_XXX3 */
        0,                      /* RF4_XXX4 */
        0,                      /* RF4_XXX5 */
        0                       /* RF4_XXX6 */
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
        3,                      /* RF5_BEAM_ELEC */
        6,                      /* RF5_BEAM_ICE */
        7,                      /* RF5_BEAM_NETHER */
        7,                      /* RF5_ARC__HFIRE */
        5                       /* RF5_ARC__FORCE */
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
        4,                      /* RF6_BRAIN_SMASH */
        4,                      /* RF6_WOUND */
        0,                      /* RF6_XXX6 */
        0,                      /* RF6_XXX7 */
        0,                      /* RF6_XXX8 */
        0,                      /* RF6_XXX9 */
        2,                      /* RF6_HUNGER */
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
	15,		        /* RF7_S_MONSTERS */ /* Summon - 8 */
	0,			/* RF7_XXX3 */
	0, 			/* RF7_XXX4 */
	0, 			/* RF7_XXX5 */
	10, 		/* RF7_S_ANT */ /* Summon - 6 */
	12,			/* RF7_S_SPIDER */ /* Summon - 6 */
	14,			/* RF7_S_HOUND */ /* Summon - 6 */
	15,			/* RF7_S_ANIMAL */ /* Summon - 6 */
        0,                      /* RF7_XXX6 */
        0,                      /* RF7_XXX7 */
        15,                     /* RF7_S_THIEF */ /* Summon - 6 */
        5,                      /* RF7_S_BERTBILLTOM */ /* Summon - 2 */
        0,                      /* RF7_XXX8 */
        0,                      /* RF7_XXX9 */
        0,                      /* RF7_XX10 */
	0, 			/* RF7_XX11 */
	14,			/* RF7_S_DRAGON */ /* Summon - 1 */
	20,			/* RF7_S_HI_DRAGON */ /* Summon - 8 */
	0, 			/* RF7_XX12 */
	0, 			/* RF7_XX13 */
	14,			/* RF7_S_DEMON */ /* Summon - 1 / 2-3 */
	20,			/* RF7_S_HI_DEMON */ /* Summon - 8 */
	0, 			/* RF7_XX14 */
        0,                      /* RF7_XX15 */
        12,                     /* RF7_S_UNDEAD */ /* Summon - 1 */
        20,                     /* RF7_S_HI_UNDEAD */ /* Summon - 8 */
        20,                     /* RF7_S_WRAITH */ /* Summon - 8 */
        20                      /* RF7_S_UNIQUE */ /* Summon - 8 */
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
	/*  d_base   d_hurt   d_esc	 d_res				    */
	/*	 d_summ   d_mana  d_tact	   d_range		    */
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
        { 50,  0,   0,   5,     0,   0, LRN_CHAOS ,   90}, /* RF4_BRTH_CHAOS*/
        { 50,  0,   0,   5,     0,   0, LRN_DISEN ,   90}, /* RF4_BRTH_DISEN*/
        { 50,  0,   0,   5,     0,   0,    0      ,   90}, /* RF4_BRTH_TIME */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF4_XXX2 */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF4_XXX3 */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF4_XXX4 */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF4_XXX5 */
        { 0,   0,   0,   0,     0,   0,    0      ,  100} /* RF4_XXX6 */
};

byte spell_desire_RF5[32][8] =
{
	/*  d_base    d_hurt  d_esc	 d_res				    */
	/*     d_summ	  d_mana  d_tact	   d_range		    */
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
        { 50,  0,   0,   0,     0,   0, LRN_ELEC  ,   90}, /* RF5_BEAM_ELEC */
        { 50,  0,   0,   0,     0,   0, LRN_ICE   ,   90}, /* RF5_BEAM_ICE  */
        { 50,  0,   0,   0,     0,   0, LRN_NETHR ,   90}, /* RF5_BEAM_NETHR*/
        { 50,  0,   0,   0,     0,   0, LRN_FIRE  ,   95}, /* RF5_ARC__HFIRE*/
        { 40,  0,   0,   0,     0,   0,    0      ,   90} /* RF5_ARC__FORCE*/
};

byte spell_desire_RF6[32][8] =
{
	/*  d_base   d_hurt  d_esc	 d_res			                    */
	/*     d_summ	  d_mana   d_tact	       d_range		            */
	{ 50,  0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_HASTE	    */
	{ 15,  0,   0,   25,0,   0,	   0	      ,  100}, /* RF6_ADD_MANA      */
	{ 10,  0,   20,  0,	0,   0,	   0	      ,  100}, /* RF6_HEAL	    */
	{ 50,  0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_CURE	    */
	{ 27,  0,   0,   0,	10,  15,   0	      ,  100}, /* RF6_BLINK	    */
	{ 10,  0,   0,   0,	20,  10,   0	      ,  100}, /* RF6_TPORT	    */
	{ 0,   0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_XXX1	    */
	{ 30,  0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_TELE_SELF_TO  */
	{ 30,  0,   0,   0,	0,   10,   0	      ,  100}, /* RF6_TELE_TO       */
	{ 10,  0,   0,   0,	20,  10,   0	      ,  100}, /* RF6_TELE_AWAY     */
	{ 10,  0,   0,   0,	20,  10,LRN_NEXUS_SAVE,  100}, /* RF6_TELE_LEVEL    */
	{ 0,   0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_XXX3	    */
	{ 20,  0,   0,   0,	5,   0,	   0	      ,  100}, /* RF6_DARKNESS      */
	{ 25,  0,   0,   0,	5,   0,	   0	      ,  100}, /* RF6_TRAPS	    */
	{ 25,  0,   0,   0,	5,   0, LRN_SAVE      ,  100}, /* RF6_FORGET        */
	{ 25,  0,   0,   15,0,   0, LRN_MANA      ,  100}, /* RF6_DRAIN_MANA    */
	{ 25,  0,   0,   5,	0,   0,	   0          ,  100}, /* RF6_DISPEL        */
	{ 0,   0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_XXX5	    */
	{ 30,  0,   0,   0,	0,   0, LRN_SAVE      ,  100}, /* RF6_MIND_BLAST    */
        { 40,  0,   0,   0,     0,   0, LRN_SAVE  ,  100}, /* RF6_BRAIN_SMASH*/
        { 40,  0,   0,   0,     0,   0, LRN_SAVE  ,  100}, /* RF6_WOUND     */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF6_XXX6      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF6_XXX7      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF6_XXX8      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF6_XXX9      */
        { 25,  0,   0,   0,     0,   0,  LRN_SAVE ,  100}, /* RF6_HUNGER    */
	{ 0,   0,   0,   0,	0,   0,	   0	      ,  100}, /* RF6_XX11	    */
	{ 25,  0,   0,   0,	0,   0, LRN_FEAR_SAVE ,  100}, /* RF6_SCARE	    */
	{ 30,  0,   0,   0,	0,   0, LRN_BLIND_SAVE,  100}, /* RF6_BLIND	    */
	{ 30,  0,   0,   0,	0,   0, LRN_CONFU_SAVE,  100}, /* RF6_CONF	    */
	{ 40,  0,   0,   0,	0,   0, LRN_FREE_SAVE,   100}, /* RF6_SLOW	    */
	{ 35,  0,   0,   0,	0,   0, LRN_FREE_SAVE,   100}  /* RF6_HOLD	    */
};

byte spell_desire_RF7[32][8] =
{
/*   d_base	d_hurt d_esc	 d_res				    */
/*	   d_summ  d_mana  d_tact	   d_range		    */
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
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XXX6      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XXX7      */
        { 0,   15,  0,   0,     0,   0,    0      ,  100}, /* RF7_S_THIEF   */
        { 0,   15,  0,   0,     0,   0,    0      ,  100}, /* RF7_S_BERTBILLTOM*/
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XXX8      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XXX9      */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XX10      */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX11	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DRAGON  */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DRAGON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX12	    */
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX13	    */
	{ 0,   15,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_DEMON   */
	{ 0,   17,  0,   0,	0,   0,	   0	  ,  100}, /* RF7_S_HI_DEMON*/
	{ 0,   0,   0,   0,	0,   0,	   0	  ,  100}, /* RF7_XX14	    */
        { 0,   0,   0,   0,     0,   0,    0      ,  100}, /* RF7_XX15      */
        { 0,   15,  0,   0,     0,   0,    0      ,  100}, /* RF7_S_UNDEAD  */
        { 0,   17,  0,   0,     0,   0,    0      ,  100}, /* RF7_S_HI_UNDEAD*/
        { 0,   18,  0,   0,     0,   0,    0      ,  100}, /* RF7_S_WRAITH  */
        { 0,   18,  0,   0,     0,   0,    0      ,  100}  /* RF7_S_UNIQUE  */
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
cptr specialty_names[TOTAL_SPECIALTIES]=
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
        "Mana Burn",
        "","","","","","","","","","","","",
        "Beguile",                              /* Magic and Mana */
        "Enhance Magic",
        "Fast Casting",
	"Power Siphon",
	"Heighten Magic",
	"Soul Siphon",
        "Harmony",
        "",
        "Channeling",
        "Attunement",
        "","","","","","","","","","",
        "Athletics",                            /* Others */
        "Clarity",
        "",
	"Fury",
	"Meditation",
	"Regeneration",
	"Extra Trap",
	"Holy Light",
	"","","","","","","","","","","","", 
	"","","","","","","","","","","","","","","","","","","","", /* 80 - 99 */
	"","","","","","","","","","","","","","","","","","","","",/* 100 - 119 */
	"","","","","","","","",                   /* 120 - 127 */
	"Sword Preference",                     /* Racial Inherent abilities */
	"Polearm Preference",
	"Hafted Preference",
	"Sling Preference",
	"Bow Preference",
	"Crossbow Preference",
	"","","",
	"Sword Distaste",
	"Polearm Distaste",
	"Hafted Distaste",
	"Sling Distaste",
	"Bow Distaste",
	"Crossbow Distaste",
        "","","",
        "Hardy",
        "Ravenous",
        "Divine Recovery",
        "Shadow",
        "Treeish",
        "Shapeshifter",
        "","","","","","","","",
        "","","","","","","","","","","","","","","","","","","","", /* 160 - 179 */
        "","","","","","","","","","","","",
        "Fast Bow Shots",
	"Very Fast Bow Shots",
	"Fast Sling Shots",
	"Very Fast Sling Shots",
	"Fast Crossbow Shots",
	"Very Fast Crossbow Shots",
	"","","",
	"Assassinate",
	"Deadly Shot",
	"Backstab",
	"Crowd Combat [40]",
	"Strong Shield Bashing",
	"Unarmed Combat",
	"Edged Weapon Distaste",
	"",
	"Charms",
	"Magic Device Expertise",
	"Full Spellcaster",
	"Extra Spell Beaming",
	"",
	"Lore",
	"Pious",
	"Relentless [30, 40]",
	"Monster Probing [35]",
	"Evil",
	"Pickpocketing",
	"Detailed Object Sensing",
	"Trap Setting",
	"Woodsmanship",
	"Specialization [1]"
	"","","","","","","","","","","","","","","","",
	"","","","","","","","","","","","","","",""                /* 240 - 255 */
};
