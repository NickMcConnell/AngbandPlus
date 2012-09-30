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
 * Circular keypad direction array
 */
s16b cdd[8] =
{ 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Global arrays for optimizing "ddx[cdd[i]]" and "ddy[cdd[i]]"
 */
s16b ddx_cdd[8] =
{ 0, 1, 1, 1, 0, -1, -1, -1 };

s16b ddy_cdd[8] =
{ 1, 1, 0, -1, -1, -1, 0, 1 };



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
 * Global array for converting numbers to a logical list symbol
 */
char listsym[] =
{
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
	'\0'
};


/*
 * Encode the screen colors
 */
cptr color_char = "dwsorgbuDWvyRGBU";


/*
 * Stat Table (INT/WIS) -- Number of spells at level 50
 */
byte adj_mag_study[] =
{
	0	/* 3 */,
	5	/* 4 */,
	10	/* 5 */,
	15	/* 6 */,
	20	/* 7 */,
	25	/* 8 */,
	30	/* 9 */,
	35	/* 10 */,
	40	/* 11 */,
	45	/* 12 */,
	50	/* 13 */,
	52	/* 14 */,
	54	/* 15 */,
	56	/* 16 */,
	58	/* 17 */,
	60	/* 18/00-18/09 */,
	62	/* 18/10-18/19 */,
	64	/* 18/20-18/29 */,
	66	/* 18/30-18/39 */,
	68	/* 18/40-18/49 */,
	70	/* 18/50-18/59 */,
	75	/* 18/60-18/69 */,
	80	/* 18/70-18/79 */,
	85	/* 18/80-18/89 */,
	90	/* 18/90-18/99 */,
	95	/* 18/100-18/109 */,
	100	/* 18/110-18/119 */,
	105	/* 18/120-18/129 */,
	110	/* 18/130-18/139 */,
	115	/* 18/140-18/149 */,
	120	/* 18/150-18/159 */,
	125	/* 18/160-18/169 */,
	130	/* 18/170-18/179 */,
	135	/* 18/180-18/189 */,
	140	/* 18/190-18/199 */,
	145	/* 18/200-18/209 */,
	150	/* 18/210-18/219 */,
	155	/* 18/220+ */
};


/*
 * Stat Table (INT/WIS) -- extra mana at level 50 divided by 2.
 */
byte adj_mag_mana[] =
{
	0        /* 3 */,
	0        /* 4 */,
	2        /* 5 */,
	5        /* 6 */,
	7        /* 7 */,
	12       /* 8 */,
	15       /* 9 */,
	17       /* 10 */,
	20       /* 11 */,
	22       /* 12 */,
	25       /* 13 */,
	27       /* 14 */,
	30       /* 15 */,
	31       /* 16 */,
	33       /* 17 */,
	35       /* 18/00-18/09 */,
	37       /* 18/10-18/19 */,
	40       /* 18/20-18/29 */,
	42       /* 18/30-18/39 */,
	47       /* 18/40-18/49 */,
	52       /* 18/50-18/59 */,
	57       /* 18/60-18/69 */,
	62       /* 18/70-18/79 */,
	75       /* 18/80-18/89 */,
	87       /* 18/90-18/99 */,
	100      /* 18/100-18/109 */,
	112      /* 18/110-18/119 */,
	125      /* 18/120-18/129 */,
	137      /* 18/130-18/139 */,
	150      /* 18/140-18/149 */,
	162      /* 18/150-18/159 */,
	175      /* 18/160-18/169 */,
	187      /* 18/170-18/179 */,
	200      /* 18/180-18/189 */,
	205      /* 18/190-18/199 */,
	210      /* 18/200-18/209 */,
	215      /* 18/210-18/219 */,
	220      /* 18/220+ */
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
 * Stat Table (CHR) -- payment percentages
 */
byte adj_chr_gold[] =
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
	128 + -4    /*  3 */,
	128 + -3    /*  4 */,
	128 + -2    /*  5 */,
	128 + -1    /*  6 */,
	128 + 0     /*  7 */,
	128 + 0     /*  8 */,
	128 + 0     /*  9 */,
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
	128 + 16    /* 18/220+ */
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
	11	/* 18/160-18/169 */,
	11	/* 18/170-18/179 */,
	11	/* 18/180-18/189 */,
	11	/* 18/190-18/199 */,
	11	/* 18/200-18/209 */,
	11	/* 18/210-18/219 */,
	11	/* 18/220+ */
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
	128 + -1	/* 7 */,
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
	128 + 2	/* 18/00-18/09 */,
	128 + 3	/* 18/10-18/19 */,
	128 + 4	/* 18/20-18/29 */,
	128 + 4	/* 18/30-18/39 */,
	128 + 5	/* 18/40-18/49 */,
	128 + 5	/* 18/50-18/59 */,
	128 + 6	/* 18/60-18/69 */,
	128 + 6	/* 18/70-18/79 */,
	128 + 7	/* 18/80-18/89 */,
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
	{  2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2},

	/* 1  */
	{  2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   3,   3},

	/* 2  */
	{  2,   2,   2,   2,   2,   2,   2,   2,   3,   3,   3,   3},

	/* 3  */
	{  2,   2,   2,   2,   2,   2,   2,   3,   3,   3,   3,   3},

	/* 4  */
	{  2,   2,   2,   2,   2,   3,   3,   3,   3,   3,   3,   4},

	/* 5  */
	{  2,   2,   2,   2,   3,   3,   3,   3,   3,   4,   4,   4},

	/* 6  */
	{  2,   2,   2,   2,   3,   3,   3,   3,   4,   4,   4,   4},

	/* 7  */
	{  2,   2,   2,   3,   3,   3,   3,   4,   4,   4,   4,   4},

	/* 8  */
	{  2,   2,   3,   3,   3,   3,   4,   4,   4,   4,   4,   4},

	/* 9  */
	{  2,   2,   3,   3,   3,   4,   4,   4,   4,   4,   4,   5},

	/* 10 */
	{  2,   3,   3,   3,   3,   4,   4,   4,   4,   4,   5,   5},

	/* 11+ */
	{  2,   3,   3,   3,   4,   4,   4,   4,   4,   4,   5,   5}
};


/*
 * Store owners (exactly MAX_OWNERS owners per store, chosen randomly)
 * { name, purse, max greed, min greed, haggle_per, tolerance, race}
 *
 * Lifted extra shopkeepers from CthAngband (don't you just love open source
 * development? ;-)). Since this gave less than 32 unique names for some
 * shops, those have their first x names copied to reach 32.
 *
 * For the weapon and armour shops, several owners have a limit of 5k.
 */
owner_type owners[MAX_STORES][MAX_OWNERS] =
{
	{
		/* General store - 32 unique names */
		{ "Bilbo the Friendly",			2,    170, 108,  5, 15, RACE_HOBBIT},
		{ "Raistlin the Chicken",		2,    175, 108,  4, 12, RACE_HUMAN},
		{ "Sultan the Midget",			3,    170, 107,  5, 15, RACE_GNOME},
		{ "Lyar-el the Comely",			3,    165, 107,  6, 18, RACE_ELF},
		{ "Falilmawen the Friendly",	3,    170, 108,  5, 15, RACE_HOBBIT},
		{ "Voirin the Cowardly",		5,    175, 108,  4, 12, RACE_HUMAN},
		{ "Erashnak the Midget",		8,    170, 107,  5, 15, RACE_BEASTMAN},
		{ "Grug the Comely",			10,   165, 107,  6, 18, RACE_HALF_TITAN},
		{ "Forovir the Cheap",			2,    170, 108,  5, 15, RACE_HUMAN},
		{ "Ellis the Fool",				5,    175, 108,  4, 12, RACE_HUMAN},
		{ "Filbert the Hungry",			7,    170, 107,  5, 15, RACE_VAMPIRE},
		{ "Fthnargl Psathiggua",		10,	  165, 107,  6, 18, RACE_MIND_FLAYER},
		{ "Eloise Long-Dead",			3,    170, 108,  5, 15, RACE_SPECTRE},
		{ "Fundi the Slow",				5,    175, 108,  4, 12, RACE_ZOMBIE},
		{ "Granthus",					8,    170, 107,  5, 15, RACE_SKELETON},
		{ "Lorax the Suave",			10,   165, 107,  6, 18, RACE_VAMPIRE},
		{ "Butch",						3,    170, 108,  5, 15, RACE_HALF_ORC},
		{ "Elbereth the Beautiful",		5,    175, 108,  4, 12, RACE_HIGH_ELF},
		{ "Sarleth the Sneaky",			8,    170, 107,  5, 15, RACE_GNOME},
		{ "Narlock",					10,   165, 107,  6, 18, RACE_DWARF},
		{ "Haneka the Small",			3,    170, 108,  5, 15, RACE_GNOME},
		{ "Loirin the Mad",				5,    175, 108,  4, 12, RACE_HALF_GIANT},
		{ "Wuto Poisonbreath",			8,    170, 107,  5, 15, RACE_DRACONIAN},
		{ "Araaka the Rotund",			10,   165, 107,  6, 18, RACE_DRACONIAN},
		{ "Poogor the Dumb",			2,    170, 108,  5, 15, RACE_BEASTMAN},
		{ "Felorfiliand",				5,    175, 108,  4, 12, RACE_ELF},
		{ "Maroka the Aged",			7,    170, 107,  5, 15, RACE_GNOME},
		{ "Sasin the Bold",				10,   165, 107,  6, 18, RACE_HALF_GIANT},
		{ "Abiemar the Peasant",		2,    170, 108,  5, 15, RACE_HUMAN},
		{ "Hurk the Poor",				5,    175, 108,  4, 12, RACE_HALF_ORC},
		{ "Soalin the Wretched",		7,    170, 107,  5, 15, RACE_ZOMBIE},
		{ "Merulla the Humble",			10,   165, 107,  6, 18, RACE_ELF},
	},
	{
		/* Armoury - 28 unique names */
		{ "Kon-Dar the Ugly",			50,   210, 115,  5,  7, RACE_HALF_ORC},
		{ "Darg-Low the Grim",			100,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_AMBERITE},
		{ "Wieland the Smith",			300,  200, 112,  4,  5, RACE_DWARF},
		{ "Kon-Dar the Ugly",			100,  210, 115,  5,  7, RACE_HALF_ORC},
		{ "Darg-Low the Grim",			150,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_AMBERITE},
		{ "Elo Dragonscale",			300,  200, 112,  4,  5, RACE_ELF},
		{ "Delicatus",					100,  210, 115,  5,  7, RACE_SPRITE},
		{ "Gruce the Huge",				150,  190, 111,  4,  9, RACE_HALF_GIANT},
		{ "Animus",						250,  200, 112,  4, 10, RACE_GOLEM},
		{ "Malvus",						300,  200, 112,  4,  5, RACE_HALF_TITAN},
		{ "Selaxis",					100,  210, 115,  5,  7, RACE_ZOMBIE},
		{ "Deathchill",					50,   190, 111,  4,  9, RACE_SPECTRE},
		{ "Drios the Faint",			250,  200, 112,  4, 10, RACE_SPECTRE},
		{ "Bathric the Cold",			300,  200, 112,  4,  5, RACE_VAMPIRE},
		{ "Vengella the Cruel",			100,  210, 115,  5,  7, RACE_HALF_TROLL},
		{ "Wyrana the Mighty",			150,  190, 111,  4,  9, RACE_HUMAN},
		{ "Yojo II",					250,  200, 112,  4, 10, RACE_DWARF},
		{ "Ranalar the Sweet",			300,  200, 112,  4,  5, RACE_AMBERITE},
		{ "Horbag the Unclean",			50,   210, 115,  5,  7, RACE_HALF_ORC},
		{ "Elelen the Telepath",		150,  190, 111,  4,  9, RACE_DARK_ELF},
		{ "Isedrelias",					250,  200, 112,  4, 10, RACE_SPRITE},
		{ "Vegnar One-eye",				50,   200, 112,  4,  5, RACE_CYCLOPS},
		{ "Rodish the Chaotic",			100,  210, 115,  5,  7, RACE_BEASTMAN},
		{ "Hesin Swordmaster",			150,  190, 111,  4,  9, RACE_NIBELUNG},
		{ "Elvererith the Cheat",		100,  200, 112,  4, 10, RACE_DARK_ELF},
		{ "Zzathath the Imp",			300,  200, 112,  4,  5, RACE_IMP},
		{ "Kon-Dar the Ugly",			50,   210, 115,  5,  7, RACE_HALF_ORC},
		{ "Darg-Low the Grim",			100,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_AMBERITE},
		{ "Wieland the Smith",			300,  200, 112,  4,  5, RACE_DWARF},
	},
	{
		/* Weapon Smith - 28 unique names */
		{ "Arnold the Beastly",			50,   210, 115,  6,  6, RACE_BARBARIAN},
		{ "Arndal Beast-Slayer",		100,  185, 110,  5,  9, RACE_HALF_ELF},
		{ "Eddie Beast-Master",			250,  190, 115,  5,  7, RACE_HALF_ORC},
		{ "Oglign Dragon-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
		{ "Drew the Skilled",			100,  210, 115,  6,  6, RACE_HUMAN},
		{ "Orrax Dragonson",			150,  185, 110,  5,  9, RACE_DRACONIAN},
		{ "Anthrax Disease-Carrier",	250,  190, 115,  5,  7, RACE_BEASTMAN},
		{ "Arkhoth the Stout",			300,  195, 112,  4,  8, RACE_DWARF},
		{ "Sarlyas the Rotten",			50,   210, 115,  6,  6, RACE_ZOMBIE},
		{ "Tuethic Bare-Bones",			150,  185, 110,  5,  9, RACE_SKELETON},
		{ "Bilious",					250,  190, 115,  5,  7, RACE_BEASTMAN},
		{ "Fasgul",						300,  195, 112,  4,  8, RACE_ZOMBIE},
		{ "Ellefris the Paladin",		100,  210, 115,  6,  6, RACE_BARBARIAN},
		{ "K'trrik'k",					150,  185, 110,  5,  9, RACE_KLACKON},
		{ "Drocus Spiderfriend",		250,  190, 115,  5,  7, RACE_DARK_ELF},
		{ "Fungus Giant-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
		{ "Delantha",					100,  210, 115,  6,  6, RACE_ELF},
		{ "Solvistani the Ranger",		150,  185, 110,  5,  9, RACE_HALF_ELF},
		{ "Xoril the Slow",				250,  190, 115,  5,  7, RACE_GOLEM},
		{ "Aeon Flux",					200,  195, 112,  4,  8, RACE_HALF_ELF},
		{ "Nadoc the Strong",			100,  210, 115,  6,  6, RACE_HOBBIT},
		{ "Eramog the Weak",			150,  185, 110,  5,  9, RACE_KOBOLD},
		{ "Eowilith the Fair",			250,  190, 115,  5,  7, RACE_VAMPIRE},
		{ "Huimog Balrog-Slayer",		300,  195, 112,  4,  8, RACE_HALF_ORC},
		{ "Peadus the Cruel",			50,   210, 115,  6,  6, RACE_HUMAN},
		{ "Vamog Slayer",				150,  185, 110,  5,  9, RACE_HALF_OGRE},
		{ "Hooshnak the Vicious",		250,  190, 115,  5,  7, RACE_BEASTMAN},
		{ "Balenn War-Dancer",			300,  195, 112,  4,  8, RACE_BARBARIAN},
		{ "Arnold the Beastly",			50,   210, 115,  6,  6, RACE_BARBARIAN},
		{ "Arndal Beast-Slayer",		100,  185, 110,  5,  9, RACE_HALF_ELF},
		{ "Eddie Beast-Master",			250,  190, 115,  5,  7, RACE_HALF_ORC},
		{ "Oglign Dragon-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
	},
	{
		/* Temple - 22 unique names */
		{ "Ludwig the Humble",			50,   175, 109,  6, 15, RACE_DWARF},
		{ "Gunnar the Paladin",			100,  185, 110,  5, 23, RACE_HALF_TROLL},
		{ "Torin the Chosen",			250,  180, 107,  6, 20, RACE_HIGH_ELF},
		{ "Sarastro the Wise",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Sir Parsival the Pure",		250,  180, 107,  6, 20, RACE_HIGH_ELF},
		{ "Asenath the Holy",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "McKinnon",					100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Mistress Chastity",			150,  185, 110,  5, 23, RACE_HIGH_ELF},
		{ "Hashnik the Druid",			250,  180, 107,  6, 20, RACE_HOBBIT},
		{ "Finak",						300,  185, 109,  5, 15, RACE_YEEK},
		{ "Krikkik",					100,  175, 109,  6, 15, RACE_KLACKON},
		{ "Morival the Wild",			150,  185, 110,  5, 23, RACE_ELF},
		{ "Hoshak the Dark",			250,  180, 107,  6, 20, RACE_IMP},
		{ "Atal the Wise",				300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Ibenidd the Chaste",			100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Eridish",					150,  185, 110,  5, 23, RACE_HALF_TROLL},
		{ "Vrudush the Shaman",			250,  180, 107,  6, 20, RACE_HALF_OGRE},
		{ "Haob the Berserker",			300,  185, 109,  5, 15, RACE_BARBARIAN},
		{ "Proogdish the Youthfull",	100,  175, 109,  6, 15, RACE_HALF_OGRE},
		{ "Lumwise the Mad",			150,  185, 110,  5, 23, RACE_YEEK},
		{ "Muirt the Virtuous",			250,  180, 107,  6, 20, RACE_KOBOLD},
		{ "Dardobard the Weak",			300,  185, 109,  5, 15, RACE_SPECTRE},
		{ "Ludwig the Humble",			50,   175, 109,  6, 15, RACE_DWARF},
		{ "Gunnar the Paladin",			100,  185, 110,  5, 23, RACE_HALF_TROLL},
		{ "Torin the Chosen",			250,  180, 107,  6, 20, RACE_HIGH_ELF},
		{ "Sarastro the Wise",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Sir Parsival the Pure",		250,  180, 107,  6, 20, RACE_HIGH_ELF},
		{ "Asenath the Holy",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "McKinnon",					100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Mistress Chastity",			150,  185, 110,  5, 23, RACE_HIGH_ELF},
		{ "Hashnik the Druid",			250,  180, 107,  6, 20, RACE_HOBBIT},
		{ "Finak",						300,  185, 109,  5, 15, RACE_YEEK},
	},
	{
		/* Alchemist - 26 unique names */
		{ "Mauser the Chemist",			100,  190, 111,  5,  8, RACE_HALF_ELF},
		{ "Wizzle the Chaotic",			100,  190, 110,  6,  8, RACE_HOBBIT},
		{ "Midas the Greedy",			150,  200, 116,  6,  9, RACE_GNOME},
		{ "Ja-Far the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Kakalrakakal",				150,  200, 116,  6,  9, RACE_KLACKON},
		{ "Jal-Eth the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Fanelath the Cautious",		100,  190, 111,  5,  8, RACE_DWARF},
		{ "Runcie the Insane",			100,  190, 110,  6,  8, RACE_HUMAN},
		{ "Grumbleworth",				150,  200, 116,  6,  9, RACE_GNOME},
		{ "Flitter",					150,  220, 111,  4,  9, RACE_SPRITE},
		{ "Xarillus",					100,  190, 111,  5,  8, RACE_HUMAN},
		{ "Egbert the Old",				100,  190, 110,  6,  8, RACE_DWARF},
		{ "Valindra the Proud",			150,  200, 116,  6,  9, RACE_HIGH_ELF},
		{ "Taen the Alchemist",			150,  220, 111,  4,  9, RACE_HUMAN},
		{ "Cayd the Sweet",				100,  190, 111,  5,  8, RACE_VAMPIRE},
		{ "Fulir the Dark",				100,  190, 110,  6,  8, RACE_NIBELUNG},
		{ "Domli the Humble",			150,  200, 116,  6,  9, RACE_DWARF},
		{ "Yaarjukka Demonspawn",		150,  220, 111,  4,  9, RACE_IMP},
		{ "Gelaraldor the Herbmaster",	100,  190, 111,  5,  8, RACE_HIGH_ELF},
		{ "Olelaldan the Wise",			100,  190, 110,  6,  8, RACE_BARBARIAN},
		{ "Fthoglo the Demonicist",		150,  200, 116,  6,  9, RACE_IMP},
		{ "Dridash the Alchemist",		150,  220, 111,  4,  9, RACE_HALF_ORC},
		{ "Nelir the Strong",			100,  190, 111,  5,  8, RACE_CYCLOPS},
		{ "Lignus the Pungent",			100,  190, 110,  6,  8, RACE_HALF_ORC},
		{ "Tilba",						150,  200, 116,  6,  9, RACE_HOBBIT},
		{ "Myrildric the Wealthy",		150,  220, 111,  4,  9, RACE_HUMAN},
		{ "Mauser the Chemist",			100,  190, 111,  5,  8, RACE_HALF_ELF},
		{ "Wizzle the Chaotic",			100,  190, 110,  6,  8, RACE_HOBBIT},
		{ "Midas the Greedy",			150,  200, 116,  6,  9, RACE_GNOME},
		{ "Ja-Far the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Kakalrakakal",				150,  200, 116,  6,  9, RACE_KLACKON},
		{ "Jal-Eth the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
	},
	{
		/* Magic Shop - 23 unique names */
		{ "Lo Pan the Sorcerer",		200,  200, 110,  7,  8, RACE_HALF_ELF},
		{ "Buggerby the Great",			200,  215, 113,  6, 10, RACE_GNOME},
		{ "The Wizard of Yendor",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Rjak the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Skidney the Sorcerer",		150,  200, 110,  7,  8, RACE_HALF_ELF},
		{ "Kyria the Illusionist",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Nikki the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Solostoran",					150,  200, 110,  7,  8, RACE_SPRITE},
		{ "Achshe the Tentacled",		200,  215, 113,  6, 10, RACE_MIND_FLAYER},
		{ "Kaza the Noble",				300,  200, 110,  7, 10, RACE_HIGH_ELF},
		{ "Fazzil the Dark",			300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Keldorn the Grand",			150,  200, 110,  7,  8, RACE_DWARF},
		{ "Philanthropus",				200,  215, 113,  6, 10, RACE_HOBBIT},
		{ "Agnar the Enchantress",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Buliance the Necromancer",	300,  175, 110,  5, 11, RACE_BEASTMAN},
		{ "Vuirak the High-Mage",		150,  200, 110,  7,  8, RACE_BEASTMAN},
		{ "Madish the Smart",			200,  215, 113,  6, 10, RACE_BEASTMAN},
		{ "Falebrimbor",				300,  200, 110,  7, 10, RACE_HIGH_ELF},
		{ "Felil-Gand the Subtle",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Thalegord the Shaman",		150,  200, 110,  7,  8, RACE_BARBARIAN},
		{ "Cthoaloth the Mystic",		200,  215, 113,  6, 10, RACE_MIND_FLAYER},
		{ "Ibeli the Illusionist",		300,  200, 110,  7, 10, RACE_SKELETON},
		{ "Heto the Necromancer",		300,  175, 110,  5, 11, RACE_YEEK},
		{ "Lo Pan the Sorcerer",		200,  200, 110,  7,  8, RACE_HALF_ELF},
		{ "Buggerby the Great",			200,  215, 113,  6, 10, RACE_GNOME},
		{ "The Wizard of Yendor",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Rjak the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Skidney the Sorcerer",		150,  200, 110,  7,  8, RACE_HALF_ELF},
		{ "Kyria the Illusionist",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Nikki the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Solostoran",					150,  200, 110,  7,  8, RACE_SPRITE},
		{ "Achshe the Tentacled",		200,  215, 113,  6, 10, RACE_MIND_FLAYER},
	},
	{
		/* Black Market - 32 unique names */
		{ "Gary Gygaz",					200,  250, 150, 10,  5, RACE_HALF_TROLL},
		{ "Histor the Goblin",			200,  250, 150, 10,  5, RACE_HALF_ORC},
		{ "Quark the Ferengi",			300,  250, 150, 10,  5, RACE_DWARF},
		{ "Topi the Fair(?)",			300,  250, 150, 10,  5, RACE_HUMAN},
		{ "Vhassa the Dead",			200,  250, 150, 10,  5, RACE_ZOMBIE},
		{ "Kyn the Treacherous",		200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Bubonicus",					300,  250, 150, 10,  5, RACE_BEASTMAN},
		{ "Corpselight",				300,  250, 150, 10,  5, RACE_SPECTRE},
		{ "Parrish the Bloodthirsty",	200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Vile",						200,  250, 150, 10,  5, RACE_SKELETON},
		{ "Prentice the Trusted",		300,  250, 150, 10,  5, RACE_SKELETON},
		{ "Griella Humanslayer",		300,  250, 150, 10,  5, RACE_IMP},
		{ "Angel",						200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Flotsam the Bloated",		200,  250, 150, 10,  5, RACE_ZOMBIE},
		{ "Nieval",						300,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Anastasia the Luminous",		300,  250, 150, 10,  5, RACE_SPECTRE},
		{ "Charity the Necromancer",	200,  250, 150, 10,  5, RACE_DARK_ELF},
		{ "Pugnacious the Pugilist",	200,  250, 150, 10,  5, RACE_HALF_ORC},
		{ "Footsore the Lucky",			300,  250, 150, 10,  5, RACE_BEASTMAN},
		{ "Sidria Lighfingered",		300,  250, 150, 10,  5, RACE_HUMAN},
		{ "Riatho the Juggler",			200,  250, 150, 10,  5, RACE_HOBBIT},
		{ "Janaaka the Shifty",			200,  250, 150, 10,  5, RACE_GNOME},
		{ "Cina the Rogue",				300,  250, 150, 10,  5, RACE_GNOME},
		{ "Arunikki Greatclaw",			300,  250, 150, 10,  5, RACE_DRACONIAN},
		{ "Chaeand the Poor",			200,  250, 150, 10,  5, RACE_HUMAN},
		{ "Afardorf the Brigand",		200,  250, 150, 10,  5, RACE_BARBARIAN},
		{ "Lathaxl the Greedy",			300,  250, 150, 10,  5, RACE_MIND_FLAYER},
		{ "Falarewyn",					300,  250, 150, 10,  5, RACE_SPRITE},
		{ "Vosur the Wrinkled",			200,  250, 150, 10,  5, RACE_NIBELUNG},
		{ "Araord the Handsome",		200,  250, 150, 10,  5, RACE_AMBERITE},
		{ "Theradfrid the Loser",		300,  250, 150, 10,  5, RACE_HUMAN},
		{ "One-Legged Eroolo",			300,  250, 150, 10,  5, RACE_HALF_OGRE},
	},
	{
		/* Home */
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
		{ "Your home",					0,    100, 100,  0, 99, 99},
	},
	{
		/* Bookstore - 21 unique names */
		{ "Dolaf the Greedy",			100,  175, 108, 4, 12, RACE_HUMAN},
		{ "Odnar the Sage",				150,  120, 105, 6, 16, RACE_HIGH_ELF},
		{ "Gandar the Neutral",			250,  120, 110, 7, 19, RACE_DARK_ELF},
		{ "Ro-sha the Patient",			300,  140, 105, 6, 12, RACE_ELF},
		{ "Randolph Carter",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Sarai the Swift",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Bodril the Seer",			200,  120, 105, 6, 16, RACE_HIGH_ELF},
		{ "Veloin the Quiet",			250,  120, 110, 7, 19, RACE_ZOMBIE},
		{ "Vanthylas the Learned",		300,  140, 105, 6, 12, RACE_MIND_FLAYER},
		{ "Ossein the Literate",		150,  175, 108, 4, 12, RACE_SKELETON},
		{ "Olvar Bookworm",				200,  120, 105, 6, 16, RACE_VAMPIRE},
		{ "Shallowgrave",				250,  120, 110, 7, 19, RACE_ZOMBIE},
		{ "Death Mask",					300,  140, 105, 6, 12, RACE_ZOMBIE},
		{ "Asuunu the Learned",			150,  175, 108, 4, 12, RACE_MIND_FLAYER},
		{ "Prirand the Dead",			200,  120, 105, 6, 16, RACE_ZOMBIE},
		{ "Ronar the Iron",				250,  120, 110, 7, 19, RACE_GOLEM},
		{ "Galil-Gamir",				300,  140, 105, 6, 12, RACE_ELF},
		{ "Rorbag Book-Eater",			150,  175, 108, 4, 12, RACE_KOBOLD},
		{ "Kiriarikirk",				200,  120, 105, 6, 16, RACE_KLACKON},
		{ "Rilin the Quiet",			250,  120, 110, 7, 19, RACE_DWARF},
		{ "Isung the Lord",				300,  140, 105, 6, 12, RACE_HIGH_ELF},
		{ "Dolaf the Greedy",			100,  175, 108, 4, 12, RACE_HUMAN},
		{ "Odnar the Sage",				150,  120, 105, 6, 16, RACE_HIGH_ELF},
		{ "Gandar the Neutral",			250,  120, 110, 7, 19, RACE_DARK_ELF},
		{ "Ro-sha the Patient",			300,  140, 105, 6, 12, RACE_ELF},
		{ "Randolph Carter",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Sarai the Swift",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Bodril the Seer",			200,  120, 105, 6, 16, RACE_HIGH_ELF},
		{ "Veloin the Quiet",			250,  120, 110, 7, 19, RACE_ZOMBIE},
		{ "Vanthylas the Learned",		300,  140, 105, 6, 12, RACE_MIND_FLAYER},
		{ "Ossein the Literate",		150,  175, 108, 4, 12, RACE_SKELETON},
		{ "Olvar Bookworm",				200,  120, 105, 6, 16, RACE_VAMPIRE},
	}
};

/*
 * The list of owners for the new buildings
 * Building owners (exactly MAX_B_OWN owners per building, chosen randomly)
 * { name, greed, race}
 */
b_own_type b_owners[MAX_BLDG][MAX_B_OWN] =
{
	{
		/* Weaponmaster */
		{ "Suiyan",				150,			RACE_HUMAN},
	},
	
	{
		/* Zymurgist */
		{ "Tanistil",			100,			RACE_ELF},
	},
	
	{
		/* Magesmith Weapons */
		{ "Aotnron",			100,			RACE_HALF_ORC},
	},
	
	{
		/* Magesmith Armour */
		{ "Paoingth",			100,			RACE_DARK_ELF},
	},
	
	{
		/* Mutations */
		{ "Aaognwth",			120,			RACE_HUMAN},
	},
	
	{
		/* Maps */
		{ "Fsanong of the East", 90,			RACE_HUMAN},
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
 * further increases in speed are more or less pointless,
 * except to balance out heavy inventory.
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
		10,  100,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
		0,
		0x7FF,
	},
	{
		"Half-Elf",
		{ -1,  1,  1,  1, -1,  1 },
		2,  3,  3,  1, 6,  11, -2,  3,
		9,  110,
		24, 16,
		66,  6, 130, 15,
		62,  6, 100, 10,
		2,
		0x7FF,
	},
	{
		"Elf",
		{ -1,  2,  2,  1, -2,  2 },
		5,  6,  6,  2, 8,  12, -6, 6,
		8,  120,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		3,
		0x75F,
	},
	{
		"Hobbit",
		{ -2,  2,  1,  3,  2,  1 },
		15, 18, 18, 5, 12,  15, -11, 6,
		7,  110,
		21, 12,
		36,  3, 60,  3,
		33,  3, 50,  3,
		4,
		0x40B,
	},
	{
		"Gnome",
		{ -1,  2,  0,  2,  1, -2 },
		10, 12, 12,  3, 6,  13, -8, 0,
		8,  135,
		50, 40,
		42,  3, 90,  6,
		39,  3, 75,  3,
		4,
		0x60F,
	},
	{
		"Dwarf",
		{  2, -2,  2, -2,  2, -3 },
		2,  9,  10,  -1,  7,  10, 7,  0,
		11,  125,
		35, 15,
		48,  3, 150, 10,
		46,  3, 120, 10,
		5,
		0x005,
	},
	{
		"Half-Orc",
		{  2, -1,  0,  0,  1, -4 },
		-3, -3, -3,  -1,  0, 7, 3, -3,
		10,  110,
		11,  4,
		66,  1, 150,  5,
		62,  1, 120,  5,
		3,
		0x18D,
	},
	{
		"Half-Troll",
		{ 4, -4, -2, -4,  3, -6 },
		-5, -8, -8, -2,  -1, 5, 10, -5,
		12,  137,
		20, 10,
		96, 10, 250, 50,
		84,  8, 225, 40,
		3,
		0x005,
	},
	{
		"Amberite",
		{  1,  2,  2,  2,  3,  2 },
		4,  5,  5,  2, 3, 13, 6, 0,
		10,  225,
		50, 50,
		82, 5, 190, 20,
		78,  6, 180, 15,
		0,
		0x7FF,
	},
	{
		"High-Elf",
		{  1,  3,  2,  3,  1,  5 },
		4,  20, 20,  4,  3, 14, 0, 10,
		10,  200,
		100, 30,
		90, 10, 190, 20,
		82, 10, 180, 15,
		4,
		0x75F,
	},
	{
		"Barbarian",
		{ 3, -2,  -1,  1,  2, -2 },
		-2, -10, 2,  -1,  1, 7, 10, 0,
		11, 120,
		14, 8,
		82, 5, 200, 20,
		78,  6, 190, 15,
		0,
		0x09D,
	},
	{
		"Half-Ogre",
		{ 3, -1, -1, -1, 3, -3 },
		-3, -5, -5, -2, -1, 5, 12, 0,
		12,  130,
		40, 10,
		92, 10, 255, 60,
		80,  8, 235, 60,
		3,
		0x407,
	},
	{
		"Half-Giant",
		{ 4, -2, -2, -2, 3, -3 },
		-6, -8, -6, -2, -1, 5, 13, 2,
		13, 150,
		40, 10,
		100, 10, 255, 65,
		80, 10, 240, 64,
		3,
		0x011,
	},
	{
		"Half-Titan",
		{ 5, 1, 1, -2, 3, 1 },
		-5, 5, 2, -2, 1, 8, 13, 0,
		14, 255,
		100, 30,
		111, 11, 255, 86,
		99, 11, 250, 86,
		0,
		0x727,
	},
	{
		"Cyclops",
		{ 4, -3, -3, -3, 4, -6 },
		-4, -5, -5, -2, -2, 5, 10, 6,
		13, 130,
		50, 24,
		92, 10, 255, 60,
		80,  8, 235, 60,
		1,
		0x005,
	},
	{
		"Yeek",
		{ -2, 1, 1, 1, -2, -7 },
		2, 4, 10, 3, 5, 15, -5, -5,
		7, 100,
		14, 3,
		50,  3, 90,  6,
		50,  3, 75,  3,
		2,
		0x60F,
	},
	{
		"Klackon",
		{ 2, -1, -1, 1, 2, -2 },
		10, 5, 5, 0, -1, 10, 5, 5,
		12, 135,
		20, 3,
		60,  3, 80,  4,
		54,  3, 70,  4,
		2,
		0x011,
	},
	{
		"Kobold",
		{ 1, -1, 0, 1, 0, -4 },
		-2, -3, -2, -1, 1, 8, 8, -8,
		9, 125,
		11,  3,
		60,  1, 130,  5,
		55,  1, 100,  5,
		3,
		0x009,
	},
	{
		"Nibelung",
		{ 1, -1, 2, 0, 2, -4 },
		3, 5, 10, 1, 5, 10, 5, 0,
		11, 135,
		40, 12,
		43,  3, 92,  6,
		40,  3, 78,  3,
		5,
		0x40F,
	},
	{
		"Dark-Elf",
		{ -1, 3, 2, 2, -2, 1 },
		5, 15, 20, 3, 8, 12, -5, 7,
		9, 150,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		5,
		0x7DF,
	},
	{
		"Draconian",
		{ 2, 1, 1, 1, 2, -3 },
		-2, 5, 3, 0, 1, 10, 5, 5,
		11, 250,
		75, 33,
		76,  1, 160,  5,
		72,  1, 130,  5,
		2,
		0x757,
	},
	{
		"Mindflayer",
		{ -3, 4, 4, 0, -2, -5 },
		10, 25, 15, 2, 5, 12, -8, -5,
		9, 140,
		100, 25,
		68,  6, 142, 15,
		63,  6, 112, 10,
		4,
		0x746,
	},
	{
		"Imp",
		{ -1, -1, -1, 1, 2, -3 },
		-3, 2, -1, 1, -1, 10, 5, -5,
		10, 110,
		13,  4,
		68,  1, 150,  5,
		64,  1, 120,  5,
		3,
		0x7CB,
	},
	{
		"Golem",
		{ 4, -5, -5, -2, 4, -4 },
		-5, -5, 10, -1, -1, 8, 10, 0,
		12, 200,
		1, 100,
		66,  1, 200,  6,
		62,  1, 180,  6,
		4,
		0x001,
	},
	{
		"Skeleton",
		{ 0, -2, -2, 0, 1, -4 },
		-5, -5, 5, -1, -1, 8, 8, 0,
		10, 145,
		100, 35,
		72,  6, 50, 5,
		66,  4, 50, 5,
		2,
		0x70F,
	},
	{
		"Zombie",
		{ 2, -6, -6, 1, 4, -5 },
		-5, -5, 8, -1, -1, 5, 10, 0,
		13, 135,
		100, 30,
		72, 6, 100, 25,
		66, 4, 100, 20,
		2,
		0x001,
	},
	{
		"Vampire",
		{ 3, 3, -1, -1, 1, 2 },
		4, 10, 10, 4, 1, 8, 5, 0,
		11, 200,
		100, 30,
		72,  6, 180, 25,
		66,  4, 150, 20,
		5,
		0x7FF,
	},
	{
		"Spectre",
		{ -5, 4, 4, 2, -3, -6 },
		10, 25, 20, 5, 5, 14, -10, -5,
		7, 180,
		100, 30,
		72, 6, 100, 25,
		66, 4, 100, 20,
		5,
		0x74E,    /* Mage, Priest, Rogue, Warrior-Mage, Monk */
	},
	{
		"Sprite",
		{ -4, 3, 3, 3, -2, 2 },
		10, 10, 10, 4, 10, 10, -8, 0,
		7, 175,
		50, 25,
		32,  2, 75,  2,
		29,  2, 65,  2,
		4,
		0x65E,
	},
	{
		"Beastman",
		{ 2, -2, -1, -1, 2, -4 },
		-5, -2, -1, -1, -1, 5, 9, 5,
		11, 140,
		14, 6,
		65,  6, 150, 20,
		61,  6, 120, 15,
		0,
		0x7CF,
	}
};


/*
 * Player Classes
 *
 *      Title,
 *      {STR,INT,WIS,DEX,CON,CHR},
 *      c_dis, c_dev, c_sav, c_stl, c_srh, c_fos, c_thn, c_thb,
 *      x_dis, x_dev, x_sav, x_stl, x_srh, x_fos, x_thn, x_thb,
 *      HD, Exp, pet_upkeep_div
 */
player_class class_info[MAX_CLASS] =
{
	{
		"Warrior",
		{ 5, -2, -2, 2, 2, -1},
		25, 18, 18, 1,  14, 2, 25, 17,
		12, 7,  10, 0,  0,  0,  100, 55,
		9,  0, 20
	},

	{
		"Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},

	{
		"Priest",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},

	{
		"Rogue",
		{ 2, 1, -2, 3, 1, -1},
		45, 32, 28, 5, 32, 24, 15, 20,
		15, 10, 10, 0,  0,  0, 70, 40,
		6, 25, 20
	},

	{
		"Ranger",
		{ 2, 2, 0, 1, 1, 1},
		30, 32, 28, 3,  24, 16, 15, 20,
		8,  10, 10, 0,  0,  0,  65, 63,
		4, 30, 20
	},

	{
		"Paladin",
		{ 3, -3, 1, 0, 2, 2},
		20, 24, 26, 1,  12, 2, 19, 10,
		7,  10, 11, 0,  0,  0,  76, 14,
		6, 35, 20
	},

	{
		"Warrior-Mage",
		{ 2, 2, 0, 1, 0, 1},
		30, 30, 28, 2,  18, 16, 20, 20,
		7,  10,  9, 0,  0,  0,  75, 50,
		4, 50, 20
	},

	{
		"Chaos-Warrior",
		{ 2, 1, 0, 1, 2, -2},
		20, 25, 25, 1,  14, 12, 23, 7,
		7,  11, 10, 0,  0,  0,  90, 40,
		6, 35, 20
	},

	{
		"Monk",
		{ 2, -1, 1, 3, 2, 1},
		45, 32, 28, 5, 32, 24, 12, 14,
		15, 11, 10, 0,  0,  0, 30, 25,
		6, 40, 20
	},

	{
		"Mindcrafter",
		{-1, 0, 3, -1, -1, 2},   /* note: spell stat is Wis */
		30, 30, 30, 3,  22, 16, 15, 15,
		10, 10, 10, 0,   0,  0, 30, 20,
		2, 25, 20
	},

	{
		"High-Mage",
		{-5, 4, 0, 0, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  15, 10,
		0, 30, 12
	},
};




/*
 * Hack -- the spell information table.
 *
 *   Class Name
 *
 *   Spell Book
 *   Spell Xtra
 *
 *   Spell Stat,
 *   Spell Type,
 *
 *   Spell Level,
 *   Spell Encumbrance,
 *
 *   Array of { Lev, Mana, Fail, Exp/Lev }
 */
player_magic magic_info[MAX_CLASS] =
{
	{
		/*** Warrior ***/

		0,
		0,

		A_STR,
		0,

		99,
		0,
		{
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			{

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
		},
	},

	{
		/*** Mage ***/

		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Mage: Life magic */

				{ 1, 1, 30, 4 },
				{ 3, 2, 35, 4 },
				{ 4, 3, 35, 4 },
				{ 5, 5, 35, 4 },
				{ 7, 7, 35, 4 },
				{ 9, 8, 40, 4 },
				{ 12, 12, 40, 3 },
				{ 15, 14, 45, 3 },

				{ 16, 16, 45, 4},
				{ 17, 17, 50, 4},
				{ 18, 18, 50, 4},
				{ 19, 19, 50, 4},
				{ 20, 20, 50, 4},
				{ 23, 23, 50, 4},
				{ 30, 30, 55, 5},
				{ 35, 70, 75, 5},

				{ 26, 30, 50, 75 },
				{ 28, 25, 70, 150 },
				{ 33, 33, 60, 75 },
				{ 35, 35, 60, 75 },
				{ 35, 35, 70, 75 },
				{ 35, 55, 80, 115 },
				{ 39, 40, 80, 125 },
				{ 46, 70, 80, 150 },

				{ 9, 9, 50, 40 },
				{ 25, 25, 50, 50 },
				{ 35, 85, 80, 115 },
				{ 42, 100, 80, 225 },
				{ 45, 90, 80, 115 },
				{ 48, 50, 80, 100 },
				{ 49, 100, 80, 250 },
				{ 50, 100, 80, 250 }
			},

			/* Mage: Sorcery */

			{
				{ 1, 1, 23, 4 },
				{ 1, 2, 24, 4 },
				{ 3, 3, 25, 1 },
				{ 3, 3, 30, 1 },
				{ 4, 4, 30, 1 },
				{ 5, 5, 35, 5 },
				{ 6, 5, 30, 4 },
				{ 7, 7, 75, 9 },

				{ 9, 7, 75, 8 },
				{ 10, 7, 75, 8 },
				{ 11, 7, 75, 7 },
				{ 13, 7, 50, 6 },
				{ 18, 12, 60, 8 },
				{ 22, 12, 60, 8 },
				{ 28, 20, 70, 15 },
				{ 33, 30, 75, 20 },

				{ 3, 3, 25, 15 },
				{ 10, 10, 70, 40 },
				{ 10, 10, 80, 40 },
				{ 12, 12, 80, 40 },
				{ 14, 10, 60, 25 },
				{ 20, 18, 85, 50 },
				{ 20, 18, 60, 25 },
				{ 25, 25, 75, 19 },

				{ 10, 10, 40, 20 },
				{ 25, 25, 75, 70 },
				{ 25, 30, 95, 160 },
				{ 30, 40, 80, 120 },
				{ 40, 80, 95, 200 },
				{ 40, 100, 95, 200 },
				{ 42, 50, 90, 175 },
				{ 45, 70, 75, 250 },
			},

			/* Mage: Nature Magic */

			{
				{ 1, 1, 23, 4 },
				{ 3, 3, 25, 3 },
				{ 3, 3, 25, 1 },
				{ 4, 4, 35, 4 },
				{ 4, 4, 50, 5 },
				{ 4, 5, 50, 5 },
				{ 5, 5, 50, 5 },
				{ 5, 5, 35, 4 },

				{ 5, 5, 40, 6 },
				{ 5, 5, 30, 6 },
				{ 7, 6, 45, 6 },
				{ 7, 6, 40, 6 },
				{ 9, 6, 30, 5 },
				{ 19, 12, 55, 8 },
				{ 25, 25, 90, 50 },
				{ 40, 100, 95, 50 },

				{ 7, 7, 20, 28 },
				{ 9, 12, 40, 44 },
				{ 10, 12, 75, 120 },
				{ 15, 20, 85, 60 },
				{ 30, 30, 90, 100 },
				{ 37, 40, 90, 200 },
				{ 38, 45, 75, 200},
				{ 40, 90, 90, 250 },


				{ 20, 18, 60, 25 },
				{ 23, 23, 80, 50 },
				{ 25, 25, 75, 29 },
				{ 30, 27, 75, 35 },
				{ 35, 30, 85, 65 },
				{ 37, 35, 90, 100 },
				{ 40, 90, 95, 250 },
				{ 40, 75, 65, 150 }
			},

				/* Mage: Chaos Magic */

			{
				{ 1, 1, 20, 4 },
				{ 1, 2, 22, 4 },
				{ 2, 2, 25, 4 },
				{ 5, 5, 30, 1 },
				{ 9, 6, 50, 1 },
				{ 13, 9, 45, 6 },
				{ 14, 9, 45, 6 },
				{ 15, 9, 35, 5 },

				{ 17, 10, 25, 5 },
				{ 19, 12, 45, 9 },
				{ 21, 13, 45, 10 },
				{ 23, 15, 50, 11 },
				{ 25, 16, 50, 12 },
				{ 25, 18, 60, 8 },
				{ 30, 20, 80, 15 },
				{ 35, 40, 85, 40 },

				{ 11, 7, 45, 9 },
				{ 15, 15, 80, 35 },
				{ 16, 14, 80, 35 },
				{25, 25, 85, 100 },
				{ 30, 25, 85, 150 },
				{ 42, 50, 85, 250 },
				{ 45, 90, 80, 250 },
				{ 47, 100, 90, 250 },

				{ 20, 20, 66, 8 },
				{ 35, 32, 85, 35 },
				{ 37, 34, 75, 40 },
				{ 41, 42, 85, 100 },
				{ 43, 44, 80, 150 },
				{ 45, 48, 85, 200 },
				{ 47, 75, 80, 200 },
				{ 49, 100, 85, 250 }
			},

				/* Mage: Death Magic */

			{
				{ 1, 1, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 3, 3, 27, 3 },
				{ 5, 5, 30, 4 },
				{ 7, 10, 75, 6 },
				{ 9, 9, 30, 4 },
				{ 10, 10, 30, 4 },

				{ 12, 12, 40, 5 },
				{ 13, 12, 30, 4 },
				{ 18, 15, 50, 10 },
				{ 23, 20, 60, 16 },
				{ 30, 75, 50, 30 },
				{ 33, 35, 60, 16 },
				{ 37, 25, 95, 25 },
				{ 45, 50, 95, 150 },

				{ 10, 20, 80, 180 },
				{ 10, 15, 80, 30 },
				{ 11, 11, 30, 15 },
				{ 30, 25, 75, 50 },
				{ 33, 35, 60, 125 },
				{ 33, 90, 70, 90 },
				{ 40, 40, 70, 200 },
				{ 40, 75, 80, 100 },

				{ 20, 20, 75, 50 },
				{ 25, 66, 95, 250 },
				{ 30, 40, 95, 250 },
				{ 33, 35, 70, 40 },
				{ 37, 35, 80, 70 },
				{ 42, 120, 95, 250 },
				{ 45, 100, 90, 250 },
				{ 47, 100, 90, 250 }
			},

				/* Mage: Trump Magic */

			{
				{ 1, 1, 25, 3 },
				{ 3, 3, 25, 4 },
				{ 5, 5, 37, 8 },
				{ 6, 6, 40, 8 },
				{ 7, 7, 20, 4 },
				{ 9, 9, 30, 6 },
				{ 14, 12, 30, 6 },
				{ 17, 15, 30, 5 },

				{ 20, 20, 40, 8 },
				{ 24, 22, 30, 8 },
				{ 28, 24, 30, 8 },
				{ 30, 25, 35, 9 },
				{ 33, 28, 40, 12 },
				{ 35, 30, 35, 10 },
				{ 40, 35, 40, 15 },
				{ 42, 40, 35, 12 },

				{ 15, 15, 40, 20 },
				{ 24, 24, 35, 25 },
				{ 26, 26, 35, 30 },
				{ 30, 30, 35, 35 },
				{ 35, 70, 40, 100 },
				{ 40, 100, 45, 250 },
				{ 42, 50, 25, 75 },
				{ 45, 100, 45, 200 },

				{ 30, 30, 30, 50 },
				{ 35, 50, 45, 100 },
				{ 36, 80, 40, 150 },
				{ 39, 80, 40, 150 },
				{ 42, 100, 40, 200 },
				{ 47, 100, 40, 150 },
				{ 48, 100, 40, 200 },
				{ 49, 100, 40, 220 }
			},

				/* Mage: Arcane Magic */

			{
				{ 1, 1, 20, 4 },
				{ 1, 1, 33, 5 },
				{ 1, 1, 33, 4 },
				{ 2, 1, 33, 5 },
				{ 2, 2, 33, 5 },
				{ 4, 4, 40, 6 },
				{ 5, 5, 33, 7 },
				{ 6, 5, 44, 5 },

				{ 7, 6, 40, 7 },
				{ 8, 8, 60, 7 },
				{ 9, 8, 50, 6 },
				{ 9, 9, 50, 6 },
				{ 9, 9, 50, 6 },
				{ 11, 10, 50, 6 },
				{ 12, 12, 50, 5 },
				{ 13, 12, 50, 5 },

				{ 14, 12, 50, 5 },
				{ 15, 12, 50, 5 },
				{ 16, 14, 33, 6 },
				{ 18, 15, 50, 8 },
				{ 20, 16, 60, 9 },
				{ 23, 18, 60, 9 },
				{ 25, 20, 70, 12 },
				{ 25, 20, 60, 13 },

				{ 28, 25, 70, 30 },
				{ 35, 35, 80, 25 },
				{ 38, 30, 60, 25 },
				{ 40, 30, 70, 25 },
				{ 41, 30, 66, 30 },
				{ 42, 30, 80, 40 },
				{ 45, 50, 70, 50 },
				{ 49, 100, 80, 200 }
			}
		}
	},

	{
		/*** Priest ***/

		TV_LIFE_BOOK,
		0,

		A_WIS,
		1,

		1,
		350,
		{
			/* Priest: Life Magic */
			{
				{ 1, 1, 10, 4 },
				{ 1, 2, 15, 4 },
				{ 1, 2, 20, 4 },
				{ 3, 2, 25, 1 },
				{ 3, 3, 27, 2 },
				{ 4, 4, 28, 2 },
				{ 5, 4, 32, 4 },
				{ 7, 5, 38, 4 },

				{ 7, 6, 38, 5 },
				{ 9, 6, 38, 4 },
				{ 9, 7, 40, 4 },
				{ 10, 8, 38, 4 },
				{ 10, 8, 40, 4 },
				{ 11, 8, 42, 4 },
				{ 20, 16, 60, 7 },
				{ 33, 55, 90, 15 },

				{ 15, 14, 50, 50 },
				{ 16, 14, 80, 60 },
				{ 17, 14, 55, 70 },
				{ 24, 20, 55, 70 },
				{ 25, 20, 70, 120 },
				{ 25, 25, 80, 250 },
				{ 39, 32, 95, 200 },
				{ 44, 44, 80, 250 },

				{ 5, 5, 50, 80 },
				{ 15, 14, 50, 100 },
				{ 30, 50, 80, 130 },
				{ 35, 70, 90, 250 },
				{ 40, 50, 80, 130 },
				{ 40, 40, 80, 200 },
				{ 42, 90, 85, 200 },
				{ 45, 90, 85, 250 },
			},

			/* Priest: Sorcery */
			{
				{ 2, 1, 23, 4 },
				{ 3, 2, 24, 4 },
				{ 4, 3, 25, 1 },
				{ 5, 4, 30, 1 },
				{ 6, 5, 30, 1 },
				{ 7, 6, 35, 5 },
				{ 9, 7, 30, 4 },
				{ 11, 10, 75, 9 },

				{ 13, 11, 75, 8 },
				{ 14, 12, 75, 6 },
				{ 15, 13, 75, 7 },
				{ 16, 14, 50, 6 },
				{ 22, 15, 60, 8 },
				{ 27, 17, 65, 10 },
				{ 30, 22, 70, 15 },
				{ 36, 33, 75, 20 },

				{ 7, 7, 25, 15 },
				{ 12, 12, 70, 40 },
				{ 14, 14, 80, 40 },
				{ 15, 15, 80, 40 },
				{ 18, 18, 60, 25 },
				{ 20, 20, 85, 50 },
				{ 22, 22, 60, 25 },
				{ 27, 27, 75, 19 },

				{ 13, 13, 40, 20 },
				{ 24, 24, 75, 70 },
				{ 27, 30, 95, 160 },
				{ 33, 40, 80, 120 },
				{ 42, 80, 95, 200 },
				{ 42, 100, 95, 200 },
				{ 45, 50, 90, 175 },
				{ 48, 70, 75, 250 },
			},

			/* Priest: Nature Magic */
			{
				{ 2, 1, 25, 4 },
				{ 5, 3, 25, 4 },
				{ 5, 4, 25, 1 },
				{ 6, 5, 35, 4 },
				{ 6, 5, 50, 5 },
				{ 6, 6, 50, 5 },
				{ 7, 7, 50, 5 },
				{ 7, 7, 35, 4 },

				{ 7, 7, 40, 6 },
				{ 8, 7, 30, 6 },
				{ 9, 10, 40, 6 },
				{ 10, 10, 40, 6 },
				{ 11, 11, 30, 5 },
				{ 20, 20, 65, 7 },
				{ 30, 30, 55, 8 },
				{ 42, 100, 95, 50 },

				{ 9, 9, 20, 28 },
				{ 11, 12, 40, 44 },
				{ 12, 13, 75, 120 },
				{ 18, 20, 85, 60 },
				{ 35, 35, 80, 50 },
				{ 39, 40, 90, 200 },
				{ 40, 50, 85, 250 },
				{ 42, 90, 90, 250 },

				{ 22, 22, 60, 24 },
				{ 25, 25, 60, 25 },
				{ 27, 27, 75, 29 },
				{ 32, 30, 75, 29 },
				{ 37, 32, 85, 65 },
				{ 39, 37, 90, 100 },
				{ 42, 90, 95, 250 },
				{ 44, 80, 65, 150 },
			},

			/* Priest: Chaos Magic */
			{
				{ 2, 1, 22, 4 },
				{ 3, 2, 24, 4 },
				{ 4, 3, 26, 4 },
				{ 5, 4, 30, 6 },
				{ 10, 6, 30, 5 },
				{ 11, 6, 50, 5 },
				{ 16, 11, 50, 6 },
				{ 17, 11, 35, 5 },

				{ 19, 15, 50, 7 },
				{ 21, 16, 50, 9 },
				{ 23, 18, 80, 20 },
				{ 25, 18, 50, 11 },
				{ 27, 20, 65, 12 },
				{ 29, 22, 60, 8 },
				{ 33, 23, 80, 15},
				{ 37, 42, 85, 40 },


				{ 14, 11, 45, 9 },
				{ 17, 17, 70, 20 },
				{ 20, 18, 80, 35 },
				{ 27, 27, 70, 35 },
				{ 35, 30, 85, 150 },
				{ 45, 55, 95, 250 },
				{ 47, 90, 90, 250 },
				{ 49, 100, 90, 250 },

				{ 25, 25, 66, 8 },
				{ 37, 37, 85, 35 },
				{ 39, 37, 75, 50 },
				{ 43, 45, 85, 100 },
				{ 45, 47, 90, 150 },
				{ 47, 50, 95, 200 },
				{ 49, 95, 80, 200 },
				{ 50, 100, 95, 250 },
			},

			/* Priest: Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 3, 3, 25, 4 },
				{ 5, 4, 27, 4 },
				{ 7, 7, 30, 4 },
				{ 9, 11, 75, 6 },
				{ 11, 11, 30, 4 },
				{ 12, 12, 40, 5 },

				{ 14, 14, 40, 5 },
				{ 16, 16, 30, 4 },
				{ 21, 20, 50, 10 },
				{ 25, 24, 60, 16 },
				{ 33, 75, 90, 30 },
				{ 35, 35, 60, 16 },
				{ 40, 30, 95, 100 },
				{ 50, 52, 95, 150 },

				{ 13, 20, 80, 180 },
				{ 13, 15, 80, 30 },
				{ 14, 15, 30, 15 },
				{ 33, 33, 70, 33 },
				{ 35, 35, 60, 125 },
				{ 35, 95, 70, 90 },
				{ 44, 44, 80, 200 },
				{ 45, 75, 80, 100 },

				{ 25, 25, 75, 50 },
				{ 30, 75, 95, 250 },
				{ 35, 45, 95, 250 },
				{ 40, 40, 70, 40 },
				{ 42, 40, 80, 70 },
				{ 48, 125, 95, 250 },
				{ 49, 100, 90, 250 },
				{ 50, 111, 90, 250 },
			},

			/* Priest: Trump Magic */
			{
				{ 1, 1, 25, 3 },
				{ 4, 4, 25, 4 },
				{ 6, 5, 37, 8 },
				{ 7, 7, 40, 8 },
				{ 9, 9, 20, 4 },
				{ 11, 11, 30, 6 },
				{ 17, 14, 30, 6 },
				{ 19, 17, 30, 5 },

				{ 22, 22, 40, 8 },
				{ 26, 24, 30, 8 },
				{ 30, 25, 30, 8 },
				{ 32, 30, 35, 9 },
				{ 35, 30, 40, 12 },
				{ 38, 35, 35, 10 },
				{ 42, 40, 40, 15 },
				{ 45, 45, 35, 12 },

				{ 17, 17, 40, 20 },
				{ 27, 25, 35, 25 },
				{ 29, 27, 35, 30 },
				{ 33, 30, 35, 35 },
				{ 38, 75, 40, 100 },
				{ 41, 110, 45, 250 },
				{ 45, 55, 25, 75 },
				{ 49, 125, 45, 200 },

				{ 32, 30, 30, 50 },
				{ 38, 55, 45, 100 },
				{ 40, 85, 40, 150 },
				{ 43, 85, 40, 150 },
				{ 46, 110, 40, 200 },
				{ 48, 115, 40, 150 },
				{ 49, 120, 40, 200 },
				{ 50, 125, 40, 220 }
			},

			/* Priest: Arcane Magic */
			{
				{ 1, 1, 20, 4 },
				{ 1, 1, 33, 5 },
				{ 2, 1, 33, 4 },
				{ 2, 2, 33, 5 },
				{ 3, 3, 33, 5 },
				{ 5, 5, 40, 6 },
				{ 6, 6, 33, 7 },
				{ 7, 6, 44, 5 },

				{ 8, 7, 40, 7 },
				{ 9, 8, 60, 7 },
				{ 10, 9, 50, 6 },
				{ 11, 10, 50, 6 },
				{ 12, 11, 50, 6 },
				{ 13, 12, 50, 6 },
				{ 14, 13, 50, 5 },
				{ 15, 14, 50, 5 },

				{ 16, 15, 50, 5 },
				{ 17, 16, 50, 5 },
				{ 18, 17, 33, 6 },
				{ 19, 18, 50, 8 },
				{ 22, 20, 60, 9 },
				{ 24, 22, 60, 9 },
				{ 27, 24, 70, 12 },
				{ 29, 26, 60, 13 },

				{ 33, 30, 80, 50 },
				{ 37, 36, 80, 25 },
				{ 40, 37, 60, 25 },
				{ 42, 38, 70, 25 },
				{ 44, 39, 66, 30 },
				{ 46, 40, 80, 40 },
				{ 47, 55, 70, 50 },
				{ 50, 120, 80, 200 }
			}
		}
	},

	{
		/*** Rogue ***/
		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		5,
		350,
		{
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},

			/* Rogue (Burglar): Sorcery */
			{
				{ 5, 1, 50, 1 },
				{ 7, 2, 55, 1 },
				{ 8, 3, 65, 1 },
				{ 9, 3, 65, 1 },
				{ 13, 6, 75, 1 },
				{ 15, 7, 75, 1 },
				{ 17, 9, 75, 1 },
				{ 21, 12, 80, 1 },

				{ 25, 14, 80, 1 },
				{ 27, 15, 80, 1 },
				{ 29, 17, 75, 2 },
				{ 30, 20, 80, 4 },
				{ 31, 23, 80, 5 },
				{ 32, 25, 70, 6 },
				{ 35, 30, 80, 12 },
				{ 40, 35, 75, 20 },

				{ 9, 3, 65, 5 },
				{ 13, 10, 70, 5 },
				{ 14, 10, 80, 8 },
				{ 15, 10, 80, 8 },
				{ 16, 10, 60, 10 },
				{ 17, 20, 80, 20 },
				{ 18, 17, 60, 30 },
				{ 30, 35, 75, 15 },

				{ 15, 15, 40, 10 },
				{ 20, 20, 70, 50 },
				{ 35, 40, 95, 100 },
				{ 37, 40, 80, 100 },
				{ 43, 80, 80, 100 },
				{ 44, 100, 80, 100 },
				{ 45, 50, 70, 100 },
				{ 99, 0, 0, 0 },
			},

			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},

			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},

			/* Rogue (Assassin): Death Magic */
			{
				{ 5, 3, 40, 1 },
				{ 7, 4, 40, 1 },
				{ 9, 5, 50, 1 },
				{ 13, 7, 60, 1 },
				{ 15, 7, 80, 1 },
				{ 17, 15, 80, 1 },
				{ 19, 17, 30, 1 },
				{ 19, 19, 30, 1 },

				{ 21, 21, 60, 3 },
				{ 23, 23, 75, 4 },
				{ 27, 25, 75, 4 },
				{ 30, 30, 75, 4 },
				{ 35, 35, 75, 5 },
				{ 45, 45, 60, 10 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },

				{ 20, 25, 80, 100 },
				{ 23, 20, 40, 20 },
				{ 28, 28, 75, 25 },
				{ 32, 32, 80, 50 },
				{ 46, 45, 75, 40 },
				{ 48, 100, 90, 100 },
				{ 50, 50, 75, 50 },
				{ 99, 0, 0, 0 },

				{ 30, 30, 80, 50 },
				{ 31, 80, 80, 250 },
				{ 32, 40, 90, 150 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 50, 125, 90, 250 },
			},

			/* Rogue (Card Shark): Trump Magic */
			{
				{ 5, 2, 25, 3 },
				{ 7, 5, 25, 4 },
				{ 9, 7, 37, 8 },
				{ 11, 9, 40, 8 },
				{ 13, 11, 20, 4 },
				{ 15, 13, 30, 6 },
				{ 19, 15, 30, 6 },
				{ 21, 20, 30, 5 },

				{ 25, 22, 40, 8 },
				{ 30, 26, 30, 8 },
				{ 33, 26, 30, 8 },
				{ 35, 32, 35, 9 },
				{ 40, 35, 40, 12 },
				{ 42, 38, 35, 10 },
				{ 46, 44, 40, 15 },
				{ 49, 50, 35, 12 },

				{ 20, 15, 40, 20 },
				{ 30, 30, 35, 25 },
				{ 33, 30, 35, 30 },
				{ 38, 33, 35, 35 },
				{ 42, 90, 40, 100 },
				{ 45, 150, 45, 250 },
				{ 48, 75, 25, 75 },
				{ 99, 0, 0, 0 },

				{ 35, 30, 30, 50 },
				{ 42, 65, 45, 100 },
				{ 44, 100, 40, 150 },
				{ 46, 100, 40, 150 },
				{ 99, 0, 0, 0 },
				{ 49, 125, 40, 150 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
			},

			/* Rogue (Thief): Arcane Magic */
			{
				{ 1, 1, 20, 4 },
				{ 1, 1, 33, 5 },
				{ 2, 1, 33, 4 },
				{ 2, 2, 33, 5 },
				{ 3, 3, 33, 5 },
				{ 5, 5, 40, 6 },
				{ 6, 6, 33, 7 },
				{ 7, 7, 44, 5 },

				{ 8, 8, 40, 7 },
				{ 9, 9, 60, 7 },
				{ 10, 10, 50, 6 },
				{ 11, 11, 50, 6 },
				{ 13, 11, 50, 6 },
				{ 14, 12, 50, 6 },
				{ 15, 13, 50, 5 },
				{ 16, 14, 50, 5 },

				{ 17, 15, 50, 5 },
				{ 18, 16, 50, 5 },
				{ 19, 17, 33, 6 },
				{ 20, 20, 50, 8 },
				{ 23, 22, 60, 9 },
				{ 25, 24, 60, 9 },
				{ 28, 25, 70, 12 },
				{ 30, 28, 60, 13 },

				{ 35, 30, 80, 40 },
				{ 39, 36, 80, 25 },
				{ 42, 37, 60, 25 },
				{ 44, 38, 70, 25 },
				{ 46, 40, 66, 30 },
				{ 47, 42, 80, 40 },
				{ 48, 60, 70, 50 },
				{ 50, 125, 80, 200 }
			}
		}
	},

	{
		/*** Ranger ***/

		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		3,
		400,

		{
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},

			/* Ranger: Sorcery */
			{
				{ 3, 2, 35, 2 },
				{ 3, 3, 35, 2 },
				{ 5, 4, 40, 2 },
				{ 7, 4, 40, 2 },
				{ 9, 6, 40, 2 },
				{ 11, 8, 45, 2 },
				{ 13, 8, 40, 3 },
				{ 17, 17, 90, 4 },

				{ 20, 19, 85, 4 },
				{ 23, 25, 90, 3 },
				{ 25, 25, 60, 3 },
				{ 27, 25, 85, 3 },
				{ 31, 27, 70, 3 },
				{ 34, 35, 70, 4 },
				{ 38, 37, 70, 8 },
				{ 42, 40, 90, 10 },

				{ 15, 7, 75, 20 },
				{ 15, 20, 70, 25 },
				{ 17, 17, 70, 25 },
				{ 18, 18, 80, 25 },
				{ 19, 25, 65, 20 },
				{ 23, 25, 60, 20 },
				{ 27, 27, 60, 15 },
				{ 35, 35, 75, 13 },

				{ 20, 20, 45, 5 },
				{ 27, 27, 70, 50 },
				{ 37, 60, 95, 120 },
				{ 40, 40, 95, 120 },
				{ 45, 80, 95, 200 },
				{ 45, 100, 95, 200 },
				{ 50, 50, 90, 175 },
				{ 99, 0, 0, 0 },
			},

			/* Ranger: Nature Magic */
			{
				{ 3, 1, 35, 2 },
				{ 4, 3, 40, 2 },
				{ 4, 4, 40, 3 },
				{ 5, 7, 55, 2 },
				{ 6, 7, 50, 3 },
				{ 7, 7, 50, 3 },
				{ 8, 7, 50, 3 },
				{ 9, 7, 45, 3 },

				{ 9, 7, 80, 4 },
				{ 10, 7, 40, 3 },
				{ 11, 9, 40, 4 },
				{ 12, 9, 55, 4 },
				{ 14, 9, 55, 4 },
				{ 18, 20, 65, 8 },
				{ 23, 23, 65, 10 },
				{ 40, 100, 95, 50 },

				{ 10, 10, 50, 25 },
				{ 12, 12, 50, 25 },
				{ 14, 15, 70, 60 },
				{ 20, 30, 85, 70 },
				{ 35, 30, 80, 75 },
				{ 38, 40, 90, 200 },
				{ 40, 55, 90, 250 },
				{ 42, 80, 90, 250 },

				{ 25, 28, 60, 150 },
				{ 26, 26, 60, 100 },
				{ 30, 35, 75, 125 },
				{ 32, 29, 75, 35 },
				{ 36, 33, 75, 45 },
				{ 40, 35, 75, 100 },
				{ 41, 80, 95, 250 },
				{ 42, 80, 75, 150 },
			},

			/* Ranger: Chaos Magic */
			{
				{ 3, 1, 20, 1 },
				{ 3, 3, 35, 2 },
				{ 5, 3, 35, 2 },
				{ 7, 5, 45, 2 },
				{ 14, 12, 40, 2 },
				{ 20, 16, 50, 6 },
				{ 25, 21, 60, 3 },
				{ 25, 22, 60, 3 },

				{ 27, 23, 60, 5 },
				{ 30, 25, 60, 8 },
				{ 33, 30, 70, 13 },
				{ 35, 31, 70, 10 },
				{ 37, 35, 75, 15 },
				{ 39, 29, 65, 5 },
				{ 43, 30, 95, 15 },
				{ 48, 50, 85, 30 },

				{ 22, 20, 60, 30 },
				{ 25, 25, 70, 20 },
				{ 28, 25, 80, 45 },
				{ 35, 32, 70, 35 },
				{ 38, 35, 85, 150 },
				{ 42, 75, 95, 250 },
				{ 48, 100, 90, 250 },
				{ 99, 0, 0, 0 },

				{ 33, 33, 66, 8 },
				{ 40, 45, 85, 35 },
				{ 42, 42, 75, 42 },
				{ 48, 48, 85, 100 },
				{ 50, 50, 90, 150 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
			},

			/* Ranger: Death Magic */
			{
				{ 5, 2, 40, 3 },
				{ 5, 3, 40, 3 },
				{ 7, 4, 50, 3 },
				{ 9, 5, 40, 3 },
				{ 11, 8, 40, 3 },
				{ 17, 25, 75, 4 },
				{ 19, 19, 50, 3 },
				{ 22, 22, 50, 3 },

				{ 24, 24, 55, 3 },
				{ 26, 26, 50, 3 },
				{ 28, 28, 75, 4 },
				{ 30, 30, 80, 5 },
				{ 40, 80, 95, 20 },
				{ 45, 40, 60, 9 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },

				{ 25, 30, 80, 125 },
				{ 25, 25, 80, 100 },
				{ 27, 27, 40, 40 },
				{ 39, 39, 76, 50 },
				{ 45, 45, 80, 100 },
				{ 46, 100, 90, 100 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },

				{ 35, 35, 75, 50 },
				{ 38, 90, 80, 100 },
				{ 40, 45, 95, 200 },
				{ 48, 50, 30, 75 },
				{ 50, 50, 90, 75 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 }
			},

			/* Ranger: Trump Magic */
			{
				{ 3, 1, 25, 3 },
				{ 6, 6, 25, 4 },
				{ 9, 7, 37, 8 },
				{ 10, 8, 40, 8 },
				{ 13, 10, 20, 4 },
				{ 17, 15, 30, 6 },
				{ 20, 17, 30, 6 },
				{ 22, 20, 30, 5 },

				{ 24, 22, 40, 8 },
				{ 28, 25, 30, 8 },
				{ 33, 26, 30, 8 },
				{ 36, 32, 35, 9 },
				{ 38, 33, 40, 12 },
				{ 42, 38, 35, 10 },
				{ 45, 42, 40, 15 },
				{ 99, 0, 0, 0 },

				{ 20, 20, 40, 20 },
				{ 28, 26, 35, 25 },
				{ 31, 30, 35, 30 },
				{ 36, 33, 35, 35 },
				{ 41, 80, 40, 100 },
				{ 44, 120, 45, 250 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },

				{ 35, 33, 30, 50 },
				{ 40, 65, 45, 100 },
				{ 99, 0, 0, 0 },
				{ 47, 95, 40, 150 },
				{ 50, 120, 40, 200 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
				{ 99, 0, 0, 0 },
			},

			/* Ranger: Arcane Magic */
			{
				{ 3, 2, 20, 4 },
				{ 3, 2, 33, 5 },
				{ 4, 3, 33, 4 },
				{ 4, 3, 33, 5 },
				{ 5, 4, 33, 5 },
				{ 6, 6, 40, 6 },
				{ 7, 7, 33, 7 },
				{ 8, 8, 44, 5 },

				{ 9, 9, 40, 7 },
				{ 10, 10, 60, 7 },
				{ 11, 11, 50, 6 },
				{ 12, 12, 50, 6 },
				{ 14, 13, 50, 6 },
				{ 15, 14, 50, 6 },
				{ 16, 15, 50, 5 },
				{ 17, 16, 50, 5 },

				{ 18, 17, 50, 5 },
				{ 19, 18, 50, 5 },
				{ 20, 19, 33, 6 },
				{ 22, 20, 50, 8 },
				{ 25, 23, 60, 9 },
				{ 27, 26, 60, 9 },
				{ 29, 27, 70, 12 },
				{ 33, 30, 60, 13 },

				{ 38, 36, 80, 40 },
				{ 42, 38, 80, 25 },
				{ 44, 38, 60, 25 },
				{ 46, 40, 70, 25 },
				{ 47, 42, 66, 30 },
				{ 48, 44, 80, 40 },
				{ 49, 65, 70, 50 },
				{ 99, 0, 0, 0 }
		    }
		}
	},

	{
		/*** Paladin ***/

		TV_LIFE_BOOK,
		0,

		A_WIS,
		1,

		1,
		400,
		{
			/* Paladin: Life Magic */
			{
				{ 1, 1, 30, 4 },
				{ 2, 2, 35, 4 },
				{ 3, 3, 35, 4 },
				{ 4, 3, 35, 4 },
				{ 5, 4, 35, 4 },
				{ 8, 5, 40, 4 },
				{ 11, 9, 40, 3 },
				{ 13, 10, 45, 3 },

				{ 14, 11, 45, 4},
				{ 15, 15, 50, 4},
				{ 17, 15, 50, 4},
				{ 18, 15, 50, 4},
				{ 18, 15, 50, 4},
				{ 19, 15, 50, 4},
				{ 30, 25, 55, 5},
				{ 35, 70, 75, 5},

				{ 25, 22, 50, 75 },
				{ 28, 24, 70, 150 },
				{ 30, 25, 60, 75 },
				{ 33, 30, 60, 75 },
				{ 35, 32, 70, 75 },
				{ 35, 55, 80, 115 },
				{ 39, 38, 80, 125 },
				{ 46, 60, 80, 150 },

				{ 9, 9, 50, 40 },
				{ 25, 20, 50, 50 },
				{ 35, 65, 80, 115 },
				{ 40, 80, 80, 225 },
				{ 45, 80, 80, 115 },
				{ 45, 45, 80, 100 },
				{ 48, 100, 90, 250 },
				{ 50, 100, 80, 250 }
			},

			{

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			/* Paladin: Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 3, 2, 25, 4 },
				{ 4, 3, 25, 4 },
				{ 6, 5, 27, 4 },
				{ 8, 8, 30, 4 },
				{ 10, 11, 75, 6 },
				{ 12, 12, 30, 4 },
				{ 15, 15, 40, 5 },

				{ 17, 17, 40, 5 },
				{ 19, 19, 30, 4 },
				{ 23, 23, 50, 10 },
				{ 28, 26, 60, 16 },
				{ 35, 75, 90, 30 },
				{ 40, 35, 60, 16 },
				{ 45, 35, 95, 100 },
				{ 50, 52, 95, 150 },

				{ 15, 20, 80, 180 },
				{ 15, 20, 80, 30 },
				{ 18, 20, 30, 15 },
				{ 38, 38, 70, 50 },
				{ 40, 40, 60, 125 },
				{ 42, 100, 70, 100 },
				{ 48, 50, 80, 200 },
				{ 48, 75, 80, 100 },

				{ 30, 35, 75, 50 },
				{ 36, 85, 90, 200 },
				{ 38, 45, 95, 250 },
				{ 45, 45, 70, 40 },
				{ 47, 45, 80, 70 },
				{ 50, 150, 95, 250 },
				{ 50, 100, 95, 250 },
				{ 50, 111, 95, 250 }
			},

			/* Paladin: No Trump magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},
			/* Paladin: No Arcane Magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			}
		}
	},

	{
		/*** Warrior-Mage ***/

		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		1,
		350,

		{
			{
				{ 2, 2, 30, 4 },
				{ 4, 4, 35, 4 },
				{ 5, 5, 35, 4 },
				{ 6, 6, 35, 4 },
				{ 8, 8, 35, 4 },
				{ 9, 9, 40, 4 },
				{ 14, 14, 40, 3 },
				{ 16, 16, 45, 3 },

				{ 18, 18, 45, 4},
				{ 20, 20, 50, 4},
				{ 22, 22, 50, 4},
				{ 24, 24, 50, 4},
				{ 26, 26, 50, 4},
				{ 28, 28, 50, 4},
				{ 33, 33, 55, 5},
				{ 40, 70, 75, 5},

				{ 28, 28, 50, 75 },
				{ 30, 30, 70, 150 },
				{ 34, 34, 60, 75 },
				{ 36, 36, 60, 75 },
				{ 38, 38, 70, 75 },
				{ 42, 55, 80, 115 },
				{ 45, 45, 80, 125 },
				{ 50, 70, 80, 150 },

				{ 10, 10, 50, 40 },
				{ 28, 28, 50, 50 },
				{ 38, 85, 80, 115 },
				{ 45, 90, 80, 225 },
				{ 46, 90, 80, 115 },
				{ 48, 50, 80, 100 },
				{ 49, 100, 90, 100 },
				{ 50, 100, 80, 250 }
			},

			/* Warrior-Mage: Sorcery */
			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
				{ 3, 3, 25, 1 },
				{ 4, 4, 30, 1 },
				{ 5, 5, 30, 1 },
				{ 6, 6, 35, 5 },
				{ 7, 7, 30, 4 },
				{ 8, 8, 75, 9 },

				{ 10, 9, 75, 8 },
				{ 11, 10, 75, 8 },
				{ 12, 11, 75, 7 },
				{ 13, 12, 50, 6 },
				{ 20, 15, 60, 8 },
				{ 27, 18, 60, 8 },
				{ 33, 25, 70, 15 },
				{ 40, 40, 75, 20 },

				{ 4, 4, 25, 15 },
				{ 12, 12, 70, 40 },
				{ 14, 12, 80, 40 },
				{ 15, 12, 70, 30 },
				{ 16, 14, 60, 25 },
				{ 19, 19, 85, 50 },
				{ 24, 22, 60, 25 },
				{ 28, 28, 75, 19 },

				{ 12, 12, 40, 20 },
				{ 19, 19, 75, 70 },
				{ 30, 35, 95, 160 },
				{ 35, 45, 80, 120 },
				{ 42, 85, 95, 200 },
				{ 45, 100, 95, 200 },
				{ 46, 55, 90, 175 },
				{ 48, 75, 75, 250 },
			},

			/* Warrior-Mage: Nature Magic */
			{
				{ 2, 2, 23, 4 },
				{ 3, 3, 25, 3 },
				{ 4, 4, 25, 1 },
				{ 5, 5, 35, 4 },
				{ 6, 6, 50, 5 },
				{ 7, 7, 50, 5 },
				{ 8, 8, 50, 5 },
				{ 9, 9, 35, 4 },

				{ 10, 10, 40, 6 },
				{ 11, 11, 30, 6 },
				{ 12, 12, 45, 6 },
				{ 13, 13, 40, 6 },
				{ 14, 14, 30, 5 },
				{ 19, 15, 65, 7 },
				{ 31, 31, 65, 10 },
				{ 45, 100, 95, 50 },

				{ 9, 9, 20, 28 },
				{ 12, 12, 40, 44 },
				{ 15, 15, 75, 120 },
				{ 20, 22, 85, 60 },
				{ 38, 38, 85, 80 },
				{ 40, 42, 90, 200 },
				{ 45, 48, 75, 200},
				{ 49, 95, 90, 250 },

				{ 25, 25, 60, 25 },
				{ 27, 27, 60, 25 },
				{ 28, 28, 75, 29 },
				{ 33, 33, 75, 35 },
				{ 38, 38, 85, 65 },
				{ 41, 41, 90, 100 },
				{ 45, 95, 95, 250 },
				{ 50, 85, 65, 150 },
			},

			/* Warrior-Mage: Chaos Magic */
			{
				{ 2, 2, 20, 4 },
				{ 3, 3, 22, 4 },
				{ 4, 4, 25, 4 },
				{ 5, 5, 30, 6 },
				{ 8, 8, 30, 1 },
				{ 11, 11, 45, 5 },
				{ 17, 15, 45, 6 },
				{ 18, 17, 35, 5 },

				{ 21, 21, 45, 7 },
				{ 23, 22, 45, 9 },
				{ 27, 25, 50, 20 },
				{ 29, 30, 50, 11 },
				{ 33, 33, 50, 12 },
				{ 37, 35, 60, 8 },
				{ 41, 40, 80, 15 },
				{ 48, 50, 85, 40 },

				{ 12, 12, 45, 9 },
				{ 17, 16, 60, 20 },
				{ 20, 18, 80, 35 },
				{ 27, 25, 60, 35 },
				{ 35, 30, 85, 150 },
				{ 45, 55, 85, 250 },
				{ 49, 95, 80, 250 },
				{ 50, 111, 80, 250 },

				{ 24, 20, 66, 8 },
				{ 40, 35, 85, 35 },
				{ 42, 40, 75, 40 },
				{ 46, 44, 85, 100 },
				{ 48, 48, 80, 150 },
				{ 49, 50, 85, 200 },
				{ 50, 100, 80, 250 },
				{ 50, 100, 85, 250 },
			},

			/* Warrior-Mage: Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 3, 3, 25, 4 },
				{ 4, 4, 27, 3 },
				{ 7, 7, 30, 4 },
				{ 9, 10, 75, 6 },
				{ 10, 10, 30, 4 },
				{ 12, 12, 30, 4 },

				{ 14, 14, 40, 5 },
				{ 16, 16, 30, 4 },
				{ 21, 21, 50, 10 },
				{ 25, 25, 60, 16 },
				{ 35, 75, 50, 30 },
				{ 40, 40, 60, 16 },
				{ 44, 45, 95, 25 },
				{ 48, 55, 95, 150 },

				{ 10, 22, 80, 180 },
				{ 12, 18, 80, 30 },
				{ 14, 18, 30, 15 },
				{ 30, 30, 75, 50 },
				{ 40, 40, 60, 125 },
				{ 42, 90, 70, 90 },
				{ 45, 50, 70, 200 },
				{ 48, 85, 80, 100 },

				{ 24, 24, 75, 50 },
				{ 33, 80, 75, 150 },
				{ 35, 45, 95, 250 },
				{ 42, 50, 70, 40 },
				{ 45, 55, 80, 70 },
				{ 50, 135, 95, 250 },
				{ 50, 100, 95, 250 },
				{ 50, 123, 95, 250 },
			},

			/* Warrior-Mage: Trump Magic */
			{
				{ 1, 1, 25, 3 },
				{ 5, 5, 25, 4 },
				{ 7, 7, 37, 8 },
				{ 8, 7, 40, 8 },
				{ 10, 10, 20, 4 },
				{ 12, 12, 30, 6 },
				{ 18, 15, 30, 6 },
				{ 20, 18, 30, 5 },

				{ 24, 23, 40, 8 },
				{ 28, 25, 30, 8 },
				{ 31, 26, 30, 8 },
				{ 33, 30, 35, 9 },
				{ 38, 32, 40, 12 },
				{ 40, 38, 35, 10 },
				{ 44, 42, 40, 15 },
				{ 48, 46, 35, 12 },

				{ 19, 18, 40, 20 },
				{ 29, 27, 35, 25 },
				{ 31, 30, 35, 30 },
				{ 35, 33, 35, 35 },
				{ 40, 80, 40, 100 },
				{ 42, 120, 45, 250 },
				{ 46, 55, 25, 75 },
				{ 50, 135, 45, 200 },

				{ 33, 30, 30, 50 },
				{ 40, 60, 45, 100 },
				{ 42, 95, 40, 150 },
				{ 45, 95, 40, 150 },
				{ 46, 120, 40, 200 },
				{ 48, 125, 40, 150 },
				{ 49, 130, 40, 200 },
				{ 50, 135, 40, 220 }
			},

			/* Warrior-Mage: Arcane Magic */
			{
				{ 1, 1, 20, 4 },
				{ 2, 1, 33, 5 },
				{ 2, 2, 33, 4 },
				{ 3, 3, 33, 5 },
				{ 4, 4, 33, 5 },
				{ 5, 5, 40, 6 },
				{ 6, 6, 33, 7 },
				{ 7, 7, 44, 5 },

				{ 8, 8, 40, 7 },
				{ 9, 9, 60, 7 },
				{ 11, 10, 50, 6 },
				{ 12, 11, 50, 6 },
				{ 13, 12, 50, 6 },
				{ 14, 13, 50, 6 },
				{ 15, 14, 50, 5 },
				{ 16, 15, 50, 5 },

				{ 17, 16, 50, 5 },
				{ 18, 17, 50, 5 },
				{ 19, 18, 33, 6 },
				{ 20, 20, 50, 8 },
				{ 23, 22, 60, 9 },
				{ 25, 23, 60, 9 },
				{ 29, 25, 70, 12 },
				{ 30, 27, 60, 13 },

				{ 35, 30, 80, 50 },
				{ 39, 38, 80, 25 },
				{ 41, 40, 60, 25 },
				{ 43, 42, 70, 25 },
				{ 45, 44, 66, 30 },
				{ 47, 45, 80, 40 },
				{ 48, 65, 70, 50 },
				{ 50, 140, 80, 200 }
		    }
		}
	},

	{
		/*** Chaos Warrior ***/
		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		2,
		400,

		{
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			/* Chaos Warrior: Chaos Magic */
			{
				{ 2, 1, 20, 4 },
				{ 3, 2, 22, 4 },
				{ 4, 3, 25, 4 },
				{ 5, 4, 30, 6 },
				{ 7, 7, 30, 1 },
				{ 8, 7, 45, 5 },
				{ 15, 9, 45, 6 },
				{ 16, 10, 35, 5 },

				{ 19, 12, 45, 7 },
				{ 22, 14, 45, 9 },
				{ 25, 17, 50, 20 },
				{ 28, 18, 50, 11 },
				{ 30, 20, 50, 12 },
				{ 33, 24, 60, 8 },
				{ 36, 26, 80, 15 },
				{ 40, 45, 85, 40 },

				{ 11, 11, 45, 9 },
				{ 14, 14, 60, 20 },
				{ 16, 15, 80, 35 },
				{ 23, 23, 60, 35 },
				{ 30, 30, 85, 150 },
				{ 42, 50, 85, 250 },
				{ 45, 90, 80, 250 },
				{ 47, 100, 80, 250 },

				{ 23, 23, 66, 10},
				{ 35, 35, 85, 35 },
				{ 37, 37, 75, 40 },
				{ 41, 42, 85, 100 },
				{ 43, 44, 80, 150 },
				{ 45, 48, 85, 200 },
				{ 48, 100, 80, 220 },
				{ 49, 100, 85, 250 },
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			/* Chaos Warrior: No Trump magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},
			/* Chaos Warrior: No Arcane Magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			}
		}
	},
	{
		/*** Monk ***/

		TV_LIFE_BOOK,
		0,

		A_WIS,
		0,

		1,
		300,

		{
			{
				{ 1, 1, 30, 4 },
				{ 2, 2, 35, 4 },
				{ 3, 3, 35, 4 },
				{ 4, 4, 35, 4 },
				{ 5, 5, 35, 4 },
				{ 8, 6, 40, 4 },
				{ 11, 10, 40, 3 },
				{ 13, 12, 45, 3 },

				{ 15, 12, 45, 4},
				{ 16, 15, 50, 4},
				{ 17, 15, 50, 4},
				{ 18, 16, 50, 4},
				{ 19, 16, 50, 4},
				{ 20, 18, 50, 4},
				{ 30, 25, 55, 5},
				{ 35, 70, 75, 5},

				{ 26, 26, 50, 75 },
				{ 28, 28, 70, 150 },
				{ 32, 32, 60, 75 },
				{ 36, 35, 60, 75 },
				{ 38, 35, 70, 75 },
				{ 40, 60, 80, 115 },
				{ 45, 45, 80, 125 },
				{ 48, 64, 80, 150 },

				{ 10, 10, 50, 40 },
				{ 25, 25, 50, 50 },
				{ 40, 65, 80, 115 },
				{ 44, 84, 80, 225 },
				{ 46, 64, 80, 115 },
				{ 48, 40, 80, 100 },
				{ 49, 100, 90, 200 },
				{ 50, 100, 80, 250 }
			},

			/* Monk: Sorcery */
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			/* Monk: Nature Magic */
			{
				{ 1, 1, 35, 2 },
				{ 3, 3, 40, 2 },
				{ 4, 4, 40, 3 },
				{ 7, 7, 55, 2 },
				{ 7, 7, 50, 3 },
				{ 7, 7, 50, 3 },
				{ 8, 8, 50, 3 },
				{ 9, 9, 45, 3 },

				{ 10, 8, 80, 4 },
				{ 11, 9, 40, 3 },
				{ 12, 10, 40, 4 },
				{ 14, 12, 55, 4 },
				{ 16, 12, 55, 4 },
				{ 18, 22, 65, 8 },
				{ 31, 31, 75, 10 },
				{ 40, 100, 95, 50 },

				{ 12, 12, 50, 25 },
				{ 14, 14, 50, 25 },
				{ 16, 16, 70, 60 },
				{ 22, 30, 85, 70 },
				{ 35, 35, 80, 80 },
				{ 40, 40, 90, 200 },
				{ 45, 55, 90, 250 },
				{ 50, 80, 90, 250 },

				{ 28, 28, 60, 150 },
				{ 30, 30, 60, 160 },
				{ 35, 50, 75, 125 },
				{ 33, 33, 75, 35 },
				{ 38, 38, 75, 45 },
				{ 42, 40, 75, 100 },
				{ 45, 85, 95, 250 },
				{ 48, 85, 75, 150 }
			},

			/* Monk: Chaos Magic */
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			/* Monk: Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 3, 3, 25, 4 },
				{ 5, 5, 27, 4 },
				{ 7, 7, 30, 4 },
				{ 11, 11, 75, 6 },
				{ 12, 12, 30, 4 },
				{ 14, 14, 40, 5 },

				{ 16, 16, 40, 5 },
				{ 19, 19, 30, 4 },
				{ 22, 22, 50, 10 },
				{ 25, 25, 60, 16 },
				{ 33, 80, 90, 30 },
				{ 40, 40, 60, 16 },
				{ 45, 45, 95, 100 },
				{ 50, 60, 95, 150 },

				{ 15, 20, 80, 180 },
				{ 16, 16, 80, 30 },
				{ 18, 18, 30, 15 },
				{ 35, 35, 75, 50 },
				{ 40, 40, 60, 125 },
				{ 42, 95, 70, 90 },
				{ 48, 50, 80, 200 },
				{ 49, 80, 80, 125 },

				{ 30, 30, 75, 50 },
				{ 37, 85, 85, 220 },
				{ 38, 50, 95, 250 },
				{ 44, 44, 70, 40 },
				{ 46, 50, 80, 70 },
				{ 50, 140, 95, 250 },
				{ 50, 100, 95, 250},
				{ 50, 115, 95, 250 }
			},

			/* Monk: No Trump magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			},

			/* Monk: No Arcane Magic */
			{
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},

				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
				{ 99, 0, 0, 0},
			}
		}
	},

	{
		/*** Mindcrafter ***/

		TV_LIFE_BOOK,
		0,

		A_WIS,
		0,

		99,
		300,
		{
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},

			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
			{
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},

				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0},
				{ 99,  0,  0,   0}
			},
		},
	},

	{
		/*** High Mage ***/

		TV_SORCERY_BOOK,
		0,

		A_INT,
		0,

		1,
		300,

		{
			/* High Mage: Life Magic */
			{
				{  1,  1, 20,   4 },
				{  2,  2, 25,   4 },
				{  3,  3, 25,   4 },
				{  4,  4, 25,   4 },
				{  5,  5, 25,   4 },
				{  6,  5, 30,   4 },
				{  9,  9, 30,   3 },
				{ 12, 10, 35,   3 },

				{ 14, 12, 35,   4 },
				{ 15, 14, 40,   4 },
				{ 15, 15, 40,   4 },
				{ 17, 15, 40,   4 },
				{ 19, 17, 40,   4 },
				{ 21, 19, 40,   4 },
				{ 25, 25, 45,   5 },
				{ 30, 50, 55,   5 },

				{ 20, 20, 40,  75 },
				{ 24, 24, 60, 150 },
				{ 30, 30, 50,  75 },
				{ 31, 30, 50,  75 },
				{ 32, 30, 60,  75 },
				{ 33, 40, 60, 115 },
				{ 35, 35, 60, 125 },
				{ 40, 70, 70, 150 },

				{  5,  5, 40,  40 },
				{ 20, 20, 40,  50 },
				{ 30, 70, 60, 115 },
				{ 40, 80, 60, 225 },
				{ 42, 75, 60, 115 },
				{ 45, 40, 60, 100 },
				{ 47, 90, 70, 250 },
				{ 49, 90, 70, 250 }
			},

			/* High Mage: Sorcery */
			{
				{  1,  1, 15,   4 },
				{  1,  1, 15,   4 },
				{  2,  2, 15,   1 },
				{  2,  2, 20,   1 },
				{  3,  3, 20,   1 },
				{  4,  3, 25,   5 },
				{  5,  4, 20,   4 },
				{  5,  5, 65,   9 },

				{  7,  5, 65,   8 },
				{  7,  5, 65,   8 },
				{  9,  5, 65,   7 },
				{  9,  5, 40,   6 },
				{ 13,  8, 50,   8 },
				{ 17, 10, 50,   8 },
				{ 24, 15, 60,  15 },
				{ 28, 20, 65,  20 },

				{  2,  2, 20,  15 },
				{  7,  7, 60,  40 },
				{  8,  8, 70,  40 },
				{  9,  9, 70,  40 },
				{ 12,  9, 50,  25 },
				{ 15, 12, 65,  50 },
				{ 17, 12, 50,  25 },
				{ 20, 20, 65,  19 },

				{  8,  8, 30,  20 },
				{ 20, 20, 65,  70 },
				{ 20, 25, 85, 160 },
				{ 25, 30, 70, 120 },
				{ 30, 65, 85, 200 },
				{ 35, 80, 85, 200 },
				{ 40, 40, 80, 175 },
				{ 42, 65, 65, 250 },
			},

			/* High Mage: Nature Magic */
			{
				{ 1, 1, 15, 4 },
				{ 2, 1, 15, 3 },
				{ 2, 2, 15, 1 },
				{ 3, 2, 25, 4 },
				{ 3, 3, 40, 5 },
				{ 4, 3, 40, 5 },
				{ 4, 4, 40, 5 },
				{ 5, 4, 25, 4 },

				{ 5, 4, 30, 6 },
				{ 5, 4, 20, 6 },
				{ 5, 5, 35, 6 },
				{ 5, 5, 30, 6 },
				{ 7, 5, 30, 5 },
				{ 14, 10, 35, 8 },
				{ 20, 20, 80, 50 },
				{ 35, 80, 85, 50 },

				{ 5, 5, 10, 28 },
				{ 7, 7, 20, 44 },
				{ 8, 8, 65, 120 },
				{ 12, 15, 75, 60 },
				{ 25, 25, 80, 100 },
				{ 33, 35, 80, 200 },
				{ 35, 40, 65, 200},
				{ 37, 65, 80, 250 },

				{ 15, 15, 50, 25 },
				{ 20, 20, 70, 50 },
				{ 22, 22, 65, 29 },
				{ 28, 25, 65, 35 },
				{ 32, 28, 75, 65 },
				{ 34, 30, 80, 100 },
				{ 36, 80, 85, 250 },
				{ 39, 65, 55, 150 }
			},

			/* High Mage: Chaos Magic */
			{
				{ 1, 1, 15, 4 },
				{ 1, 1, 15, 4 },
				{ 2, 1, 15, 4 },
				{ 4, 2, 20, 1 },
				{ 6, 4, 40, 1 },
				{ 10, 5, 35, 6 },
				{ 12, 6, 35, 6 },
				{ 14, 7, 25, 5 },

				{ 15, 9, 20, 5 },
				{ 17, 10, 35, 9 },
				{ 19, 11, 35, 10 },
				{ 21, 12, 40, 11 },
				{ 22, 13, 40, 12 },
				{ 23, 15, 50, 8 },
				{ 27, 17, 70, 15 },
				{ 30, 35, 75, 40 },

				{ 9, 5, 35, 9 },
				{ 12, 12, 70, 35 },
				{ 14, 12, 70, 35 },
				{ 21, 21, 75, 100 },
				{ 26, 22, 75, 150 },
				{ 39, 40, 75, 250 },
				{ 42, 80, 70, 250 },
				{ 44, 90, 80, 250 },

				{ 16, 16, 55, 8 },
				{ 32, 30, 75, 35 },
				{ 34, 32, 65, 40 },
				{ 36, 36, 75, 100 },
				{ 38, 38, 70, 150 },
				{ 40, 45, 75, 200 },
				{ 43, 55, 70, 200 },
				{ 46, 90, 75, 250 }
			},

			/* High Mage: Death Magic */
			{
				{ 1, 1, 20, 4 },
				{ 1, 1, 20, 4 },
				{ 2, 1, 20, 4 },
				{ 2, 2, 20, 3 },
				{ 3, 3, 20, 4 },
				{ 5, 9, 55, 6 },
				{ 7, 7, 20, 4 },
				{ 8, 8, 20, 4 },

				{ 10, 10, 30, 5 },
				{ 11, 10, 20, 4 },
				{ 14, 12, 40, 10 },
				{ 20, 16, 50, 16 },
				{ 26, 65, 40, 30 },
				{ 30, 30, 50, 16 },
				{ 33, 30, 85, 25 },
				{ 40, 40, 85, 150 },

				{ 8, 15, 70, 180 },
				{ 8, 10, 70, 30 },
				{ 9, 9, 20, 15 },
				{ 25, 20, 65, 50 },
				{ 30, 30, 50, 125 },
				{ 30, 80, 60, 90 },
				{ 36, 35, 60, 200 },
				{ 38, 66, 70, 100 },

				{ 16, 16, 65, 50 },
				{ 22, 60, 85, 250 },
				{ 26, 35, 85, 250 },
				{ 29, 30, 60, 40 },
				{ 33, 30, 70, 70 },
				{ 39, 100, 85, 250 },
				{ 41, 85, 80, 250 },
				{ 44, 75, 80, 250 }
			},

			/* High Mage: Trump Magic */
			{
				{ 1, 1, 20, 3 },
				{ 2, 2, 20, 4 },
				{ 4, 4, 32, 8 },
				{ 5, 5, 35, 8 },
				{ 6, 5, 15, 4 },
				{ 7, 7, 25, 6 },
				{ 10, 10, 25, 6 },
				{ 14, 12, 25, 5 },

				{ 16, 16, 35, 8 },
				{ 20, 20, 25, 8 },
				{ 25, 22, 25, 8 },
				{ 28, 24, 30, 9 },
				{ 28, 26, 35, 12 },
				{ 30, 28, 30, 10 },
				{ 35, 30, 35, 15 },
				{ 39, 36, 30, 12 },

				{ 11, 11, 35, 20 },
				{ 21, 21, 30, 25 },
				{ 23, 23, 30, 30 },
				{ 25, 25, 30, 35 },
				{ 31, 65, 35, 100 },
				{ 36, 90, 40, 250 },
				{ 39, 45, 20, 75 },
				{ 42, 90, 40, 200 },

				{ 25, 25, 25, 50 },
				{ 32, 45, 40, 100 },
				{ 34, 75, 35, 150 },
				{ 36, 75, 35, 150 },
				{ 38, 90, 35, 200 },
				{ 42, 90, 35, 150 },
				{ 44, 90, 35, 200 },
				{ 46, 90, 35, 220 }
			},

			/* High Mage: Arcane Magic */
			{
				{ 1, 1, 15, 4 },
				{ 1, 1, 23, 5 },
				{ 1, 1, 23, 4 },
				{ 1, 1, 23, 5 },
				{ 2, 1, 23, 5 },
				{ 3, 2, 33, 6 },
				{ 4, 4, 23, 7 },
				{ 5, 4, 33, 5 },

				{ 6, 5, 30, 7 },
				{ 7, 7, 50, 7 },
				{ 8, 7, 40, 6 },
				{ 8, 8, 40, 6 },
				{ 9, 8, 40, 6 },
				{ 10, 9, 40, 6 },
				{ 10, 10, 40, 5 },
				{ 11, 10, 40, 5 },

				{ 12, 10, 40, 5 },
				{ 13, 10, 40, 5 },
				{ 14, 11, 22, 6 },
				{ 15, 12, 40, 8 },
				{ 17, 15, 50, 9 },
				{ 20, 15, 50, 9 },
				{ 20, 16, 60, 12 },
				{ 22, 18, 50, 13 },

				{ 24, 22, 60, 30 },
				{ 30, 30, 70, 25 },
				{ 33, 28, 50, 25 },
				{ 38, 28, 60, 25 },
				{ 40, 28, 55, 30 },
				{ 41, 28, 70, 40 },
				{ 43, 40, 60, 50 },
				{ 46, 80, 70, 200 }
			}
		}
	},
};


/*
 * Zangband uses this array instead of the spell flags table, as there
 * are 5 realms of magic, each with 4 spellbooks and 8 spells per book -- TY
 */
u32b fake_spell_flags[4]=
{
	0x000000ff,
	0x0000ff00,
	0x00ff0000,
	0xff000000
};


byte realm_choices1[] =
{
	(CH_NONE),					            /* Warrior */
	(CH_LIFE | CH_SORCERY | CH_NATURE |
	 CH_CHAOS | CH_DEATH | CH_TRUMP |
	 CH_ARCANE),                            /* Mage */
	(CH_LIFE | CH_DEATH),                   /* Priest */
	(CH_SORCERY | CH_DEATH | CH_TRUMP |
	 CH_ARCANE),                            /* Rogue */
	(CH_NATURE),                            /* Ranger */
	(CH_LIFE | CH_DEATH),                   /* Paladin */
	(CH_ARCANE),                            /* Warrior-Mage */
	(CH_CHAOS),                             /* Chaos-Warrior */
	(CH_LIFE | CH_NATURE | CH_DEATH),       /* Monk */
	(CH_NONE),                              /* Mindcrafter */
	(CH_LIFE | CH_SORCERY | CH_NATURE |
	 CH_CHAOS | CH_DEATH | CH_TRUMP |
	 CH_ARCANE),                            /* High-Mage */
};


byte realm_choices2[] =
{
	(CH_NONE),                              /* Warrior */
	(CH_LIFE | CH_SORCERY | CH_NATURE |
	 CH_CHAOS | CH_DEATH | CH_TRUMP |
	 CH_ARCANE),                            /* Mage */
	(CH_SORCERY | CH_NATURE | CH_CHAOS |
	 CH_TRUMP | CH_ARCANE),                 /* Priest */
	(CH_NONE),                              /* Rogue */
	(CH_SORCERY | CH_CHAOS | CH_DEATH |
	 CH_TRUMP | CH_ARCANE),                 /* Ranger */
	(CH_NONE),                              /* Paladin */
	(CH_LIFE | CH_NATURE | CH_CHAOS |
	 CH_DEATH | CH_TRUMP | CH_ARCANE |
	 CH_SORCERY),                           /* Warrior-Mage */
	(CH_NONE),                              /* Chaos-Warrior */
	(CH_NONE),                              /* Monk */
	(CH_NONE),                              /* Mindcrafter */
	(CH_NONE),                              /* High-Mage */
};


cptr realm_names[] =
{
	"no magic",
	"Life",
	"Sorcery",
	"Nature",
	"Chaos",
	"Death",
	"Trump",
	"Arcane",
	"unknown"
};


/*
 * Names of the spells (mage spells then priest spells)
 */
cptr spell_names[7][32] =
{
	/*** Life Spells ***/
	{
		/* Common Life Spellbooks */
		"Detect Evil",
		"Cure Light Wounds",
		"Bless",
		"Remove Fear",
		"Call Light",
		"Detect Traps and Secret Doors",
		"Cure Medium Wounds",
		"Satisfy Hunger",

		"Remove Curse",
		"Cure Poison",
		"Cure Critical Wounds",
		"Sense Unseen",
		"Holy Orb",
		"Protection from Evil",
		"Healing",
		"Glyph of Warding",

		/* Rare Life Spellbooks */
		"Exorcism",
		"Dispel Curse",
		"Dispel Undead & Demons",
		"Day of the Dove",
		"Dispel Evil",
		"Banish",
		"Holy Word",
		"Warding True",

		"Heroism",
		"Prayer",
		"Bless Weapon",
		"Restoration",
		"Healing True",
		"Holy Vision",
		"Divine Intervention",
		"Holy Invulnerability"
	},

	/*** Sorcery Spells ***/

	{
		/* Common Sorcery Spellbooks */
		"Detect Monsters",
		"Phase Door",
		"Detect Doors and Traps",
		"Light Area",
		"Confuse Monster",
		"Teleport",
		"Sleep Monster",
		"Recharging",

		"Magic Mapping",
		"Identify",
		"Slow Monster",
		"Mass Sleep",
		"Teleport Away",
		"Haste Self",
		"Detection True",
		"Identify True",

		/* Rare Sorcery Spellbooks */
		"Detect Objects and Treasure",
		"Detect Enchantment",
		"Charm Monster",
		"Dimension Door",
		"Sense Minds",
		"Self Knowledge",
		"Teleport Level",
		"Word of Recall",

		"Stasis",
		"Telekinesis",
		"Explosive Rune",
		"Clairvoyance",
		"Enchant Weapon",
		"Enchant Armour",
		"Alchemy",
		"Globe of Invulnerability"
	},

	/*** Nature Spellbooks ***/

	{
		/* Common Nature Spellbooks */
		"Detect Creatures",
		"First Aid",
		"Detect Doors and Traps",
		"Foraging",
		"Daylight",
		"Animal Taming",
		"Resist Environment",
		"Cure Wounds & Poison",

		"Stone to Mud",
		"Lightning Bolt",
		"Nature Awareness",
		"Frost Bolt",
		"Ray of Sunlight",
		"Entangle",
		"Summon Animal",
		"Herbal Healing",

		/* Rare Nature Spellbooks */
		"Door Building",
		"Stair Building",
		"Stone Skin",
		"Resistance True",
		"Animal Friendship",
		"Stone Tell",
		"Wall of Stone",
		"Protect from Corrosion",

		"Earthquake",
		"Whirlwind Attack",
		"Blizzard",
		"Lightning Storm",
		"Whirlpool",
		"Call Sunlight",
		"Elemental Branding",
		"Nature's Wrath"
	},

	/*** Chaos Spells ***/

	{
		/* Common Chaos Spellbooks */
		"Magic Missile",
		"Trap / Door Destruction",
		"Flash of Light",
		"Touch of Confusion",
		"Mana Burst",
		"Fire Bolt",
		"Fist of Force",
		"Teleport Self",

		"Wonder",
		"Chaos Bolt",
		"Sonic Boom",
		"Doom Bolt",
		"Fire Ball",
		"Teleport Other",
		"Word of Destruction",
		"Invoke Logrus",

		/* Rare Chaos Spellbooks */
		"Polymorph Other",
		"Chain Lightning",
		"Arcane Binding",
		"Disintegrate",
		"Alter Reality",
		"Polymorph Self",
		"Chaos Branding",
		"Summon Demon",

		"Beam of Gravity",
		"Meteor Swarm",
		"Flame Strike",
		"Call Chaos",
		"Magic Rocket",
		"Mana Storm",
		"Breathe Logrus",
		"Call the Void"
	},

	/*** Death Spells ***/

	{
		/* Common Death Spellbooks */
		"Detect Unlife",
		"Malediction",
		"Detect Evil",
		"Stinking Cloud",
		"Black Sleep",
		"Resist Poison",
		"Horrify",
		"Enslave Undead",

		"Orb of Entropy",
		"Nether Bolt",
		"Terror",
		"Vampiric Drain",
		"Poison Branding",
		"Dispel Good",
		"Genocide",
		"Restore Life",

		/* Rare Death Spellbooks */
		"Berserk",
		"Invoke Spirits",
		"Dark Bolt",
		"Battle Frenzy",
		"Vampirism True",
		"Vampiric Branding",
		"Darkness Storm",
		"Mass Genocide",

		"Death Ray",
		"Raise the Dead",
		"Esoteria",
		"Word of Death",
		"Evocation",
		"Hellfire",
		"Omnicide",
		"Wraithform"
	},

	/* Trump Spellbooks */

	{
		/* Common Trump Spellbooks */
		"Phase Door",
		"Mind Blast",
		"Shuffle",
		"Reset Recall",
		"Teleport",
		"Dimension Door",
		"Trump Spying",
		"Teleport Away",

		"Trump Reach",
		"Trump Animal",
		"Phantasmal Servant",
		"Trump Monster",
		"Conjure Elemental",
		"Teleport Level",
		"Word of Recall",
		"Banish",

		/* Rare Trump Spellbooks */
		"Joker Card",
		"Trump Spiders",
		"Trump Reptiles",
		"Trump Hounds",
		"Trump Branding",
		"Living Trump",
		"Death Dealing",
		"Trump Cyberdemon",


		"Trump Divination",
		"Trump Lore",
		"Trump Undead",
		"Trump Dragon",
		"Mass Trump",
		"Trump Demon",
		"Trump Ancient Dragon",
		"Trump Greater Undead"
	},

	/* Arcane Spellbooks (_only_ common spells) */

	{
		"Zap",
		"Wizard Lock",
		"Detect Invisibility",
		"Detect Monsters",
		"Blink",
		"Light Area",
		"Trap & Door Destruction",
		"Cure Light Wounds",

		"Detect Doors & Traps",
		"Phlogiston",
		"Detect Treasure",
		"Detect Enchantment",
		"Detect Objects",
		"Cure Poison",
		"Resist Cold",
		"Resist Fire",

		"Resist Lightning",
		"Resist Acid",
		"Cure Medium Wounds",
		"Teleport",
		"Stone to Mud",
		"Ray of Light",
		"Satisfy Hunger",
		"See Invisible",

		"Recharging",
		"Teleport Level",
		"Identify",
		"Teleport Away",
		"Elemental Ball",
		"Detection",
		"Word of Recall",
		"Clairvoyance"
	}
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
cptr player_title[MAX_CLASS][PY_MAX_LEVEL / 5] =
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
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
	},

	/* Priest */
	{
		"Believer",
		"Acolyte",
		"Adept",
		"Curate",
		"Canon",
		"Priest",
		"High Priest",
		"Cardinal",
		"Inquisitor",
		"Pope",
	},

	/* Rogues */
	{
		"Cutpurse",
		"Robber",
		"Burglar",
		"Filcher",
		"Sharper",
		"Low Thief",
		"High Thief",
		"Master Thief",
		"Assassin",
		"Guildmaster",
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

	/* Warrior-Mage */
	{
		"Novice",
		"Apprentice",
		"Journeyman",
		"Veteran",
		"Enchanter",
		"Champion",
		"Mage-Hero",
		"Baron Mage",
		"Battlemage",
		"Wizard Lord",
	},

	/* Chaos Warrior */
	{
		"Rookie",
		"Soldier",
		"Mercenary",
		"Veteran",
		"Swordsman",
		"Champion",
		"Chaos Hero",
		"Chaos Baron",
		"Chaos Duke",
		"Chaos Lord",
	},

	/* Monk */
	{
		"Initiate",
		"Brother",
		"Disciple",
		"Immaculate",
		"Master",
		"Soft Master",
		"Hard Master",
		"Flower Master",
		"Dragon Master",
		"Grand Master",
	},

	/* Mindcrafter */
	{
		"Trainee",
		"Acolyte",
		"Adept",
		"Immaculate",
		"Contemplator",
		"Mentalist",
		"Psychic",
		"Psionicist",
		"Esper",
		"Mindmaster",
	},

	/* High Mage; same as Mage */
	{
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
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
	"Display dungeon view",
	"Display snap-shot",
	NULL,
	"Display script messages",
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
option_type option_info[OPT_MAX] =
{
	{FALSE, 1, "rogue_like_commands",	"Rogue-like commands" },
	{TRUE,  1, "quick_messages",		"Activate quick messages" },
	{FALSE, 1, "other_query_flag",		"Prompt for various information" },
	{TRUE,  1, "carry_query_flag",		"Prompt before picking things up" },
	{FALSE, 1, "use_old_target",		"Use old target by default" },
	{TRUE,  1, "always_pickup",			"Pick things up by default" },
	{TRUE,  1, "always_repeat",			"Repeat obvious commands" },
	{FALSE, 5, "depth_in_feet",			"Show dungeon level in feet" },
	{TRUE,  1, "stack_force_notes",		"Merge inscriptions when stacking" },
	{FALSE, 1, "stack_force_costs",		"Merge discounts when stacking" },
	{TRUE,  5, "show_labels",			"Show labels in object listings" },
	{TRUE,  5, "show_weights",			"Show weights in object listings" },
	{TRUE,  0, NULL,					"Number 12" },
	{TRUE,  0, NULL,					"Number 13" },
	{FALSE, 2, "ring_bell",				"Audible bell (on errors, etc)" },
	{TRUE,  5, "use_color",				"Use color if possible (slow)" },
	{FALSE, 2, "find_ignore_stairs",	"Run past stairs" },
	{TRUE,  2, "find_ignore_doors",		"Run through open doors" },
	{FALSE, 2, "find_cut",				"Run past known corners" },
	{TRUE,  2, "find_examine",			"Run into potential corners" },
	{TRUE,  2, "disturb_move",			"Disturb whenever any monster moves" },
	{TRUE,  2, "disturb_near",			"Disturb whenever viewable monster moves" },
	{TRUE,  2, "disturb_panel",			"Disturb whenever map panel changes" },
	{TRUE,  2, "disturb_state",			"Disturb whenever player state changes" },
	{TRUE,  2, "disturb_minor",			"Disturb whenever boring things happen" },
	{TRUE,  2, "disturb_other",			"Disturb whenever random things happen" },
	{TRUE,  2, "disturb_traps",			"Disturb when you leave detection radius" },
	{FALSE, 2, "alert_failure",			"Alert user to various failures" },
	{TRUE,  3, "last_words",			"Get last words when the character dies" },
	{TRUE,  3, "speak_unique",			"Allow uniques to speak" },
	{TRUE,  3, "small_levels",			"Allow unusually small dungeon levels" },
	{TRUE,  3, "empty_levels",			"Allow empty 'arena' levels" },

	{TRUE,  1, "auto_haggle",			"Auto-haggle in stores" },
	{FALSE, 3, "auto_scum",				"Auto-scum for good levels" },
	{FALSE, 1, "stack_allow_items",		"Allow weapons and armor to stack" },
	{TRUE,  1, "stack_allow_wands",		"Allow wands/staffs/rods to stack" },
	{TRUE,  1, "expand_look",			"Expand the power of the look command" },
	{TRUE,  1, "expand_list",			"Expand the power of the list commands" },
	{TRUE,  3, "view_perma_grids",		"Map remembers all perma-lit grids" },
	{FALSE, 3, "view_torch_grids",		"Map remembers all torch-lit grids" },
	{FALSE, 3, "dungeon_align",			"Generate dungeons with aligned rooms" },
	{TRUE,  3, "dungeon_stair",			"Generate dungeons with connected stairs" },
	{TRUE,  7, "flow_by_sound",			"Monsters chase current location (slow)" },
	{TRUE,  7, "flow_by_smell",			"Monsters chase recent locations (slow)" },
	{TRUE,  0, NULL,					"Number 44" },
	{TRUE,  0, NULL,					"Number 45" },
	{TRUE,  7, "smart_learn",			"Monsters learn from their mistakes" },
	{FALSE, 7, "smart_cheat",			"Monsters exploit players weaknesses" },
	{FALSE, 4, "view_reduce_lite",		"Reduce lite-radius when running" },
	{FALSE, 4, "view_reduce_view",		"Reduce view-radius in town" },
	{FALSE, 4, "avoid_abort",			"Avoid checking for user abort" },
	{FALSE, 4, "avoid_other",			"Avoid processing special colors" },
	{TRUE,  4, "flush_failure",			"Flush input on various failures" },
	{FALSE, 4, "flush_disturb",			"Flush input whenever disturbed" },
	{FALSE, 4, "flush_command",			"Flush input before every command" },
	{TRUE,  4, "fresh_before",			"Flush output before every command" },
	{FALSE, 4, "fresh_after",			"Flush output after every command" },
	{FALSE, 4, "fresh_message",			"Flush output after every message" },
	{TRUE,  4, "compress_savefile",		"Compress messages in savefiles" },
	{TRUE,  5, "hilite_player",			"Hilite the player with the cursor" },
	{FALSE, 5, "view_yellow_lite",		"Use special colors for torch-lit grids" },
	{FALSE, 5, "view_bright_lite",		"Use special colors for 'viewable' grids" },
	{FALSE, 5, "view_granite_lite",		"Use special colors for wall grids (slow)" },
	{FALSE, 5, "view_special_lite",		"Use special colors for floor grids (slow)" },

	{TRUE,  0, NULL,					"Number 64" },
	{TRUE,  0, NULL,					"Number 65" },
	{TRUE,  0, NULL,					"Number 66" },
	{TRUE,  0, NULL,					"Number 67" },
	{TRUE,  0, NULL,					"Number 68" },
	{TRUE,  0, NULL,					"Number 69" },
	{TRUE,  0, NULL,					"Number 70" },
	{TRUE,  0, NULL,					"Number 71" },
	{TRUE,  0, NULL,					"Number 72" },
	{TRUE,  0, NULL,					"Number 73" },
	{TRUE,  0, NULL,					"Number 74" },
	{TRUE,  0, NULL,					"Number 75" },
	{TRUE,  0, NULL,					"Number 76" },
	{TRUE,  0, NULL,					"Number 77" },
	{TRUE,  0, NULL,					"Number 78" },
	{TRUE,  0, NULL,					"Number 79" },
	{TRUE,  0, NULL,					"Number 80" },
	{TRUE,  0, NULL,					"Number 81" },
	{TRUE,  0, NULL,					"Number 82" },
	{TRUE,  0, NULL,					"Number 83" },
	{TRUE,  0, NULL,					"Number 84" },
	{TRUE,  0, NULL,					"Number 85" },
	{TRUE,  0, NULL,					"Number 86" },
	{TRUE,  0, NULL,					"Number 87" },
	{TRUE,  0, NULL,					"Number 88" },
	{TRUE,  0, NULL,					"Number 89" },
	{TRUE,  0, NULL,					"Number 90" },
	{TRUE,  0, NULL,					"Number 91" },
	{TRUE,  0, NULL,					"Number 92" },
	{TRUE,  0, NULL,					"Number 93" },
	{TRUE,  0, NULL,					"Number 94" },
	{TRUE,  0, NULL,					"Number 95" },
	
	{TRUE,  0, NULL,					"Number 96" },
	{TRUE,  0, NULL,					"Number 97" },
	{TRUE,  0, NULL,					"Number 98" },
	{TRUE,  0, NULL,					"Number 99" },
	{TRUE,  0, NULL,					"Number 100" },
	{TRUE,  0, NULL,					"Number 101" },
	{TRUE,  0, NULL,					"Number 102" },
	{TRUE,  0, NULL,					"Number 103" },
	{TRUE,  0, NULL,					"Number 104" },
	{TRUE,  0, NULL,					"Number 105" },
	{TRUE,  0, NULL,					"Number 106" },
	{TRUE,  0, NULL,					"Number 107" },
	{TRUE,  0, NULL,					"Number 108" },
	{TRUE,  0, NULL,					"Number 109" },
	{TRUE,  0, NULL,					"Number 110" },
	{TRUE,  0, NULL,					"Number 111" },
	{TRUE,  0, NULL,					"Number 112" },
	{TRUE,  0, NULL,					"Number 113" },
	{TRUE,  0, NULL,					"Number 114" },
	{TRUE,  0, NULL,					"Number 115" },
	{TRUE,  0, NULL,					"Number 116" },
	{TRUE,  0, NULL,					"Number 117" },
	{TRUE,  0, NULL,					"Number 118" },
	{TRUE,  0, NULL,					"Number 119" },
	{TRUE,  0, NULL,					"Number 120" },
	{TRUE,  0, NULL,					"Number 121" },
	{TRUE,  0, NULL,					"Number 122" },
	{TRUE,  0, NULL,					"Number 123" },
	{TRUE,  0, NULL,					"Number 124" },
	{TRUE,  0, NULL,					"Number 125" },
	{TRUE,  0, NULL,					"Number 126" },
	{TRUE,  0, NULL,					"Number 127" },

	{TRUE,  0, NULL,					"Number 128" },
	{TRUE,  0, NULL,					"Number 129" },
	{TRUE,  0, NULL,					"Number 130" },
	{TRUE,  0, NULL,					"Number 131" },
	{TRUE,  0, NULL,					"Number 132" },
	{TRUE,  0, NULL,					"Number 133" },
	{TRUE,  0, NULL,					"Number 134" },
	{TRUE,  0, NULL,					"Number 135" },
	{TRUE,  0, NULL,					"Number 136" },
	{TRUE,  0, NULL,					"Number 137" },
	{TRUE,  0, NULL,					"Number 138" },
	{TRUE,  0, NULL,					"Number 139" },
	{TRUE,  0, NULL,					"Number 140" },
	{TRUE,  0, NULL,					"Number 141" },
	{TRUE,  0, NULL,					"Number 142" },
	{TRUE,  0, NULL,					"Number 143" },
	{TRUE,  0, NULL,					"Number 144" },
	{TRUE,  0, NULL,					"Number 145" },
	{TRUE,  0, NULL,					"Number 146" },
	{TRUE,  0, NULL,					"Number 147" },
	{TRUE,  0, NULL,					"Number 148" },
	{TRUE,  0, NULL,					"Number 149" },
	{TRUE,  0, NULL,					"Number 150" },
	{TRUE,  0, NULL,					"Number 151" },
	{TRUE,  0, NULL,					"Number 152" },
	{TRUE,  0, NULL,					"Number 153" },
	{TRUE,  0, NULL,					"Number 154" },
	{TRUE,  0, NULL,					"Number 155" },
	{TRUE,  0, NULL,					"Number 156" },
	{TRUE,  0, NULL,					"Number 157" },
	{TRUE,  0, NULL,					"Number 158" },
	{TRUE,  0, NULL,					"Number 159" },

	{TRUE,  0, NULL,					"Number 160" },
	{FALSE, 5, "plain_descriptions",	"Plain object descriptions" },
	{FALSE, 6, "stupid_monsters",		"Monsters behave stupidly" },
	{FALSE, 1, "auto_destroy",			"No query to destroy known worthless items" },
	{FALSE, 1, "confirm_wear",			"Confirm to wear/wield known cursed items" },
	{FALSE, 1, "confirm_stairs",		"Prompt before exiting a dungeon level" },
	{TRUE,  1, "easy_open",				"Automatically open doors" },
	{TRUE,  1, "easy_disarm",			"Automatically disarm traps" },
	{FALSE, 1, "easy_floor",			"Display floor stacks in a list" },
	{FALSE, 1, "use_command",			"Allow unified use command" },
	{FALSE, 5, "center_player",			"Always center on the player (*slow*)" },
	{FALSE, 5, "avoid_center",			"Avoid centering while running" },
	{TRUE,  0, NULL,					"Number 172" },
	{TRUE,  0, NULL,					"Number 173" },
	{TRUE,  0, NULL,					"Number 174" },
	{TRUE,  0, NULL,					"Number 175" },
	{TRUE,  0, NULL,					"Number 176" },
	{TRUE,  0, NULL,					"Number 177" },
	{TRUE,  0, NULL,					"Number 178" },
	{TRUE,  0, NULL,					"Number 179" },
	{TRUE,  0, NULL,					"Number 180" },
	{TRUE,  0, NULL,					"Number 181" },
	{TRUE,  0, NULL,					"Number 182" },
	{TRUE,  0, NULL,					"Number 183" },
	{TRUE,  0, NULL,					"Number 184" },
	{TRUE,  0, NULL,					"Number 185" },
	{TRUE,  0, NULL,					"Number 186" },
	{TRUE,  0, NULL,					"Number 187" },
	{TRUE,  0, NULL,					"Number 188" },
	{TRUE,  0, NULL,					"Number 189" },
	{TRUE,  0, NULL,					"Number 190" },
	{TRUE,  0, NULL,					"Number 191" },

	{FALSE, 6, "vanilla_town",			"Use 'vanilla' town without quests and wilderness" },
	{TRUE,  0, NULL,					"Number 193" },
	{FALSE, 6, "ironman_shops",			"Stores are permanently closed" },
	{FALSE, 6, "ironman_small_levels",	"Always create unusually small dungeon levels" },
	{FALSE, 6, "ironman_downward",		"Don't allow climbing upwards/recalling" },
	{FALSE, 6, "ironman_autoscum",		"Permanently enable the autoscummer" },
	{FALSE, 6, "ironman_hard_quests",	"Quest monsters get reinforcements" },
	{TRUE,  0, NULL,					"Number 199" },
	{FALSE, 6, "ironman_empty_levels",	"Always create empty 'arena' levels" },
	{TRUE,  6, "terrain_streams",		"Create terrain 'streamers' in the dungeon" },
	{FALSE,  6, "ironman_moria",			"The good old days..." },
	{FALSE, 6, "munchkin_death",		"Ask for saving death" },
	{FALSE, 6, "ironman_rooms",			"Always generate very unusual rooms" },
	{TRUE,  6, "maximize_mode",			"Maximize stats" },
	{TRUE,  6, "preserve_mode",			"Preserve artifacts" },
	{TRUE,  6, "autoroller",			"Specify 'minimal' stats" },
	{FALSE, 6, "point_based",			"Generate character using a point system" },
	{TRUE,  6, "silly_monsters",		"Allow silly monsters" },
	{FALSE, 6, "ironman_nightmare",		"Nightmare mode (this isn't even remotely fair!)" },
	{TRUE,  0, NULL,					"Number 211" },
	{TRUE,  0, NULL,					"Number 212" },
	{TRUE,  0, NULL,					"Number 213" },
	{TRUE,  0, NULL,					"Number 214" },
	{TRUE,  0, NULL,					"Number 215" },
	{TRUE,  0, NULL,					"Number 216" },
	{TRUE,  0, NULL,					"Number 217" },
	{TRUE,  0, NULL,					"Number 218" },
	{TRUE,  0, NULL,					"Number 219" },
	{TRUE,  0, NULL,					"Number 220" },
	{TRUE,  0, NULL,					"Number 221" },
	{TRUE,  0, NULL,					"Number 222" },
	{TRUE,  0, NULL,					"Number 223" },
#if 0
	{FALSE, 8, "destroy_worthless",		"Auto-destroy known worthless items" },
#else /* 0 */
	{FALSE, 0, NULL,					"Auto-destroy known worthless items" },
#endif /* 0 */
	{FALSE, 8, "monster_light",			"Allow monsters to carry lights" },
	{TRUE,  0, NULL,					"Turn on muliplayer client - server code" },
	{TRUE,  0, NULL,					"Number 227" },
	{TRUE,  0, NULL,					"Number 228" },
	{TRUE,  0, NULL,					"Number 229" },
	{TRUE,  0, NULL,					"Number 230" },
	{TRUE,  0, NULL,					"Number 231" },
	{TRUE,  0, NULL,					"Number 232" },
	{TRUE,  0, NULL,					"Number 233" },
	{TRUE,  0, NULL,					"Number 234" },
	{TRUE,  0, NULL,					"Number 235" },
	{TRUE,  0, NULL,					"Number 236" },
	{TRUE,  0, NULL,					"Number 237" },
	{TRUE,  0, NULL,					"Number 238" },
	{TRUE,  0, NULL,					"Number 239" },
	{TRUE,  0, NULL,					"Number 240" },
	{TRUE,  0, NULL,					"Number 241" },
	{TRUE,  0, NULL,					"Number 242" },
	{TRUE,  0, NULL,					"Number 243" },
	{TRUE,  0, NULL,					"Number 244" },
	{TRUE,  0, NULL,					"Number 245" },
	{TRUE,  0, NULL,					"Number 246" },
	{TRUE,  0, NULL,					"Number 247" },
	{TRUE,  0, NULL,					"Number 248" },
	{TRUE,  0, NULL,					"Number 249" },
	{TRUE,  0, NULL,					"Number 250" },
	{FALSE, 3, "auto_notes",			"Automatically note important events" },
	{FALSE, 3, "take_notes",			"Allow notes to be appended to a file" },
	{TRUE,  0, NULL,					"Number 253" },
	{TRUE,  3, "testing_stack",			"Allow objects to stack on floor" },
	{TRUE,  3, "testing_carry",			"Allow monsters to carry objects" },
};

int birth_options[OPT_BIRTH + 1] =
{
	162, 192, 194, 195, 196, 197, 198, 199,
	200, 201, 202, 203, 204, 205, 206, 207,
	208, 209, 210, 211, 212, 213, 214, 215,
	216, 217, 218, 219, 220, 221, 222, 223, 0
};

int server_options[OPT_SERVER + 1] =
{
	30,  31,  33,  34,  35,  36,  37,  40,
	41,  42,  43,  44,  45,  46,  47,  224,
	225, 226, 227, 228, 229, 230, 231, 232,
	233, 234, 235, 236, 237, 238, 254, 255, 0
};

cptr chaos_patrons[MAX_PATRON] =
{
	"Slortar",
	"Mabelode",
	"Chardros",
	"Hionhurn",
	"Xiombarg",

	"Pyaray",
	"Balaan",
	"Arioch",
	"Eequor",
	"Narjhan",

	"Balo",
	"Khorne",
	"Slaanesh",
	"Nurgle",
	"Tzeentch",

	"Khaine"
};

int chaos_stats[MAX_PATRON] =
{
	A_CON,  /* Slortar */
	A_CON,  /* Mabelode */
	A_STR,  /* Chardros */
	A_STR,  /* Hionhurn */
	A_STR,  /* Xiombarg */

	A_INT,  /* Pyaray */
	A_STR,  /* Balaan */
	A_INT,  /* Arioch */
	A_CON,  /* Eequor */
	A_CHR,  /* Narjhan */

	-1,     /* Balo */
	A_STR,  /* Khorne */
	A_CHR,  /* Slaanesh */
	A_CON,  /* Nurgle */
	A_INT,  /* Tzeentch */

	A_STR,  /* Khaine */
};




int chaos_rewards[MAX_PATRON][20] =
{
	/* Slortar the Old: */
	{
		REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL,
		REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF,
		REW_POLY_SLF, REW_POLY_SLF, REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP,
		REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL, REW_AUGM_ABL
	},

	/* Mabelode the Faceless: */
	{
		REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_H_SUMMON, REW_SUMMON_M,
		REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_WND,
		REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_ABL, REW_SER_UNDE,
		REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GOOD_OBS
	},

	/* Chardros the Reaper: */
	{
		REW_WRATH, REW_WRATH, REW_HURT_LOT, REW_PISS_OFF, REW_H_SUMMON,
		REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_DESTRUCT, REW_SER_UNDE,
		REW_GENOCIDE, REW_MASS_GEN, REW_MASS_GEN, REW_DISPEL_C, REW_GOOD_OBJ,
		REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL
	},

	/* Hionhurn the Executioner: */
	{
		REW_WRATH, REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL,
		REW_IGNORE, REW_IGNORE, REW_SER_UNDE, REW_DESTRUCT, REW_GENOCIDE,
		REW_MASS_GEN, REW_MASS_GEN, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL,
		REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL
	},

	/* Xiombarg the Sword-Queen: */
	{
		REW_TY_CURSE, REW_TY_CURSE, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL,
		REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND, REW_POLY_WND,
		REW_GENOCIDE, REW_DISPEL_C, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS,
		REW_GAIN_ABL, REW_CHAOS_WP, REW_GAIN_EXP, REW_AUGM_ABL, REW_GOOD_OBS
	},


	/* Pyaray the Tentacled Whisperer of Impossible Secretes: */
	{
		REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_H_SUMMON, REW_H_SUMMON,
		REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF,
		REW_POLY_SLF, REW_SER_DEMO, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL,
		REW_CHAOS_WP, REW_DO_HAVOC, REW_GOOD_OBJ, REW_GREA_OBJ, REW_GREA_OBS
	},

	/* Balaan the Grim: */
	{
		REW_TY_CURSE, REW_HURT_LOT, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL,
		REW_SUMMON_M, REW_LOSE_EXP, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND,
		REW_SER_UNDE, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_GAIN_EXP,
		REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GREA_OBS, REW_AUGM_ABL
	},

	/* Arioch, Duke of Hell: */
	{
		REW_WRATH, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_EXP, REW_H_SUMMON,
		REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF,
		REW_POLY_SLF, REW_MASS_GEN, REW_SER_DEMO, REW_HEAL_FUL, REW_CHAOS_WP,
		REW_CHAOS_WP, REW_GOOD_OBJ, REW_GAIN_EXP, REW_GREA_OBJ, REW_AUGM_ABL
	},

	/* Eequor, Blue Lady of Dismay: */
	{
		REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_CURSE_WP, REW_RUIN_ABL,
		REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND,
		REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS, REW_HEAL_FUL, REW_GAIN_EXP,
		REW_GAIN_ABL, REW_CHAOS_WP, REW_GOOD_OBS, REW_GREA_OBJ, REW_AUGM_ABL
	},

	/* Narjhan, Lord of Beggars: */
	{
		REW_WRATH, REW_CURSE_AR, REW_CURSE_WP, REW_CURSE_WP, REW_CURSE_AR,
		REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF,
		REW_POLY_WND, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_AUGM_ABL,
		REW_GOOD_OBJ, REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_GREA_OBS
	},

	/* Balo the Jester: */
	{
		REW_WRATH, REW_SER_DEMO, REW_CURSE_WP, REW_CURSE_AR, REW_LOSE_EXP,
		REW_GAIN_ABL, REW_LOSE_ABL, REW_POLY_WND, REW_POLY_SLF, REW_IGNORE,
		REW_DESTRUCT, REW_MASS_GEN, REW_CHAOS_WP, REW_GREA_OBJ, REW_HURT_LOT,
		REW_AUGM_ABL, REW_RUIN_ABL, REW_H_SUMMON, REW_GREA_OBS, REW_AUGM_ABL
	},

	/* Khorne the Bloodgod: */
	{
		REW_WRATH, REW_HURT_LOT, REW_HURT_LOT, REW_H_SUMMON, REW_H_SUMMON,
		REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_SER_MONS, REW_SER_DEMO,
		REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GOOD_OBJ,
		REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_GREA_OBJ, REW_GREA_OBS
	},

	/* Slaanesh: */
	{
		REW_WRATH, REW_PISS_OFF, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL,
		REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_SER_DEMO,
		REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_EXP,
		REW_GAIN_EXP, REW_CHAOS_WP, REW_GAIN_ABL, REW_GREA_OBJ, REW_AUGM_ABL
	},

	/* Nurgle: */
	{
		REW_WRATH, REW_PISS_OFF, REW_HURT_LOT, REW_RUIN_ABL, REW_LOSE_ABL,
		REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF,
		REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_ABL,
		REW_GAIN_ABL, REW_SER_UNDE, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL
	},

	/* Tzeentch: */
	{
		REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL,
		REW_LOSE_EXP, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_SLF,
		REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_CHAOS_WP, REW_GREA_OBJ,
		REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP, REW_GAIN_EXP, REW_AUGM_ABL
	},

	/* Khaine: */
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
	{ "You kick %s.",                           2, 0, 1, 5, 0 },
	{ "You strike %s.",                         3, 0, 1, 6, 0 },
	{ "You hit %s with your knee.",             5, 5, 2, 3, MA_KNEE },
	{ "You hit %s with your elbow.",            7, 5, 1, 7, 0 },
	{ "You butt %s.",                           9, 10, 2, 4, 0 },
	{ "You kick %s.",                           11, 10, 2, 5, MA_SLOW },
	{ "You uppercut %s.",                       13, 12, 3, 5, 6 },
	{ "You double-kick %s.",                    16, 15, 6, 3, 8 },
	{ "You hit %s with a Cat's Claw.",          20, 20, 4, 6, 0 },
	{ "You hit %s with a jump kick.",           25, 25, 4, 7, 10 },
	{ "You hit %s with an Eagle's Claw.",       29, 25, 5, 6, 0 },
	{ "You hit %s with a circle kick.",         33, 30, 5, 8, 10 },
	{ "You hit %s with an Iron Fist.",          37, 35, 6, 8, 10 },
	{ "You hit %s with a flying kick.",         41, 35, 7, 8, 12 },
	{ "You hit %s with a Dragon Fist.",         45, 35, 7, 10, 16 },
	{ "You hit %s with a Crushing Blow.",       48, 35, 7, 12, 18 },
#else
	{ "You punch %s.",                          1, 0, 1, 4, 0 },
	{ "You kick %s.",                           2, 0, 1, 5, 0 },
	{ "You strike %s.",                         3, 0, 1, 6, 0 },
	{ "You knee %s.",                           5, 5, 2, 3, MA_KNEE },
	{ "You hit %s.",                            7, 5, 1, 7, 0 },
	{ "You butt %s.",                           9, 10, 2, 4, 0 },
	{ "You kick %s.",                           11, 10, 2, 5, MA_SLOW },
	{ "You uppercut %s.",                       13, 12, 3, 5, 6 },
	{ "You double-kick %s.",                    16, 15, 6, 3, 8 },
	{ "You hit %s.",                            20, 20, 4, 6, 0 },
	{ "You kick %s.",                           25, 25, 4, 7, 10 },
	{ "You hit %s.",                            29, 25, 5, 6, 0 },
	{ "You kick %s.",                           33, 30, 5, 8, 10 },
	{ "You punch %s.",                          37, 35, 6, 8, 10 },
	{ "You kick %s.",                           41, 35, 7, 8, 12 },
	{ "You punch %s.",                          45, 35, 7, 10, 16 },
	{ "You punch %s.",                          48, 35, 7, 12, 18 },
#endif
};

/*
 * Table of game-generated inscriptions (indexed by the defines in
 * defines.h). -- RG
 */
cptr game_inscriptions[FEEL_MAX] =
{
	NULL,            /* FEEL_NONE */
	"broken",        /* FEEL_BROKEN */
	"terrible",      /* FEEL_TERRIBLE */
	"worthless",     /* FEEL_WORTHLESS */
	"cursed",        /* FEEL_CURSED */
	"uncursed",      /* FEEL_UNCURSED */
	"average",       /* FEEL_AVERAGE */
	"good",          /* FEEL_GOOD */
	"excellent",     /* FEEL_EXCELLENT */
	"special",       /* FEEL_SPECIAL */
	"bad",			 /* FEEL_BAD */
	"dubious",		 /* FEEL_DUBIOUS */
	"tainted",		 /* FEEL_TAINTED */
};


/* Weird melee attack types when hallucinating */
cptr silly_attacks[MAX_SILLY_ATTACK] =
{
	"smothers",
	"hugs",
	"humiliates",
	"whips",
	"kisses",

	"disgusts",
	"pees all over",
	"passes the gas on",
	"makes obscene gestures at",
	"licks",

	"stomps on",
	"swallows",
	"drools on",
	"misses",
	"shrinks",

	"emasculates",
	"evaporates",
	"solidifies",
	"digitizes",
	"insta-kills",

	"massacres",
	"slaughters",
	"drugs",
	"psychoanalyzes",
	"deconstructs",

	"falsifies",
	"disbelieves",
	"molests"
};

/* Field function's + names */
field_action f_action[FIELD_ACTION_TYPES] =
{
	/* Null - Do absolutely nothing */
	{NULL, "nothing"}, 
	
	/* Test - Do absolutely nothing */
	{field_action_nothing, "field_action_nothing"},
	
	/* Delete the field */
	{field_action_delete, "field_action_delete"},
	
	/* Glyph of warding function */
	{field_action_glyph_warding, "field_action_glyph_warding"},
	
	/* Explosive rune function */
	{field_action_glyph_explode, "field_action_glyph_explode"},
	
	/* Corpse decay function */
	{field_action_corpse_decay, "field_action_corpse_decay"},
	
	/* Initialise corpse on load of savefile */
	{field_action_corpse_load, "field_action_corpse_load"},
	
	/* Corpse init function */
	{field_action_corpse_init, "field_action_corpse_init"},
	
	/* Corpse raising function for spell */
	{field_action_corpse_raise, "field_action_corpse_raise"},
	
	/* Function returning name of the monster the corpse once was. */
	{field_action_corpse_look, "field_action_corpse_look"},
	
	/* Tunnel into a "standard" wall to pass */
	{field_action_wall_tunnel, "field_action_wall_tunnel"},
	
	/* Walls interact with GF_KILL_WALL */
	{field_action_wall_gf, "field_action_wall_gf"},
	
	/* Interact with grid 'tunnels' */
	{field_action_interact_tunnel, "field_action_interact_tunnel"},
	
	/* Interact with grid 'disarms' */
	{field_action_interact_disarm, "field_action_interact_disarm"},
	
	/* Interact with grid 'opens' */
	{field_action_interact_open, "field_action_interact_open"},
		
	/* Trap init function */
	{field_action_trap_init, "field_action_trap_init"},
	
	/* Trap disarming function */
	{field_action_trap_disarm, "field_action_trap_disarm"},

	/* Trap magical interaction */
	{field_action_trap_gf, "field_action_trap_gf"},
	
	/* Trapdoor */
	{field_action_hit_trap_door, "field_action_hit_trap_door"},
	
	/* Pit */
	{field_action_hit_trap_pit, "field_action_hit_trap_pit"},
	
	/* Spiked Pit */
	{field_action_hit_trap_spike, "field_action_hit_trap_spike"},
	
	/* Poisoned Spiked Pit */
	{field_action_hit_trap_poison_pit, "field_action_hit_trap_poison_pit"},
	
	/* Curse Trap */
	{field_action_hit_trap_curse, "field_action_hit_trap_curse"},
	
	/* Teleport Trap */
	{field_action_hit_trap_teleport, "field_action_hit_trap_teleport"},
	
	/* Elemental Trap */
	{field_action_hit_trap_element, "field_action_hit_trap_element"},
	
	/* Elemental Ball Trap */
	{field_action_hit_trap_ba_element, "field_action_hit_trap_ba_element"},
	
	/* Gas Trap */
	{field_action_hit_trap_gas, "field_action_hit_trap_gas"},
	
	/* Trap Trap */
	{field_action_hit_trap_traps, "field_action_hit_trap_trap"},
	
	/* Temp Drain Stat Trap */
	{field_action_hit_trap_temp_stat, "field_action_hit_trap_temp_stat"},
	
	/* Perm Drain Stat Trap */
	{field_action_hit_trap_perm_stat, "field_action_hit_trap_perm_stat"},
	
	/* Lose XP Trap */
	{field_action_hit_trap_lose_xp, "field_action_hit_trap_lose_xp"},
	
	/* Disenchant Trap */
	{field_action_hit_trap_disenchant, "field_action_hit_trap_disenchant"},
	
	/* Drop Item Trap */
	{field_action_hit_trap_drop_item, "field_action_hit_trap_drop_item"},
	
	/* Mutation Trap */
	{field_action_hit_trap_mutate, "field_action_hit_trap_mutate"},
	
	/* New Life Trap */
	{field_action_hit_trap_new_life, "field_action_hit_trap_new_life"},
	
	/* Light sucking Trap */
	{field_action_hit_trap_no_lite, "field_action_hit_trap_no_lite"},
	
	/* Hunger Trap */
	{field_action_hit_trap_hunger, "field_action_hit_trap_hunger"},
	
	/* Gold destruction Trap */
	{field_action_hit_trap_no_gold, "field_action_hit_trap_no_gold"},
	
	/* Haste Monster Trap */
	{field_action_hit_trap_haste_mon, "field_action_hit_trap_haste_monster"},
	
	/* Raise Monster Trap */
	{field_action_hit_trap_raise_mon, "field_action_hit_trap_raise_mon"},
	
	/* Drain charges Trap */
	{field_action_hit_trap_drain_magic, "field_action_hit_trap_drain_magic"},
	
	/* Aggravate Monster Trap */
	{field_action_hit_trap_aggravate, "field_action_hit_trap_aggravate"},
	
	/* Summon Monster Trap */
	{field_action_hit_trap_summon, "field_action_hit_trap_summon"},
	
	/* Lose Memory Trap */
	{field_action_hit_trap_lose_memory, "field_action_hit_trap_lose_memory"},
	
	/* Initialise a field with a counter */
	{field_action_counter_init, "field_action_counter_init"},
	
	/* Attempt to unlock a door */
	{field_action_door_unlock, "field_action_door_unlock"},
		
	/* Attempt to bash a door */
	{field_action_door_bash, "field_action_door_bash"},
	
	/* Monster attemts to enter locked door */
	{field_action_door_lock_monster, "field_action_door_lock_monster"},
	
	/* Monster attemts to enter jammed door */
	{field_action_door_jam_monster, "field_action_door_jam_monster"},
	
	/* Doors interact with magic */
	{field_action_door_gf, "field_action_door_gf"},
	
	/* Stores open when walked on */
	{field_action_door_store, "field_action_door_store"},
	
	/* Buildings open when walked on */
	{field_action_door_build, "field_action_door_build"},
};

