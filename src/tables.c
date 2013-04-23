
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
const s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };

/*
 * Global arrays for converting "keypad direction" into offsets
 */
const s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

const s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };

/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 */
const s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

const s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };


/*
 * Circular keypad direction array
 */
const s16b cdd[8] =
{ 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Global arrays for optimizing "ddx[cdd[i]]" and "ddy[cdd[i]]"
 */
const s16b ddx_cdd[8] =
{ 0, 1, 1, 1, 0, -1, -1, -1 };

const s16b ddy_cdd[8] =
{ 1, 1, 0, -1, -1, -1, 0, 1 };



/*
 * Global array for converting numbers to uppercase hecidecimal digit
 * This array can also be used to convert a number to an octal digit
 */
const char hexsym[16] =
{
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};


/*
 * Global array for converting numbers to a logical list symbol
 */
const char listsym[] =
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
const byte adj_mag_study[] =
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
const byte adj_mag_mana[] =
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
const byte adj_mag_fail[] =
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
const byte adj_mag_stat[] =
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
const byte adj_chr_gold[] =
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
const byte adj_int_dev[] =
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
const byte adj_wis_sav[] =
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
const byte adj_dex_dis[] =
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
const byte adj_int_dis[] =
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
const byte adj_dex_ta[] =
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
const byte adj_dex_th[] =
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
const byte adj_str_wgt[] =
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
const byte adj_str_hold[] =
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
const byte adj_str_dig[] =
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
const byte adj_str_blow[] =
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
const byte adj_dex_blow[] =
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
const byte adj_dex_safe[] =
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
const byte adj_con_fix[] =
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
const byte adj_con_mhp[] =
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
const byte blows_table[12][12] =
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
const owner_type owners[MAX_STORES][MAX_OWNERS] =
{
	{
		/* General store - 32 unique names */
		{ "Bilbo the Friendly",			2,    170, 108,  5, 15, RACE_HALFLING},
		{ "Raistlin the Chicken",		2,    175, 108,  4, 12, RACE_HUMAN},
		{ "Sultan the Midget",			3,    170, 107,  5, 15, RACE_GNOME},
		{ "Lyar-el the Comely",			3,    165, 107,  6, 18, RACE_ELF},
		{ "Falilmawen the Friendly",	3,    170, 108,  5, 15, RACE_HALFLING},
		{ "Voirin the Cowardly",		5,    175, 108,  4, 12, RACE_HUMAN},
		{ "Erashnak the Midget",		8,    170, 107,  5, 15, RACE_KAOTI},
		{ "Grug the Comely",			10,   165, 107,  6, 18, RACE_SAURIAN},
		{ "Forovir the Cheap",			2,    170, 108,  5, 15, RACE_HUMAN},
		{ "Ellis the Fool",				5,    175, 108,  4, 12, RACE_HUMAN},
		{ "Filbert the Hungry",			7,    170, 107,  5, 15, RACE_VAMPIRE},
		{ "Fthnargl Psathiggua",		10,	  165, 107,  6, 18, RACE_ILLITHID},
		{ "Eloise Long-Dead",			3,    170, 108,  5, 15, RACE_FAIRY},
		{ "Fundi the Slow",				5,    175, 108,  4, 12, RACE_AQUARIAN},
		{ "Granthus",					8,    170, 107,  5, 15, RACE_GOBLIN},
		{ "Lorax the Suave",			10,   165, 107,  6, 18, RACE_VAMPIRE},
		{ "Butch",						3,    170, 108,  5, 15, RACE_ORC},
		{ "Elbereth the Beautiful",		5,    175, 108,  4, 12, RACE_CENTAUR},
		{ "Sarleth the Sneaky",			8,    170, 107,  5, 15, RACE_GNOME},
		{ "Narlock",					10,   165, 107,  6, 18, RACE_DWARF},
		{ "Haneka the Small",			3,    170, 108,  5, 15, RACE_GNOME},
		{ "Loirin the Mad",				5,    175, 108,  4, 12, RACE_MINOTAUR},
		{ "Wuto Poisonbreath",			8,    170, 107,  5, 15, RACE_DRACONIAN},
		{ "Araaka the Rotund",			10,   165, 107,  6, 18, RACE_DRACONIAN},
		{ "Poogor the Dumb",			2,    170, 108,  5, 15, RACE_KAOTI},
		{ "Felorfiliand",				5,    175, 108,  4, 12, RACE_ELF},
		{ "Maroka the Aged",			7,    170, 107,  5, 15, RACE_GNOME},
		{ "Sasin the Bold",				10,   165, 107,  6, 18, RACE_DRYAD},
		{ "Abiemar the Peasant",		2,    170, 108,  5, 15, RACE_HUMAN},
		{ "Hurk the Poor",				5,    175, 108,  4, 12, RACE_ORC},
		{ "Soalin the Wretched",		7,    170, 107,  5, 15, RACE_CATLING},
		{ "Merulla the Humble",			10,   165, 107,  6, 18, RACE_ELF},
	},
	{
		/* Armoury - 28 unique names */
		{ "Kon-Dar the Ugly",			50,   210, 115,  5,  7, RACE_ORC},
		{ "Darg-Low the Grim",			100,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_ENT},
		{ "Wieland the Smith",			300,  200, 112,  4,  5, RACE_DWARF},
		{ "Kon-Dar the Ugly",			100,  210, 115,  5,  7, RACE_ORC},
		{ "Darg-Low the Grim",			150,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_ELF},
		{ "Elo Dragonscale",			300,  200, 112,  4,  5, RACE_SAURIAN},
		{ "Delicatus",					100,  210, 115,  5,  7, RACE_PIXIE},
		{ "Gruce the Huge",				150,  190, 111,  4,  9, RACE_URUK},
		{ "Animus",						250,  200, 112,  4, 10, RACE_DRIDER},
		{ "Malvus",						300,  200, 112,  4,  5, RACE_RATLING},
		{ "Selaxis",					100,  210, 115,  5,  7, RACE_LEPRECHAUN},
		{ "Deathchill",					50,   190, 111,  4,  9, RACE_FAIRY},
		{ "Drios the Faint",			250,  200, 112,  4, 10, RACE_KOBOLD},
		{ "Bathric the Cold",			300,  200, 112,  4,  5, RACE_VAMPIRE},
		{ "Vengella the Cruel",			100,  210, 115,  5,  7, RACE_TROLL},
		{ "Wyrana the Mighty",			150,  190, 111,  4,  9, RACE_HUMAN},
		{ "Yojo II",					250,  200, 112,  4, 10, RACE_DWARF},
		{ "Ranalar the Sweet",			300,  200, 112,  4,  5, RACE_DRACONIAN},
		{ "Horbag the Unclean",			50,   210, 115,  5,  7, RACE_ORC},
		{ "Elelen the Telepath",		150,  190, 111,  4,  9, RACE_DARK_ELF},
		{ "Isedrelias",					250,  200, 112,  4, 10, RACE_AQUARIAN},
		{ "Vegnar One-eye",				50,   200, 112,  4,  5, RACE_CATLING},
		{ "Rodish the Chaotic",			100,  210, 115,  5,  7, RACE_KAOTI},
		{ "Hesin Swordmaster",			150,  190, 111,  4,  9, RACE_MINOTAUR},
		{ "Elvererith the Cheat",		100,  200, 112,  4, 10, RACE_DARK_ELF},
		{ "Zzathath the Imp",			300,  200, 112,  4,  5, RACE_CENTAUR},
		{ "Kon-Dar the Ugly",			50,   210, 115,  5,  7, RACE_ORC},
		{ "Darg-Low the Grim",			100,  190, 111,  4,  9, RACE_HUMAN},
		{ "Decado the Handsome",		250,  200, 112,  4, 10, RACE_TROLL},
		{ "Wieland the Smith",			300,  200, 112,  4,  5, RACE_DWARF},
	},
	{
		/* Weapon Smith - 28 unique names */
		{ "Arnold the Beastly",			50,   210, 115,  6,  6, RACE_SAURIAN},
		{ "Arndal Beast-Slayer",		100,  185, 110,  5,  9, RACE_AQUARIAN},
		{ "Eddie Beast-Master",			250,  190, 115,  5,  7, RACE_ORC},
		{ "Oglign Dragon-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
		{ "Drew the Skilled",			100,  210, 115,  6,  6, RACE_HUMAN},
		{ "Orrax Dragonson",			150,  185, 110,  5,  9, RACE_DRACONIAN},
		{ "Anthrax Disease-Carrier",	250,  190, 115,  5,  7, RACE_KAOTI},
		{ "Arkhoth the Stout",			300,  195, 112,  4,  8, RACE_DWARF},
		{ "Sarlyas the Rotten",			50,   210, 115,  6,  6, RACE_PIXIE},
		{ "Tuethic Big-Bones",			150,  185, 110,  5,  9, RACE_MINOTAUR},
		{ "Bilious",					250,  190, 115,  5,  7, RACE_KAOTI},
		{ "Fasgul",						300,  195, 112,  4,  8, RACE_CENTAUR},
		{ "Ellefris the Paladin",		100,  210, 115,  6,  6, RACE_CATLING},
		{ "K'trrik'k",					150,  185, 110,  5,  9, RACE_DRYAD},
		{ "Drocus Spiderfriend",		250,  190, 115,  5,  7, RACE_DARK_ELF},
		{ "Fungus Giant-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
		{ "Delantha",					100,  210, 115,  6,  6, RACE_ELF},
		{ "Solvistani the Ranger",		150,  185, 110,  5,  9, RACE_FAUN},
		{ "Xoril the Slow",				250,  190, 115,  5,  7, RACE_FAIRY},
		{ "Aeon Flux",					200,  195, 112,  4,  8, RACE_DRIDER},
		{ "Nadoc the Strong",			100,  210, 115,  6,  6, RACE_HALFLING},
		{ "Eramog the Weak",			150,  185, 110,  5,  9, RACE_KOBOLD},
		{ "Eowilith the Fair",			250,  190, 115,  5,  7, RACE_VAMPIRE},
		{ "Huimog Balrog-Slayer",		300,  195, 112,  4,  8, RACE_ORC},
		{ "Peadus the Cruel",			50,   210, 115,  6,  6, RACE_HUMAN},
		{ "Vamog Slayer",				150,  185, 110,  5,  9, RACE_OGRE},
		{ "Hooshnak the Vicious",		250,  190, 115,  5,  7, RACE_KAOTI},
		{ "Balenn War-Dancer",			300,  195, 112,  4,  8, RACE_URUK},
		{ "Arnold the Beastly",			50,   210, 115,  6,  6, RACE_RATLING},
		{ "Arndal Beast-Slayer",		100,  185, 110,  5,  9, RACE_ENT},
		{ "Eddie Beast-Master",			250,  190, 115,  5,  7, RACE_ORC},
		{ "Oglign Dragon-Slayer",		300,  195, 112,  4,  8, RACE_DWARF},
	},
	{
		/* Temple - 22 unique names */
		{ "Ludwig the Humble",			50,   175, 109,  6, 15, RACE_DWARF},
		{ "Gunnar the Paladin",			100,  185, 110,  5, 23, RACE_TROLL},
		{ "Torin the Chosen",			250,  180, 107,  6, 20, RACE_LEPRECHAUN},
		{ "Sarastro the Wise",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Sir Parsival the Pure",		250,  180, 107,  6, 20, RACE_SAURIAN},
		{ "Asenath the Holy",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "McKinnon",					100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Mistress Chastity",			150,  185, 110,  5, 23, RACE_AQUARIAN},
		{ "Hashnik the Druid",			250,  180, 107,  6, 20, RACE_HALFLING},
		{ "Finak",						300,  185, 109,  5, 15, RACE_YEEK},
		{ "Krikkik",					100,  175, 109,  6, 15, RACE_GOBLIN},
		{ "Morival the Wild",			150,  185, 110,  5, 23, RACE_ELF},
		{ "Hoshak the Dark",			250,  180, 107,  6, 20, RACE_MINOTAUR},
		{ "Atal the Wise",				300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Ibenidd the Chaste",			100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Eridish",					150,  185, 110,  5, 23, RACE_TROLL},
		{ "Vrudush the Shaman",			250,  180, 107,  6, 20, RACE_OGRE},
		{ "Haob the Berserker",			300,  185, 109,  5, 15, RACE_CENTAUR},
		{ "Proogdish the Youthfull",	100,  175, 109,  6, 15, RACE_OGRE},
		{ "Lumwise the Mad",			150,  185, 110,  5, 23, RACE_YEEK},
		{ "Muirt the Virtuous",			250,  180, 107,  6, 20, RACE_KOBOLD},
		{ "Dardobard the Weak",			300,  185, 109,  5, 15, RACE_FAIRY},
		{ "Ludwig the Humble",			50,   175, 109,  6, 15, RACE_DWARF},
		{ "Gunnar the Paladin",			100,  185, 110,  5, 23, RACE_TROLL},
		{ "Torin the Chosen",			250,  180, 107,  6, 20, RACE_CATLING},
		{ "Sarastro the Wise",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Sir Parsival the Pure",		250,  180, 107,  6, 20, RACE_DRYAD},
		{ "Asenath the Holy",			300,  185, 109,  5, 15, RACE_HUMAN},
		{ "Laura the Friendly",			100,  175, 109,  6, 15, RACE_HUMAN},
		{ "Mistress Chastity",			150,  185, 110,  5, 23, RACE_FAUN},
		{ "Hashnik the Druid",			250,  180, 107,  6, 20, RACE_HALFLING},
		{ "Finak",						300,  185, 109,  5, 15, RACE_YEEK},
	},
	{
		/* Alchemist - 26 unique names */
		{ "Mauser the Chemist",			100,  190, 111,  5,  8, RACE_ILLITHID},
		{ "Wizzle the Chaotic",			100,  190, 110,  6,  8, RACE_HALFLING},
		{ "Midas the Greedy",			150,  200, 116,  6,  9, RACE_GNOME},
		{ "Ja-Far the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Kakalrakakal",				150,  200, 116,  6,  9, RACE_ENT},
		{ "Jal-Eth the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Fanelath the Cautious",		100,  190, 111,  5,  8, RACE_DWARF},
		{ "Runcie the Insane",			100,  190, 110,  6,  8, RACE_HUMAN},
		{ "Grumbleworth",				150,  200, 116,  6,  9, RACE_GNOME},
		{ "Flitter",					150,  220, 111,  4,  9, RACE_PIXIE},
		{ "Xarillus",					100,  190, 111,  5,  8, RACE_HUMAN},
		{ "Egbert the Old",				100,  190, 110,  6,  8, RACE_DWARF},
		{ "Valindra the Proud",			150,  200, 116,  6,  9, RACE_DRIDER},
		{ "Taen the Alchemist",			150,  220, 111,  4,  9, RACE_HUMAN},
		{ "Cayd the Sweet",				100,  190, 111,  5,  8, RACE_VAMPIRE},
		{ "Fulir the Dark",				100,  190, 110,  6,  8, RACE_RATLING},
		{ "Domli the Humble",			150,  200, 116,  6,  9, RACE_DWARF},
		{ "Yaarjukka Demonspawn",		150,  220, 111,  4,  9, RACE_GOBLIN},
		{ "Gelaraldor the Herbmaster",	100,  190, 111,  5,  8, RACE_LEPRECHAUN},
		{ "Olelaldan the Wise",			100,  190, 110,  6,  8, RACE_DRACONIAN},
		{ "Fthoglo the Demonicist",		150,  200, 116,  6,  9, RACE_GOBLIN},
		{ "Dridash the Alchemist",		150,  220, 111,  4,  9, RACE_ORC},
		{ "Nelir the Strong",			100,  190, 111,  5,  8, RACE_SAURIAN},
		{ "Lignus the Pungent",			100,  190, 110,  6,  8, RACE_ORC},
		{ "Tilba",						150,  200, 116,  6,  9, RACE_HALFLING},
		{ "Myrildric the Wealthy",		150,  220, 111,  4,  9, RACE_HUMAN},
		{ "Mauser the Chemist",			100,  190, 111,  5,  8, RACE_AQUARIAN},
		{ "Wizzle the Chaotic",			100,  190, 110,  6,  8, RACE_HALFLING},
		{ "Midas the Greedy",			150,  200, 116,  6,  9, RACE_GNOME},
		{ "Ja-Far the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
		{ "Kakalrakakal",				150,  200, 116,  6,  9, RACE_ILLITHID},
		{ "Jal-Eth the Alchemist",		150,  220, 111,  4,  9, RACE_ELF},
	},
	{
		/* Magic Shop - 23 unique names */
		{ "Lo Pan the Sorcerer",		200,  200, 110,  7,  8, RACE_MINOTAUR},
		{ "Buggerby the Great",			200,  215, 113,  6, 10, RACE_GNOME},
		{ "The Wizard of Yendor",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Rjak the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Skidney the Sorcerer",		150,  200, 110,  7,  8, RACE_CENTAUR},
		{ "Kyria the Illusionist",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Nikki the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Solostoran",					150,  200, 110,  7,  8, RACE_PIXIE},
		{ "Achshe the Tentacled",		200,  215, 113,  6, 10, RACE_ILLITHID},
		{ "Kaza the Noble",				300,  200, 110,  7, 10, RACE_CATLING},
		{ "Fazzil the Dark",			300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Keldorn the Grand",			150,  200, 110,  7,  8, RACE_DWARF},
		{ "Philanthropus",				200,  215, 113,  6, 10, RACE_HALFLING},
		{ "Agnar the Enchantress",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Buliance the Necromancer",	300,  175, 110,  5, 11, RACE_KAOTI},
		{ "Vuirak the High-Mage",		150,  200, 110,  7,  8, RACE_KAOTI},
		{ "Madish the Smart",			200,  215, 113,  6, 10, RACE_KAOTI},
		{ "Falebrimbor",				300,  200, 110,  7, 10, RACE_DRYAD},
		{ "Felil-Gand the Subtle",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Thalegord the Shaman",		150,  200, 110,  7,  8, RACE_ENT},
		{ "Cthoaloth the Mystic",		200,  215, 113,  6, 10, RACE_ILLITHID},
		{ "Ibeli the Illusionist",		300,  200, 110,  7, 10, RACE_DRIDER},
		{ "Heto the Necromancer",		300,  175, 110,  5, 11, RACE_YEEK},
		{ "Lo Pan the Sorcerer",		200,  200, 110,  7,  8, RACE_RATLING},
		{ "Buggerby the Great",			200,  215, 113,  6, 10, RACE_GNOME},
		{ "The Wizard of Yendor",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Rjak the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Skidney the Sorcerer",		150,  200, 110,  7,  8, RACE_LEPRECHAUN},
		{ "Kyria the Illusionist",		300,  200, 110,  7, 10, RACE_HUMAN},
		{ "Nikki the Necromancer",		300,  175, 110,  5, 11, RACE_DARK_ELF},
		{ "Solostoran",					150,  200, 110,  7,  8, RACE_PIXIE},
		{ "Achshe the Tentacled",		200,  215, 113,  6, 10, RACE_ILLITHID},
	},
	{
		/* Black Market - 32 unique names */
		{ "Gary Gygaz",					200,  250, 150, 10,  5, RACE_TROLL},
		{ "Manteg the Hidden",			200,  250, 150, 10,  5, RACE_KAOTI},
		{ "Quark the Ferengi",			300,  250, 150, 10,  5, RACE_GOBLIN},
		{ "Topi the Fair(?)",			300,  250, 150, 10,  5, RACE_HUMAN},
		{ "Vhassa the Dead",			200,  250, 150, 10,  5, RACE_URUK},
		{ "Kyn the Treacherous",		200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Bubonicus",					300,  250, 150, 10,  5, RACE_KAOTI},
		{ "Corpselight",				300,  250, 150, 10,  5, RACE_FAIRY},
		{ "Parrish the Bloodthirsty",	200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Vile",						200,  250, 150, 10,  5, RACE_SAURIAN},
		{ "Prentice the Trusted",		300,  250, 150, 10,  5, RACE_AQUARIAN},
		{ "Griella Humanslayer",		300,  250, 150, 10,  5, RACE_ORC},
		{ "Angel",						200,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Flotsam the Bloated",		200,  250, 150, 10,  5, RACE_MINOTAUR},
		{ "Nieval",						300,  250, 150, 10,  5, RACE_VAMPIRE},
		{ "Anastasia the Luminous",		300,  250, 150, 10,  5, RACE_FAIRY},
		{ "Charity the Necromancer",	200,  250, 150, 10,  5, RACE_DARK_ELF},
		{ "Pugnacious the Pugilist",	200,  250, 150, 10,  5, RACE_ORC},
		{ "Footsore the Lucky",			300,  250, 150, 10,  5, RACE_KAOTI},
		{ "Sidria Lighfingered",		300,  250, 150, 10,  5, RACE_HUMAN},
		{ "Riatho the Juggler",			200,  250, 150, 10,  5, RACE_HALFLING},
		{ "Janaaka the Shifty",			200,  250, 150, 10,  5, RACE_GNOME},
		{ "Cina the Rogue",				300,  250, 150, 10,  5, RACE_GNOME},
		{ "Arunikki Greatclaw",			300,  250, 150, 10,  5, RACE_DRACONIAN},
		{ "Chaeand the Poor",			200,  250, 150, 10,  5, RACE_HUMAN},
		{ "Afardorf the Brigand",		200,  250, 150, 10,  5, RACE_URUK},
		{ "Lathaxl the Greedy",			300,  250, 150, 10,  5, RACE_ILLITHID},
		{ "Falarewyn",					300,  250, 150, 10,  5, RACE_PIXIE},
		{ "Vosur the Wrinkled",			200,  250, 150, 10,  5, RACE_CENTAUR},
		{ "Araord the Handsome",		200,  250, 150, 10,  5, RACE_CATLING},
		{ "Theradfrid the Loser",		300,  250, 150, 10,  5, RACE_HUMAN},
		{ "One-Legged Eroolo",			300,  250, 150, 10,  5, RACE_OGRE},
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
		{ "Home Sweet Home",				0,    100, 100,  0, 99, 99},
	},
	{
		/* Bookstore - 21 unique names */
		{ "Dolaf the Greedy",			100,  175, 108, 4, 12, RACE_HUMAN},
		{ "Odnar the Sage",				150,  120, 105, 6, 16, RACE_DRYAD},
		{ "Gandar the Neutral",			250,  120, 110, 7, 19, RACE_DARK_ELF},
		{ "Ro-sha the Patient",			300,  140, 105, 6, 12, RACE_ELF},
		{ "Randolph Carter",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Sarai the Swift",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Bodril the Seer",			200,  120, 105, 6, 16, RACE_FAUN},
		{ "Veloin the Quiet",			250,  120, 110, 7, 19, RACE_ENT},
		{ "Vanthylas the Learned",		300,  140, 105, 6, 12, RACE_ILLITHID},
		{ "Ossein the Literate",		150,  175, 108, 4, 12, RACE_FAIRY},
		{ "Olvar Bookworm",				200,  120, 105, 6, 16, RACE_VAMPIRE},
		{ "Shallowgrave",				250,  120, 110, 7, 19, RACE_DRIDER},
		{ "Death Mask",					300,  140, 105, 6, 12, RACE_RATLING},
		{ "Asuunu the Learned",			150,  175, 108, 4, 12, RACE_ILLITHID},
		{ "Prirand the Dead",			200,  120, 105, 6, 16, RACE_LEPRECHAUN},
		{ "Ronar the Iron",				250,  120, 110, 7, 19, RACE_DRACONIAN},
		{ "Galil-Gamir",				300,  140, 105, 6, 12, RACE_ELF},
		{ "Rorbag Book-Eater",			150,  175, 108, 4, 12, RACE_KOBOLD},
		{ "Kiriarikirk",				200,  120, 105, 6, 16, RACE_SAURIAN},
		{ "Rilin the Quiet",			250,  120, 110, 7, 19, RACE_DWARF},
		{ "Isung the Lord",				300,  140, 105, 6, 12, RACE_AQUARIAN},
		{ "Dolaf the Greedy",			100,  175, 108, 4, 12, RACE_HUMAN},
		{ "Odnar the Sage",				150,  120, 105, 6, 16, RACE_MINOTAUR},
		{ "Gandar the Neutral",			250,  120, 110, 7, 19, RACE_DARK_ELF},
		{ "Ro-sha the Patient",			300,  140, 105, 6, 12, RACE_ELF},
		{ "Randolph Carter",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Sarai the Swift",			150,  175, 108, 4, 12, RACE_HUMAN},
		{ "Bodril the Seer",			200,  120, 105, 6, 16, RACE_CENTAUR},
		{ "Veloin the Quiet",			250,  120, 110, 7, 19, RACE_CATLING},
		{ "Vanthylas the Learned",		300,  140, 105, 6, 12, RACE_ILLITHID},
		{ "Ossein the Literate",		150,  175, 108, 4, 12, RACE_DRYAD},
		{ "Olvar Bookworm",				200,  120, 105, 6, 16, RACE_VAMPIRE},
	}
};

/*
 * The list of owners for the new buildings
 * Building owners (exactly MAX_B_OWN owners per building, chosen randomly)
 * { name, greed, race}
 */
const b_own_type b_owners[MAX_BLDG][MAX_B_OWN] =
{
	{
		/* Weaponmaster */
		{ "Suiyan",				150,			RACE_HUMAN},
		{ "Aadocpeth",			100,			RACE_URUK},
		{ "Ognoqutoin",			95,				RACE_TROLL},
		{ "Nothall",			125,			RACE_DWARF},
		{ "Athoang",			90,				RACE_GNOME},
	},
	
	{
		/* Zymurgist */
		{ "Tanistil",			100,			RACE_ELF},
		{ "Paitnaw",			110,			RACE_GOBLIN},
		{ "Thiaeth",			125,			RACE_FAIRY},
		{ "Kaoghequin",			90,				RACE_YEEK},
		{ "Yaowing",			95,				RACE_FAUN},
	},
	
	{
		/* Magesmith Weapons */
		{ "Aotnron",			100,			RACE_ORC},
		{ "Pwetholn",			95,				RACE_DRACONIAN},
		{ "Tim",				120,			RACE_HUMAN},
		{ "Waowenth",			130,			RACE_ELF},
		{ "Yiquent",			90,				RACE_VAMPIRE},
	},
	
	{
		/* Magesmith Armour */
		{ "Paoingth",			100,			RACE_DARK_ELF},
		{ "Aargh'nt",			130,			RACE_KAOTI},
		{ "Wylntes",			125,			RACE_ELF},
		{ "Baongthan",			90,				RACE_DWARF},
		{ "Vitholm",			105,			RACE_PIXIE},
	},
	
	{
		/* Mutations */
		{ "Aaognwth",			120,			RACE_HUMAN},
		{ "Naothwell",			130,			RACE_DARK_ELF},
		{ "Jaltip",				95,				RACE_KAOTI},
		{ "Yillwyn",			100,			RACE_AQUARIAN},
		{ "Zyxlen",				110,			RACE_MINOTAUR},
	},
	
	{
		/* Maps */
		{ "Fsanong of the East", 90,			RACE_HUMAN},
		{ "Paginoth the Strider", 100,			RACE_ELF},
		{ "Xaingol the Wanderer", 110,			RACE_CENTAUR},
		{ "Wop of the high",	125,			RACE_GNOME},
		{ "Kaquin of the West",	95,				RACE_CATLING},
	},
	
	{
		/* Library */
		{ "Agpoan",				 85,			RACE_HUMAN},
		{ "Wewton", 			100,			RACE_DRYAD},
		{ "Masoognnix", 		110,			RACE_VAMPIRE},
		{ "Pagpon",				120,			RACE_ENT},
		{ "Leiwthen",			100,			RACE_LEPRECHAUN},
	},

	{
		/* Casino */
		{ "Key East",			100,			RACE_HUMAN},
		{ "Point Rip", 			100,			RACE_DRIDER},
		{ "Lean West",		 	100,			RACE_DWARF},
		{ "Tile Green",			100,			RACE_HUMAN},
		{ "Gold Red",			100,			RACE_DWARF},
	},
	
	{
		/* Inn */
		{ "Pwvnom",				100,			RACE_DARK_ELF},
		{ "Laign Mawan",		150,			RACE_PIXIE},
		{ "Palson",			 	200,			RACE_HUMAN},
		{ "Thwynyhtm",			250,			RACE_GOBLIN},
		{ "Chaillnew",			50,				RACE_HALFLING},
	},
	
	{
		/* Healer */
		{ "Nethlew",			80,			RACE_ELF},
		{ "Alorn Peln",			150,				RACE_GNOME},
		{ "Ahsilth Peon",	 	100,			RACE_HALFLING},
		{ "McPallion",			120,			RACE_HUMAN},
		{ "Qonwyn",				110,			RACE_DWARF},
	},


};


/*
 * Buying and selling adjustments for race combinations.
 * Entry[owner][player] gives the basic "cost inflation".
 */
const byte rgold_adj[MAX_RACES][MAX_RACES] =
{
	/*
	 * Hum, HfE, Elf,  Hal, Gno, Dwa, HfO, HfT, Dun, HiE, Barbarian,
	 * HfOg, HGn, HTn, Cyc, Yek, Klc, Kbd, Nbl, DkE, Drc, Mind Flayer,
	 * Imp,  Glm, Skl, Zombie, Vampire, Spectre, Sprite, Beastman, Ghoul
	 */

	/* Human */
	{ 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
	  124, 120, 110, 125, 115, 120, 120, 120, 120, 115, 120,
	  115, 105, 125, 125, 125, 125, 105, 120, 125 },

	/* Half-Elf */
	{ 110, 100, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  120, 115, 108, 115, 110, 110, 120, 120, 115, 115, 110,
	  120, 110, 110, 110, 120, 110, 100, 125, 110 },

	/* Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  120, 120, 105, 120, 110, 105, 125, 125, 110, 115, 108,
	  120, 115, 110, 110, 120, 110, 100, 125, 125},

	/* Halfling */
	{ 115, 110, 105,  95, 105, 110, 115, 130, 115, 105, 115,
	  125, 120, 120, 125, 115, 110, 120, 120, 120, 115, 115,
	  120, 110, 120, 120, 130, 110, 110, 130, 120 },

	/* Gnome */
	{ 115, 115, 110, 105,  95, 110, 115, 130, 115, 110, 115,
	  120, 125, 110, 120, 110, 105, 120, 110, 110, 105, 110,
	  120, 101, 110, 110, 120, 120, 115, 130, 110 },

	/* Dwarf */
	{ 115, 120, 120, 110, 110,	95, 125, 135, 115, 120, 115,
	  125, 140, 130, 130, 120, 115, 115, 115, 135, 125, 120,
	  120, 105, 115, 115, 115, 115, 120, 130, 125 },

	/* Half-Orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 115, 120, 125, 115, 120},

	/* Half-Troll */
	{ 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
	  110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
	  110, 115, 112, 112, 115, 112, 120, 110, 110 },

	/* Amberite (Dunedain)	*/
	{ 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
	  120, 120, 105, 120, 115, 105, 115, 120, 110, 105, 105,
	  120, 105, 120, 120, 125, 120, 105, 135, 120 },

	/* High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 125, 120, 100, 125, 120 },

	/* Human / Barbarian (copied from human) */
	{ 100, 105, 105, 110, 113, 115, 120, 125, 100, 105, 100,
	  124, 120, 110, 125, 115, 120, 120, 120, 120, 115, 120,
	  115, 105, 125, 125, 130, 125, 115, 120, 125 },

	/* Half-Ogre: theoretical, copied from half-troll */
	{ 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
	  110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
	  110, 115, 112, 112, 115, 112, 120, 110, 110 },

	/* Half-Giant: theoretical, copied from half-troll */
	{ 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
	  110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
	  110, 115, 112, 112, 115, 112, 130, 120, 110 },

	/* Half-Titan: theoretical, copied from High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 120, 120, 130, 130, 120   },

	/* Cyclops: theoretical, copied from half-troll */
	{ 110, 115, 115, 110, 110, 130, 110, 110, 110, 115, 110,
	  110, 115, 120, 110, 120, 120, 110, 110, 110, 115, 110,
	  110, 115, 112, 112, 115, 112, 130, 130, 110  },

	/* Yeek: theoretical, copied from Half-Orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 120  },

	/* Klackon: theoretical, copied from Gnome */
	{ 115, 115, 110, 105,  95, 110, 115, 130, 115, 110, 115,
	  120, 125, 110, 120, 110, 105, 120, 110, 110, 105, 110,
	  120, 101, 110, 110, 120, 120, 130, 130, 110  },

	/* Kobold: theoretical, copied from Half-Orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 110  },

	/* Nibelung: theoretical, copied from Dwarf */
	{ 115, 120, 120, 110, 110,	95, 125, 135, 115, 120, 115,
	  125, 140, 130, 130, 120, 115, 115, 115, 135, 125, 120,
	  120, 105, 115, 115, 120, 120, 130, 130, 115   },

	/* Dark Elf */
	{ 110, 110, 110, 115, 120, 130, 115, 115, 120, 110, 115,
	  115, 115, 116, 115, 120, 120, 115, 115, 101, 110, 110,
	  110, 110, 112, 122, 110, 110, 110, 115  },

	/* Draconian: theoretical, copied from High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 120, 120, 130, 130, 120  },

	/* Mind Flayer: theoretical, copied from High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 120, 120, 130, 130, 115   },

	/* Imp: theoretical, copied from High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 120, 120, 130, 130, 110   },

	/* Golem: theoretical, copied from High_Elf */
	{ 110, 105, 100, 105, 110, 120, 125, 130, 110, 100, 110,
	  125, 125, 101, 120, 115, 110, 115, 125, 110, 110, 110,
	  125, 115, 120, 120, 120, 120, 130, 130, 120 },

	/* Skeleton: theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 110   },

	/* Zombie: Theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 110   },

	/* Vampire: Theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 110   },

	/* Spectre: Theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 110   },

	/* Sprite: Theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 130   },

	/* Beastman: Theoretical, copied from half-orc */
	{ 115, 120, 125, 115, 115, 130, 110, 115, 115, 125, 115,
	  110, 110, 120, 110, 120, 125, 115, 115, 110, 120, 110,
	  115, 125, 120, 120, 120, 120, 130, 130, 115   },

	/* Ghoul  */
	{ 120, 125, 125, 120, 117, 120, 113, 110, 120, 125, 115,
	  112, 111, 120, 111, 125, 120, 116, 115, 110, 115, 105,
	  105, 120, 105, 105, 105, 105, 130, 115, 100   },
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
const byte extract_energy[200] =
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
const s32b player_exp[PY_MAX_LEVEL] =
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
		10,  90,
		14,  6,
		72,  6, 180, 25,
		66,  4, 150, 20,
		0,
		0x7FFFFFFF,
	},
	{
		"Elf",
		{ -1,  1,  0,  1, -2,  1 },
		4,  5,  5,  3, 7,  12, -6,  6,
		9,  125,
		24, 16,
		66,  9, 130, 15,
		62,  9, 100, 10,
		2,
		0x7FFDFF7F,
	},
	{
		"Dwarf",
		{  1, -2, 0, 0,  2, -1 },
		2,  9,  10,  -1,  7,  10, 7,  0,
		11,  125,
		35, 15,
		48,  3, 150, 10,
		46,  3, 120, 10,
		4,
		0x3FFF07FF,
	},
	{
		"Halfling",
		{ -2,  0,  0,  1,  -1,  1 },
		15, 10, 18, 5, 12,  15, -11, 6,
		10,  125,
		21, 12,
		36,  3, 60,  3,
		33,  3, 50,  3,
		3,
		0x7FFFFFF8,
	},
	{
		"Dark Elf",
		{ -1, 1, 0, 1, -1, 0 },
		5, 15, 20, 3, 8, 12, -5, 7,
		9, 125,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		4,
		0x7FFEFFBF,
	},
	{
		"Gnome",
		{ -2,  1,  1,  -1,  1, 0 },
		10, 12, 12,  3, 6,  13, -8, 0,
		8,  125,
		50, 40,
		42,  3, 90,  6,
		39,  3, 75,  3,
		3,
		0x7FFFBFFC,
	},
	{
		"Orc",
		{  2, -2,  0,  -1,  2, -1 },
		-3, -3, -3,  -1,  0, 7, 4, -3,
		12,  150,
		11,  4,
		66,  1, 150,  5,
		62,  1, 120,  5,
		2,
		0x3FFB06FF,
	},
	{
		"Troll",
		{ 4, -2, -2,  0,  3, -2 },
		-5, -8, -8, -2,  -1, 5, 10, -5,
		14,  150,
		20, 10,
		96, 10, 250, 50,
		84,  8, 225, 40,
		2,
		0x000003C3,
	},
	{
		"Saurian",
		{  1,  0,  0,  -2,  1,  0 },
		4,  5,  5, -2, 3, 13, 6, 0,
		13,  125,
		50, 50,
		70,  6, 250, 20,
		70,  5, 225, 15,
		0,
		0x7F7FEFC7,
	},
	{
		"Aquarian",
		{  0,  0,  1,  0,  -1,  0 },
		7,  1, 3,  4,  3, 14, 0, 0,
		9,  125,
		18, 3,
		90, 10, 190, 20,
		82, 10, 180, 15,
		0,
		0x7C1FF7FF,
	},
	{
		"Goblin",
		{ 0,  1,  -1,  0,  0, 0 },
		10, 10, 4, 3, 1, 17, 0, 2,
		11, 100,
		14, 8,
		60,  5, 200, 20,
		55,  6, 190, 15,
		0,
		0x6CE0FFDE,
	},
	{
		"Pixie",
		{ -2, 0, 1, 2, -2, 1 },
		4, 15, 7, 4, -1, 19, 0, 2,
		6,  125,
		40, 10,
		42, 10, 100, 6,
		30,  8, 99, 6,
		2,
		0x7FBFDFFC,
	},
	{
		"Kobold",
		{ -1, -1, 0, 1, 1, 0 },
		-2, -3, -2, 1, 1, 8, 8, -5,
		9, 125,
		11,  3,
		60,  1, 130,  5,
		55,  1, 100,  5,
		2,
		0x3FEF07FC,
	},
	{
		"Kaoti",
		{ 2, -2, -1, -1, 2, -4 },
		-5, -2, -1, -1, -1, 5, 9, 5,
		10, 125,
		17, 10,
		65,  15, 180, 45,
		61,  15, 150, 45,
		0,
		0x7FFBFEFF,
	},
	{
		"Illithid",
		{ -2, 2, 2, 0, -2, 0 },
		10, 12, 15, 2, 5, 12, -8, -5,
		9, 125,
		100, 25,
		68,  6, 142, 15,
		63,  6, 112, 10,
		3,
		0x7BA0FFF8,
	},
	{
		"Minotaur",
		{ 4, 0, -2, 0, 0, -2 },
		-2, 5, 3, 0, 1, 10, 9, 0,
		14, 150,
		15, 3,
		70,  3, 200,  6,
		70,  3, 175,  3,
		0,
		0x66FFF83F,
	},
	{
		"Centaur",
		{ 0, 0, 2, -2, 0, 0 },
		-3, 2, -1, 1, -1, 10, -1, 7,
		12, 150,
		20, 3,
		60,  3, 180,  4,
		54,  3, 170,  4,
		0,
		0x7F77FDC3,
	},
	{
		"Vampire",
		{ 1, 0, 0, 0, 0, -2 },
		4, 10, 10, 4, 1, 8, 5, 0,
		11, 100,
		100, 30,
		72,  6, 180, 25,
		66,  4, 150, 20,
		4,
		0x7FFFFFFF,
	},
	{
		"Catling",
		{ 0, 0, -2, 3, -1, 0 },
		3, 5, 10, 5, 7, 10, 5, 0,
		10, 125,
		21, 3,
		53,  9, 152,  6,
		50,  9, 148,  3,
		4,
		0x6EFFF83F,
	},
	{
		"Dryad",
		{ 0, 1, 0, -2, 0, 1 },
		5, 15, 20, 3, 8, 12, -5, 7,
		8, 125,
		75, 75,
		60,  4, 100,  6,
		54,  4, 80,  6,
		4,
		0x78AFFFFC,
	},
	{
		"Ogre",
		{ 3, -2, -2, -1, 3, -1 },
		-3, -5, -5, -2, -1, 5, 12, 0,
		15,  150,
		40, 10,
		92, 10, 255, 60,
		80,  8, 235, 60,
		2,
		0x6C00FFE0,
	},
	{
		"Faun",
		{ 0, 0, 0, 0, 0, 1 },
		10, 5, 15, 5, 5, 12, -8, -5,
		9, 125,
		60, 25,
		68,  6, 142, 15,
		63,  6, 112, 10,
		3,
		0x7C20FFDB,
	},
	{
		"Ent",
		{ 5, 0, 1, 0, 5, 1 },
		7, 12, 11, 5, 9, 10, 5, -15,
		14, 275,
		13,  4,
		200,  30, 450,  50,
		200,  30, 450,  50,
		2,
		0x5FFFFFE9,
	},
	{
		"Fairy",
		{ 0, 1, 1, 2, 0, 1 },
		5, 20, 20, 5, 5, 8, 0, 10,
		12, 200,
		100, 100,
		66,  1, 150,  6,
		62,  1, 150,  6,
		3,
		0x7C20FFDB,
	},
	{
		"Drider",
		{ 2, 2, 0, 2, 0, -1 },
		-5, 5, 5, -1, -1, 8, 8, 8,
		12, 175,
		100, 35,
		62,  6, 170, 5,
		56,  4, 160, 5,
		2,
		0x7FFEFFBF,
	},
	{
		"Uruk",
		{ 3, -1, -1, 0, 4, -2 },
		-5, -5, 8, -1, -1, 5, 30, 20,
		15, 175,
		100, 30,
		72, 6, 200, 25,
		66, 4, 200, 20,
		1,
		0x45F0007,
	},
	{
		"Yeek",
		{ -2, 1, 1, 1, -2, 0 },
		2, 4, 10, 3, 5, 15, -5, -5,
		7, 100,
		14, 3,
		50,  3, 90,  6,
		50,  3, 75,  3,
		1,
		0x7C20FFDB,
	},
	{
		"Ratling",
		{ 0, 0, 0, 1, 0, 0 },
		15, 5, 20, 7, 5, 14, -10, -3,
		7, 125,
		100, 30,
		50, 6, 100, 25,
		50, 4, 100, 20,
		4,
		0x3E0003C,    
	},
	{
		"Leprechaun",
		{ -2, 1, 1, 2, -2, 0 },
		10, 10, 10, 4, 10, 10, -8, 0,
		6, 125,
		50, 25,
		32,  2, 75,  2,
		29,  2, 65,  2,
		3,
		0x6290F83C,
	},
	{
		"Perthoron",
		{ 1, -1, -1, 2, -1, 0 },
		-5, -2, -1, -1, 20, 25, 9, 0,
		11, 125,
		14, 6,
		65,  6, 150, 20,
		61,  6, 120, 15,
		1,
		0x7FFFFFFF,
	},
	{
		"Draconian",
		{ 1, 1, 1, 1, 1, 1 },
		5, 5, 5, 5, 5, 5, 5, 5,
		12,  200,
		40, 10,
		92, 10, 190, 60,
		80,  8, 175, 60,
		2,
		0x7FFFFFFF,
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
		{ 3, -3, -3, 2, 2, 0},
		25, 18, 18, 1,  14, 2, 25, 17,
		12, 7,  10, 0,  0,  0,  100, 55,
		9,  0, 20
	},
	
	{
		"Berserker",
		{ 5, -3, -3, 1, 2, -1},
		25, 18, 18, 1,  14, 2, 30, 10,
		12, 7,  10, 0,  0,  0,  100, 55,
		9,  0, 20
	},
	
	{
		"Ninja",
		{ 2, -2, -2, 3, 1, -1},
		45, 32, 28, 5, 32, 24, 15, 20,
		15, 10, 10, 1,  0,  0, 75, 40,
		6, 25, 20
	},
	
	{
		"Rogue",
		{ 0, -1, -1, 3, 0, 0},
		45, 32, 28, 5, 32, 24, 15, 20,
		15, 10, 10, 1,  1,  0, 70, 40,
		6, 25, 20
	},
	
	{
		"Assassin",
		{ 1, -1, -1, 3, 0, -1},
		45, 32, 28, 5, 32, 24, 15, 20,
		15, 10, 10, 1,  0,  0, 70, 40,
		3, 25, 20
	},
	
	{
		"Thief-Mage",
		{ 0, 2, -1, 2, -1, 0},
		40, 30, 25, 5, 32, 24, 10, 10,
		15, 12, 10, 1,  0,  0, 50, 20,
		6, 25, 18
	},
	
	
	{
		"Druid",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},
	
	{
		"Necromancer",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},

	{
		"Priest",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},

	{
		"Shaman",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},

	{
		"Sage",
		{-1, -3, 3, -1, 0, 2},
		25, 30, 32, 2,  16, 8, 16, 7,
		7,  10, 12, 0,  0,  0, 50, 18,
		2, 20, 20
	},

	
	{
		"Fire Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},

	{
		"Water Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},

	{
		"Earth Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},

	{
		"Air Mage",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},

	{
		"Wizard",
		{-5, 3, 0, 1, -2, 1},
		30, 36, 30, 2,  16, 20, 10, 10,
		7,  13, 9,  0,  0,  0,  25, 14,
		0, 30, 15
	},


	{
		"Ranger",
		{ 0, -1, 1, 1, 0, 0},
		30, 20, 28, 1,  24, 16, 15, 20,
		8,  7, 10, 1,  0,  0,  50, 70,
		4, 35, 20
	},

	{
		"Dark Knight",
		{ 1, -1, 1, 0, 0, 0},
		20, 25, 25, 1,  14, 12, 20, 10,
		7,  11, 10, 0,  0,  0,  95, 35,
		6, 35, 20
	},

	{
		"Paladin",
		{ 1, -1, 1, 0, 0, 0},
		20, 24, 26, 1,  12, 14, 20, 10,
		7,  10, 11, 0,  0,  0, 80, 40,
		6, 35, 20
	},
	
	{
		"Chaos-Warrior",
		{ 1, -1, 1, 0, 0, 0},
		20, 25, 25, 1,  14, 12, 20, 10,
		7,  11, 10, 0,  0,  0,  75, 40,
		6, 35, 20
	},

	{
		"Warrior-Mage",
		{ 1, 1, -1, 0, 0, 0},
		30, 30, 28, 2,  18, 16, 20, 20,
		7,  10,  9, 0,  0,  0,  75, 50,
		5, 30, 20
	},

	{
		"Technician",
		{0, 1, 0, 0, 0, 0},
		30, 30, 25, 2,  16, 20, 25, 10,
		8,  13, 9,  0,  0,  0,  30, 15,
		2, 10, 15
	},
	
	{
		"Blacksmith",
		{1, 1, -2, 0, 1, 0},
		30, 20, 25, 1,  12, 15, 40, 10,
		8,  13, 9,  0,  0,  0,  85, 17,
		4, 10, 15
	},
	
	{
		"Tech-Thief",
		{-1, 1, -1, 2, 0, 0},
		30, 20, 25, 3,  20, 25, 25, 10,
		8,  13, 9,  1,  0,  0,  30, 20,
		1, 10, 15
	},
	
	{
		"Tech-Cleric",
		{-1, 1, 2, 0, -1, 0},
		30, 30, 25, 2,  17, 22, 30, 15,
		8,  13, 9,  0,  0,  0,  50, 15,
		1, 10, 15
	},
	
	{
		"Tech-Mage",
		{-1, 3, 0, 0, -1, 0},
		30, 35, 35, 2,  16, 20, 25, 10,
		8,  14, 10, 0,  0,  0,  30, 15,
		1, 10, 15
	},

	{
		"Archer",
		{ 2, -2, -2, 2, 1, 0},
		20, 25, 25, 2,  14, 12, 15, 15,
		7,  11, 10, 0,  0,  0,  35, 90,
		6, 15, 20
	},
	
	{
		"Mindcrafter",
		{-1, 0, 3, -1, -1, 2},   /* note: spell stat is Wis */
		30, 30, 30, 3,  22, 16, 15, 15,
		10, 10, 10, 0,   0,  0, 30, 20,
		2, 25, 20
	},
	
	{
		"Monk",
		{ 0, 0, 2, 0, 0, 0},
		45, 32, 28, 5, 32, 24, 12, 14,
		15, 11, 10, 0,  0,  0, 30, 25,
		6, 40, 20
	},

	{
		"Witch",
		{-1, 2, 2, -1, -1, 0},   
		30, 30, 30, 3,  22, 16, 15, 15,
		10, 10, 11, 0,   0,  0, 30, 20,
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
 *   Spell Book,
 *   Tech Type,
 *   Class First,
 *
 *   Spell Stat,
 *   Spell Type,
 *
 *   Spell Level,
 *   Spell Encumbrance,
 *
 *   Array of { Lev, Mana, Fail, Exp/Lev }
 *    The Array does not need to be there for all classes
 *    and a 0 represents an unusable realm for that class.
 *					´x
 */
player_magic magic_info[MAX_CLASS] =
{
	{
		/*** Warrior ***/

		0,
		2,
		12,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},
	
	{
		/*** Berserker ***/

		0,
		3,
		13,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},

	{
		/*** Ninja ***/
		TV_LIFE_BOOK,
		0,
		0,

		A_DEX,
		0,

		5,
		350,
		{	/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},
			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos */
			{0
			},

			/* Rogue Magic (Astral) */
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
			},
			/* Water */
			/* Earth */
			/* Wizardy */
			
		}
	},
	
	
	{
		/*** Rogue ***/
		TV_LIFE_BOOK,
		0,
		0,

		A_DEX,
		0,

		5,
		350,
		{	/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},
			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos */
			{0
			},

			/* Rogue Magic (Astral) */
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
			},
			/* Water */
			/* Earth */
			/* Wizardy */
			
		}
	},


	{
		/*** Assassin ***/
		TV_LIFE_BOOK,
		0,
		0,

		A_DEX,
		0,

		5,
		350,
		{	/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},
			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos */
			{0
			},

			/* Rogue Magic (Astral) */
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
			},
			/* Water */
			/* Earth */
			/* Wizardy */
			
		}
	},



	{
		/*** Thief-Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order */

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

			/* Fire Magic */

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

				/* Air Magic */

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
				{ 25, 25, 85, 100 },
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

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

			{0
			},
			
			{
				/* Water magic */

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
			{
				/* Earth magic */

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

			/* Wizardry Magic */

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

		}
	},
	

	{
		/*** Druid ***/

		TV_LIFE_BOOK,
		0,
		0,
		
		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{
				{ 1, 1, 10, 4 },
				{ 1, 1, 15, 4 },
				{ 1, 1, 20, 4 },
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

			/* Order Magic */
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

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{0
			},

			/* Chaos Magic */
			{
				{ 2, 2, 25, 3 },
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

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
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
		}
	},
	
	
	{
		/*** Necromancer ***/

		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{0
			},

			/* Order Magic */
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

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 1, 1, 25, 4 },
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

			/* Chaos Magic */
			{
				{ 2, 2, 25, 3 },
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

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
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
		}
	},
	

	{
		/*** Priest ***/

		TV_LIFE_BOOK,
		7,
		35,

		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{
				{ 2, 2, 10, 4 },
				{ 3, 2, 15, 4 },
				{ 4, 2, 20, 4 },
				{ 5, 2, 25, 1 },
				{ 6, 3, 27, 2 },
				{ 7, 4, 28, 2 },
				{ 8, 4, 32, 4 },
				{ 10, 5, 38, 4 },

				{ 9, 6, 38, 5 },
				{ 9, 6, 38, 4 },
				{ 10, 7, 40, 4 },
				{ 10, 8, 38, 4 },
				{ 11, 8, 40, 4 },
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

			/* Order Magic */
			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 24, 4 },
				{ 2, 3, 25, 1 },
				{ 3, 4, 30, 1 },
				{ 4, 5, 30, 1 },
				{ 6, 6, 35, 5 },
				{ 8, 7, 30, 4 },
				{ 10, 10, 75, 9 },

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

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{
				{ 2, 1, 25, 4 },
				{ 3, 2, 25, 4 },
				{ 4, 3, 25, 4 },
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

			/* Chaos Magic */
			{0
			},

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
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
		}
	},


	{
		/*** Shaman ***/

		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{
				{ 2, 2, 10, 4 },
				{ 3, 3, 15, 4 },
				{ 4, 3, 20, 4 },
				{ 5, 3, 25, 1 },
				{ 6, 4, 27, 2 },
				{ 7, 5, 28, 2 },
				{ 8, 5, 32, 4 },
				{ 9, 6, 38, 4 },

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

			/* Order Magic */
			{0
			},

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{
				{ 2, 2, 25, 4 },
				{ 3, 2, 25, 4 },
				{ 4, 3, 25, 4 },
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

			/* Chaos Magic */
			{
				{ 1, 1, 25, 3 },
				{ 1, 1, 25, 4 },
				{ 2, 3, 37, 8 },
				{ 5, 4, 40, 8 },
				{ 7, 7, 20, 4 },
				{ 9, 9, 30, 6 },
				{ 12, 12, 30, 6 },
				{ 15, 15, 30, 5 },

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

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
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
		}
	},

	{
		/*** Sage ***/

		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{
				{ 2, 2, 10, 4 },
				{ 3, 3, 15, 4 },
				{ 4, 3, 20, 4 },
				{ 5, 3, 25, 1 },
				{ 6, 4, 27, 2 },
				{ 7, 5, 28, 2 },
				{ 8, 5, 32, 4 },
				{ 9, 6, 38, 4 },

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

			/* Order Magic */
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

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{
				{ 2, 1, 25, 4 },
				{ 3, 2, 25, 4 },
				{ 4, 3, 25, 4 },
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

			/* Chaos Magic */
			{
				{ 2, 2, 25, 3 },
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

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 24, 4 },
				{ 3, 3, 25, 1 },
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
		}
	},


	{
		/*** Fire Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */

			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 25, 3 },
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

				/* Air Magic */

			{
				{ 1, 1, 20, 4 },
				{ 2, 2, 22, 4 },
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

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

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
			},
			{ 0	/* Water magic */
			},
			{
				/* Earth magic */

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

			/* Wizardry Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

		}
	},

	{
		/*** Water Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */

			{0
			},

				/* Air Magic */

			{
				{ 1, 1, 20, 4 },
				{ 2, 2, 22, 4 },
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

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

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
			},
			{
				/* Water magic */

				{ 1, 1, 30, 4 },
				{ 1, 1, 35, 4 },
				{ 3, 3, 35, 4 },
				{ 4, 4, 35, 4 },
				{ 6, 6, 35, 4 },
				{ 8, 7, 40, 4 },
				{ 10, 9, 40, 3 },
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
			{
				/* Earth magic */

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

			/* Wizardry Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

		}
	},

	{
		/*** Earth Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */

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

				/* Air Magic */

			{0
			},

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

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
			},
			{
				/* Water magic */

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
			{
				/* Earth magic */

				{ 1, 1, 30, 4 },
				{ 1, 1, 35, 4 },
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

			/* Wizardry Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

		}
	},

	{
		/*** Air Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */

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

				/* Air Magic */

			{
				{ 1, 1, 20, 4 },
				{ 1, 1, 22, 4 },
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
				{ 25, 25, 85, 100 },
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

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

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
			},
			{
				/* Water magic */

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
			{0	/* Earth magic */
			},

			/* Wizardry Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

		}
	},

	{
		/*** Wizard ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			{
				/* Life magic */

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

			/* Order Magic */

			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */

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

				/* Air Magic */

			{
				{ 1, 1, 20, 4 },
				{ 2, 2, 22, 4 },
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

				/* Death Magic */

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

				/* Chaos Magic */

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

				/* Astral Magic */

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
			},
			{
				/* Water magic */

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
			/* Earth magic */
			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Wizardry Magic */

			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 24, 4 },
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

		}
	},



	{
		/*** Ranger ***/

		TV_LIFE_BOOK,
		4,
		14,

		A_WIS,
		0,

		3,
		400,

		{
			{	/* Life */
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
			/* Order */
			/* Fire */
			/* Air */
			/* Death */
			/* Chaos */
			/* Astral */
			/* Water */
			/* Earth */
			/* Wizardry */
		}
	},
	
	
	{
		/*** Dark Knight ***/
		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		0,

		2,
		400,

		{	/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},

			/* Air */
			{0
			},
			/* Death */
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
			/* Chaos magic */
			{0
			},
			/* Astral */
			/* Water */
			/* Earth */
			/* Wizardry */
			
		}
	},
	

	{
		/*** Paladin ***/

		TV_LIFE_BOOK,
		8,
		36,

		A_WIS,
		1,

		1,
		400,
		{
			/* Life */
			{0
			},
			/* Order Magic */
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
			/* Fire */
			/* Air */
			/* Death */
			/* Chaos */
			/* Astral */
			/* Water */
			/* Earth */
			/* Wizardry */
		}
	},
	
	
	{
		/*** Chaos Warrior ***/
		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		0,

		2,
		400,

		{	/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},

			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos magic */
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
			/* Astral */
			/* Water */
			/* Earth */
			/* Wizardry */
			
		}
	},


	{
		/*** Warrior-Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		350,

		{
			/* Life Magic */
			{0
			},

			/* Order Magic */
			{0
			},

			/* Fire Magic */
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

			/* Air Magic */
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

			/* Death Magic */
			{0
			},

			/* Chaos Magic */
			{0
			},

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
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
			
			/* Earth Magic */
			
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
			
			/* Wizardry Magic */
			
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
		}
	},
	

	{
		/*** Technician ***/

		0,
		5,
		15,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},
	
	
	{
		/*** Tech Warrior ***/

		0,
		5,
		15,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},
	
	
	{
		/*** Tech Thief ***/

		0,
		5,
		15,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},
	
	
	{
		/*** Tech Cleric ***/

		TV_LIFE_BOOK,
		5,
		15,

		A_WIS,
		1,

		1,
		350,
		{
			/* Life Magic */
			{
				{ 1, 1, 10, 4 },
				{ 3, 2, 15, 4 },
				{ 4, 3, 20, 4 },
				{ 5, 3, 25, 1 },
				{ 6, 4, 27, 2 },
				{ 7, 5, 28, 2 },
				{ 8, 5, 32, 4 },
				{ 9, 6, 38, 4 },

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

			/* Order Magic */
			{
				{ 1, 1, 23, 4 },
				{ 2, 2, 24, 4 },
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

			/* Fire Magic */
			{0
			},

			/* Air Magic */
			{0
			},

			/* Death Magic */
			{
				{ 1, 1, 25, 4 },
				{ 2, 2, 25, 4 },
				{ 4, 3, 25, 4 },
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

			/* Chaos Magic */
			{
				{ 1, 1, 25, 3 },
				{ 2, 1, 25, 4 },
				{ 4, 4, 37, 8 },
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

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
			{0
			},
			
			/* Earth Magic */
			
			{0
			},
			
			/* Wizardry Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 2, 1, 24, 4 },
				{ 3, 3, 25, 1 },
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
		}
	},
	
		
	{
		/*** Tech Mage ***/

		TV_ORDER_BOOK,
		5,
		15,

		A_INT,
		0,

		1,
		350,
		{
			/* Life Magic */
			{0
			},

			/* Order Magic */
			{0
			},

			/* Fire Magic */
			{
				{ 1, 1, 25, 4 },
				{ 2, 1, 25, 4 },
				{ 3, 4, 25, 1 },
				{ 4, 5, 35, 4 },
				{ 5, 5, 50, 5 },
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

			/* Air Magic */
			{
				{ 1, 1, 22, 4 },
				{ 2, 1, 24, 4 },
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

			/* Death Magic */
			{0
			},

			/* Chaos Magic */
			{0
			},

			/* Astral Magic */
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
			},
			
			/* Water Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 2, 1, 24, 4 },
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
			
			/* Earth Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 2, 1, 24, 4 },
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
			
			/* Wizardry Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 2, 1, 24, 4 },
				{ 3, 3, 25, 1 },
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
		}
	},
	
	
	{
		/*** Archer ***/

		0,
		4,
		14,

		A_STR,
		0,

		99,
		0,
		/* No magic */
	},
	
	
	{
		/*** Mindcrafter ***/

		TV_LIFE_BOOK,
		1,
		0,

		A_WIS,
		0,

		99,
		300,
		{0
			/* No Magic */
		},
	},

	{
		/*** Monk ***/

		TV_LIFE_BOOK,
		0,
		0,

		A_WIS,
		0,

		1,
		300,

		{
			/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},
			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos */
			{0
			},

			/* Astral Magic */
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
			},
			/* Water */
			/* Earth */
			/* Wizardy */
			
		}
	},

	{
		/*** Witch ***/

		TV_ORDER_BOOK,
		6,
		30,

		A_INT,
		0,

		1,
		300,

		{
			/* Life */
			{0
			},
			/* Order */
			{0
			},
			/* Fire */
			{0
			},
			/* Air */
			{0
			},
			/* Death */
			{0
			},
			/* Chaos */
			{0
			},

			/* Astral Magic */
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
				{ 50, 100, 80, 200 }
			},
			/* Water */
			/* Earth */
			/* Wizardy */
			
		}
	},


	{
		/*** High Mage ***/

		TV_ORDER_BOOK,
		0,
		0,

		A_INT,
		0,

		1,
		300,

		{
			/* High Mage: Life Magic */
			{
				{  1,  1, 20,   4 },
				{  1,  1, 25,   4 },
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
				{ 1, 1, 15, 3 },
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
				{ 1, 2, 20, 4 },
				{ 3, 4, 32, 8 },
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
			},
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
			/* Earth Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 24, 4 },
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
			
			/* Wizardry Magic */
			
			{
				{ 1, 1, 23, 4 },
				{ 1, 1, 24, 4 },
				{ 3, 3, 25, 1 },
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
		}
	},
};


/*
 * Zangband uses this array instead of the spell flags table, as there
 * are 5 realms of magic, each with 4 spellbooks and 8 spells per book -- TY
 */
const u32b fake_spell_flags[4]=
{
	0x000000ff,
	0x0000ff00,
	0x00ff0000,
	0xff000000
};


const u16b realm_choices1[] =
{
	(CH_NONE),				/* Warrior */
	(CH_NONE),				/* Berserker */
	(CH_ASTRAL),                            /* Ninja */
	(CH_ASTRAL),                            /* Rogue */
	(CH_ASTRAL),                            /* Assassin */
	(CH_EARTH | CH_ORDER | CH_FIRE |
	 CH_AIR | CH_DEATH | CH_CHAOS |
	 CH_WATER | CH_LIFE | CH_WIZARD),       /* Thief Mage */
	(CH_LIFE ),         		        /* Druid */
	(CH_DEATH),		                /* Necromancer */
	(CH_ORDER),		                /* Priest */
	(CH_CHAOS),   		                /* Shaman */
	(CH_WIZARD), 		                /* Sage */
	(CH_FIRE ),       			/* Fire Mage */ 
	(CH_WATER ),       			/* Water Mage */ 
	(CH_EARTH ),       			/* Earth Mage */ 
	(CH_AIR ),       			/* Air Mage */ 
	(CH_WIZARD ),       			/* Wizard */
	(CH_LIFE),          			/* Ranger */
	(CH_DEATH),                             /* Dark Knight */
	(CH_ORDER),		                /* Paladin */
	(CH_CHAOS),                             /* Chaos-Warrior */
	(CH_FIRE | CH_AIR | CH_ASTRAL |
	CH_WATER | CH_EARTH | CH_WIZARD),       /* Warrior-Mage */
	(CH_NONE),				/* Tech */
	(CH_NONE),				/* Tech Warrior */
	(CH_NONE),				/* Tech Thief */
	(CH_ORDER | CH_DEATH | CH_CHAOS |
	 CH_LIFE | CH_ASTRAL | CH_WIZARD),      /* Tech Cleric */
	 (CH_EARTH | CH_FIRE | CH_AIR |
	 CH_WATER | CH_ASTRAL | CH_WIZARD),     /* Tech Mage */	
	(CH_NONE),                              /* Archer */
	(CH_NONE),                              /* Mindcrafter */
	(CH_ASTRAL),         			/* Monk */
	(CH_ASTRAL),                            /* Witch */
	(CH_LIFE | CH_ORDER | CH_FIRE |
	 CH_AIR | CH_DEATH | CH_CHAOS |
	 CH_ASTRAL | CH_WATER | CH_EARTH |
	 CH_WIZARD),      			/* High-Mage */
};


const u16b realm_choices2[] =
{
	(CH_NONE),                              /* Warrior */
	(CH_NONE),				/* Berserker */
	(CH_NONE),				/* Ninja */
	(CH_NONE),                              /* Rogue */
	(CH_NONE),				/* Assassin */
	(CH_NONE),				/* Thief-Mage */
	(CH_ORDER | CH_FIRE | CH_AIR | CH_CHAOS |
	 CH_ASTRAL | CH_WATER | CH_EARTH | CH_WIZARD),      /* Druid */
	(CH_ORDER | CH_FIRE | CH_AIR | CH_CHAOS |
	 CH_ASTRAL | CH_WATER | CH_EARTH | CH_WIZARD),      /* Necromancer */
	(CH_LIFE | CH_FIRE | CH_AIR | CH_DEATH |
	 CH_ASTRAL | CH_WATER | CH_EARTH | CH_WIZARD),      /* Priest */
	(CH_LIFE | CH_FIRE | CH_AIR | CH_DEATH |
	 CH_ASTRAL | CH_WATER | CH_EARTH | CH_WIZARD),      /* Shaman */
	(CH_LIFE | CH_ORDER | CH_FIRE |
	 CH_AIR | CH_DEATH | CH_CHAOS |
	 CH_ASTRAL | CH_WATER | CH_EARTH),      /* Sage */
	(CH_LIFE | CH_ORDER | CH_AIR | 
	 CH_DEATH | CH_CHAOS | CH_ASTRAL | 
	 CH_EARTH | CH_WIZARD),   		/* Fire Mage */
	(CH_LIFE | CH_ORDER | CH_AIR | 
	 CH_DEATH | CH_CHAOS | CH_ASTRAL |
	 CH_EARTH | CH_WIZARD),      		/* Water Mage */
	(CH_LIFE | CH_ORDER | CH_FIRE | 
	 CH_DEATH | CH_CHAOS | CH_ASTRAL | 
	 CH_WATER |CH_WIZARD),      		/* Earth Mage */
	(CH_LIFE | CH_ORDER | CH_FIRE | 
	 CH_DEATH | CH_CHAOS | CH_ASTRAL | 
	 CH_WATER | CH_WIZARD),     		/* Air Mage */
	(CH_LIFE | CH_ORDER | CH_FIRE |
	 CH_AIR | CH_DEATH | CH_CHAOS |
	 CH_ASTRAL | CH_WATER | CH_EARTH),      /* Wizard */
	(CH_NONE),                              /* Ranger */
	(CH_NONE),                              /* Dark Knight */
	(CH_NONE),                              /* Paladin */
	(CH_NONE),                              /* Chaos-Warrior */
	(CH_NONE),       			/* Warrior-Mage */
	(CH_NONE),       			/* Tech */
	(CH_NONE),       			/* Tech Warrior */
	(CH_NONE),       			/* Tech Thief */
	(CH_NONE),       			/* Tech Cleric */
	(CH_NONE),       			/* Tech Mage */
	(CH_NONE),                              /* Archer */
	(CH_NONE),                              /* Mindcrafter */
	(CH_NONE),                              /* Monk */
	(CH_NONE),                              /* Witch */
	(CH_NONE),                              /* High-Mage */
};


cptr realm_names[] =
{
	"no magic",
	"Life",
	"Order",
	"Fire",
	"Air",
	"Death",
	"Chaos",
	"Astral",
	"Water",
	"Earth",
	"Wizard",
	"unknown"
};


/*
 * Names of the spells (mage spells then priest spells)
 */
cptr spell_names[10][32] =
{
	/*** Life Spells ***/
	{
		/* Common Life Spellbooks */
		/* Mix of ZAngband life and nature common spells */
		"Detect Life",
		"Holy Smite",
		"Cure Light Wounds",
		"Bless",
		"Daylight",
		"Grow Food",
		"Remove Fear",
		"Cure",

		"Remove Curse",
		"Tame Animal",
		"Nature's Awareness",
		"Holy Orb",
		"Sunray",
		"Protection from Evil",
		"Healing",
		"Sanctuary",

		/* Rare Life Spellbooks */
		/* ZAngband rare nature spells */
		"Summon Animals",
		"Create Doors",
		"Create Stairs",
		"Resistance True",
		"Animal Friendship",
		"Call Sunlight",
		"Rustproof",
		"Nature's Wrath",
		
		/* ZAngband rare life spells */
		"Exorcism",
		"Heroism",
		"Shadow Bane",
		"Banish",
		"Bless Weapon",
		"Mega Heal",
		"Divine Intervention",
		"Holy Invulnerability"
	},

	/*** Order Spells ***/
	/* Based on Sorcery */

	{
		/* Common Order Spellbooks */
		"Phase Door",
		"Justice",
		"Detect Doors and Traps",
		"Light Area",
		"Identify",
		"Controlled Shift",
		"Sleep Monster",
		"Recharging",

		"Magic Mapping",
		"Slow Monster",
		"Fire And Ice",
		"Mass Sleep",
		"Haste Self",
		"Inertia Bolt",
		"Detection True",
		"Greater Identify",

		/* Rare Order Spellbooks */
		"Detect Objects and Treasure",
		"Detect Enchantment",
		"Resist Elements",
		"Sense Minds",
		"Cold Flame",
		"Self Knowledge",
		"Inertia Wave",
		"Word of Recall",

		"Stasis",
		"Telekinesis",
		"Explosive Rune",
		"Clairvoyance",
		"Enchant Weapon",
		"Enchant Armour",
		"Alchemy",
		"Pattern Sign"
	},

	/*** Fire Spellbooks ***/

	{
		/* Common Fire Spellbooks */
		"Resist Fire",
		"Ignite",
		"Firesight",
		"Globe of Flame",
		"Fire Light",
		"Fire Blast",
		"Destroy Doors",
		"Fire Bolt",

		"Warm Aura",
		"Flame Thrower",
		"Refuel",
		"Engulf",
		"Recharge",
		"Explode",
		"Warmth",
		"Wildfire",

		/* Rare Fire Spellbooks */
		"Phoenix Shield",
		"Fire Ball",
		"Flame Animation",
		"Heat Wave",
		"Phoenix Tear",
		"Scorching Beam",
		"Solar Flare",
		"Dragon's Breath",

		"Volcanic Forge",
		"Lava Bolt",
		"Lava Armour",
		"Fire Storm",
		"Melt",
		"Lava Flow",
		"Fire Brand",
		"Volcano"
	},

	/*** Air Spells ***/

	{
		/* Common Air Spellbooks */
		"Resist Lightning",
		"Zap",
		"Call light",
		"Wind Blast",
		"Speed",
		"Light Beam",
		"Air Shift",
		"Lightning Bolt",

		"Fly",
		"Shock Ball",
		"Free Action",
		"Whirlwind",
		"Lightning Shift",
		"Fork Lightning",
		"Air Detect",
		"Minor Storm",

		/* Rare Air Spellbooks */
		"Greater Speed",
		"AntiGravity",
		"Essence of Air",
		"Sonic Boom",
		"Restore Mind",
		"Tornado",
		"Aether Shift",
		"Cyclone",

		"Lightning Shield",
		"Ball Lightning",
		"Recharge",
		"Storm",
		"Storm Lore",
		"Chain Lightning",
		"Storm Brand",
		"Mega Storm"
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
		"Steal Soul",

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
		"Necropotence",
		"Wraithform"
	},

	/* Chaos Spellbooks */
	/* Chaos and Trump mix */
	{
		/* Common Chaos Spellbooks */
		"Blink",
		"Crazy Bullet",
		"Shuffle",
		"Weird Light",
		"Minor Trump",
		"Confusion",
		"Trump Spying",
		"Hand of Fate",

		"Chaos Bolt",
		"Trump",
		"Doom Bolt",
		"Conjure Elemental",
		"Teleport Other",
		"Teleport Level",
		"Invoke Logrus",
		"Banish",

		/* Rare Chaos Spellbooks */
		/* All Trump */
		"Joker Card",
		"Trump Divination",
		"Trump Branding",
		"Trump Lore",
		"Creature Trump",
		"Living Trump",
		"Trump Undead",
		"Trump Dragon",

		/* All Logrus */
		"Morph Other",
		"Call Chaos",
		"Fortean Flicker",
		"Drink Logrus",
		"Infinity Bolt",
		"Breathe Logrus",
		"Chaos Branding",
		"Call The Void"
	},

	/* Astral Spellbooks (_only_ common spells) */

	{
		"Magic Lock",
		"Detect Invisible",
		"Phase Door",
		"Light",
		"Disarm",
		"Detect Life",
		"Detect Unlife",
		"Magic Missile",

		"Detect Treasure",
		"First Aid",
		"Cure Light Wounds",
		"Blink",
		"Detect Portals",
		"Detect Monsters",
		"Refuel",
		"Luck",

		"Multi Missile",
		"Resist Cold",
		"Resist Fire",
		"Teleport",
		"Resist Lightning",
		"Resist Acid",
		"Identify",
		"Ray of Light",

		"Stone to Mud",
		"See Invisible",
		"Satisfy Hunger",
		"Detect Objects",
		"Word of Recall",
		"Force Bolt",
		"Aura of Fire",
		"SEER"
	},
	/* Water */

	{
		"Resist Cold",
		"Chill",
		"Resist Acid",
		"Touch of Acid",
		"Water Slip",
		"Rime Circle",
		"Detect Creatures",
		"Hydroblast",

		"Quench",
		"Frost Bolt",
		"Dew Drop",
		"Holy Water",
		"Ice Shield",
		"Acid Spray",
		"Healing Waters",
		"Snowfall",

		"Wave Speed",
		"Acidic Flow",
		"Aquarius Shift",
		"Quicksilver",
		"Summon Wave",
		"Acid Rain",
		"Acid Brand",
		"Tidal Wave",

		"Ice Lore",
		"Freeze",
		"Restore Soul",
		"Snowflake",
		"Ice Brand",
		"Freezing Sphere",
		"Ice Air",
		"Arctic Blast"
	}, 
	/* Earth */

	{
		"Shield",
		"Slow",
		"Detect Gold",
		"Rock Bolt",
		"Sense Invisible",
		"Stun Bolt",
		"Dig",
		"Increase Gravity",

		"Grounding",
		"Gravity Blast",
		"Armour",
		"Multi-stun",
		"Detect Walls",
		"Gravity Beam",
		"Stun Wave",
		"Greater Detection",

		"Remove Traps",
		"Tremor",
		"Tunnel",
		"Earthquake",
		"Passage",
		"Greater Quake",
		"Earthquake Brand",
		"Geoforce",

		"Gem Bolt",
		"Stone Skin",
		"Crystal Vortex",
		"Improve Armour",
		"Crush",
		"Restore Body",
		"Rain of Shards",
		"Crystal Skin"
	}, 
	/* Wizard */

	{
		"Infravision",
		"Mana Bolt",
		"Light",
		"Blink",
		"Detect Traps+Doors",
		"Remove Fear",
		"Detect Danger",
		"Slow Poison",

		"Teleport",
		"Haste",
		"See Invisible",
		"Dispell",
		"Slowing Orb",
		"Magic Report",
		"Cure Medium Wounds",
		"Mantle",

		"Disintegrate",
		"Protection From Evil",
		"Teleport Other",
		"Dimensional Door",
		"Summon Monster",
		"Hold Monster",
		"Disintergration Wave",
		"Recharge",

		"Aura of Energy",
		"Magic Meteorite",
		"Word of Recall",
		"Greater Identify",
		"Meteorite Storm",
		"Restoration",
		"Cosmic Wrath",
		"Star Shield"
	} 
};


/*
 * A description of all spells.(Shown by browse)
 */
cptr spell_xtra[10][32] =
{
	/*** Life Spells ***/
	{
		/* Common Life Spellbooks */
		"Detect the life energy of monsters",
		"A small burst of holy energy",
		"A small amount of healing",
		"A small boost to help you survive combat.",
		"The light of day to aid you.",
		"Grows a bush full of tasty berries.",
		"Overcome your fears.",
		"Cures all poison and heals a bit.",

		"Removes light curses from anything you are wielding.",
		"Make an animal a friend.",
		"See what Nature sees in the area.",
		"A blast of Holy Fire.",
		"A powerful ray of sunlight.",
		"Repels evil creatures",
		"Heals 300 points, stops bleeding and stun.",
		"Grows plants all around you as a shield.",

		/* Rare Life Spellbooks */
		"Calls upon some animals to aid you.",
		"Take a guess.",
		"A quick way up or down.",
		"Resistance to all elemantal and poison damage.",
		"Charms all the animals in the area.",
		"Sheds a little light on the area.",
		"Makes an item resist corrosion.",
		"Causes everything in the area to come apart.",

		"Hurts Undead and Demons. Scares evil.",
		"Improve combat and instills courage.",
		"Smashes Undead and Demons",
		"Cause evil to disappear",
		"The blessing of the gods on a weapon.",
		"Restores *EVERYTHING*.",
		"A Holy storm, a Blessing and an Angel.",
		"Invulnerability + Protection from Evil."
	},

	/*** Order Spells ***/
	{
		/* Common Order Spellbooks */
		"A spell to get behind a door or wall.",
		"Deals damage that you've been dealt.",
		"Detects Doors and Traps.",
		"Lights the area.",
		"Basic knowledge of an Item.",
		"A teleport that you control.",
		"Puts an enemy to sleep.",
		"Recharge a magic item.",

		"Shows the walls.",
		"Slows a monster.",
		"A bolt that burns and freezes.",
		"Snooze the whole area.",
		"Makes you faster.",
		"Slows and hurts.",
		"Detect lots of things.",
		"Complete knowledge of an item.",

		/* Rare Order Spellbooks */
		"Detects objects and treasure.",
		"Detects magical items.",
		"Protects from elemental dammage.",
		"Grants telepathy.",
		"An explosion of heat and cold.",
		"Shows all Virtues, Resistances and Abilities.",
		"A wave of slowing and damage.",
		"Read a help file.",

		"Holds any monster.",
		"Picks up an item from a distance.",
		"Places an explosive rune on the ground.",
		"Show the area and telepathy.",
		"Improves a weapon.",
		"Improves an armour.",
		"Turns an item into gold.",
		"Brands a weapon with a Pattern piece."
	},

	/*** Fire Spellbooks ***/

	{
		/* Common Fire Spellbooks */
		"Hmmm... I wonder.",
		"Causes the target to burn.",
		"Shows invisible monsters.",
		"Flames burn all around you.",
		"Lights the area.",
		"A small but powerful explosion.",
		"Destroys doors.",
		"A Tennis ball sized flaming projectile.",

		"Warms any cold about to damage you.",
		"A ray of fire.",
		"Refills a light item.",
		"Fire raging around you.",
		"Recharges a magic item.",
		"A red ball that explodes on impact.",
		"A warm glow inside that heals.",
		"Several out of control bursts of fire.",

		/* Rare Fire Spellbooks */
		"Surrounds you with fire. Burning all who touch you.",
		"A Basketball sized flaming projectile.",
		"Summons Fire elementals.",
		"A searing wave of heat.",
		"Heals and restores life levels.",
		"A scorching beam of Plasma.",
		"Detection and hurts Undead.",
		"A really powerful fire breath.",

		"Improves a weapon.",
		"A hot blob a lava.",
		"An armour boost.",
		"Fire raining down on the area.",
		"Melts nearby nonliving objects.",
		"A beam of lava.",
		"Causes a weapon to start flaming.",
		"Makes a Volcano erupt under you."
	},

	/*** Air Spells ***/

	{
		/* Common Air Spellbooks */
		"Hmmm... I wonder.",
		"A small spark of electricity.",
		"Lights the area.",
		"A gust of wind to knock enemies over.",
		"Improves your speed.",
		"A beam of light.",
		"Blink.",
		"A bolt of electricity.",

		"Lets you fly.",
		"Zaps a large area.",
		"Protects you from holding.",
		"A circular wind blast.",
		"Teleport.",
		"A lightning bolt that splits to hit the next target along.",
		"Doors, Stairs and creatures.",
		"An eight-way lightning bolt.",

		/* Rare Air Spellbooks */
		"Improves speed for longer.",
		"Causes nearby monsters to fall to the ceiling",
		"Speed, Flying and Free Action.",
		"A really LOUD sound.",
		"Restores intelligence and wisdom.",
		"Powerful wind that attacks enemies.",
		"Word of Recall.",
		"A wind storm that does lots of damage.",

		"A ring of lightning around you that zappes when touched.",
		"A large ball of lightning.",
		"Recharge a magic item.",
		"An eight-way lightning blast.",
		"*Identify*.",
		"A wave of powerful electricity.",
		"Makes a weapon shock when it hits.",
		"A very powerful version of Storm."
	},

	/*** Death Spells ***/

	{
		/* Common Death Spellbooks */
		"Detects nonliving monsters.",
		"An Evil bolt of magic.",
		"Detects evil monsters.",
		"A cloud of poisonous gas.",
		"Makes a target fall asleep.",
		"Hmmm... I wonder.",
		"Scares a target.",
		"Gives control of an undead creature.",

		"Anything in the target area decays.",
		"A bolt of Nether.",
		"Scares all monsters in the area.",
		"Drains blood out of the victim.",
		"Coats a weapon in a deadly poison.",
		"Blasts all good monsters in the area.",
		"Kills all monsters of the chosen type.",
		"Steals life levels from the target and give them to you.",

		/* Rare Death Spellbooks */
		"Improves attack, decreases defence and stops fear.",
		"Calls upon the dead to cast a spell to aid you.",
		"A powerful bolt of darkness.",
		"Berserk and Haste.",
		"3 bolts of life draining.",
		"Makes a weapon work like a Vampire.",
		"An intense storm of darkness.",
		"Genocides everything.",

		"Slaying magic kills the target.",
		"Raises the dead.",
		"Identify.",
		"Slaying magic hits all living things in the area.",
		"Obliterates monsters.",
		"Floods the chosen area with Hellfire.",
		"Kills monsters and lets you use their souls as mana.",
		"Lets you walk through walls."
	},

	/* Chaos Spellbooks */
	{
		/* Common Chaos Spellbooks */
		"Blink.",
		"Uncontrolled magic missiles.",
		"Shuffles a tarot deck.",
		"A random colour of light.",
		"A small selection of random book 1 spells.",
		"Confuses the enemy.",
		"Telepathy",
		"Calls upon the hand of fate to strike down an enemy.",

		"A bolt of pure chaos.",
		"A small selection of random book 2 spells.",
		"A powerful mana blast.",
		"Summons a random elemental.",
		"Teleports monsters.",
		"A quick escape from the level.",
		"A blast of pure chaos.",
		"Banishes monsters to a random plane.",

		/* Rare Chaos Spellbooks */
		"Summon a weird monster",
		"Detects all.",
		"Trump Branding",
		"Makes a weapon unstable.",
		"Summons a random creature type.",
		"Gives a teleport mutation.",
		"Summon Undead",
		"Summon Dragon",

		"Polymorphs the enemies.",
		"A random damage effect.",
		"Rips reality.",
		"Polymorph self.",
		"A powerful damage spell that can continue to infinity.",
		"A wave of anarchy.",
		"Dips a weapon in raw Logrus.",
		"Rockets, Mana Storms and Nukes."
	},

	/* Astral Spellbooks (_only_ common spells) */

	{
		"Locks a door.",
		"Shows invisible monsters.",
		"Get behind a door.",
		"Lights the area.",
		"Disarms traps.",
		"Detects living creatures.",
		"Detects nonliving creatures.",
		"Magic Missile.",

		"Detects money.",
		"Stops bleeding.",
		"Cures Light Wounds.",
		"Blink.",
		"Detects doors and stairs.",
		"Detect all monsters.",
		"Refuels a light source.",
		"Blesses you.",

		"Magic missiles anyone in the area.",
		"Resists Cold.",
		"Resists Fire.",
		"Teleports you at random.",
		"Resists Lightning.",
		"Resists Acid.",
		"Identify.",
		"Ray of Light.",

		"Stone to Mud.",
		"Gives you the ability to see invisible monsters.",
		"Satisfies Hunger.",
		"Detects objects.",
		"Word of Recall.",
		"A force blast like being hit with a big fist.",
		"Burns anyone that touches you.",
		"Shows area and gives telepathy."
	},
	/* Water */

	{
		"Hmmm... I wonder.",
		"A small bolt of coldness.",
		"Hmmm... I wonder.",
		"A small amount of acid hits the target.",
		"Blink.",
		"Covers the area in a blanket of frost.",
		"Detects creatures.",
		"A blast of water.",

		"Cools fire damage about to hit you.",
		"A bolt of icy shards.",
		"Heals light wounds.",
		"Damages undead.",
		"A small defence bonus.",
		"Sprays acid in the chosen direction.",
		"Cures medium wounds.",
		"Covers an area in a snow drift.",

		"Grants the speed of water.",
		"A stream of acid.",
		"Word of Recall.",
		"The speed and blessing of water.",
		"Creates a wave of water.",
		"Cause it to rain acid in an area.",
		"Covers a weapon in acid.",
		"A huge wave rams and the drowns enemies.",

		"*Identify*.",
		"Attempts to freeze the target in a block of ice.",
		"Restores charisma and life levels.",
		"Makes ice form a giant snow flake.",
		"Makes a weapon icy cold.",
		"Freezes everything in the area.",
		"Chills the air so much that monsters slow. Also detects all.",
		"A powerful storm of razor sharp ice."
	}, 
	/* Earth */

	{
		"A minor defence spell.",
		"Slows a target.",
		"Detects gold.",
		"Throws a hard rock at the target.",
		"Senses vibrations in the earth caused by invisible monsters.",
		"Stuns the target.",
		"Digs throw rock.",
		"Increases gravity, slowing monsters.",

		"Ground lightning that will hurt you.",
		"A bolt of gravity.",
		"Hardens you armour.",
		"Stuns multiple enemies.",
		"Maps the area.",
		"A beam of increased gravity.",
		"A large wave of stunning magic.",
		"Detects everything.",

		"Destroys traps in the area.",
		"Creates a small earthquake towards the target.",
		"Digs through thicker walls.",
		"Creates an earthquake centered on you.",
		"Word of Recall.",
		"Causes an earthquake where you choose.",
		"Causes a weapon to have a heavy impact.",
		"Stuns everyone in the area and strengthens armour.",

		"Throws a razor sharp crystal.",
		"Turns your skin to stone.",
		"Creates a vortex of shards.",
		"Improves your armour, permanently.",
		"Makes areas of crushing gravity.",
		"Restores strength, dexterity and constitution.",
		"Creates sharp crystals that rain down at random.",
		"Turns your skin into indestructable crystal."
	}, 
	/* Wizard */

	{
		"Grants infravision",
		"A magic missile spell.",
		"Lights the area.",
		"Blink.",
		"Detects traps, doors and stairs.",
		"Removes fear.",
		"Detects traps and monsters even if they are invisible.",
		"Slows poison.",

		"Teleports the player at random.",
		"Hastes self.",
		"Grants the ability to see invisible monsters.",
		"Dispells bad effects.",
		"Slows multible targets.",
		"Reports about magic effects on you.",
		"Cures medium wounds.",
		"Boosts your armour.",

		"A bolt of disintegration.",
		"Repels attacks by evil monsters.",
		"Teleports monsters.",
		"A controlled teleport.",
		"Summons a monster to fight for you.",
		"Holds the target.",
		"Makes everything in a direction disintegrate.",
		"Recharges a magic item.",

		"Zappes anyone that touches you.",
		"A meteorite strikes the target down.",
		"Word of Recall.",
		"Greater Identify.",
		"Makes meteorites rain down at random.",
		"Restorse all stats and life levels.",
		"Dispels monsters in the area.",
		"Grants extra defence and damages anyone that hits you."
	} 
};





/*
 * Conversion of plusses to Deadliness to a percentage added to damage.
 * Much of this table is not intended ever to be used, and is included
 * only to handle possible inflation elsewhere. -LM-
 */
const byte deadliness_conversion[151] =
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
const byte chest_traps[64] =
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

	/* Besreker */
	{
		"Rookie",
		"Soldier",
		"Mercenary",
		"Veteran",
		"Rager",
		"Rage Champion",
		"Berserker Hero",
		"Baron",
		"Duke",
		"Berserker Lord",
	},
	
	/* Ninja */
	{
		"Cutpurse",
		"Robber",
		"Burglar",
		"Footpad",
		"Filcher",
		"Sharper",
		"Low Ninja",
		"High Ninja",
		"Master Ninja",
		"Shadow Ninja",
	},
	
	/* Rogues */
	{
		"Cutpurse",
		"Robber",
		"Burglar",
		"Footpad",
		"Filcher",
		"Sharper",
		"Low Thief",
		"High Thief",
		"Master Thief",
		"Guildmaster",
	},
	
	/* Assassin */
	{
		"Cutpurse",
		"Robber",
		"Burglar",
		"Footpad",
		"Filcher",
		"Sharper",
		"Low Assassin",
		"High Assassin",
		"Master Slayer",
		"Fell Assassin",
	},
	
	/* Thief Mage */
	{
		"Cutpurse",	
		"Sneak",	
		"Burglar",	
		"Trickster",	
		"Sharper",	
		"Mage Thief",	
		"High Thief",	
		"Rogue Evoker",	
		"Far Snatcher",	
		"Guild Mage",	
	},
	
	
	/* Druid */
	{
		"Believer",
		"Acolyte",
		"Adept",
		"Curate",
		"Canon",
		"Druid",
		"High Druid",
		"Grove Keeper",
		"Inquisitor",
		"Angel",
	},
	
	/* Necromancer */
	{
		"Believer",
		"Acolyte",
		"Adept",
		"Curate",
		"Canon",
		"Necromancer",
		"Corpse Master",
		"Dark Cardinal",
		"Inquisitor",
		"Demon",
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

	/* Shaman */
	{
		"Believer",
		"Acolyte",
		"Adept",
		"Anarchist",
		"Canon",
		"Shaman",
		"High Shaman",
		"Cardinal",
		"Inquisitor",
		"Spirit Master",
	},

	/* Sage */
	{
		"Believer",
		"Acolyte",
		"Seeker",
		"Librarian",
		"Curate",
		"Sage",
		"High Sage",
		"Curioser",
		"Inquisitor",
		"Lore Master",
	},

	/* Fire Mage */
	{
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Flame Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
	},

	/* Water Mage */
	{
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Water Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
	},

	/* Earth Mage */
	{
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Rock Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
	},

	/* Air Mage */
	{
		"Apprentice",
		"Trickster",
		"Illusionist",
		"Spellbinder",
		"Air Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archimage",
	},

	/* Wizard */
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
	
	/* Dark Knight */
	{
		"Rookie",
		"Soldier",
		"Mercenary",
		"Veteran",
		"Swordsman",
		"Champion",
		"Dark Hero",
		"Dark Baron",
		"Dark Duke",
		"Dark Lord",
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

	/* Chaos Warrior */
	{
		"Rookie",
		"Anarchist",
		"Mercenary",
		"Veteran",
		"Swordsman",
		"Champion",
		"Chaos Hero",
		"Chaos Baron",
		"Chaos Duke",
		"Chaos Lord",
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
		"Magic Lord",
	},
	
	/* Technician */
	{
		"Trainee",
		"Apprentice",
		"Constructer",
		"Fixer",
		"Designer",
		"Shaper",
		"Crafter",
		"Devicer",
		"Technologist",
		"Tech Master",
	},
	
	/* Tech Warrior */
	{
		"Trainee",
		"Apprentice",
		"Soldier",
		"Fixer",
		"Designer",
		"Sword Shaper",
		"Crafter",
		"Weapon Smith",
		"Arcane Smith",
		"Smith Lord",
	},
	
	/* Tech Thief */
	{
		"Trainee",
		"Apprentice",
		"Sneak",
		"Constructer",
		"Designer",
		"Traper",
		"Crafter",
		"Devicer",
		"Tech Thief",
		"Guild Master",
	},
	
	/* Tech Cleric */
	{
		"Trainee",
		"Acolyte",
		"Builder",
		"Constructer",
		"Curate",
		"Shaper",
		"Crafter",
		"Curioser",
		"Tech Cleric",
		"Relic Master",
	},

	/* Tech Mage */
	{
		"Trainee",
		"Trickster",
		"Builder",
		"Charm Maker"
		"Spell Shaper",
		"Crafter",
		"Devicer",
		"Enchanter",
		"Tech Mage",
		"Mage Smith",
	},

	/* Archer */
	{
		"Rookie",
		"Soldier",
		"Mercenary",
		"Veteran",
		"Archer",
		"Champion",
		"Hero",
		"Baron",
		"Duke",
		"Missile Lord",
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
	
	/* Monk */
	{
		"Initiate",
		"Brother",
		"Disciple",
		"Immaculate",
		"Master",
		"Soft Master",
		"Hard Master",
		"Lotus Master",
		"Dragon Master",
		"Grand Master",
	},

	/* Witch */
	{
		"Apprentice",
		"Gypsy",
		"Illusionist",
		"Spellbinder",
		"Evoker",
		"Conjurer",
		"Warlock",
		"Sorcerer",
		"Ipsissimus",
		"Archwitch",
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
	"Display visible monsters",
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
	{FALSE, 8, "view_monster_grids",	"Map remembers monster-lit grids" },
	{FALSE, 1, "toggle_xp",				"Reverse experience display" },
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
	{TRUE,  0, NULL,					"Number 42" },
	{TRUE,  0, NULL,					"Number 43" },
	{TRUE,  0, NULL,					"Number 44" },
	{TRUE,  0, NULL,					"Number 45" },
	{TRUE,  7, "smart_packs",			"Pack monsters use new AI" },
	{FALSE, 0, NULL,					"Number 47" },
	{FALSE, 0, NULL,					"Number 48" },
	{FALSE, 0, NULL,					"Number 49" },
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
	{FALSE, 6, "ironman_los",			"Monsters use player line of sight" },
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
	{FALSE, 0, NULL,					"Number 224" },
	{FALSE, 5, "monster_light",			"Allow monsters to carry lights" },
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
	{TRUE,  8, "testing_stack",			"Allow objects to stack on floor" },
	{TRUE,  0, NULL,					NULL },
};

const int birth_options[OPT_BIRTH + 1] =
{
	162, 192, 194, 195, 196, 197, 198, 199,
	200, 201, 202, 203, 204, 205, 206, 207,
	208, 209, 210, 211, 212, 213, 214, 215,
	216, 217, 218, 219, 220, 221, 222, 223, 0
};

const int server_options[OPT_SERVER + 1] =
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

const int chaos_stats[MAX_PATRON] =
{
	A_CON,  /* Slortar */
	A_CON,  /* Mabelode */
	A_STR,  /* Chardros */
	A_STR,  /* Hionhurn */
	A_STR,  /* Xiombarg */

	A_WIS,  /* Pyaray */
	A_STR,  /* Balaan */
	A_WIS,  /* Arioch */
	A_CON,  /* Eequor */
	A_CHR,  /* Narjhan */

	-1,     /* Balo */
	A_STR,  /* Khorne */
	A_CHR,  /* Slaanesh */
	A_CON,  /* Nurgle */
	A_INT,  /* Tzeentch */

	A_STR,  /* Khaine */
};




const int chaos_rewards[MAX_PATRON][20] =
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

const martial_arts ma_blows[MAX_MA] =
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
const field_action f_action[] =
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

	/* Weaponmaster part 1 */
	{field_action_weaponmaster1, "field_action_weaponmaster1"},
	
	/* Weaponmaster part 2 */
	{field_action_weaponmaster2, "field_action_weaponmaster2"},
	
	/* Recharge items part 1 */
	{field_action_recharge1, "field_action_recharge1"},
	
	/* Recharge items part 2 */
	{field_action_recharge2, "field_action_recharge2"},
	
	/* Enchant weapons part 1 */
	{field_action_weaponplus1, "field_action_weaponplus1"},
	
	/* Enchant weapons part 2 */
	{field_action_weaponplus2, "field_action_weaponplus2"},
	
	/* Enchant armour part 1 */
	{field_action_armourplus1, "field_action_armourplus1"},
	
	/* Enchant armour part 2 */
	{field_action_armourplus2, "field_action_armourplus2"},
	
	/* Mutatalist part 1 */
	{field_action_mutate1, "field_action_mutate1"},
	
	/* Mutatalist part 2 */
	{field_action_mutate2, "field_action_mutate2"},
	
	/* Map maker part 1 */
	{field_action_buymap1, "field_action_buymap1"},
	
	/* Map maker part 2 */
	{field_action_buymap2, "field_action_buymap2"},
	
	/* Library part 1 */
	{field_action_library1, "field_action_library1"},
	
	/* Librry part 2 */
	{field_action_library2, "field_action_library2"},
	
	/* Casino part 1 */
	{field_action_casino1, "field_action_casino1"},
	
	/* Casino part 2 */
	{field_action_casino2, "field_action_casino2"},
	
	/* Inn part 1 */
	{field_action_inn1, "field_action_inn1"},
	
	/* Inn part 2 */
	{field_action_inn2, "field_action_inn2"},
	
	/* Healer part 1 */
	{field_action_healer1, "field_action_healer1"},
	
	/* Healer part 2 */
	{field_action_healer2, "field_action_healer2"},
	
	/* Bookstore */
	{field_action_isbook_tester, "field_action_isbook_tester"},
	
	/* Weaponstore */
	{field_action_isweapon_tester, "field_action_isweapon_tester"},
	
	/* Armourstore */
	{field_action_isarmour_tester, "field_action_isarmour_tester"},
	
	/* Weapon/Armour store */
	{field_action_isweaparmour_tester, "field_action_isweaparmour_tester"},
	
	/* Ammostore */
	{field_action_isammo_tester, "field_action_isammo_tester"},
	
	/* Potion store */
	{field_action_ispotion_tester, "field_action_ispotion_tester"},
	
	/* Scroll store */
	{field_action_isscroll_tester, "field_action_isscroll_tester"},
	
	/* Statue store */
	{field_action_isstatue_tester, "field_action_isstatue_tester"},
	
	/* Figurine store */
	{field_action_isfigurine_tester, "field_action_isfigurine_tester"},
	
	/* Food store */
	{field_action_isfood_tester, "field_action_isfood_tester"},
	
	/* Rechargable items store */
	{field_action_isrecharge_tester, "field_action_isrecharge_tester"},
	
	/* Jewels store */
	{field_action_isjewel_tester, "field_action_isjewel_tester"},
	
	/* Wieldable items store */
	{field_action_iswield_tester, "field_action_iswield_tester"},
	
	/* Fletcher store */
	{field_action_isfletcher_tester, "field_action_isfletcher_tester"},
	
	/* Swordsman */
	{field_action_issword_tester, "field_action_issword_tester"},
	
	/* Axeman */
	{field_action_isnonsword_tester, "field_action_isnonsword_tester"},

	/* Shieldsman */
	{field_action_isshield_tester, "field_action_isshield_tester"},
	
	/* Clothes store */
	{field_action_isclothes_tester, "field_action_isclothes_tester"},
	
	/* Hard Armour store */
	{field_action_ishardarmour_tester, "field_action_ishardarmour_tester"},
	
	/* Pure Hard armour store */
	{field_action_isphardarmour_tester, "field_action_isphardarmour_tester"},
	
	/* Helms maker */
	{field_action_ishelm_tester, "field_action_ishelm_tester"},
	
	/* Non-scroll magic items maker */
	{field_action_issupplies_tester, "field_action_issupplies_tester"},

	/* Done */
	{NULL, NULL}
};

/*
 * The mutations:
 *
 * Actual mutation,
 * Description of mutation
 * Text on gaining mutation
 * Text on losing mutation
 * Short text (for choosing activatable mutations)
 * level, cost, stat, difficulty (each for activatable mutations only)
 * chance (random mutations only)
 */
const mutation_type mutations[MUT_SETS_MAX * MUT_PER_SET] =
{
	/* Activatable mutations */
	{
		MUT1_SPIT_ACID,
		"You can spit acid.",
		"You gain the ability to spit acid.",
		"You lose the ability to spit acid.",
		"Spit acid (dam lvl)",
		9, 9, A_DEX, 15,
		0
	},

	{
		MUT1_BR_FIRE,
		"You can breathe fire.",
		"You gain the ability to breathe fire.",
		"You lose the ability to breathe fire.",
		"Fire breath (dam lvl*2)",
		20, 20, A_CON, 18,
		0
	},

	{
		MUT1_HYPN_GAZE,
		"Your gaze is hypnotic.",
		"Your eyes look mesmerizing...",
		"Your eyes look uninteresting.",
		"Hypnotic gaze",
		12, 12, A_CHR, 18,
		0
	},

	{
		MUT1_TELEKINES,
		"You are telekinetic.",
		"You gain the ability to move objects telekinetically.",
		"You lose the ability to move objects telekinetically.",
		"Telekinesis",
		9, 9, A_WIS, 14,
		0
	},

	{
	 	MUT1_VTELEPORT,
		"You can teleport at will.",
		"You gain the power of teleportation at will.",
		"You lose the power of teleportation at will.",
		"Teleport",
		7, 7, A_WIS, 15,
		0
	},

	{
		MUT1_MIND_BLST,
		"You can Mind Blast your enemies.",
		"You gain the power of Mind Blast.",
		"You lose the power of Mind Blast.",
		"Mind blast",
		5, 3, A_WIS, 15,
		0
	},

	{
		MUT1_RADIATION,
		"You can emit hard radiation at will.",
		"You start emitting hard radiation.",
		"You stop emitting hard radiation.",
		"Emit radiation",
		15, 15, A_CON, 14,
		0
	},

	{
	    MUT1_VAMPIRISM,
	    "You can drain life from a foe like a vampire.",
	    "You become vampiric.",
	    "You are no longer vampiric.",
	    "Vampiric drain",
	    10, 10, A_CON, 9,
	    0
	},

	{
	    MUT1_SMELL_MET,
	    "You can smell nearby precious metal.",
	    "You smell a metallic odor.",
	    "You no longer smell a metallic odor.",
	    "Smell metal",
	    3, 2, A_INT, 12,
	    0
	},

	{
	    MUT1_SMELL_MON,
	    "You can smell nearby monsters.",
	    "You smell filthy monsters.",
	    "You no longer smell filthy monsters.",
	    "Smell monsters",
	    5, 4, A_INT, 15,
	    0
	},

	{
	    MUT1_BLINK,
	    "You can teleport yourself short distances.",
	    "You gain the power of minor teleportation.",
	    "You lose the power of minor teleportation.",
	    "Blink",
	    3, 3, A_WIS, 12,
	    0
	},

	{
	    MUT1_EAT_ROCK,
	    "You can consume solid rock.",
	    "The walls look delicious.",
	    "The walls look unappetizing.",
	    "eat rock",
	    8, 12, A_CON, 18,
	    0
	},

	{
	    MUT1_SWAP_POS,
	    "You can switch locations with another being.",
	    "You feel like walking a mile in someone else's shoes.",
	    "You feel like staying in your own shoes.",
	    "Swap position",
	    15, 12, A_DEX, 16,
	    0
	},

	{
		MUT1_SHRIEK,
	    "You can emit a horrible shriek.",
	    "Your vocal cords get much tougher.",
	    "Your vocal cords get much weaker.",
	    "Shriek",
	    20, 14, A_CON, 16,
	    0
	},

	{
	    MUT1_ILLUMINE,
	    "You can emit bright light.",
	    "You can light up rooms with your presence.",
	    "You can no longer light up rooms with your presence.",
	    "Illuminate area",
	    3, 2, A_INT, 10,
	    0
	},

	{
	    MUT1_DET_CURSE,
	    "You can feel the danger of evil magic.",
	    "You can feel evil magics.",
	    "You can no longer feel evil magics.",
	    "Detect curses",
	    7, 14, A_WIS, 14,
	    0
	},

	{
	    MUT1_BERSERK,
	    "You can drive yourself into a berserk frenzy.",
	    "You feel a controlled rage.",
	    "You no longer feel a controlled rage.",
	    "Berserk",
	    8, 8, A_STR, 14,
	    0
	},

	{
	    MUT1_POLYMORPH,
	    "You can polymorph yourself at will.",
	    "Your body seems mutable.",
	    "Your body seems stable.",
	    "Polymorph",
	    18, 20, A_CON, 18,
	    0
	},

	{
	    MUT1_MIDAS_TCH,
	    "You can turn ordinary items to gold.",
	    "You gain the Midas touch.",
	    "You lose the Midas touch.",
	    "Midas touch",
	    10, 5, A_INT, 12,
	    0
	},

	{
	    MUT1_GROW_MOLD,
	    "You can cause mold to grow near you.",
	    "You feel a sudden affinity for mold.",
	    "You feel a sudden dislike for mold.",
	    "Grow mold",
	    1, 6, A_CON, 14,
	    0
	},

	{
	    MUT1_RESIST,
	    "You can harden yourself to the ravages of the elements.",
	    "You feel like you can protect yourself.",
	    "You feel like you might be vulnerable.",
	    "Resist elements",
	    10, 12, A_CON, 12,
	    0
	},

	{
	    MUT1_EARTHQUAKE,
	    "You can bring down the dungeon around your ears.",
	    "You gain the ability to wreck the dungeon.",
	    "You lose the ability to wreck the dungeon.",
	    "Earthquake",
	    12, 12, A_STR, 16,
	    0
	},

	{
	    MUT1_EAT_MAGIC,
	    "You can consume magic energy for your own use.",
	    "Your magic items look delicious.",
	    "Your magic items no longer look delicious.",
	    "Eat magic",
	    17, 1, A_WIS, 15,
	    0
	},

	{
	    MUT1_WEIGH_MAG,
	    "You can feel the strength of the magics affecting you.",
	    "You feel you can better understand the magic around you.",
	    "You no longer sense magic.",
	    "Weigh magic",
	    6, 6, A_INT, 10,
	    0
	},

	{
	    MUT1_STERILITY,
	    "You can cause mass impotence.",
	    "You can give everything around you a headache.",
	    "You hear a massed sigh of relief.",
	    "Sterilize",
	    12, 23, A_CHR, 15,
	    0
	},

	{
	    MUT1_PANIC_HIT,
	    "You can run for your life after hitting something.",
	    "You suddenly understand how thieves feel.",
	    "You no longer feel jumpy.",
	    "Panic hit",
	    10, 12, A_DEX, 14,
	    0
	},

	{
	    MUT1_DAZZLE,
		"You can emit confusing, blinding radiation.",
		"You gain the ability to emit dazzling lights.",
		"You lose the ability to emit dazzling lights.",
	    "Dazzle",
	    7, 15, A_CHR, 8,
	    0
	},

	{
	    MUT1_LASER_EYE,
	    "Your eyes can fire laser beams.",
	    "Your eyes burn for a moment.",
	    "Your eyes burn for a moment, then feel soothed.",
	    "Laser eye",
	    7, 10, A_WIS, 9,
	    0
	},

	{
	    MUT1_RECALL,
	    "You can travel between town and the depths.",
	    "You feel briefly homesick, but it passes.",
	    "You feel briefly homesick.",
	    "Recall",
	    17, 50, A_INT, 16,
	    0
	},

	{
	    MUT1_BANISH,
	    "You can send evil creatures directly to Hell.",
	    "You feel a holy wrath fill you.",
	    "You no longer feel a holy wrath.",
	    "Banish evil",
	    25, 25, A_WIS, 18,
	    0
	},

	{
	    MUT1_COLD_TOUCH,
	    "You can freeze things with a touch.",
	    "Your hands get very cold.",
	    "Your hands warm up.",
	    "Cold touch",
	    2, 2, A_CON, 11,
	    0
	},

	{
	    MUT1_LAUNCHER,
	    "You can hurl objects with great force.",
	    "Your throwing arm feels much stronger.",
	    "Your throwing arm feels much weaker.",
	    "Throw object",
	    10, 15, A_STR, 6,
	    0
	},

	/* Randomly activating mutations */
	{
	    MUT2_BERS_RAGE,
	    "You are subject to berserker fits.",
	    "You become subject to fits of berserk rage!",
	    "You are no longer subject to fits of berserk rage!",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	 },

	{
	    MUT2_COWARDICE,
	    "You are subject to cowardice.",
	    "You become an incredible coward!",
	    "You are no longer an incredible coward!",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_RTELEPORT,
	    "You are teleporting randomly.",
	    "Your position seems very uncertain...",
	    "Your position seems more certain.",
	    "(nothing)",
	    0, 0, 0, 0,
	    50
	},

	{
	    MUT2_ALCOHOL,
	    "Your body produces alcohol.",
	    "Your body starts producing alcohol!",
	    "Your body stops producing alcohol!",
	    "(nothing)",
	    0, 0, 0, 0,
	    64
	},

	{
	    MUT2_HALLU,
	    "You have a hallucinatory insanity.",
	    "You are afflicted by a hallucinatory insanity!",
	    "You are no longer afflicted by a hallucinatory insanity!",
	    "(nothing)",
	    0, 0, 0, 0,
	    64
	},

	{
	    MUT2_FLATULENT,
	    "You are subject to uncontrollable flatulence.",
	    "You become subject to uncontrollable flatulence.",
	    "You are no longer subject to uncontrollable flatulence.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_SCOR_TAIL,
	    "You have a scorpion tail (poison, 3d7).",
	    "You grow a scorpion tail!",
	    "You lose your scorpion tail!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_HORNS,
	    "You have horns (dam. 2d6).",
	    "You have horns on your forehead!",
	    "Your horns vanish from your forehead!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_BEAK,
	    "You have a beak (dam. 2d4).",
	    "Your mouth turns into a sharp, powerful beak!",
	    "Your mouth reverts to normal!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_ATT_DEMON,
	    "You attract demons.",
	    "You start attracting demons.",
	    "You stop attracting demons.",
	    "(nothing)",
	    0, 0, 0, 0,
	    66
	},

	{
	    MUT2_PROD_MANA,
	    "You are producing magical energy uncontrollably.",
	    "You start producing magical energy uncontrollably.",
	    "You stop producing magical energy uncontrollably.",
	    "(nothing)",
	    0, 0, 0, 0,
	    90
	},

	{
	    MUT2_SPEED_FLUX,
	    "You move faster or slower randomly.",
	    "You become manic-depressive.",
	    "You are no longer manic-depressive.",
	    "(nothing)",
	    0, 0, 0, 0,
	    60
	},

	{
	    MUT2_BANISH_ALL,
	    "You sometimes cause nearby creatures to vanish.",
	    "You feel a terrifying power lurking behind you.",
	    "You no longer feel a terrifying power lurking behind you.",
	    "(nothing)",
	    0, 0, 0, 0,
	    90
	},

	{
	    MUT2_EAT_LIGHT,
	    "You sometimes feed off of the light around you.",
	    "You feel a strange kinship with Ungoliant.",
	    "You feel the world's a brighter place.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_TRUNK,
	    "You have an elephantine trunk (dam 1d4).",
	    "Your nose grows into an elephant-like trunk.",
	    "Your nose returns to a normal length.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_ATT_ANIMAL,
	    "You attract animals.",
	    "You start attracting animals.",
	    "You stop attracting animals.",
	    "(nothing)",
	    0, 0, 0, 0,
	    70
	},

	{
	    MUT2_TENTACLES,
	    "You have evil looking tentacles (dam 2d5).",
	    "Evil-looking tentacles sprout from your sides.",
	    "Your tentacles vanish from your sides.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_RAW_CHAOS,
	    "You occasionally are surrounded with raw chaos.",
	    "You feel the universe is less stable around you.",
	    "You feel the universe is more stable around you.",
	    "(nothing)",
	    0, 0, 0, 0,
	    80
	},

	{
	    MUT2_NORMALITY,
	    "You may be mutated, but you're recovering.",
	    "You feel strangely normal.",
	    "You feel normally strange.",
	    "(nothing)",
	    0, 0, 0, 0,
	    50
	},

	{
	    MUT2_WRAITH,
	    "You fade in and out of physical reality.",
	    "You start to fade in and out of the physical world.",
	    "You are firmly in the physical world.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_POLY_WOUND,
	    "Your health is subject to chaotic forces.",
	    "You feel forces of chaos entering your old scars.",
	    "You feel forces of chaos departing your old scars.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_WASTING,
	    "You have a horrible wasting disease.",
	    "You suddenly contract a horrible wasting disease.",
	    "You are cured of the horrible wasting disease!",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_ATT_DRAGON,
	    "You attract dragons.",
	    "You start attracting dragons.",
	    "You stop attracting dragons.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_WEIRD_MIND,
	    "Your mind randomly expands and contracts.",
	    "Your thoughts suddenly take off in strange directions.",
	    "Your thoughts return to boring paths.",
	    "(nothing)",
	    0, 0, 0, 0,
	    30
	},

	{
	    MUT2_NAUSEA,
	    "You have a seriously upset stomach.",
	    "Your stomach starts to roil nauseously.",
	    "Your stomach stops roiling.",
	    "(nothing)",
	    0, 0, 0, 0,
	    90
	},

	{
	    MUT2_CHAOS_GIFT,
	    "Chaos deities give you gifts.",
	    "You attract the notice of a chaos deity!",
	    "You lose the attention of the chaos deities.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT2_WALK_SHAD,
	    "You occasionally stumble into other shadows.",
	    "You feel like reality is as thin as paper.",
	    "You feel like you're trapped in reality.",
	    "(nothing)",
	    0, 0, 0, 0,
	    120
	},

	{
	    MUT2_WARNING,
	    "You receive warnings about your foes.",
	    "You suddenly feel paranoid.",
	    "You no longer feel paranoid.",
	    "(nothing)",
	    0, 0, 0, 0,
	    10
	},

	{
	    MUT2_INVULN,
	    "You occasionally feel invincible.",
	    "You are blessed with fits of invulnerability.",
	    "You are no longer blessed with fits of invulnerability.",
	    "(nothing)",
	    0, 0, 0, 0,
	    50
	},

	{
	    MUT2_SP_TO_HP,
	    "Your blood sometimes rushes to your muscles.",
	    "You are subject to fits of magical healing.",
	    "You are no longer subject to fits of magical healing.",
	    "(nothing)",
	    0, 0, 0, 0,
	    20
	},

	{
	    MUT2_HP_TO_SP,
	    "Your blood sometimes rushes to your head.",
	    "You are subject to fits of painful clarity.",
	    "You are no longer subject to fits of painful clarity.",
	    "(nothing)",
	    0, 0, 0, 0,
	    40
	},

	{
	    MUT2_DISARM,
	    "You occasionally stumble and drop things.",
	    "Your feet grow to four times their former size.",
	    "Your feet shrink to their former size.",
	    "(nothing)",
	    0, 0, 0, 0,
	    100
	},

	/* Other Mutations */
  	{
		MUT3_HYPER_STR,
	    "You are superhumanly strong (+4 STR).",
	    "You turn into a superhuman he-man!",
	    "Your muscles revert to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_PUNY,
	    "You are puny (-4 STR).",
	    "Your muscles wither away...",
	    "Your muscles revert to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_HYPER_INT,
	    "Your brain is a living computer (+4 INT/WIS).",
	    "Your brain evolves into a living computer!",
	    "Your brain reverts to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_MORONIC,
	    "You are moronic (-4 INT/WIS).",
	    "Your brain withers away...",
	    "Your brain reverts to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_RESILIENT,
	    "You are very resilient (+4 CON).",
	    "You become extraordinarily resilient.",
	    "You become ordinarily resilient again.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_XTRA_FAT,
	    "You are extremely fat (+2 CON, -2 speed).",
	    "You become sickeningly fat!",
	    "You benefit from a miracle diet!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_ALBINO,
	    "You are albino (-4 CON).",
	    "You turn into an albino! You feel frail...",
	    "You are no longer an albino!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_FLESH_ROT,
	    "Your flesh is rotting (-2 CON, -1 CHR).",
	    "Your flesh is afflicted by a rotting disease!",
	    "Your flesh is no longer afflicted by a rotting disease!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_SILLY_VOI,
	    "Your voice is a silly squeak (-4 CHR).",
	    "Your voice turns into a ridiculous squeak!",
	    "Your voice returns to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_BLANK_FAC,
	    "Your face is featureless (-1 CHR).",
	    "Your face becomes completely featureless!",
	    "Your facial features return.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_ILL_NORM,
	    "Your appearance is masked with illusion.",
	    "You start projecting a reassuring image.",
	    "You stop projecting a reassuring image.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_XTRA_EYES,
	    "You have an extra pair of eyes (+15 search).",
	    "You grow an extra pair of eyes!",
	    "Your extra eyes vanish!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_MAGIC_RES,
	    "You are resistant to magic.",
	    "You become resistant to magic.",
	    "You become susceptible to magic again.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_XTRA_NOIS,
	    "You make a lot of strange noise (-3 stealth).",
	    "You start making strange noise!",
	    "You stop making strange noise!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_INFRAVIS,
	    "You have remarkable infravision (+3).",
	    "Your infravision is improved.",
	    "Your infravision is degraded.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_XTRA_LEGS,
	    "You have an extra pair of legs (+3 speed).",
	    "You grow an extra pair of legs!",
	    "Your extra legs disappear!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_SHORT_LEG,
	    "Your legs are short stubs (-3 speed).",
	    "Your legs turn into short stubs!",
	    "Your legs lengthen to normal.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_ELEC_TOUC,
	    "Electricity is running through your veins.",
	    "Electricity starts running through you!",
	    "Electricity stops running through you.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_FIRE_BODY,
	    "Your body is enveloped in flames.",
	    "Your body is enveloped in flames!",
	    "Your body is no longer enveloped in flames.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_WART_SKIN,
	    "Your skin is covered with warts (-2 CHR, +5 AC).",
	    "Disgusting warts appear everywhere on you!",
	    "Your warts disappear!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_SCALES,
	    "Your skin has turned into scales (-1 CHR, +10 AC).",
	    "Your skin turns into black scales!",
	    "Your scales vanish!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_IRON_SKIN,
	    "Your skin is made of steel (-1 DEX, +25 AC).",
	    "Your skin turns to steel!",
	    "Your skin reverts to flesh!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_WINGS,
	    "You have wings.",
	    "You grow a pair of wings.",
	    "Your wings fall off.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_FEARLESS,
	    "You are completely fearless.",
	    "You become completely fearless.",
	    "You begin to feel fear again.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_REGEN,
	    "You are regenerating.",
	    "You start regenerating.",
	    "You stop regenerating.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_ESP,
	    "You are telepathic.",
	    "You develop a telepathic ability!",
	    "You lose your telepathic ability!",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_LIMBER,
	    "Your body is very limber (+3 DEX).",
	    "Your muscles become limber.",
	    "Your muscles stiffen.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_ARTHRITIS,
	    "Your joints ache constantly (-3 DEX).",
	    "Your joints suddenly hurt.",
	    "Your joints stop hurting.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_BAD_LUCK,
	    "There is a black aura surrounding you.",
	    "There is a malignant black aura surrounding you...",
	    "Your black aura swirls and fades.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_VULN_ELEM,
	    "You are susceptible to damage from the elements.",
	    "You feel strangely exposed.",
	    "You feel less exposed.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
	    MUT3_MOTION,
	    "Your movements are precise and forceful (+1 STL).",
	    "You move with new assurance.",
	    "You move with less assurance.",
	    "(nothing)",
	    0, 0, 0, 0,
	    0
	},

	{
		MUT3_GOOD_LUCK,
		"There is a white aura surrounding you.",
		"There is a benevolent white aura surrounding you...",
		"Your white aura shimmers and fades.",
		"(nothing)",
		0, 0, 0, 0,
		0
	}
};


/*
 * The racial powers:
 *
 * Actual mutation,
 * Description of mutation
 * Text on gaining mutation
 * Text on losing mutation
 * Short text (for choosing activatable mutations)
 * level, cost, stat, difficulty (each for activatable mutations only)
 * chance (random mutations only)
 */
const mutation_type race_powers[MAX_RACE_POWERS] =
{
	{
	    
	   RACE_DWARF,
	    "You can find traps, doors and stairs.",
	    "(nothing)",
	    "(nothing)",
	    "Detect doors+traps",
	    5, 5, A_WIS, 12,
	    0
	},

	{
	    RACE_HALFLING,
	    "You can forage in the dungeon.",
	    "(nothing)",
	    "(nothing)",
	    "Create food",
	    15, 10, A_INT, 10,
	    0
	},

	{
	    RACE_GNOME,
	    "You can move youself accross the dungeon.",
	    "(nothing)",
	    "(nothing)",
	    "Telport (range 10 + plev)",
	    5, 10, A_INT, 12,
	    0
	},

	{
	    RACE_GOBLIN,
	    "You can recharge items.",
	    "(nothing)",
	    "(nothing)",
	    "Recharge",
	    25, 25, A_WIS, 8,
	    0
	},

//	{
//	    RACE_ORC,
//	    "You can drive yourself into a berserk frenzy.",
//	    "(nothing)",
//	    "(nothing)",
//	    "Berserk",
//	    8, 10, A_WIS, 9,
//	    0
//	},

	{
	    RACE_DRYAD,
	    "You can cure your wounds.",
	    "(nothing)",
	    "(nothing)",
	    "Cure",
	    10, 12, A_INT, 9,
	    0
	},
	
	{
	    RACE_DRACONIAN,
	    "You can hit multiple enemies at once.",
	    "(nothing)",
	    "(nothing)",
	    "Dragon Swipe",
	    30, 30, A_STR, 50,
	    0
	},

	{
	    RACE_DRACONIAN,
	    "You can cast Dragon spells.",
	    "(nothing)",
	    "(nothing)",
	    "Dragon Magic",
	    20, 50, A_INT, 50,
	    0
	},

	{
	    RACE_ENT,
	    "You can regrow your body.",
	    "(nothing)",
	    "(nothing)",
	    "Regrow",
	    25, 60, A_CON, 12,
	    0
	},
	
	{
	    RACE_ENT,
	    "You can dig through rock.",
	    "(nothing)",
	    "(nothing)",
	    "Dig",
	    20, 20, A_STR, 9,
	    0
	},

	{
	    RACE_YEEK,
	    "You can make a terrifying scream.",
	    "(nothing)",
	    "(nothing)",
	    "Scare monsters",
	    15, 15, A_WIS, 10,
	    0
	},

	{
	    RACE_KOBOLD,
	    "You can throw poisoned darts.",
	    "(nothing)",
	    "(nothing)",
	    "Poison dart (dam lvl)",
	    12, 8, A_DEX, 14,
	    0
	},

	{
	    RACE_ILLITHID,
	    "You can blast your enemies with psionic energy.",
	    "(nothing)",
	    "(nothing)",
	    "Mind blast (dam lvl)",
	    15, 12, A_INT, 14,
	    0
	},

	{
	    RACE_VAMPIRE,
	    "You can steal life from a foe.",
	    "(nothing)",
	    "(nothing)",
	    "Drain life",
	    5, 10, A_CON, 9,
	    0
	},

	{
	    RACE_FAUN,
	    "You can induce dreams.",
	    "(nothing)",
	    "(nothing)",
	    "Dream Bolt",
	    30, 12, A_INT, 15,
	    0
	},

	{
	    RACE_PIXIE,
	    "You can throw magic dust which induces sleep.",
	    "(nothing)",
	    "(nothing)",
	    "Sleeping dust",
	    12, 12, A_INT, 15,
	    0
	},

};    

