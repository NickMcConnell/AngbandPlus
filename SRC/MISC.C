/* File: misc.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * The standard R.N.G. state array (used below)
 */
static char *old_state = NULL;

/*
 * Hack -- The dummy R.N.G. state array
 * Used to keep consistent object colors and town layout
 */
static char *dummy_state = NULL;


/*
 * Gets a new random seed for the random number generator
 * Hack -- saves seeds for the town layout and object colors
 */
void init_seeds(void)
{
    static u32b old_buf[256/4];
    static u32b dummy_buf[8/4];

    /* Allocate some RNG arrays */
    old_state = (char*)old_buf; /* excellent R.N.G. */
    dummy_state = (char*)dummy_buf; /* simple R.N.G. */

    /* Grab a random seed from the clock -- is this ignored by unix? */
    (void)initstate(time(NULL), dummy_state, 8);

    /* Note that "getpid()" is less than informative except on Unix */
    /* This may need to be changed.  It's fine for PCs, anyways... -CFT */

#ifdef SET_UID

    /* Grab a random seed from the clock & PID... */
    (void)initstate(((getpid() << 1) * (time(NULL) >> 3)), old_state, 256);

#else

    /* Use the seed we got above to re-seed */
    (void)initstate(random(), old_state, 256);

#endif

    /* Hack -- Extract seeds for the town layout and object colors */
    town_seed = random();
    randes_seed = random();
}


/*
 * change to different random number generator state
 * Hack -- used to keep consistent object colors and town layout
 */
void set_seed(u32b seed)
{
    setstate(dummy_state);
    srandom((seed % 2147483646L) + 1);
}


/*
 * restore the normal random generator state
 */
void reset_seed(void)
{
    (void)setstate(old_state);
}



/*
 * Generates a random integer number of NORMAL distribution -RAK-
 *
 * XXX This should probably be retested just for paranoia's sake.
 *
 * There has been some suspicion that this function actually uses
 * a standard deviation of about 1.25 times the one given. XXX XXX
 */
int randnor(int mean, int stand)
{
    int tmp, offset, low, iindex, high;

    tmp = randint(MAX_SHORT);

    /* off scale, assign random value between 4 and 5 times SD */
    if (tmp == MAX_SHORT) {

	offset = 4 * stand + randint(stand);

	/* one half are negative */
	if (rand_int(2)) offset = (-offset);

	return (mean + offset);
    }


    /* binary search normal normal_table to get index that matches tmp */
    low = 0;
    iindex = NORMAL_TABLE_SIZE >> 1;
    high = NORMAL_TABLE_SIZE;

    /* this takes up to 8 iterations */
    while (TRUE) {

	if ((normal_table[iindex] == tmp) || (high == (low + 1))) {
	    break;
	}

	if (normal_table[iindex] > tmp) {
	    high = iindex;
	    iindex = low + ((iindex - low) >> 1);
	}
	else {
	    low = iindex;
	    iindex = iindex + ((high - iindex) >> 1);
	}
    }

    /* might end up one below target, check that here */
    if (normal_table[iindex] < tmp) iindex = iindex + 1;

    /* normal_table is based on SD of 64, so adjust the index value here, */
    /* round the half way case up */
    offset = ((stand * iindex) + (NORMAL_TABLE_SD >> 1)) / NORMAL_TABLE_SD;


    /* one half should be negative */
    if (rand_int(2)) offset = (-offset);

    return (mean + offset);
}


/*
 * Returns position of first set bit (and clears that bit)
 * Note that the low-order bit is bit zero, and the highest
 * order bit is 31.  We return (-1) if no bits are set.
 */
int bit_pos(u32b *test)
{
    int    i;
    u32b mask = 0x1L;

    /* Efficiency */
    if (!(*test)) return (-1);

    /* Scan the input */
    for (i = 0; i < sizeof(*test) * 8; i++) {

	/* Test and clear */
	if (*test & mask) {
	    *test &= ~mask;
	    return (i);
	}

	/* Next! */
	mask <<= 0x1L;
    }

    /* Paranoia */
    return (-1);
}


/*
 * Generates damage for "2d6" style dice rolls
 */
int damroll(int num, int sides)
{
    int i, sum = 0;
    for (i = 0; i < num; i++) sum += randint(sides);
    return (sum);
}


/*
 * Same as above, but always maximal
 */
int maxroll(int num, int sides)
{
    return (num * sides);
}





/*
 * Modify a stat by a "modifier", return new value
 * Stats go: 3,4,...,17,18,18/10,18/20,...
 */
int modify_stat(int stat, int amount)
{
    int    loop, i, tmp_stat;

    tmp_stat = p_ptr->cur_stat[stat];

    loop = ABS(amount);

    for (i = 0; i < loop; i++) {
	if (amount > 0) {
	    if (tmp_stat < 18) tmp_stat++;
	    else tmp_stat += 10;
	}
	else {
	    if (tmp_stat > 27) tmp_stat -= 10;
	    else if (tmp_stat > 18) tmp_stat = 18;
	    else if (tmp_stat > 3) tmp_stat--;
	}
    }

    return tmp_stat;
}


/*
 * Modify the "max" stat by a "modifier", return new value
 * Stats go: 3,4,...,17,18,18/10,18/20,...
 */
static int modify_max_stat(int stat, int amount)
{
    int    loop, i;
    u16b tmp_stat;

    tmp_stat = p_ptr->max_stat[stat];

    loop = ABS(amount);

    for (i = 0; i < loop; i++) {
	if (amount > 0) {
	    if (tmp_stat < 18) tmp_stat++;
	    else tmp_stat += 10;
	}
	else {
	    if (tmp_stat > 27) tmp_stat -= 10;
	    else if (tmp_stat > 18) tmp_stat = 18;
	    else if (tmp_stat > 3) tmp_stat--;
	}
    }

    return tmp_stat;
}



/*
 * Set the value of the stat which is actually used.     -CJS-
 * Also, make sure to recalculate any changed values.
 */
void set_use_stat(int stat)
{
    /* Calculate the "total" stat value */
    p_ptr->use_stat[stat] = modify_stat(stat, p_ptr->mod_stat[stat]);

    /* Calculate various effects */
    if (stat == A_STR) {
	p_ptr->update |= PU_BONUS;
    }
    else if (stat == A_DEX) {
	p_ptr->update |= PU_BONUS;
    }
    else if (stat == A_CON) {
	p_ptr->update |= PU_HP;
    }
    else if (cp_ptr->spell_stat && (stat == cp_ptr->spell_stat)) {
	p_ptr->update |= (PU_MANA | PU_SPELLS);
    }
}


/*
 * Increases a stat by one randomized level             -RAK-   
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
int inc_stat(int stat)
{
    int tmp_stat, gain;

    /* Hmmm -- First, restore the stat */
    res_stat(stat);

    /* Then augment the current/max stat */
    tmp_stat = p_ptr->cur_stat[stat];

    /* Cannot go above 18/100 */
    if (tmp_stat < 118) {
	if (tmp_stat < 18) {    
	    gain = randint(2);
	    tmp_stat += gain;
	}
	else if (tmp_stat < 116) {
	    /* stat increases by 1/6 to 1/3 of difference from max */
	    gain = ((118 - tmp_stat) / 2 + 3) >> 1;
	    tmp_stat += randint(gain) + gain / 2;
	    if (tmp_stat > 117) tmp_stat = 117;
	}
	else {
	    tmp_stat++;
	}

	p_ptr->cur_stat[stat] = tmp_stat;
	if (tmp_stat > p_ptr->max_stat[stat]) {
	    p_ptr->max_stat[stat] = tmp_stat;
	}
	set_use_stat(stat);

	/* Redraw the stats later */
	p_ptr->redraw |= PR_STATS;

	/* Success */
	return TRUE;
    }

    return FALSE;
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults.  -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm.
 */
int dec_stat(int stat, int amount, int permanent)
{
    int cur, max, loss, same, res = FALSE;


    /* Acquire current value */
    cur = p_ptr->cur_stat[stat];
    max = p_ptr->max_stat[stat];

    /* Note when the values are identical */
    same = (cur == max);

    /* Damage "current" value */
    if (cur > 3) {

	/* Handle "low" values */
	if (cur <= 18) {

	    if (amount > 90) cur--;
	    if (amount > 50) cur--;
	    if (amount > 20) cur--;
	    cur--;
	}

	/* Handle "high" values */
	else {

	    /* Hack -- Decrement by a random amount between one-quarter */
	    /* and one-half of the stat bonus times the percentage, with a */
	    /* minimum damage of half the percentage. -CWS */
	    loss = (((cur-18) / 2 + 1) / 2 + 1);
	    loss = ((randint(loss) + loss) * amount) / 100;
	    if (loss < amount/2) loss = amount/2;

	    /* Lose some points */
	    cur = cur - loss;

	    /* Hack -- Only reduce stat to 17 sometimes */
	    if (cur < 18) cur = (amount <= 20) ? 18 : 17;
	}

	/* Prevent illegal values */
	if (cur < 3) cur = 3;

	/* Something happened */
	if (cur != p_ptr->cur_stat[stat]) res = TRUE;
    }

    /* Damage "max" value */
    if (permanent && (max > 3)) {

	/* Handle "low" values */
	if (max <= 18) {

	    if (amount > 90) max--;
	    if (amount > 50) max--;
	    if (amount > 20) max--;
	    max--;
	}

	/* Handle "high" values */
	else {

	    /* Hack -- Decrement by a random amount between one-quarter */
	    /* and one-half of the stat bonus times the percentage, with a */
	    /* minimum damage of half the percentage. -CWS */
	    loss = (((max-18) / 2 + 1) / 2 + 1);
	    loss = ((randint(loss) + loss) * amount) / 100;
	    if (loss < amount/2) loss = amount/2;

	    /* Lose some points */
	    max = max - loss;

	    /* Hack -- Only reduce stat to 17 sometimes */
	    if (max < 18) max = (amount <= 20) ? 18 : 17;
	}

	/* Hack -- keep it clean */
	if (same || (max < cur)) max = cur;

	/* Something happened */
	if (max != p_ptr->max_stat[stat]) res = TRUE;
    }

    /* Apply changes */
    if (res) {

	/* Actually set the stat to its new value. */
	p_ptr->cur_stat[stat] = cur;
	p_ptr->max_stat[stat] = max;

	/* Update the stat */
	set_use_stat(stat);

	/* Redraw the stats later */
	p_ptr->redraw |= PR_STATS;
    }

    /* Done */
    return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
int res_stat(int stat)
{
    /* Restore */
    if (p_ptr->cur_stat[stat] != p_ptr->max_stat[stat]) {

	p_ptr->cur_stat[stat] = p_ptr->max_stat[stat];
	set_use_stat(stat);

	/* Redraw the stats later */
	p_ptr->redraw |= PR_STATS;

	/* Success */
	return TRUE;
    }

    /* Nothing to restore */
    return FALSE;
}



/*
 * Convert a stat into the 0-39 scale
 *
 * See adj_val_min[] and adj_val_max[] in "tables.c" for ranges.
 */
int stat_index(int stat)
{
    int value = p_ptr->use_stat[stat];
  
    /* Values: 3, 4, ..., 18 */
    if (value <= 18) return (value - 3);

    /* Ranges: 18/01-18/09, 18/10-18/19, ..., 18/90-18/99 */
    if (value <= 18+99) return (16 + (value - 18) / 10);

    /* Value: 18/100 */
    if (value == 18+100) return (26);

    /* Ranges: 18/101-18/109, 18/110-18/119, ..., 18/210-18/219 */
    if (value <= 18+219) return (27 + (value - (18+100)) / 10);

    /* Range: 18/220+ */
    return (39);
}





/*
 * Weapon weight VS strength and dexterity              -RAK-   
 */
int attack_blows(int weight)
{
    int i, wgt, str_index, dex_index;

    /* Number of blows */
    int b = 0;

    /* Start with the strength/dexterity */
    int s = p_ptr->use_stat[A_STR];
    int d = p_ptr->use_stat[A_DEX];

    /* Normal weapons */
    if (weight <= s * 15) {

	/* Access the dexterity */
	if (d < 10) dex_index = 0;
	else if (d < 19) dex_index = 1;
	else if (d < 68) dex_index = 2;
	else if (d < 108) dex_index = 3;
	else if (d < 118) dex_index = 4;
	else if (d < 119) dex_index = 5;
	else if (d < 128) dex_index = 6;
	else if (d < 138) dex_index = 7;
	else if (d < 148) dex_index = 8;
	else if (d < 158) dex_index = 9;
	else if (d < 178) dex_index = 10;
	else if (d < 198) dex_index = 11;
	else if (d < 218) dex_index = 12;
	else dex_index = 13;

	/* new class-based weight penalties -CWS */
	switch (p_ptr->pclass) {
	  case 0:                               /* Warriors */
	    wgt = ((s * 10) / ((weight < 30) ? 30 : weight));
	    break;
	  case 1:                               /* Mages */
	    wgt = ((s * 4) / ((weight < 20) ? 20 : weight));
	    break;
	  case 2:                               /* Priests */
	    wgt = ((s * 6) / ((weight < 35) ? 35 : weight));
	    break;
	  case 3:                               /* Rogues */
	    wgt = ((s * 7) / ((weight < 15) ? 15 : weight));
	    break;
	  case 4:                               /* Rangers */
	    wgt = ((s * 8) / ((weight < 35) ? 35 : weight));
	    break;
	  default:                      /* Paladins */
	    wgt = ((s * 8) / ((weight < 30) ? 30 : weight));
	    break;
	}

	/* Access the strength vs weight */
	if (wgt < 2) str_index = 0;
	else if (wgt < 3) str_index = 1;
	else if (wgt < 4) str_index = 2;
	else if (wgt < 6) str_index = 3;
	else if (wgt < 8) str_index = 4;
	else if (wgt < 10) str_index = 5;
	else if (wgt < 13) str_index = 6;
	else if (wgt < 15) str_index = 7;
	else if (wgt < 18) str_index = 8;
	else if (wgt < 20) str_index = 9;
	else if (wgt < 25) str_index = 10;
	else str_index = 11;

	/* Use the blows table */
	b = (int)blows_table[str_index][dex_index];

	/* took out non-warrior/mage attack limits 'cause they suck */

	/* Hack -- Equipment of Extra Attacks */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
	    if (inventory[i].flags1 & TR1_ATTACK_SPD) {
		b += inventory[i].pval;
	    }
	}
    }

    /* Always get at least one attack */
    if (b < 1) b = 1;

    /* Return the attack count */
    return (b);
}



/*
 * Saving throws for player character.          -RAK-   
 */
int player_saves(void)
{
    return (rand_int(100) < p_ptr->skill_sav);
}





/*
 * Some screen locations for various display routines
 */

#define ROW_RACE        1
#define ROW_CLASS       2
#define ROW_TITLE       3

#define ROW_LEVEL       (new_screen_layout ? 4 : 12)
#define COL_LEVEL       6

#define ROW_EXP         (new_screen_layout ? 5 : 13)
#define COL_EXP         4

#define ROW_STAT        (new_screen_layout ? 7 : 5)
#define COL_STAT        6

#define ROW_AC          (new_screen_layout ? 14 : 17)
#define COL_AC          6

#define ROW_CURHP       (new_screen_layout ? 15 : 16)
#define COL_CURHP       6

#define ROW_MAXHP       (new_screen_layout ? 16 : 15)
#define COL_MAXHP       6

#define ROW_MANA        (new_screen_layout ? 17 : 14)
#define COL_MANA        6

#define ROW_GOLD        18
#define COL_GOLD        3

#define ROW_WINNER      (new_screen_layout ? 19 : 20)
#define ROW_EQUIPPY     (new_screen_layout ? 20 : 4)

#define ROW_CUT         21
#define ROW_STUN        22

#define ROW_HUNGRY      23

#define COL_HUNGRY      0       /* "Hungry" or "Weak" */
#define COL_BLIND       7       /* "Blind" */
#define COL_CONFUSED    13      /* "Confused" */
#define COL_AFRAID      22      /* "Afraid" */
#define COL_POISONED    29      /* "Poisoned" */
#define COL_STATE       38      /* "Paralyzed!" or "Searching " or <state> */
#define COL_SPEED       49      /* "Slow (-N)" or "Fast (+N)" */
#define COL_STUDY       64      /* "Study" */
#define COL_DEPTH       70      /* "Lev N" or "N ft" (right justified) */



/*
 * Abbreviations of healthy stats
 */
static cptr stat_names[] = {
    "STR: ", "INT: ", "WIS: ", "DEX: ", "CON: ", "CHR: "
};

/*
 * Abbreviations of damaged stats
 */
static cptr stat_names_reduced[] = {
    "Str: ", "Int: ", "Wis: ", "Dex: ", "Con: ", "Chr: "
};




/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, int row, int col)
{
    /* Dump 13 spaces to clear */
    c_put_str(TERM_WHITE, "             ", row, col);

    /* Dump the info itself */
    c_put_str(TERM_L_BLUE, info, row, col);
}




/*
 * Converts stat num into a six-char string
 */
void cnv_stat(int my_stat, char *out_val)
{
    u16b stat = my_stat;
    int    part1, part2;

    if (stat > 18) {
	part1 = 18;
	part2 = stat - 18;
	if (part2 >= 220) {
	    (void)sprintf(out_val, "%2d/***", part1);
	}
	else if (part2 >= 100) {
	    (void)sprintf(out_val, "%2d/%03d", part1, part2);
	}
	else {
	    (void)sprintf(out_val, " %2d/%02d", part1, part2);
	}
    }
    else {
	(void)sprintf(out_val, "%6d", stat);
    }
}


/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
    char tmp[32];

    /* Display "injured" stat */
    if (p_ptr->cur_stat[stat] < p_ptr->max_stat[stat]) {
	put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
	cnv_stat(p_ptr->use_stat[stat], tmp);
	c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT);
    }

    /* Display "healthy" stat */
    else {
	put_str(stat_names[stat], ROW_STAT + stat, 0);
	cnv_stat(p_ptr->use_stat[stat], tmp);
	c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT);
    }
}




/*
 * Prints title of character                            -RAK-   
 */
static void prt_title()
{
    prt_field(title_string(), ROW_TITLE, 0);
}


/*
 * Prints level
 */
static void prt_level()
{
    char tmp[32];

    sprintf(tmp, "%6d", p_ptr->lev);

    if (p_ptr->lev >= p_ptr->max_plv) {
	put_str("LEVEL       ", ROW_LEVEL, 0);
	c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL);
    }
    else {
	put_str("Level       ", ROW_LEVEL, 0);
	c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL);
    }
}


/*
 * Display the experience
 */
static void prt_exp()
{
    char out_val[32];

    (void)sprintf(out_val, "%8ld", (long)p_ptr->exp);

    if (p_ptr->exp >= p_ptr->max_exp) {
	put_str("EXP.        ", ROW_EXP, 0);
	c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP);
    }
    else {
	put_str("Exp.        ", ROW_EXP, 0);
	c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP);
    }
}


/*
 * Prints players current mana points.
 */
static void prt_cmana()
{
    char tmp[32];
    byte color;

    /* Hack -- no mana for warriors */
    if (p_ptr->pclass == 0) return;

    sprintf(tmp, "%6d", p_ptr->cmana);

    if (p_ptr->cmana >= p_ptr->mana) {
	color = TERM_L_GREEN;
    }
    else if (p_ptr->cmana > (p_ptr->mana * hitpoint_warn) / 10) {
	color = TERM_YELLOW;
    }
    else {
	color = TERM_RED;
    }

    /* Show mana */
    c_put_str(color, tmp, ROW_MANA, COL_MANA);
}


/*
 * Prints Max hit points
 */
static void prt_mhp()
{
    char tmp[32];
    sprintf(tmp, "%6d", p_ptr->mhp);
    c_put_str(TERM_L_GREEN, tmp, ROW_MAXHP, COL_MAXHP);
}


/*
 * Prints players current hit points
 */
static void prt_chp()
{
    char tmp[32];
    byte color;
    sprintf(tmp, "%6d", p_ptr->chp);

    if (p_ptr->chp >= p_ptr->mhp) {
	color = TERM_L_GREEN;
    }
    else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10) {
	color = TERM_YELLOW;
    }
    else {
	color = TERM_RED;
    }

    c_put_str(color, tmp, ROW_CURHP, COL_CURHP);
}


/*
 * prints current AC
 */
static void prt_pac()
{
    char tmp[32];
    sprintf(tmp, "%6d", p_ptr->dis_ac);
    c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC);
}


/*
 * Prints current gold
 */
static void prt_gold()
{
    char tmp[32];

    sprintf(tmp, "%9ld", (long)p_ptr->au);
    c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD);
}


/*
 * Prints winner/wizard status on display                       -RAK-   
 */
static void prt_winner(void)
{
    if (wizard) {
	put_str("Wizard", ROW_WINNER, 0);
    }
    else if (total_winner) {
	put_str("Winner", ROW_WINNER, 0);
    }
    else {
	put_str("       ", ROW_WINNER, 0);
    }
}



/*
 * Display the "equippy chars"
 */
static void prt_equippy_chars(void)
{
    int i, j;
    inven_type *i_ptr;

    byte attr;
    char out_val[2];


    /* Hack -- skip if requested */
    if (!equippy_chars) return;


    /* Wipe the equippy chars */
    put_str("            ", ROW_EQUIPPY, 0);

    /* Analyze the pack */
    for (j = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++, j++) {

	/* Get the item */
	i_ptr = &inventory[i];

	/* Skip empty slots */
	if (!i_ptr->tval) continue;

	/* Hack -- remap unless using graphics */
	if (use_graphics) {

	    /* Extract the color */
	    attr = inven_attr(i_ptr);
	
	    /* Extract the symbol */
	    out_val[0] = inven_char(i_ptr);
	}
	
	/* Normal display */
	else {
	
	    /* Extract the attr */
	    attr = tval_to_attr[i_ptr->tval];

	    /* Extract the char */
	    out_val[0] = tval_to_char[i_ptr->tval];
	}
	
	/* Hack -- terminate the string */
	out_val[1] = '\0';

	/* Display the item symbol */
	c_put_str(attr, out_val, ROW_EQUIPPY, j);
    }
}


/*
 * Prints depth in stat area
 */
static void prt_depth()
{
    char depths[32];

    if (!dun_level) {
	(void)strcpy(depths, "Town");
    }
    else if (depth_in_feet) {
	(void)sprintf(depths, "%d ft", dun_level * 50);
    }
    else {
	(void)sprintf(depths, "Lev %d", dun_level);
    }

    /* Right-Adjust the "depth", and clear old values */
    prt(format("%7s", depths), 23, COL_DEPTH);
}


/*
 * Prints status of hunger
 */
static void prt_hunger()
{
    /* Getting Weak */
    if (p_ptr->food < PLAYER_FOOD_WEAK) {
	c_put_str(TERM_ORANGE, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
    }

    /* Getting Hungry */
    else if (p_ptr->food < PLAYER_FOOD_ALERT) {
	c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);
    }

    /* Well fed */
    else {
	put_str("      ", ROW_HUNGRY, COL_HUNGRY);
    }
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
    if (p_ptr->blind) {
	c_put_str(TERM_ORANGE, "Blind", 23, COL_BLIND);
    }
    else {
	put_str("     ", 23, COL_BLIND);
    }
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
    if (p_ptr->confused) {
	c_put_str(TERM_ORANGE, "Confused", 23, COL_CONFUSED);
    }
    else {
	put_str("        ", 23, COL_CONFUSED);
    }
}


/*
 * Prints Fear status
 */
static void prt_afraid()
{
    if (p_ptr->afraid) {
	c_put_str(TERM_ORANGE, "Afraid", 23, COL_AFRAID);
    }
    else {
	put_str("      ", 23, COL_AFRAID);
    }
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
    if (p_ptr->poisoned) {
	c_put_str(TERM_L_GREEN, "Poisoned", 23, COL_POISONED);
    }
    else {
	put_str("        ", 23, COL_POISONED);
    }
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 */
static void prt_state(void)
{
    byte attr = TERM_WHITE;
    char text[16];


    /* Paralysis */
    if (p_ptr->paralysis) {
	attr = TERM_RED;
	strcpy(text, "Paralyzed!");
    }

    /* Resting */
    else if (p_ptr->rest) {

	int i;

	/* Start with "Rest" */
	strcpy(text, "Rest      ");

	/* Extensive (timed) rest */
	if (p_ptr->rest >= 1000) {
	    i = p_ptr->rest / 100;
	    text[9] = '0';
	    text[8] = '0';
	    text[7] = '0' + (i % 10);
	    if (i >= 10) {
		i = i / 10;
		text[6] = '0' + (i % 10);
		if (i >= 10) {
		    text[5] = '0' + (i / 10);
		}
	    }
	}

	/* Long (timed) rest */
	else if (p_ptr->rest >= 100) {
	    i = p_ptr->rest;
	    text[9] = '0' + (i % 10);
	    i = i / 10;
	    text[8] = '0' + (i % 10);
	    text[7] = '0' + (i / 10);
	}

	/* Medium (timed) rest */
	else if (p_ptr->rest >= 10) {
	    i = p_ptr->rest;
	    text[9] = '0' + (i % 10);
	    text[8] = '0' + (i / 10);
	}

	/* Short (timed) rest */
	else if (p_ptr->rest > 0) {
	    i = p_ptr->rest;
	    text[9] = '0' + (i);
	}

	/* Rest until healed */
	else if (p_ptr->rest == -1) {
	    text[5] = text[6] = text[7] = text[8] = text[9] = '*';
	}

	/* Rest until done */
	else if (p_ptr->rest == -2) {
	    text[5] = text[6] = text[7] = text[8] = text[9] = '&';
	}

	/* Hack -- remember to clear it later */
	p_ptr->redraw |= PR_STATE;
    }

    /* Repeating */
    else if (command_rep) {

	if (command_rep > 999) {
	    (void)sprintf(text, "Rep. %3d00", command_rep / 100);
	}
	else {
	    (void)sprintf(text, "Repeat %3d", command_rep);
	}
    }

    /* Searching */
    else if (p_ptr->searching) {

	strcpy(text, "Searching ");
    }

    /* Nothing interesting */
    else {

	strcpy(text, "          ");
    }

    /* Display the info (or blanks) */
    c_put_str(attr, text, 23, COL_STATE);
}


/*
 * Prints the speed of a character.                     -CJS-
 */
static void prt_speed()
{
    int i = p_ptr->pspeed;

    int attr = TERM_WHITE;
    char buf[32] = "";

    /* Hack -- Visually "undo" the Search Mode Slowdown */
    if (p_ptr->searching) i += 10;

    /* Fast */
    if (i > 110) {
	attr = TERM_L_GREEN;
	sprintf(buf, "Fast (+%d)", (i - 110));
    }

    /* Slow */
    else if (i < 110) {
	attr = TERM_L_UMBER;
	sprintf(buf, "Slow (-%d)", (110 - i));
    }

    /* Display the speed */
    c_put_str(attr, format("%-14s", buf), 23, COL_SPEED);
}


static void prt_study()
{
    if (p_ptr->new_spells) {
	put_str("Study", 23, 64);
    }
    else {
	put_str("     ", 23, COL_STUDY);
    }
}


static void prt_cut()
{
    int c = p_ptr->cut;

    if (c > 1000) {
	c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, 0);
    }
    else if (c > 200) {
	c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, 0);
    }
    else if (c > 100) {
	c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, 0);
    }
    else if (c > 50) {
	c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, 0);
    }
    else if (c > 25) {
	c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, 0);
    }
    else if (c > 10) {
	c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, 0);
    }
    else if (c) {
	c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, 0);
    }
    else {
	put_str("            ", ROW_CUT, 0);
    }
}

/*
 * Cut the player
 */
void cut_player(int c)
{
    /* Total cut */
    c = p_ptr->cut + c;

    /* Save the cut */
    c = p_ptr->cut = (c < 0) ? 0 : (c < 5000) ? c : 5000;

    /* Describe the cut */
    if (c > 1000) {
	msg_print("You have been given a mortal wound.");
    }
    else if (c > 200) {
	msg_print("You have been given a deep gash.");
    }
    else if (c > 100) {
	msg_print("You have been given a severe cut.");
    }
    else if (c > 50) {
	msg_print("You have been given a nasty cut.");
    }
    else if (c > 25) {
	msg_print("You have been given a bad cut.");
    }
    else if (c > 10) {
	msg_print("You have been given a light cut.");
    }
    else if (c > 0) {
	msg_print("You have been given a graze.");
    }
}



static void prt_stun(void)
{
    int s = p_ptr->stun;

    if (s > 100) {
	c_put_str(TERM_RED, "Knocked out ", ROW_STUN, 0);
    }
    else if (s > 50) {
	c_put_str(TERM_ORANGE, "Heavy stun  ", ROW_STUN, 0);
    }
    else if (s) {
	c_put_str(TERM_ORANGE, "Stun        ", ROW_STUN, 0);
    }
    else {
	put_str("            ", ROW_STUN, 0);
    }
}


/*
 * Stun the player
 */
void stun_player(int s)
{
    /* Hack -- no stunning */
    if (p_ptr->resist_sound) return;

    /* Total stun */
    s = p_ptr->stun + s;

    /* Save the stun */
    p_ptr->stun = (s < 0) ? 0 : (s < 500) ? s : 500;

    /* Describe the stun */
    if (s > 100) {
	msg_print("You have been knocked out.");
    }
    else if (s > 50) {
	msg_print("You have been heavily stunned.");
    }
    else if (s) {
	msg_print("You have been stunned.");
    }

    /* Hack -- recalculate the bonuses */
    p_ptr->update |= PU_BONUS;
}


/*
 * Display entire block
 */
static void prt_stat_block()
{
    int          i;

    /* Dump the race and class and title */
    prt_field(rp_ptr->trace, ROW_RACE, 0);
    prt_field(cp_ptr->title, ROW_CLASS, 0);

    /* Dump the title */
    prt_title();

    /* Equippy chars (if enabled) */
    if (equippy_chars) prt_equippy_chars();

    /* Show monster health (if enabled) */
    if (show_health_bar) health_redraw();

    /* Dump stats */
    for (i = 0; i < 6; i++) prt_stat(i);

    prt_level();

    prt_exp();

    if (p_ptr->pclass != 0) {
	put_str("Mana        ", ROW_MANA, 0);
	prt_cmana();
    }

    put_str("Max HP      ", ROW_MAXHP, 0);
    prt_mhp();

    put_str("Cur HP      ", ROW_CURHP, 0);
    prt_chp();

    put_str("AC          ", ROW_AC, 0);
    prt_pac();

    put_str("AU          ", ROW_GOLD, 0);
    prt_gold();

    prt_winner();
    prt_cut();
    prt_stun();
    prt_study();

    prt_hunger();
    prt_blind();
    prt_confused();
    prt_afraid();
    prt_poisoned();

    prt_state();

    /* Hack -- always print speed */
    prt_speed();

    /* Hack -- always print depth */
    prt_depth();
}




/*
 * Print long number with header at given row, column
 * Use the color for the number, not the header
 */
static void prt_lnum(cptr header, u32b num, int row, int col, byte color)
{
    int len = strlen(header);
    char out_val[32];
    put_str(header, row, col);
    (void)sprintf(out_val, "%9ld", (long)num);
    c_put_str(color, out_val, row, col + len);
}

/*
 * Print number with header at given row, column
 */
static void prt_num(cptr header, int num, int row, int col, byte color)
{
    int len = strlen(header);
    char out_val[32];
    put_str(header, row, col);
    put_str("   ", row, col + len);
    (void)sprintf(out_val, "%6ld", (long)num);
    c_put_str(color, out_val, row, col + len + 3);
}



/*
 * Prints the following information on the screen.      -JWT-   
 */
void put_character()
{
    clear_screen();

    put_str("Name        :", 2, 1);
    put_str("Sex         :", 3, 1);
    put_str("Race        :", 4, 1);
    put_str("Class       :", 5, 1);

    c_put_str(TERM_L_BLUE, player_name, 2, 15);
    c_put_str(TERM_L_BLUE, (p_ptr->male ? "Male" : "Female"), 3, 15);
    c_put_str(TERM_L_BLUE, rp_ptr->trace, 4, 15);
    c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);
}



/*
 * Prints some information on the screen
 */
void put_stats()
{
    int         i;

    char        buf[80];


    /* Display the stats */
    for (i = 0; i < 6; i++) {

	/* Special treatment of "injured" stats */
	if (p_ptr->cur_stat[i] < p_ptr->max_stat[i]) {

	    /* Use lowercase stat name */
	    put_str(stat_names_reduced[i], 2 + i, 61);

	    /* Display the stat itself */
	    cnv_stat(p_ptr->use_stat[i], buf);
	    c_put_str(TERM_YELLOW, buf, 2 + i, 66);

	    /* Display the maximum stat */
	    cnv_stat(modify_max_stat(i, p_ptr->mod_stat[i]), buf);

	    /* Display the maximum stat */
	    c_put_str(TERM_L_GREEN, buf, 2 + i, 73);
	}

	else {

	    /* Assume uppercase stat name */
	    put_str(stat_names[i], 2 + i, 61);

	    /* Display the normal stat */
	    cnv_stat(p_ptr->use_stat[i], buf);
	    c_put_str(TERM_L_GREEN, buf, 2 + i, 66);
	}
    }
}


/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;


/*
 * Returns a "rating" of x depending on y
 */
cptr likert(int x, int y)
{
    /* Paranoia */
    if (y <= 0) y = 1;
    
    /* Negative value */
    if (x < 0) {
	likert_color = TERM_RED;
	return ("Very Bad");
    }

    /* Analyze the value */
    switch ((x / y)) {
      case 0:
      case 1:
	likert_color = TERM_RED;
	return ("Bad");
      case 2:
	likert_color = TERM_RED;
	return ("Poor");
      case 3:
      case 4:
	likert_color = TERM_YELLOW;
	return ("Fair");
      case 5:
	likert_color = TERM_YELLOW;
	return ("Good");
      case 6:
	likert_color = TERM_YELLOW;
	return ("Very Good");
      case 7:
      case 8:
	likert_color = TERM_L_GREEN;
	return ("Excellent");
      case 9:
      case 10:
      case 11:
      case 12:
      case 13:
	likert_color = TERM_L_GREEN;
	return ("Superb");
      case 14:
      case 15:
      case 16:
      case 17:
	likert_color = TERM_L_GREEN;
	return ("Heroic");
      default:
	likert_color = TERM_L_GREEN;
	return ("Legendary");
    }
}


/*
 * Prints age, height, weight, and Social Class
 */
static void put_misc1()
{
    prt_num("Age          ", (int)p_ptr->age, 2, 32, TERM_L_BLUE);
    prt_num("Height       ", (int)p_ptr->ht, 3, 32, TERM_L_BLUE);
    prt_num("Weight       ", (int)p_ptr->wt, 4, 32, TERM_L_BLUE);
    prt_num("Social Class ", (int)p_ptr->sc, 5, 32, TERM_L_BLUE);
}


/*
 * Prints the following information on the screen.
 *
 * For this to look right, the following should be spaced the
 * same as in the prt_lnum code... -CFT
 */
static void put_misc2()
{
    int show_tohit = p_ptr->dis_th;
    int show_todam = p_ptr->dis_td;

    inven_type *i_ptr = &inventory[INVEN_WIELD];

    /* Hack -- add in weapon info if known */
    if (inven_known_p(i_ptr)) show_tohit += i_ptr->tohit;
    if (inven_known_p(i_ptr)) show_todam += i_ptr->todam;

    /* Dump the bonuses to hit/dam */
    prt_num("+ To Hit    ", show_tohit, 9, 1, TERM_L_BLUE);
    prt_num("+ To Damage ", show_todam, 10, 1, TERM_L_BLUE);

    /* Dump the armor class bonus */
    prt_num("+ To AC     ", p_ptr->dis_ta, 11, 1, TERM_L_BLUE);

    /* Dump the total armor class */
    prt_num("  Total AC  ", p_ptr->dis_ac, 12, 1, TERM_L_BLUE);

    prt_num("Level      ", (int)p_ptr->lev, 9, 28, TERM_L_GREEN);

    prt_lnum("Experience ", p_ptr->exp, 10, 28,
	     (p_ptr->exp == p_ptr->max_exp) ? TERM_L_GREEN : TERM_YELLOW);

    prt_lnum("Max Exp    ", p_ptr->max_exp, 11, 28, TERM_L_GREEN);

    if (p_ptr->lev >= MAX_PLAYER_LEVEL) {
	put_str("Exp to Adv.     ", 12, 28);
	c_put_str(TERM_L_GREEN, "****", 12, 28+16);
    }
    else {
	prt_lnum("Exp to Adv.",
	    (s32b)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L),
	    12, 28, TERM_L_GREEN);
    }

    prt_lnum("Gold       ", p_ptr->au, 13, 28, TERM_L_GREEN);

    prt_num("Max Hit Points ", p_ptr->mhp, 9, 52, TERM_L_GREEN);

    if (p_ptr->mhp == p_ptr->chp) {
	prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_L_GREEN);
    }
    else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10) {
	prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_YELLOW);
    }
    else {
	prt_num("Cur Hit Points ", p_ptr->chp, 10, 52, TERM_RED);
    }

    prt_num("Max Mana       ", p_ptr->mana, 11, 52, TERM_L_GREEN);

    if (p_ptr->mana == p_ptr->cmana) {
	prt_num("Cur Mana       ", p_ptr->cmana, 12, 52, TERM_L_GREEN);
    }
    else if (p_ptr->cmana > (p_ptr->mana * hitpoint_warn) / 10) {
	prt_num("Cur Mana       ", p_ptr->cmana, 12, 52, TERM_YELLOW);
    }
    else {
	prt_num("Cur Mana       ", p_ptr->cmana, 12, 52, TERM_RED);
    }
}


/*
 * Prints ratings on certain abilities
 *
 * This code is "repeated" elsewhere to "dump" a character sheet.
 */
static void put_misc3()
{
    int                 tmp;
    int                 xthn, xthb, xfos, xsrh;
    int                 xdis, xdev, xsav, xstl;
    char                xinfra[32];
    cptr                desc;

    /* Fighting Skill (with current weapon) */
    tmp = p_ptr->ptohit + inventory[INVEN_WIELD].tohit;
    xthn = p_ptr->skill_thn + (tmp * BTH_PLUS_ADJ);

    /* Shooting Skill (with current bow) */
    tmp = p_ptr->ptohit + inventory[INVEN_BOW].tohit;
    xthb = p_ptr->skill_thb + (tmp * BTH_PLUS_ADJ);

    /* Basic abilities */
    xdis = p_ptr->skill_dis;
    xdev = p_ptr->skill_dev;
    xsav = p_ptr->skill_sav;
    xstl = p_ptr->skill_stl;
    xsrh = p_ptr->skill_srh;
    xfos = p_ptr->skill_fos;

    /* Infravision string */
    (void)sprintf(xinfra, "%d feet", p_ptr->see_infra * 10);

    /* Clear it */
    clear_from(14);

    put_str("(Miscellaneous Abilities)", 15, 25);

    put_str("Fighting    :", 16, 1);
    desc=likert(xthn, 12);
    c_put_str(likert_color, desc, 16, 15);

    put_str("Stealth     :", 16, 28);
    desc=likert(xstl, 1);
    c_put_str(likert_color, desc, 16, 42);

    put_str("Perception  :", 16, 55);
    desc=likert(xfos, 6);
    c_put_str(likert_color, desc, 16, 69);

    put_str("Bows/Throw  :", 17, 1);
    desc=likert(xthb, 12);
    c_put_str(likert_color, desc, 17, 15);

    put_str("Disarming   :", 17, 28);
    desc=likert(xdis, 8);
    c_put_str(likert_color, desc, 17, 42);

    put_str("Searching   :", 17, 55);
    desc=likert(xsrh, 6);
    c_put_str(likert_color, desc, 17, 69);

    put_str("Saving Throw:", 18, 1);
    desc=likert(xsav, 6);
    c_put_str(likert_color, desc, 18, 15);

    put_str("Magic Device:", 18, 28);
    desc=likert(xdev, 6);
    c_put_str(likert_color, desc, 18, 42);

    put_str("Infra-Vision:", 18, 55);
    c_put_str(TERM_WHITE, xinfra, 18, 69);
}


/*
 * Used to display the character on the screen.
 */
void display_player()
{
    put_character();
    put_stats();
    put_misc1();
    put_misc2();
    put_misc3();
}



/*
 * Redraw the "monster health bar"      -DRS-
 * Rather extensive modifications by    -BEN-
 *
 * The "monster health bar" is drawn instead of the "equippy chars"
 * and provides visual feedback on the "health" of the "tracked"
 * monster.  Attacking a monster auto-tracks it, and otherwise the
 * health bat tracks the current target monster if any.
 *
 * This is a spoiler, but not a major one, since you can get most
 * of that information by "looking" at the monster.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

    /* Disabled */
    if (!show_health_bar) return;

    /* Hack -- not tracking, auto-track current "target" */
    if (!health_who) {

	/* Examine legal current monster targets */
	if ((target_who > 0) && target_okay()) {

	    /* Auto-track */
	    health_who = target_who;
	}
    }

    /* Not tracking */
    if (!health_who) {

	/* Erase the health bar */
	Term_erase(0, ROW_EQUIPPY, 11, ROW_EQUIPPY);

	/* Mega-Hack -- Equippy chars (if enabled) */
	if (equippy_chars) prt_equippy_chars();
    }

    /* Tracking an unseen monster */
    else if (!m_list[health_who].ml) {

	/* Indicate that the monster health is "unknown" */
	Term_putstr(0, ROW_EQUIPPY, 12, TERM_WHITE, "[----------]");
    }

    /* Tracking a dead monster */
    else if (!m_list[health_who].hp < 0) {

	/* Indicate that the monster health is "unknown" */
	Term_putstr(0, ROW_EQUIPPY, 12, TERM_WHITE, "[----------]");
    }

    /* Tracking a visible monster */
    else {

	int pct, len;

	monster_type *m_ptr = &m_list[health_who];

	/* Default to almost dead */
	byte attr = TERM_RED;

	/* Extract the "percent" of health */
	pct = 100L * m_ptr->hp / m_ptr->maxhp;

	/* Badly wounded */
	if (pct >= 10) attr = TERM_L_RED;

	/* Wounded */
	if (pct >= 25) attr = TERM_ORANGE;

	/* Somewhat Wounded */
	if (pct >= 60) attr = TERM_YELLOW;

	/* Healthy */
	if (pct >= 100) attr = TERM_L_GREEN;

	/* Afraid */
	if (m_ptr->monfear) attr = TERM_VIOLET;

	/* Asleep */
	if (m_ptr->csleep) attr = TERM_BLUE;

	/* Convert percent into "health" */
	len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

	/* Default to "unknown" */
	Term_putstr(0, ROW_EQUIPPY, 12, TERM_WHITE, "[----------]");

	/* Dump the current "health" (use '*' symbols) */
	Term_putstr(1, ROW_EQUIPPY, len, attr, "**********");
    }

#endif

}

/*
 * Track a new monster
 */
void health_track(int m_idx)
{
    /* Track a new guy */
    health_who = m_idx;

    /* Redraw (later) */
    p_ptr->redraw |= (PR_HEALTH);
}



/*
 * Increases hit points and level                       -RAK-   
 */
static void gain_level(void)
{
    char        out_val[80];

    /* Increase the level */
    p_ptr->lev++;

    /* Save the highest level */
    if (p_ptr->lev > p_ptr->max_plv) p_ptr->max_plv = p_ptr->lev;

    /* Sound */
    sound(SOUND_LEVEL);
    
    /* Message */
    (void)sprintf(out_val, "Welcome to level %d.", (int)p_ptr->lev);
    msg_print(out_val);

    /* Update some stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    
    /* Redraw some stuff */
    p_ptr->redraw |= (PR_BLOCK);
    
    /* Handle stuff */
    handle_stuff();
}


/*
 * Advance experience levels and print experience
 */
void check_experience()
{
    /* Hack -- maximal limit */
    if (p_ptr->exp > MAX_EXP) p_ptr->exp = MAX_EXP;

    /* Hack -- maximal limit */
    if (p_ptr->max_exp > MAX_EXP) p_ptr->max_exp = MAX_EXP;
    
    /* Gain levels while possible */
    while ((p_ptr->lev < MAX_PLAYER_LEVEL) &&
	   ((player_exp[p_ptr->lev-1] * p_ptr->expfact / 100L) <= p_ptr->exp)) {

	/* Gain a level */
	gain_level();
    }

    /* Keep track of maximal experience */
    if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

    /* Display experience */
    prt_exp();
}





/*
 * Stat Table (CON) -- extra tenth-hitpoints per level
 */
static s16b adj_con_mhp[45] = {
    -40  /* 3 */,
    -30  /* 4 */,
    -20  /* 5 */,
    -10  /* 6 */,
    -5   /* 7 */,
    -2   /* 8 */,
    -1   /* 9 */,
    0   /* 10 */,
    0   /* 11 */,
    0   /* 12 */,
    0   /* 13 */,
    2   /* 14 */,
    4   /* 15 */,
    7   /* 16 */,
    9   /* 17 */,
    13   /* 18 */,
    15   /* 18/01-18/09 */,
    17   /* 18/10-18/19 */,
    18   /* 18/20-18/29 */,
    19   /* 18/30-18/39 */,
    20   /* 18/40-18/49 */,
    23   /* 18/50-18/59 */,
    27   /* 18/60-18/69 */,
    30   /* 18/70-18/79 */,
    32   /* 18/80-18/89 */,
    35   /* 18/90-18/94 */,
    38   /* 18/95 */,
    40   /* 18/96 */,
    40   /* 18/97 */,
    42   /* 18/98 */,
    44   /* 18/99 */,
    47   /* 18/100 */,
    50  /* 18/101-18/109 */,
    60  /* 18/110-18/119 */,
    65  /* 18/120-18/129 */,
    70  /* 18/130-18/139 */,
    80  /* 18/140-18/149 */,
    90  /* 18/150-18/159 */,
    100  /* 18/160-18/169 */,
    110  /* 18/170-18/179 */,
    120  /* 18/180-18/189 */,
    125  /* 18/190-18/199 */,
    130  /* 18/200-18/209 */,
    135  /* 18/210-18/219 */,
    140  /* 18/220+ */
};


#if 0

/*
 * Returns a character's adjustment to hit points
 */
int con_adj()
{
    int con = p_ptr->use_stat[A_CON];

    if (con < 7) return (con - 7);
    if (con < 17) return (0);
    if (con < 18) return (1);
    if (con < 18+76) return (2);
    if (con < 18+99) return (3);
    if (con < 18+101) return (4);
    if (con < 18+110) return (5);
    if (con < 18+120) return (6);
    if (con < 18+140) return (7);
    if (con < 18+150) return (8);
    if (con < 18+160) return (9);
    if (con < 18+170) return (10);
    if (con < 18+180) return (11);
    if (con < 18+190) return (12);
    if (con < 18+210) return (13);
    return (14);
}

#endif



/*
 * Returns spell chance of failure for spell            -RAK-   
 */
int spell_chance(int spell)
{
    int         chance, minfail;
    spell_type  *s_ptr;

    /* Paranoia -- warriors never succeed */
    if (p_ptr->pclass == 0) return (100);

    /* Access the spell */
    s_ptr = &magic_spell[p_ptr->pclass-1][spell];

    /* Extract the base spell failure rate */
    chance = s_ptr->sfail;
	
    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (p_ptr->lev - s_ptr->slevel);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= 3 * (adj_mag_stat[stat_index(cp_ptr->spell_stat)] - 1);

    /* Not enough mana to cast */
    if (s_ptr->smana > p_ptr->cmana) {
	chance += 5 * (s_ptr->smana - p_ptr->cmana);
    }

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[stat_index(cp_ptr->spell_stat)];
    
    /* Non mage/priest characters never get too good */
    if ((p_ptr->pclass != 1) && (p_ptr->pclass != 2)) {
	if (minfail < 5) minfail = 5;
    }

    /* Hack -- Priest prayer penalty for "edged" weapons  -DGK */
    if (p_ptr->pclass == 2) {
	inven_type *i_ptr;
	i_ptr = &inventory[INVEN_WIELD];
	if ((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_POLEARM)) {
	    if (!(i_ptr->flags3 & TR3_BLESSED)) {
		chance += 25;
	    }
	}
    }

    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder */
    if (p_ptr->stun > 50) chance += 25;
    else if (p_ptr->stun) chance += 15;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);
}




/*
 * Calculate number of spells player should have,
 * and learn/forget spells until that number is met -JEW-
 *
 * XXX XXX XXX Use "bit_pos()" in this function...
 */
void calc_spells(int stat)
{
    int                 i, k, j, index, levels;
    int                 num_allowed, new_spells, num_known;

    u32b                mask, spell_flag;

    spell_type          *s_ptr;

    cptr                p;

    char                tmp_str[80];


    /* Spell array index (could use "cp_ptr->spell_type") */
    index = (cp_ptr->spell_stat == A_INT) ? 0 : 1;

    /* Name of spells of this type */
    p = (cp_ptr->spell_stat == A_INT) ? "spell" : "prayer";


    /* XXX Hack -- this technically runs in the "wrong" order */

    /* Check to see if know any spells greater than level, eliminate them */
    for (i = 31; i >= 0; i--) {

	mask = 1L << i;

	if (mask & spell_learned1) {
	    s_ptr = &magic_spell[p_ptr->pclass-1][i];
	    if (s_ptr->slevel > p_ptr->lev) {
		spell_learned1 &= ~mask;
		spell_forgotten1 |= mask;
		(void)sprintf(tmp_str, "You have forgotten the %s of %s.", p,
			      spell_names[index][i]);
		msg_print(tmp_str);
	    }
	}

	if (mask & spell_learned2) {
	    s_ptr = &magic_spell[p_ptr->pclass-1][i+32];
	    if (s_ptr->slevel > p_ptr->lev) {
		spell_learned2 &= ~mask;
		spell_forgotten2 |= mask;
		(void)sprintf(tmp_str, "You have forgotten the %s of %s.", p,
			      spell_names[index][i + 32]);
		msg_print(tmp_str);
	    }
	}
    }


    /* Determine the number of spells allowed */
    levels = p_ptr->lev - cp_ptr->spell_first + 1;

    /* Extract allowed spells */
    num_allowed = (adj_mag_study[stat_index(stat)] * levels) / 10;

    /* Count the number of spells we know */
    num_known = 0;
    for (i = 0; i < 32; i++) {
	mask = 1L << i;
	if (mask & spell_learned1) num_known++;
	if (mask & spell_learned2) num_known++;
    }

    /* See how many spells we must forget or may learn */
    new_spells = num_allowed - num_known;


    /* We can learn some forgotten spells */
    if (new_spells > 0) {

	/* Remember spells that were forgetten */
	for (i = 0; new_spells > 0; i++) {

	    /* No spells left to remember */
	    if (i >= 64) break;

	    /* Not allowed to remember any more */
	    if (i >= num_allowed) break;

	    /* No more forgotten spells to remember */
	    if (!spell_forgotten1 && !spell_forgotten2) break;

	    /* Get the (i+1)th spell learned */
	    j = spell_order[i];

	    /* Don't process unknown spells... -CFT */
	    if (j == 99) continue;

	    /* First set of spells */
	    if (j < 32) {
		mask = 1L << j;
		if (mask & spell_forgotten1) {
		    s_ptr = &magic_spell[p_ptr->pclass-1][j];
		    if (s_ptr->slevel <= p_ptr->lev) {
			spell_forgotten1 &= ~mask;
			spell_learned1 |= mask;
			new_spells--;
			sprintf(tmp_str, "You have remembered the %s of %s.",
				p, spell_names[index][j]);
			msg_print(tmp_str);
		    }
		    else {
			/* if was too high lv to remember */
			num_allowed++;
		    }
		}
	    }

	    /* Second set of spells */
	    else {
		mask = 1L << (j - 32);
		if (mask & spell_forgotten2) {
		    s_ptr = &magic_spell[p_ptr->pclass-1][j];
		    if (s_ptr->slevel <= p_ptr->lev) {
			spell_forgotten2 &= ~mask;
			spell_learned2 |= mask;
			new_spells--;
			sprintf(tmp_str, "You have remembered the %s of %s.",
				p, spell_names[index][j]);
			msg_print(tmp_str);
		    }
		    else {
			/* if was too high lv to remember */
			num_allowed++;
		    }
		}
	    }
	}
    }


    /* Learn some new spells */
    if (new_spells > 0) {

	/* Count the spells */
	k = 0;

	/* Count remaining learnable spells */
	spell_flag = 0xFFFFFFFFL & ~spell_learned1;
	while (spell_flag) {
	    j = bit_pos(&spell_flag);
	    s_ptr = &magic_spell[p_ptr->pclass-1][j];
	    if (s_ptr->slevel <= p_ptr->lev) k++;
	}

	/* Count remaining learnable spells */
	spell_flag = 0xFFFFFFFFL & ~spell_learned2;
	while (spell_flag) {
	    j = bit_pos(&spell_flag) + 32;
	    s_ptr = &magic_spell[p_ptr->pclass-1][j];
	    if (s_ptr->slevel <= p_ptr->lev) k++;
	}

	/* Cannot learn more spells than exist */
	if (new_spells > k) new_spells = k;
    }


    /* Forget spells */
    if (new_spells < 0) {

	/* Forget spells in the opposite order they were learned */
	for (i = 63; new_spells < 0; i--) {

	    /* Hack -- we might run out of spells to forget */
	    if (!spell_learned1 && !spell_learned2) {
		new_spells = 0;
		break;
	    }

	    /* Get the (i+1)th spell learned */
	    j = spell_order[i];

	    /* don't process unknown spells... -CFT */
	    if (j == 99) continue;

	    /* First set of spells */
	    if (j < 32) {
		mask = 1L << j;
		if (mask & spell_learned1) {
		    spell_learned1 &= ~mask;
		    spell_forgotten1 |= mask;
		    new_spells++;
		    sprintf(tmp_str, "You have forgotten the %s of %s.",
			    p, spell_names[index][j]);
		    msg_print(tmp_str);
		}
	    }

	    /* Assume second set of spells */
	    else {
		mask = 1L << (j - 32);
		if (mask & spell_learned2) {
		    spell_learned2 &= ~mask;
		    spell_forgotten2 |= mask;
		    new_spells++;
		    sprintf(tmp_str, "You have forgotten the %s of %s.",
			    p, spell_names[index][j]);
		    msg_print(tmp_str);
		}
	    }
	}

	/* Paranoia -- in case we run out of spells to forget */
	new_spells = 0;
    }


    /* Hack -- no messages during character generation */
    if (!character_generated) return;
    
    /* Take note when "study" changes */
    if (new_spells != p_ptr->new_spells) {

	/* Player can learn new spells now */
	if ((new_spells > 0) && (p_ptr->new_spells == 0)) {
	    (void)sprintf(tmp_str, "You can learn some new %ss now.", p);
	    msg_print(tmp_str);
	}

	/* Save the new_spells value */
	p_ptr->new_spells = new_spells;

	/* Display "study state" later */
	p_ptr->redraw |= (PR_STUDY);
    }
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or innapropriate) armor.
 */
void calc_mana(int stat)
{
    int         new_mana, levels, cur_wgt, max_wgt;

    inven_type  *i_ptr;

    static bool cumber_armor = FALSE;
    static bool cumber_glove = FALSE;

    bool old_glove = cumber_glove;
    bool old_armor = cumber_armor;


    /* Extract "effective" player level */
    levels = (p_ptr->lev - cp_ptr->spell_first) + 1;

    /* Extract total mana */
    new_mana = (adj_mag_mana[stat_index(stat)] * levels) / 10;

    /* Hack -- usually add one mana */
    if (new_mana) new_mana++;


#if 0
    /* No spells learned, thus no mana */
    if (!spell_learned1 && !spell_learned2) new_mana = 0;
#endif


    /* Assume player is not encumbered by gloves */
    cumber_glove = FALSE;

    /* Get the gloves */
    i_ptr = &inventory[INVEN_HANDS];

    /* Normal gloves hurt mage-type spells */
    if (i_ptr->tval &&
	!(i_ptr->flags2 & TR2_FREE_ACT) &&
	!((i_ptr->flags1 & TR1_DEX) && (i_ptr->pval > 0))) {

	/* Only non-dragon mages are affected (dragons have a ring there)*/
	if (stat == A_INT && p_ptr->prace<MIN_DRAGON) {

	    /* Encumbered */
	    cumber_glove = TRUE;

	    /* Reduce mana */
	    new_mana = (3 * new_mana) / 4;
	}
    }

    /* Message */
    if (cumber_glove && !old_glove) {
	msg_print("Your covered hands feel unsuitable for spellcasting.");
    }
    if (old_glove && !cumber_glove) {
	msg_print("Your hands feel more suitable for spellcasting.");
    }


    /* Assume player not encumbered by armor */
    cumber_armor = FALSE;

    /* Weigh the armor */
    cur_wgt = 0;
    cur_wgt += inventory[INVEN_BODY].weight;
    cur_wgt += inventory[INVEN_HEAD].weight;
    cur_wgt += inventory[INVEN_ARM].weight;
    cur_wgt += inventory[INVEN_OUTER].weight;
    cur_wgt += inventory[INVEN_HANDS].weight;
    cur_wgt += inventory[INVEN_FEET].weight;

    /* Determine the weight allowance */
    max_wgt = cp_ptr->spell_weight;

    /* Too much armor */
    if (cur_wgt > max_wgt) {

	/* Encumbered */
	cumber_armor = TRUE;

	/* Reduce mana by 1% per lb */
	new_mana -= (int)(((long)new_mana * (long)(cur_wgt - max_wgt)) / 1000);
    }

    /* Message */
    if (cumber_armor && !old_armor) {
	msg_print("The weight of your armor encumbers your movement.");
    }
    if (old_armor && !cumber_armor) {
	msg_print("You feel able to move more freely.");
    }


    /* Mana can never be negative */
    if (new_mana < 0) new_mana = 0;
	
	
    /* Maximum mana has changed */
    if (p_ptr->mana != new_mana) {

	/* Player has no mana now */
	if (!new_mana) {

	    /* No mana left */
	    p_ptr->cmana = 0;
	    p_ptr->cmana_frac = 0;
	}

	/* Player had no mana, has some now */
	else if (!p_ptr->mana) {

	    /* Reset mana */
	    p_ptr->cmana = new_mana;
	    p_ptr->cmana_frac = 0;
	}

	/* Player had some mana, adjust current mana */
	else {

	    s32b value;

	    /* change current mana proportionately to change of max mana, */
	    /* divide first to avoid overflow, little loss of accuracy */
	    value = ((((long)p_ptr->cmana << 16) + p_ptr->cmana_frac) /
		     p_ptr->mana * new_mana);

	    /* Extract mana components */
	    p_ptr->cmana = (value >> 16);
	    p_ptr->cmana_frac = (value & 0xFFFF);
	}

	/* Save new mana */
	p_ptr->mana = new_mana;

	/* Display mana later */
	p_ptr->redraw |= (PR_MANA);
    }
}



/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints()
{
    int bonus, mhp;

    /* Compute extra half-hitpoint bonus */
    bonus = adj_con_mhp[stat_index(A_CON)] * p_ptr->lev / 10;

    /* Calculate hitpoints */
    mhp = player_hp[p_ptr->lev-1] + bonus;
    
    /* Always have at least one hitpoint per level */
    if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

    /* Factor in the hero / superhero settings */
    if (p_ptr->hero) mhp += 10;
    if (p_ptr->shero) mhp += 30;

    /* New maximum hitpoints */
    if (mhp != p_ptr->mhp) {

	s32b value;

	/* change current hit points proportionately to change of mhp */
	/* divide first to avoid overflow, little loss of accuracy */
	value = (((long)p_ptr->chp << 16) + p_ptr->chp_frac) / p_ptr->mhp;
	value = value * mhp;
	p_ptr->chp = (value >> 16);
	p_ptr->chp_frac = (value & 0xFFFF);

	/* Save the new max-hitpoints */
	p_ptr->mhp = mhp;

	/* Display hitpoints (later) */
	p_ptr->redraw |= PR_HP;
    }
}


/*
 * Current weapon is not priestly
 */
static int notlike = FALSE;

/*
 * Current weapon is too heavy
 */
static int heavy_weapon = FALSE;

/*
 * Current bow is too heavy
 */
static int heavy_bow = FALSE;


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
 * and helps because now carrying 300 pounds is not really very
 * painful (4/5 the speed of a normal kobold).
 *
 * Note that the "weapon" and "bow" do *not* add to the bonuses to hit
 * or to damage, since that would affect non-combat things.
 */
static void calc_bonuses(void)
{
    u32b                item_flags1, item_flags2, item_flags3;

    int                 i, j, old_dis_ac, old_mod[6];

    inven_type          *i_ptr;


    /* Clear all the flags */
    p_ptr->see_inv = FALSE;
    p_ptr->teleport = FALSE;
    p_ptr->free_act = FALSE;
    p_ptr->slow_digest = FALSE;
    p_ptr->aggravate = FALSE;
    p_ptr->regenerate = FALSE;
    p_ptr->ffall = FALSE;
    p_ptr->hold_life = FALSE;
    p_ptr->telepathy = FALSE;
    p_ptr->lite = FALSE;
    p_ptr->sustain_str = FALSE;
    p_ptr->sustain_int = FALSE;
    p_ptr->sustain_wis = FALSE;
    p_ptr->sustain_con = FALSE;
    p_ptr->sustain_dex = FALSE;
    p_ptr->sustain_chr = FALSE;
    p_ptr->resist_fire = FALSE;
    p_ptr->resist_acid = FALSE;
    p_ptr->resist_cold = FALSE;
    p_ptr->resist_elec = FALSE;
    p_ptr->resist_pois = FALSE;
    p_ptr->resist_conf = FALSE;
    p_ptr->resist_sound = FALSE;
    p_ptr->resist_lite = FALSE;
    p_ptr->resist_dark = FALSE;
    p_ptr->resist_chaos = FALSE;
    p_ptr->resist_disen = FALSE;
    p_ptr->resist_shard = FALSE;
    p_ptr->resist_nexus = FALSE;
    p_ptr->resist_blind = FALSE;
    p_ptr->resist_neth = FALSE;
    p_ptr->resist_fear = FALSE;
    p_ptr->immune_fire = FALSE;
    p_ptr->immune_acid = FALSE;
    p_ptr->immune_pois = FALSE;
    p_ptr->immune_cold = FALSE;
    p_ptr->immune_elec = FALSE;


    /* Save the old armor class */
    old_dis_ac = p_ptr->dis_ac;

    /* Clear the stat modifiers */
    for (i = 0; i < 6; i++) {
	old_mod[i] = p_ptr->mod_stat[i];
	p_ptr->mod_stat[i] = 0;
    }


    /* Base infravision (purely racial) */
    p_ptr->see_infra = rp_ptr->infra;


    /* Base skill -- disarming */
    p_ptr->skill_dis = rp_ptr->rdis + cp_ptr->c_dis;

    /* Base skill -- magic devices */
    p_ptr->skill_dev = rp_ptr->rdev + cp_ptr->c_dev;

    /* Base skill -- saving throw */
    p_ptr->skill_sav = rp_ptr->rsav + cp_ptr->c_sav;

    /* Base skill -- stealth */
    p_ptr->skill_stl = rp_ptr->rstl + cp_ptr->c_stl;

    /* Base skill -- searching ability */
    p_ptr->skill_srh = rp_ptr->rsrh + cp_ptr->c_srh;

    /* Base skill -- searching frequency */
    p_ptr->skill_fos = rp_ptr->rfos + cp_ptr->c_fos;

    /* Base skill -- To Hit (normal) */
    p_ptr->skill_thn = rp_ptr->rthn + cp_ptr->c_thn;

    /* Base skill -- To Hit (bows) */
    p_ptr->skill_thb = rp_ptr->rthb + cp_ptr->c_thb;


    /* Displayed/Real Bonuses */
    p_ptr->dis_th = p_ptr->ptohit = 0;
    p_ptr->dis_td = p_ptr->ptodam = 0;
    p_ptr->dis_ta = p_ptr->ptoac = 0;

    /* Displayed/Real armor class */
    p_ptr->dis_ac = p_ptr->pac = 0;


    /* Start with "normal" digestion */
    p_ptr->food_digested = 2;

    /* Start with "normal" speed */
    p_ptr->pspeed = (p_ptr->prace < MIN_DRAGON?111:110);

    /* Elf */
    if (p_ptr->prace == ELF) p_ptr->resist_lite = TRUE;

    /* Hobbit */
    if (p_ptr->prace == HOBBIT) p_ptr->sustain_dex = TRUE;

    /* Gnome */
    if (p_ptr->prace == GNOME) p_ptr->free_act = TRUE;

    /* Dwarf */
    if (p_ptr->prace == DWARF) p_ptr->resist_blind = TRUE;

    /* Half-Orc */
    if (p_ptr->prace == HALFORC) p_ptr->resist_dark = TRUE;

    /* Half-Troll */
    if (p_ptr->prace == HALFTROLL) p_ptr->sustain_str = TRUE;

    /* Dunadan */
    if (p_ptr->prace == DUNADAN) p_ptr->sustain_con = TRUE;

    /* High Elf */
    if (p_ptr->prace == HIGHELF) p_ptr->resist_lite = TRUE;
    if (p_ptr->prace == HIGHELF) p_ptr->see_inv = TRUE;

    /* Pixie */
    if (p_ptr->prace == PIXIE) p_ptr->pspeed += 10;
    if (p_ptr->prace == PIXIE) p_ptr->ffall = TRUE;
    if (p_ptr->prace == PIXIE && p_ptr->lev > 40) p_ptr->telepathy = TRUE;

    /* Yeek */

    /* Crystal Dragon (dragons get a bunch, eh?)*/
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->free_act = TRUE;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->ffall = TRUE;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->resist_shard = TRUE;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->resist_fear = TRUE;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->pac += 25;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->ptoac += p_ptr->lev + 10;
    if (p_ptr->prace == CRYSTALDRAG) p_ptr->dis_ac += p_ptr->lev + 35;

    /* Copper Dragon */
    if (p_ptr->prace == COPPERDRAG) p_ptr->free_act = TRUE;
    if (p_ptr->prace == COPPERDRAG) p_ptr->ffall = TRUE;
    if (p_ptr->prace == COPPERDRAG) p_ptr->resist_disen = TRUE;
    if (p_ptr->prace == COPPERDRAG) p_ptr->resist_fear = TRUE;
    if (p_ptr->prace == COPPERDRAG) p_ptr->regenerate = TRUE;
    if (p_ptr->prace == COPPERDRAG) p_ptr->ptoac += p_ptr->lev + 10;
    if (p_ptr->prace == COPPERDRAG) p_ptr->dis_ac += p_ptr->lev + 10;

    /* Bronze Dragon */
    if (p_ptr->prace == BRONZEDRAG) p_ptr->ffall = TRUE;
    if (p_ptr->prace == BRONZEDRAG) p_ptr->resist_conf = TRUE;
    if (p_ptr->prace == BRONZEDRAG) p_ptr->resist_fear = TRUE;
    if (p_ptr->prace == BRONZEDRAG) p_ptr->regenerate = TRUE;
    if (p_ptr->prace == BRONZEDRAG) p_ptr->ptoac += p_ptr->lev + 10;
    if (p_ptr->prace == BRONZEDRAG) p_ptr->dis_ac += p_ptr->lev + 10;

    /* Gold Dragon */
    if (p_ptr->prace == GOLDDRAG) p_ptr->ffall = TRUE;
    if (p_ptr->prace == GOLDDRAG) p_ptr->resist_sound = TRUE;
    if (p_ptr->prace == GOLDDRAG) p_ptr->resist_fear = TRUE;
    if (p_ptr->prace == GOLDDRAG) p_ptr->regenerate = TRUE;
    if (p_ptr->prace == GOLDDRAG) p_ptr->ptoac += p_ptr->lev + 10;
    if (p_ptr->prace == GOLDDRAG) p_ptr->dis_ac += p_ptr->lev + 10;

    /* Pseudo Dragon */
    if (p_ptr->prace == PSEUDODRAG) p_ptr->ffall = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->resist_lite = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->resist_dark = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->resist_blind = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->resist_fear = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->regenerate = TRUE;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->ptoac += p_ptr->lev + 10;
    if (p_ptr->prace == PSEUDODRAG) p_ptr->dis_ac += p_ptr->lev + 10;

    /* Hack -- apply racial/class stat maxes */
    if (p_ptr->maximize) {

	/* Apply the racial modifiers */
	for (i = 0; i < 6; i++) {

	    /* Modify the stats for "race" */
	    p_ptr->mod_stat[i] += rp_ptr->radj[i];

	    /* Modify the stats for "class" */
	    p_ptr->mod_stat[i] += cp_ptr->c_adj[i];
	}
    }


    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {

	i_ptr = &inventory[i];

	/* Skip missing items */
	if (!i_ptr->tval) continue;

	/* Affect stats */
	if (i_ptr->flags1 & TR1_STR) p_ptr->mod_stat[A_STR] += i_ptr->pval;
	if (i_ptr->flags1 & TR1_INT) p_ptr->mod_stat[A_INT] += i_ptr->pval;
	if (i_ptr->flags1 & TR1_WIS) p_ptr->mod_stat[A_WIS] += i_ptr->pval;
	if (i_ptr->flags1 & TR1_DEX) p_ptr->mod_stat[A_DEX] += i_ptr->pval;
	if (i_ptr->flags1 & TR1_CON) p_ptr->mod_stat[A_CON] += i_ptr->pval;
	if (i_ptr->flags1 & TR1_CHR) p_ptr->mod_stat[A_CHR] += i_ptr->pval;

	/* Affect speed */
	if (i_ptr->flags1 & TR1_SPEED) p_ptr->pspeed += i_ptr->pval;

	/* Affect infravision */
	if (i_ptr->flags1 & TR1_INFRA) p_ptr->see_infra += i_ptr->pval;

	/* Affect stealth */
	if (i_ptr->flags1 & TR1_STEALTH) p_ptr->skill_stl += i_ptr->pval;

	/* Affect searching ability (factor of five) */
	if (i_ptr->flags1 & TR1_SEARCH) p_ptr->skill_srh += i_ptr->pval * 5;

	/* Affect searching frequency (factor of five) */
	if (i_ptr->flags1 & TR1_SEARCH) p_ptr->skill_fos += i_ptr->pval * 5;

	/* Modify the base armor class */
	p_ptr->pac += i_ptr->ac;

	/* The base armor class is always known */
	p_ptr->dis_ac += i_ptr->ac;

	/* Apply the bonuses to armor class */
	p_ptr->ptoac += i_ptr->toac;

	/* Apply the mental bonuses to armor class, if known */
	if (inven_known_p(i_ptr)) p_ptr->dis_ta += i_ptr->toac;

	/* Hack -- do not apply "weapon" bonuses */
	if (i == INVEN_WIELD) continue;

	/* Hack -- do not apply "bow" bonuses */
	if (i == INVEN_BOW) continue;

	/* Apply the bonuses to hit/damage */
	p_ptr->ptohit += i_ptr->tohit;
	p_ptr->ptodam += i_ptr->todam;

	if (i_ptr->tval == TV_HARD_ARMOR || i_ptr->tval == TV_SOFT_ARMOR)
	   p_ptr->pspeed += (i_ptr->tohit-1)/2;

	/* Apply the mental bonuses tp hit/damage, if known */
	if (inven_known_p(i_ptr)) p_ptr->dis_th += i_ptr->tohit;
	if (inven_known_p(i_ptr)) p_ptr->dis_td += i_ptr->todam;
    }


    /* Calculate the "total" stat values */
    for (i = 0; i < 6; i++) {

	/* Ignore non-changes */
	if (old_mod[i] == p_ptr->mod_stat[i]) continue;

	/* Save the new value for the stat */
	p_ptr->use_stat[i] = modify_stat(i, p_ptr->mod_stat[i]);

	/* Redisplay the stats later */
	p_ptr->redraw |= PR_STATS;
    }


    /* Apply temporary "stun" */
    if (p_ptr->stun > 50) {
	p_ptr->ptohit -= 20;
	p_ptr->dis_th -= 20;
	p_ptr->ptodam -= 20;
	p_ptr->dis_td -= 20;
    }
    else if (p_ptr->stun) {
	p_ptr->ptohit -= 5;
	p_ptr->dis_th -= 5;
	p_ptr->ptodam -= 5;
	p_ptr->dis_td -= 5;
    }

    /* Add in temporary spell increases */
    /* these changed from pac to ptoac, since mana now affected by */
    /* high pac (to simulate encumberence), and these really should */
    /* be magical bonuses anyway -CFT */

    if (p_ptr->invuln) {
	p_ptr->ptoac += 100;
	p_ptr->dis_ta += 100;
    }

    /* Temporary blessing */
    if (p_ptr->blessed) {
	p_ptr->ptoac += 5;
	p_ptr->dis_ta += 5;
	p_ptr->ptohit += 10;
	p_ptr->dis_th += 10;
    }

    /* Temprory shield */
    if (p_ptr->shield) {
	p_ptr->ptoac += 50;
	p_ptr->dis_ta += 50;
    }

    /* Temporary "Hero" */
    if (p_ptr->hero) {
	p_ptr->ptohit += 12;
	p_ptr->dis_th += 12;
    }

    /* Temporary "Beserk" */
    if (p_ptr->shero) {
	p_ptr->ptohit += 24;
	p_ptr->dis_th += 24;
	p_ptr->ptoac -= 10;
	p_ptr->dis_ta -= 10;
    }

    /* Temporary see invisible */
    if (p_ptr->detect_inv) {
	p_ptr->see_inv = TRUE;
    }

    /* Temporary infravision boost */
    if (p_ptr->tim_infra) {
	p_ptr->see_infra++;
    }

    /* Temporary "fast" */
    if (p_ptr->fast) {
	p_ptr->pspeed += 10;
    }

    /* Temporary "slow" */
    if (p_ptr->slow) {
	p_ptr->pspeed -= 10;
    }


    /* Extract the current weight */
    j = inven_weight;

    /* Extract the "weight limit" */
    i = weight_limit();

    /* XXX Hack -- Apply "encumbrance" from weight */
    if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));


    /* Searching slows the player down */
    if (p_ptr->searching) p_ptr->pspeed -= 10;

    /* Display the speed later */
    p_ptr->redraw |= (PR_SPEED);


    /* Check the item flags */
    item_flags1 = item_flags2 = item_flags3 = 0L;
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
	i_ptr = &inventory[i];
	item_flags1 |= i_ptr->flags1;
	item_flags2 |= i_ptr->flags2;
	item_flags3 |= i_ptr->flags3;
    }

    /* Process the item flags */
    if (TR3_SLOW_DIGEST & item_flags3) p_ptr->slow_digest = TRUE;
    if (TR3_AGGRAVATE & item_flags3) p_ptr->aggravate = TRUE;
    if (TR3_TELEPORT & item_flags3) p_ptr->teleport = TRUE;
    if (TR3_REGEN & item_flags3) p_ptr->regenerate = TRUE;
    if (TR3_TELEPATHY & item_flags3) p_ptr->telepathy = TRUE;
    if (TR3_LITE & item_flags3) p_ptr->lite = TRUE;
    if (TR3_SEE_INVIS & item_flags3) p_ptr->see_inv = TRUE;
    if (TR3_FEATHER & item_flags3) p_ptr->ffall = TRUE;
    if (TR2_FREE_ACT & item_flags2) p_ptr->free_act = TRUE;
    if (TR2_HOLD_LIFE & item_flags2) p_ptr->hold_life = TRUE;

    /* Immunity and resistance */
    if (TR2_IM_FIRE & item_flags2) p_ptr->immune_fire = TRUE;
    if (TR2_IM_ACID & item_flags2) p_ptr->immune_acid = TRUE;
    if (TR2_IM_COLD & item_flags2) p_ptr->immune_cold = TRUE;
    if (TR2_IM_ELEC & item_flags2) p_ptr->immune_elec = TRUE;
    if (TR2_IM_POIS & item_flags2) p_ptr->immune_pois = TRUE;
    if (TR2_RES_ACID & item_flags2) p_ptr->resist_acid = TRUE;
    if (TR2_RES_ELEC & item_flags2) p_ptr->resist_elec = TRUE;
    if (TR2_RES_FIRE & item_flags2) p_ptr->resist_fire = TRUE;
    if (TR2_RES_COLD & item_flags2) p_ptr->resist_cold = TRUE;
    if (TR2_RES_POIS & item_flags2) p_ptr->resist_pois = TRUE;
    if (TR2_RES_CONF & item_flags2) p_ptr->resist_conf = TRUE;
    if (TR2_RES_SOUND & item_flags2) p_ptr->resist_sound = TRUE;
    if (TR2_RES_LITE & item_flags2) p_ptr->resist_lite = TRUE;
    if (TR2_RES_DARK & item_flags2) p_ptr->resist_dark = TRUE;
    if (TR2_RES_CHAOS & item_flags2) p_ptr->resist_chaos = TRUE;
    if (TR2_RES_DISEN & item_flags2) p_ptr->resist_disen = TRUE;
    if (TR2_RES_SHARDS & item_flags2) p_ptr->resist_shard = TRUE;
    if (TR2_RES_NEXUS & item_flags2) p_ptr->resist_nexus = TRUE;
    if (TR2_RES_BLIND & item_flags2) p_ptr->resist_blind = TRUE;
    if (TR2_RES_NETHER & item_flags2) p_ptr->resist_neth = TRUE;

    /* New method for sustaining stats */
    if (item_flags2 & TR2_SUST_STR) p_ptr->sustain_str = TRUE;
    if (item_flags2 & TR2_SUST_INT) p_ptr->sustain_int = TRUE;
    if (item_flags2 & TR2_SUST_WIS) p_ptr->sustain_wis = TRUE;
    if (item_flags2 & TR2_SUST_DEX) p_ptr->sustain_dex = TRUE;
    if (item_flags2 & TR2_SUST_CON) p_ptr->sustain_con = TRUE;
    if (item_flags2 & TR2_SUST_CHR) p_ptr->sustain_chr = TRUE;


    /* Regeneration takes more food */
    if (p_ptr->regenerate) p_ptr->food_digested += 3;

    /* Slow digestion takes less food */
    if (p_ptr->slow_digest) p_ptr->food_digested--;

    /* Resting/Searching takes less food */
    if (p_ptr->rest || p_ptr->searching) p_ptr->food_digested--;


    /* Actual Modifier Bonuses */
    p_ptr->ptoac += adj_dex_ta[stat_index(A_DEX)];
    p_ptr->ptodam += adj_str_td[stat_index(A_STR)];
    p_ptr->ptohit += adj_dex_th[stat_index(A_DEX)];
    p_ptr->ptohit += adj_str_th[stat_index(A_STR)];

    /* Displayed Modifier Bonuses */
    p_ptr->dis_ta += adj_dex_ta[stat_index(A_DEX)];
    p_ptr->dis_td += adj_str_td[stat_index(A_STR)];
    p_ptr->dis_th += adj_dex_th[stat_index(A_DEX)];
    p_ptr->dis_th += adj_str_th[stat_index(A_STR)];


    /* Compute the total displayed armor */
    p_ptr->dis_ac += p_ptr->dis_ta;


    /* Hack -- always redraw armor */
    p_ptr->redraw |= (PR_ARMOR);


    /* Mega-Hack -- Recalculate hitpoints */
    p_ptr->update |= (PU_HP);

    /* Mega-Hack -- Recalculate spell info */
    p_ptr->update |= (PU_MANA | PU_SPELLS);


    /* Examine the "main weapon" */
    i_ptr = &inventory[INVEN_WIELD];


    /* Priest weapon penalty for non-blessed edged weapons */
    if ((p_ptr->pclass == 2) &&
	((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_POLEARM)) &&
	(!(i_ptr->flags3 & TR3_BLESSED))) {

	/* Reduce the real bonuses */
	p_ptr->ptohit -= 2;
	p_ptr->ptodam -= 2;

	/* Reduce the mental bonuses */
	p_ptr->dis_th -= 2;
	p_ptr->dis_td -= 2;

	/* Notice icky weapons */
	if (!notlike) {
	    msg_print("You do not feel comfortable with your weapon.");
	    notlike = TRUE;
	}
    }

    /* Check priest for newly comfortable weapon */
    else if (notlike) {
	notlike = FALSE;
	if (i_ptr->tval) {
	    msg_print("You feel comfortable with your weapon.");
	}
	else {
	    msg_print("You feel more comfortable after removing your weapon.");
	}
    }


    /* It is hard to hit with a heavy weapon */
    if (p_ptr->use_stat[A_STR] * 15 < i_ptr->weight) {

	/* Hard to wield a heavy weapon */
	p_ptr->ptohit += (p_ptr->use_stat[A_STR] * 15 - i_ptr->weight);
	p_ptr->dis_th += (p_ptr->use_stat[A_STR] * 15 - i_ptr->weight);

	/* Notice Heavy Weapon */
	if (!heavy_weapon) {
	    msg_print("You have trouble wielding such a heavy weapon.");
	    heavy_weapon = TRUE;
	}
    }

    /* Notice disappearance of Heavy Weapon */
    else if (heavy_weapon) {
	heavy_weapon = FALSE;
	if (i_ptr->tval) {
	    msg_print("You have no trouble wielding your weapon.");
	}
	else {
	    msg_print("You feel relieved to put down your heavy weapon.");
	}
    }


    /* XXX XXX XXX Examine the "current bow" */
    i_ptr = &inventory[INVEN_BOW];

    /* It is hard to carry a heavy bow */
    if (p_ptr->use_stat[A_STR] * 15 < i_ptr->weight) {

	/* Hard to wield a heavy bow */
	p_ptr->ptohit += (p_ptr->use_stat[A_STR] * 15 - i_ptr->weight);
	p_ptr->dis_th += (p_ptr->use_stat[A_STR] * 15 - i_ptr->weight);

	/* Notice Heavy Bow */
	if (!heavy_bow) {
	    msg_print("You have trouble wielding such a heavy bow.");
	    heavy_bow = TRUE;
	}
    }

    /* Notice disappearance of Heavy Bow */
    else if (heavy_bow) {
	heavy_bow = FALSE;
	if (i_ptr->tval) {
	    msg_print("You have no trouble wielding your bow.");
	}
	else {
	    msg_print("You feel relieved to put down your heavy bow.");
	}
    }


    /* Affect Skill -- disarming (DEX and INT) */
    p_ptr->skill_dis += adj_dex_dis[stat_index(A_DEX)];
    p_ptr->skill_dis += adj_int_dis[stat_index(A_INT)];

    /* Affect Skill -- magic devices (INT) */
    p_ptr->skill_dev += adj_int_dev[stat_index(A_INT)];

    /* Affect Skill -- saving throw (WIS) */
    p_ptr->skill_sav += adj_wis_sav[stat_index(A_WIS)];

    /* Affect Skill -- stealth (bonus one) */
    p_ptr->skill_stl += 1;
    

    /* Affect Skill -- disarming (Level, by Class) */
    p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

    /* Affect Skill -- magic devices (Level, by Class) */
    p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

    /* Affect Skill -- saving throw (Level, by Class) */
    p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

    /* Affect Skill -- stealth (Level, by Class) */
    p_ptr->skill_stl += (cp_ptr->x_stl * p_ptr->lev / 10);

    /* Affect Skill -- search ability (Level, by Class) */
    p_ptr->skill_srh += (cp_ptr->x_srh * p_ptr->lev / 10);

    /* Affect Skill -- search frequency (Level, by Class) */
    p_ptr->skill_fos += (cp_ptr->x_fos * p_ptr->lev / 10);

    /* Affect Skill -- to hit (normal) (Level, by Class) */
    p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);
    
    /* Affect Skill -- to hit (bows) (Level, by Class) */
    p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);
}



/*
 * Handle "p_ptr->update" and "p_ptr->redraw".
 */
void handle_stuff(void)
{
    /* Redraw recall window */
    if (p_ptr->redraw & PR_RECALL) {
	p_ptr->redraw &= ~PR_RECALL;
	if (use_recall_win && term_recall) {
	    roff_recall(-1);
	}
    }

    /* Redraw choice window */
    if (p_ptr->redraw & PR_CHOICE) {
	p_ptr->redraw &= ~PR_CHOICE;
	if (!choice_default) choice_inven(0, inven_ctr - 1);
	else choice_equip(INVEN_WIELD, INVEN_TOTAL - 1);
    }


    /* First pass */
    if (p_ptr->update) {
    
	if (p_ptr->update & PU_BONUS) {
	    p_ptr->update &= ~PU_BONUS;
	    calc_bonuses();
	}

	if (p_ptr->update & PU_HP) {
	    p_ptr->update &= ~PU_HP;
	    calc_hitpoints();
	}
	
	if (p_ptr->update & PU_MANA) {
	    p_ptr->update &= ~PU_MANA;
	    if (cp_ptr->spell_stat) calc_mana(cp_ptr->spell_stat);
	}
	
	if (p_ptr->update & PU_SPELLS) {
	    p_ptr->update &= ~PU_SPELLS;
	    if (cp_ptr->spell_stat) calc_spells(cp_ptr->spell_stat);
	}
    }
    

    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;
    
    
    /* Character is in "icky" mode, no screen updates */
    if (character_icky) return;

    
    /* Update */
    if (p_ptr->update) {
    
	if (p_ptr->update & PU_VIEW) {
	    p_ptr->update &= ~PU_VIEW;
	    update_view();
	}

	if (p_ptr->update & PU_LITE) {
	    p_ptr->update &= ~PU_LITE;
	    update_lite();
	}


	if (p_ptr->update & PU_FLOW) {
	    p_ptr->update &= ~PU_FLOW;
	    update_flow();
	}


	if (p_ptr->update & PU_DISTANCE) {
	    p_ptr->update &= ~PU_DISTANCE;
	    p_ptr->update &= ~PU_MONSTERS;
	    update_monsters(TRUE);
	}
	
	if (p_ptr->update & PU_MONSTERS) {
	    p_ptr->update &= ~PU_MONSTERS;
	    update_monsters(FALSE);
	}
    }


    /* Redraw */
    if (p_ptr->redraw) {
	
	if (p_ptr->redraw & PR_CAVE) {
	    p_ptr->redraw &= ~PR_CAVE;
	    p_ptr->redraw |= PR_BLOCK;
	    p_ptr->redraw |= PR_MAP;
	    clear_screen();
	}
	
	
	if (p_ptr->redraw & PR_MAP) {
	    p_ptr->redraw &= ~PR_MAP;
	    prt_map();
	}


	if (p_ptr->redraw & PR_BLOCK) {
	    p_ptr->redraw &= ~PR_BLOCK;
	    p_ptr->redraw &= ~PR_EQUIPPY;
	    p_ptr->redraw &= ~PR_HEALTH;
	    p_ptr->redraw &= ~PR_STUDY;
	    p_ptr->redraw &= ~PR_SPEED;
	    p_ptr->redraw &= ~PR_STATE;
	    p_ptr->redraw &= ~PR_STATS;
	    p_ptr->redraw &= ~PR_ARMOR;
	    p_ptr->redraw &= ~PR_HP;
	    p_ptr->redraw &= ~PR_MANA;
	    p_ptr->redraw &= ~PR_HUNGER;
	    prt_stat_block();
	}


	if (p_ptr->redraw & PR_EQUIPPY) {
	    p_ptr->redraw &= ~PR_EQUIPPY;
	    if (equippy_chars) prt_equippy_chars();
	}
	
	if (p_ptr->redraw & PR_HEALTH) {
	    p_ptr->redraw &= ~PR_HEALTH;
	    if (show_health_bar) health_redraw();
	}
    
	
	if (p_ptr->redraw & PR_STUDY) {
	    p_ptr->redraw &= ~PR_STUDY;
	    prt_study();
	}

	if (p_ptr->redraw & PR_SPEED) {
	    p_ptr->redraw &= ~PR_SPEED;
	    prt_speed();
	}

	if (p_ptr->redraw & PR_STATE) {
	    p_ptr->redraw &= ~PR_STATE;
	    prt_state();
	}

	if (p_ptr->redraw & PR_STATS) {
	    p_ptr->redraw &= ~PR_STATS;
	    prt_stat(A_STR);
	    prt_stat(A_INT);
	    prt_stat(A_WIS);
	    prt_stat(A_DEX);
	    prt_stat(A_CON);
	    prt_stat(A_CHR);
	}

	if (p_ptr->redraw & PR_ARMOR) {
	    p_ptr->redraw &= ~PR_ARMOR;
	    prt_pac();
	}

	if (p_ptr->redraw & PR_HP) {
	    p_ptr->redraw &= ~PR_HP;
	    prt_mhp();
	    prt_chp();
	}

	if (p_ptr->redraw & PR_MANA) {
	    p_ptr->redraw &= ~PR_MANA;
	    prt_cmana();
	}

	if (p_ptr->redraw & PR_HUNGER) {
	    p_ptr->redraw &= ~PR_HUNGER;
	    prt_hunger();
	}
    }
}


/*
 * Handle "p_ptr->notice".
 */
void notice_stuff(void)
{
    static bool         old_protevil = FALSE;
    static bool         old_shield = FALSE;

    static int          old_cut = 0;
    static int          old_stun = 0;

    

    /*** Cancel some states ***/
	
    /* Poisoned */
    if (p_ptr->poisoned) {
	if (p_ptr->immune_pois ||
	    p_ptr->resist_pois ||
	    p_ptr->oppose_pois) {
	    p_ptr->poisoned = 0;
	}
    }

    /* Afraid */
    if (p_ptr->afraid) {
	if (p_ptr->shero ||
	    p_ptr->hero ||
	    p_ptr->resist_fear) {
	    p_ptr->afraid = 0;
	}
    }

    /* XXX XXX XXX Various resistances */


    /*** Notice some things ***/
    
    /* Paralysis */
    if (p_ptr->paralysis) {
	if (!(PN_PARALYSED & p_ptr->notice)) {
	    p_ptr->notice |= PN_PARALYSED;
	    p_ptr->redraw |= (PR_STATE);
	}
    }
    else {
	if (PN_PARALYSED & p_ptr->notice) {
	    p_ptr->notice &= ~PN_PARALYSED;
	    p_ptr->redraw |= (PR_STATE);
	}
    }

    /* Hallucinating */
    if (p_ptr->image) {
	if (!(PN_IMAGE & p_ptr->notice)) {
	    p_ptr->notice |= PN_IMAGE;
	    p_ptr->update |= (PU_MONSTERS);
	    p_ptr->redraw |= (PR_MAP | PR_BLOCK);
	}
    }
    else {
	if (PN_IMAGE & p_ptr->notice) {
	    p_ptr->notice &= ~PN_IMAGE;
	    p_ptr->update |= (PU_MONSTERS);
	    p_ptr->redraw |= (PR_MAP | PR_BLOCK);
	    msg_print("You can see clearly!");
	}
    }

    /* Blindness */
    if (p_ptr->blind) {
	if (!(PN_BLIND & p_ptr->notice)) {
	    p_ptr->notice |= PN_BLIND;
	    p_ptr->update |= (PU_MONSTERS);
	    p_ptr->redraw |= (PR_MAP | PR_BLOCK);
	}
    }
    else {
	if (PN_BLIND & p_ptr->notice) {
	    p_ptr->notice &= ~PN_BLIND;
	    p_ptr->update |= (PU_MONSTERS);
	    p_ptr->redraw |= (PR_MAP | PR_BLOCK);
	    msg_print("You can see again!");
	}
    }

    /* Detect Invisible */
    if (p_ptr->detect_inv) {
	if (!(PN_DET_INVIS & p_ptr->notice)) {
	    p_ptr->notice |= PN_DET_INVIS;
	    p_ptr->update |= (PU_BONUS | PU_MONSTERS);
	}
    }
    else {
	if (PN_DET_INVIS & p_ptr->notice) {
	    p_ptr->notice &= ~PN_DET_INVIS;
	    p_ptr->update |= (PU_BONUS | PU_MONSTERS);
	}
    }

    /* Timed infravision */
    if (p_ptr->tim_infra) {
	if (!(PN_TIM_INFRA & p_ptr->notice)) {
	    p_ptr->notice |= PN_TIM_INFRA;
	    p_ptr->update |= (PU_BONUS);
	    p_ptr->update |= (PU_MONSTERS);
	}
    }
    else {
	if (PN_TIM_INFRA & p_ptr->notice) {
	    p_ptr->notice &= ~PN_TIM_INFRA;
	    p_ptr->update |= (PU_BONUS | PU_MONSTERS);
	}
    }

    /* Confusion */
    if (p_ptr->confused) {
	if (!(PN_CONFUSED & p_ptr->notice)) {
	    p_ptr->notice |= PN_CONFUSED;
	    p_ptr->redraw |= (PR_BLOCK);
	}
    }
    else {
	if (PN_CONFUSED & p_ptr->notice) {
	    p_ptr->notice &= ~PN_CONFUSED;
	    p_ptr->redraw |= PR_BLOCK;
	    msg_print("You feel less confused now.");
	}
    }

    /* Poisoned */
    if (p_ptr->poisoned) {
	if (!(PN_POISONED & p_ptr->notice)) {
	    p_ptr->notice |= PN_POISONED;
	    p_ptr->redraw |= PR_BLOCK;
	}
    }
    else {
	if (PN_POISONED & p_ptr->notice) {
	    p_ptr->notice &= ~PN_POISONED;
	    p_ptr->redraw |= PR_BLOCK;
	    msg_print("You feel better.");
	}
    }

    /* Afraid */
    if (p_ptr->afraid) {
	if (!(PN_FEAR & p_ptr->notice)) {
	    p_ptr->notice |= PN_FEAR;
	    p_ptr->redraw |= PR_BLOCK;
	}
    }
    else {
	if (PN_FEAR & p_ptr->notice) {
	    p_ptr->notice &= ~PN_FEAR;
	    p_ptr->redraw |= PR_BLOCK;
	    msg_print("You feel bolder now.");
	}
    }

    /* Temporary Fast */
    if (p_ptr->fast) {
	if (!(PN_FAST & p_ptr->notice)) {
	    p_ptr->notice |= PN_FAST;
	    p_ptr->update |= PU_BONUS;
	    msg_print("You feel yourself moving faster.");
	}
    }
    else {
	if (PN_FAST & p_ptr->notice) {
	    p_ptr->notice &= ~PN_FAST;
	    p_ptr->update |= PU_BONUS;
	    msg_print("You feel yourself slow down.");
	}
    }

    /* Temporary Slow */
    if (p_ptr->slow) {
	if (!(PN_SLOW & p_ptr->notice)) {
	    p_ptr->notice |= PN_SLOW;
	    p_ptr->update |= PU_BONUS;
	    msg_print("You feel yourself moving slower.");
	}
    }
    else {
	if (PN_SLOW & p_ptr->notice) {
	    p_ptr->notice &= ~PN_SLOW;
	    p_ptr->update |= PU_BONUS;
	    msg_print("You feel yourself speed up.");
	}
    }

    /* Invulnerability */
    if (p_ptr->invuln) {
	if (!(PN_INVULN & p_ptr->notice)) {
	    p_ptr->notice |= PN_INVULN;
	    p_ptr->update |= PU_BONUS;
	    msg_print("Your skin turns to steel!");
	}
    }
    else {
	if (PN_INVULN & p_ptr->notice) {
	    p_ptr->notice &= ~PN_INVULN;
	    p_ptr->update |= PU_BONUS;
	    msg_print("Your skin returns to normal.");
	}
    }

    /* Heroism */
    if (p_ptr->hero) {
	if (!(PN_HERO & p_ptr->notice)) {
	    p_ptr->notice |= PN_HERO;
	    p_ptr->update |= (PU_BONUS | PU_HP);
	    p_ptr->redraw |= (PR_HP);
	    msg_print("You feel like a HERO!");
	}
    }
    else {
	if (PN_HERO & p_ptr->notice) {
	    p_ptr->notice &= ~PN_HERO;
	    p_ptr->update |= (PU_BONUS | PU_HP);
	    p_ptr->redraw |= (PR_HP);
	    msg_print("The heroism wears off.");
	}
    }

    /* Super Heroism */
    if (p_ptr->shero) {
	if (!(PN_SHERO & p_ptr->notice)) {
	    p_ptr->notice |= PN_SHERO;
	    p_ptr->update |= (PU_BONUS | PU_HP);
	    p_ptr->redraw |= (PR_HP);
	    msg_print("You feel like a killing machine!");
	}
    }
    else {
	if (PN_SHERO & p_ptr->notice) {
	    p_ptr->notice &= ~PN_SHERO;
	    p_ptr->update |= (PU_BONUS | PU_HP);
	    p_ptr->redraw |= (PR_HP);
	    msg_print("You feel less Berserk.");
	}
    }

    /* Blessed */
    if (p_ptr->blessed) {
	if (!(PN_BLESSED & p_ptr->notice)) {
	    p_ptr->notice |= PN_BLESSED;
	    p_ptr->update |= PU_BONUS;
	    msg_print("You feel righteous!");
	}
    }
    else {
	if (PN_BLESSED & p_ptr->notice) {
	    p_ptr->notice &= ~PN_BLESSED;
	    p_ptr->update |= PU_BONUS;
	    msg_print("The prayer has expired.");
	}
    }

    /* Protection from evil */
    if (p_ptr->protevil) {
	if (!old_protevil) {
	    old_protevil = TRUE;
	}
    }
    else {
	if (old_protevil) {
	    msg_print("You no longer feel safe from evil.");
	    old_protevil = FALSE;
	}
    }
    
    /* Shield */
    if (p_ptr->shield) {
	if (!old_shield) {
	    old_shield = TRUE;
	    p_ptr->update |= PU_BONUS;
	}
    }
    else {
	if (old_shield) {
	    old_shield = FALSE;
	    p_ptr->update |= PU_BONUS;
	    msg_print("Your mystic shield crumbles away.");
	}
    }

    
    /*** Notice other stuff ***/
    
    /* Display the stun */
    if (p_ptr->stun != old_stun) {
	old_stun = p_ptr->stun;
	if (p_ptr->stun == 0) {
	    msg_print("You are no longer stunned.");
	}
	p_ptr->update |= (PU_BONUS);
	p_ptr->redraw |= (PR_BLOCK);
    }

    /* Display the cut */
    if (p_ptr->cut != old_cut) {
	old_cut = p_ptr->cut;
	if (p_ptr->cut == 0) {
	    msg_print("You are no longer bleeding.");
	}
	p_ptr->update |= (PU_BONUS);
	p_ptr->redraw |= (PR_BLOCK);
    }
}


