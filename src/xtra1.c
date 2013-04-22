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

/* Ugly, ugly hack to determine when to print god bonus messages */

static int old_badness = -1000;

/*
 * Give the player a bonus.
 */
static void god_bonus(int god, int goodness, bool do_print) {
        switch (god)
        {
                case GOD_YAVANNA:    /* Forest */
                        p_ptr->resist_cold = TRUE;
                        if (do_print)  msg_print("Your body resists cold.");

                        if (goodness > 2)
                        {
                                p_ptr->immune_cold = TRUE;
                                if (do_print)  msg_print("Your body *resists* cold.");
                        }
                        break;

                case GOD_ULMO:    /* Water */
                        p_ptr->slow_digest = TRUE;
                        if (do_print)  msg_print("Your body changes subtly.");

                        if (goodness > 1)
                        {
                                p_ptr->resist_pois = TRUE;
                                if (do_print) msg_print("You are protected from poisons.");
                        }

                        if (goodness > 2)
                        {
                                p_ptr->resist_sound = TRUE;
                                if (do_print) msg_print("You are protected from sound.");
                        }
                        break;

                case GOD_AULE:    /* Earth */
                        p_ptr->ffall = TRUE;
                        p_ptr->free_act = TRUE;
                        if (do_print) msg_print("You feel one with the Earth.");

                        if (goodness > 1)
                        {
                                p_ptr->regenerate = TRUE;
                                if (do_print) msg_print("You start to regenerate.");
                        }

                        if (goodness > 2)
                        {
                                p_ptr->resist_shard = TRUE;
                                if (do_print) msg_print("You resist shards.");
                        }
                        break;

                case GOD_MELKOR:   /* Darkness */
                        p_ptr->aggravate = TRUE;
                        p_ptr->resist_dark = TRUE;
                        p_ptr->sustain_str = TRUE;
                        p_ptr->sustain_con = TRUE;
                        p_ptr->hold_life = TRUE;
                        if (do_print) msg_print("You feel evil seep into your soul.");

                        if (goodness > 1) {
                                p_ptr->see_inv = TRUE;
                                p_ptr->telepathy |= ESP_SPIDER | ESP_ORC | ESP_TROLL | ESP_GOOD | ESP_UNDEAD | ESP_DEMON | ESP_EVIL;
                                if (do_print) msg_print("You sense your prey.");
                        }

                        if (goodness > 2) {
                                p_ptr->resist_disen = TRUE;
                                p_ptr->resist_chaos = TRUE;
                                if (do_print) msg_print("You feel fortified.");
                        }
                        break;

                case GOD_TILION:   /* Moon */
                        p_ptr->lite = TRUE;
                        if (do_print) msg_print("You radiate silver light.");

                        if (goodness > 1) {
                                p_ptr->resist_dark = TRUE;
                                if (do_print) msg_print("You feel fortified against darkness.");
                        }

                        if (goodness > 2) {
                                p_ptr->hold_life = TRUE;
                                if (do_print) msg_print("You feel your lifeforce grow stronger.");
                        }
                        break;

                case GOD_ARIEN:   /* Sun */
                        p_ptr->lite = TRUE;
                        if (do_print) msg_print("You radiate a holy aura.");

                        if (goodness > 1) {
                                p_ptr->resist_lite = TRUE;
                                if (do_print) msg_print("You feel fortified against light.");
                        }

                        if (goodness > 2) {
                                p_ptr->regenerate = TRUE;
                                if (do_print) msg_print("You start to regenerate.");
                        }
                        break;


                case GOD_TULKAS:   /* Rage */
                        p_ptr->shero += 1000;
                        if (do_print) msg_print("You feel the rage boiling inside you.");

                        if (goodness > 1) {
                                p_ptr->stat_add[A_STR] += 1;
                                p_ptr->stat_add[A_DEX] += 1;
                                if (do_print) msg_print("You feel stronger and faster.");
                        }

                        if (goodness > 2) {
                                p_ptr->free_act = TRUE;
                                p_ptr->resist_blind = TRUE;
                                p_ptr->resist_conf = TRUE;
                                if (do_print) msg_print("You resist the tricks of your enemies.");
                        }
                        break;
                        
                case GOD_MANWE:   /* Winds */
                        p_ptr->sustain_dex = TRUE;
                        p_ptr->resist_sound = TRUE;
                        p_ptr->free_act = TRUE;
                        if (do_print) msg_print("You feel one with the wind.");

                        if (goodness > 1) {
                                p_ptr->stat_add[A_DEX] += 1;
                                if (do_print) msg_print("You feel more dextrous.");
                        }

                        if (goodness > 2) {
                                p_ptr->pspeed += 7;
                                if (do_print) msg_print("You feel very fast.");
                        }
                        break;


                case GOD_VARDA:   /* Stars */
                        p_ptr->sustain_int = TRUE;
                        p_ptr->sustain_wis = TRUE;
                        p_ptr->telepathy |= ESP_ALL;
                        if (do_print) msg_print("Your mind finds new analytical skills.");

                        if (goodness > 1) {
                                p_ptr->stat_add[A_INT] += 1;
                                p_ptr->stat_add[A_WIS] += 1;
                                if (do_print) msg_print("You feel smarter and wiser.");
                        }

                        if (goodness > 2) {
                                p_ptr->resist_blind = TRUE;
                                p_ptr->resist_conf = TRUE;
                                p_ptr->resist_fear = TRUE;
                                if (do_print) msg_print("You gain clarity of mind.");
                        }
                        break;

                case GOD_ERU:  /* Being. */
                        p_ptr->stat_add[A_INT] += 15;
                        p_ptr->stat_add[A_WIS] += 15;
                        p_ptr->stat_add[A_CHR] += 15;
                        p_ptr->to_h -= 50;
                        p_ptr->dis_to_h -= 50;
                        p_ptr->to_d -= 50;
                        p_ptr->dis_to_d -= 50;
                        p_ptr->lite = TRUE;
                        p_ptr->regenerate = TRUE;

                        if (do_print) msg_print("You have been branded with the Mark of Eternal Being.");
                        break;
                case GOD_RNG: /* Randomness */
                        if (do_print) msg_print("You are randomized.");
                        p_ptr->stat_add[A_STR] += rand_int(6) - 3;
                        p_ptr->stat_add[A_DEX] += rand_int(6) - 3;
                        p_ptr->stat_add[A_CON] += rand_int(6) - 3;
                        p_ptr->stat_add[A_INT] += rand_int(6) - 3;
                        p_ptr->stat_add[A_WIS] += rand_int(6) - 3;
                        p_ptr->stat_add[A_CHR] += rand_int(6) - 3;
                        break;
        }
}

static void god_effect(int god, int badness) {
  cptr name = deity_info[god-1].name;
  bool do_print = FALSE;

  if (badness != old_badness) {
    do_print = TRUE;
  }

  if (badness > 7) {
    god_bonus(god, badness-7, do_print);
    old_badness = badness;
    return;
  }

  if (dun_level == 0) {
    return;
  }

  switch (badness) {
  case 7:
    if (do_print)
      {
          msg_format("The voice of %s booms:", name);
          msg_format("So you want to serve me, %s? Very well, but be diligent!", player_name);
      }
    break;
  case 6:
    if (do_print && randint(2000) == 1)
    {
          msg_format("The voice of %s booms:", name);
          msg_format("%s, it's time thou do a honorable duty!", player_name);
    }
    break;
  case 5:
    if (randint(1500) == 1)
    {
    msg_format("The voice of %s booms:", name);
    msg_format("I am displeased at your continued ignoring me, %s!", player_name);
    take_hit(5, "Godly wrath.");
    break;
    }
  case 4:
    if (randint(1000) == 1)
    {
    msg_format("%s smites you for your negligence!", name);
    godly_wrath_blast();
    break;
    }
  case 3:
    if (randint(800) == 1)
    {
    msg_format("You are struck by the malevolent will of %s!", name);
    p_ptr->paralyzed += 10;
    p_ptr->confused += 10;
    p_ptr->redraw |= PR_CONFUSED;
    p_ptr->redraw |= PR_STATE;
    handle_stuff();
    break;
    }
  case 2:
    if (randint(500) == 1)
    {
    msg_print("Your mind spins as horrible thoughts fill your mind.");
    p_ptr->paralyzed += 15;
    p_ptr->confused += 15;
    p_ptr->stun += 15;
    p_ptr->image += 15;
    p_ptr->redraw |= PR_CONFUSED;
    p_ptr->redraw |= PR_STUN;
    p_ptr->redraw |= PR_STATE;
    handle_stuff();
    break;
    }
  case 1:
    if (randint(200) == 1)
    {
    msg_format("The voice of %s booms:", name);
    msg_format("A curse on you for your carelessness, %s!", player_name);
    take_hit(damroll(5, 5), "Godly wrath.");
    break;
    }
  case 0:
    if (randint(100) == 1)
    {
    msg_format("The voice of %s booms:", name);
    msg_format("You elected to forego me, %s? Then die!", player_name);
    deadly_side_effect(TRUE);
    break;
    }
  }

  old_badness = badness;
}


/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 220)
		{
			sprintf(out_val, "18/%3s", "***");
		}
		else if (bonus >= 100)
		{
			sprintf(out_val, "18/%03d", bonus);
		}
		else
		{
			sprintf(out_val, " 18/%02d", bonus);
		}
	}

	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "    %2d", val);
	}
}



/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
	int    i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18) value++;

			/* Ten "points" at a time */
			else value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18+10) value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18) value = 18;

			/* One point at a time */
			else if (value > 3) value--;
		}
	}

	/* Return new value */
	return (value);
}



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
 * Prints players max/cur tank points
 */
static void prt_tp(void)
{
	char tmp[32];
	byte color;


        /* Do not show tank unless it matters */
        if (p_ptr->prace!=RACE_DRAGONRIDDER) return;


        put_str("TP ", ROW_TP, COL_TP);

        sprintf(tmp, "%5d/%5d", p_ptr->ctp, p_ptr->mtp);
        if (p_ptr->ctp >= p_ptr->mtp)
	{
		color = TERM_L_GREEN;
	}
        else if (p_ptr->ctp > (p_ptr->mtp * hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}
        c_put_str(color, tmp, ROW_TP, COL_TP + 2);
}

/*
 * Prints the player's current sanity.
 */
static void prt_sane(void) {
  char tmp[32];
  byte color;
  int perc;

  if (p_ptr->msane == 0) {
    perc = 100;
  } else {
    perc = (100*p_ptr->csane)/p_ptr->msane;
  }

  put_str("SN ", ROW_SANITY, COL_SANITY);

  sprintf(tmp, "%5d/%5d", p_ptr->csane, p_ptr->msane);

  if (perc >= 100) {
    color = TERM_L_GREEN;
  } else if (perc > (10 * hitpoint_warn)) {
    color = TERM_YELLOW;
  } else {
    color = TERM_RED;
  }

  c_put_str(color, tmp, ROW_SANITY, COL_SANITY+2);
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		int colour;

		if (p_ptr->stat_cnt[stat])
			colour=TERM_ORANGE;
		else
			colour=TERM_YELLOW;

                put_str(format("%s: ", stat_names_reduced[stat]), ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(colour, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
                put_str(format("%s: ", stat_names[stat]), ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
                c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}




/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p = "";

        /* Mimic shape */
        if (p_ptr->mimic_form)
        {
                p = p_ptr->mimic_name;
        }

	/* Wizard */
        else if (wizard)
	{
		p = "[=-WIZARD-=]";
	}

	/* Winner */
	else if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
	}

	/* Normal */
	else
	{
		p = player_title[p_ptr->pclass][(p_ptr->lev-1)/5];

	}

	prt_field(p, ROW_TITLE, COL_TITLE);
}


/*
 * Prints level
 */
static void prt_level(void)
{
	char tmp[32];

	sprintf(tmp, "%6d", p_ptr->lev);

	if (p_ptr->lev >= p_ptr->max_plv)
	{
		put_str("LEVEL ", ROW_LEVEL, 0);
		c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 6);
	}
	else
	{
		put_str("Level ", ROW_LEVEL, 0);
		c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 6);
	}
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
	char out_val[32];

        if (!exp_need)
        {
                (void)sprintf(out_val, "%8ld", (long)p_ptr->exp);
        }
        else
        {
                if (p_ptr->lev >= PY_MAX_LEVEL)
                {
                        (void)sprintf(out_val, "********");
                }
                else
                {
                        (void)sprintf(out_val, "%8ld", (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) - p_ptr->exp);
                }
        }

	if (p_ptr->exp >= p_ptr->max_exp)
	{
		put_str("EXP ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
	}
	else
	{
		put_str("Exp ", ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	char tmp[32];

	put_str("AU ", ROW_GOLD, COL_GOLD);
	sprintf(tmp, "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

	put_str("Cur AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	char tmp[32];

	byte color;


        if(p_ptr->class_extra3 & CLASS_UNDEAD)
        {
                put_str("DP ", ROW_HP, COL_HP);

                sprintf(tmp, "%5d/%5d", p_ptr->chp, p_ptr->mhp);
                if (p_ptr->chp >= p_ptr->mhp)
                {
                        color = TERM_L_BLUE;
                }
                else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
                {
                        color = TERM_VIOLET;
                }
                else
                {
                        color = TERM_L_RED;
                }
                c_put_str(color, tmp, ROW_HP, COL_HP + 2);
        }
        else
        {
                put_str("HP ", ROW_HP, COL_HP);

                sprintf(tmp, "%5d/%5d", p_ptr->chp, p_ptr->mhp);
                if (p_ptr->chp >= p_ptr->mhp)
                {
                        color = TERM_L_GREEN;
                }
                else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
                {
                        color = TERM_YELLOW;
                }
                else
                {
                        color = TERM_RED;
                }
                c_put_str(color, tmp, ROW_HP, COL_HP + 2);
        }
}

/*
 * Prints Cur/Max monster hit points
 */
static void prt_mh(void)
{
	char tmp[32];

	byte color;

        int max;

        object_type *o_ptr;
        monster_race *r_ptr;

        /* Get the carried monster */
        o_ptr = &inventory[INVEN_CARRY];

        if(!o_ptr->pval2){
                put_str("             ", ROW_MH, COL_MH);
                return;
        }

        r_ptr = &r_info[o_ptr->pval];
        max = maxroll(r_ptr->hdice, r_ptr->hside);

        put_str("MH ", ROW_MH, COL_MH);

        sprintf(tmp, "%5d/%5d", o_ptr->pval2, max);
        if (o_ptr->pval2 >= max)
	{
		color = TERM_L_GREEN;
	}
        else if (o_ptr->pval2 > (max * hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}
        c_put_str(color, tmp, ROW_MH, COL_MH + 2);
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte color;


	/* Do not show mana unless it matters */
        if ((!mp_ptr->spell_book) && (p_ptr->pclass != CLASS_RUNECRAFTER) 
	   && (p_ptr->pclass != CLASS_MIMIC) && (p_ptr->pclass != CLASS_POSSESSOR)) return;


        put_str("SP ", ROW_SP, COL_SP);

        sprintf(tmp, "%5d/%5d", p_ptr->csp, p_ptr->msp);
        if (p_ptr->csp >= p_ptr->msp)
	{
		color = TERM_L_GREEN;
	}
        else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}
        c_put_str(color, tmp, ROW_SP, COL_SP + 2);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];
        dungeon_info_type *d_ptr = &d_info[dungeon_type];

	if (p_ptr->inside_arena)
	{
		strcpy(depths, "Arena");
	}
        else if (get_dungeon_name(depths)) {}
        else if (dungeon_flags1 & LF1_SPECIAL)
	{
                strcpy(depths, "Special");
        }
	else if (p_ptr->inside_quest)
	{
		strcpy(depths, "Quest");
	}
	else if (!dun_level)
	{
                if (wf_info[wild_map[p_ptr->wilderness_y][p_ptr->wilderness_x].feat].name + wf_name)
                        strcpy(depths, wf_info[wild_map[p_ptr->wilderness_y][p_ptr->wilderness_x].feat].name + wf_name);
		else
			strcpy(depths, "Town/Wild");
	}
	else if (depth_in_feet)
	{
                if (d_ptr->flags1 & DF1_TOWER) (void)sprintf(depths, "-%d ft", dun_level * 50);
                else (void)sprintf(depths, "%d ft", dun_level * 50);
	}
	else
	{
                if (d_ptr->flags1 & DF1_TOWER) (void)sprintf(depths, "%c%c%c -%d", d_ptr->short_name[0], d_ptr->short_name[1], d_ptr->short_name[2], dun_level);
                else (void)sprintf(depths, "%c%c%c %d", d_ptr->short_name[0], d_ptr->short_name[1], d_ptr->short_name[2],dun_level);
	}

	/* Right-Adjust the "depth", and clear old values */
	prt(format("%7s", depths), 23, COL_DEPTH);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		c_put_str(TERM_ORANGE, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		c_put_str(TERM_L_GREEN, "Full  ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Gorged */
	else
	{
		c_put_str(TERM_GREEN, "Gorged", ROW_HUNGRY, COL_HUNGRY);
	}
}


/*
 * Prints Blind status
 */
static void prt_blind(void)
{
	if (p_ptr->blind)
	{
		c_put_str(TERM_ORANGE, "Blind", ROW_BLIND, COL_BLIND);
	}
	else
	{
		put_str("     ", ROW_BLIND, COL_BLIND);
	}
}


/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
	if (p_ptr->confused)
	{
		c_put_str(TERM_ORANGE, "Confused", ROW_CONFUSED, COL_CONFUSED);
	}
	else
	{
		put_str("        ", ROW_CONFUSED, COL_CONFUSED);
	}
}


/*
 * Prints Fear status
 */
static void prt_afraid(void)
{
	if (p_ptr->afraid)
	{
		c_put_str(TERM_ORANGE, "Afraid", ROW_AFRAID, COL_AFRAID);
	}
	else
	{
		put_str("      ", ROW_AFRAID, COL_AFRAID);
	}
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
	if (p_ptr->poisoned)
	{
		c_put_str(TERM_ORANGE, "Poisoned", ROW_POISONED, COL_POISONED);
	}
	else
	{
		put_str("        ", ROW_POISONED, COL_POISONED);
	}
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
	byte attr = TERM_WHITE;

	char text[16];


	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		attr = TERM_RED;

		strcpy(text, "Paralyzed!");
	}

	/* Resting */
	else if (resting)
	{
		int i;

		/* Start with "Rest" */
		strcpy(text, "Rest      ");

		/* Extensive (timed) rest */
		if (resting >= 1000)
		{
			i = resting / 100;
			text[9] = '0';
			text[8] = '0';
			text[7] = '0' + (i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[6] = '0' + (i % 10);
				if (i >= 10)
				{
					text[5] = '0' + (i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (resting >= 100)
		{
			i = resting;
			text[9] = '0' + (i % 10);
			i = i / 10;
			text[8] = '0' + (i % 10);
			text[7] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (resting >= 10)
		{
			i = resting;
			text[9] = '0' + (i % 10);
			text[8] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (resting > 0)
		{
			i = resting;
			text[9] = '0' + (i);
		}

		/* Rest until healed */
		else if (resting == -1)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '*';
		}

		/* Rest until done */
		else if (resting == -2)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '&';
		}
	}

	/* Repeating */
	else if (command_rep)
	{
		if (command_rep > 999)
		{
			(void)sprintf(text, "Rep. %3d00", command_rep / 100);
		}
		else
		{
			(void)sprintf(text, "Repeat %3d", command_rep);
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
		strcpy(text, "Searching ");
	}

	/* Nothing interesting */
	else
	{
		strcpy(text, "          ");
	}

	/* Display the info (or blanks) */
	c_put_str(attr, text, ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	byte attr = TERM_WHITE;
	char buf[32] = "";

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Fast */
	if (i > 110)
	{
		attr = TERM_L_GREEN;
		sprintf(buf, "Fast (+%d)", (i - 110));
	}

	/* Slow */
	else if (i < 110)
	{
		attr = TERM_L_UMBER;
		sprintf(buf, "Slow (-%d)", (110 - i));
	}

	/* Display the speed */
	c_put_str(attr, format("%-14s", buf), ROW_SPEED, COL_SPEED);
}


static void prt_study(void)
{
	if (p_ptr->new_spells)
	{
		put_str("Study", ROW_STUDY, 64);
	}
	else
	{
		put_str("     ", ROW_STUDY, COL_STUDY);
	}
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
		c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);
	}
	else if (c > 200)
	{
		c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);
	}
	else if (c > 100)
	{
		c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);
	}
	else if (c > 50)
	{
		c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);
	}
	else if (c > 25)
	{
		c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);
	}
	else if (c > 10)
	{
		c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, COL_CUT);
	}
	else if (c)
	{
		c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, COL_CUT);
	}
	else
	{
		put_str("            ", ROW_CUT, COL_CUT);
	}
}



static void prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
		c_put_str(TERM_RED, "Knocked out ", ROW_STUN, COL_STUN);
	}
	else if (s > 50)
	{
		c_put_str(TERM_ORANGE, "Heavy stun  ", ROW_STUN, COL_STUN);
	}
	else if (s)
	{
		c_put_str(TERM_ORANGE, "Stun        ", ROW_STUN, COL_STUN);
	}
	else
	{
		put_str("            ", ROW_STUN, COL_STUN);
	}
}



/*
 * Redraw the "monster health bar"	-DRS-
 * Rather extensive modifications by	-BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	/* Not tracking */
	if (!health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
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

                /* Poisoned */
                if (m_ptr->poisoned) attr = TERM_GREEN;

                /* Bleeding */
                if (m_ptr->bleeding) attr = TERM_RED;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}

#endif

}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Race and Class */
        prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < 6; i++) prt_stat(i);

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();

	/* Current sanity */
	prt_sane();

	/* Spellpoints */
	prt_sp();

        /* Tankerpoints */
        prt_tp();

        /* Monster hitpoints */
        prt_mh();

	/* Gold */
	prt_gold();

	/* Current depth */
	prt_depth();

	/* Special */
	health_redraw();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
	/* Cut/Stun */
	prt_cut();
	prt_stun();

	/* Food */
	prt_hunger();

	/* Various */
	prt_blind();
	prt_confused();
	prt_afraid();
	prt_poisoned();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

	/* Study spells */
	prt_study();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_INVEN))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display inventory */
		display_inven();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_EQUIP))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display equipment */
		display_equip();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display equipment in sub-windows
 */
static void fix_spell(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_SPELL))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display spell list */
		display_spell_list();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display character in sub-windows
 */
static void fix_player(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_PLAYER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display player */
		display_player(0);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display recent messages in sub-windows
 *
 * XXX XXX XXX Adjust for width and split messages
 */
static void fix_message(void)
{
	int j, i;
	int w, h;
	int x, y;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MESSAGE))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Get size */
		Term_get_size(&w, &h);

		/* Dump messages */
		for (i = 0; i < h; i++)
		{
			/* Dump the message on the appropriate line */
                        Term_putstr(0, (h - 1) - i, -1, message_color((s16b)i), message_str((s16b)i));

			/* Cursor */
			Term_locate(&x, &y);

			/* Clear to end of line */
			Term_erase(x, y, 255);
		}

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
	int j;

	int cy, cx;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw map */
		display_map(&cy, &cx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MONSTER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
                if (monster_race_idx) display_roff(monster_race_idx, monster_ego_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OBJECT))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (object_kind_idx) display_koff(object_kind_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/* Show the monster list in a window */

static void fix_m_list(void)
{
	int i, j; 

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		int c = 0;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
                if (!(window_flag[j] & (PW_M_LIST))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Clear */
		Term_clear();

		/* Hallucination */
		if (p_ptr->image)
		{
			c_prt(TERM_WHITE,"You can not see clearly",0,0);

			/* Fresh */
			Term_fresh();

			/* Restore */
			Term_activate(old);

			return;
		}

		/* reset visible count */
                for (i = 1; i < max_r_idx; i++)
		{
			monster_race *r_ptr = &r_info[i];
			
			r_ptr->total_visible = 0;
		}

		/* Count up the number visible in each race */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];
                        monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Skip dead monsters */
			if (m_ptr->hp < 0) continue;

			/* Skip unseen monsters */
			if (!m_ptr->ml) continue;

			/* Increase for this race */
			r_ptr->total_visible++;

			/* Increase total Count */
			c++;
		}

		/* Are monsters visible? */
		if (c)
		{
			int  w, h, num = 0;

			(void)Term_get_size(&w, &h);

			c_prt(TERM_WHITE,format("You can see %d monster%s", c, (c > 1 ? "s:" : ":")), 0, 0);

                        for (i = 1; i < max_r_idx; i++)
			{
				monster_race *r_ptr = &r_info[i];

				/* Default Colour */
				byte attr = TERM_SLATE;

				/* Only visible monsters */
				if (!r_ptr->total_visible) continue;

				/* Uniques */
				if (r_ptr->flags1 & RF1_UNIQUE)
				{
					attr = TERM_L_BLUE;
				}

				/* Have we ever killed one? */
                                if (r_ptr->r_tkills)
				{
                                        if (r_ptr->level > dun_level)
					{
						attr = TERM_VIOLET;

						if (r_ptr->flags1 & RF1_UNIQUE)
						{
							attr = TERM_RED;
						}
					}
				}
				else
				{
					if (!(r_ptr->flags1 & RF1_UNIQUE)) attr = TERM_GREEN;
				}
			
				
				/* Dump the monster name */
				if (r_ptr->total_visible == 1)
				{
					c_prt(attr, (r_name + r_ptr->name), (num % (h - 1)) + 1, (num / (h - 1) * 26));
				}
				else
				{
					c_prt(attr,format("%s (x%d)",r_name + r_ptr->name, r_ptr->total_visible), (num % (h - 1)) + 1, (num / (h - 1)) * 26);
				}

				num++;

			}

		}
		else
		{
			c_prt(TERM_WHITE,"You see no monsters.",0,0);
		}
		
		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
	int			i, j, k, levels;
	int			num_allowed, num_known;

	magic_type		*s_ptr;
	int which;
        int class_max;


        cptr p = (((mp_ptr->spell_book == TV_VALARIN_BOOK) || (mp_ptr->spell_book == TV_PRAYER_BOOK)) ? "prayer" : "spell");

        if (p_ptr->pclass == CLASS_SORCERER)
        {
                p_ptr->new_spells = 0;
                return;
        }

        if (1) {

	/* Hack -- must be literate */
	if (!mp_ptr->spell_book) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2);

	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
        for (i = 0; i < MAX_REALM; i++)
        {
                for (j = 0; j < 64; j++)
                {
                        if (spell_learned[i][(j < 32)] & (1L << (j % 32)))
                        {
                                num_known += spell_level[i][j];
                        }
                }
        }

        /* Limit the number of spells each class can learn */
        switch(p_ptr->pclass)
        {
                case CLASS_ROGUE:
                case CLASS_MONK:
                        class_max = 24;
                        break;

                case CLASS_RANGER:
                case CLASS_HARPER:
                        class_max = 32;
                        break;

                case CLASS_DRUID:
                        class_max = 40;
                        break;

                case CLASS_SYMBIANT:
                        class_max = 13;
                        break;

                default:                
                        class_max = 64;
                        break;
        }

       if (num_allowed > class_max) num_allowed = class_max;

	/* See how many spells we must forget or may learn */
        /* Substract the spell learned by other means than studying(potion) */
        p_ptr->new_spells = num_allowed - (num_known - p_ptr->xtra_spells);

	/* Forget spells which are too hard */
	for (i = 63; i >= 0; i--)
	{
		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;


		/* Get the spell */
                s_ptr = &realm_info[realm_order[i]][j];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
                if (spell_learned[realm_order[i]][(j < 32)] & (1L << (j % 32)))
		{
			/* Mark as forgotten */
                        spell_forgotten[realm_order[i]][(j < 32)] |= (1L << (j % 32));
                        which = realm_order[i];

			/* No longer known */
                        spell_learned[realm_order[i]][(j < 32)] &= ~(1L << (j % 32));
                        which = realm_order[i];

			/* Message */
			msg_format("You have forgotten the %s of %s.", p,
                        spell_names[which][j%64][0]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 63; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Get the (i+1)th spell learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
                if (spell_learned[realm_order[i]][(j < 32)] & (1L << (j % 32)))
		{
			/* Mark as forgotten */
                        spell_forgotten[realm_order[i]][(j < 32)] |= (1L << (j % 32));
                        which = realm_order[i];

			/* No longer known */
                        spell_learned[realm_order[i]][(j < 32)] &= ~(1L << (j % 32));
                        which = realm_order[i];

			/* Message */
                        msg_format("You have forgotten the %s of %s.", p,
                                   spell_names[which][j%64][0]);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 64; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Access the spell */
                s_ptr = &realm_info[realm_order[i]][j];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
                if (spell_forgotten[realm_order[i]][(j < 32)] & (1L << (j % 32)))
		{
			/* No longer forgotten */
                        spell_forgotten[realm_order[i]][(j < 32)] &= ~(1L << (j % 32));
                        which = realm_order[i];

			/* Known once more */
                        spell_learned[realm_order[i]][(j < 32)] |= (1L << (j % 32));
                        which = realm_order[i];

			/* Message */
			msg_format("You have remembered the %s of %s.",
                                   p, spell_names[which][j%64][0]);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
        for (i = 0; i < MAX_REALM; i++)
        {
                for (j = 0; j < 64; j++)
                {
                        /* Access the spell */
                        s_ptr = &realm_info[i][j];

                        /* Skip spells we cannot remember */
                        if (s_ptr->slevel > p_ptr->lev) continue;

                        /* Skip spells we already know */
                        if (spell_learned[realm_order[i]][(j < 32)] & (1L << (j % 32)))
                        {
                                continue;
                        }

                        /* Count it */
                        k++;
                }
        }

        if (k > class_max) k = class_max;

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

    

	/* Spell count changed */
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
                        cmsg_format(TERM_L_BLUE, "You can learn %d more %s%s.",
			           p_ptr->new_spells, p,
			           (p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
        }

}

/* Ugly hack */
bool calc_powers_silent = FALSE;

/* Calc the player powers */
static void calc_powers(void)
{
        int                     i, p = 0;
        s32b old_powers[POWER_SLOT];

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

        /* Save old powers */
        for (i = 0; i < POWER_SLOT; i++) old_powers[i] = p_ptr->powers[i];

        /* Get intrinsincs */
        for (i = 0; i < POWER_SLOT; i++) p_ptr->powers[i] = p_ptr->powers_mod[i];

        /* Add corruptions */
        p_ptr->powers[0] |= p_ptr->muta1;

        /* Add objects powers */
        for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
        {
                object_type *o_ptr = &inventory[i];

                if (!o_ptr->k_idx) continue;

                p = object_power(o_ptr);
                if (p != -1) p_ptr->powers[p / 32] |= BIT(p % 32);
        }

        if ((!p_ptr->tim_mimic) && (!p_ptr->body_monster))
        {
                /* Add in racial and subracial powers */
                for (i = 0; i < 4; i++)
                {
                        p = rp_ptr->powers[i];
                        if (p != -1) p_ptr->powers[p / 32] |= BIT(p % 32);

                        p = rmp_ptr->powers[i];
                        if (p != -1) p_ptr->powers[p / 32] |= BIT(p % 32);
                }
        }
        else if (p_ptr->mimic_form)
        switch(p_ptr->mimic_form)
        {
                case MIMIC_ENT:
                        p = PWR_GROW_TREE;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
                case MIMIC_MANA_BALL:
                        p = PWR_VTELEPORT;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
                        break;
                case MIMIC_VAMPIRE:
                        p = PWR_VAMPIRISM;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
                        break;
                case MIMIC_FIRE_CLOUD:
                        p = PWR_BR_FIRE;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
                        break;
                case MIMIC_COLD_CLOUD:
                        p = PWR_BR_COLD;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
                        break;
                case MIMIC_CHAOS_CLOUD:
                        p = PWR_BR_CHAOS;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
                        break;
                case MIMIC_KOBOLD:
                        p = PWR_POIS_DART;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
                case MIMIC_DEMON:
		case MIMIC_DEMON_LORD:
                        p = PWR_BR_FIRE;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
                case MIMIC_DRAGON:
                        p = PWR_BR_ELEM;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
                case MIMIC_QUYLTHULG:
                        p = PWR_SUMMON_MONSTER;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
                case MIMIC_VALAR:
                        p = PWR_WRECK_WORLD;
                        p_ptr->powers[p / 32] |= BIT(p % 32);
			break;
        }

        /* Add in class powers */
        for (i = 0; i < 4; i++)
        {
                p = cp_ptr->powers[i];
                if (p != -1) p_ptr->powers[p / 32] |= BIT(p % 32);
        }

        p = PWR_UNHYPNO;
        p_ptr->powers[p / 32] |= BIT(p % 32);


        p = PWR_HYPNO;
        p_ptr->powers[p / 32] |= BIT(p % 32);

        if (p_ptr->disembodied)
	{
                p = PWR_INCARNATE;
                p_ptr->powers[p / 32] |= BIT(p % 32);
	}

        /* Now lets warn the player */
        for (i = 0; i < POWER_MAX; i++)
        {
                s32b old = old_powers[i / 32] & BIT(i % 32);
                s32b new = p_ptr->powers[i / 32] & BIT(i % 32);

                if (new > old)
                {
                        if (!calc_powers_silent) cmsg_print(TERM_GREEN, powers_type[i].gain_text);
                }
                else if (new < old)
                {
                        if (!calc_powers_silent) cmsg_print(TERM_RED, powers_type[i].lose_text);
                }
        }

        calc_powers_silent = FALSE;
}


/*
 * Calculate the player's sanity
 */

void calc_sanity(void) {
  int bonus, msane;

  /* Hack -- use the con/hp table for sanity/wis */
  bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_WIS]]) - 128);

  /* Hack -- assume 5 sanity points per level. */
  msane = 5*(p_ptr->lev+1) + (bonus * p_ptr->lev / 2);

  if (msane < p_ptr->lev + 1) msane = p_ptr->lev + 1;

  if (p_ptr->msane != msane) {

    /* Sanity carries over between levels. */
    p_ptr->csane += (msane - p_ptr->msane);

    p_ptr->msane = msane;

    if (p_ptr->csane >= msane) {
      p_ptr->csane = msane;
      p_ptr->csane_frac = 0;
    }

    p_ptr->redraw |= (PR_SANITY);
    p_ptr->window |= (PW_SPELL | PW_PLAYER);
  }
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int		msp, levels, cur_wgt, max_wgt;
        u32b f1, f2, f3, f4, f5, esp;

	object_type	*o_ptr;


	/* Hack -- Must be literate */
        if ((!mp_ptr->spell_book)&&(p_ptr->pclass!=CLASS_RUNECRAFTER)&&(p_ptr->pclass!=CLASS_MIMIC)&&(p_ptr->pclass!=CLASS_POSSESSOR)) return;

        if ((p_ptr->pclass == CLASS_MIMIC)||(p_ptr->pclass == CLASS_RUNECRAFTER)
	  ||(p_ptr->pclass == CLASS_POSSESSOR))
	{
		levels = p_ptr->lev;
	}
	else
	{
		/* Extract "effective" player level */
		levels = (p_ptr->lev - mp_ptr->spell_first) + 1;
	}


	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp) msp++;

        /* Possessors mana is different */
        if (p_ptr->pclass == CLASS_POSSESSOR)
        {
                monster_race *r_ptr = &r_info[p_ptr->body_monster];
                int f = 100 / (r_ptr->freq_spell?r_ptr->freq_spell:1);

                msp = 21 - f;

                if (msp < 1) msp = 1;
        }

	/* Hack: High mages have a 25% mana bonus */
	if (msp && (p_ptr->pclass == CLASS_HIGH_MAGE)) msp += msp / 4;

        /* Hack: Sorcerer have a 150% mana bonus */
        if (msp && (p_ptr->pclass == CLASS_SORCERER)) msp += msp * 3 / 2;

        /* Hack: Hermit have a 20% mana bonus */
        if (msp && (p_ptr->pracem == RMOD_HERMIT)) msp += msp / 5;

	/* Only mages are affected */
        if (cp_ptr->flags1 & CF1_NO_GLOVES)
	{
		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
		    !(f2 & (TR2_FREE_ACT)) &&
		    !((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;

			/* Reduce mana */
			msp = (3 * msp) / 4;
		}
	}

        /* Augment mana */
        if (munchkin_multipliers)
        {
                if (p_ptr->to_m) msp += msp * p_ptr->to_m / 5;
        }
        else
        {
                if (p_ptr->to_m) msp += msp * p_ptr->to_m / 10;
        }

	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		msp -= ((cur_wgt - max_wgt) / 10);
	}

        /* When meditating your mana is increased ! */
        if(p_ptr->meditation)
        {
                msp += 50;
                p_ptr->csp += 50;
        }

	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{

#if 1

		/* XXX XXX XXX New mana maintenance */

		/* Enforce maximum */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

#else

		/* Player has no mana now */
		if (!msp)
		{
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}

		/* Player had no mana, has some now */
		else if (!p_ptr->msp)
		{
			/* Reset mana */
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Player had some mana, adjust current mana */
		else
		{
			s32b value;

			/* change current mana proportionately to change of max mana, */
			/* divide first to avoid overflow, little loss of accuracy */
			value = ((((long)p_ptr->csp << 16) + p_ptr->csp_frac) /
			         p_ptr->msp * msp);

			/* Extract mana components */
			p_ptr->csp = (value >> 16);
			p_ptr->csp_frac = (value & 0xFFFF);
		}

#endif

		/* Save new mana */
		p_ptr->msp = msp;

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msg_print("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}

		/* Save it */
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
	}


	/* Take note when "armor state" changes */
	if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			msg_print("The weight of your armor encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}

		/* Save it */
		p_ptr->old_cumber_armor = p_ptr->cumber_armor;
	}
}



/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
void calc_hitpoints(void)
{
        int bonus, mhp, i;
        u32b f1, f2, f3, f4, f5, esp;

	object_type	*o_ptr;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

        /* Hack: Sorcerer have a -10% hp penality */
        if (mhp && (p_ptr->pclass == CLASS_SORCERER)) mhp -= mhp / 10;

        /* Factor in the pernament hp modifications */
        mhp += p_ptr->hp_mod;
        if (mhp < 1) mhp = 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                /* Get the weapon */
                o_ptr = &inventory[INVEN_WIELD + i];

                /* Examine the sword */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* Hitpoints multiplier */
                if (o_ptr->k_idx && (f2 & (TR2_LIFE)))
                {
                        /* Augment Hitpoint */
                        if (munchkin_multipliers)
                        {
                                mhp += mhp * o_ptr->pval / 5;
                        }
                        else
                        {
                                mhp += mhp * o_ptr->pval / 10;
                        }
                }

                i++;
        }

        if (p_ptr->body_monster)
        {
                monster_race *r_ptr = &r_info[p_ptr->body_monster];
                int rhp = maxroll(r_ptr->hdice, r_ptr->hside);

                mhp = (rhp + sroot(rhp) + mhp) / 3;
        }
        if (p_ptr->disembodied) mhp = 1;

        /* HACK - being undead means less DP */
        if(p_ptr->class_extra3 & CLASS_UNDEAD)
        {
                int divisor = p_ptr->lev / 4;

                /* Beware of the horrible division by zero ! :) */
                if(divisor == 0) divisor = 1;

                /* Actually decrease the max hp */
                mhp /= divisor;

                /* Never less than 1 */
                if (mhp < 1) mhp = 1;
        }

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* XXX XXX XXX New hitpoint maintenance */

		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Save the new max-hitpoints */
		p_ptr->mhp = mhp;

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}
}



/*
 * Extract and set the current "lite radius"
 *
 * SWD: Experimental modification: multiple light sources have additive effect.
 *
 */
static void calc_torch(void)
{
	int i;
	object_type *o_ptr;
        u32b f1, f2, f3, f4, f5, esp;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

                /* Skip empty slots */
                if (!o_ptr->k_idx) continue;

                /* Extract the flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* does this item glow? */
                if (((f4 & TR4_FUEL_LITE) && (o_ptr->timeout > 0)) || (!(f4 & TR4_FUEL_LITE)))
                {
                        if (f3 & TR3_LITE1) p_ptr->cur_lite++;
                        if (f4 & TR4_LITE2) p_ptr->cur_lite += 2;
                        if (f4 & TR4_LITE3) p_ptr->cur_lite += 3;
                }

	}

        if (p_ptr->tim_lite) p_ptr->cur_lite += 2;

        if (p_ptr->holy) p_ptr->cur_lite += 1;

	/* max radius is 5 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
	if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

	/* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/* end experimental mods */

	/* Reduce lite when running if requested */
	if (running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

#ifdef MONSTER_LITE
        /* Update the visuals */
        p_ptr->update |= (PU_LITE);
        p_ptr->update |= (PU_MONSTERS);
#else
        /* Notice changes in the "lite radius" */
	if (p_ptr->old_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Remember the old lite */
		p_ptr->old_lite = p_ptr->cur_lite;
	}
#endif
}



/*
 * Computes current weight limit.
 */
int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;

	/* Return the result */
	return (i);
}

void calc_wield_monster()
{
        object_type *o_ptr;
        monster_race *r_ptr;

        /* Get the carried monster */
        o_ptr = &inventory[INVEN_CARRY];

        if(o_ptr->k_idx)
        {
                r_ptr = &r_info[o_ptr->pval];

                if(r_ptr->flags2 & RF2_INVISIBLE)
                        p_ptr->invis += 20;
                if(r_ptr->flags2 & RF2_REFLECTING)
                        p_ptr->reflect = TRUE;
                if(r_ptr->flags7 & RF7_CAN_FLY)
                        p_ptr->ffall = TRUE;
        }
}

/*
 * Calc which body parts the player have, based on the
 * monster he incarnate, note that that's bnot a hack
 * since body parts of the player when in it's own body
 * are also defined in r_info(monster 0)
 */
void calc_body()
{
        monster_race *r_ptr = &r_info[p_ptr->body_monster];
        int i, b_weapon, b_legs, b_arms;
        byte *body_parts, bp[BODY_MAX];

        if (!p_ptr->body_monster)
        {
                body_parts = bp;
                for (i = 0; i < BODY_MAX; i++)
                {
                        int b;

                        b = rp_ptr->body_parts[i] + rmp_ptr->body_parts[i];
                        if (b < 0) b = 0;
                        bp[i] = b;
                }
        }
        else body_parts = r_ptr->body_parts;

        b_weapon = body_parts[BODY_WEAPON];
        b_arms = body_parts[BODY_ARMS];
        b_legs = body_parts[BODY_LEGS];

        if ((p_ptr->pclass == CLASS_MIMIC) && (p_ptr->class_extra6 & CLASS_ARMS))
        {
                b_weapon++;
                b_arms++;

                if (b_weapon > 3) b_weapon = 3;
                if (b_arms > 3) b_arms = 3;
        }

        if ((p_ptr->pclass == CLASS_MIMIC) && (p_ptr->class_extra6 & CLASS_LEGS))
        {
                b_legs++;

                if (b_legs > 2) b_legs = 2;
        }

        for (i = 0; i < INVEN_TOTAL - INVEN_WIELD; i++)
                p_ptr->body_parts[i] = 0;

        for (i = 0; i < b_weapon; i++)
                p_ptr->body_parts[INVEN_WIELD - INVEN_WIELD + i] = INVEN_WIELD;
        if (body_parts[BODY_WEAPON])
                p_ptr->body_parts[INVEN_BOW - INVEN_WIELD] = INVEN_BOW;

        for (i = 0; i < body_parts[BODY_TORSO]; i++)
        {
                p_ptr->body_parts[INVEN_BODY - INVEN_WIELD + i] = INVEN_BODY;
                p_ptr->body_parts[INVEN_OUTER - INVEN_WIELD + i] = INVEN_OUTER;
                p_ptr->body_parts[INVEN_LITE - INVEN_WIELD + i] = INVEN_LITE;
                p_ptr->body_parts[INVEN_AMMO - INVEN_WIELD + i] = INVEN_AMMO;
                p_ptr->body_parts[INVEN_CARRY - INVEN_WIELD + i] = INVEN_CARRY;
        }

        for (i = 0; i < body_parts[BODY_FINGER]; i++)
                p_ptr->body_parts[INVEN_RING - INVEN_WIELD + i] = INVEN_RING;

        for (i = 0; i < body_parts[BODY_HEAD]; i++)
        {
                p_ptr->body_parts[INVEN_HEAD - INVEN_WIELD + i] = INVEN_HEAD;
                p_ptr->body_parts[INVEN_NECK - INVEN_WIELD + i] = INVEN_NECK;
        }

        for (i = 0; i < b_arms; i++)
        {
                p_ptr->body_parts[INVEN_ARM - INVEN_WIELD + i] = INVEN_ARM;
                p_ptr->body_parts[INVEN_HANDS - INVEN_WIELD + i] = INVEN_HANDS;
        }
        if (body_parts[BODY_ARMS])
                p_ptr->body_parts[INVEN_TOOL - INVEN_WIELD] = INVEN_TOOL;

        for (i = 0; i < b_legs; i++)
                p_ptr->body_parts[INVEN_FEET - INVEN_WIELD + i] = INVEN_FEET;

        /* Ok now if the player lost a body part, he must drop the object he had on it */
        for (i = 0; i < INVEN_TOTAL - INVEN_WIELD; i++)
        {
                if ((!p_ptr->body_parts[i]) && (inventory[i + INVEN_WIELD].k_idx))
                {
                        /* Drop it NOW ! */
                        inven_takeoff(i + INVEN_WIELD, 255, TRUE);
                }
        }
}

/* Should be called by every calc_bonus call */
void calc_body_bonus()
{
        monster_race *r_ptr = &r_info[p_ptr->body_monster];

        /* If in the player body nothing have to be done */
        if (!p_ptr->body_monster) return;

        if(p_ptr->disembodied)
        {
                p_ptr->wraith_form = TRUE;
                return;
        }

        p_ptr->ac += r_ptr->ac;
        p_ptr->pspeed = r_ptr->speed;

        if(r_ptr->flags1 & RF1_NEVER_MOVE) p_ptr->immovable = TRUE;
        if(r_ptr->flags2 & RF2_STUPID) p_ptr->stat_add[A_INT] -= 1;
        if(r_ptr->flags2 & RF2_SMART) p_ptr->stat_add[A_INT] += 1;
        if(r_ptr->flags2 & RF2_REFLECTING) p_ptr->reflect = TRUE;
        if(r_ptr->flags2 & RF2_INVISIBLE) p_ptr->invis += 20;
        if(r_ptr->flags2 & RF2_REGENERATE) p_ptr->regenerate = TRUE;
        if(r_ptr->flags2 & RF2_AURA_FIRE) p_ptr->sh_fire = TRUE;
        if(r_ptr->flags2 & RF2_AURA_ELEC) p_ptr->sh_elec = TRUE;
        if(r_ptr->flags2 & RF2_PASS_WALL) p_ptr->wraith_form = TRUE;
        if(r_ptr->flags3 & RF3_SUSCEP_FIRE) p_ptr->sensible_fire = TRUE;
        if(r_ptr->flags3 & RF3_IM_ACID) p_ptr->resist_acid = TRUE;
        if(r_ptr->flags3 & RF3_IM_ELEC) p_ptr->resist_elec = TRUE;
        if(r_ptr->flags3 & RF3_IM_FIRE) p_ptr->resist_fire = TRUE;
        if(r_ptr->flags3 & RF3_IM_POIS) p_ptr->resist_pois = TRUE;
        if(r_ptr->flags3 & RF3_IM_COLD) p_ptr->resist_cold = TRUE;
        if(r_ptr->flags3 & RF3_RES_NETH) p_ptr->resist_neth = TRUE;
        if(r_ptr->flags3 & RF3_RES_NEXU) p_ptr->resist_nexus = TRUE;
        if(r_ptr->flags3 & RF3_RES_DISE) p_ptr->resist_disen = TRUE;
        if(r_ptr->flags3 & RF3_NO_FEAR) p_ptr->resist_fear = TRUE;
        if(r_ptr->flags3 & RF3_NO_SLEEP) p_ptr->free_act = TRUE;
        if(r_ptr->flags3 & RF3_NO_CONF) p_ptr->resist_conf = TRUE;
        if(r_ptr->flags7 & RF7_CAN_FLY) p_ptr->ffall = TRUE;
}


byte calc_mimic()
{
        byte blow=0;

        switch(p_ptr->mimic_form)
        {
                case MIMIC_ABOMINATION:
                {
                        p_ptr->mimic_name="[Abomination]";
                        p_ptr->stat_add[A_STR]-=12;
                        p_ptr->stat_add[A_INT]-=12;
                        p_ptr->stat_add[A_WIS]-=12;
                        p_ptr->stat_add[A_DEX]-=12;
                        p_ptr->stat_add[A_CON]-=12;
                        p_ptr->stat_add[A_CHR]-=12;
                        p_ptr->pspeed-=9;
                        p_ptr->dis_to_d-=7;
                        p_ptr->to_d-=7;
                        p_ptr->dis_to_h-=7;
                        p_ptr->to_h-=7;
                        break;
                }
                case MIMIC_WOLF:
                {
                        p_ptr->mimic_name="[Wolf]";
                        p_ptr->stat_add[A_STR]-=1;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->stat_add[A_INT]-=1;
                        p_ptr->stat_add[A_DEX]+=7;
                        p_ptr->pspeed+=3;
                        p_ptr->dis_to_h+=7;
                        p_ptr->to_h+=7;
                        break;
                }
                case MIMIC_APE:
                {
                        p_ptr->mimic_name="[Ape]";
                        p_ptr->stat_add[A_STR]+=1;
                        p_ptr->stat_add[A_CHR]-=1;
                        p_ptr->stat_add[A_INT]-=2;
                        p_ptr->stat_add[A_DEX]+=7;
                        p_ptr->dis_to_h+=14;
                        p_ptr->to_h+=14;
                        break;
                }
                case MIMIC_GOAT:
                {
                        p_ptr->mimic_name="[Goat]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]-=5;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->pspeed+=1;
                        p_ptr->slow_digest=TRUE;
                        break;
                }
                case MIMIC_INSECT:
                {
                        p_ptr->mimic_name="[Insect]";
                        p_ptr->stat_add[A_STR]-=2;
                        p_ptr->stat_add[A_INT]+=5;
                        p_ptr->stat_add[A_WIS]+=5;
                        p_ptr->stat_add[A_CON]-=5;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->pspeed+=4;
                        p_ptr->telepathy |= ESP_ANIMAL;
                        break;
                }
                case MIMIC_SPARROW:
                {
                        p_ptr->mimic_name="[Sparrow]";
                        p_ptr->stat_add[A_STR]-=2;
                        p_ptr->stat_add[A_INT]-=1;
                        p_ptr->stat_add[A_WIS]-=1;
                        p_ptr->stat_add[A_CON]-=1;
                        p_ptr->stat_add[A_CHR]+=6;
                        p_ptr->pspeed+=6;
                        p_ptr->fly = TRUE;
                        break;
                }
                case MIMIC_ENT:
                {
                        p_ptr->mimic_name="[Ent]";
                        p_ptr->stat_add[A_STR]+=10;
                        p_ptr->stat_add[A_INT]-=4;
                        p_ptr->stat_add[A_WIS]-=4;
                        p_ptr->stat_add[A_DEX]-=5;
                        p_ptr->stat_add[A_CON]+=12;
                        p_ptr->stat_add[A_CHR]+=0;
                        p_ptr->sensible_fire=TRUE;
                        p_ptr->pspeed-=5;
                        break;
                }
                case MIMIC_VAMPIRE:
                {
                        p_ptr->mimic_name="[Vampire]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]+=4;
                        p_ptr->stat_add[A_WIS]-=4;
                        p_ptr->stat_add[A_DEX]+=5;
                        p_ptr->stat_add[A_CON]+=3;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->hold_life=TRUE;
                        p_ptr->resist_dark=TRUE;
                        p_ptr->resist_nexus=TRUE;
                        p_ptr->resist_blind=TRUE;
                        p_ptr->lite=TRUE;
                        break;
                }
                case MIMIC_SPIDER:
                {
                        p_ptr->mimic_name="[Spider]";
                        p_ptr->stat_add[A_STR]-=2;
                        p_ptr->stat_add[A_INT]+=4;
                        p_ptr->stat_add[A_WIS]+=4;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]-=1;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->resist_fear=TRUE;
                        p_ptr->resist_pois=TRUE;
                        break;
                }
                case MIMIC_MANA_BALL:
                {
                        p_ptr->mimic_name="[Mana Ball]";
                        p_ptr->stat_add[A_STR]-=2;
                        p_ptr->stat_add[A_INT]+=8;
                        p_ptr->stat_add[A_WIS]+=4;
                        p_ptr->stat_add[A_DEX]+=8;
                        p_ptr->stat_add[A_CON]-=5;
                        p_ptr->stat_add[A_CHR]-=5;
                        p_ptr->ffall=TRUE;
                        p_ptr->invis+=20;
                        p_ptr->to_s += 1;
                        p_ptr->teleport=TRUE;
                        p_ptr->resist_disen=TRUE;
                        break;
                }
                case MIMIC_FIRE_CLOUD:
                {
                        p_ptr->mimic_name="[Fire Cloud]";
                        p_ptr->stat_add[A_STR]-=2;
                        p_ptr->stat_add[A_INT]+=0;
                        p_ptr->stat_add[A_WIS]+=0;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]-=1;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->resist_lite=TRUE;
                        p_ptr->immune_fire=TRUE;
                        p_ptr->sh_fire=TRUE;
                        break;
                }
                case MIMIC_COLD_CLOUD:
                {
                        p_ptr->mimic_name="[Cold Cloud]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]+=0;
                        p_ptr->stat_add[A_WIS]+=0;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]+=1;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->resist_lite=TRUE;
                        p_ptr->immune_cold=TRUE;
                        p_ptr->sh_elec=TRUE;
                        p_ptr->telepathy |= ESP_EVIL;
                        p_ptr->regenerate=TRUE;
                        break;
                }
                case MIMIC_CHAOS_CLOUD:
                {
                        p_ptr->mimic_name="[Chaos Cloud]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]+=1;
                        p_ptr->stat_add[A_WIS]+=1;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]+=1;
                        p_ptr->stat_add[A_CHR]+=2;
                        p_ptr->resist_disen=TRUE;
                        p_ptr->resist_chaos=TRUE;
                        p_ptr->resist_lite=TRUE;
                        p_ptr->immune_fire=TRUE;
                        p_ptr->immune_cold=TRUE;
                        p_ptr->sh_fire=TRUE;
                        p_ptr->sh_elec=TRUE;
                        break;
                }
                case MIMIC_GOST:
                {
                        p_ptr->mimic_name="[Gost]";
                        p_ptr->stat_add[A_STR]-=4;
                        p_ptr->stat_add[A_INT]+=1;
                        p_ptr->stat_add[A_WIS]+=1;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]+=2;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->wraith_form=TRUE;
                        p_ptr->hold_life=TRUE;
                        break;
                }
                case MIMIC_KOBOLD:
                {
                        p_ptr->mimic_name="[Kobold]";
                        p_ptr->stat_add[A_STR]+=3;
                        p_ptr->stat_add[A_INT]-=3;
                        p_ptr->stat_add[A_WIS]-=2;
                        p_ptr->stat_add[A_DEX]+=0;
                        p_ptr->stat_add[A_CON]+=3;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->resist_pois=TRUE;
                        break;
                }
                case MIMIC_DRAGON:
                {
                        p_ptr->mimic_name="[Dragon]";
                        p_ptr->stat_add[A_STR]+=6;
                        p_ptr->stat_add[A_INT]+=1;
                        p_ptr->stat_add[A_WIS]+=1;
                        p_ptr->stat_add[A_DEX]-=3;
                        p_ptr->stat_add[A_CON]+=6;
                        p_ptr->stat_add[A_CHR]-=4;
                        p_ptr->fly=TRUE;
                        p_ptr->resist_fire=TRUE;
                        p_ptr->resist_cold=TRUE;
                        p_ptr->resist_elec=TRUE;
                        p_ptr->resist_dark=TRUE;
                        blow += 2;
                        break;
                }
                case MIMIC_DEMON:
                {
                        p_ptr->mimic_name="[Demon]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]+=4;
                        p_ptr->stat_add[A_WIS]-=8;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]+=6;
                        p_ptr->stat_add[A_CHR]-=6;
                        p_ptr->hold_life=TRUE;
                        p_ptr->resist_chaos=TRUE;
                        p_ptr->resist_neth=TRUE;
                        blow += 2;
                        break;
                }
                case MIMIC_HOUND:
                {
                        p_ptr->mimic_name="[Hound]";
                        p_ptr->stat_add[A_STR]+=5;
                        p_ptr->stat_add[A_INT]-=1;
                        p_ptr->stat_add[A_WIS]-=1;
                        p_ptr->stat_add[A_DEX]-=1;
                        p_ptr->stat_add[A_CON]+=3;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->pspeed+=5;
                        p_ptr->resist_lite=TRUE;
                        p_ptr->resist_dark=TRUE;
                        break;
                }
                case MIMIC_QUYLTHULG:
                {
                        p_ptr->mimic_name="[Quylthulg]";
                        p_ptr->stat_add[A_STR]+=2;
                        p_ptr->stat_add[A_INT]+=6;
                        p_ptr->stat_add[A_WIS]-=6;
                        p_ptr->stat_add[A_DEX]+=7;
                        p_ptr->stat_add[A_CON]+=5;
                        p_ptr->stat_add[A_CHR]-=7;
                        p_ptr->see_inv=TRUE;
                        p_ptr->immovable = TRUE;
                        break;
                }
                case MIMIC_MAIAR:
                {
                        p_ptr->mimic_name="[Maiar]";
                        p_ptr->stat_add[A_STR]+=15;
                        p_ptr->stat_add[A_INT]+=15;
                        p_ptr->stat_add[A_WIS]+=15;
                        p_ptr->stat_add[A_DEX]+=15;
                        p_ptr->stat_add[A_CON]+=15;
                        p_ptr->stat_add[A_CHR]+=15;
                        p_ptr->immune_fire=TRUE;
                        p_ptr->immune_cold=TRUE;
                        p_ptr->immune_elec=TRUE;
                        p_ptr->immune_acid=TRUE;
                        p_ptr->resist_pois=TRUE;
                        p_ptr->resist_lite=TRUE;
                        p_ptr->resist_dark=TRUE;
                        p_ptr->resist_chaos=TRUE;
                        p_ptr->hold_life=TRUE;
                        p_ptr->ffall=TRUE;
                        p_ptr->regenerate=TRUE;
                        blow += 3;
                        break;
                }
                case MIMIC_SERPENT:
                {
                        p_ptr->mimic_name="[Serpent]";
                        p_ptr->stat_add[A_STR]-=1;
                        p_ptr->stat_add[A_INT]+=3;
                        p_ptr->stat_add[A_WIS]+=3;
                        p_ptr->stat_add[A_DEX]+=6;
                        p_ptr->stat_add[A_CON]+=1;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->see_infra+=100;
                        p_ptr->pspeed+=10;
                        break;
                }
                case MIMIC_GIANT:
                {
                        p_ptr->mimic_name="[Giant]";
                        p_ptr->stat_add[A_STR]+=8;
                        p_ptr->stat_add[A_INT]+=1;
                        p_ptr->stat_add[A_WIS]+=1;
                        p_ptr->stat_add[A_DEX]+=1;
                        p_ptr->stat_add[A_CON]+=9;
                        p_ptr->stat_add[A_CHR]-=2;
                        p_ptr->resist_acid = TRUE;
                        p_ptr->resist_elec = TRUE;
                        p_ptr->resist_fire = TRUE;
                        p_ptr->resist_cold = TRUE;
                        p_ptr->resist_pois = TRUE;
                        p_ptr->resist_conf = TRUE;
                        p_ptr->resist_sound = TRUE;
                        p_ptr->resist_lite = TRUE;
                        p_ptr->resist_dark = TRUE;
                        p_ptr->resist_nexus = TRUE;
                        p_ptr->resist_fear = TRUE;
                        p_ptr->reflect = TRUE;
                        p_ptr->pspeed += 7;
                        blow += 1;
                        break;
                }
                case MIMIC_VALAR:
                {
                        p_ptr->mimic_name="[Valar]";
                        p_ptr->stat_add[A_STR]+=36;
                        p_ptr->stat_add[A_INT]+=36;
                        p_ptr->stat_add[A_WIS]+=36;
                        p_ptr->stat_add[A_DEX]+=36;
                        p_ptr->stat_add[A_CON]+=36;
                        p_ptr->stat_add[A_CHR]+=36;
                        p_ptr->see_inv = TRUE;
                        p_ptr->free_act = TRUE;
                        p_ptr->slow_digest = TRUE;
                        p_ptr->regenerate = TRUE;
                        p_ptr->ffall = TRUE;
                        p_ptr->hold_life = TRUE;
                        p_ptr->telepathy |= ESP_ALL;
                        p_ptr->lite = TRUE;
                        p_ptr->sustain_str = TRUE;
                        p_ptr->sustain_int = TRUE;
                        p_ptr->sustain_wis = TRUE;
                        p_ptr->sustain_con = TRUE;
                        p_ptr->sustain_dex = TRUE;
                        p_ptr->sustain_chr = TRUE;
                        p_ptr->resist_acid = TRUE;
                        p_ptr->resist_elec = TRUE;
                        p_ptr->resist_fire = TRUE;
                        p_ptr->resist_cold = TRUE;
                        p_ptr->resist_pois = TRUE;
                        p_ptr->resist_conf = TRUE;
                        p_ptr->resist_sound = TRUE;
                        p_ptr->resist_lite = TRUE;
                        p_ptr->resist_dark = TRUE;
                        p_ptr->resist_chaos = TRUE;
                        p_ptr->resist_disen = TRUE;
                        p_ptr->resist_shard = TRUE;
                        p_ptr->resist_nexus = TRUE;
                        p_ptr->resist_blind = TRUE;
                        p_ptr->resist_neth = TRUE;
                        p_ptr->resist_fear = TRUE;
                        p_ptr->reflect = TRUE;
                        p_ptr->sh_fire = TRUE;
                        p_ptr->sh_elec = TRUE;
                        p_ptr->immune_acid = TRUE;
                        p_ptr->immune_elec = TRUE;
                        p_ptr->immune_fire = TRUE;
                        p_ptr->immune_cold = TRUE;
                        p_ptr->pspeed += 15;
                        blow += 5;
                        break;
                }

                case MIMIC_WEREWOLF:
		{
                        p_ptr->mimic_name="[Werewolf]";
			p_ptr->stat_add[A_STR] += 2;
			p_ptr->stat_add[A_CHR] -= 5;
			p_ptr->see_infra += 3;
			p_ptr->regenerate = TRUE;
			p_ptr->aggravate = TRUE;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			p_ptr->to_h += 20;
			p_ptr->dis_to_h += 20;
			p_ptr->to_d += 20;
			p_ptr->dis_to_d += 20;
			p_ptr->skill_dev /= 2;
			break;	
		}

                case MIMIC_BALROG:
                {
                        p_ptr->mimic_name="[Balrog]";
                        p_ptr->stat_add[A_STR]+=15;
                        p_ptr->stat_add[A_INT]+=5;
                        p_ptr->stat_add[A_WIS]-=15;
                        p_ptr->stat_add[A_DEX]+=5;
                        p_ptr->stat_add[A_CON]+=15;
                        p_ptr->stat_add[A_CHR]-=15;
                        p_ptr->immune_fire=TRUE;
                        p_ptr->immune_elec=TRUE;
                        p_ptr->immune_acid=TRUE;
                        p_ptr->resist_pois=TRUE;
                        p_ptr->resist_dark=TRUE;
                        p_ptr->resist_chaos=TRUE;
                        p_ptr->hold_life=TRUE;
                        p_ptr->ffall=TRUE;
                        p_ptr->regenerate=TRUE;
                        p_ptr->sh_fire = TRUE;
                        blow += 1;
                        break;
                }
		
                case MIMIC_DEMON_LORD:
                {
                        p_ptr->mimic_name="[Demon Lord]";
                        p_ptr->stat_add[A_STR]+=20;
                        p_ptr->stat_add[A_INT]+=20;
                        p_ptr->stat_add[A_WIS]+=20;
                        p_ptr->stat_add[A_DEX]+=20;
                        p_ptr->stat_add[A_CON]+=20;
                        p_ptr->stat_add[A_CHR]+=20;
                        p_ptr->see_inv = TRUE;
                        p_ptr->free_act = TRUE;
                        p_ptr->regenerate = TRUE;
                        p_ptr->ffall = TRUE;
                        p_ptr->hold_life = TRUE;
                        p_ptr->telepathy |= ESP_EVIL | ESP_GOOD | ESP_DEMON;
                        p_ptr->resist_acid = TRUE;
                        p_ptr->resist_elec = TRUE;
                        p_ptr->resist_fire = TRUE;
                        p_ptr->resist_pois = TRUE;
                        p_ptr->resist_conf = TRUE;
                        p_ptr->resist_sound = TRUE;
                        p_ptr->resist_lite = TRUE;
                        p_ptr->resist_dark = TRUE;
                        p_ptr->resist_chaos = TRUE;
                        p_ptr->resist_disen = TRUE;
                        p_ptr->resist_shard = TRUE;
                        p_ptr->resist_nexus = TRUE;
                        p_ptr->resist_blind = TRUE;
                        p_ptr->resist_neth = TRUE;
                        p_ptr->resist_fear = TRUE;
                        p_ptr->reflect = TRUE;
                        p_ptr->sh_fire = TRUE;
                        p_ptr->immune_acid = TRUE;
                        p_ptr->immune_elec = TRUE;
                        p_ptr->immune_fire = TRUE;
                        p_ptr->pspeed += 15;
                        blow += 4;
                        break;
                }
        }
        return blow;
}

/* Returns the blow information based on class */
void analyze_blow(int *num, int *wgt, int *mul)
{
		/* Analyze the class */
		switch (p_ptr->pclass)
		{
			/* Warrior */
			case CLASS_WARRIOR:
                        case CLASS_UNBELIEVER:
                        case CLASS_WEAPONMASTER:
                                *num = 6; *wgt = 30; *mul = 5; break;

                        case CLASS_MIMIC:
                        case CLASS_HARPER:
                        case CLASS_BEASTMASTER:
                                *num = 5; *wgt = 35; *mul = 4; break;

                        /* Sorcerer */
                        case CLASS_SORCERER:
                                *num = 1; *wgt = 1; *mul = 1; break;

			/* Mage */
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
                        case CLASS_RUNECRAFTER:
                                *num = 4; *wgt = 40; *mul = 2; break;

			/* Rogue */
			case CLASS_ROGUE:
                        case CLASS_SYMBIANT:
                        case CLASS_POSSESSOR:
                                *num = 5; *wgt = 30; *mul = 3; break;

			/* Ranger */
                        case CLASS_ALCHEMIST:
			case CLASS_RANGER:
                                *num = 5; *wgt = 35; *mul = 4; break;

                        /* Druid */
                        case CLASS_DRUID:
                                *num = 4; *wgt = 35; *mul = 3; break;

                        case CLASS_ARCHER:
                                *num = 4; *wgt = 35; *mul = 4; break;

			/* Monk */
			case CLASS_MONK:
                                *num = (p_ptr->lev<40?3:4); *wgt = 40; *mul = 4; break;

		}
}

/* Are all the weapons wielded of the right type ? */
bool is_weaponmaster_weapon()
{
        int i;
        object_type *o_ptr;

        if (p_ptr->pclass != CLASS_WEAPONMASTER) return TRUE;

        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                o_ptr = &inventory[INVEN_WIELD + i];

                /* If we find even only one non specialized weapon it's bad luck for the poor weaponmaster */
                if (inventory[INVEN_WIELD + i].tval != p_ptr->class_extra1) return FALSE;

                i++;
        }

        /* Everything is ok */
        return TRUE;
}

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
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
void calc_bonuses(void)
{
	int             i, j, hold;
        int             old_invis;
	int             old_speed;
	int             old_telepathy;
	int             old_see_inv;
	int             old_dis_ac;
	int             old_dis_to_a;
	int             extra_blows;
	int             extra_shots;
	object_type     *o_ptr;
        u32b            f1, f2, f3, f4, f5, esp;


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

        /* Save the old invisibility */
        old_invis = p_ptr->invis;

	/* Clear extra blows/shots */
	extra_blows = extra_shots = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;

        /* Mana multiplier */
        p_ptr->to_m = 0;

        /* Spell power */
        p_ptr->to_s = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

        /* Start with 0% additionnal crits */
        p_ptr->xtra_crit = 0;

	/* Start with a single blow per turn */
	p_ptr->num_blow = 1;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 1;

	/* Reset the "xtra" tval */
	p_ptr->tval_xtra = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;

	/* Clear all the flags */
        p_ptr->invis = 0;
        p_ptr->immovable = FALSE;
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
        p_ptr->drain_mana = 0;
        p_ptr->drain_life = 0;
	p_ptr->bless_blade = FALSE;
        p_ptr->xtra_might = 0;
        p_ptr->auto_id = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
        p_ptr->fly = FALSE;
        p_ptr->climb = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
        p_ptr->telepathy = 0;
	p_ptr->lite = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;
	p_ptr->resist_acid = FALSE;
	p_ptr->resist_elec = FALSE;
	p_ptr->resist_fire = FALSE;
	p_ptr->resist_cold = FALSE;
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
        p_ptr->immune_neth = FALSE;
	p_ptr->resist_fear = FALSE;
        p_ptr->resist_continuum = FALSE;
	p_ptr->reflect = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;

        p_ptr->sensible_fire = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;

        p_ptr->precognition = FALSE;

        p_ptr->wraith_form = FALSE;

        /* The anti magic field surrounding the player */
        p_ptr->antimagic = 0;
        p_ptr->antimagic_dis = 0;


	/* Base infravision (purely racial) */
        p_ptr->see_infra = rp_ptr->infra + rmp_ptr->infra;


	/* Base skill -- disarming */
        p_ptr->skill_dis = rp_ptr->r_dis + rmp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
        p_ptr->skill_dev = rp_ptr->r_dev + rmp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
        p_ptr->skill_sav = rp_ptr->r_sav + rmp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- stealth */
        p_ptr->skill_stl = rp_ptr->r_stl + rmp_ptr->r_stl + cp_ptr->c_stl;

	/* Base skill -- searching ability */
        p_ptr->skill_srh = rp_ptr->r_srh + rmp_ptr->r_srh + cp_ptr->c_srh;

	/* Base skill -- searching frequency */
        p_ptr->skill_fos = rp_ptr->r_fos + rmp_ptr->r_fos + cp_ptr->c_fos;

	/* Base skill -- combat (normal) */
        p_ptr->skill_thn = rp_ptr->r_thn + rmp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
        p_ptr->skill_thb = rp_ptr->r_thb + rmp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
        p_ptr->skill_tht = rp_ptr->r_thb + rmp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

        /* Mimic override body's bonuses */
        if(p_ptr->mimic_form)
        {
                extra_blows += calc_mimic();
        }
        else
        {
                calc_body_bonus();
        }

        /* The powers gived by the wielded monster */
        calc_wield_monster();

        /* Calculate bonuses due to gods. */
        if (p_ptr->pgod) {
          int badness = interpret_grace();

          god_effect(p_ptr->pgod, badness);
        }

	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
			if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;
			break;
                case CLASS_BEASTMASTER:
                        if (p_ptr->lev > 34) p_ptr->resist_fear = TRUE;
			break;
		case CLASS_MONK:
			/* Unencumbered Monks become faster every 10 levels */
			if (!(monk_heavy_armor()))
					p_ptr->pspeed += (p_ptr->lev) / 10;

			/* Free action if unencumbered at level 25 */
			if  ((p_ptr->lev > 24) && !(monk_heavy_armor()))
				p_ptr->free_act = TRUE;
			break;
                case CLASS_UNBELIEVER:
                        p_ptr->antimagic += p_ptr->lev;
                        p_ptr->antimagic_dis += (p_ptr->lev / 10) + 1;

                        if (p_ptr->class_extra6 & CLASS_ANTIMAGIC)
                        {
                                p_ptr->anti_magic = TRUE;
                                p_ptr->anti_tele = TRUE;
                                p_ptr->resist_continuum = TRUE;
                        }
			break;
		case CLASS_WEAPONMASTER:
			if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;
			break;
	}

	/***** Races ****/
        if((!p_ptr->mimic_form)&&(!p_ptr->body_monster))
        {
	switch (p_ptr->prace)
	{
                case RACE_WOOD_ELF:
                        if (p_ptr->pracem != RMOD_VAMPIRE) p_ptr->resist_lite = TRUE;
			break;
		case RACE_ELF:
                        if (p_ptr->pracem != RMOD_VAMPIRE) p_ptr->resist_lite = TRUE;
			break;
		case RACE_HOBBIT:
			p_ptr->sustain_dex = TRUE;
			break;
		case RACE_DWARF:
			p_ptr->resist_blind = TRUE;
			break;
		case RACE_HALF_ORC:
			p_ptr->resist_dark = TRUE;
			break;
		case RACE_HALF_TROLL:
			p_ptr->sustain_str = TRUE;

			if (p_ptr->lev > 14)
			{
				/* High level trolls heal fast... */
				p_ptr->regenerate = TRUE;

				if (p_ptr->pclass == CLASS_WARRIOR)
				{
					p_ptr->slow_digest = TRUE;
					/* Let's not make Regeneration
					 * a disadvantage for the poor warriors who can
					 * never learn a spell that satisfies hunger (actually
					 * neither can rogues, but half-trolls are not
					 * supposed to play rogues) */
				}
			}
			break;
                case RACE_DUNADAN:
			p_ptr->sustain_con = TRUE;
                        p_ptr->regenerate = TRUE;
			break;
		case RACE_HIGH_ELF:
                        if (p_ptr->pracem != RMOD_VAMPIRE) p_ptr->resist_lite = TRUE;
			p_ptr->see_inv = TRUE;
			break;
	        case RACE_RKNIGHT:
                        /* Rohan's Knights become faster */
                        p_ptr->pspeed += 3;
                        p_ptr->pspeed += (p_ptr->lev) / 5;
			break;
                case RACE_ENT:
			p_ptr->slow_digest = TRUE;
                        p_ptr->sensible_fire = TRUE;
                        p_ptr->pspeed -= 5;
                        if (p_ptr->lev > 4) p_ptr->see_inv = TRUE;
                        if (p_ptr->lev > 24) p_ptr->telepathy |= ESP_ORC | ESP_TROLL | ESP_EVIL;
			break;
                case RACE_DRAGONRIDDER:
			p_ptr->ffall = TRUE;
                        if (p_ptr->lev >= 17) p_ptr->fly = TRUE;

                        /* Clear the Tanker point */
                        p_ptr->mtp = 100;

                        p_ptr->mtp+=p_ptr->lev*5;
                        if(p_ptr->ctp>p_ptr->mtp)p_ptr->ctp=p_ptr->mtp;
                        p_ptr->redraw |= PR_TANK;

                        if (p_ptr->lev >  3) p_ptr->telepathy |= ESP_DRAGONRIDDER | ESP_DRAGON;
			if (p_ptr->lev >  4) p_ptr->resist_fire = TRUE;
			if (p_ptr->lev >  9) p_ptr->resist_cold = TRUE;
			if (p_ptr->lev > 14) p_ptr->resist_acid = TRUE;
			if (p_ptr->lev > 19) p_ptr->resist_elec = TRUE;
			if (p_ptr->lev > 34) p_ptr->resist_pois = TRUE;
			break;
		default:
			/* Do nothing */
			;
	}
        switch (p_ptr->pracem)
	{
                case RMOD_VAMPIRE:
			p_ptr->resist_dark = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->resist_neth = TRUE;
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->lite = TRUE;
			break;
                case RMOD_SPECTRE:
			p_ptr->resist_neth = TRUE;
                        p_ptr->immune_neth = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->slow_digest = TRUE;
			p_ptr->resist_cold = TRUE;
                        p_ptr->wraith_form = TRUE;
                        if (p_ptr->lev > 34) p_ptr->telepathy |= ESP_UNIQUE | ESP_GOOD | ESP_EVIL;
			break;
                case RMOD_SKELETON:
                        p_ptr->resist_shard = TRUE;
                        p_ptr->hold_life = TRUE;
                        p_ptr->see_inv = TRUE;
                        p_ptr->resist_pois = TRUE;
                        if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
                        break;
                case RMOD_ZOMBIE:
                        p_ptr->resist_neth = TRUE;
                        p_ptr->hold_life = TRUE;
                        p_ptr->see_inv = TRUE;
                        p_ptr->resist_pois = TRUE;
                        p_ptr->slow_digest = TRUE;
                        if (p_ptr->lev > 4) p_ptr->resist_cold = TRUE;
                        break;
                case RMOD_BARBARIAN:
			p_ptr->resist_fear = TRUE;
			break;
		default:
			/* Do nothing */
			;
	}
        }

	/* Hack -- apply racial/class stat maxes */
	if (p_ptr->maximize)
	{
		/* Apply the racial modifiers */
		for (i = 0; i < 6; i++)
		{
			/* Modify the stats for "race" */
                        p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + rmp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}
	}


        /* I'm adding the corruptions here for the lack of a better place... */
	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			p_ptr->stat_add[A_STR] += 4;
		}
		
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			p_ptr->stat_add[A_STR] -= 4;
		}
		
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			p_ptr->stat_add[A_INT] += 4;
			p_ptr->stat_add[A_WIS] += 4;
		}
		
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			p_ptr->stat_add[A_INT] -= 4;
			p_ptr->stat_add[A_WIS] -= 4;
		}
		
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			p_ptr->stat_add[A_CON] += 4;
		}
		
		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			p_ptr->stat_add[A_CON] += 2;
			p_ptr->pspeed -= 2;
		}
		
		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			p_ptr->stat_add[A_CON] -= 4;
		}
		
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			p_ptr->stat_add[A_CON] -= 2;
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->regenerate = FALSE;
			/* Cancel innate regeneration */
		}
		
		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			p_ptr->stat_add[A_CHR] -= 4;
		}
		
		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
			p_ptr->stat_add[A_CHR] -= 1;
		}
		
		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
			p_ptr->skill_fos += 15;
			p_ptr->skill_srh += 15;
		}
		
		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			p_ptr->skill_sav += (15 + (p_ptr->lev / 5));
		}
		
		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			p_ptr->skill_stl -= 3;
		}
		
		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			p_ptr->see_infra += 3;
		}
		
		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			p_ptr->pspeed += 3;
		}
		
		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			p_ptr->pspeed -= 3;
		}
		
		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			p_ptr->sh_elec = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			p_ptr->sh_fire = TRUE;
			p_ptr->lite = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			p_ptr->stat_add[A_CHR] -= 2;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
		}
		
		if (p_ptr->muta3 & MUT3_SCALES)
		{
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
		}
		
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
		}
		
		if (p_ptr->muta3 & MUT3_WINGS)
		{
			p_ptr->ffall = TRUE;
                        p_ptr->fly = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			p_ptr->resist_fear = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_REGEN)
		{
			p_ptr->regenerate = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_ESP)
		{
                        p_ptr->telepathy |= (((1 << (p_ptr->lev * 32 / 50)) - 1) << 1) + 1;
		}

		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			p_ptr->stat_add[A_DEX] += 3;
		}
		
		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			p_ptr->stat_add[A_DEX] -= 3;
		}
		
		if (p_ptr->muta3 & MUT3_MOTION)
		{
			p_ptr->free_act =TRUE;
			p_ptr->skill_stl += 1;
		}
		
		if (p_ptr->muta3 & MUT3_SUS_STATS)
		{
			p_ptr->sustain_con =TRUE;
			if (p_ptr->lev > 9)
				p_ptr->sustain_str = TRUE;
			if (p_ptr->lev > 19)
				p_ptr->sustain_dex = TRUE;
			if (p_ptr->lev > 29)
				p_ptr->sustain_wis = TRUE;
			if (p_ptr->lev > 39)
				p_ptr->sustain_int = TRUE;
			if (p_ptr->lev > 49)
				p_ptr->sustain_chr = TRUE;
		}
		
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}
    

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
                object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                /* MEGA ugly hack -- set spacetime distortion resistance */
                if(o_ptr->name1 == ART_ANCHOR)
                {
                        p_ptr->resist_continuum = TRUE;
                }

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->pval;

                /* Affect spell power */
                if (f1 & (TR1_SPELL)) p_ptr->to_s += o_ptr->pval;

                /* Affect mana capacity */
                if (f1 & (TR1_MANA)) p_ptr->to_m += o_ptr->pval;

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->skill_stl += o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_srh += (o_ptr->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_fos += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA)) p_ptr->see_infra += o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->skill_dig += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->pspeed += o_ptr->pval;

		/* Affect blows */
                if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;
                if (f5 & (TR5_CRIT)) p_ptr->xtra_crit += o_ptr->pval;

		/* Hack -- cause earthquakes */
		if (f1 & (TR1_IMPACT)) p_ptr->impact = TRUE;

		/* Affect invisibility */
                if (f2 & (TR2_INVIS)) p_ptr->invis += (o_ptr->pval * 10);

		/* Boost shots */
		if (f3 & (TR3_XTRA_SHOTS)) extra_shots++;

		/* Various flags */
		if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
                if (f5 & (TR5_DRAIN_MANA)) p_ptr->drain_mana++;
                if (f5 & (TR5_DRAIN_HP)) p_ptr->drain_life++;
		if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;
                if (f3 & (TR3_XTRA_MIGHT)) p_ptr->xtra_might += o_ptr->pval;
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
                if (esp) p_ptr->telepathy |= esp;
                if ((o_ptr->tval != TV_LITE) && (f3 & (TR3_LITE1))) p_ptr->lite = TRUE;
                if ((o_ptr->tval != TV_LITE) && (f4 & (TR4_LITE2))) p_ptr->lite = TRUE;
                if ((o_ptr->tval != TV_LITE) && (f4 & (TR4_LITE3))) p_ptr->lite = TRUE;
		if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
		if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
                if (f3 & (TR3_WRAITH)) p_ptr->wraith_form = TRUE;
		if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
                if (f4 & (TR4_FLY)) p_ptr->fly = TRUE;
                if (f4 & (TR4_CLIMB)) p_ptr->climb = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID)) p_ptr->resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC)) p_ptr->resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE)) p_ptr->resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD)) p_ptr->resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS)) p_ptr->resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_CONF)) p_ptr->resist_conf = TRUE;
		if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;
		if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_NETHER)) p_ptr->resist_neth = TRUE;
                if (f4 & (TR4_IM_NETHER)) p_ptr->immune_neth = TRUE;

		if (f2 & (TR2_REFLECT)) p_ptr->reflect = TRUE;
		if (f3 & (TR3_SH_FIRE)) p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC)) p_ptr->sh_elec = TRUE;
		if (f3 & (TR3_NO_MAGIC)) p_ptr->anti_magic = TRUE;
		if (f3 & (TR3_NO_TELE)) p_ptr->anti_tele = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;

                if (f4 & (TR4_PRECOGNITION)) p_ptr->precognition = TRUE;

                if (f4 & (TR4_ANTIMAGIC_50))
                {
                        p_ptr->antimagic += 50 - o_ptr->to_h - o_ptr->to_d - o_ptr->pval - o_ptr->to_a;
                        p_ptr->antimagic_dis += 5 - ((o_ptr->to_h + o_ptr->to_d + o_ptr->pval + o_ptr->to_a) / 15);
                }
                if (f4 & (TR4_ANTIMAGIC_30))
                {
                        p_ptr->antimagic += 30 - o_ptr->to_h - o_ptr->to_d - o_ptr->pval - o_ptr->to_a;
                        p_ptr->antimagic_dis += 3 - ((o_ptr->to_h + o_ptr->to_d + o_ptr->pval + o_ptr->to_a) / 15);
                }
                if (f4 & (TR4_ANTIMAGIC_20))
                {
                        p_ptr->antimagic += 20 - o_ptr->to_h - o_ptr->to_d - o_ptr->pval - o_ptr->to_a;
                        p_ptr->antimagic_dis += 2;
                }
                if (f4 & (TR4_ANTIMAGIC_10))
                {
                        p_ptr->antimagic += 10 - o_ptr->to_h - o_ptr->to_d - o_ptr->pval - o_ptr->to_a;
                        p_ptr->antimagic_dis += 1;
                }
                if (f4 & (TR4_AUTO_ID))
                {
                        p_ptr->auto_id = TRUE;
                }

                /* The new code implementing Tolkien's concept of "Black Breath"
                 * takes advantage of the existing drain_exp character flag, renamed
                 * "black_breath". This flag can also be set by a unlucky blow from
                 * an undead.  -LM-
                 */
                if (f4 & (TR4_BLACK_BREATH)) p_ptr->black_breath = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
                if (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
                if (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_BOW) continue;

                /* Hack -- do not apply "ammo" bonuses */
                if (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_AMMO) continue;

                /* Hack -- do not apply "tool" bonuses */
                if (p_ptr->body_parts[i - INVEN_WIELD] == INVEN_TOOL) continue;

		/* Apply the bonuses to hit/damage */
                p_ptr->to_h += o_ptr->to_h;
                p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}

    /* Monks get extra ac for armour _not worn_ */
    if ((p_ptr->pclass == CLASS_MONK) && !(monk_heavy_armor()))
    {
        if (!(inventory[INVEN_BODY].k_idx))
        {
            p_ptr->to_a += (p_ptr->lev * 3) / 2;
            p_ptr->dis_to_a += (p_ptr->lev * 3) / 2;
        }
        if (!(inventory[INVEN_OUTER].k_idx) && (p_ptr->lev > 15))
        {
            p_ptr->to_a += ((p_ptr->lev - 13) / 3);
            p_ptr->dis_to_a += ((p_ptr->lev - 13) / 3);
        }
        if (!(inventory[INVEN_ARM].k_idx) && (p_ptr->lev > 10))
        {
            p_ptr->to_a += ((p_ptr->lev - 8) / 3);
            p_ptr->dis_to_a += ((p_ptr->lev - 8) / 3);
        }
        if (!(inventory[INVEN_HEAD].k_idx)&& (p_ptr->lev > 4))
        {
            p_ptr->to_a += (p_ptr->lev - 2) / 3;
            p_ptr->dis_to_a += (p_ptr->lev -2) / 3;
        }
        if (!(inventory[INVEN_HANDS].k_idx))
        {
            p_ptr->to_a += (p_ptr->lev / 2);
            p_ptr->dis_to_a += (p_ptr->lev / 2);
        }
        if (!(inventory[INVEN_FEET].k_idx))
        {
            p_ptr->to_a += (p_ptr->lev / 3);
            p_ptr->dis_to_a += (p_ptr->lev / 3);
        }
    }

	/* Hack -- aura of fire also provides light */
	if (p_ptr->sh_fire) p_ptr->lite = TRUE;

        if (p_ptr->prace == RACE_ENT) /* Ents also get an intrinsic AC bonus */
	{
		p_ptr->to_a += 20 + (p_ptr->lev / 5);
		p_ptr->dis_to_a += 20 + (p_ptr->lev / 5);
	}

	/* Calculate stats */
	for (i = 0; i < 6; i++)
	{
		int top, use, ind;


		/* Extract the new "stat_use" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);

		/* Notice changes */
		if (p_ptr->stat_top[i] != top)
		{
			/* Save the new value */
			p_ptr->stat_top[i] = top;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		if ((i == A_CHR) && (p_ptr->muta3 & MUT3_ILL_NORM))
		{
			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < 8 + 2 * p_ptr->lev)
			{
				use = 8 + 2 * p_ptr->lev;
			}
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != use)
		{
			/* Save the new value */
			p_ptr->stat_use[i] = use;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
		

		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Notice changes */
		if (p_ptr->stat_ind[i] != ind)
		{
			/* Save the new index */
			p_ptr->stat_ind[i] = ind;

			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{
				if (mp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (mp_ptr->spell_stat == A_WIS)
				{
                                        p_ptr->update |= (PU_MANA | PU_SPELLS | PU_SANITY);
				}
			}

                        /* Change in CHR may affect Mana/Spells */
                        else if (i == A_CHR)
			{
                                if (mp_ptr->spell_stat == A_CHR)
				{
                                        p_ptr->update |= (PU_MANA | PU_SPELLS | PU_SANITY);
				}
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
	}

   /* jk - add in the tactics */

   p_ptr->dis_to_h += tactic_info[(byte)p_ptr->tactic].to_hit;
   p_ptr->to_h += tactic_info[(byte)p_ptr->tactic].to_hit;
   p_ptr->dis_to_d += tactic_info[(byte)p_ptr->tactic].to_dam;
   p_ptr->to_d += tactic_info[(byte)p_ptr->tactic].to_dam;
   p_ptr->dis_to_a += tactic_info[(byte)p_ptr->tactic].to_ac;
   p_ptr->to_a += tactic_info[(byte)p_ptr->tactic].to_ac;

   p_ptr->skill_stl += tactic_info[(byte)p_ptr->tactic].to_stealth;
   p_ptr->skill_dis += tactic_info[(byte)p_ptr->tactic].to_disarm;
   p_ptr->skill_sav += tactic_info[(byte)p_ptr->tactic].to_saving;

   p_ptr->pspeed += move_info[(byte)p_ptr->movement].to_speed;
   p_ptr->skill_srh += move_info[(byte)p_ptr->movement].to_search;
   p_ptr->skill_fos += move_info[(byte)p_ptr->movement].to_percep;
   p_ptr->skill_stl += move_info[(byte)p_ptr->movement].to_stealth;


	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}


	/* Invulnerability */
	if (p_ptr->invuln)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* wraith_form */
        if (p_ptr->tim_wraith)
	{
                if(p_ptr->disembodied)
                {
                        p_ptr->to_a += 10;
                        p_ptr->dis_to_a += 10;
                }
                else
                {
                        p_ptr->to_a += 50;
                        p_ptr->dis_to_a += 50;
                        p_ptr->reflect = TRUE;
                }
                p_ptr->wraith_form = TRUE;
	}

        /* Temporary holy aura */
        if (p_ptr->holy)
	{
                p_ptr->hold_life = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temporary invisibility */
        if (p_ptr->tim_invisible)
	{
                p_ptr->invis += p_ptr->tim_inv_pow;
	}

	/* Temporary shield */
	if (p_ptr->shield)
	{
                p_ptr->to_a += p_ptr->shield_power;
                p_ptr->dis_to_a += p_ptr->shield_power;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
	}

        /* Temporary "Accurancy" */
        if (p_ptr->strike)
	{
                p_ptr->to_d += 15;
                p_ptr->dis_to_d += 15;
                p_ptr->to_h += 15;
                p_ptr->dis_to_h += 15;
	}

        /* Temporary "Meditation" */
        if (p_ptr->meditation)
	{
                p_ptr->to_d -= 25;
                p_ptr->dis_to_d -= 25;
                p_ptr->to_h -= 25;
                p_ptr->dis_to_h -= 25;
	}

        /* Temporary "Reflection" */
        if (p_ptr->tim_reflect)
	{
                p_ptr->reflect = TRUE;
	}

        /* Temporary "Time Resistance" */
        if (p_ptr->tim_res_time)
	{
                p_ptr->resist_continuum = TRUE;
	}

        /* Temporary "Levitation" */
        if (p_ptr->tim_ffall)
	{
                p_ptr->ffall = TRUE;
	}

        /* Temporary "Fire Aura" */
        if (p_ptr->tim_fire_aura)
	{
                p_ptr->sh_fire = TRUE;
	}

        /* Oppose Light & Dark */
        if (p_ptr->oppose_ld)
	{
                p_ptr->resist_lite = TRUE;
                p_ptr->resist_dark = TRUE;
	}

        /* Oppose Chaos & Confusion */
        if (p_ptr->oppose_cc)
	{
                p_ptr->resist_chaos = TRUE;
                p_ptr->resist_conf = TRUE;
	}

        /* Oppose Sound & Shards */
        if (p_ptr->oppose_ss)
	{
                p_ptr->resist_sound = TRUE;
                p_ptr->resist_shard = TRUE;
	}

        /* Oppose Nexus */
        if (p_ptr->oppose_nex)
	{
                p_ptr->resist_nexus = TRUE;
	}

        /* Mental barrier */
        if (p_ptr->tim_mental_barrier)
	{
                p_ptr->sustain_int = TRUE;
                p_ptr->sustain_wis = TRUE;
	}

	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
	}

        /* Temporary "light speed" */
        if (p_ptr->lightspeed)
	{
                p_ptr->pspeed += 50;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
	}

	if (p_ptr->tim_esp)
	{
                p_ptr->telepathy |= ESP_ALL;
	}

	/* Temporary see invisible */
	if (p_ptr->tim_invis)
	{
		p_ptr->see_inv = TRUE;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}

        /* Hack -- Can Fly -> Can Levitate */
        if (p_ptr->fly)
	{
                p_ptr->ffall = TRUE;
	}

	/* Hack -- Res Chaos -> Res Conf */
	if (p_ptr->resist_chaos)
	{
		p_ptr->resist_conf = TRUE;
	}

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}


	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		p_ptr->update |= (PU_MONSTERS);
	}


	/* Extract the current weight (in tenth pounds) */
	j = calc_total_weight();

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

        /* In order to get a "nice" mana path druids need to ahve a 0 speed */
        if ((p_ptr->class_extra7 == CLASS_MANA_PATH) && (p_ptr->pspeed > 110))
                p_ptr->pspeed = 110;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);


	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];


	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carholdry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}

        /* Take note of required "tval" for missiles */
        switch (o_ptr->sval)
        {
                case SV_SLING:
                {
                        p_ptr->tval_ammo = TV_SHOT;
                        break;
                }

                case SV_SHORT_BOW:
                case SV_LONG_BOW:
                {
                        p_ptr->tval_ammo = TV_ARROW;
                        break;
                }

                case SV_LIGHT_XBOW:
                case SV_HEAVY_XBOW:
                {
                        p_ptr->tval_ammo = TV_BOLT;
                        break;
                }
        }

	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx && !p_ptr->heavy_shoot)
	{
		/* Hack -- Reward High Level Rangers using Bows */
		if (((p_ptr->pclass == 4) && (p_ptr->tval_ammo == TV_ARROW)))
		{
			/* Extra shot at level 20 */
			if (p_ptr->lev >= 20) p_ptr->num_fire++;

			/* Extra shot at level 40 */
			if (p_ptr->lev >= 40) p_ptr->num_fire++;
		}

                /* Hack -- Reward High Level Archer */
                if (p_ptr->pclass == CLASS_ARCHER)
		{
                        /* Extra shot at level 15 */
                        if (p_ptr->lev >= 15) p_ptr->num_fire++;

                        /* Extra shot at level 30 */
                        if (p_ptr->lev >= 30) p_ptr->num_fire++;

                        /* Extra shot at level 45 */
                        if (p_ptr->lev >= 45) p_ptr->num_fire++;

                        /* Extra might at level 25 */
                        if (p_ptr->lev >= 25) p_ptr->xtra_might++;
		}

                /* Hack -- Reward Wood Elves */
                if (p_ptr->prace == RACE_WOOD_ELF)
		{
                        p_ptr->xtra_might++;

                        /* Even better if bow & high lvl */
                        if ((p_ptr->tval_ammo == TV_ARROW) && (p_ptr->lev >= 25)) p_ptr->xtra_might++;
		}

		/*
		 * Addendum -- also "Reward" high level warriors,
		 * with _any_ missile weapon -- TY
		 */
		if (p_ptr->pclass == CLASS_WARRIOR &&
		    (p_ptr->tval_ammo <= TV_BOLT) &&
		    (p_ptr->tval_ammo >= TV_SHOT))
		{
			/* Extra shot at level 25 */
			if (p_ptr->lev >= 25) p_ptr->num_fire++;

			/* Extra shot at level 50 */
			if (p_ptr->lev >= 50) p_ptr->num_fire++;
		}

		/* Add in the "bonus shots" */
		p_ptr->num_fire += extra_shots;

		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}

        /* Boost digging skill by tool weight */
        if(inventory[INVEN_TOOL].k_idx && (inventory[INVEN_TOOL].tval == TV_DIGGING))
        {
		p_ptr->skill_dig += (o_ptr->weight / 10);
        }

        /* Examine the main weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
        if ((hold < o_ptr->weight / 10)||(beastmaster_whip()==TRUE))
	{
		/* Hard to wield a heavy weapon */
                if(beastmaster_whip()==TRUE){
                        p_ptr->to_h -= (o_ptr->weight / 10);
                        p_ptr->dis_to_h -= (o_ptr->weight / 10);
                }else{
                        p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
                        p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);
                }

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}

	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

                analyze_blow(&num, &wgt, &mul);

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		/* Access the strength vs weight */
		str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > num) p_ptr->num_blow = num;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR: /* 3 extra blows */
                        case CLASS_UNBELIEVER:
				p_ptr->num_blow += (p_ptr->lev / 15);
				break;
			case CLASS_WEAPONMASTER:
			/*
			 * Weaponmasters only get 1 blow with weapons that
			 * don't match their specialty. Otherwise, they get a
			 * bonus of 1-3 blows (as per Warriors). -- Gumby
			 */
                                if (!is_weaponmaster_weapon())
					p_ptr->num_blow = 1;
                                else
					p_ptr->num_blow += (p_ptr->lev / 15);
				break;
			default: /* no extra blows */
				break;
		}

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;
	}
        /* Monsters that only have their "natural" attacks */
        else if (!r_info[p_ptr->body_monster].body_parts[BODY_WEAPON])
        {
		int str_index, dex_index;
                int num = 0, wgt = 0, mul = 0;

                analyze_blow(&num, &wgt, &mul);

		/* Access the strength vs weight */
                str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / 3);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Maximal value */
                if (p_ptr->num_blow > 4) p_ptr->num_blow = 4;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;
        }

	/* Different calculation for monks with empty hands */
	else if (p_ptr->pclass == CLASS_MONK && monk_empty_hands())
	{
		p_ptr->num_blow = 0;

		if (p_ptr->lev >  9) p_ptr->num_blow++;
		if (p_ptr->lev > 19) p_ptr->num_blow++;
		if (p_ptr->lev > 29) p_ptr->num_blow++;
		if (p_ptr->lev > 34) p_ptr->num_blow++;
		if (p_ptr->lev > 39) p_ptr->num_blow++;
		if (p_ptr->lev > 44) p_ptr->num_blow++;
		if (p_ptr->lev > 49) p_ptr->num_blow++;

		if (monk_heavy_armor()) p_ptr->num_blow /= 2;

		p_ptr->num_blow += 1 + extra_blows;

		if (!monk_heavy_armor())
		{
			p_ptr->to_h += (p_ptr->lev / 3);
			p_ptr->to_d += (p_ptr->lev / 3);

			p_ptr->dis_to_h += (p_ptr->lev / 3);
			p_ptr->dis_to_d += (p_ptr->lev / 3);
		}
	}
 
	/* Assume okay */
	p_ptr->icky_wield = FALSE;
	monk_armour_aux = FALSE;

	/*
	 * Bonuses to-hit and to-dam based on class. Was a series of if()
	 * statements, but switches are *much* better. -- Gumby
	 */
	switch (p_ptr->pclass)
	{
                case CLASS_WARRIOR: case CLASS_UNBELIEVER:
			p_ptr->to_h += (p_ptr->lev/5);
			p_ptr->to_d += (p_ptr->lev/5);
			p_ptr->dis_to_h += (p_ptr->lev/5);
			p_ptr->dis_to_d += (p_ptr->lev/5);
			break;
		/*
		 * Weaponmasters are fabulous with their preferred weapon
		 * type and all other weapon types are 'icky'.
		 */
		case CLASS_WEAPONMASTER:
                        if (!is_weaponmaster_weapon())
			{
				p_ptr->icky_wield = TRUE;
			}
                        else
			{
                                p_ptr->to_h += p_ptr->lev / 2;
                                p_ptr->to_d += p_ptr->lev / 2;
                                p_ptr->dis_to_h += p_ptr->lev / 2;
                                p_ptr->dis_to_d += p_ptr->lev / 2;
			}
			break;
		default: /* no bonuses */
			break;
	}

        /* Parse all the weapons */
        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                o_ptr = &inventory[INVEN_WIELD + i];

                /* 2handed weapon and shield = less damage */
                if (inventory[INVEN_WIELD + i].k_idx && inventory[INVEN_ARM + i].k_idx)
                {
                        /* Extract the item flags */
                        object_flags(&inventory[INVEN_WIELD + i], &f1, &f2, &f3, &f4, &f5, &esp);

                        if (f4 & TR4_COULD2H)
                        {                
                                /* Reduce the real bonuses */
                                p_ptr->to_h = (3 * p_ptr->to_h) / 4;
                                p_ptr->to_d = (3 * p_ptr->to_d) / 4;

                                /* Reduce the mental bonuses */
                                p_ptr->dis_to_h = (3 * p_ptr->dis_to_h) / 4;
                                p_ptr->dis_to_d = (3 * p_ptr->dis_to_d) / 4;
                        }
                }

                /* Sorcerer can't wield a weapon unless it's a mage staff */
                if (p_ptr->pclass == CLASS_SORCERER)
                {
                        if ((o_ptr->tval != TV_MSTAFF) && (o_ptr->k_idx))
                        {
                                /* Reduce the real bonuses */
                                p_ptr->to_h -= 200;
                                p_ptr->to_d -= 200;

                                /* Reduce the mental bonuses */
                                p_ptr->dis_to_h -= 200;
                                p_ptr->dis_to_d -= 200;

                                /* Icky weapon */
                                p_ptr->icky_wield = TRUE;
                        }
                        else
                        {
                                /* Reduce the real bonuses */
                                p_ptr->to_h -= 10;
                                p_ptr->to_d -= 10;

                                /* Reduce the mental bonuses */
                                p_ptr->dis_to_h -= 10;
                                p_ptr->dis_to_d -= 10;
                        }
                }

                /* Check next weapon */
                i++;
        }

	if (monk_heavy_armor())
	{
		monk_armour_aux = TRUE;
	}

	/* Hack by SBF: Rogues get a stealth bonus every 5 levels */
	/* while Rangers and Monks get one every 10 levels. */
        if (!p_ptr->aggravate)
	{
		if (p_ptr->pclass == CLASS_ROGUE)
		{
			p_ptr->skill_stl += (p_ptr->lev/5);   /* give a stealth bonus */
		}

		if (p_ptr->pclass == CLASS_RANGER || p_ptr->pclass == CLASS_MONK) 
		{
			p_ptr->skill_stl += (p_ptr->lev/10); /* give a stealth bonus */
		}
	}

	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill_dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
	p_ptr->skill_dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill_dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

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

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 10);


	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95)) p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			msg_print("You have no trouble wielding your bow.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}

		/* Save it */
		p_ptr->old_heavy_shoot = p_ptr->heavy_shoot;
	}


	/* Take note when "heavy weapon" changes */
	if (p_ptr->old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}

		/* Save it */
		p_ptr->old_heavy_wield = p_ptr->heavy_wield;
	}


	/* Take note when "illegal weapon" changes */
	if (p_ptr->old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			if (p_ptr->pclass == CLASS_WEAPONMASTER)
				msg_print("You feel uncomfortable without your preferred weapon.");
			else
				msg_print("You do not feel comfortable with your weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You feel comfortable with your weapon.");
		}
		else
		{
			msg_print("You feel more comfortable after removing your weapon.");
		}

		/* Save it */
		p_ptr->old_icky_wield = p_ptr->icky_wield;
	}

	if (p_ptr->pclass == CLASS_MONK && (monk_armour_aux != monk_notify_aux))
	{
		if (monk_heavy_armor())
			msg_print("The weight of your armor disrupts your balance.");
		else
			msg_print("You regain your balance.");
		monk_notify_aux = monk_armour_aux;
	}

        /* resistance to fire cancel sensibility to fire */
        if(p_ptr->resist_fire || p_ptr->oppose_fire || p_ptr->immune_fire)
                p_ptr->sensible_fire=FALSE;
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
	}
}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;

        if (p_ptr->update & (PU_BODY))
	{
                p_ptr->update &= ~(PU_BODY);
                calc_body();
	}

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
                calc_bonuses();
			
                /* Ok now THAT is an ugly hack */
                p_ptr->update &= ~(PU_POWERS);
                calc_powers();
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		calc_hitpoints();
	}

        if (p_ptr->update & (PU_SANITY))
        {
                p_ptr->update &= ~(PU_SANITY);
                calc_sanity();
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		calc_mana();
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}

        if (p_ptr->update & (PU_POWERS))
	{
                p_ptr->update &= ~(PU_POWERS);
                calc_powers();
	}

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;


	if (p_ptr->update & (PU_UN_LITE))
	{
		p_ptr->update &= ~(PU_UN_LITE);
		forget_lite();
	}

	if (p_ptr->update & (PU_UN_VIEW))
	{
		p_ptr->update &= ~(PU_UN_VIEW);
		forget_view();
	}


#ifndef MONSTER_LITE
	if (p_ptr->update & (PU_VIEW))
	{
		p_ptr->update &= ~(PU_VIEW);
		update_view();
	}
#endif /* MONSTER_LITE */

#ifndef MONSTER_LITE
	if (p_ptr->update & (PU_LITE))
	{
		p_ptr->update &= ~(PU_LITE);
		update_lite();
	}
#endif /* MONSTER_LITE */

	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_flow();
	}


#ifndef MONSTER_LITE
	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MONSTERS))
	{
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(FALSE);
	}
#else /* MONSTER_LITE */
	/* Monster lites must be updated each time */
        update_monsters(TRUE);
	update_view();
	update_lite();
#endif /* MONSTER_LITE */
}


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;



	/* Hack -- clear the screen */
	if (p_ptr->redraw & (PR_WIPE))
	{
		p_ptr->redraw &= ~(PR_WIPE);
		msg_print(NULL);
		Term_clear();
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}


	if (p_ptr->redraw & (PR_BASIC))
	{
		p_ptr->redraw &= ~(PR_BASIC);
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS);
                p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
                p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA | PR_TANK | PR_MH);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
                prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
		prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & (PR_LEV))
	{
		p_ptr->redraw &= ~(PR_LEV);
		prt_level();
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp();
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		p_ptr->redraw &= ~(PR_STATS);
		prt_stat(A_STR);
		prt_stat(A_INT);
		prt_stat(A_WIS);
		prt_stat(A_DEX);
		prt_stat(A_CON);
		prt_stat(A_CHR);
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac();
	}

	if (p_ptr->redraw & (PR_HP))
	{
		p_ptr->redraw &= ~(PR_HP);
		prt_hp();
	}

	if (p_ptr->redraw & (PR_MANA))
	{
		p_ptr->redraw &= ~(PR_MANA);
		prt_sp();
	}

        if (p_ptr->redraw & (PR_TANK))
	{
                p_ptr->redraw &= ~(PR_TANK);
                prt_tp();
	}

        if (p_ptr->redraw & (PR_MH))
        {
                p_ptr->redraw &= ~(PR_MH);
                prt_mh();
        }

	if (p_ptr->redraw & (PR_GOLD))
	{
		p_ptr->redraw &= ~(PR_GOLD);
		prt_gold();
	}

	if (p_ptr->redraw & (PR_DEPTH))
	{
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_depth();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_CUT | PR_STUN);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_BLIND | PR_CONFUSED);
		p_ptr->redraw &= ~(PR_AFRAID | PR_POISONED);
                p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY | PR_SANITY);
		prt_frame_extra();
	}

	if (p_ptr->redraw & (PR_CUT))
	{
		p_ptr->redraw &= ~(PR_CUT);
		prt_cut();
	}

	if (p_ptr->redraw & (PR_STUN))
	{
		p_ptr->redraw &= ~(PR_STUN);
		prt_stun();
	}

	if (p_ptr->redraw & (PR_HUNGER))
	{
		p_ptr->redraw &= ~(PR_HUNGER);
		prt_hunger();
	}

	if (p_ptr->redraw & (PR_BLIND))
	{
		p_ptr->redraw &= ~(PR_BLIND);
		prt_blind();
	}

	if (p_ptr->redraw & (PR_CONFUSED))
	{
		p_ptr->redraw &= ~(PR_CONFUSED);
		prt_confused();
	}

	if (p_ptr->redraw & (PR_AFRAID))
	{
		p_ptr->redraw &= ~(PR_AFRAID);
		prt_afraid();
	}

	if (p_ptr->redraw & (PR_POISONED))
	{
		p_ptr->redraw &= ~(PR_POISONED);
		prt_poisoned();
	}

	if (p_ptr->redraw & (PR_STATE))
	{
		p_ptr->redraw &= ~(PR_STATE);
		prt_state();
	}

	if (p_ptr->redraw & (PR_SPEED))
	{
		p_ptr->redraw &= ~(PR_SPEED);
		prt_speed();
	}

	if (p_ptr->redraw & (PR_STUDY))
	{
		p_ptr->redraw &= ~(PR_STUDY);
		prt_study();
	}

	if (p_ptr->redraw & (PR_SANITY)) {
                p_ptr->redraw &= ~(PR_SANITY);
                prt_sane();
	}
}


/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
	int j;
	
	u32b mask = 0L;


	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Save usable flags */
		if (angband_term[j]) mask |= window_flag[j];
	}

	/* Apply usable flags */
	p_ptr->window &= mask;

	/* Nothing to do */
	if (!p_ptr->window) return;


	/* Display inventory */
	if (p_ptr->window & (PW_INVEN))
	{
		p_ptr->window &= ~(PW_INVEN);
		fix_inven();
	}

	/* Display equipment */
	if (p_ptr->window & (PW_EQUIP))
	{
		p_ptr->window &= ~(PW_EQUIP);
		fix_equip();
	}

	/* Display spell list */
	if (p_ptr->window & (PW_SPELL))
	{
		p_ptr->window &= ~(PW_SPELL);
		fix_spell();
	}

	/* Display player */
	if (p_ptr->window & (PW_PLAYER))
	{
		p_ptr->window &= ~(PW_PLAYER);
		fix_player();
	}

	/* Display monster list */
	if (p_ptr->window & (PW_M_LIST))
	{
		p_ptr->window &= ~(PW_M_LIST);
		fix_m_list();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_MESSAGE))
	{
		p_ptr->window &= ~(PW_MESSAGE);
		fix_message();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_OVERHEAD))
	{
		p_ptr->window &= ~(PW_OVERHEAD);
		fix_overhead();
	}

	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
	}

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

	/* Window stuff */
	if (p_ptr->window) window_stuff();
}


bool monk_empty_hands(void)
{
        int i;
        object_type *o_ptr;

	if (!(p_ptr->pclass == CLASS_MONK)) return FALSE;

        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                o_ptr = &inventory[INVEN_WIELD + i];

                if (o_ptr->k_idx) return FALSE;

                i++;
        }

        return TRUE;
}

bool monk_heavy_armor(void)
{
	u16b monk_arm_wgt = 0;

	if (!(p_ptr->pclass == CLASS_MONK)) return FALSE;

	/* Weight the armor */
	monk_arm_wgt += inventory[INVEN_BODY].weight;
	monk_arm_wgt += inventory[INVEN_HEAD].weight;
	monk_arm_wgt += inventory[INVEN_ARM].weight;
	monk_arm_wgt += inventory[INVEN_OUTER].weight;
	monk_arm_wgt += inventory[INVEN_HANDS].weight;
	monk_arm_wgt += inventory[INVEN_FEET].weight;

	return (monk_arm_wgt > ( 100 + (p_ptr->lev * 4))) ;
}

bool beastmaster_whip()
{
	object_type *o_ptr;
        int i;

        if(p_ptr->pclass!=CLASS_BEASTMASTER)return FALSE;

        i = 0;
        while (p_ptr->body_parts[i] == INVEN_WIELD)
        {
                o_ptr = &inventory[INVEN_WIELD + (i++)];

                if (!o_ptr->k_idx) continue;

                if(o_ptr->tval!=TV_HAFTED)return TRUE;
                else
                switch(o_ptr->sval)
                {
                        case SV_WHIP:
                        case SV_BALL_AND_CHAIN:
                        case SV_MORNING_STAR:
                        case SV_FLAIL:
                        case SV_TWO_HANDED_FLAIL:
                                break;
                        default:
                                return TRUE;
                                break;
                }
        }
        return FALSE;
}

int get_artifact_idx(int level)
{
        int count = 0, i;
        bool OK = FALSE;

        while(count < 1000)
        {
                artifact_type *a_ptr; 

                count++;
                i = randint(max_a_idx-1);
                a_ptr = &a_info[i];

                if(!a_ptr->tval) continue;
                if(a_ptr->cur_num) continue;
                if(a_ptr->level > level) continue;

                OK = TRUE;
                break;
        }
        if(OK == FALSE) i = 1; /* The Phial */

        return i;
}

/* Chose a fate */
void gain_fate(byte fate)
{
        int i;
        int level;

        for(i = 0; i < MAX_FATES; i++)
        {
                if(!fates[i].fate)
                {
                        fates[i].level = 0;

                        cmsg_print(TERM_VIOLET, "More of your prophecy has been unearthed!");
                        cmsg_print(TERM_VIOLET, "You should see a soothsayer quickly.");

                        if(fate)
                                fates[i].fate = fate;
                        else
                                switch(rand_int(18))
                                {
                                        case 0:case 2:case 3:case 7:case 8:case 9:
                                        case 13:
                                                fates[i].fate = FATE_FIND_O;
                                                break;
                                        case 1:case 4:case 5:case 10:case 11:case 12:
                                        case 14:
                                                fates[i].fate = FATE_FIND_R;
                                                break;
                                        case 15:case 16:
                                                fates[i].fate = FATE_FIND_A;
                                                break;
                                        case 6:
                                                fates[i].fate = FATE_DIE;
                                                break;
                                        case 17:
                                        {
                                                /* The deepest the better */
                                                int chance = dun_level / 4;

                                                /* No more than 1/2 chances */
                                                if (chance > 50) chance = 50;

                                                /* It's HARD to get now */
                                                if (magik(chance))
                                                {
                                                        fates[i].fate = FATE_NO_DIE_MORTAL;
                                                }
                                                else
                                                {
                                                        fates[i].fate = FATE_FIND_O;
                                                }
                                                break;
                                        }
                                }

                        switch(fates[i].fate)
                        {
                                case FATE_FIND_O:
                                {
                                        while (TRUE)
                                        {
                                                object_kind *k_ptr;

                                                fates[i].o_idx = get_obj_num(max_dlv[dungeon_type] + randint(10));
                                                k_ptr = &k_info[fates[i].o_idx];

                                                if (!(k_ptr->flags3 & TR3_INSTA_ART) && !(k_ptr->flags3 & TR3_NORM_ART)) break;
                                        }
                                        level = rand_range(max_dlv[dungeon_type] - 20, max_dlv[dungeon_type] + 20);
                                        fates[i].level = (level < 1)?1:(level > 98)?98:level;
                                        fates[i].serious = rand_int(2);
                                        fates[i].know = FALSE;
                                        if (wizard) msg_format("New fate : Find object %d on level %d", fates[i].o_idx, fates[i].level);
                                        break;
                                }
                                case FATE_FIND_R:
                                        /* Prepare allocation table */
                                        get_mon_num_prep();

                                        fates[i].r_idx = get_mon_num(max_dlv[dungeon_type] + randint(10));
                                        level = rand_range(max_dlv[dungeon_type] - 20, max_dlv[dungeon_type] + 20);
                                        fates[i].level = (level < 1)?1:(level > 98)?98:level;
                                        fates[i].serious = rand_int(2);
                                        fates[i].know = FALSE;
                                        if (wizard) msg_format("New fate : Meet monster %d on level %d", fates[i].r_idx, fates[i].level);
                                        break;

                                case FATE_FIND_A:
                                        fates[i].a_idx = get_artifact_idx(max_dlv[dungeon_type] + randint(10));
                                        level = rand_range(max_dlv[dungeon_type] - 20, max_dlv[dungeon_type] + 20);
                                        fates[i].level = (level < 1)?1:(level > 98)?98:level;
                                        fates[i].serious = TRUE;
                                        fates[i].know = FALSE;
                                        if (wizard) msg_format("New fate : Find artifact %d on level %d", fates[i].a_idx, fates[i].level);
                                        break;

                                case FATE_DIE:
                                        level = rand_range(max_dlv[dungeon_type] - 20, max_dlv[dungeon_type] + 20);
                                        fates[i].level = (level < 1)?1:(level > 98)?98:level;
                                        fates[i].serious = TRUE;
                                        fates[i].know = FALSE;
                                        if((wizard) || (p_ptr->precognition)) msg_format("New fate : Death on level %d", fates[i].level);
                                        break;

                                case FATE_NO_DIE_MORTAL:
                                        fates[i].serious = TRUE;
                                        p_ptr->no_mortal = TRUE;
                                        if((wizard) || (p_ptr->precognition)) msg_format("New fate : Never to die by the hand of a mortal being.");
                                        break;
                        }

                        break;
                }
        }
} 

void fate_desc(char *desc, int fate)
{
	char buf[120];

	if(fates[fate].serious)
	{
		strcpy(desc, "You are fated to ");
	}
	else
	{
		strcpy(desc, "You may ");
	}
	switch(fates[fate].fate)
	{
		case FATE_FIND_O:
		{
			object_type *o_ptr, forge;
			char o_name[80];

			o_ptr = &forge;
			object_prep(o_ptr, fates[fate].o_idx);
			object_desc_store(o_name, o_ptr, 1, 0);

			sprintf(buf, "find %s on level %d.", o_name, fates[fate].level);
			strcat(desc, buf);
			break;
		}
		case FATE_FIND_A:
		{
			object_type *q_ptr, forge;
			char o_name[80];
			artifact_type *a_ptr = &a_info[fates[fate].a_idx];
			int I_kind;

			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Acquire the "kind" index */
			I_kind = lookup_kind(a_ptr->tval, a_ptr->sval);

			/* Create the artifact */
			object_prep(q_ptr, I_kind);

			/* Save the name */
			q_ptr->name1 = fates[fate].a_idx;

			/* Extract the fields */
			q_ptr->pval = a_ptr->pval;
			q_ptr->ac = a_ptr->ac;
			q_ptr->dd = a_ptr->dd;
			q_ptr->ds = a_ptr->ds;
			q_ptr->to_a = a_ptr->to_a;
			q_ptr->to_h = a_ptr->to_h;
			q_ptr->to_d = a_ptr->to_d;
			q_ptr->weight = a_ptr->weight;

			/* Hack -- acquire "cursed" flag */
			if (a_ptr->flags3 & (TR3_CURSED)) q_ptr->ident |= (IDENT_CURSED);

			random_artifact_resistance(q_ptr);

			object_desc_store(o_name, q_ptr, 1, 0);

			sprintf(buf, "find %s on level %d.", o_name, fates[fate].level);
			strcat(desc, buf);
			break;
		}
		case FATE_FIND_R:
		{
			char m_name[80];

                        monster_race_desc(m_name, fates[fate].r_idx, 0);
			sprintf(buf, "meet %s on level %d.", m_name, fates[fate].level);
			strcat(desc, buf);
			break;
		}
		case FATE_DIE:
		{
			sprintf(buf, "die on level %d.", fates[fate].level);
			strcat(desc, buf);
			break;
		}
		case FATE_NO_DIE_MORTAL:
		{
			strcat(desc, "never to die by the hand of a mortal being.");
			break;
		}
	}
}

void dump_fates(FILE *OutFile)
{
	int i;
	char buf[120];

	if (!OutFile) return;

	for (i = 0; i < MAX_FATES; i++)
	{
		if((fates[i].fate) && (fates[i].know))
		{
			fate_desc(buf, i);
			fprintf(OutFile, "%s\n", buf);
		}
	}
}
