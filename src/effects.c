/* File: effects.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
	/* No healing */
	if (!num) return (FALSE);

	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		int actual;

		/* Enforce Maximum */
		if ((p_ptr->mhp - p_ptr->chp) < num) actual = p_ptr->mhp - p_ptr->chp;
		else actual = num;

		/* Gain hitpoints */
		p_ptr->chp += actual;
		p_ptr->chp_frac = 0;

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		message_format(MSG_EFFECT, 0, "You regain %d hp.", actual);

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
	return (FALSE);
}

/*
 * Heal a player a percentage of his wounds.
 */
bool heal_player(int perc, int min)
{
	int i;

	/* Taint */
	if (p_ptr->taint)
	{
		min = (3 * min) / 4;
		perc = (4 * perc) / 5;
	}

	/* No healing */
	if ((perc <= 0) && (min <= 0)) return (FALSE);

	/* No healing needed */
	if (p_ptr->chp >= p_ptr->mhp) return (FALSE);

	/* Figure healing level */
	i = ((p_ptr->mhp - p_ptr->chp) * perc) / 100;

	/* Enforce minimums */
	if (i < min) i = min;

	/* Actual healing */
	return hp_player(i);
}

/*
 * Decreases players hit points and sets death flag if necessary
 */
void damage_player(int dam, cptr kb_str)
{
	int old_chp = p_ptr->chp;
	int warning = (p_ptr->mhp * op_ptr->hitpoint_warn / 10);

	/* Paranoia */
	if (p_ptr->is_dead) return;

	/* Disturb */
    disturb(1);

	/* Mega-Hack -- Apply "resilence"*/
	if (p_ptr->resilient) dam /= 3;
	
	/* Hurt the player */
	p_ptr->chp -= dam;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Wounded or dead player */
	if (p_ptr->chp < 0)
	{
		int easy_wound;
		int chance = rand_int(3);

		/* At shallow depths, wounds cause none or less permanent ability damage */
		if (p_ptr->depth <= 24) easy_wound = 3; /* Both stats secure */
		else if (p_ptr->depth <= 36) easy_wound = randint(2); /* One of the stats secure */
		else easy_wound = 0;

		if (chance == 0)
		{
			/* Wound against Vigor */
			if (p_ptr->depth)
			{
				bell ("You suffer a nasty internal wound!");
				message(MSG_HITPOINT_WARN, TRUE, "*** YOU SUFFER A NASTY INTERNAL WOUND! ***");
			}
			else
			{
				bell ("You get an ugly scar to your chest!");
				message(MSG_HITPOINT_WARN, TRUE, "*** YOU GET AN UGLY SCAR TO YOUR CHEST! ***");
			}

			if (p_ptr->wound_vigor == 0)
			{				
				if (((p_ptr->sustain_str) && (p_ptr->sustain_con)) || (easy_wound == 3))
				{
					p_ptr->wound_vigor = 1;
				}

				else if ((p_ptr->sustain_con) || (easy_wound == 1))
				{
					p_ptr->wound_vigor = 2;
					message(MSG_EFFECT, 0, "Your strength was permanently damaged.");
				}

				else if ((p_ptr->sustain_str) || (easy_wound == 2))
				{
					p_ptr->wound_vigor = 3;
					message(MSG_EFFECT, 0, "Your constitution was permanently damaged.");
				}
								
				else
				{
					p_ptr->wound_vigor = 4;
					message(MSG_EFFECT, 0, "Your strength and constitution were permanently damaged.");
				}
				
				if (p_ptr->depth >= 13)
				{
					(void)do_dec_stat(A_STR, randint(4) + 1, FALSE, FALSE);
					(void)do_dec_stat(A_CON, randint(4) + 1, FALSE, FALSE);
				}
				else if (p_ptr->depth)
				{
					if (rand_int(100) < 50)
					{
						(void)do_dec_stat(A_STR, randint(4) + 1, FALSE, FALSE);
					}
					else
					{	
						(void)do_dec_stat(A_CON, randint(4) + 1, FALSE, FALSE);
					}
				}
				
				p_ptr->chp = ((p_ptr->mhp)/2);
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_slow(0);
				(void)set_paralyzed(0);

				return;
			}
		}
		
		else if (chance == 1)
		{
			/* Wound against Wit */
			if (p_ptr->depth)
			{
				bell ("Your brain is damaged!");
				message(MSG_HITPOINT_WARN, TRUE, "*** YOUR BRAIN IS DAMAGED! ***");
			}
			else
			{
				bell ("You get an ugly scar to your head!");
				message(MSG_HITPOINT_WARN, TRUE, "*** YOU GET AN UGLY SCAR TO YOUR HEAD! ***");
			}

			if (p_ptr->wound_wit == 0)
			{
				if (((p_ptr->sustain_int) && (p_ptr->sustain_wis)) || (easy_wound == 3))
				{
					p_ptr->wound_wit = 1;
				}
								
				else if ((p_ptr->sustain_wis) || (easy_wound == 1))
				{
					p_ptr->wound_wit = 2;
					message(MSG_EFFECT, 0, "Your memory was permanently damaged.");
				}
				
				else if ((p_ptr->sustain_int) || (easy_wound == 2))
				{
					p_ptr->wound_wit = 3;
					message(MSG_EFFECT, 0, "Your wisdom was permanently damaged.");
				}

				else
				{
					p_ptr->wound_wit = 4;
					message(MSG_EFFECT, 0, "Your memory and wisdom were permanently damaged.");
				}

				if (p_ptr->depth >= 13)
				{
					(void)do_dec_stat(A_INT, randint(4) + 1, FALSE, TRUE);
					(void)do_dec_stat(A_WIS, randint(4) + 1, FALSE, TRUE);
				}
				else if (p_ptr->depth)
				{
					if (rand_int(100) < 50)
					{
						(void)do_dec_stat(A_INT, randint(4) + 1, FALSE, TRUE);
					}
					else
					{	
						(void)do_dec_stat(A_WIS, randint(4) + 1, FALSE, TRUE);
					}
				}

				p_ptr->chp = ((p_ptr->mhp)/2);
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_slow(0);
				(void)set_paralyzed(0);

				return;
			}
		}
		
		else
		{
			/* Wound against Grace */
			if (p_ptr->depth)
			{
				bell ("A broken back renders you graceless!");
				message(MSG_HITPOINT_WARN, TRUE, "*** A BROKEN BACK RENDERS YOU GRACELESS! ***");
			}
			else
			{
				bell ("You get an ugly scar to your back!");
				message(MSG_HITPOINT_WARN, TRUE, "*** YOU GET AN UGLY SCAR TO YOUR BACK! ***");
			}

			if (p_ptr->wound_grace == 0)
			{				
				if (((p_ptr->sustain_dex) && (p_ptr->sustain_chr)) || (easy_wound == 3))
				{
					p_ptr->wound_grace = 1;
				}
				
				else if ((p_ptr->sustain_chr) || (easy_wound == 1))
				{
					p_ptr->wound_grace = 2;
					message(MSG_EFFECT, 0, "Your dexterity was permanently damaged.");
				}

				else if ((p_ptr->sustain_dex) || (easy_wound == 2))
				{
					p_ptr->wound_grace = 3;
					message(MSG_EFFECT, 0, "Your presence was permanently damaged.");
				}
								
				else
				{
					p_ptr->wound_grace = 4;
					message(MSG_EFFECT, 0, "Your dexterity and presence were permanently damaged.");
				}
				
				if (p_ptr->depth >= 13)
				{
					(void)do_dec_stat(A_DEX, randint(4) + 1, FALSE, FALSE);
					(void)do_dec_stat(A_CHR, randint(4) + 1, FALSE, FALSE);
				}
				else if (p_ptr->depth)
				{
					if (rand_int(100) < 50)
					{
						(void)do_dec_stat(A_DEX, randint(4) + 1, FALSE, FALSE);
					}
					else
					{	
						(void)do_dec_stat(A_CHR, randint(4) + 1, FALSE, FALSE);
					}
				}

				p_ptr->chp = ((p_ptr->mhp)/2);
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_slow(0);
				(void)set_paralyzed(0);

				return;
			}
		}

		/* Hack -- Note death */
		message(MSG_DEATH, TRUE, "You die.");
		message_flush();

		/* Note cause of death */
		my_strcpy(p_ptr->died_from, kb_str, sizeof(p_ptr->died_from));

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Dead */
		return;
	}

	/* Hitpoint warning  */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning)
		{
			bell("Low hitpoint warning!");
		}

		/* Message */
		message(MSG_HITPOINT_WARN, TRUE, "*** LOW HITPOINT WARNING! ***");

		message_flush();
	}

	/* Teleport to Circle of Recall? */
	if (p_ptr->chp < p_ptr->mhp / 5)
	{
		if ((p_ptr->recall_y > 0) && (!(t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_RECALL)))
		{
			/* Sound */
			sound(MSG_TELEPORT);

			/* Message */
			message(MSG_EFFECT, 0, "The Circle of Recall summons you back.");

			/* Move player */
			teleport_player_to(p_ptr->recall_y + rand_int(2), p_ptr->recall_x + rand_int(2));

			/* Handle stuff XXX XXX XXX */
			handle_stuff();
		}
	}
}

/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
	/* Gain some experience */
	p_ptr->exp += amount;

	/* Slowly recover from experience drainage */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Gain max experience (10%) */
		p_ptr->max_exp += amount / 10;
	}

	/* Check Experience */
	check_experience();

	/* Update condition window */
	p_ptr->window |= (PW_CONDITION);
}

/*
 * Lose experience
 */
void lose_exp(s32b amount)
{
	/* Never drop below zero experience */
	if (amount > p_ptr->exp) amount = p_ptr->exp;

	/* Lose some experience */
	p_ptr->exp -= amount;

	/* Check Experience */
	check_experience();

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);
}

/*
 * Restores any drained experience
 */
bool restore_exp(void)
{
	/* Restore experience */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Message */
		message(MSG_EFFECT, 0, "You feel your life energies returning.");

		/* Restore the experience */
		p_ptr->exp = p_ptr->max_exp;

		/* Check the experience */
		check_experience();

		/* Window stuff */
		p_ptr->window |= (PW_CONDITION);

		/* Did something */
		return (TRUE);
	}

	/* No effect */
	return (FALSE);
}

/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_pos[] =
{
	"strong",
	"smart",
	"wise",
	"dextrous",
	"healthy",
	"charismatic"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_neg[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"unconfident"
};

/*
 * Mix stats
 */
void scramble_stats(void)
{
	int ii, jj;
	byte max1, cur1, max2, cur2;

	/* Pick a pair of stats */
	ii = rand_int(A_MAX);
	for (jj = ii; jj == ii; jj = rand_int(A_MAX)) /* loop */;

	/* Get values */
	max1 = p_ptr->stat_max[ii];
	cur1 = p_ptr->stat_cur[ii];
	max2 = p_ptr->stat_max[jj];
	cur2 = p_ptr->stat_cur[jj];

	/* Swap values */
	p_ptr->stat_max[ii] = max2;
	p_ptr->stat_cur[ii] = cur2;
	p_ptr->stat_max[jj] = max1;
	p_ptr->stat_cur[jj] = cur1;

	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);
}

/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat)
{
	int gain;
	byte value;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above 18/100 */
	if (value < A_CAP)
	{
		/* Gain one (sometimes two) points */
		if (value < 15)
		{
			gain = ((rand_int(100) < 80) ? 1 : 2);
			value += gain;
		}

		/* Gain one (rarely two) points */
		else if (value < 18)
		{
			gain = ((rand_int(100) < 97) ? 1 : 2);
			value += gain;
		}

		/* Gain one point at a time */
		else
		{
			value++;
		}

		/* Save the new value */
		p_ptr->stat_cur[stat] = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat_max[stat])
		{
			p_ptr->stat_max[stat] = value;
		}

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_CONDITION);

		/* Success */
		return (TRUE);
	}

	/* Nothing to gain */
	return (FALSE);
}

/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, bool permanent)
{
	byte cur, max;
	int same, res = FALSE;

	/* Get the current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
	if (cur > 0)
	{
		/* Prevent illegal values */
		if (cur > amount) cur -= amount;
		else cur = 0;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 0))
	{
		/* Prevent illegal values */
		if (max > amount) max -= amount;
		else max = 0;

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_CONDITION);
	}

	/* Done */
	return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_CONDITION);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}


/*
 * Lose a "point"
 */
bool do_dec_stat(int stat, int amount, bool permanent, bool can_sustain)
{
	if (adult_nightmare_mode && !permanent && (rand_int(3) == 0)) permanent = TRUE;

	/* Get the "sustain" */
	if (can_sustain)
	{
		bool sust = FALSE;

		switch (stat)
		{
			case A_STR: if (p_ptr->sustain_str) sust = TRUE; break;
			case A_INT: if (p_ptr->sustain_int) sust = TRUE; break;
			case A_WIS: if (p_ptr->sustain_wis) sust = TRUE; break;
			case A_DEX: if (p_ptr->sustain_dex) sust = TRUE; break;
			case A_CON: if (p_ptr->sustain_con) sust = TRUE; break;
			case A_CHR: if (p_ptr->sustain_chr) sust = TRUE; break;
		}

		/* Sustain */
		if (sust)
		{
			/* Message */
			message_format(MSG_EFFECT, 0, "You feel very %s for a moment, but the feeling passes.",
				       desc_stat_neg[stat]);

			/* Notice effect */
			return (TRUE);
		}
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, amount, permanent))
	{
		/* Message */
		message_format(MSG_EFFECT, 0, "You feel very %s.", desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Restore lost "points" in a stat
 */
bool do_res_stat(int stat)
{
	/* Attempt to increase */
	if (res_stat(stat))
	{
		/* Message */
		message_format(MSG_EFFECT, 0, "You no longer feel as %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Gain a "point" in a stat
 */
bool do_inc_stat(int stat)
{
	bool res;

	/* Restore strength */
	res = res_stat(stat);

	/* Attempt to increase */
	if (inc_stat(stat))
	{
		/* Message */
		message_format(MSG_EFFECT, 0, "You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
		message_format(MSG_EFFECT, 0, "You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}

/*
 * Set "p_ptr->blind", notice observable changes
 *
 * Note the use of "PU_FORGET_VIEW" and "PU_UPDATE_VIEW", which are needed
 * because "p_ptr->blind" affects the "CAVE_SEEN" flag, and "PU_MONSTERS",
 * because "p_ptr->blind" affects monster visibility, and "PU_MAP", because
 * "p_ptr->blind" affects the way in which many cave grids are displayed.
 */
bool set_blind(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->blind)
		{
			message(MSG_EFFECT, 0, "You are blind!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
			message(MSG_EFFECT, 0, "You can see again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blind = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Redraw the "blind" */
	p_ptr->redraw |= (PR_BLIND);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->confused", notice observable changes
 */
bool set_confused(int v)
{
	int old_aux, new_aux;
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Insane */
	if (p_ptr->confused > PY_CONF_INSANE)
	{
		old_aux = 4;
	}

	/* Terrified */
	else if (p_ptr->confused > PY_CONF_BEFUDDLE)
	{
		old_aux = 3;
	}
	
	/* Afraid */
	else if (p_ptr->confused > PY_CONF_CONFUSE)
	{
		old_aux = 2;
	}

	/* Wary */
	else if (p_ptr->confused > 0)
	{
		old_aux = 1;
	}

	else
	{
		old_aux = 0;
	}

	/* Insane */
	if (v > PY_CONF_INSANE)
	{
		new_aux = 4;
	}

	/* Terrified */
	else if (v > PY_CONF_BEFUDDLE)
	{
		new_aux = 3;
	}

	/* Afraid */
	else if (v > PY_CONF_CONFUSE)
	{
		new_aux = 2;
	}

	/* Wary */
	else if (v > 0)
	{
		new_aux = 1;
	}

	else
	{
		new_aux = 0;
	}

	/* Increase confusion */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Perplexed */
			case 1:
			{
				message(MSG_EFFECT, 0, "You feel somewhat perplexed.");
				break;
			}

			/* Confused */
			case 2:
			{
				message(MSG_EFFECT, 0, "You are confused.");
				break;
			}

			/* Befuddled */
			case 3:
			{
				message(MSG_EFFECT, 0, "You feel totally befuddled!");
				break;
			}

			/* Insane */
			case 4:
			{
				message(MSG_EFFECT, 0, "You have lost your grip on sanity!");
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
			{
				message(MSG_EFFECT, 0, "You feel less confused now.");
				if (disturb_state) disturb(0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->confused = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Redraw the "confused" */
	p_ptr->redraw |= (PR_CONFUSED);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->poisoned", notice observable changes
 */
bool set_poisoned(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->poisoned)
		{
			message(MSG_EFFECT, 0, "You are poisoned!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->poisoned)
		{
			message(MSG_EFFECT, 0, "You are no longer poisoned.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->poisoned = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Redraw the "poisoned" */
	p_ptr->redraw |= (PR_POISONED);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->diseased", notice observable changes
 */
bool set_diseased(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->diseased)
		{
			message(MSG_EFFECT, 0, "You have contracted a disease!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->diseased)
		{
			message(MSG_EFFECT, 0, "You are no longer suffering from a disease.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->diseased = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Redraw the "diseased" */
	p_ptr->redraw |= (PR_DISEASED);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->taint", notice observable changes
 */
bool set_taint(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->taint)
		{
			/* Hack - no message for demons */
			if (rp_ptr->special != RACE_SPECIAL_DEMON) 
				message(MSG_EFFECT, 0, "You feel a dark taint descending upon your soul!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->taint)
		{
			message(MSG_EFFECT, 0, "The taint on your soul lightens.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->taint = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Update bonuses because Taint shuts down deity bonuses */
	p_ptr->update |= (PU_BONUS | PR_MANA);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->afraid", notice observable changes
 */
bool set_afraid(int v)
{
	int old_aux, new_aux;
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Panic */
	if (p_ptr->afraid > PY_FEAR_PANIC)
	{
		old_aux = 4;
	}

	/* Terrified */
	else if (p_ptr->afraid > PY_FEAR_TERROR)
	{
		old_aux = 3;
	}
	
	/* Afraid */
	else if (p_ptr->afraid > PY_FEAR_AFRAID)
	{
		old_aux = 2;
	}

	/* Wary */
	else if (p_ptr->afraid > 0)
	{
		old_aux = 1;
	}

	else
	{
		old_aux = 0;
	}

	/* PANIC */
	if (v > PY_FEAR_PANIC)
	{
		new_aux = 4;
	}

	/* Terrified */
	else if (v > PY_FEAR_TERROR)
	{
		new_aux = 3;
	}

	/* Afraid */
	else if (v > PY_FEAR_AFRAID)
	{
		new_aux = 2;
	}

	/* Wary */
	else if (v > 0)
	{
		new_aux = 1;
	}

	else
	{
		new_aux = 0;
	}

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Wary */
			case 1:
			{
				message(MSG_EFFECT, 0, "You feel wary.");
				break;
			}

			/* Afraid */
			case 2:
			{
				message(MSG_EFFECT, 0, "You are afraid.");
				break;
			}

			/* Terror */
			case 3:
			{
				message(MSG_EFFECT, 0, "You are terrified.");
				break;
			}

			/* Terror */
			case 4:
			{
				message(MSG_EFFECT, 0, "You are in total panic!");
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
			{
				message(MSG_EFFECT, 0, "You feel bolder now.");
				if (disturb_state) disturb(0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->afraid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the "afraid" */
	p_ptr->redraw |= (PR_AFRAID);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->paralyzed", notice observable changes
 */
bool set_paralyzed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->paralyzed)
		{
			message(MSG_EFFECT, 0, "You are paralyzed!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->paralyzed)
		{
			message(MSG_EFFECT, 0, "You can move again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->paralyzed = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note the use of "PR_MAP", which is needed because "p_ptr->image" affects
 * the way in which monsters, objects, and some normal grids, are displayed.
 */
bool set_image(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->image)
		{
			message(MSG_EFFECT, 0, "You feel drugged!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
			message(MSG_EFFECT, 0, "You can see clearly again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->image = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_VISIBLE);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->fast", notice observable changes
 */
bool set_fast(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->fast)
		{
			message(MSG_EFFECT, 0, "You feel yourself moving faster!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->fast_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
			message(MSG_EFFECT, 0, "You feel yourself slow down.");
			notice = TRUE;
		}

		p_ptr->fast_perm = 0;
	}

	/* Use the value */
	p_ptr->fast = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->slow", notice observable changes
 */
bool set_slow(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->slow)
		{
			message(MSG_EFFECT, 0, "You feel yourself moving slower!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slow)
		{
			message(MSG_EFFECT, 0, "You feel yourself speed up.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slow = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->shield", notice observable changes
 */
bool set_shield(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->shield)
		{
			message(MSG_EFFECT, 0, "You feel your skin harden!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->shield_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			message(MSG_EFFECT, 0, "Your skin loses its toughness.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shield = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->blessed", notice observable changes
 */
bool set_blessed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->blessed)
		{
			if (p_ptr->taint)
			{
				message(MSG_FAIL, 0, "The taint on your soul outweighs the blessing.");
				return (TRUE);
			}
			else 
			{
				message(MSG_EFFECT, 0, "You feel righteous!");
				notice = TRUE;

				/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
				if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->blessed_perm = 1;
			}
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blessed)
		{
			message(MSG_EFFECT, 0, "The prayer has expired.");
			notice = TRUE;
		}

		p_ptr->blessed_perm = 0;
	}

	/* Use the value */
	p_ptr->blessed = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->hero", notice observable changes
 */
bool set_hero(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->hero)
		{
			message(MSG_EFFECT, 0, "You feel heroic!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->hero_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->hero)
		{
			message(MSG_EFFECT, 0, "The heroism wears off.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->hero = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->rage", notice observable changes
 */
bool set_rage(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->rage)
		{
			message(MSG_EFFECT, 0, "You feel like a killing machine!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->rage_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->rage)
		{
			message(MSG_EFFECT, 0, "You feel less Berserk.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->rage = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->protevil", notice observable changes
 */
bool set_protevil(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->protevil)
		{
			if (p_ptr->taint)
			{
				message(MSG_FAIL, 0, "You must first defeat the evil within!");
				return (TRUE);
			}
			else 
			{
				message(MSG_EFFECT, 0, "You feel safe from evil!");
				notice = TRUE;
			}

			/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->protevil_perm = 1;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
			message(MSG_EFFECT, 0, "You no longer feel safe from evil.");
			notice = TRUE;
		}

		p_ptr->protevil_perm = 0;
	}

	/* Use the value */
	p_ptr->protevil = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->protchaos", notice observable changes
 */
bool set_protchaos(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->protchaos)
		{
			message(MSG_EFFECT, 0, "You feel safe from the forces of Chaos!");
			notice = TRUE;

			/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->protchaos_perm = 1;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protchaos)
		{
			message(MSG_EFFECT, 0, "You no longer feel safe from Chaos.");
			notice = TRUE;
		}

		p_ptr->protchaos_perm = 0;
	}

	/* Use the value */
	p_ptr->protchaos = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->flaming_hands", notice observable changes
 */
bool set_flaming_hands(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->flaming_hands)
		{
			message(MSG_EFFECT, 0, "Your hands are on fire!");
			notice = TRUE;

			/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->flaming_hands_perm = 1;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->flaming_hands)
		{
			message(MSG_EFFECT, 0, "Your hands are no longer on fire.");
			notice = TRUE;
		}

		p_ptr->flaming_hands_perm = 0;
	}

	/* Use the value */
	p_ptr->flaming_hands = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->icy_hands", notice observable changes
 */
bool set_icy_hands(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->icy_hands)
		{
			message(MSG_EFFECT, 0, "Your fingertips are freezing!");
			notice = TRUE;

			/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
			if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->icy_hands_perm = 1;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->icy_hands)
		{
			message(MSG_EFFECT, 0, "Your fingers feel warmer.");
			notice = TRUE;
		}

		p_ptr->icy_hands_perm = 0;
	}

	/* Use the value */
	p_ptr->icy_hands = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->resilent", notice observable changes
 */
bool set_resilient(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->resilient)
		{
			message(MSG_EFFECT, 0, "You feel resilient!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->resilient_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->resilient)
		{
			message(MSG_EFFECT, 0, "You feel vulnerable once more.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->resilient = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->absorb", notice observable changes
 */
bool set_absorb(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values, plus a very low cap */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->absorb)
		{
			message(MSG_EFFECT, 0, "An aura of magical light emanates from your body!");
			notice = TRUE;
		}
		else if (v > 400)
		{
			message(MSG_EFFECT, 0, "Your aura of magical light reaches full intensity!");
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->absorb_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->absorb)
		{
			message(MSG_EFFECT, 0, "Your aura has abated.");
			notice = TRUE;
		}

		p_ptr->absorb_perm = 0;
	}

	/* Use the value. HACK - Note that it caps at 490. */
	p_ptr->absorb = (v > 400) ? 400 : v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->safety", notice observable changes
 */
bool set_safety(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->safety)
		{
			message(MSG_EFFECT, 0, "You feel secure from traps!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->safety_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->safety)
		{
			message(MSG_EFFECT, 0, "You no longer feel secure from traps.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->safety = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_see_invis", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->tim_see_invis" affects monster visibility.
 */
bool set_tim_see_invis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_see_invis)
		{
			message(MSG_EFFECT, 0, "Your eyes feel very sensitive!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_see_invis_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_see_invis)
		{
			message(MSG_EFFECT, 0, "Your eyes feel less sensitive.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_see_invis = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_invis", notice observable changes
 * (Temporary invisibility)
 */
bool set_tim_invis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_invis <= 0)
		{
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_invis_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
			notice = TRUE;
		}

		p_ptr->tim_invis_perm = 0;
	}

	/* Use the value */
	p_ptr->tim_invis = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* 
	 * Note - no need to flag PW_CONDITION because it's updated
	 * when p_ptr->invis changes 
	 */

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->stability", notice observable changes
 */
bool set_stability(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->stability <= 0)
		{
			message(MSG_EFFECT, 0, "You feel a strong sense of stability");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->stability_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->stability)
		{
			message(MSG_EFFECT, 0, "Your sense of stability subsides");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->stability = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_sp_dur", notice observable changes
 */
bool set_tim_sp_dur(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sp_dur <= 0)
		{
			message(MSG_EFFECT, 0, "You have gained understanding of the nature of magic!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->sp_dur_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sp_dur)
		{
			message(MSG_EFFECT, 0, "Your feel your enhanced understanding of magic fade away...");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sp_dur = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_sp_dam", notice observable changes
 */
bool set_tim_sp_dam(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sp_dam <= 0)
		{
			message(MSG_EFFECT, 0, "You have gained understanding of the nature of magic!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_sp_dam_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sp_dam)
		{
			message(MSG_EFFECT, 0, "Your feel your enhanced understanding of magic fade away...");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sp_dam = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_sp_dam", notice observable changes
 */
bool set_tim_sp_inf(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_sp_inf <= 0)
		{
			message(MSG_EFFECT, 0, "You have gained understanding of the nature of magic!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_sp_inf_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sp_inf)
		{
			message(MSG_EFFECT, 0, "Your feel your enhanced understanding of magic fade away...");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sp_inf = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_bravery", notice observable changes
 */
bool set_tim_bravery(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values. Note that it can be negative */
	v = (v > 10000) ? 10000 : v;

	/* Open */
	if (v)
	{
		if (p_ptr->tim_bravery <= 0)
		{
			message(MSG_EFFECT, 0, "You forget the meaning of fear!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_bravery_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_bravery)
		{
			message(MSG_EFFECT, 0, "You no longer feel as fearless.");
			notice = TRUE;
		}

		p_ptr->tim_bravery_perm = 0;
	}

	/* Use the value */
	p_ptr->tim_bravery = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Nullify invisibility 
 */
void nullify_invis(void)
{
	/* If temporary invis is on, reduce it considerably */
	if (p_ptr->tim_invis > 0) set_tim_invis(p_ptr->tim_invis - (5 * INVIS_DELAY));
	/* If permanent invis is on, nullify it for a short time */
	else set_tim_invis(0 - INVIS_DELAY);

	/* Check bounds */
	if (p_ptr->tim_invis < (0 - INVIS_DELAY)) set_tim_invis(0 - INVIS_DELAY);
}

/*
 * Set "p_ptr->tim_infra", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because because
 * "p_ptr->tim_infra" affects monster visibility.
 */
bool set_tim_infra(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_infra)
		{
			message(MSG_EFFECT, 0, "Your eyes begin to tingle!");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_infra_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_infra)
		{
			message(MSG_EFFECT, 0, "Your eyes stop tingling.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_infra = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_stealth", notice observable changes
 */
bool set_tim_stealth(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_stealth)
		{
			message(MSG_EFFECT, 0, "Your movements grow more silent.");
			notice = TRUE;
		}

		/* If standing on Circle of Permanence, make the effect permanent until leaving the level */
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_PERMANENCE) p_ptr->tim_stealth_perm = 1;
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_stealth)
		{
			message(MSG_EFFECT, 0, "Your movements grow noisier.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_stealth = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_res[type]", notice observable changes
 */
bool set_tim_res(int type, int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_res[type])
		{
			/* Only notice if it makes some difference */
			if (p_ptr->dis_res[type] < resist_caps[type].temp)
			{
				if (p_ptr->dis_res[type])
					message_format(MSG_EFFECT, 0, 
					"You feel more resistant to %s!", resist_names[type]);
				else
					message_format(MSG_EFFECT, 0, 
					"You feel resistant to %s!", resist_names[type]);

				notice = TRUE;
			}
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_res[type])
		{
			/* Only notice if it makes some difference */
			if (p_ptr->dis_res[type] <= resist_caps[type].temp)
			{
				if (p_ptr->dis_res[type] <= TEMP_RES_BONUS)
					message_format(MSG_EFFECT, 0, 
					"You no longer feel resistant to %s.", resist_names[type]);
				else
					message_format(MSG_EFFECT, 0, 
					"You feel less resistant to %s.", resist_names[type]);
				
				notice = TRUE;
			}
		}
	}

	/* Use the value */
	p_ptr->tim_res[type] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_stun(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Knocked out */
	if (p_ptr->stun > PY_STUN_KO)
	{
		old_aux = 3;
	}

	/* Heavy stun */
	else if (p_ptr->stun > PY_STUN_HEAVY)
	{
		old_aux = 2;
	}

	/* Stun */
	else if (p_ptr->stun > 0)
	{
		old_aux = 1;
	}

	/* None */
	else
	{
		old_aux = 0;
	}

	/* Knocked out */
	if (v > PY_STUN_KO)
	{
		new_aux = 3;
	}

	/* Heavy stun */
	else if (v > PY_STUN_HEAVY)
	{
		new_aux = 2;
	}

	/* Stun */
	else if (v > 0)
	{
		new_aux = 1;
	}

	/* None */
	else
	{
		new_aux = 0;
	}

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Stun */
			case 1:
			{
				message(MSG_EFFECT, 0, "You have been stunned.");
				break;
			}

			/* Heavy stun */
			case 2:
			{
				message(MSG_EFFECT, 0, "You have been heavily stunned.");
				break;
			}

			/* Knocked out */
			case 3:
			{
				message(MSG_EFFECT, 0, "You have been knocked out.");
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
			{
				message(MSG_EFFECT, 0, "You are no longer stunned.");
				if (disturb_state) disturb(0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->stun = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the "stun" */
	p_ptr->redraw |= (PR_STUN);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
bool set_cut(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Mortal wound */
	if (p_ptr->cut > PY_CUT_MORTAL)
	{
		old_aux = 7;
	}

	/* Deep gash */
	else if (p_ptr->cut > PY_CUT_DEEP)
	{
		old_aux = 6;
	}

	/* Severe cut */
	else if (p_ptr->cut > PY_CUT_SEVERE)
	{
		old_aux = 5;
	}

	/* Nasty cut */
	else if (p_ptr->cut > PY_CUT_NASTY)
	{
		old_aux = 4;
	}

	/* Bad cut */
	else if (p_ptr->cut > PY_CUT_BAD)
	{
		old_aux = 3;
	}

	/* Light cut */
	else if (p_ptr->cut > PY_CUT_LIGHT)
	{
		old_aux = 2;
	}

	/* Graze */
	else if (p_ptr->cut > 0)
	{
		old_aux = 1;
	}

	/* None */
	else
	{
		old_aux = 0;
	}

	/* Mortal wound */
	if (v > PY_CUT_MORTAL)
	{
		new_aux = 7;
	}

	/* Deep gash */
	else if (v > PY_CUT_DEEP)
	{
		new_aux = 6;
	}

	/* Severe cut */
	else if (v > PY_CUT_SEVERE)
	{
		new_aux = 5;
	}

	/* Nasty cut */
	else if (v > PY_CUT_NASTY)
	{
		new_aux = 4;
	}

	/* Bad cut */
	else if (v > PY_CUT_BAD)
	{
		new_aux = 3;
	}

	/* Light cut */
	else if (v > PY_CUT_LIGHT)
	{
		new_aux = 2;
	}

	/* Graze */
	else if (v > 0)
	{
		new_aux = 1;
	}

	/* None */
	else
	{
		new_aux = 0;
	}

	/* Increase cut */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Graze */
			case 1:
			{
				message(MSG_EFFECT, 0, "You have been given a graze.");
				break;
			}

			/* Light cut */
			case 2:
			{
				message(MSG_EFFECT, 0, "You have been given a light cut.");
				break;
			}

			/* Bad cut */
			case 3:
			{
				message(MSG_EFFECT, 0, "You have been given a bad cut.");
				break;
			}

			/* Nasty cut */
			case 4:
			{
				message(MSG_EFFECT, 0, "You have been given a nasty cut.");
				break;
			}

			/* Severe cut */
			case 5:
			{
				message(MSG_EFFECT, 0, "You have been given a severe cut.");
				break;
			}

			/* Deep gash */
			case 6:
			{
				message(MSG_EFFECT, 0, "You have been given a deep gash.");
				break;
			}

			/* Mortal wound */
			case 7:
			{
				message(MSG_EFFECT, 0, "You have been given a mortal wound.");
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
			{
				message(MSG_EFFECT, 0, "You are no longer bleeding.");
				if (disturb_state) disturb(0);
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->cut = v;

	/* No change */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the "cut" */
	p_ptr->redraw |= (PR_CUT);

	/* Window stuff */
	p_ptr->window |= (PW_CONDITION);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

#define COL_WIDTH	22

/*
 * Prints some status information on the screen.
 */
void display_player_status(void)
{
	int h, w;
	int col = 0;
	int row = 1;
	int i = 0;
	int j;
	cptr conds[25];

	(void)Term_get_size(&w, &h);

	/* Erase screen */
	clear_from(0);

	/* Title */
	put_str("Current Effects:", 0, 0);

	/* Good effects */
	if (p_ptr->shield)		conds[i++] = "Shield";
	if (p_ptr->blessed)		conds[i++] = "Blessed";
	if (p_ptr->tim_bravery)	conds[i++] = "Boldness";
	if (p_ptr->hero)		conds[i++] = "Heroism";
	if (p_ptr->stability)	conds[i++] = "Stability";
	if (p_ptr->rage)		conds[i++] = "Berserk";
	if (p_ptr->protevil)	conds[i++] = "Prot. Evil";
	if (p_ptr->safety)		conds[i++] = "Prot. Traps";
	if (p_ptr->absorb)		conds[i++] = "Absorb Hit";
	if (p_ptr->resilient)	conds[i++] = "Resilient";
	if (p_ptr->fast)		conds[i++] = "Hasted";
	if (p_ptr->invis)		conds[i++] = "Invisible";
	if (p_ptr->tim_infra)	conds[i++] = "Infravision Bonus";
	if (p_ptr->tim_stealth)	conds[i++] = "Stealth Bonus";
	if (p_ptr->tim_see_invis) conds[i++] = "Sense Invisible";
	if (p_ptr->sp_dam) 		conds[i++] = "Spell Dam. Bonus";
	if (p_ptr->sp_dur)		conds[i++] = "Spell Dur. Bonus";
	if (p_ptr->sp_inf) 		conds[i++] = "Spell Inf. Bonus";

	for (j = 0; j < i; j++)
	{
		if (row >= h)
		{
			row = 1;
			col += COL_WIDTH;

			if (col >= (w - COL_WIDTH - 1)) break;
		}
		
		c_put_str(TERM_L_GREEN, conds[j], row++, col);
	}

	/* Resistances */
	i = 0;

	for (j = 0; j < RS_MAX; j++)
	{
		if (p_ptr->tim_res[j]) conds[i++] = resist_names[j];
	}

	for (j = 0; j < i; j++)
	{
		if (row >= h)
		{
			row = 1;
			col += COL_WIDTH;

			if (col >= (w - COL_WIDTH - 1)) break;
		}
		
		c_put_str(TERM_L_GREEN, format("Res. %^s Bonus",conds[j]), row++, col);
	}

	/* Bad effects */
	i = 0;
	
	if (p_ptr->blind)							 conds[i++] = "Blind";
	if (p_ptr->confused > PY_CONF_INSANE)		 conds[i++] = "Insane";
	else if (p_ptr->confused > PY_CONF_BEFUDDLE) conds[i++] = "Befuddled";
	else if (p_ptr->confused > PY_CONF_CONFUSE)	 conds[i++] = "Confused";
	else if (p_ptr->confused)					 conds[i++] = "Perplexed";
	if (p_ptr->poisoned)						 conds[i++] = "Poisoned";
	if (p_ptr->diseased)						 conds[i++] = "Diseased";
	if (p_ptr->afraid > PY_FEAR_PANIC)			 conds[i++] = "Panic";
	else if (p_ptr->afraid > PY_FEAR_TERROR)	 conds[i++] = "Terrified";
	else if (p_ptr->afraid > PY_FEAR_AFRAID)	 conds[i++] = "Afraid";
	else if (p_ptr->afraid)						 conds[i++] = "Wary";
	if (p_ptr->paralyzed)						 conds[i++] = "Paralyzed";
	if (p_ptr->image)							 conds[i++] = "Hallucinating";
	if (p_ptr->slow)							 conds[i++] = "Slowed";
	if (p_ptr->stun > PY_STUN_KO)				 conds[i++] = "Knocked Out";
	else if (p_ptr->stun > PY_STUN_HEAVY)		 conds[i++] = "Heavy Stun";
	else if (p_ptr->stun)						 conds[i++] = "Stunned";
	if (p_ptr->cut > PY_CUT_MORTAL)				 conds[i++] = "Mortal Wound";
	else if (p_ptr->cut > PY_CUT_DEEP)			 conds[i++] = "Deep Gash";
	else if (p_ptr->cut > PY_CUT_SEVERE)		 conds[i++] = "Severe Cut";
	else if (p_ptr->cut > PY_CUT_NASTY)			 conds[i++] = "Nasty Cut";
	else if (p_ptr->cut > PY_CUT_BAD)			 conds[i++] = "Bad Cut";
	else if (p_ptr->cut > PY_CUT_LIGHT)			 conds[i++] = "Light Cut";
	else if (p_ptr->cut)					     conds[i++] = "Graze";
	if (p_ptr->taint)							 conds[i++] = "Tainted";
	for (j = 0; j < A_MAX; j++)
	{
		if (p_ptr->stat_cur[j] < p_ptr->stat_max[j]) conds[i++] = desc_stat_neg[j];
	}
	if (p_ptr->exp < p_ptr->max_exp) conds[i++] = "Exp. Drained";

	/* Bad effects */
	for (j = 0; j < i; j++)
	{
		if (row >= h)
		{
			row = 1;
			col += COL_WIDTH;

			if (col >= (w - COL_WIDTH - 1)) break;
		}
		
		c_put_str(TERM_L_RED, format("%^s",conds[j]), row++, col);
	}

	if (!col && (row == 1)) put_str("None", row++, col);

}
