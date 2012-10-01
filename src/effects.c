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

	/* No healing */
	if (!perc) return (FALSE);

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

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		/* Hack -- Note death */
		message(MSG_DEATH, TRUE, "You die.");
		message_flush();

		/* Note cause of death */
		strcpy(p_ptr->died_from, kb_str);

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Dead */
		return;
	}

	/* Hitpoint warning */
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
	"cute"
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
	"ugly"
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
}

/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat potions will always restore the stat and
 * then increase the fully restored value.
 */
static bool inc_stat(int stat)
{
	int gain;
	byte value;

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above 18/100 */
	if (value < 20)
	{
		/* Gain one (sometimes two) points */
		if (value < 15)
		{
			gain = ((rand_int(100) < 75) ? 1 : 2);
			value += gain;
		}

		/* Gain one (rarely two) points */
		else if (value < 18)
		{
			gain = ((rand_int(100) < 95) ? 1 : 2);
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
static bool dec_stat(int stat, int amount, bool permanent)
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
	}

	/* Done */
	return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
static bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

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
		message_format(MSG_EFFECT, 0, "You feel less %s.", desc_stat_neg[stat]);

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
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
			message(MSG_EFFECT, 0, "You feel yourself slow down.");
			notice = TRUE;
		}
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
			message(MSG_EFFECT, 0, "A mystic shield forms around your body!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			message(MSG_EFFECT, 0, "Your mystic shield crumbles away.");
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
			message(MSG_EFFECT, 0, "You feel righteous!");
			notice = TRUE;
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
			message(MSG_EFFECT, 0, "You feel like a hero!");
			notice = TRUE;
		}
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
			message(MSG_EFFECT, 0, "You feel safe from evil!");
			notice = TRUE;
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
			message(MSG_EFFECT, 0, "Your arua of magical light reaches full intensity!");
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->absorb)
		{
			message(MSG_EFFECT, 0, "Your aura has abated.");
			notice = TRUE;
		}
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
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_invis = v;

	/* Nothing to notice */
	if (!notice) return(FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->stability)
		{
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
			message(MSG_EFFECT, 0, "You feel your movements grow more silent!");
			notice = TRUE;
		}
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
			message_format(MSG_EFFECT, 0, "You feel resistant to %s!", resist_names[type]);
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_res[type])
		{
			message_format(MSG_EFFECT, 0, "You feel less resistant to %s.", resist_names[type]);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_res[type] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

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

/*
 * Set "p_ptr->food", notice observable changes
 *
 * The "p_ptr->food" variable can get as large as 20000, allowing the
 * addition of the most "filling" item, Elvish Waybread, which adds
 * 7500 food units, without overflowing the 32767 maximum limit.
 *
 * Perhaps we should disturb the player with various messages,
 * especially messages about hunger status changes.  XXX XXX XXX
 *
 * Digestion of food is handled in "dungeon.c", in which, normally,
 * the player digests about 20 food units per 100 game turns, more
 * when "fast", more when "regenerating", less with "slow digestion",
 * but when the player is "gorged", he digests 100 food units per 10
 * game turns, or a full 1000 food units per 100 game turns.
 *
 * Note that the player's speed is reduced by 10 units while gorged,
 * so if the player eats a single food ration (5000 food units) when
 * full (15000 food units), he will be gorged for (5000/100)*10 = 500
 * game turns, or 500/(100/5) = 25 player turns (if nothing else is
 * affecting the player speed).
 */
bool set_food(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 20000) ? 20000 : (v < 0) ? 0 : v;

	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		old_aux = 0;
	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		old_aux = 1;
	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
		old_aux = 2;
	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		old_aux = 3;
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
		old_aux = 4;
	}

	/* Gorged */
	else
	{
		old_aux = 5;
	}

	/* Fainting / Starving */
	if (v < PY_FOOD_FAINT)
	{
		new_aux = 0;
	}

	/* Weak */
	else if (v < PY_FOOD_WEAK)
	{
		new_aux = 1;
	}

	/* Hungry */
	else if (v < PY_FOOD_ALERT)
	{
		new_aux = 2;
	}

	/* Normal */
	else if (v < PY_FOOD_FULL)
	{
		new_aux = 3;
	}

	/* Full */
	else if (v < PY_FOOD_MAX)
	{
		new_aux = 4;
	}

	/* Gorged */
	else
	{
		new_aux = 5;
	}

	/* Food increase */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Weak */
			case 1:
			{
				message(MSG_EFFECT, 0, "You are still weak.");
				break;
			}

			/* Hungry */
			case 2:
			{
				message(MSG_EFFECT, 0, "You are still hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				message(MSG_EFFECT, 0, "You are no longer hungry.");
				break;
			}

			/* Full */
			case 4:
			{
				message(MSG_EFFECT, 0, "You are full!");
				break;
			}

			/* Bloated */
			case 5:
			{
				message(MSG_EFFECT, 0, "You have gorged yourself!");
				break;
			}
		}

		/* Change */
		notice = TRUE;
	}

	/* Food decrease */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* Fainting / Starving */
			case 0:
			{
				message(MSG_EFFECT, 0, "You are getting faint from hunger!");
				break;
			}

			/* Weak */
			case 1:
			{
				message(MSG_EFFECT, 0, "You are getting weak from hunger!");
				break;
			}

			/* Hungry */
			case 2:
			{
				message(MSG_EFFECT, 0, "You are getting hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				message(MSG_EFFECT, 0, "You are no longer full.");
				break;
			}

			/* Full */
			case 4:
			{
				message(MSG_EFFECT, 0, "You are no longer gorged.");
				break;
			}
		}

		/* Change */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->food = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw hunger */
	p_ptr->redraw |= (PR_HUNGER);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}
