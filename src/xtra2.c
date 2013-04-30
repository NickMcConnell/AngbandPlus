/* File: xtra2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



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
			msg_print("You are blinded!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
			msg_print("You can see again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blind = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Redraw the "blind" */
	p_ptr->redraw |= (PR_BLIND);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

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
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->confused)
		{
			msg_print("You are confused!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->confused)
		{
			msg_print("You feel less confused now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->confused = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "confused" */
	p_ptr->redraw |= (PR_CONFUSED);

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
			msg_print("You are poisoned!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->poisoned)
		{
			msg_print("You are no longer poisoned.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->poisoned = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "poisoned" */
	p_ptr->redraw |= (PR_POISONED);

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
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->afraid)
		{
			msg_print("You are terrified!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->afraid)
		{
			msg_print("You feel bolder now.");
			notice = TRUE;
		}
	}

	/* Hack -- petrify the player if over 100 */
	if (v > 100)
	{
		p_ptr->afraid = 100;
		set_petrify(p_ptr->afraid - 100 / 10);
	}

	/* Use the value */
	else p_ptr->afraid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "afraid" */
	p_ptr->redraw |= (PR_AFRAID);

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
			msg_print("You are paralyzed!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->paralyzed) && !(p_ptr->stastis) && !(p_ptr->petrify))
		{
			msg_print("You can move again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->paralyzed = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

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
			msg_print("You feel drugged!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
			msg_print("You can see clearly again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->image = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->amnesia", notice observable changes
 */
bool set_amnesia(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->amnesia)
		{
			msg_print("Your memories temporarily fade.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->amnesia)
		{
			msg_print("You feel less forgetful now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->amnesia = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "amnesia" */
	p_ptr->redraw |= (PR_AMNESIA);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->cursed", notice observable changes
 */
bool set_cursed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->cursed)
		{
			msg_print("You feel unlucky!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->cursed)
		{
			msg_print("The curse has expired.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->cursed = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "cursed" */
	p_ptr->redraw |= (PR_CURSED);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->msleep", notice observable changes
 */
bool set_msleep(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->msleep)
		{
			msg_print("Your eyelids feel heavy.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->msleep) && !(p_ptr->psleep))
		{
			msg_print("You no longer feel sleepy.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->msleep = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->psleep", notice observable changes
 */
bool set_psleep(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Hack -- Wake player when finished sleeping */
	if (v >= PY_SLEEP_MAX) v = 0;

	/* Recover stats */
	if (p_ptr->psleep >= PY_SLEEP_RECOVER)
	{
		int i;

		for (i = 0; i < A_MAX; i++)
		{
			if (p_ptr->stat_cur[i]<p_ptr->stat_max[i])
			{
				if (p_ptr->stat_cur[i] < 18) p_ptr->stat_cur[i]++;
				else p_ptr->stat_cur[i] += 10;

				if (p_ptr->stat_cur[i] > p_ptr->stat_max[i]) p_ptr->stat_cur[i] = p_ptr->stat_max[i];

				p_ptr->redraw |= (PR_STATS);

				v = 0;

				notice = TRUE;
			}
		}

		/* Hack -- allow 'built-in' equipment to recover from disenchantment */
		change_shape(p_ptr->prace, p_ptr->lev);

		if (notice) msg_print("You recover somewhat.");
	}

	/* Open */
	if (v)
	{
		if ((p_ptr->psleep < PY_SLEEP_ASLEEP) && (v >= PY_SLEEP_ASLEEP))
		{
			msg_print("You fall asleep.");
			notice = TRUE;
		}
		else if ((p_ptr->psleep < PY_SLEEP_DROWSY) && (v >= PY_SLEEP_DROWSY))
		{
			msg_print("You feel drowsy.");
			/* notice = TRUE; */
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->psleep)
		{
			msg_print("You wake up.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->psleep = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Redraw state */
	p_ptr->redraw |= (PR_STATE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->petrify", notice observable changes
 */
bool set_petrify(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->petrify)
		{
			msg_print("You are petrified to the spot!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->petrify) && !(p_ptr->stastis) && !(p_ptr->paralyzed))
		{
			msg_print("You can move again.");
			notice = TRUE;
		}
	}

	/* Hack -- paralyze the player if over 100 */
	if (v > 100)
	{
		p_ptr->petrify = 100;
		set_paralyzed(p_ptr->petrify - 100 / 10);
	}

	/* Use the value */
	else p_ptr->petrify = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the "petrify" */
	p_ptr->redraw |= (PR_PETRIFY);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->stastis", notice observable changes
 */
bool set_stastis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* MegaHack -- alter reality if too high */
	if (v > 100)
	{
		msg_print("You are thrown into an alternate reality!");
		p_ptr->leaving = TRUE;
		notice = TRUE;
		v = 0;
	}

	/* Open */
	if (v)
	{
		if (!p_ptr->stastis)
		{
			msg_print("You are stuck in a time-loop!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->stastis) && !(p_ptr->paralyzed) && !(p_ptr->petrify))
		{
			msg_print("You can move again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->stastis = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Array of stat "descriptions"
 */
cptr desc_stat_imp[A_MAX] =
{
	"stronger",
	"smarter",
	"wiser",
	"more dextrous",
	"healthier",
	"cuter",
	"more agile",
	"bigger"
};


/*
 * Array of stat "descriptions"
 */
cptr desc_stat_imp_end[A_MAX] =
{
	"strong",
	"smart",
	"wise",
	"dextrous",
	"healthy",
	"cute",
	"agile",
	"big"
};


/*
 * Set "p_ptr->stat_inc_tim", notice observable changes
 */
bool set_stat_inc_tim(int v, int i)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->stat_inc_tim[i])
		{
			msg_format("You feel temporarily %s!",desc_stat_imp[i]);
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->stat_inc_tim[i])
		{
			msg_format("You feel less %s.", desc_stat_imp_end[i]);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->stat_inc_tim[i] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Array of stat "descriptions"
 */
cptr desc_stat_dec[] =
{
	"weaker",
	"stupider",
	"more naive",
	"clumsier",
	"sicklier",
	"uglier",
	"more slugish",
	"smaller"
};


/*
 * Array of stat "descriptions"
 */
static cptr desc_stat_dec_end[] =
{
	"weak",
	"stupid",
	"naive",
	"clumsy",
	"sickly",
	"ugly",
	"slugish",
	"small"
};

/*
 * Set "p_ptr->stat_dec_tim", notice observable changes
 */
bool set_stat_dec_tim(int v, int i)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->stat_dec_tim[i])
		{
			msg_format("You feel temporarily %s!", desc_stat_dec[i]);
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->stat_dec_tim[i])
		{
			msg_format("You no longer feel so %s.", desc_stat_dec_end[i]);
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->stat_dec_tim[i] = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("You feel yourself moving faster!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
			msg_print("You feel yourself slow down.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->fast = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("You feel yourself moving slower!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slow)
		{
			msg_print("You feel yourself speed up.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slow = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("A mystic shield forms around your body!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			msg_print("Your mystic shield crumbles away.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shield = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("You feel righteous!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blessed)
		{
			msg_print("The prayer has expired.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blessed = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("You feel like a hero!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->hero)
		{
			msg_print("The heroism wears off.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->hero = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->shero", notice observable changes
 */
bool set_shero(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->shero)
		{
			msg_print("You feel like a killing machine!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shero)
		{
			msg_print("You feel less Berserk.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shero = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msg_print("You feel safe from evil!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
			msg_print("You no longer feel safe from evil.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->protevil = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->invuln", notice observable changes
 */
bool set_invuln(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->invuln)
		{
			msg_print("You feel invulnerable!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
			msg_print("You feel vulnerable once more.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->invuln = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->free_act", notice observable changes
 */
bool set_free_act(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->free_act)
		{
			msg_print("You feel you may act freely!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
			msg_print("You feel less free in your actions.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->free_act = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->tim_image" affects monster visibility.
 */
bool set_tim_invis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_invis)
		{
			msg_print("Your eyes feel very sensitive!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
			msg_print("Your eyes feel less sensitive.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_invis = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

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
			msg_print("Your eyes begin to tingle!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_infra)
		{
			msg_print("Your eyes stop tingling.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_infra = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

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
 * Set "p_ptr->oppose_acid", notice observable changes
 */
bool set_oppose_acid(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_acid)
		{
			msg_print("You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_acid)
		{
			msg_print("You feel less resistant to acid.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_acid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_elec", notice observable changes
 */
bool set_oppose_elec(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_elec)
		{
			msg_print("You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_elec)
		{
			msg_print("You feel less resistant to electricity.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_elec = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_fire", notice observable changes
 */
bool set_oppose_fire(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_fire)
		{
			msg_print("You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_fire)
		{
			msg_print("You feel less resistant to fire.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_fire = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_cold", notice observable changes
 */
bool set_oppose_cold(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_cold)
		{
			msg_print("You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_cold)
		{
			msg_print("You feel less resistant to cold.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_cold = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->oppose_pois", notice observable changes
 */
bool set_oppose_pois(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_pois)
		{
			msg_print("You feel resistant to poison!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_pois)
		{
			msg_print("You feel less resistant to poison.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_pois = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

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
	if (p_ptr->stun > 100)
	{
		old_aux = 3;
	}

	/* Heavy stun */
	else if (p_ptr->stun > 50)
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
	if (v > 100)
	{
		new_aux = 3;
	}

	/* Heavy stun */
	else if (v > 50)
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
				msg_print("You have been stunned.");
				break;
			}

			/* Heavy stun */
			case 2:
			{
				msg_print("You have been heavily stunned.");
				break;
			}

			/* Knocked out */
			case 3:
			{
				msg_print("You have been knocked out.");
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
				msg_print("You are no longer stunned.");
				if (disturb_state) disturb(0, 0);
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
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the "stun" */
	p_ptr->redraw |= (PR_STUN);

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
	if (p_ptr->cut > 1000)
	{
		old_aux = 7;
	}

	/* Deep gash */
	else if (p_ptr->cut > 200)
	{
		old_aux = 6;
	}

	/* Severe cut */
	else if (p_ptr->cut > 100)
	{
		old_aux = 5;
	}

	/* Nasty cut */
	else if (p_ptr->cut > 50)
	{
		old_aux = 4;
	}

	/* Bad cut */
	else if (p_ptr->cut > 25)
	{
		old_aux = 3;
	}

	/* Light cut */
	else if (p_ptr->cut > 10)
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
	if (v > 1000)
	{
		new_aux = 7;
	}

	/* Deep gash */
	else if (v > 200)
	{
		new_aux = 6;
	}

	/* Severe cut */
	else if (v > 100)
	{
		new_aux = 5;
	}

	/* Nasty cut */
	else if (v > 50)
	{
		new_aux = 4;
	}

	/* Bad cut */
	else if (v > 25)
	{
		new_aux = 3;
	}

	/* Light cut */
	else if (v > 10)
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
				msg_print("You have been given a graze.");
				break;
			}

			/* Light cut */
			case 2:
			{
				msg_print("You have been given a light cut.");
				break;
			}

			/* Bad cut */
			case 3:
			{
				msg_print("You have been given a bad cut.");
				break;
			}

			/* Nasty cut */
			case 4:
			{
				msg_print("You have been given a nasty cut.");
				break;
			}

			/* Severe cut */
			case 5:
			{
				msg_print("You have been given a severe cut.");
				break;
			}

			/* Deep gash */
			case 6:
			{
				msg_print("You have been given a deep gash.");
				break;
			}

			/* Mortal wound */
			case 7:
			{
				msg_print("You have been given a mortal wound.");
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
				msg_print("You are no longer bleeding.");
				if (disturb_state) disturb(0, 0);
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
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw the "cut" */
	p_ptr->redraw |= (PR_CUT);

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
				msg_print("You are still weak.");
				break;
			}

			/* Hungry */
			case 2:
			{
				msg_print("You are still hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				msg_print("You are no longer hungry.");
				break;
			}

			/* Full */
			case 4:
			{
				msg_print("You are full!");
				break;
			}

			/* Bloated */
			case 5:
			{
				msg_print("You have gorged yourself!");
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
				msg_print("You are getting faint from hunger!");
				break;
			}

			/* Weak */
			case 1:
			{
				msg_print("You are getting weak from hunger!");
				break;
			}

			/* Hungry */
			case 2:
			{
				msg_print("You are getting hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				msg_print("You are no longer full.");
				break;
			}

			/* Full */
			case 4:
			{
				msg_print("You are no longer gorged.");
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

	/* Disturb - wake player if hungry */
	if (disturb_state) disturb(0, new_aux <= 2 ? TRUE : FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw hunger */
	p_ptr->redraw |= (PR_HUNGER);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}
/*
 * Set "p_ptr->rest", notice observable changes
 *
 * Tiring is handled in "dungeon.c", but computation of the rate of
 * tiring is handled in "xtra1.c". The player tires at a rate dependent
 * on their constitution, but this is impacted if they are slowed by
 * heavy equipment, and by moving through shallow, deep or filled
 * locations.
 *
 * Note the player rests to "catch their breath", but may not do so in
 * locations that are filled with a terrain type.
 *
 * Note that the player automatically catches their breath when searching,
 * (With the same caveat), but searching takes a full turns energy, rather
 * than a partial turn.
 *
 */

bool set_rest(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > PY_REST_MAX) ? PY_REST_MAX : (v < 0) ? 0 : v;

	/* Fainting / Starving */
	if (p_ptr->rest < PY_REST_FAINT)
	{
		old_aux = 0;
	}

	/* Weak */
	else if (p_ptr->rest < PY_REST_WEAK)
	{
		old_aux = 1;
	}

	/* Hungry */
	else if (p_ptr->rest < PY_REST_ALERT)
	{
		old_aux = 2;
	}

	/* Normal */
	else
	{
		old_aux = 3;
	}

	/* Fainting / Starving */
	if (v < PY_REST_FAINT)
	{
		new_aux = 0;
	}

	/* Weak */
	else if (v < PY_REST_WEAK)
	{
		new_aux = 1;
	}

	/* Hungry */
	else if (v < PY_REST_ALERT)
	{
		new_aux = 2;
	}

	/* Normal */
	else
	{
		new_aux = 3;
	}

	/* Rest increase */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{

			/* Normal */
			case 3:
			{
				msg_print("You are no longer tired.");

				/* Change */
				notice = TRUE;

				break;
			}

		}

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
				msg_print("You are getting faint from exhaustion!");
				break;
			}

			/* Weak */
			case 1:
			{
				msg_print("You are getting weak from exhaustion!");
				break;
			}

			/* Hungry */
			case 2:
			{
				msg_print("You are getting short of breath.");
				break;
			}

		}

		/* Change */
		notice = TRUE;
	}

	/* Use the value */
	p_ptr->rest = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw hunger */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Mark items as aware as a result of gaining a level.
 */
void improve_aware(void)
{
	int i;
	int awareness = -1;
	int tip_count_start = 0;
	int tip_count = 0;
	
	/* Hack -- Check for id'ed */
	for (i=1;i<z_info->w_max;i++)
	{
		if (w_info[i].benefit != WB_ID) continue;

		if (w_info[i].class != p_ptr->pclass) continue;

		if (w_info[i].level > p_ptr->lev) continue;

		if ((w_info[i].styles & (1L << p_ptr->pstyle)) == 0) continue;
		
		awareness = 2*(p_ptr->lev - w_info[i].level)+1;
	}

	/* Hack -- efficiency for tips */
	for (i=1;i<z_info->k_max;i++)
	{
		/*Already aware */
		if (k_info[i].aware) continue;
		
		/* Check awareness */
		if (k_info[i].tval == style2tval[p_ptr->pstyle])
		{
			tip_count_start++;
		}
	}
	
	/* Hack -- Check for id'ed */
	for (i=1;i<z_info->k_max;i++)
	{
		/*Already aware */
		if (k_info[i].aware) continue;

		/* Check for awareness */
		if (k_info[i].level > awareness) continue;
		
		/* Check awareness */
		if (k_info[i].tval == style2tval[p_ptr->pstyle])
		{
			queue_tip(format("kind%d.txt", i));
			k_info[i].aware=TRUE;
			p_ptr->notice |= (PN_REORDER | PN_COMBINE);
			
			tip_count++;
		}
	}
	
	/* Show all tval tips */
	for(i = tip_count_start; i < tip_count_start + tip_count; i++)
	{
		/* Show tips */
		queue_tip(format("tval%d-%d.txt", style2tval[p_ptr->pstyle], i));
	}
}


static int stat_gain_selection[PY_MAX_STAT_GAIN];
static int stat_gain_selected;


/*
 * Print a list of stats (for improvement).
 */
void print_stats(const s16b *sn, int num, int y, int x)
{
	int i, j;

	byte attr;

	/* Display title */
	/* prt("", y, x); */

	/* Add labels */
	for (i = 0; i < num; i++)
	{
		attr = TERM_WHITE;

		if (p_ptr->stat_cur[sn[i]] < p_ptr->stat_max[sn[i]]) attr = TERM_YELLOW;
		if (p_ptr->stat_max[sn[i]] == 18 + 999) attr = TERM_L_DARK;

		for (j = 0; j < stat_gain_selected; j++)
		{
			if (stat_gain_selection[j] == i) attr = TERM_L_BLUE;
		}

		/* Display the label */
		c_prt(attr, format("  %c) ", I2A(i)), y + i + 1, x);

		/* Display the stats */
		display_player_stat_info(y + 1, x + 5, i, i + 1, attr);	
	}

	/* Display drop-shadow */
	prt("", y + i + 1, x);
}


/*
 * Improve a player-chosen set of stats.
 * TODO: upon pressing ESC restart the choice.
 * Note the hack to always improve the maximal value of a stat.
 */
static void improve_stat(void)
{
	int tmp = 0;
	int i;

	s16b table[A_MAX+1];

	char buf[32];

	cptr p = "";

	int count = 0;

#ifdef ALLOW_BORG
	if (count_stop) return;
#endif

	/* Flush messages */
	if (easy_more) messages_easy(FALSE);
	
	/* Check which stats can still be improved */
	for (i = 0; i < A_MAX; i++)
	{
		table[i] = i;
		if (p_ptr->stat_max[i] < 18 + 999) count++;
	}

	/* No stats left to improve */
	if (!count) return;

	/* Reduce count to number of abilities allowed improvements */
	if (count > stat_gains[p_ptr->lev -1]) count = stat_gains[p_ptr->lev -1];

	/* Confirm stat selection */
	while (count)
	{
		/* Improve how many stats with level gain */
		for (stat_gain_selected = 0; stat_gain_selected < count; stat_gain_selected++)
		{
			/* Should be paranoid here */
			while (TRUE)
			{
				sprintf(buf,"Improve which attribute%s (%d)", count > 1 ? "s" : "", count - stat_gain_selected); 

				/* Select stat to improve */
				if (get_list(print_stats, table, A_MAX, "Attribute", buf, 1, 36, &(stat_gain_selection[stat_gain_selected])))
				{
					/* Check if stat at maximum */
					if (p_ptr->stat_max[stat_gain_selection[stat_gain_selected]] >= 18 + 999)
					{
						msg_format("You cannot get any %s",desc_stat_imp[stat_gain_selection[stat_gain_selected]]);
					}

					/* Good stat_gain_selection? */
					else
					{
						bool okay = TRUE;

						/* Check we are not improving another stat */
						for (i = 0; i < stat_gain_selected; i++)
						{
							if (stat_gain_selection[i] == stat_gain_selection[stat_gain_selected]) okay = FALSE;
						}

						if (okay)
							break;
						else
							msg_print("You must choose another attribute.");
					}
				}
				else
				{
					/* Clear the attributes */
					stat_gain_selected = 0;
				}
			}
		}

		/* Save screen */
		screen_save();

		/* Redisplay stats */
		print_stats(table, A_MAX, 1, 36);

		/* Confirm? */
		if (get_check("Increasing highlighted stats. Are you sure? "))
		{
			/* Load screen */
			screen_load();

			break;
		}

		/* Load screen */
		screen_load();
	}
	
	/* Improve how many stats with level gain */
	for (stat_gain_selected = 0; stat_gain_selected < count; stat_gain_selected++)
	{
		/* Display */
		if (p_ptr->stat_cur[stat_gain_selection[stat_gain_selected]] < p_ptr->stat_max[stat_gain_selection[stat_gain_selected]])
		{
			/* Set description */
			p = "you could be ";

			/* Hack --- store stat */
			tmp = p_ptr->stat_cur[stat_gain_selection[stat_gain_selected]];
			p_ptr->stat_cur[stat_gain_selection[stat_gain_selected]] = p_ptr->stat_max[stat_gain_selection[stat_gain_selected]];
		}
		else
		{ 
			p = "";
			tmp = 0;
		}

		/* Increase */
		inc_stat(stat_gain_selection[stat_gain_selected]);

		/* Message */
		msg_format("You feel %s%s.",p,desc_stat_imp[stat_gain_selection[stat_gain_selected]]);

		/* Hack --- restore stat */
		if (tmp) p_ptr->stat_cur[stat_gain_selection[stat_gain_selected]] = tmp;
	}
}



/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;

	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp)
	{
		int adjust = -1;
		s32b new_exp = p_ptr->exp;
		s32b new_exp_frac = p_ptr->exp_frac;

		/* Gain levels while possible */
		while ((p_ptr->max_lev < PY_MAX_LEVEL) &&
		       (p_ptr->exp >= (player_exp[p_ptr->max_lev+adjust] *
				       p_ptr->expfact / 100L)))
		{

			/* Reduce outstanding experience after level gain */
			p_ptr->exp = (player_exp[p_ptr->max_lev+adjust] * p_ptr->expfact / 100L) +
				(p_ptr->exp - (player_exp[p_ptr->max_lev+adjust] * p_ptr->expfact / 100L)) * (p_ptr->max_lev + adjust + 1) / (p_ptr->max_lev + adjust + 2);

			/* Modify adjustment */
			adjust++;
		}

		/* Add fractional experience */
		/* Handle fractional experience */
		if (adjust > -1) new_exp_frac = (((new_exp * (p_ptr->max_lev + adjust + 1)) % (p_ptr->max_lev + adjust + 2))
				* 0x10000L / (p_ptr->max_lev + adjust + 2)) + p_ptr->exp_frac;

		/* Keep track of experience */
		if (new_exp_frac >= 0x10000L)
		{
			p_ptr->exp++;
			p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
		}
		else
		{
			p_ptr->exp_frac = (u16b)new_exp_frac;
		}

		/* Set new maximum experience */
		p_ptr->max_exp = p_ptr->exp;

	}

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();

	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
	       (p_ptr->exp < (player_exp[p_ptr->lev-2] *
			      p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= (player_exp[p_ptr->lev-1] *
			       p_ptr->expfact / 100L)))
	{
		int i;

		/* Gain a level */
		p_ptr->lev++;

		/* Improve a stat */
		if (p_ptr->lev > p_ptr->max_lev) improve_stat();

		/* Improve awareness */
		if (p_ptr->lev > p_ptr->max_lev) improve_aware();

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Show all tips for intermediate levels */
		for (i = p_ptr->max_lev; i <= p_ptr->lev; i++)
		{
			/* Level tips */
			queue_tip(format("level%d.txt", p_ptr->prace, i));

			/* Race tips */
			queue_tip(format("race%d-%d.txt", p_ptr->prace, i));

			/* Class tips */
			queue_tip(format("class%d-%d.txt", p_ptr->pclass, i));

			/* Style tips */
			queue_tip(format("ws%d-%d-%d.txt", p_ptr->pclass, p_ptr->pstyle, i));
		}

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_lev) p_ptr->max_lev = p_ptr->lev;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_EXP | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
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
 * Hack -- Return the "automatic food type" of a monster race
 * Used to allocate proper treasure when "Mushrooms" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
int get_food_type(const monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze "food" monsters */
	if ((r_ptr->d_char == ',') && (strstr(name,"mushroom")))
	{
		/* Look for textual clues */
		if (strstr(name, "Spotted ")) return (SV_FOOD_POISON+1);
		if (strstr(name, "Silver ")) return (SV_FOOD_BLINDNESS+1);
		if (strstr(name, "Yellow ")) return (SV_FOOD_PARANOIA+1);
		if (strstr(name, "Grey ")) return (SV_FOOD_CONFUSION+1);
		if (strstr(name, "Copper ")) return (SV_FOOD_PARALYSIS+1);
		if (strstr(name, "Pink ")) return (SV_FOOD_WEAKNESS+1);
		if (strstr(name, "Purple ")) return (SV_FOOD_SICKNESS+1);
		if (strstr(name, "Black ")) return (SV_FOOD_STUPIDITY+1);
		if (strstr(name, "Green ")) return (SV_FOOD_NAIVETY+1);
		if (strstr(name, "Rotting ")) return (SV_FOOD_UNHEALTH+1);
		if (strstr(name, "Brown ")) return (SV_FOOD_DISEASE+1);
		if (strstr(name, "Shrieker ")) return (SV_FOOD_CURE_PARANOIA+1);
		if (strstr(name, "Noxious ")) return (SV_FOOD_DISEASE+1);
		if (strstr(name, "Magic ")) return ((rand_int(100)<30?SV_FOOD_MANA+1:SV_FOOD_HALLUCINATION+1));

		/* Force nothing */
		return (-1);
	}
	/* Analyze "food" monsters */
	else if (strstr(name,", Farmer Maggot"))
	{
		return (SV_FOOD_MANA+1);
	}
	/* Analyze "slime mold" monsters */
	else if (strstr(name,"lime mold"))
	{
		return (SV_FOOD_SLIME_MOLD+1);
	}


	/* Assume nothing */
	return (0);
}


/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
int get_coin_type(const monster_race *r_ptr)
{
	int i;
	cptr name = (r_name + r_ptr->name);

	/* Analyze "coin" monsters */
	if (strchr("$gdDAaI", r_ptr->d_char))
	{
		for (i = 0; i < MAX_GOLD; i++)
		{
			object_kind *k_ptr = &k_info[i + OBJ_GOLD_LIST];

			/* Look for textual clues */
			if (strstr(name, format(" %s ",k_name + k_ptr->name))) return (i);
			if (strstr(name, format("%^s ", k_name + k_ptr->name))) return (i);
			if (strstr(name, format(" %ss",k_name + k_ptr->name))) return (i);
		}

		/* Hack -- rubies */
		if (strstr(name, " rubies")) return(13);
	}

	/* Assume nothing */
	return (0);
}


/*
 * Handle the quest assignment
 *
 * Getting a quest assigned.
 */
void quest_assign(int q_idx)
{
	(void)q_idx;
}


/*
 * Handle the quest reward.
 *
 * Completing a quest 
 */
void quest_reward(int q_idx)
{
	(void)q_idx;

}

/*
 * Handle the quest reward.
 *
 * Completing a quest 
 */
void quest_penalty(int q_idx)
{
	(void)q_idx;
}


void scatter_objects_under_feat(int y, int x)
{
  s16b this_o_idx, next_o_idx = 0;
  
  object_type *o_ptr;

  assert (in_bounds(y, x));

  /* Scan all objects in the grid */
  for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
      /* Get the object */
      o_ptr = &o_list[this_o_idx];

      /* Get the next object */
      next_o_idx = o_ptr->next_o_idx;

      /* Drop the object */
      drop_near(o_ptr, -1, y, x);
    }

  /* Scan all objects in the grid */
  for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
    {
      /* Get the object */
      o_ptr = &o_list[this_o_idx];

      /* Get the next object */
      next_o_idx = o_ptr->next_o_idx;

		/* Delete object */
		delete_object_idx(this_o_idx);
    }

  /* Visual update */
  lite_spot(y, x);
}


/*
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques.
 * Thus (for now) all Quest monsters should be Uniques.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx)
{
	int i, j, y, x, ny, nx;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));
	bool do_chest = (r_ptr->flags8 & (RF8_DROP_CHEST) ? TRUE : FALSE);

	int force_food = ((r_ptr->flags9 & (RF9_DROP_MUSHROOM)) != 0 ? get_food_type(r_ptr) : 0);
	int force_coin = ((r_ptr->flags9 & (RF9_DROP_MINERAL)) != 0 ? get_coin_type(r_ptr) : 0);

	object_type *i_ptr;
	object_type object_type_body;


	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Incur summoning debt */
	if ((m_ptr->mflag & (MFLAG_ALLY)) && (m_ptr->summoned))
	{
		/* Summoning debt requires blood */
		if (r_ptr->level > p_ptr->csp)
		{
			/* Incur blood debt */
			take_hit(damroll(r_ptr->level - p_ptr->csp, 3),"blood debt for a slain minion");
			
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
		}
		
		/* Debt can be met by mana */
		else
		{
			p_ptr->csp -= r_ptr->level;		
		}

		/* Update mana */
		p_ptr->update |= (PU_MANA);
		p_ptr->redraw |= (PR_MANA);
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1 | PW_PLAYER_2 | PW_PLAYER_3);

		/* Player death */
		if (p_ptr->is_dead) return;
	}

	
	/* Extinguish lite */
	delete_monster_lite(m_idx);

	/* Monster death updates visible monsters */
	p_ptr->window |= (PW_MONLIST);

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Copy the object */
		object_copy(i_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(i_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;

	/* Hack -- only sometimes drop bodies */
	if ((rand_int(100)<30) || ((r_ptr->flags1 & (RF1_UNIQUE)) != 0) ||
		((r_ptr->flags2 & (RF2_REGENERATE)) != 0) ||
		(r_ptr->level > p_ptr->depth) ||
		((r_ptr->flags8 & (RF8_ASSEMBLY)) != 0))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */ 
		object_wipe(i_ptr);

		/* Drop a body? */
		if (make_body(i_ptr, m_ptr->r_idx))
		{
			/* Note who dropped it */
			i_ptr->name3 = m_ptr->r_idx;

			/* I'll be back, Bennett */
			if (r_ptr->flags2 & (RF2_REGENERATE)) i_ptr->timeout = damroll(3,6);

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);
		}
	}

	/* Add some residue */
	if (r_ptr->flags3 & (RF3_DEMON)) feat_near(FEAT_FLOOR_FIRE_T,m_ptr->fy,m_ptr->fx);
	if (r_ptr->flags3 & (RF3_UNDEAD)) feat_near(FEAT_FLOOR_DUST_T,m_ptr->fy,m_ptr->fx);
	if (r_ptr->flags8 & (RF8_HAS_SLIME)) feat_near(FEAT_FLOOR_SLIME_T,m_ptr->fy,m_ptr->fx);

	/* Do we drop more treasure? */
	if (m_ptr->mflag & (MFLAG_MADE)) return;

	/* Mega-Hack -- drop "winner" treasures */
	if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Grond" */
		object_prep(i_ptr, lookup_kind(TV_HAFTED, SV_GROND));

		/* Mega-Hack -- Mark this item as "Grond" */
		i_ptr->name1 = ART_GROND;

		/* Mega-Hack -- Actually create "Grond" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);


		/* Get local object */
		i_ptr = &object_type_body;

		/* Mega-Hack -- Prepare to make "Morgoth" */
		object_prep(i_ptr, lookup_kind(TV_CROWN, SV_MORGOTH));

		/* Mega-Hack -- Mark this item as "Morgoth" */
		i_ptr->name1 = ART_MORGOTH;

		/* Mega-Hack -- Actually create "Morgoth" */
		apply_magic(i_ptr, -1, TRUE, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);

		/* Hack -- this is temporary */
		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
		msg_print("*** CONGRATULATIONS ***");
		msg_print("You have won the game!");
		msg_print("You may retire (commit suicide) when you are ready.");

	}


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	/* Hack -- handle mushroom patches */
	food_type = force_food;

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Hack -- handle monster drops */
	race_drop_idx = m_ptr->r_idx;

	/* Average dungeon and monster levels */
	object_level = (p_ptr->depth + r_ptr->level) / 2;

	/* Clear monster equipment */
	hack_monster_equip = 0L;
	
	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make Chest */
		if (do_chest && do_item && (rand_int(100) < 5))
		{
			int chest;

			/* Drop it in the dungeon */
			if (make_chest(&chest)) feat_near(chest,y,x);

			l_ptr->flags8 |= (RF8_DROP_CHEST);

			hack_monster_equip |= (RF8_DROP_CHEST);
			
			continue;
		}

		/* Hack - Have applied all items in monster equipment. We force gold, then clear. */
		if ((r_ptr->flags8 & (RF8_DROP_MASK)) &&
				((r_ptr->flags8 & (RF8_DROP_MASK)) == (hack_monster_equip & (RF8_DROP_MASK))))
		{
			if (do_gold && do_item)
			{
				do_item = FALSE;
			}
			else
			{
				hack_monster_equip = 0L;
			}
		}
		
		/* Make Gold */
		if (do_gold && (!do_item || (rand_int(100) < 50) ))
		{
			/* Make some gold */
			if (!make_gold(i_ptr, good, great)) continue;

			if (coin_type) l_ptr->flags9 |= (RF9_DROP_MINERAL);

			/* Assume seen XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, good, great)) continue;

			/* Hack -- chest/bag mimics drop matching tvals */
			if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (r_ptr->d_char == '&')) tval_drop_idx = i_ptr->tval;

			/* Hack -- mimics */
			if (r_ptr->flags1 & (RF1_CHAR_MULTI)) l_ptr->flags1 |= (RF1_CHAR_MULTI);

			/* Learn about drops */
			else if (food_type) l_ptr->flags9 |= (RF9_DROP_MUSHROOM);

			/* Hack -- ignore bodies */
			else switch (i_ptr->tval)
			{
				case TV_JUNK:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_JUNK);
					l_ptr->flags8 |= (RF8_DROP_JUNK);
					break;
				}

				case TV_BOW:
				{
					hack_monster_equip |= (RF8_DROP_MISSILE);
				}
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				{
					l_ptr->flags8 |= (RF8_DROP_MISSILE);
					break;
				}

				case TV_DIGGING:
				case TV_SPIKE:
				case TV_FLASK:
				{
					hack_monster_equip |= (RF8_DROP_TOOL);
					l_ptr->flags8 |= (RF8_DROP_TOOL);
					break;
				}

				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				{
					hack_monster_equip |= (RF8_DROP_WEAPON);
					l_ptr->flags8 |= (RF8_DROP_WEAPON);
					break;
				}

				case TV_SONG_BOOK:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_MUSIC);
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_WRITING);					
				}
				case TV_INSTRUMENT:
				{
					hack_monster_equip |= (RF8_DROP_MUSIC);
					l_ptr->flags8 |= (RF8_DROP_MUSIC);
					break;
				}

				case TV_BOOTS:
				{
					hack_monster_equip |= (RF8_HAS_LEG);
					l_ptr->flags8 |= (RF8_DROP_CLOTHES);
					break;
				}
				case TV_GLOVES:
				{
					hack_monster_equip |= (RF8_HAS_HAND);
					l_ptr->flags8 |= (RF8_DROP_CLOTHES);
					break;
				}
				case TV_CLOAK:
				{
					hack_monster_equip |= (RF8_HAS_CORPSE);
					l_ptr->flags8 |= (RF8_DROP_CLOTHES);
					break;
				}
				case TV_SOFT_ARMOR:
				{
					hack_monster_equip |= (RF8_DROP_ARMOR);
					l_ptr->flags8 |= (RF8_DROP_CLOTHES);
					break;
				}

				case TV_HELM:
				{
					hack_monster_equip |= (RF8_HAS_HEAD);
					l_ptr->flags8 |= (RF8_DROP_ARMOR);
					break;
				}
				case TV_SHIELD:
				{
					hack_monster_equip |= (RF8_HAS_ARM);
					l_ptr->flags8 |= (RF8_DROP_ARMOR);
					break;
				}
				case TV_DRAG_ARMOR:
				case TV_HARD_ARMOR:
				{
					hack_monster_equip |= (RF8_DROP_ARMOR);
					l_ptr->flags8 |= (RF8_DROP_ARMOR);
					break;
				}

				case TV_CROWN:
				{
					hack_monster_equip |= (RF8_HAS_HEAD);
					l_ptr->flags8 |= (RF8_DROP_JEWELRY);
					break;
				}
				case TV_AMULET:
				{
					hack_monster_equip |= (RF8_HAS_SKULL);
					l_ptr->flags8 |= (RF8_DROP_JEWELRY);
					break;
				}
				case TV_RING:
				{
					hack_monster_equip |= (RF8_HAS_HAND);
					l_ptr->flags8 |= (RF8_DROP_JEWELRY);
					break;
				}

				case TV_LITE:
				{
					hack_monster_equip |= (RF8_DROP_LITE);
					l_ptr->flags8 |= (RF8_DROP_LITE);
					break;
				}

				case TV_STAFF:
				{
					hack_monster_equip |= (RF8_DROP_WEAPON);
				}
				case TV_ROD:
				case TV_WAND:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_RSW);
					l_ptr->flags8 |= (RF8_DROP_RSW);
					break;
				}

				case TV_SCROLL:
				case TV_MAP:
				case TV_MAGIC_BOOK:
				case TV_PRAYER_BOOK:
				case TV_RUNESTONE:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_WRITING);
					l_ptr->flags8 |= (RF8_DROP_WRITING);
					break;
				}

				case TV_POTION:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_POTION);
					l_ptr->flags8 |= (RF8_DROP_POTION);
					break;
				}

				case TV_FOOD:
				{
					if (rand_int(100) < 50) hack_monster_equip |= (RF8_DROP_FOOD);
					l_ptr->flags8 |= (RF8_DROP_FOOD);
					break;
				}
			}

			/* Assume seen XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset monster equipment */
	hack_monster_equip = 0L;
	
	/* Reset the object level */
	object_level = p_ptr->depth;

	/* Reset "food" type */
	food_type = 0;

	/* Reset "coin" type */
	coin_type = 0;

	/* Reset "tval drop" type */
	tval_drop_idx = 0;

	/* Reset "monster drop" type */
	race_drop_idx = 0;

	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & (RF1_QUESTOR | RF1_GUARDIAN)))
		return;

	/* Check quests for completion */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest_type *q_ptr = &(q_list[i]);
		quest_event *qe_ptr = &(q_ptr->event[q_ptr->stage]);

		if (q_ptr->stage == QUEST_ACTION) qe_ptr = &(q_ptr->event[QUEST_ACTIVE]);

		if ((qe_ptr->dungeon != p_ptr->dungeon) ||
			(qe_ptr->level != p_ptr->depth - min_depth(p_ptr->dungeon))) continue;

		if (!(qe_ptr->race) || (qe_ptr->race != m_list[m_idx].r_idx)) continue;

		/* Assign quest */
		if (q_ptr->stage == QUEST_ASSIGN)
		{
			/* Wipe the structure */
			(void)WIPE(qe_ptr, quest_event);

			qe_ptr->dungeon = p_ptr->dungeon;
			qe_ptr->level = p_ptr->depth - min_depth(p_ptr->dungeon);
			qe_ptr->race = m_list[m_idx].r_idx;
			qe_ptr->number = 1;
			qe_ptr->flags |= (EVENT_KILL_RACE);

			quest_assign(i);

			continue;
		}

		if (q_ptr->stage != QUEST_ACTION) continue;

		/* If last monster killed, drop artifact */
		if ((q_ptr->event[QUEST_ACTION].number + 1 >= qe_ptr->number) && (qe_ptr->artifact))
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Prepare artifact */
			qe_ptr->kind = lookup_kind(a_info[qe_ptr->artifact].tval, a_info[qe_ptr->artifact].sval);

			/* Prepare object */
			object_prep(i_ptr, qe_ptr->kind);

			/* Prepare artifact */
			i_ptr->name1 = qe_ptr->artifact;

			/* Apply magic */
			apply_magic(i_ptr, object_level, FALSE, FALSE, FALSE);

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, m_list[m_idx].fy, m_list[m_idx].fx);
		}

		/* All slain quest monsters drop items */
		else if ((qe_ptr->kind) || (qe_ptr->ego_item_type))
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Prepare ego item */
			if ((qe_ptr->ego_item_type) && !(qe_ptr->kind)) qe_ptr->kind =
				lookup_kind(e_info[qe_ptr->ego_item_type].tval[0],
					e_info[qe_ptr->ego_item_type].min_sval[0]);

			/* Prepare object */
			object_prep(i_ptr, qe_ptr->kind);

			/* Prepare ego item */
			i_ptr->name2 = qe_ptr->ego_item_type;

			/* Apply magic */
			apply_magic(i_ptr, object_level, FALSE, FALSE, FALSE);

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
		}

		/* Update actions */
		qe_ptr = &(q_ptr->event[QUEST_ACTION]);

		/* Fail quest because we killed someone */
		if (q_ptr->event[QUEST_FAILED].flags & (EVENT_KILL_RACE))
		{
			/* Wipe the structure */
			(void)WIPE(qe_ptr, quest_event);

			/* Set action details */
			qe_ptr->dungeon = p_ptr->dungeon;
			qe_ptr->level = p_ptr->depth - min_depth(p_ptr->dungeon);
			qe_ptr->race = m_list[m_idx].r_idx;
			qe_ptr->number = 1;
			qe_ptr->flags |= (EVENT_KILL_RACE);

			quest_penalty(i);
		}

		/* Get closer to success because we need to terrify someone */
		else if ((qe_ptr->flags & (EVENT_KILL_RACE)) && (qe_ptr->number + 1 >= q_ptr->event[QUEST_ACTIVE].number))
		{
			/* Don't count terrified monsters if we can kill _or_ terrify them */
			if (!m_ptr->monfear || !(qe_ptr->flags & (EVENT_FEAR_RACE))) qe_ptr->number++;

			qe_ptr->flags |= (EVENT_KILL_RACE);

			/* Have completed quest? */
			if ((qe_ptr->flags == q_ptr->event[QUEST_ACTIVE].flags) && (qe_ptr->number >= q_ptr->event[QUEST_ACTIVE].number))
			{
				msg_print("Congratulations. You have succeeded at your quest.");

				quest_reward(i);
			}
			/* Partially completed quest */
			else
			{
				msg_print("You have xxx to go.");
			}
		}
	}

	/* Dungeon guardian defeated - need some stairs except on surface */
	if (r_ptr->flags1 & (RF1_GUARDIAN))
	{
		/* Generate stairs if path is opened from this location */
		if ((p_ptr->depth != min_depth(p_ptr->dungeon)) &&
				/* Last quest on level */
				(p_ptr->depth == max_depth(p_ptr->dungeon)))
		{
			/* Stagger around */
			while (!cave_valid_bold(y, x) && !cave_clean_bold(y,x))
		    {
				int d = 1;
	
				/* Pick a location */
				scatter(&ny, &nx, y, x, d, 0);
	
		    	/* Stagger */
				y = ny; x = nx;
		    }
	
		  /* Explain the staircase */
		  msg_print("A magical staircase appears...");
	
		  /* Create stairs down */
		  cave_set_feat(y, x, FEAT_MORE);
	
		  /* Save any objects in that place */
		  scatter_objects_under_feat(y, x);
		}
	}


	/* Hack -- Finishing quest 1 completes the game */
	/* Nothing left, game over... */
	if (q_list[1].stage == QUEST_REWARD)
	{
		/* Total winner */
		p_ptr->total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
		msg_print("*** CONGRATULATIONS ***");
		msg_print("You have won the game!");
		msg_print("You may retire (commit suicide) when you are ready.");
	}
}




/*
 * Decrease a monster's hit points, handle monster death.
 *
 * We return TRUE if the monster has been killed (and deleted).
 *
 * We announce monster death (using an optional "death message"
 * if given, and a otherwise a generic killed/destroyed message).
 *
 * Only "physical attacks" can induce the "You have slain" message.
 * Missile and Spell attacks will induce the "dies" message, or
 * various "specialized" messages.  Note that "You have destroyed"
 * and "is destroyed" are synonyms for "You have slain" and "dies".
 *
 * Invisible monsters induce a special "You have killed it." message.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * Consider decreasing monster experience over time, say, by using
 * "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))" instead
 * of simply "(m_exp * m_lev) / (p_lev)", to make the first monster
 * worth more than subsequent monsters.  This would also need to
 * induce changes in the monster recall code.  XXX XXX XXX
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac;
	byte new_level;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Are we hurting it badly? */
	if (((m_ptr->maxhp / 3) < dam) && (m_ptr->maxhp > rand_int(100)))
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Rip off a head */
		if ((m_ptr->hp < dam) && make_head(i_ptr,m_ptr->r_idx))
		{
			/* Note who dropped it */
			i_ptr->name3 = m_ptr->r_idx;

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
		}
		/* Rip off a limb */
		else if ((m_ptr->hp - dam < m_ptr->maxhp / 10) && make_part(i_ptr,m_ptr->r_idx))
		{
			/* Note who dropped it */
			i_ptr->name3 = m_ptr->r_idx;

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
		}

		/* Rip off some skin */
		else if (make_skin(i_ptr,m_idx))
		{
			/* Note who dropped it */
			i_ptr->name3 = m_ptr->r_idx;

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, m_ptr->fy, m_ptr->fx);
		}

		/* Add some blood */
		if (r_ptr->flags8 & (RF8_HAS_BLOOD)) feat_near(FEAT_FLOOR_BLOOD_T,m_ptr->fy,m_ptr->fx);

		/* Add some slime */
		if (r_ptr->flags8 & (RF8_HAS_SLIME)) feat_near(FEAT_FLOOR_SLIME_T,m_ptr->fy,m_ptr->fx);
	}

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, sizeof(m_name), m_idx, 0);

		/* Death by Missile/Spell attack */
		if (note)
		{
			message_format(MSG_KILL, m_ptr->r_idx, "%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
			 (r_ptr->flags2 & (RF2_STUPID)))
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have slain %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		if ((r_ptr->flags3 & (RF3_NONLIVING)) ||
			 (r_ptr->flags2 & (RF2_STUPID)))
		{
			/* Warn allies */
			tell_allies_death(m_ptr->fy, m_ptr->fx, "& has destroyed one of us!");
		}
		else
		{
			/* Warn allies */
			tell_allies_death(m_ptr->fy, m_ptr->fx, "& has killed one of us!");
		}

		/* Allies don't provide experience */
		if ((m_ptr->mflag & (MFLAG_ALLY)) == 0)
		{
			/* Maximum player level */
			div = p_ptr->max_lev;

			/* Give some experience for the kill */
			new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

			/* Base adjustment */
			new_level = -1;

			/* Handle fractional experience */
			new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
					* 0x10000L / div) + p_ptr->exp_frac;

			/* Keep track of experience */
			if (new_exp_frac >= 0x10000L)
			{
				new_exp++;
				p_ptr->exp_frac = (u16b)(new_exp_frac - 0x10000L);
			}
			else
			{
				p_ptr->exp_frac = (u16b)new_exp_frac;
			}

			/* Gain experience */
			gain_exp(new_exp);
		}

		/* Generate treasure */
		monster_death(m_idx);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 0;

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			/* Count kills this life */
			if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

			/* Show killer tips */
			if (!l_ptr->tkills) queue_tip(format("kill%d.txt", m_ptr->r_idx));
			
			/* Count kills in all lives */
			if (l_ptr->tkills < MAX_SHORT) l_ptr->tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->r_idx);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Monster is dead */
		return (TRUE);
	}

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			set_monster_fear(m_ptr, 0, FALSE);

			/* No more fear */
			(*fear) = FALSE;

			/* Warn allies */
			tell_allies_mflag(m_ptr->fy, m_ptr->fx, MFLAG_AGGR, "& has attacked me!");
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)) && (dam > 0))
	{
		long oldhp, newhp, tmp;
		int percentage, fitness;

		/* Note -- subtle fix -CFT */
		newhp = (long)(m_ptr->hp);
		oldhp = newhp + (long)(dam);
		tmp = (100L * dam) / (oldhp);
		percentage = (int)(tmp);

		/* Percentage of fully healthy. Note maxhp can be zero. */
		fitness = (100L * (m_ptr->hp + 1)) / (m_ptr->maxhp + 1);

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (more often) when hit for 11% or more its current hit points
		 * 
		 * Percentages depend on player's charisma.
		 */
		if ((randint(adj_chr_fear[p_ptr->stat_ind[A_CHR]]) >= fitness) ||
		    ((percentage > 20) && (rand_int(adj_chr_fear[p_ptr->stat_ind[A_CHR]] * percentage) > 100)) )
		{
			int fear_amt;

			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			fear_amt = rand_range(20, 30);

			/* Get frightened */
			set_monster_fear(m_ptr, fear_amt, TRUE);
		}
	}

	/* Monster will always go active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/* Recalculate desired minimum range */
	if (dam > 0) m_ptr->min_range = 0;

	/* Not dead yet */
	return (FALSE);
}


/*
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * Hack -- The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(int wy, int wx)
{
	dungeon_zone *zone=&t_info[0].zone[0];

	/* Get the zone */
	get_zone(&zone,p_ptr->dungeon,p_ptr->depth);

	/* Verify wy, adjust if needed */
	if (!zone->fill)
	{
		if (wy > TOWN_HGT - SCREEN_HGT) wy = TOWN_HGT - SCREEN_HGT;
		else if (wy < 0) wy = 0;
	}
	else if (wy > DUNGEON_HGT - SCREEN_HGT) wy = DUNGEON_HGT - SCREEN_HGT;

	if (wy < 0) wy = 0;

	/* Verify wx, adjust if needed */
	if (!zone->fill)
	{
		if (wx > TOWN_WID - SCREEN_WID) wx = TOWN_WID - SCREEN_WID;
		else if (wx < 0) wx = 0;
	}
	else if (wx > DUNGEON_WID - SCREEN_WID) wx = DUNGEON_WID - SCREEN_WID;

	if (wx < 0) wx = 0;

	/* React to changes */
	if ((p_ptr->wy != wy) || (p_ptr->wx != wx))
	{
		/* Save wy, wx */
		p_ptr->wy = wy;
		p_ptr->wx = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Hack -- Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Changed */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


/*
 * Perform the minimum "whole panel" adjustment to ensure that the given
 * location is contained inside the current panel, and return TRUE if any
 * such adjustment was performed.
 */
bool adjust_panel(int y, int x)
{
	int wy = p_ptr->wy;
	int wx = p_ptr->wx;

	/* Adjust as needed */
	while (y >= wy + SCREEN_HGT) wy += SCREEN_HGT;
	while (y < wy) wy -= SCREEN_HGT;

	/* Adjust as needed */
	while (x >= wx + SCREEN_WID) wx += SCREEN_WID;
	while (x < wx) wx -= SCREEN_WID;

	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	int wy = p_ptr->wy + ddy[dir] * PANEL_HGT;
	int wx = p_ptr->wx + ddx[dir] * PANEL_WID;

	/* Use "modify_panel" */
	return (modify_panel(wy, wx));
}

/* 
 * Hack - generate the current room description 
 */
static void get_room_desc(int room, char *name, int name_s, char *text_visible, int text_visible_s, char *text_always, int text_always_s)
{
	bool scan_name = FALSE;
	bool scan_desc = FALSE;
	bool beware = FALSE;

	town_type *t_ptr = &t_info[p_ptr->dungeon];
	dungeon_zone *zone=&t_ptr->zone[0];

	/* Get the zone */ 
	get_zone(&zone,p_ptr->dungeon,max_depth(p_ptr->dungeon));

	/* Initialize text */
	my_strcpy(name, "", name_s);
	if (text_always) my_strcpy(text_always,"", text_always_s);
	if (text_visible) my_strcpy(text_visible,  cheat_xtra ? format("Room %d:", room) : "", text_visible_s);

	/* Town or not in room */
	if (!room)
	{
		if ((p_ptr->depth == min_depth(p_ptr->dungeon)) || (!zone->fill))
		{
			my_strcpy(name, t_name + t_ptr->name, name_s);

			if (!text_always) return;

			/* Describe location */
			my_strcpy(text_always, t_text + t_ptr->text, text_always_s);
		}
		else
		{
			my_strcpy(name, "empty room", name_s);
		}
		return;
	}
	
	/* In room */
	switch (room_info[room].type)
	{
		case (ROOM_TOWER):
		{
			my_strcpy(name, "the tower of ", name_s);
			my_strcat(name, t_name + t_ptr->name, name_s);

			/* Brief description */
			if (text_visible) my_strcpy(text_visible, "This tower is filled with monsters and traps.  ", text_visible_s);

			/* Describe height of tower */
			if (text_visible)
			{
				my_strcpy(text_visible, format("It looks about %d feet tall.  ", (max_depth(p_ptr->dungeon) - min_depth(p_ptr->dungeon) + 1) * 50), text_visible_s);
			}
			break;
		}

		case (ROOM_LAIR):
		{
			monster_race *r_ptr = &r_info[room_info[room].deepest_race];

			my_strcpy(name, "the lair of ", name_s);
			my_strcat(name, r_name + r_ptr->name, name_s);
			if ((r_ptr->flags1 & (RF1_UNIQUE)) == 0) my_strcat(name, "s", name_s);

			if (text_visible) my_strcpy(text_visible, "This is the lair of ", text_visible_s);
			if (text_visible) my_strcat(text_visible, r_name + r_ptr->name, text_visible_s);
			if ((text_visible) && ((r_ptr->flags1 & (RF1_UNIQUE)) == 0)) my_strcat(name, "s", text_visible_s);
			if (text_visible) my_strcat(text_visible, ".  ", text_visible_s);
			beware = TRUE;

			break;
		}

		case (ROOM_GREATER_VAULT):
		{
			my_strcpy(name, "greater ", name_s);

			if (text_visible) my_strcpy(text_visible, "This vast sealed chamber is amongst the largest of its kind and is filled with ", text_visible_s);
			if (text_visible) my_strcat(text_visible, "deadly monsters and rich treasure.  ", text_visible_s);
			beware = TRUE;

			/* Fall through */	
		}

		case (ROOM_LESSER_VAULT):
		{
			/* Display vault name */
			if ((v_name + v_info[room_info[room].vault].name)[0] == '\'')
			{
				my_strcat(name, "vault ", name_s);
				my_strcat(name, v_name + v_info[room_info[room].vault].name, name_s);
			}
			else
			{
				my_strcat(name, "vault", name_s);
			}

			scan_desc = TRUE;
			break;
		}

		case (ROOM_INTERESTING):
		{
			my_strcat(name, v_name + v_info[room_info[room].vault].name, name_s);
			if (text_visible) my_strcpy(text_visible, "There is something remarkable here.  ", text_visible_s);
			break;
		}

		case (ROOM_CHAMBERS):
		{
			if (text_visible) my_strcpy(text_visible, "This is one of many rooms crowded with monsters.  ", text_visible_s);
			scan_name = TRUE;
			scan_desc = TRUE;
			break;
		}

		default:
		{
			scan_name = TRUE;
			scan_desc = TRUE;
			break;
		}
	}

	/* Read through and display the description if required */
	if ((scan_name) || (scan_desc))
	{
		int i, j;

		char buf_name1[16];
		char buf_name2[16];
		char *last_buf = NULL;
		int last_buf_s = 0;

		/* Clear the name1 text */
		buf_name1[0] = '\0';

		/* Clear the name2 text */
		buf_name2[0] = '\0';
			
		i = 0;

		if (cheat_xtra && text_always) my_strcat(text_visible, format ("%s (%ld)", r_name + r_info[room_info[room].deepest_race].name, room_info[room].ecology), text_visible_s);
		
		while ((room >= 0) && (i < ROOM_DESC_SECTIONS))
		{
			/* Get description */
			j = room_info[room].section[i++];

			/* End of description */
			if (j < 0) break;

			/* Get the name1 text if needed */
			if (!strlen(buf_name1)) my_strcpy(buf_name1, (d_name + d_info[j].name1), sizeof(buf_name1));

			/* Get the name2 text if needed */
			if (!strlen(buf_name2)) my_strcpy(buf_name2, (d_name + d_info[j].name2), sizeof(buf_name2));

			/* Need description? */
			if (!scan_desc) continue;

			/* Must understand language? */
			if (d_info[j].flags & (ROOM_LANGUAGE))
			{
				/* Does the player understand the main language of the level? */
				if ((last_buf) && (cave_ecology.ready) && (cave_ecology.num_races)
					&& player_understands(monster_language(room_info[room].deepest_race)))
				{
					/* Get the textual history */
					my_strcat(last_buf, (d_text + d_info[j].text), last_buf_s);
				}
				else if (last_buf)
				{
					/* Fake it */
					my_strcat(last_buf, "nothing you can understand.  ", last_buf_s);

					/* Clear last buf to skip remaining language lines */
					last_buf = NULL;
				}

				/* Diagnostics */
				if ((cheat_xtra) && (last_buf)) my_strcat(last_buf, format("%d", i), last_buf_s);
			}

			/* Visible description */
			else if (d_info[j].flags & (ROOM_SEEN))
			{
				/* Get the textual history */
				if (text_visible) my_strcat(text_visible, (d_text + d_info[j].text), text_visible_s);

				/* Record last buffer for language */
				last_buf = text_visible;
				last_buf_s = text_visible_s;

				/* Diagnostics */
				if ((cheat_xtra) && (text_visible)) my_strcat(text_visible, format("%d", i), text_visible_s);
			}

			/* Description always present */
			else
			{
				/* Get the textual history */
				if (text_always) my_strcat(text_always, (d_text + d_info[j].text), text_always_s);

				/* Record last buffer for language */
				last_buf = text_always;
				last_buf_s = text_always_s;

				/* Diagnostics */
				if ((cheat_xtra) && (text_always)) my_strcat(text_always, format("%d", i), text_always_s);
			}
		}

		/* Set the name if required */
		if (scan_name)
		{

			/* Set room name */
			if (strlen(buf_name1)) my_strcpy(name, buf_name1, name_s);

			/* And add second room name if necessary */
			if (strlen(buf_name2))
			{
				if (strlen(buf_name1))
				{
					my_strcat(name, " ", name_s);
					my_strcat(name, buf_name2, name_s);
				}
				else
				{
					my_strcpy(name, buf_name2, name_s);
				}
			}
		}
	}

	/* Beware */
	if ((beware) && (text_always)) my_strcpy(text_always, "Beware!  ", text_always_s);

	if ((cheat_room) && (text_always))
	{
		if (room_info[room].flags & (ROOM_ICKY)) my_strcat(text_always,"This room cannot be teleported into.  ", text_always_s);
		if (room_info[room].flags & (ROOM_BLOODY)) my_strcat(text_always,"This room prevent you naturally healing your wounds.  ", text_always_s);
		if (room_info[room].flags & (ROOM_CURSED)) my_strcat(text_always,"This room makes you vulnerable to being hit.  ", text_always_s);
		if (room_info[room].flags & (ROOM_GLOOMY)) my_strcat(text_always,"This room cannot be magically lit.  ", text_always_s);
		if (room_info[room].flags & (ROOM_PORTAL)) my_strcat(text_always,"This room magically teleports you occasionally.  ", text_always_s);
		if (room_info[room].flags & (ROOM_SILENT)) my_strcat(text_always,"This room prevents you casting spells.  ", text_always_s);
		if (room_info[room].flags & (ROOM_STATIC)) my_strcat(text_always,"This room prevents you using wands, staffs or rods.  ", text_always_s);
	}

}


/*
 * Hack -- Display the "name" of a given room
 */
static void room_info_top(int room)
{
	char first[2];
	char name[40];
	char text_visible[1024];
	char text_always[1024];

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Get the actual room description */
	get_room_desc(room, name, sizeof(name), text_visible, sizeof(text_visible), text_always, sizeof(text_always));

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Hack - set first character to upper */
	if (first[0] >= 'a') first[0] = name[0]-32;
	else first[0] = name[0];
	first[1] = '\0';

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, first);

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (name+1));

}

/*
 * Hack -- describe the given room at the top of the screen
 */
static void screen_room_info(int room)
{
	char name[32];
	char text_visible[1024];
	char text_always[1024];

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Get the actual room description */
	get_room_desc(room, name, sizeof(name), text_visible, sizeof(text_visible), text_always, sizeof(text_always));

	/* Flush messages */
	msg_print(NULL);

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Describe room */
	if ((strlen(text_visible)) && (room_info[room].flags & (ROOM_SEEN)))
	{
		/* Recall monster */
		text_out(text_visible);

		if (strlen(text_always))
		{
			text_out(text_always);
		}

	}
	else if (strlen(text_always))
	{
		text_out(text_always);
	}
	else
	{
		text_out("There is nothing remarkable about it.");
	}

	/* Describe room */
	room_info_top(room);
}


/*
 * Hack -- describe the given room info in the current "term" window
 */
void display_room_info(int room)
{
	int y;
	char name[32];
	char text_visible[1024];
	char text_always[1024];

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Get the actual room description */
	get_room_desc(room, name, sizeof(name), text_visible, sizeof(text_visible), text_always, sizeof(text_always));

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Describe room */
	if ((strlen(text_visible)) && (room_info[room].flags & (ROOM_SEEN)))
	{
		/* Recall monster */
		text_out(text_visible);

		if (strlen(text_always))
		{
			text_out(text_always);
		}

	}
	else if (strlen(text_always))
	{
		text_out(text_always);
	}
	else
	{
		text_out("There is nothing remarkable about it.");
	}

	/* Describe room */
	room_info_top(room);
}

/*
 * Hack -- describe players current location.
 */
void describe_room(void)
{
	int room = room_idx(p_ptr->py, p_ptr->px);
	char name[32];
	char text_visible[1024];
	char text_always[1024];


#ifdef ALLOW_BORG
	/* Hack -- No descriptions for the borg */
	if (count_stop) return;
#endif

	/* Hack -- handle "xtra" mode */
	if (!character_dungeon) return;

	/* Hack -- not a room */
	if (!(cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))) room = 0;

	/* Get the actual room description */
	get_room_desc(room, name, sizeof(name), text_visible, sizeof(text_visible), text_always, sizeof(text_always));

	if ((cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW))
	 && (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)))
	{
		if (room_names)
		{
			msg_format("You have entered %s%s.",
				 (prefix(name, "the ") ? "" :
				 (is_a_vowel(name[0]) ? "an " : "a ")),name);
		}

		if (!(room_descriptions) || (room_info[room].flags & (ROOM_ENTERED)))
		{
			/* Nothing more */
		}
		else if ((!easy_more) && (!auto_more) && (strlen(text_visible) + strlen(text_always) > 80))
		{
			message_flush();

			screen_save();

			/* Set text_out hook */
			text_out_hook = text_out_to_screen;

			text_out_c(TERM_L_BLUE, name);
			text_out("\n");

			if (strlen(text_visible))
			{
				/* Message */
				text_out(text_visible);
				
				/* Add it to the buffer */
				message_add(text_visible, MSG_GENERIC);
			}

			if (strlen(text_always))
			{
				/* Message */
				text_out(text_always);

				/* Add it to the buffer */
				message_add(text_always, MSG_GENERIC);
			}

			(void)anykey();

			/* Load screen */
			screen_load();
		}
		else if (strlen(text_visible) && strlen(text_always))
		{
			msg_format("%s %s", text_visible, text_always);
		}
		else if (strlen(text_visible))
		{
			msg_print(text_visible);
		}
		else if (strlen(text_always))
		{
			msg_print(text_always);
		}

		/* Room has been entered */
		if (room_descriptions) room_info[room].flags |= (ROOM_ENTERED);
	}
	else if ((strlen(text_always)) && !(room_info[room].flags & (ROOM_HEARD)) &&
	  (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)))
	{
		/* Message */
		if (room_descriptions) msg_format("%s", text_always);

		/* Room has been heard */
		if (room_descriptions) room_info[room].flags |= (ROOM_HEARD);
	}
	else if ((p_ptr->depth == town_depth(p_ptr->dungeon))
		|| (p_ptr->depth == min_depth(p_ptr->dungeon)))
	{
		msg_format("You have entered %s.",name);

		if (strlen(text_always))
		{
			if (easy_more || auto_more)
			{
				msg_print(text_always);
			}
			else
			{
				message_flush();
	
				screen_save();
	
				/* Set text_out hook */
				text_out_hook = text_out_to_screen;
	
				text_out_c(TERM_L_BLUE, name);
				text_out("\n");
	
				/* Message */
				text_out(text_always);
	
				(void)anykey();
	
				screen_load();
			}
		}
	}

	/* Window stuff */
	p_ptr->window |= (PW_ROOM_INFO);

}



/*
 * Verify the current panel (relative to the player location).
 *
 * By default, when the player gets "too close" to the edge of the current
 * panel, the map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 *
 * The "center_player" option allows the current panel to always be centered
 * around the player, which is very expensive, and also has some interesting
 * gameplay ramifications.
 */
void verify_panel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int wy = p_ptr->wy;
	int wx = p_ptr->wx;


	/* Scroll screen vertically when off-center */
	if (center_player && (!p_ptr->running || !run_avoid_center) &&
	    (py != wy + SCREEN_HGT / 2))
	{
		wy = py - SCREEN_HGT / 2;
	}

	/* Scroll screen vertically when 2 grids from top/bottom edge */
	else if ((py < wy + 2) || (py >= wy + SCREEN_HGT - 2))
	{
		wy = ((py - PANEL_HGT / 2) / PANEL_HGT) * PANEL_HGT;
	}


	/* Scroll screen horizontally when off-center */
	if (center_player && (!p_ptr->running || !run_avoid_center) &&
	    (px != wx + SCREEN_WID / 2))
	{
		wx = px - SCREEN_WID / 2;
	}

	/* Scroll screen horizontally when 4 grids from left/right edge */
	else if ((px < wx + 4) || (px >= wx + SCREEN_WID - 4))
	{
		wx = ((px - PANEL_WID / 2) / PANEL_WID) * PANEL_WID;
	}


	/* Scroll if needed */
	if (modify_panel(wy, wx))
	{
		/* Optional disturb on "panel change" */
		if (disturb_panel && !center_player) disturb(0, 0);
	}
}


/*
 * Monster health description
 */
cptr look_mon_desc(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool living = TRUE;

	/* Report status effects */
	if (m_ptr->csleep) return("asleep");
	if (m_ptr->confused) return("confused");
	if (m_ptr->monfear) return("afraid");
	if (m_ptr->stunned) return("stunned");
	if (m_ptr->cut) return("bleeding");
	if (m_ptr->poisoned) return("poisoned");
	if (find_monster_ammo(m_idx, -1, FALSE) < 0) return("out of ammo");
	if (m_ptr->mflag & (MFLAG_TOWN)) return("townsfolk");

	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags3 & (RF3_UNDEAD)) living = FALSE;
	if (r_ptr->flags3 & (RF3_DEMON)) living = FALSE;
	if (r_ptr->flags3 & (RF3_NONLIVING)) living = FALSE;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		return (living ? "unhurt" : "undamaged");
	}
	else
	{
		/* Calculate a health "percentage" */
		int perc = 100L * m_ptr->hp / m_ptr->maxhp;

		if (perc >= 60)
			return(living ? "somewhat wounded" : "somewhat damaged");
		else if (perc >= 25)
			return (living ? "wounded" : "damaged");
		else if (perc >= 10)
			return (living ? "badly wounded" : "badly damaged");
		else
			return(living ? "almost dead" : "almost destroyed");
	}
}



/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(vptr u, vptr v, int p, int q)
{
	int z, a, b;

	/* Done sort */
	if (p >= q) return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (TRUE)
	{
		/* Slide i2 */
		while (!(*ang_sort_comp)(u, v, b, z)) b--;

		/* Slide i1 */
		while (!(*ang_sort_comp)(u, v, z, a)) a++;

		/* Done partition */
		if (a >= b) break;

		/* Swap */
		(*ang_sort_swap)(u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b+1, q);
}


/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(vptr u, vptr v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}





/*** Targetting Code ***/


/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
sint motion_dir(int y1, int x1, int y2, int x2)
{
	/* No movement required */
	if ((y1 == y2) && (x1 == x2)) return (5);

	/* South or North */
	if (x1 == x2) return ((y1 < y2) ? 2 : 8);

	/* East or West */
	if (y1 == y2) return ((x1 < x2) ? 6 : 4);

	/* South-east or South-west */
	if (y1 < y2) return ((x1 < x2) ? 3 : 1);

	/* North-east or North-west */
	if (y1 > y2) return ((x1 < x2) ? 9 : 7);

	/* Paranoia */
	return (5);
}


/*
 * Extract a direction (or zero) from a character
 */
sint target_dir(char ch)
{
	int d;

	int mode;

	cptr act;

	cptr s;


	/* Default direction */
	d = (isdigit(ch) ? D2I(ch) : 0);

	/* Roguelike */
	if (rogue_like_commands)
	{
		mode = KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		mode = KEYMAP_MODE_ORIG;
	}

	/* Extract the action (if any) */
	act = keymap_act[mode][(byte)(ch)];

	/* Analyze */
	if (act)
	{
		/* Convert to a direction */
		for (s = act; *s; ++s)
		{
			/* Use any digits in keymap */
			if (isdigit(*s)) d = D2I(*s);
		}
	}

	/* Paranoia */
	if (d == 5) d = 0;

	/* Return direction */
	return (d);
}


/*
 * Determine is a monster makes a reasonable target
 *
 * The concept of "targetting" was stolen from "Morgul" (?)
 *
 * The player can target any location, or any "target-able" monster.
 *
 * Currently, a monster is "target_able" if it is visible, and if
 * the player can hit it with a projection, and the player is not
 * hallucinating.  This allows use of "use closest target" macros.
 *
 * Future versions may restrict the ability to target "trappers"
 * and "mimics", but the semantics is a little bit weird.
 */
bool target_able(int m_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx, 0)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* Hack -- Never target trappers XXX XXX XXX */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Assume okay */
	return (TRUE);
}




/*
 * Update (if necessary) and verify (if possible) the target.
 *
 * We return TRUE if the target is "okay" and FALSE otherwise.
 */
bool target_okay(void)
{
	/* No target */
	if (!p_ptr->target_set) return (FALSE);

	/* Accept "location" targets */
	if (p_ptr->target_who == 0) return (TRUE);

	/* Check "monster" targets */
	if (p_ptr->target_who > 0)
	{
		int m_idx = p_ptr->target_who;

		/* Accept reasonable targets */
		if (target_able(m_idx))
		{
			monster_type *m_ptr = &m_list[m_idx];

			/* Get the monster location */
			p_ptr->target_row = m_ptr->fy;
			p_ptr->target_col = m_ptr->fx;

			/* Good target */
			return (TRUE);
		}
	}

	/* Assume no target */
	return (FALSE);
}


/*
 * Get allies to adopt the player's target.
 * 
 * If order is true, we order allies to this.
 * If order is false, only allies without targets will go here.
 */
static void player_tell_allies_target(int y, int x, bool order)
{
	int i;
	
	/* Get allies to target this location */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* Skip non-allies, or allies who ignore the player */
		if ( ((m_ptr->mflag & (MFLAG_ALLY)) == 0) || ((m_ptr->mflag & (MFLAG_IGNORE)) != 0) ) continue;
		
		/* Skip unseen monsters that the player cannot speak to or telepathically communicate with */
		if (!m_ptr->ml && ((r_info[m_ptr->r_idx].flags3 & (RF3_NONVOCAL)) == 0))
		{
			/* Cannot hear the player */
			if ((m_ptr->cdis > 3) && ((play_info[m_ptr->fy][m_ptr->fx] & (PLAY_FIRE)) == 0)) continue;
			
			/* Cannot understand the player */
			if (!player_understands(monster_language(m_ptr->r_idx))) continue;
		}

		/* Skip monsters with targets already */
		if ((!order) && (m_ptr->ty || m_ptr->tx)) continue;
		
		/* Set the monster target */
		m_ptr->ty = y;
		m_ptr->tx = x;
	}
}


/*
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx, s16b flags)
{
	/* Acceptable target */
	if ((m_idx > 0) && target_able(m_idx))
	{
		monster_type *m_ptr = &m_list[m_idx];

		/* Save target info */
		p_ptr->target_set = flags | (TARGET_KILL);
		p_ptr->target_who = m_idx;
		p_ptr->target_row = m_ptr->fy;
		p_ptr->target_col = m_ptr->fx;
		
		/* Get allies to target this */
		player_tell_allies_target(m_ptr->fy, m_ptr->fx, FALSE);
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = 0;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}
}


/*
 * Set the target to a location
 */
void target_set_location(int y, int x, s16b flags)
{
	/* Legal target */
	if (in_bounds_fully(y, x))
	{
		/* Save target info */
		p_ptr->target_set = flags | (TARGET_GRID);
		p_ptr->target_who = 0;
		p_ptr->target_row = y;
		p_ptr->target_col = x;
		
		/* Get allies to target this */
		player_tell_allies_target(y,x, FALSE);
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = 0;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}
}


/*
 * Sorting hook -- comp function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by double-distance to the player.
 */
static bool ang_sort_comp_distance(vptr u, vptr v, int a, int b)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	int da, db, kx, ky;

	/* Absolute distance components */
	kx = x[a]; kx -= px; kx = ABS(kx);
	ky = y[a]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	da = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Absolute distance components */
	kx = x[b]; kx -= px; kx = ABS(kx);
	ky = y[b]; ky -= py; ky = ABS(ky);

	/* Approximate Double Distance to the first point */
	db = ((kx > ky) ? (kx + kx + ky) : (ky + ky + kx));

	/* Compare the distances */
	return (da <= db);
}


/*
 * Sorting hook -- swap function -- by "distance to player"
 *
 * We use "u" and "v" to point to arrays of "x" and "y" positions,
 * and sort the arrays by distance to the player.
 */
static void ang_sort_swap_distance(vptr u, vptr v, int a, int b)
{
	byte *x = (byte*)(u);
	byte *y = (byte*)(v);

	byte temp;

	/* Swap "x" */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;

	/* Swap "y" */
	temp = y[a];
	y[a] = y[b];
	y[b] = temp;
}



/*
 * Hack -- help "select" a location (see below)
 */
static s16b target_pick(int y1, int x1, int dy, int dx)
{
	int i, v;

	int x2, y2, x3, y3, x4, y4;

	int b_i = -1, b_v = 9999;


	/* Scan the locations */
	for (i = 0; i < temp_n; i++)
	{
		/* Point 2 */
		x2 = temp_x[i];
		y2 = temp_y[i];

		/* Directed distance */
		x3 = (x2 - x1);
		y3 = (y2 - y1);

		/* Verify quadrant */
		if (dx && (x3 * dx <= 0)) continue;
		if (dy && (y3 * dy <= 0)) continue;

		/* Absolute distance */
		x4 = ABS(x3);
		y4 = ABS(y3);

		/* Verify quadrant */
		if (dy && !dx && (x4 > y4)) continue;
		if (dx && !dy && (y4 > x4)) continue;

		/* Approximate Double Distance */
		v = ((x4 > y4) ? (x4 + x4 + y4) : (y4 + y4 + x4));

		/* Penalize location XXX XXX XXX */

		/* Track best */
		if ((b_i >= 0) && (v >= b_v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Result */
	return (b_i);
}


/*
 * Hack -- determine if a given location is "interesting"
 */
static bool target_set_interactive_accept(int y, int x, int mode)
{
	s16b this_o_idx, next_o_idx = 0;


	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized object */
		if (o_ptr->ident & (IDENT_MARKED)) return (TRUE);
	}

	/* Interesting memorized features */
	if ((play_info[y][x] & (PLAY_MARK)) && (f_info[cave_feat[y][x]].flags1 & (FF1_NOTICE)))
	{
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_interactive_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_interactive_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = p_ptr->wy; ((y < p_ptr->wy + SCREEN_HGT) && (temp_n<TEMP_MAX)); y++)
	{
		for (x = p_ptr->wx; ((x < p_ptr->wx + SCREEN_WID)&&(temp_n<TEMP_MAX)); x++)
		{
			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x, mode)) continue;

			/* Special mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must be a targettable monster */
				if (!target_able(cave_m_idx[y][x])) continue;
				
				/* Must not be an ally */
				if (m_list[cave_m_idx[y][x]].mflag & (MFLAG_ALLY)) continue;
			}

			/* Special mode */
			if (mode & (TARGET_ALLY))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must be an ally */
				if ((m_list[cave_m_idx[y][x]].mflag & (MFLAG_ALLY)) == 0) continue;
			}
			
			/* If matching race, must match race */
			if (p_ptr->target_race)
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must match the monster */
				if (m_list[cave_m_idx[y][x]].r_idx != p_ptr->target_race) continue;
			}

			/* Save the location */
			temp_x[temp_n] = x;
			temp_y[temp_n] = y;
			temp_n++;

		}
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_distance;
	ang_sort_swap = ang_sort_swap_distance;

	/* Sort the positions */
	ang_sort(temp_x, temp_y, temp_n);
}


/*
 * Examine a grid, return a keypress.
 *
 * The "mode" argument contains the "TARGET_LOOK" bit flag, which
 * indicates that the "space" key should scan through the contents
 * of the grid, instead of simply returning immediately.  This lets
 * the "look" command get complete information, without making the
 * "target" command annoying.
 *
 * The "info" argument contains the "commands" which should be shown
 * inside the "[xxx]" text.  This string must never be empty, or grids
 * containing monsters will be displayed with an extra comma.
 *
 * Note that if a monster is in the grid, we update both the monster
 * recall info and the health bar info to track that monster.
 *
 * This function correctly handles multiple objects per grid, and objects
 * and terrain features in the same grid, though the latter never happens.
 *
 * This function must handle blindness/hallucination.
 */
key_event target_set_interactive_aux(int y, int x, int *room, int mode, cptr info)
{
	s16b this_o_idx, next_o_idx = 0;

	cptr s1, s2, s3;

	bool boring;

	bool floored;

	int feat;

	key_event query;

	char out_val[160];

	/* Repeat forever */
	while (1)
	{
		/* Paranoia */
		query.key = ' ';

		/* Assume boring */
		boring = TRUE;

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "";


		/* The player */
		if (cave_m_idx[y][x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}


		/* Hack -- hallucination */
		if (p_ptr->image)
		{
			cptr name = "something strange";

			/* Display a message */
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = anykey();

			/* Stop on everything but "return" */
			if ((query.key != '\n') && (query.key != '\r')) break;

			/* Repeat forever */
			continue;
		}


		/* Actual monsters */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Visible */
			if (m_ptr->ml)
			{
				bool recall = FALSE;

				char m_name[80];

				/* Not boring */
				boring = FALSE;

				/* Get the monster name ("a goblin") */
				monster_desc(m_name, sizeof(m_name), cave_m_idx[y][x], 0x08);

				/* Hack -- track this monster race */
				monster_race_track(m_ptr->r_idx);

				/* Hack -- health bar for this monster */
				health_track(cave_m_idx[y][x]);

				/* Hack -- handle stuff */
				handle_stuff();

				/* Interact */
				while (1)
				{
					/* Recall */
					if (recall)
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_monster_look(cave_m_idx[y][x]);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,s,%s]", info));

						/* Command */
						query = inkey_ex();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						/* Describe, and prompt for recall */
						sprintf(out_val, "%s%s%s%s (%s) [r,s,%s]",
							s1, s2, s3, m_name, look_mon_desc(cave_m_idx[y][x]), info);
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey_ex();
					}

					/* Select any monsters similar to this */
					if (query.key == 's')
					{
						if (p_ptr->target_race)
						{
							p_ptr->target_race = 0;
						}
						else
						{
							p_ptr->target_race = m_list[cave_m_idx[y][x]].r_idx;
						}
					}
					
					/* Normal commands */
					if (query.key != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Hack -- take account of gender */
				if (r_ptr->flags1 & (RF1_FEMALE)) s1 = "She is ";
				else if (r_ptr->flags1 & (RF1_MALE)) s1 = "He is ";

				/* Use a preposition */
				s2 = "carrying ";

				/* Scan all objects being carried */
				for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					char o_name[80];

					object_type *o_ptr;

					bool recall = FALSE;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

					/* Interact */
					while (1)
					{
						/* Recall */
						if (recall)
						{
							/* Save screen */
							screen_save();

							/* Recall on screen */
							screen_object(o_ptr);

							/* Hack -- Complete the prompt (again) */
							Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

							/* Command */
							query = inkey_ex();

							/* Load screen */
							screen_load();
						}

						/* Normal */
						else
						{
							/* Describe the object */
							sprintf(out_val, "%s%s%s%s [r,%s]", s1, s2, s3, o_name, info);
							prt(out_val, 0, 0);

							/* Place cursor */
							move_cursor_relative(y, x);

							/* Command */
							query = inkey_ex();
						}

						/* Normal commands */
						if (query.key != 'r') break;

						/* Toggle recall */
						recall = !recall;
					}

					/* Stop on everything but "return"/"space" */
					if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

					/* Sometimes stop at "space" key */
					if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
					s2 = "also carrying ";
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
				if (m_ptr->mflag && (MFLAG_OVER))
				{
					s2 = "above ";
				}
				else
				{
					/* Use a preposition */
					s2 = "on ";
				}
			}
		}

		/* Assume not floored */
		floored = FALSE;

#ifdef ALLOW_EASY_FLOOR

		/* Scan all objects in the grid */
		if (easy_floor)
		{
			int floor_list[MAX_FLOOR_STACK];
			int floor_num;

			/* Scan for floor objects */
			floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, y, x, 0x02 | 0x08);

			/* Actual pile */
			if (floor_num > 1)
			{
				/* Not boring */
				boring = FALSE;

				/* Floored */
				floored = TRUE;

				/* Describe */
				while (1)
				{
					/* Describe the pile */
					sprintf(out_val, "%s%s%sa pile of %d objects [r,%s]",
						s1, s2, s3, floor_num, info);
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey_ex();

					/* Display objects */
					if (query.key == 'r')
					{
						/* Save screen */
						screen_save();

						/* Display */
						show_floor(floor_list, floor_num);

						/* Describe the pile */
						prt(out_val, 0, 0);
						query = inkey_ex();

						/* Load screen */
						screen_load();

						/* Continue on 'r' only */
						if (query.key == 'r') continue;
					}

					/* Done */
					break;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";
			}
		}

#endif /* ALLOW_EASY_FLOOR */

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Get the object */
			o_ptr = &o_list[this_o_idx];

			/* Get the next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Skip objects if floored */
			if (floored) continue;

			/* Describe it */
			if (o_ptr->ident & (IDENT_MARKED))
			{
				bool recall = FALSE;

				char o_name[80];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				/* Interact */
				while (1)
				{
					/* Recall */
					if (recall)
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_object(o_ptr);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey_ex();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						/* Describe the object */
						sprintf(out_val, "%s%s%s%s [r,%s]", s1, s2, s3, o_name, info);
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey_ex();
					}

					/* Normal commands */
					if (query.key != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query.key == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Plurals */
				if (o_ptr->number != 1) s1 = "They are ";

				/* Preposition */
				s2 = "on ";
			}
		}

		/* Double break */
		if (this_o_idx) break;


		/* Feature (apply "mimic") */
		feat = f_info[cave_feat[y][x]].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(play_info[y][x] & (PLAY_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			cptr name = f_name + f_info[feat].name;

			bool recall = FALSE;

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* Pick a prefix */
			if ((*s2) && ((!(f_info[feat].flags1 & (FF1_MOVE)) && !(f_info[feat].flags3 & (FF3_EASY_CLIMB))) ||
			    !(f_info[feat].flags1 & (FF1_LOS)) ||
			    (f_info[feat].flags1 & (FF1_ENTER)) ||
			    (f_info[feat].flags2 & (FF2_SHALLOW)) ||
			    (f_info[feat].flags2 & (FF2_DEEP)) ||
			    (f_info[feat].flags2 & (FF2_FILLED)) ||
			    (f_info[feat].flags2 & (FF2_CHASM)) ||
			    (f_info[feat].flags2 & (FF2_HIDE_SNEAK)) ||
			    (f_info[feat].flags3 & (FF3_NEED_TREE)) ))
			{
				s2 = "in ";
			}

			else if (*s2)
			{
				s2 = "on ";
			}

			/* Pick a prefix */
			if ((f_info[feat].flags2 & (FF2_SHALLOW)) ||
			    (f_info[feat].flags2 & (FF2_DEEP)) ||
			    (f_info[feat].flags2 & (FF2_FILLED)) ||
			    (f_info[feat].flags3 & (FF3_GROUND)) )
			{
				s3 = "";
			}

			else
			{
				/* Pick proper indefinite article */
				s3 = (is_a_vowel(name[0]) ? "an " : "a ");
			}

			/* Hack -- already a 'the' prefix */
			if (prefix(name, "the ")) s3 = "";

			/* Hack -- special introduction for filled areas */
			if (*s2 && (f_info[feat].flags2 & (FF2_FILLED))) s2 = "surrounded by ";

			/* Hack -- special introduction for store doors */
			if (f_info[feat].flags1 & (FF1_ENTER))
			{
				s3 = "the entrance to the ";
			}


			/* Interact */
			while (1)
			{
				/* Recall */
				if (recall)
				{
					/* Save screen */
					screen_save();

					/* Recall on screen */
					screen_feature_roff(feat);

					/* Hack -- Complete the prompt (again) */
					Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

					/* Command */
					query = inkey_ex();

					/* Load screen */
					screen_load();
				}

				/* Normal */
				else
				{

					/* Display a message */
					sprintf(out_val, "%s%s%s%s [r,%s]", s1, s2, s3, name, info);
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey_ex();
				}

				/* Normal commands */
				if (query.key != 'r') break;

				/* Toggle recall */
				recall = !recall;
			}


			/* Stop on everything but "return"/"space" */
			if ((query.key != '\n') && (query.key != '\r') && (query.key != ' ')) break;

			/* Change the intro */
			s1 = "It is ";
		}

		/* Room description if needed */
		if (((play_info[y][x] & (PLAY_MARK)) != 0) &&
			((cave_info[y][x] & (CAVE_ROOM)) != 0) &&
			(room_has_flag(y, x, ROOM_SEEN) != 0) &&
			(room_names) && (*room != dun_room[y/BLOCK_HGT][x/BLOCK_HGT]))
		{
			int i;
			bool edge = FALSE;
			bool recall = FALSE;

			/* Get room location */
			int by = y/BLOCK_HGT;
			int bx = x/BLOCK_HGT;

			char name[32];
			*room = dun_room[by][bx];

			/* Get the actual room description */
			get_room_desc(*room, name, sizeof(name), NULL, 0, NULL, 0);

			/* Always in rooms */
			s2 = "in ";

			/* Hack --- edges of rooms */
			for (i = 0;i<8;i++)
			{
				int yy = y+ddy[i];
				int xx = x+ddx[i];

				if (!(cave_info[yy][xx] & (CAVE_ROOM))) edge = TRUE;
			}

			if (edge) s2 = "outside ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Interact */
			while (1)
			{
				/* Recall */
				if (recall)
				{
					/* Save screen */
					screen_save();

					/* Recall on screen */
					screen_room_info(*room);

					/* Hack -- Complete the prompt (again) */
					Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

					/* Command */
					query = inkey_ex();

					/* Load screen */
					screen_load();
				}

				/* Normal */
				else
				{
					/* Describe, and prompt for recall */
					sprintf(out_val, "%s%s%s%s [r,%s]",
						s1, s2, s3, name, info);
						prt(out_val, 0, 0);

					/* Place cursor */
					move_cursor_relative(y, x);

					/* Command */
					query = inkey_ex();
				}

				/* Normal commands */
				if (query.key != 'r') break;

				/* Toggle recall */
				recall = !recall;
		       }
		}


		/* Stop on everything but "return" */
		if ((query.key != '\n') && (query.key != '\r')) break;
	}

	/* Keep going */
	return (query);
}

/*
 * Variables used to highlight project path to target.
 */
static int target_path_n;
static u16b target_path_g[256];


/*
 * Modify a 'boring' grid appearance based on the 'projectability'
 */
void modify_grid_boring_project(byte *a, char *c, int y, int x, byte cinfo, byte pinfo)
{
	(void)y;
	(void)x;
	(void)cinfo;

	/* Handle "blind" first*/
	if (p_ptr->blind)
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark gray" */
			*a = dark_attr[dark_attr[*a]];
		}
	}

	/* Handle "fire" grids */
	else if (pinfo & (PLAY_FIRE))
	{
		int i;

		for (i = 0; i < target_path_n; i++)
		{
			int path_y = GRID_Y(target_path_g[i]);
			int path_x = GRID_X(target_path_g[i]);

			/* Hack -- Stop before hitting walls */
			if (!cave_project_bold(y, x)) break;

			/* X, Y on projection path? */
			if ((path_y == y) && (path_x == x))
			{
				/* Mega-hack */
				if (*a & 0x80)
				{
					/* Use a brightly lit tile */
					if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
						*c -= 1;
					else if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
						*c += 2;
				}
				else
				{
					/* Use "yellow" */
					*a = lite_attr[*a];
				}

				/* Important -- exit loop */
				break;
			}
		}

		/* Use normal */
	}

	/* Handle "dark" grids */
	else
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark tile" */
			*a = dark_attr[*a];
		}
	}
}

/*
 * Modify an 'unseen' grid appearance based on  'projectability'
 */
void modify_grid_unseen_project(byte *a, char *c)
{
	/* Handle "blind" first */
	if (p_ptr->blind)
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark gray" */
			*a = dark_attr[dark_attr[*a]];
		}
	}

	/* Handle "dark" grids */
	else
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark tile" */
			*a = dark_attr[*a];
		}
	}
}

/*
 * Modify an 'interesting' grid appearance based on 'projectability'
 */
void modify_grid_interesting_project(byte *a, char *c, int y, int x, byte cinfo, byte pinfo)
{
	(void)cinfo;

	/* Handle "blind" and night time*/
	if (p_ptr->blind)
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark gray" */
			*a = dark_attr[dark_attr[*a]];
		}
	}

	/* Handle "fire" grids */
	else if (pinfo & (PLAY_FIRE))
	{
		int i;

		for (i = 0; i < target_path_n; i++)
		{
			int path_y = GRID_Y(target_path_g[i]);
			int path_x = GRID_X(target_path_g[i]);

			/* Hack -- Stop before hitting walls */
			if (!cave_project_bold(y, x)) break;

			/* X, Y on projection path? */
			if ((path_y == y) && (path_x == x))
			{
				/* Mega-hack */
				if (*a & 0x80)
				{
					/* Use a brightly lit tile */
					if (arg_graphics == GRAPHICS_DAVID_GERVAIS)
						*c -= 1;
					else if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
						*c += 2;
				}
				else
				{
					/* Use "yellow" */
					*a = lite_attr[*a];
				}

				/* Important -- exit loop */
				break;
			}
		}

		/* Use normal */
	}

	/* Handle "dark" grids */
	else
	{
		/* Mega-hack */
		if (*a & 0x80)
		{
			if ((arg_graphics != GRAPHICS_ORIGINAL) && (arg_graphics != GRAPHICS_DAVID_GERVAIS_ISO))
			{
				/* Use a dark tile */
				*c += 1;
			}
		}
		else
		{
			/* Use "dark tile" */
			*a = dark_attr[*a];
		}
	}
}


/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel, unless the "scroll_target"
 * option is used, which allows changing the current panel during "look"
 * and "target" commands.  Currently, when "flag" is true, that is, when
 * "interesting" grids are being used, and a directional key is used, we
 * only scroll by a single panel, in the direction requested, and check
 * for any interesting grids on that panel.  The "correct" solution would
 * actually involve scanning a larger set of grids, including ones in
 * panels which are adjacent to the one currently scanned, but this is
 * overkill for this function.  XXX XXX
 *
 * Hack -- targetting/observing an "outer border grid" may induce
 * problems, so this is not currently allowed.
 *
 * The player can use the direction keys to move among "interesting"
 * grids in a heuristic manner, or the "space", "+", and "-" keys to
 * move through the "interesting" grids in a sequential manner, or
 * can enter "location" mode, and use the direction keys to move one
 * grid at a time in any direction.  The "t" (set target) command will
 * only target a monster (as opposed to a location) if the monster is
 * target_able and the "interesting" mode is being used.
 *
 * The current grid is described using the "look" method above, and
 * a new command may be entered at any time, but note that if the
 * "TARGET_LOOK" bit flag is set (or if we are in "location" mode,
 * where "space" has no obvious meaning) then "space" will scan
 * through the description of the current grid until done, instead
 * of immediately jumping to the next "interesting" grid.  This
 * allows the "target" command to retain its old semantics.
 *
 * The "*", "+", and "-" keys may always be used to jump immediately
 * to the next (or previous) interesting grid, in the proper mode.
 *
 * The "return" key may always be used to scan through a complete
 * grid description (forever).
 *
 * This command will cancel any old target, even if used from
 * inside the "look" command.
 */
bool target_set_interactive(int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, d, m, t, bd;

	int y = py;
	int x = px;

	int ty = y;
	int tx = x;

	bool done = FALSE;

	bool flag = TRUE;

	key_event query;

	char info[80];

	int room = -1;

	/* Cancel target */
	target_set_monster(0, 0);


	/* Cancel tracking */
	/* health_track(0); */


	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);
	
	/* Nothing in it. Clear race filter if set and try again. */
	if (!temp_n && (p_ptr->target_race))
	{
		p_ptr->target_race = 0;
		
		target_set_interactive_prepare(mode);
	}

	/* If targetting monsters, display projectable grids */
	if (mode & (TARGET_KILL))
	{
		/* Set path */
		target_path_n = 0;

		/* Use the projection hook functions */
		modify_grid_boring_hook = modify_grid_boring_project;
		modify_grid_unseen_hook = modify_grid_unseen_project;
		modify_grid_interesting_hook = modify_grid_interesting_project;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Hack -- Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Handle stuff */
		handle_stuff();
	}

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			ty = y = temp_y[m];
			tx = x = temp_x[m];

			/* Calculate the path */
			if (mode & (TARGET_KILL))
			{
				target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);

				/* Redraw map */
				p_ptr->redraw |= (PR_MAP);

				/* Hack -- Window stuff */
				p_ptr->window |= (PW_OVERHEAD);

				/* Handle stuff */
				handle_stuff();					
			}

			/* Allow target */
			if ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]))
			{
				my_strcpy(info, "q,t,p,o,a,g,+,-,.,?,<dir>", sizeof (info));
			}

			/* Dis-allow target */
			else
			{
				my_strcpy(info, "q,p,o,a,g,+,-,.,?,<dir>", sizeof (info));
			}

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, &room, mode, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query.key)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				{
					if (++m == temp_n)
					{
						m = 0;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case '-':
				{
					if (m-- == 0)
					{
						m = temp_n - 1;
						if (!expand_list) done = TRUE;
					}
					break;
				}

				case 'p':
				{
					y = py;
					x = px;
					
					/* Calculate the path */
					if (mode & (TARGET_KILL))
					{
						target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);

						/* Redraw map */
						p_ptr->redraw |= (PR_MAP);

						/* Hack -- Window stuff */
						p_ptr->window |= (PW_OVERHEAD);

						/* Handle stuff */
						handle_stuff();					
					}

					if (scroll_target)
					{
						/* Recenter around player */
						verify_panel();

						/* Handle stuff */
						handle_stuff();
					}
				}

				case 'o':
				{
					flag = FALSE;
					break;
				}

				case 'm':
				{
					break;
				}

				case '!':
				case '\xff':
				{
					/* Bounds check */
					if (in_bounds(KEY_GRID_Y(query), KEY_GRID_X(query)))
					{	
						ty = y = KEY_GRID_Y(query);
						tx = x = KEY_GRID_X(query);
	
						/* Go to if clicked with BUTTON_MOVE */
						if (query.mousebutton == BUTTON_MOVE)
						{
							do_cmd_pathfind(y,x);
							done = TRUE;
						}
						/* Set target if clicked. ! is mainly for macro use. */
						else if ((query.mousebutton == BUTTON_AIM) || (query.key == '!'))
						{
							target_set_location(y, x, mode);
							done = TRUE;
						}
						else
						{
							flag = FALSE;
	
							/* Calculate the path */
							if (mode & (TARGET_KILL))
							{
								target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);
	
								/* Redraw map */
								p_ptr->redraw |= (PR_MAP);
	
								/* Hack -- Window stuff */
								p_ptr->window |= (PW_OVERHEAD);
	
								/* Handle stuff */
								handle_stuff();	
	
								/* Force an update */
								Term_fresh();
							}
						}
					}
					break;
				}

				case 'g':
				{
					do_cmd_pathfind(y,x);
					done = TRUE;
					break;
				}

				case '?':
				{
					screen_save();
					
					/* Help file depends on mode */
					if (mode & (TARGET_KILL))
					{
						(void)show_file("cmdkill.txt", NULL, 0, 0);
					}
					else
					{
						(void)show_file("cmdlook.txt", NULL, 0, 0);
					}
					
					screen_load();
					break;
				}
				
				/* Get allies to attack anything near here */
				case 'a':
				{
					target_set_location(y, x, mode | (TARGET_NEAR));
					done = TRUE;
					break;
				}

				case '.':
				case 't':
				case '5':
				case '0':
				{
					int m_idx = cave_m_idx[y][x];

					if ((m_idx > 0) && target_able(m_idx))
					{
						health_track(m_idx);
						target_set_monster(m_idx, mode);
						done = TRUE;
					}
					else
					{
						bell("Illegal target!");
					}
					break;
				}
				
				/* Have set or changed race filter */
				case 's':
				{
					/* Forget */
					temp_n = 0;

					/* Re-prepare targets */
					target_set_interactive_prepare(mode);
					
					/* Start near the player again and fake movement */
					m = 0;
					d = 5;
					
					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query.key);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				int old_y = temp_y[m];
				int old_x = temp_x[m];

				/* Find a new monster */
				i = target_pick(old_y, old_x, ddy[d], ddx[d]);

				/* Scroll to find interesting grid */
				if (scroll_target && (i < 0))
				{
					int old_wy = p_ptr->wy;
					int old_wx = p_ptr->wx;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(old_wy, old_wx))
						{

							/* Recalculate interesting grids */
							target_set_interactive_prepare(mode);
						}

						/* Handle stuff */
						handle_stuff();
					}
				}

				/* Use interesting grid if found */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Default prompt */
			my_strcpy(info, "q,t,p,m,+,-,<dir>", sizeof (info));

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, &room, mode | TARGET_LOOK, info);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query.key)
			{
				case ESCAPE:
				case 'q':
				{
					done = TRUE;
					break;
				}

				case ' ':
				case '*':
				case '+':
				case '-':
				{
					break;
				}

				case 'p':
				{
					y = py;
					x = px;

					/* Calculate the path */
					if (mode & (TARGET_KILL))
					{
						target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);

						/* Redraw map */
						p_ptr->redraw |= (PR_MAP);

						/* Hack -- Window stuff */
						p_ptr->window |= (PW_OVERHEAD);

						/* Handle stuff */
						handle_stuff();					
					}

					if (scroll_target)
					{
						/* Recenter around player */
						verify_panel();

						/* Handle stuff */
						handle_stuff();
					}
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					flag = TRUE;

					m = 0;
					bd = 999;

					/* Pick a nearby monster */
					for (i = 0; i < temp_n; i++)
					{
						t = distance(y, x, temp_y[i], temp_x[i]);

						/* Pick closest */
						if (t < bd)
						{
							m = i;
							bd = t;
						}
					}

					/* Nothing interesting */
					if (bd == 999) flag = FALSE;

					break;
				}

				case '!':
				case '\xff':
				{
					/* Bounds check */
					if (in_bounds(KEY_GRID_Y(query), KEY_GRID_X(query)))
					{	
						ty = y = KEY_GRID_Y(query);
						tx = x = KEY_GRID_X(query);

						/* Go to if clicked with BUTTON_MOVE */
						if (query.mousebutton == BUTTON_MOVE)
						{
							do_cmd_pathfind(y,x);
							done = TRUE;
						}
						/* Set target if clicked. ! is mainly for macro use. */
						else if ((query.mousebutton == BUTTON_AIM) || (query.key == '!'))
						{
							target_set_location(y, x, mode);
							done = TRUE;
						}
						else
						{
							/* Calculate the path */
							if (mode & (TARGET_KILL))
							{
								target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);

								/* Redraw map */
								p_ptr->redraw |= (PR_MAP);

								/* Hack -- Window stuff */
								p_ptr->window |= (PW_OVERHEAD);

								/* Handle stuff */
								handle_stuff();

								/* Force an update */
								Term_fresh();			
							}
						}
					}
					break;
				}

				case 'g':
				{
					do_cmd_pathfind(y,x);
					done = TRUE;
					break;
				}

				case '?':
				{
					screen_save();
					(void)show_file("target.txt", NULL, 0, 0);
					screen_load();
					break;
				}
				
				case 'a':
				{
					target_set_location(y, x, mode | (TARGET_NEAR));
					done = TRUE;
					break;
				}

				case '.':
				case 't':
				case '5':
				case '0':
				{
					target_set_location(y, x, mode);
					done = TRUE;
					break;
				}

				/* Have set or changed race filter */
				case 's':
				{
					/* Forget */
					temp_n = 0;

					/* Re-prepare targets */
					target_set_interactive_prepare(mode);
					
					/* Start near the player again and fake movement */
					m = 0;
					d = 5;
					
					break;
				}

				default:
				{
					/* Extract a direction */
					d = target_dir(query.key);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				/* Move */
				x += ddx[d];
				y += ddy[d];

				if (scroll_target)
				{
					/* Slide into legality */
					if (x >= DUNGEON_WID - 1) x--;
					else if (x <= 0) x++;

					/* Slide into legality */
					if (y >= DUNGEON_HGT - 1) y--;
					else if (y <= 0) y++;

					/* Adjust panel if needed */
					if (adjust_panel(y, x))
					{
						/* Handle stuff */
						handle_stuff();

						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);
					}
				}

				else
				{
					/* Slide into legality */
					if (x >= p_ptr->wx + SCREEN_WID) x--;
					else if (x < p_ptr->wx) x++;

					/* Slide into legality */
					if (y >= p_ptr->wy + SCREEN_HGT) y--;
					else if (y < p_ptr->wy) y++;
				}

				ty = y;
				tx = x;

				/* Calculate the path */
				if (mode & (TARGET_KILL))
				{
					target_path_n = project_path(target_path_g, MAX_SIGHT, py, px, &ty, &tx, 0);

					/* Redraw map */
					p_ptr->redraw |= (PR_MAP);

					/* Hack -- Window stuff */
					p_ptr->window |= (PW_OVERHEAD);

					/* Handle stuff */
					handle_stuff();					
				}
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Paranoia - reset the modify_grid functions */
	modify_grid_boring_hook = modify_grid_boring_view;
	modify_grid_unseen_hook = modify_grid_unseen_view;
	modify_grid_interesting_hook = modify_grid_interesting_view;

	if (mode & (TARGET_KILL))
	{
		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Hack -- Window stuff */
		p_ptr->window |= (PW_OVERHEAD);

		/* Handle stuff */
		handle_stuff();
	}

	if (scroll_target)
	{
		/* Recenter around player */
		verify_panel();

		/* Handle stuff */
		handle_stuff();
	}

	/* Failure to set target */
	if (!p_ptr->target_set) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Get an "aiming direction" (1,2,3,4,6,7,8,9 or 5) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * The direction "5" is special, and means "use current target".
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Currently this function applies confusion directly.
 */
bool get_aim_dir(int *dp)
{
	int dir;

	key_event ke;

	cptr p;

#ifdef ALLOW_REPEAT

	if (repeat_pull(dp))
	{
		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Hack -- auto-target if requested */
	if (use_old_target && target_okay()) dir = 5;

	/* Ask until satisfied */
	while (!dir)
	{
		/* Choose a prompt */
		if (!target_okay())
		{
			p = "Direction ('*' to choose a target, Escape to cancel)? ";
		}
		else
		{
			p = "Direction ('5' for target, '*' to re-target, Escape to cancel)? ";
		}

		/* Get a command (or Cancel) */
		if (!get_com_ex(p, &ke)) break;

		/* Analyze */
		switch (ke.key)
		{
			/* Mouse aiming */
			case '\xff':
			{
				if (ke.mousebutton)
				{
					target_set_location(KEY_GRID_Y(ke), KEY_GRID_X(ke), 0);
					dir = 5;
					break;
				}
				else continue;
			}

			/* Set new target, use target if legal */
			case '*':
			{
				if (target_set_interactive(TARGET_KILL)) dir = 5;
				break;
			}

			/* Use current target, if set and legal */
			case 't':
			case '5':
			case '0':
			case '.':
			{
				if (target_okay()) dir = 5;
				break;
			}

			/* Possible direction */
			default:
			{
				dir = target_dir(ke.key);
				break;
			}
		}

		/* Error */
		if (!dir) bell("Illegal aim direction!");
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (p_ptr->command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT

	repeat_push(dir);

#endif /* ALLOW_REPEAT */

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user.
 *
 * Return TRUE if a direction was chosen, otherwise return FALSE.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
	int dir;

	key_event ke;

	cptr p;

#ifdef ALLOW_REPEAT

	if (repeat_pull(dp))
	{
		return (TRUE);
	}

#endif /* ALLOW_REPEAT */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		/* Choose a prompt */
		p = "Direction (Escape to cancel)? ";

		/* Get a command (or Cancel) */
		if (!get_com_ex(p, &ke)) break;

		/* Check mouse coordinates */
		if (ke.key == '\xff')
		{
			if (ke.mousebutton)
			{
				int y = KEY_GRID_Y(ke);
				int x = KEY_GRID_X(ke);

				/* Calculate approximate angle */
				int angle = get_angle_to_target(p_ptr->py, p_ptr->px,y, x, 0);

				/* Convert angle to direction */
				if (angle < 15) dir = 6;
				else if (angle < 33) dir = 9;
				else if (angle < 59) dir = 8;
				else if (angle < 78) dir = 7;
				else if (angle < 104) dir = 4;
				else if (angle < 123) dir = 1;
				else if (angle < 149) dir = 2;
				else if (angle < 168) dir = 3;
				else dir = 6;
			}
			else continue;
		}

		/* Convert keypress into a direction */
		else dir = target_dir(ke.key);

		/* Oops */
		if (!dir) bell("Illegal repeatable direction!");
	}

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT

	repeat_push(dir);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}


/*
 * Apply confusion, if needed, to a direction
 *
 * Display a message and return TRUE if direction changes.
 */
bool confuse_dir(int *dp)
{
	int dir;

	/* Default */
	dir = (*dp);

	/* Apply "confusion" */
	if ((p_ptr->confused) && (p_ptr->confused > rand_int(33)))
	{
		/* Apply confusion XXX XXX XXX */
		if ((dir == 5) || (rand_int(100) < 75))
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
		}
	}

	/* Notice confusion */
	if ((*dp) != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");

		/* Save direction */
		(*dp) = dir;

		/* Confused */
		return (TRUE);
	}

	/* Not confused */
	return (FALSE);
}

int min_depth(int dungeon)
{
	town_type *t_ptr=&t_info[dungeon];

	return (t_ptr->zone[0].level);
}

int max_depth(int dungeon)
{
	town_type *t_ptr=&t_info[dungeon];
	dungeon_zone *zone = &t_ptr->zone[0];
	int i;

	/* Get the zone */	
	for (i = 0;i<MAX_DUNGEON_ZONES;i++)
	{
		if ((i) && (t_ptr->zone[i].level <= t_ptr->zone[i-1].level)) break;

		zone = &t_info[dungeon].zone[i];
	}

	return (zone->level);
}


int town_depth(int dungeon)
{
	town_type *t_ptr=&t_info[dungeon];
	dungeon_zone *zone = &t_ptr->zone[0];
	int i;

	/* Get the zone */	
	for (i = 0;i<MAX_DUNGEON_ZONES;i++)
	{
		zone = &t_info[dungeon].zone[i];
		if (!zone->fill) break;
	}

	return (zone->level);
}


void get_zone(dungeon_zone **zone_handle, int dungeon, int depth)
{
	town_type *t_ptr = &t_info[dungeon];
	dungeon_zone *zone = &t_ptr->zone[0];
	int i;

	/* Get the zone */	
	for (i = 0;i<MAX_DUNGEON_ZONES;i++)
	{
		if ((i) && (t_ptr->zone[i].level <= t_ptr->zone[i-1].level)) break;

		if (t_ptr->zone[i].level > depth) break;

		zone = &t_ptr->zone[i];
	}

	*zone_handle = zone;

}
