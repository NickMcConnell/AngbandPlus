/* File: xtra2.c */

/* Handlers for most of the player's temporary attributes, resistances,
 * nutrition, experience.  Monsters that drop a specific treasure, monster
 * death and subsequent events, screen scrolling, monster health descrip-
 * tions.  Sorting, targetting, what and how squares appear when looked at,
 * prompting for a direction to aim at and move in.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
			msg_print("You are blind!");
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
	if (!v)
	{
		if (p_ptr->poisoned)
		{
			msg_print("You are no longer poisoned.");
			notice = TRUE;
		}
	}

	/* Shut */
	else if (!p_ptr->poisoned)
	{
		msg_print("You are poisoned!");
		notice = TRUE;
	}

	/* Increasing */
	else if (v > p_ptr->poisoned)
	{
		msg_print("You are more poisoned.");
		notice = TRUE;
	}

	/* Decreasing */
	else if (v <= p_ptr->poisoned / 2)
	{
		msg_print("You are less poisoned.");
		notice = TRUE;
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

	/* Use the value */
	p_ptr->afraid = v;

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
		if (p_ptr->paralyzed)
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
			image_count = randint(511);
			msg_print("You feel drugged!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
			image_count = randint(0);
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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->magicdef", notice observable changes
 */
bool set_extra_defences(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->magicdef)
		{
			msg_print("You feel your magical vulnerablity diminish.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->magicdef)
		{
			msg_print("Your magical defences fall to their normal values.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->magicdef = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->tim_invis" affects monster visibility.
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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_esp", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->tim_esp" affects monster visibility.
 */
bool set_tim_esp(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_esp)
		{
			msg_print("You feel your conciousness expand!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_esp)
		{
			msg_print("You feel your conciousness contract.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_esp = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters XXX */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}



/*
 * Set "p_ptr->superstealth", notice observable changes
 */
bool set_superstealth(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->superstealth)
		{
			msg_print("You are mantled in shadow from ordinary eyes!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->superstealth)
		{
			msg_print("You are exposed to common sight once more.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->superstealth = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Hack - Vampires don't resist fire */
	if (p_ptr->schange == SHAPE_VAMPIRE) v = 0;

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

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;

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

	/* Increase stun */
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
				msg_print("You have been knocked out!");
				break;
			}
		}

		/* Notice */
		notice = TRUE;
	}

	/* Decrease stun */
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

	/* Disturb, except if going from full to normal. */
	if ((disturb_state) && (new_aux != 3)) disturb(0, 0);

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
 * Set "p_ptr->word_recall", notice observable changes
 */
void word_recall(int v)
{
	if (!p_ptr->word_recall)
	{
		(void)set_recall(v);
	}
	else
	{
		(void)set_recall(0);
	}
}


/*
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	int i;


	/* Note current level */
	i = p_ptr->lev;


	/* Hack -- lower limit */
	if (p_ptr->exp < 0) p_ptr->exp = 0;

	/* Hack -- lower limit */
	if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;

	/* Hack -- upper limit */
	if (p_ptr->exp > PY_MAX_EXP) p_ptr->exp = PY_MAX_EXP;

	/* Hack -- upper limit */
	if (p_ptr->max_exp > PY_MAX_EXP) p_ptr->max_exp = PY_MAX_EXP;


	/* Hack -- maintain "max" experience */
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

	/* Redraw experience */
	p_ptr->redraw |= (PR_EXP);

	/* Handle stuff */
	handle_stuff();


	/* Lose levels while possible */
	while ((p_ptr->lev > 1) &&
	       (p_ptr->exp < (player_exp[p_ptr->lev-2])))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPECIALTY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_EXP | PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= (player_exp[p_ptr->lev-1])))
	{
		/* Gain a level */
		p_ptr->lev++;

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_lev) p_ptr->max_lev = p_ptr->lev;

		/* Sound */
		sound(SOUND_LEVEL);

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPECIALTY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_EXP | PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain max levels while possible
	 * Called rarely - only when leveling while experience is drained.  */
	while ((p_ptr->max_lev < PY_MAX_LEVEL) &&
	       (p_ptr->max_exp >= (player_exp[p_ptr->max_lev-1])))
	{
		/* Gain max level */
		p_ptr->max_lev++;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPECIALTY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

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
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
static int get_coin_type(monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze "coin" monsters */
	if (r_ptr->d_char == '$')
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (lookup_kind(TV_GOLD, SV_COPPER));
		if (strstr(name, " silver ")) return (lookup_kind(TV_GOLD, SV_SILVER));
		if (strstr(name, " gold ")) return (lookup_kind(TV_GOLD, SV_GOLD));
		if (strstr(name, " mithril "))
			return (lookup_kind(TV_GOLD, SV_MITHRIL));
		if (strstr(name, " adamantite "))
			return (lookup_kind(TV_GOLD, SV_ADAMANTITE));

		/* Look for textual clues */
		if (strstr(name, "Copper ")) return (lookup_kind(TV_GOLD, SV_COPPER));
		if (strstr(name, "Silver ")) return (lookup_kind(TV_GOLD, SV_SILVER));
		if (strstr(name, "Gold ")) return (lookup_kind(TV_GOLD, SV_GOLD));
		if (strstr(name, "Mithril "))
			return (lookup_kind(TV_GOLD, SV_MITHRIL));
		if (strstr(name, "Adamantite "))
			return (lookup_kind(TV_GOLD, SV_ADAMANTITE));
	}

	/* Assume nothing */
	return (0);
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
	int total = 0;

	int level_boost = 0;

	int deeper_chance = 0;
	int gold_chance = 0;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));
	bool do_chest = (r_ptr->flags1 & (RF1_DROP_CHEST));

	int force_coin = get_coin_type(r_ptr);

	object_type *i_ptr;
	object_type object_type_body;

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
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
	}


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_40)) && (rand_int(100) < 40)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if (r_ptr->flags1 & (RF1_DROP_ONE)) number += 1;
	if (r_ptr->flags1 & (RF1_DROP_TWO)) number += 2;

	/* Decode level boost */
	level_boost += (r_ptr->flags1 & (RF1_DROP_PLUS_5)) ? 5 : 0;
	level_boost += (r_ptr->flags1 & (RF1_DROP_PLUS_10)) ? 10 : 0;

	/*
	 * Mega-Hack - the Living Dungeon hack
	 * Creatures with enhanced drop depth and only gold,
	 * actually get more drops as well.
	 */
	if (!do_item)
	{
		number += (r_ptr->flags1 & (RF1_DROP_PLUS_5)) ? 3 : 0;
		number += (r_ptr->flags1 & (RF1_DROP_PLUS_10)) ? 6 : 0;
	}

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/*
	 * Hack -- Low level creatures may drop better than expected.
	 * Compensates for increased chance of gold drops.
	 */
	if (p_ptr->depth > r_ptr->level)
	{
		/* May drop better */
		deeper_chance = MIN(((p_ptr->depth - r_ptr->level) * 2), 40);
	}

	/* Normally Average dungeon and monster levels */
	if (!(rand_int(100) < deeper_chance)) object_level = ((p_ptr->depth + r_ptr->level) / 2);

	/* Otherwise just base from dungeon level */
	else object_level = p_ptr->depth;

	/* Boost for criters with higher quality drops */
	object_level += level_boost;

	/* Chance of gold */
	/* 100% if only gold */
	if (!do_item) gold_chance = 100;

	/* 0% if only items */
	else if (!do_gold) gold_chance = 0;

	/* 30% Normally */
	else gold_chance = 30;

	/* Hack -- Low-level monsters drop gold much more often */
	if (!great && !do_chest)
	{
		/* Increase gold chance */
		if (MIN(85, p_ptr->depth) > r_ptr->level + 5)
			gold_chance += 3 * (MIN(85, p_ptr->depth) - r_ptr->level - 5);

		/* Always have some chance of generating items */
		if (do_item && (gold_chance > 90)) gold_chance = 90;
	}

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make gold when allowed */
		if (rand_int(100) < gold_chance)
		{

			/* Make some gold */
			if (make_gold(i_ptr))
			{

				/* Assume seen XXX XXX XXX */
				dump_gold++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
		}

		/* Make chest. */
		else if (do_chest)
		{
			required_tval = TV_CHEST;
			if (make_object(i_ptr, FALSE, FALSE, TRUE))
			{

				/* Assume seen XXX XXX XXX */
				dump_item++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
			required_tval = 0;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (make_object(i_ptr, good, great, FALSE))
			{

				/* Assume seen XXX XXX XXX */
				dump_item++;

				/* Drop it in the dungeon */
				drop_near(i_ptr, -1, y, x);
			}
		}

	}

	/* Reset the object level */
	object_level = p_ptr->depth;

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}


	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & (RF1_QUESTOR))) return;


	/* Hack -- Mark quests as complete */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		/* Hack -- note completed quests */
		if (q_list[i].level == r_ptr->level) q_list[i].level = 0;

		/* Count incomplete quests */
		if (q_list[i].level) total++;
	}


	/* Need some stairs */
	if (total)
	{
		/* Stagger around */
		while (!cave_valid_bold(y, x))
		{
			int d = 1;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

			/* Stagger */
			y = ny; x = nx;
		}

		/* Destroy any objects */
		delete_object(y, x);

		/* Explain the staircase */
		msg_print("A magical staircase appears...");

		/* Create stairs down */
		cave_set_feat(y, x, FEAT_MORE);

		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}


	/* Nothing left, game over... */
	else
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
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac;

	char	path[1024];

	/* Hack - Monsters in stasis are invulnerable. */
	if (m_ptr->stasis) return (FALSE);

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Always go active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/* Black Breath causes minor damage amplification */
	if (m_ptr->black_breath)
	{
		dam += dam / 20;
	}

	/* Hack - Cancel any special player stealth magics. */
	if (p_ptr->superstealth)
	{
		set_superstealth(0);
	}

	/* Complex message. Moved from melee and archery, now allows spell and
	 * magical items damage to be debugged by wizards.  -LM-
	 */
	if (p_ptr->wizard)
	{
		msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
	}

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Make a sound */
		sound(SOUND_KILL);

		/* Specialty Ability SOUL_SIPHON */
		if ((check_ability(SP_SOUL_SIPHON)) &&
		    (p_ptr->csp < p_ptr->msp) &&
		    (!((r_ptr->flags3 & (RF3_DEMON)) ||
		       (r_ptr->flags3 & (RF3_UNDEAD)) ||
		       (r_ptr->flags2 & (RF2_STUPID)))))
		{
			p_ptr->mana_gain += 2 + (m_ptr->maxhp / 30);
		}

		/* Increase the noise level slightly. */
		if (add_wakeup_chance <= 8000) add_wakeup_chance += 500;

		/* Death by Missile/Spell attack */
		if (note)
		{
			message_format(MSG_KILL, p_ptr->lev, "%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(MSG_KILL, p_ptr->lev, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if ((r_ptr->flags3 & (RF3_DEMON)) ||
			 (r_ptr->flags3 & (RF3_UNDEAD)) ||
			 (r_ptr->flags2 & (RF2_STUPID)) ||
			 (strchr("Evg", r_ptr->d_char)))
		{
			message_format(MSG_KILL, p_ptr->lev, "You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			message_format(MSG_KILL, p_ptr->lev, "You have slain %s.", m_name);
		}

		/* Maximum player level */
		div = p_ptr->max_lev;

		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

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

		/* Generate treasure */
		monster_death(m_idx);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num--;

		/* When the player kills a player ghost, the bones file that
		 * it used is (often) deleted.
		 */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			if (randint(3) != 1)
			{
				sprintf(path, "%s/bone.%03d", ANGBAND_DIR_BONE,
					bones_selector);
				remove(path);
			}
		}

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			/* Count kills this life */
			if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

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
			m_ptr->monfear = 0;

			/* Flag minimum range for recalculation */
			m_ptr->min_range = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((dam > 0) && ((randint(10) >= percentage) ||
		    ((dam >= m_ptr->hp) && (rand_int(100) < 80))))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
					  (((dam >= m_ptr->hp) && (percentage > 7)) ?
					   20 : ((11 - percentage) * 5)));

			/* Flag minimum range for recalculation */
			m_ptr->min_range = 0;
		}
	}

	/* Recalculate desired minimum range */
	if (dam > 0) m_ptr->min_range = 0;

	/* Not dead yet */
	return (FALSE);
}


/*
 * Calculates current boundaries
 */
void panel_recalc_bounds(void)
{
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	panel_row_max = panel_row_min + hgt - ROW_MAP - 2;
	panel_row_prt = panel_row_min - 1;
	panel_col_max = panel_col_min + wid - COL_MAP - 2;
	panel_col_prt = panel_col_min - 13;

	if (hgt > 25)
	{
		panel_row_max -= 2;
		panel_extra_rows = TRUE;
	}
	else panel_extra_rows = FALSE;
}


/*
 * Handle a request to change the current panel
 *
 * Return TRUE if the panel was changed.
 *
 * Also used in do_cmd_locate
 */
bool change_panel(int dy, int dx)
{
	int y, x;
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Offset */
	hgt -= ROW_MAP + 1;
	wid -= COL_MAP + 1;

	if (panel_extra_rows) hgt -= 2;

	/* Apply the motion */
	y = panel_row_min + dy * (hgt / 2);
	x = panel_col_min + dx * (wid / 2);

	/* Bounds */
	if (y > DUNGEON_HGT - hgt) y = DUNGEON_HGT - hgt;
	if (y < 0) y = 0;
	if (x > DUNGEON_WID - wid) x = DUNGEON_WID - wid;
	if (x < 0) x = 0;

	if (!p_ptr->depth)
	{
		x = 65 - wid / 2;
		y = 75 - hgt / 2;
	}

	/* Handle "changes" */
	if ((y != panel_row_min) || (x != panel_col_min))
	{
		/* Save the new panel info */
		panel_row_min = y;
		panel_col_min = x;

		/* Recalculate the boundaries */
		panel_recalc_bounds();

		/* Update stuff */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Handle stuff */
		handle_stuff();

		/* Success */
		return (TRUE);
	}

	/* No change */
	return (FALSE);
}


void verify_panel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y = py;
	int x = px;

	int wid, hgt;

	int prow_min;
	int pcol_min;

	int max_prow_min;
	int max_pcol_min;


	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Offset */
	hgt -= ROW_MAP + 1;
	wid -= COL_MAP + 1;

	if (panel_extra_rows) hgt -= 2;

	/* Hack - in town  */
	if (!p_ptr->depth)
	{
		prow_min = DUNGEON_HGT/3;
		pcol_min = DUNGEON_WID/3;
	}

	else
	{
		max_prow_min = DUNGEON_HGT - hgt;
		max_pcol_min = DUNGEON_WID - wid;

		/* Bounds checking */
		if (max_prow_min < 0) max_prow_min = 0;
		if (max_pcol_min < 0) max_pcol_min = 0;

		/* Center on player */
		if (center_player && (center_running || !p_ptr->running))
		{
			/* Center vertically */
			prow_min = y - hgt / 2;
			if (prow_min < 0) prow_min = 0;
			else if (prow_min > max_prow_min) prow_min = max_prow_min;

			/* Center horizontally */
			pcol_min = x - wid / 2;
			if (pcol_min < 0) pcol_min = 0;
			else if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
		}
		else
		{
			prow_min = panel_row_min;
			pcol_min = panel_col_min;

			/* Make certain the initial values are valid. */
			if (prow_min > max_prow_min) prow_min = max_prow_min;
			if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;

			/* Scroll screen when 2 grids from top/bottom edge */
			if (y > panel_row_max - 2)
			{
				while (y > prow_min + hgt - 2)
				{
					prow_min += (hgt / 2);
				}

				if (prow_min > max_prow_min) prow_min = max_prow_min;
			}

			if (y < panel_row_min + 2)
			{
				while (y < prow_min + 2)
				{
					prow_min -= (hgt / 2);
				}

				if (prow_min < 0) prow_min = 0;
			}

			/* Scroll screen when 4 grids from left/right edge */
			if (x > panel_col_max - 4)
			{
				while (x > pcol_min + wid - 4)
				{
					pcol_min += (wid / 2);
				}

				if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
			}

			if (x < panel_col_min + 4)
			{
				while (x < pcol_min + 4)
				{
					pcol_min -= (wid / 2);
				}

				if (pcol_min < 0) pcol_min = 0;
			}
		}

	}

	/* Check for "no change" */
	if ((prow_min == panel_row_min) && (pcol_min == panel_col_min)) return;

	/* Save the new panel info */
	panel_row_min = prow_min;
	panel_col_min = pcol_min;

	/* Hack -- optional disturb on "panel change" */
	if (disturb_panel && !center_player) disturb(0, 0);

	/* Recalculate the boundaries */
	panel_recalc_bounds();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}

/*
 * Center the dungeon display around the player
 */

void panel_center(void)
{
	int wid, hgt;
	int new_panel_row, new_panel_col;

	/* Hack - in town - do not move the screen */
	if (!p_ptr->depth)
	{
		(void)change_panel(0, 0);
		return;
	}

	/* Get the screen size */
	Term_get_size(&wid, &hgt);

	/* Calculate the dimensions of the displayed map */
	hgt -= ROW_MAP + 1;
	wid -= COL_MAP + 1;

	if (panel_extra_rows) hgt -= 2;

	/* Center the map around the player */
	new_panel_row = p_ptr->py - (hgt / 2);
	new_panel_col = p_ptr->px - (wid / 2);

	/* Move the map so that it only shows the dungeon */
	if (new_panel_row + hgt > DUNGEON_HGT) new_panel_row = DUNGEON_HGT - hgt;
	if (new_panel_col + wid > DUNGEON_WID) new_panel_col = DUNGEON_WID - wid;

	/* Verify the lower panel bounds */
	if (new_panel_row < 0) new_panel_row = 0;
	if (new_panel_col < 0) new_panel_col = 0;

	/* Check for "no change" */
	if ((new_panel_row == panel_row_min) && (new_panel_col ==
		panel_col_min)) return;

	/* Save the new panel info */
	panel_row_min = new_panel_row;
	panel_col_min = new_panel_col;

	/* Hack -- optional disturb on "panel change" */
	if (disturb_panel && !center_player) disturb(0, 0);

	/* Recalculate the boundaries */
	panel_recalc_bounds();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}


/*
 * Monster health description
 */
cptr look_mon_desc(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool living = TRUE;
	int perc;


	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags3 & (RF3_UNDEAD)) living = FALSE;
	if (r_ptr->flags3 & (RF3_DEMON)) living = FALSE;
	if (strchr("Egv", r_ptr->d_char)) living = FALSE;


	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		return (living ? "unhurt" : "undamaged");
	}


	/* Calculate a health "percentage" */
	perc = 100L * m_ptr->hp / m_ptr->maxhp;

	if (perc >= 60)
	{
		return (living ? "somewhat wounded" : "somewhat damaged");
	}

	if (perc >= 25)
	{
		return (living ? "wounded" : "damaged");
	}

	if (perc >= 10)
	{
		return (living ? "badly wounded" : "badly damaged");
	}

	return (living ? "almost dead" : "almost destroyed");
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
 * Extract a direction (or zero) from a character
 */
sint target_dir(char ch)
{
	int d = 0;

	int mode;

	cptr act;

	cptr s;


	/* Already a direction? */
	if (isdigit(ch))
	{
		/* Use that */
		d = D2I(ch);
	}

	/* Look up keymap */
	else
	{
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

			/* Acquire monster location */
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
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx)
{
	/* Acceptable target */
	if ((m_idx > 0) && target_able(m_idx))
	{
		monster_type *m_ptr = &m_list[m_idx];

		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = m_idx;
		p_ptr->target_row = m_ptr->fy;
		p_ptr->target_col = m_ptr->fx;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
		p_ptr->target_who = 0;
		p_ptr->target_row = 0;
		p_ptr->target_col = 0;
	}
}


/*
 * Set the target to a location
 */
void target_set_location(int y, int x)
{
	/* Legal target */
	if (in_bounds_fully(y, x))
	{
		/* Save target info */
		p_ptr->target_set = TRUE;
		p_ptr->target_who = 0;
		p_ptr->target_row = y;
		p_ptr->target_col = x;
	}

	/* Clear target */
	else
	{
		/* Reset target info */
		p_ptr->target_set = FALSE;
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
static bool target_set_interactive_accept(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Bounds */
	if (!(in_bounds(y, x))) return (FALSE);

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

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	if (cave_info[y][x] & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (cave_feat[y][x] == FEAT_GLYPH) return (TRUE);

		/* Notice doors */
		if (cave_feat[y][x] == FEAT_OPEN) return (TRUE);
		if (cave_feat[y][x] == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (cave_feat[y][x] == FEAT_LESS) return (TRUE);
		if (cave_feat[y][x] == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice traps */
		if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_TRAP_TAIL)) return (TRUE);

		/* Notice doors */
		if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
		    (cave_feat[y][x] <= FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice rubble */
		if (cave_feat[y][x] == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (cave_feat[y][x] == FEAT_MAGMA_K) return (TRUE);
		if (cave_feat[y][x] == FEAT_QUARTZ_K) return (TRUE);
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
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x)) continue;

			/* Special mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) continue;

				/* Must be a targettable monster */
				if (!target_able(cave_m_idx[y][x])) continue;
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
 * This function uses Tim Baker's code to look at objects.
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_interactive_aux(int y, int x, int mode, cptr info)
{
	s16b this_o_idx, next_o_idx = 0;

	cptr s1, s2, s3;

	bool boring;

	int feat;

	int query;

	char out_val[160];

	int floor_list[24], floor_num;


	/* Repeat forever */
	while (1)
	{
		/* Paranoia */
		query = ' ';

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
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\r') && (query != '\n')) break;

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

				/* Get the monster name ("a kobold") */
				monster_desc(m_name, m_ptr, 0x08);

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
						screen_roff(m_ptr->r_idx);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						/* Describe, and prompt for recall */
						sprintf(out_val, "%s%s%s%s (%s) [r,%s]",
							s1, s2, s3, m_name, look_mon_desc(cave_m_idx[y][x]), info);
						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r') break;

					/* Toggle recall */
					recall = !recall;
				}

				/* Always stop at "normal" keys */
				if ((query != '\r') && (query != '\n') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

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
					char o_name[120];

					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Obtain an object description */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Describe the object */
					sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Always stop at "normal" keys */
					if ((query != '\r') && (query != '\n') && (query != ' ')) break;

					/* Sometimes stop at "space" key */
					if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

					/* Change the intro */
					s2 = "also carrying ";
				}

				/* Double break */
				if (this_o_idx) break;

				/* Use a preposition */
				s2 = "on ";
			}
		}


		/* Scan all objects in the grid */
		if (scan_floor(floor_list, &floor_num, y, x, 0x02))
		{
			/* Not boring */
			boring = FALSE;

			while (1)
			{
				if (floor_num == 1)
				{
					char o_name[120];

					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[floor_list[0]];

					/* Describe the object */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Message */
					sprintf(out_val, "%s%s%s%s [%s]",
						s1, s2, s3, o_name, info);
				}
				else
				{
					/* Message */
					if (rogue_like_commands) sprintf(out_val,
						"%s%s%sa pile of %d items [x,%s]",
						s1, s2, s3, floor_num, info);
					else sprintf(out_val,
						"%s%s%sa pile of %d items [l,%s]",
						s1, s2, s3, floor_num, info);
				}

				prt(out_val, 0, 0);
				move_cursor_relative(y, x);

				/* Command */
				query = inkey();

				/* Display list of items (query == "el", not "won") */
				if ((floor_num > 1) &&
					((rogue_like_commands ? (query == 'x') : (query == 'l')) ||
					(query == ' ') || (query == '*') || (query == '?')))

				{
					/* Save screen */
					screen_save();

					/* Display */
					show_floor(floor_list, floor_num, TRUE);

					/* Prompt */
					prt("Hit any key to continue", 0, 0);

					/* Wait */
					(void) inkey();

					/* Load screen */
					screen_load();
				}
				else
				{
					/* Stop */
					break;
				}
			}

			/* Stop */
			break;
		}


		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Describe it */
			if (o_ptr->marked)
			{
				char o_name[120];

				/* Not boring */
				boring = FALSE;

				/* Obtain an object description */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Describe the object */
				sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, o_name, info);
				prt(out_val, 0, 0);
				move_cursor_relative(y, x);
				query = inkey();

				/* Always stop at "normal" keys */
				if ((query != '\r') && (query != '\n') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

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
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat > FEAT_INVIS))
		{
			cptr name = f_name + f_info[feat].name;

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* Pick a prefix */
			if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special treatment for certain terrain features. */
			if ((feat == FEAT_WATER) || (feat == FEAT_LAVA) || (feat == FEAT_TREE))
			{
				s3 = "";
			}

			/* Hack -- special introduction for store doors */
			if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
			{
				s3 = "the entrance to the ";
			}

			/* Display a message */
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, info);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Always stop at "normal" keys */
			if ((query != '\r') && (query != '\n') && (query != ' ')) break;
		}

		/* Stop on everything but "return" */
		if ((query != '\r') && (query != '\n')) break;
	}

	/* Keep going */
	return (query);
}




/*
 * Draw a visible path over the squares between (x1,y1) and (x2,y2).
 * The path consists of "*", which are white except where there is a
 * monster, object or feature in the grid.
 *
 * This routine has (at least) three weaknesses:
 * - remembered objects/walls which are no longer present are not shown,
 * - squares which (e.g.) the player has walked through in the dark are
 *   treated as unknown space.
 * - walls which appear strange due to hallucination aren't treated correctly.
 *
 * The first two result from information being lost from the dungeon arrays,
 * which requires changes elsewhere
 *
 * From NPPangband
 */
static int draw_path(u16b *path, char *c, byte *a,
    int y1, int x1, int y2, int x2)
{
	int i;
	int max;
	bool on_screen;

	/* Find the path. */
	max = project_path(path, MAX_RANGE, y1, x1, y2, x2, PROJECT_THRU);

	/* No path, so do nothing. */
	if (max < 1) return 0;

	/* The starting square is never drawn, but notice if it is being
	 * displayed. In theory, it could be the last such square.
	 */
	on_screen = panel_contains(y1, x1);

	/* Draw the path. */
	for (i = 0; i < max; i++)
	{
		byte colour;

		/* Find the co-ordinates on the level. */
		int y = GRID_Y(path[i]);
		int x = GRID_X(path[i]);
		/*
		 * As path[] is a straight line and the screen is oblong,
		 * there is only section of path[] on-screen.
		 * If the square being drawn is visible, this is part of it.
		 * If none of it has been drawn, continue until some of it
		 * is found or the last square is reached.
		 * If some of it has been drawn, finish now as there are no
		 * more visible squares to draw.
		 *
		 */
		if (panel_contains(y,x)) on_screen = TRUE;
		else if (on_screen) break;
		else continue;

	 	/* Find the position on-screen */
		move_cursor_relative(y,x);

		/* This square is being overwritten, so save the original. */
		Term_what(Term->scr->cx, Term->scr->cy, a+i, c+i);

		/* Choose a colour. */
		/* Visible monsters are red. */
		if (cave_m_idx[y][x] && m_list[cave_m_idx[y][x]].ml)
		{
			colour = TERM_L_RED;
		}

		/* Known objects are yellow. */
		else if (cave_o_idx[y][x] && o_list[cave_o_idx[y][x]].marked)
		{
			colour = TERM_YELLOW;
		}

		/* Known walls are blue. */
		else if (!cave_floor_bold(y,x) && (cave_info[y][x] & (CAVE_MARK) ||
					player_can_see_bold(y,x)))
		{
			colour = TERM_BLUE;
		}
		/* Unknown squares are grey. */
		else if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			colour = TERM_L_DARK;
		}
		/* Unoccupied squares are white. */
		else
		{
			colour = TERM_WHITE;
		}

		/* Draw the path segment */
		(void)Term_addch(colour, '*');
	}
	return i;
}

/*
 * Load the attr/char at each point along "path" which is on screen from
 * "a" and "c". This was saved in draw_path().
 *
 * From NPPangband
 */
static void load_path(int max, u16b *path, char *c, byte *a)
{
	int i;
	for (i = 0; i < max; i++)
	{
		if (!panel_contains(GRID_Y(path[i]), GRID_X(path[i]))) continue;

		move_cursor_relative(GRID_Y(path[i]), GRID_X(path[i]));

		(void)Term_addch(a[i], c[i]);
	}

	Term_fresh();
}

/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * All locations must be on the current panel.  XXX XXX XXX
 *
 * Perhaps consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require dynamic updating
 * of the "temp" grid set.  XXX XXX XXX
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

	int i, d, m;

	int y = py;
	int x = px;

	bool done = FALSE;

	bool flag = TRUE;

	char query;

	char info[80];

	/* These are used for displaying the path to the target */
	u16b path[MAX_RANGE];
	char path_char[MAX_RANGE];
	byte path_attr[MAX_RANGE];
	int max = 0;

	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);
	if (panel_extra_rows) hgt -= 2;

	/* Cancel target */
	target_set_monster(0);

	/* Cancel tracking */
	/* health_track(0); */

	/*
	 * Hack -- Start out by selecting any grid by using the TARGET_GRID
	 * flag. I do this so dimen_door() is a bit nicer. -TNB-
	 */
	if (mode & TARGET_GRID)
	{
		flag = FALSE;
		mode &= ~TARGET_GRID;
	}

	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			/* Allow target */
			if ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]))
			{
				strcpy(info, "q,t,p,o,+,-,<dir>");
			}

			/* Dis-allow target */
			else
			{
				strcpy(info, "q,p,o,+,-,<dir>");
			}

			/* Draw the path in "target" mode. If there is one */
			if (mode & (TARGET_KILL))
				max = draw_path (path, path_char, path_attr, py, px, y, x);

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info);

			/* Remove the path */
			if (max > 0) load_path (max, path, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no "direction" */
			d = 0;

			/* Analyze */
			switch (query)
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
				}

				case 'o':
				{
					flag = !flag;
					break;
				}

				case 'm':
				{
					break;
				}

				case 't':
				case '5':
				case '0':
				case '.':
				{
					int m_idx = cave_m_idx[y][x];

					if ((m_idx > 0) && target_able(m_idx))
					{
						health_track(m_idx);
						target_set_monster(m_idx);
						done = TRUE;
					}
					else
					{
						bell("Illegal target!");
					}
					break;
				}

				default:
				{
					/* Extract direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				/* Modified to scroll to monster */
				int y2 = panel_row_min;
				int x2 = panel_col_min;

				/* Find a new monster */
				i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

				/* Request to target past last interesting grid */
				while (i < 0)
				{
					/* Note the change */
					if (change_panel(ddy[d], ddx[d]))
					{
						int v = temp_y[m];
						int u = temp_x[m];

						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Look at interesting grids */
						flag = TRUE;

						/* Find a new monster */
						i = target_pick(v, u, ddy[d], ddx[d]);

						/* Use that grid */
						if (i >= 0) m = i;
					}

					/* Nothing interesting */
					else
					{
						/* Restore previous position */
						panel_row_min = y2;
						panel_col_min = x2;
						panel_recalc_bounds();

						/* Update stuff */
						p_ptr->update |= (PU_MONSTERS);

						/* Redraw map */
						p_ptr->redraw |= (PR_MAP);

						/* Window stuff */
						p_ptr->window |= (PW_OVERHEAD);

						/* Handle stuff */
						handle_stuff();

						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Done */
						break;
					}
				}

				/* Use that grid */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Default prompt */
			strcpy(info, "q,t,p,m,+,-,<dir>");

			/* Draw the path in "target" mode. If there is one */
			if (mode & (TARGET_KILL))
				max = draw_path (path, path_char, path_attr, py, px, y, x);

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, mode | TARGET_LOOK, info);

			/* Remove the path */
			if (max > 0) load_path (max, path, path_char, path_attr);

			/* Cancel tracking */
			/* health_track(0); */

			/* Assume no direction */
			d = 0;

			/* Analyze the keypress */
			switch (query)
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
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					flag = !flag;
					break;
				}

				case 't':
				case '5':
				case '0':
				{
					target_set_location(y, x);
					done = TRUE;
					break;
				}

				default:
				{
					/* Extract a direction */
					d = target_dir(query);

					/* Oops */
					if (!d) bell("Illegal command for target mode!");

					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				int dx = ddx[d];
				int dy = ddy[d];

				/* Move */
				x += dx;
				y += dy;

				/* Do not move horizontally if unnecessary */
				if (((x < panel_col_min + (wid - 14) / 2) && (dx > 0)) ||
				    ((x > panel_col_min + (wid - 14) / 2) && (dx < 0)))
				{
					dx = 0;
				}

				/* Do not move vertically if unnecessary */
				if (((y < panel_row_min + (hgt - 2) / 2) && (dy > 0)) ||
				    ((y > panel_row_min + (hgt - 2) / 2) && (dy < 0)))
				{
					dy = 0;
				}

				/* Apply the motion */
				if ((y >= panel_row_min + hgt - 2) || (y < panel_row_min) ||
				    (x >= panel_col_min + wid - 14) || (x < panel_col_min))
				{
					if (change_panel(dy, dx)) target_set_interactive_prepare(mode);
				}


				/* Slide into legality */
				if (x <= 0) x = 0 + 1;
				else if (x >= DUNGEON_WID - 1) x = DUNGEON_WID - 2;

				/* Slide into legality */
				if (y <= 0) y = 0 + 1;
				else if (y >= DUNGEON_HGT - 1) y = DUNGEON_HGT - 2;
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Failure to set target */
	if (!p_ptr->target_set) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Given a starting position, find the 'n'th closest monster.
 *
 * Note:  "require_visible" only works when this function is looking around
 * the character.
 *
 * Set ty and tx to zero on failure.
 */
void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible)
{
	monster_type *m_ptr;

	int i, j;
	int r_idx;
	int dist = 100;

	int *monster_dist;
	int *monster_index;
	int monster_count = 0;

	bool use_view = FALSE;

	/* Allocate some arrays */
	C_MAKE(monster_dist, m_max, int);
	C_MAKE(monster_index, m_max, int);

	/* Note that we're looking from the character's grid */
	if ((y0 == p_ptr->py) && (x0 == p_ptr->px)) use_view = TRUE;

	/* Reset target grids */
	*ty = 0;  *tx = 0;

	/* N, as input, goes from 1+.  Map it to 0+ for table access */
	if (n > 0) n--;


	/* Check all the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Check for visibility */
		if (require_visible)
		{
			if (!m_ptr->ml) continue;
		}

		/* Use CAVE_VIEW information (fast way) */
		if (use_view)
		{
			if (!(cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_VIEW))) continue;

			/* Get stored distance */
			dist = m_ptr->cdis;
		}

		/* Monster must be in los from the starting position (slower way) */
		else
		{
			/* Get distance from starting position */
			dist = distance(y0, x0, m_ptr->fy, m_ptr->fx);

			/* Monster location must be within range */
			if (dist > MAX_SIGHT) continue;

			/* Require line of sight */
			if (!los(y0, x0, m_ptr->fy, m_ptr->fx)) continue;
		}

		/* Remember this monster */
		monster_dist[monster_count] = dist;
		monster_index[monster_count++] = i;
	}

	/* Not enough monsters found */
	if (monster_count <= n)
	{
		/* Free some arrays */
		C_KILL(monster_dist, m_max, int);
		C_KILL(monster_index, m_max, int);

		return;
	}


	/* Sort the monsters in ascending order of distance */
	for (i = 0; i < monster_count - 1; i++)
	{
		for (j = 0; j < monster_count - 1; j++)
		{
			int this_dist = monster_dist[j];
			int next_dist = monster_dist[j + 1];

			/* Bubble sort */
			if (this_dist > next_dist)
			{
				int tmp_dist  = monster_dist[j];
				int tmp_index = monster_index[j];

				monster_dist[j] = monster_dist[j + 1];
				monster_dist[j + 1] = tmp_dist;

				monster_index[j] = monster_index[j + 1];
				monster_index[j + 1] = tmp_index;
			}
		}
	}


	/* Get the nth closest monster's index */
	r_idx = monster_index[n];

	/* Get the monster */
	m_ptr = &m_list[r_idx];

	/* Set the target to its location */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;

	/* Free some arrays */
	C_KILL(monster_dist, m_max, int);
	C_KILL(monster_index, m_max, int);
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

	char ch;

	cptr p;

#ifdef ALLOW_REPEAT /* TNB */

	if (repeat_pull(dp))
	{
		/* Verify */
		if ((*dp > 0) && (*dp < 10) && (!(*dp == 5 && !target_okay())))
		{
			return (TRUE);
		}

		/* Invalid repeat - reset it */
		else repeat_clear();
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
			p = "Direction ('*' choose target; '.' closest; Esc cancel)?";
		}
		else
		{
			p = "Direction ('5' target; '*' re-target; '.' closest; Esc cancel)? ";
		}

		/* Get a command (or Cancel) */
		if (!get_com(p, &ch)) break;

		/* Analyze */
		switch (ch)
		{
			/* Set new target, use target if legal */
			case '*':
			{
				if (target_set_interactive(TARGET_KILL)) dir = 5;
				break;
			}

			/* Target the closest visible monster in line of fire */
			case '.':
			{
				int n = 0;

				/* Check closest monsters */
				while (TRUE)
				{
					int y = 0, x = 0;

					/* Get next monster */
					n++;

					/* Find the 'n'th closest viewable, visible monster */
					get_closest_los_monster(n, p_ptr->py, p_ptr->px, &y, &x, TRUE);

					/* We have a valid target */
					if ((y) && (x) && (cave_m_idx[y][x] > 0))
					{
						/* Get monster index */
						int m_idx = cave_m_idx[y][x];

						/* Monster must be in line of fire */
						if (target_able(m_idx))
						{
							health_track(m_idx);
							target_set_monster(m_idx);
							dir = 5;
							break;
						}
					}

					/* We've run out of targetable monsters */
					else
					{
						bell("No targetable monsters!");
						break;
					}
				}
				break;
			}

			/* Use current target, if set and legal */
			case 't':
			case '5':
			case '0':
			{
				if (target_okay()) dir = 5;
				break;
			}

			/* Possible direction */
			default:
			{
				dir = target_dir(ch);
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

#ifdef ALLOW_REPEAT /* TNB */

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
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.
 *
 * Directions "5" and "0" are illegal and will not be accepted.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
	int dir;

	char ch;

	cptr p;

#ifdef ALLOW_REPEAT /* TNB */

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
		if (!get_com(p, &ch)) break;

		/* Convert keypress into a direction */
		dir = target_dir(ch);

		/* Oops */
		if (!dir) bell("Illegal repeatable direction!");
	}

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

#ifdef ALLOW_REPEAT /* TNB */

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
	if (p_ptr->confused)
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

