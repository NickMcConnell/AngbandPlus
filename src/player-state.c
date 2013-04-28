/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* player-state.c: operations that change player's state */

#include "posband.h"

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
 * Set "p_ptr->tim_invis", notice observable changes
 *
 * Note the use of "PU_MONSTERS", which is needed because
 * "p_ptr->tim_image" affects monster visibility.
 */
bool set_tim_seeinvis(int v)
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
 * Set "p_ptr->tim_immaterial", notice observable changes
 */
bool set_tim_immaterial(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_immaterial)
		{
			msg_print("Your body becomes immaterial!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_immaterial)
		{
			msg_print("Your body becomes material again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_immaterial = v;

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
 * Set "p_ptr->tim_invisible", notice observable changes
 */
bool set_tim_invisibility(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Notice only if we have no permanent invisibility. */
	
	/* Open */
	if (v)
	{
		if (!p_ptr->tim_invisible && !p_ptr->invisible)
		{
			msg_print("You body fades!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invisible && !p_ptr->invisible)
		{
			msg_print("Your body reappears again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_invisible = v;

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
		/*first check for immunity to acid*/
		if ((p_ptr->immune_acid) && (v > p_ptr->oppose_acid))
		{
			msg_print("You feel no change in your resistance to acid.");
			notice = TRUE;
		}

		/*then check if player has permanent resist to acid*/
		else if ((p_ptr->resist_acid) && (!p_ptr->oppose_acid))
		{
			msg_print("You feel exceptionally resistant to acid!");
			notice = TRUE;
		}


		else if (!p_ptr->oppose_acid)
		{
			msg_print("You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_acid) && (!p_ptr->immune_acid))
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
		/*first check for immunity to electricity*/
		if ((p_ptr->immune_elec) && (v > p_ptr->oppose_elec))
		{
			msg_print("You feel no change in your resistance to electricity.");
			notice = TRUE;
		}

		/*then check if player has permanent resist to electricity*/
		else if ((p_ptr->resist_elec) && (!p_ptr->oppose_elec))
		{
			msg_print("You feel exceptionally resistant to electricity!");
			notice = TRUE;
		}

		/*if player has neither*/
		else if (!p_ptr->oppose_elec)
		{
			msg_print("You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_elec) && (!p_ptr->immune_elec))
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
		/*first check for immunity to fire*/
		if ((p_ptr->immune_fire) && (v > p_ptr->oppose_fire))
		{
			msg_print("You feel no change in your resistance to fire.");
			notice = TRUE;
		}

		/*then check if player has permanent resist to fire*/
		else if ((p_ptr->resist_fire) && (!p_ptr->oppose_fire))
		{
			msg_print("You feel exceptionally resistant to fire!");
			notice = TRUE;
		}

		/*if player has neither*/
		else if (!p_ptr->oppose_fire)
		{
			msg_print("You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_fire) && (!p_ptr->immune_fire))
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
		/*first check for immunity to cold*/
		if ((p_ptr->immune_cold) && (v > p_ptr->oppose_cold))
		{
			msg_print("You feel no change in your resistance to cold.");
			notice = TRUE;
		}

		/*then check if player has permanent resist to cold*/
		else if ((p_ptr->resist_cold) && (!p_ptr->oppose_cold))
		{
			msg_print("You feel exceptionally resistant to cold!");
			notice = TRUE;
		}
		/*if player has neither*/
		else if (!p_ptr->oppose_cold)
		{
			msg_print("You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_cold) && (!p_ptr->immune_cold))
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
		/*first check for immunity to pois*/
		if ((p_ptr->immune_pois) && (v > p_ptr->oppose_pois))
		{
			msg_print("You feel no change in your resistance to poison.");
			notice = TRUE;
		}

		/*Check if player has permanent resist to poison*/
		else if ((p_ptr->resist_pois) && (!p_ptr->oppose_pois))
		{
			msg_print("You feel exceptionally resistant to poison!");
			notice = TRUE;
		}

		/*if player doesn't have permanent resistance to poison*/
		else if (!p_ptr->oppose_pois)
		{
			msg_print("You feel resistant to poison!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_pois) && (!p_ptr->immune_pois))
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

	/*hack - don't increase stunning if already stunned.
	 *this is an effort to eliminate the "knocked out" instadeath.
	 */
	if ((p_ptr->stun > 100) && (v > p_ptr->stun)) return (FALSE);

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

	/* Disturb */
	if (disturb_state) disturb(0, 0);

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
	if (p_ptr->exp > p_ptr->max_exp) p_ptr->max_exp = p_ptr->exp;

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
		/* Gain a level */
		p_ptr->lev++;

		/* Message */
		message_format(MSG_LEVEL, p_ptr->lev, "Welcome to level %d.", p_ptr->lev);

		/* Save the highest level*/
		if (p_ptr->lev > p_ptr->max_lev)
		{
		     	/* update the highest level*/
			p_ptr->max_lev = p_ptr->lev;

			/* If auto-note taking enabled, write a note to the file every 5th level. */
        		if ((p_ptr->lev % 5) == 0)
			{
                		char buf[120];

		                /* Build the message */
                		sprintf(buf, "Reached level %d", p_ptr->lev);

		                /* Write message */
                		do_cmd_note(buf,  p_ptr->depth);
           		}
		}

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		/* Handle stuff */
		handle_stuff();
	}

	/* Gain max levels while possible */
	while ((p_ptr->max_lev < PY_MAX_LEVEL) &&
	       (p_ptr->max_exp >= (player_exp[p_ptr->max_lev-1] *
	                           p_ptr->expfact / 100L)))
	{
		/* Gain max level */
		p_ptr->max_lev++;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

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
int get_coin_type(const monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze "coin" monsters */
	if (r_ptr->d_char == '$')
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (3);
		if (strstr(name, " silver ")) return (6);
		if (strstr(name, " gold ")) return (11);
		if (strstr(name, " mithril ")) return (17);
		if (strstr(name, " adamantite ")) return (18);

		/* Look for textual clues */
		if (strstr(name, "Copper ")) return (3);
		if (strstr(name, "Silver ")) return (6);
		if (strstr(name, "Gold ")) return (11);
		if (strstr(name, "Mithril ")) return (17);
		if (strstr(name, "Adamantite ")) return (18);
	}

	/* Assume nothing */
	return (0);
}


/*
 * Create magical stairs after finishing a quest monster.
 */
static void build_quest_stairs(int y, int x)
{
	int ny, nx;

	/* Stagger around */
	while (!cave_valid_bold(y, x))
	{

		/* Pick a location */
		scatter(&ny, &nx, y, x, 5, 1);

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


/*
 * Handle the experience gain of monsters
 */
void monster_exp(int m_idx, int exp, int level)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *mr_ptr = &r_info[m_ptr->r_idx];
	char m_name[80];
	
	/* Give exp */
	m_ptr->exp += exp;
	
	/* For pets only -- silently give player 1/10 exp (counted as if he killed
	 * target monster himself)
	 */
	if (m_ptr->align & (AL_PET_MASK))
	{
		s32b div, new_exp, new_exp_frac;

		/* Player level */
		div = p_ptr->lev;

		/* Give some experience for the kill. Assume integer */
		new_exp = ((long)exp * level) / div;
		
		/* Handle fractional experience */
		new_exp_frac = ((((long)exp * level) % div)
		                * 0x10000L / div) + p_ptr->exp_frac;
		                
		/* Divide */
		/* Hack -- 1/2 exp for Qs */
		new_exp /= (r_info[p_ptr->m_r_idx].d_char == 'Q') ? 2 : 10;
		new_exp_frac /= (r_info[p_ptr->m_r_idx].d_char == 'Q') ? 2 : 10;

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
	
	/* Get the killer name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);
	
	/* Gain levels */
	while ((m_ptr->level < MONSTER_LEVEL_MAX) && (m_ptr->exp >= MONSTER_EXP(m_ptr->level + 1)))
	{
		m_ptr->level++;
		
		if (m_ptr->ml) msg_format("%^s advances to level %d.", m_name, m_ptr->level);
		
		/* +2 AC per level */
		m_ptr->ac += 2;
		
		/* +1 dice/3 levels is handled somewhere else */
		
		/* +HPs. Hack -- temporary workaround for undefined hit dies */
		if (!mr_ptr->body.hitdie)
			m_ptr->maxhp += randint(mr_ptr->level);
		else
			m_ptr->maxhp += randint(mr_ptr->body.hitdie);
		
		/* +2 mana, spell power (only if already had some) */
		if (m_ptr->maxmana) m_ptr->maxmana += 2;
		if (m_ptr->spell_power) m_ptr->spell_power += 2;
		
		/* Evolve if needed and possible */
		if (m_ptr->level >= mr_ptr->body.evolve_level && mr_ptr->body.evolve_index[0])
		{
			int i = 0;
			bool evolve = TRUE;
			
			/* Quest monsters should not evolve! */
			for (i = 0; i < z_info->q_max; i++)
			{
				quest_type *q_ptr = &q_info[i];
				if (m_ptr->r_idx == q_ptr->mon_idx && q_ptr->active_level == p_ptr->depth)
				{
					evolve = FALSE;
					break;
				}
			}
			
			if (evolve)
			{
				/* Count evolution forms */
				while (mr_ptr->body.evolve_index[i]) i++;
				
				/* Change race */
				do
				{
					m_ptr->r_idx = mr_ptr->body.evolve_index[rand_int(i)];
				} while (!m_ptr->r_idx);
				mr_ptr = &r_info[m_ptr->r_idx];
				
				/* Give message */
				if (m_ptr->ml) 
				{
				    	char t_name[80];
					monster_desc(t_name, sizeof(t_name), m_ptr, 0);
					msg_format("%^s grows into %s!", m_name, t_name);
				}
				
				/* Reassign base stats */
				if (mr_ptr->flags1 & (RF1_FORCE_MAXHP))
				{
					m_ptr->maxhp = (mr_ptr->hdice * mr_ptr->hside);
				}
				else
				{
					m_ptr->maxhp = damroll(mr_ptr->hdice, mr_ptr->hside);
				}
				m_ptr->maxmana = mr_ptr->mana;
				m_ptr->spell_power = mr_ptr->spell_power;
				m_ptr->mspeed = mr_ptr->speed;
				m_ptr->ac = mr_ptr->ac;

				/* Reroll bonuses for experience */
				m_ptr->ac += 2 * (m_ptr->level - 1);
				if (m_ptr->maxmana) m_ptr->maxmana += 2 * (m_ptr->level - 1);
				if (m_ptr->spell_power) m_ptr->spell_power += 2 * (m_ptr->level - 1);
				for (i = 1; i < m_ptr->level; i++)
				{
					if (!mr_ptr->body.hitdie)
						m_ptr->maxhp += randint(mr_ptr->level);
					else
						m_ptr->maxhp += randint(mr_ptr->body.hitdie);
				}
				
				/* Ensure normal mana and HP */
				if (m_ptr->mana > m_ptr->maxmana) m_ptr->mana = m_ptr->maxmana;
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;
			}
		}
	}


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
void monster_death(int m_idx, int who)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;
	int total = 0;

	bool questlevel = FALSE;
	bool completed = FALSE;
	bool fixedquest = FALSE;
	bool writenote = TRUE;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool chest = (r_ptr->flags1 & (RF1_DROP_CHEST)) ? TRUE : FALSE;
	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

	int force_coin = get_coin_type(r_ptr);

	object_type *i_ptr;
	object_type object_type_body;

	/* Give exp to killer */
	if (who > 0)
	{
	    	monster_exp(who, r_ptr->mexp, r_ptr->level);
	}


	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

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

	/* Mega-Hack -- drop "unique" treasures */
	if (r_ptr->flags1 & (RF1_UNIQUE) && r_ptr->artifact)
	{
		artifact_type *u_ptr = &u_info[r_ptr->artifact];
		
		/* Kings always drop their treasure. Others with 1/15 chance */
		if (rp_ptr->king_r_idx == m_ptr->r_idx || one_in_(15))
		{
			char o_name[80];
			char m_poss[80];
			
			monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);

			/* Message */
			if (rp_ptr->king_r_idx == m_ptr->r_idx)
			{
				msg_print("*** CONGRATULATIONS ***");
				msg_format("You have slain the %s %s, and take %s place!", p_name + rp_ptr->name,
					(r_ptr->flags1 & RF1_FEMALE ? "lady" : "lord"), m_poss);
				msg_print("From now, you will have permanent blessing and heroism.");
				set_blessed(p_ptr->blessed + 1);
				set_hero(p_ptr->hero + 1);
			}

			/* Get local object */
			i_ptr = &object_type_body;
		
			/* Wipe it */
			object_wipe(i_ptr);
		
			/* Set all flags ("pseudo-artifact") */
			i_ptr->number = 1;
			i_ptr->name3 = r_ptr->artifact;
			i_ptr->k_idx = lookup_kind(u_ptr->tval, u_ptr->sval);
			i_ptr->tval = u_ptr->tval;
			i_ptr->sval = u_ptr->sval;
			i_ptr->pval = u_ptr->pval;
			i_ptr->to_h = u_ptr->to_h;
			i_ptr->to_d = u_ptr->to_d;
			i_ptr->to_a = u_ptr->to_a;
			i_ptr->ac = u_ptr->ac;
			i_ptr->dd = u_ptr->dd;
			i_ptr->ds = u_ptr->ds;
			i_ptr->weight = u_ptr->weight;
			i_ptr->useof = 1;
			i_ptr->flags1 = u_ptr->flags1;
			i_ptr->flags2 = u_ptr->flags2;
			i_ptr->flags3 = u_ptr->flags3;
			i_ptr->flags4 = u_ptr->flags4;

			/* Instantly *ID* */
			object_aware(i_ptr);
			object_known(i_ptr);
			i_ptr->ident |= (IDENT_MENTAL);
			
			object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 0);
			msg_format("You notice dying %s dropping strange %s.", r_name + r_ptr->name,
				o_name);
			
			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);
		}
	}


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	object_level = (p_ptr->depth + r_ptr->level) / 2;

	/* Drop some objects */
	/* Hack -- don't if this was our pet */
	if (!(m_ptr->align & (AL_PET_MASK)))
	{
		for (j = 0; j < number; j++)
		{

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Make Gold */
			if (do_gold && (!chest) && (!do_item || (rand_int(100) < 50)))
			{
				/* Make some gold */
				if (!make_gold(i_ptr)) continue;

				/* Assume seen XXX XXX XXX */
				dump_gold++;
			}

			/* Make Object */
			else
			{
				if (chest)
				{
					if (!make_object(i_ptr, good, great,DROP_TYPE_CHEST)) continue;
				}

				/* Make an object */
				else if (!make_object(i_ptr, good, great,DROP_TYPE_UNTHEMED)) continue;

				/* Assume seen XXX XXX XXX */
				dump_item++;
			}

			/* Drop it in the dungeon */
			drop_near(i_ptr, -1, y, x);
		}
	}
	
	/* Mega-hack -- drop corpse in 35% of kills; for possessor, 65% */
	if (r_ptr->body.weight && ((p_ptr->prealclass == POSS_CLASS && rand_int(100) < 65) || rand_int(100) < 35) &&
		(!squelch_corpses || r_ptr->flags1 & RF1_UNIQUE))
	{
		/* Get local object */
		i_ptr = &object_type_body;
		
		/* Wipe the object */
		object_wipe(i_ptr);
		
		/* Prepare the 'corpse' object */
		object_prep(i_ptr, lookup_kind(TV_CORPSE, 0));
		
		/* Mega-Hack - mark the corpse for monster */
		i_ptr->pval = m_ptr->r_idx;
		
		/* Hack - set corpse weight */
		/* XXX Should randomize it a bit */
		i_ptr->weight = r_ptr->body.weight;
		
		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
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

	/* Update monster list window */
	p_ptr->window |= PW_MONLIST;

	/* Only process dungeon kills */
	if (!p_ptr->depth) return;

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/*hack - don't count if player didn't kill
		 *this assumes only player can kill uniques!!!!!
		 * This line is also ugly codeing. :)
		 */
		/* (...Count quest monsters killed by pets, though. Hmm?) */
		if (who != -1 && !(mon_list[who].align & (AL_PET_MASK))) continue;

		/* Quest level? */
		if (q_ptr->active_level == p_ptr->depth)
		{
			/* One on the level */
			questlevel = TRUE;

			/* Require "Quest Monsters" */
			if (q_ptr->mon_idx == m_ptr->r_idx)
			{

				char race_name[80];

				/* Get the monster race name (singular)*/
				monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

				/* Mark kills */
				q_ptr->cur_num++;

				/* Completed quest? */
				if (q_ptr->cur_num == q_ptr->max_num)
				{
					/* Mark complete */
					q_ptr->active_level = 0;

					/* Mark fixed quests */
					if ((q_ptr->type == QUEST_FIXED) || (q_ptr->type == QUEST_FIXED_U))
						fixedquest = TRUE;

					/* One complete */
					completed = TRUE;

					/* make a note of the completed quest, but not for fixed or
					 * fixed unique quests
					 */
					if (!fixedquest)
					{
						char note[120];

						/* Multiple quest monsters */
						if (q_ptr->max_num > 1)
						{
							plural_aux(race_name, sizeof(race_name));
						}

						if (r_ptr->flags1 & (RF1_UNIQUE))
						{
							/*write note*/
							if monster_nonliving(r_ptr)
								sprintf(note, "Quest: Destroyed %s", race_name);
							else sprintf(note, "Quest: Killed %s", race_name);
						}

						else
						{
							/* Write note */
							if monster_nonliving(r_ptr)
            					sprintf(note, "Quest: Destroyed %d %s", q_ptr->max_num, race_name);
							else sprintf(note, "Quest: Killed %d %s", q_ptr->max_num, race_name);
						}

 		  				do_cmd_note(note, p_ptr->depth);

						/*don't double-write uniques*/
						writenote = FALSE;
					}
				}

				/*let the player know how many left*/
				else if (q_ptr->type == QUEST_GUILD)
				{
						int remaining = q_ptr->max_num - q_ptr->cur_num;

						/* Multiple quest monsters */
						if (remaining > 1)
						{
							plural_aux(race_name, sizeof(race_name));
						}

						msg_format("You have %d %s remaining to complete your quest.",
								remaining, race_name);
				}
			}

		}

		/* Count incomplete fixed quests */
		if (q_ptr->active_level && (fixedquest)) total++;
	}

	/* If the player kills a Unique, and the notes option is on, write a note.
	 * The quest section will write the note if the unique is a quild questor*/
   	if ((r_ptr->flags1 & RF1_UNIQUE) && (writenote))
	{

		char note2[120];
 		char real_name[120];

		/* Get the monster's real name for the notes file */
		monster_desc(real_name, sizeof(real_name), m_ptr, 0x80);

		/* Write note */
       	if monster_nonliving(r_ptr) sprintf(note2, "Destroyed %s", real_name);
		else sprintf(note2, "Killed %s", real_name);

 		do_cmd_note(note2, p_ptr->depth);
	}
	
	/* -AU- Moved the following from mon_take_hit(),
	 * since uniques can now be killed by pets in certain cases
	 */

	/* When the player kills a Unique, it stays dead */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
	    	r_ptr->max_num = 0;

		/* reputation bonus */
		if (r_ptr->level) p_ptr->fame++;

		/* Count kills this life */
		if (l_ptr->pkills < MAX_SHORT) l_ptr->pkills++;

		/* Count kills in all lives */
		if (l_ptr->tkills < MAX_SHORT) l_ptr->tkills++;

		/* Hack -- Auto-recall */
		monster_race_track(m_ptr->r_idx);
	}


	/* Require a quest level */
	if (!questlevel) return;

	/* Require all quests on this level to be completed */
	if (!completed) return;

	/* Check quest type */
	if (!fixedquest)
	{
		/* Give a message */
		msg_print("You have completed your quest - collect your reward at the guild!");
		return;
	}

	/* Need some stairs */
	else if (total)
	{
		/* Build magical stairs */
		build_quest_stairs(y, x);

		p_ptr->fame += 10;
	}

 	/* Nothing left, game over... */
	else
 	{
		time_t ct = time((time_t*)0);
 		char long_day[25];

		/* Total winner */
 		p_ptr->total_winner = TRUE;

 		/* Redraw the "title" */
 		p_ptr->redraw |= (PR_TITLE);

		p_ptr->fame += 50;

		/* Build magical stairs */
		build_quest_stairs(y, x);

 		/* Congratulations */
 		msg_print("*** CONGRATULATIONS ***");
 		msg_print("You have won the game!");
 		msg_print("You may retire (commit suicide) when you are ready.");

 		/* Write a note */
  		note_printf("============================================================\n");
		(void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));
		note_printf("%s slew Morgoth on %s.\n", op_ptr->full_name, long_day);
 		note_printf("Long live %s!\n", op_ptr->full_name);
 		note_printf("Long live %s!\n", op_ptr->full_name);
		note_printf("============================================================\n");
	}
}

/*
 * Hurt the monster -- decrease HPs, handle death.
 * This function is used when the monster is hurt by "external factors"
 * (e.g. terrain), and simplifies many things a lot.
 * This function will never kill a unique.
 */
bool mon_hurt_misc(monster_type *m_ptr, int dam)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];
	bool fear = FALSE;
	
	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;
	
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && m_ptr->hp < 0) m_ptr->hp = 1;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Increase the noise level slightly. */
		if (add_wakeup_chance <= 8000) add_wakeup_chance += 300;

		/* Don't spam player with screams of agony */
		if (m_ptr->ml)
		{
			/* Non-living monster */
			if (monster_nonliving(r_ptr))
			{
				msg_format("%^s is destroyed.", m_name);
			}

			/* Living monster */
			else
			{
				msg_format("%^s is killed.", m_name);
			}
		}

		/* Handle death (note the "orphan" killer) */
		monster_death(m_idx, 0);

		/* Delete the monster */
		delete_monster_idx(m_idx);

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

			/* No more fear */
			fear = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)) && (dam > 0))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((randint(10) >= percentage) ||
		    ((dam >= m_ptr->hp) && (!one_in_(5))))
		{
			int fear_amt;

			/* Hack -- note fear */
			fear = TRUE;

			/* Hack -- Add some timed fear */
			fear_amt = rand_range(20, 30);

			/* Get frightened */
			set_mon_fear(m_ptr, fear_amt, TRUE);

			/*a monster can't be wary and afraid*/
			m_ptr->mflag &= ~(MFLAG_WARY);

		}
	}

	/* Monster will always go active */
	m_ptr->mflag |= (MFLAG_ACTV);

	/* Take note */
	if (fear && m_ptr->ml)
	{
		char m_name[80];

		/* Get the monster name (or "it") */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx,
			       "%^s flees in terror!", m_name);
	}
	
	/* Alive */
	return (FALSE);
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
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, int who)
{
	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Allow the debugging of damage done. */
	if ((dam > 0) && (p_ptr->wizard))
	{
		msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
	}

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Increase the noise level slightly. */
		if (add_wakeup_chance <= 8000) add_wakeup_chance += 300;

		/* Death by Missile/Spell attack */
		if (note)
		{
			/* Hack -- allow message suppression */
			if (strlen(note) <= 1)
			{
				/* Be silent */
			}

			else
			{
				message_format(MSG_KILL, m_ptr->r_idx, "%^s%s", m_name, note);
			}

		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if (monster_nonliving(r_ptr))
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			message_format(MSG_KILL, m_ptr->r_idx, "You have slain %s.", m_name);
		}


		/* Player level */
		div = p_ptr->lev;

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
		
		/* 3x experience for slaying king */
		if (m_ptr->r_idx == rp_ptr->king_r_idx) new_exp *= 3;

		/* Gain experience */
		gain_exp(new_exp);

		/* Generate treasure */
		monster_death(m_idx, who);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
	    	r_ptr->max_num = 0;

			/* reputation bonus */
 			if (r_ptr->level) p_ptr->fame++;
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

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)) && (dam > 0))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((randint(10) >= percentage) ||
		    ((dam >= m_ptr->hp) && (!one_in_(5))))
		{
			int fear_amt;

			/* Hack -- note fear */
			(*fear) = TRUE;

			/* Hack -- Add some timed fear */
			fear_amt = rand_range(20, 30);

			/* Get frightened */
			set_mon_fear(m_ptr, fear_amt, TRUE);

			/*a monster can't be wary and afraid*/
			m_ptr->mflag &= ~(MFLAG_WARY);

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
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(void *u, void *v, int p, int q)
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
void ang_sort(void *u, void *v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n-1);
}
