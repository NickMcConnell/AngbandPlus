/* File: xtra2.c */

/*
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
			message(MSG_BLIND, 0, "You are blind!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
			message(MSG_RECOVER, 0, "You can see again.");
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
	p_ptr->window |= (PW_OVERHEAD | PW_MAP);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/* Players with chaos or confusion resistance don't get confused*/
bool allow_player_confusion(void)
{
	if (p_ptr->resist_confu  || p_ptr->oppose_conf) return (FALSE);
	if (p_ptr->resist_chaos) return (FALSE);

	/*Don't have the right resists*/
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
			message(MSG_CONFUSED, 0, "You are confused!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->confused)
		{
			message(MSG_RECOVER, 0, "You feel less confused now.");
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
			message(MSG_POISONED, 0, "You are poisoned!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->poisoned)
		{
			message(MSG_RECOVER, 0, "You are no longer poisoned.");
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
			message(MSG_AFRAID, 0, "You are terrified!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->afraid)
		{
			message(MSG_RECOVER, 0, "You feel bolder now.");
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
			message(MSG_PARALYZED, 0, "You are paralyzed!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->paralyzed)
		{
			message(MSG_RECOVER, 0, "You can move again.");
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
			message(MSG_DRUGGED, 0, "You feel drugged!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
			message(MSG_RECOVER, 0, "You can see clearly again.");
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
	p_ptr->window |= (PW_OVERHEAD | PW_MAP);

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
			message(MSG_SPEED, 0, "You feel yourself moving faster!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
			message(MSG_RECOVER, 0, "You feel yourself slow down.");
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
			message(MSG_SLOW, 0, "You feel yourself moving slower!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slow)
		{
			message(MSG_RECOVER, 0, "You feel yourself speed up.");
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
bool set_flying(int v, bool can_crash)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 125) ? 125 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->flying)
		{
			/*Record any terrain damage incurred so far.*/
			process_player_terrain_damage();

			msg_print("You take flight!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->flying)
		{
			msg_print("Your mystic wings dissappear.");
			notice = TRUE;

			if (can_crash)
			{

				if (p_ptr->ffall)
				{
					msg_print("You float down gently.");
				}

				else
				{
					int dam;

					/* activate the ordinary daggers. */
					msg_print("Gravity sends you crashing down!");

					/* Base damage */
					dam = damroll(5, 5);

					/* Take the damage */
					take_hit(dam, "a freefall from the air");
				}
			}
		}
	}

	/* Use the value */
	p_ptr->flying = v;



	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Update player stealth */
	p_ptr->update |= (PU_STEALTH);

	/* Redraw visual indicator */
	p_ptr->redraw |= (PR_RESIST);

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
			message(MSG_SHIELD, 0, "A mystic shield forms around your body!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			message(MSG_RECOVER, 0, "Your mystic shield crumbles away.");
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

bool set_megashield(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->megashield)
		{
			message(MSG_SHIELD, 0, "A globe of force forms around your body!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->megashield)
		{
			message(MSG_RECOVER, 0, "Your magical globe crumbles away.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->megashield = v;

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
 * Set "p_ptr->slay_elements", notice observable changes
 */
bool set_slay_elements(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 100) ? 100 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->slay_elements)
		{
			message(MSG_SHIELD, 0, "Your weapon glows with many colors!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slay_elements)
		{
			message(MSG_RECOVER, 0, "Your weapon return to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slay_elements = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw visual indicator */
	p_ptr->redraw |= (PR_RESIST);

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
			message(MSG_BLESSED, 0, "You feel righteous!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blessed)
		{
			message(MSG_RECOVER, 0, "The prayer has expired.");
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
			message(MSG_HERO, 0, "You feel like a hero!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->hero)
		{
			message(MSG_RECOVER, 0, "The heroism wears off.");
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
			message(MSG_BERSERK, 0, "You feel like a killing machine!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shero)
		{
			message(MSG_RECOVER, 0, "You feel less Berserk.");
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
			message(MSG_PROT_EVIL, 0, "You feel safe from evil!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
			message(MSG_RECOVER, 0, "You no longer feel safe from evil.");
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
			message(MSG_INVULN, 0, "You feel invulnerable!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
			message(MSG_RECOVER, 0, "You feel vulnerable once more.");
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
			message(MSG_RECOVER, 0, "Your eyes feel very sensitive!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
			message(MSG_INFRARED, 0, "Your eyes feel less sensitive.");
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
			message(MSG_INFRARED, 0, "Your eyes begin to tingle!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_infra)
		{
			message(MSG_RECOVER, 0, "Your eyes stop tingling.");
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
		/*first check for immunity to acid*/
		if ((p_ptr->immune_acid) && (v > p_ptr->oppose_acid))
		{
			msg_print("You feel no change in your resistance to acid.");
			notice = TRUE;
		}

		/*then check if player has permanent resist to acid*/
		else if ((p_ptr->resist_acid) && (!p_ptr->oppose_acid))
		{
			message(MSG_RES_ACID, 0, "You feel exceptionally resistant to acid!");
			notice = TRUE;
		}


		else if (!p_ptr->oppose_acid)
		{
			message(MSG_RES_ACID, 0, "You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_acid) && (!p_ptr->immune_acid))
		{
			message(MSG_RECOVER, 0, "You feel less resistant to acid.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_acid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->oppose_conf", notice observable changes
 */
bool set_oppose_conf(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/*first check for resistance to confusion */
		if ((p_ptr->resist_chaos || p_ptr->resist_confu) && (v > p_ptr->oppose_conf))
		{
			msg_print("You feel no change in your resistance to confusion.");
			notice = TRUE;
		}

		else if (!p_ptr->oppose_conf)
		{
			msg_print("You feel resistant to confusion!");
			notice = TRUE;
		}
	} else if (p_ptr->oppose_conf && !(p_ptr->resist_chaos || p_ptr->resist_confu))
	{
			msg_print("You feel less resistant to confusion.");
			notice = TRUE;
	}

	/* Use the value */
	p_ptr->oppose_conf = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

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
			message(MSG_RES_ELEC, 0, "You feel exceptionally resistant to electricity!");
			notice = TRUE;
		}

		/*if player has neither*/
		else if (!p_ptr->oppose_elec)
		{
			message(MSG_RES_ELEC, 0, "You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_elec) && (!p_ptr->immune_elec))
		{
			message(MSG_RECOVER, 0, "You feel less resistant to electricity.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_elec = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

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
			message(MSG_RES_FIRE, 0, "You feel exceptionally resistant to fire!");
			notice = TRUE;
		}

		/*if player has neither*/
		else if (!p_ptr->oppose_fire)
		{
			message(MSG_RES_FIRE, 0, "You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_fire) && (!p_ptr->immune_fire))
		{
			message(MSG_RECOVER, 0, "You feel less resistant to fire.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_fire = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

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
			message(MSG_RES_COLD, 0, "You feel exceptionally resistant to cold!");
			notice = TRUE;
		}
		/*if player has neither*/
		else if (!p_ptr->oppose_cold)
		{
			message(MSG_RES_COLD, 0, "You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_cold) && (!p_ptr->immune_cold))
		{
			message(MSG_RECOVER, 0, "You feel less resistant to cold.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_cold = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

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
		/*first check for immunity to cold*/
		if ((p_ptr->immune_pois) && (v > p_ptr->oppose_pois))
		{
			msg_print("You feel no change in your resistance to poison.");
			notice = TRUE;
		}

		/*Then check if player has permanent resist to poison*/
		else if ((p_ptr->resist_pois) && (!p_ptr->oppose_pois))
		{
			message(MSG_RES_POIS, 0, "You feel exceptionally resistant to poison!");
			notice = TRUE;
		}

		/*if player doesn't have permanent resistance to poison*/
		else if (!p_ptr->oppose_pois)
		{
			message(MSG_RES_POIS, 0, "You feel resistant to poison!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if ((p_ptr->oppose_pois) && (!(p_ptr->immune_pois)))
		{
			message(MSG_RECOVER, 0, "You feel less resistant to poison.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_pois = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->temp_native_lava", notice observable changes
 */
bool set_temp_native_lava(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently native*/
		if ((!p_ptr->temp_native_lava) && !(p_ptr->p_native & P_NATIVE_LAVA))
		{
			msg_print("You feel native to lava terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/*first check if already native to lava*/
		else if (v > p_ptr->temp_native_lava)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}

	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_lava)
		{
			message(MSG_RECOVER, 0, "You are no longer native to lava terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_lava = v;

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
 * Set "p_ptr->temp_native_oil", notice observable changes
 */
bool set_temp_native_oil(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently native*/
		if ((!p_ptr->temp_native_oil) && !(p_ptr->p_native & P_NATIVE_OIL))
		{
			msg_print("You feel native to oil terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/*first check if already native to oil*/
		else if (v > p_ptr->temp_native_oil)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}

	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_oil)
		{
			message(MSG_RECOVER, 0, "You are no longer native to oil terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_oil = v;

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
 * Set "p_ptr->temp_native_sand", notice observable changes
 */
bool set_temp_native_sand(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently native*/
		if ((!p_ptr->temp_native_sand) && !(p_ptr->p_native & P_NATIVE_SAND))
		{
			msg_print("You feel native to sandy terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/*first check if already native to sand*/
		else if (v > p_ptr->temp_native_sand)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}

	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_sand)
		{
			message(MSG_RECOVER, 0, "You are no longer native to sandy terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_sand = v;

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
 * Set "p_ptr->temp_native_forest", notice observable changes
 */
bool set_temp_native_forest(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently native*/
		if ((!p_ptr->temp_native_forest) && !(p_ptr->p_native & P_NATIVE_FOREST))
		{
			msg_print("You feel native to forest terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/*first check if already native to forest*/
		else if (v > p_ptr->temp_native_forest)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}

	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_forest)
		{
			message(MSG_RECOVER, 0, "You are no longer native to forest terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_forest = v;

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
 * Set "p_ptr->temp_native_water", notice observable changes
 */
bool set_temp_native_water(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently water*/
		if ((!p_ptr->temp_native_water) && !(p_ptr->p_native & P_NATIVE_WATER))
		{
			msg_print("You feel native to water terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/*first check if already native to water*/
		else if (v > p_ptr->temp_native_water)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_water)
		{
			message(MSG_RECOVER, 0, "You are no longer native to water terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_water = v;

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
 * Set "p_ptr->temp_native_mud", notice observable changes
 */
bool set_temp_native_mud(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		/* Not currently native*/
		if ((!p_ptr->temp_native_mud) && !(p_ptr->p_native & P_NATIVE_MUD))
		{
			msg_print("You feel native to muddy terrains!");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}

		/* First check if already native to mud*/
		else if (v > p_ptr->temp_native_mud)
		{
			msg_print("You feel no different.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->temp_native_mud)
		{
			message(MSG_RECOVER, 0, "You are no longer native to muddy terrains.");
			notice = TRUE;

			/* Redo native */
			p_ptr->update |= (PU_NATIVE);
		}
	}

	/* Use the value */
	p_ptr->temp_native_mud = v;

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
				message(MSG_STUN, 0, "You have been stunned.");
				break;
			}

			/* Heavy stun */
			case 2:
			{
				message(MSG_STUN, 0, "You have been heavily stunned.");
				break;
			}

			/* Knocked out */
			case 3:
			{
				message(MSG_STUN, 0, "You have been knocked out.");
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
				message(MSG_RECOVER, 0, "You are no longer stunned.");
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

	/* Redraw resistances */
	p_ptr->redraw |= (PR_RESIST);

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
				message(MSG_CUT, 0,"You have been given a graze.");
				break;
			}

			/* Light cut */
			case 2:
			{
				message(MSG_CUT, 0,"You have been given a light cut.");
				break;
			}

			/* Bad cut */
			case 3:
			{
				message(MSG_CUT, 0,"You have been given a bad cut.");
				break;
			}

			/* Nasty cut */
			case 4:
			{
				message(MSG_CUT, 0,"You have been given a nasty cut.");
				break;
			}

			/* Severe cut */
			case 5:
			{
				message(MSG_CUT, 0,"You have been given a severe cut.");
				break;
			}

			/* Deep gash */
			case 6:
			{
				message(MSG_CUT, 0,"You have been given a deep gash.");
				break;
			}

			/* Mortal wound */
			case 7:
			{
				message(MSG_CUT, 0,"You have been given a mortal wound.");
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
				message(MSG_RECOVER, 0, "You are no longer bleeding.");
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
				message(MSG_HUNGRY, 0, "You have gorged yourself!");
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
				sound(MSG_NOTICE);
				message(MSG_PARALYZED, 0, "You are getting faint from hunger!");
				break;
			}

			/* Weak */
			case 1:
			{
				sound(MSG_NOTICE);
				message(MSG_HUNGRY, 0, "You are getting weak from hunger!");
				break;
			}

			/* Hungry */
			case 2:
			{
				sound(MSG_HUNGRY);
				message(MSG_HUNGRY, 0, "You are getting hungry.");
				break;
			}

			/* Normal */
			case 3:
			{
				sound(MSG_NOTICE);
				msg_print("You are no longer full.");
				break;
			}

			/* Full */
			case 4:
			{
				sound(MSG_NOTICE);
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
	s16b gain_stat = 0, choice, success;
	char prompt[80];

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

			gain_stat = 1;

			/* If auto-note taking enabled, write a note to the file every 5th level. */
            if ((adult_take_notes) && ((p_ptr->lev % 5) == 0))

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
		
		if (gain_stat){
			screen_save();
			msg_print("You can develop one of your stats further. (Press a key) ");
			inkey();
			msg_flag = FALSE;
			while (1){
				Term_clear();
				display_player_stat_info(2, 5);
				put_str("(a)",2,1);
				put_str("(b)",3,1);
				put_str("(c)",4,1);
				put_str("(d)",5,1);
				put_str("(e)",6,1);
				put_str("(f)",7,1);
				put_str("(g)",8,1);
				put_str("(h)",9,1);
				put_str("(i)",10,1);
				my_strcpy(prompt, "Which stat do you want to increase? (a-i) ",
					sizeof(prompt));
				choice = get_menu_choice(A_MAX, prompt);
				if (choice>=0 && choice<A_MAX){
					success = inc_stat(choice);
					if (success){
						break;
					} else {
						msg_print("That stat is already maxed out! (Press a key) ");
						inkey();
						msg_flag = FALSE;
					}
				}
			}
			screen_load();
			msg_flag = FALSE;
		}

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE | PR_EXP);

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

	/* Analyze "coin" or golem monsters */
	if ((r_ptr->d_char == '$') || (r_ptr->d_char == 'g'))
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (SV_GOLD_COPPER);
		if (strstr(name, " silver ")) return (SV_GOLD_SILVER);
		if (strstr(name, " gold ")) return (SV_GOLD_GOLD);
		if (strstr(name, " mithril ")) return (SV_GOLD_MITHRIL);
		if (strstr(name, " opal")) return (SV_GOLD_OPALS);
		if (strstr(name, " sapphire")) return (SV_GOLD_SAPPHIRES);
		if (strstr(name, " ruby")) return (SV_GOLD_RUBIES);
		if (strstr(name, " diamond")) return (SV_GOLD_DIAMOND);
		if (strstr(name, " adamantite ")) return (SV_GOLD_ADAMANTITE);

		/* Look for textual clues */
		if (strstr(name, " Copper ")) return (SV_GOLD_COPPER);
		if (strstr(name, " Silver ")) return (SV_GOLD_SILVER);
		if (strstr(name, " Gold ")) return (SV_GOLD_GOLD);
		if (strstr(name, " Mithril ")) return (SV_GOLD_MITHRIL);
		if (strstr(name, " Opal")) return (SV_GOLD_OPALS);
		if (strstr(name, " Sapphire")) return (SV_GOLD_SAPPHIRES);
		if (strstr(name, " Ruby")) return (SV_GOLD_RUBIES);
		if (strstr(name, " Diamond")) return (SV_GOLD_DIAMOND);
		if (strstr(name, " Adamantite ")) return (SV_GOLD_ADAMANTITE);
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
 * Handle the "death" of a monster.
 *
 * Disperse treasures centered at the monster location based on the
 * various flags contained in the monster flags fields.
 *
 * Check for "Quest" completion when a quest monster is killed.
 *
 * Note that only the player can induce "monster_death()" on Uniques or quest monsters.
 *
 * Note that monsters can now carry objects, and when a monster dies,
 * it drops all of its objects, which may disappear in crowded rooms.
 */
void monster_death(int m_idx, int who)
{
	int i, j, y, x;

	int dump_item = 0;
	int dump_gold = 0;

	int number_drops = 0;
	int total = 0;

	bool questlevel = FALSE;
	bool completed = FALSE;
	bool fixedquest = FALSE;
	bool writenote = TRUE;

	s16b set_object_level;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &mon_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool chest = (r_ptr->flags1 & (RF1_DROP_CHEST)) ? TRUE : FALSE;
	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

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

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/*Remove the mark to hide when monsters carry this object*/
		o_ptr->ident &= ~(IDENT_HIDE_CARRY);

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


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number_drops++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number_drops++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number_drops += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number_drops += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number_drops += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number_drops += damroll(4, 2);

	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	set_object_level = object_level = (danger(p_ptr->depth) + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number_drops; j++)
	{
		bool interesting = FALSE;

		/* Re-set the object level */
		object_level = set_object_level;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* work on the "too much junk" problem, large drops sometimes are less items with a "boost". */
		if ((randint(400) < (number_drops * number_drops)) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
		{
			interesting = TRUE;
			number_drops -= 5;
			object_level += 5;

			/*Boundry Control*/
			if (number_drops < 0) number_drops = 0;
			if (object_level > MAX_DEPTH) object_level = MAX_DEPTH;
		}

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
			bool this_good = good;
			bool this_great = great;

			if (chest)
			{
				if (!make_object(i_ptr, this_good, this_great, DROP_TYPE_CHEST, interesting)) continue;
			}

			/* Make an object */
			else if (!make_object(i_ptr, this_good, this_great, DROP_TYPE_UNTHEMED, interesting)) continue;

			/* Remember history */
			if (visible) object_history(i_ptr, ORIGIN_DROP_KNOWN, m_ptr->r_idx);
			else object_history(i_ptr, ORIGIN_DROP_UNKNOWN, 0);

			/* Assume seen XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = danger(p_ptr->depth);

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

	/* Count incomplete quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/*
		 * Hack - don't count if player didn't kill, or on a town level
		 * This assumes only a player can kill quest monsters!!!!!
		 * This line is also ugly coding. :)
		 */
		if (((who != SOURCE_PLAYER) && (who != SOURCE_TRAP)) || (!p_ptr->depth)) continue;

		/* Quest level? */
		if ((q_ptr->active_level == p_ptr->depth) && (p_ptr->depth > 0))
		{
			/* One on the level */
			questlevel = TRUE;

			/* Require "Quest Monsters" */
			if 	(q_ptr->mon_idx == m_ptr->r_idx)
			{
				char race_name[80];

				/* Get the monster race name (singular)*/
				monster_desc_race(race_name, sizeof(race_name), q_ptr->mon_idx);

				/* Mark kills */
				q_ptr->cur_num++;

				/* Redraw quest indicator */
				p_ptr->redraw |= (PR_DEPTH);

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

					/*make a note of the completed quest, but not for fixed or
				     * fixed unique quests
					 */
					if ((adult_take_notes) && (!fixedquest))
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
				else if (q_ptr->type == QUEST_MONSTER)
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
			/*quest monster from a themed level, nest, or pit*/
			else if ((m_ptr->mflag & (MFLAG_QUEST)) &&
					 ((q_ptr->type ==  QUEST_THEMED_LEVEL) ||
			 		  (q_ptr->type == QUEST_PIT) ||
				  	  (q_ptr->type == QUEST_NEST)))
			{
				char note[120];

				/* Mark kills */
				q_ptr->cur_num++;

				/* Redraw quest indicator */
				p_ptr->redraw |= (PR_DEPTH);

				/* Completed quest? */
				if (q_ptr->cur_num == q_ptr->max_num)
				{
					/* Mark complete */
					q_ptr->active_level = 0;

					/* One complete */
					completed = TRUE;

					if (adult_take_notes)
					{
						char mon_theme[80];
						/*Get the theme*/
						my_strcpy(mon_theme, feeling_themed_level[q_ptr->theme], sizeof(mon_theme));

						my_strcpy(note, "Quest: Cleared out ", sizeof(note));

						/*make the grammar proper*/
						if (my_is_vowel(mon_theme[0])) my_strcat(note, "an ", sizeof(note));
						else my_strcat(note, "a ", sizeof(note));

						/*dump the monster theme*/
						my_strcat(note, mon_theme, sizeof(note));

						/*Finish off the line*/
						if  (q_ptr->type ==  QUEST_THEMED_LEVEL) 	my_strcat(note, " stronghold.", sizeof(note));
			 			else if (q_ptr->type == QUEST_PIT)	my_strcat(note, " pit.", sizeof(note));
				  		else if (q_ptr->type == QUEST_NEST)	my_strcat(note, " nest.", sizeof(note));

						/*write it*/
						do_cmd_note(note, p_ptr->depth);

					}
				}
				/*not done yet*/
				else
				{
					int remaining = q_ptr->max_num - q_ptr->cur_num;

					if (remaining > 1)
					{
						my_strcpy(note, format("There are %d creatures remaining ", remaining), sizeof(note));
					}
					else my_strcpy(note, "There is one creature remaining ", sizeof(note));
					my_strcat(note, format("from the %s", feeling_themed_level[q_ptr->theme]),
									sizeof(note));
					if  (q_ptr->type ==  QUEST_THEMED_LEVEL) 	my_strcat(note, " stronghold.", sizeof(note));
			 		else if (q_ptr->type == QUEST_PIT)	my_strcat(note, " pit.", sizeof(note));
				  	else if (q_ptr->type == QUEST_NEST)	my_strcat(note, " nest.", sizeof(note));

					/*dump the final note*/
					msg_format(note);
				}

			}

		}

		/* Count incomplete fixed quests */
		if (q_ptr->active_level && (fixedquest)) total++;
	}

	/* If the player kills a Unique, and the notes option is on, write a note.
	 * The quest section will write the note if the unique is a quild questor*/
   	if ((r_ptr->flags1 & RF1_UNIQUE) && (adult_take_notes) && (writenote))
	{

		char note2[120];
 		char real_name[120];

		/*write note for player ghosts*/
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			my_strcpy(note2, format("Destroyed %^s, the %^s", ghost_name, r_name + r_ptr->name), sizeof (note2));
		}

		/*All other uniques*/
		else
		{
			/* Get the monster's real name for the notes file */
			monster_desc_race(real_name, sizeof(real_name), m_ptr->r_idx);

			/* Write note */
       		if monster_nonliving(r_ptr) my_strcpy(note2, format("Destroyed %s", real_name), sizeof (note2));
			else my_strcpy(note2, format("Killed %s", real_name), sizeof (note2));
		}

 		do_cmd_note(note2, p_ptr->depth);
	}

	/* Only process dungeon kills */
	if (!p_ptr->depth) return;

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

 		/* Write a note, if that option is on */
  		if (adult_take_notes)
 		{
	 		/* Variable for the date */
 			time_t ct = time((time_t*)0);
 			char long_day[25];
			fprintf(notes_file, "============================================================\n");
  		    (void)strftime(long_day, 25, "%m/%d/%Y at %I:%M %p", localtime(&ct));
			fprintf(notes_file, "%s slew the Witch-King of Angmar on %s.\n", op_ptr->full_name, long_day);
 			fprintf(notes_file, "Long live %s!\n", op_ptr->full_name);
 		    fprintf(notes_file, "Long live %s!\n", op_ptr->full_name);
			fprintf(notes_file, "============================================================\n");
      	}
	}
}


/*Helper function to calculate the monster experience*/
static s32b calc_mon_exp(const monster_race *r_ptr)
{
	/*calculate the monster experience*/
	s32b new_exp = ((long)r_ptr->mexp * r_ptr->level) / p_ptr->lev;

	s16b new_level = p_ptr->max_lev;

	/*not a full point of experience to gain*/
	if (new_exp < 1) return (0);

	/*
	 * Check to make sure player is at level 50, so no adjustmetn necessary,
	 * also prevents next line from crashing the game
	 */
	while (new_level < PY_MAX_LEVEL)
	{
		s32b remaining_exp;
		s32b net_exp_gain;

		/*
		 * Player is not gaining a new max level
		 * (in the player_exp chart level 1 exp-to-gain-next-level is at slot 0)
		 */
		if ((p_ptr->exp + new_exp) <=
		    (player_exp[new_level - 1] * p_ptr->expfact / 100L)) break;

		/*just checking this again*/
		if (new_exp < 1) break;

		/*figure out the remainder*/
		net_exp_gain = (p_ptr->exp + new_exp) -
					   (player_exp[new_level - 1] * p_ptr->expfact / 100L);

		/*add one level*/
		new_level++;

		/*player is going up a max level, adjust*/
		remaining_exp = ((long)net_exp_gain * (new_level - 1)) / new_level;

		/*slightly reduce new experience*/
		new_exp -= (net_exp_gain - remaining_exp);
	}

	return (new_exp);
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

	s32b new_exp, new_exp_frac;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Allow the debugging of damage done. */
	if ((dam > 0) && (p_ptr->wizard))
	{
		if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
		{
			msg_format("You do %d (out of %d) damage.", dam, m_ptr->hp);
		}
		else msg_format("%d (out of %d) damage has been done.", dam, m_ptr->hp);

	}

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Assume normal death sound */
		int soundfx = MSG_KILL;

		/* Play a special sound if the monster was unique */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			soundfx = MSG_KILL_UNIQUE;
		}

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
				message_format(soundfx, m_ptr->r_idx, "%^s%s", m_name, note);
			}

		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have killed %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been killed.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if (monster_nonliving(r_ptr))
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have destroyed %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been destroyed", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
			{
				message_format(soundfx, m_ptr->r_idx, "You have slain %s.", m_name);
			}
			else message_format(soundfx, m_ptr->r_idx, "%^s has been slain", m_name);
		}

		if ((who == SOURCE_PLAYER) || (who == SOURCE_TRAP))
		{

			/* Give some experience for the kill */
			new_exp = calc_mon_exp(r_ptr);

			/* Handle fractional experience */
			new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % p_ptr->lev)
						 * 0x10000L / p_ptr->lev) + p_ptr->exp_frac;

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
		monster_death(m_idx, who);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
	    	r_ptr->max_num = 0;

			/* reputation bonus, except for the town unique */
 			if (r_ptr->level > p_ptr->lev) p_ptr->fame++;
		}

		/* When the player kills a player ghost, the bones file that
		 * it used is (often) deleted.
		 */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/*Another point of player fame*/
			p_ptr->fame++;

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
			fear_amt = rand_range(20, 30) + dam / 5;

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
 * Modify the current panel to the given coordinates, adjusting only to
 * ensure the coordinates are legal, and return TRUE if anything done.
 *
 * The town should never be scrolled around.
 *
 * Note that monsters are no longer affected in any way by panel changes.
 *
 * As a total hack, whenever the current panel changes, we assume that
 * the "overhead view" window should be updated.
 */
bool modify_panel(term *t, int wy, int wx)
{
	int dungeon_hgt = p_ptr->cur_map_hgt;
	int dungeon_wid = p_ptr->cur_map_wid;

	int screen_hgt = (t == Term) ? (t->hgt - ROW_MAP - 1) : t->hgt;
	int screen_wid = (t == Term) ? (t->wid - COL_MAP - 1) : t->wid;

	/* Bigtile panels only have half the width */
	if (use_bigtile) screen_wid = screen_wid / 2;

	/* Verify wy, adjust if needed */
	if (wy > dungeon_hgt - screen_hgt) wy = dungeon_hgt - screen_hgt;
	if (wy < 0) wy = 0;

	/* Verify wx, adjust if needed */
	if (wx > dungeon_wid - screen_wid) wx = dungeon_wid - screen_wid;
	if (wx < 0) wx = 0;


	/* React to changes */
	if ((t->offset_y != wy) || (t->offset_x != wx))
	{
		/* Save wy, wx */
		t->offset_y = wy;
		t->offset_x = wx;

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);
		p_ptr->window |= (PW_OVERHEAD | PW_MAP);

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
	bool changed = FALSE;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int wx, wy;
		int screen_hgt, screen_wid;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Adjust as needed */
		while (y >= wy + screen_hgt) wy += screen_hgt / 2;
		while (y < wy) wy -= screen_hgt / 2;

		/* Adjust as needed */
		while (x >= wx + screen_wid) wx += screen_wid / 2;
		while (x < wx) wx -= screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
}


/*
 * Change the current panel to the panel lying in the given direction.
 *
 * Return TRUE if the panel was changed.
 */
bool change_panel(int dir)
{
	bool changed = FALSE;
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		int screen_hgt, screen_wid;
		int wx, wy;

		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & PW_MAP)) continue;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		/* Shift by half a panel */
		wy = t->offset_y + ddy[dir] * screen_hgt / 2;
		wx = t->offset_x + ddx[dir] * screen_wid / 2;

		/* Use "modify_panel" */
		if (modify_panel(t, wy, wx)) changed = TRUE;
	}

	return (changed);
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
	int wy, wx;
	int screen_hgt, screen_wid;

	int panel_wid, panel_hgt;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		term *t = angband_term[j];

		/* No window */
		if (!t) continue;

		/* No relevant flags */
		if ((j > 0) && !(op_ptr->window_flag[j] & (PW_MAP))) continue;

		wy = t->offset_y;
		wx = t->offset_x;

		screen_hgt = (j == 0) ? (Term->hgt - ROW_MAP - 1) : t->hgt;
		screen_wid = (j == 0) ? (Term->wid - COL_MAP - 1) : t->wid;

		/* Bigtile panels only have half the width */
		if (use_bigtile) screen_wid = screen_wid / 2;

		panel_wid = screen_wid / 2;
		panel_hgt = screen_hgt / 2;

		/* Scroll screen vertically when off-center */
		if (center_player && (!p_ptr->running || !run_avoid_center) &&
		    (py != wy + panel_hgt))
		{
			wy = py - panel_hgt;
		}

		/* Scroll screen vertically when 2 grids from top/bottom edge */
		else if ((py < wy + panel_change_offset_y) || (py >= wy + screen_hgt - panel_change_offset_y))
		{
			wy = py - panel_hgt;
		}

		/* Scroll screen horizontally when off-center */
		if (center_player && (!p_ptr->running || !run_avoid_center) &&
		    (px != wx + panel_wid))
		{
			wx = px - panel_wid;
		}

		/* Scroll screen horizontally when 4 grids from left/right edge */
		else if ((px < wx + panel_change_offset_x) || (px >= wx + screen_wid - panel_change_offset_x))
		{
			wx = px - panel_wid;
		}

		/* Scroll if needed */
		if ((wy != t->offset_y) || (wx != t->offset_x)) modify_panel(t, wy, wx);
	}
}


/*
 * Monster health description.
 * This function should not be called without a check that the monster is a mimic
 */
static void look_mon_desc(char *buf, size_t max, int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool living = TRUE;

	/*monster is an undiscovered mimic, don't desccribe and return*/
	if (m_ptr->mimic_k_idx)
	{
		/*Paranoia - terminate the string*/
		buf[0] = '\0';

	    return;
	}

	/* Determine if the monster is "living" (vs "undead") */
	if (monster_nonliving(r_ptr)) living = FALSE;

	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		my_strcpy(buf, (living ? "unhurt" : "undamaged"), max);
	}
	else
	{
		/* Calculate a health "percentage" */
		int perc = 100L * m_ptr->hp / m_ptr->maxhp;

		if (perc >= 60)
			my_strcpy(buf, (living ? "somewhat wounded" : "somewhat damaged"), max);
		else if (perc >= 25)
			my_strcpy(buf, (living ? "wounded" : "damaged"), max);
		else if (perc >= 10)
			my_strcpy(buf, (living ? "badly wounded" : "badly damaged"), max);
		else
			my_strcpy(buf, (living ? "almost dead" : "almost destroyed"), max);
	}

	if (m_ptr->mflag & (MFLAG_TOWN)) strcat(buf, ", town");
	if (m_ptr->mflag & (MFLAG_STERILE)) strcat(buf, ", sterile");
	if (m_ptr->mflag & (MFLAG_WARY)) strcat(buf, ", wary");
	if (m_ptr->mflag & (MFLAG_FLYING)) strcat(buf, ", flying");
	if (m_ptr->csleep) my_strcat(buf, ", asleep", max);
	if (m_ptr->confused) my_strcat(buf, ", confused", max);
	if (m_ptr->monfear) my_strcat(buf, ", afraid", max);
	if (m_ptr->stunned) my_strcat(buf, ", stunned", max);
	if ((m_ptr->slowed) && (!m_ptr->hasted)) my_strcat(buf, ", slowed", max);
	if ((!m_ptr->slowed) && (m_ptr->hasted)) my_strcat(buf, ", hasted", max);
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


/*** Targetting Code ***/



/*
 * Given a "source" and "target" location, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
int motion_dir(int y1, int x1, int y2, int x2)
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
int target_dir(char ch)
{
	int d = 0;

	int mode;

	cptr act;

	cptr s;


	/* Already a direction? */
	if (isdigit((unsigned char)ch))
	{
		d = D2I(ch);
	}
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
				if (isdigit((unsigned char)*s)) d = D2I(*s);
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

	monster_type *m_ptr;

	/* No monster */
	if (m_idx <= 0) return (FALSE);

	/* Get monster */
	m_ptr = &mon_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);

	/*monster is an undiscovered mimic*/
	if (m_ptr->mimic_k_idx) return (FALSE);

	/* Monster must be projectable */
	if (!player_can_fire_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Walls protect monsters */
	if (!cave_project_bold(m_ptr->fy, m_ptr->fx) &&
		!cave_passable_bold(m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

	/* Hack -- Never target trappers XXX XXX XXX */
	/* if (CLEAR_ATTR && (CLEAR_CHAR)) return (FALSE); */

	/* Hidden monsters cannot be targets */
	if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

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
			monster_type *m_ptr = &mon_list[m_idx];

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
 * Set the target to a monster (or nobody)
 */
void target_set_monster(int m_idx)
{
	/* Acceptable target */
	if ((m_idx > 0) && target_able(m_idx))
	{
		monster_type *m_ptr = &mon_list[m_idx];

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
static bool ang_sort_comp_distance(const void *u, const void *v, int a, int b)
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
static void ang_sort_swap_distance(void *u, void *v, int a, int b)
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
	object_type *o_ptr;

	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);

	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);

	/* Visible monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
	{
		/* Memorized object */
		if (o_ptr->marked) return (TRUE);
	}

	/* Interesting memorized features */
	/* Ignore unknown features */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Find interesting effects */
   	if (cave_x_idx[y][x] > 0)
	{
		/* Get the first effect */
		u16b x_idx = cave_x_idx[y][x];

		/* Scan the effects on that grid */
		while (x_idx)
		{
			/* Get the effect data */
			effect_type *x_ptr = &x_list[x_idx];

			/* Point to the next effect */
			x_idx = x_ptr->next_x_idx;

			/* Ignore hidden effects */
			if (!(x_ptr->x_f_idx) ||
				(x_ptr->x_flags & (EF1_HIDDEN))) continue;

			/* We have an interesting effect */
			return (TRUE);
		}
	}

	/* Check grid type with dungeon capabilities */
	return ((*dun_cap->can_target_feature)(cave_feat[y][x]));
}

/*
 * Determine if a trap makes a reasonable target
 */
static bool target_able_trap(int y, int x)
{
	/* Must be on line of fire */
	if (!player_can_fire_bold(y, x)) return (FALSE);

	/* Only player traps allowed. Ignore monster traps and glyphs */
	if (!cave_player_trap_bold(y, x)) return (FALSE);

	/* Ignore hidden traps */
	if (x_list[cave_x_idx[y][x]].x_flags & (EF1_HIDDEN)) return (FALSE);

	/* Known player traps are okay */
	return (TRUE);
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
	for (y = Term->offset_y; y < Term->offset_y + SCREEN_HGT; y++)
	{
		for (x = Term->offset_x; x < Term->offset_x + SCREEN_WID; x++)
		{
			bool do_continue = FALSE;

			/* Check overflow */
			if (temp_n >= TEMP_MAX) continue;

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_interactive_accept(y, x)) continue;

			/* Special mode */
			if (mode & (TARGET_KILL))
			{
				/* Must contain a monster */
				if (!(cave_m_idx[y][x] > 0)) do_continue = TRUE;

				/* Must be a targettable monster */
			 	if (!target_able(cave_m_idx[y][x])) do_continue = TRUE;
			}

			/* Don't continue on the trap exception*/
			if ((mode & (TARGET_TRAP)) && target_able_trap(y, x)) do_continue = FALSE;

			if (do_continue) continue;

			/*
			 * Hack - don't go over redundant elemental terrain \
			 * (since we have large lakes and pools of the same terrain)
			 */
			if ((p_ptr->target_row > 0) || (p_ptr->target_col > 0))
			{
				if (cave_feat[p_ptr->target_row][p_ptr->target_col] == cave_feat[y][x])
				{
					if (cave_ff3_match(y, x, TERRAIN_MASK)) continue;
				}
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
static int target_set_interactive_aux(int y, int x, int mode, cptr info)
{
	s16b this_o_idx, next_o_idx = 0;
	s16b this_x_idx, next_x_idx = 0;

	int i;

	cptr s1, s2, s3;

	bool floored;

	u16b feat;

	int query = ESCAPE;

	char out_val[256];

	/* Repeat forever */
	while (1)
	{
		char feat_name[80];
		/* Terrain suffix for monsters and objects */
		char terrain_suffix[200];

		/* Temporary array of visible effects */
		s16b x_seen[50];
		int size_x_seen = 0;

		/* Paranoia */
		query = ' ';

		/* Default */
		s1 = "You see ";
		s2 = "";
		s3 = "on ";

		/* The player */
		if (cave_m_idx[y][x] < 0)
		{
			/* Description */
			s1 = "You are ";

			/* Preposition */
			s2 = "on ";
		}

		/* Feature (apply "mimic") */
		feat = f_info[cave_feat[y][x]].f_mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(cave_info[y][x] & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		else
		{
			/* Hack -- track the current feature */
			feature_kind_track(feat);

			/* Window stuff */
			p_ptr->window |= (PW_FEATURE);
		}

		/* Pick a prefix */
		if (*s2 && (!feat_ff1_match(feat, FF1_MOVE) ||
			!feat_ff1_match(feat, FF1_LOS) ||
			feat_ff1_match(feat, FF1_SHOP | FF1_DOOR) ||
			feat_ff2_match(feat, FF2_SHALLOW | FF2_DEEP) ||
			feat_ff3_match(feat, FF3_NEED_TREE)))
		{
			s3 = "in ";
		}

		/* Get a default name */
		if (feat <= FEAT_NONE)
		{
			my_strcpy(feat_name, "an unknown grid", sizeof(feat_name));
		}
		/* Get the real name */
		else
		{
			feature_desc(feat_name, sizeof (feat_name), feat, TRUE, FALSE);
		}

		/* List all effect in the grid */
		for (this_x_idx = cave_x_idx[y][x]; this_x_idx; this_x_idx = next_x_idx)
		{
			effect_type *x_ptr;

			/* Get the effect */
			x_ptr = &x_list[this_x_idx];

			/* Get the next effect */
			next_x_idx = x_ptr->next_x_idx;

			/* Describe it, if not hidden */
			if (!(x_ptr->x_flags & (EF1_HIDDEN)) && x_ptr->x_f_idx)
			{
				/* Check for available space */
				if (size_x_seen < N_ELEMENTS(x_seen))
				{
					x_seen[size_x_seen++] = x_ptr->x_f_idx;
				}
			}
		}

		/* Prepare the terrain suffix for monsters and objects */
		my_strcpy(terrain_suffix, format(" %s%s", s3, feat_name), sizeof(terrain_suffix));

		/* Concat the collected effect names */
		for (i = 0; i < size_x_seen; i++)
		{
			char x_name[80];

			/* Obtain an object description */
			feature_desc(x_name, sizeof(x_name), x_seen[i], TRUE, TRUE);

			/* First effect */
			if (i == 0)
			{
				if ((feat == FEAT_NONE) || !feat_ff1_match(feat, FF1_MOVE) ||
					cave_any_trap_bold(y, x))
				{
					/* Basic info */
					my_strcat(terrain_suffix, format(" with %s", x_name),
						sizeof(terrain_suffix));
				}
				else
				{
					/* Basic info */
					my_strcat(terrain_suffix, format(" beneath %s", x_name),
						sizeof(terrain_suffix));
				}
			}

			/* Basic info */
			else if (i < (size_x_seen - 1))
			{
				my_strcat(terrain_suffix, format(", %s", x_name), sizeof(terrain_suffix));
			}

			/* Basic info */
			else
			{
				my_strcat(terrain_suffix, format(" and %s", x_name), sizeof(terrain_suffix));
			}
		}

		/* Ignore the terrain suffix if certain things happen */
		if ((size_x_seen == 0) && (feat <= FEAT_FLOOR))
		{
			terrain_suffix[0] = '\0';
		}

		/* Hack -- hallucination */
		if (p_ptr->image)
		{
			cptr name = "something strange";

			/* Display a message */
			if (p_ptr->wizard)
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s [%s] (%d:%d)", s1, s2, name, info, y, x);
			}
			else
			{
				strnfmt(out_val, sizeof(out_val),
				        "%s%s%s [%s]", s1, s2, name, info);
			}

			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\n') && (query != '\r')) break;

			/* Repeat forever */
			continue;
		}

		/* Actual monsters */
		if (cave_m_idx[y][x] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Visible */
			if (m_ptr->ml)
			{


				/*Assume no recall*/
				bool recall = FALSE;

				char m_name[80];

				if (m_ptr->mimic_k_idx)
				{
					/*get the description*/
					mimic_desc_object(m_name, sizeof(m_name), m_ptr->mimic_k_idx);
				}

				else
				{
					/* Get the monster name ("a kobold") */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0x08);

					/* Hack -- track this monster race */
					monster_race_track(m_ptr->r_idx);

					/* Hack -- health bar for this monster */
					health_track(cave_m_idx[y][x]);

					/*Track the feature*/
					feature_kind_track(cave_feat[y][x]);

					/* Window stuff */
					p_ptr->window |= (PW_FEATURE);

					/* Hack -- handle stuff */
					handle_stuff();

				}

				/* Interact */
				while (1)
				{
					/* Recall, but not mimics */
					if ((recall) && (!(m_ptr->mimic_k_idx)))
					{
						/* Save screen */
						screen_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format(" [r,%s]", info));

						/* Command */
						query = inkey();

						/* Load screen */
						screen_load();
					}

					/* Normal */
					else
					{
						/* Basic info */
						strnfmt(out_val, sizeof(out_val),
							"%s%s%s", s1, s2, m_name);

						/* Describe the monster, unless a mimic */
						if (!(m_ptr->mimic_k_idx))
						{
							char buf[80];

							look_mon_desc(buf, sizeof(buf), cave_m_idx[y][x]);

							/* Monster state, terrain suffix, options  */
							my_strcat(out_val, format(" (%s)%s [r,%s]",
								buf, terrain_suffix, info),
								sizeof(out_val));
						}

						/* Mimics */
						else
						{
							/* Terrain suffix, options */
							my_strcat(out_val,
								format("%s [I,%s]", terrain_suffix,
								info), sizeof(out_val));
						}

						/* Wizards want coordinates */
						if (p_ptr->wizard)
						{
							my_strcat(out_val, format(" (%d:%d)", y, x),
								sizeof(out_val));
						}

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);

						/* Command */
						query = inkey();
					}

					/* Handle fake object recall */
					if (m_ptr->mimic_k_idx)
					{
						object_type body;
						object_type *o_ptr = &body;

						/* Validate input first */
						if (query != 'I') break;

						/* Paranoia */
						object_wipe(o_ptr);

						/* Prepare */
						object_prep(o_ptr, m_ptr->mimic_k_idx);

						/* Fake history */
						object_history(o_ptr, ORIGIN_FLOOR, 0);

						/* Clear prompt. Place cursor */
						prt("", 0, 0);

						/* Show the fake info - EXPERIMENTAL */
						object_info_screen(o_ptr);
					}
					/* Regular monsters */
					else
					{
						/* Normal commands */
						if (query != 'r') break;

						/* Toggle recall */
						recall = !recall;
					}
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

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
					char o_name[80];

					object_type *o_ptr;

					/* Get the object */
					o_ptr = &o_list[this_o_idx];

					/* Get the next object */
					next_o_idx = o_ptr->next_o_idx;

					/*Don't let the player see certain objects (used for vault treasure)*/
					if ((o_ptr->ident & (IDENT_HIDE_CARRY)) && (!p_ptr->wizard) &&
						(!cheat_peek))	 continue;

					/* Obtain an object description */
					object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

					/* Describe the object */
					strnfmt(out_val, sizeof(out_val),
						"%s%s%s [%s]", s1, s2, o_name, info);

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Stop on everything but "return"/"space" */
					if ((query != '\n') && (query != '\r') && (query != ' ')) break;

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


		/* Assume not floored */
		floored = FALSE;

		/* Scan all objects in the grid */
		if (easy_floor)
		{
			int floor_list[MAX_FLOOR_STACK];
			int floor_num;

			/* Scan for floor objects */
			floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, y, x, 0x02);

			/* Actual pile */
			if (floor_num > 1)
			{
				/* Floored */
				floored = TRUE;

				/* Describe */
				while (1)
				{
					/* Basic info */
					strnfmt(out_val, sizeof(out_val),
						"%s%sa pile of %d objects%s [r,%s]", s1, s2,
						floor_num, terrain_suffix, info);

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Display objects */
					if (query == 'r')
					{
						/* Save screen */
						screen_save();

						/* Display */
						show_floor(floor_list, floor_num);

						/* Describe the pile */
						prt(out_val, 0, 0);
						query = inkey();

						/* Load screen */
						screen_load();

						/* Continue on 'r' only */
						if (query == 'r') continue;
					}

					/* Done */
					break;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

				/* Sometimes stop at "space" key */
				if ((query == ' ') && !(mode & (TARGET_LOOK))) break;

				/* Change the intro */
				s1 = "It is ";

				/* Preposition */
				s2 = "on ";
			}
		}

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
			if (o_ptr->marked)
			{
				char o_name[80];

				/* Obtain an object description */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

				/* Basic info */
				strnfmt(out_val, sizeof(out_val), "%s%s%s%s [I,%s]",
					s1, s2, o_name, terrain_suffix, info);

				/* Wizards want coordinates */
				if (p_ptr->wizard)
				{
					my_strcat(out_val, format(" (%d:%d)", y, x),
						sizeof(out_val));
				}

				/* Show object. Handle object recall */
				while (TRUE)
				{
					/* Print the prompt */
					prt(out_val, 0, 0);

					/* Move cursor to that location */
					move_cursor_relative(y, x);

					/* Read input key */
					query = inkey();

					/* No object recall */
					if (query != 'I') break;

					/* Object recall. Clear the first line */
					prt("", 0, 0);

					/* Do it */
					object_info_screen(o_ptr);
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;

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

		/* Display terrain */
		if (TRUE)
		{
			u16b temp_feat;
			bool enable_recall;
			bool show_recall = FALSE;
			char temp_name[80];

			/*
			 * Display terrain and effects
			 */
			for (i = 0; i <= size_x_seen; i++)
			{
				/* Hack - This is the mark for the feature stored in cave_feat */
				if (i == size_x_seen)
				{
				       	temp_feat = feat;

					/* Just copy the feature name */
					my_strcpy(temp_name, feat_name, sizeof(temp_name));
				}
				/* Any other value is an effect stored x_list */
				else
				{
					temp_feat = x_seen[i];

					/* Get the effect's name */
					feature_desc(temp_name, sizeof(temp_name), temp_feat, TRUE, TRUE);
				}

				/* Don't display feature recall if the grid is unknown */
				enable_recall = (temp_feat != FEAT_NONE);

				/* Handle recall */
				while (TRUE)
				{
					/* Handle recall mode */
					if (show_recall && enable_recall)
					{
						/* Save screen */
						screen_save();

						/* Recall feature on screen */
						screen_feature_roff(temp_feat);
					}

					/* Display a message */
					strnfmt(out_val, sizeof(out_val),
						"%s%s%s [%s%s]%s", s1, s2, temp_name,
						(enable_recall ? "r,": ""), info,
						(i < size_x_seen) ? " (more)": "");

					/* Wizards want coordinates */
					if (p_ptr->wizard)
					{
						my_strcat(out_val, format(" (%d:%d)", y, x), sizeof(out_val));
					}

					/*Track this feature*/
					feature_kind_track(temp_feat);

					/* Hack -- handle stuff */
					handle_stuff();

					prt(out_val, 0, 0);
					move_cursor_relative(y, x);
					query = inkey();

					/* Load screen if necessary */
					if (show_recall && enable_recall)
					{
						screen_load();
					}

					/* Stop on everything but the recall command, if enabled */
					if (!enable_recall || (query != 'r')) break;

					/* Toggle recall */
					show_recall = !show_recall;
				}

				/* Stop on everything but "return"/"space" */
				if ((query != '\n') && (query != '\r') && (query != ' ')) break;
			}
		}

		/* Hack -- handle stuff */
		handle_stuff();

		/* Stop on everything but "return" */
		if ((query != '\n') && (query != '\r')) break;

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
 */
static int draw_path(char *c, byte *a, int y1, int x1, int y2, int x2)
{
	int i;
	int max;
	bool on_screen;

	/* Find the path. */
	max = project_path(MAX_RANGE, y1, x1, &y2, &x2, PROJECT_THRU);

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
		int y = GRID_Y(path_g[i]);
		int x = GRID_X(path_g[i]);
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
		if (cave_m_idx[y][x] && mon_list[cave_m_idx[y][x]].ml)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];

			/*mimics act as objects*/
			if (m_ptr->mimic_k_idx) colour = TERM_YELLOW;
			else colour = TERM_L_RED;
		}

		/* Known objects are yellow. */
		else if (cave_o_idx[y][x] && o_list[cave_o_idx[y][x]].marked)
		{

			colour = TERM_YELLOW;
    	}

		/* Known walls are blue. */
		else if (!cave_project_bold(y,x) &&
				((cave_info[y][x] & (CAVE_MARK)) ||	player_can_see_bold(y,x)))
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
 */
static void load_path(int max, char *c, byte *a)
{
	int i;
	for (i = 0; i < max; i++)
	{
		if (!panel_contains(GRID_Y(path_g[i]), GRID_X(path_g[i]))) continue;

		move_cursor_relative(GRID_Y(path_g[i]), GRID_X(path_g[i]));

		(void)Term_addch(a[i], c[i]);
	}

	(void)Term_fresh();
}

/*
 * Handle "target" and "look".
 *
 * Note that this code can be called from "get_aim_dir()".
 *
 * Currently, when "flag" is true, that is, when
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

	bool done = FALSE;

	bool flag = TRUE;

	char query;

	char info[80];

	/* These are used for displaying the path to the target */
	char path_char[MAX_RANGE];
	byte path_attr[MAX_RANGE];
	int max;

	/* Cancel target */
	target_set_monster(0);

	/* Cancel tracking playtesting*/
	/* health_track(0); */

	  /* All grids are selectable */
	if (mode & (TARGET_GRID))
	{
		/* Disable other modes */
		mode &= ~(TARGET_LOOK | TARGET_KILL | TARGET_TRAP);

		/* Disable interesting grids */
		flag = FALSE;
	}


	/* Prepare the "temp" array */
	target_set_interactive_prepare(mode);

	/* Start near the player */
	m = 0;

	/* Interact */
	while (!done)
	{
		max = 0;

		/* Interesting grids */
		if (flag && temp_n)
		{
			y = temp_y[m];
			x = temp_x[m];

			/* Allow targets for monsters....or traps, if applicable */
			if (((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x])) ||
				((mode & (TARGET_TRAP)) && target_able_trap(y, x)))
			{
				strcpy(info, "q,t,p,o,+,-,<dir>");
			}



			/* Dis-allow target */
			else
			{
				strcpy(info, "q,p,o,+,-,<dir>");
			}

			/* Adjust panel if needed */
			if (adjust_panel(y, x))
			{
				/* Handle stuff */
				handle_stuff();
			}

			/* Draw the path in "target" mode. If there is one */
			if (mode & (TARGET_KILL))
			  max = draw_path (path_char, path_attr, py, px, y, x);

			/* Describe and Prompt */
			query = target_set_interactive_aux(y, x, mode, info);

			/* Remove the path */
			if (max > 0)	load_path (max, path_char, path_attr);

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
					/* Recenter around player */
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;
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
					else if ((mode & (TARGET_TRAP)) && target_able_trap(y, x))
 					{
 						target_set_location(y, x);
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
				int old_y = temp_y[m];
				int old_x = temp_x[m];

				/* Find a new monster */
				i = target_pick(old_y, old_x, ddy[d], ddx[d]);

				/* Scroll to find interesting grid */
				if (i < 0)
				{
					int old_wy = Term->offset_y;
					int old_wx = Term->offset_x;

					/* Change if legal */
					if (change_panel(d))
					{
						/* Recalculate interesting grids */
						target_set_interactive_prepare(mode);

						/* Find a new monster */
						i = target_pick(old_y, old_x, ddy[d], ddx[d]);

						/* Restore panel if needed */
						if ((i < 0) && modify_panel(Term, old_wy, old_wx))
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
			if (!(mode & (TARGET_GRID)))
			{
				strcpy(info, "q,t,p,m,+,-,<dir>");
			}

			/* Disable monster selection */
			else
			{
				strcpy(info, "q,t,p,+,-,<dir>");
			}

			/* Draw the path in "target" mode. If there is one */
			if (mode & (TARGET_KILL))
			  max = draw_path (path_char, path_attr, py, px, y, x);

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_interactive_aux(y, x, mode | TARGET_LOOK, info);

			/* Remove the path */
			if (max > 0)  load_path (max, path_char, path_attr);

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
					/* Recenter around player */
					verify_panel();

					/* Handle stuff */
					handle_stuff();

					y = py;
					x = px;
				}

				case 'o':
				{
					break;
				}

				case 'm':
				{
					/* Monster selection is disabled */
					if (mode & (TARGET_GRID)) break;

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

				case 't':
				case '5':
				case '0':
				case '.':
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
				int dungeon_hgt = p_ptr->cur_map_hgt;
				int dungeon_wid = p_ptr->cur_map_wid;

				/* Move */
				x += ddx[d];
				y += ddy[d];

				/* Slide into legality */
				if (x >= dungeon_wid - 1) x--;
				else if (x <= 0) x++;

				/* Slide into legality */
				if (y >= dungeon_hgt - 1) y--;
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
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Recenter around player */
	verify_panel();

	/* Handle stuff */
	handle_stuff();

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
bool get_aim_dir(int *dp, bool target_trap)
{
	int dir;

	char ch;

	cptr p;

	if (repeat_pull(dp))
	{
		/* Verify */
		if (!p_ptr->confused && ((*dp != 5) || target_okay()))
		{
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}


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
		if (!get_com(p, &ch)) break;

		/* Analyze */
		switch (ch)
		{
			/* Set new target, use target if legal */
			case '*':
			{
				int mode = TARGET_KILL;
				if (target_trap) mode |= TARGET_TRAP;

				if (target_set_interactive(mode)) dir = 5;
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



	repeat_push(dir);



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



	if (repeat_pull(dp))
	{
		return (TRUE);
	}


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



	repeat_push(dir);



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



