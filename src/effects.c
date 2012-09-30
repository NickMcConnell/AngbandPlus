/* File: effects.c */

/* Purpose: effects of various "objects" */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Set "p_ptr->blind", notice observable changes
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
			msgf("You are blind!");
			notice = TRUE;

			chg_virtue(V_ENLIGHTEN, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blind)
		{
			msgf("You can see again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blind = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Fully update the visuals - hack set torch to be radius 0 */
	p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_TORCH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Redraw the "blind" */
	p_ptr->redraw |= (PR_BLIND);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
			msgf("You are confused!");
			notice = TRUE;

			chg_virtue(V_HARMONY, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->confused)
		{
			msgf("You feel less confused now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->confused = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You are poisoned!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->poisoned)
		{
			msgf("You are no longer poisoned.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->poisoned = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You are terrified!");
			notice = TRUE;

			chg_virtue(V_VALOUR, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->afraid)
		{
			msgf("You feel bolder now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->afraid = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You are paralyzed!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->paralyzed)
		{
			msgf("You can move again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->paralyzed = v;

	/* Redraw status bar + message*/
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Redraw the state */
	p_ptr->redraw |= (PR_STATE | PR_SPEED);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note that we must redraw the map when hallucination changes.
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
			msgf("Oh, wow! Everything looks so cosmic now!");
			notice = TRUE;

			/* Update the monster vis window */
			p_ptr->window |= PW_VISIBLE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->image)
		{
			msgf("You can see clearly again.");
			notice = TRUE;

			/* Update the monster vis window */
			p_ptr->window |= PW_VISIBLE;
		}
	}

	/* Use the value */
	p_ptr->image = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

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
			msgf("You feel yourself moving faster!");
			notice = TRUE;

			chg_virtue(V_PATIENCE, -1);
			chg_virtue(V_DILIGENCE, 1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->fast)
		{
			msgf("You feel yourself slow down.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->fast = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel yourself moving slower!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->slow)
		{
			msgf("You feel yourself speed up.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->slow = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("Your skin turns to stone.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			msgf("Your skin returns to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shield = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel righteous!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->blessed)
		{
			msgf("The prayer has expired.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->blessed = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel like a hero!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->hero)
		{
			msgf("The heroism wears off.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->hero = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

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
			msgf("You feel like a killing machine!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shero)
		{
			msgf("You feel less Berserk.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->shero = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate hitpoints */
	p_ptr->update |= (PU_HP);

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
			msgf("You feel safe from evil!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
			msgf("You no longer feel safe from evil.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->protevil = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->wraith_form", notice observable changes
 */
bool set_wraith_form(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->wraith_form)
		{
			msgf
				("You leave the physical world and turn into a wraith-being!");
			notice = TRUE;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->wraith_form)
		{
			msgf("You feel opaque.");
			notice = TRUE;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Use the value */
	p_ptr->wraith_form = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

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
			msgf("Invulnerability!");
			notice = TRUE;

			chg_virtue(V_TEMPERANCE, -5);
			chg_virtue(V_HONOUR, -5);
			chg_virtue(V_SACRIFICE, -5);
			chg_virtue(V_VALOUR, -10);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
			msgf("The invulnerability wears off.");
			notice = TRUE;

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}

	/* Use the value */
	p_ptr->invuln = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_esp", notice observable changes
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
			msgf("You feel your consciousness expand!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_esp)
		{
			msgf("Your consciousness contracts again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_esp = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_invis", notice observable changes
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
			msgf("Your eyes feel very sensitive!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_invis)
		{
			msgf("Your eyes feel less sensitive.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_invis = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->tim_infra", notice observable changes
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
			msgf("Your eyes begin to tingle!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_infra)
		{
			msgf("Your eyes stop tingling.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_infra = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Update the monsters */
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
			msgf("You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_acid)
		{
			msgf("You feel less resistant to acid.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_acid = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_elec)
		{
			msgf("You feel less resistant to electricity.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_elec = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_fire)
		{
			msgf("You feel less resistant to fire.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_fire = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_cold)
		{
			msgf("You feel less resistant to cold.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_cold = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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
			msgf("You feel resistant to poison!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_pois)
		{
			msgf("You feel less resistant to poison.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_pois = v;

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(FALSE);

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

	/*
	 * Golems cannot be stunned when they are being used as a
	 * "training" class.  However, when they are being used in
	 * a hard game - they lose this advantage.  (Golems are
	 * designed for newbies - not scummers.)
	 */
	if ((p_ptr->prace == RACE_GOLEM) &&
		!(ironman_shops || ironman_downward || ironman_hard_quests ||
		  ironman_empty_levels || ironman_rooms || ironman_nightmare ||
		  ironman_deep_quests))
	{
		v = 0;
	}

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
			case 1:
			{
				/* Stun */
				msgf("You have been stunned.");
				break;
			}

			case 2:
			{
				/* Heavy stun */
				msgf("You have been heavily stunned.");
				break;
			}

			case 3:
			{
				/* Knocked out */
				msgf("You have been knocked out.");
				break;
			}
		}

		/*
		 * XXX XXX Hack -
		 * Mindcrafters cannot get this effect when
		 * casting a spell.  It really doesn't make sense.
		 * Unfortunately, there is no way to know if this is
		 * the case... so it is disabled in all circumstances
		 * if you are a Mindcrafter.  (Perhaps it can be
		 * explained away by their "superior mental skills" or
		 * something...
		 */
		if ((randint1(1000) < v || one_in_(16)) &&
			(!(p_ptr->pclass == CLASS_MINDCRAFTER)))
		{
			msgf("A vicious blow hits your head.");
			if (one_in_(3))
			{
				if (!p_ptr->sustain_int) (void)do_dec_stat(A_INT);
				if (!p_ptr->sustain_wis) (void)do_dec_stat(A_WIS);
			}
			else if (one_in_(2))
			{
				if (!p_ptr->sustain_int) (void)do_dec_stat(A_INT);
			}
			else
			{
				if (!p_ptr->sustain_wis) (void)do_dec_stat(A_WIS);
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
			case 0:
			{
				/* None */
				msgf("You are no longer stunned.");
				if (disturb_state) disturb(FALSE);
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
	if (disturb_state) disturb(FALSE);

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

	if (p_ptr->prace == RACE_GOLEM ||
		p_ptr->prace == RACE_SKELETON ||
		p_ptr->prace == RACE_SPECTRE ||
		(p_ptr->prace == RACE_ZOMBIE && p_ptr->lev > 11))
		v = 0;

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
			case 1:
			{
				/* Graze */
				msgf("You have been given a graze.");
				break;
			}

			case 2:
			{
				/* Light cut */
				msgf("You have been given a light cut.");
				break;
			}

			case 3:
			{
				/* Bad cut */
				msgf("You have been given a bad cut.");
				break;
			}

			case 4:
			{
				/* Nasty cut */
				msgf("You have been given a nasty cut.");
				break;
			}

			case 5:
			{
				/* Severe cut */
				msgf("You have been given a severe cut.");
				break;
			}

			case 6:
			{
				/* Deep gash */
				msgf("You have been given a deep gash.");
				break;
			}

			case 7:
			{
				/* Mortal wound */
				msgf("You have been given a mortal wound.");
				break;
			}
		}

		/* Notice */
		notice = TRUE;

		if (randint1(1000) < v || one_in_(16))
		{
			if (!p_ptr->sustain_chr)
			{
				msgf("You have been horribly scarred.");

				(void)do_dec_stat(A_CHR);
			}
		}
	}

	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			case 0:
			{
				/* None */
				msgf("You are no longer bleeding.");
				if (disturb_state) disturb(FALSE);
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
	if (disturb_state) disturb(FALSE);

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

	if (old_aux < 1 && new_aux > 0)
		chg_virtue(V_PATIENCE, 2);
	else if (old_aux < 3 && (old_aux != new_aux))
		chg_virtue(V_PATIENCE, 1);
	if (old_aux == 2)
		chg_virtue(V_TEMPERANCE, 1);
	if (old_aux == 0)
		chg_virtue(V_TEMPERANCE, -1);

	/* Food increase */
	if (new_aux > old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			case 1:
			{
				/* Weak */
				msgf("You are still weak.");
				break;
			}

			case 2:
			{
				/* Hungry */
				msgf("You are still hungry.");
				break;
			}

			case 3:
			{
				/* Normal */
				msgf("You are no longer hungry.");
				break;
			}

			case 4:
			{
				/* Full */
				msgf("You are full!");
				break;
			}

			case 5:
			{
				/* Bloated */
				msgf("You have gorged yourself!");

				chg_virtue(V_HARMONY, -1);
				chg_virtue(V_PATIENCE, -1);
				chg_virtue(V_TEMPERANCE, -2);
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
			case 0:
			{
				/* Fainting / Starving */
				msgf("You are getting faint from hunger!");
				break;
			}

			case 1:
			{
				/* Weak */
				msgf("You are getting weak from hunger!");
				break;
			}

			case 2:
			{
				/* Hungry */
				msgf("You are getting hungry.");
				break;
			}

			case 3:
			{
				/* Normal */
				msgf("You are no longer full.");
				break;
			}

			case 4:
			{
				/* Full */
				msgf("You are no longer gorged.");
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
	if (disturb_state) disturb(FALSE);

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
 * Increases a stat by one randomized level             -RAK-
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
bool inc_stat(int stat)
{
    int value, gain;
    int min_gain, max_gain;

	int cap = stat_cap(stat);

	/* Then augment the current/max stat */
	value = p_ptr->stat_cur[stat];

	/* Cannot go above limit */
	if (value < cap)
    {
        min_gain = (cap - value) / 6;
        max_gain = (cap - value) / 3;

        if (min_gain > 5)  min_gain = 5;
        if (min_gain < 1)  min_gain = 1;
        if (max_gain > 20) max_gain = 20;
        if (max_gain < 1)  max_gain = 1;

        gain = rand_range(min_gain, max_gain);
        value += gain;

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
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults.  -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(int stat, int amount, int permanent)
{
	int cur, max, loss, same, res = FALSE;


	/* Acquire current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
	if (cur > 30)
	{
		/* Handle "low" values */
		if (cur <= 180)
		{
			if (amount > 90) cur -= 10;
			if (amount > 50) cur -= 10;
			if (amount > 20) cur -= 10;
			cur -= 10;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((cur - 180) / 2 + 1) / 2 + 1);

			/* Paranoia */
			if (loss < 1) loss = 1;

			/* Randomize the loss */
			loss = (rand_range(loss, loss * 2) * amount) / 100;

			/* Maximal loss */
			if (loss < amount / 2) loss = amount / 2;

			/* Lose some points */
			cur = cur - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (cur < 180) cur = (amount <= 20) ? 180 : 170;
		}

		/* Prevent illegal values */
		if (cur < 30) cur = 30;

		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 30))
	{
		chg_virtue(V_SACRIFICE, 1);
		if (stat == A_WIS || stat == A_INT)
			chg_virtue(V_ENLIGHTEN, -2);

		/* Handle "low" values */
		if (max <= 180)
		{
			if (amount > 90) max -= 10;
			if (amount > 50) max -= 10;
			if (amount > 20) max -= 10;
			max -= 10;
		}

		/* Handle "high" values */
		else
		{
			/* Hack -- Decrement by a random amount between one-quarter */
			/* and one-half of the stat bonus times the percentage, with a */
			/* minimum damage of half the percentage. -CWS */
			loss = (((max - 180) / 2 + 1) / 2 + 1);
			loss = (rand_range(loss, loss * 2) * amount) / 100;
			if (loss < amount / 2) loss = amount / 2;

			/* Lose some points */
			max = max - loss;

			/* Hack -- Only reduce stat to 17 sometimes */
			if (max < 180) max = (amount <= 20) ? 180 : 170;
		}

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
bool res_stat(int stat)
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
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		chg_virtue(V_CHANCE, -1);
		if ((num > 0) && (p_ptr->chp < (p_ptr->mhp / 3)))
			chg_virtue(V_TEMPERANCE, 1);

		/* Gain hitpoints */
		p_ptr->chp += num;

		/* Enforce maximum */
		if (p_ptr->chp >= p_ptr->mhp)
		{
			p_ptr->chp = p_ptr->mhp;
			p_ptr->chp_frac = 0;
		}

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Heal 0-4 */
		if (num < 5)
		{
			msgf("You feel a little better.");
		}

		/* Heal 5-14 */
		else if (num < 15)
		{
			msgf("You feel better.");
		}

		/* Heal 15-34 */
		else if (num < 35)
		{
			msgf("You feel much better.");
		}

		/* Heal 35+ */
		else
		{
			msgf("You feel very good.");
		}

		/* Notice */
		return (TRUE);
	}

	/* Ignore */
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
 * Lose a "point"
 */
bool do_dec_stat(int stat)
{
	bool sust = FALSE;

	/* Access the "sustain" */
	switch (stat)
	{
		case A_STR:
		{
			if (p_ptr->sustain_str) sust = TRUE;
			break;
		}
		case A_INT:
		{
			if (p_ptr->sustain_int) sust = TRUE;
			break;
		}
		case A_WIS:
		{
			if (p_ptr->sustain_wis) sust = TRUE;
			break;
		}
		case A_DEX:
		{
			if (p_ptr->sustain_dex) sust = TRUE;
			break;
		}
		case A_CON:
		{
			if (p_ptr->sustain_con) sust = TRUE;
			break;
		}
		case A_CHR:
		{
			if (p_ptr->sustain_chr) sust = TRUE;
			break;
		}
	}

	/* Sustain */
	if (sust && !(ironman_nightmare && one_in_(13)))
	{
		/* Message */
		msgf("You feel %s for a moment, but the feeling passes.",
				   desc_stat_neg[stat]);

		/* Notice effect */
		return (TRUE);
	}

	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, (ironman_nightmare && !one_in_(13))))
	{
		/* Message */
		msgf("You feel very %s.", desc_stat_neg[stat]);

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
		msgf("You feel less %s.", desc_stat_neg[stat]);

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
		if (stat == A_WIS)
		{
			chg_virtue(V_ENLIGHTEN, 1);
			chg_virtue(V_FAITH, 1);
		}
		else if (stat == A_INT)
		{
			chg_virtue(V_KNOWLEDGE, 1);
			chg_virtue(V_ENLIGHTEN, 1);
		}
		else if (stat == A_CON)
			chg_virtue(V_VITALITY, 1);

		/* Message */
		msgf("Wow!  You feel very %s!", desc_stat_pos[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Restoration worked */
	if (res)
	{
		/* Message */
		msgf("You feel less %s.", desc_stat_neg[stat]);

		/* Notice */
		return (TRUE);
	}

	/* Nothing obvious */
	return (FALSE);
}


/*
 * Restores any drained experience
 */
bool restore_level(void)
{
	/* Restore experience */
	if (p_ptr->exp < p_ptr->max_exp)
	{
		/* Message */
		msgf("You feel your life energies returning.");

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
 * Forget everything
 */
bool lose_all_info(void)
{
	int i, k;

	object_type *o_ptr;

	chg_virtue(V_KNOWLEDGE, -5);
	chg_virtue(V_ENLIGHTEN, -5);

	/* Forget info about equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Allow "protection" if know all the flags... */
		if (object_known_full(o_ptr)) continue;

		/* Remove "default inscriptions" */
		o_ptr->feeling = FEEL_NONE;

		/* Hack -- Clear the "empty" flag */
		o_ptr->info &= ~(OB_EMPTY);

		/* Hack -- Clear the "known" flag */
		o_ptr->info &= ~(OB_KNOWN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->info &= ~(OB_SENSE);
	}

	/* Forget info about objects */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		/* Allow "protection" if know all the flags... */
		if (object_known_full(o_ptr)) continue;

		/* Remove "default inscriptions" */
		o_ptr->feeling = FEEL_NONE;

		/* Hack -- Clear the "empty" flag */
		o_ptr->info &= ~(OB_EMPTY);

		/* Hack -- Clear the "known" flag */
		o_ptr->info &= ~(OB_KNOWN);

		/* Hack -- Clear the "felt" flag */
		o_ptr->info &= ~(OB_SENSE);
	}
	OBJ_ITT_END;

	/* Hack - Remove all knowledge about objects */

	/* Scan the object kinds */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Forget flavored items, with saving throw */
		if (k_ptr->flavor && one_in_(p_ptr->skill_sav))
		{
			/* Forget knowledge */
			k_ptr->aware = FALSE;
			k_ptr->tried = FALSE;
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->lev, 5);
	bool Nasty_effect = (one_in_(5));

	if (!(wounds || hit_p || Nasty_effect)) return;

	msgf("Your wounds are polymorphed into less serious ones.");
	(void)hp_player(change);
	if (Nasty_effect)
	{
		msgf("A new wound was created!");
		take_hit(change / 2, "a polymorphed wound");
		(void)set_cut(change);
	}
	else
	{
		(void)set_cut(p_ptr->cut - (change / 2));
	}
}


void do_poly_self(void)
{
	int i;
	int power = p_ptr->lev;

	msgf("You feel a change coming over you...");

	chg_virtue(V_CHANCE, 1);

	if ((power > randint0(20)) && one_in_(3))
	{
		char effect_msg[80] = "";
		int old_race, new_race, expfact, goalexpfact;

		/* Some form of racial polymorph... */
		power -= 10;

		if ((power > randint0(5)) && one_in_(4))
		{
			/* sex change */
			power -= 2;

			if (p_ptr->psex == SEX_MALE)
			{
				p_ptr->psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->psex];
				strnfmt(effect_msg, 80, "female ");
			}
			else
			{
				p_ptr->psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->psex];
				strnfmt(effect_msg, 80, "male ");
			}
		}

		if ((power > randint0(30)) && one_in_(5))
		{
			int tmp = 0;

			/* Harmful deformity */
			power -= 15;

			while (tmp < A_MAX)
			{
				if (one_in_(2))
				{
					(void)dec_stat(tmp, rand_range(6, 12), one_in_(3));
					power -= 1;
				}
				tmp++;
			}

			/* Deformities are discriminated against! */
			(void)dec_stat(A_CHR, randint1(6), TRUE);

			if (effect_msg[0])
			{
				strnfmt(effect_msg, 80, "deformed %s ", effect_msg);
			}
			else
			{
				strnfmt(effect_msg, 80, "deformed ");
			}
		}

		while ((power > randint0(20)) && one_in_(10))
		{
			/* Polymorph into a less mutated form */
			power -= 10;

			if (!lose_mutation(0))
				msgf("You feel oddly normal.");
		}

		/*
		 * Restrict the race choices by exp penalty so
		 * weak polymorph always means weak race
		 */
		if (power < 0)
			goalexpfact = 100;
		else
			goalexpfact = 100 + 3 * randint0(power);

		do
		{
			new_race = randint0(MAX_RACES);
			expfact = race_info[new_race].r_exp;
		}
		while ((new_race == p_ptr->prace) && (expfact > goalexpfact));

		if (!effect_msg[0])
		{
			msgf("You turn into a%s %s!",
					   (((new_race == RACE_AMBERITE) ||
						 (new_race == RACE_ELF) ||
						 (new_race == RACE_IMP)) ? "n" : ""),
					   race_info[new_race].title);
		}
		else
		{
			msgf("You turn into a %s%s!", effect_msg,
					   race_info[new_race].title);
		}

		chg_virtue(V_CHANCE, 2);

		old_race = p_ptr->prace;
		p_ptr->prace = new_race;
		rp_ptr = &race_info[p_ptr->prace];

		/* Adjust the stats */
		for (i = 0; i < A_MAX; i++)
		{
			int change;

			/* Calculate the difference between the races */
			change = rp_ptr->r_adj[i] - race_info[old_race].r_adj[i];

			/* Adjust current stat */
			p_ptr->stat_cur[i] = adjust_stat(i, p_ptr->stat_cur[i], change);

			/* Adjust maximum stat */
			p_ptr->stat_max[i] = adjust_stat(i, p_ptr->stat_max[i], change);
		}

		/* Experience factor */
		p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

		/* Calculate the height/weight for males */
		if (p_ptr->psex == SEX_MALE)
		{
			p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
			p_ptr->wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
		}

		/* Calculate the height/weight for females */
		else if (p_ptr->psex == SEX_FEMALE)
		{
			p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
			p_ptr->wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
		}

		check_experience();
		p_ptr->max_lev = p_ptr->lev;

		p_ptr->redraw |= (PR_BASIC);

		p_ptr->update |= (PU_BONUS);

		handle_stuff();
		lite_spot(p_ptr->px, p_ptr->py);
	}

	if ((power > randint0(30)) && one_in_(6))
	{
		int tmp = 0;

		/* Abomination! */
		power -= 20;

		msgf("Your internal organs are rearranged!");
		while (tmp < A_MAX)
		{
			(void)dec_stat(tmp, rand_range(6, 12), one_in_(3));
			tmp++;
		}
		if (one_in_(6))
		{
			msgf("You find living difficult in your present form!");
			take_hit(damroll(randint1(10), p_ptr->lev), "a lethal mutation");
			power -= 10;
		}
	}

	if ((power > randint0(20)) && one_in_(4))
	{
		power -= 10;

		do_cmd_rerate();
	}

	while ((power > randint0(15)) && one_in_(3))
	{
		power -= 7;
		(void)gain_mutation(0);
	}

	if (power > randint0(5))
	{
		power -= 5;
		do_poly_wounds();
	}

	/* Note: earlier deductions may have left power < 0 already. */
	while (power > 0)
	{
		mutate_player();
		power--;
	}

	/* Hack - reset visuals so the player's tile can change */
	reset_visuals();
}


/*
 * Decreases players hit points and sets death flag if necessary
 *
 * XXX XXX XXX Invulnerability needs to be changed into a "shield"
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */
void take_hit(int damage, cptr hit_from)
{
	int old_chp = p_ptr->chp;

	bool pen_invuln = FALSE;

	char death_message[1024];

	int warning = (p_ptr->mhp * hitpoint_warn / 10);


	/* Paranoia */
	if (p_ptr->is_dead) return;

	/* Disturb */
	disturb(TRUE);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->invuln && (damage < 9000))
	{
		if (one_in_(PENETRATE_INVULNERABILITY))
		{
			pen_invuln = TRUE;
		}
		else
		{
			return;
		}
	}

	if (p_ptr->wraith_form)
	{
		damage /= 10;
		if ((damage == 0) && one_in_(10)) damage = 1;
	}

	/* Hurt the player */
	p_ptr->chp -= damage;

	/* Display the hitpoints */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Do not skip the message */
	p_ptr->skip_more = FALSE;

	if (pen_invuln)
		msgf("The attack penetrates your shield of invulnerability!");

	if (!(p_ptr->invuln) || (pen_invuln))
	{
		if (p_ptr->chp == 0)
		{
			chg_virtue(V_SACRIFICE, 1);
			chg_virtue(V_CHANCE, 2);
		}
	}

	/* Dead player */
	if (p_ptr->chp < 0)
	{
		int len;
	
		/* Sound */
		sound(SOUND_DEATH);

		chg_virtue(V_SACRIFICE, 10);

		/* Hack -- Note death */
		if (!last_words)
		{
			msgf(MSGT_DEATH, "You die.");
			message_flush();
		}
		else
		{
			if (!get_rnd_line("death.txt", 0, death_message))
				msgf(death_message);
		}

		/* Note cause of death */
		len = strnfmt(p_ptr->died_from, 80, hit_from);

		if (p_ptr->image) strnfcat(p_ptr->died_from, 80, &len, "(?)");

		/* No longer a winner */
		p_ptr->total_winner = FALSE;

		/* Leaving */
		p_ptr->leaving = TRUE;

		/* Note death */
		p_ptr->is_dead = TRUE;

		if (get_check("Dump the screen? "))
		{
			do_cmd_save_screen();
		}

		/* Dead */
		return;
	}

	/* Hitpoint warning */
	if (p_ptr->chp < warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning) bell("Low hitpoint warning!");

		sound(SOUND_WARN);

		/* Message */
		msgf(MSGT_HITPOINT_WARN, "*** LOW HITPOINT WARNING! ***");
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
		/* Gain max experience (20%) (was 10%) */
		p_ptr->max_exp += amount / 5;
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
 * Make some noise
 */
void make_noise(byte amount)
{
	int total = amount + p_ptr->noise_level;

	/* Paranoia (watching for overflow) */
	if (total > MONSTER_FLOW_DEPTH)
	{
		total = MONSTER_FLOW_DEPTH;
	}

	/* Update the flow if this gets too high */
	if (total >= 3 * MONSTER_FLOW_DEPTH / 4)
	{
		p_ptr->update |= PU_FLOW;
	}

	/* Save the new noise level */
	p_ptr->noise_level = (byte)total;
}
