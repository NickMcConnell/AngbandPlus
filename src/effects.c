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
static bool set_blind(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.blind)
		{
			msgf("You are blind!");
			notice = TRUE;

			chg_virtue(V_ENLIGHTEN, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.blind)
		{
			msgf("You can see again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.blind = v;

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
 * Increase the blind counter
 */
bool inc_blind(int v)
{
	return(set_blind(p_ptr->tim.blind + v));
}

/*
 * No more blindness
 */
bool clear_blind(void)
{
	return(set_blind(0));
}


/*
 * Set "p_ptr->confused", notice observable changes
 */
static bool set_confused(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.confused)
		{
			msgf("You are confused!");
			notice = TRUE;

			chg_virtue(V_HARMONY, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.confused)
		{
			msgf("You feel less confused now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.confused = v;

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
 * Increase the confusion counter
 */
bool inc_confused(int v)
{
	return(set_confused(p_ptr->tim.confused + v));
}


/*
 * No more confusion
 */
bool clear_confused(void)
{
	return(set_confused(0));
}


/*
 * Set "p_ptr->poisoned", notice observable changes
 */
static bool set_poisoned(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.poisoned)
		{
			msgf("You are poisoned!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.poisoned)
		{
			msgf("You are no longer poisoned.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.poisoned = v;

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
 * Increase the poison counter
 */
bool inc_poisoned(int v)
{
	return(set_poisoned(p_ptr->tim.poisoned + v));
}


/*
 * No more poison
 */
bool clear_poisoned(void)
{
	return(set_poisoned(0));
}


/*
 * Set "p_ptr->afraid", notice observable changes
 */
static bool set_afraid(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.afraid)
		{
			msgf("You are terrified!");
			notice = TRUE;

			chg_virtue(V_VALOUR, -1);
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.afraid)
		{
			msgf("You feel bolder now.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.afraid = v;

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
 * Increase the afraid counter
 */
bool inc_afraid(int v)
{
	return(set_afraid(p_ptr->tim.afraid + v));
}


/*
 * No more fear
 */
bool clear_afraid(void)
{
	return(set_afraid(0));
}

/*
 * Set "p_ptr->paralyzed", notice observable changes
 */
static bool set_paralyzed(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.paralyzed)
		{
			msgf("You are paralyzed!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.paralyzed)
		{
			msgf("You can move again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.paralyzed = v;

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
 * Increase the paralyzed counter
 */
bool inc_paralyzed(int v)
{
	return(set_paralyzed(p_ptr->tim.paralyzed + v));
}


/*
 * No more paralyzation
 */
bool clear_paralyzed(void)
{
	return(set_paralyzed(0));
}


/*
 * Set "p_ptr->image", notice observable changes
 *
 * Note that we must redraw the map when hallucination changes.
 */
static bool set_image(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.image)
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
		if (p_ptr->tim.image)
		{
			msgf("You can see clearly again.");
			notice = TRUE;

			/* Update the monster vis window */
			p_ptr->window |= PW_VISIBLE;
		}
	}

	/* Use the value */
	p_ptr->tim.image = v;

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
 * Increase the image counter
 */
bool inc_image(int v)
{
	return(set_image(p_ptr->tim.image + v));
}


/*
 * No more hallucination
 */
bool clear_image(void)
{
	return(set_image(0));
}


/*
 * Set "p_ptr->fast", notice observable changes
 */
static bool set_fast(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.fast)
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
		if (p_ptr->tim.fast)
		{
			msgf("You feel yourself slow down.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.fast = v;

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
 * Increase the "fast" counter.
 *
 * Hack - only increase speed a little bit if already hasted.
 */
bool inc_fast(int v)
{
	/* Haste */
	if ((!p_ptr->tim.fast) || (v < 0))
	{
		 return (set_fast(p_ptr->tim.fast + v));
	}
	else
	{
		return (set_fast(p_ptr->tim.fast + randint1(5)));
	}
}


/*
 * No more increased speed
 */
bool clear_fast(void)
{
	return(set_fast(0));
}


/*
 * Set "p_ptr->slow", notice observable changes
 */
static bool set_slow(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.slow)
		{
			msgf("You feel yourself moving slower!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.slow)
		{
			msgf("You feel yourself speed up.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.slow = v;

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
 * Increase the "slow" counter
 */
bool inc_slow(int v)
{
	return(set_slow(p_ptr->tim.slow + v));
}


/*
 * No more "slowness"
 */
bool clear_slow(void)
{
	return(set_slow(0));
}

/*
 * Increment "p_ptr->shield", notice observable changes
 */
bool inc_shield(int v)
{
	bool notice = FALSE;
	
	/* What will the new value be? */
	v = v + p_ptr->tim.shield;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.shield)
		{
			msgf("Your skin turns to stone.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.shield)
		{
			msgf("Your skin returns to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.shield = v;

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
 * Increment "p_ptr->blessed", notice observable changes
 */
bool inc_blessed(int v)
{
	bool notice = FALSE;
	
	/* What will the new value be? */
	v = v + p_ptr->tim.blessed;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.blessed)
		{
			msgf("You feel righteous!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.blessed)
		{
			msgf("The prayer has expired.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.blessed = v;

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
 * Increment "p_ptr->hero", notice observable changes
 */
bool inc_hero(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.hero;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.hero)
		{
			msgf("You feel like a hero!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.hero)
		{
			msgf("The heroism wears off.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.hero = v;

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
 * Increment "p_ptr->shero", notice observable changes
 */
bool inc_shero(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.shero;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.shero)
		{
			msgf("You feel like a killing machine!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.shero)
		{
			msgf("You feel less Berserk.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.shero = v;

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
 * Increment "p_ptr->protevil", notice observable changes
 */
bool inc_protevil(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.protevil;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.protevil)
		{
			msgf("You feel safe from evil!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.protevil)
		{
			msgf("You no longer feel safe from evil.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.protevil = v;

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
 * Increment "p_ptr->wraith_form", notice observable changes
 */
bool inc_wraith_form(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.wraith_form;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.wraith_form)
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
		if (p_ptr->tim.wraith_form)
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
	p_ptr->tim.wraith_form = v;

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
 * Increment "p_ptr->invuln", notice observable changes
 */
bool inc_invuln(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.invuln;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.invuln)
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
		if (p_ptr->tim.invuln)
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
	p_ptr->tim.invuln = v;

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
 * Set "p_ptr->tim.esp", notice observable changes
 */
static bool set_tim_esp(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.esp)
		{
			msgf("You feel your consciousness expand!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.esp)
		{
			msgf("Your consciousness contracts again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.esp = v;

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
	
	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);


	/* Handle stuff */
	handle_stuff();

	/* Result */
	return (TRUE);
}

/*
 * Increase the "esp" counter
 */
bool inc_tim_esp(int v)
{
	return(set_tim_esp(p_ptr->tim.esp + v));
}


/*
 * No more "esp"
 */
bool clear_tim_esp(void)
{
	return(set_tim_esp(0));
}


/*
 * Increment "p_ptr->tim_invis", notice observable changes
 */
bool inc_tim_invis(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.invis;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.invis)
		{
			msgf("Your eyes feel very sensitive!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.invis)
		{
			msgf("Your eyes feel less sensitive.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.invis = v;

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
 * Increment "p_ptr->tim_infra", notice observable changes
 */
bool inc_tim_infra(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.invuln;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.infra)
		{
			msgf("Your eyes begin to tingle!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.infra)
		{
			msgf("Your eyes stop tingling.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.infra = v;

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
 * Increment "p_ptr->oppose_acid", notice observable changes
 */
bool inc_oppose_acid(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.oppose_acid;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.oppose_acid)
		{
			msgf("You feel resistant to acid!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.oppose_acid)
		{
			msgf("You feel less resistant to acid.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.oppose_acid = v;

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
 * Increment "p_ptr->oppose_elec", notice observable changes
 */
bool inc_oppose_elec(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.oppose_elec;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.oppose_elec)
		{
			msgf("You feel resistant to electricity!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.oppose_elec)
		{
			msgf("You feel less resistant to electricity.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.oppose_elec = v;

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
 * Increment "p_ptr->oppose_fire", notice observable changes
 */
bool inc_oppose_fire(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.oppose_fire;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.oppose_fire)
		{
			msgf("You feel resistant to fire!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.oppose_fire)
		{
			msgf("You feel less resistant to fire.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.oppose_fire = v;

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
 * Increment "p_ptr->oppose_cold", notice observable changes
 */
bool inc_oppose_cold(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.oppose_cold;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.oppose_cold)
		{
			msgf("You feel resistant to cold!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.oppose_cold)
		{
			msgf("You feel less resistant to cold.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.oppose_cold = v;

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
 * Increment "p_ptr->oppose_pois", notice observable changes
 */
bool inc_oppose_pois(int v)
{
	bool notice = FALSE;

	/* What will the new value be? */
	v = v + p_ptr->tim.oppose_pois;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim.oppose_pois)
		{
			msgf("You feel resistant to poison!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim.oppose_pois)
		{
			msgf("You feel less resistant to poison.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim.oppose_pois = v;

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
 * Helper functions to test resistance status for the various elements
 *
 * These return a value from 0 to 200 indicating how much damage you
 * take from an element, as a percentage of normal damage.
 */

static int resist_table[12] = {3, 5, 7, 11, 16, 22, 33, 50, 66, 100, 150, 200};

/*
 * Acid resist level
 */
int res_acid_lvl(void)
{
	int level = 9;
	
	if (FLAG(p_ptr, TR_IM_ACID)) return (0);
	
	if (FLAG(p_ptr, TR_RES_ACID))  level -= 3;
	if (p_ptr->tim.oppose_acid)    level -= 3;
	if (FLAG(p_ptr, TR_HURT_ACID)) level += 2;

	if (level < 0)  level = 0;
	if (level > 11) level = 11;
	
	return resist_table[level];;
}

/*
 * Electricity resist level
 */
int res_elec_lvl(void)
{
	int level = 9;
	
	if (FLAG(p_ptr, TR_IM_ELEC)) return (0);
	
	if (FLAG(p_ptr, TR_RES_ELEC))  level -= 3;
	if (p_ptr->tim.oppose_elec)    level -= 3;
	if (FLAG(p_ptr, TR_HURT_ELEC)) level += 2;

	if (level < 0)  level = 0;
	if (level > 11) level = 11;
	
	return resist_table[level];;
}

/*
 * Fire resist level
 */
int res_fire_lvl(void)
{
	int level = 9;
	
	if (FLAG(p_ptr, TR_IM_FIRE)) return (0);

	if (FLAG(p_ptr, TR_RES_FIRE))  level -= 3;
	if (p_ptr->tim.oppose_fire)    level -= 3;
	if (FLAG(p_ptr, TR_HURT_FIRE)) level += 2;

	if (level < 0)  level = 0;
	if (level > 11) level = 11;
	
	return resist_table[level];;
}

/*
 * Cold resist level
 */
int res_cold_lvl(void)
{
	int level = 9;
	
	if (FLAG(p_ptr, TR_IM_COLD)) return (0);
	
	if (FLAG(p_ptr, TR_RES_COLD))  level -= 3;
	if (p_ptr->tim.oppose_cold)    level -= 3;
	if (FLAG(p_ptr, TR_HURT_COLD)) level += 2;

	if (level < 0)  level = 0;
	if (level > 11) level = 11;
	
	return resist_table[level];;
}

/*
 * Poison resist level
 */
int res_pois_lvl(void)
{
	int level = 9;
	
	if (FLAG(p_ptr, TR_IM_POIS)) return (0);

	if (FLAG(p_ptr, TR_RES_POIS)) level -= 3;
	if (p_ptr->tim.oppose_pois)   level -= 3;

	if (level < 0)  level = 0;
	if (level > 11) level = 11;
	
	return resist_table[level];;
}

/*
 * Apply resistance to damage
 */
int resist(int dam, int (*f_func) (void))
{
	/* Invulnerability */
	if (p_ptr->tim.invuln) return (0);

	/* Use the function we were passed, and round up the damage */
	return ((dam * f_func() + 99) / 100);
}


/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static int minus_ac(void)
{
	object_type *o_ptr = NULL;

	/* Pick a (possibly empty) inventory slot */
	switch (randint1(6))
	{
		case 1:
		{
			o_ptr = &p_ptr->equipment[EQUIP_BODY];
			break;
		}
		case 2:
		{
			o_ptr = &p_ptr->equipment[EQUIP_ARM];
			break;
		}
		case 3:
		{
			o_ptr = &p_ptr->equipment[EQUIP_OUTER];
			break;
		}
		case 4:
		{
			o_ptr = &p_ptr->equipment[EQUIP_HANDS];
			break;
		}
		case 5:
		{
			o_ptr = &p_ptr->equipment[EQUIP_HEAD];
			break;
		}
		case 6:
		{
			o_ptr = &p_ptr->equipment[EQUIP_FEET];
			break;
		}
	}

	/* Nothing to damage */
	if (!o_ptr->k_idx) return (FALSE);

	/* No damage left to be done */
	if (o_ptr->ac + o_ptr->to_a <= 0) return (FALSE);


	/* Object resists */
	if (FLAG(o_ptr, TR_IGNORE_ACID))
	{
		msgf("Your %v is unaffected!", OBJECT_FMT(o_ptr, FALSE, 0));

		return (TRUE);
	}

	/* Message */
	msgf("Your %v is damaged!", OBJECT_FMT(o_ptr, FALSE, 0));

	/* Damage the item */
	o_ptr->to_a--;

	/* Calculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_PLAYER);

	/* Item was damaged */
	return (TRUE);
}


/*
 * Hurt the player with Acid
 */
bool acid_dam(int dam, cptr kb_str)
{
	int inv;
	
	dam = resist(dam, res_acid_lvl);

	/* Total Immunity? */
	if (dam <= 0) return (FALSE);

	inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	if ((res_acid_lvl() > 50) && one_in_(HURT_CHANCE))
		(void)do_dec_stat(A_CHR);

	/* If any armor gets hit, defend the player */
	if (minus_ac()) dam = (dam + 1) / 2;

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (res_acid_lvl() > 25)
		(void)inven_damage(set_acid_destroy, inv);
	
	/* Obvious */
	return (TRUE);
}


/*
 * Hurt the player with electricity
 */
bool elec_dam(int dam, cptr kb_str)
{
	int inv;
	
	dam = resist(dam, res_elec_lvl);

	/* Total immunity */
	if (dam <= 0) return (FALSE);

	inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	if ((res_elec_lvl() > 50) && one_in_(HURT_CHANCE))
		(void)do_dec_stat(A_DEX);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (res_acid_lvl() > 25)
		(void)inven_damage(set_elec_destroy, inv);

	/* Obvious */
	return (TRUE);
}


/*
 * Hurt the player with Fire
 */
bool fire_dam(int dam, cptr kb_str)
{
	int inv;
	
	dam = resist(dam, res_fire_lvl);

	/* Totally immune? */
	if (dam <= 0) return (FALSE);

	inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	if ((res_fire_lvl() > 50) && one_in_(HURT_CHANCE))
		(void)do_dec_stat(A_STR);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (res_fire_lvl() > 25)
		(void)inven_damage(set_fire_destroy, inv);

	/* Obvious */
	return (TRUE);
}


/*
 * Hurt the player with Cold
 */
bool cold_dam(int dam, cptr kb_str)
{
	int inv;
	
	dam = resist(dam, res_cold_lvl);

	/* Total immunity? */
	if (dam <= 0) return (FALSE);

	inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

	if ((res_cold_lvl() > 50) && one_in_(HURT_CHANCE))
		(void)do_dec_stat(A_STR);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Inventory damage */
	if (res_cold_lvl() > 25)
		(void)inven_damage(set_cold_destroy, inv);

	/* Obvious */
	return (TRUE);
}


/*
 * Hurt the player with Poison
 *
 * Hack - this should probably take a second argument
 * to add to the poison counter
 */
bool pois_dam(int dam, cptr kb_str, int pois)
{
	dam = resist(dam, res_pois_lvl);

	/* Totally immune? */
	if (dam <= 0) return (FALSE);

	if ((res_pois_lvl() > 50) && one_in_(HURT_CHANCE))
		(void)do_dec_stat(A_CON);

	/* Take damage */
	take_hit(dam, kb_str);

	/* Add poison to counter */
	if (res_pois_lvl() > 25)
	{
		pois = resist(pois, res_pois_lvl);
		inc_poisoned(pois);
	}
	
	/* Obvious */
	return (TRUE);
}


/*
 * Set "p_ptr->stun", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_stun(int v)
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
	if ((p_ptr->rp.prace == RACE_GOLEM) &&
		!(ironman_shops || ironman_downward || ironman_nightmare))
	{
		v = 0;
	}

	/* Knocked out */
	if (p_ptr->tim.stun > 100)
	{
		old_aux = 3;
	}

	/* Heavy stun */
	else if (p_ptr->tim.stun > 50)
	{
		old_aux = 2;
	}

	/* Stun */
	else if (p_ptr->tim.stun > 0)
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
			(!(p_ptr->rp.pclass == CLASS_MINDCRAFTER)))
		{
			msgf("A vicious blow hits your head.");
			if (one_in_(3))
			{
				if (!(FLAG(p_ptr, TR_SUST_INT))) (void)do_dec_stat(A_INT);
				if (!(FLAG(p_ptr, TR_SUST_WIS))) (void)do_dec_stat(A_WIS);
			}
			else if (one_in_(2))
			{
				if (!(FLAG(p_ptr, TR_SUST_INT))) (void)do_dec_stat(A_INT);
			}
			else
			{
				if (!(FLAG(p_ptr, TR_SUST_WIS))) (void)do_dec_stat(A_WIS);
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
	p_ptr->tim.stun = v;

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
 * Increase the "stun" counter
 */
bool inc_stun(int v)
{
	return(set_stun(p_ptr->tim.stun + v));
}


/*
 * No more "stun"
 */
bool clear_stun(void)
{
	return(set_stun(0));
}


/*
 * Set "p_ptr->cut", notice observable changes
 *
 * Note the special code to only notice "range" changes.
 */
static bool set_cut(int v)
{
	int old_aux, new_aux;

	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	if (p_ptr->rp.prace == RACE_GOLEM ||
		p_ptr->rp.prace == RACE_SKELETON ||
		p_ptr->rp.prace == RACE_SPECTRE ||
		(p_ptr->rp.prace == RACE_ZOMBIE && p_ptr->lev > 11))
		v = 0;

	/* Mortal wound */
	if (p_ptr->tim.cut > 1000)
	{
		old_aux = 7;
	}

	/* Deep gash */
	else if (p_ptr->tim.cut > 200)
	{
		old_aux = 6;
	}

	/* Severe cut */
	else if (p_ptr->tim.cut > 100)
	{
		old_aux = 5;
	}

	/* Nasty cut */
	else if (p_ptr->tim.cut > 50)
	{
		old_aux = 4;
	}

	/* Bad cut */
	else if (p_ptr->tim.cut > 25)
	{
		old_aux = 3;
	}

	/* Light cut */
	else if (p_ptr->tim.cut > 10)
	{
		old_aux = 2;
	}

	/* Graze */
	else if (p_ptr->tim.cut > 0)
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
			if (!(FLAG(p_ptr, TR_SUST_CHR)))
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
	p_ptr->tim.cut = v;

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
 * Increase the "cut" counter
 */
bool inc_cut(int v)
{
	return(set_cut(p_ptr->tim.cut + v));
}


/*
 * No more "cuts"
 */
bool clear_cut(void)
{
	return(set_cut(0));
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
	value = p_ptr->stat[stat].cur;

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
		p_ptr->stat[stat].cur = value;

		/* Bring up the maximum too */
		if (value > p_ptr->stat[stat].max)
		{
			p_ptr->stat[stat].max = value;
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
	int cur, max, same, res = FALSE;


	/* Acquire current value */
	cur = p_ptr->stat[stat].cur;
	max = p_ptr->stat[stat].max;

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
	if (cur > 30)
	{
		if (cur > 30 && amount > 90) cur -= rand_range(3, cur / 10);
		if (cur > 30 && amount > 50) cur -= rand_range(3, cur / 10);
		if (cur > 30 && amount > 20) cur -= rand_range(3, cur / 10);
		if (cur > 30) cur -= rand_range(3, cur / 10);

		/* Prevent illegal values */
		if (cur < 30) cur = 30;

		/* Something happened */
		if (cur != p_ptr->stat[stat].cur) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent && (max > 30))
	{
		chg_virtue(V_SACRIFICE, 1);
		if (stat == A_WIS || stat == A_INT)
			chg_virtue(V_ENLIGHTEN, -2);

		if (max > 30 && amount > 90) max -= rand_range(3, max / 10);
		if (max > 30 && amount > 50) max -= rand_range(3, max / 10);
		if (max > 30 && amount > 20) max -= rand_range(3, max / 10);
		if (max > 30) max -= rand_range(3, max / 10);

		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat[stat].max) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat[stat].cur = cur;
		p_ptr->stat[stat].max = max;

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
	if (p_ptr->stat[stat].cur != p_ptr->stat[stat].max)
	{
		/* Restore */
		p_ptr->stat[stat].cur = p_ptr->stat[stat].max;

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
			if (FLAG(p_ptr, TR_SUST_STR)) sust = TRUE;
			break;
		}
		case A_INT:
		{
			if (FLAG(p_ptr, TR_SUST_INT)) sust = TRUE;
			break;
		}
		case A_WIS:
		{
			if (FLAG(p_ptr, TR_SUST_WIS)) sust = TRUE;
			break;
		}
		case A_DEX:
		{
			if (FLAG(p_ptr, TR_SUST_DEX)) sust = TRUE;
			break;
		}
		case A_CON:
		{
			if (FLAG(p_ptr, TR_SUST_CON)) sust = TRUE;
			break;
		}
		case A_CHR:
		{
			if (FLAG(p_ptr, TR_SUST_CHR)) sust = TRUE;
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
		if (k_ptr->flavor && !player_save(k_ptr->level - 50))
		{
			/* Forget knowledge */
			k_ptr->aware = FALSE;
			k_ptr->tried = FALSE;
		}
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Notice changes */
	notice_item();

	/* Mega-Hack -- Forget the map */
	wiz_dark();

	/* It worked */
	return (TRUE);
}


void do_poly_wounds(void)
{
	/* Changed to always provide at least _some_ healing */
	s16b wounds = p_ptr->tim.cut;
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
		(void)set_cut(p_ptr->tim.cut - (change / 2));
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

			if (p_ptr->rp.psex == SEX_MALE)
			{
				p_ptr->rp.psex = SEX_FEMALE;
				sp_ptr = &sex_info[p_ptr->rp.psex];
				strnfmt(effect_msg, 80, "female ");
			}
			else
			{
				p_ptr->rp.psex = SEX_MALE;
				sp_ptr = &sex_info[p_ptr->rp.psex];
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
		while ((new_race == p_ptr->rp.prace) && (expfact > goalexpfact));

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

		old_race = p_ptr->rp.prace;
		p_ptr->rp.prace = new_race;
		rp_ptr = &race_info[p_ptr->rp.prace];

		/* Adjust the stats */
		for (i = 0; i < A_MAX; i++)
		{
			int change;

			/* Calculate the difference between the races */
			change = rp_ptr->r_adj[i] - race_info[old_race].r_adj[i];

			/* Adjust current stat */
			p_ptr->stat[i].cur = adjust_stat(i, p_ptr->stat[i].cur, change);

			/* Adjust maximum stat */
			p_ptr->stat[i].max = adjust_stat(i, p_ptr->stat[i].max, change);
		}

		/* Experience factor */
		p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

		/* Calculate the height/weight for males */
		if (p_ptr->rp.psex == SEX_MALE)
		{
			p_ptr->rp.ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
			p_ptr->rp.wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
		}

		/* Calculate the height/weight for females */
		else if (p_ptr->rp.psex == SEX_FEMALE)
		{
			p_ptr->rp.ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
			p_ptr->rp.wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
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
	if (p_ptr->state.is_dead) return;

	/* Disturb */
	disturb(TRUE);

	/* Mega-Hack -- Apply "invulnerability" */
	if (p_ptr->tim.invuln && (damage < 9000))
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

	if (p_ptr->tim.wraith_form)
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
	p_ptr->state.skip_more = FALSE;

	if (pen_invuln)
		msgf("The attack penetrates your shield of invulnerability!");

	if (!(p_ptr->tim.invuln) || (pen_invuln))
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
		len = strnfmt(p_ptr->state.died_from, 80, hit_from);

		if (p_ptr->tim.image) strnfcat(p_ptr->state.died_from, 80, &len, "(?)");

		/* No longer a winner */
		p_ptr->state.total_winner = FALSE;

		/* Leaving */
		p_ptr->state.leaving = TRUE;

		/* Note death */
		p_ptr->state.is_dead = TRUE;

		if (get_check("Dump the screen? "))
		{
			do_cmd_save_screen();
		}

		/* Dead */
		return;
	}

	/* Hitpoint warning */
	if (p_ptr->chp <= warning)
	{
		/* Hack -- bell on first notice */
		if (old_chp > warning)
		{
			/* Alert */
			bell("Low hitpoint warning!");

			if (emergency_stop)
			{
				/* Show all the messages */
				message_flush();

				/* Alert the user to the problem */
				put_fstr(0, 0, "Emergency stop.  Press 'c' to continue.");

				/* Wait for acknowledgement */
				while (inkey() != 'c') ;

				disturb(TRUE);
			}
		}

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
	int total = amount + p_ptr->state.noise_level;

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
	p_ptr->state.noise_level = (byte)total;
}


/*
 * Various notice 'blah' functions
 */


/*
 * Change an item in the inventory
 */
void notice_inven(void)
{
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);
}


/*
 * Change an item in the equipment
 */
void notice_equip(void)
{
	/* Window stuff */
	p_ptr->window |= (PW_EQUIP);
}


/*
 * Change an item somewhere
 */
void notice_item(void)
{
	notice_inven();
	notice_equip();
}


/*
 * Something has happened to disturb the player.
 *
 * The arg indicates a major disturbance, which affects search.
 *
 * All disturbance cancels repeated commands, resting, and running.
 */
void disturb(bool stop_search)
{
	/* Cancel repeated commands */
	if (p_ptr->cmd.rep)
	{
		/* Cancel */
		p_ptr->cmd.rep = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel Resting */
	if (p_ptr->state.resting)
	{
		/* Cancel */
		p_ptr->state.resting = 0;

		/* Redraw the state (later) */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Cancel running */
	if (p_ptr->state.running)
	{
		/* Cancel */
		p_ptr->state.running = 0;

		/* Check for new panel if appropriate */
		if (center_player && avoid_center) verify_panel();

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Cancel searching if requested */
	if (stop_search && p_ptr->state.searching)
	{
		/* Cancel */
		p_ptr->state.searching = FALSE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	}

	/* Flush the input if requested */
	if (flush_disturb) flush();
}

