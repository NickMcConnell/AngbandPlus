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
 * Note the use of "PU_UN_LITE" and "PU_UN_VIEW", which is needed to
 * memorize any terrain features which suddenly become "visible".
 * Note that blindness is currently the only thing which can affect
 * "player_can_see_bold()".
 */
bool set_blind(int v)
{
	bool notice = FALSE;

	if (p_ptr->perma_blind) {
	  p_ptr->blind = 10000;
	  return FALSE;
	}

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->blind)
		{
			mprint(MSG_URGENT, "You are blind!");
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

	/* Forget stuff */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

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
			mprint(MSG_WARNING, "You are confused!");
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
			mprint(MSG_URGENT, "You are poisoned!");
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
 * Try to drop something.
 */

static void try_drop_armor(int slot) {
  if (!inventory[slot].k_idx) return;

  if (inventory[slot].ident & (IDENT_CURSED)) {
    mprint(MSG_STUPID, "Ouch, your armor is stuck!");
    take_hit(damroll(5, 10), "constricting armor");
  } else {
    inven_drop(slot, 1);
  }
}


/*
 * Drop the armor you're wearing.
 */

static void drop_armor(void) {
  mprint(MSG_WARNING, "Your armor pops off!");
  p_ptr->energy_use = 100;

  try_drop_armor(INVEN_BODY);
  try_drop_armor(INVEN_OUTER);
  try_drop_armor(INVEN_ARM);
  try_drop_armor(INVEN_HEAD);
  try_drop_armor(INVEN_HANDS);
  try_drop_armor(INVEN_FEET);
};

/*
 * Set call ``set_shape'' with default values. 
 */

bool change_shape(byte shape) {
  return set_shape(shape, shape_info[shape-1].duration);
}

/*
 * Set "p_ptr->shape" and "p_ptr->shape_timed", notice observable changes
 */
bool set_shape(byte shape, int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Mega-hack -- Return to natural shape */
	if (v == 0 && shape == 0) {
	    msg_print("You return to your natural shape.");
	    msg_print(NULL);
	    notice = TRUE;
	} else {
	    /* Open */
	    if (v)
	    {
		  if (!p_ptr->shape || p_ptr->shape != shape)
		  {
			msg_print(shape_info[shape-1].mesg_on);
			msg_print(NULL);
			notice = TRUE;
			drop_armor();
		  }
	    }

	    /* Shut */
	    else
	    {
		  if (p_ptr->shape)
		  {
			msg_print(shape_info[shape-1].mesg_off);
			msg_print(NULL);
			shape = 0;
			notice = TRUE;
		  }
	    }
	}

	/* Use the value */
	p_ptr->shape_timed = v;
	p_ptr->shape = shape;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Handle stuff */
	p_ptr->update |= (PU_BONUS | PU_MANA | PU_SPELLS | 
 			  PU_HP | PU_SANITY);
	p_ptr->redraw |= (PR_STATE | PR_MISC | PR_MAP | PR_STUDY);
	handle_stuff();

	/* Result */
	return (TRUE);
}


/*
 * Set "p_ptr->grace", notice observable changes
 */
void set_grace(s32b v) {
  p_ptr->grace = v;
  p_ptr->update |= PU_BONUS;
  handle_stuff();
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
			mprint(MSG_WARNING, "You are terrified!");
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
			mprint(MSG_URGENT, "You are paralyzed!");
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
			mprint(MSG_STUPID, "You feel drugged!");
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

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

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
			mprint(MSG_BONUS, "You feel yourself moving faster!");
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
			mprint(MSG_WARNING, "You feel yourself moving slower!");
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
			mprint(MSG_BONUS, "A mystic shield forms around your body!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
			mprint(MSG_WARNING, "Your mystic shield crumbles away.");
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
			mprint(MSG_BONUS, "You feel righteous!");
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
			mprint(MSG_BONUS, "You feel like a hero!");
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
			mprint(MSG_BONUS, "You feel like a killing machine!");
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
			mprint(MSG_BONUS, "You feel safe from evil!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->protevil)
		{
			mprint(MSG_WARNING, "You no longer feel safe from evil.");
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
			mprint(MSG_BIG_BONUS, "You feel invulnerable!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
			mprint(MSG_URGENT, "You feel vulnerable once more.");
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
			mprint(MSG_BONUS, "Your eyes feel very sensitive!");
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
			mprint(MSG_BONUS, "Your eyes begin to tingle!");
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
			mprint(MSG_BONUS, "You feel resistant to acid!");
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
			mprint(MSG_BONUS, "You feel resistant to electricity!");
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
			mprint(MSG_BONUS, "You feel resistant to fire!");
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
			mprint(MSG_BONUS, "You feel resistant to cold!");
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
			mprint(MSG_BONUS, "You feel resistant to poison!");
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
				mprint(MSG_WARNING, "You have been stunned.");
				break;
			}

			/* Heavy stun */
			case 2:
			{
				mprint(MSG_WARNING, "You have been heavily stunned.");
				break;
			}

			/* Knocked out */
			case 3:
			{
				mprint(MSG_URGENT, "You have been knocked out.");
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
				mprint(MSG_WARNING, "You have been given a graze.");
				break;
			}

			/* Light cut */
			case 2:
			{
				mprint(MSG_WARNING, "You have been given a light cut.");
				break;
			}

			/* Bad cut */
			case 3:
			{
				mprint(MSG_WARNING, "You have been given a bad cut.");
				break;
			}

			/* Nasty cut */
			case 4:
			{
				mprint(MSG_URGENT, "You have been given a nasty cut.");
				break;
			}

			/* Severe cut */
			case 5:
			{
				mprint(MSG_URGENT, "You have been given a severe cut.");
				break;
			}

			/* Deep gash */
			case 6:
			{
				mprint(MSG_URGENT, "You have been given a deep gash.");
				break;
			}

			/* Mortal wound */
			case 7:
			{
				mprint(MSG_URGENT, "You have been given a mortal wound.");
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
				mprint(MSG_WARNING, "You are still weak.");
				break;
			}

			/* Hungry */
			case 2:
			{
				mprint(MSG_WARNING, "You are still hungry.");
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
				mprint(MSG_BONUS, "You are full!");
				break;
			}

			/* Bloated */
			case 5:
			{
				mprint(MSG_STUPID, "You have gorged yourself!");
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
				mprint(MSG_STUPID, "You are getting faint from hunger!");
				break;
			}

			/* Weak */
			case 1:
			{
				mprint(MSG_STUPID, "You are getting weak from hunger!");
				break;
			}

			/* Hungry */
			case 2:
			{
				mprint(MSG_STUPID, "You are getting hungry.");
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
	       (p_ptr->exp < (player_exp[p_ptr->lev-2] *
	                      p_ptr->expfact / 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);

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

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_lev) p_ptr->max_lev = p_ptr->lev;

		/* Sound */
		sound(SOUND_LEVEL);

		/* Message */
		mformat(MSG_BONUS, "Welcome to level %d.", p_ptr->lev);

		/* Update some stuff */
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_SPELL | PW_PLAYER);

		/* Handle stuff */
		handle_stuff();

		if (p_ptr->pclass == CLASS_CORRUPTED) {
		  if (p_ptr->lev % 5 == 0 && p_ptr->prace != RACE_MUTANT) {
		    mprint(MSG_WARNING, "You can't resist the corrupting power of chaos!");
		    do_dec_stat(A_STR);
		    do_dec_stat(A_DEX);
		    do_dec_stat(A_CON);
		    do_dec_stat(A_INT);
		    do_dec_stat(A_WIS);
		    do_dec_stat(A_CHR);
		  }
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
 * XXX XXX XXX Note the use of actual "monster names"
 */
static int get_coin_type(monster_race *r_ptr)
{
	cptr name = (r_name + r_ptr->name);

	/* Analyze "coin" monsters */
	if (r_ptr->d_char == '$')
	{
		/* Look for textual clues */
		if (strstr(name, " copper ")) return (2);
		if (strstr(name, " silver ")) return (5);
		if (strstr(name, " gold ")) return (10);
		if (strstr(name, " mithril ")) return (16);
		if (strstr(name, " adamantite ")) return (17);

		/* Look for textual clues */
		if (strstr(name, "Copper ")) return (2);
		if (strstr(name, "Silver ")) return (5);
		if (strstr(name, "Gold ")) return (10);
		if (strstr(name, "Mithril ")) return (16);
		if (strstr(name, "Adamantite ")) return (17);
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
	int j, y, x, ny, nx;
	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

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
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Make Gold */
		if (do_gold && (!do_item || (rand_int(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(i_ptr)) continue;

			/* XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!make_object(i_ptr, good, great)) continue;

			/* XXX XXX XXX */
			dump_item++;
		}

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


	/* Only process "Quest Monsters" */
	if (!(r_ptr->flags1 & (RF1_QUESTOR))) return;

	/* Stagger around */
	while (!cave_valid_bold(y, x)) {
	  int d = 1;
	  
	  /* Pick a location */
	  scatter(&ny, &nx, y, x, d, 0);

	  /* Stagger */
	  y = ny; x = nx;
	}

	/* XXX XXX XXX */
	delete_object(y, x);
		
	/* Explain the staircase */
	mprint(MSG_BONUS, "A magical staircase appears...");

	/* Create stairs down */
	cave_set_feat(y, x, FEAT_MORE);

	/* Remember to update everything */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);


	/* Only process "Endgame monsters" */
	if (!(r_ptr->flags7 & (RF7_ENDGAME))) return;

	/* Nothing left, game over... */
	/* Total winner */
	p_ptr->total_winner = TRUE;

	/* Redraw the "title" */
	p_ptr->redraw |= (PR_TITLE);

	/* Congratulations */
	mprint(MSG_BIG_BONUS, "*** CONGRATULATIONS ***");
	mprint(MSG_BIG_BONUS, "You have won the game!");
	mprint(MSG_BIG_BONUS, "You may retire (commit suicide) when you are ready.");
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
 *
 * The next-to-last argument desides whether to award exp to the player.
 * The last argument tells the fn. whether a pet did the fighting.
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, bool give_exp,
		  bool hit_by_pet)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s32b div, new_exp, new_exp_frac;

	bool god_liked, god_hated;

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);


	/* Wake it up if it's hurt */
	if (dam > 0)
	  m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* Find out religious info */
	god_liked = sacred_monster(r_ptr);
	god_hated = despised_monster(r_ptr);

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		if (hit_by_pet) {
		  monster_desc(m_name, m_ptr, 0x88);
		} else {
		  monster_desc(m_name, m_ptr, 0);
		}

		/* Make a sound */
		sound(SOUND_KILL);

		/* Death by Missile/Spell attack */
		if (note)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Death by physical attack -- invisible monster */
		else if (!m_ptr->ml)
		{
			msg_format("You have killed %s.", m_name);
		}

		/* Death by Physical attack -- non-living monster */
		else if ((r_ptr->flags3 & (RF3_DEMON)) ||
		         (r_ptr->flags3 & (RF3_UNDEAD)) ||
		         (r_ptr->flags2 & (RF2_STUPID)) ||
		         (strchr("Evg", r_ptr->d_char)))
		{
			msg_format("You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			msg_format("You have slain %s.", m_name);
		}


		/* Maximum player level */
		div = p_ptr->max_lev;

		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level) / div;

		/* Handle fractional experience */
		new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
				  * 0x10000L / div) + p_ptr->exp_frac;


		/* Give triple exp. for hated monsters. */

		if (god_hated) {
		  new_exp *= 3;
		  new_exp_frac *= 3;
		}

		/* Keep track of experience */
		if (new_exp_frac >= 0x10000L)
		  {
		    new_exp++;
		    p_ptr->exp_frac = new_exp_frac - 0x10000L;
		  }
		else
		  {
		    p_ptr->exp_frac = new_exp_frac;
		  }
		
		/* Gain experience */
		if (give_exp) {
		  gain_exp(new_exp);
		}

		/* Player vanquishing arena combatant -KMW- */
		if (p_ptr->inside_special == 1) {
		  p_ptr->exit_bldg = TRUE;
		  mprint(MSG_BONUS, "Victorious! You're on your way to becoming Champion.");
		  p_ptr->arena_number[p_ptr->which_arena]++;
		}

		/* Generate treasure */
		monster_death(m_idx);

		/* When the player kills a Unique, it stays dead */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 0;

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			/* Count kills this life */
			if (r_ptr->r_pkills < MAX_SHORT) r_ptr->r_pkills++;

			/* Count kills in all lives */
			if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;

			/* Hack -- Auto-recall */
			monster_race_track(m_ptr->r_idx);
		}

		/* Delete the monster */
		delete_monster_idx(m_idx);

		/* Not afraid */
		(*fear) = FALSE;

		/* Wake up nerby monsters */

		if (!hit_by_pet) {
		  awake_monsters(1);
		}

		/* Handle religous consequenses. */

		if (give_exp && !hit_by_pet) {
		  if (god_hated) {
		    char tmp_name[80];

		    strcpy(tmp_name, deity_info[p_ptr->pgod-1].name);

		    mformat(MSG_BONUS, "%s seems pleased.", tmp_name);
		    set_grace(p_ptr->grace + new_exp*5);
		  }

 		  if (god_liked) {
		    char tmp_name[80]; 

		    strcpy(tmp_name, deity_info[p_ptr->pgod-1].name);
		  
		    mformat(MSG_STUPID, "%s howls in rage! You have killed a sacred creature!", tmp_name);
		    set_grace(p_ptr->grace - new_exp*15);
		    godly_wrath_blast();
		  }
		}


		/* Has a crime been commited? */
		if ((r_ptr->flags2 & RF2_INNOCENT) && give_exp) {
		  /* Activate generators */
		  activate_generators();

		}

		/* Has the quest been completed? */
		if (p_ptr->inside_special == 2 && p_ptr->which_quest) {
		  vault_type* v_ptr = q_v_ptrs[p_ptr->which_quest-1];

		  if (v_ptr->q_type == QT_KILL) {
		    int i;
		    bool comp = TRUE;
		    monster_type* m_ptr;

		    for (i = 1; i < m_max; i++) {
		      m_ptr = &m_list[i];

		      if (!m_ptr->r_idx) continue;

		      if (!m_ptr->is_pet) {
			comp = FALSE;
			break;
		      }
		    }
		    
		    if (comp) 
		      complete_quest();
		  }
		}
		
		/* Monster is dead */
		return (TRUE);
	}


#ifdef ALLOW_FEAR

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
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)))
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if (((percentage <= 10) && (rand_int(10) < percentage)) ||
		    ((dam >= m_ptr->hp) && (rand_int(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
			                  (((dam >= m_ptr->hp) && (percentage > 7)) ?
			                   20 : ((11 - percentage) * 5)));
		}
	}

#endif

	/* Not dead yet */
	return (FALSE);
}




/*
 * Check for, and react to, the player leaving the panel
 *
 * When the player gets too close to the edge of a panel, the
 * map scrolls one panel in that direction so that the player
 * is no longer so close to the edge.
 */
void verify_panel(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i;

	bool scroll = FALSE;


	/* Initial row */
	i = p_ptr->wy;

	/* Scroll screen when 2 grids from top/bottom edge */
	if ((py < p_ptr->wy + 2) || (py >= p_ptr->wy+SCREEN_HGT - 2))
	{
		i = ((py - PANEL_HGT / 2) / PANEL_HGT) * PANEL_HGT;
		if (i < 0) i = 0;
		if (i > DUNGEON_HGT - SCREEN_HGT) i = DUNGEON_HGT - SCREEN_HGT;
	}

	/* Hack -- handle town - DON'T need to do this - dungeon size now -KMW- */
	/* if (!p_ptr->depth) i = TOWN_HEIGHT; */

	/* New panel row */
	if (p_ptr->wy != i)
	{
		/* Update panel */
		p_ptr->wy = i;

		/* Scroll */
		scroll = TRUE;
	}


	/* Initial col */
	i = p_ptr->wx;

	/* Scroll screen when 4 grids from left/right edge */
	if ((px < p_ptr->wx + 4) || (px >= p_ptr->wx+SCREEN_WID - 4))
	{
		i = ((px - PANEL_WID / 2) / PANEL_WID) * PANEL_WID;
		if (i < 0) i = 0;
		if (i > DUNGEON_WID - SCREEN_WID) i = DUNGEON_WID - SCREEN_WID;
	}

	/* Hack -- handle town - DON'T need to do this - dungeon size now -KMW-*/
	/* if (!p_ptr->depth) i = SCREEN_WID; */

	/* New panel col */
	if (p_ptr->wx != i)
	{
		/* Update panel */
		p_ptr->wx = i;

		/* Scroll */
		scroll = TRUE;
	}

	/* Scroll */
	if (scroll)
	{
		/* Optional disturb on "panel change" */
		if (disturb_panel) disturb(0, 0);

		/* Update stuff */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
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
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	/* Got rid of this for now.
	   if (p_ptr->image) return (FALSE);
	*/

	/* XXX XXX XXX Hack -- Never target trappers */
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
	/* Accept stationary targets */
	if (p_ptr->target_who < 0) return (TRUE);

	/* Check moving targets */
	if (p_ptr->target_who > 0)
	{
		/* Accept reasonable targets */
		if (target_able(p_ptr->target_who))
		{
			monster_type *m_ptr = &m_list[p_ptr->target_who];

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

		/* XXX XXX XXX Penalize location */

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
static bool target_set_accept(int y, int x)
{
	s16b this_o_idx, next_o_idx = 0;


	/* Player grids are always interesting */
	if (cave_m_idx[y][x] < 0) return (TRUE);


	/* Handle hallucination */
	/* Got rid of this for now.
	   if (p_ptr->image) return (FALSE);
	*/

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

		/* Notice buildings -KMW- */
		if ((cave_feat[y][x] >= FEAT_BLDG_HEAD) &&
		    (cave_feat[y][x] <= FEAT_BLDG_TAIL)) return (TRUE);

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

		/* Notice water & lava -KMW- */
		if (cave_feat[y][x] == FEAT_DEEP_WATER) return (TRUE);
		if (cave_feat[y][x] == FEAT_SHAL_WATER) return (TRUE);
		if (cave_feat[y][x] == FEAT_DEEP_LAVA) return (TRUE);
		if (cave_feat[y][x] == FEAT_SHAL_LAVA) return (TRUE);
		if (cave_feat[y][x] == FEAT_TREES) return (TRUE);
		if (cave_feat[y][x] == FEAT_MOUNTAIN) return (TRUE);

		/* Notice Chaos Fog */
		if (cave_feat[y][x] == FEAT_CHAOS_FOG) return TRUE;

		/* Notice altars. */
		if (cave_feat[y][x] >= FEAT_ALTAR_HEAD &&
		    cave_feat[y][x] <= FEAT_ALTAR_TAIL) return TRUE;
		
	}

	/* Nope */
	return (FALSE);
}


/*
 * Prepare the "temp" array for "target_set"
 *
 * Return the number of target_able monsters in the set.
 */
static void target_set_prepare(int mode)
{
	int y, x;

	/* Reset "temp" array */
	temp_n = 0;

	/* Scan the current panel */
	for (y = p_ptr->wy; y < p_ptr->wy+SCREEN_HGT; y++)
	{
		for (x = p_ptr->wx; x < p_ptr->wx+SCREEN_WID; x++)
		{
			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_accept(y, x)) continue;

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
 * Eventually, we may allow multiple objects per grid, or objects
 * and terrain features in the same grid. XXX XXX XXX
 *
 * This function must handle blindness/hallucination.
 */
static int target_set_aux(int y, int x, int mode, cptr info)
{
	s16b this_o_idx, next_o_idx = 0;

	cptr s1, s2, s3;

	bool boring;

	int feat;

	int query;

	char out_val[160];


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
						/* Save */
						Term_save();

						/* Recall on screen */
						screen_roff(m_ptr->r_idx, 0);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,%s]", info));

						/* Command */
						query = inkey();

						/* Restore */
						Term_load();
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
					char o_name[80];

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
				char o_name[80];

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

			/* Hack -- special introduction for store & building doors -KMW- */
			if (((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
			    ((feat >= FEAT_BLDG_HEAD) & (feat <= FEAT_BLDG_TAIL)))
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
bool target_set(int mode)
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


	/* Cancel target */
	p_ptr->target_who = 0;


	/* Cancel tracking */
	/* health_track(0); */


	/* Prepare the "temp" array */
	target_set_prepare(mode);

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

			/* Describe and Prompt */
			query = target_set_aux(y, x, mode, info);

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

				case 't':
				case '.':
				case '5':
				case '0':
				{
					if ((cave_m_idx[y][x] > 0) && target_able(cave_m_idx[y][x]))
					{
						health_track(cave_m_idx[y][x]);
						p_ptr->target_who = cave_m_idx[y][x];
						p_ptr->target_row = y;
						p_ptr->target_col = x;
						done = TRUE;
					}
					else
					{
						bell();
					}
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

				default:
				{
					d = keymap_dirs[query & 0x7F];
					if (!d) bell();
					break;
				}
			}

			/* Hack -- move around */
			if (d)
			{
				/* Find a new monster */
				i = target_pick(temp_y[m], temp_x[m], ddy[d], ddx[d]);

				/* Use that grid */
				if (i >= 0) m = i;
			}
		}

		/* Arbitrary grids */
		else
		{
			/* Default prompt */
			strcpy(info, "q,t,p,m,+,-,<dir>");

			/* Describe and Prompt (enable "TARGET_LOOK") */
			query = target_set_aux(y, x, mode | TARGET_LOOK, info);

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

				case 't':
				case '.':
				case '5':
				case '0':
				{
					p_ptr->target_who = -1;
					p_ptr->target_row = y;
					p_ptr->target_col = x;
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

				default:
				{
					d = keymap_dirs[query & 0x7F];
					if (!d) bell();
					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				/* Move */
				x += ddx[d];
				y += ddy[d];

				/* Slide into legality */
				if ((x >= DUNGEON_WID-1) || (x >= p_ptr->wx+SCREEN_WID)) x--;
				else if ((x <= 0) || (x < p_ptr->wx)) x++;

				/* Slide into legality */
				if ((y >= DUNGEON_HGT-1) || (y >= p_ptr->wy+SCREEN_HGT)) y--;
				else if ((y <= 0) || (y < p_ptr->wy)) y++;
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Failure to set target */
	if (!p_ptr->target_who) return (FALSE);

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

	char command;

	cptr p;


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
		if (!get_com(p, &command)) break;

		/* Convert various keys to "standard" keys */
		switch (command)
		{
			/* Use current target */
			case 'T':
			case 't':
			case '.':
			case '5':
			case '0':
			{
				dir = 5;
				break;
			}

			/* Set new target */
			case '*':
			{
				if (target_set(TARGET_KILL)) dir = 5;
				break;
			}

			default:
			{
				dir = keymap_dirs[command & 0x7F];
				break;
			}
		}

		/* Verify requested targets */
		if ((dir == 5) && !target_okay()) dir = 0;

		/* Error */
		if (!dir) bell();
	}

	/* No direction */
	if (!dir) return (FALSE);

	/* Save the direction */
	p_ptr->command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (p_ptr->command_dir != dir)
	{
		/* Warn the user */
		mprint(MSG_TEMP, "You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

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
 * Direction "5" (and "0") are illegal and will not be accepted.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", if it is set.
 */
bool get_rep_dir(int *dp)
{
	int dir;


	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = p_ptr->command_dir;

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
		if (!get_com("Direction (Escape to cancel)? ", &ch)) break;

		/* Look up the direction */
		dir = keymap_dirs[ch & 0x7F];

		/* Prevent weirdness */
		if (dir == 5) dir = 0;

		/* Oops */
		if (!dir) bell();
	}

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	p_ptr->command_dir = dir;

	/* Save direction */
	(*dp) = dir;

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
		/* Apply confusion */
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
		mprint(MSG_TEMP, "You are confused.");

		/* Save direction */
		(*dp) = dir;

		/* Confused */
		return (TRUE);
	}

	/* Not confused */
	return (FALSE);
}

/*
 * Here come the side effect functions. They are classified according
 * to how beneficial they are, to avoid a giant nested switch/case
 * statement. 
 */

/* Great side effect. */

void great_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific_friendly(p_ptr->py, p_ptr->px, 
				 p_ptr->depth+rand_spread(10, 5), 0)) {
      mprint(MSG_BIG_BONUS, "Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    acquirement(p_ptr->py, p_ptr->px, 1, 1);
  } else if (tmp <= 30) {
    enchant_spell(0, 0, randint(3) + 2);
  } else if (tmp <= 40) {
    enchant_spell(randint(3), randint(3), 0);
  } else if (tmp <= 50) {
    fire_explosion(p_ptr->py, p_ptr->px, GF_MAKE_GLYPH, 9, 0);
  } else if (tmp <= 60) {
    recharge(100);
  } else if (tmp <= 70) {
    remove_all_curse();
  } else if (tmp <= 80) {
    identify_fully();
  } else if (tmp <= 90) {
    restore_level();
    hp_player(5000);
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
    (void)do_res_stat(A_STR);
    (void)do_res_stat(A_CON);
    (void)do_res_stat(A_DEX);
    (void)do_res_stat(A_WIS);
    (void)do_res_stat(A_INT);
    (void)do_res_stat(A_CHR);
  } else if (tmp <= 100) {
    do_inc_stat(A_STR);
    do_inc_stat(A_INT);
    do_inc_stat(A_WIS);
    do_inc_stat(A_DEX);
    do_inc_stat(A_CON);
    do_inc_stat(A_CHR);
  }
}



/* Good side effect. */

void good_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific_friendly(p_ptr->py, p_ptr->px, 
				 p_ptr->depth, 0)) {
      mprint(MSG_BIG_BONUS, "Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    acquirement(p_ptr->py, p_ptr->px, 1, 0);
  } else if (tmp <= 30) {
    enchant_spell(0, 0, 1);
  } else if (tmp <= 40) {
    enchant_spell(1, 1, 0);
  } else if (tmp <= 50) {
    fire_explosion(p_ptr->py, p_ptr->px, GF_MAKE_GLYPH, 4, 0);
  } else if (tmp <= 60) {
    recharge(60);
  } else if (tmp <= 70) {
    remove_curse();
  } else if (tmp <= 80) {
    ident_spell();
  } else if (tmp <= 90) {
    hp_player(1200);
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
  } else if (tmp <= 100) {
    wiz_lite();
  }
}


/* OK side effect */

void ok_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    teleport_player(50);
  } else if (tmp <= 20) {
    destroy_area(p_ptr->py, p_ptr->px, 7, TRUE);
  } else if (tmp <= 30) {
    int i;
    
    mprint(MSG_WARNING, "A dimensional gate appears!");

    for (i = 0; i < 60; i++) {
      summon_specific(p_ptr->py, p_ptr->px, 1, 0);
    }

  } else if (tmp <= 40) {
    lite_area(1, 6);
  } else if (tmp <= 50) {
    unlite_area(1, 6);
  } else if (tmp <= 60) {
    fire_explosion(p_ptr->py, p_ptr->px, GF_MAKE_DOOR, 9, 0);
  } else if (tmp <= 70) {
    mprint(MSG_WARNING, "The world twists.");
    chaos_destroy_area(p_ptr->py, p_ptr->px, 7);
  } else if (tmp <= 80) {
    mprint(MSG_WARNING, "Your body mutates!");
    generate_mutation();
  } else if (tmp <= 90) {
    remove_mutation();
  } else if (tmp <= 100) {
    change_shape(rand_range(SHAPE_ABOMINATION, SHAPE_COLD_CLOUD));
  }
}


/* Neutral side effect. */

void neutral_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific(p_ptr->py, p_ptr->px, 
			p_ptr->depth, 0)) {
      mprint(MSG_WARNING, "Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    curse_armor();
  } else if (tmp <= 30) {
    curse_weapon();
  } else if (tmp <= 40) {
    msg_print("Hundreds of voices wail in agony...");
    genocide();
  } else if (tmp <= 50) {
    msg_print("Thousands of voices wail in agony...");
    mass_genocide();
  } else if (tmp <= 60) {
    mprint(MSG_WARNING, "Your body mutates!");
    generate_mutation();
  } else if (tmp <= 70) {
    teleport_player_level();
  } else if (tmp <= 80) {
    change_shape(SHAPE_ABOMINATION);
  } else if (tmp <= 90) {
    earthquake();
  } else if (tmp <= 100) {
    fire_explosion(p_ptr->py, p_ptr->px, GF_QUAKE, 6, 20);
  }
}


/* Nasty side effect. */

void nasty_side_effect(void) {
  int tmp;

  tmp = randint(100);
  if (tmp < 10) {
    set_poisoned(p_ptr->poisoned + 10 + randint(10));
  } else if (tmp < 20) {
    set_confused(p_ptr->confused + 10 + randint(10));
  } else if (tmp < 30) {
    set_blind(p_ptr->blind + 10 + randint(10));
  } else if (tmp < 40) {
    set_slow(p_ptr->slow + 10 + randint(10));
  } else if (tmp < 50) {
    set_cut(p_ptr->cut + 10 + randint(100));
  } else if (tmp < 60) {
    set_stun(p_ptr->stun + randint(110));
  } else if (tmp < 70) {
    mprint(MSG_WARNING, "You hear a loud shriek!");
    aggravate_monsters(1);
  } else if (tmp < 80) {
    set_image(p_ptr->image + 20 + randint(20));
  } else if (tmp < 90) {
    mprint(MSG_WARNING, "You feel very sick.");
    set_slow(p_ptr->slow + 10 + randint(10));
    set_confused(p_ptr->confused + 10 + randint(10));
    set_poisoned(p_ptr->poisoned + 10 + randint(10));
  } else if (tmp < 100) {
    if (summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth+randint(7), 0)) {
      mprint(MSG_WARNING, "Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else {
    chaos_destroy_area(p_ptr->py, p_ptr->px, 15);
  }
}


/* Deadly side effect. */

void deadly_side_effect(bool god) {
  int tmp;
  bool (*boom)(int, int, int, int, int);

  /* Lisp-like hack to save typing. */
  if (god) {
    boom = fire_godly_wrath;
  } else {
    boom = fire_explosion;
  }

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific(p_ptr->py, p_ptr->px, 
			p_ptr->depth+20, 0)) {
      mprint(MSG_URGENT, "Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    boom(p_ptr->py, p_ptr->px, GF_MAKE_WALL, 9, 0);
  } else if (tmp <= 30) {
    mprint(MSG_WARNING, "The world twists!");
    boom(p_ptr->py, p_ptr->px, GF_QUAKE, 9, 60);
  } else if (tmp <= 40) {
    mprint(MSG_URGENT, "Your nerves and muscles feel weak and lifeless.");
    (void)dec_stat(A_STR, 50, TRUE);
    (void)dec_stat(A_INT, 50, TRUE);
    (void)dec_stat(A_WIS, 50, TRUE);
    (void)dec_stat(A_DEX, 50, TRUE);
    (void)dec_stat(A_CON, 50, TRUE);
    (void)dec_stat(A_CHR, 50, TRUE);    
  } else if (tmp <= 50) {
    mprint(MSG_URGENT, "You feel somehow inadequate...");
    p_ptr->exp -= (p_ptr->exp / 4);
    p_ptr->max_exp -= (p_ptr->exp / 4);
    check_experience();
  } else if (tmp <= 60) {
    mprint(MSG_URGENT, "Your whole life flashes before your eyes.");
    boom(p_ptr->py, p_ptr->px, GF_TIME, 5, 100);
  } else if (tmp <= 70) {
    mprint(MSG_URGENT, "Everything seems grayer somehow...");
    boom(p_ptr->py, p_ptr->px, GF_DISENCHANT, 5, 100);
  } else if (tmp <= 80) {
    boom(p_ptr->py, p_ptr->px, GF_MAKE_TRAP, 9, 0);
  } else if (tmp <= 90) {
    mprint(MSG_URGENT, "Something is trying to destroy your brain!");
    boom(p_ptr->py, p_ptr->px, GF_BRAIN_SMASH, 5, 100);
  } else if (tmp <= 100) {
    godly_wrath_blast();
  }
}


/* 
 * Fire a godly blast from the sky. 
 * Note that only attacks which are not resisted are used.
 * (Gods are omnipotent, aren't they?)
 */

void godly_wrath_blast(void) {
  int tmp;
  int type = 0;
  bool ok = FALSE;

  while (1) {
    tmp = randint(10);

    switch (tmp) {
    case 1:
      if (!p_ptr->immune_acid) {
	type = GF_ACID;
	ok = TRUE;
	mprint(MSG_STUPID, "You are blasted by acid from the sky!");
      }
      break;

    case 2:
      if (!p_ptr->immune_elec) {
	type = GF_ELEC;
	ok = TRUE;
	mprint(MSG_STUPID, "You are blasted by a giant ball lightning from the sky!");
      }
      break;

    case 3:
      if (!(p_ptr->resist_pois || p_ptr->oppose_pois)) {
	type = GF_POIS;
	ok = TRUE;
	mprint(MSG_STUPID, "A poisonous cloud descends from the sky!");
      }
      break;

    case 4:
      if (!p_ptr->resist_nethr) {
	type = GF_NETHER;
	ok = TRUE;
	mprint(MSG_STUPID, "A force of death surrounds you!");
      }
      break;

    case 5:
      if (!(p_ptr->resist_sound || p_ptr->resist_confu)) {
	type = GF_WATER;
	ok = TRUE;
	mprint(MSG_STUPID, "A flood of water falls from the sky!");
      }
      break;

    case 6:
      if (!p_ptr->resist_chaos) {
	type = GF_CHAOS;
	ok = TRUE;
	mprint(MSG_STUPID, "You are blasted by a gale of chaos!");
      }
      break;

    case 7:
      if (!p_ptr->resist_shard) {
	type = GF_SHARD;
	ok = TRUE;
	mprint(MSG_STUPID, "A multitude of shards descends on you from the sky!");
      }
      break;

    case 8:
      if (!p_ptr->resist_disen) {
	type = GF_DISENCHANT;
	ok = TRUE;
	mprint(MSG_STUPID, "You feel your magical aura wane!");
      }
      break;

    case 9:
      type = GF_TIME;
      ok = TRUE;
      mprint(MSG_STUPID, "A dizzying array of blinking dots falls from the sky!");
      break;

    case 10:
      type = GF_METEOR;
      ok = TRUE;
      mprint(MSG_STUPID, "A giant meteor falls on you from the sky!");
      break;
    }
    
    if (ok) break;
  }

  fire_godly_wrath(p_ptr->py, p_ptr->px, type, 1, damroll(4, 5));
}

/*
 * Return a description of a given attack type.
 */

void describe_attack(int type, char* r) {
  switch (type) {
  case GF_ARROW:       strcpy(r, "small arrows"); break; 
  case GF_MISSILE:     strcpy(r, "small darts"); break;
  case GF_MANA:        strcpy(r, "pure energy"); break;
  case GF_HOLY_ORB:    strcpy(r, "holy aura"); break;
  case GF_LITE_WEAK:   strcpy(r, "light"); break;
  case GF_DARK_WEAK:   strcpy(r, "dark"); break;
  case GF_WATER:       strcpy(r, "water"); break;
  case GF_PLASMA:      strcpy(r, "white-hot fire"); break;
  case GF_METEOR:      strcpy(r, "meteors"); break;
  case GF_ICE:         strcpy(r, "ice"); break;
  case GF_GRAVITY:     strcpy(r, "heavyness"); break;
  case GF_INERTIA:     strcpy(r, "impelling force"); break;
  case GF_FORCE:       strcpy(r, "force"); break;
  case GF_TIME:        strcpy(r, "pure time"); break;
  case GF_ACID:        strcpy(r, "acid"); break;
  case GF_ELEC:        strcpy(r, "lightning"); break;
  case GF_FIRE:        strcpy(r, "flames"); break;
  case GF_COLD:        strcpy(r, "cold"); break;
  case GF_POIS:        strcpy(r, "pungent gas"); break;
  case GF_LITE:        strcpy(r, "pure light"); break;
  case GF_DARK:        strcpy(r, "pure dark"); break;
  case GF_CONFUSION:   strcpy(r, "scintillating colors"); break;
  case GF_SOUND:       strcpy(r, "high-pitched noise"); break;
  case GF_SHARD:       strcpy(r, "shards"); break;
  case GF_NEXUS:       strcpy(r, "black, heavy clouds"); break;
  case GF_NETHER:      strcpy(r, "nothingness"); break;
  case GF_CHAOS:       strcpy(r, "chaos"); break;
  case GF_DISENCHANT:  strcpy(r, "emptyness"); break;
  case GF_QUAKE:       strcpy(r, "space-time distortion"); break;
  case GF_KILL_WALL:
  case GF_KILL_DOOR:
  case GF_KILL_TRAP:
  case GF_MAKE_WALL:
  case GF_MAKE_DOOR:
  case GF_MAKE_TRAP:
  case GF_MAKE_GLYPH:
  case GF_MAKE_STAIR:
    strcpy(r, "swirling pebbles");
    break;
  case GF_AWAY_UNDEAD:
  case GF_AWAY_EVIL:
  case GF_AWAY_ALL:
  case GF_AWAY_ALL_VERT:
  case GF_RECALL:
    strcpy(r, "forbidding noise"); 
    break;
  case GF_DISP_UNDEAD:
  case GF_DISP_EVIL:
  case GF_DISP_ALL:
    strcpy(r, "terrifying noise");
    break;
  case GF_CLONE:   strcpy(r, "popping sounds"); break;
  case GF_POLY:    strcpy(r, "echoing sounds"); break;
  case GF_SPEED:   strcpy(r, "fast-pased rhythmic sounds"); break;
  case GF_SLOW:    strcpy(r, "slow, sonorous music"); break;
  case GF_CONF:    strcpy(r, "discordant sounds"); break;
  case GF_SLEEP:   strcpy(r, "soothing music"); break;
  case GF_DRAIN:   strcpy(r, "sucking noise"); break;
  case GF_CHAOS_DESTRUCTION:
  case GF_WALL_TO_CHAOS: strcpy(r, "swirling motes of chaos"); break;
  case GF_MIND_BLAST:  strcpy(r, "stifling, smelly air"); break;
  case GF_BRAIN_SMASH: strcpy(r, "essence of stupidity"); break;
  case GF_EARTHQUAKE:
  case GF_WORD_OF_DESTRUCTION: strcpy(r, "minuature whirlwinds"); break;

  case GF_DETECT_DOOR:
  case GF_DETECT_TRAP:
  case GF_DETECT_STAIR:
  case GF_DETECT_TREASURE:
  case GF_DETECT_GOLD:
  case GF_DETECT_OBJECT:
  case GF_DETECT_MAGIC:
  case GF_DETECT_MONSTER:
  case GF_DETECT_INVIS:
  case GF_DETECT_EVIL:
  case GF_DETECT_ANY:
    strcpy(r, "blinking lights");
    break;

  case GF_HEAL:
  case GF_HEAL_INSANITY:
  case GF_HEAL_FEAR:
  case GF_HEAL_CUT:
  case GF_HEAL_STUN:
  case GF_HEAL_POISON:
  case GF_HEAL_DEX:
  case GF_HEAL_CHR:
  case GF_HEAL_STR:
  case GF_HEAL_CON:
  case GF_HEAL_WIS:
  case GF_HEAL_INT:
  case GF_HEAL_LIFE:
    strcpy(r, "soothing noise");
    break;

  case GF_SHIELD:   strcpy(r, "solidified air"); break;
  case GF_HEROISM:  strcpy(r, "exciting music"); break;
  case GF_BLESS:    strcpy(r, "serene hymns"); break;
  case GF_FOOD:     strcpy(r, "delicious smells"); break;
  case GF_HEAVY_UNCURSE:
  case GF_UNCURSE:  strcpy(r, "high-pitched noise"); break;
  case GF_RECHARGE: strcpy(r, "miniature lightnings"); break;
  case GF_IDENT:
  case GF_HEAVY_IDENT:
  case GF_SUPER_IDENT:
    strcpy(r, "low-pitched mumbling"); break;

  case GF_RES_FIRE: strcpy(r, "heatless flames"); break;
  case GF_RES_COLD: strcpy(r, "snowflakes"); break;
  case GF_RES_ACID: strcpy(r, "acidic raindrops"); break;
  case GF_RES_ELEC: strcpy(r, "crackling sparks"); break;
  case GF_RES_POIS: strcpy(r, "smelly slime"); break;

  case GF_MASS_GENOCIDE:
  case GF_GENOCIDE: strcpy(r, "terrifying shrieking"); break;

  case GF_ENCHANT_TO_HIT:
  case GF_ENCHANT_TO_DAM:
  case GF_ENCHANT_AC:
  case GF_BRAND_AMMO:
  case GF_BRAND_WEAPON:
    strcpy(r, "blue glow"); break;

  case GF_MAKE_MONSTER:
  case GF_MAKE_PET:
    strcpy(r, "beckoning whispering"); break;

  case GF_ALTER: strcpy(r, "deranged sounds"); break;
    
  default:
    /* Get something silly randomly */
    get_random_line("sfail.txt", r);
    break;
  }
}


/*
 * Describe the attack using normal names. 
 */

void describe_attack_fully(int type, char* r) {
  switch (type) {
  case GF_ARROW:       strcpy(r, "arrows"); break; 
  case GF_MISSILE:     strcpy(r, "magic missiles"); break;
  case GF_MANA:        strcpy(r, "mana"); break;
  case GF_HOLY_ORB:    strcpy(r, "holy aura"); break;
  case GF_LITE_WEAK:   strcpy(r, "light"); break;
  case GF_DARK_WEAK:   strcpy(r, "dark"); break;
  case GF_WATER:       strcpy(r, "water"); break;
  case GF_PLASMA:      strcpy(r, "plasma"); break;
  case GF_METEOR:      strcpy(r, "meteors"); break;
  case GF_ICE:         strcpy(r, "ice"); break;
  case GF_GRAVITY:     strcpy(r, "gravity"); break;
  case GF_INERTIA:     strcpy(r, "inertia"); break;
  case GF_FORCE:       strcpy(r, "force"); break;
  case GF_TIME:        strcpy(r, "pure time"); break;
  case GF_ACID:        strcpy(r, "acid"); break;
  case GF_ELEC:        strcpy(r, "lightning"); break;
  case GF_FIRE:        strcpy(r, "flames"); break;
  case GF_COLD:        strcpy(r, "cold"); break;
  case GF_POIS:        strcpy(r, "poison"); break;
  case GF_LITE:        strcpy(r, "pure light"); break;
  case GF_DARK:        strcpy(r, "pure dark"); break;
  case GF_CONFUSION:   strcpy(r, "confusion"); break;
  case GF_SOUND:       strcpy(r, "sound"); break;
  case GF_SHARD:       strcpy(r, "shards"); break;
  case GF_NEXUS:       strcpy(r, "nexus"); break;
  case GF_NETHER:      strcpy(r, "nether"); break;
  case GF_CHAOS:       strcpy(r, "chaos"); break;
  case GF_DISENCHANT:  strcpy(r, "disenchantment"); break;
  case GF_QUAKE:       strcpy(r, "space-time distortion"); break;
  case GF_KILL_WALL:   strcpy(r, "wall destruction"); break;
  case GF_KILL_DOOR:   strcpy(r, "door destruction"); break;
  case GF_KILL_TRAP:   strcpy(r, "trap destruction"); break;
  case GF_MAKE_WALL:   strcpy(r, "wall creation"); break;
  case GF_MAKE_DOOR:   strcpy(r, "door creation"); break;
  case GF_MAKE_TRAP:   strcpy(r, "trap creation"); break;
  case GF_MAKE_GLYPH:   strcpy(r, "glyph of warding"); break;
  case GF_MAKE_STAIR:   strcpy(r, "stair creation"); break;

  case GF_AWAY_UNDEAD:  strcpy(r, "teleport undead"); break;
  case GF_AWAY_EVIL:    strcpy(r, "teleport evil");   break;
  case GF_AWAY_ALL:     strcpy(r, "teleport monster"); break;

  case GF_DISP_UNDEAD:  strcpy(r, "turn undead"); break;
  case GF_DISP_EVIL:    strcpy(r, "turn evil");   break;
  case GF_DISP_ALL:     strcpy(r, "fear");        break;

  case GF_CLONE:   strcpy(r, "clone monster"); break;
  case GF_POLY:    strcpy(r, "polymorph"); break;
  case GF_HEAL:    strcpy(r, "healing"); break;
  case GF_SPEED:   strcpy(r, "speed"); break;
  case GF_SLOW:    strcpy(r, "slowness"); break;
  case GF_CONF:    strcpy(r, "confusion"); break;
  case GF_SLEEP:   strcpy(r, "sleep"); break;
  case GF_DRAIN:   strcpy(r, "life leech"); break;
  case GF_WALL_TO_CHAOS:
  case GF_CHAOS_DESTRUCTION:
    strcpy(r, "chaotic destruction"); break;
  case GF_MIND_BLAST:  strcpy(r, "mind blast"); break;
  case GF_BRAIN_SMASH: strcpy(r, "brain smash"); break;
  case GF_EARTHQUAKE:
    strcpy(r, "earthquake"); break;
  case GF_WORD_OF_DESTRUCTION:
    strcpy(r, "destruction"); break;


  case GF_AWAY_ALL_VERT:   strcpy(r, "teleport vertically"); break;
  case GF_RECALL:          strcpy(r, "recall"); break;
  case GF_ALTER:           strcpy(r, "alteration"); break;
  case GF_DETECT_DOOR:     strcpy(r, "detect doors"); break;
  case GF_DETECT_TRAP:     strcpy(r, "detect traps"); break;
  case GF_DETECT_STAIR:    strcpy(r, "detect stairs"); break;
  case GF_DETECT_TREASURE: strcpy(r, "detect treasure"); break;
  case GF_DETECT_GOLD:     strcpy(r, "detect gold"); break;
  case GF_DETECT_OBJECT:   strcpy(r, "detect object"); break;
  case GF_DETECT_MAGIC:    strcpy(r, "detect magic"); break;
  case GF_DETECT_MONSTER:  strcpy(r, "detect monster"); break;
  case GF_DETECT_INVIS:    strcpy(r, "detect invisible"); break;
  case GF_DETECT_EVIL:     strcpy(r, "detect evil"); break;
  case GF_DETECT_ANY:      strcpy(r, "detection"); break;
  case GF_HEAL_INSANITY:   strcpy(r, "heal insanity"); break;
  case GF_HEAL_FEAR:       strcpy(r, "remove fear"); break;
  case GF_HEAL_CUT:        strcpy(r, "heal cuts"); break;
  case GF_HEAL_STUN:       strcpy(r, "heal stun"); break;
  case GF_HEAL_POISON:     strcpy(r, "neutralize poison"); break;
  case GF_HEAL_DEX:        strcpy(r, "restore dexterity"); break;
  case GF_HEAL_CHR:        strcpy(r, "restore charisma"); break;
  case GF_HEAL_STR:        strcpy(r, "restore strength"); break;
  case GF_HEAL_CON:        strcpy(r, "restore constitution"); break;
  case GF_HEAL_WIS:        strcpy(r, "restore wisdom"); break;
  case GF_HEAL_INT:        strcpy(r, "restore intelligence"); break;
  case GF_HEAL_LIFE:       strcpy(r, "restore life levels"); break;
  case GF_SHIELD:          strcpy(r, "shield"); break;
  case GF_HEROISM:         strcpy(r, "heroism"); break;
  case GF_BLESS:           strcpy(r, "blessing"); break;
  case GF_FOOD:            strcpy(r, "satisfy hunger"); break;
  case GF_UNCURSE:         strcpy(r, "remove curse"); break;
  case GF_RECHARGE:        strcpy(r, "recharge"); break;
  case GF_HEAVY_UNCURSE:   strcpy(r, "*remove curse*"); break;
  case GF_IDENT:           strcpy(r, "identify"); break;
  case GF_HEAVY_IDENT:     strcpy(r, "*identify*"); break;
  case GF_SUPER_IDENT:     strcpy(r, "**identify**"); break;
  case GF_RES_FIRE:        strcpy(r, "resist fire"); break;
  case GF_RES_COLD:        strcpy(r, "resist cold"); break;
  case GF_RES_ELEC:        strcpy(r, "resist electricity"); break;
  case GF_RES_ACID:        strcpy(r, "resist acid"); break;
  case GF_RES_POIS:        strcpy(r, "resist poison"); break;
  case GF_GENOCIDE:        strcpy(r, "genocide"); break;
  case GF_MASS_GENOCIDE:   strcpy(r, "mass genocide"); break;
  case GF_ENCHANT_TO_HIT:  strcpy(r, "enchant to-hit"); break;
  case GF_ENCHANT_TO_DAM:  strcpy(r, "enchant to-dam"); break;
  case GF_ENCHANT_AC:      strcpy(r, "enchant AC"); break;
  case GF_BRAND_AMMO:      strcpy(r, "brand ammo"); break;
  case GF_BRAND_WEAPON:    strcpy(r, "brand weapon"); break;
  case GF_MAKE_MONSTER:    strcpy(r, "summon monster"); break;
  case GF_MAKE_PET:        strcpy(r, "summon pet"); break;

  default:
    strcpy(r, "something unknown");
    break;
  }
}


/*
 * Explode an object, currently only potions and flasks.
 */

bool explode_object(object_type* o_ptr, cptr o_name, int y, int x) {
  int type;
  int rad = 5;
  int dam = 0;

  if (!o_ptr->k_idx) {
    return FALSE;
  }

  if (o_ptr->tval != TV_POTION && o_ptr->tval != TV_FLASK) {
    return FALSE;
  }

  if (o_ptr->tval == TV_FLASK) {
    type = GF_FIRE;
    dam = damroll(2, 6);
    rad = 2;
  } else {

    switch (o_ptr->sval) {
    case SV_POTION_SLOWNESS:
      type = GF_SLOW;
      break;

    case SV_POTION_POISON:
      type = GF_POIS;
      break;

    case SV_POTION_BLINDNESS:
      type = GF_DARK_WEAK;
      break;

    case SV_POTION_CONFUSION:
      type = GF_CONF;
      break;

    case SV_POTION_SLEEP:
      type = GF_SLEEP;
      break;

    case SV_POTION_RUINATION:
      type = GF_TIME;
      dam = damroll(10, 40);
      break;

    case SV_POTION_DEATH:
      type = GF_NETHER;
      dam = 5000;
      break;

    case SV_POTION_DETONATIONS:
      type = GF_PLASMA;
      dam = damroll(50, 20);
      break;

    case SV_POTION_SPEED:
      type = GF_SPEED;
      break;

    case SV_POTION_STAR_HEALING:
      dam += damroll(2, 6);
    case SV_POTION_HEALING:
      dam += damroll(2, 6);
    case SV_POTION_CURE_CRITICAL:
      dam += damroll(2, 6);
    case SV_POTION_CURE_SERIOUS:
      dam += damroll(2, 6);
    case SV_POTION_CURE_LIGHT:
      dam += damroll(2, 6);
      type = GF_HEAL;
      break;

    case SV_POTION_MUTATION:
      type = GF_CHAOS;
      break;

    default:
      return FALSE;
    }
  }

  mformat(MSG_WARNING, "The %s explodes!", o_name);
  fire_explosion(y, x, type, rad, dam);  
  return TRUE;
}


/*
 * Return a number denoting your current standing with your god,
 * ranging from 0 (really bad) to 10 (really good).
 * Note that 0-5 mean ``cursed'', 6-7 mean ``neutral'',
 * and 8-10 mean ``blessed''.
 */

int interpret_grace(void) {
  if (p_ptr->grace <      -60000) return 0;
  else if (p_ptr->grace < -50000) return 1;
  else if (p_ptr->grace < -40000) return 2;
  else if (p_ptr->grace < -30000) return 3;
  else if (p_ptr->grace < -20000) return 4;
  else if (p_ptr->grace < -10000) return 5;
  else if (p_ptr->grace <      0) return 6;
  
  else if (p_ptr->grace >  20000) return 10;
  else if (p_ptr->grace >  10000) return 9;
  else if (p_ptr->grace >    200) return 8;
  else if (p_ptr->grace >      0) return 7;

  /* Should never happen! */
  return -1;
}


/*
 * Same as interpret_grace, but for p_ptr->god_favor.
 */

int interpret_favor(void) {
  if (p_ptr->god_favor <      -60000) return 0;
  else if (p_ptr->god_favor < -50000) return 1;
  else if (p_ptr->god_favor < -40000) return 2;
  else if (p_ptr->god_favor < -30000) return 3;
  else if (p_ptr->god_favor < -20000) return 4;
  else if (p_ptr->god_favor < -10000) return 5;
  else if (p_ptr->god_favor <      0) return 6;
  
  else if (p_ptr->god_favor >  20000) return 10;
  else if (p_ptr->god_favor >  10000) return 9;
  else if (p_ptr->god_favor >    200) return 8;
  else if (p_ptr->god_favor >      0) return 7;

  /* Should never happen! */
  return -1;
}
