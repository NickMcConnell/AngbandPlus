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

#define REWARD_CHANCE 10


extern void do_poly_self();
extern void do_poly_wounds();
extern bool curse_weapon();
extern bool curse_armor();
extern void random_resistance(object_type * q_ptr, bool is_scroll, int specific);


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
            msg_print("Oh, wow! Everything looks so cosmic now!");
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
            msg_print("Your skin turns to stone.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->shield)
		{
            msg_print("Your skin returns to normal.");
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
 * Set "p_ptr->set_shadow", notice observable changes
 */
bool set_shadow(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
        if (!p_ptr->wraith_form)
		{

            msg_print("You leave the physical world and turn into a wraith-being!");
			notice = TRUE;

            {
                /* Redraw map */
                p_ptr->redraw |= (PR_MAP);

                /* Update monsters */
                p_ptr->update |= (PU_MONSTERS);

                /* Window stuff */
                p_ptr->window |= (PW_OVERHEAD);
            }
		}
	}

	/* Shut */
	else
	{
        if (p_ptr->wraith_form)
		{
            msg_print("You feel opaque.");
			notice = TRUE;
            {
                /* Redraw map */
                p_ptr->redraw |= (PR_MAP);

                /* Update monsters */
                p_ptr->update |= (PU_MONSTERS);

                /* Window stuff */
                p_ptr->window |= (PW_OVERHEAD);
            }
		}
	}

	/* Use the value */
    p_ptr->wraith_form = v;

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

            msg_print("Invulnerability!");
			notice = TRUE;

            {
                /* Redraw map */
                p_ptr->redraw |= (PR_MAP);

                /* Update monsters */
                p_ptr->update |= (PU_MONSTERS);

                /* Window stuff */
                p_ptr->window |= (PW_OVERHEAD);
            }
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->invuln)
		{
            msg_print("The invulnerability wears off.");
			notice = TRUE;
            {
                /* Redraw map */
                p_ptr->redraw |= (PR_MAP);

                /* Update monsters */
                p_ptr->update |= (PU_MONSTERS);

                /* Window stuff */
                p_ptr->window |= (PW_OVERHEAD);
            }
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
            msg_print("You feel your consciousness expand!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
        if (p_ptr->tim_esp)
		{
            msg_print("Your consciousness contracts again.");
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

    if (p_ptr->prace == RACE_GOLEM)
        v = 0;

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
			msg_print("You have been stunned.");
			break;

			/* Heavy stun */
			case 2:
			msg_print("You have been heavily stunned.");
			break;

			/* Knocked out */
			case 3:
			msg_print("You have been knocked out.");
			break;
		}

        if (randint(1000)<v || randint(16)==1)
        {

            msg_print("A vicious blow hits your head.");
            if(randint(3)==1)
            {
                if (!p_ptr->sustain_int) { (void) do_dec_stat(A_INT); }
                if (!p_ptr->sustain_wis) { (void) do_dec_stat(A_WIS); }
            }
            else if (randint(2)==1)
            {
                if (!p_ptr->sustain_int) { (void) do_dec_stat(A_INT); }
            }
            else
            {
                if (!p_ptr->sustain_wis) { (void) do_dec_stat(A_WIS); }
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
			msg_print("You are no longer stunned.");
			if (disturb_state) disturb(0, 0);
			break;
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

    if (p_ptr->prace == RACE_GOLEM || p_ptr->prace == RACE_SKELETON ||
        p_ptr->prace == RACE_SPECTRE )
        v = 0;
    else if (p_ptr->prace == RACE_ZOMBIE && (skill_set[SKILL_RACIAL].value/2) > 11)
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
			/* Graze */
			case 1:
			msg_print("You have been given a graze.");
			break;

			/* Light cut */
			case 2:
			msg_print("You have been given a light cut.");
			break;

			/* Bad cut */
			case 3:
			msg_print("You have been given a bad cut.");
			break;

			/* Nasty cut */
			case 4:
			msg_print("You have been given a nasty cut.");
			break;

			/* Severe cut */
			case 5:
			msg_print("You have been given a severe cut.");
			break;

			/* Deep gash */
			case 6:
			msg_print("You have been given a deep gash.");
			break;

			/* Mortal wound */
			case 7:
			msg_print("You have been given a mortal wound.");
			break;
		}

		/* Notice */
		notice = TRUE;

    if (randint(1000)<v || randint(16)==1)
        { 
            if(!p_ptr->sustain_chr)
            {
            msg_print("You have been horribly scarred.");

            do_dec_stat(A_CHR);
            }
        }

	}



	/* Decrease cut */
	else if (new_aux < old_aux)
	{
		/* Describe the state */
		switch (new_aux)
		{
			/* None */
			case 0:
			msg_print("You are no longer bleeding.");
			if (disturb_state) disturb(0, 0);
			break;
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
			msg_print("You are still weak.");
			break;

			/* Hungry */
			case 2:
			msg_print("You are still hungry.");
			break;

			/* Normal */
			case 3:
			msg_print("You are no longer hungry.");
			break;

			/* Full */
			case 4:
			msg_print("You are full!");
			break;

			/* Bloated */
			case 5:
			msg_print("You have gorged yourself!");
			break;
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
			msg_print("You are getting faint from hunger!");
			break;

			/* Weak */
			case 1:
			msg_print("You are getting weak from hunger!");
			break;

			/* Hungry */
			case 2:
			msg_print("You are getting hungry.");
			break;

			/* Normal */
			case 3:
			msg_print("You are no longer full.");
			break;

			/* Full */
			case 4:
			msg_print("You are no longer gorged.");
			break;
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
 * Gain experience
 */
void gain_exp(s32b amount)
{
	/* Gain some experience */
	p_ptr->exp += amount;
}

/* Gain random skills */
void gain_skills(s32b amount)
{
	int i;
	for(i=0;i<amount;i++)
	{
		skill_exp(rand_int(MAX_SKILLS));
	}
}

/*
 * Lose experience
 */
void lose_skills(s32b amount)
{
	byte lost[MAX_SKILLS];
	int i,j,which,prev;

	/* clear the initial array */
	for (i=0;i<MAX_SKILLS;i++) lost[i] = 0;
	/* dummy 'previous' */
	prev=rand_int(MAX_SKILLS);

	/* Repeat 'amount' times */
	for (i=0;i<amount;i++)
	{
		/* Have 1000 goes at picking a random skill */
		for(j=0;j<1000;j++)
		{
			which=rand_int(MAX_SKILLS);
			/* 60% chance of continuing with previous skill */
			if(rand_int(100) < 60) which = prev;
			/* Never remove a skill completely */
			if (skill_set[which].value > 1)
			{
				/* Decrement the skill */
				skill_set[which].value--;
				/* Keep count */
				lost[which]++;
				/* This was valid so stop the loop */
				j = 1000;
				prev = which;
			}
		}
	}
	/* Tell the player what has happened */
	for(i=0;i<MAX_SKILLS;i++)
	{
		if (lost[i] > 0)
		{
			msg_format("Your %s skill has been lost (%d%%->%d%%)",skill_set[i].name,skill_set[i].value+lost[i],skill_set[i].value);
		}
	}
	/* Re-calculate some things */
	calc_hitpoints(); /* The hit-points might have changed */
	calc_mana(); /* As might mana */
	calc_spells(); /* And spells */
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
	int i, j, y, x, ny, nx;

	int dump_item = 0;
	int dump_gold = 0;

	int number = 0;
	int total = 0;
	int q_idx = 0;

	bool quest = FALSE;
	

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));

	bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
	bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

	bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
	bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

    bool cloned = FALSE;

	int force_coin = get_coin_type(r_ptr);

	object_type forge;
	object_type *q_ptr;


	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

    if (m_ptr->smart &(SM_CLONED))
        cloned = TRUE;

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
		q_ptr = &forge;

		/* Copy the object */
		object_copy(q_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(q_ptr, -1, y, x);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;

    /* Mega^2-hack -- destroying the Stormbringer gives it us! */
    if (strstr((r_name + r_ptr->name),"Stormbringer"))
	{
		/* Get local object */
		q_ptr = &forge;

        /* Prepare to make the Stormbringer */
        object_prep(q_ptr, lookup_kind(TV_SWORD, SV_BLADE_OF_CHAOS));

        /* Mega-Hack -- Name the sword  */

        q_ptr->art_name = quark_add("'Stormbringer'");
        q_ptr->to_h = 16;
        q_ptr->to_d = 16;
        q_ptr->ds = 6;
        q_ptr->dd = 6;
        q_ptr->pval = 2;

        q_ptr->art_flags1 |= ( TR1_VAMPIRIC | TR1_STR | TR1_CON );
        q_ptr->art_flags2 |= ( TR2_FREE_ACT | TR2_HOLD_LIFE |
                               TR2_RES_NEXUS | TR2_RES_CHAOS | TR2_RES_NETHER |
                               TR2_RES_CONF ); /* No longer resist_disen */
        q_ptr->art_flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
                            TR3_IGNORE_FIRE | TR3_IGNORE_COLD);
                            /* Just to be sure */

        q_ptr->art_flags3 |= TR3_NO_TELE; /* How's that for a downside? */

        /* For game balance... */
        q_ptr->art_flags3 |= (TR3_CURSED | TR3_HEAVY_CURSE);
        q_ptr->ident |= IDENT_CURSED;

        if (randint(2)==1) q_ptr->art_flags3 |= (TR3_DRAIN_EXP);
        q_ptr->art_flags3 |= (TR3_AGGRAVATE);

		/* Drop it in the dungeon */
        drop_near(q_ptr, -1, y, x);

    }

    /* Mega^3-hack: killing an 'Avatar of Nyarlathotep' is likely to
       spawn another in the fallen one's place! */
    else if (strstr((r_name + r_ptr->name),"Avatar of Nyarlathotep"))
    {
        if (!(randint(15)==13))
        {
            int wy = py, wx = px;
            int attempts = 100;

            do {
                scatter(&wy, &wx, py, px, 20, 0);
                }
                while (!(in_bounds(wy,wx) && cave_floor_bold(wy,wx)) && (cave[wy][wx].feat != FEAT_WATER) &&
                       --attempts);

            if (attempts > 0)
            {
             if (m_ptr->smart & SM_ALLY)
             {
                 if (summon_specific_friendly(wy, wx, 100, SUMMON_AVATAR, FALSE))
                 {
                    if (player_can_see_bold(wy, wx))
                         msg_print ("Nyarlathotep creates a new avatar!");
                 }
             }
             else
             {
                 if (summon_specific(wy, wx, 100, SUMMON_AVATAR))
                 {
                     if (player_can_see_bold(wy, wx))
                         msg_print ("Nyarlathotep creates a new avatar!");
                 }
             }
            }
        }
        
    }

    /* One more ultra-hack: An Unmaker goes out with a big bang! */
    else if (strstr((r_name + r_ptr->name),"Unmaker"))
    {
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    (void)project(m_idx, 6, y, x, 100, GF_CHAOS, flg);
    }

	/* Mega-Hack -- drop "winner" treasures */
    else if (r_ptr->flags1 & (RF1_DROP_CHOSEN))
	{
        if (strstr((r_name + r_ptr->name),"Azathoth"))
        {

            /* Get local object */
            q_ptr = &forge;

            /* Mega-Hack -- Prepare to make "Mighty Hammer of Worlds" */
            object_prep(q_ptr, lookup_kind(TV_HAFTED, SV_WORLDS));

            /* Mega-Hack -- Mark this item as "Mighty Hammer of Worlds" */
            q_ptr->name1 = ART_WORLDS;

            /* Mega-Hack -- Actually create "Mighty Hammer of Worlds" */
            apply_magic(q_ptr, -1, TRUE, TRUE, TRUE);

            /* Drop it in the dungeon */
            drop_near(q_ptr, -1, y, x);


            /* Get local object */
            q_ptr = &forge;

            /* Mega-Hack -- Prepare to make "Crown of the Universe" */
            object_prep(q_ptr, lookup_kind(TV_CROWN, SV_UNIVERSE));

            /* Mega-Hack -- Mark this item as "Crown of the Universe" */
            q_ptr->name1 = ART_UNIVERSE;

            /* Mega-Hack -- Actually create "Crown of the Universe" */
            apply_magic(q_ptr, -1, TRUE, TRUE, TRUE);

            /* Drop it in the dungeon */
            drop_near(q_ptr, -1, y, x);
        }
        else
        {
            byte a_idx = 0;
            int chance = 0;
            int I_kind = 0;

            if (strstr((r_name + r_ptr->name),"Groo"))
            {
                a_idx = ART_GROO;
                chance = 75;
            }

            if ((a_idx > 0) && ((randint(99)<chance) || (cheat_wzrd)))
            {
                if (a_info[a_idx].cur_num == 0)
                {
                   artifact_type *a_ptr = &a_info[a_idx];

                   /* Get local object */
                   q_ptr = &forge;

                   /* Wipe the object */
                   object_wipe(q_ptr);

                   /* Acquire the "kind" index */
                   I_kind = lookup_kind(a_ptr->tval, a_ptr->sval);

                   /* Create the artifact */
                   object_prep(q_ptr, I_kind);

                   /* Save the name */
                   q_ptr->name1 = a_idx;

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

                    a_info[a_idx].cur_num = 1;

                   /* Drop the artifact from heaven */
                   drop_near(q_ptr, -1, y, x);
                    

                }
            }
        }
	}


	/* Determine how much we can drop */
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
	if (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
	if (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
	if (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

    if (cloned) number = 0; /* Clones drop no stuff */
 	if ((is_quest(dun_level)) && ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)))
	{
		q_idx = get_quest_number ();
		q_list[q_idx].cur_num++;

		if (q_list[q_idx].cur_num == q_list[q_idx].max_num)
		{
			/* Drop at least 2 items (the stair will probably destroy one */
			number += 2;
			quest = TRUE;
			q_list[q_idx].level=0;
		}
	}




	/* Hack -- handle creeping coins */
	coin_type = force_coin;

	/* Average dungeon and monster levels */
	object_level = ((dun_level+dun_offset) + r_ptr->level) / 2;

	/* Drop some objects */
	for (j = 0; j < number; j++)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Make Gold, but not for first two quest items */
		if ((!quest || (j > 1)) && do_gold && (!do_item || (rand_int(100) < 50)))
		{
			/* Make some gold */
			if (!make_gold(q_ptr)) continue;

			/* XXX XXX XXX */
			dump_gold++;
		}

		/* Make Object */
		else
		{
			/* Make an object */
			if (!quest || (j>1))
			{
				if (!make_object(q_ptr, good, great)) continue;
			}
			else
			{
				/* The first two items for a quest monster are great */
				if (!make_object(q_ptr, TRUE, TRUE)) continue;
			}

			/* XXX XXX XXX */
			dump_item++;
		}

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	/* Reset the object level */
	object_level = (dun_level+dun_offset);

	/* Reset "coin" type */
	coin_type = 0;


	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}


	/* Only process "Quest Monsters" */
	if (!((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD))) return;

	/* Check if quest is complete (Heino Vander Sanden) */
	if (q_list[q_idx].cur_num != q_list[q_idx].max_num) return;

	/* No longer quest monster (Heino Vander Sanden) */
	r_ptr->flags1 ^= (RF1_GUARDIAN);

	/* Count incomplete quests (Heino Vander Sanden) */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		if (q_list[i].level || (q_list[i].cur_num != q_list[i].max_num)) total++;
	}



	/* Need some stairs */
	if (total)
	{
		/* but only if not at the max level */
		if (dun_level < dun_defs[cur_dungeon].max_level)
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

			/* XXX XXX XXX */
			delete_object(y, x);

			/* Explain the stairway */
			msg_print("A magical stairway appears...");

			if (dun_defs[cur_dungeon].tower)
			{
				/* Create stairs up */
				cave_set_feat(y, x, FEAT_LESS);
			}
			else
			{
				/* Create stairs down */
				cave_set_feat(y, x, FEAT_MORE);
			}

			/* Remember to update everything */
			p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
		}
	}
	/* Nothing left, game over... */
	else
	{
		/* Total winner */
		total_winner = TRUE;

		/* Redraw the "title" */
		p_ptr->redraw |= (PR_TITLE);

		/* Congratulations */
		msg_print("*** CONGRATULATIONS ***");
		msg_print("You have won the game!");
		msg_print("You may retire (commit suicide) when you are ready.");
	}
}




/*
 * Decreases monsters hit points, handling monster death.
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
 * Hack -- unseen monsters yield "You have killed it." message.
 *
 * Added fear (DGK) and check whether to print fear messages -CWS
 *
 * Genericized name, sex, and capitilization -BEN-
 *
 * As always, the "ghost" processing is a total hack.
 *
 * Hack -- we "delay" fear messages by passing around a "fear" flag.
 *
 * XXX XXX XXX Consider decreasing monster experience over time, say,
 * by using "(m_exp * m_lev * (m_lev)) / (p_lev * (m_lev + n_killed))"
 * instead of simply "(m_exp * m_lev) / (p_lev)", to make the first
 * monster worth more than subsequent monsters.  This would also need
 * to induce changes in the monster recall code.
 */
bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note)
{
	char tmp[1024];
	
	monster_type	*m_ptr = &m_list[m_idx];

	monster_race	*r_ptr = &r_info[m_ptr->r_idx];

	s32b		div, new_exp, new_exp_frac;


	/* Redraw (later) if needed */
	if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);


	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

       if ((r_ptr->flags3 & (RF3_GREAT_OLD_ONE)) && (randint(2)==1))
       {
            int curses = 1 + randint(3);
			msg_format("%^s retreats into another dimension!",m_name);
            msg_format("Nyarlathotep puts a terrible curse on you!", m_name);
            curse_equipment(100, 50);
            do { activate_ty_curse(); } while (--curses);
        }

       if (speak_unique && (r_ptr->flags2 & (RF2_CAN_SPEAK)))
			{
                char line_got[80];
                int reward=0;

                /* Dump a message */

                get_rnd_line("mondeath.txt", line_got);
                msg_format("%^s says: %s", m_name, line_got);

                if (randint(REWARD_CHANCE)==1)
                {
                    msg_format("There was a price on %s's head.", m_name);
                    get_rnd_line("crime.txt", line_got);
                    msg_format("%^s was wanted for %s", m_name, line_got);
                    reward = 250 * (randint (10) + r_ptr->level - 5);
 
                    if (reward > 32000) reward = 32000;/* Force 'good' values */
                    else if (reward < 250) reward = 250;

                    msg_format("You collect a reward of %d gold pieces.", reward);
                    p_ptr -> au += reward;
                    p_ptr->redraw |= (PR_GOLD);
                    

                }
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
		         (r_ptr->flags3 & (RF3_CTHULOID)) ||
		         (r_ptr->flags2 & (RF2_STUPID)) ||
                 (r_ptr->flags3 & (RF3_NONLIVING)) ||
		         (strchr("Evg", r_ptr->d_char)))
		{
			msg_format("You have destroyed %s.", m_name);
		}

		/* Death by Physical attack -- living monster */
		else
		{
			msg_format("You have slain %s.", m_name);
		}

		div = 10;

		/* Half experience for common monsters */
		if(r_ptr->r_pkills >= 19) 
		{
			div = div * 2;
		}
		/* Triple experience for first kill */
		if(r_ptr->r_pkills == 0)
		{
			div = div / 3;
		}
		/* double experience for second or third kill */
		if (r_ptr->r_pkills == 1) 
		{
			div = div /2;
		}
		if (r_ptr->r_pkills == 2) 
		{
			div = div/2;
		}
		/* don't divide by 0 */
		if (div < 1) div = 1;
		/* Give some experience for the kill */
		new_exp = ((long)r_ptr->mexp * r_ptr->level*10) / div;

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
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 0;

		/* XXX XXX Mega-Hack -- allow another ghost later
		 * Remove the slain bone file */
		if (m_ptr->r_idx == MAX_R_IDX-1)
		{
			r_ptr->max_num = 1;

			/* Delete the bones file */
			sprintf(tmp, "%s%sbone.%03d", ANGBAND_DIR_BONE, PATH_SEP, dun_level + dun_offset);
			
			fd_kill(tmp);
		}

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
		delete_monster_idx(m_idx,TRUE);

		/* Not afraid */
		(*fear) = FALSE;

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
		int		percentage;

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
 * Calculates current boundaries
 * Called below and from "do_cmd_locate()".
 */
void panel_bounds(void)
{
	panel_row_min = panel_row * (SCREEN_HGT / 2);
	panel_row_max = panel_row_min + SCREEN_HGT - 1;
	panel_row_prt = panel_row_min - 1;
	panel_col_min = panel_col * (SCREEN_WID / 2);
	panel_col_max = panel_col_min + SCREEN_WID - 1;
	panel_col_prt = panel_col_min - 13;
}

void panel_bounds_center(void)
{
	panel_row = panel_row_min / (SCREEN_HGT / 2);
	panel_row_max = panel_row_min + SCREEN_HGT - 1;
	panel_row_prt = panel_row_min - 1;
	panel_col = panel_col_min / (SCREEN_WID / 2);
	panel_col_max = panel_col_min + SCREEN_WID - 1;
	panel_col_prt = panel_col_min - 13;
}


/*
 * Given an row (y) and col (x), this routine detects when a move
 * off the screen has occurred and figures new borders. -RAK-
 *
 * "Update" forces a "full update" to take place.
 *
 * The map is reprinted if necessary, and "TRUE" is returned.
 */
void verify_panel(void)
{
	int y = py;
	int x = px;

	if ((centre_view) && !((no_centre_run && running)))
	{
		int prow_min;
		int pcol_min;

		int max_prow_min = max_panel_rows * (SCREEN_HGT / 2);
		int max_pcol_min = max_panel_cols * (SCREEN_WID / 2);

		/* Center vertically */
		prow_min = y - SCREEN_HGT / 2;
		if (prow_min > max_prow_min) prow_min = max_prow_min;
		else if (prow_min < 0) prow_min = 0;

		/* Center horizontally */
		pcol_min = x - SCREEN_WID / 2;
		if (pcol_min > max_pcol_min) pcol_min = max_pcol_min;
		else if (pcol_min < 0) pcol_min = 0;

		/* Check for "no change" */
		if ((prow_min == panel_row_min) && (pcol_min == panel_col_min)) return;

		/* Save the new panel info */
		panel_row_min = prow_min;
		panel_col_min = pcol_min;

		/* Recalculate the boundaries */
		panel_bounds_center();
	}

	else
  	{

		int prow = panel_row;
		int pcol = panel_col;

		/* Scroll screen when 2 grids from top/bottom edge */
		if ((y < panel_row_min + 2) || (y > panel_row_max - 2))
		{
			prow = ((y - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
			if (prow > max_panel_rows) prow = max_panel_rows;
			else if (prow < 0) prow = 0;
		}

		/* Scroll screen when 4 grids from left/right edge */
		if ((x < panel_col_min + 4) || (x > panel_col_max - 4))
		{
			pcol = ((x - SCREEN_WID / 4) / (SCREEN_WID / 2));
			if (pcol > max_panel_cols) pcol = max_panel_cols;
			else if (pcol < 0) pcol = 0;
		}

		/* Check for "no change" */
		if ((prow == panel_row) && (pcol == panel_col)) return;

		/* Hack -- optional disturb on "panel change" */
		if (disturb_panel) disturb(0, 0);

		/* Save the new panel info */
		panel_row = prow;
		panel_col = pcol;

		/* Recalculate the boundaries */
		panel_bounds();
	}

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * Monster health description
 */
cptr look_mon_desc(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool          living = TRUE;
	int           perc;


	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags3 & (RF3_UNDEAD)) living = FALSE;
	if (r_ptr->flags3 & (RF3_DEMON)) living = FALSE;
	if (r_ptr->flags3 & (RF3_CTHULOID)) living = FALSE;
    if (r_ptr->flags3 & (RF3_NONLIVING)) living = FALSE;
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
	monster_type *m_ptr = &m_list[m_idx];

	/* Monster must be alive */
	if (!m_ptr->r_idx) return (FALSE);

	/* Monster must be visible */
	if (!m_ptr->ml) return (FALSE);
	
#ifdef DRS_SMART_OPTIONS
	/* Monster must not be friendly */
	if (m_ptr->smart & SM_ALLY) return FALSE;
#endif

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

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
	if (target_who < 0) return (TRUE);

	/* Check moving targets */
	if (target_who > 0)
	{
		/* Accept reasonable targets */
		if (target_able(target_who))
		{
			monster_type *m_ptr = &m_list[target_who];

			/* Acquire monster location */
			target_row = m_ptr->fy;
			target_col = m_ptr->fx;

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
	cave_type *c_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Player grid is always interesting */
	if ((y == py) && (x == px)) return (TRUE);


	/* Handle hallucination */
	if (p_ptr->image) return (FALSE);


	/* Examine the grid */
	c_ptr = &cave[y][x];

	/* Visible monsters */
	if (c_ptr->m_idx)
	{
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		/* Visible monsters */
		if (m_ptr->ml) return (TRUE);
	}

	/* Scan all objects in the grid */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
	if (c_ptr->info & (CAVE_MARK))
	{
		/* Notice glyphs */
		if (c_ptr->feat == FEAT_GLYPH) return (TRUE);
        if (c_ptr->feat == FEAT_MINOR_GLYPH) return (TRUE);

        /* Notice the Pattern */
        if ((c_ptr->feat <= FEAT_PATTERN_XTRA2) &&
            (c_ptr->feat >= FEAT_PATTERN_START))
                return (TRUE);

		/* Notice doors */
		if (c_ptr->feat == FEAT_OPEN) return (TRUE);
		if (c_ptr->feat == FEAT_BROKEN) return (TRUE);

		/* Notice stairs */
		if (c_ptr->feat == FEAT_LESS) return (TRUE);
		if (c_ptr->feat == FEAT_MORE) return (TRUE);

		/* Notice shops */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
		    (c_ptr->feat <= FEAT_SHOP_TAIL)) return (TRUE);

		/* Notice traps */
		if ((c_ptr->feat >= FEAT_TRAP_HEAD) &&
		    (c_ptr->feat <= FEAT_TRAP_TAIL)) return (TRUE);

		/* Notice doors */
		if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
		    (c_ptr->feat <= FEAT_DOOR_TAIL)) return (TRUE);

		/* Notice rubble */
		if (c_ptr->feat == FEAT_RUBBLE) return (TRUE);

		/* Notice veins with treasure */
		if (c_ptr->feat == FEAT_MAGMA_K) return (TRUE);
		if (c_ptr->feat == FEAT_QUARTZ_K) return (TRUE);
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
	for (y = panel_row_min; y <= panel_row_max; y++)
	{
		for (x = panel_col_min; x <= panel_col_max; x++)
		{
			cave_type *c_ptr = &cave[y][x];

			/* Require line of sight, unless "look" is "expanded" */
			if (!expand_look && !player_has_los_bold(y, x)) continue;

			/* Require "interesting" contents */
			if (!target_set_accept(y, x)) continue;

			/* Require target_able monsters for "TARGET_KILL" */
			if ((mode & (TARGET_KILL)) && !target_able(c_ptr->m_idx)) continue;

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
	cave_type *c_ptr = &cave[y][x];

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

		/* Hack -- under the player */
		if ((y == py) && (x == px))
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
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
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
				health_track(c_ptr->m_idx);

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
						screen_roff(m_ptr->r_idx);

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
                       sprintf(out_val, "%s%s%s%s (%s)%s%s[r,%s]",
                               s1, s2, s3, m_name, look_mon_desc(c_ptr->m_idx),
                           (m_ptr->smart & SM_CLONED ? " (clone)": ""),
                           (m_ptr->smart & SM_ALLY ? " (allied) " : " "), info);

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
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
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
		feat = f_info[c_ptr->feat].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(c_ptr->info & (CAVE_MARK)) && !player_can_see_bold(y,x))
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
            if (*s2 && ((feat >= FEAT_MINOR_GLYPH) &&
                        (feat <= FEAT_PATTERN_XTRA2))) s2 = "on ";
            else if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special introduction for store doors */
			if ((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL))
			{
				s3 = (is_a_vowel(name[0])) ? "the entrance to an " : "the entrance to a ";
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
 * All locations must be on the current panel.  Consider the use of
 * "panel_bounds()" to allow "off-panel" targets, perhaps by using
 * some form of "scrolling" the map around the cursor.  XXX XXX XXX
 * That is, consider the possibility of "auto-scrolling" the screen
 * while the cursor moves around.  This may require changes in the
 * "update_mon()" code to allow "visibility" even if off panel, and
 * may require dynamic recalculation of the "temp" grid set.
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
	int		i, d, m;

	int		y = py;
	int		x = px;

	bool	done = FALSE;

	bool	flag = TRUE;

	char	query;

	char	info[80];
	
	cave_type		*c_ptr;


	/* Cancel target */
	target_who = 0;


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

			/* Access */
			c_ptr = &cave[y][x];

			/* Allow target */
			if (target_able(c_ptr->m_idx))
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
					if (target_able(c_ptr->m_idx))
					{
						health_track(c_ptr->m_idx);
						target_who = c_ptr->m_idx;
						target_row = y;
						target_col = x;
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
					d = get_keymap_dir(query);
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
			/* Access */
			c_ptr = &cave[y][x];

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
					target_who = -1;
					target_row = y;
					target_col = x;
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
					d = get_keymap_dir(query);
					if (!d) bell();
					break;
				}
			}

			/* Handle "direction" */
			if (d)
			{
				x += ddx[d];
				y += ddy[d];

				/* Hack -- Verify x */
				if ((x>=cur_wid-1) || (x>panel_col_max)) x--;
				else if ((x<=0) || (x<panel_col_min)) x++;

				/* Hack -- Verify y */
				if ((y>=cur_hgt-1) || (y>panel_row_max)) y--;
				else if ((y<=0) || (y<panel_row_min)) y++;
			}
		}
	}

	/* Forget */
	temp_n = 0;

	/* Clear the top line */
	prt("", 0, 0);

	/* Failure to set target */
	if (!target_who) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Get an "aiming direction" from the user.
 *
 * The "dir" is loaded with 1,2,3,4,6,7,8,9 for "actual direction", and
 * "0" for "current target", and "-1" for "entry aborted".
 *
 * Note that "Force Target", if set, will pre-empt user interaction,
 * if there is a usable target already set.
 *
 * Note that confusion over-rides any (explicit?) user choice.
 */
bool get_aim_dir(int *dp)
{
	int		dir;

	char	command;

	cptr	p;

 #ifdef ALLOW_REPEAT /* TNB */
 
 	if (repeat_pull(dp)) {
 	
 		/* Confusion? */
 
 		/* Verify */
 		if (!(*dp == 5 && !target_okay())) {
 			return (TRUE);
 		}
 	}
     
 #endif /* ALLOW_REPEAT -- TNB */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

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
				dir = get_keymap_dir(command);
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
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

 #ifdef ALLOW_REPEAT /* TNB */
 
     repeat_push(dir);
 
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* A "valid" direction was entered */
	return (TRUE);
}



/*
 * Request a "movement" direction (1,2,3,4,6,7,8,9) from the user,
 * and place it into "command_dir", unless we already have one.
 *
 * This function should be used for all "repeatable" commands, such as
 * run, walk, open, close, bash, disarm, spike, tunnel, etc, as well
 * as all commands which must reference a grid adjacent to the player,
 * and which may not reference the grid under the player.  Note that,
 * for example, it is no longer possible to "disarm" or "open" chests
 * in the same grid as the player.
 *
 * Direction "5" is illegal and will (cleanly) abort the command.
 *
 * This function tracks and uses the "global direction", and uses
 * that as the "desired direction", to which "confusion" is applied.
 */
bool get_rep_dir(int *dp)
{
	int dir;

 #ifdef ALLOW_REPEAT /* TNB */
 
     if (repeat_pull(dp)) {
         return (TRUE);
     }
     
 #endif /* ALLOW_REPEAT -- TNB */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

	/* Get a direction */
	while (!dir)
	{
		char ch;

		/* Get a command (or Cancel) */
		if (!get_com("Direction (Escape to cancel)? ", &ch)) break;

		/* Look up the direction */
		dir = get_keymap_dir(ch);

		/* Oops */
		if (!dir) bell();
	}

	/* Prevent weirdness */
	if (dir == 5) dir = 0;

	/* Aborted */
	if (!dir) return (FALSE);

	/* Save desired direction */
	command_dir = dir;

	/* Apply "confusion" */
	if (p_ptr->confused)
	{
		/* Standard confusion */
		if (rand_int(100) < 75)
		{
			/* Random direction */
			dir = ddd[rand_int(8)];
		}
	}
	
	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;

 
 #ifdef ALLOW_REPEAT /* TNB */
 
     repeat_push(dir);
 
 #endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


int get_chaos_patron()
{
    return (((p_ptr->age)+(p_ptr->sc)+(p_ptr->birthday))%MAX_PATRON);
}

void gain_level_reward(int chosen_reward)
{
    int type, effect;
    char wrath_reason[32] = "";
    int dummy = 0, dummy2 = 0;
    object_type *q_ptr;
    object_type forge;
    int nasty_chance = 6;
	int skill_used;


	/* Broo might have got here through their racial ability */
	if (p_ptr->prace == RACE_BROO)
	{
		skill_used = (MAX(skill_set[SKILL_RACIAL].value,skill_set[SKILL_THAUMATURGY].value))/2;
	}
	else
	{
		skill_used = skill_set[SKILL_THAUMATURGY].value/2;
	}

    if (!chosen_reward)
    {
        if (multi_rew) return;
        else multi_rew = TRUE;
    }


    if (skill_used == 13) nasty_chance = 2;
    else if (!(skill_used % 13)) nasty_chance = 3;
    else if (!(skill_used % 14)) nasty_chance = 12;

    if (randint(nasty_chance)==1)
        type = randint(20); /* Allow the 'nasty' effects */
    else
        type = randint(15) + 5; /* Or disallow them */

    if (type < 1) type = 1;
    if (type > 20) type = 20;
    type--;


    sprintf(wrath_reason, "the Wrath of %s",
        chaos_patron_shorts[p_ptr->chaos_patron]);

    effect = chaos_rewards[p_ptr->chaos_patron][type];

    if ((randint(6)==1) && !(chosen_reward))
    {
        msg_format("%^s rewards you with a chaos feature!",
            chaos_patron_shorts[p_ptr->chaos_patron]);
        (void)gain_chaos_feature(0);
        return;
    }

    switch (chosen_reward?chosen_reward:effect)
    {
        case REW_POLY_SLF:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou needst a new form, mortal!'");
            do_poly_self();
            break;
        case REW_GAIN_EXP:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Well done, mortal! Lead on!'");
            msg_print("You feel more experienced.");
            gain_skills(200);
            break;
        case REW_LOSE_EXP:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou didst not deserve that, slave.'");
            lose_skills(5);
            break;
        case REW_GOOD_OBJ:
            msg_format("The voice of %s whispers:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Use my gift wisely.'");
            acquirement(py, px, 1, FALSE);
            break;
        case REW_GREA_OBJ:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Use my gift wisely.'");
            acquirement(py, px, 1, TRUE);
            break;
        case REW_CHAOS_WP:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thy deed hath earned thee a worthy blade.'");
                /* Get local object */
                q_ptr = &forge;
                        dummy = TV_SWORD;
                        switch(randint(skill_used))
                        {
                            case 1: case 2: case 0:
                            dummy2 = SV_DAGGER;
                            break;
                            case 3: case 4:
                            dummy2 = SV_MAIN_GAUCHE;
                            break;
                            case 5: case 6:
                            dummy2 = SV_RAPIER;
                            break;
                            case 7: case 8:
                            dummy2 = SV_SMALL_SWORD;
                            break;
                            case 9: case 10:
                            dummy2 = SV_SHORT_SWORD;
                            break;
                            case 11: case 12: case 13:
                            dummy2 = SV_SABRE;
                            break;
                            case 14: case 15: case 16:
                            dummy2 = SV_CUTLASS;
                            break;
                            case 17:
                            dummy2 = SV_TULWAR;
                            break;
                            case 18: case 19: case 20:
                            dummy2 = SV_BROAD_SWORD;
                            break;
                            case 21: case 22: case 23:
                            dummy2 = SV_LONG_SWORD;
                            break;
                            case 24: case 25: case 26:
                            dummy2 = dummy2 = SV_SCIMITAR;
                            break;
                            case 27:
                            dummy2 = SV_KATANA;
                            break;
                            case 28: case 29:
                            dummy2 = SV_BASTARD_SWORD;
                            break;
                            case 30: case 31:
                            dummy2 = SV_TWO_HANDED_SWORD;
                            break;
                            case 32:
                            dummy2 = SV_EXECUTIONERS_SWORD;
                            break;
                            default:
                            dummy2 = SV_BLADE_OF_CHAOS;
                        }

                object_prep(q_ptr, lookup_kind(dummy, dummy2));
                q_ptr->to_h = 3 + (randint((dun_level+dun_offset)))%10;
                q_ptr->to_d = 3 + (randint((dun_level+dun_offset)))%10;
                random_resistance(q_ptr, FALSE, ((randint(34))+4));
                q_ptr->name2 = EGO_CHAOTIC;
                /* Drop it in the dungeon */
                drop_near(q_ptr, -1, py, px);
            break;
        case REW_GOOD_OBS:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thy deed hath earned thee a worthy reward.'");
            acquirement(py, px, randint(2) + 1, FALSE);
            break;
        case REW_GREA_OBS:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Behold, mortal, how generously I reward thy loyalty.'");
            acquirement(py, px, randint(2) + 1, TRUE);
            break;
        case REW_TY_CURSE:
            msg_format("The voice of %s thunders:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou art growing arrogant, mortal.'");
            activate_ty_curse();
            break;
        case REW_SUMMON_M:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'My pets, destroy the arrogant mortal!'");
            for (dummy = 0; dummy < randint(5) + 1; dummy++)
            {
                (void) summon_specific(py, px, (dun_level+dun_offset), 0);
            }
            break;
        case REW_H_SUMMON:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou needst worthier opponents!'");
            activate_hi_summon();
            break;
        case REW_DO_HAVOC:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Death and destruction! This pleaseth me!'");
            call_chaos(skill_used);
            break;
        case REW_GAIN_ABL:                
            msg_format("The voice of %s rings out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Stay, mortal, and let me mold thee.'");
            if ((randint(3)==1) && !(chaos_stats[p_ptr->chaos_patron] < 0))
                do_inc_stat(chaos_stats[p_ptr->chaos_patron]);
            else
                do_inc_stat((randint(6))-1);
            break;
        case REW_LOSE_ABL:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'I grow tired of thee, mortal.'");
            if ((randint(3)==1) && !(chaos_stats[p_ptr->chaos_patron] < 0))
                do_dec_stat(chaos_stats[p_ptr->chaos_patron]);
            else
                (void) do_dec_stat(randint(6)-1);
            break;
        case REW_RUIN_ABL:
            msg_format("The voice of %s thunders:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou needst a lesson in humility, mortal!'");
            msg_print("You feel less powerful!");
            for (dummy = 0; dummy < 6; dummy++)
            {
                (void) dec_stat(dummy, 10 + randint(15), TRUE);
            }
            break;
        case REW_POLY_WND:
            msg_format("You feel the power of %s touch you.",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            do_poly_wounds();
            break;
        case REW_AUGM_ABL:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Receive this modest gift from me!'");
            for (dummy = 0; dummy < 6; dummy++)
            {
                (void) do_inc_stat(dummy);
            }
            break;
        case REW_HURT_LOT:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Suffer, pathetic fool!'");
            fire_ball(GF_DISINTEGRATE, 0, (skill_used * 4), 4);
            take_hit(skill_used * 4, wrath_reason);
            break;
       case REW_HEAL_FUL:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Rise, my servant!'");
            restore_level();
            (void)set_poisoned(0);
            (void)set_blind(0);
            (void)set_confused(0);
            (void)set_image(0);
            (void)set_stun(0);
            (void)set_cut(0);
            hp_player(5000);
            for (dummy = 0; dummy < 6; dummy++)
            {
                (void) do_res_stat(dummy);
            }
            break;
        case REW_CURSE_WP:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou reliest too much on thy weapon.'");
            (void)curse_weapon();
            break;
        case REW_CURSE_AR:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Thou reliest too much on thine equipment.'");
            (void)curse_armor();
            break;
        case REW_PISS_OFF:
            msg_format("The voice of %s whispers:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Now thou shalt pay for annoying me.'");
            switch(randint(4))
            {
                    case 1:
                        activate_ty_curse();
                        break;
                    case 2:
                        activate_hi_summon();
                        break;
                    case 3:
                        if (randint(2)==1) (void)curse_weapon();
                        else (void)curse_armor();
                        break;
                    default:
                    for (dummy = 0; dummy < 6; dummy++)
                    {
                        (void) dec_stat(dummy, 10 + randint(15), TRUE);
                    }
            }
            break;
        case REW_WRATH:
            msg_format("The voice of %s thunders:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Die, mortal!'");
            take_hit(skill_used * 4, wrath_reason);
            for (dummy = 0; dummy < 6; dummy++)
            {
                (void) dec_stat(dummy, 10 + randint(15), FALSE);
            }
            activate_hi_summon();
            activate_ty_curse();
            if (randint(2)==1) (void)curse_weapon();
            if (randint(2)==1) (void)curse_armor();
            break;
        case REW_DESTRUCT:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Death and destruction! This pleaseth me!'");
            destroy_area(py, px, 25, TRUE);
            break;
        case REW_GENOCIDE:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Let me relieve thee of thine oppressors!'");
            (void) genocide(FALSE);
            break;
        case REW_MASS_GEN:
            msg_format("The voice of %s booms out:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_print("'Let me relieve thee of thine oppressors!'");
            (void) mass_genocide(FALSE);
            break;
        case REW_DISPEL_C:
            msg_format("You can feel the power of %s assault your enemies!",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            (void) dispel_monsters(skill_used * 4);
            break;
        case REW_IGNORE:
            msg_format("%s ignores you.",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            break;
        case REW_SER_DEMO:
            msg_format("%s rewards you with a demonic servant!",chaos_patron_shorts[p_ptr->chaos_patron]);
            if (!(summon_specific_friendly(py, px, (dun_level+dun_offset), SUMMON_DEMON, FALSE)))
            msg_print("Nobody ever turns up...");
            break;
        case REW_SER_MONS:
            msg_format("%s rewards you with a servant!",chaos_patron_shorts[p_ptr->chaos_patron]);
            if (!(summon_specific_friendly(py, px, (dun_level+dun_offset), SUMMON_NO_UNIQUES, FALSE)))
            msg_print("Nobody ever turns up...");
            break;
        case REW_SER_UNDE:
            msg_format("%s rewards you with an undead servant!",chaos_patron_shorts[p_ptr->chaos_patron]);
            if (!(summon_specific_friendly(py, px, (dun_level+dun_offset), SUMMON_UNDEAD, FALSE)))
            msg_print("Nobody ever turns up...");
            break;
        default:
            msg_format("The voice of %s stammers:",
                chaos_patron_shorts[p_ptr->chaos_patron]);
            msg_format("'Uh... uh... the answer's %d/%d, what's the question?'", type,
                effect );
        }
             

}


 /*
  * old -- from PsiAngband.
  */
 bool tgt_pt(int *x,int *y)
 {
    char ch = 0;
    int d,cu,cv;
    bool success = FALSE;

    *x = px;
    *y = py;

    cu = Term->scr->cu;
    cv = Term->scr->cv;
    Term->scr->cu = 0;
    Term->scr->cv = 1;
    msg_print("Select a point and press space.");

    while ((ch != 27) && (ch != ' '))
    {
       move_cursor_relative(*y,*x);
       ch = inkey();
       switch (ch)
       {
    case 27: break;
    case ' ': success = TRUE; break;
   default:
   {
      d = get_keymap_dir(ch);
      if (!d) break;
               *x += ddx[d];
               *y += ddy[d];

               /* Hack -- Verify x */
               if ((*x>=cur_wid-1) || (*x>=panel_col_min + SCREEN_WID)) (*x)--;
                 else if ((*x<=0) || (*x<=panel_col_min)) (*x)++;

               /* Hack -- Verify y */
               if ((*y>=cur_hgt-1) || (*y>=panel_row_min + SCREEN_HGT)) (*y)--;
                 else if ((*y<=0) || (*y<=panel_row_min)) (*y)++;

    break;
   }
       }
    }

    Term->scr->cu = cu;
    Term->scr->cv = cv;
    Term_fresh();
    return success;

 }


bool gain_chaos_feature(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;
	
	if (choose_mut) attempts_left = 1;
	
	while (attempts_left--)
	{
		switch(choose_mut?choose_mut:randint(194))
		{
		case 1: case 2: case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You gain the ability to spit acid.";
			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "You gain the ability to breathe fire.";
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look mesmerizing...";
			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_TELEKINES;
			muta_desc = "You gain the ability to move objects telekinetically.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You gain the power of teleportation at will.";
			break;
		case 15: case 16:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You gain the power of Mind Blast.";
			break;
		case 17: case 18:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RADIATION;
			muta_desc = "You start emitting hard radiation.";
			break;
		case 19: case 20:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VAMPIRISM;
			muta_desc = "You become vampiric.";
			break;
		case 21: case 22: case 23:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MET;
			muta_desc = "You smell a metallic odor.";
			break;
		case 24: case 25: case 26: case 27:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MON;
			muta_desc = "You smell filthy monsters.";
			break;
		case 28: case 29: case 30:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BLINK;
			muta_desc = "You gain the power of minor teleportation.";
			break;
		case 31: case 32:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look delicious.";
			break;
		case 33: case 34:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SWAP_POS;
			muta_desc = "You feel like walking a mile in someone else's shoes.";
			break;
		case 35: case 36: case 37:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much tougher.";
			break;
		case 38: case 39: case 40:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can light up rooms with your presence.";
			break;
		case 41: case 42:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can feel evil magics.";
			break;
		case 43: case 44: case 45:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BERSERK;
			muta_desc = "You feel a controlled rage.";
			break;
		case 46:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems mutable.";
			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You gain the Midas touch.";
			break;
		case 49:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You feel a sudden affinity for mold.";
			break;
		case 50: case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you can protect yourself.";
			break;
		case 53: case 54: case 55:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You gain the ability to wreck the dungeon.";
			break;
		case 56:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_MAGIC;
			muta_desc = "Your magic items look delicious.";
			break;
		case 57: case 58:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_WEIGH_MAG;
			muta_desc = "You feel you can better understand the magic around you.";
			break;
		case 59:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_STERILITY;
			muta_desc = "You can give everything around you a headache.";
			break;
		case 60: case 61:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_PANIC_HIT;
			muta_desc = "You suddenly understand how thieves feel.";
			break;
		case 62: case 63: case 64:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You gain the ability to emit dazzling lights.";
			break;
		case 65: case 66: case 67:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EYE_BEAM;
			muta_desc = "Your eyes burn for a moment.";
			break;
		case 68: case 69:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RECALL;
			muta_desc = "You feel briefly homesick, but it passes.";
			break;
		case 70:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BANISH;
			muta_desc = "You feel a holy wrath fill you.";
			break;
		case 71: case 72:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands get very cold.";
			break;
		case 73: case 74:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LAUNCHER;
			muta_desc = "Your throwing arm feels much stronger.";
			break;
		case 75:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You become subject to fits of berserk rage!";
			break;
		case 76:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You become an incredible coward!";
			break;
		case 77:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position seems very uncertain...";
			break;
		case 78:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body starts producing alcohol!";
			break;
		case 79:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HALLU;
			muta_desc = "You are afflicted by a hallucinatory insanity!";
			break;
		case 80:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_FLATULENT;
			muta_desc = "You become subject to uncontrollable flatulence.";
			break;
		case 81: case 82:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SCOR_TAIL;
			muta_desc = "You grow a scorpion tail!";
			break;
		case 83: case 84:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HORNS;
			muta_desc = "Horns pop forth into your forehead!";
			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BEAK;
			muta_desc = "Your mouth turns into a sharp, powerful beak!";
			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You start attracting demons.";
			break;
		case 89:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You start producing magical energy uncontrollably.";
			break;
		case 90: case 91:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SPEED_FLUX;
			muta_desc = "You become manic-depressive.";
			break;
		case 92: case 93:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BANISH_ALL;
			muta_desc = "You feel a terrifying power lurking behind you.";
			break;
		case 94:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel a strange kinship with Nyogtha.";
			break;
		case 95: case 96:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TRUNK;
			muta_desc = "Your nose grows into an elephant-like trunk.";
			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You start attracting animals.";
			break;
		case 98:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TENTACLES;
			muta_desc = "Evil-looking tentacles sprout from your sides.";
			break;
		case 99:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is less stable around you.";
			break;
		case 100: case 101: case 102:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NORMALITY;
			muta_desc = "You feel strangely normal.";
			break;
		case 103:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WRAITH;
			muta_desc = "You start to fade in and out of the physical world.";
			break;
		case 104:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of chaos entering your old scars.";
			break;
		case 105:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WASTING;
			muta_desc = "You suddenly contract a horrible wasting disease.";
			break;
		case 106:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You start attracting dragons.";
			break;
		case 107: case 108:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts suddenly take off in strange directions.";
			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach starts to roil nauseously.";
			break;
		case 110: case 111:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CHAOS_GIFT;
			muta_desc = "You attract the notice of a chaos deity!";
			break;
		case 112:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "You feel like reality is as thin as paper.";
			break;
		case 113: case 114:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "You suddenly feel paranoid.";
			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are blessed with fits of invulnerability.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are subject to fits of magical healing.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are subject to fits of painful clarity.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet grow to four times their former size.";
			break;
		case 120: case 121: case 122:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "You turn into a superhuman he-man!";
			break;
		case 123: case 124: case 125:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PUNY;
			muta_desc = "Your muscles wither away...";
			break;
		case 126: case 127: case 128:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "Your brain evolves into a living computer!";
			break;
		case 129: case 130: case 131:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MORONIC;
			muta_desc = "Your brain withers away...";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RESILIENT;
			muta_desc = "You become extraordinarily resilient.";
			break;
		case 134: case 135:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You become sickeningly fat!";
			break;
		case 136: case 137:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ALBINO;
			muta_desc = "You turn into an albino! You feel frail...";
			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "Your flesh is afflicted by a rotting disease!";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SILLY_VOI;
			muta_desc = "Your voice turns into a ridiculous squeak!";
			break;
		case 143: case 144:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your face becomes completely featureless!";
			break;
		case 145:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You start projecting a reassuring image.";
			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "You grow an extra pair of eyes!";
			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MAGIC_RES;
			muta_desc = "You become resistant to magic.";
			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You start making strange noise!";
			break;
		case 154: case 155: case 156:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is improved.";
			break;
		case 157: case 158:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_LEGS;
			muta_desc = "You grow an extra pair of legs!";
			break;
		case 159: case 160:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SHORT_LEG;
			muta_desc = "Your legs turn into short stubs!";
			break;
		case 161: case 162:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "Electricity starts running through you!";
			break;
		case 163: case 164:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your body is enveloped in flames!";
			break;
		case 165: case 166: case 167:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WART_SKIN;
			muta_desc = "Disgusting warts appear everywhere on you!";
			break;
		case 168: case 169: case 170:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SCALES;
			muta_desc = "Your skin turns into black scales!";
			break;
		case 171: case 172:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_IRON_SKIN;
			muta_desc = "Your skin turns to steel!";
			break;
		case 173: case 174:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WINGS;
			muta_desc = "You grow a pair of wings.";
			break;
		case 175: case 176: case 177:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FEARLESS;
			muta_desc = "You become completely fearless.";
			break;
		case 178: case 179:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_REGEN;
			muta_desc = "You start regenerating.";
			break;
		case 180: case 181:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ESP;
			muta_desc = "You develop a telepathic ability!";
			break;
		case 182: case 183: case 184:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles become limber.";
			break;
		case 185: case 186: case 187:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints suddenly hurt.";
			break;
		case 188:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RES_TIME;
			muta_desc = "You feel immortal.";
			break;
		case 189:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You feel strangely exposed.";
			break;
		case 190: case 191: case 192:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MOTION;
			muta_desc = "You move with new assurance.";
			break;
		case 193: case 194:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SUS_STATS;
			muta_desc = "You feel like you can recover from anything.";
			break;
		default:
			muta_class = 0;
			muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (!(*(muta_class) & muta_which))
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen == TRUE) break;
	}

	if (!muta_chosen)
	{
		msg_print("You feel normal.");
		return FALSE;
	}
	else
	{
		if (p_ptr->prace == RACE_VAMPIRE &&
		  !(p_ptr->muta1 & MUT1_HYPN_GAZE) &&
		   (randint(10)<7))
		{
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look mesmerizing...";
		}
	
		else if (p_ptr->prace == RACE_IMP &&
			!(p_ptr->muta2 & MUT2_HORNS) &&
			(randint(10)<7))
		{
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HORNS;
			muta_desc = "Horns pop forth into your forehead!";
		}
	
		else if (p_ptr->prace == RACE_YEEK &&
			!(p_ptr->muta1 & MUT1_SHRIEK) &&
			(randint(10)<7))
		{
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much tougher.";
		}
	
		else if (p_ptr->prace == RACE_BROO &&
			!(p_ptr->muta1 & MUT1_POLYMORPH) &&
			(randint(10)<2))
		{
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems mutable.";
		}
	
		else if (p_ptr->prace == RACE_MIND_FLAYER &&
			!(p_ptr->muta2 & MUT2_TENTACLES) &&
			(randint(10)<7))
		{
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TENTACLES;
			muta_desc = "Evil-looking tentacles sprout from your mouth.";
		}

		msg_print("You change!");
		msg_print(muta_desc);
		*(muta_class) |= muta_which;

		if (muta_class == &(p_ptr->muta3))
		{
			if (muta_which == MUT3_PUNY)
			{
				if (p_ptr->muta3 & MUT3_HYPER_STR)
				{
					msg_print("You no longer feel super-strong!");
					p_ptr->muta3 &= ~(MUT3_HYPER_STR);
				}
			}
			else if (muta_which == MUT3_HYPER_STR)
			{
				if (p_ptr->muta3 & MUT3_PUNY)
				{
					msg_print("You no longer feel puny!");
					p_ptr->muta3 &= ~(MUT3_PUNY);
				}
			}
			else if (muta_which == MUT3_MORONIC)
			{
				if (p_ptr->muta3 & MUT3_HYPER_INT)
				{
					msg_print("Your brain is no longer a living computer.");
					p_ptr->muta3 &= ~(MUT3_HYPER_INT);
				}
			}
			else if (muta_which == MUT3_HYPER_INT)
			{
				if (p_ptr->muta3 & MUT3_MORONIC)
				{
					msg_print("You are no longer moronic.");
					p_ptr->muta3 &= ~(MUT3_MORONIC);
				}
			}
			else if (muta_which == MUT3_IRON_SKIN)
			{
				if (p_ptr->muta3 & MUT3_SCALES)
				{
					msg_print("You lose your scales.");
					p_ptr->muta3 &= ~(MUT3_SCALES);
				}
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msg_print("Your flesh rots no longer.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
				if (p_ptr->muta3 & MUT3_WART_SKIN)
				{
					msg_print("You lose your warts.");
					p_ptr->muta3 &= ~(MUT3_WART_SKIN);
				}
			}
			else if (muta_which == MUT3_WART_SKIN || muta_which == MUT3_SCALES
				|| muta_which == MUT3_FLESH_ROT)
			{
				if (p_ptr->muta3 & MUT3_IRON_SKIN)
				{
					msg_print("Your skin is no longer made of steel.");
					p_ptr->muta3 &= ~(MUT3_IRON_SKIN);
				}
			}
			else if (muta_which == MUT3_FEARLESS)
			{
				if (p_ptr->muta2 & MUT2_COWARDICE)
				{
					msg_print("You are no longer cowardly.");
					p_ptr->muta2 &= ~(MUT2_COWARDICE);
				}
			}
			else if (muta_which == MUT3_FLESH_ROT)
			{
				if (p_ptr->muta3 & MUT3_REGEN)
				{
					msg_print("You stop regenerating.");
					p_ptr->muta3 &= ~(MUT3_REGEN);
				}
			}
			else if (muta_which == MUT3_REGEN)
			{
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msg_print("Your flesh stops rotting.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
			}
			else if (muta_which == MUT3_LIMBER)
			{
				if (p_ptr->muta3 & MUT3_ARTHRITIS)
				{
					msg_print("Your joints stop hurting.");
					p_ptr->muta3 &= ~(MUT3_ARTHRITIS);
				}
			}
			else if (muta_which == MUT3_ARTHRITIS)
			{
				if (p_ptr->muta3 & MUT3_LIMBER)
				{
					msg_print("You no longer feel limber.");
					p_ptr->muta3 &= ~(MUT3_LIMBER);
				}
			}
		}
		else if (muta_class == &(p_ptr->muta2))
		{
			if (muta_which == MUT2_COWARDICE)
			{
				if (p_ptr->muta3 & MUT3_FEARLESS)
				{
					msg_print("You no longer feel fearless.");
					p_ptr->muta3 &= ~(MUT3_FEARLESS);
				}
			}
			if (muta_which == MUT2_BEAK)
			{
				if (p_ptr->muta2 & MUT2_TRUNK)
				{
					msg_print("Your nose is no longer elephantine.");
					p_ptr->muta2 &= ~(MUT2_TRUNK);
				}
			}
			if (muta_which == MUT2_TRUNK)
			{
				if (p_ptr->muta2 & MUT2_BEAK)
				{
					msg_print("You no longer have a hard beak.");
					p_ptr->muta2 &= ~(MUT2_BEAK);
				}
			}
		}
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}

bool lose_chaos_feature(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;
	
	if (choose_mut) attempts_left = 1;
	
	while (attempts_left--)
	{
		switch(choose_mut?choose_mut:randint(194))
		{
		case 1: case 2: case 3: case 4:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SPIT_ACID;
			muta_desc = "You lose the ability to spit acid.";
			break;
		case 5: case 6: case 7:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BR_FIRE;
			muta_desc = "You lose the ability to breathe fire.";
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_HYPN_GAZE;
			muta_desc = "Your eyes look uninteresting.";
			break;
		case 10: case 11:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_TELEKINES;
			muta_desc = "You lose the ability to move objects telekinetically.";
			break;
		case 12: case 13: case 14:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VTELEPORT;
			muta_desc = "You lose the power of teleportation at will.";
			break;
		case 15: case 16:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIND_BLST;
			muta_desc = "You lose the power of Mind Blast.";
			break;
		case 17: case 18:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RADIATION;
			muta_desc = "You stop emitting hard radiation.";
			break;
		case 19: case 20:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_VAMPIRISM;
			muta_desc = "You are no longer vampiric.";
			break;
		case 21: case 22: case 23:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MET;
			muta_desc = "You no longer smell a metallic odor.";
			break;
		case 24: case 25: case 26: case 27:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SMELL_MON;
			muta_desc = "You no longer smell filthy monsters.";
			break;
		case 28: case 29: case 30:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BLINK;
			muta_desc = "You lose the power of minor teleportation.";
			break;
		case 31: case 32:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_ROCK;
			muta_desc = "The walls look unappetizing.";
			break;
		case 33: case 34:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SWAP_POS;
			muta_desc = "You feel like staying in your own shoes.";
			break;
		case 35: case 36: case 37:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_SHRIEK;
			muta_desc = "Your vocal cords get much weaker.";
			break;
		case 38: case 39: case 40:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_ILLUMINE;
			muta_desc = "You can no longer light up rooms with your presence.";
			break;
		case 41: case 42:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DET_CURSE;
			muta_desc = "You can no longer feel evil magics.";
			break;
		case 43: case 44: case 45:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BERSERK;
			muta_desc = "You no longer feel a controlled rage.";
			break;
		case 46:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_POLYMORPH;
			muta_desc = "Your body seems stable.";
			break;
		case 47: case 48:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_MIDAS_TCH;
			muta_desc = "You lose the Midas touch.";
			break;
		case 49:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_GROW_MOLD;
			muta_desc = "You feel a sudden dislike for mold.";
			break;
		case 50: case 51: case 52:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RESIST;
			muta_desc = "You feel like you might be vulnerable.";
			break;
		case 53: case 54: case 55:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EARTHQUAKE;
			muta_desc = "You lose the ability to wreck the dungeon.";
			break;
		case 56:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EAT_MAGIC;
			muta_desc = "Your magic items no longer look delicious.";
			break;
		case 57: case 58:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_WEIGH_MAG;
			muta_desc = "You no longer sense magic.";
			break;
		case 59:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_STERILITY;
			muta_desc = "You hear a massed sigh of relief.";
			break;
		case 60: case 61:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_PANIC_HIT;
			muta_desc = "You no longer feel jumpy.";
			break;
		case 62: case 63: case 64:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_DAZZLE;
			muta_desc = "You lose the ability to emit dazzling lights.";
			break;
		case 65: case 66: case 67:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_EYE_BEAM;
			muta_desc = "Your eyes burn for a moment, then feel soothed.";
			break;
		case 68: case 69:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_RECALL;
			muta_desc = "You feel briefly homesick.";
			break;
		case 70:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_BANISH;
			muta_desc = "You no longer feel a holy wrath.";
			break;
		case 71: case 72:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_COLD_TOUCH;
			muta_desc = "Your hands warm up.";
			break;
		case 73: case 74:
			muta_class = &(p_ptr->muta1);
			muta_which = MUT1_LAUNCHER;
			muta_desc = "Your throwing arm feels much weaker.";
			break;
		case 75:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BERS_RAGE;
			muta_desc = "You are no longer subject to fits of berserk rage!";
			break;
		case 76:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_COWARDICE;
			muta_desc = "You are no longer an incredible coward!";
			break;
		case 77:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RTELEPORT;
			muta_desc = "Your position seems more certain.";
			break;
		case 78:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ALCOHOL;
			muta_desc = "Your body stops producing alcohol!";
			break;
		case 79:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HALLU;
			muta_desc = "You are no longer afflicted by a hallucinatory insanity!";
			break;
		case 80:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_FLATULENT;
			muta_desc = "You are no longer subject to uncontrollable flatulence.";
			break;
		case 81: case 82:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SCOR_TAIL;
			muta_desc = "You lose your scorpion tail!";
			break;
		case 83: case 84:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HORNS;
			muta_desc = "Your horns vanish from your forehead!";
			break;
		case 85: case 86:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BEAK;
			muta_desc = "Your mouth reverts to normal!";
			break;
		case 87: case 88:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DEMON;
			muta_desc = "You stop attracting demons.";
			break;
		case 89:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_PROD_MANA;
			muta_desc = "You stop producing magical energy uncontrollably.";
			break;
		case 90: case 91:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SPEED_FLUX;
			muta_desc = "You are no longer manic-depressive.";
			break;
		case 92: case 93:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_BANISH_ALL;
			muta_desc = "You no longer feel a terrifying power lurking behind you.";
			break;
		case 94:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_EAT_LIGHT;
			muta_desc = "You feel the world's a brighter place.";
			break;
		case 95: case 96:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TRUNK;
			muta_desc = "Your nose returns to a normal length.";
			break;
		case 97:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_ANIMAL;
			muta_desc = "You stop attracting animals.";
			break;
		case 98:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_TENTACLES;
			muta_desc = "Your tentacles vanish from your sides.";
			break;
		case 99:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_RAW_CHAOS;
			muta_desc = "You feel the universe is more stable around you.";
			break;
		case 100: case 101: case 102:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NORMALITY;
			muta_desc = "You feel normally strange.";
			break;
		case 103:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WRAITH;
			muta_desc = "You are firmly in the physical world.";
			break;
		case 104:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_POLY_WOUND;
			muta_desc = "You feel forces of chaos departing your old scars.";
			break;
		case 105:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WASTING;
			muta_desc = "You are cured of the horrible wasting disease!";
			break;
		case 106:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_ATT_DRAGON;
			muta_desc = "You stop attracting dragons.";
			break;
		case 107: case 108:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WEIRD_MIND;
			muta_desc = "Your thoughts return to boring paths.";
			break;
		case 109:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_NAUSEA;
			muta_desc = "Your stomach stops roiling.";
			break;
		case 110: case 111:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_CHAOS_GIFT;
			muta_desc = "You lose the attention of the chaos deities.";
			break;
		case 112:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WALK_SHAD;
			muta_desc = "You feel like you're trapped in reality.";
			break;
		case 113: case 114:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_WARNING;
			muta_desc = "You no longer feel paranoid.";
			break;
		case 115:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_INVULN;
			muta_desc = "You are no longer blessed with fits of invulnerability.";
			break;
		case 116: case 117:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_SP_TO_HP;
			muta_desc = "You are no longer subject to fits of magical healing.";
			break;
		case 118:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_HP_TO_SP;
			muta_desc = "You are no longer subject to fits of painful clarity.";
			break;
		case 119:
			muta_class = &(p_ptr->muta2);
			muta_which = MUT2_DISARM;
			muta_desc = "Your feet shrink to their former size.";
			break;
		case 120: case 121: case 122:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_STR;
			muta_desc = "Your muscles revert to normal.";
			break;
		case 123: case 124: case 125:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_PUNY;
			muta_desc = "Your muscles revert to normal.";
			break;
		case 126: case 127: case 128:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_HYPER_INT;
			muta_desc = "Your brain reverts to normal.";
			break;
		case 129: case 130: case 131:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MORONIC;
			muta_desc = "Your brain reverts to normal";
			break;
		case 132: case 133:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RESILIENT;
			muta_desc = "You become ordinarily resilient again.";
			break;
		case 134: case 135:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_FAT;
			muta_desc = "You benefit from a miracle diet!";
			break;
		case 136: case 137:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ALBINO;
			muta_desc = "You are no longer an albino!";
			break;
		case 138: case 139: case 140:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FLESH_ROT;
			muta_desc = "Your flesh is no longer afflicted by a rotting disease!";
			break;
		case 141: case 142:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SILLY_VOI;
			muta_desc = "Your voice returns to normal.";
			break;
		case 143: case 144:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_BLANK_FAC;
			muta_desc = "Your facial features return.";
			break;
		case 145:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ILL_NORM;
			muta_desc = "You stop projecting a reassuring image.";
			break;
		case 146: case 147: case 148:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_EYES;
			muta_desc = "Your extra eyes vanish!";
			break;
		case 149: case 150:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MAGIC_RES;
			muta_desc = "You become susceptible to magic again.";
			break;
		case 151: case 152: case 153:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_NOIS;
			muta_desc = "You stop making strange noise!";
			break;
		case 154: case 155: case 156:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_INFRAVIS;
			muta_desc = "Your infravision is degraded.";
			break;
		case 157: case 158:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_XTRA_LEGS;
			muta_desc = "Your extra legs disappear!";
			break;
		case 159: case 160:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SHORT_LEG;
			muta_desc = "Your legs lengthen to normal.";
			break;
		case 161: case 162:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ELEC_TOUC;
			muta_desc = "Electricity stops running through you.";
			break;
		case 163: case 164:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FIRE_BODY;
			muta_desc = "Your body is no longer enveloped in flames.";
			break;
		case 165: case 166: case 167:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WART_SKIN;
			muta_desc = "Your warts disappear!";
			break;
		case 168: case 169: case 170:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SCALES;
			muta_desc = "Your scales vanish!";
			break;
		case 171: case 172:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_IRON_SKIN;
			muta_desc = "Your skin reverts to flesh!";
			break;
		case 173: case 174:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_WINGS;
			muta_desc = "Your wings fall off.";
			break;
		case 175: case 176: case 177:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_FEARLESS;
			muta_desc = "You begin to feel fear again.";
			break;
		case 178: case 179:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_REGEN;
			muta_desc = "You stop regenerating.";
			break;
		case 180: case 181:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ESP;
			muta_desc = "You lose your telepathic ability!";
			break;
		case 182: case 183: case 184:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_LIMBER;
			muta_desc = "Your muscles stiffen.";
			break;
		case 185: case 186: case 187:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_ARTHRITIS;
			muta_desc = "Your joints stop hurting.";
			break;
		case 188:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_RES_TIME;
			muta_desc = "You feel all too mortal.";
			break;
		case 189:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_VULN_ELEM;
			muta_desc = "You feel less exposed.";
			break;
		case 190: case 191: case 192:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_MOTION;
			muta_desc = "You move with less assurance.";
			break;
		case 193: case 194:
			muta_class = &(p_ptr->muta3);
			muta_which = MUT3_SUS_STATS;
			muta_desc = "You no longer feel like you can recover from anything.";
			break;
		default:
			muta_class = 0;
			muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (*(muta_class) & muta_which)
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen == TRUE) break;
	}

	if (!muta_chosen)
	{
		msg_print("You feel oddly normal.");
		return FALSE;
	}
	else
	{
		msg_print(muta_desc);
		*(muta_class) &= ~(muta_which);
		
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}

bool get_hack_dir(int *dp)
{
	int		dir;
    cptr    p;
    char command;


	/* Initialize */
	(*dp) = 0;

	/* Global direction */
    dir = 0;

    /* (No auto-targetting */

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
				dir = get_keymap_dir(command);
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
	command_dir = dir;

	/* Check for confusion */
	if (p_ptr->confused)
	{
		/* XXX XXX XXX */
		/* Random direction */
		dir = ddd[rand_int(8)];
	}

	/* Notice confusion */
	if (command_dir != dir)
	{
		/* Warn the user */
		msg_print("You are confused.");
	}

	/* Save direction */
	(*dp) = dir;
 
 #ifdef ALLOW_REPEAT /* TNB */
 
     repeat_push(dir);
 
 #endif /* ALLOW_REPEAT -- TNB */

	/* A "valid" direction was entered */
	return (TRUE);
}


void dump_chaos_features(FILE * OutFile)
{
	
	if (!OutFile) return;
	
	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			fprintf(OutFile, " You can spit acid (dam lvl).\n");
		}
		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			fprintf(OutFile, " You can breathe fire (dam lvl * 2).\n");
		}
		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			fprintf(OutFile, " Your gaze is hypnotic.\n");
		}
		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			fprintf(OutFile, " You are telekinetic.\n");
		}
		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			fprintf(OutFile, " You can teleport at will.\n");
		}
		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			fprintf(OutFile, " You can Mind Blast your enemies.\n");
		}
		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			fprintf(OutFile, " You can emit hard radiation at will.\n");
		}
		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			fprintf(OutFile, " You can drain life from a foe like a vampire.\n");
		}
		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
			fprintf(OutFile, " You can smell nearby precious metal.\n");
		}
		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
			fprintf(OutFile, " You can smell nearby monsters.\n");
		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
			fprintf(OutFile, " You can teleport yourself short distances.\n");
		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			fprintf(OutFile, " You can consume solid rock.\n");
		}
		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
			fprintf(OutFile, " You can switch locations with another being.\n");
		}
		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			fprintf(OutFile, " You can emit a horrible shriek.\n");
		}
		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			fprintf(OutFile, " You can emit bright light.\n");
		}
		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			fprintf(OutFile, " You can feel the danger of evil magic.\n");
		}
		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			fprintf(OutFile, " You can drive yourself into a berserk frenzy.\n");
		}
		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			fprintf(OutFile, " You can polymorph yourself at will.\n");
		}
		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			fprintf(OutFile, " You can turn ordinary items to gold.\n");
		}
		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			fprintf(OutFile, " You can cause mold to grow near you.\n");
		}
		if (p_ptr->muta1 & MUT1_RESIST)
		{
			fprintf(OutFile, " You can harden yourself to the ravages of the elements.\n");
		}
		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			fprintf(OutFile, " You can bring down the dungeon around your ears.\n");
		}
		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
			fprintf(OutFile, " You can consume magic energy for your own use.\n");
		}
		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
			fprintf(OutFile, " You can feel the strength of the magics affecting you.\n");
		}
		if (p_ptr->muta1 & MUT1_STERILITY)
		{
			fprintf(OutFile, " You can cause mass impotence.\n");
		}
		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
			fprintf(OutFile, " You can run for your life after hitting something.\n");
		}
		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			fprintf(OutFile, " You can emit confusing, blinding radiation.\n");
		}
		if (p_ptr->muta1 & MUT1_EYE_BEAM)
		{
			fprintf(OutFile, " Your eyes can fire beams of light.\n");
		}
		if (p_ptr->muta1 & MUT1_RECALL)
		{
			fprintf(OutFile, " You can travel between town and the depths.\n");
		}
		if (p_ptr->muta1 & MUT1_BANISH)
		{
			fprintf(OutFile, " You can send evil creatures directly to Hell.\n");
		}
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			fprintf(OutFile, " You can freeze things with a touch.\n");
		}
		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
			fprintf(OutFile, " You can hurl objects with great force.\n");
		}
	}

	if (p_ptr->muta2)
	{
		if (p_ptr->muta2 & MUT2_BERS_RAGE)
		{
			fprintf(OutFile, " You are subject to berserker fits.\n");
		}
		if (p_ptr->muta2 & MUT2_COWARDICE)
		{
			fprintf(OutFile, " You are subject to cowardice.\n");
		}
		if (p_ptr->muta2 & MUT2_RTELEPORT)
		{
			fprintf(OutFile, " You are teleporting randomly.\n");
		}
		if (p_ptr->muta2 & MUT2_ALCOHOL)
		{
			fprintf(OutFile, " Your body produces alcohol.\n");
		}
		if (p_ptr->muta2 & MUT2_HALLU)
		{
			fprintf(OutFile, " You have a hallucinatory insanity.\n");
		}
		if (p_ptr->muta2 & MUT2_FLATULENT)
		{
			fprintf(OutFile, " You are subject to uncontrollable flatulence.\n");
		}
		if (p_ptr->muta2 & MUT2_PROD_MANA)
		{
			fprintf(OutFile, " You are producing magical energy uncontrollably.\n");
		}
		if (p_ptr->muta2 & MUT2_ATT_DEMON)
		{
			fprintf(OutFile, " You attract demons.\n");
		}
		if (p_ptr->muta2 & MUT2_SCOR_TAIL)
		{
			fprintf(OutFile, " You have a scorpion tail (poison, 3d7).\n");
		}
		if (p_ptr->muta2 & MUT2_HORNS)
		{
			fprintf(OutFile, " You have horns (dam. 2d6).\n");
		}
		if (p_ptr->muta2 & MUT2_BEAK)
		{
			fprintf(OutFile, " You have a beak (dam. 2d4).\n");
		}
		if (p_ptr->muta2 & MUT2_SPEED_FLUX)
		{
			fprintf(OutFile, " You move faster or slower randomly.\n");
		}
		if (p_ptr->muta2 & MUT2_BANISH_ALL)
		{
			fprintf(OutFile, " You sometimes cause nearby creatures to vanish.\n");
		}
		if (p_ptr->muta2 & MUT2_EAT_LIGHT)
		{
			fprintf(OutFile, " You sometimes feed off of the light around you.\n");
		}
		if (p_ptr->muta2 & MUT2_TRUNK)
		{
			fprintf(OutFile, " You have an elephantine trunk (dam 1d4).\n");
		}
		if (p_ptr->muta2 & MUT2_ATT_ANIMAL)
		{
			fprintf(OutFile, " You attract animals.\n");
		}
		if (p_ptr->muta2 & MUT2_TENTACLES)
		{
			fprintf(OutFile, " You have evil looking tentacles (dam 2d5).\n");
		}
		if (p_ptr->muta2 & MUT2_RAW_CHAOS)
		{
			fprintf(OutFile, " You occasionally are surrounded with raw chaos.\n");
		}
		if (p_ptr->muta2 & MUT2_NORMALITY)
		{
			fprintf(OutFile, " You may be chaotic, but you're recovering.\n");
		}
		if (p_ptr->muta2 & MUT2_WRAITH)
		{
			fprintf(OutFile, " You fade in and out of physical reality.\n");
		}
		if (p_ptr->muta2 & MUT2_POLY_WOUND)
		{
			fprintf(OutFile, " Your health is subject to chaotic forces.\n");
		}
		if (p_ptr->muta2 & MUT2_WASTING)
		{
			fprintf(OutFile, " You have a horrible wasting disease.\n");
		}
		if (p_ptr->muta2 & MUT2_ATT_DRAGON)
		{
			fprintf(OutFile, " You attract dragons.\n");
		}
		if (p_ptr->muta2 & MUT2_WEIRD_MIND)
		{
			fprintf(OutFile, " Your mind randomly expands and contracts.\n");
		}
		if (p_ptr->muta2 & MUT2_NAUSEA)
		{
			fprintf(OutFile, " You have a seriously upset stomach.\n");
		}
		if (p_ptr->muta2 & MUT2_CHAOS_GIFT)
		{
			fprintf(OutFile, " Chaos deities give you gifts.\n");
		}
		if (p_ptr->muta2 & MUT2_WALK_SHAD)
		{
			fprintf(OutFile, " You occasionally stumble into other shadows.\n");
		}
		if (p_ptr->muta2 & MUT2_WARNING)
		{
			fprintf(OutFile, " You receive warnings about your foes.\n");
		}
		if (p_ptr->muta2 & MUT2_INVULN)
		{
			fprintf(OutFile, " You occasionally feel invincible.\n");
		}
		if (p_ptr->muta2 & MUT2_SP_TO_HP)
		{
			fprintf(OutFile, " Your blood sometimes rushes to your muscles.\n");
		}
		if (p_ptr->muta2 & MUT2_HP_TO_SP)
		{
			fprintf(OutFile, " Your blood sometimes rushes to your head.\n");
		}
		if (p_ptr->muta2 & MUT2_DISARM)
		{
			fprintf(OutFile, " You occasionally stumble and drop things.\n");
		}
	}

	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			fprintf(OutFile, " You are superhumanly strong (+4 STR).\n");
		}
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			fprintf(OutFile, " You are puny (-4 STR).\n");
		}
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			fprintf(OutFile, " Your brain is a living computer (+4 INT/WIS).\n");
		}
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			fprintf(OutFile, " You are moronic (-4 INT/WIS).\n");
		}
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			fprintf(OutFile, " You are very resilient (+4 CON).\n");
		}
		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			fprintf(OutFile, " You are extremely fat (+2 CON, -2 speed).\n");
		}
		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			fprintf(OutFile, " You are albino (-4 CON).\n");
		}
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			fprintf(OutFile, " Your flesh is rotting (-2 CON, -1 CHR).\n");
		}
		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			fprintf(OutFile, " Your voice is a silly squeak (-4 CHR).\n");
		}
		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
			fprintf(OutFile, " Your face is featureless (-1 CHR).\n");
		}
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			fprintf(OutFile, " Your appearance is masked with illusion.\n");
		}
		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
			fprintf(OutFile, " You have an extra pair of eyes (+15 search).\n");
		}
		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			fprintf(OutFile, " You are resistant to magic.\n");
		}
		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			fprintf(OutFile, " You make a lot of strange noise (-3 stealth).\n");
		}
		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			fprintf(OutFile, " You have remarkable infravision (+3).\n");
		}
		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			fprintf(OutFile, " You have an extra pair of legs (+3 speed).\n");
		}
		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			fprintf(OutFile, " Your legs are short stubs (-3 speed).\n");
		}
		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			fprintf(OutFile, " Electricity is running through your veins.\n");
		}
		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			fprintf(OutFile, " Your body is enveloped in flames.\n");
		}
		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			fprintf(OutFile, " Your skin is covered with warts (-2 CHR, +5 AC).\n");
		}
		if (p_ptr->muta3 & MUT3_SCALES)
		{
			fprintf(OutFile, " Your skin has turned into scales (-1 CHR, +10 AC).\n");
		}
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			fprintf(OutFile, " Your skin is made of steel (-1 DEX, +25 AC).\n");
		}
		if (p_ptr->muta3 & MUT3_WINGS)
		{
			fprintf(OutFile, " You have wings.\n");
		}
		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			fprintf(OutFile, " You are completely fearless.\n");
		}
		if (p_ptr->muta3 & MUT3_REGEN)
		{
			fprintf(OutFile, " You are regenerating.\n");
		}
		if (p_ptr->muta3 & MUT3_ESP)
		{
			fprintf(OutFile, " You are telepathic.\n");
		}
		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			fprintf(OutFile, " Your body is very limber (+3 DEX).\n");
		}
		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			fprintf(OutFile, " Your joints ache constantly (-3 DEX).\n");
		}
		if (p_ptr->muta3 & MUT3_RES_TIME)
		{
			fprintf(OutFile, " You are protected from the ravages of time.\n");
		}
		if (p_ptr->muta3 & MUT3_VULN_ELEM)
		{
			fprintf(OutFile, " You are susceptible to damage from the elements.\n");
		}
		if (p_ptr->muta3 & MUT3_MOTION)
		{
			fprintf(OutFile, " Your movements are precise and forceful (+1 STL).\n");
		}
		if (p_ptr->muta3 & MUT3_SUS_STATS)
		{
			fprintf(OutFile, " Your body resists serious damage.\n");
		}
	}
}
