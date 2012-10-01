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

static cptr favor_text[11] = {
  "impossible",
  "impossible",
  "impossible",
  "extremely unlikely",
  "extremely unlikely",
  "extremely unlikely",
  "unlikely",
  "unlikely",
  "likely",
  "certain",
  "certain beyond any doubt"
};

/* Always return FALSE at the moment. Won't delete it since it's technically implemented... */
static bool too_weak(monster_type *m_ptr)
{
	/*monster_race *r_ptr = &r_info[m_ptr->r_idx];*/
	
	/*if (p_ptr->lev > (r_ptr->level * 4)) return (TRUE);*/

	return (FALSE);
}


/*
 * Set "p_ptr->tim_invis", and "p_ptr->tim_inv_pow",
 * notice observable changes
 */
bool set_invis(int v, int p)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->tim_invisible)
		{
			msg_print("You feel your body fade away.");
                        /* Shadow Stalker's Shadow Magic! */
                        if (p_ptr->abilities[(CLASS_SHADOW * 10) + 8] >= 1)
                        {
                                p_ptr->csp += ((p_ptr->skill[6] * 5) + ((p_ptr->skill[6] * 5) * ((p_ptr->abilities[(CLASS_SHADOW * 10) + 8] * 20) / 100)));
                        }
			notice = TRUE;
                        update_and_handle();
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->tim_invisible)
		{
			msg_print("You are no longer invisible.");
                        if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
			notice = TRUE;
			p = 0;
                        update_and_handle();
		}
	}

	/* Use the value */
        p_ptr->tim_invisible = v;
	p_ptr->tim_inv_pow = p;

	/* Nothing to notice */
	if (!notice)
		return (FALSE);

	/* Disturb */
	if (disturb_state)
		disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Result */
	return (TRUE);
}

/*
 * Set "p_ptr->tim_ffall"
 */
bool set_tim_ffall(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->tim_ffall)
		{
                        msg_print("You feel very light.");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->tim_ffall)
		{
                        msg_print("You are suddently heavier.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->tim_ffall = v;

	/* Nothing to notice */
	if (!notice)
		return (FALSE);

	/* Disturb */
	if (disturb_state)
		disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Result */
	return (TRUE);
}


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

	/* Fully update the visuals */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE | PU_VIEW | PU_LITE | PU_MONSTERS);

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
bool set_shield(int v, int p)
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
                        p = 0;
		}
	}

	/* Use the value */
	p_ptr->shield = v;
        p_ptr->shield_power = p;

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

                        /* Redraw map */
                        p_ptr->redraw |= (PR_MAP);

                        /* Update monsters */
                        p_ptr->update |= (PU_MONSTERS);

                        /* Window stuff */
                        p_ptr->window |= (PW_OVERHEAD);
                }
	}

	/* Shut */
	else
	{
		if (p_ptr->shero)
		{
			msg_print("You feel less Berserk.");
			notice = TRUE;

                        /* Redraw map */
                        p_ptr->redraw |= (PR_MAP);

                        /* Update monsters */
                        p_ptr->update |= (PU_MONSTERS);

                        /* Window stuff */
                        p_ptr->window |= (PW_OVERHEAD);
		}
	}

	/* Use the value */
	p_ptr->shero = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
        /*if (disturb_state) disturb(0, 0);*/

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
            /* msg_print("You feel opaque."); */
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
        /* if (disturb_state) disturb(0, 0); */

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

    if (p_ptr->prace == RACE_ENT)
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
        if (safety_check()) old_aux = 0;


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
        if (safety_check()) old_aux = 0;

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
                if (!p_ptr->sustain_int) { (void) do_dec_stat(A_INT, STAT_DEC_NORMAL); }
                if (!p_ptr->sustain_wis) { (void) do_dec_stat(A_WIS, STAT_DEC_NORMAL); }
            }
            else if (randint(2)==1)
            {
                if (!p_ptr->sustain_int) { (void) do_dec_stat(A_INT, STAT_DEC_NORMAL); }
            }
            else
            {
                if (!p_ptr->sustain_wis) { (void) do_dec_stat(A_WIS, STAT_DEC_NORMAL); }
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

    if (p_ptr->prace == RACE_VAMPIRE || p_ptr->prace == RACE_SKELETON )
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

            do_dec_stat(A_CHR, STAT_DEC_NORMAL);
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
 * Advance experience levels and print experience
 */
void check_experience(void)
{
	int		i;
	bool level_mutation = FALSE;


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
	       (p_ptr->exp < multiply_divide(player_exp[p_ptr->lev-2], p_ptr->expfact, 100L)))
	{
		/* Lose a level */
		p_ptr->lev--;
		lite_spot(py, px);

		/* Update some stuff */
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);

		/* Handle stuff */
		handle_stuff();
	}


	/* Gain levels while possible */
	while ((p_ptr->lev < PY_MAX_LEVEL) &&
	       (p_ptr->exp >= multiply_divide(player_exp[p_ptr->lev-1], p_ptr->expfact, 100L)))
	{
                bool nostats = FALSE;
		/* Gain a level */
                if (p_ptr->lev < p_ptr->max_plv) nostats = TRUE;
		p_ptr->lev++;
		lite_spot(py, px);

		/* Save the highest level */
		if (p_ptr->lev > p_ptr->max_plv)
		{
			p_ptr->max_plv = p_ptr->lev;
		}

                /* Stat and skill points. */
                /* You always gain 2 stat points. You gain at least 2 */
                /* skill point, maybe more if you have a high wisdom/intelligence. */
                if (p_ptr->lev >= p_ptr->max_plv && nostats == FALSE)
                {
                        int statamt, skillamt, apamt;
			call_lua("stat_points_per_levels", "", "d", &statamt);
                        p_ptr->statpoints += statamt;
                        
			call_lua("skill_points_per_levels", "", "d", &skillamt);
                        p_ptr->skillpoints += skillamt;
			
			call_lua("ability_points_per_levels", "", "d", &apamt);
                        p_ptr->ability_points += apamt; 
                }

		/* Sound */
		sound(SOUND_LEVEL);

		/* Message */
		msg_format("Welcome to level %d.", p_ptr->lev);

		/* Update some stuff */
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

		/* Redraw some stuff */
		p_ptr->redraw |= (PR_LEV | PR_TITLE);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_SPELL);

		/* Handle stuff */
		handle_stuff();

		if (level_mutation)
		{
			msg_print("You feel different...");
			(void)gain_random_mutation(0);
			level_mutation = FALSE;
		}
	}
}


/*
 * Gain experience
 */
void gain_exp(s32b amount)
{
        /* Do not gain experience in town. */
        if (dun_level == 0) amount = 0;

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
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * XXX XXX XXX Note the use of actual "monster names"
 */
int get_coin_type(monster_race *r_ptr)
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
 * This routine handles the production of corpses/skeletons/heads/skulls
 * when a monster is killed.
 */
void place_corpse(monster_type *m_ptr)
{
   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   object_type *i_ptr;
   object_type object_type_body;

   int x = m_ptr->fx;
   int y = m_ptr->fy;
#if 0 /* If I can find some time to implement the decapitation ... */
   object_type *w_ptr = &inventory[INVEN_WIELD];

   int i = w_ptr->weight + ((p_ptr->to_h + w_ptr->to_h) * 5) + (p_ptr->lev * 3);

	/* Handle decapitations. This is not allowed with hafted weapons. */
   bool crit = (randint(5000) <= i);

   bool decapitate = ((rand_int(m_ptr->maxhp) <= -(m_ptr->hp)) &&
                       (w_ptr->tval != TV_HAFTED) && crit);
#endif

   /* An Essence capturing monster will not get corpses. */
   if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29015] == 1) return;
	
  	/* Get local object */
	i_ptr = &object_type_body;
	
   /* It has a physical form */
   if (r_ptr->flags9 & RF9_DROP_CORPSE)
   {
		/* Wipe the object */
                object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_CORPSE));

     	/* Unique corpses are unique */
     	if (r_ptr->flags1 & RF1_UNIQUE)
     	{
        	object_aware(i_ptr);
                i_ptr->name1 = 201;
     	}

     	/* Calculate length of time before decay */
        i_ptr->pval = r_ptr->weight + rand_int(r_ptr->weight);

		/* Set weight */
        i_ptr->weight = (r_ptr->weight + rand_int(r_ptr->weight) / 10) + 1;

     	/* Remember what we are */
        i_ptr->pval2 = m_ptr->r_idx;

        /* Some hp */
        /* i_ptr->pval3 = ((maxroll(r_ptr->hdice, r_ptr->hside) + p_ptr->mhp) / 2); */
        /* i_ptr->pval3 -= randint(i_ptr->pval3) / 3; */
        /* In order to work best with Corpse Explosion... */
        i_ptr->pval3 = m_ptr->maxhp / 2;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
   }

   /* The creature is an animated skeleton. */
   if (!(r_ptr->flags9 & RF9_DROP_CORPSE) && (r_ptr->flags9 & RF9_DROP_SKELETON))
   {
		/* Wipe the object */
                object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKELETON));

     	/* Unique corpses are unique */
     	if (r_ptr->flags1 & RF1_UNIQUE)
     	{
			object_aware(i_ptr);
                i_ptr->name1 = 201;
     	}

      i_ptr->pval = 0;

		/* Set weight */
        i_ptr->weight = (r_ptr->weight / 4 + rand_int(r_ptr->weight) / 40) + 1;

     	/* Remember what we are */
        i_ptr->pval2 = m_ptr->r_idx;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
   }

#if 0 /* If I can find some time to implement the decapitation ... */
   /*
    * Decapitated it if it has a head, or if it *is* a head.
    * This is rather messy.
    */
   if ((!(r_ptr->flags9 & RF9_HAS_NO_HEAD)) && (decapitate ||
         (!(r_ptr->flags9 & RF9_DROP_CORPSE) &&
       !(r_ptr->flags9 & RF9_DROP_SKELETON))))
   {
		/* Wipe the object */
                object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_HEAD));

     	/* Unique heads are unique */
      if (r_ptr->flags1 & RF1_UNIQUE)
      {
         object_aware(i_ptr);
                i_ptr->name1 = 201;
		}

      /* Calculate length of time before decay */
        i_ptr->pval = r_ptr->weight / 30 + rand_int(r_ptr->weight) / 30;

		/* Set weight */
           i_ptr->weight = (r_ptr->weight / 30 + rand_int(r_ptr->weight) / 300) + 1;

      /* Remember what we are */
        i_ptr->pval2 = m_ptr->r_idx;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
   }

   /* It has a skull, but no head */
   if (!(r_ptr->flags9 & RF9_HAS_NO_HEAD) && (!(r_ptr->flags9 & RF9_HAS_NO_SKULL)) &&
       (decapitate || (!(r_ptr->flags9 & RF9_DROP_CORPSE) &&
       !(r_ptr->flags9 & RF9_DROP_SKELETON))))
   {
		/* Wipe the object */
                object_prep(i_ptr, lookup_kind(TV_CORPSE, SV_CORPSE_SKULL));

     	/* Unique heads are unique */
      if (r_ptr->flags1 & RF1_UNIQUE)
		{
			object_aware(i_ptr);
                        i_ptr->name1 = 201;
		}

		i_ptr->pval = 0;

		/* Set weight */
                i_ptr->weight = (r_ptr->weight / 60 + rand_int(r_ptr->weight) / 600) + 1;

		/* Remember what we are */
                i_ptr->pval2 = m_ptr->r_idx;

		/* Drop it in the dungeon */
		drop_near(i_ptr, -1, y, x);
	}
#endif
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
        int i, y, x, ny, nx, i2, j2;

	int dump_item = 0;
	int dump_gold = 0;

	int number_mon;

	s16b this_o_idx, next_o_idx = 0;

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	bool visible = (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)));


	bool cloned = FALSE;
	bool create_stairs = FALSE;
	bool reward = FALSE;
	int force_coin = get_coin_type(r_ptr);

	object_type forge;
	object_type *q_ptr;

#ifdef USE_PYTHON
        if (perform_event(EVENT_MONSTER_DEATH, Py_BuildValue("(iii)", m_idx, m_ptr->r_idx, 0))) return;
#endif

	/* Get the location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	if (m_ptr->smart &(SM_CLONED))
		cloned = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_death > 0)
	{
		call_lua("monster_dies", "(dd)", "", m_idx, r_ptr->event_death);
	}

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

                if(q_ptr->tval == TV_GOLD) dump_gold++;
                else dump_item++;

		/* Drop it */
		drop_near(q_ptr, -1, y, x);
	}
        
	/* Forget objects */
	m_ptr->hold_o_idx = 0;

	/* Average dungeon and monster levels */
	object_level = (dun_level + r_ptr->level) / 2;

        /* Handle the special treasure a monster may carry */
        if (r_ptr->treasuretval > 0 && (randint(100) <= r_ptr->treasurechance || r_ptr->treasuremagic == 4) && ((!is_pet(m_ptr) && m_ptr->angered_pet == 0 && m_ptr->summoned == 0 && !(m_ptr->no_experience)) || (r_ptr->flags9 & (RF9_SPECIAL_GENE))))
	{
		q_ptr = &forge;

                object_prep(q_ptr, lookup_kind(r_ptr->treasuretval, r_ptr->treasuresval));

                if (r_ptr->treasuremagic == 1)
                        apply_magic(q_ptr, object_level, TRUE, TRUE, FALSE, FALSE);
                if (r_ptr->treasuremagic == 2)
                        apply_magic(q_ptr, object_level, TRUE, TRUE, TRUE, FALSE);
                if (r_ptr->treasuremagic == 3)
                        apply_magic(q_ptr, object_level, TRUE, TRUE, TRUE, TRUE);
		if (r_ptr->treasuremagic == 4)
		{
			q_ptr->name1 = r_ptr->treasurechance;
                        apply_magic(q_ptr, object_level, TRUE, TRUE, TRUE, TRUE);
		}


                drop_near(q_ptr, -1, y, x);
	}

	/* A defeated Nightmare Horror always drops this. */
        if (r_ptr->cursed > 0 && ((!is_pet(m_ptr) && m_ptr->angered_pet == 0 && m_ptr->summoned == 0 && !(m_ptr->no_experience))))
	{
		q_ptr = &forge;

                object_prep(q_ptr, lookup_kind(TV_LICIALHYD, 2));

                q_ptr->pval = m_ptr->r_idx;
		q_ptr->pval2 = -1;
		q_ptr->pval3 = m_ptr->level;

		q_ptr->cursed = r_ptr->cursed;

		object_aware(q_ptr);
		object_known(q_ptr);
		q_ptr->ident |= (IDENT_MENTAL);

                drop_near(q_ptr, -1, y, x);
	}

	/* Monsters can capture essences. */
        if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29015] == 1 && ((!is_pet(m_ptr) && m_ptr->angered_pet == 0 && m_ptr->summoned == 0 && !(m_ptr->no_experience))))
	{
		int mtype;
		q_ptr = &forge;

                object_prep(q_ptr, lookup_kind(TV_ESSENCE, 1));

		if (r_ptr->cursed > 0) mtype = 4;
		else if (r_ptr->flags1 & (RF1_UNIQUE)) mtype = 3;
		else if (m_ptr->boss == 2) mtype = 2;
		else if (m_ptr->boss == 1) mtype = 1;
		else mtype = 0;

                prepare_essence(q_ptr, m_ptr->r_idx, m_ptr->level, mtype);
		q_ptr->pval = m_ptr->r_idx;
		q_ptr->pval2 = m_ptr->level;

		object_aware(q_ptr);
		object_known(q_ptr);
		q_ptr->ident |= (IDENT_MENTAL);

                drop_near(q_ptr, -1, y, x);
	}

	/* Defeating a Scaled UNIQUE monster gives you nice rewards! */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->flags7 & (RF7_SCALED)) && (!is_pet(m_ptr) && m_ptr->angered_pet == 0 && m_ptr->summoned == 0 && !(m_ptr->no_experience)))
	{
		dungeon_info_type *d_ptr;
		int statamt, skillamt, apamt;

		d_ptr = &d_info[dungeon_type];

		call_lua("stat_points_per_levels", "", "d", &statamt);
                p_ptr->statpoints += statamt * r_ptr->cr;
                        
		call_lua("skill_points_per_levels", "", "d", &skillamt);
                p_ptr->skillpoints += skillamt * r_ptr->cr;
			
		call_lua("ability_points_per_levels", "", "d", &apamt);
                p_ptr->ability_points += apamt * r_ptr->cr;

		/* If it was also a secret boss, you gain more, and beat the secret level! */
		if (r_ptr->flags7 & (RF7_SECRET_BOSS))
		{

			call_lua("stat_points_per_levels", "", "d", &statamt);
                	p_ptr->statpoints += statamt * r_ptr->cr;
                        
			call_lua("skill_points_per_levels", "", "d", &skillamt);
                	p_ptr->skillpoints += skillamt * r_ptr->cr;
			
			call_lua("ability_points_per_levels", "", "d", &apamt);
                	p_ptr->ability_points += apamt * r_ptr->cr;

			p_ptr->events[d_info[dungeon_type].secretevent] = 2;
			p_ptr->secretscleared += 1;

			update_and_handle();
		}
	}

	/* Reset */
	object_level = dun_level;

	/* Set an event associated with the death of this monster */
        if (r_ptr->event > 0)
	{
		p_ptr->events[r_ptr->event] = r_ptr->extra1;
	}
        /* Hostile monster is killed, show dialog(if any) */
	/* But only if there's a CAN_SPEAK or DEATH_DIALOG flag!! */
        if (r_ptr->extra2 > 0 && ((r_ptr->flags2 & (RF2_CAN_SPEAK)) || (r_ptr->flags7 & (RF7_DEATH_DIALOG))))
	{
		show_dialog(r_ptr->extra2);
	}

        if((!force_coin)&&(randint(100)<50)) place_corpse(m_ptr);

	/* Take note of any dropped treasure */
	if (visible && (dump_item || dump_gold))
	{
		/* Take notes on treasure */
		lore_treasure(m_idx, dump_item, dump_gold);
	}

	/* Create a magical staircase */
	if (create_stairs)
	{
                int i, j;

                for(i = -1; i <= 1; i++)
                for(j = -1; j <= 1; j++)
                        if(!(f_info[cave[y + j][x + i].feat].flags1 & FF1_PERMANENT)) cave_set_feat(y + j, x + i, d_info[dungeon_type].floor1);

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

		/* Remember to update everything */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
	}

	/*
	 * Drop quest reward
	 */
	if (reward)
	{
		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Make a great object */
		make_object(q_ptr, TRUE, TRUE);

		/* Drop it in the dungeon */
		drop_near(q_ptr, -1, y, x);
	}

	return;

#ifdef USE_PYTHON
        perform_event(EVENT_MONSTER_DEATH, Py_BuildValue("(iii)", m_idx, m_ptr->r_idx, 1));
#endif
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
bool mon_take_hit(int m_idx, s32b dam, bool *fear, cptr note)
{
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	s32b            div, new_exp, new_exp_frac;


	/* Redraw (later) if needed */
	if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	/* ...or not. */
	if (!(dontwakeup)) m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* Cancel CON_JOB */
	if (dam > 0) m_ptr->abilities &= ~(CON_JOB);

	/* Call a lua script. */
	if (r_ptr->event_take_damages > 0)
	{
		call_lua("monster_take_damages", "(dd)", "", m_idx, r_ptr->event_take_damages);
	}	

        /* It is dead now */
	if (m_ptr->hp < 0)
	{
		char m_name[80];
		bool gainexp = TRUE;

		/* Extract monster name */
		monster_desc(m_name, m_ptr, 0);

                if (m_ptr->lives > 0)
		{
			sound(SOUND_FLEE);
			m_ptr->lives -= 1;
			msg_format("%^s has lost a life point.", m_name);
			m_ptr->hp = m_ptr->maxhp;
			return (FALSE);
		}

		/* Immortality. */
		if ((r_ptr->flags7 & (RF7_IMMORTAL)) && (enemy_immortality))
		{
			msg_format("%^s has been knocked out.", m_name);
			m_ptr->hp = 1;
			m_ptr->seallight = 5;
			return (FALSE);
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
                
                /* Maximum player level */
                /* div = p_ptr->max_plv; */

                /* You killed something! Now, check if you should */
                /* advance your class level... */
		/*if (!(((p_ptr->inside_quest != 0 && p_ptr->inside_quest != 9000) || too_weak(m_ptr) || (m_ptr->no_experience)) && !(r_ptr->flags1 & (RF1_UNIQUE)))) add_class_kill(m_ptr); */

		/* Give some experience for the kill */
                /* new_exp = ((long)r_ptr->mexp * r_ptr->level) / div; */

		/* Handle fractional experience */
                /*new_exp_frac = ((((long)r_ptr->mexp * r_ptr->level) % div)
                                * 0x10000L / div) + p_ptr->exp_frac;*/

		/* Keep track of experience */
                /*if (new_exp_frac >= 0x10000L)
		{
			new_exp++;
			p_ptr->exp_frac = new_exp_frac - 0x10000L;
		}
		else
		{
			p_ptr->exp_frac = new_exp_frac;
                }*/

		/* Gain experience *

		/* Killing guards/townsfolk will make you EVIL!! */
		if (((r_ptr->flags7 & (RF7_TOWNSFOLK)) || (r_ptr->flags7 & (RF7_GUARD))) && m_ptr->angered_pet)
		{
			/* Killing innocents will make us evil. However, */
			/* we won't go lower than -5. */
			if (p_ptr->alignment > -5) p_ptr->alignment -= 1;
		}

                /* Do not gain any exp for killing friends! */
                if (is_pet(m_ptr) || m_ptr->angered_pet == 1)
                {
                        msg_print("No experience for killing a friend!");
			gainexp = FALSE;

			/* Also, if we kill a townsfolk or a guard, guards gets angry! */
			if ((r_ptr->flags7 & (RF7_TOWNSFOLK)) || (r_ptr->flags7 & (RF7_GUARD)))
			{
				int i;
				/* Delete the monsters of that "type" */
				for (i = 1; i < m_max; i++)
				{
					monster_type    *m_ptr = &m_list[i];
					monster_race    *r_ptr = &r_info[m_ptr->r_idx];

					/* Paranoia -- Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Guards will get angry! */
					if (r_ptr->flags7 & (RF7_GUARD))
					{
						set_pet(m_ptr, FALSE);
						m_ptr->angered_pet = 1;
					}

					/* While other non-unique townsfolk will get panicked! */
					if ((r_ptr->flags7 & (RF7_TOWNSFOLK)) && !(r_ptr->flags1 & (RF1_UNIQUE)))
					{
						m_ptr->monfear = 30;
					}
				}
			}                       
                }
                if (gainexp)
		{
                        gain_exp_kill(m_ptr);
                }
                
		/* Generate treasure */
                monster_death(m_idx);
		
		/* When the player kills a Unique, it stays dead */
                if (r_ptr->flags1 & (RF1_UNIQUE))
                {
                        r_ptr->max_num = 0;
                }

		/* Recall even invisible uniques or winners */
		if (m_ptr->ml || (r_ptr->flags1 & (RF1_UNIQUE)))
		{
			/* Count kills this life */
			if (r_ptr->r_pkills < MAX_SHORT)
			{
				r_ptr->r_pkills++;
				if (m_ptr->boss == 1) r_ptr->r_ekills++;
				if (m_ptr->boss == 2) r_ptr->r_bkills++;
			}

			/* Count kills in all lives */
			if (r_ptr->r_tkills < MAX_SHORT) r_ptr->r_tkills++;

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
	if (!m_ptr->monfear && !(r_ptr->flags3 & (RF3_NO_FEAR)) && m_ptr->lives == 0)
	{
		int percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		 * Run (sometimes) if at 10% or less of max hit points,
		 * or (usually) when hit for half its current hit points
		 */
		if ((percentage <= 10) && (rand_int(10) < percentage) && r_ptr->cr == 1)
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
	if (!center_player)
	{
		panel_row_min = panel_row * (SCREEN_HGT / 2);
		panel_row_max = panel_row_min + SCREEN_HGT - 1;
		panel_row_prt = panel_row_min - 1;
		panel_col_min = panel_col * (SCREEN_WID / 2);
		panel_col_max = panel_col_min + SCREEN_WID - 1;
		panel_col_prt = panel_col_min - 13;
	}
	else
	{
		panel_row_min = py - (SCREEN_HGT / 2);
		panel_row_max = py + ((SCREEN_HGT / 2) - 1);
		if (panel_row_min < 0)
		{
			panel_row_min = 0;
			panel_row_max = SCREEN_HGT-1;
		}
		if (panel_row_max > (MAX_HGT - 1))
		{
			panel_row_min = (MAX_HGT - 1) - (SCREEN_HGT - 1);
			panel_row_max = (MAX_HGT - 1);
		}
		panel_row_prt = panel_row_min - 1;

		panel_col_min = px - (SCREEN_WID / 2);
		panel_col_max = px + ((SCREEN_WID / 2) - 1);
		if (panel_col_min < 0)
		{
			panel_col_min = 0;
			panel_col_max = SCREEN_WID-1;
		}
		if (panel_col_max > (MAX_WID - 1))
		{
			panel_col_min = (MAX_WID - 1) - (SCREEN_WID - 1);
			panel_col_max = (MAX_WID - 1);
		}
		panel_col_prt = panel_col_min - 13;
	}
}

void panel_bounds_center(int y, int x)
{
	panel_row_min = y - (SCREEN_HGT / 2);
	panel_row_max = y + ((SCREEN_HGT / 2) - 1);
	if (panel_row_min < 0)
	{
		panel_row_min = 0;
		panel_row_max = SCREEN_HGT-1;
	}
	if (panel_row_max > (MAX_HGT - 1))
	{
		panel_row_min = (MAX_HGT - 1) - (SCREEN_HGT - 1);
		panel_row_max = (MAX_HGT - 1);
	}
	panel_row_prt = panel_row_min - 1;

	panel_col_min = x - (SCREEN_WID / 2);
	panel_col_max = x + ((SCREEN_WID / 2) - 1);
	if (panel_col_min < 0)
	{
		panel_col_min = 0;
		panel_col_max = SCREEN_WID-1;
	}
	if (panel_col_max > (MAX_WID - 1))
	{
		panel_col_min = (MAX_WID - 1) - (SCREEN_WID - 1);
		panel_col_max = (MAX_WID - 1);
	}
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

	int prow = panel_row;
	int pcol = panel_col;

	if (!center_player)
	{
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

		/* Update stuff */
		p_ptr->update |= (PU_MONSTERS);

		/* Redraw map */
		p_ptr->redraw |= (PR_MAP);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD);
	}
	else
	{
		/* Recalculate the boundaries */
		panel_bounds();

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

	bool          living = TRUE;
	s32b           perc;


	/* Determine if the monster is "living" (vs "undead") */
	if (r_ptr->flags3 & (RF3_UNDEAD)) living = FALSE;
	if (r_ptr->flags3 & (RF3_DEMON)) living = FALSE;
	if (r_ptr->flags3 & (RF3_NONLIVING)) living = FALSE;
	if (strchr("Egv", r_ptr->d_char)) living = FALSE;


	/* Healthy monsters */
	if (m_ptr->hp >= m_ptr->maxhp)
	{
		/* No damage */
		return (living ? "unhurt" : "undamaged");
	}


	/* Calculate a health "percentage" */
	if (m_ptr->maxhp > 10000)
	{
		perc = multiply_divide((m_ptr->hp / 100), 100, (m_ptr->maxhp / 100));
	}
	else
	{
		perc = multiply_divide(m_ptr->hp, 100, m_ptr->maxhp);
	}

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

	/* Monster must be projectable */
	if (!projectable(py, px, m_ptr->fy, m_ptr->fx)) return (FALSE);

	/* Hack -- no targeting hallucinations */
	if (p_ptr->image) return (FALSE);

        /* Dont target pets */
        /* if (is_pet(m_ptr)) return (FALSE); */

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
                if(f_info[c_ptr->feat].flags1 & FF1_NOTICE) return TRUE;
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

				/* Hack -- track this monster race */
				monster_type_track(c_ptr->m_idx);

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
                                                screen_roff_boss(m_ptr->r_idx, 0, m_ptr);

						/* Hack -- Complete the prompt (again) */
						Term_addstr(-1, TERM_WHITE, format("  [r,R,%s]", info));
					
						/* Command */
						query = inkey();

						/* Restore */
						Term_load();
					}

					/* Normal */
					else
					{
						/* Describe, and prompt for recall */
						sprintf(out_val, "%s%s%s%s (%s)%s%s[r,R,%s]",
						    s1, s2, s3, m_name, look_mon_desc(c_ptr->m_idx),
						    (m_ptr->smart & SM_CLONED ? " (clone)": ""),
						    (is_pet(m_ptr) ? " (friendly) " : " "), info);

						prt(out_val, 0, 0);

						/* Place cursor */
						move_cursor_relative(y, x);
					
						/* Command */
						query = inkey();
					}

					/* Normal commands */
					if (query != 'r')
					{
						if (query == 'R') evolution_compare(m_ptr->r_idx, FALSE, TRUE);
						else break;
					}

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

		/* Actual traps */
		if (c_ptr->t_idx)
		{
                        cptr name = "a trap", s4;

			/* Name trap */
                        if (t_info[c_ptr->t_idx].ident)
                                s4 = format("(%s)", t_name + t_info[c_ptr->t_idx].name);
                        else
                                s4 = "an unknown trap";

			/* Display a message */
			sprintf(out_val, "%s%s%s%s [%s]", s1, s2, s3, name, s4);
			prt(out_val, 0, 0);
			move_cursor_relative(y, x);
			query = inkey();

			/* Stop on everything but "return" */
			if ((query != '\r') && (query != '\n')) break;

			/* Repeat forever */
			continue;
                }

		/* Feature (apply "mimic") */
		feat = f_info[c_ptr->feat].mimic;

		/* Require knowledge about grid, or ability to see grid */
		if (!(c_ptr->info & (CAVE_MARK)) && !player_can_see_bold(y,x))
		{
			/* Forget feature */
			feat = FEAT_NONE;
		}

		/* Terrain feature if needed */
		if (boring || (feat >= FEAT_GLYPH))
		{
			cptr name;

			/* Hack -- special handling for building doors */
			if ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL))
			{
				name = building[feat - FEAT_BLDG_HEAD].name;
			}
			else
			{
				name = f_name + f_info[feat].name;
			}

			/* Hack -- handle unknown grids */
			if (feat == FEAT_NONE) name = "unknown grid";

			/* Pick a prefix */
			if (*s2 && ((feat >= FEAT_MINOR_GLYPH) &&
			   (feat <= FEAT_PATTERN_XTRA2))) s2 = "on ";
			else if (*s2 && (feat >= FEAT_DOOR_HEAD)) s2 = "in ";

			/* Pick proper indefinite article */
			s3 = (is_a_vowel(name[0])) ? "an " : "a ";

			/* Hack -- special introduction for store & building doors -KMW- */
			if (((feat >= FEAT_SHOP_HEAD) && (feat <= FEAT_SHOP_TAIL)) ||
			    ((feat >= FEAT_BLDG_HEAD) && (feat <= FEAT_BLDG_TAIL)))
			{
				s3 = "the entrance to the ";
			}

                        if ((feat == FEAT_MORE) && c_ptr->special)
                        {
                                s3 = "";
                                name = d_text + d_info[c_ptr->special].text;
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
					/* Extract the action (if any) */
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
					/* Extract the action (if any) */
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

	if (repeat_pull(dp))
	{
		/* Confusion? */

		/* Verify */
		if (!(*dp == 5 && !target_okay()))
		{
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
	dir = 0;
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
				/* Extract the action (if any) */
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

	if (repeat_pull(dp))
	{
		return (TRUE);
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Initialize */
	(*dp) = 0;

	/* Global direction */
	dir = command_dir;

	/* Get a direction */
	dir = 0;
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
		case 27:
			break;
		case ' ':
			success = TRUE; break;
		default:
			/* Look up the direction */
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

	Term->scr->cu = cu;
	Term->scr->cv = cv;
	Term_fresh();
	return success;
}


bool gain_random_mutation(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;
	
	if (choose_mut) attempts_left = 1;
	
	
}

bool lose_mutation(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	int muta_which = 0;
	u32b * muta_class = 0;
	
	if (choose_mut) attempts_left = 1;
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
				/* Look up the direction */
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
	
	/* A "valid" direction was entered */
	return (TRUE);
}


void dump_mutations(FILE * OutFile)
{
	
	if (!OutFile) return;
	
	if (p_ptr->muta1)
	{
		/*if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			fprintf(OutFile, " You can spit acid (dam lvl).\n");
		}*/
		
	}

	if (p_ptr->muta2)
	{
		
	}

	if (p_ptr->muta3)
	{
		
	}
}

/*
 * Allow the player to make a wish
 */
void make_wish(void)
{
        char buf[80], name[80];
        int i, j;
        object_type *q_ptr, forge;

        /* Make an empty string */
        buf[0] = 0;

        /* Ask for the wish */
        if(!get_string("Wish for what? ", buf, 80)) return;

        /* Lowercase the wish */
        strlower(buf);

        /* You can't wish for a wish ! */
        if(strstr(buf, "wish"))
        {
                msg_print("You can't wish for a wish !");
                return;
        }

        /* First, try all the objects */
        for(i = 0; i < max_k_idx; i++)
        {
                /* Get local object */
                q_ptr = &forge;

                /* Hack -- Give the player an object */
                object_prep(q_ptr, i);

                /* Can have great items but NEVER artifacts */
                apply_magic(q_ptr, dun_level, TRUE, TRUE, FALSE, FALSE);

                object_aware(q_ptr);
                object_known(q_ptr);

                object_desc(name, q_ptr, 0, 0);

                /* Paranoia */
                if(q_ptr->name1) continue;
                if(q_ptr->art_name) continue;

                /* Lowercase the name */
                strlower(name);

                if(strstr(name, buf))
                {
                        /* You can't wish for a wish ! */
                        if((q_ptr->tval == TV_STAFF) && (q_ptr->sval == SV_STAFF_WISHING))
                        {
                                msg_print("You can't wish for a wish !");
                                return;
                        }

                        msg_print("Your wish become truth!");

                        /* Give it to the player */
                        drop_near(q_ptr, -1, py, px);

                        /* Don't search any more */
                        return;
                }
        }

        /* Try all the monsters */
        for(i = 1; i < max_r_idx - 1; i++)
        {
                monster_race_desc(name, i);

                /* Never Uniques */
                if(r_info[i].flags1 & RF1_UNIQUE) continue;

                /* Nor quest monsters */
                if(r_info[i].flags1 & RF1_QUESTOR) continue;

		/* Nor SPECIAL_GENE */
                if(r_info[i].flags9 & RF9_SPECIAL_GENE) continue;

		/* Nor cursed. */
		if(r_info[i].cursed > 0) continue;

                /* Lowercase the name */
                strlower(name);

                if(strstr(name, buf))
                {
                        int y = py, x = px;

                        scatter(&y, &x, py, px, 5, 0);

                        /* Create the monster */
                        if(place_monster_aux(y, x, i, FALSE, FALSE, FALSE, 0))
                                msg_print("Your wish become truth!");

                        /* Don't search any more */
                        return;
                }
        }       
}

/* Know what kind of monster you are... */
void know_body_monster()
{
        int query;

        /* Save */
        Term_save();

        /* Recall on screen */
        screen_roff(p_ptr->body_monster, 0);

        /* Hack -- Complete the prompt (again) */
        Term_addstr(-1, TERM_WHITE, format("  [r,%s]", ""));
					
        /* Command */
        query = inkey();

        /* Restore */
        Term_load();
}        
        
/*
 * Gain experience for a kill
 */
void gain_exp_kill(monster_type *m_ptr)
{
	call_lua("gain_exp_kills", "(M)", "", m_ptr);
}

/* New functions to check stats boosts! */
/* Before calling those, the value of the */
/* bonus must be calculated! */
bool set_str_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->str_boost_dur)
		{
                        msg_print("You become much stronger!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->str_boost_dur)
		{
                        msg_print("Your strength returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->str_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_int_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->int_boost_dur)
		{
                        msg_print("You become much smarter!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->int_boost_dur)
		{
                        msg_print("Your intelligence returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->int_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_wis_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->wis_boost_dur)
		{
                        msg_print("You become much wiser!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->wis_boost_dur)
		{
                        msg_print("Your wisdom returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->wis_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_dex_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->dex_boost_dur)
		{
                        msg_print("You become much more dextrous!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->dex_boost_dur)
		{
                        msg_print("Your dexterity returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->dex_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_con_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->con_boost_dur)
		{
                        msg_print("You become much tougher!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->con_boost_dur)
		{
                        msg_print("Your constitution returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->con_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_chr_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->chr_boost_dur)
		{
                        msg_print("You become much more attractive!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->chr_boost_dur)
		{
                        msg_print("Your charisma returned to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->chr_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_pres(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->pres_dur)
		{
                        msg_print("You become resistant to melee attacks!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->pres_dur)
		{
                        msg_print("You no longer resist melee attacks.");
                        p_ptr->pres = 0;
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->pres_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_mres(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->mres_dur)
		{
                        msg_print("You become resistant to magic attacks!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->mres_dur)
		{
                        msg_print("You no longer resist magic attacks.");
                        p_ptr->mres = 0;
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->mres_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

bool set_ac_boost(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->ac_boost_dur)
		{
                        msg_print("You gain a bonus to AC!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->ac_boost_dur)
		{
                        msg_print("Your AC returns to normal.");
                        p_ptr->ac_boost = 0;
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->ac_boost_dur = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

void add_class_kill(monster_type *m_ptr)
{
        /* monster_race *r_ptr = &r_info[m_ptr->r_idx]; */

        /* First, check the monster's depth. Must be at least */
        /* 33% your experience level. */
        /* Disabled for the moment. */
        /* if (r_ptr->level < (p_ptr->lev / 3)) return; */

        /* Then, check the monster's level. Must be equal or higher */
        /* than yours! */
        if (m_ptr->level < p_ptr->lev) return;

        /* And of course, no friends killing! */
        if (is_pet(m_ptr) || m_ptr->angered_pet == 1) return;

        /* Seems strong enough... add it to kills! :) */
        p_ptr->class_kills[p_ptr->pclass] += 1;

        /* Stop counting... */
        if (p_ptr->class_kills[p_ptr->pclass] > 1000) p_ptr->class_kills[p_ptr->pclass] = 1000;

        /* Gain a class level? */
        gain_class_level();
}

void gain_class_level()
{
        switch (p_ptr->class_level[p_ptr->pclass])
        {
                case 1:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 15)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 2:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 30)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 3:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 60)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 4:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 100)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 5:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 150)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 6:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 200)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 7:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 300)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 8:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 400)
                        {
                                msg_print("You gained a class level!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;
                case 9:
                        if (p_ptr->class_kills[p_ptr->pclass] >= 500)
                        {
                                msg_print("You gained a class level!");
                                msg_print("You are now a master of this class!");
                                p_ptr->class_level[p_ptr->pclass] += 1;
                        }
                        break;

        }
}

bool set_elem_shield(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->elem_shield)
		{
                        msg_format("You surround yourself in %s!", get_element_name(p_ptr->elemlord));
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->elem_shield)
		{
                        msg_format("Your %s shield fades away...", get_element_name(p_ptr->elemlord));
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->elem_shield = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

/* New functions to check stats boosts! */
/* Before calling those, the value of the */
/* bonus must be calculated! */
bool set_powerattack(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
                if (!p_ptr->powerattack)
		{
                        msg_print("You start gathering power!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
                if (p_ptr->powerattack)
		{
			notice = TRUE;
		}
	}

	/* Use the value */
        p_ptr->powerattack = v;

	/* Nothing to notice */
	if (!notice) return (FALSE);

	/* Disturb */
	if (disturb_state) disturb(0, 0);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

        /* Update */
        update_and_handle();

	/* Result */
	return (TRUE);
}

void verify_panel_always_update(void)
{
	int y = py;
	int x = px;

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

	/* Hack -- optional disturb on "panel change" */
	if (disturb_panel) disturb(0, 0);

	/* Save the new panel info */
	panel_row = prow;
	panel_col = pcol;

	/* Recalculate the boundaries */
	panel_bounds();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}

bool get_rep_dir_repeat(int *dp)
{
	int dir;

#ifdef ALLOW_REPEAT /* TNB */

	if (repeat_pull(dp))
	{
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