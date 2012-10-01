/* File: xtra3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Set "p_ptr->ts_anchor", notice observable changes
 */
bool set_tim_ts_anchor(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->ts_anchor)
		{
			msg_print("You temporarily stabilize local space!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->ts_anchor)
		{
			msg_print("Your time/space anchor ravels apart.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->ts_anchor = v;

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
 * Set "p_ptr->tim_pl_invis", notice observable changes
 */
bool set_tim_pl_invis(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_pl_invis)
		{
			msg_print("You can see through yourself!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_pl_invis)
		{
			msg_print("You become substantial again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_pl_invis = v;

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
 * Set "p_ptr->tim_ghostly", notice observable changes
 */
bool set_tim_ghostly(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_ghostly)
		{
			msg_print("You no longer feel solid");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_ghostly)
		{
			msg_print("You become solid again.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_ghostly = v;

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
 * Set "p_ptr->tim_levitate", notice observable changes
 */
bool set_tim_levitate(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_levitate)
		{
			msg_print("You feel lighter than air!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_levitate)
		{
			msg_print("Your weight returns to normal.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_levitate = v;

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
 * Set "p_ptr->tim_sus_str", notice observable changes
 */
bool set_tim_sus_str(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_str)
		{
			msg_print("Your strength is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_str)
		{
			msg_print("Your strength is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_str = v;

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
 * Set "p_ptr->tim_sus_int", notice observable changes
 */
bool set_tim_sus_int(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_int)
		{
			msg_print("Your intelligence is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_int)
		{
			msg_print("Your intelligence is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_int = v;

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
 * Set "p_ptr->tim_sus_wis", notice observable changes
 */
bool set_tim_sus_wis(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_wis)
		{
			msg_print("Your wisdom is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_wis)
		{
			msg_print("Your wisdom is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_wis = v;

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
 * Set "p_ptr->tim_sus_dex", notice observable changes
 */
bool set_tim_sus_dex(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_dex)
		{
			msg_print("Your dexterity is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_dex)
		{
			msg_print("Your dexterity is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_dex = v;

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
 * Set "p_ptr->tim_sus_con", notice observable changes
 */
bool set_tim_sus_con(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_con)
		{
			msg_print("Your constitution is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_con)
		{
			msg_print("Your constitution is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_con = v;

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
 * Set "p_ptr->tim_sus_chr", notice observable changes
 */
bool set_tim_sus_chr(int v)
{
	bool notice = FALSE;


	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->tim_sus_chr)
		{
			msg_print("Your charisma is temporarily sustained!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->tim_sus_chr)
		{
			msg_print("Your charisma is no longer protected.!");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->tim_sus_chr = v;

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
* Set "p_ptr->oppose_ld", notice observable changes
*/
bool set_oppose_ld(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_ld)
		{
			msg_print("You feel resistant to light & dark!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_ld)
		{
			msg_print("You feel less resistant to light & dark.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_ld = v;

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
 * Set "p_ptr->oppose_cc", notice observable changes
 */
bool set_oppose_cc(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_cc)
		{
			msg_print("You feel resistant to chaos & confusion!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_cc)
		{
			msg_print("You feel less resistant to chaos & confusion.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_cc = v;

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
 * Set "p_ptr->oppose_ss", notice observable changes
 */
bool set_oppose_ss(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_ss)
		{
			msg_print("You feel resistant to sound & shards!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_ss)
		{
			msg_print("You feel less resistant to sound & shards.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_ss = v;

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
 * Set "p_ptr->oppose_nexus", notice observable changes
 */
bool set_oppose_nexus(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_nexus)
		{
			msg_print("You feel resistant to nexus!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_nexus)
		{
			msg_print("You feel less resistant to nexus.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_nexus = v;

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
 * Set "p_ptr->oppose_nethr", notice observable changes
 */
bool set_oppose_nethr(int v)
{
	bool notice = FALSE;

	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

	/* Open */
	if (v)
	{
		if (!p_ptr->oppose_nethr)
		{
			msg_print("You feel resistant to life-draining forces!");
			notice = TRUE;
		}
	}

	/* Shut */
	else
	{
		if (p_ptr->oppose_nethr)
		{
			msg_print("You feel less resistant to life-draining forces.");
			notice = TRUE;
		}
	}

	/* Use the value */
	p_ptr->oppose_nethr = v;

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
 * From Psionic Angband by Aram Harrow
 */
bool tgt_pt(int *x,int *y)
{
	char ch = 0;
	int d,cu,cv;
	bool success = FALSE;

	*x = p_ptr->px;
	*y = p_ptr->py;

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
				d = target_dir(ch);
				if (!d) break;
				*x += ddx[d];
				*y += ddy[d];

				/* Hack -- Verify x */
				if ((*x>=DUNGEON_WID-1) || (*x>=p_ptr->wx + SCREEN_WID)) (*x)--;
				else if ((*x<=0) || (*x<=p_ptr->wx)) (*x)++;

				/* Hack -- Verify y */
				if ((*y>=DUNGEON_HGT-1) || (*y>=p_ptr->wy + SCREEN_HGT)) (*y)--;
				else if ((*y<=0) || (*y<=p_ptr->wy)) (*y)++;

				break;
			}
		}
	}

	Term->scr->cu = cu;
	Term->scr->cv = cv;
	Term_fresh();
	return success;

}


