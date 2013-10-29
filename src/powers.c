/* PosBand -- A variant of reposband roguelike
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

/* powers.c: monster race powers */

#include "reposband.h"
#include "powers_strings.h"
#include "powers_titles.h"
#include "powers_code.c"
#include "powers_info.c"

/*
 * Returns chance of failure for a power
 */
s16b power_chance(int power)
{
	int chance, minfail;

	/* Extract the base power failure rate */
	chance = rp_ptr->racial_powers[power].fail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - rp_ptr->racial_powers[power].level);

	/* Reduce failure rate by stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->state.stat_ind[rp_ptr->racial_power_stat]] - 1);

	/* Not enough mana to use */
	if (rp_ptr->racial_powers[power].sp > p_ptr->csp)
	{
		chance += 5 * (rp_ptr->racial_powers[power].sp - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[rp_ptr->racial_power_stat]];

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes powers harder (after minfail) */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

void print_powers(void)
{
    int i;
	char out_val[160];
    byte line_attr;
    cptr desc;
        
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Lv Mana Fail Info", 1, 55);
        
        for (i = 0; i < RACIAL_POWERS_MAX; i++)
                if (rp_ptr->racial_powers[i].level)
                {
                	/* Generic description; try themed if allowed */
					/* TODO: this -Simon */
					desc = pwr_desc[rp_ptr->racial_powers[i].type];
                /*	if (p_ptr->al_special == PAL_CHAOS)
                	{
                		for (int j = 0; racial_power_desc_chaos[j].name[0]; j++)
                			if (racial_power_desc_chaos[j].power == rp_ptr->racial_powers[i].type)
                				desc = racial_power_desc_chaos[j].name;
                	}
                */	
					prt("", i + 2, 20);
					line_attr = (p_ptr->lev >= rp_ptr->racial_powers[i].level) ? TERM_WHITE : TERM_L_DARK;
					strnfmt(out_val, sizeof(out_val), "  %c) %s", I2A(i), desc);
					c_prt(line_attr, out_val, i + 2, 20);
					strnfmt(out_val, sizeof(out_val), "%2d", rp_ptr->racial_powers[i].level);
					c_prt(line_attr, out_val, i + 2, 55);
					strnfmt(out_val, sizeof(out_val), "%4d", rp_ptr->racial_powers[i].sp);
					c_prt(line_attr, out_val, i + 2, 58);
					strnfmt(out_val, sizeof(out_val), "%2d%%", power_chance(i));
					c_prt(line_attr, out_val, i + 2, 64);
					c_prt(line_attr, power_info(i), i + 2, 68);
                } else break;
        prt("", i + 2, 20);
		return;
}

void do_cmd_monster_power(void)
{
	char out_val[160];
	int i, num = 0, chance;
	bool flag = FALSE, redraw = FALSE;
	char choice;
	//char res = -1;

	for (i = 0; i < RACIAL_POWERS_MAX; i++)
			if (rp_ptr->racial_powers[i].type)
					num++;

	/* Require at least one power */
	if (!num)
	{
			msg_print("You do not have any special powers.");
			return;
	}

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Must not be confused */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	/* Build a prompt */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
	        I2A(0), I2A(num - 1));
			
	/* Get a power from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of powers */
				print_powers();
			}

			/* Ask again */
			continue;
		}

		/* Lowercase */
		choice = tolower((unsigned char)choice);

		/* Extract request */
		i = (islower((unsigned char)choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Illegal power choice!");
			continue;
		}

		/* Require "okay" powers */
		if (p_ptr->lev < rp_ptr->racial_powers[i].level)
		{
			bell("Illegal power choice!");
			msg_format("You cannot use this power yet.");
			continue;
		}

		//repeat_push(i);

		//rep_racial_power:

		/* Stop the loop */
		flag = TRUE;

		/* Verify "dangerous" powers */
		if (rp_ptr->racial_powers[i].sp > p_ptr->csp)
		{
			/* Warning */
			msg_print("You do not have enough mana to use this power.");

			/* Flush input */
			flush();

			/* Verify */
			if (!get_check("Attempt it anyway? ")) continue;
		}

		/* Restore the screen */
		if (redraw)
		{
					redraw = FALSE;
			/* Load screen */
			screen_load();
		}

		/* Power failure chance */
		chance = power_chance(i);

		/* Check for failure */
		if (randint0(100) < chance)
		{
			//if (flush_failure) flush();
			msg_print("You failed to concentrate hard enough!");
		}

		/* Success */
		else
		{
			/* Use the power */
			if (!use_power(rp_ptr->racial_powers[i].type)) return;
		}

		/* Take a turn */
		if (!p_ptr->energy_use) p_ptr->energy_use = 100;

		/* Sufficient mana */
		if (rp_ptr->racial_powers[i].sp <= p_ptr->csp)
		{
			/* Use some mana */
			p_ptr->csp -= rp_ptr->racial_powers[i].sp;
		}

		/* Over-exert the player */
		else
		{
			int oops = rp_ptr->racial_powers[i].sp - p_ptr->csp;

			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;

			/* Message */
			msg_print("You faint from the effort!");

			/* Hack -- Bypass free action */
			(void)inc_timed(TMD_PARALYZED, randint1(5 * oops + 1), TRUE);

			/* Damage CON (possibly permanently) */
			if (randint0(100) < 50)
			{
				bool perm = (randint0(100) < 25);

				/* Message */
				msg_print("You have damaged your health!");

				/* Reduce constitution */
				player_stat_dec(p_ptr, A_CON, perm);
			}
		}

	        /* Redraw mana */
	        p_ptr->redraw |= (PR_MANA);

	        /* Window stuff */
			/* TODO: figure out what this means, fix it */
	        //p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}
}