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

/* powers.c: monster race powers */

#include "posband.h"

/* powers_defines.inc must be generated from powers.m4 via GNU M4 */
#include "powers_defines.inc"

/* powers_info.inc must be generated from powers.m4 via GNU M4 */
#include "powers_info.inc"

/*
 * Returns chance of failure for a power
 */
s16b power_chance(int power)
{
	int chance, minfail;

	/* Extract the base power failure rate */
	chance = r_info[p_ptr->m_r_idx].body.pwr[power].fail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - r_info[p_ptr->m_r_idx].body.pwr[power].level);

	/* Reduce failure rate by stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[r_info[p_ptr->m_r_idx].body.pwrstat]] - 1);

	/* Not enough mana to use */
	if (r_info[p_ptr->m_r_idx].body.pwr[power].sp > p_ptr->csp)
	{
		chance += 5 * (r_info[p_ptr->m_r_idx].body.pwr[power].sp - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[r_info[p_ptr->m_r_idx].body.pwrstat]];

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes powers harder (after minfail) */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/* Item tester -- non-ID'ed, non-pseudo-ID'ed items */
static bool item_tester_nonpseudoid(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		
			if (o_ptr->discount > 0 && o_ptr->discount != INSCRIP_INDESTRUCTIBLE) return (FALSE);
			if (o_ptr->ident & IDENT_SENSE) return (FALSE);
			if (object_known_p(o_ptr)) return (FALSE);
			return (TRUE);
	}
	
	return (FALSE);
}

/* Item tester -- "bones" item */
static bool item_tester_bones(const object_type *o_ptr)
{
	char o_name[80];
	if (o_ptr->tval != TV_CORPSE) return (FALSE);
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
	return (strstr(o_name, "Bones") || strstr(o_name, "Skeleton"));
}

/* Powers */
/* powers_code.inc must be generated from powers.m4 via GNU M4 */
#include "powers_code.inc"

void print_powers(void)
{
        int i, j;
	char out_val[160];
        byte line_attr;
        cptr desc;
        
	prt("", 1, 20);
	put_str("Name", 1, 25);
	put_str("Lv Mana Fail Info", 1, 55);
        
        for (i = 0; i < PWR_BODY_MAX; i++)
                if (r_info[p_ptr->m_r_idx].body.pwr[i].type)
                {
                	/* Generic description; try themed if allowed */
                	desc = pwr_desc[r_info[p_ptr->m_r_idx].body.pwr[i].type];
                	if (p_ptr->al_special == PAL_CHAOS)
                	{
                		for (j = 0; pwr_desc_chaos[j].name[0]; j++)
                			if (pwr_desc_chaos[j].power == r_info[p_ptr->m_r_idx].body.pwr[i].type)
                				desc = pwr_desc_chaos[j].name;
                	}
                	
                        prt("", i + 2, 20);
                        line_attr = (p_ptr->lev >= r_info[p_ptr->m_r_idx].body.pwr[i].level) ? TERM_WHITE : TERM_L_DARK;
                        strnfmt(out_val, sizeof(out_val), "  %c) %s", I2A(i), desc);
                        c_prt(line_attr, out_val, i + 2, 20);
                        strnfmt(out_val, sizeof(out_val), "%2d", r_info[p_ptr->m_r_idx].body.pwr[i].level);
                        c_prt(line_attr, out_val, i + 2, 55);
                        strnfmt(out_val, sizeof(out_val), "%4d", r_info[p_ptr->m_r_idx].body.pwr[i].sp);
                        c_prt(line_attr, out_val, i + 2, 58);
                        strnfmt(out_val, sizeof(out_val), "%2d%%", power_chance(i));
                        c_prt(line_attr, out_val, i + 2, 64);
                        c_prt(line_attr, power_info(i), i + 2, 68);
                } else break;
        prt("", i + 2, 20);
}

void do_cmd_monster_power(void)
{
	char out_val[160];
        int i, num = 0, chance;
	bool flag = FALSE, redraw = FALSE;
	char choice;
        int result = -1;

        /* Require monster race */
        if (!p_ptr->m_r_idx)
        {
                msg_print("You do not have any special powers.");
                return;
        }
        
        for (i = 0; i < PWR_BODY_MAX; i++)
                if (r_info[p_ptr->m_r_idx].body.pwr[i].type)
                        num++;

        /* Require at least one power */
        if (!num)
        {
                msg_print("You do not have any special powers.");
                return;
        }

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Get the spell, if available */
	if (repeat_pull(&result))
	{
		/* Verify the spell */
                
		/* Totally Illegal */
		if ((result < 0) || (result >= num))
		{
                        result = -1;
                        repeat_clear();
		}

		/* Require "okay" powers */
		else if (p_ptr->lev < r_info[p_ptr->m_r_idx].body.pwr[result].level)
		{
                        result = -1;
                        repeat_clear();
		}

	}

	/* Must not be confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Build a prompt */
	strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
	        I2A(0), I2A(num - 1));

        /* Hack - allow repeat */
        if (result != -1)
        {
                i = result;
                goto rep_pwr;
        }

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
		if (p_ptr->lev < r_info[p_ptr->m_r_idx].body.pwr[i].level)
		{
			bell("Illegal power choice!");
			msg_format("You cannot use this power yet.");
			continue;
		}

	        repeat_push(i);

rep_pwr:

		/* Stop the loop */
		flag = TRUE;

	        /* Verify "dangerous" powers */
	        if (r_info[p_ptr->m_r_idx].body.pwr[i].sp > p_ptr->csp)
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
	        if (rand_int(100) < chance)
	        {
		        if (flush_failure) flush();
		        msg_print("You failed to concentrate hard enough!");
	        }

	        /* Success */
	        else
	        {
		        /* Use the power */
		        if (!use_power(r_info[p_ptr->m_r_idx].body.pwr[i].type)) return;
		}

	        /* Take a turn */
	        if (!p_ptr->energy_use) p_ptr->energy_use = 100;

	        /* Sufficient mana */
	        if (r_info[p_ptr->m_r_idx].body.pwr[i].sp <= p_ptr->csp)
	        {
		        /* Use some mana */
		        p_ptr->csp -= r_info[p_ptr->m_r_idx].body.pwr[i].sp;
	        }

	        /* Over-exert the player */
	        else
	        {
		        int oops = r_info[p_ptr->m_r_idx].body.pwr[i].sp - p_ptr->csp;

		        /* No mana left */
		        p_ptr->csp = 0;
		        p_ptr->csp_frac = 0;

		        /* Message */
		        msg_print("You faint from the effort!");

		        /* Hack -- Bypass free action */
		        (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		        /* Damage CON (possibly permanently) */
		        if (rand_int(100) < 50)
		        {
			        bool perm = (rand_int(100) < 25);

			        /* Message */
			        msg_print("You have damaged your health!");

			        /* Reduce constitution */
			        (void)dec_stat(A_CON, 15 + randint(10), perm);
		        }
	        }

	        /* Redraw mana */
	        p_ptr->redraw |= (PR_MANA);

	        /* Window stuff */
	        p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}
}
