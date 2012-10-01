/* File: spells2.c */

/* Healing spells, glyphs of warding, reducing or sustaining a stat, ID 
 * everything, chance for enchant spells to fail, remove curses, regain 
 * exp, detection spells, create stairs.  Definitions of armour & weapons, 
 * enchantment, branding, temporary branding, cursing, and ID code, what 
 * items are rechargable and the recharging code.  Various special object 
 * spells.  Spells that effect an area or LOS, lighten & darken rooms and 
 * areas, casting ball, projection, beam, and bolt spells.  Some miscel-
 * lanious non-damage spell functions.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Increase players hit points, notice effects
 */
bool hp_player(int num)
{
	/* Healing needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
      
		/* Heal 0-4 */
		if (num < 5)
		{
			msg_print("You feel a little better.");
		}
      
		/* Heal 5-14 */
		else if (num < 15)
		{
			msg_print("You feel better.");
		}
      
		/* Heal 15-34 */
		else if (num < 35)
		{
			msg_print("You feel much better.");
		}
      
		/* Heal 35+ */
		else
		{
			msg_print("You feel very good.");
		}
      
		/* Notice */
		return (TRUE);
	}
  
	/* Ignore */
	return (FALSE);
}

/*
 * Jam a closed door with a magical spike.  
 * Code is taken from do_cmd_spike. -LM-
 */
void magic_spiking(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int y, x, i, dir;
  
  
	/* Get a direction (or abort) */
	if (!get_rep_dir(&dir)) return;
  
	/* Get location */
	y = py + ddy[dir];
	x = px + ddx[dir];
  
  
	/* Verify legality */
	if (!do_cmd_spike_test(y, x)) return;
  
	/* Monster */
	if (cave_m_idx[y][x] > 0)
	{
		/* Message */
		msg_print("There is a monster in the way!");
      
		/* Attack */
		py_attack(y, x);
	}
  
	/* Go for it */
	else
	{
		/* Verify legality */
		if (!do_cmd_spike_test(y, x)) return;
      
		/* Successful jamming */
		msg_print("You magically jam the door.");
      
		/* Convert "locked" to "stuck" XXX XXX XXX */
		if (cave_feat[y][x] < FEAT_DOOR_HEAD + 0x08)
		{
			cave_feat[y][x] += 0x08;
		}
      
		/* Add three magical spikes to the door. */
		for (i = 0; i < 3; i++)
		{
			if (cave_feat[y][x] < FEAT_DOOR_TAIL)
			{
				cave_feat[y][x] += 0x01;
			}
		}
	}
}



/*
 * Leave a "glyph of warding" which prevents monster movement.  Glyphs of 
 * warding are now rationed because priests are otherwise too easy to win 
 * with. -LM-
 */
bool warding_glyph(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	/* XXX XXX XXX */
	if (!cave_clean_bold(py, px))
	{
		msg_print("The object resists the spell.");
		return (FALSE);
	}
  
	/* Limit total number of glyphs. -LM- */
	if (num_glyph_on_level >= 4)
	{
		msg_print("You cannot set any more glyphs until you desanctify your existing ones.");
		return (FALSE);
	}
  
	/* Create a glyph */
	cave_set_feat(py, px, FEAT_GLYPH);
  
	/* Increment the glyph count. */
	num_glyph_on_level++;
  
	/* Warning. */
	if (num_glyph_on_level == 4)
		msg_print("You have now reached your glyph limit.  In order to set more, desanctify some.");
  
	return (TRUE);
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
	bool clarity = (check_ability(SP_CLARITY));
	bool athletics = (check_ability(SP_ATHLETICS));
  
	/* Access the "sustain" and specialty skills */
	switch (stat)
	{
	case A_STR: if (p_ptr->sustain_str)
			sust = TRUE; break;
	case A_INT: if ((p_ptr->sustain_int) || 
			(clarity && (rand_int(2) != 0)))
			sust = TRUE; break;
	case A_WIS: if ((p_ptr->sustain_wis) || 
			(clarity && (rand_int(2) != 0)))
			sust = TRUE; break;
	case A_DEX: if ((p_ptr->sustain_dex) || 
			(athletics && (rand_int(2) != 0)))
			sust = TRUE; break;
	case A_CON: if ((p_ptr->sustain_con) || 
			(athletics && (rand_int(2) != 0)))
			sust = TRUE; break;
	case A_CHR: if (p_ptr->sustain_chr)
			sust = TRUE; break;}
  
  
	/* Sustain */
	if (sust)
	{
		/* Message */
		msg_format("You feel very %s for a moment, but the feeling passes.",
			   desc_stat_neg[stat]);
      
		/* Notice effect */
		return (TRUE);
	}
  
	/* Attempt to reduce the stat */
	if (dec_stat(stat, 10, FALSE))
	{
		/* Message */
		msg_format("You feel very %s.", desc_stat_neg[stat]);
      
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
		msg_format("You feel less %s.", desc_stat_neg[stat]);
      
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
                /* Message */
                msg_format("You feel very %s!", desc_stat_pos[stat]);
      
		/* Notice */
		return (TRUE);
	}
  
	/* Restoration worked */
	if (res)
	{
		/* Message */
		msg_format("You feel less %s.", desc_stat_neg[stat]);
      
		/* Notice */
		return (TRUE);
	}
  
	/* Nothing obvious */
	return (FALSE);
}



/*
 * Identify everything being carried.
 * Done by a potion of "self knowledge".
 */
void identify_pack(void)
{
	int i;
  
	/* Simply identify and know every item */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];
      
		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
      
		/* Aware and Known */
		object_aware(o_ptr);
		object_known(o_ptr);
	}
  
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
  
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
  
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
}






/*
 * Used by the "enchant" function (chance of failure)
 */
static int enchant_table[16] =
{
	0, 10, 50, 100, 200,
	300, 400, 500, 700, 950,
	990, 992, 995, 997, 999,
	1000
};


/*
 * Hack -- Removes curse from an object.
 */
static void uncurse_object(object_type *o_ptr)
{
	/* Uncurse it */
	o_ptr->ident &= ~(IDENT_CURSED);
  
	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);
  
	o_ptr->feel = FEEL_UNCURSED;
}


/*
 * Removes curses from items in inventory
 *
 * Note that Items which are "Perma-Cursed" (The One Ring,
 * The Crown of Morgoth) can NEVER be uncursed.
 *
 * Note that if "all" is FALSE, then Items which are
 * "Heavy-Cursed" (Mormegil, Calris, and Weapons of Morgul)
 * will not be uncursed.
 */
static int remove_curse_aux(int all)
{
	int i, cnt = 0;
  
	/* Attempt to uncurse items being worn */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b f1, f2, f3;
      
		object_type *o_ptr = &inventory[i];
      
		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
      
		/* Uncursed already */
		if (!cursed_p(o_ptr)) continue;
      
		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);
      
		/* Heavily Cursed Items need a special spell */
		if (!all && (f3 & (TR3_HEAVY_CURSE))) continue;
      
		/* Perma-Cursed Items can NEVER be uncursed */
		if (f3 & (TR3_PERMA_CURSE)) continue;
      
		/* Uncurse the object */
		uncurse_object(o_ptr);
      
		/* Recalculate the bonuses */
		p_ptr->update |= (PU_BONUS);
      
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
      
		/* Count the uncursings */
		cnt++;
	}
  
	/* Return "something uncursed" */
	return (cnt);
}


/*
 * Remove most curses
 */
bool remove_curse(void)
{
	return (remove_curse_aux(FALSE));
}

/*
 * Remove all curses
 */
bool remove_all_curse(void)
{
	return (remove_curse_aux(TRUE));
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
		msg_print("You feel your life energies returning.");
      
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


void do_cmd_bear_shape(void)
{
	/* Sanity */
	if ((SCHANGE) || (!check_ability(SP_BEARSKIN))) return;
  
	/* Confirm */
	if (!get_check("Assume the form of a bear? ")) return;
  
	/* Change */
	shapechange(SHAPE_BEAR);
  
	/* Use some energy */
	p_ptr->energy_use = 100;
}


/*
 * Stop doing a shapechange.  From Sangband.
 */
void do_cmd_unchange(void)
{
	if (!SCHANGE)
	{
		msg_print("You aren't in another form right now.");
		return;
	}
  
	/* Confirm */
	if (!get_check("Really return to normal? "))
		return;
  
	/* Return to normal form */
	shapechange(SHAPE_NORMAL);
  
	/* Hack - refund mana (2/3 mana when shapeshifted). */
	if (p_ptr->csp > 0)
	{
		/* Hack - Recalculate mana now, even though we will */
		/* update it fully, to ensure the refund to current */
		/* mana doesn't get cleared */
                p_ptr->msp *= 3;
                p_ptr->msp /= 2;

		/* Refund current mana - removed, as mana is not reduced in the other
		 * shapechange
		 p_ptr->csp *= 3;
		 p_ptr->csp /= 2;
		 if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp; */

                /* Display mana later */
                p_ptr->redraw |= (PR_MANA);
	}
  
	/* Recalculate mana. */
	p_ptr->update |= (PU_MANA);

	/* Show or hide shapechange on main window. */
	p_ptr->redraw |= (PR_SHAPE);

	/* Use some energy */
	p_ptr->energy_use = 100;
}


/*
 * Forget everything
 */
bool lose_all_info(void)
{
        int i;

        /* Forget info about objects */
        for (i = 0; i < INVEN_TOTAL; i++)
        {
                object_type *o_ptr = &inventory[i];

                /* Skip non-objects */
                if (!o_ptr->k_idx) continue;

                /* Allow "protection" by the MENTAL flag */
                if (o_ptr->ident & (IDENT_MENTAL)) continue;

                /* Forget the feeling */
                o_ptr->feel = FEEL_NONE;

                /* Hack -- Clear the "empty" flag */
                o_ptr->ident &= ~(IDENT_EMPTY);

                /* Hack -- Clear the "known" flag */
                o_ptr->ident &= ~(IDENT_KNOWN);

                /* Hack -- Clear the "felt" flag */
                o_ptr->ident &= ~(IDENT_SENSE);
        }

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

        /* Mega-Hack -- Forget the map */
        wiz_dark();
  
	/* It worked */
	return (TRUE);
}


/*
 * Set "p_ptr->word_recall", notice observable changes
 */
bool set_recall(int v)
{
	bool notice = FALSE;
  
	/* Hack -- Force good values */
	v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;
  
	if (v)
	{
		if (!p_ptr->word_recall)
		{
			msg_print("The air about you becomes charged...");
			notice = TRUE;
		}
	}
  
	/* Shut */
	else
	{
		if (p_ptr->word_recall)
		{
			msg_print("A tension leaves the air around you...");
			notice = TRUE;
		}
	}
  
	/* Use the value */
	p_ptr->word_recall = v;
  
	/* Nothing to notice */
	if (!notice) return (FALSE);
  
	/* Disturb */
	if (disturb_state) disturb(0, 0);
  
	/* Redraw status */
	p_ptr->redraw |= PR_STATUS;
  
	/* Handle stuff */
	handle_stuff();
  
	/* Result */
	return (TRUE);
}


/*
 * Hack - displays areas effected by detection spells.
 *
 */
static void animate_detect(int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int x, y;

	byte a, c;
  
	int msec = op_ptr->delay_factor * op_ptr->delay_factor;
  
	/* Exit if not desired */
	if (!show_detect) return;
  
	/* Hack - Needs to last a bit longer to be visible */
	msec *= 6;
  
	/* Scan the maximal area of detection */
	for (y = py - rad; y <= py + rad; y++)
	{
		for (x = px - rad; x <= px + rad; x++)
		{
	  
			/* Ignore "illegal" locations */
			if (!in_bounds(y, x)) continue;
	  
			/* Enforce a "circular" area */
			if (distance(py, px, y, x) > rad) continue;
	  
                        /* Only show the region that the player can see */
                        if (panel_contains(y, x))
                        {
				/* Hack - Obtain attr/char */
				a = misc_to_attr[0x3B];
				c = misc_to_char[0x3B];

                                /* Hack -- Visual effects -- Display a yellow star */
				print_rel(c, a, y, x);
                        }
                }
        }
  
	/* Flush the image of detected region */
	if (fresh_before) Term_fresh();
  
	/* Delay (efficiently) */
	Term_xtra(TERM_XTRA_DELAY, msec);
  
	/* Now erase the effect */
	for (y = py - rad; y <= py + rad; y++)
	{
		for (x = px - rad; x <= px + rad; x++)
		{
			/* Ignore "illegal" locations */
			if (!in_bounds(y, x)) continue;
	  
			/* Enforce a "circular" area */
			if (distance(py, px, y, x) > rad) continue;
	  
			/* Hack -- Erase only if needed */
			if (panel_contains(y, x))
			{
				lite_spot(y, x);
			}
		}
	}
  
	/* Hack -- center the cursor */
	move_cursor_relative(py, px);
  
	/* Flush screen back to normal */
	if (fresh_before) Term_fresh();
  
	/* Exit */
	return;
	
}

/*
 * Detect all traps within range 
 */
bool detect_traps(int range, bool show)
{
        int y, x;

        bool detect = FALSE;

        int py = p_ptr->py;
        int px = p_ptr->px;

        int num=0;

        /* Hack - flash the effected region on the current panel */
        if (show) animate_detect(range);

	/* Scan the map */
	for (y = 0; y < DUNGEON_HGT; y++)
        {
                for (x = 0; x<DUNGEON_WID; x++)
                {

                        /* check range */
                        if (distance(py, px, y, x) <= range)
                        {
                                /* Detect invisible traps */
                                if (cave_feat[y][x] == FEAT_INVIS)
                                {
                                        /* Pick a trap */
                                        pick_trap(y, x);
                                }

                                /* Detect traps */
                                if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
                                    (cave_feat[y][x] <= FEAT_TRAP_TAIL))
                                {
                                        /* Hack -- Memorize */
                                        cave_info[y][x] |= (CAVE_MARK);
		  
					/* Redraw */
					lite_spot(y, x);
		  
                                        /* increment number found */
                                        num++;
                                }
              
				/* Mark grid as detected */
				cave_info2[y][x] |= (CAVE2_DTRAP);
                        }
                }
        }

        /* Found some */
        if (num > 0)
        {

                /* Obvious */
                detect = TRUE;

                /* Print success message */
                msg_print("You detect traps.");

        }

        /* Redraw DTrap Status */
	p_ptr->redraw |= (PR_DTRAP);
  
	/* Result - trap detection items are easy to recognize for now -BR- */
	return (TRUE);
}



/*
 * Detect all doors within range 
 */
bool detect_doors(int range, bool show)
{
	int y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool detect = FALSE;

        int num=0;

        /* Hack - flash the effected region on the current panel */\
        if (show) animate_detect(range);

        /* Scan the map */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
	  
			/* check range */
			if (distance(py, px, y, x) <= range)
			{
				/* Detect secret doors */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Pick a door */
                                        place_closed_door(y, x);
                                }

                                /* Detect doors */
                                if (((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
                                     (cave_feat[y][x] <= FEAT_DOOR_TAIL)) ||
                                    ((cave_feat[y][x] == FEAT_OPEN) ||
                                     (cave_feat[y][x] == FEAT_BROKEN)))
                                {
                                        /* Hack -- Memorize */
                                        cave_info[y][x] |= (CAVE_MARK);
		  
					/* Redraw */
					lite_spot(y, x);
		  
					/* increment number found */
					num++;
				}
			}
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
      
		/* Print success message */
		msg_print("You detect doors.");
	}
  
	/* Result */
	return (detect);
}



/*
 * Detect all stairs within range 
 */
bool detect_stairs(int range, bool show)
{
	int y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int num=0;
  
	bool detect = FALSE;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan the map */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
	  
			/* check range */
			if (distance(py, px, y, x) <= range)
			{
				/* Detect stairs */
				if ((cave_feat[y][x] == FEAT_LESS) ||
				    (cave_feat[y][x] == FEAT_MORE))
				{
					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
		  
					/* Redraw */
					lite_spot(y, x);
		  
					/* increment number found */
					num++;
				}
			}
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
      
		/* Print success message */
		msg_print("You detect stairs.");
	}
  
	/* Result */
	return (detect);
}


/*
 * Detect any treasure within range
 */
bool detect_treasure(int range, bool show)
{
	int y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool detect = FALSE;
  
	int num=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan the map */
	for (y = 0; y<DUNGEON_HGT; y++)
	{
		for (x = 0; x<DUNGEON_WID; x++)
		{
	  
			/* check range */
			if (distance(py, px, y, x) <= range)
			{
				/* Notice embedded gold */
				if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
				    (cave_feat[y][x] == FEAT_QUARTZ_H))
				{
					/* Expose the gold */
					cave_feat[y][x] += 0x02;
				}
	      
				/* Magma/Quartz + Known Gold */
				if ((cave_feat[y][x] == FEAT_MAGMA_K) || 
				    (cave_feat[y][x] == FEAT_QUARTZ_K))
				{
					/* Hack -- Memorize */
					cave_info[y][x] |= (CAVE_MARK);
		  
					/* Redraw */
					lite_spot(y, x);
		  
					/* increment number found */
					num++;
				}
			}
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
      
		/* Print success message */
		msg_print("You detect buried treasure.");
      
	}
  
	/* Result */
	return (detect);
}



/*
 * Detect all "gold" objects within range
 */
bool detect_objects_gold(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int num=0;
  
	bool detect = FALSE;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];
      
		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;
      
		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;
      
		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;
	  
			/* Redraw */
			lite_spot(y, x);
	  
			/* increment number found */
			num++;
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
		msg_print("You detect treasure.");
      
	}
  
	/* Result */
	return (detect);
  
}


/*
 * Detect all "normal" objects within range
 */
bool detect_objects_normal(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int num=0;
  
	bool detect = FALSE;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];
      
		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;
      
		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;
      
		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked = TRUE;
	  
			/* Redraw */
			lite_spot(y, x);
          
			/* increment number found */
			if (!squelch_hide_item(o_ptr))
				num++;
          
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
      
		/* Print success message */
		msg_print("You detect objects.");
      
	}
  
	/* Result */
	return (detect);
}


/*
 * Detect all "magic" objects within range.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(int range, bool show)
{
	int i, y, x, tv;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool detect = FALSE;
  
	int num=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];
      
		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;
      
		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;
      
		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Examine the tval */
		tv = o_ptr->tval;
      
		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
		    (tv == TV_AMULET) || (tv == TV_RING) ||
		    (tv == TV_STAFF) || (tv == TV_WAND) || (tv == TV_ROD) ||
		    (tv == TV_SCROLL) || (tv == TV_POTION) ||
		    (tv == TV_MAGIC_BOOK) || (tv == TV_PRAYER_BOOK) ||
		    (tv == TV_DRUID_BOOK) || (tv == TV_NECRO_BOOK) ||
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked = TRUE;
	  
			/* Redraw */
			lite_spot(y, x);
          
			/* increment number found */
			if (!squelch_hide_item(o_ptr))
				num++;
          
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		detect = TRUE;
      
		/* Print success message */
		msg_print("You detect magic objects.");
      
	}
  
	/* Return result */
	return (detect);
}


/*
 * Detect all "normal" monsters within range
 */
bool detect_monsters_normal(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool flag = FALSE;
  
	int num=0;
	int num_off=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Detect all non-invisible monsters */
		if (!(r_ptr->flags2 & (RF2_INVISIBLE)))
		{
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;
	  
			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
	  
			/* Update the monster */
			update_mon(i, FALSE);
	  
			/* increment number found */
			num++;
	  
			/* increment number found offscreen */
			if (!panel_contains(y, x)) num_off++;
	  
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
		flag = TRUE;
      
		/* Print success message */
		if (num_off > 0) msg_format("You detect monsters (%i offscreen).",
					    num_off);
		else msg_print("You detect monsters.");
	}
  
	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters within range
 */
bool detect_monsters_invis(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool flag = FALSE;
  
	int num=0;
	int num_off=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];
      
		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Detect invisible monsters */
		if (r_ptr->flags2 & (RF2_INVISIBLE))
		{
			/* Take note that they are invisible */
			l_ptr->flags2 |= (RF2_INVISIBLE);
	  
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}
	  
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;
	  
			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
	  
			/* Update the monster */
			update_mon(i, FALSE);
	  
			/* increment number found */
			num++;
	  
			/* increment number found offscreen */
			if (!panel_contains(y, x)) num_off++;
	  
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
                flag = TRUE;

                /* Print success message */
                if (num_off > 0) msg_format("You detect invisible creatures (%i offscreen).",
                                            num_off);
                else msg_print("You detect invisible creatures.");
        }

	/* Result */
	return (flag);
}



/*
 * Detect all "evil" monsters within range
 */
bool detect_monsters_evil(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool flag = FALSE;
  
	int num=0;
	int num_off=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];
      
		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Detect evil monsters */
		if (r_ptr->flags3 & (RF3_EVIL))
		{
			/* Take note that they are evil */
			l_ptr->flags3 |= (RF3_EVIL);
	  
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}
	  
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;
	  
			/* Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
	  
			/* Update the monster */
			update_mon(i, FALSE);
	  
			/* increment number found */
			num++;
	  
			/* increment number found offscreen */
			if (!panel_contains(y, x)) num_off++;
	  
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
                flag = TRUE;

                /* Print success message */
                if (num_off > 0) msg_format("You detect evil creatures (%i offscreen).",
                                            num_off);
                else msg_print("You detect evil creatures.");

        }
  
	/* Result */
	return (flag);
  
  
}


/*
 * Detect all "living" monsters within range.
 */
bool detect_monsters_living(int range, bool show)
{
	int i, y, x;
  
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	bool flag = FALSE;
  
	int num=0;
	int num_off=0;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/* check range */
		if (distance(py, px, y, x) > range) continue;
      
		/* Hack -- Detect all living monsters. */
		if ((!strchr("Egv", r_ptr->d_char)) && 
		    (!(r_ptr->flags3 & (RF3_UNDEAD))))
		{
			/* Optimize -- Repair flags */
			repair_mflag_mark = repair_mflag_show = TRUE;
	  
			/* Hack -- Detect the monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);
	  
			/* Update the monster */
			update_mon(i, FALSE);
	  
			/* increment number found */
			num++;
	  
			/* increment number found offscreen */
			if (!panel_contains(y, x)) num_off++;
		}
	}
  
	/* Found some */
	if (num > 0)
	{
      
		/* Obvious */
                flag = TRUE;

                /* Print success message */
                if (num_off > 0) msg_format("You detect living creatures (%i offscreen).",
					    num_off);
                else msg_print("You detect living creatures.");

        }

        /* Result */
        return (flag);
}



/*
 * Detect everything
 */
bool detect_all(int range, bool show)
{
	bool detect = FALSE;
  
	/* Hack - flash the effected region on the current panel */
	if (show) animate_detect(range);
  
	/* Detect everything */
	/* Do not 'show' the affected region for each
	 * detect individually 
	 */
	if (detect_traps(range, FALSE)) detect = TRUE;
	if (detect_doors(range, FALSE)) detect = TRUE;
	if (detect_stairs(range, FALSE)) detect = TRUE;
	if (detect_treasure(range, FALSE)) detect = TRUE;
	if (detect_objects_gold(range, FALSE)) detect = TRUE;
	if (detect_objects_normal(range, FALSE)) detect = TRUE;
	if (detect_monsters_invis(range, FALSE)) detect = TRUE;
	if (detect_monsters_normal(range, FALSE)) detect = TRUE;
  
	/* Result */
	return (detect);
}



/*
 * Create stairs at the player location
 */
void stair_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	/* XXX XXX XXX */
	if (!cave_valid_bold(py, px))
	{
		msg_print("The object resists the spell.");
                return;
        }

        /* XXX XXX XXX */
        delete_object(py, px);

        /* Create a staircase */
        if (!p_ptr->depth)
        {
                cave_set_feat(py, px, FEAT_MORE);
        }
        else if (is_quest(p_ptr->depth) || (p_ptr->depth >= MAX_DEPTH-1))
        {
                cave_set_feat(py, px, FEAT_LESS);
        }
	else if (rand_int(100) < 50)
	{
		cave_set_feat(py, px, FEAT_MORE);
	}
	else
	{
		cave_set_feat(py, px, FEAT_LESS);
	}
}




/*
 * Hook to specify "weapon"
 */
static bool item_tester_hook_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_SWORD:
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_DIGGING:
	case TV_BOW:
	case TV_BOLT:
	case TV_ARROW:
	case TV_SHOT:
	{
		return (TRUE);
	}
	}
  
	return (FALSE);
}


/*
 * Hook to specify "ammunition"
 */
static bool item_tester_hook_ammo(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_BOLT:
	case TV_ARROW:
	case TV_SHOT:
	{
		return (TRUE);
	}
	}
  
	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
static bool item_tester_hook_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_DRAG_ARMOR:
	case TV_HARD_ARMOR:
	case TV_SOFT_ARMOR:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_CROWN:
	case TV_HELM:
	case TV_BOOTS:
	case TV_GLOVES:
	{
		return (TRUE);
	}
	}
  
	return (FALSE);
}

static bool item_tester_unknown(object_type *o_ptr)
{
	if (object_known_p(o_ptr))
		return FALSE;
	else
		return TRUE;
}


static bool item_tester_unknown_star(object_type *o_ptr)
{
        if (o_ptr->ident & IDENT_MENTAL)
                return FALSE;
        else
                return TRUE;
}


/*
 * Enchant an item
 *
 * Revamped!  Now takes item pointer, number of times to try enchanting,
 * and a flag of what to try enchanting.  Artifacts resist enchantment
 * some of the time, and successful enchantment to at least +0 might
 * break a curse on the item.  -CFT
 *
 * Note that an item can technically be enchanted all the way to +15 if
 * you wait a very, very, long time.  Going from +9 to +10 only works
 * about 5% of the time, and from +10 to +11 only about 1% of the time.
 *
 * Note that this function can now be used on "piles" of items, and
 * the larger the pile, the lower the chance of success.
 */
bool enchant(object_type *o_ptr, int n, int eflag)
{
	int i, chance, prob;
  
	bool res = FALSE;
  
	bool a = artifact_p(o_ptr);
  
	u32b f1, f2, f3;
  
	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);
  
  
	/* Large piles resist enchantment */
	prob = o_ptr->number * 100;

	/* Missiles are easy to enchant */
	if ((o_ptr->tval == TV_BOLT) ||
	    (o_ptr->tval == TV_ARROW) ||
	    (o_ptr->tval == TV_SHOT))
	{
		prob = prob / 35;
	}
  
	/* Try "n" times */
	for (i=0; i<n; i++)
	{
		/* Hack -- Roll for pile resistance */
		if ((prob > 100) && (rand_int(prob) >= 100)) continue;
      
		/* Enchant to hit */
		if (eflag & (ENCH_TOHIT))
		{
			if (o_ptr->to_h < 0) chance = 0;
			else if (o_ptr->to_h > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_h];
	  
			/* Attempt to enchant */
	  
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;
	      
				/* Enchant */
				o_ptr->to_h++;
	      
				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_h >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
		  
					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
      
		/* Enchant to damage */
		if (eflag & (ENCH_TODAM))
		{
			if (o_ptr->to_d < 0) chance = 0;
			else if (o_ptr->to_d > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_d];
	  
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;
	      
				/* Enchant */
				o_ptr->to_d++;
	      
				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_d >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
		  
					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
      
		/* Enchant to armor class */
		if (eflag & (ENCH_TOAC))
		{
			if (o_ptr->to_a < 0) chance = 0;
			else if (o_ptr->to_a > 15) chance = 1000;
			else chance = enchant_table[o_ptr->to_a];
	  
			if ((randint(1000) > chance) && (!a || (rand_int(100) < 50)))
			{
				res = TRUE;
	      
				/* Enchant */
				o_ptr->to_a++;
	      
				/* only when you get it above -1 -CFT */
				if (cursed_p(o_ptr) &&
				    (!(f3 & (TR3_PERMA_CURSE))) &&
				    (o_ptr->to_a >= 0) && (rand_int(100) < 25))
				{
					msg_print("The curse is broken!");
		  
					/* Uncurse the object */
					uncurse_object(o_ptr);
				}
			}
		}
	}
  
	/* Failure */
	if (!res) return (FALSE);
  
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
  
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
  
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
  
	/* Success */
	return (TRUE);
}



/*
 * Enchant an item (in the inventory or on the floor)
 * Note that "num_ac" requires armour, else weapon
 * Returns TRUE if attempted, FALSE if cancelled
 */
bool enchant_spell(int num_hit, int num_dam, int num_ac)
{
	int item;
	bool okay = FALSE;
  
	object_type *o_ptr;
  
	char o_name[120];
  
	cptr q, s;
  
  
        /* Assume enchant weapon */
        item_tester_hook = item_tester_hook_weapon;

	/* Don't restrict choices */
	item_tester_tval = 0;
  
        /* Enchant armor if requested */
        if (num_ac) item_tester_hook = item_tester_hook_armour;

        /* Get an item */
        q = "Enchant which item? ";
        s = "You have nothing to enchant.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

        /* Get the item (in the pack) */
        if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
  
	/* Description */
        object_desc(o_name, o_ptr, FALSE, 0);

        /* Describe */
        msg_format("%s %s glow%s brightly!",
                   ((item >= 0) ? "Your" : "The"), o_name,
                   ((o_ptr->number > 1) ? "" : "s"));

        /* Enchant */
	if (enchant(o_ptr, num_hit, ENCH_TOHIT)) okay = TRUE;
	if (enchant(o_ptr, num_dam, ENCH_TODAM)) okay = TRUE;
	if (enchant(o_ptr, num_ac, ENCH_TOAC)) okay = TRUE;
  
	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();
      
		/* Message */
		msg_print("The enchantment failed.");
	}
  
	/* Something happened */
	return (TRUE);
}

/*
 * Enchant some missiles and give them an elemental brand
 *
 * Combines the old brand_bolts and brand_missiles routines.
 *
 * ammo_type is the tval of the relevant ammunition.  
 * If set to 0, any ammunition is enchantable.
 *
 * Brand type is the EGO flag for the relevant type element.
 * If set to 0, a non-poison brand is picked randomly.
 *
 */
bool brand_missile(int ammo_type, int brand_type)
{
	int item, choice;
	object_type *o_ptr;
	cptr q, s;
	bool status;
  
	/* Restrict choices
	 * Hack - check for restricted choice */
	if ((ammo_type >= TV_SHOT) && (ammo_type <= TV_BOLT)) 
		item_tester_tval = ammo_type;
  
	/* Otherwise any ammo will do */
	else item_tester_hook = item_tester_hook_ammo;
  
        /* Get an item */
        q = "Enchant which ammunition? ";
        s = "You have no ammunition to brand.";
	status = get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

        /* Hack - if failed, return, but only after resetting the ammo hack */
        if (!status) return (FALSE);
  
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
	/*
	 * Don't enchant artifacts, ego-items, cursed or broken items
	 */
	if (artifact_p(o_ptr) || ego_item_p(o_ptr) || 
	    cursed_p(o_ptr) || broken_p(o_ptr))
	{
		/* Flush */
		if (flush_failure) flush();
      
		/* Fail */
		msg_print("The ammunition enchantment failed.");
      
		/* Notice */
		return (TRUE);
	}
  
	/* Type of brand may be restricted */
	if (brand_type) choice = brand_type;
  
	/* Otherwise choose randomly
	 * Hack - Never get poison brand randomly */
	else choice = rand_int(4) + EGO_ACIDIC;
  
	switch (choice)
	{
	case EGO_FLAME:
	{
		/* Print message and fire brand missiles. */
		msg_print("Your missiles are covered in a fiery aura!");
		break;
	}
      
	case EGO_FROST:
	{
		/* Print message and frost brand missiles. */
		msg_print("Your missiles are covered in a frosty sheath!");
		break;
	}
      
	case EGO_ACIDIC:
	{
		/* Print message and acid brand missiles. */
		msg_print("Your missiles sizzle with acid!");
		break;
	}
      
	case EGO_ELECT:
	{
		/* Print message and electric brand missiles. */
		msg_print("Your missiles are covered in sparks!");
		break;
	}
      
	case EGO_POISON:
	{
		/* Print message and poison brand missiles. */
		msg_print("Your missiles drip with deadly poison!");
		break;
	}
      
	default:
	{
		/* Oops */
		return (FALSE);
	}
	}
  
	/* Brand */
	o_ptr->name2 = choice;
  
	/* Enchant */
	enchant(o_ptr, rand_int(4) + 3, ENCH_TOHIT | ENCH_TODAM);
  
	/* Prevent money-making. */
	o_ptr->discount = 80;
  
	/* Notice */
	return (TRUE);
}

/*
 * Set a temporary elemental brand.  Clear all other brands.  Print status 
 * messages. -LM-
 */
void set_ele_attack(u32b attack_type, int duration)
{
	/* Clear all elemental attacks (only one is allowed at a time). */
	if ((p_ptr->special_attack & (ATTACK_ACID)) && (attack_type != ATTACK_ACID))
	{
		p_ptr->special_attack &= ~(ATTACK_ACID);
		msg_print("Your temporary acidic brand fades away.");
	}
	if ((p_ptr->special_attack & (ATTACK_ELEC)) && (attack_type != ATTACK_ELEC))
	{
		p_ptr->special_attack &= ~(ATTACK_ELEC);
		msg_print("Your temporary electrical brand fades away.");
	}
	if ((p_ptr->special_attack & (ATTACK_FIRE)) && (attack_type != ATTACK_FIRE))
	{
		p_ptr->special_attack &= ~(ATTACK_FIRE);
		msg_print("Your temporary fiery brand fades away.");
	}
	if ((p_ptr->special_attack & (ATTACK_COLD)) && (attack_type != ATTACK_COLD))
	{
		p_ptr->special_attack &= ~(ATTACK_COLD);
		msg_print("Your temporary frost brand fades away.");
	}
	if ((p_ptr->special_attack & (ATTACK_POIS)) && (attack_type != ATTACK_POIS))
	{
		p_ptr->special_attack &= ~(ATTACK_POIS);
		msg_print("Your temporary poison brand fades away.");
	}
  
	if ((duration) && (attack_type))
	{
		/* Set attack type. */
		p_ptr->special_attack |= (attack_type);
      
		/* Set duration. */
		p_ptr->ele_attack = duration;
      
		/* Message. */
		msg_format("For a while, the blows you deal will %s",
			   ((attack_type == ATTACK_ACID) ? "melt with acid!" :
			    ((attack_type == ATTACK_ELEC) ? "shock your foes!" :
			     ((attack_type == ATTACK_FIRE) ? "burn with fire!" : 
			      ((attack_type == ATTACK_COLD) ? "chill to the bone!" : 
			       ((attack_type == ATTACK_POIS) ? "poison your enemies!" : 
				"do nothing special."))))));
	}
  
	/* Redraw the state */
	p_ptr->redraw |= (PR_STATUS);
  
	/* Handle stuff */
	handle_stuff();
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;
  
	char o_name[120];
  
  
	/* Curse the body armor */
	o_ptr = &inventory[INVEN_BODY];
  
	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);
  
  
	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);
  
	/* Attempt a saving throw for artifacts */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
			   "terrible black aura", "surround your armor", o_name);
	}
  
	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);
      
		/* Blast the armor */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_BLASTED;
		o_ptr->to_a = 0 - randint(5) - randint(5);
		o_ptr->to_h = 0;
		o_ptr->to_d = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
      
		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);
      
		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);
      
		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
      
		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);
      
		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}
  
	return (TRUE);
}


/*
 * Curse the players weapon
 */
bool curse_weapon(void)
{
	object_type *o_ptr;
  
	char o_name[120];
  
  
	/* Curse the weapon */
	o_ptr = &inventory[INVEN_WIELD];
  
	/* Nothing to curse */
	if (!o_ptr->k_idx) return (FALSE);
  
  
	/* Describe */
	object_desc(o_name, o_ptr, FALSE, 3);
  
	/* Attempt a saving throw */
	if (artifact_p(o_ptr) && (rand_int(100) < 50))
	{
		/* Cool */
		msg_format("A %s tries to %s, but your %s resists the effects!",
			   "terrible black aura", "surround your weapon", o_name);
	}
  
	/* not artifact or failed save... */
	else
	{
		/* Oops */
		msg_format("A terrible black aura blasts your %s!", o_name);
      
		/* Shatter the weapon */
		o_ptr->name1 = 0;
		o_ptr->name2 = EGO_SHATTERED;
		o_ptr->to_h = 0 - randint(5) - randint(5);
		o_ptr->to_d = 0 - randint(5) - randint(5);
		o_ptr->to_a = 0;
		o_ptr->ac = 0;
		o_ptr->dd = 0;
		o_ptr->ds = 0;
      
		/* Curse it */
		o_ptr->ident |= (IDENT_CURSED);
      
		/* Break it */
		o_ptr->ident |= (IDENT_BROKEN);
      
		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
      
		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);
      
		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}
  
	/* Notice */
	return (TRUE);
}




/*
 * Identify an object in the inventory (or on the floor)
 * This routine does *not* automatically combine objects.
 * Returns TRUE if something was identified, else FALSE.
 */
bool ident_spell(void)
{
        int item;
        object_type *o_ptr;

        char o_name[120];
  
	cptr q, s;
  
  
        /* Only un-id'ed items */
        item_tester_hook = item_tester_unknown;

	/* Don't restrict choices */
	item_tester_tval = 0;
  
        /* Get an item */
        q = "Identify which item? ";
        s = "You have nothing to identify.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

        /* Get the item (in the pack) */
        if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
  
	/* Identify it fully */
        object_aware(o_ptr);
        object_known(o_ptr);

	/* Apply an autoinscription, if necessary */
	apply_autoinscription(o_ptr);

	/* Set squelch flag */
	p_ptr->notice |= PN_SQUELCH;

        /* Recalculate bonuses */
        p_ptr->update |= (PU_BONUS);
  
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
  
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
  
	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);
  
	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			   describe_use(item), o_name, index_to_label(item));
        }
        else if (item >= 0)
        {
		msg_format("In your pack: %s (%c).", o_name, index_to_label(item));
        }
        else
        {
		msg_format("On the ground: %s.", o_name);

        }

	/* If artifact, check for Set Item and write a note if applicable */
        if (o_ptr->name1)
        {
                artifact_type *a_ptr = &a_info[o_ptr->name1];
		if (a_ptr->set_no != 0)
                {
                        msg_print("This item is part of a set!");
                }
      
		if (o_ptr->found)
		{
		        int artifact_stage, lev;
			char note[120];
			char shorter_desc[120];
			s32b real_turn = turn;
          
			/* Get a shorter description to fit the notes file */
			object_desc(shorter_desc, o_ptr, TRUE, 0);
          
			/* Build note and write */
			sprintf(note, "Found %s", shorter_desc);
          
			/* Record the depth where the artifact was created */
			artifact_stage = o_ptr->found;
          
			/* Hack - record the turn when the artifact was first picked up
			 * or wielded by the player.  This may result in out of order
			 * entries in the notes file, which really should be re-ordered 
			 */
			turn = a_info[o_ptr->name1].creat_turn & 0x00FFFFFF;
			if (turn < 2) turn = real_turn;
			lev = (a_info[o_ptr->name1].creat_turn & 0xFF000000) >> 24;
			if (lev == 0) lev = p_ptr->lev;
			make_note(note, artifact_stage, NOTE_ARTIFACT, lev);
			turn = real_turn;
          
			/*
			 * Mark item creation depth 0, which will indicate the artifact
			 * has been previously identified.  This prevents an artifact
			 * from showing up on the notes list twice ifthe artifact had
			 * been previously identified.  JG
			 */
			o_ptr->found = 0 ;
		}

	}


        /* Something happened */
        return (TRUE);
}



/*
 * Fully "*identify*" an object in the inventory
 *
 * This routine returns TRUE if an item was identified.
 */
bool identify_fully(void)
{
        int item;
        int squelch=0;
  
	object_type *o_ptr;
	object_kind *k_ptr;
  
	char o_name[120];

        cptr q, s;

	bool noted;
  
        /* Only un-*id*'ed items */
        item_tester_hook = item_tester_unknown_star;

        /* Get an item.   */
        q = "*Identify* which item? ";
        s = "You have nothing to *identify*.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

        /* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];
  
	/* Artifacts not yet normally ID'd need noting */
	noted = (o_ptr->ident & IDENT_KNOWN);
  
	/* Identify it fully */
	object_aware(o_ptr);
	object_known(o_ptr);
  
        /* Mark the item as fully known */
        o_ptr->ident |= (IDENT_MENTAL);


	/* If artifact, write a note if applicable */
	if ((o_ptr->name1) && (o_ptr->found))
	{
                int artifact_stage, lev;
		char note[120];
		char shorter_desc[120];
		s32b real_turn = turn;
      
		/* Get a shorter description to fit the notes file */
		object_desc(shorter_desc, o_ptr, TRUE, 0);
      
		/* Build note and write */
		sprintf(note, "Found %s", shorter_desc);
      
		/* Record the depth where the artifact was created */
		artifact_stage = o_ptr->found;
      
		/* Hack - record the turn when the artifact was first picked up
		 * or wielded by the player.  This may result in out of order
		 * entries in the notes file, which really should be re-ordered 
		 */
		turn = a_info[o_ptr->name1].creat_turn & 0x00FFFFFF;
		if (turn < 2) turn = real_turn;
		lev = (a_info[o_ptr->name1].creat_turn & 0xFF000000) >> 24;
		if (lev == 0) lev = p_ptr->lev;
		make_note(note, artifact_stage, NOTE_ARTIFACT, lev);
		turn = real_turn;
      
		/*
		 * Mark item creation depth 0, which will indicate the artifact
		 * has been previously identified.  This prevents an artifact
		 * from showing up on the notes list twice ifthe artifact had
		 * been previously identified.  JG
		 */
		o_ptr->found = 0 ;
	}

	/* If the object is flavored, also make all items of that type, 
	 * except for variable rings and amulets, fully known. */
        if (k_ptr->flavor)
        {
                if (((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) &&
		    (k_ptr->flags3 & (TR3_EASY_KNOW))) k_ptr->known_effect = TRUE;
		else if ((o_ptr->tval == TV_FOOD) || (o_ptr->tval == TV_STAFF) || 
			 (o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)) 
			k_ptr->known_effect = TRUE;
	}
  
  
  
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
  
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
  
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
  
	/* Handle stuff */
	handle_stuff();
  
	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);
  
	/* Describe */
	if (item >= INVEN_WIELD)
	{
		msg_format("%^s: %s (%c).",
			   describe_use(item), o_name, index_to_label(item));
	}
	else if (item >= 0)
	{
		msg_format("In your pack: %s (%c).  %s",
			   o_name, index_to_label(item),
			   ((squelch==1) ? "(Squelch)" :
			    ((squelch==-1) ? "(Squelch Failed)" : "")));
        }
        else
        {
                msg_format("On the ground: %s. %s",
                           o_name,
                           ((squelch==1) ? "(Squelch)" :
                            ((squelch==-1) ? "(Squelch Failed)" : "")));

        }

	/* If artifact, check for Set Item */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];
		if (a_ptr->set_no != 0)
		{
			msg_print("This item is part of a set!");
                }
        }

        object_info_screen(o_ptr, FALSE);

        /* Success */
        return (TRUE);
}

/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
static bool item_tester_hook_recharge(object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return (TRUE);
  
	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return (TRUE);
  
	/* Recharge rods */
	if (o_ptr->tval == TV_ROD) return (TRUE);
  
	/* Nope */
	return (FALSE);
}


/*
 * Recharge a wand/staff/rod from the pack or on the floor.
 * This function has been rewritten in Oangband. -LM-
 *
 * Mage -- Recharge I --> recharge(85)
 * Mage -- Recharge II --> recharge(150)
 * Mage -- Recharge III --> recharge(220)
 *
 * Druid -- Infusion --> recharge(125)
 *
 * Priest or Necromancer -- Recharge --> recharge(140)
 *
 * Scroll of recharging --> recharge(130)
 * Scroll of *recharging* --> recharge(200)
 *
 * It is harder to recharge high level, and highly charged wands, 
 * staffs, and rods.  The more wands in a stack, the more easily and 
 * strongly they recharge.  Staffs, however, each get fewer charges if 
 * stacked.
 *
 * XXX XXX XXX Beware of "sliding index errors".
 */
bool recharge(int power)
{
	int item, lev;
	int recharge_strength, recharge_amount;
  
	object_type *o_ptr;
	object_kind *k_ptr;
  
	bool fail = FALSE;
	byte fail_type = 1;
  
	cptr q, s;
	char o_name[120];
  
  
	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;
  
	/* Don't restrict choices */
	item_tester_tval = 0;
  
	/* Get an item */
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);
  
	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
	/* Get the object kind. */
	k_ptr = &k_info[o_ptr->k_idx];
  
	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;
  
  
	/* Recharge a rod */
	if (o_ptr->tval == TV_ROD)
	{
		/* Extract a recharge strength by comparing object level to power. */
		recharge_strength = ((power > lev) ? (power - lev) : 0) / 5;
      
      
		/* Back-fire */
		if (rand_int(recharge_strength) == 0)
		{
			/* Activate the failure code. */
			fail = TRUE;
		}
      
		/* Recharge */
		else
		{
			/* Recharge amount */
			recharge_amount = (power * damroll(3, 2));
	  
			/* Recharge by that amount */
			if (o_ptr->timeout > recharge_amount)
				o_ptr->timeout -= recharge_amount;
			else
				o_ptr->timeout = 0;
		}
	}

  
	/* Recharge wand/staff */
	else
	{
		/* Extract a recharge strength by comparing object level to power. 
		 * Divide up a stack of wands' charges to calculate charge penalty.
		 */
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			recharge_strength = (100 + power - lev - 
					     (8 * o_ptr->pval / o_ptr->number)) / 15;
      
		/* All staffs, unstacked wands. */
		else recharge_strength = (100 + power - lev - 
					  (8 * o_ptr->pval)) / 15;
      
      
		/* Back-fire */
		if ((recharge_strength < 0) || (rand_int(recharge_strength) == 0))
		{
			/* Activate the failure code. */
			fail = TRUE;
		}
      
		/* If the spell didn't backfire, recharge the wand or staff. */
		else
		{
			/* Recharge based on the standard number of charges. */
			recharge_amount = randint(1 + k_ptr->pval / 2);
	  
			/* Multiple wands in a stack increase recharging somewhat. */
			if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
			{
				recharge_amount += 
					(randint(recharge_amount * (o_ptr->number - 1))) / 2;
				if (recharge_amount < 1) recharge_amount = 1;
				if (recharge_amount > 12) recharge_amount = 12;
			}
	  
			/* But each staff in a stack gets fewer additional charges, 
			 * although always at least one.
			 */
			if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
			{
				recharge_amount /= o_ptr->number;
				if (recharge_amount < 1) recharge_amount = 1;
			}
	  
			/* Recharge the wand or staff. */
			o_ptr->pval += recharge_amount;
	  
			/* Hack - Artifacts have a maximum # of charges. */
			if (artifact_p(o_ptr) && (o_ptr->pval > k_ptr->pval)) 
				o_ptr->pval = k_ptr->pval;
	  
			/* Hack -- we no longer "know" the item */
			o_ptr->ident &= ~(IDENT_KNOWN);
	  
			/* Hack -- we no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}
	}
  
  
	/* Inflict the penalties for failing a recharge. */
	if (fail)
	{
		/* Artifacts are never destroyed. */
                if (artifact_p(o_ptr))
                {
                        object_desc(o_name, o_ptr, TRUE, 0);
                        msg_format("The recharging backfires - %s is completely drained!", o_name);

                        /* Artifact rods. */
                        if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout < 10000))
				o_ptr->timeout = (o_ptr->timeout + 100) * 2;
	  
			/* Artifact wands and staffs. */
			else if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) 
				o_ptr->pval = 0;
		}
		else 
		{
			/* Get the object description */
			object_desc(o_name, o_ptr, FALSE, 0);
	  
			/*** Determine Seriousness of Failure ***/
	  
			/* Mages recharge objects more safely. */
			if (check_ability(SP_DEVICE_EXPERT))
			{
				/* 10% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (randint(10) == 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 67% chance to blow up one wand, otherwise draining. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (randint(3) != 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 50% chance to blow up one staff, otherwise no effect. */
				else if (o_ptr->tval == TV_STAFF)
				{
					if (randint(2) == 1) fail_type = 2;
					else fail_type = 0;
				}
			}
	  
			/* All other classes get no special favors. */
			else
			{
				/* 33% chance to blow up one rod, otherwise draining. */
				if (o_ptr->tval == TV_ROD)
				{
					if (randint(3) == 1) fail_type = 2;
					else fail_type = 1;
				}
				/* 20% chance of the entire stack, else destroy one wand. */
				else if (o_ptr->tval == TV_WAND)
				{
					if (randint(5) == 1) fail_type = 3;
					else fail_type = 2;
				}
				/* Blow up one staff. */
				else if (o_ptr->tval == TV_STAFF)
				{
					fail_type = 2;
				}
			}
	  
			/*** Apply draining and destruction. ***/
	  
			/* Drain object or stack of objects. */
			if (fail_type == 1)
			{
				if (o_ptr->tval == TV_ROD)
				{
					msg_print("The recharge backfires, draining the rod further!");
					if (o_ptr->timeout < 10000) 
						o_ptr->timeout = (o_ptr->timeout + 100) * 2;
				}
				else if (o_ptr->tval == TV_WAND)
				{
					msg_format("You save your %s from destruction, but all charges are lost.", o_name);
					o_ptr->pval = 0;
				}
				/* Staffs aren't drained. */
			}
	  
			/* Destroy an object or one in a stack of objects. */
			if (fail_type == 2)
			{
				if (o_ptr->number > 1)
					msg_format("Wild magic consumes one of your %s!", o_name);
				else
					msg_format("Wild magic consumes your %s!", o_name);
	      
				/* Reduce rod stack maximum timeout, drain wands. */
				if (o_ptr->tval == TV_ROD) o_ptr->pval -= k_ptr->pval;
				if (o_ptr->tval == TV_WAND) o_ptr->pval = 0;
	      
				/* Reduce and describe inventory */
				if (item >= 0)
				{
					inven_item_increase(item, -1);
					inven_item_describe(item);
					inven_item_optimize(item);
				}
	      
				/* Reduce and describe floor item */
				else
				{
					floor_item_increase(0 - item, -1);
					floor_item_describe(0 - item);
					floor_item_optimize(0 - item);
                                }
                        }

                        /* Destroy all memebers of a stack of objects. */
                        if (fail_type == 3)
                        {
                                if (o_ptr->number > 1)
                                        msg_format("Wild magic consumes all your %s!", o_name);
                                else
                                        msg_format("Wild magic consumes your %s!", o_name);


                                /* Reduce and describe inventory */
                                if (item >= 0)
                                {
                                        inven_item_increase(item, -999);
                                        inven_item_describe(item);
                                        inven_item_optimize(item);
                                }
	      
                                /* Reduce and describe floor item */
                                else
                                {
                                        floor_item_increase(0 - item, -999);
                                        floor_item_describe(0 - item);
                                        floor_item_optimize(0 - item);
                                }
			}
		}
	}
  
	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
  
	/* Window stuff */
	p_ptr->window |= (PW_INVEN);
  
	/* Something was done */
	return (TRUE);
}


/*
 * Mages can get mana from magical objects at need. -LM-
 *
 * Returns true if tapped, false if cancelled.
 */
bool tap_magical_energy(void)
{
        int item, lev;
        int energy = 0;
  
	object_type *o_ptr;
  
	cptr q, s;
	cptr item_name = "";
  
  
	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;
  
        /* Get an item */
        q = "Drain charges from which item? ";
        s = "You have nothing to drain charges from.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return(FALSE);

        /* Get the item (in the pack) */
        if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
  
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
  
	/* Extract the object "level" */
	lev = k_info[o_ptr->k_idx].level;
  
	/* Extract the object's energy and get its generic name. */
	if (o_ptr->tval == TV_ROD) 
	{
		/* Rods have little usable energy, for obvious balance reasons... */
		energy = (lev * o_ptr->number * 2)/3;
      
		/* No tapping rods with instant recharge */
		if (!(o_ptr->pval)) energy = 0;
      
		/* Modify Based on charged-ness */ 
		if (o_ptr->pval)
			energy = (energy * (o_ptr->pval-o_ptr->timeout))/ o_ptr->pval;
		item_name = "rod";
	}
	if (o_ptr->tval == TV_STAFF)
	{
		energy = (5 + lev) * o_ptr->pval;
      
		item_name = "staff";
	}
	if (o_ptr->tval == TV_WAND)
	{
		energy = (5 + lev) * 3 * o_ptr->pval / 2;
      
		item_name = "wand";
	}
  
	/* Turn energy into mana. */
  
	/* Require a resonable amount of energy */
	if (energy < 36)
	{
		/* Notify of failure. */
		msg_format("That %s had no useable energy", item_name);
	}
	else
	{
		/* If mana below maximum, increase mana and drain object. */
		if (p_ptr->csp < p_ptr->msp)
		{
			/* Drain the object. */
			if (o_ptr->tval == TV_ROD) o_ptr->timeout = o_ptr->pval;
			else o_ptr->pval = 0;
	  

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	  
			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
	  
			/* Increase mana. */
			p_ptr->csp += energy / 12;
			p_ptr->csp_frac = 0;
			if (p_ptr->csp > p_ptr->msp) (p_ptr->csp = p_ptr->msp);
	  
			msg_print("You feel your head clear.");
	  
			p_ptr->redraw |= (PR_MANA);
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
                }

                /* Player is a smart cookie. */
                else msg_format("Your mana was already at its maximum.  %^s not drained.", item_name);
        }

        return(TRUE);
}


/*
 * Special code for staff of starlight and the Staff of Gandalf.  Most 
 * effective against monsters that start out in darkness, and against 
 * those who hate light. -LM-
 */
void do_starlight(int burst_number, int dam, bool strong)
{
	int i, j, y, x;
  
	/* Is the player in a square already magically lit? */
	bool player_lit = cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW);
  
	for (i = 0; i < burst_number; i++)
	{
		/* First, we find the spot. */
		for (j = 0; j < 20; j++)
		{
			/* Pick a (scattered) distance. */
			int d = 2 + rand_int(4);
	  
			/* Admit failure.  Switch to Plan B. */
			if (j == 19)
			{
				y = p_ptr->py;
				x = p_ptr->px;
				break;
			}
	  
			/* Pick a location */
			scatter(&y, &x, p_ptr->py, p_ptr->px, d, 0);
	  
			/* Not on top of the player. */
			if (cave_m_idx[y][x] < 0) continue;
	  
			/* Require passable terrain */
			if (!cave_passable_bold(y, x)) continue;
	  
			/* Spot chosen. */
			break;
		}
      
		/* Then we hit the spot. */
      
		/* Confusing to be suddenly lit up. */
		if (!(cave_info[y][x] & (CAVE_GLOW))) 
			fire_meteor(-1, GF_CONFUSION, y, x, dam, strong ? 1 : 0, FALSE);
      
		/* The actual burst of light. */
		fire_meteor(-1, GF_LITE_WEAK, y, x, dam, strong ? 2 : 1, FALSE);
		fire_meteor(-1, GF_LITE, y, x, dam, strong ? 1 : 0, FALSE);
      
      
		/* Hack - assume that the player's square is typical of the area, 
		 * and only light those squares that weren't already magically lit 
		 * temporarily.
		 */
		if (!player_lit) 
			fire_meteor(-1, GF_DARK_WEAK, y, x, 0, strong ? 2 : 1, FALSE);
	}
  
	/* Hard not to notice. */
	add_wakeup_chance = 10000;
}


/*
 * Find some animals on the current dungeon level, and magic map the dungeon 
 * near them.  Learn about traps on the entire level if at least one natural 
 * creature is found. -LM-
 */
bool listen_to_natural_creatures(void)
{
	int i, y, x;
	int count = 0;
  
	/* Check all the monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
                /* Paranoia -- skip "dead" monsters */
                if (!m_ptr->r_idx) continue;

                /* Only natural creatures are eligible, and some don't feel like talking. */
                if ((r_ptr->flags3 & (RF3_ANIMAL)) && (rand_int(2) == 0))
                {
                        /* Learn about their surroundings. */
			map_area(m_ptr->fy, m_ptr->fx, FALSE);
	  
			/* increment the count. */
			count++;
		}
      
		/* Avoid excessive processing time. */
		if (count > 15) break;
	}
  
	/* No natural allies. */
	if (!count) return(FALSE);
  
	/* Find every trap on the level. */
  
	/* Scan all normal grids */
	for (y = 1; y < DUNGEON_HGT-1; y++)
	{
                /* Scan all normal grids */
                for (x = 1; x < DUNGEON_WID-1; x++)
                {

                        /* Detect invisible traps */
                        if (cave_feat[y][x] == FEAT_INVIS)
                        {
                                /* Pick a trap */
                                pick_trap(y, x);
                        }

                        /* Detect traps */
                        if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
                            (cave_feat[y][x] <= FEAT_TRAP_TAIL))
                        {
                                /* Hack -- Memorize */
                                cave_info[y][x] |= (CAVE_MARK);
	      
				/* Redraw */
				lite_spot(y, x);
			}
		}
	}
  
	/* Report success. */
	return(TRUE);
}

/*
 * Only the bold and the desperate dare to unleash chaos. -LM-
 */
void unmake(int dir)
{
	byte chaotic_effect;
	int i;
	bool repeat = TRUE;
  
	while (repeat)
	{
		/* Pick an effect. */
		chaotic_effect = (byte)rand_int(18);
      
                switch (chaotic_effect)
                {
                        /* Massive chaos bolt. */
		case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
		{
			fire_bolt(GF_CHAOS, dir, randint(500));
			break;
		}
		/* Chaos balls in every directioon */
		case 8: case 9:
		{
			for (i = 0; i < 8; i++) fire_ball(GF_CHAOS, ddd[i], randint(400), 2, FALSE);
			break;
		}
		/* Tear up the dungeon. */
		case 10:
		{
			destroy_area(p_ptr->py, p_ptr->px, 5 + randint(20), TRUE);
			break;
		}
		/* Chaos cloud right on top of the poor caster. */
		case 11:
		{
			fire_cloud(GF_CHAOS, 0, randint(400), 6);	
			break;
		}
		/* Chaos spray. */
		case 12: case 13: case 14: case 15: case 16:
		{
			fire_arc(GF_CHAOS, dir, randint(600), 8, 90);
			break;
		}
		/* Unmake the caster. */
		case 17:
		{
			(void)dec_stat(A_STR, 20, (rand_int(3) == 0));
			(void)dec_stat(A_INT, 20, (rand_int(3) == 0));
			(void)dec_stat(A_WIS, 20, (rand_int(3) == 0));
			(void)dec_stat(A_DEX, 20, (rand_int(3) == 0));
			(void)dec_stat(A_CON, 20, (rand_int(3) == 0));
			(void)dec_stat(A_CHR, 20, (rand_int(3) == 0));
			break;	
		}
		}
      
		/* Chaos, once unleashed, likes to stay... */
		if (rand_int(4) == 0) repeat = TRUE;
		else repeat = FALSE;
	}
}


/*
 * Unleash the wrath of the beings of Air. -LM-
 */
void ele_air_smite(void)
{
	byte i, j;
	int y, x;
  
	/* Due warning. */
	msg_print("The powers of Air rain down destruction!");
  
	/* Multiple gravity, light, and electricity balls. */
	for (i = 0; i < 8; i++)
	{
		/* Select a legal nearby location at random. */
		for (j = 0; j < 20; j++)
		{
			/* Pick a (short) distance. */
			int d = randint(3);
	  
			/* Admit failure.  Switch to Plan B. */
			if (j == 19)
			{
				y = p_ptr->py;
				x = p_ptr->px;
				break;
			}
			/* Pick a location */
			scatter(&y, &x, p_ptr->py, p_ptr->px, d, 0);
	  
			/* Not on top of the player. */
			if (cave_m_idx[y][x] < 0) continue;
	  
			/* Require passable terrain */
			if (!cave_passable_bold(y, x)) continue;
	  
			/* Slight preference for actual monsters. */
			if (cave_m_idx[y][x] > 0) break;
	  
			/* Will accept any passable grid after a few tries. */
			else if (j > 3) break;
		}
      
		if (rand_int(3) == 0) 
			(void)fire_meteor(-1, GF_GRAVITY, y, x, 100, 1, FALSE);
		else if (rand_int(2) == 0)
			(void)fire_meteor(-1, GF_LITE, y, x, 100, 1, FALSE);
		else
			(void)fire_meteor(-1, GF_ELEC, y, x, 100, 1, FALSE);
      
      
		/* This is a bombardment.  Make it look like one. */
		Term_xtra(TERM_XTRA_DELAY, 10);
	}
  
	/* I would /probably/ be awake at this point... */
	add_wakeup_chance = 10000;
}


/*
 * Apply a "project()" directly to all monsters in view of a certain spot.
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * This function is not optimized for efficieny.  It should only be used
 * in non-bottleneck functions such as spells. It should not be used in functions
 * that are major code bottlenecks such as process monster or update_view. -JG
 */
bool project_los_not_player(int y1, int x1, int dam, int typ)
{
	int i, x, y;
  
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
  
	bool obvious = FALSE;
  
	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/*The LOS function doesn't do well with long distances*/
		if (distance(y1, x1, y, x) > MAX_RANGE) continue;
      
		/* Require line of sight or the monster being right on the square */
		if ((y != y1) || (x != x1))
		{
	  
			if (!los(y1, x1, y, x)) continue;
	  
		}
      
		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, dam, typ, flg,0 ,0)) obvious = TRUE;
	}
  
	/* Result */
	return (obvious);
}


/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 */
static bool project_hack(int typ, int dam)
{
	int i, x, y;
  
	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
  
	bool obvious = FALSE;
  
  
	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;
      
		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;
      
		/* Jump directly to the target monster */
		if (project(-1, 0, y, x, dam, typ, flg, 0, 0)) obvious = TRUE;
	}
  
	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(int dam)
{
	return (project_hack(GF_OLD_SLOW, dam));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int dam)
{
	return (project_hack(GF_OLD_SLEEP, dam));
}

/*
 * Frighten monsters. -LM-
 */
bool fear_monsters(int dam)
{
	return (project_hack(GF_TURN_ALL, dam));
}

/*
 * Confuse monsters. -LM-
 */
bool confu_monsters(int dam)
{
	return (project_hack(GF_OLD_CONF, dam));
}

/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(int dam)
{
	return (project_hack(GF_TURN_UNDEAD, dam));
}

/*
 * Turn evil. -LM-
 */
bool turn_evil(int dam)
{
	return (project_hack(GF_TURN_EVIL, dam));
}

/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_hack(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_hack(GF_DISP_EVIL, dam));
}

/*
 * Dispel demons
 */
bool dispel_demons(int dam)
{
	return (project_hack(GF_DISP_DEMON, dam));
}

/*
 * Dispel non-evil monsters
 */
bool dispel_not_evil(int dam)
{
	return (project_hack(GF_DISP_NOT_EVIL, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
}

/*
 * Dispel all small monsters
 */
bool dispel_small_monsters(int dam)
{
	return (project_hack(GF_DISP_SMALL_ALL, dam));
}

/*
 * Dispel all living creatures.   From Sangband.
 */
bool dispel_living(int dam)
{
	return (project_hack(GF_SPIRIT, dam));
}

/*
 * Dispel monsters who can't stand bright light. -LM-
 */
bool dispel_light_hating(int dam)
{
	return (project_hack(GF_LITE_WEAK, dam));
}

/* 
 * Put undead monsters into stasis. -LM-
 */
bool hold_undead(void)
{
	return (project_hack(GF_HOLD_UNDEAD, 0));
}

/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who, bool the_entire_level)
{
	int i;
  
	bool sleep = FALSE;
  
	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Skip aggravating monster (or player) */
		if (i == who) continue;
      
		/* Wake up and hasten all monsters. No additional messages. */
		if (the_entire_level)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
			}
	  
			/* Go active */
			m_ptr->mflag |= (MFLAG_ACTV);
	  
			/* Get mad. */
			if (m_ptr->mspeed < r_ptr->speed + 10) 
				m_ptr->mspeed = r_ptr->speed + 10;
		}
      
		/* Standard aggravation */
		else 
		{
			/* Wake up nearby sleeping monsters */
			if (m_ptr->cdis < (p_ptr->themed_level ? 
					   MAX_SIGHT : MAX_SIGHT * 2))
			{
				/* Wake up */
				if (m_ptr->csleep)
				{
					/* Wake up */
					m_ptr->csleep = 0;
					sleep = TRUE;
		  
					/* Do not necessarily go active */
				}
			}
		}
	}
  
	/* Messages */
	if (sleep) msg_print("You hear a sudden stirring in the distance!");
}



/*
 * Delete all non-unique monsters of a given "type" from the level
 */
bool genocide(void)
{
	int i;
  
	char typ;
  
	bool result = FALSE;
  
  
	/* Mega-Hack -- Get a monster symbol */
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));
  
	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;
      
		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;
      
		/* Ignore monsters in icky squares */
		if ((cave_info[m_ptr->fy][m_ptr->fx] & CAVE_ICKY) == CAVE_ICKY) continue;
      
		/* Delete the monster */
		delete_monster_idx(i);
      
		/* Take some damage */
		take_hit(randint(4), "the strain of casting Genocide");
      
		/* Take note */
		result = TRUE;
	}
  
	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(void)
{
	int i;
  
	bool result = FALSE;
  
  
	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;
      
		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;
      
		/* Ignore monsters in icky squares */
		if ((cave_info[m_ptr->fy][m_ptr->fx] & CAVE_ICKY) == CAVE_ICKY) continue;
      
		/* Delete the monster */
		delete_monster_idx(i);
      
		/* Take some damage */
		take_hit(randint(3), "the strain of casting Mass Genocide");
      
		/* Note effect */
		result = TRUE;
	}
  
	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int i;
  
	bool probe = FALSE;
  
  
	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
      
		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
      
		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;
      
		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char m_name[80];
	  
			/* Start the message */
			if (!probe) msg_print("Probing...");
	  
			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);
	  
			/* Describe the monster */
                        if (!(r_ptr->mana))
				msg_format("%^s has %d hit points.", m_name, m_ptr->hp);
                        else
				msg_format("%^s has %d hit points and %d mana.", m_name, m_ptr->hp, m_ptr->mana);

                        /* Learn all of the non-spell, non-treasure flags */
                        lore_do_probe(i);
	  
			/* Probe worked */
			probe = TRUE;
		}
	}
  
	/* Done */
	if (probe)
	{
		msg_print("That's all.");
	}
  
	/* Result */
	return (probe);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
void destroy_area(int y1, int x1, int r, bool full)
{
	int y, x, k, t;
  
	bool flag = FALSE;
  
  
	/* XXX XXX */
	full = full ? full : 0;
  
	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds_fully(y, x)) continue;
	  
			/* Extract the distance */
			k = distance(y1, x1, y, x);
	  
			/* Stay in the circle of death */
			if (k > r) continue;
	  
			/* Ignore icky squares */
			if ((cave_info[y][x] & CAVE_ICKY) == CAVE_ICKY) continue;
	  
			/* Lose room and vault */
			cave_info[y][x] &= ~(CAVE_ROOM);
	  
			/* Lose light and knowledge */
			cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);
	  
			/* Hack -- Notice player affect */
			if (cave_m_idx[y][x] < 0)
			{
				/* Hurt the player later */
				flag = TRUE;
	      
				/* Do not hurt this grid */
				continue;
			}
	  
			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;
	  
			/* Delete the monster (if any) */
			delete_monster(y, x);
	  
			/* Destroy "valid" grids */
                        if (cave_valid_bold(y, x))
                        {
                                int feat = FEAT_FLOOR;

                                /* Delete objects */
                                delete_object(y, x);

                                /* Decrement the trap or glyph count. */
                                if ((cave_feat[y][x] >= FEAT_MTRAP_HEAD) && (cave_feat[y][x] <= FEAT_MTRAP_TAIL))
                                        num_trap_on_level--;
                                else if (cave_feat[y][x] == FEAT_GLYPH)
                                        num_glyph_on_level--;
	      
	      
				/* Wall (or floor) type */
				t = rand_int(200);
	      
				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					feat = FEAT_WALL_EXTRA;
				}
	      
				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					feat = FEAT_QUARTZ;
				}
	      
				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					feat = FEAT_MAGMA;
				}
	      
				/* Change the feature */
				cave_set_feat(y, x, feat);
			}
		}
	}
  
  
	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
                msg_print("There is a searing blast of light!");

                /* Blind the player */
                if (!p_ptr->no_blind && !p_resist_pos(P_RES_LITE))
                {
                        /* Become blind */
                        (void)set_blind(p_ptr->blind + 10 + randint(10));
		}
	}
  
  
	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
  
	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);
  
	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that players and monsters (except eaters of walls and passers
 * through walls) will never occupy the same grid as a wall (or door).
 */
void earthquake(int cy, int cx, int r, bool volcano)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int i, t, y, x, yy, xx, dy, dx;
  
	int damage = 0;
  
	int sn = 0, sy = 0, sx = 0;

	bool hurt = FALSE;
  
	bool map[32][32];
  
  
	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;
  
	/* Clear the "maximal blast" area */
	for (y = 0; y < 32; y++)
	{
		for (x = 0; x < 32; x++)
		{
			map[y][x] = FALSE;
		}
	}
  
	/* Check around the epicenter */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;
	  
			/* Skip illegal grids */
			if (!in_bounds_fully(yy, xx)) continue;
	  
                        /* Skip distant grids */
                        if (distance(cy, cx, yy, xx) > r) continue;

                        /* Lose room and vault */
                        cave_info[yy][xx] &= ~(CAVE_ROOM);

                        /* Lose light and knowledge */
                        cave_info[yy][xx] &= ~(CAVE_GLOW | CAVE_MARK);

                        /* Skip the epicenter */
                        if (!dx && !dy) continue;

                        /* Skip most grids */
                        if (rand_int(100) < 75) continue;

                        /* Damage this grid */
                        map[16+yy-cy][16+xx-cx] = TRUE;
	  
			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px)) hurt = TRUE;
		}
	}
  
	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Access the grid */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];
	  
			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;
	  
			/* Important -- Skip "quake" grids */
			if (map[16 + y - cy][16 + x - cx]) continue;
	  
			/* Count "safe" grids, apply the randomizer */
			if ((++sn > 1) && (rand_int(sn) != 0)) continue;
	  
			/* Save the safe location */
			sy = y; sx = x;
		}
      
		/* Random message */
		switch (randint(3))
		{
		case 1:
		{
			msg_print("The cave ceiling collapses!");
			break;
		}
		case 2:
		{
			msg_print("The cave floor twists in an unnatural way!");
			break;
		}
		default:
		{
			msg_print("The cave quakes!");
			msg_print("You are pummeled with debris!");
			break;
		}
		}
      
		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
			msg_print("You are severely crushed!");
			damage = damroll(5, 80);
		}
      
		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint(3))
			{
			case 1:
			{
				msg_print("You nimbly dodge the blast!");
				damage = 0;
				break;
			}
			case 2:
			{
				msg_print("You are bashed by rubble!");
				damage = damroll(10, 4);
				(void)set_stun(p_ptr->stun + randint(50));
				break;
			}
			case 3:
			{
				msg_print("You are crushed between the floor and ceiling!");
				damage = damroll(10, 8);
				(void)set_stun(p_ptr->stun + randint(50));
				break;
			}
                        }

                        /* Move player */
                        monster_swap(py, px, sy, sx);
		}
      
		/* Take some damage */
		if (damage) take_hit(damage, "an earthquake");
	}
  
  
	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;
	  
			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;
	  
			/* Process monsters */
			if (cave_m_idx[yy][xx] > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx[yy][xx]];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
	      
				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;
		  
					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];
			  
							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;
			  
							/* Hack -- no safety on glyph of warding */
							if (cave_feat[y][x] == FEAT_GLYPH) continue;
			  
							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;
			  
							/* Count "safe" grids, apply the randomizer */
							if ((++sn > 1) && (rand_int(sn) != 0)) continue;
			  
							/* Save the safe grid */
							sy = y;
							sx = x;
						}
					}
		  
					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);
		  
					/* Scream in pain */
					msg_format("%^s wails out in pain!", m_name);
		  
					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : damroll(5, 80));
		  
					/* Monster is certainly awake */
					m_ptr->csleep = 0;
		  
					/* Go active */
					m_ptr->mflag |= (MFLAG_ACTV);
		  
					/* Apply damage directly */
					m_ptr->hp -= damage;
		  
					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
						msg_format("%^s is embedded in the rock!", m_name);
		      
						/* Delete the monster */
						delete_monster(yy, xx);
		      
						/* No longer safe */
						sn = 0;
					}
		  
					/* Hack -- Escape from the rock */
					if (sn)
					{
						/* Move the monster */
						monster_swap(yy, xx, sy, sx);
					}
				}
			}
		}
	}
  
  
	/* XXX XXX XXX */
  
	/* New location */
	py = p_ptr->py;
	px = p_ptr->px;
  
	/* Important -- no wall on player */
	map[16 + py - cy][16 + px - cx] = FALSE;
  
  
	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;
	  
			/* Skip unaffected grids */
			if (!map[16 + yy - cy][16 + xx - cx]) continue;
	  
			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;
	  
			/* Destroy location (if valid).   Increment trap/glyph count. */
			if (cave_valid_bold(yy, xx))
			{
				int feat = FEAT_FLOOR;

                                bool floor = cave_floor_bold(yy, xx);

                                /* Delete objects */
                                delete_object(yy, xx);

                                /* Hack -- Increment the trap or glyph count. */
                                if ((cave_feat[y][x] >= FEAT_MTRAP_HEAD) && (cave_feat[y][x] <= FEAT_MTRAP_TAIL))
                                        num_trap_on_level--;
                                else if (cave_feat[y][x] == FEAT_GLYPH)
                                        num_glyph_on_level--;
	      
				/* Wall (or floor) type */
				t = (floor ? rand_int(120) : 200);
	      
	      
				/* Granite (rubble if monster is present) */
				if (t < 20)
				{
					/* Dump rubble on top of monsters. */
					if (cave_m_idx[yy][xx] > 0) feat = FEAT_RUBBLE;
		  
					/* Otherwise, create granite wall */
					else feat = FEAT_WALL_EXTRA;
				}
	      
				/* Quartz */
				else if (t < 55)
				{
					/* Dump rubble on top of monsters. */
					if (cave_m_idx[yy][xx] > 0) feat = FEAT_RUBBLE;
		  
					/* If this was a volcanic eruption, create lava near 
					 * center. -LM-
					 */
					else if ((volcano) && (distance(cy, cx, yy, xx) < 3)) 
						feat = FEAT_LAVA;
		  
					/* Otherwise, create quartz vein */
					else feat = FEAT_QUARTZ;
				}
	      
				/* Magma */
				else if (t < 90)
				{
					/* Dump rubble on top of monsters. */
					if (cave_m_idx[yy][xx] > 0) feat = FEAT_RUBBLE;
		  
					/* If this was a volcanic eruption, create lava near 
					 * center. -LM-
					 */
					else if ((volcano) && (distance(cy, cx, yy, xx) < 3)) 
						feat = FEAT_LAVA;
		  
					/* Otherwise, create magma vein */
					else feat = FEAT_MAGMA;
				}
	      
				/* Rubble. */
				else if (t < 120)
				{
					/* Create rubble */
					feat = FEAT_RUBBLE;
				}

				/* Change the feature */
				cave_set_feat(yy, xx, feat);
			}
		}
	}


	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
  
	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);
  
	/* Update the health and mana bars */
	p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);
  
	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
	int i;
  
	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];
      
		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);
      
		/* Perma-Lite */
		cave_info[y][x] |= (CAVE_GLOW);
	}
  
	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
  
	/* Update stuff */
	update_stuff();
  
	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];
      
		/* Redraw the grid */
		lite_spot(y, x);
      
		/* Process affected monsters */
		if (cave_m_idx[y][x] > 0)
		{
			int chance = 25;
	  
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
	  
			/* Stupid monsters rarely wake up */
			if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;
	  
			/* Smart monsters always wake up */
			if (r_ptr->flags2 & (RF2_SMART)) chance = 100;
	  
			/* Sometimes monsters wake up */
			if (m_ptr->csleep && (rand_int(100) < chance))
			{
				/* Wake up! */
				m_ptr->csleep = 0;
	      
				/* Go active */
				m_ptr->mflag |= (MFLAG_ACTV);
	      
				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];
		  
					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0);
		  
					/* Dump a message */
					msg_format("%^s wakes up.", m_name);
				}
			}
		}
	}
  
	/* None left */
	temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 */
static void cave_temp_room_unlite(void)
{
	int i;
  
	/* Apply flag changes */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];
      
		/* No longer in the array */
		cave_info[y][x] &= ~(CAVE_TEMP);
      
		/* Darken the grid */
		cave_info[y][x] &= ~(CAVE_GLOW);
      
		/* Hack -- Forget "boring" grids */
		if (cave_feat[y][x] <= FEAT_INVIS)
		{
			/* Forget the grid */
			cave_info[y][x] &= ~(CAVE_MARK);
		}
	}
  
	/* Fully update the visuals */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
  
	/* Update stuff */
	update_stuff();
  
	/* Process the grids */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];
      
		/* Redraw the grid */
		lite_spot(y, x);
	}
  
	/* None left */
	temp_n = 0;
}




/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x)
{
	/* Check in bounds - thanks George */
	if (!in_bounds(y, x)) return;

	/* Avoid infinite recursion */
	if (cave_info[y][x] & (CAVE_TEMP)) return;
  
	/* Do not "leave" the current room */
	if (!(cave_info[y][x] & (CAVE_ROOM))) return;
  
	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;
  
	/* Mark the grid as "seen" */
	cave_info[y][x] |= (CAVE_TEMP);
  
	/* Add it to the "seen" set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}




/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	int i, x, y;
  
	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);
  
	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
                x = temp_x[i], y = temp_y[i];

                /* Walls (but not trees) get lit, but stop light */
                if ((!cave_floor_bold(y, x)) && (cave_feat[y][x] != FEAT_TREE)) continue;

                /* Spread adjacent */
                cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);
      
		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}
  
	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
	int i, x, y;
  
	/* Add the initial grid */
	cave_temp_room_aux(y1, x1);
  
	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];
      
		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x)) continue;
      
		/* Spread adjacent */
		cave_temp_room_aux(y + 1, x);
		cave_temp_room_aux(y - 1, x);
		cave_temp_room_aux(y, x + 1);
		cave_temp_room_aux(y, x - 1);
      
		/* Spread diagonal */
		cave_temp_room_aux(y + 1, x + 1);
		cave_temp_room_aux(y - 1, x - 1);
		cave_temp_room_aux(y - 1, x + 1);
		cave_temp_room_aux(y + 1, x - 1);
	}
  
	/* Now, darken them all at once */
	cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_GRID | PROJECT_KILL;
  
	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("You are surrounded by a white light.");
	}
  
	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_LITE_WEAK, flg, 0, 0);
  
	/* Lite up the room */
	lite_room(py, px);
  
	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_GRID | PROJECT_KILL;
  
	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		msg_print("Darkness surrounds you.");
	}
  
	/* Hook into the "project()" function */
	(void)project(-1, rad, py, px, dam, GF_DARK_WEAK, flg, 0, 0);
  
	/* Lite up the room */
	unlite_room(py, px);
  
	/* Assume seen */
	return (TRUE);
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Allow "jump" option to remove the standard "trail" from the caster to 
 * the target. -LM-
 * Affect grids, objects, and monsters
 */
bool fire_ball(int typ, int dir, int dam, int rad, bool jump)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int ty, tx;
  
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
  
	if (jump) flg |= PROJECT_JUMP;
  
	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];
  
	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
      
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
  
	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, ty, tx, dam, typ, flg, 0, 0));
}


/*
 * Fire a sphere, defined as a ball spell that does not lose strength with 
 * distance from the center, up to a given diameter.  This spell is most 
 * often used to cast balls centered on the player with diameter 20, because 
 * it then offers "what you see is what you get" damage to adjacent monsters.
 * It could also be used to cast pinpoints of extremely intense energy (use 
 * a diameter of 5 or even less) more "realistically" than ball spells of 
 * radius 0. -LM-
 */
bool fire_sphere(int typ, int dir, int dam, int rad, byte diameter_of_source)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int ty, tx;
  
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
  
	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];
  
	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
      
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
  
	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, ty, tx, dam, typ, flg, 0, diameter_of_source));
}


/*
 * Fire a cloud, defined as a ball spell that effects the player. -LM-
 */
bool fire_cloud(int typ, int dir, int dam, int rad)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int ty, tx;
  
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | 
		PROJECT_PLAY;
  
	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];
  
	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
      
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
  
	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(-1, rad, ty, tx, dam, typ, flg, 0, 0));
}

/*
 * Cast a meteor spell, defined as a ball spell cast by an arbitary monster, 
 * player, or outside source, that starts out at an arbitrary location, and 
 * that leaves no trail from the "caster" to the target.  This function is 
 * especially useful for bombardments and similar. -LM-
 *
 * Option to hurt the player.
 */
bool fire_meteor(int who, int typ, int y, int x, int dam, int rad, bool hurt_player)
{
        int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP;

        if (hurt_player) flg |= PROJECT_PLAY;

  
	/* Analyze the "target" and the caster. */
	return (project(who, rad, y, x, dam, typ, flg, 0, 0));
}


/*
 * Cast an arc-shaped spell.  This is nothing more than a sphere spell 
 * centered on the caster with a value for degrees_of_arc (how many degrees 
 * wide the the arc is) that is not 360.  The direction given will be the 
 * center of the arc, which travels outwards from the caster to a distance 
 * given by rad. -LM-
 *
 * Because all arcs start out as being one grid wide, arc spells with a 
 * value for degrees_of_arc less than (roughly) 60 do not dissipate as 
 * quickly.  In the extreme case where degrees_of_arc is 0, the arc is 
 * actually a defined length beam, and loses no strength at all over the 
 * ranges found in the game.
 */
bool fire_arc(int typ, int dir, int dam, int rad, int degrees_of_arc)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	/* Diameter of source of energy is normally, but not always, 20. */
	int diameter_of_source = 20;
  
	int ty, tx;
  
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_ARC;
  
  
	/* If a full circle is asked for, just cast a ball spell and have done. */
	if (degrees_of_arc >= 360) return (fire_sphere(typ, 0, dam, rad, 20));
  
  
	/* Use the given direction */
	ty = py + 99 * ddy[dir];
	tx = px + 99 * ddx[dir];
  
	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
      
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
  
	/* Calculate the effective diameter of the energy source, if necessary. */
	if (degrees_of_arc < 60)
	{
		if (degrees_of_arc == 0) diameter_of_source = rad * 10;
		else diameter_of_source = diameter_of_source * 60 / degrees_of_arc;
	}
  
	/* Max */
	if (diameter_of_source > 250) diameter_of_source = 250;
  
	/* Analyze the "dir" and the "target".  Use the given degrees of arc, 
	 * and the calculated source diameter.
	 */
	return (project(-1, rad, ty, tx, dam, typ, flg, degrees_of_arc, 
			(byte)diameter_of_source));
}



/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
static bool project_hook(int typ, int dir, int dam, int flg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int ty, tx;
  
	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);
  
	/* Use the given direction */
	ty = py + ddy[dir];
	tx = px + ddx[dir];
  
	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;
	}
  
	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(-1, 0, ty, tx, dam, typ, flg, 0, 0));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	int flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}

/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (rand_int(100) < prob)
	{
		return (fire_beam(typ, dir, dam));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}


/*
 * Some of the old functions
 */

bool lite_line(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(4, 5), flg));
}

bool drain_life(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}

bool wall_to_mud(int dir)
{
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint(30), flg));
}

bool wall_to_mud_hack(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, dam, flg));
}

bool destroy_door(int dir)
{
	int flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}

/* This function now casts a radius 0 ball spell on the adjacent square 
 * in whatever direction has been chosen. -LM-
 */
bool disarm_trap(int dir)
{
	/* Use the given direction */
	int ty = p_ptr->py + ddy[dir];
	int tx = p_ptr->px + ddx[dir];
  
	int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM;
  
	return (project(-1, 0, ty, tx, 0, GF_KILL_TRAP, flg, 0, 0));
  
}

bool heal_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_HEAL, dir, damroll(4, 6), flg));
}

bool speed_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg));
}

bool slow_monster(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLOW, dir, dam, flg));
}

bool sleep_monster(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_SLEEP, dir, dam, flg));
}

bool confuse_monster(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CONF, dir, dam, flg));
}

bool poly_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg));
}

bool clone_monster(int dir)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_OLD_CLONE, dir, 0, flg));
}

bool fear_monster(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_TURN_ALL, dir, dam, flg));
}

bool dispel_an_undead(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DISP_UNDEAD, dir, dam, flg));
}

bool dispel_a_demon(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DISP_DEMON, dir, dam, flg));
}

bool dispel_a_dragon(int dir, int dam)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_DISP_DRAGON, dir, dam, flg));
}

bool teleport_monster(int dir, int dist)
{
	int flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, dist, flg));
}



/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */

bool door_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_MAKE_DOOR, flg, 0, 0));
}

bool trap_creation(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_MAKE_TRAP, flg, 0, 0));
}

bool destroy_doors_touch(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(-1, 1, py, px, 0, GF_KILL_DOOR, flg, 0, 0));
}

bool sleep_monsters_touch(int dam)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
  
	int flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(-1, 1, py, px, dam, GF_OLD_SLEEP, flg, 0, 0));
}


