#define DUNGEON_C
/* File: dungeon.c */

/* Purpose: Angband game engine */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define TY_CURSE_CHANCE 100
#define CHAINSWORD_NOISE 100


/*
 * Return what o_ptr->ident would be if a monster had just failed to pick
 * it up.
 */
u16b ident_power(object_type *o_ptr)
{
	/* Clear the current flag. */
	u16b out = o_ptr->ident & ~(IDENT_POWER_ALL);

	/* Paranoia */
	if (o_ptr->ident & IDENT_MENTAL);

	/* Set the appropriate flag. */
	else if (object_known_p(o_ptr)) out |= IDENT_POWER_KNOWN;
	else if (object_aware_p(o_ptr)) out |= IDENT_POWER_AWARE;
	else out |= IDENT_POWER_UNAWARE;
	
	return out;
}

/*
 * Test whether an object has been observed to have special powers which
 * could not hav e been expected based on its current level of identification.
 */
static bool is_powerful(object_type *o_ptr)
{
	object_type t_ptr[1],u_ptr[1];

	/* The player knows everything about *identified* items. */
	if (o_ptr->ident & IDENT_MENTAL) return FALSE;

	/* Items which a monster has failed to pick up are flagged as powerful.
	 * If the current flag is the same as the flag it would have if a monster
	 * did so now, the item is powerful. */
	if (ident_power(o_ptr) == o_ptr->ident) return TRUE;

	/* If the player hasn't tried it, there's no other way to know its
	 * powers. */
	if (~o_ptr->ident & IDENT_TRIED) return FALSE;

	/* Check whether trying it added to its known powers. Only check flags,
	 * as there is expected to be a pval based on the flags even if it isn't
	 * known, and everything else should be the same. */
	object_info_known(t_ptr, o_ptr, 0);
	o_ptr->ident &= ~IDENT_TRIED;
	object_info_known(u_ptr, o_ptr, 0);
	o_ptr->ident |= IDENT_TRIED;
	if (t_ptr->flags1 != u_ptr->flags1) return TRUE;
	if (t_ptr->flags2 != u_ptr->flags2) return TRUE;
	if (t_ptr->flags3 != u_ptr->flags3) return TRUE;

	return FALSE;
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static cptr value_check_aux1(object_type *o_ptr)
{

	/* Artifacts */
    if (allart_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return "terrible";

		/* Normal */
		return "special";
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return "worthless";

		/* Normal */
		return "excellent";
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return "cursed";

	/* Broken items */
	if (broken_p(o_ptr)) return "useless";

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return "good";

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return "good";

	/* Default to "average" */
	return "average";
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static cptr value_check_aux2(object_type *o_ptr)
{
	bool powerful = is_powerful(o_ptr);

	/* Cursed items (all of them) */
	/* Should this be "very cursed", or would that just be confusing? */
	if (cursed_p(o_ptr)) return (powerful) ? "very bad" : "cursed";

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return "useless";

	/* Artifacts -- except cursed/broken ones */
	/* Ego-Items -- except cursed/broken ones */
	/* Good armor bonus */
	/* Good weapon bonuses */
    if (allart_p(o_ptr) || ego_item_p(o_ptr) || (o_ptr->to_a > 0) ||
		(o_ptr->to_h + o_ptr->to_d > 0))
		return (powerful) ? "very good" : "good";

	/* No feeling */
	return (powerful) ? "powerful" : "";
}





/*
 *  Return an appropriate "feeling" for an object
 */
cptr find_feeling(object_type *o_ptr)
{
	/* Some feelings that don't depend on sensing, but on trying. */
	if (!object_known_p(o_ptr) && (o_ptr->ident & (IDENT_EMPTY)))
		return "empty";
	/* Items in stores never return a feeling. */
	else if (o_ptr->ident & IDENT_STORE) return "";
	/* Hack - wearable items become "poss. cursed", usable ones "tried"*/
	else if (!object_aware_p(o_ptr) && object_tried_p(o_ptr))
	{
		switch (o_ptr->tval)
		{
		/* Food, potions, scrolls, staves, wands and rods can be tried. */
			case TV_FOOD: case TV_POTION: case TV_SCROLL:
			case TV_ROD: case TV_WAND: case TV_STAFF:
		return "tried";
		/* Amulets and rings can have k_idx-specific curses... */
			case TV_RING: case TV_AMULET:
		/* ... as might weapons and armour, at least in theory... */
			case TV_DRAG_ARMOR: case TV_HARD_ARMOR: case TV_SOFT_ARMOR:
			case TV_CLOAK: case TV_SHIELD: case TV_CROWN: case TV_HELM:
			case TV_GLOVES: case TV_BOOTS: case TV_SWORD: case TV_POLEARM:
			case TV_HAFTED: case TV_DIGGING: case TV_BOW:
		/* ... but this should only be mentioned if the player doesn't know better. */
			if (!(o_ptr->ident & IDENT_SENSE_CURSED))
				return "poss. cursed";
			else
				break;
		/* Nothing should get here, but... */
			default:
				return "buggy";
		}
	}

	switch (o_ptr->ident & IDENT_SENSE)
	{
		case IDENT_SENSE:
			/* Fall through if you know what it is, but catch the BROKEN flag here. */
			if (!object_known_p(o_ptr) || broken_p(o_ptr))
			{
				if (o_ptr->ident & IDENT_SENSE_HEAVY)
					return value_check_aux1(o_ptr);
				else
					return value_check_aux2(o_ptr);
			}
		case IDENT_SENSE_CURSED:
		{
			bool powerful = is_powerful(o_ptr);

			if (o_ptr->ident & IDENT_CURSED)
				return (powerful) ? "very bad" : "cursed";
			else
				return (powerful) ? "powerful" : "uncursed";
		}
		/* IDENT_SENSE_VALUE is only set alone by an attempt to break an
		 * artefact, so has few plausible values. */
		case IDENT_SENSE_VALUE:
			if (allart_p(o_ptr))
				/* Artefacts */
				return "unbreakable";
			else if (o_ptr->ident & IDENT_BROKEN)
				/* Artefacts which have been shattered */
				return "useless";
			else
				/* This should never happen, but... */
				return "Strangely normal";
		default:
		{
			bool powerful = is_powerful(o_ptr);
			return (powerful) ? "powerful" : "";
		}
	}
}

/*
 * A function for "(rand_range(min, max) < x)" which only calls rand_int()
 * where necessary.
 */
static bool rand_range_test(u32b min, u32b max, u32b x)
{
	if (x <= min) return FALSE;
	else if (x > max) return TRUE;
	else if (rand_range(min, max) < x) return TRUE;
	else return FALSE;
}


/*
 * Sense the inventory
 */
static void sense_inventory(void)
{
	int		i;

	int		plev = skill_set[SKILL_PSEUDOID].value;

	bool	heavy = FALSE;

	cptr	feel;

	object_type *o_ptr;

	C_TNEW(o_name, ONAME_MAX, char);


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused ||

	/* Okay sensing for everyone*/
		0 != rand_int(80000L / (plev * plev + 160)))
	{
		TFREE(o_name);
		return;
	}

	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		bool okay = FALSE, repeat;
		u16b oldident;

		/* Pseudo-id heavily at 50+, lightly at 30- and sometimes do either
		 * in between. */
		heavy = rand_range_test(30, 49, skill_set[SKILL_PSEUDOID].value);

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Valid "tval" codes */
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
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip non-sense machines */
		if (!okay) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* If it has been identified, or pseudo-identified at least as
		heavily as it is being now, don't bother. */
		if ((o_ptr->ident & (IDENT_KNOWN)) || ((o_ptr->ident & IDENT_SENSE)
		&& ((o_ptr->ident & IDENT_SENSE_HEAVY) || (!heavy)))) continue;

		/* Get an object description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Remember how things used to be */
		oldident = o_ptr->ident;
		feel = find_feeling(o_ptr);

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Remember if we "feel" it completely. */
		if (heavy) o_ptr->ident |= IDENT_SENSE_HEAVY;

		/* Check if the feeling has been changed by this process */
		repeat = (!strcmp(feel, find_feeling(o_ptr)));

		/* And then insert the new version */
		if (!repeat)
		feel = find_feeling(o_ptr);

		/* Skip non-feelings */
		if (feel == "")
		{
			o_ptr->ident = oldident;
			continue;
		}

		/* Skip repeated feelings */
		if (streq(feel, quark_str(o_ptr->note))) continue;
		
		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Message */
		msg_format("You feel the %s (%c) %s %s%s %s...",
			           o_name, index_to_label(i),
		(i >= INVEN_WIELD) ? format("you are %s", describe_use(i)) : "in your pack",
		(repeat ? "really " : ""), ((o_ptr->number == 1) ? "is" : "are"), feel);

		/* Get a bit better (this does allow objects to boost skill twice
		 * in the 21-59 range). */
		if (strcmp(feel, "average")) skill_exp(SKILL_PSEUDOID);

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
	TFREE(o_name);
}

/*
 * Change level, setting various relevant things.
 */
void change_level(s16b new_level, byte come_from)
{
	/* Display any pending text. */
	msg_print(NULL);

	/* Save if desired. */
	if (autosave_l)
	{
		do_cmd_save_game(TRUE);
	}

	/* Try to recognise when the player wants stairs next to him. */
	if (come_from == START_STAIRS && new_level && new_level != dun_level)
	{
		if ((new_level > dun_level) ^ dun_defs[cur_dungeon].tower)
			create_up_stair = TRUE;
		else
			create_down_stair = TRUE;
	}
	
	/* Set the means of entry. */
	came_from = come_from;

	/* Set the level. */
	dun_level = new_level;

	/* Change level. */
	new_level_flag = TRUE;
}
/*
 * Go to any level (ripped off from wiz_jump)
 */

static void pattern_teleport(void)
{
	char highestquest;
	
	/* Ask for level */
    if (get_check("Teleport level? "))
	{
		char	ppp[80];

		char	tmp_val[160];
		int i;
		
		highestquest=99;

		for (i = 0; i < MAX_Q_IDX; i++)
		{
			if ((q_list[i].level || (q_list[i].cur_num != q_list[i].max_num)) && (q_list[i].level < highestquest))   highestquest = q_list[i].level;
		}
		if(highestquest > dun_defs[cur_dungeon].max_level) highestquest = dun_defs[cur_dungeon].max_level;
		/* Prompt */
        sprintf(ppp, "Teleport to level (0-%d): ", highestquest);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);
	}
    else if (get_check("Normal teleport? "))
    {
        teleport_player(200);
        return;
    }
    else
    {
        return;
    }

	/* Paranoia */
	if (command_arg < 1) command_arg = 1;

	/* Paranoia */
    if (command_arg > highestquest) command_arg = highestquest;

	/* Accept request */
    msg_format("You teleport to dungeon level %d.", command_arg);

	/* Change level */
	change_level(command_arg, START_RANDOM);
}


static void wreck_the_pattern(void)
{
    int to_ruin = 0, r_y, r_x;

    if (cave[py][px].feat == FEAT_PATTERN_XTRA2)
    {
        /* Ruined already */
        return;
    }

    msg_print("You bleed on the Pattern!");
    msg_print("Something terrible happens!");

    if (!(p_ptr->invuln))
        take_hit(damroll(10,8), "corrupting the Pattern");

    to_ruin = randint(45) + 35;
    while (to_ruin--)
    {
        scatter(&r_y, &r_x, py, px, 4, 0);
        if ((cave[r_y][r_x].feat >= FEAT_PATTERN_START)
            && (cave[r_y][r_x].feat < FEAT_PATTERN_XTRA2))
        {
                    cave_set_feat(r_y, r_x, FEAT_PATTERN_XTRA2);
        }
    }
    cave_set_feat(py, px, FEAT_PATTERN_XTRA2);

}


/* Returns TRUE if we are on the Pattern... */
static bool pattern_effect(void)
{
    if ((cave[py][px].feat < FEAT_PATTERN_START)
        || (cave[py][px].feat > FEAT_PATTERN_XTRA2))
        return FALSE;


    if ((p_ptr->prace == RACE_GREAT) && (p_ptr->cut>0) &&
        (randint(10)==1))
        {
            wreck_the_pattern();
        }

    if (cave[py][px].feat == FEAT_PATTERN_END)
    {

                    (void)set_poisoned(0);
                    (void)set_image(0);
                    (void)set_stun(0);
                    (void)set_cut(0);
                    (void)set_blind(0);
                    (void)set_afraid(0);
                    (void)do_res_stat(A_STR);
                    (void)do_res_stat(A_INT);
                    (void)do_res_stat(A_WIS);
                    (void)do_res_stat(A_DEX);
                    (void)do_res_stat(A_CON);
                    (void)do_res_stat(A_CHR);
                    (void)restore_level();
                    (void)hp_player(1000);
                    cave_set_feat(py, px, FEAT_PATTERN_OLD);
                    msg_print("This section of the Pattern looks less powerful.");
     }


    /* We could make the healing effect of the
    Pattern center one-time only to avoid various kinds
    of abuse, like luring the win monster into fighting you
    in the middle of the pattern... */

     else if (cave[py][px].feat == FEAT_PATTERN_OLD)
     {
        /* No effect */
     }
     else if (cave[py][px].feat == FEAT_PATTERN_XTRA1)
     {
                            pattern_teleport();
     }
     else if (cave[py][px].feat == FEAT_PATTERN_XTRA2)
     {
        if (!(p_ptr->invuln))
            take_hit(200, "walking the corrupted Pattern");
     }

     else
     {
        if ((p_ptr->prace == RACE_GREAT) && (randint(2)!=1))
            return TRUE;
        else
            if (!(p_ptr->invuln))
                take_hit(damroll(1,3),
                    "walking the Pattern");
     }

    return TRUE;


}





/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	s32b        new_chp, new_chp_frac;
	int                   old_chp;

	/* Save the old hitpoints */
	old_chp = p_ptr->chp;

	/* Extract the new hitpoints */
	new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += (short)(new_chp >> 16);   /* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
	if (new_chp_frac >= 0x10000L)
	{
		p_ptr->chp_frac = (short)(new_chp_frac - 0x10000L);
		p_ptr->chp++;
	}
	else
	{
		p_ptr->chp_frac = (short)new_chp_frac;
	}

	/* Fully healed */
	if (p_ptr->chp >= p_ptr->mhp)
	{
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;
	}

	/* Notice changes */
	if (old_chp != p_ptr->chp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}
}


/*
 * Regenerate mana points				-RAK-
 * Also regenerate chi points
 */
static void regenmana(int percent)
{
	s32b        new_mana, new_mana_frac;
	s32b		new_chi,new_chi_frac;
	int                   old_csp,old_chi;

	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += (short)(new_mana >> 16);	/* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = (short)(new_mana_frac - 0x10000L);
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = (short)new_mana_frac;
	}

	/* Must set frac to zero even if equal */
	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	old_chi = p_ptr->cchi;
	new_chi = ((long)p_ptr->mchi) * percent + PY_REGEN_MNBASE;
	p_ptr->cchi += (short)(new_chi >> 16);	/* div 65536 */
	/* check for overflow */
	if ((p_ptr->cchi < 0) && (old_chi > 0))
	{
		p_ptr->cchi = MAX_SHORT;
	}
	new_chi_frac = (new_chi & 0xFFFF) + p_ptr->chi_frac;	/* mod 65536 */
	if (new_chi_frac >= 0x10000L)
	{
		p_ptr->chi_frac = (short)(new_chi_frac - 0x10000L);
		p_ptr->cchi++;
	}
	else
	{
		p_ptr->chi_frac = (short)new_chi_frac;
	}

	/* Must set frac to zero even if equal */
	if (p_ptr->cchi >= p_ptr->mchi)
	{
		p_ptr->cchi = p_ptr->mchi;
		p_ptr->chi_frac = 0;
	}

	/* Redraw mana */
	if ((old_csp != p_ptr->csp) || (old_chi != p_ptr->cchi))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_PLAYER);
	}
}






/*
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_monsters(void)
{
	int i, frac;


    /* Regenerate everyone */
	for (i = 1; i < m_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];



		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Allow regeneration (if needed) */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			/* Hack -- Base regeneration */
			frac = m_ptr->maxhp / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;


			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (health_who == i) p_ptr->redraw |= (PR_HEALTH);
		}
	}
}


/*
 * Forcibly pseudo-identify an object in the inventory
 * (or on the floor)
 * 
 * note: currently this function allows pseudo-id of any object,
 * including silly ones like potions & scrolls, which always
 * get '{average}'. This should be changed, either to stop such
 * items from being pseudo-id'd, or to allow psychometry to
 * detect whether the unidentified potion/scroll/etc is
 * good (Cure Light Wounds, Restore Strength, etc) or
 * bad (Poison, Weakness etc) or 'useless' (Slime Mold Juice, etc).
 */
bool psychometry(void)
{
	int                     item;

	object_type             *o_ptr;

	C_TNEW(o_name, ONAME_MAX, char);
        cptr            feel, oldfeel;

	/* Get an item (from equip or inven or floor) */
    if (!get_item(&item, "Meditate on which item? ", TRUE, TRUE, TRUE))
	{
        if (item == -2) msg_print("You have nothing appropriate.");
		TFREE(o_name);
		return (FALSE);
	}

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

	/* It is fully known, no information needed. */
	if ((object_known_p(o_ptr)) || (o_ptr->ident & IDENT_SENSE_HEAVY))
	{
		msg_print("You cannot find out anything more about that.");
		TFREE(o_name);
		return TRUE;
	}

	/* Remember the previous feeling */
	oldfeel = find_feeling(o_ptr);

	/* We will have "felt" it */
	o_ptr->ident |= (IDENT_SENSE);

	/* We will have "felt" it heavily */
	o_ptr->ident |= (IDENT_SENSE_HEAVY);

    /* Check for a feeling */
	feel = find_feeling(o_ptr);

    /* Get an object description */
    object_desc(o_name, o_ptr, FALSE, 0);
    
    /* Skip non-feelings */
    if (feel == "") {
    	msg_format("You do not perceive anything unusual about the %s.",
			o_name);
		TFREE(o_name);
		return TRUE;
    }

	msg_format("You feel that the %s %s%s %s...",
	o_name, (strcmp(feel, oldfeel) ? "" : "really "),((o_ptr->number == 1) ? "is" : "are"), feel);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	TFREE(o_name);

    /* Something happened */
    return (TRUE);
}




/*
 * Lightly curse something specific.
 */
void curse(object_type *o_ptr)
{
	o_ptr->ident |= IDENT_CURSED;
	o_ptr->flags3 |= TR3_CURSED;
	o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);
	if (streq(quark_str(o_ptr->note), "uncursed"))
	{
		o_ptr->note = 0;
	}
}

/*
 * If player has inscribed the object with "!!", let him know when it's 
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[120];

	cptr s;

	/* No inscription */
	if (!o_ptr->note) return;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			cptr verb = (o_ptr->number == 1) ? "is" : "are";
			cptr gen = (allart_p(o_ptr)) ? "The" : "Your";

			/* Describe (briefly) */
			object_desc(o_name, o_ptr, FALSE, 0);

			/* Notify the player */
			msg_format("%s %s %s recharged.", gen, o_name, verb);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = strchr(s + 1, '!');
	}
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int		x, y, i, j;

	int		regen_amount;
    bool    cave_no_regen = FALSE;
    int     upkeep_factor = 0;
	s16b day;

	cave_type		*c_ptr;

	object_type		*o_ptr;
    u32b            f1 = 0 , f2 = 0 , f3 = 0;



	/* Every 10 game turns */
	if (turn % 10) return;

	/* Redraw Time */
	p_ptr->redraw |= PR_TIME;

	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{
		/* Check time and load */
		if ((0 != check_time()) || (0 != check_load()))
		{
			/* Warning */
			if (closing_flag <= 2)
			{
				/* Disturb */
				disturb(0, 0);

				/* Count warnings */
				closing_flag++;

				/* Message */
				msg_print("The Gates of Slumber are closing...");
				msg_print("Please finish up and/or save your game.");
			}

			/* Slam the gate */
			else
			{
				/* Message */
				msg_print("The Gates of Slumber are now closed.");

				/* Stop playing */
				alive = FALSE;
			}
		}
	}

    /*** Attempt timed autosave ***/
    if (autosave_t && autosave_freq)
    {
        if (!(turn % ((s32b) autosave_freq * 10 )))
        {
	   do_cmd_save_game(TRUE);
        }
    }

	/* Handle special dates */
	if ((turn % (10L* TOWN_DAWN)) == ((10L*TOWN_DAWN)/4))
	{
		/* It's Noon */
		/* Work out day */
		if (turn <= 3*(10L * TOWN_DAWN)/4)
		{
			day = 1;
		}
		else
		{
			day = (turn - 3*(10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 2;
		}
		day += p_ptr->startdate;

		if (YEARDAY(p_ptr->birthday) == YEARDAY(day))
		{
			/* It's your birthday... */
			msg_print("Happy Birthday!");

			/* Those born on 29th Feb. get lots of presents */
			if (YEARDAY(day) == 59)
				acquirement(py, px, damroll(3,4), TRUE);
			else
				acquirement(py, px, 1+randint(2), TRUE);
			p_ptr->age++;
		}

		/* You don't appear to have a birthday this year... */
		else if (YEARDAY(p_ptr->birthday) < YEARDAY(day-1) && YEARDAY(p_ptr->birthday) > YEARDAY(day))
		{
			p_ptr->age++;
		}
	}
	if ((turn % (10L* TOWN_DAWN)) == ((30L*TOWN_DAWN)/4))
	{
		/* It's Midnight */
		/* Work out day */
		if (turn <= 3*(10L * TOWN_DAWN)/4)
		{
			day = 1;
		}
		else
		{
			day = (turn - 3*(10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 2;
		}
		day += p_ptr->startdate;

		/* Specials on some nights of the year */
		switch (YEARDAY(day))
		{
		case 0: /* January 1st */
			{
				msg_print("Happy New Year!");
				acquirement(py,px,randint(2)+1,FALSE);
				break;
			}
		case 305: /* November 1st (Night of October 31st) */
			{
				msg_print("All Hallows Eve and the ghouls come out to play...");
				summon_specific(py,px,dun_depth,SUMMON_UNDEAD);
				break;
			}
		default: /* Any other night */
			{
				break;
			}
		}
	}
	

	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
	if (dun_level <= 0)
	{
		/* Hack -- Daybreak/Nighfall in town */
		if (!(turn % ((10L * TOWN_DAWN) / 2)))
		{
			bool dawn;

			/* Check for dawn */
			dawn = (!(turn % (10L * TOWN_DAWN)));

			/* Day breaks */
			if (dawn)
			{
				/* Message */
				msg_print("The sun has risen.");

				/* Disturb */
				if (disturb_dawn) disturb(0,0);

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Assume lit */
						c_ptr->info |= (CAVE_GLOW);

						/* Hack -- Memorize lit grids if allowed */
						if (view_perma_grids) c_ptr->info |= (CAVE_MARK);

						/* Hack -- Notice spot */
						note_spot(y, x);
					}
				}
			}

			/* Night falls */
			else
			{
				/* Message */
				msg_print("The sun has fallen.");

				/* Disturb */
				if (disturb_dawn) disturb(0,0);

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Darken "boring" features */
						if (c_ptr->feat <= FEAT_INVIS)
						{
							/* Forget the grid */
							c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

							/* Hack -- Notice spot */
							note_spot(y, x);
						}
					}
				}
			}

			/* Update the monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);
		}
	}


	/* While in the dungeon */
	else
	{
		/*** Update the Stores ***/

		/* Update the stores once a day (while in dungeon) */
		if (!(turn % (10L * STORE_TURNS)))
		{
			int n;

			/* Message */
			if (cheat_xtra) msg_print("Updating Shops...");

			/* Maintain each shop */
			for (n = 0; n < MAX_STORES_TOTAL; n++)
			{
				/* Ignore home, hall  and pawnbrokers */
				if ((store[n].type != 99) &&
					(store[n].type != STORE_HOME) &&
					(store[n].type != STORE_HALL) &&
					(store[n].type != STORE_PAWN))
				{
					/* Maintain */
					store_maint(n);
				}
			}

			/* Sometimes, shuffle the shop-keepers */
			if (rand_int(STORE_SHUFFLE) == 0)
			{
				/* Message */
				if (cheat_xtra) msg_print("Shuffling a Shopkeeper...");

				/* Shuffle a random shop */
				store_shuffle(rand_int(MAX_STORES_TOTAL));
			}

			/* Message */
			if (cheat_xtra) msg_print("Done.");
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation */
	if ((full_grid > MAX_SIGHT + 5) && (rand_int(MAX_M_ALLOC_CHANCE) == 0))
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
    if ((p_ptr->poisoned) && !(p_ptr->invuln))
	{
		/* Take damage */
		take_hit(1, "poison");
	}





       /* (Vampires) Take damage from sunlight */
   if (p_ptr->prace == RACE_VAMPIRE)

    {
       if ((dun_level <= 0)
           && (!(p_ptr->resist_lite)) && !(p_ptr->invuln)
           && (!((turn / ((10L * TOWN_DAWN)/2)) % 2)))
       {
           if (cave[py][px].info & (CAVE_GLOW))
           {
            /* Take damage */
            msg_print("The sun's rays scorch your undead flesh!");
            take_hit(1, "sunlight");
            cave_no_regen = TRUE;
           }
       }

       if ((allart_p(&inventory[INVEN_LITE]))
            && !(p_ptr->resist_lite))
        {
            object_type * o_ptr = &inventory[INVEN_LITE];
            char o_name [80];
            char ouch [80];

            /* Get an object description */
            object_desc(o_name, o_ptr, FALSE, 0);


            msg_format("The %s scorches your undead flesh!", o_name);

            cave_no_regen = TRUE;

            /* Get an object description */
            object_desc(o_name, o_ptr, TRUE, 0);

            sprintf(ouch, "wielding %s", o_name);
            if (!(p_ptr->invuln))
                take_hit(1, ouch);
        }
    }

         /* Spectres -- take damage when moving through walls */
         /* Added: ANYBODY takes damage if inside through walls
            without wraith form -- NOTE: Spectres will never be
            reduced below 0 hp by being inside a stone wall; others
            WILL BE!  */

       if ((!cave_floor_bold(py, px)) && (cave[py][px].feat != FEAT_BUSH))  {

       cave_no_regen = TRUE;
       if (!(p_ptr->invuln)
        && !(p_ptr->wraith_form) && ((p_ptr->chp > ((skill_set[SKILL_TOUGH].value)/10))
                                    || (p_ptr->prace != RACE_SPECTRE)))
        {
            cptr dam_desc;
           if (p_ptr->prace == RACE_SPECTRE)
           {
               msg_print("Your body feels disrupted!");
               dam_desc = "density";

            }
            else
            {
                msg_print("You are being crushed!");
                dam_desc = "solid rock";
            }

               take_hit(1 + ((skill_set[SKILL_TOUGH].value)/10), dam_desc);
        }

   }


	/* Take damage from cuts */
    if ((p_ptr->cut) && !(p_ptr->invuln))
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->cut > 200)
		{
			i = 3;
		}

		/* Severe cut */
		else if (p_ptr->cut > 100)
		{
			i = 2;
		}

		/* Other cuts */
		else
		{
			i = 1;
		}

		/* Take damage */
		take_hit(i, "a fatal wound");
	}


	/*** Check the Food, and Regenerate ***/

#ifdef ALLOW_WIZARD
	/* Hack - for a wizard, amulets of adornment become amulets of no digestion */
	if (cheat_wzrd && inventory[INVEN_NECK].k_idx == OBJ_AMULET_ADORNMENT);
	else
#endif
	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate no longer based on speed */
			i = 20;

			/* Regeneration takes more food */
			if (p_ptr->regenerate) i += 30;

			/* Slow digestion takes less food */
			if (p_ptr->slow_digest) i -= 10;

			/* Minimal digestion */
			if (i < 1) i = 1;

			/* Digest some food */
			(void)set_food(p_ptr->food - i);
		}
	}

	/* Digest quickly when gorged */
	else
	{
		/* Digest a lot of food */
		(void)set_food(p_ptr->food - 100);
	}

	/* Starve to death (slowly) */
	if (p_ptr->food < PY_FOOD_STARVE)
	{
		/* Calculate damage */
		i = (PY_FOOD_STARVE - p_ptr->food) / 10;

		/* Take damage */
        if (!(p_ptr->invuln))
            take_hit(i, "starvation");
	}

	/* Default regeneration */
	regen_amount = PY_REGEN_NORMAL;

	/* Getting Weak */
	if (p_ptr->food < PY_FOOD_WEAK)
	{
		/* Lower regeneration */
		if (p_ptr->food < PY_FOOD_STARVE)
		{
			regen_amount = 0;
		}
		else if (p_ptr->food < PY_FOOD_FAINT)
		{
			regen_amount = PY_REGEN_FAINT;
		}
		else
		{
			regen_amount = PY_REGEN_WEAK;
		}

		/* Getting Faint */
		if (p_ptr->food < PY_FOOD_FAINT)
		{
			/* Faint occasionally */
			if (!p_ptr->paralyzed && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
			}
		}
	}



    /* Are we walking the pattern? */

    if (pattern_effect())
    {
        cave_no_regen = TRUE;
    }
    else
    {
    /* Regeneration ability */
       if (p_ptr->regenerate)
       {
                   regen_amount = regen_amount * 2;
       }
    }


	/* Sneaking or Resting */
	if (p_ptr->sneaking || resting)
	{
		regen_amount = regen_amount * 2;
	}

    if (total_friends)
    {
        int upkeep_divider = 20;

#ifdef TRACK_FRIENDS
        if (cheat_wzrd)
        msg_format("Total friends: %d.", total_friends);
#endif
        if (total_friends > 1 + (skill_set[SKILL_RACIAL].value / (upkeep_divider*2)))
        {
            upkeep_factor = (total_friend_levels);

            if (upkeep_factor > 100) upkeep_factor = 100;
            else if (upkeep_factor < 10) upkeep_factor = 10;

#ifdef TRACK_FRIENDS
        if (cheat_wzrd)
        msg_format("Levels %d, upkeep %d", total_friend_levels, upkeep_factor);
#endif
        }
    }

	/* Regenerate the mana */
	if ((p_ptr->csp < p_ptr->msp) || (p_ptr->cchi < p_ptr->mchi))
	{
        if (upkeep_factor)
        {
            s16b upkeep_regen = (((100 - upkeep_factor) * regen_amount) / 100);
            regenmana(upkeep_regen);

#ifdef TRACK_FRIENDS
            if (cheat_wzrd)
            msg_format("Regen: %d/%d", upkeep_regen,
                                        regen_amount);
#endif
        }
        else
        {
            regenmana(regen_amount);
        }
	}

	/* Poisoned or cut yields no healing */
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

    /* Special floor -- Pattern, in a wall -- yields no healing */
    if (cave_no_regen) regen_amount = 0;

	/* Regenerate Hit Points if needed */
    if ((p_ptr->chp < p_ptr->mhp) && !(cave_no_regen))
	{
        if ((cave[py][px].feat < FEAT_PATTERN_END) &&
            (cave[py][px].feat >= FEAT_PATTERN_START))
            regenhp(regen_amount / 5); /* Hmmm. this should never happen? */
        else
            regenhp(regen_amount);
	}


	/*** Timeout Various Things ***/

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - 1);
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1);
	}

    if (multi_rew)
    {
        multi_rew = FALSE;
    }

    /* Timed esp */
    if (p_ptr->tim_esp)
	{
        (void)set_tim_esp(p_ptr->tim_esp - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		(void)set_paralyzed(p_ptr->paralyzed - 1);
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		(void)set_confused(p_ptr->confused - 1);
	}

	/* Afraid */
	if (p_ptr->afraid)
	{
		(void)set_afraid(p_ptr->afraid - 1);
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void)set_fast(p_ptr->fast - 1);
	}

	/* Slow */
	if (p_ptr->slow)
	{
		(void)set_slow(p_ptr->slow - 1);
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		(void)set_protevil(p_ptr->protevil - 1);
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		(void)set_invuln(p_ptr->invuln - 1);
	}

    /* Wraith form */
    if (p_ptr->wraith_form)
	{
        (void)set_shadow(p_ptr->wraith_form - 1);
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		(void)set_hero(p_ptr->hero - 1);
	}

	/* Super Heroism */
	if (p_ptr->shero)
	{
		(void)set_shero(p_ptr->shero - 1);
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		(void)set_blessed(p_ptr->blessed - 1);
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void)set_shield(p_ptr->shield - 1);
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		(void)set_oppose_acid(p_ptr->oppose_acid - 1);
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		(void)set_oppose_elec(p_ptr->oppose_elec - 1);
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		(void)set_oppose_fire(p_ptr->oppose_fire - 1);
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		(void)set_oppose_cold(p_ptr->oppose_cold - 1);
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		(void)set_oppose_pois(p_ptr->oppose_pois - 1);
	}


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000) adjust = 0;

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}



	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &inventory[INVEN_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LITE)
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!allart_p(o_ptr) && (o_ptr->pval > 0))
		{
			/* Decrease life-span */
			o_ptr->pval--;

			/* Hack -- notice interesting fuel steps */
			if ((o_ptr->pval < 100) || (!(o_ptr->pval % 100)))
			{
				/* Window stuff */
				p_ptr->window |= (PW_EQUIP);
			}

			/* Hack -- Special treatment when blind */
			if (p_ptr->blind)
			{
				/* Hack -- save some light for later */
				if (o_ptr->pval == 0) o_ptr->pval++;
			}

			/* The light is now out */
			else if (o_ptr->pval == 0)
			{
				disturb(0, 0);
				msg_print("Your light has gone out!");
			}

			/* The light is getting dim */
			else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
			{
				if (disturb_minor) disturb(0, 0);
				msg_print("Your light is growing faint.");
			}
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (p_ptr->exp_drain)
	{
		if ((rand_int(5000) < 10) && (p_ptr->exp > 0))
		{
			lose_skills(1);
		}
	}

    /* Rarely, take damage from the Jewel of Judgement */
    if ((randint(999)==1) && !(p_ptr->anti_magic))
    {
        if ((inventory[INVEN_LITE].k_idx == OBJ_GEMSTONE_TRAPEZODEDRON) &&
			!(p_ptr->invuln))
        {
            msg_print("The Jewel of Judgement drains life from you!");
            take_hit(MIN(skill_set[SKILL_TOUGH].value/2, 50), "the Jewel of Judgement");
        }
    }


	/* Process equipment */
	for (j = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

        object_flags(o_ptr, &f1, &f2, &f3);

	/*
	 * Auto-curse
	 * Curse_turn is set in remove_curse_aux()).
	 */
	if (f3 & TR3_AUTO_CURSE && ~(o_ptr->ident) & IDENT_CURSED && turn > curse_turn)
	{
		C_TNEW(o_name, ONAME_MAX, char);
		curse(o_ptr);
		object_desc(o_name, o_ptr, FALSE, 0);
		msg_format("The %s suddenly feels deathly cold!", o_name);
		TFREE(o_name);
	}
        if ((f3 & TR3_TY_CURSE) && (randint(TY_CURSE_CHANCE)==1))
            activate_ty_curse();
        if ((o_ptr->name1 == ART_DEMONBLADE) && randint(CHAINSWORD_NOISE) == 1)
        {
            char noise[80];
            get_rnd_line("chainswd.txt", noise);
            msg_print(noise);
            disturb(FALSE, FALSE);
        }

        /* Hack: Uncursed teleporting items (e.g. Planar Weapons)
        can actually be useful! */

        if ((f3 & TR3_TELEPORT) && (rand_int(100)<1))
        {
            if ((o_ptr->ident & IDENT_CURSED) && !(p_ptr->anti_tele))
            {
                 disturb(0,0);
                /* Teleport player */
                teleport_player(40);
            }
            else
                {
                if (strchr(quark_str(o_ptr->note),'.'))
                    {
                        /* Do nothing */
                        /* msg_print("Teleport aborted.") */ ;
                    }
                else if (get_check("Teleport? "))
                    {
                        disturb(0,0);
                        teleport_player(50);
                    }
                }
        }
	/*** Process chaos feature effects ***/
	if (p_ptr->muta2)
	{
		if ((p_ptr->muta2 & MUT2_BERS_RAGE) && (randint(3000)==1))
		{
			disturb(0,0);
			msg_print("RAAAAGHH!");
			msg_print("You feel a fit of rage coming over you!");
			(void) set_shero(p_ptr->shero + 10 + randint(skill_set[SKILL_RACIAL].value/2));
		}

		if ((p_ptr->muta2 & MUT2_COWARDICE) && (randint(3000)==13))
		{
			if (!(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
			{
				disturb(0,0);
				msg_print("It's so dark... so scary!");
				p_ptr->redraw |= PR_AFRAID;
				p_ptr->afraid = (p_ptr->afraid) + 13 + randint(26);
			}
		}

		if ((p_ptr->muta2 & MUT2_RTELEPORT) && (randint(5000)==88))
		{
			if (!(p_ptr->resist_nexus) && !(p_ptr->muta1 & MUT1_VTELEPORT)
			    && !(p_ptr->anti_tele))
			{
				disturb(0,0);

				/* Teleport player */
				msg_print("Your position suddenly seems very uncertain...");
				msg_print(NULL);
				teleport_player(40);
			}
		}

		if ((p_ptr->muta2 & MUT2_ALCOHOL) && (randint(6400)==321))
		{
			if (!(p_ptr->resist_chaos || p_ptr->resist_conf))
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				msg_print("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");

				if (randint(20)==1)
				{
					msg_print(NULL);
					if (randint(3)==1) lose_all_info();
					else wiz_dark();
					teleport_player(100);
					wiz_dark();
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
				}
				else
				{
					if (!(p_ptr->resist_conf))
					{
						(void)set_confused(p_ptr->confused + rand_int(20) + 15);
					}

					if ((randint(3)==1) && !(p_ptr->resist_chaos))
					{
						msg_print("Thishcischs GooDSChtuff!");
						(void)set_image(p_ptr->image + rand_int(150) + 150);
					}
				}
			}
		}

		if ((p_ptr->muta2 & MUT2_HALLU) && (randint(6400)==42))
		{
			if (!(p_ptr->resist_chaos))
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				(void)set_image(p_ptr->image + rand_int(50) + 20);
			}
		}

		if ((p_ptr->muta2 & MUT2_FLATULENT) && (randint(3000)==13))
		{
			disturb(0,0);

			msg_print("BRRAAAP! Oops.");
			msg_print(NULL);
			fire_ball(GF_POIS, 0, skill_set[SKILL_RACIAL].value/2,3);
		}

		if ((p_ptr->muta2 & MUT2_PROD_MANA) &&
		    (!(p_ptr->anti_magic)) && (randint(9000)==1))
		{
			int dire = 0;
			disturb(0,0);
			msg_print("Magical energy flows through you! You must release it!");
			flush();
			msg_print(NULL);
			(void)get_hack_dir(&dire);
			fire_ball(GF_MANA, dire, skill_set[SKILL_RACIAL].value, 3);
		}

		if ((p_ptr->muta2 & MUT2_ATT_DEMON) &&
		    (!(p_ptr->anti_magic)) && (randint(6666)==666))
		{
			bool d_summon = FALSE;
			if (randint(6)==1)
			{
				d_summon = summon_specific_friendly(py, px,
				    dun_level, SUMMON_DEMON, TRUE);
			}
			else
			{
				d_summon = summon_specific(py, px,
				    dun_level, SUMMON_DEMON);
			}

			if (d_summon)
			{
				msg_print("You have attracted a demon!");
				disturb(0,0);
			}
		}

		if ((p_ptr->muta2 & MUT2_SPEED_FLUX) && (randint(6000)==1))
		{
			
			disturb(0,0);
			if (randint(2)==1)
			{
				msg_print("You feel less energetic.");
				if (p_ptr->fast > 0)
				{
					set_fast(0);
				}
				else
				{
					set_slow(p_ptr->slow + randint(30) + 10);
				}
			}
			else
			{
				msg_print("You feel more energetic.");
				if (p_ptr->slow > 0)
				{
					set_slow(0);
				}
				else
				{
					set_fast(p_ptr->fast + randint(30) + 10);
				}
			}
			msg_print(NULL);
		}
		if ((p_ptr->muta2 & MUT2_BANISH_ALL) &&
			(randint(9000)==1))
		{
			
			disturb(0,0);
			msg_print("You suddenly feel almost lonely.");
			banish_monsters(100);
			msg_print(NULL);
		}
		if ((p_ptr->muta2 & MUT2_EAT_LIGHT) && (randint(3000) == 1))
		{
			object_type *o_ptr;

			msg_print("A shadow passes over you.");
			msg_print(NULL);

			/* Absorb light from the current possition */
			if (cave[py][px].info & CAVE_GLOW)
			{
                hp_player(10);
			}

			o_ptr = &inventory[INVEN_LITE];

			/* Absorb some fuel in the current lite */
			if (o_ptr->tval == TV_LITE)
			{
                /* Use some fuel (except on artifacts) */
                if (!allart_p(o_ptr) && (o_ptr->pval > 0))
                {
                    /* Heal the player a bit */
                    hp_player(o_ptr->pval/20);

                    /* Decrease life-span of lite */
                    o_ptr->pval /= 2;

                    msg_print("You absorb energy from your light!");

                    /*
                     * ToDo: Implement a function to handle
                     * changes of the fuel
                     */

                    /* Hack -- notice interesting fuel steps */
                    if ((o_ptr->pval < 100) || (!(o_ptr->pval % 100)))
                    {
                            /* Window stuff */
                            p_ptr->window |= (PW_EQUIP);
                    }

                    /* Hack -- Special treatment when blind */
                    if (p_ptr->blind)
                    {
                            /* Hack -- save some light for later */
                            if (o_ptr->pval == 0) o_ptr->pval++;
                    }

                    /* The light is now out */
                    else if (o_ptr->pval == 0)
                    {
                            disturb(0, 0);
                            msg_print("Your light has gone out!");
                    }

                    /* The light is getting dim */
                    else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
                    {
                            if (disturb_minor) disturb(0, 0);
                            msg_print("Your light is growing faint.");
                    }
				}
			}

			/*
			* Unlite the area (radius 10) around player and
			* do 50 points damage to every affected monster
			*/
			unlite_area(50, 10);
		}
		if ((p_ptr->muta2 & MUT2_ATT_ANIMAL) && !(p_ptr->anti_magic) &&
			(randint(7000)==1))
		{
			
			bool a_summon = FALSE;
			if (randint(3)==1)
				a_summon = summon_specific_friendly(py,
				px, dun_level, SUMMON_ANIMAL, TRUE);
			else
				a_summon = summon_specific(py,
				px, dun_level, SUMMON_ANIMAL);
			if (a_summon)
			{
				msg_print("You have attracted an animal!");
				disturb(0,0);
			}
		}
		if ((p_ptr->muta2 & MUT2_RAW_CHAOS) && !(p_ptr->anti_magic) &&
			(randint(8000)==1))
		{
			
			disturb(0,0);
			msg_print("You feel the world warping around you!");
			msg_print(NULL);
			fire_ball(GF_CHAOS, 0, skill_set[SKILL_RACIAL].value/2,8);
		}
		if ((p_ptr->muta2 & MUT2_NORMALITY) &&
			(randint(5000)==1))
		{
			lose_chaos_feature(0);
		}
		if ((p_ptr->muta2 & MUT2_WRAITH) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{
			
			disturb(0,0);
			msg_print("You feel insubstantial!");
			msg_print(NULL);
			set_shadow(p_ptr->wraith_form + randint(skill_set[SKILL_RACIAL].value/4) + (skill_set[SKILL_RACIAL].value/4));
		}
		if ((p_ptr->muta2 & MUT2_POLY_WOUND) &&
			(randint(3000)==1))
		{
			do_poly_wounds();
		}
		if ((p_ptr->muta2 & MUT2_WASTING) &&
			(randint(3000)==13))
		{
			int which_stat = rand_int(6);
			int sustained = FALSE;
			
			switch (which_stat)
			{
			case A_STR:
				if (p_ptr->sustain_str) sustained = TRUE;
				break;
			case A_INT:
				if (p_ptr->sustain_int) sustained = TRUE;
				break;
			case A_WIS:
				if (p_ptr->sustain_wis) sustained = TRUE;
				break;
			case A_DEX:
				if (p_ptr->sustain_dex) sustained = TRUE;
				break;
			case A_CON:
				if (p_ptr->sustain_con) sustained = TRUE;
				break;
			case A_CHR:
				if (p_ptr->sustain_chr) sustained = TRUE;
				break;
			default:
				msg_print("Invalid stat chosen!");
				sustained = TRUE;
			}
			
			if (!sustained)
			{
				disturb(0,0);
				msg_print("You can feel yourself wasting away!");
				msg_print(NULL);
				(void)dec_stat(which_stat, randint(6)+6, randint(3)==1);
			}
		}
		if ((p_ptr->muta2 & MUT2_ATT_DRAGON) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{
			
			bool d_summon = FALSE;
			if (randint(5)==1)
				d_summon = summon_specific_friendly(py,
				px, dun_level, SUMMON_DRAGON, TRUE);
			else
				d_summon = summon_specific(py,
				px, dun_level, SUMMON_DRAGON);
			if (d_summon)
			{
				msg_print("You have attracted a dragon!");
				disturb(0,0);
			}
		}
		if ((p_ptr->muta2 & MUT2_WEIRD_MIND) && !(p_ptr->anti_magic) &&
			(randint(3000)==1))
		{
			if (p_ptr->tim_esp > 0)
			{
				msg_print("Your mind feels cloudy!");
				set_tim_esp(0);
			}
			else
			{
				msg_print("Your mind expands!");
				set_tim_esp(skill_set[SKILL_RACIAL].value/2);
			}
		}
		if ((p_ptr->muta2 & MUT2_NAUSEA) && !(p_ptr->slow_digest) &&
			(randint(9000)==1))
		{
			
			disturb(0,0);
			msg_print("Your stomach roils, and you lose your lunch!");
			msg_print(NULL);
			set_food(PY_FOOD_WEAK);
		}

		if ((p_ptr->muta2 & MUT2_WALK_SHAD) &&
		   !(p_ptr->anti_magic) &&
		    (randint(12000) == 1))
		{
			alter_reality();
		}

		if ((p_ptr->muta2 & MUT2_WARNING) &&
			(randint(1000)==1))
		{
			int danger_amount = 0;
			int monster;
			
			for (monster = 0; monster < m_max; monster++)
			{
				monster_type    *m_ptr = &m_list[monster];
				monster_race    *r_ptr = &r_info[m_ptr->r_idx];
				
				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;
				
				if (r_ptr->level >= skill_set[SKILL_RACIAL].value/2)
				{
					danger_amount += r_ptr->level - (skill_set[SKILL_RACIAL].value/2) + 1;
				}
			}
			
			if (danger_amount > 100)
				msg_print("You feel utterly terrified!");
			else if (danger_amount > 50)
				msg_print("You feel terrified!");
			else if (danger_amount > 20)
				msg_print("You feel very worried!");
			else if (danger_amount > 10)
				msg_print("You feel paranoid!");
			else if (danger_amount > 5)
				msg_print("You feel almost safe.");
			else
				msg_print("You feel lonely.");
		}
		if ((p_ptr->muta2 & MUT2_INVULN) && !(p_ptr->anti_magic) &&
			(randint(5000)==1))
		{
			
			disturb(0,0);
			msg_print("You feel invincible!");
			msg_print(NULL);
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
		}
		if ((p_ptr->muta2 & MUT2_SP_TO_HP) &&
			(randint(2000)==1))
		{
			int wounds = p_ptr->mhp - p_ptr->chp;
			
			if (wounds > 0)
			{
				int healing = p_ptr->csp;
				
				if (healing > wounds)
				{
					healing = wounds;
				}
				
				hp_player(healing);
				p_ptr->csp -= healing;
			}
		}
		if ((p_ptr->muta2 & MUT2_HP_TO_SP) && !(p_ptr->anti_magic) &&
			(randint(4000)==1))
		{
			int wounds = p_ptr->msp - p_ptr->csp;
			
			if (wounds > 0)
			{
				int healing = p_ptr->chp;
				
				if (healing > wounds)
				{
					healing = wounds;
				}
				
				p_ptr->csp += healing;
				take_hit(healing, "blood rushing to the head");
			}
		}
		if ((p_ptr->muta2 & MUT2_DISARM) &&
			(randint(10000)==1))
		{
			object_type *o_ptr;
			
			disturb(0,0);
			msg_print("You trip over your own feet!");
			take_hit(randint(p_ptr->wt/6), "tripping");
			
			msg_print(NULL);
			o_ptr = &inventory[INVEN_WIELD];
			if (o_ptr->k_idx)
			{
				msg_print("You drop your weapon!");
				inven_drop(INVEN_WIELD,1);
			}
        }
	}


		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				j++;
				recharged_notice(o_ptr);
			}
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Recharge rods */
	for (j = 0, i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->pval))
		{
			/* Charge it */
			o_ptr->pval--;

			/* Notice changes */
			if (!(o_ptr->pval))
			{
				j++;
				recharged_notice(o_ptr);
			}
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}


	/* calm down spirits */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		if(spirits[i].annoyance)
		{
			spirits[i].annoyance--;
			p_ptr->redraw |= PR_SPIRIT;
		if (!spirits[i].annoyance && disturb_minor)
			disturb(0, 0);
		}
	}

	/* Feel the inventory */
	sense_inventory();


	/*** Process Objects ***/

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Access object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge rods on the ground */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->pval)) o_ptr->pval--;
	}

	/* Delayed Word-of-Recall */
	if (p_ptr->word_recall)
	{
		/* Count down towards recall */
		p_ptr->word_recall--;

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Determine the level */
			if (dun_level != 0)
			{
				msg_print("You feel yourself yanked upwards!");

				change_level(0, START_RANDOM);
                
				wildx=town_defs[cur_town].x;
				wildy=town_defs[cur_town].y;

				came_from=START_RANDOM;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");

				change_level(MAX(1, p_ptr->max_dlv[cur_dungeon]), START_RANDOM);

				cur_dungeon=recall_dungeon;
			}
			
			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}
}







#ifdef ALLOW_BORG

/*
 * Verify use of "borg" commands
 */
static bool enter_borg_mode(void)
{
	/* Ask first time */
	if (!(noscore & 0x0010))
	{
		/* Mention effects */
		msg_print("The borg commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use borg commands.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use borg commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		noscore |= 0x0010;
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the Ben Borg
 */
extern void do_cmd_borg(void);

#endif



/*
 * A temporary script to run on the game.
 */
static void do_cmd_script(void)
{
	msg_print("You are NOT allowed to do THAT!");
}

/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{

                char error_m[80];

	char track_str[20]="cmd=";
	cptr t;

 #ifdef ALLOW_REPEAT
 
     /* Handle repeating the last command */
     repeat_check();
 
 #endif /* ALLOW_REPEAT -- TNB */
 
	/* Track this command (if not instantaneous). */
	t = format("%c%c", (command_cmd & 0xFF00)/0x0100, command_cmd & 0x00FF);
	if (!*t) t++;
	ascii_to_text(track_str+strlen("cmd="), t);
	help_track(track_str);

	/* Parse the command */
	switch (command_cmd)
	{
			/* Ignore */
		case ESCAPE: case CMD_DEBUG+ESCAPE: 
		case ' ': case CMD_DEBUG+' ':
		case '\r': case CMD_DEBUG+'\r':
		case '\n': case CMD_DEBUG+'\n':
		{
			break;
		}




			/*** Wizard Commands ***/

#ifdef ALLOW_WIZARD

			/* Special "debug" commands */
		case KTRL('A'):
		{
			/* Enter debug mode */
			if (cheat_wzrd)
			{
				do_cmd_debug();
			}
			else
			{
				if (randint(2)==1)
				{
					get_rnd_line("error.txt", error_m);
					msg_print(error_m);
				}
				else
				{
					prt("Type '?' for help.", 0, 0);
				}
			}
			break;
		}

#endif


#ifdef ALLOW_BORG

			/* Special "borg" commands */
		case KTRL('Z'):
		{
			/* Enter borg mode */
			if (enter_borg_mode())
			{
				do_cmd_borg();
			}

			break;
		}

#endif



			/*** Inventory Commands ***/

			/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

			/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

			/* Drop an item */
		case 'd':
		{
			do_cmd_drop();
			break;
		}

			/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

			/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

			/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


			/*** Various commands ***/

			/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

			/* Hack -- toggle windows */
		case KTRL('I'):
		{
			toggle_inven_equip();
			break;
		}


			/*** Standard "Movement" Commands ***/

			/* Alter a grid */
		case '+':
		{
			do_cmd_alter();
			break;
		}

			/* Dig a tunnel */
		case 'T':
		{
			do_cmd_tunnel();
			break;
		}

		/* Hurt a monster */
		case 'H':
		{
			do_cmd_attack();
			break;
		}

			/* Move (usually pick up things) */
		case ';':
		{
 #ifdef ALLOW_EASY_DISARM
 
 			do_cmd_walk(FALSE);
 
 #else /* ALLOW_EASY_DISARM -- TNB */
 
 			do_cmd_walk(always_pickup);
 
 #endif /* ALLOW_EASY_DISARM -- TNB */
  			break;
		}

			/* Move (usually do not pick up) */
		case '-':
		{
 #ifdef ALLOW_EASY_DISARM
 
 			do_cmd_walk(TRUE);
 
 #else /* ALLOW_EASY_DISARM -- TNB */
 
 			do_cmd_walk(!always_pickup);
 
 #endif /* ALLOW_EASY_DISARM -- TNB */
			break;
		}


			/*** Running, Resting, Searching, Staying */

			/* Begin Running -- Arg is Max Distance */
		case '.':
		{
			do_cmd_run();
			break;
		}

			/* Stay still (usually pick things up) */
		case ',':
		{
			do_cmd_stay(always_pickup);
			break;
		}

			/* Stay still (usually do not pick up) */
		case 'g':
		{
			do_cmd_stay(!always_pickup);
			break;
		}

			/* Rest -- Arg is time */
		case 'R':
		{
			do_cmd_rest();
			break;
		}

			/* Search for traps/doors */
		case 's':
		{
			do_cmd_search();
			break;
		}

			/* Toggle sneak mode */
		case 'S':
		{
			do_cmd_toggle_sneak();
			break;
		}


			/*** Stairs and Doors and Chests and Traps ***/

			/* Enter store */
		case KTRL('E'):
		{
			do_cmd_store();
			break;
		}

			/* Go up staircase */
		case '<':
		{
            do_cmd_go_up();
			break;
		}

			/* Go down staircase */
		case '>':
		{
            do_cmd_go_down();
			break;
		}

			/* Open a door or chest */
		case 'o':
		{
			do_cmd_open();
			break;
		}

			/* Close a door */
		case 'c':
		{
			do_cmd_close();
			break;
		}

			/* Jam a door with spikes */
		case 'j':
		{
			do_cmd_spike();
			break;
		}

			/* Bash a door */
		case 'B':
		{
			do_cmd_bash();
			break;
		}

			/* Disarm a trap or chest */
		case 'D':
		{
			do_cmd_disarm();
			break;
		}


			/*** Magic and Prayers ***/

			/* Gain new spells/prayers */
		case 'G':
		{
			msg_format("You need some peace and quiet to research.");
			msg_format("Why not try a bookstore?");
			break;
		}

			/* Browse a book */
		case 'b':
		{
			if(!unify_commands)
			{
			do_cmd_browse(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Cast a spell */
		case 'm':
		{
            if (p_ptr->anti_magic)
            {
                msg_print("An anti-magic shell disrupts your spell!");
                energy_use = TURN_ENERGY/20; /* Still use a bit */
            }
            else
            {
				do_cmd_cast();
            }
            break;
		}

		/* call upon a spirit for a favour */
		case 'p':
            if (p_ptr->anti_magic)
            {
                msg_print("An anti-magic shell blocks your call!");
                energy_use = TURN_ENERGY/20; /* Still use a bit */
            }
            else
            {
				do_cmd_invoke();
            }
            break;

			/* Use a Psionic Ability */
		case 'P':
		{
            if (p_ptr->anti_magic)
            {
                msg_print("An anti-magic shell disrupts your psionic ability!");
                energy_use = TURN_ENERGY/20; /* Still use a bit */
            }
            else
            {
				do_cmd_mindcraft();
            }
            break;
		}
			/* Cast a Cantrip */
		case 'h':
		{
            if (p_ptr->anti_magic)
            {
                msg_print("An anti-magic shell disrupts your magic!");
                energy_use = TURN_ENERGY/20; /* Still use a bit */
            }
            else
            {
				do_cmd_cantrip();
            }
            break;
		}


			/*** Use various objects ***/

			/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

			/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}

			/* Activate an artifact */
		case 'A':
		{
			if(!unify_commands)
			{
			do_cmd_activate(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Eat some food */
		case 'E':
		{
			if(!unify_commands)
			{
			do_cmd_eat_food(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Fuel your lantern/torch */
		case 'F':
		{
			if(!unify_commands)
			{
			do_cmd_refill(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Fire an item */
		case 'f':
		{
			do_cmd_fire();
			break;
		}

			/* Throw an item */
		case 'v':
		{
            do_cmd_throw();
			break;
		}

			/* Aim a wand */
		case 'a':
		{
			if(!unify_commands)
			{
			do_cmd_aim_wand(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Zap a rod */
		case 'z':
		{
			if(!unify_commands)
			{
			do_cmd_zap_rod(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Quaff a potion */
		case 'q':
		{
			if(!unify_commands)
			{
			do_cmd_quaff_potion(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}

			/* Unified use function */
			case KTRL('u'):
			{
				do_cmd_handle();
				break;
			}

			/* Read a scroll */
		case 'r':
		{
			if(!unify_commands)
			{
			do_cmd_read_scroll(-999);
			} else {
				do_cmd_handle();
		}
			break;
		}

			/* Use a staff */
		case 'u':
		{
			if(!unify_commands)
			{
			do_cmd_use_staff(-999);
			} else {
				do_cmd_handle();
			}
			break;
		}


        case 'U':
        {
            do_cmd_racial_power();
            break;
        }


			/*** Looking at Things (nearby or on map) ***/

			/* Full dungeon map */
		case 'M':
		{
			do_cmd_view_map();
			break;
		}

			/* Locate player on map */
		case 'L':
		{
			do_cmd_locate();
			break;
		}

			/* Look around */
		case 'l':
		{
			do_cmd_look();
			break;
		}

			/* Target monster or location */
		case '*':
		{
			do_cmd_target();
			break;
		}



			/*** Help and Such ***/

			/* Help */
		case '?':
		{
			do_cmd_help(syshelpfile);
			break;
		}

			/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

			/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


			/*** System Commands ***/

			/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

			/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

			/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

			/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

			/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

			/* Interact with options */
		case '=':
		{
			do_cmd_options();
            p_ptr->redraw |= (PR_EQUIPPY);
			break;
		}


			/*** Misc Commands ***/

			/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

			/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

			/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling(FALSE);
			break;
		}

			/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

			/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

			/* Redraw the screen */
		case KTRL('R'):
		{
			do_cmd_redraw();
			break;
		}

#ifndef VERIFY_SAVEFILE

			/* Hack -- Save and don't quit */
		case KTRL('S'):
		{
			do_cmd_save_game(FALSE);
			break;
		}

#endif

			/* Save and quit */
		case KTRL('X'):
		{
			alive = FALSE;
			break;
		}

			/* Quit (commit suicide) */
		case 'Q':
		{
			do_cmd_suicide();
			break;
		}

		/* Rotate the stack of items under the player. */
		case KTRL('W'):
		{
			do_cmd_rotate_stack();
			break;
		}
		
		
            /* Check artifacts, uniques, objects */
		case '~':
        case '|':
		{
            do_cmd_knowledge();
			break;
		}

			/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

			/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

#ifdef ALLOW_WIZARD
# ifdef ALLOW_SPOILERS 
		/* Hack -- Generate Spoilers */
		case CMD_DEBUG+'"':
		{
			do_cmd_spoilers();
			break;
		}
# endif /* ALLOW_SPOILERS */

		/* Debug help (should be in help?) */
		case CMD_DEBUG+'?':
		{
			do_cmd_wiz_help();
			break;
		}
		
		/* Cure all maladies */
		case CMD_DEBUG+'a':
		{
			do_cmd_wiz_cure_all();
			break;
		}

		/* Teleport to target */
		case CMD_DEBUG+'b':
		{
			do_cmd_wiz_bamf();
			break;
		}

		/* Create any object */
		case CMD_DEBUG+'c':
		{
			wiz_create_item(command_arg);
			break;
		}


        /* Create a named artifact */
        case CMD_DEBUG+'C':
		{
    	    wiz_create_named_art(command_arg);
	        break;
		}

		/* Detect everything */
		case CMD_DEBUG+'d':
		{
			detect_all();
			break;
		}

		/* Edit character */
		case CMD_DEBUG+'e':
		{
			do_cmd_wiz_change();
			break;
		}

		/* View item info */
		case CMD_DEBUG+'f':
		{
			(void)identify_fully();
			break;
		}

		/* Good Objects */
		case CMD_DEBUG+'g':
		{
			if (command_arg <= 0) command_arg = 1;
			acquirement(py, px, command_arg, FALSE);
			break;
		}

		/* Hitpoint rerating */
		case CMD_DEBUG+'h':
		{
			do_cmd_rerate(); break;
		}

#ifdef MONSTER_HORDES
        case CMD_DEBUG+'H':
		{
    	    do_cmd_summon_horde(); break;
		}
#endif

		/* Identify */
		case CMD_DEBUG+'i':
		{
			identify_pack();
			break;
		}

		/* Go up or down in the dungeon */
		case CMD_DEBUG+'j':
		{
			do_cmd_wiz_jump();
			break;
		}

		/* Self-Knowledge */
		case CMD_DEBUG+'k':
		{
			self_knowledge();
			break;
		}

		/* Learn about objects */
		case CMD_DEBUG+'l':
		{
			do_cmd_wiz_learn();
			break;
		}

		/* Magic Mapping */
		case CMD_DEBUG+'m':
		{
			map_area();
			break;
		}

        /* Chaos Feature */
        case CMD_DEBUG+'M':
		{
    	    (void) gain_chaos_feature(command_arg);
	        break;
		}

        /* Specific reward */
        case CMD_DEBUG+'r':
		{
    	    (void) gain_level_reward(command_arg);
	        break;
		}

        /* Summon _friendly_ named monster */
        case CMD_DEBUG+'N':
		{
    	    do_cmd_wiz_named_friendly(command_arg, TRUE);
	        break;
		}

		/* Summon Named Monster */
		case CMD_DEBUG+'n':
		{
			do_cmd_wiz_named(command_arg, TRUE);
			break;
		}


		/* Object playing routines */
		case CMD_DEBUG+'o':
		{
			do_cmd_wiz_play();
			break;
		}

		/* Phase Door */
		case CMD_DEBUG+'p':
		{
			teleport_player(10);
			break;
		}

		/* Summon Random Monster(s) */
		case CMD_DEBUG+'s':
		{
			if (command_arg <= 0) command_arg = 1;
			do_cmd_wiz_summon(command_arg);
			break;
		}

		/* Teleport */
		case CMD_DEBUG+'t':
		{
			teleport_player(100);
			break;
		}

		/* Very Good Objects */
		case CMD_DEBUG+'v':
		{
			if (command_arg <= 0) command_arg = 1;
			acquirement(py, px, command_arg, TRUE);
			break;
		}

		/* Wizard Light the Level */
		case CMD_DEBUG+'w':
		{
			wiz_lite();
			break;
		}

		/* Increase Skills */
		case CMD_DEBUG+'x':
		{
			if (command_arg)
			{
				gain_skills(command_arg);
			}
			else
			{
				gain_skills(100);
			}
			break;
		}

		/* Zap Monsters (Genocide) */
		case CMD_DEBUG+'z':
		{
			do_cmd_wiz_zap();
			break;
		}

		case CMD_DEBUG+'Z':
		{
			do_cmd_magebolt();
			break;
		}

		/* Hack -- whatever I desire */
		case CMD_DEBUG+'_':
		{
			do_cmd_wiz_hack_ben();
			break;
		}
#endif /* ALLOW_WIZARD */

		/* Hack - process a temporary function. */
		case '$':
		{
			do_cmd_script();
			break;
		}
			/* Hack -- Unknown command */
		default:
		{
            if (randint(2)==1)
            {
                get_rnd_line("error.txt", error_m);
                msg_print(error_m);
            }
            else
                prt("Type '?' for help.", 0, 0);
			break;
		}
	}

	/* Clear the tracked command. */
	help_track(NULL);
}




/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 */
static void process_player(void)
{
	int i;

	/*** Apply energy ***/

    if (hack_chaos_feature)
    {
        msg_print("You feel different!");
        (void)gain_chaos_feature(0);
        hack_chaos_feature = FALSE;
    }

	/* Give the player some energy */
	p_ptr->energy += TICK_ENERGY; 

	/* No turn yet */
	if (p_ptr->energy < 1000) return;


	/*** Check for interupts ***/

	/* Complete resting */
	if (resting < 0)
	{
		/* Basic resting */
		if (resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
                (p_ptr->csp >= p_ptr->msp) &&
				(p_ptr->cchi >= p_ptr->mchi))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    (p_ptr->cchi == p_ptr->mchi) &&
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall)
			{
				disturb(0, 0);
			}
		}
	}

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" (semi-efficiently for resting) */
		if (running || command_rep || (resting && !(resting & 0x0F)))
		{
			/* Do not wait */
			inkey_scan = TRUE;

			/* Check for a key other than space */
			if (((i = inkey())) && i != ' ')
			{
				/* Flush input */
				flush();

				/* Disturb */
				disturb(0, 0);

				/* Hack -- Show a Message */
				msg_print("Cancelled.");
			}
		}
	}


	/*** Handle actual user input ***/

	/* Repeat until out of energy */
	while (p_ptr->energy >= 1000)
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();


		/* Place the cursor on the player */
		move_cursor_relative(py, px);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();


		/* Hack -- Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			C_TNEW(o_name, ONAME_MAX, char);

			object_type *o_ptr;

			/* Access the slot to be dropped */
			o_ptr = &inventory[item];

			/* Disturbing */
			disturb(0, 0);

			/* Warning */
			msg_print("Your pack overflows!");

			/* Describe */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_format("You drop %s (%c).", o_name, index_to_label(item));

			/* Drop it (carefully) near the player */
			drop_near(o_ptr, 0, py, px);

			/* Modify, Describe, Optimize */
			inven_item_increase(item, -255);
			inven_item_describe(item);
			inven_item_optimize(item);
				
			/* Notice stuff (if needed) */
			if (p_ptr->notice) notice_stuff();

			/* Update stuff (if needed) */
			if (p_ptr->update) update_stuff();

			/* Redraw stuff (if needed) */
			if (p_ptr->redraw) redraw_stuff();

			TFREE(o_name);
		}

		/* Hack -- cancel "lurking browse mode" */
		if (!command_new) command_see = FALSE;


		/* Assume free turn */
		energy_use = 0;


		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];
		}

		/* Resting */
		else if (resting)
		{
			/* Timed rest */
			if (resting > 0)
			{
				/* Reduce rest count */
				resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];
		}

		/* Running */
		else if (running)
		{
			/* Take a step */
			run_step(0);
		}

		/* Repeated command */
		else if (command_rep)
		{
			/* Count this execution */
			command_rep--;

			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);

			/* Redraw stuff */
			redraw_stuff();

			/* Redraw stuff (if needed) */
			window_stuff();

			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();
		}

		/* Normal command */
		else
		{
			/* Place the cursor on the player */
			move_cursor_relative(py, px);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}


		/*** Clean up ***/

		/* Significant */
		if (energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= energy_use;

			/* Remember this use of energy */
			old_energy_use = energy_use;
			p_ptr->redraw |= PR_ENERGY;

			/* Hack -- constant hallucination */
			if (p_ptr->image) p_ptr->redraw |= (PR_MAP);


			/* Shimmer monsters if needed */
			if (!avoid_other && shimmer_monsters)
			{
				/* Clear the flag */
				shimmer_monsters = FALSE;

				/* Shimmer multi-hued monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;
					monster_race *r_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Access the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}


			/* Handle monster detection */
			if (repair_monsters)
			{
				/* Reset the flag */
				repair_monsters = FALSE;

				/* Rotate detection flags */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Nice monsters get mean */
					if (m_ptr->mflag & (MFLAG_NICE))
					{
						/* Nice monsters get mean */
						m_ptr->mflag &= ~(MFLAG_NICE);
					}

					/* Handle memorized monsters */
					if (m_ptr->mflag & (MFLAG_MARK))
					{
						/* Maintain detection */
						if (m_ptr->mflag & (MFLAG_SHOW))
						{
							/* Forget flag */
							m_ptr->mflag &= ~(MFLAG_SHOW);
							
							/* Still need repairs */
							repair_monsters = TRUE;
						}

						/* Remove detection */
						else
						{
							/* Forget flag */
							m_ptr->mflag &= ~(MFLAG_MARK);

							/* Assume invisible */
							m_ptr->ml = FALSE;

							/* Update the monster */
							update_mon(i, FALSE);

							/* Redraw regardless */
							lite_spot(m_ptr->fy, m_ptr->fx);
						}
					}
				}
			}
		}


		/* Hack -- notice death or departure */
		if (!alive || death || new_level_flag) break;
	}
}



/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	/* Reset various flags */
	new_level_flag = FALSE;
    hack_mind = FALSE;
	full_grid = MAX_FULL_GRID;


	/* Reset the "command" vars */
	command_cmd = 0;
	command_new = 0;
	command_rep = 0;
	command_arg = 0;
	command_dir = 0;


	/* Cancel the target */
	target_who = 0;

	/* Cancel the health bar */
	health_track(0);


	/* Check visual effects */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;
	repair_monsters = TRUE;
	repair_objects = TRUE;


	/* Disturb */
	disturb(1, 0);


	/* Track maximum dungeon level */
	if (p_ptr->max_dlv[cur_dungeon] < dun_level)
	{
		p_ptr->max_dlv[cur_dungeon] = dun_level;
	}


	/* Paranoia -- No stairs down from Quest */
	if (is_quest(dun_level))
	{
		if (dun_defs[cur_dungeon].tower)
		{
			create_up_stair = FALSE;
		}
		else
		{
			create_down_stair = FALSE;
		}
	}

	/* Paranoia -- no stairs from town */
	if (dun_level <= 0) create_down_stair = create_up_stair = FALSE;

	/* Option -- no connected stairs */
	if (!dungeon_stair) create_down_stair = create_up_stair = FALSE;

	/* Make a stairway. */
	if (create_up_stair || create_down_stair)
	{
		/* Place a stairway */
		if (cave_valid_bold(py, px))
		{
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if (create_down_stair)
			{
				cave_set_feat(py, px, FEAT_MORE);
			}
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		create_down_stair = create_up_stair = FALSE;
	}

	/* Verify the panel */
	verify_panel();

	/* Validate the panel */
	if (centre_view)
	{
		panel_bounds_center();
	}
	else
	{
	panel_bounds();
	}

	/* Flush messages */
	msg_print(NULL);


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_SHOPS);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_VISIBLE);

	/* Redraw dungeon */
    p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_DISTANCE | PU_ROOM);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Refresh */
	Term_fresh();


	/* Hack -- notice death or departure */
	if (!alive || death || new_level_flag) return;

	/* Notice a Quest Level */
	if (is_quest(dun_level)) quest_discovery();

	/* Notice the final level of a dungeon/tower */
	else if (dun_level == dun_defs[cur_dungeon].max_level)
	{
		msg_print("You suddenly feel that you can't go on.");
	}

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = (dun_depth);

	/* Reset the object generation level */
	object_level = (dun_depth);

    hack_mind = TRUE;

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > MAX_M_IDX) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > MAX_O_IDX) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);

		if ((turn-old_turn) == ((ironman_feeling) ? 2500 : 0))
		{
			if(dun_level >0)
			{
				do_cmd_feeling(TRUE);
			}
		}

		/* Process the player */
		process_player();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death || new_level_flag) break;

        total_friends = 0;
        total_friend_levels = 0;

        /* Process all of the monsters */
		process_monsters();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death || new_level_flag) break;


		/* Process the world */
		process_world();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(py, px);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!alive || death || new_level_flag) break;


		/* Count game turns */
		turn++;
	}
}




/* Arcum Dagsson's code to support separate macro files for different realms */

/*
 * Load all relevant pref files to avoid problems with preferences being set to
 * to defaults.
 */
static void process_some_user_pref_files(void)
{
	char buf[NAME_LEN+4];

	/* Process the "font" or "graf" pref file, based on "use_graphics" */
	if (use_graphics)
	{
		process_pref_file("graf.prf");
	}
	else
	{
		process_pref_file("font.prf");
	}

	/* Process the "user.prf" file */
	process_pref_file("user.prf");

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);
}

/*
 * Handle wizard mode resurrection and ritual of recall.
 * Set "wizard" in the former case.
 */
static void resurrect(bool wizard)
{
	if (wizard)
	{
		/* Message */
		msg_print("You invoke wizard mode and cheat death.");
	}
	else
	{
		/* Message */
		msg_print("Your ritual saves you from death.");
	}

	msg_print(NULL);

	/* Restore hit points */
	p_ptr->chp = p_ptr->mhp;
	p_ptr->chp_frac = 0;

	/* Restore spell and chi points */
	p_ptr->csp = p_ptr->msp;
	p_ptr->csp_frac = 0;
	p_ptr->cchi=p_ptr->mchi;
	p_ptr->chi_frac = 0;

	/* Hack -- Healing */
	(void)set_blind(0);
	(void)set_confused(0);
	(void)set_poisoned(0);
	(void)set_afraid(0);
	(void)set_paralyzed(0);
	(void)set_image(0);
	(void)set_stun(0);
	(void)set_cut(0);

	/* Hack -- Prevent starvation */
	(void)set_food(PY_FOOD_MAX - 1);

	/* Hack -- cancel recall */
	if (p_ptr->word_recall)
	{
		/* Message */
		msg_print("A tension leaves the air around you...");
		msg_print(NULL);

		/* Hack -- Prevent recall */
		p_ptr->word_recall = 0;
	}

	/* Note cause of death XXX XXX XXX */
	if (wizard) (void)strcpy(died_from, "Cheating death");

	/* Teleport to town */
	new_level_flag = TRUE;
	came_from=START_RANDOM;

	/* Go to town */
	dun_level = 0;

	/* Do not die */
	death = FALSE;

	if (wizard)
	{
		/* Mark social class, reset age, if needed */
		if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

		/* Increase age */
		p_ptr->age++;

		/* Mark savefile */
		noscore |= 0x0001;
	}
	else
	{
		/* Go to town of recall */
		dun_level = 0;
		cur_town = p_ptr->ritual;
		wildx=town_defs[cur_town].x;
		wildy=town_defs[cur_town].y;

		/* Ritual is used */
		p_ptr->ritual = MAX_TOWNS + 1;

		/* Lose most money and all items */
		/* You only keep the change in your pocket */
		if(p_ptr->au > 100) p_ptr->au = 100;
		destroy_pack();
	}
}

/*
 * Create a new character, and all that goes with it.
 */
static void create_character(void)
{
	int i,j,x,y;

	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Hack -- seeds for flavors and wilderness*/
	seed_flavor = rand_int(0x10000000);
	seed_wild = rand_int(0x10000000);
	/* Initialise wilderness grid */
	for (i=0;i<12;i++)
	{
		for(j=0;j<12;j++)
		{
			wild_grid[i][j].seed=rand_int(0x10000000);
			wild_grid[i][j].dungeon=MAX_CAVES; /* 0-(MAX_CAVES-1) indicates dungeon */
			wild_grid[i][j].road_map=0;
		}
	}

	/* Hack -- seed for town layouts */
	for (i = 0; i < MAX_TOWNS; i++)
	{
		town_defs[i].seed = rand_int(0x10000000);
	}

	/* Place towns and dungeons */
	for(i=0;i<MAX_CAVES;i++)
	{
		if(i<MAX_TOWNS)
		{
			j=0;
			while(j == 0)
			{
				x=rand_range(2,9);
				y=rand_range(2,9);
				j=1;
				if((wild_grid[y][x].dungeon != MAX_CAVES) ||
					(wild_grid[y-1][x].dungeon != MAX_CAVES) ||
					(wild_grid[y+1][x].dungeon != MAX_CAVES) ||
					(wild_grid[y][x-1].dungeon != MAX_CAVES) ||
					(wild_grid[y][x+1].dungeon != MAX_CAVES) ||
					(wild_grid[y-1][x+1].dungeon != MAX_CAVES) ||
					(wild_grid[y+1][x+1].dungeon != MAX_CAVES) ||
					(wild_grid[y-1][x-1].dungeon != MAX_CAVES) ||
					(wild_grid[y+1][x-1].dungeon != MAX_CAVES))
				{
					j=0;
				}
			}
		}
		else
		{
			j=0;
			while(j == 0)
			{
				x=rand_range(2,9);
				y=rand_range(2,9);
				j=1;
				if(wild_grid[y][x].dungeon != MAX_CAVES)
				{
					j=0;
				}
			}
		}
		/* There are no dungeons next to this */
		wild_grid[y][x].dungeon=i;
		/* now let the town & dungeon know where they are */
		if (i<MAX_TOWNS)
		{
			town_defs[i].x=x;
			town_defs[i].y=y;
		}
		dun_defs[i].x=x;
		dun_defs[i].y=y;
	}
			
	/* Generate road map... */
	for(i=0;i<MAX_TOWNS-1;i++)
	{
		int cur_x,cur_y,dest_x,dest_y;
		int x_disp,y_disp,x_sgn,y_sgn;
		int fin;
		byte curdir,nextdir;

		cur_x=town_defs[i].x;
		cur_y=town_defs[i].y;
		dest_x=town_defs[i+1].x;
		dest_y=town_defs[i+1].y;
		fin=0;
		while(!fin)
		{
			x_disp=dest_x-cur_x;
			x_sgn=0;
			if (x_disp>0)
			{
				x_sgn=1;
			}
			if (x_disp<0)
			{
				x_sgn=-1;
				x_disp=-x_disp;
			}
			y_disp=dest_y-cur_y;
			y_sgn=0;
			if(y_disp>0)
			{
				y_sgn=1;
			}
			if(y_disp<0)
			{
				y_sgn=-1;
				y_disp=-y_disp;
			}
			/* _disp holds distance, _sgn holds sign (direction). Adding ( _disp * _sgn ) to cur_ gives dest_ */
			if ((x_disp == 0) && (y_disp == 0))
			{
				fin=1;
			}
			else
			{
				/* Check the four directions - with an extra check at the start for symmetry*/
				if ((x_disp == y_disp) && (x_sgn==1) && (y_sgn == -1))
				{
					curdir=ROAD_UP;
					nextdir=ROAD_DOWN;
				}
				else if((x_sgn == 1) && (x_disp >= y_disp))
				{
					curdir=ROAD_RIGHT;
					nextdir=ROAD_LEFT;
				}
				else if((y_sgn == 1) && (y_disp >= x_disp))
				{
					curdir=ROAD_DOWN;
					nextdir=ROAD_UP;
				}
				else if((x_sgn == -1) && (x_disp >= y_disp))
				{
					curdir=ROAD_LEFT;
					nextdir=ROAD_RIGHT;
				}
				else
				{
					curdir=ROAD_UP;
					nextdir=ROAD_DOWN;
				}
				/* We now have the current dir and the next dir... */
				wild_grid[cur_y][cur_x].road_map |= curdir;
				if(curdir==ROAD_RIGHT)
				{
					cur_x++;
				}
				else if(curdir == ROAD_LEFT)
				{
					cur_x--;
				}
				else if(curdir == ROAD_DOWN)
				{
					cur_y++;
				}
				else
				{
					cur_y--;
				}
				wild_grid[cur_y][cur_x].road_map |= nextdir;
			}
		}
	}

	/* Start in town 0 */
	dun_level = 0;
	cur_town = TOWN_KADATH;
	while((cur_town == TOWN_KADATH) || (cur_town == TOWN_NIR))
	{
		cur_town = (byte)rand_range(0,MAX_TOWNS-1);
	}
	cur_dungeon = cur_town;
	dun_offset = 0;
	dun_bias = 0;
	wildx=town_defs[cur_town].x;
	wildy=town_defs[cur_town].y;
	came_from = START_RANDOM;

	/* Roll up a new character */
	player_birth();

	/* Hack -- enter the world at day unless undead*/
	if ((p_ptr->prace == RACE_SPECTRE) ||
		 (p_ptr->prace == RACE_ZOMBIE) ||
		 (p_ptr->prace == RACE_SKELETON) ||
		 (p_ptr->prace == RACE_VAMPIRE))
	{
		turn=50001;
	}
	else
	{
		turn=1;
	}
}

/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game. (But see below for win32 systems.)
 */
void play_game(bool new_game)
{
	int i;

	hack_chaos_feature = FALSE;

	/* Hack -- Character is "icky" */
	character_icky = TRUE;


	/* Hack -- turn off the cursor */
	(void)Term_set_cursor(0);

	/* Initialise the resize hook XXX XXX XXX */
	term_screen->resize_hook = resize_map;

	/* Terms controlled by window_stuff() always use resize_window(). */
	for (i = 1; i < ANGBAND_TERM_MAX; i++)
	{
		if (windows[i].term)
		{
			/* Add redraw hook */
			windows[i].term->resize_hook = resize_window;
		}
	}

	/*
	 * In win32 settings we are unlikely to have a command argument passed,
	 * but on the other hand we have a dialog box so that we will have already
	 * given a name for our new save file by this point. In this case, we must
	 * suppress the attempted load & suicide as there is no file to load.
	 *
	 * (It's a mega-hack, but it works...)
	 *
	 */

#ifdef WINDOWS
	if (!new_game)
	{
#endif /* WINDOWS */

		/* Attempt to load */
		if (!load_player())
		{
			/* Oops */
			quit("broken savefile");
		}

#ifdef WINDOWS
	}
#endif /* WINDOWS */

#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(ACORN)
	/* Display news.txt again, as in init_angband(). This
	 * is done because news.txt was removed when the save game
	 * was loaded, and we didn't know whether to wait below without
	 * it. We don't do this without display_credits because the rest
	 * of the initialisation process should be very quick. */
	if (display_credits)
	{
		char buf[1024];
		FILE *fp;
		Term_clear();
		path_build(buf, 1024, ANGBAND_DIR_FILE, "news.txt");
		fp = my_fopen(buf, "r");
		if (fp)
		{
			int i = 0;
			while (0 == my_fgets(fp, buf, 1024))
			{
				Term_putstr(0, i++, -1, TERM_WHITE, buf);
			}
			my_fclose(fp);
		}
		Term_fresh();
	}
#endif

	/* Nothing loaded */
	if (!character_loaded)
	{
		/* Make new player */
		new_game = TRUE;

		/* The dungeon is not ready */
		character_dungeon = FALSE;
	}

	/* Process old character */
	if (!new_game)
	{
		/* Process the player name */
		process_player_name();
	}

	/* Init the RNG */
	if (Rand_quick)
	{
		u32b seed;

		/* Basic seed */
		seed = (time(NULL));

#ifdef SET_UID

		/* Mutate the seed on Unix machines */
		seed = ((seed >> 3) * (getpid() << 1));

#endif

		/* Use the complex RNG */
		Rand_quick = FALSE;

		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}

	/* Extract the options */
	for (i = 0; option_info[i].o_desc; i++)
	{
		int os = option_info[i].o_set;
		int ob = option_info[i].o_bit;

		/* Set the "default" options */
		if (option_info[i].o_var)
		{
			/* Set */
			if (option_flag[os] & (1L << ob))
			{
				/* Set */
				(*option_info[i].o_var) = TRUE;
			}
			
			/* Clear */
			else
			{
				/* Clear */
				(*option_info[i].o_var) = FALSE;
			}
		}
	}

#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(ACORN)
	/* Wait for response if required */
	if (display_credits) pause_line();
#endif


	/* Roll new character */
	if (new_game) create_character();

	/* Remove the stat_default array */
	KILL(stat_default);

	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();

    /* Flavor the objects */
	flavor_init();

	/* Reset the visual mappings */
	reset_visuals();


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);


	/* Load the "pref" files */
	process_some_user_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Character is no longer "icky" */
	character_icky = FALSE;


	/* Start game */
	alive = TRUE;

	/* Hack -- Enforce "delayed death" */
    if (p_ptr->chp < 0) death = TRUE;

	/* Process */
	while (TRUE)
	{
		/* Process the level */
		dungeon();


		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff */
		if (p_ptr->window) window_stuff();


		/* Cancel the target */
		target_who = 0;

		/* Cancel the health bar */
		health_track(0);


		/* Forget the lite */
		forget_lite();

		/* Forget the view */
		forget_view();


		/* Handle "quit and save" */
		if (!alive && !death) break;


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (alive && death)
		{
			/* Mega-Hack -- Allow player to cheat death */
            if ((cheat_wzrd || cheat_live) && !get_check("Die? "))
			{
				resurrect(TRUE);
			}
			
			/* Almost identical code -- Allow player to recall at point of death */
            else if (p_ptr->ritual < MAX_TOWNS + 1)
			{
				resurrect(FALSE);
			}
		}

		/* Handle "death" */
		if (death) break;

		/* Make a new level */
 		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}


