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
u16b ident_power(object_ctype *o_ptr)
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
 * Return TRUE if an object wuth the specified k_idx can be generated either
 * cursed or uncursed.
 * This isn't definitive, so it should be kept in line with apply_magic(),
 * curse_equipment(), etc..
 */
bool PURE k_can_curse(int k_idx)
{
	object_type o_ptr[1];
	int i;

	if (magic_can_curse(k_idx)) return TRUE;

	object_prep(o_ptr, k_idx);
	i = wield_slot(o_ptr);
	if (i >= INVEN_WIELD && i <= INVEN_FEET) return TRUE;

	return FALSE;
}

/*
 * Test whether an object has been observed to have special powers which
 * could not hav e been expected based on its current level of identification.
 */
static bool PURE is_powerful(object_ctype *o_ptr)
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

	/* Set t_ptr and u_ptr as the tried and untried versions of this object. */
	object_copy(u_ptr, o_ptr);
	u_ptr->ident &= ~IDENT_TRIED;
	object_info_known(t_ptr, u_ptr);
	object_info_known(u_ptr, o_ptr);

	/*
	 * Hack - check for the extra information the TRIED flag may give in
	 * object_knowledge, as it's easier than checking stuff which may have
	 * changed.
	 */
	if (t_ptr->flags1 != u_ptr->flags1) return TRUE;
	if (t_ptr->flags2 != u_ptr->flags2) return TRUE;
	if (t_ptr->flags3 != u_ptr->flags3) return TRUE;

	/* Nothing obviously different. */
	return FALSE;
}



/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static int PURE value_check_aux1(object_ctype *o_ptr)
{

	/* Artifacts */
	if (allart_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return SENSE_C_ART;

		/* Normal */
		return SENSE_G_ART;
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return SENSE_C_EGO;

		/* Normal */
		return SENSE_G_EGO;
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return SENSE_C_OBJ;

	/* Broken items */
	if (broken_p(o_ptr)) return SENSE_BROKEN;

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return SENSE_G_OBJ;

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return SENSE_G_OBJ;

	/* Default to "average" */
	return SENSE_U_OBJ;
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static int PURE value_check_aux2(object_ctype *o_ptr)
{
	bool powerful = is_powerful(o_ptr);

	/* Cursed items (all of them) */
	/* Should this be "very cursed", or would that just be confusing? */
	if (cursed_p(o_ptr)) return (powerful) ? SENSE_CP_OBJ : SENSE_C_OBJ;

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return SENSE_BROKEN;

	/* Artifacts -- except cursed/broken ones */
	/* Ego-Items -- except cursed/broken ones */
	/* Good armor bonus */
	/* Good weapon bonuses */
	if (allart_p(o_ptr) || ego_item_p(o_ptr) || (o_ptr->to_a > 0) ||
		(o_ptr->to_h + o_ptr->to_d > 0))
		return (powerful) ? SENSE_GP_OBJ : SENSE_G_OBJ;

	/* No feeling */
	return (powerful) ? SENSE_QP_OBJ : SENSE_NONE;
}

/*
 *  Return an appropriate "feeling" for an object
 */
int PURE find_feeling(object_ctype *o_ptr)
{
	/* Some feelings that don't depend on sensing, but on trying. */
	if (!object_known_p(o_ptr) && (o_ptr->ident & (IDENT_EMPTY)))
		return SENSE_EMPTY;

	/* Hack - wearable items become "poss. cursed", usable ones "tried"*/
	if (!object_aware_p(o_ptr) && object_tried_p(o_ptr))
	{
		switch (o_ptr->tval)
		{
		/* Food, potions, scrolls, staves, wands and rods can be tried. */
			case TV_FOOD: case TV_POTION: case TV_SCROLL:
			case TV_ROD: case TV_WAND: case TV_STAFF:
		return SENSE_TRIED;
		/* Amulets and rings can have k_idx-specific curses... */
			case TV_RING: case TV_AMULET:
		/* ... as might weapons and armour, at least in theory... */
			case TV_DRAG_ARMOR: case TV_HARD_ARMOR: case TV_SOFT_ARMOR:
			case TV_CLOAK: case TV_SHIELD: case TV_CROWN: case TV_HELM:
			case TV_GLOVES: case TV_BOOTS: case TV_SWORD: case TV_POLEARM:
			case TV_HAFTED: case TV_DIGGING: case TV_BOW:
		/* ... but this should only be mentioned if the player doesn't know better. */
			if (!(o_ptr->ident & IDENT_SENSE_CURSED))
				return SENSE_PCURSE;
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
				return (powerful) ? SENSE_CP_OBJ : SENSE_C_OBJ;
			else if (k_can_curse(o_ptr->k_idx))
				return (powerful) ? SENSE_QP_OBJ : SENSE_Q_OBJ;
			else
				return (powerful) ? SENSE_QP_OBJ : SENSE_NONE;
		}
		/* IDENT_SENSE_VALUE is only set alone by an attempt to break an
		 * artefact, so has few plausible values. */
		case IDENT_SENSE_VALUE:
			if (allart_p(o_ptr))
				/* Artefacts */
				return SENSE_Q_ART;
			else if (o_ptr->ident & IDENT_BROKEN)
				/* Artefacts which have been shattered */
				return SENSE_BROKEN;
			else
				return SENSE_NONE;
		default:
		{
			bool powerful = is_powerful(o_ptr);
			return (powerful) ? SENSE_QP_OBJ : SENSE_NONE;
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
 * Return TRUE if a k_idx refers to an object which can be pseudo-identified.
 */
bool k_can_sense(int k_idx)
{
	switch (k_info[k_idx].tval)
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
			return TRUE;
		default:
			return FALSE;
	}
}

/*
 * Sense the inventory
 */
static void sense_inventory(void)
{
	int i;

	const int plev = MAX(1, skill_set[SKILL_PSEUDOID].value/2);


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* Okay sensing for everyone*/
	if (rand_int(80000L / (plev * plev + 160))) return;

	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		cptr you_are, using, really, is;
		bool repeat;
		u16b oldident;

		int feel, oldfeel;

		/* Pseudo-id heavily at 50+, lightly at 30- and sometimes do either
		 * in between. */
		bool heavy = rand_range_test(30, 49, skill_set[SKILL_PSEUDOID].value);

		object_type *o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Skip non-sense machines */
		if (!k_can_sense(o_ptr->k_idx)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* If it has been identified, or pseudo-identified at least as
		heavily as it is being now, don't bother. */
		if ((o_ptr->ident & (IDENT_KNOWN)) || ((o_ptr->ident & IDENT_SENSE)
		&& ((o_ptr->ident & IDENT_SENSE_HEAVY) || (!heavy)))) continue;

		/* Remember how things used to be */
		oldident = o_ptr->ident;
		oldfeel = find_feeling(o_ptr);

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Remember if we "feel" it completely. */
		if (heavy) o_ptr->ident |= IDENT_SENSE_HEAVY;

		/* Remember how things are now. */
		feel = find_feeling(o_ptr);

		/* Check if the feeling has been changed by this process */
		repeat = (feel == oldfeel);

		/* Skip non-feelings */
		if (feel == SENSE_NONE)
		{
			o_ptr->ident = oldident;
			continue;
		}

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Skip unchanged feelings */
		if (o_ptr->ident == oldident) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0);

		really = (repeat) ? "really " : "";
		is = (o_ptr->number == 1) ? "is" : "are";
		you_are = (i >= INVEN_WIELD) ? "you are " : "";
		using = (i >= INVEN_WIELD) ? describe_use(o_ptr) : "in your pack";

		/* Message */
		msg_format("You feel the %v (%c) %s%s %s%s %s...",
			object_desc_f3, o_ptr, FALSE, 0, index_to_label(o_ptr),
			you_are, using, really, is, feeling_str[feel].str);

		/* Get a bit better (this does allow objects to boost skill twice
		 * in the 21-59 range). */
		if (feel != SENSE_U_OBJ) skill_exp(SKILL_PSEUDOID);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr);
	}
}

static bool create_up_stair; /* Auto-create "up stairs" */
static bool create_down_stair; /* Auto-create "down stairs" */

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
		if ((new_level > dun_level) ^ !!(dun_defs[cur_dungeon].flags & DF_TOWER))
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
	/* Ask for level */
	if (get_check("Teleport level? "))
	{
		char ppp[80];

		char tmp_val[160];
		int i;

		int highestquest = dun_defs[cur_dungeon].max_level;

		for (i = 0; i < MAX_Q_IDX; i++)
		{
			quest_type *q_ptr = q_list+i;
			if ((q_ptr->level || (q_ptr->cur_num != q_ptr->max_num))
				&& (q_ptr->level < highestquest) &&
				q_ptr->dungeon == cur_dungeon)
			{
				highestquest = q_ptr->level;
			}
		}

		/* Prompt */
		sprintf(ppp, "Teleport to level (0-%d): ", highestquest);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		i = atoi(tmp_val);

		/* Paranoia */
		if (i < 1) i = 1;

		/* Paranoia */
		if (i > highestquest) i = highestquest;

		/* Accept request */
		msg_format("You teleport to dungeon level %d.", i);

		/* Change level */
		change_level(i, START_RANDOM);
	}
	else if (get_check("Normal teleport? "))
	{
		teleport_player(200);
	}
}


static void wreck_the_pattern(void)
{
	int i, r_y, r_x;

	if (cave[py][px].feat == FEAT_PATTERN_XTRA2)
	{
		/* Ruined already */
		return;
	}

	msg_print("You bleed on the Pattern!");
	msg_print("Something terrible happens!");

	if (!(p_ptr->invuln))
		take_hit(damroll(10,8), "corrupting the Pattern", MON_CORRUPT_PATTERN);

	for (i = rand_range(36, 80); i; i--)
	{
		if (!scatter(&r_y, &r_x, py, px, 4, 0)) break;
		if ((cave[r_y][r_x].feat >= FEAT_PATTERN_START)
			&& (cave[r_y][r_x].feat < FEAT_PATTERN_XTRA2))
		{
					cave_set_feat(r_y, r_x, FEAT_PATTERN_XTRA2);
		}
	}
	cave_set_feat(py, px, FEAT_PATTERN_XTRA2);

}


/*
 * Return TRUE if we are on the pattern.
 */
static bool pattern_effect_p(void)
{
	return (cave[py][px].feat >= FEAT_PATTERN_START &&
		cave[py][px].feat <= FEAT_PATTERN_XTRA2);
}

/*
 * Give effect to standing on various pattern squares.
 */
static void pattern_effect(void)
{
	const bool special = player_has_flag(TR0, TR0_PATTERN);

	if (!pattern_effect_p()) return;

	if (special && p_ptr->cut>0 && one_in(10))
	{
		wreck_the_pattern();
	}

	if (cave[py][px].feat == FEAT_PATTERN_END)
	{
		(void)set_flag(TIMED_POISONED, 0);
		(void)set_flag(TIMED_IMAGE, 0);
		(void)set_flag(TIMED_STUN, 0);
		(void)set_flag(TIMED_CUT, 0);
		(void)set_flag(TIMED_BLIND, 0);
		(void)set_flag(TIMED_AFRAID, 0);
		do_res_stats();
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
			take_hit(200, "walking the corrupted Pattern", MON_CORRUPT_PATTERN);
	}
	else if (!p_ptr->invuln && (!special || one_in(2)))
	{
		take_hit(damroll(1,3), "walking the Pattern", MON_PATTERN);
	}
}





/*
 * Regenerate hit points -RAK-
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
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac; /* mod 65536 */
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
 * Regenerate mana points -RAK-
 * Also regenerate chi points
 */
static void regenmana(int percent)
{
	s32b        new_mana, new_mana_frac;
	s32b new_chi,new_chi_frac;
	int                   old_csp,old_chi;

	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += (short)(new_mana >> 16); /* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac; /* mod 65536 */
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
	p_ptr->cchi += (short)(new_chi >> 16); /* div 65536 */
	/* check for overflow */
	if ((p_ptr->cchi < 0) && (old_chi > 0))
	{
		p_ptr->cchi = MAX_SHORT;
	}
	new_chi_frac = (new_chi & 0xFFFF) + p_ptr->chi_frac; /* mod 65536 */
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
		p_ptr->window |= (PW_SPELL);
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
 * bad (Poison, Weakness etc) or 'useless' (Slime Mould Juice, etc).
 */
bool psychometry(void)
{
	errr err;

	object_type             *o_ptr;

	int feel, oldfeel;
	cptr really, is;

	/* Get an item (from equip or inven or floor) */
	if (!((o_ptr = get_item(&err, "Meditate on which item? ", TRUE, TRUE, TRUE))))
	{
		if (err == -2) msg_print("You have nothing appropriate.");
		return (FALSE);
	}

	/* It is fully known, no information needed. */
	if ((object_known_p(o_ptr)) || (o_ptr->ident & IDENT_SENSE_HEAVY))
	{
		msg_print("You cannot find out anything more about that.");
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

	/* Skip non-feelings */
	if (feel == SENSE_NONE)
	{
		msg_format("You do not perceive anything unusual about the %v.",
			object_desc_f3, o_ptr, FALSE, 0);
		return TRUE;
	}

	/* Say "really" if this merely repeats a previous light feeling. */
	really = (feel == oldfeel) ? "really " : "";

	/* Handle plurals correctly. */
	is = (o_ptr->number == 1) ? "is" : "are";

	msg_format("You feel that the %v %s%s %s...",
		object_desc_f3, o_ptr, FALSE, 0, really , is, feeling_str[feel].str);

	/* Recalculate/redraw stuff (later) */
	update_object(o_ptr);

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
}

/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	/* Process notification request. */
	if (strstr(get_inscription(o_ptr), "!!"))
	{
		cptr verb = (o_ptr->number == 1) ? "is" : "are";
		cptr gen = (allart_p(o_ptr)) ? "The" : "Your";

		/* Notify the player */
		msg_format("%s %v %s recharged.",
			gen, object_desc_f3, o_ptr, FALSE, 0, verb);
	}
}


/*
 * Finish the game if the computer load or the current time are deemed
 * incompatible with playing it.
 */
static void check_time_load(void)
{
	/* Only finish on the third problem. */
	static int closing_flag = 0;

	/* Only do this rarely. */
	if (turn % 1000) return;

	/* No time/load problems. */
	if (!check_time() && !check_load()) return;

	/* Warning */
	if (closing_flag <= 2)
	{
		/* Disturb */
		disturb(0);

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

/*
 * Attempt timed autosave if requested.
 */
static void process_autosave(void)
{
	/* No request. */
	if (!autosave_t || !autosave_freq) return;

	/* Wrong time. */
	if ((turn % ((s32b) autosave_freq * 10 ))) return;

	/* Save (possibly quietly). */
	do_cmd_save_game(TRUE);
}


/*
 * Process events at midday.
 */
static void process_midday(const int day)
{
	bool leap = YEARDAY(p_ptr->birthday) == 59;

	/* It's your birthday. */
	if (YEARDAY(p_ptr->birthday) == YEARDAY(day))
	{
		/* It's your birthday... */
		msg_print("Happy Birthday!");

		/* Those born on 29th Feb. get lots of presents */
		if (leap)
			acquirement(py, px, damroll(3,4), TRUE);
		else
			acquirement(py, px, 1+randint(2), TRUE);

		p_ptr->age++;
	}
	/* You don't appear to have a birthday this year... */
	else if (leap && YEARDAY(day) - YEARDAY(day-1) != 1)
	{
		p_ptr->age++;
	}
}

/*
 * Process events at midnight.
 */
static void process_midnight(const int day)
{
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
	}
}

/*
 * Handle sunrise and sunset.
 */
static void process_sun(void)
{
	int x, y;
	const bool dawn = daytime_p();
	cptr msg = (dawn) ? "risen" : "set";

	/* No sun underground, and towers are all windowless. */
	if (dun_level) return;

	/* Message */
	msg_format("The sun has %s.", msg);

	/* Disturb */
	if (disturb_dawn) disturb(0);

	/* Hack -- Scan the town */
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			/* Get the cave grid */
			cave_type *c_ptr = &cave[y][x];

			if (dawn)
			{
				/* Assume lit */
				c_ptr->info |= (CAVE_GLOW);

				/* Hack -- Memorize lit grids if allowed */
				if (view_perma_grids) c_ptr->info |= (CAVE_MARK);

			}
			else
			{
				/* Only darken "boring" features. */
				if (c_ptr->feat > FEAT_INVIS) continue;

				/* Forget the grid */
				c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);
			}
			/* Hack -- Notice spot */
			note_spot(y, x);
		}
	}

	/* Reset the suitable monster list. */
	get_mon_num_prep(NULL, 0);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);
}


/*
 * Update the shops as necessary.
 */
static void process_shops(void)
{
	int n;

	/* No updates when the player is on the surface. */
	if (!dun_level) return;

	/* Only update once a day. */
	if ((turn % (10L * STORE_TURNS))) return;

	/* Message */
	if (cheat_xtra) msg_print("Updating Shops...");

	/* Maintain each shop */
	for (n = 0; n < MAX_STORES_TOTAL; n++)
	{
		store_maint(n);
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

/*
 * Handle various periodic activities.
 */
static void process_time(void)
{
	int day;

	/* Work out the date. */
	if (turn <= 3*(10L * TOWN_DAWN)/4)
	{
		day = 1;
	}
	else
	{
		day = (turn - 3*(10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 2;
	}
	day += p_ptr->startdate;

	/* Check the date every six hours. */
	switch (turn % (10L * TOWN_DAWN))
	{
		case 0: case ((20L*TOWN_DAWN)/4):
		{
			process_sun();
			break;
		}
		case ((10L*TOWN_DAWN)/4):
		{
			process_midday(day);
			break;
		}
		case ((30L*TOWN_DAWN)/4):
		{
			process_midnight(day);
			break;
		}
	}

	/* Check the shops as required. */
	process_shops();
}

/*
 * Handle periodic monster-related things.
 */
static void process_monsters_new(void)
{
	/* Check for creature generation */
	if ((full_grid > MAX_SIGHT + 5) && (rand_int(MAX_M_ALLOC_CHANCE) == 0))
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, dun_depth, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();
}


/*
 * Take damage from poison. Returns TRUE if regeneration is impossible.
 */
static bool process_damage_poison(void)
{
	/* No poison. */
	if (!p_ptr->poisoned) return FALSE;

	/* Immune to damage. */
	if (p_ptr->invuln) return TRUE;

	/* Take damage. */
	take_hit(1, "poison", MON_POISON);

	return TRUE;
}

/*
 * Take damage from cuts. Returns TRUE if regeneration is impossible.
 */
static bool process_damage_cuts(void)
{
	int i;

	/* No cuts. */
	if (!p_ptr->cut) return FALSE;

	/* Immune to damage. */
	if (p_ptr->invuln) return TRUE;

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
	take_hit(i, "a fatal wound", MON_BLEEDING);

	return TRUE;
}

/*
 * Take damage from sunlight. Returns TRUE if regeneration is impossible.
 */
static bool process_damage_sunlight(void)
{
	/* Only vampires have this weakness. */
	if (!p_ptr->hurt_light) return FALSE;

	/* Only happens on the surface during the day. */
	if (dun_level || !daytime_p()) return FALSE;

	/* Resistance prevents all ill effects. */
	if (p_ptr->resist_lite || p_ptr->invuln) return FALSE;

	/* Darkened areas are safe. */
	if (~cave[py][px].info & CAVE_GLOW) return FALSE;

	/* Take damage */
	msg_print("The sun's rays scorch your undead flesh!");
	take_hit(1, "sunlight", MON_LIGHT);

	return TRUE;
}

/*
 * Take damage from artefact lights. Returns TRUE if regeneration is impossible.
 */
static bool process_damage_artlight(void)
{
	cptr ouch;
	object_type *o_ptr = inventory+INVEN_LITE;

	/* Only vampires have this weakness. */
	if (!p_ptr->hurt_light) return FALSE;

	/* Only wielded artefact light sources are dangerous. */
	if (!o_ptr->k_idx || allart_p(o_ptr)) return FALSE;

	/* Resistance prevents all ill effects. */
	if (p_ptr->resist_lite) return FALSE;

	msg_format("The %v scorches your undead flesh!",
		object_desc_f3, o_ptr, FALSE, 0);

	/* Invulnerability prevents damage. */
	if (p_ptr->invuln) return TRUE;

	/* Take damage. */
	ouch = format("wielding %v", object_desc_f3, o_ptr, TRUE, 0);
	take_hit(1, ouch, MON_LIGHT);

	return TRUE;
}

/*
 * Take damage from being in a wall. Returns TRUE if regeneration is impossible.
 */
static bool process_damage_walls(void)
{
	cptr dam_desc;

	/* Not in a wall. */
	if (cave_floor_bold(py, px) || cave[py][px].feat == FEAT_BUSH) return FALSE;

	if (p_ptr->invuln || p_ptr->wraith_form) return TRUE;

	if (p_ptr->weak_wraith &&
		(p_ptr->chp <= skill_set[SKILL_TOUGH].value/10)) return TRUE;

	if (p_ptr->weak_wraith)
	{
		msg_print("Your body feels disrupted!");
		dam_desc = "density";
	}
	else
	{
		msg_print("You are being crushed!");
		dam_desc = "solid rock";
	}
	take_hit(1 + ((skill_set[SKILL_TOUGH].value)/10), dam_desc, MON_SOLID_ROCK);

	return TRUE;
}

/*
 * Take damage by various methods as appropriate. Returns TRUE if regeneration
 * is impossible because of this.
 * pattern_effect_p() causes no direct damage, but it does prevent regeneration.
 */
static bool process_damage(void)
{
	return process_damage_cuts() | process_damage_poison() |
		process_damage_sunlight() | process_damage_artlight() |
		process_damage_walls() | pattern_effect_p();
}


/*
 * Use up food at the appropriate rate.
 */
static void process_food(void)
{

#ifdef ALLOW_WIZARD
	/* Hack - for a wizard, amulets of adornment become amulets of no digestion */
	if (cheat_wzrd && inventory[INVEN_NECK].k_idx == OBJ_AMULET_ADORNMENT) return;
#endif

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{

			/* Basic digestion rate no longer based on speed */
			int i = 20;

			/* Regeneration takes more food */
			if (p_ptr->regenerate) i += 30;

			/* Slow digestion takes less food */
			if (p_ptr->slow_digest) i -= 10;

			/* Minimal digestion */
			if (i < 1) i = 1;

			/* Digest some food */
			(void)set_flag(TIMED_FOOD, p_ptr->food - i);
		}
	}

	/* Digest quickly when gorged */
	else
	{
		/* Digest a lot of food */
		(void)set_flag(TIMED_FOOD, p_ptr->food - 100);
	}

	/* Starve to death (slowly) */
	if (p_ptr->food < PY_FOOD_STARVE)
	{
		/* Calculate damage */
		int i = (PY_FOOD_STARVE - p_ptr->food) / 10;

		/* Take damage */
		if (!(p_ptr->invuln))
			take_hit(i, "starvation", MON_STARVATION);
	}

	/* Getting Faint */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
		/* Faint occasionally */
		if (!p_ptr->paralyzed && (rand_int(100) < 10))
		{
			/* Message */
			msg_print("You faint from the lack of food.");
			disturb(1);

			/* Hack -- faint (bypass free action) */
			(void)add_flag(TIMED_PARALYZED, 1 + rand_int(5));
		}
	}
}

static int process_upkeep(void)
{
	int i, n = 0, l = 0;

	for (i = 1; i < m_max; i++)
	{
		if (m_list[i].r_idx && (m_list[i].smart & SM_ALLY))
		{
			n++;
			l += r_info[m_list[i].r_idx].level;
		}
	}

	/* No pets, no upkeep. */
	if (!n) return 0;

#ifdef TRACK_FRIENDS
	if (cheat_wzrd) msg_format("Total friends: %d.", n);
#endif

	/* Not enough pets to be costly. */
	if (n <= 1 + (skill_set[SKILL_RACIAL].value / 40)) return 0;

	/* Put "l" within reasonable bounds. */
	i = MIN(MAX(l, 10), 100);

#ifdef TRACK_FRIENDS
	if (cheat_wzrd) msg_format("Levels %d, upkeep %d", l, i);
#endif

	return i;
}

static int get_regen_amount(void)
{
	int mult = 1;

	/* Sneaking or Resting */
	if (p_ptr->sneaking || command_cmd == 'R')
	{
		mult *= 2;
	}

	/* Regeneration, except on the pattern (?). */
	if (p_ptr->regenerate && !pattern_effect_p())
	{
		mult *= 2;
	}

	/* Lower regeneration */
	if (p_ptr->food < PY_FOOD_STARVE)
	{
		return 0;
	}
	else if (p_ptr->food < PY_FOOD_FAINT)
	{
		return PY_REGEN_FAINT;
	}
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
		return PY_REGEN_WEAK;
	}
	else
	{
		return PY_REGEN_NORMAL;
	}
}

static void process_regeneration(bool cave_no_regen)
{
	const int regen_amount = get_regen_amount();

	/* Regenerate the mana */
	if ((p_ptr->csp < p_ptr->msp) || (p_ptr->cchi < p_ptr->mchi))
	{
		int upkeep_factor = process_upkeep();

		if (upkeep_factor)
		{
			int upkeep_regen = (((100 - upkeep_factor) * regen_amount) / 100);
			regenmana(upkeep_regen);

#ifdef TRACK_FRIENDS
			if (cheat_wzrd)
				msg_format("Regen: %d/%d", upkeep_regen, regen_amount);
#endif
		}
		else
		{
			regenmana(regen_amount);
		}
	}

	/* Regenerate Hit Points if needed */
	if ((p_ptr->chp < p_ptr->mhp) && !(cave_no_regen))
	{
		regenhp(regen_amount);
	}
}

/*
 * Timeout various p_ptr variables.
 */
static void process_timeout(void)
{
	int i, adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

	for (i = 0; i < TIMED_MAX; i++)
	{
		switch (i)
		{
			case TIMED_POISONED: case TIMED_STUN:
			{
				/* Problems healed quickly by high constitution */
				add_flag(i, -adjust);
				break;
			}
			case TIMED_CUT:
			{
				/* Hack - very severe cuts do not heal over time. */
				if (p_ptr->cut < 1001) add_flag(i, -adjust);
				break;
			}
			case TIMED_FOOD:
			{
				/* Food is handled in another function. */
				break;
			}
			default:
			{
				/* Everything else uses a simple counter. */
				add_flag(i, -1);
			}
		}
	}
}

static void process_light(void)
{
	object_type *o_ptr = inventory+INVEN_LITE;

	/* No object. */
	if (!o_ptr->k_idx) return;

	/* Don't burn strange objects. */
	if (o_ptr->tval != TV_LITE) return;

	/* Don't burn permanent lights. */
	if (allart_p(o_ptr)) return;

	/* Do nothing with known exhausted lights. */
	if (o_ptr->ident & IDENT_EMPTY) return;

	/* Decrease life-span if possible. */
	if (o_ptr->pval) o_ptr->pval--;

	/* Hack -- notice interesting fuel steps */
	if ((o_ptr->pval < 100) || (!(o_ptr->pval % 100)))
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* The light is now out */
	if (o_ptr->pval == 0)
	{
		disturb(0);
		msg_print("Your light has gone out!");

		/* Remember it. */
		o_ptr->ident |= IDENT_EMPTY;
	}

	/* The light is getting dim */
	else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
	{
		if (disturb_minor) disturb(0);
		msg_print("Your light is growing faint.");
	}
}

/*
 * Process chaos feature effects.
 */
static void process_chaos(void)
{
	int i;

	/* Don't ask me... */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		if ((p_has_mutation(MUT_BERS_RAGE)) && (randint(3000)==1))
		{
			disturb(0);
			msg_print("RAAAAGHH!");
			msg_print("You feel a fit of rage coming over you!");
			(void) add_flag(TIMED_SHERO, 10 + randint(skill_set[SKILL_RACIAL].value/2));
		}

		if ((p_has_mutation(MUT_COWARDICE)) && (randint(3000)==13))
		{
			if (!(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
			{
				disturb(0);
				msg_print("It's so dark... so scary!");
				no_msg_print = TRUE;
				add_flag(TIMED_AFRAID, rand_range(14, 39));
				no_msg_print = FALSE;
			}
		}

		if ((p_has_mutation(MUT_RTELEPORT)) && (randint(5000)==88))
		{
			if (!(p_ptr->resist_nexus) && !(p_has_mutation(MUT_VTELEPORT))
				&& !(p_ptr->anti_tele))
			{
				disturb(0);

				/* Teleport player */
				msg_print("Your position suddenly seems very uncertain...");
				msg_print(NULL);
				teleport_player(40);
			}
		}

		if ((p_has_mutation(MUT_ALCOHOL)) && (randint(6400)==321))
		{
			if (!(p_ptr->resist_chaos || p_ptr->resist_conf))
			{
				disturb(0);
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
						(void)add_flag(TIMED_CONFUSED, rand_int(20) + 15);
					}

					if ((randint(3)==1) && !(p_ptr->resist_chaos))
					{
						msg_print("Thishcischs GooDSChtuff!");
						(void)add_flag(TIMED_IMAGE, rand_int(150) + 150);
					}
				}
			}
		}

		if ((p_has_mutation(MUT_HALLU)) && (randint(6400)==42))
		{
			if (!(p_ptr->resist_chaos))
			{
				disturb(0);
				(void)add_flag(TIMED_IMAGE, rand_int(50) + 20);
			}
		}

		if ((p_has_mutation(MUT_FLATULENT)) && (randint(3000)==13))
		{
			disturb(0);

			msg_print("BRRAAAP! Oops.");
			msg_print(NULL);
			fire_ball(GF_POIS, 0, skill_set[SKILL_RACIAL].value/2,3);
		}

		if ((p_has_mutation(MUT_PROD_MANA)) &&
			(!(p_ptr->anti_magic)) && (randint(9000)==1))
		{
			int dire = 0;
			disturb(0);
			msg_print("Magical energy flows through you! You must release it!");
			flush();
			msg_print(NULL);
			(void)get_hack_dir(&dire);
			fire_ball(GF_MANA, dire, skill_set[SKILL_RACIAL].value, 3);
		}

		if ((p_has_mutation(MUT_ATT_DEMON)) &&
			(!(p_ptr->anti_magic)) && (randint(6666)==666))
		{
			bool d_summon = summon_specific_aux(py, px,
				dun_level, SUMMON_DEMON, TRUE, !rand_int(6));

			if (d_summon)
			{
				msg_print("You have attracted a demon!");
				disturb(0);
			}
		}

		if ((p_has_mutation(MUT_SPEED_FLUX)) && (randint(6000)==1))
		{

			disturb(0);
			if (randint(2)==1)
			{
				msg_print("You feel less energetic.");
				if (p_ptr->fast > 0)
				{
					set_flag(TIMED_FAST, 0);
				}
				else
				{
					add_flag(TIMED_SLOW, randint(30) + 10);
				}
			}
			else
			{
				msg_print("You feel more energetic.");
				if (p_ptr->slow > 0)
				{
					set_flag(TIMED_SLOW, 0);
				}
				else
				{
					add_flag(TIMED_FAST, randint(30) + 10);
				}
			}
			msg_print(NULL);
		}
		if ((p_has_mutation(MUT_BANISH_ALL)) &&
			(randint(9000)==1))
		{

			disturb(0);
			msg_print("You suddenly feel almost lonely.");
			banish_monsters(100);
			msg_print(NULL);
		}
		if ((p_has_mutation(MUT_EAT_LIGHT)) && (randint(3000) == 1))
		{
			object_type *o_ptr = &inventory[INVEN_LITE];

			msg_print("A shadow passes over you.");
			msg_print(NULL);

			/* Absorb light from the current possition */
			if (cave[py][px].info & CAVE_GLOW)
			{
				hp_player(10);
			}

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
							disturb(0);
							msg_print("Your light has gone out!");
					}

					/* The light is getting dim */
					else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
					{
							if (disturb_minor) disturb(0);
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
		if ((p_has_mutation(MUT_ATT_ANIMAL)) && !(p_ptr->anti_magic) &&
			(randint(7000)==1))
		{

			bool a_summon = summon_specific_aux(py,
				px, dun_level, SUMMON_ANIMAL, TRUE, !rand_int(3));
			if (a_summon)
			{
				msg_print("You have attracted an animal!");
				disturb(0);
			}
		}
		if ((p_has_mutation(MUT_RAW_CHAOS)) && !(p_ptr->anti_magic) &&
			(randint(8000)==1))
		{

			disturb(0);
			msg_print("You feel the world warping around you!");
			msg_print(NULL);
			fire_ball(GF_CHAOS, 0, skill_set[SKILL_RACIAL].value/2,8);
		}
		if ((p_has_mutation(MUT_NORMALITY)) &&
			(randint(5000)==1))
		{
			lose_chaos_feature(0);
		}
		if ((p_has_mutation(MUT_WRAITH)) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{

			disturb(0);
			msg_print("You feel insubstantial!");
			msg_print(NULL);
			add_flag(TIMED_WRAITH, randint(skill_set[SKILL_RACIAL].value/4) + (skill_set[SKILL_RACIAL].value/4));
		}
		if ((p_has_mutation(MUT_POLY_WOUND)) &&
			(randint(3000)==1))
		{
			do_poly_wounds(MON_DANGEROUS_MUTATION);
		}
		if ((p_has_mutation(MUT_WASTING)) &&
			(randint(3000)==13))
		{
			int which_stat = rand_int(6);
			if (!p_ptr->sustain[which_stat])
			{
				disturb(0);
				msg_print("You can feel yourself wasting away!");
				msg_print(NULL);
				(void)dec_stat(which_stat, randint(6)+6, randint(3)==1);
			}
		}
		if ((p_has_mutation(MUT_ATT_DRAGON)) && !(p_ptr->anti_magic) &&
			(randint(3000)==13))
		{

			bool d_summon = summon_specific_aux(py,
				px, dun_level, SUMMON_DRAGON, TRUE, !rand_int(5));

			if (d_summon)
			{
				msg_print("You have attracted a dragon!");
				disturb(0);
			}
		}
		if ((p_has_mutation(MUT_WEIRD_MIND)) && !(p_ptr->anti_magic) &&
			(randint(3000)==1))
		{
			if (p_ptr->tim_esp > 0)
			{
				msg_print("Your mind feels cloudy!");
				set_flag(TIMED_ESP, 0);
			}
			else
			{
				msg_print("Your mind expands!");
				set_flag(TIMED_ESP, skill_set[SKILL_RACIAL].value/2);
			}
		}
		if ((p_has_mutation(MUT_NAUSEA)) && !(p_ptr->slow_digest) &&
			(randint(9000)==1))
		{

			disturb(0);
			msg_print("Your stomach roils, and you lose your lunch!");
			msg_print(NULL);
			set_flag(TIMED_FOOD, PY_FOOD_WEAK);
		}

		if ((p_has_mutation(MUT_WALK_SHAD)) &&
			!(p_ptr->anti_magic) &&
			(randint(12000) == 1))
		{
			alter_reality();
		}

		if ((p_has_mutation(MUT_WARNING)) &&
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
		if ((p_has_mutation(MUT_INVULN)) && !(p_ptr->anti_magic) &&
			(randint(5000)==1))
		{

			disturb(0);
			msg_print("You feel invincible!");
			msg_print(NULL);
			(void)add_flag(TIMED_INVULN, randint(8) + 8);
		}
		if ((p_has_mutation(MUT_SP_TO_HP)) &&
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
		if ((p_has_mutation(MUT_HP_TO_SP)) && !(p_ptr->anti_magic) &&
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
				take_hit(healing, "blood rushing to the head", MON_DANGEROUS_MUTATION);
			}
		}
		if ((p_has_mutation(MUT_DISARM)) &&
			(randint(10000)==1))
		{
			object_type *o_ptr = &inventory[INVEN_WIELD];

			disturb(0);
			msg_print("You trip over your own feet!");
			take_hit(randint(p_ptr->wt/6), "tripping", MON_DANGEROUS_MUTATION);

			msg_print(NULL);
			if (o_ptr->k_idx)
			{
				msg_print("You drop your weapon!");
				inven_drop(o_ptr,1);
			}
		}
	}
}

/*
 * Recharge a charging object.
 */
static void process_recharge(object_type *o_ptr, int mode)
{
	/* Skip non-objects. */
	if (!o_ptr->k_idx) return;

	/* Skip non-rods (except when equipped). */
	if (mode != OUP_EQUIP && o_ptr->tval != TV_ROD) return;

	/* Skip recharged rods. */
	if (!o_ptr->timeout) return;

	/* Charge it, do nothing else if still recharging. */
	if (--o_ptr->timeout) return;

	if (mode & OUP_CARRIED_MASK)
	{
		/* Tell the player if requested. */
		recharged_notice(o_ptr);

		/* Recalculate/redraw stuff (later) */
		update_object(o_ptr);
	}
}

/*
 * Do various things to an object known to be equipped.
 */
static void process_equip(object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Skip non-objects */
	if (!o_ptr->k_idx) return;

	object_flags(o_ptr, &f1, &f2, &f3);

	/*
	 * Auto-curse.
	 * Curse_turn is set in remove_curse_aux()).
	 */
	if (f3 & TR3_AUTO_CURSE && ~(o_ptr->ident) & IDENT_CURSED &&
		turn > curse_turn)
	{
		curse(o_ptr);
		msg_format("The %v suddenly feels deathly cold!",
			object_desc_f3, o_ptr, FALSE, 0);
	}

	/* TY-curse. */
	if ((f3 & TR3_TY_CURSE) && (randint(TY_CURSE_CHANCE)==1))
		activate_ty_curse();
	if ((o_ptr->name1 == ART_DEMONBLADE) && randint(CHAINSWORD_NOISE) == 1)
	{
		msg_format("%v", get_rnd_line_f1, "chainswd.txt");
		disturb(FALSE);
	}

	/* Hack: Uncursed teleporting items (e.g. Planar Weapons)
	can actually be useful! */

	if ((f3 & TR3_TELEPORT) && (rand_int(100)<1))
	{
		if ((o_ptr->ident & IDENT_CURSED) && !(p_ptr->anti_tele))
		{
			disturb(0);
			/* Teleport player */
			teleport_player(40);
		}
		else
		{
			if (strchr(get_inscription(o_ptr),'.'))
			{
				/* Do nothing */
				/* msg_print("Teleport aborted.") */ ;
			}
			else if (player_has_flag_known(3, TR3_NO_TELE))
			{
				/* Give a message to make it clear what's happening. */
				msg_print(
					"You feel uncertain about your position for a moment.");
			}
			else if (get_check("Teleport? "))
			{
				disturb(0);
				teleport_player(50);
			}
		}
	}

	/* Recharge if it is activatable. */
	process_recharge(o_ptr, OUP_EQUIP);

	/* Hack - handle damage from the Gemstone 'Trapezohedron'. */
	if (o_ptr->name1 == ART_TRAPEZOHEDRON && !p_ptr->invuln &&
		!p_ptr->anti_magic && !rand_int(999))
	{
		msg_format("The %v drains life from you!",
			object_desc_f3, o_ptr, FALSE, 0);
		take_hit(MIN(skill_set[SKILL_TOUGH].value/2, 50),
			"the Shining Trapezohedron", MON_DANGEROUS_EQUIPMENT);
	}
}

static void process_spirit(spirit_type *s_ptr)
{
	/* Nothing to do. */
	if(!s_ptr->annoyance) return;

	/* Become less annoyed, and redraw just in case it should look different. */
	s_ptr->annoyance--;
	p_ptr->redraw |= PR_SPIRIT;

	/* Disturb if newly placated and requested. */
	if (!s_ptr->annoyance && disturb_minor) disturb(0);
}

/*
 * Move the player to the dungeon/surface.
 */
static void process_recall(void)
{
	/* Not started. */
	if (!p_ptr->word_recall) return;

	/* Not finished. */
	if (--p_ptr->word_recall) return;

	/* Disturbing! */
	disturb(0);

	/* Sound */
	sound(SOUND_TPLEVEL);

	/* Determine the level */
	if (!dun_level)
	{
		msg_print("You feel yourself yanked downwards!");

		change_level(MAX(1, p_ptr->max_dlv), START_RANDOM);

		cur_dungeon=recall_dungeon;
	}
	else
	{
		msg_print("You feel yourself yanked upwards!");

		change_level(0, START_RANDOM);

		wildx=town_defs[cur_town].x;
		wildy=town_defs[cur_town].y;
	}
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i;
	bool    cave_no_regen;

	/* Hack - check vampiric drain every turn. */
	add_flag(TIMED_VAMP, -VAMPIRIC_RECOVERY);

	/* Every 10 game turns */
	if (turn % 10) return;

	/* Redraw Time */
	p_ptr->redraw |= PR_TIME;

	/* Check the time and load. */
	check_time_load();

	/* Check autosave. */
	process_autosave();

	/* Handle special times and dates */
	process_time();

	/* Handle monster generation and regeneration. */
	process_monsters_new();

	/* Process most of the things which cause physical damage over time. */
	cave_no_regen = process_damage();

	/* Digest food. */
	process_food();

	/* Carry out the effects of standing on the pattern. */
	pattern_effect();

	/* Regenerate HP and SP. */
	process_regeneration(cave_no_regen);

	/* Allow another chaos reward now. Is this rare enough to be redundant? */
	multi_rew = FALSE;

	/* Process most timed player_type parameters. */
	process_timeout();

	/* Burn fuel or the player as appropriate. */
	process_light();

	/* Handle experience draining */
	if (p_ptr->exp_drain && p_ptr->exp > 0 && !rand_int(500))
	{
			lose_skills(1);
	}

	/* Process randomly activating chaos features. */
	process_chaos();

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) process_equip(inventory+i);

	for (i = 0; i < INVEN_PACK; i++) process_recharge(inventory+i, OUP_INVEN);

	/* calm down spirits */
	for (i=0;i<MAX_SPIRITS;i++) process_spirit(spirits+i);

	/* Feel the inventory */
	sense_inventory();

	/*** Process Objects ***/
	for (i = 1; i < o_max; i++) process_recharge(o_list+i, OUP_FLOOR);

	/* Delayed Word-of-Recall */
	process_recall();
}







#ifdef ALLOW_BORG

/*
 * Verify use of "borg" commands
 */
static bool enter_borg_mode(void)
{
	/* Only ask first time */
	if (noscore & NOSCORE_BORG) return TRUE;

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
	noscore |= NOSCORE_BORG;

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
void process_command(void)
{
	char help_str[20];

#ifdef ALLOW_REPEAT

	/* Handle repeating the last command */
	repeat_check();

#endif /* ALLOW_REPEAT -- TNB */

	/* Look up various object commands from a table. */
	if (do_cmd_use_object(command_cmd)) return;

	/* Track this command (if not instantaneous). */
	strnfmt(help_str, sizeof(help_str),
		"cmd=%v", s16b_to_string_f1, command_cmd);

	help_track(help_str);

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



			/*** Inventory Commands ***/

		/* Reveal all hidden objects. */
		case KTRL('K'):
		{
			do_cmd_unhide_objects();
			break;
		}

			/* Equipment list */
		case 'e':
		{
			do_cmd_inven(TRUE);
			break;
		}

			/* Inventory list */
		case 'i':
		{
			do_cmd_inven(FALSE);
			break;
		}


			/*** Various commands ***/

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

			/* Begin/continue Running -- Arg is Max Distance */
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
			msg_print("You need some peace and quiet to research.");
			msg_print("Why not try a bookstore?");
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

		/* Use a chaos/racial power. */
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
			do_cmd_help(NULL);
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
			Term_user();
			break;
		}

			/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

			/* Interact with options */
		case '=':
		{
			do_cmd_options();
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

		/* Hack - process a temporary function. */
		case '$':
		{
			do_cmd_script();
			break;
		}

		/*** Wizard Commands ***/

#ifdef ALLOW_WIZARD
# ifdef ALLOW_SPOILERS
		/* Hack -- Generate Spoilers */
		case CMD_DEBUG+'"':
		{
			do_cmd_spoilers();
			break;
		}
# endif /* ALLOW_SPOILERS */

		/* Debug help. */
		case CMD_DEBUG+'?':
		{
			show_link("brief debug");
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
			do_cmd_wiz_jump(command_arg);
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
			do_cmd_wiz_learn(command_arg);
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
			(void) gain_level_reward(command_arg, 0);
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

#ifdef ALLOW_WIZARD

			/* Special "debug" commands */
		case KTRL('A'):
		{
			/* Enter debug mode */
			if (cheat_wzrd)
			{
				do_cmd_debug();
				break;
			}
			/* Else fall through to default. */
		}

#endif /* ALLOW_WIZARD */

			/* Hack -- Unknown command */
		default:
		{
			if (one_in(2))
				mc_put_fmt(0, 0, "%v", get_rnd_line_f1, "error.txt");
			else
				mc_put_fmt(0, 0, "Type '?' for help.");
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

	/*
	 * Don't give chaos features here without patrons, as they're the only
	 * things which reward levels like that.
	 */
	if (hack_chaos_feature && chaos_patrons)
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
	if (command_cmd == 'R')
	{
		/* Basic resting */
		if (command_rep == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
				(p_ptr->csp >= p_ptr->msp) &&
				(p_ptr->cchi >= p_ptr->mchi))
			{
				disturb(0);
			}
		}

		/* Complete resting */
		else if (command_rep == -3)
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
				disturb(0);
			}
		}
	}

	/* Hack - allow the character to die messily after it gets a chance to
	 * move. */
	p_ptr->min_hp = -MAX_SHORT;

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" (semi-efficiently for resting) */
		if (command_rep && !(command_cmd == 'R' && command_rep & 0x0F))
		{
			/* Do not wait */
			inkey_scan = TRUE;

			/* Check for a key other than space */
			if (((i = inkey())) && i != ' ')
			{
				/* Flush input */
				flush();

				/* Disturb */
				disturb(0);

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

			object_type *o_ptr;

			/* Access the slot to be dropped */
			o_ptr = &inventory[item];

			/* Disturbing */
			disturb(0);

			/* Warning */
			msg_print("Your pack overflows!");

			/* Message */
			msg_format("You drop %v (%c).",
				object_desc_f3, o_ptr, TRUE, 3, index_to_label(o_ptr));

			/* Drop it (carefully) near the player */
			drop_near(o_ptr, 0, py, px);

			/* Modify, Describe, Optimize */
			item_increase(o_ptr, -255);
			item_describe(o_ptr);
			item_optimize(o_ptr);

			/* Notice stuff (if needed) */
			if (p_ptr->notice) notice_stuff();

			/* Update stuff (if needed) */
			if (p_ptr->update) update_stuff();

			/* Redraw stuff (if needed) */
			if (p_ptr->redraw) redraw_stuff();
		}

		/* Hack -- cancel "lurking browse mode" */
		if (!command_new) command_see = FALSE;


		/* Assume free turn */
		energy_use = 0;


		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Update the windows if needed. */
			window_stuff();

			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];
		}

		/* Repeated command */
		else if (command_rep)
		{
			/* Update the windows if needed. */
			window_stuff();

			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();

			/* Count this execution, if finite. */
			if (command_rep > 0) command_rep--;

			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);
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
	/* Remember if the player is starting a new level. */
	bool old_new_level_flag = new_level_flag;

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
	repair_monsters = TRUE;


	/* Disturb */
	disturb(1);


	/* Track maximum dungeon level */
	if (p_ptr->max_dlv < dun_level)
	{
		p_ptr->max_dlv = dun_level;
	}


	/* Paranoia -- No stairs down from Quest */
	if (is_quest(dun_level))
	{
		if (dun_defs[cur_dungeon].flags & DF_TOWER)
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
	verify_panel(FALSE);

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
	p_ptr->redraw |= (PR_ALL);

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
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_QUIET);

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
	if (is_quest(dun_level)) quest_discovery(old_new_level_flag);

	/* Notice the final level of a dungeon/tower */
	else if (dun_level && dun_level == dun_defs[cur_dungeon].max_level)
	{
		msg_print("You suddenly feel that you can't go on.");
	}

	/* Squelch stuff now the initial messages have been given. */
	p_ptr->notice |= (PN_FSQUELCH);


	/*** Process this dungeon level ***/

	/* Reset the object generation level */
	object_level = (dun_depth);

	hack_mind = TRUE;

	/* Hack - the character can be immortal until its first move. */
	p_ptr->min_hp = (p_ptr->chp - (p_ptr->mhp+1)*2/3 - 1);

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
void process_some_user_pref_files(void)
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
	(void)set_flag(TIMED_BLIND, 0);
	(void)set_flag(TIMED_CONFUSED, 0);
	(void)set_flag(TIMED_POISONED, 0);
	(void)set_flag(TIMED_AFRAID, 0);
	(void)set_flag(TIMED_PARALYZED, 0);
	(void)set_flag(TIMED_IMAGE, 0);
	(void)set_flag(TIMED_STUN, 0);
	(void)set_flag(TIMED_CUT, 0);

	/* Hack -- Prevent starvation */
	(void)set_flag(TIMED_FOOD, PY_FOOD_MAX - 1);

	/* Hack -- cancel recall */
	if (p_ptr->word_recall)
	{
		/* Message */
		msg_print("A tension leaves the air around you...");
		msg_print(NULL);

		/* Hack -- Prevent recall */
		p_ptr->word_recall = 0;
	}

	/* Teleport to town */
	change_level(0, START_RANDOM);

	/* Do not die */
	death = FALSE;

	if (wizard)
	{
		/* Mark social class, reset age, if needed */
		if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

		/* Increase age */
		p_ptr->age++;

		/* Mark savefile */
		noscore |= NOSCORE_WIZARD;
	}
	else
	{
		/* Go to town of recall */
		dun_level = 0;
		cur_town = p_ptr->ritual;
		wildx=town_defs[cur_town].x;
		wildy=town_defs[cur_town].y;

		/* Ritual is used */
		p_ptr->ritual = TOWN_NONE;

		/* Lose most money and all items */
		/* You only keep the change in your pocket */
		if(p_ptr->au > 100) p_ptr->au = 100;
		destroy_pack();
	}
}

/*
 * Initialise the towns and map.
 * Place towns on the map.
 */
static void place_towns(void)
{
	int good_squares[64][2];
	int i, t, x, y;

	/* Hack -- seeds for flavors and wilderness*/
	seed_flavor = rand_int(0x10000000);
	seed_wild = rand_int(0x10000000);
	/* Initialise wilderness grid */
	for (x=0; x<12; x++)
	{
		for(y=0; y<12; y++)
		{
			wild_grid[x][y].seed=rand_int(0x10000000);
			wild_grid[x][y].dungeon=TOWN_NONE;
			wild_grid[x][y].road_map=0;
		}
	}

	/* Place towns and dungeons */
	for(i=0;i<MAX_CAVES;i++)
	{
		const bool town = i < MAX_TOWNS && town_defs[i].name;

		/* Towns can't be generated next to other towns. */
		for (t = 0, x = 2; x < 10; x++)
		{
			for (y = 2; y < 10; y++)
			{
				/* Dungeons can't share a location. */
				if((wild_grid[y][x].dungeon != TOWN_NONE)) continue;

				/* Towns can't be next to other towns. */
				if (town && (is_town_p(y-1, x-1) || is_town_p(y-1, x) ||
					is_town_p(y-1, x+1) || is_town_p(y, x-1) ||
					is_town_p(y, x+1) || is_town_p(y+1, x-1) ||
					is_town_p(y+1, x) || is_town_p(y+1, x+1)))
				{
					continue;
				}

				/* This is a suitable location for this dungeon. */
				good_squares[t][0] = x;
				good_squares[t++][1] = y;
			}
		}

		/* Locations were found, so pick one. */
		if (t)
		{
			t = rand_int(t);
			x = good_squares[t][0];
			y = good_squares[t][1];

			/* There are no dungeons next to this */
			wild_grid[y][x].dungeon = i;

			/* now let the town & dungeon know where they are */
			if (town)
			{
				town_defs[i].x=x;
				town_defs[i].y=y;
			}
			dun_defs[i].x=x;
			dun_defs[i].y=y;
		}
		else
		{
			msg_format("Can't place dungeon %d", i);
		}
	}

	/* Choose a starting town. */
	do
	{
		cur_town = rand_int(MAX_CAVES);
	}
	while(!(dun_defs[cur_town].flags & DF_START) || !(dun_defs[cur_town].x));
}

/*
 * Generate roads to connect the towns together.
 */
static void place_roads(void)
{
	int i;

	for(i=0;i<MAX_TOWNS-1;)
	{
		int cur_x,cur_y,dest_x,dest_y;
		int x_disp,y_disp,x_sgn,y_sgn;
		int fin, j;
		byte curdir,nextdir;

		cur_x=town_defs[i].x;
		cur_y=town_defs[i].y;
		for (j = i+1; j < MAX_TOWNS; j++) if (town_defs[j].name) break;
		if (j == MAX_TOWNS) break;
		dest_x=town_defs[j].x;
		dest_y=town_defs[j].y;
		i = j;
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
}

/*
 * Create a new character, and all that goes with it.
 */
static void create_character(void)
{
	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Initialise stuff and add towns in. */
	place_towns();

	/* Generate road map... */
	place_roads();

	/* Start in town 0 */
	dun_level = 0;
	cur_dungeon = cur_town;
	p_ptr->max_dlv = 0;
	dun_offset = 0;
	dun_bias = 0;
	wildx=town_defs[cur_town].x;
	wildy=town_defs[cur_town].y;
	came_from = START_RANDOM;

	/* Roll up a new character */
	player_birth();

	/* Hack -- enter the world at dawn unless undead*/
	if (rp_ptr->grace == RACE_UNDEAD)
	{
		turn=5*TOWN_DAWN+1;
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

	/* Hack -- Character is "icky" */
	character_icky = TRUE;


	/* Hack -- turn off the cursor */
	(void)Term_set_cursor(0);

	/* Initialise the resize hook XXX XXX XXX */
	term_screen->resize_hook = resize_main_term;

	/* The main screen always attempts to resize the map. */
	add_resize_hook(resize_map);

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
		if (!load_player(new_game))
		{
			/* Oops */
			quit("broken savefile");
		}

#ifdef WINDOWS
	}
#endif /* WINDOWS */

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
	if (Rand_place == RAND_DEG)
	{
		/* Basic seed */
		u32b seed = (time(NULL));

#ifdef SET_UID

		/* Mutate the seed on Unix machines */
		seed = ((seed >> 3) * (getpid() << 1));

#endif
		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}

#ifdef USE_MAIN_C
	/* Display news.txt again, as in init_angband(). This
	 * is done because news.txt was removed when the save game
	 * was loaded, and we didn't know whether to wait below beforehand.
	 */
	if (display_credits)
	{
		clear_from(1);
		showfile("news.txt", 1);
		pause_line();
	}
#endif


	/* Roll new character */
	if (new_game) create_character();

	/* Remove the stat_default array */
	KILL(stat_default);

	/* Flash a message if none are already shown. */
	if (!msg_flag) prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();

	/* Flavor the objects */
	flavor_init();

	/* Reset the visual mappings */
	reset_visuals();


	/* Update stuff */
	p_ptr->update |= (PU_MA_ARMOUR);

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

	/* Set quest monsters. */
	set_guardians();

	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Reset the suitable monster list if not done above. */
	else get_mon_num_prep(NULL, 0);


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Character is no longer "icky" */
	character_icky = FALSE;


	/* Start game */
	alive = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) death = TRUE;

	/* Hack - display a special help file if allowed. */
	if (!death && beginner_help)
	{
		do_cmd_help("beginner.txt");
	}

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
			if ((cheat_live) && !get_check("Die? "))
			{
				resurrect(TRUE);
			}

			/* Almost identical code -- Allow player to recall at point of death */
			else if (p_ptr->ritual != TOWN_NONE)
			{
				resurrect(FALSE);
			}

			/* Forget died_from. */
			if (!death) safe_free((vptr)died_from);
		}

		/* Handle "death" */
		if (death) break;

		/* Set quest monsters for the new level. */
		set_guardians();

		/* Make a new level */
		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}


