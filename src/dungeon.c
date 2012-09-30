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
#include "script.h"

#define TY_CURSE_CHANCE 100


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static byte value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (FLAG(o_ptr, TR_INSTA_ART))
	{
		/* Cursed / Worthless */
		if (cursed_p(o_ptr) || !o_ptr->cost) return FEEL_TERRIBLE;

		/* Normal */
		return FEEL_SPECIAL;
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Ego items with negative pvals or flags like aggravate or teleport */
		if (!o_ptr->cost)
		{
			return FEEL_WORTHLESS;
		}
		else if (cursed_p(o_ptr))
		{
			return FEEL_TAINTED;
		}

		/* Normal */
		return FEEL_EXCELLENT;
	}

	/* Broken items */
	if (!o_ptr->cost) return FEEL_BROKEN;

	/* Good bonus */
	if ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0))
	{
		/* Cursed good item? */
		if (cursed_p(o_ptr)) return FEEL_DUBIOUS;

		/* Normal good item */
		return FEEL_GOOD;
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return FEEL_CURSED;

	/* Worthless is "bad" */
	if (!object_value(o_ptr)) return FEEL_BAD;

	/* Default to "average" */
	return FEEL_AVERAGE;
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static byte value_check_aux2(const object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return FEEL_CURSED;

	/* Broken items (all of them) */
	if (o_ptr->cost <= 0) return FEEL_BROKEN;

	/* Artifacts -- except cursed/broken ones */
	if (FLAG(o_ptr, TR_INSTA_ART)) return FEEL_GOOD;

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return FEEL_GOOD;

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return FEEL_GOOD;

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return FEEL_GOOD;

	/* Worthless is "bad" */
	if (!object_value(o_ptr)) return FEEL_BAD;

	/* No feeling */
	return FEEL_NONE;
}

/*
 * Psuedo-id the item
 */
void sense_item(object_type *o_ptr, bool heavy, bool wield, bool msg)
{
	byte feel;

	int slot;

	bool okay = FALSE;

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
		case TV_FIGURINE:
		{
			if (!heavy)
				okay = TRUE;
			break;
		}
		default:
		{
			/* Skip */
			return;
		}
	}

	/* We know about it already, do not tell us again */
	if (o_ptr->info & (OB_SENSE)) return;

	/* It is fully known, no information needed */
	if (object_known_p(o_ptr)) return;

	/* Occasional failure on inventory items */
	if (!wield && !one_in_(5)) return;

	/* Good luck */
	if ((p_ptr->muta3 & MUT3_GOOD_LUCK) && !one_in_(13))
	{
		heavy = TRUE;
	}

	/* Check for a feeling */
	feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

	/* Skip non-changes */
	if (feel == o_ptr->feeling) return;

	/* hack the knowledge flag */
	if (cursed_p(o_ptr)) o_ptr->kn_flags[2] |= TR2_CURSED;

	/* Bad luck */
	if ((p_ptr->muta3 & MUT3_BAD_LUCK) && !one_in_(13))
	{
		switch (feel)
		{
			case FEEL_TERRIBLE:
			{
				feel = FEEL_SPECIAL;
				break;
			}
			case FEEL_WORTHLESS:
			{
				feel = FEEL_EXCELLENT;
				break;
			}
			case FEEL_CURSED:
			{
				feel = one_in_(3) ? FEEL_AVERAGE : FEEL_GOOD;
				break;
			}
			case FEEL_AVERAGE:
			{
				feel = one_in_(2) ? FEEL_BAD : FEEL_GOOD;
				break;
			}
			case FEEL_GOOD:
			case FEEL_BAD:
			{
				feel = one_in_(3) ? FEEL_AVERAGE : FEEL_CURSED;
				break;
			}
			case FEEL_EXCELLENT:
			{
				feel = FEEL_WORTHLESS;
				break;
			}
			case FEEL_SPECIAL:
			{
				feel = FEEL_TERRIBLE;
				break;
			}
		}
	}

	/* Stop everything */
	if (disturb_minor) disturb(FALSE);

	/* Message */
	if (msg)
	{
		/* Message (equipment) */
		if (wield)
		{
			slot = GET_ARRAY_INDEX(p_ptr->equipment, o_ptr);

			msgf("You feel the %v (%c) you are %s %s %s...",
				   OBJECT_FMT(o_ptr, FALSE, 0), I2A(slot),
				   describe_use(slot),
				   ((o_ptr->number == 1) ? "is" : "are"),
				   game_inscriptions[feel]);
		}

		/* Message (inventory) */
		else
		{
			slot = get_item_position(p_ptr->inventory, o_ptr);

			msgf("You feel the %v (%c) in your pack %s %s...",
				   OBJECT_FMT(o_ptr, FALSE, 0), I2A(slot),
				   ((o_ptr->number == 1) ? "is" : "are"),
				   game_inscriptions[feel]);
		}
	}

	/* We have "felt" it */
	o_ptr->info |= (OB_SENSE);

	/* Set the "inscription" */
	o_ptr->feeling = feel;

	/* Notice changes */
	notice_item();
}


/*
 * Sense the inventory
 *
 *   Class 0 = Warrior --> fast and heavy
 *   Class 1 = Mage    --> slow and light
 *   Class 2 = Priest  --> fast but light
 *   Class 3 = Rogue   --> okay and heavy
 *   Class 4 = Ranger  --> slow but heavy  (changed!)
 *   Class 5 = Paladin --> slow but heavy
 */
static void sense_inventory(void)
{
	int i;
	bool heavy;

	object_type *o_ptr;

	long difficulty;


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->tim.confused) return;
	
	/* Analyze the class */
	switch (p_ptr->rp.pclass)
	{
		case CLASS_WARRIOR:
		{
			/* Good (heavy) sensing */
			difficulty = 9000L;

			/* Done */
			break;
		}

		case CLASS_MAGE:
		case CLASS_HIGH_MAGE:
		{
			/* Very bad (light) sensing */
			difficulty = 240000L;

			/* Done */
			break;
		}

		case CLASS_PRIEST:
		{
			/* Good (light) sensing */
			difficulty = 10000L;

			/* Done */
			break;
		}

		case CLASS_ROGUE:
		{
			/* Okay sensing */
			difficulty = 20000L;

			/* Done */
			break;
		}

		case CLASS_RANGER:
		{
			/* Bad (heavy) sensing */
			difficulty = 95000L;

			/* Done */
			break;
		}

		case CLASS_PALADIN:
		{
			/* Bad (heavy) sensing */
			difficulty = 77777L;

			/* Done */
			break;
		}

		case CLASS_WARRIOR_MAGE:
		{
			/* Bad sensing */
			difficulty = 75000L;

			/* Done */
			break;
		}

		case CLASS_MINDCRAFTER:
		{
			/* Bad sensing */
			difficulty = 55000L;
	
			/* Done */
			break;
		}

		case CLASS_CHAOS_WARRIOR:
		{
			/* Bad (heavy) sensing */
			difficulty = 80000L;

			/* Done */
			break;
		}

		case CLASS_MONK:
		{
			/* Okay sensing */
			difficulty = 20000L;
			heavy = FALSE;

			/* Done */
			break;
		}

		default:
		{
			/* Paranoia */
			difficulty = 0;
		}
	}

	/*
	 * Scale difficulty depending on sensing ability 
	 * This can be affected by objects.
	 */
	difficulty /= (p_ptr->skills[SKILL_SNS] > 0 ? p_ptr->skills[SKILL_SNS] : 1);

	/* Rescale larger by a facter of 25 */
	difficulty *= 25;

	/* Sensing gets better as you get more experienced */
	difficulty /= p_ptr->lev * p_ptr->lev + 40;

	/* Does it work? */
	if (!(one_in_(difficulty))) return;
	
	/* Heavy sensing? */
	heavy = class_info[p_ptr->rp.pclass].heavy_sense;

	/*** Sense everything ***/

	/* Scan Equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = &p_ptr->equipment[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		sense_item(o_ptr, heavy, TRUE, TRUE);
	}

	/* Scan inventory */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		sense_item(o_ptr, heavy, FALSE, TRUE);
	}
	OBJ_ITT_END;
}


/*
 * Go to any level (ripped off from wiz_jump)
 */
static void pattern_teleport(void)
{
	int min_level = 0;
	int max_level = 99;

	/* Ask for level */
	if (get_check("Teleport level? "))
	{
		char tmp_val[160];

		/* Only downward in ironman mode */
		if (ironman_downward)
			min_level = p_ptr->depth;

		/* Maximum level */
		if (p_ptr->depth > 100)
			max_level = dungeon()->max_level;
		else if (p_ptr->depth == 100)
			max_level = 100;

		/* Default */
		strnfmt(tmp_val, 160, "%d", p_ptr->depth);

		/* Ask for a level */
		if (!get_string(tmp_val, 11, "Teleport to level (%d-%d): ",
						min_level, max_level)) return;

		/* Extract request */
		p_ptr->cmd.arg = atoi(tmp_val);
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
	if (p_ptr->cmd.arg < min_level) p_ptr->cmd.arg = min_level;

	/* Paranoia */
	if (p_ptr->cmd.arg > max_level) p_ptr->cmd.arg = max_level;

	/* Accept request */
	msgf("You teleport to dungeon level %d.", p_ptr->cmd.arg);

	/* Change level */
	p_ptr->depth = p_ptr->cmd.arg;

	/* Leaving */
	p_ptr->state.leaving = TRUE;
}


static void wreck_the_pattern(void)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int to_ruin, r_y, r_x;

	if (area(px, py)->feat == FEAT_PATTERN_XTRA2)
	{
		/* Ruined already */
		return;
	}

	msgf("You bleed on the Pattern!");
	msgf("Something terrible happens!");

	if (!p_ptr->tim.invuln)
		take_hit(damroll(10, 8), "corrupting the Pattern");

	to_ruin = rand_range(35, 80);

	while (to_ruin--)
	{
		scatter(&r_x, &r_y, px, py, 4);

		if ((area(r_x, r_y)->feat >= FEAT_PATTERN_START) &&
			(area(r_x, r_y)->feat < FEAT_PATTERN_XTRA2))
		{
			cave_set_feat(r_x, r_y, FEAT_PATTERN_XTRA2);
		}
	}

	cave_set_feat(px, py, FEAT_PATTERN_XTRA2);
}


/*
 * Returns TRUE if we are on the Pattern...
 */
static bool pattern_effect(void)
{
	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

	if ((c_ptr->feat < FEAT_PATTERN_START) ||
		(c_ptr->feat > FEAT_PATTERN_XTRA2))
		return FALSE;

	if ((p_ptr->rp.prace == RACE_AMBERITE) && (p_ptr->tim.cut > 0) && one_in_(10))
	{
		wreck_the_pattern();
	}

	if (c_ptr->feat == FEAT_PATTERN_END)
	{
		(void)clear_poisoned();
		(void)clear_image();
		(void)clear_stun();
		(void)clear_cut();
		(void)clear_blind();
		(void)clear_afraid();
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)hp_player(1000);
		cave_set_feat(p_ptr->px, p_ptr->py, FEAT_PATTERN_OLD);
		msgf("This section of the Pattern looks less powerful.");
	}


	/*
	 * We could make the healing effect of the
	 * Pattern center one-time only to avoid various kinds
	 * of abuse, like luring the win monster into fighting you
	 * in the middle of the pattern...
	 */

	else if (c_ptr->feat == FEAT_PATTERN_OLD)
	{
		/* No effect */
	}
	else if (c_ptr->feat == FEAT_PATTERN_XTRA1)
	{
		pattern_teleport();
	}
	else if (c_ptr->feat == FEAT_PATTERN_XTRA2)
	{
		if (!p_ptr->tim.invuln)
			take_hit(200, "walking the corrupted Pattern");
	}
	else
	{
		if ((p_ptr->rp.prace == RACE_AMBERITE) && one_in_(2))
			return TRUE;
		else if (!p_ptr->tim.invuln)
			take_hit(damroll(1, 3), "walking the Pattern");
	}

	return TRUE;
}


/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	u32b new_chp, new_chp_frac;
	int old_chp;

	/* Save the old hitpoints */
	old_chp = p_ptr->chp;

	/* Extract the new hitpoints */
	new_chp = ((u32b)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += (s16b)(new_chp >> 16);	/* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
	if (new_chp_frac >= 0x10000L)
	{
		p_ptr->chp_frac = (u16b)(new_chp_frac - 0x10000L);
		p_ptr->chp++;
	}
	else
	{
		p_ptr->chp_frac = (u16b)new_chp_frac;
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
 */
static void regenmana(int percent)
{
	u32b new_mana, new_mana_frac;
	int old_csp;

	old_csp = p_ptr->csp;
	new_mana = ((u32b)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += (s16b)(new_mana >> 16);	/* div 65536 */

	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = (u16b)(new_mana_frac - 0x10000L);
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = (u16b)(new_mana_frac);
	}

	/* Must set frac to zero even if equal */
	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
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
			if (FLAG(r_ptr, RF_REGENERATE)) frac *= 2;

			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
		}
	}
}


void notice_lite_change(object_type *o_ptr)
{
	/* Hack -- notice interesting fuel steps */
	if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
	{
		/* Notice changes */
		notice_equip();
	}

	/* Hack -- Special treatment when blind */
	if (p_ptr->tim.blind)
	{
		/* Hack -- save some light for later */
		if (o_ptr->timeout == 0) o_ptr->timeout++;
	}

	/* The light is now out */
	else if (o_ptr->timeout == 0)
	{
		disturb(FALSE);
		msgf("Your light has gone out!");

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* The light is getting dim */
	else if ((o_ptr->timeout < 100) && (!(o_ptr->timeout % 10)))
	{
		if (disturb_minor) disturb(FALSE);
		msgf("Your light is growing faint.");
	}
}

static bool item_tester_unsensed(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Check to see if we have identified the item */
	if (object_known_p(o_ptr)) return (FALSE);
	
	/* Cannot sense flavoured items */
	if (k_ptr->flavor) return (FALSE);

	return (TRUE);
}


/*
 * Forcibly pseudo-identify an object in the inventory
 * (or on the floor)
 */
bool psychometry(void)
{
	object_type *o_ptr;
	byte feel;
	cptr q, s;
	
	/* Only un-id'ed items */
	item_tester_hook = item_tester_unsensed;

	/* Get an item */
	q = "Meditate on which item? ";
	s = "You have nothing appropriate.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return (FALSE);

	/* Check for a feeling */
	feel = value_check_aux1(o_ptr);

	/* Skip non-feelings */
	if (!feel)
	{
		msgf("You do not perceive anything unusual about the %v.",
				   OBJECT_FMT(o_ptr, FALSE, 0));
		return TRUE;
	}

	msgf("You feel that the %v %s %s...",
			   OBJECT_FMT(o_ptr, FALSE, 0),
			   ((o_ptr->number == 1) ? "is" : "are"),
			   game_inscriptions[feel]);

	/* We have "felt" it */
	o_ptr->info |= (OB_SENSE);

	/* hack the knowledge flag */
	if (cursed_p(o_ptr)) o_ptr->kn_flags[2] |= TR2_CURSED;

	/* "Inscribe" it */
	o_ptr->feeling = feel;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	
	/* Notice changes */
	notice_item();

	/* Something happened */
	return (TRUE);
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(const object_type *o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->inscription) return;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->inscription), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Count the item */
			if (o_ptr->number == 1)
			{
				/* One item has recharged */
				msgf("Your %v has recharged.", OBJECT_FMT(o_ptr, FALSE, 0));
			}
			else
			{
				object_kind *k_ptr = &k_info[o_ptr->k_idx];

				/* Find out how many are recharging still */
				int power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

				/* Watch the top */
				if (power > o_ptr->number) power = o_ptr->number;

				/* All are charged now */
				if (power == 0)
				{
					msgf("All %d of your %v are charged.", o_ptr->number,
						OBJECT_FMT(o_ptr, FALSE, 0));
				}
				/* One is charged */
				else if (o_ptr->number - power == 1)
				{
					msgf("One of your %v is charged.", 
						OBJECT_FMT(o_ptr, FALSE, 0));
				}
				/* Some are charged, some are recharging */
				else
				{
					msgf("%d of your %v are charged.", o_ptr->number - power,
						OBJECT_FMT(o_ptr, FALSE, 0));
				}
			}

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
	int i;
	s32b regen_amount;
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;

	u16b x, y;

	object_type *o_ptr;
	int temp;
	object_kind *k_ptr;
	cave_type *c_ptr = area(p_ptr->px, p_ptr->py);
	const mutation_type *mut_ptr;

	int depth = base_level();

	/* Announce the level feeling */
	if ((turn - old_turn == 1000) && (p_ptr->depth)) do_cmd_feeling();

	/* Every 10 game turns */
	if (turn % 10) return;

	/*** Attempt timed autosave ***/
	if (autosave_t && autosave_freq)
	{
		if (!(turn % ((s32b)autosave_freq * 10)))
			do_cmd_save_game(TRUE);
	}

	if (p_ptr->state.mon_fight)
	{
		msgf("You hear noise.");
	}

	/*** Handle the wilderness/town (sunshine) ***/

	/* While in town/wilderness */
	if (!p_ptr->depth)
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
				msgf("The sun has risen.");
			}
			else
			{
				/* Message */
				msgf("The sun has fallen.");
			}

			/* Light up or darken the area */
			for (y = p_ptr->min_hgt; y < p_ptr->max_hgt; y++)
			{
				for (x = p_ptr->min_wid; x < p_ptr->max_wid; x++)
				{
					light_dark_square(x, y, dawn);
				}
			}

			/* Update the monsters */
			p_ptr->update |= (PU_MONSTERS | PU_VIEW);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation. */
	if (one_in_(MAX_M_ALLOC_CHANCE))
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE, 0);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->tim.poisoned && !p_ptr->tim.invuln)
	{
		/* Take damage */
		take_hit(1, "poison");
	}


	/* (Vampires) Take damage from sunlight */
	if (FLAG(p_ptr, TR_HURT_LITE))
	{
		if (!p_ptr->depth && !(FLAG(p_ptr, TR_RES_LITE)) &&
			!(FLAG(p_ptr, TR_IM_LITE)) &&
			!p_ptr->tim.invuln &&
			(!((turn / ((10L * TOWN_DAWN) / 2)) % 2)))
		{
			if (c_ptr->info & CAVE_GLOW)
			{
				/* Take damage */
				msgf("The sun's rays scorch your flesh!");
				take_hit(1, "sunlight");
				cave_no_regen = TRUE;
			}
		}

		o_ptr = &p_ptr->equipment[EQUIP_LITE];

		if (o_ptr->tval &&
			(o_ptr->sval >= SV_LITE_GALADRIEL) &&
			(o_ptr->sval < SV_LITE_THRAIN) && 
			!(FLAG(p_ptr, TR_RES_LITE)) &&
			!(FLAG(p_ptr, TR_IM_LITE)))
		{
			char ouch[280];

			msgf("The %v scorches your undead flesh!", OBJECT_FMT(o_ptr, FALSE, 0));

			cave_no_regen = TRUE;

			strnfmt(ouch, 280, "wielding %v", OBJECT_FMT(o_ptr, TRUE, 0));
			
			if (!p_ptr->tim.invuln) take_hit(1, ouch);
		}
	}

	if ((c_ptr->feat == FEAT_SHAL_LAVA) && !(FLAG(p_ptr, TR_FEATHER)))
	{
		int damage = resist(depth / 2 + 1, res_fire_lvl);

		if (damage)
		{
			/* Take damage */
			msgf("The lava burns you!");
			take_hit(damage, "shallow lava");
			cave_no_regen = TRUE;
		}
	}

	else if (c_ptr->feat == FEAT_DEEP_LAVA)
	{
		int damage = resist(depth, res_fire_lvl);
		cptr message;
		cptr hit_from;

		if (FLAG(p_ptr, TR_FEATHER))
		{
			damage = damage / 5;

			message = "The heat burns you!";
			hit_from = "flying over deep lava";
		}
		else
		{
			message = "The lava burns you!";
			hit_from = "deep lava";
		}

		if (damage)
		{
			/* Take damage */
			msgf(message);
			take_hit(damage, hit_from);

			cave_no_regen = TRUE;
		}
	}

	if ((c_ptr->feat == FEAT_SHAL_ACID) && !(FLAG(p_ptr, TR_FEATHER)))
	{
		int damage = resist(depth / 2 + 1, res_acid_lvl);

		if (damage)
		{
			/* Take damage */
			msgf("The acid burns you!");
			take_hit(damage, "shallow acid");
			cave_no_regen = TRUE;
		}
	}

	else if (c_ptr->feat == FEAT_DEEP_ACID)
	{
		int damage = resist(depth, res_acid_lvl);
		cptr message;
		cptr hit_from;

		if (FLAG(p_ptr, TR_FEATHER))
		{
			damage = damage / 5;

			message = "The fumes burn you!";
			hit_from = "flying over deep acid";
		}
		else
		{
			message = "The acid burns you!";
			hit_from = "deep acid";
		}

		if (damage)
		{
			/* Take damage */
			msgf(message);
			take_hit(damage, hit_from);

			cave_no_regen = TRUE;
		}
	}

	if ((c_ptr->feat == FEAT_SHAL_SWAMP) &&	!(FLAG(p_ptr, TR_FEATHER)) &&
		!FLAG(p_ptr, TR_WILD_WALK))
	{
		int damage = resist(depth / 4 + 1, res_pois_lvl);

		/* Hack - some resistance will save you */
		if (damage && damage >= p_ptr->depth / 4)
		{
			/* Take damage */
			msgf("The plants poison you!");
			take_hit(damage, "swamp");
			cave_no_regen = TRUE;
		}
	}

	else if ((c_ptr->feat == FEAT_DEEP_SWAMP) && !p_ptr->tim.invuln &&
		!FLAG(p_ptr, TR_WILD_WALK))
	{
		int damage = resist(depth / 2, res_pois_lvl);
		cptr message;
		cptr hit_from;

		if (FLAG(p_ptr, TR_FEATHER))
		{
			damage = damage / 5;

			message = "The fumes poison you!";
			hit_from = "flying over thick swamp";
		}
		else
		{
			message = "The fumes poison you!";
			hit_from = "thick swamp";
		}

		if (damage)
		{
			/* Take damage */
			msgf(message);
			take_hit(damage, hit_from);

			cave_no_regen = TRUE;
		}
	}

	else if (((c_ptr->feat == FEAT_DEEP_WATER) ||
			  (c_ptr->feat == FEAT_OCEAN_WATER)) &&
			  !(FLAG(p_ptr, TR_FEATHER)))
	{
		if (p_ptr->total_weight >
			((adj_str_wgt[p_ptr->stat[A_STR].ind] * 100) / 2))
		{
			/* Take damage */
			msgf("You are drowning!");
			take_hit(randint1(depth + 1), "drowning");
			cave_no_regen = TRUE;
		}
	}

	/* Spectres -- take damage when moving through walls */
	/*
	 * Added: ANYBODY takes damage if inside through walls
	 * without wraith form -- NOTE: Spectres will never be
	 * reduced below 0 hp by being inside a stone wall; others
	 * WILL BE!
	 */
	if (cave_wall_grid(c_ptr))
	{
		if (!p_ptr->tim.invuln && !p_ptr->tim.wraith_form &&
			((p_ptr->chp > (depth / 10)) || 
			 !(FLAG(p_ptr, TR_PASS_WALL))))
		{
			cptr dam_desc;

			cave_no_regen = TRUE;

			if (FLAG(p_ptr, TR_PASS_WALL))
			{
				msgf("Your molecules feel disrupted!");
				dam_desc = "density";
			}
			else
			{
				msgf("You are being crushed!");
				dam_desc = "solid rock";
			}

			take_hit(1 + (depth / 10), dam_desc);
		}
	}

	/* 
	 * Fields you are standing on may do something.
	 */
	field_script(c_ptr, FIELD_ACT_PLAYER_ON, "");

	/* Nightmare mode activates the TY_CURSE at midnight */
	if (ironman_nightmare)
	{
		s32b len = 10L * TOWN_DAWN;
		s32b tick = turn % len + len / 4;

		int hour = (24 * tick / len) % 24;
		int min = (1440 * tick / len) % 60;
		int prev_min = (1440 * (tick - 10) / len) % 60;

		/* Require exact minute */
		if (min != prev_min)
		{
			/* Every 15 minutes after 11:00 pm */
			if ((hour == 23) && !(min % 15))
			{
				/* Disturbing */
				disturb(FALSE);

				switch (min / 15)
				{
					case 0:
					{
						msgf("You hear a distant bell toll ominously.");
						break;
					}
					case 1:
					{
						msgf("A distant bell sounds twice.");
						break;
					}
					case 2:
					{
						msgf("A distant bell sounds three times.");
						break;
					}
					case 3:
					{
						msgf("A distant bell tolls four times.");
						break;
					}
				}
			}

			/* TY_CURSE activates at mignight! */
			if (!hour && !min)
			{
				int count = 0;

				disturb(TRUE);
				msgf
					("A distant bell tolls many times, fading into an deathly silence.");
				(void)activate_ty_curse(FALSE, &count);
			}
		}
	}

	/* Take damage from cuts */
	if (p_ptr->tim.cut && !p_ptr->tim.invuln)
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->tim.cut > 200)
		{
			i = 3;
		}

		/* Severe cut */
		else if (p_ptr->tim.cut > 100)
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

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			if (p_ptr->pspeed > 199) i = 49;
			else if (p_ptr->pspeed < 0) i = 1;
			else
				i = extract_energy[p_ptr->pspeed];

			i *= 2;

			/* Regeneration takes more food */
			if (FLAG(p_ptr, TR_REGEN)) i += 30;

			/* Some specific mutations increase food requirement */
			if (p_ptr->muta3 & (MUT3_RESILIENT)) i += 20;
			if (p_ptr->muta1 & (MUT1_EAT_ROCK)) i += 20;

			/* Slow digestion takes less food */
			if (FLAG(p_ptr, TR_SLOW_DIGEST)) i -= 10;

			/* Slow healing gives some benefit... */
			if (FLAG(p_ptr, TR_SLOW_HEAL)) i -= 5;

			/* Wasting disease - almost no digestion */
			if (p_ptr->muta2 & MUT2_WASTING) i -= 50;

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
		if (!p_ptr->tim.invuln) take_hit(i, "starvation");
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
			if (!p_ptr->tim.paralyzed && (randint0(100) < 10))
			{
				/* Message */
				msgf("You faint from the lack of food.");
				disturb(TRUE);

				/* Hack -- faint (bypass free action) */
				(void)inc_paralyzed(randint1(5));
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
		if (FLAG(p_ptr, TR_REGEN))
		{
			regen_amount = regen_amount * 2;
		}
	
		if (FLAG(p_ptr, TR_SLOW_HEAL))
		{
			regen_amount = regen_amount / 4;
		}

	}


	/* Searching or Resting */
	if (p_ptr->state.searching || p_ptr->state.resting)
	{
		regen_amount = regen_amount * 2;
	}

	if (total_friends)
	{
		if (total_friends > 1 + (p_ptr->lev / cp_ptr->pet_upkeep_div))
		{
			upkeep_factor = total_friend_levels;

			/* Bounds checking */
			if (upkeep_factor > 95) upkeep_factor = 95;
			if (upkeep_factor < 5) upkeep_factor = 5;
		}
	}

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		if (upkeep_factor)
		{
			s16b upkeep_regen = (((100 - upkeep_factor) * regen_amount) / 100);
			regenmana(upkeep_regen);
		}
		else
		{
			regenmana(regen_amount);
		}
	}

	/* Poisoned or cut yields no healing */
	if (p_ptr->tim.poisoned) regen_amount = 0;
	if (p_ptr->tim.cut) regen_amount = 0;

	/* Special floor -- Pattern, in a wall -- yields no healing */
	if (cave_no_regen) regen_amount = 0;

	/* Regenerate Hit Points */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}


	/*** Timeout Various Things ***/
	if (p_ptr->tim.image) (void)inc_image(-1);
	if (p_ptr->tim.blind) (void)inc_blind(-1);
	if (p_ptr->tim.invis) (void)inc_tim_invis(-1);
	if (p_ptr->tim.esp) (void)inc_tim_esp(-1);
	if (p_ptr->tim.infra) (void)inc_tim_infra(-1);
	if (p_ptr->tim.paralyzed) (void)inc_paralyzed(-1);
	if (p_ptr->tim.confused) (void)inc_confused(-1);
	if (p_ptr->tim.afraid) (void)inc_afraid(-1);
	if (p_ptr->tim.fast) (void)inc_fast(-1);
	if (p_ptr->tim.slow) (void)inc_slow(-1);
	if (p_ptr->tim.protevil) (void)inc_protevil(-1);
	if (p_ptr->tim.invuln) (void)inc_invuln(-1);
	if (p_ptr->tim.wraith_form) (void)inc_wraith_form(-1);
	if (p_ptr->tim.hero) (void)inc_hero(-1);
	if (p_ptr->tim.shero) (void)inc_shero(-1);
	if (p_ptr->tim.blessed) (void)inc_blessed(-1);
	if (p_ptr->tim.shield) (void)inc_shield(-1);
	if (p_ptr->tim.oppose_acid) (void)inc_oppose_acid(-1);
	if (p_ptr->tim.oppose_elec) (void)inc_oppose_elec(-1);
	if (p_ptr->tim.oppose_fire) (void)inc_oppose_fire(-1);
	if (p_ptr->tim.oppose_cold) (void)inc_oppose_cold(-1);
	if (p_ptr->tim.oppose_pois) (void)inc_oppose_pois(-1);


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->tim.poisoned)
	{
		int adjust = adj_con_fix[p_ptr->stat[A_CON].ind] + 1;

		/* Apply some healing */
		(void)inc_poisoned(-adjust);
	}

	/* Stun */
	if (p_ptr->tim.stun)
	{
		int adjust = adj_con_fix[p_ptr->stat[A_CON].ind] + 1;

		/* Apply some healing */
		(void)inc_stun(-adjust);
	}

	/* Cut */
	if (p_ptr->tim.cut)
	{
		int adjust = adj_con_fix[p_ptr->stat[A_CON].ind] + 1;

		if (FLAG(p_ptr, TR_SLOW_HEAL))
			adjust /= 2;

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->tim.cut > 1000) adjust = 0;

		/* Apply some healing */
		(void)inc_cut(-adjust);
	}

	/*** Process mutation effects ***/
	for (i = MUT_PER_SET; i < MUT_PER_SET * 2; i++)
	{
		mut_ptr = &mutations[i];

		/*
		 * Do we have this mutation and
		 * is it truly a randomly activating one?
		 */
		if (player_has_mut(i) && (mut_ptr->chance > 0))
		{
			mutation_random_aux(mut_ptr);
		}
	}


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (FLAG(p_ptr, TR_DRAIN_EXP))
	{
		if ((randint0(100) < 10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Handle stat draining */
	if (FLAG(p_ptr, TR_DRAIN_STATS))
	{
		if (one_in_(250))
		{
			int stat = randint0(6);

			if (p_ptr->stat[stat].cur > 30)
			{
				p_ptr->stat[stat].cur--;
				p_ptr->update |= (PU_BONUS);
			}
		}
	}

	/* Process equipment */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		/* Get the object */
		o_ptr = &p_ptr->equipment[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Apply extra scripts */
		apply_object_trigger(TRIGGER_TIMED, o_ptr, "");

		/* TY Curse */
		if ((FLAG(o_ptr, TR_TY_CURSE)) && one_in_(TY_CURSE_CHANCE))
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);
		}

		/* Auto-curse */
		if ((FLAG(o_ptr, TR_AUTO_CURSE)) && 
				!(FLAG(o_ptr, TR_CURSED)) && one_in_(1000))
		{
			msgf("There is a malignant black aura surrounding you...");
			SET_FLAG(o_ptr, TR_CURSED);
			o_ptr->feeling = FEEL_NONE;
		}

		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 * can actually be useful!
		 */
		if ((FLAG(o_ptr, TR_TELEPORT)) && one_in_(100))
		{
			if (cursed_p(o_ptr) && !(FLAG(p_ptr, TR_NO_TELE)))
			{
				disturb(FALSE);

				/* Teleport player */
				teleport_player(40);
			}
			else
			{
				if (!disturb_other || (o_ptr->inscription &&
									   (strchr
										(quark_str(o_ptr->inscription), '.'))))
				{
					/* Do nothing */
					/* msgf("Teleport aborted.") */ ;
				}
				else if (get_check("Teleport? "))
				{
					disturb(FALSE);
					teleport_player(50);
				}
			}
		}

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Lights are special */
			if (o_ptr->tval == TV_LITE)
			{
				/* Artifact lights decrease timeout */
				if (FLAG(o_ptr, TR_INSTA_ART))
				{
					/* Recharge */
					o_ptr->timeout--;

					if (!o_ptr->timeout)
					{
						recharged_notice(o_ptr);
						
						/* Notice changes */
						notice_equip();
					}
				}
				else if (!(FLAG(o_ptr, TR_LITE)))
				{
					/* Normal lights that are not everburning */
					o_ptr->timeout--;

					/* Notice interesting fuel steps */
					notice_lite_change(o_ptr);
				}
			}

			/* Notice changes */
			else
			{
				/* Recharge */
				o_ptr->timeout--;

				if (!o_ptr->timeout)
				{
					recharged_notice(o_ptr);
					
					/* Notice changes */
					notice_equip();
				}
			}
		}
	}

	/*
	 * Recharge rods.  Rods now use timeout to control charging status,
	 * and each charging rod in a stack decreases the stack's timeout by
	 * one per turn. -LM-
	 */
	OBJ_ITT_START (p_ptr->inventory, o_ptr)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Must have a timeout */
		if (!o_ptr->timeout) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if (o_ptr->tval == TV_ROD)
		{
			/* Determine how many rods are charging. */
			temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Notice changes, provide message if object is inscribed. */
			if (temp > (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval)
			{
				recharged_notice(o_ptr);

				/* Notice changes */
				notice_inven();
			}
		}
	}
	OBJ_ITT_END;

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

		/* Exit if not in dungeon */
		if (!(o_ptr->ix || o_ptr->iy)) continue;

		field_script(area(o_ptr->ix, o_ptr->iy),
				   FIELD_ACT_OBJECT_ON, "p", LUA_OBJECT(o_ptr));

		if (!o_ptr->timeout) continue;

		/* Recharge rods on the ground.  No messages. */
		if (o_ptr->tval == TV_ROD)
		{
			/* Charge it */
			o_ptr->timeout -= o_ptr->number;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
		}
	}

	/*
	 * Cycle ultra-quick R"bool"G to prevent periodic patterns
	 * in the illumination in a forest after dark.
	 */

	quick_rand_add();


	/*** Involuntary Movement ***/

	/* Delayed Word-of-Recall */
	if (p_ptr->tim.word_recall)
	{
		/*
		 * HACK: Autosave BEFORE resetting the recall counter (rr9)
		 * The player is yanked up/down as soon as
		 * he loads the autosaved game.
		 */
		if (autosave_l && (p_ptr->tim.word_recall == 1))
			do_cmd_save_game(TRUE);

		/* Count down towards recall */
		p_ptr->tim.word_recall--;

		p_ptr->redraw |= (PR_STATUS);
		
		/* Hack - no recalling in the middle of the wilderness */
		if ((!p_ptr->depth) && (!p_ptr->place_num)) return;

		/* Activate the recall */
		if (!p_ptr->tim.word_recall)
		{
			/* Disturbing! */
			disturb(FALSE);

			/* Leaving */
			p_ptr->state.leaving = TRUE;

			/* Determine the level */
			if (p_ptr->depth)
			{
				msgf("You feel yourself yanked upwards!");

				p_ptr->depth = 0;
			}
			else
			{
				dun_type *d_ptr = dungeon();
				
				msgf("You feel yourself yanked downwards!");

				/* Not lower than bottom of dungeon */
				p_ptr->depth = d_ptr->recall_depth;
				
				/* Go down at least to the start of the dungeon */
				if (p_ptr->depth < d_ptr->min_level)
				{
					p_ptr->depth = d_ptr->min_level;
				}
				
				/* Nightmare mode makes recall more dangerous */
				if (ironman_nightmare && one_in_(666))
				{
					if (p_ptr->depth < 50)
					{
						p_ptr->depth *= 2;
					}
					else if (p_ptr->depth < 99)
					{
						p_ptr->depth = (p_ptr->depth + 99) / 2;
					}
					else if (p_ptr->depth > 100)
					{
						p_ptr->depth = MAX_DEPTH - 1;
					}
					
					if (p_ptr->depth > d_ptr->max_level)
					{
						p_ptr->depth = d_ptr->max_level;
					}
				}
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}
}



/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
#if 0
	if (!(p_ptr->state.noscore & 0x0002))
#else
	if (!p_ptr->state.noscore)
#endif
	{
		/* Mention effects */
		msgf("Wizard mode is for debugging and experimenting.");
		msgf("The game will not be scored if you enter wizard mode.");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->state.noscore |= 0x0002;
	}

	/* Success */
	return (TRUE);
}


#ifdef ALLOW_WIZARD

/*
 * Verify use of "debug" commands
 */
static bool enter_debug_mode(void)
{
	/* Ask first time */
#if 0
	if (!(p_ptr->state.noscore & 0x0008))
#else
	if (!p_ptr->state.noscore)
#endif
	{
		/* Mention effects */
		msgf("The debug commands are for debugging and experimenting.");
		msgf("The game will not be scored if you use debug commands.");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use debug commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->state.noscore |= 0x0008;
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif /* ALLOW_WIZARD */


#ifdef ALLOW_BORG

/*
 * Verify use of "borg" commands
 */
static bool enter_borg_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->state.noscore & 0x0040))
	{
		/* Mention effects */
		msgf("The borg commands are for debugging and experimenting.");
		msgf("The game will not be scored if you use borg commands.");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use borg commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->state.noscore |= 0x0040;
	}

	/* Success */
	return (TRUE);
}

#endif /* ALLOW_BORG */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{
	/* Handle repeating the last command */
	repeat_check();

	/* Parse the command */
	switch (p_ptr->cmd.cmd)
	{
		case ESCAPE:
		case ' ':
		{
			/* Ignore */
			break;
		}

		case '\r':
		{
			/* Ignore return */
			break;
		}

		/*** Wizard Commands ***/

		case KTRL('W'):
		{
			/* Toggle Wizard Mode */
			if (p_ptr->state.wizard)
			{
				p_ptr->state.wizard = FALSE;
				msgf("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				p_ptr->state.wizard = TRUE;
				msgf("Wizard mode on.");
			}

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw "title" */
			p_ptr->redraw |= (PR_TITLE);

			break;
		}


#ifdef ALLOW_WIZARD

		case KTRL('A'):
		{
			/* Enter debug mode */
			if (enter_debug_mode())
			{
				do_cmd_debug();
			}
			break;
		}

#endif /* ALLOW_WIZARD */


#ifdef ALLOW_BORG

		case KTRL('Z'):
		{
			/* Enter borg mode */
			if (enter_borg_mode())
			{
				do_cmd_borg();
			}

			break;
		}

#endif /* ALLOW_BORG */



		/*** Inventory Commands ***/

		case 'w':
		{
			/* Wear/wield equipment */
			do_cmd_wield();
			break;
		}

		case 't':
		{
			/* Take off equipment */
			do_cmd_takeoff();
			break;
		}

		case 'd':
		{
			/* Drop an item */
			do_cmd_drop();
			break;
		}

		case 'k':
		{
			/* Destroy an item */
			do_cmd_destroy();
			break;
		}

		case 'e':
		{
			/* Equipment list */
			do_cmd_equip();
			break;
		}

		case 'i':
		{
			/* Inventory list */
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		case 'I':
		{
			/* Identify an object */
			do_cmd_observe();
			break;
		}

		case KTRL('I'):
		{
			/* Hack -- toggle windows */
			toggle_inven_equip();
			break;
		}


		/*** Standard "Movement" Commands ***/

		case '+':
		{
			/* Alter a grid */
			do_cmd_alter();
			break;
		}

		case 'T':
		{
			/* Dig a tunnel */
			do_cmd_tunnel();
			break;
		}

		case ';':
		{
			/* Move (usually pick up things) */
			do_cmd_walk(FALSE);
			break;
		}

		case '-':
		{
			/* Move (usually do not pick up) */
			do_cmd_walk(TRUE);
			break;
		}


		/*** Running, Resting, Searching, Staying */

		case '.':
		{
			/* Begin Running -- Arg is Max Distance */
			do_cmd_run();
			break;
		}

		case ',':
		{
			/* Stay still (usually pick things up) */
			do_cmd_stay(always_pickup);
			break;
		}

		case 'g':
		{
			/* Stay still (usually do not pick up) */
			do_cmd_stay(!always_pickup);
			break;
		}

		case 'R':
		{
			/* Rest -- Arg is time */
			do_cmd_rest();
			break;
		}

		case 's':
		{
			/* Search for traps/doors */
			do_cmd_search();
			break;
		}

		case 'S':
		{
			/* Toggle search mode */
			do_cmd_toggle_search();
			break;
		}


		/*** Stairs and Doors and Chests and Traps ***/

		case '<':
		{
			/* Go up staircase */
			do_cmd_go_up();
			break;
		}

		case '>':
		{
			/* Go down staircase */
			do_cmd_go_down();
			break;
		}

		case 'o':
		{
			/* Open a door or chest */
			do_cmd_open();
			break;
		}

		case 'c':
		{
			/* Close a door */
			do_cmd_close();
			break;
		}

		case 'j':
		{
			/* Jam a door with spikes */
			do_cmd_spike();
			break;
		}

		case 'D':
		{
			/* Disarm a trap or chest */
			do_cmd_disarm();
			break;
		}


		/*** Magic and Prayers ***/

		case 'G':
		{
			/* Gain new spells/prayers */
			do_cmd_study();
			break;
		}

		case 'b':
		{
			/* Browse a book */
			do_cmd_browse();
			break;
		}

		case 'm':
		{
			/* Cast a spell */
			if (FLAG(p_ptr, TR_NO_MAGIC))
			{
				cptr which_power = "magic";
				if (p_ptr->rp.pclass == CLASS_MINDCRAFTER)
					which_power = "psionic powers";
				else if (mp_ptr->spell_book == TV_LIFE_BOOK)
					which_power = "prayer";

				msgf("An anti-magic shell disrupts your %s!",
						   which_power);

				p_ptr->state.energy_use = 0;
			}
			else
			{
				if (p_ptr->rp.pclass == CLASS_MINDCRAFTER)
					do_cmd_mindcraft();
				else
					do_cmd_cast();
			}

			break;
		}

		case 'p':
		{
			/* Issue a pet command */
			do_cmd_pet();
			break;
		}

		/*** Use various objects ***/

		case '{':
		{
			/* Inscribe an object */
			do_cmd_inscribe();
			break;
		}

		case '}':
		{
			/* Uninscribe an object */
			do_cmd_uninscribe();
			break;
		}

		case 'A':
		{
			/* Activate an artifact */
			do_cmd_activate();
			break;
		}

		case 'E':
		{
			/* Eat some food */
			do_cmd_eat_food();
			break;
		}

		case 'F':
		{
			/* Fuel your lantern/torch */
			do_cmd_refill();
			break;
		}

		case 'f':
		{
			/* Fire an item */
			do_cmd_fire();
			break;
		}

		case 'v':
		{
			/* Throw an item */
			do_cmd_throw();
			break;
		}

		case 'a':
		{
			/* Aim a wand */
			do_cmd_aim_wand();
			break;
		}

		case 'z':
		{
			/* Zap a rod */
			do_cmd_zap_rod();
			break;
		}

		case 'q':
		{
			/* Quaff a potion */
			do_cmd_quaff_potion();
			break;
		}

		case 'r':
		{
			/* Read a scroll */
			do_cmd_read_scroll();
			break;
		}

		case 'u':
		{
			/* Use a staff */
			do_cmd_use_staff();
			break;
		}

		case 'U':
		{
			/* Use racial power */
			do_cmd_racial_power();
			break;
		}


		/*** Looking at Things (nearby or on map) ***/

		case 'M':
		{
			/* Full dungeon map */
			do_cmd_view_map();
			break;
		}

		case 'L':
		{
			/* Locate player on map */
			do_cmd_locate();
			break;
		}

		case 'l':
		{
			/* Look around */
			do_cmd_look();
			break;
		}

		case '*':
		{
			/* Target monster or location */
			do_cmd_target();
			break;
		}



		/*** Help and Such ***/

		case '?':
		{
			/* Help */
			do_cmd_help();
			break;
		}

		case '/':
		{
			/* Identify symbol */
			do_cmd_query_symbol();
			break;
		}

		case 'C':
		{
			/* Character description */
			do_cmd_character();
			break;
		}


		/*** System Commands ***/

		case '!':
		{
			/* Hack -- User interface */
			(void)Term_user(0);
			break;
		}

		case '"':
		{
			/* Single line from a pref file */
			do_cmd_pref();
			break;
		}

		case '@':
		{
			/* Interact with macros */
			do_cmd_macros();
			break;
		}

		case '%':
		{
			/* Interact with visuals */
			do_cmd_visuals();
			break;
		}

		case '&':
		{
			/* Interact with colors */
			do_cmd_colors();
			break;
		}

		case '=':
		{
			/* Interact with options */
			do_cmd_options(OPT_FLAG_SERVER | OPT_FLAG_PLAYER);
			do_cmd_redraw();
			break;
		}


		/*** Misc Commands ***/

		case ':':
		{
			/* Take notes */
			do_cmd_note();
			break;
		}

		case 'V':
		{
			/* Version info */
			do_cmd_version();
			break;
		}

		case KTRL('F'):
		{
			/* Repeat level feeling */
			do_cmd_feeling();
			break;
		}

		case KTRL('P'):
		{
			/* Show previous messages */
			do_cmd_messages();
			break;
		}

		case KTRL('Q'):
		{
			/* Show quest status -KMW- */
			do_cmd_checkquest();
			break;
		}

		case KTRL('R'):
		{
			/* Redraw the screen */
			do_cmd_redraw();
			break;
		}

		case KTRL('S'):
		{
			/* Hack -- Save and don't quit */
			do_cmd_save_game(FALSE);
			break;
		}

		case KTRL('T'):
		{
			/* Get the time of day */
			do_cmd_time();
			break;
		}

		case KTRL('X'):
		{
			/* Save and quit */
			do_cmd_save_and_exit();
			break;
		}

		case 'Q':
		{
			/* Quit (commit suicide) */
			do_cmd_suicide();
			break;
		}

		case '~':
		case '|':
		{
			/* Check artifacts, uniques, objects, quests etc. */
			do_cmd_knowledge();
			break;
		}

		case '(':
		{
			/* Load "screen dump" */
			do_cmd_load_screen();
			break;
		}

		case ')':
		{
			/* Save "screen dump" */
			do_cmd_save_screen();
			break;
		}

		default:
		{
			/* Hack -- Unknown command */
			if (one_in_(2))
			{
				char error_m[1024];
				sound(SOUND_ILLEGAL);
				if (!get_rnd_line("error.txt", 0, error_m))
					msgf(error_m);
			}
			else
				prtf(0, 0, "Type '?' for help.");
			break;
		}
	}
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
	/*** Check for interupts ***/

	/* Complete resting */
	if (p_ptr->state.resting < 0)
	{
		/* Basic resting */
		if (p_ptr->state.resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp >= p_ptr->msp))
			{
				disturb(FALSE);
			}
		}

		/* Complete resting */
		else if (p_ptr->state.resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
				(p_ptr->csp == p_ptr->msp) &&
				!p_ptr->tim.blind && !p_ptr->tim.confused &&
				!p_ptr->tim.poisoned && !p_ptr->tim.afraid &&
				!p_ptr->tim.stun && !p_ptr->tim.cut &&
				!p_ptr->tim.slow && !p_ptr->tim.paralyzed &&
				!p_ptr->tim.image && !p_ptr->tim.word_recall)
			{
				disturb(FALSE);
			}
		}
	}

	/*** Handle "abort" ***/

	/* Check for "player abort" */
	if (p_ptr->state.running || p_ptr->cmd.rep || p_ptr->state.resting)
	{
		/* Do not wait */
		p_ptr->cmd.inkey_scan = TRUE;

		/* Check for a key */
		if (inkey())
		{
			/* Flush input */
			flush();

			/* Disturb */
			disturb(FALSE);

			/* Hack -- Show a Message */
			msgf("Cancelled.");
		}
	}

	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	while (TRUE)
	{
		/* Notice stuff */
		notice_stuff();

		/* Update */
		handle_stuff();

		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();
		
		/* Hack -- Pack Overflow */
		if (get_list_length(p_ptr->inventory) > INVEN_PACK)
		{
			int i = 0;

			object_type *o_ptr;

			/* Scan pack */
			OBJ_ITT_START (p_ptr->inventory, o_ptr)
			{
				/* Count items */
				i++;

				/* Does item need to be dropped? */
				if (i > INVEN_PACK)
				{
					/* Disturbing */
					disturb(FALSE);

					/* Warning */
					msgf("Your pack overflows!");

					/* Drop the excess item(s) */
					inven_drop(o_ptr, o_ptr->number);
				}

			}
			OBJ_ITT_END;

			/* Notice stuff */
			notice_stuff();

			/* Update */
			handle_stuff();
		}
		
		/* Assume free turn */
		p_ptr->state.energy_use = 0;


		/* Paralyzed or Knocked Out */
		if (p_ptr->tim.paralyzed || (p_ptr->tim.stun >= 100))
		{
			/* Take a turn */
			p_ptr->state.energy_use = 100;
		}

		/* Resting */
		else if (p_ptr->state.resting)
		{
			/* Timed rest */
			if (p_ptr->state.resting > 0)
			{
				/* Reduce rest count */
				p_ptr->state.resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Take a turn */
			p_ptr->state.energy_use = 100;
		}

		/* Running */
		else if (p_ptr->state.running)
		{
			/* Take a step */
			run_step(0);
		}

		/* Repeated command */
		else if (p_ptr->cmd.rep)
		{
			/* Count this execution */
			p_ptr->cmd.rep--;

			/* Redraw the state */
			p_ptr->redraw |= (PR_STATE);

			/* Redraw stuff */
			redraw_stuff();

			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			clear_msg();

			/* Process the command */
			process_command();
		}

		/* Normal command */
		else
		{
			/* Place the cursor on the player */
			move_cursor_relative(p_ptr->px, p_ptr->py);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}


		/*** Clean up ***/

		/* Significant */
		if (p_ptr->state.energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= p_ptr->state.energy_use;
			
			/* Change stuff */
			change_stuff();

			/* Hack -- constant hallucination */
			if (p_ptr->tim.image) p_ptr->redraw |= (PR_MAP);
		}

		/* Hack -- notice death */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->state.leaving)
		{
			/* Hack - save game if asked */
			if (autosave_l) do_cmd_save_game(TRUE);
		
			break;
		}
		/* Used up energy for this turn */
		if (p_ptr->state.energy_use) break;
	}
}


/*
 * Add energy to player and monsters.
 * Those with the most energy move first.
 * (This prevents monsters like Morgoth getting double moves
 * when he is at a lower speed than the player.)
 */
static void process_energy(void)
{
	int i, speed, e;
	monster_type *m_ptr;

	/*** Apply energy to player ***/
	if (p_ptr->pspeed > 199) i = 49;
	else if (p_ptr->pspeed < 0) i = 1;
	else
		i = extract_energy[p_ptr->pspeed];

	p_ptr->energy += i;

	/* Give energy to all monsters */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		speed = m_ptr->mspeed;

		/* Monsters move quickly in Nightmare mode */
		if (ironman_nightmare)
		{
			speed = MIN(199, m_ptr->mspeed + 5);
		}

		e = extract_energy[speed];

		/* Give this monster some energy */
		m_ptr->energy += e;
	}

	/* Can the player move? */
	while (p_ptr->energy >= 100 && !p_ptr->state.leaving)
	{
		/* process monster with even more energy first */
		process_monsters(p_ptr->energy + 1);

		/* Process the player while still alive */
		if (!p_ptr->state.leaving)
		{
			process_player();
		}
	}

	/* Process the fields */
	process_fields();
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void evolve_dungeon(void)
{
	cave_type *c_ptr;

	/* Not leaving */
	p_ptr->state.leaving = FALSE;

	/* Reset the "command" vars */
	p_ptr->cmd.cmd = 0;
	p_ptr->cmd.new = 0;
	p_ptr->cmd.rep = 0;
	p_ptr->cmd.arg = 0;
	p_ptr->cmd.dir = 0;


	/* Cancel the target */
	p_ptr->target_who = 0;

	/* Cancel the health bar */
	health_track(0);


	/* Check visual effects.  Should this be here? */
	p_ptr->change |= (PC_SHIMMER | PC_REPAIR);

	/* Disturb */
	disturb(TRUE);

	/* Track maximum player level */
	if (p_ptr->max_lev < p_ptr->lev)
	{
		p_ptr->max_lev = p_ptr->lev;
	}

	/* No stairs down from final quests */
	if (is_special_level(p_ptr->depth))
	{
		p_ptr->state.create_down_stair = FALSE;
	}

	/* Paranoia -- no stairs from town or wilderness */
	if (!p_ptr->depth) p_ptr->state.create_down_stair = p_ptr->state.create_up_stair = FALSE;

	/* Option -- no connected stairs */
	if (!dungeon_stair) p_ptr->state.create_down_stair = p_ptr->state.create_up_stair = FALSE;

	/* Nightmare mode is no fun... */
	if (ironman_nightmare) p_ptr->state.create_down_stair = p_ptr->state.create_up_stair = FALSE;

	/* Option -- no up stairs */
	if (ironman_downward) p_ptr->state.create_down_stair = p_ptr->state.create_up_stair = FALSE;

	/* Make a stairway. */
	if (p_ptr->state.create_up_stair || p_ptr->state.create_down_stair)
	{
		/* Place a stairway */
		c_ptr = area(p_ptr->px, p_ptr->py);
		if (cave_valid_grid(c_ptr))
		{
			/* XXX XXX XXX */
			delete_object(p_ptr->px, p_ptr->py);

			/* Make stairs */
			if (p_ptr->state.create_down_stair)
			{
				cave_set_feat(p_ptr->px, p_ptr->py, FEAT_MORE);
			}
			else
			{
				cave_set_feat(p_ptr->px, p_ptr->py, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		p_ptr->state.create_down_stair = p_ptr->state.create_up_stair = FALSE;
	}


	/* Center the panel on the player */
	panel_center(p_ptr->px, p_ptr->py);

	/* Flush messages */
	message_flush();


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MESSAGE | PW_VISIBLE);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_EQUIPPY);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update */
	handle_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_DISTANCE | PU_MON_LITE);

	/* Update */
	handle_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Notice changes */
	notice_item();

	/* Notice stuff */
	notice_stuff();

	/* Update */
	handle_stuff();

	/* Refresh */
	Term_fresh();

	/* Hack -- notice death or departure */
	if (!p_ptr->state.playing || p_ptr->state.is_dead) return;

	/* Print quest message if appropriate */
	quest_discovery();

	/*** Process this dungeon level ***/

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > z_info->m_max) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > z_info->o_max) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);


		/* Hack -- Compact the field list occasionally */
		if (fld_cnt + 32 > z_info->fld_max) compact_fields(64);

		/* Hack -- Compress the field list occasionally */
		if (fld_cnt + 32 < fld_max) compact_fields(0);

		/*
		 * Add energy to player and monsters.
		 * Those with the most energy move first.
		 */
		process_energy();

		/* Notice */
		notice_stuff();

		/* Update  */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Process all of the monsters */
		process_monsters(100);

		/* Reset monsters */
		reset_monsters();

		/* Notice */
		notice_stuff();

		/* Update */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->state.leaving) break;

		/* Process the world */
		process_world();

		/* Hack -- Notice death or departure */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->state.leaving) break;

		/* Notice */
		notice_stuff();

		/* Update */
		handle_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->px, p_ptr->py);

		/* Optional fresh */
		if (fresh_after) Term_fresh();

		/* Hack -- Notice death or departure */
		if (!p_ptr->state.playing || p_ptr->state.is_dead) break;

		/* Handle "leaving" */
		if (p_ptr->state.leaving) break;


		/* Count game turns */
		turn++;
	}

	/* The dungeon is not ready */
	character_dungeon = FALSE;
}


/*
 * Load some "user pref files"
 *
 * Modified by Arcum Dagsson to support
 * separate macro files for different realms.
 */
static void load_all_pref_files(void)
{
	/* Process global pref file */
	(void)process_pref_file("player.prf");
	
	/* Process race pref file */
	(void)process_pref_file("%s.prf", rp_ptr->title);

	/* Process class pref file */
	(void)process_pref_file("%s.prf", cp_ptr->title);

	/* Process character file */
	(void)process_pref_file("%s.prf", player_base);

	/* Access the "realm 1" pref file */
	if (p_ptr->spell.r[0].realm != REALM_NONE)
	{
		/* Process that file */
		(void)process_pref_file("%s.prf", realm_names[p_ptr->spell.r[0].realm]);
	}

	/* Access the "realm 2" pref file */
	if (p_ptr->spell.r[1].realm != REALM_NONE)
	{
		/* Process that file */
		(void)process_pref_file("%s.prf", realm_names[p_ptr->spell.r[1].realm]);
	}
}


/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 */
void play_game(bool new_game)
{
	int i;

	/* Verify main term */
	if (!angband_term[0])
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(angband_term[0]);

	if (!angband_term[0]) quit("Main term does not exist!");
	
	/* Hack -- Character is "icky" */
	screen_save();

	/* Initialise the resize hooks */
	angband_term[0]->resize_hook = resize_map;

	for (i = 1; i < 8; i++)
	{
		/* Does the term exist? */
		if (angband_term[i])
		{
			/* Add the redraw on resize hook */
			angband_term[i]->resize_hook = redraw_window;
		}
	}

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Hack -- turn off the cursor */
	(void)Term_set_cursor(0);

	/*
	 * Initialize wilderness info
	 * This needs to be done before old savefiles are loaded.
	 */
	if (init_w_info()) quit("Cannot initialize wilderness");

	/* Initialize field info */
	if (init_t_info()) quit("Cannot initialize fields");


	/* Attempt to load */
	if (!load_player())
	{
		/* Oops */
		quit("broken savefile");
	}

	/* Nothing loaded */
	if (!character_loaded)
	{
		/* Make new player */
		new_game = TRUE;

		/* The dungeon is not ready */
		character_dungeon = FALSE;
	}

	/* Hack -- Default base_name */
	if (!player_base[0])
	{
		strcpy(player_base, "PLAYER");
	}

	/* Init the RNG */
	if (Rand_quick || new_game)
	{
		u32b seed;

		/* Basic seed */
		seed = (time(NULL));

#ifdef SET_UID

		/* Mutate the seed on Unix machines */
		seed = ((seed >> 3) * (getpid() * 2));

#endif

		/* Use the complex RNG */
		Rand_quick = FALSE;

		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Roll new character */
	if (new_game)
	{
		/* Initialize the panel bounds to prevent a crash (rr9) */
		verify_panel();

		/* Wipe everything */
		wipe_all_list();

		/* Roll up a new character */
		player_birth();

		/* Hack -- enter the world */
		if ((p_ptr->rp.prace == RACE_VAMPIRE) ||
			(p_ptr->rp.prace == RACE_SKELETON) ||
			(p_ptr->rp.prace == RACE_ZOMBIE) ||
			(p_ptr->rp.prace == RACE_SPECTRE) ||
			(p_ptr->rp.prace == RACE_GHOUL))
		{
			/* Undead start just after midnight */
			turn = (30L * TOWN_DAWN) / 4 + 1;
		}
		else
		{
			turn = 1;
		}
		
		/* Create a new wilderness for the player */
		create_wilderness();

		/* The dungeon is ready */
		character_dungeon = TRUE;

		/* Hack -- seed for flavors */
		seed_flavor = randint0(0x10000000);
	}

	/* Reset the visual mappings */
	reset_visuals();

	/* Normal machine (process player name) */
	if (savefile[0])
	{
		process_player_name(FALSE);
	}

	/* Weird machine (process player name, pick savefile name) */
	else
	{
		process_player_name(TRUE);
	}

	/* Hack - if note file exists, load it */
	if (!new_game && take_notes)
	{
		add_note_type(NOTE_ENTER_DUNGEON);
	}

	/* Flash a message */
	prtf(0, 0, "Please wait...");

	/* Flush the message */
	Term_fresh();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->state.wizard = TRUE;

	/* Flavor the objects */
	flavor_init();

	/* Load the "pref" files */
	load_all_pref_files();

	/*
	 * Set or clear "rogue_like_commands" if requested
	 * (Do it again, because of loading the pref files 
	 *  can stomp on the options.)
	 */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;

	/* Hack -- Character is no longer "icky" */
	screen_load();
	
	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);
	
	/* Start game */
	p_ptr->state.playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->state.is_dead = TRUE;

	/* Enter "xtra" mode */
	character_xtra = TRUE;
	
	/* Resize / init the map */
	p_ptr->update |= (PU_MAP);

	/* Need to recalculate some transient things */
	p_ptr->update |= (PU_BONUS | PU_SPELLS | PU_WEIGHT);

	/* Update some stuff not stored in the savefile any more */
	p_ptr->update |= (PU_VIEW | PU_MON_LITE);

	/* Update stuff */
	update_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Window stuff */
	window_stuff();

	/* Initialise inventory and equipment info for ports */
	Term_write_list(p_ptr->inventory, LIST_INVEN);
	Term_write_equipment();

	/* Process */
	while (TRUE)
	{
		/* Process the level */
		evolve_dungeon();

		/* Notice */
		notice_stuff();

		/* Update */
		handle_stuff();

		/* Cancel the target */
		p_ptr->target_who = 0;

		/* Cancel the health bar */
		health_track(0);

		/* Forget the view */
		forget_view();

		/* Handle "quit and save" */
		if (!p_ptr->state.playing && !p_ptr->state.is_dead) break;

		/* Go to the new level */
		change_level(p_ptr->depth);

		/* XXX XXX XXX */
		message_flush();

		/* Accidental Death */
		if (p_ptr->state.playing && p_ptr->state.is_dead)
		{
			/* Mega-Hack -- Allow player to cheat death */
			if ((p_ptr->state.wizard || cheat_live) && !get_check("Die? "))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->rp.sc) p_ptr->rp.sc = p_ptr->rp.age = 0;

				/* Increase age */
				p_ptr->rp.age++;

				/* Mark savefile */
				p_ptr->state.noscore |= 0x0001;

				/* Message */
				msgf("You invoke wizard mode and cheat death.");
				message_flush();

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void)clear_blind();
				(void)clear_confused();
				(void)clear_poisoned();
				(void)clear_afraid();
				(void)clear_paralyzed();
				(void)clear_image();
				(void)clear_stun();
				(void)clear_cut();

				/* Hack"-- Prevent starvation */
				(void)set_food(PY_FOOD_MAX - 1);

				/* Hack -- cancel recall */
				if (p_ptr->tim.word_recall)
				{
					/* Message */
					msgf("A tension leaves the air around you...");
					message_flush();

					/* Hack -- Prevent recall */
					p_ptr->tim.word_recall = 0;
					p_ptr->redraw |= (PR_STATUS);
				}

				/* Note cause of death XXX XXX XXX */
				(void)strcpy(p_ptr->state.died_from, "Cheating death");

				/* Do not die */
				p_ptr->state.is_dead = FALSE;

				p_ptr->depth = 0;
				change_level(p_ptr->depth);

				/* Leaving */
				p_ptr->state.leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (p_ptr->state.is_dead) break;

		/* Make a new level */
		generate_cave();
		
		/* Update panels */
		p_ptr->update |= (PU_MAP);
		
		update_stuff();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
