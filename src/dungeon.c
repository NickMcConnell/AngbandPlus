/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static cptr value_check_aux1(object_type * o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
			return "terrible";

		/* Normal */
		return "special";
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr))
			return "worthless";

		/* Normal */
		return "excellent";
	}

	/* Cursed items */
	if (cursed_p(o_ptr))
		return "cursed";

	/* Broken items */
	if (broken_p(o_ptr))
		return "broken";

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0)
		return "good";

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0)
		return "good";

	/* Default to "average" */
	return "average";
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static cptr value_check_aux2(object_type * o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr))
		return "cursed";

	/* Broken items (all of them) */
	if (broken_p(o_ptr))
		return "broken";

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr))
		return "good";

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr))
		return "good";

	/* Good armor bonus */
	if (o_ptr->to_a > 0)
		return "good";

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0)
		return "good";

	/* No feeling */
	return (NULL);
}




/*
 * Sense the inventory
 *
 *   Class 0 = Warrior --> fast and heavy
 *   Class 1 = Mage    --> slow and light
 *   Class 2 = Priest  --> fast but light
 *   Class 3 = Rogue   --> okay and heavy
 *   Class 4 = Ranger  --> slow and light
 *   Class 5 = Paladin --> slow but heavy
 *   Class 6 = Illusionist --> slow and light -KMW-
 */
static void sense_inventory(void)
{
	int i, j;

	int plev = p_ptr->lev;

	bool heavy = FALSE;

	cptr feel, slot;

	object_type *o_ptr;

	char o_name[80];


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused)
		return;

	/* Analyze the class */
	switch (p_ptr->pclass)
	{
		case CLASS_CORRUPTED:
		case CLASS_LYCANTH:
		case CLASS_WARRIOR:
		case CLASS_MIMIC:
		{
			/* Good sensing */
			if (0 != rand_int(9000L / (plev * plev + 40)))
				return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

	case CLASS_AVATAR: {

	  /* Good sensing */
	  if (0 != rand_int(15000L / (plev * plev + 40)))
	    return;

	  /* Heavy sensing */
	  heavy = TRUE;
	  break;
	}

		case CLASS_MAGE:
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(240000L / (plev + 5)))
				return;

			/* Done */
			break;
		}

		case CLASS_BEASTMASTER:
		case CLASS_PRIEST:
		case CLASS_NECRO:
		{
			/* Good (light) sensing */
			if (0 != rand_int(10000L / (plev * plev + 40)))
				return;

			/* Done */
			break;
		}

		case CLASS_VAMPIRE:
		case CLASS_ROGUE:
		case CLASS_ELEMENTAL:
		{
			/* Okay sensing */
			if (0 != rand_int(20000L / (plev * plev + 40)))
				return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_RANGER:
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(120000L / (plev + 5)))
				return;

			/* Done */
			break;
		}

		case CLASS_PALADIN:
		{
			/* Bad sensing */
			if (0 != rand_int(80000L / (plev * plev + 40)))
				return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_BARD:
		case CLASS_ILLUSIONIST: /* Added -KMW- */
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(240000L / (plev + 5)))
				return;

			/* Done */
			break;
		}
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0, o_ptr = inventory; o_ptr != NULL; i++, o_ptr = o_ptr->next)
	{

		bool okay = FALSE;

		/* Skip empty slots */
		if (!o_ptr->k_idx)
			continue;

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
		if (!okay)
			continue;

		/* We know about it already, do not tell us again */
		if (o_ptr->ident & (IDENT_SENSE))
			continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr))
			continue;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel)
			continue;

		/* Stop everything */
		if (disturb_minor)
			disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, o_ptr, FALSE, 0);

		slot = "carrying in your pack";

		/* Message (equipment) */
		for (j = 0; j < EQUIP_MAX; j++)
		{
			if (equipment[j] == o_ptr)
				slot = describe_use(j);
		}

		msg_format("You feel the %s you are %s %s %s...", o_name, slot,
			((o_ptr->number == 1) ? "is" : "are"), feel);

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Inscribe it textually */
		if (!o_ptr->note)
			o_ptr->note = quark_add(feel);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}



/*
 * Regenerate hit points
 */
static void regenhp(int percent)
{
	s32b new_chp, new_chp_frac;
	int old_chp;

	/* Save the old hitpoints */
	old_chp = p_ptr->chp;

	/* Extract the new hitpoints */
	new_chp = ((long) p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += new_chp >> 16; /* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0))
		p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac; /* mod 65536 */
	if (new_chp_frac >= 0x10000L)
	{
		p_ptr->chp_frac = new_chp_frac - 0x10000L;
		p_ptr->chp++;
	}
	else
	{
		p_ptr->chp_frac = new_chp_frac;
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
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
	}
}


/*
 * Regenerate mana points
 */
static void regenmana(int percent)
{
	s32b new_mana, new_mana_frac;
	int old_csp;

	old_csp = p_ptr->csp;
	new_mana = ((long) p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += new_mana >> 16; /* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac; /* mod 65536 */
	if (new_mana_frac >= 0x10000L)
	{
		p_ptr->csp_frac = new_mana_frac - 0x10000L;
		p_ptr->csp++;
	}
	else
	{
		p_ptr->csp_frac = new_mana_frac;
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
		p_ptr->window |= (PW_SPELL | PW_PLAYER);
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
		if (!m_ptr->r_idx)
			continue;

		/* Allow regeneration (if needed) */
		if (m_ptr->hp < m_ptr->maxhp)
		{
			/* Hack -- Base regeneration */
			frac = m_ptr->maxhp / 100;

			/* Hack -- Minimal regeneration rate */
			if (!frac)
				frac = 1;

			/* Hack -- Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE))
				frac *= 2;

			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp)
				m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i)
				p_ptr->redraw |= (PR_HEALTH);
		}
	}
}



/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int x, y, i, j;

	int regen_amount;

	object_type *o_ptr;


	/* Every 10 game turns */
	if (turn % 10)
		return;


	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{

		/* Slowly increase social class to 1. */
		if (p_ptr->sc < 1 && p_ptr->sc < 100)
		{
			p_ptr->sc++;
		}

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
				msg_print("The gates to ANGBAND are closing...");
				msg_print("Please finish up and/or save your game.");
			}

			/* Slam the gate */
			else
			{
				/* Message */
				msg_print("The gates to ANGBAND are now closed.");

				/* Stop playing */
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}


	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town or outside */
	if (!p_ptr->depth || p_ptr->inside_special == SPECIAL_WILD)
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

				/* Hack -- Scan the town */
				for (y = 0; y < DUNGEON_HGT; y++)
				{
					for (x = 0; x < DUNGEON_WID; x++)
					{
						/* Assume lit */
						cave_info[y][x] |= (CAVE_GLOW);

						/* Wiz-lite if appropriate. */
						if (wiz_lite_town)
						{
							cave_info[y][x] |= CAVE_MARK;
						}

						/* Memorize */
						note_spot(y, x);
					}
				}
			}

			/* Night falls */
			else
			{
				/* Message */
				msg_print("The sun has fallen.");

				/* Hack -- Scan the town */
				for (y = 0; y < DUNGEON_HGT; y++)
				{
					for (x = 0; x < DUNGEON_WID; x++)
					{
						/* Darken "boring" features */
						if (cave_boring_bold(y, x))
						{
							/* Forget the grid */
							cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);

							/* Hack -- Notice spot */
							note_spot(y, x);
						}
					}
				}
			}

			/* Now add light in appropriate places */
			for (y = 1; y < DUNGEON_HGT - 1; y++)
			{
				for (x = 1; x < DUNGEON_WID - 1; x++)
				{
					/* If this is a shop, light it and surrounding squares */
					int f = cave_feat[y][x];
					if ((f >= FEAT_SHOP_HEAD && f <= FEAT_SHOP_TAIL) ||
						(f >= FEAT_BLDG_HEAD && f <= FEAT_BLDG_TAIL) ||
						(f == FEAT_STORE_EXIT))
					{
						cave_info[y - 1][x - 1] |= CAVE_GLOW;
						cave_info[y - 1][x] |= CAVE_GLOW;
						cave_info[y - 1][x + 1] |= CAVE_GLOW;
						cave_info[y][x - 1] |= CAVE_GLOW;
						cave_info[y][x] |= CAVE_GLOW;
						cave_info[y][x + 1] |= CAVE_GLOW;
						cave_info[y + 1][x - 1] |= CAVE_GLOW;
						cave_info[y + 1][x] |= CAVE_GLOW;
						cave_info[y + 1][x + 1] |= CAVE_GLOW;
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
			if (cheat_xtra)
				msg_print("Updating Shops...");

			/* Maintain each shop (except home) */
			for (n = 0; n < MAX_STORES - 1; n++)
			{
				/* Maintain */
				store_maint(n);
			}

			/* Select new bounties. */
			select_bounties();

			/* Sometimes, shuffle the shop-keepers */
			if (rand_int(STORE_SHUFFLE) == 0)
			{
				/* Message */
				if (cheat_xtra)
					msg_print("Shuffling a Shopkeeper...");

				/* Shuffle a random shop (except home) */
				store_shuffle(rand_int(MAX_STORES - 1));
			}

			/* Message */
			if (cheat_xtra)
				msg_print("Done.");
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation. Changed -KMW- */
	if ((rand_int(MAX_M_ALLOC_CHANCE) == 0) &&
		(p_ptr->inside_special == 0))
	{
		/* Make a new monster */
		(void) alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100))
		regen_monsters();


	/*** Process the generators ***/
	if (m_generators > 0)
	{
		process_generators();
	}


	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->poisoned)
	{
		/* Take damage */
		take_hit(1, "poison");
	}

	/* Take damage from light, if appropriate. */
	if (p_ptr->hates_light &&
	    (cave_info[p_ptr->py][p_ptr->px] & CAVE_GLOW)) {

	  mprint(MSG_TEMP, "The scorching light burns your skin!");
	  take_hit(1, "sunburn");
	}

	/* Take damage from cuts */
	if (p_ptr->cut)
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

	/* Digest normally */
	if (p_ptr->food < PY_FOOD_MAX)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			i = extract_energy[p_ptr->pspeed] * 2;

			/* Regeneration takes more food */
			if (p_ptr->regenerate)
				i += 30;

			/* Slow digestion takes less food */
			if (p_ptr->slow_digest)
				i -= 10;

			/* Minimal digestion */
			if (i < 1)
				i = 1;

			/* Digest some food */
			(void) set_food(p_ptr->food - i);
		}
	}

	/* Digest quickly when gorged */
	else
	{
		/* Digest a lot of food */
		(void) set_food(p_ptr->food - 100);
	}

	/* Starve to death (slowly) */
	if (p_ptr->food < PY_FOOD_STARVE)
	{
		/* Calculate damage */
		i = (PY_FOOD_STARVE - p_ptr->food) / 10;

		/* Take damage */
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
				mprint(MSG_URGENT, "You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void) set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
			}
		}
	}

	/* Regeneration ability */
	if (p_ptr->regenerate)
	{
		regen_amount = regen_amount * 2;
	}

	/* Searching or Resting */
	if (p_ptr->searching || p_ptr->resting)
	{
		regen_amount = regen_amount * 2;
	}

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(regen_amount);
	}

	/* Various things interfere with healing */
	if (p_ptr->paralyzed)
		regen_amount = 0;
	if (p_ptr->poisoned)
		regen_amount = 0;
	if (p_ptr->stun)
		regen_amount = 0;
	if (p_ptr->cut)
		regen_amount = 0;


	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}


	/*** Timeout Various Things ***/

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void) set_image(p_ptr->image - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void) set_blind(p_ptr->blind - 1);
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void) set_tim_invis(p_ptr->tim_invis - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void) set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		(void) set_paralyzed(p_ptr->paralyzed - 1);
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		(void) set_confused(p_ptr->confused - 1);
	}

	/* Afraid */
	if (p_ptr->afraid)
	{
		(void) set_afraid(p_ptr->afraid - 1);
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void) set_fast(p_ptr->fast - 1);
	}

	/* Slow */
	if (p_ptr->slow)
	{
		(void) set_slow(p_ptr->slow - 1);
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		(void) set_protevil(p_ptr->protevil - 1);
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		(void) set_invuln(p_ptr->invuln - 1);
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		(void) set_hero(p_ptr->hero - 1);
	}

	/* Super Heroism */
	if (p_ptr->shero)
	{
		(void) set_shero(p_ptr->shero - 1);
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		(void) set_blessed(p_ptr->blessed - 1);
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void) set_shield(p_ptr->shield - 1);
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		(void) set_oppose_acid(p_ptr->oppose_acid - 1);
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		(void) set_oppose_elec(p_ptr->oppose_elec - 1);
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		(void) set_oppose_fire(p_ptr->oppose_fire - 1);
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		(void) set_oppose_cold(p_ptr->oppose_cold - 1);
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		(void) set_oppose_pois(p_ptr->oppose_pois - 1);
	}

	if (p_ptr->shape && p_ptr->shape_timed)
	{
		set_shape(p_ptr->shape, p_ptr->shape_timed - 1);
	}

	if (p_ptr->immov_cntr)
	{
		p_ptr->immov_cntr--;
	}

	/*** Process Religion. ***/

	if (p_ptr->pgod > 0)
	{
		if ((turn % 50) == 0)
		{
			set_grace(p_ptr->grace - deity_info[p_ptr->pgod -
					1].grace_deduction + 1);
		}

		if (p_ptr->god_favor > -100000)
		{
			int mns = 10 - deity_info[p_ptr->pgod - 1].grace_deduction;

			mns = (mns / 4) + 1;

			if (p_ptr->pclass == CLASS_PRIEST) {
			  mns *= 2;

			} else if (p_ptr->pclass == CLASS_PALADIN) {
			  mns = (mns * 3) / 2;

			} else if (p_ptr->pclass == CLASS_AVATAR) {
			  mns *= 4;
			}
			

			p_ptr->god_favor -= mns;
		}
	}

	/* That segfault was fated to happen. */
	hand_of_fate();
	
	strike_it_lucky();

	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void) set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void) set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000)
			adjust = 0;

		/* Apply some healing */
		(void) set_cut(p_ptr->cut - adjust);
	}


	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = equipment[EQUIP_LITE];

	/* Burn some fuel in the current lite */
	if (o_ptr && o_ptr->tval == TV_LITE)
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!artifact_p(o_ptr) && (o_ptr->pval > 0))
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
				if (o_ptr->pval == 0)
					o_ptr->pval++;
			}

			/* The light is now out */
			else if (o_ptr->pval == 0)
			{
				disturb(0, 0);
				msg_print("Your light has gone out!");

				/* Automatically get a new light from the 
				 * stack */
				if (o_ptr->number > 1)
				{
					object_type *waste = object_unabsorb(o_ptr, 1);
					remove_object(waste);

					/* Get some more turns of light. */
					if (o_ptr->sval == SV_LITE_TORCH)
						o_ptr->pval = FUEL_TORCH / 2;

					if (o_ptr->sval == SV_LITE_LANTERN)
						o_ptr->pval = FUEL_LAMP / 2;

					msg_print("You automatically refuel your light.");
				}
			}

			/* The light is getting dim */
			else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
			{
				if (disturb_minor)
					disturb(0, 0);
				mprint(MSG_TEMP, "Your light is growing faint.");
			}
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);


	/*** Process Inventory ***/

	/* Handle experience draining */
	if (p_ptr->exp_drain)
	{
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Feel the inventory */
	sense_inventory();


	/*** Process Objects ***/

	/* Process objects, dungeon and home. */
	o_ptr = o_list;

	for (o_ptr = o_list, j = 0; j < 2; o_ptr = store[7].stock, j++)
	{
		object_type *o_nxt;

		while (o_ptr)
		{
			o_nxt = o_ptr->next_global;

			/* Skip dead objects */
			if (!o_ptr->k_idx)
			{
				o_ptr = o_nxt;
				continue;
			}

			/* Recharge activatable objects */
			if (o_ptr->timeout > 0)
			{
				/* Recharge */
				o_ptr->timeout--;

				/* Notice changes */
				if (!o_ptr->timeout && (o_ptr->stack == STACK_INVEN))
				{
					/* Window stuff */
					p_ptr->window |= (PW_EQUIP);
				}
			}

			/* Corpses decompose. (Slowly.) */
			if (!(turn % 10000) && (o_ptr->stuff == STUFF_FLESH))
			{
				object_take_hit(o_ptr, 1, "rotted away");
			}

			o_ptr = o_nxt;
		}
	}

	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && (rand_int(100) < 1))
	{
		/* Teleport player */
		teleport_player(40);
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

			/* Ghosts don't WoR */

			if (p_ptr->prace == RACE_GHOST && !p_ptr->prace_info)
			{
				msg_print("You feel a terrible sense of loss.");
				return;
			}

			/* Determine the level */
			if (p_ptr->depth && 
			    p_ptr->inside_special != SPECIAL_WILD)
			{
				mprint(MSG_BONUS, "You feel yourself yanked upwards!");
				p_ptr->inside_special = SPECIAL_WILD;
				p_ptr->leaving = TRUE;
			}
			else
			{
				mprint(MSG_BONUS, "You feel yourself yanked downwards!");

				if (p_ptr->inside_special == SPECIAL_WILD) {
				  p_ptr->wilderness_px = p_ptr->px;
				  p_ptr->wilderness_py = p_ptr->py;
				  p_ptr->wilderness_depth = p_ptr->depth;
				}

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;

				if (p_ptr->depth < 1)
					p_ptr->depth = 1;

				p_ptr->inside_special = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
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
	if (!(p_ptr->noscore & 0x0002))
	{
		/* Mention effects */
		msg_print("Wizard mode is for debugging and experimenting.");
		msg_print("The game will not be scored if you enter wizard mode.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->noscore |= 0x0002;
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
	if (!(p_ptr->noscore & 0x0008))
	{
		/* Mention effects */
		msg_print
			("The debug commands are for debugging and experimenting.");
		msg_print
			("The game will not be scored if you use debug commands.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use debug commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->noscore |= 0x0008;
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif


#ifdef ALLOW_BORG

/*
 * Verify use of "borg" commands
 */
static bool enter_borg_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & 0x0010))
	{
		/* Mention effects */
		msg_print
			("The borg commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use borg commands.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you sure you want to use borg commands? "))
		{
			return (FALSE);
		}

		/* Mark savefile */
		p_ptr->noscore |= 0x0010;
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
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{
	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
			/* Ignore */
		case ESCAPE:
		case ' ':
		{
			break;
		}

			/* Ignore return */
		case '\r':
		{
			break;
		}



	  /*** Wizard Commands ***/

			/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (p_ptr->wizard)
			{
				p_ptr->wizard = FALSE;
				msg_print("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				p_ptr->wizard = TRUE;
				msg_print("Wizard mode on.");
			}

			/* Update monsters */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw "title" */
			p_ptr->redraw |= (PR_TITLE);

			break;
		}


#ifdef ALLOW_WIZARD

			/* Special "debug" commands */
		case KTRL('A'):
		{
			/* Enter debug mode */
			if (enter_debug_mode())
			{
				do_cmd_debug();
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

			/* Walk */
		case ';':
		{
			do_cmd_walk();
			break;
		}

			/* Jump */
		case '-':
		{
			do_cmd_jump();
			break;
		}


	  /*** Running, Resting, Searching, Staying */

			/* Begin Running -- Arg is Max Distance */
		case '.':
		{
			do_cmd_run();
			break;
		}

			/* Hold still */
		case ',':
		{
			do_cmd_hold();
			break;
		}

			/* Stay still */
		case 'g':
		{
			do_cmd_stay();
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

			/* Toggle search mode */
		case 'S':
		{
			do_cmd_toggle_search();
			break;
		}


	  /*** Stairs and Doors and Chests and Traps ***/

			/* Enter store */
		case '_':
		{
			do_cmd_store();
			break;
		}

			/* Enter building -KMW- */
		case ']':
		{
			do_cmd_bldg();
			break;
		}

			/* Enter quest level -KMW- */
		case '[':
		{
			do_cmd_quest();
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

			/* Gain new spells/prayers - changed -KMW- */
			/* XXX */
		case 'G':
		{
			do_cmd_gain_helper();
			break;
		}

			/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

			/* Cast a spell */
		case 'm':
		{
			do_cmd_cast_helper();
			break;
		}

		case 'p':
		{
			do_cmd_pray();
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
			do_cmd_activate();
			break;
		}

			/* Eat some food */
		case 'E':
		{
			do_cmd_eat_food();
			break;
		}

			/* Fuel your lantern/torch */
		case 'F':
		{
			do_cmd_refill();
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
			do_cmd_aim_wand();
			break;
		}

			/* Zap a rod */
		case 'z':
		{
			do_cmd_zap_rod();
			break;
		}

			/* Quaff a potion */
		case 'q':
		{
			do_cmd_quaff_potion();
			break;
		}

			/* Read a scroll */
		case 'r':
		{
			do_cmd_read_scroll();
			break;
		}

			/* Use a staff */
		case 'u':
		{
			do_cmd_use_staff();
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
			do_cmd_help();
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
			(void) Term_user(0);
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
			do_cmd_feeling();
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
			do_cmd_save_game();
			break;
		}

#endif

			/* Time */
		case KTRL('T'):
		{
			do_cmd_time();
			break;
		}

			/* Save and quit */
		case KTRL('X'):
		{
			do_cmd_save_quit();
			break;
		}

			/* Quit (commit suicide) */
		case 'Q':
		{
			do_cmd_suicide();
			break;
		}

			/* Check knowledge */
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

		case '$':
		{
			do_cmd_brew_stuff();
			break;
		}

		case '#':
		{
			call_pets_toggle();
			break;
		}

		case 'O':
		{
			do_cmd_sacrifice();
			break;
		}

		case '\'':
		{
			do_cmd_cli();
			break;
		}

			/* Hack -- Unknown command */
		default:
		{
			prt("Type '?' for help.", 0, 0);
			break;
		}
	}
}



/*
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
	static int old_monster_race_idx = 0;

	static u32b old_r_flags1 = 0L;
	static u32b old_r_flags2 = 0L;
	static u32b old_r_flags3 = 0L;
	static u32b old_r_flags4 = 0L;
	static u32b old_r_flags5 = 0L;
	static u32b old_r_flags6 = 0L;
	static u32b old_r_flags7 = 0L;

	static byte old_r_blows0 = 0;
	static byte old_r_blows1 = 0;
	static byte old_r_blows2 = 0;
	static byte old_r_blows3 = 0;

	static byte old_r_cast_inate = 0;
	static byte old_r_cast_spell = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		monster_race *r_ptr;

		/* Acquire monster race */
		r_ptr = &r_info[p_ptr->monster_race_idx];

		/* Check for change of any kind */
		if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
			(old_r_flags1 != r_ptr->r_flags1) ||
			(old_r_flags2 != r_ptr->r_flags2) ||
			(old_r_flags3 != r_ptr->r_flags3) ||
			(old_r_flags4 != r_ptr->r_flags4) ||
			(old_r_flags5 != r_ptr->r_flags5) ||
			(old_r_flags6 != r_ptr->r_flags6) ||
			(old_r_flags7 != r_ptr->r_flags7) ||
			(old_r_blows0 != r_ptr->r_blows[0]) ||
			(old_r_blows1 != r_ptr->r_blows[1]) ||
			(old_r_blows2 != r_ptr->r_blows[2]) ||
			(old_r_blows3 != r_ptr->r_blows[3]) ||
			(old_r_cast_inate != r_ptr->r_cast_inate) ||
			(old_r_cast_spell != r_ptr->r_cast_spell))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_r_flags1 = r_ptr->r_flags1;
			old_r_flags2 = r_ptr->r_flags2;
			old_r_flags3 = r_ptr->r_flags3;
			old_r_flags4 = r_ptr->r_flags4;
			old_r_flags5 = r_ptr->r_flags5;
			old_r_flags6 = r_ptr->r_flags6;
			old_r_flags7 = r_ptr->r_flags7;

			/* Memorize blows */
			old_r_blows0 = r_ptr->r_blows[0];
			old_r_blows1 = r_ptr->r_blows[1];
			old_r_blows2 = r_ptr->r_blows[2];
			old_r_blows3 = r_ptr->r_blows[3];

			/* Memorize castings */
			old_r_cast_inate = r_ptr->r_cast_inate;
			old_r_cast_spell = r_ptr->r_cast_spell;

			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);

			/* Window stuff */
			window_stuff();
		}
	}
}


/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 *
 * Notice the annoying code to handle "monster memory" changes,
 * which allows us to avoid having to update the window flags
 * every time we change any internal monster memory field, and
 * also reduces the number of times that the recall window must
 * be redrawn.
 */
static void process_player(void)
{
	int i;


	/*** Apply energy ***/

	/* Give the player some energy */
	p_ptr->energy += extract_energy[p_ptr->pspeed];

	/* No turn yet */
	if (p_ptr->energy < 100)
		return;


	/*** Check for interupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp == p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp == p_ptr->msp) &&
				(p_ptr->perma_blind ? 1 : !p_ptr->blind) &&
				!p_ptr->confused && !p_ptr->poisoned && !p_ptr->afraid &&
				!p_ptr->stun && !p_ptr->cut && !p_ptr->slow &&
				!p_ptr->paralyzed && !p_ptr->image && !p_ptr->word_recall
				&& p_ptr->immov_cntr == 0)
			{
				disturb(0, 0);
			}
		}
	}

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" (semi-efficiently for resting) */
		if (p_ptr->running || p_ptr->command_rep || (p_ptr->resting &&
				!(p_ptr->resting & 0x0F)))
		{
			/* Do not wait */
			inkey_scan = TRUE;

			/* Check for a key */
			if (inkey())
			{
				/* Flush input */
				flush();

				/* Disturb */
				disturb(0, 0);

				/* Hack -- Show a Message */
				mprint(MSG_TEMP, "Cancelled.");
			}
		}
	}


	/*** Handle actual user input ***/

	/* Repeat until out of energy */
	while (p_ptr->energy >= 100)
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice)
			notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update)
			update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw)
			redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window)
			window_stuff();


		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Refresh (optional) */
		if (fresh_before)
			Term_fresh();


		/* Hack -- cancel "lurking browse mode" */
		if (!p_ptr->command_new)
			p_ptr->command_see = FALSE;


		/* Assume free turn */
		p_ptr->energy_use = 0;


		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Take a turn */
			p_ptr->energy_use = 100;
		}

		/* Resting */
		else if (p_ptr->resting)
		{
			/* Timed rest */
			if (p_ptr->resting > 0)
			{
				/* Reduce rest count */
				p_ptr->resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Take a turn */
			p_ptr->energy_use = 100;
		}

		/* Running */
		else if (p_ptr->running)
		{
			/* Take a step */
			run_step(0);
		}

		/* Repeated command */
		else if (p_ptr->command_rep)
		{
			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();

			/* Count this execution */
			if (p_ptr->command_rep)
			{
				/* Count this execution */
				p_ptr->command_rep--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);

				/* Redraw stuff */
				/* redraw_stuff(); */
			}
		}

		/* Normal command */
		else
		{
			/* Check monster recall */
			process_player_aux();

			/* Place the cursor on the player */
			move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}


		/*** Clean up ***/

		/* Significant */
		if (p_ptr->energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= p_ptr->energy_use;


			/* Hack -- constant hallucination */
			if (p_ptr->image)
				p_ptr->redraw |= (PR_MAP);


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
					if (!m_ptr->r_idx)
						continue;

					/* Access the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI)))
						continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}

			/* Repair "nice" flags */
			if (repair_mflag_nice)
			{
				/* Clear flag */
				repair_mflag_nice = FALSE;

				/* Process monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					/* if (!m_ptr->r_idx) continue; */

					/* Clear "nice" flag */
					m_ptr->mflag &= ~(MFLAG_NICE);
				}
			}

			/* Repair "mark" flags */
			if (repair_mflag_mark)
			{
				/* Reset the flag */
				repair_mflag_mark = FALSE;

				/* Process the monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr;

					/* Access monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					/* if (!m_ptr->r_idx) continue; */

					/* Repair "mark" flag */
					if (m_ptr->mflag & (MFLAG_MARK))
					{
						/* Skip "show" monsters */
						if (m_ptr->mflag & (MFLAG_SHOW))
						{
							/* Repair "mark" flag */
							repair_mflag_mark = TRUE;

							/* Skip */
							continue;
						}

						/* Forget flag */
						m_ptr->mflag &= ~(MFLAG_MARK);

						/* Update the monster */
						update_mon(i, FALSE);
					}
				}
			}
		}

		/* Repair "show" flags */
		if (repair_mflag_show)
		{
			/* Reset the flag */
			repair_mflag_show = FALSE;

			/* Process the monsters */
			for (i = 1; i < m_max; i++)
			{
				monster_type *m_ptr;

				/* Access monster */
				m_ptr = &m_list[i];

				/* Skip dead monsters */
				/* if (!m_ptr->r_idx) continue; */

				/* Clear "show" flag */
				m_ptr->mflag &= ~(MFLAG_SHOW);
			}
		}


		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;
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
	int py = p_ptr->py;
	int px = p_ptr->px;


	/* Hack -- enforce illegal panel */
	p_ptr->wy = DUNGEON_HGT;
	p_ptr->wx = DUNGEON_WID;


	/* Not leaving */
	p_ptr->leaving = FALSE;


	/* Reset the "command" vars */
	p_ptr->command_cmd = 0;
	p_ptr->command_new = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_arg = 0;
	p_ptr->command_dir = 0;


	/* Cancel the target */
	p_ptr->target_who = 0;

	/* Cancel the health bar */
	health_track(0);


	/* Reset shimmer flags */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;

	/* Reset repair flags */
	repair_mflag_born = TRUE;
	repair_mflag_nice = TRUE;
	repair_mflag_show = TRUE;
	repair_mflag_mark = TRUE;


	/* Disturb */
	disturb(1, 0);


	/* Track maximum player level */
	if (p_ptr->max_lev < p_ptr->lev)
	{
		p_ptr->max_lev = p_ptr->lev;
	}


	/* Track maximum dungeon level (if not in quest -KMW-) */
	if ((p_ptr->max_depth < p_ptr->depth) &&
	    (p_ptr->inside_special != SPECIAL_QUEST &&
	     p_ptr->inside_special != SPECIAL_WILD))
	{
		p_ptr->max_depth = p_ptr->depth;
	}


	/* No stairs down from Quest */
	if (p_ptr->inside_special == SPECIAL_QUEST)
	{
		p_ptr->create_down_stair = FALSE;
	}

	/* No stairs from town or if not allowed */
	if (!p_ptr->depth || !dungeon_stair)
	{
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}

	/* Make a staircase */
	if (p_ptr->create_down_stair || p_ptr->create_up_stair)
	{
		/* Place a staircase */
		if (cave_valid_bold(py, px))
		{

			/* Make stairs */
			if (p_ptr->create_down_stair)
			{
			  if (p_ptr->inside_special == SPECIAL_WILD) {
			    cave_set_feat(py, px, FEAT_SHAFT);
			  } else {
			    cave_set_feat(py, px, FEAT_MORE);
			  }
			}
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}


	/* Choose panel */
	verify_panel();


	/* Flush messages */
	msg_print(NULL);


	/* Enter "xtra" mode */
	character_xtra = TRUE;

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_DISTANCE);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Leave "xtra" mode */
	character_xtra = FALSE;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Window stuff */
	window_stuff();

	/* Refresh */
	Term_fresh();


	/* Handle delayed death */
	if (p_ptr->is_dead)
		return;


	/* Announce (or repeat) the feeling */
	if (p_ptr->depth)
		do_cmd_feeling();


	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > MAX_M_IDX)
			compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max)
			compact_monsters(0);

		/* Process the player */
		process_player();

		/* Notice stuff */
		if (p_ptr->notice)
			notice_stuff();

		/* Update stuff */
		if (p_ptr->update)
			update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw)
			redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window)
			window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after)
			Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;


		/* Process all of the monsters */
		process_monsters();

		/* Notice stuff */
		if (p_ptr->notice)
			notice_stuff();

		/* Update stuff */
		if (p_ptr->update)
			update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw)
			redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window)
			window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after)
			Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;


		/* Process the world */
		process_world();

		/* Notice stuff */
		if (p_ptr->notice)
			notice_stuff();

		/* Update stuff */
		if (p_ptr->update)
			update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw)
			redraw_stuff();

		/* Window stuff */
		if (p_ptr->window)
			window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after)
			Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving)
			break;


		/* Count game turns */
		turn++;
	}
}



/*
 * Process some user pref files
 */
static void process_some_user_pref_files(void)
{
	char buf[128];

	/* Process the "user.prf" file */
	(void) process_pref_file("user.prf");

	/* Access the "race" pref file */
	sprintf(buf, "%s.prf", rp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "class" pref file */
	sprintf(buf, "%s.prf", cp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Process the "PLAYER" file */
	sprintf(buf, "%s.prf", op_ptr->base_name);

	/* Process the "PLAYER" file */
	(void) process_pref_file(buf);
}


/*
 * Can a player be resurrected?
 */

static bool granted_resurrection(void)
{
	if (p_ptr->pgod && p_ptr->grace > 80000)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 *
 * Note that we load the RNG state from savefiles (2.8.0 or later)
 * and so we only initialize it if we were unable to load it, and
 * we mark successful loading using the "Rand_quick" flag.  This
 * is a hack but it optimizes loading of savefiles.  XXX XXX
 */
void play_game(bool new_game)
{
	/* Hack -- Increase "icky" depth */
	character_icky++;


	/* Verify main term */
	if (!angband_term[0])
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(angband_term[0]);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Calculate the height of the dungeon display */
	SCREEN_HGT = screen_y - 2;

	/* Hack -- turn off the cursor */
	(void) Term_set_cursor(0);


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

	/* Process old character */
	if (!new_game)
	{
		/* Process the player name */
		process_player_name(FALSE);
	}

	/* Init RNG */
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

	/* Roll new character */
	if (new_game)
	{
		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Start in town -KMW- */
		p_ptr->depth = 0;

		p_ptr->inside_special = 0;

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

		/* Seed for the wilderness */
		seed_wild = rand_int(0x10000000);

		/* Dungeon seeded in the following function. */

		/* Roll up a new character */
		player_birth();

		/* Hack -- enter the world */
		turn = 1;

		/* Read the default options */
		process_pref_file("birth.prf");
	}


	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode())
		p_ptr->wizard = TRUE;


	/* Flavor the objects */
	flavor_init();

	/* Reset visuals */
	reset_visuals(TRUE);


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Window stuff */
	window_stuff();


	/* Process some user pref files */
	process_some_user_pref_files();


	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original)
		rogue_like_commands = FALSE;
	if (arg_force_roguelike)
		rogue_like_commands = TRUE;


	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Generate a dungeon level if needed */
	if (!character_dungeon)
		generate_cave();

	if (p_ptr->depth && new_game)
		stair_creation();


	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Decrease "icky" depth */
	character_icky--;


	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0)
		p_ptr->is_dead = TRUE;

	/* Process */
	while (TRUE)
	{
		/* Process the level */
		dungeon();

		/* Notice stuff */
		if (p_ptr->notice)
			notice_stuff();

		/* Update stuff */
		if (p_ptr->update)
			update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw)
			redraw_stuff();

		/* Window stuff */
		if (p_ptr->window)
			window_stuff();

		/* Cancel the target */
		p_ptr->target_who = 0;

		/* Cancel the health bar */
		health_track(0);


		/* Forget the lite */
		forget_lite();

		/* Forget the view */
		forget_view();

		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead)
			break;

		/* Erase the old cave */
		wipe_o_list();
		wipe_m_list();

		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			if (granted_resurrection() &&
				p_ptr->inside_special != SPECIAL_ARENA &&
				p_ptr->inside_special != SPECIAL_MAGIC_ARENA)
			{
				mformat(MSG_BIG_BONUS,
					"The power of %s raises you back from the " "grave!",
					deity_info[p_ptr->pgod - 1].name);
				msg_print(NULL);


				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Heal sanity */
				p_ptr->csane = p_ptr->msane;
				p_ptr->csane_frac = 0;

				/* Hack -- Healing */
				(void) set_blind(0);
				(void) set_confused(0);
				(void) set_poisoned(0);
				(void) set_afraid(0);
				(void) set_paralyzed(0);
				(void) set_image(0);
				(void) set_stun(0);
				(void) set_cut(0);

				/* Hack -- Prevent starvation */
				(void) set_food(PY_FOOD_MAX - 1);

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
				strcpy(p_ptr->died_from, "Cheating death");

				/* New depth -KMW- */
				p_ptr->depth = 0;

				p_ptr->inside_special = 0;

				p_ptr->max_exp = 1;
				p_ptr->exp = 1;

				/* Lower some stats */
				(void) dec_stat(A_STR, 250, TRUE);
				(void) dec_stat(A_INT, 250, TRUE);
				(void) dec_stat(A_WIS, 250, TRUE);
				(void) dec_stat(A_DEX, 250, TRUE);
				(void) dec_stat(A_CON, 250, TRUE);
				(void) dec_stat(A_CHR, 250, TRUE);

				check_experience();

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			/* Mega-Hack -- Allow player to cheat death */
			else if ((p_ptr->wizard || cheat_live ||
					p_ptr->prace == RACE_MUNCHKIN) && !get_check("Die? "))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc)
					p_ptr->sc = p_ptr->age = 1;

				/* Mark savefile */
				p_ptr->noscore |= 0x0001;

				/* Message */

				if (p_ptr->prace == RACE_MUNCHKIN)
				{
					msg_print("True munchkins never die.");
				}
				else
				{
					msg_print("You invoke wizard mode and cheat death.");
				}

				msg_print(NULL);

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void) set_blind(0);
				(void) set_confused(0);
				(void) set_poisoned(0);
				(void) set_afraid(0);
				(void) set_paralyzed(0);
				(void) set_image(0);
				(void) set_stun(0);
				(void) set_cut(0);

				/* Hack -- Prevent starvation */
				(void) set_food(PY_FOOD_MAX - 1);

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
				strcpy(p_ptr->died_from, "Cheating death");

				/* New depth -KMW- */

				if (p_ptr->prace == RACE_GHOST)
				{
					p_ptr->depth = 90 + randnor(0, 2);
				}
				else if (p_ptr->prace == RACE_MUNCHKIN)
				{
					p_ptr->depth = 30 + randnor(0, 4);
				}
				else
				{
					p_ptr->depth = 0;
				}

				p_ptr->inside_special = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead)
			break;

		/* Make a new level */
		generate_cave();
	}

	/* Hack -- update all info. */
	/* Notice stuff */
	if (p_ptr->notice)
		notice_stuff();

	/* Update stuff */
	if (p_ptr->update)
		update_stuff();

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
