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
 * Remove the ironman ego_items of the probability tables.
 */
static void remove_ironman_ego_items(void)
{
	s16b i;

	alloc_entry *table = alloc_ego_table;

	/* Go through "normal" ego-item types */
	for (i = 0; i < alloc_ego_size; i++)
	{
		ego_item_type *e_ptr = &e_info[table[i].index];

		/* Ignore ironman ego-item types */
		if (e_ptr->flags3 & TR3_IRONMAN_ONLY)
		{
			/*No chance to be created normally*/
			table[i].prob1 = 0;
			table[i].prob2 = 0;
			table[i].prob3 = 0;
		}

	}

}

/*
 * Remove the ironman items of the probability tables.
 */
static void remove_ironman_items(void)
{
	s16b i;

	alloc_entry *table = alloc_kind_table;

	/* Go through "normal" object types */
	for (i = 0; i < alloc_kind_size; i++)
	{
		object_kind *k_ptr = &k_info[table[i].index];

		/* Ignore ironman object types */
		if (k_ptr->k_flags3 & TR3_IRONMAN_ONLY)
		{
			/*No chance to be generated normally*/
			table[i].prob1 = 0;
			table[i].prob2 = 0;
			table[i].prob3 = 0;
		}
	}
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
int value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_GOOD_STRONG);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD_STRONG);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD_STRONG);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static int value_check_aux2(const object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr)) return (INSCRIP_GOOD_WEAK);

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return (INSCRIP_GOOD_WEAK);

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD_WEAK);

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD_WEAK);

	/* No feeling */
	return (0);
}
/*
 * Returns TRUE if this object can be pseudo-ided.
 */
bool can_be_pseudo_ided(const object_type *o_ptr)
{
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
		case TV_DRAG_SHIELD:
      	{
			return (TRUE);
			break;
      	}
    	case TV_LITE:
      	{
			if (o_ptr->sval == SV_LITE_LANTERN)
	  		return (TRUE);
			break;
      	}
  	}
  	return (FALSE);
}



/*
 * Sense the inventory
 */
static void sense_inventory(void)
{
	int i;

	int plev = p_ptr->lev;

	int feel;

	object_type *o_ptr;

	char o_name[80];


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	if (0 != rand_int((cp_ptr->sense_base * adj_per_pseudo_delay[p_ptr->stat_ind[A_PER]]) / (10*(plev * 4 + cp_ptr->sense_div))))
			return;


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{

		int squelch = SQUELCH_NO;

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Skip non-sense machines */
		if (!can_be_pseudo_ided(o_ptr)) continue;

		/* It already has a discount or special inscription */
		if ((o_ptr->discount > 0) &&
		    (o_ptr->discount != INSCRIP_INDESTRUCTIBLE)) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* 80% failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* Check for a feeling */
		feel = value_check_aux1(o_ptr);

		/* Skip non-feelings */
		if (!feel) continue;

		/* Squelch it? */
		if (i < INVEN_WIELD)
		{
			squelch = squelch_itemp(o_ptr, feel, FALSE);
		}

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Message (equipment) */

		sound(MSG_PSEUDOID);
		if (i >= INVEN_WIELD)
		{
			msg_format("You feel the %s (%c) you are %s %s %s...",
			           o_name, index_to_label(i), describe_use(i),
			           ((o_ptr->number == 1) ? "is" : "are"),
			           inscrip_text[feel - INSCRIP_NULL]);
		}

		/* Message (inventory) */
		else
		{
			msg_format("You feel the %s (%c) in your pack %s %s...  %s",
			           o_name, index_to_label(i),
			           ((o_ptr->number == 1) ? "is" : "are"),
			           inscrip_text[feel - INSCRIP_NULL], squelch_to_label(squelch));
		}

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);
		if ((o_ptr->ident & (IDENT_WIELDED)) && (o_ptr->discount==INSCRIP_AVERAGE || o_ptr->discount==INSCRIP_GOOD_STRONG)){
			object_known(o_ptr);
		}

		/* Squelch it if necessary */
		do_squelch_item(squelch, i, o_ptr);

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
	new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	p_ptr->chp += (s16b)(new_chp >> 16);   /* div 65536 */

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
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}


/*
 * Regenerate mana points
 */
void regenmana(int percent)
{
	int old_csp;

	update_stuff();

	old_csp = p_ptr->csp;
	p_ptr->csp = p_ptr->csp + MAX((percent * p_ptr->msp)/100,1);

	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}





/*
 * Give the monsters terrain damage (once per 10 game turns)
 */

static void monster_terrain_damage(void)
{
	int i;

	/* Regenerate everyone */
	for (i = 1; i < mon_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		/* Get the feature */
		feature_type *f_ptr = &f_info[cave_feat[m_ptr->fy][m_ptr->fx]];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Monsters in non-native terrain take damage, and isn't flying */
		if ((f_ptr->dam_non_native > 0) &&
			!is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr) && (!(m_ptr->mflag & (MFLAG_FLYING))))
 		{
			byte gf_type = 0;

			/* Wake it up */
			m_ptr->csleep = 0;

			/*If we saw this, count this in the lore*/
			if (m_ptr->ml)
			{
				feature_lore *f_l_ptr = &f_l_list[cave_feat[m_ptr->fy][m_ptr->fx]];

				/*Count the number of times this damage has been felt*/
				if (f_l_ptr->f_l_dam_non_native < MAX_UCHAR) f_l_ptr->f_l_dam_non_native ++;
			}

			/* Take the worst damage from the feature */
			if (_feat_ff3_match(f_ptr, FF3_ICE) &&
				!(r_ptr->r_native & (RN1_N_ICE))) gf_type = GF_ICE;
			else if (_feat_ff3_match(f_ptr, FF3_FIRE) &&
				!(r_ptr->r_native & (RN1_N_FIRE))) gf_type = GF_FIRE;
			else if (_feat_ff3_match(f_ptr, FF3_LAVA) &&
				!(r_ptr->r_native & (RN1_N_LAVA))) gf_type = GF_LAVA;
			else if (_feat_ff3_match(f_ptr, FF3_ACID) &&
				!(r_ptr->r_native & (RN1_N_ACID))) gf_type = GF_ACID;
			else if (_feat_ff3_match(f_ptr, FF3_WATER) &&
				!(r_ptr->r_native & (RN1_N_WATER))) gf_type = GF_WATER;
			/* Default, although it should never happen */
			else gf_type = GF_MANA;

			/*Take damage*/
			(void)project_m(SOURCE_OTHER, m_ptr->fy, m_ptr->fx, f_ptr->dam_non_native, gf_type, 0L);

		}

	}
}





/*
 * Regenerate the monsters (once per 100 game turns)
 */

static void regen_monsters(void)
{
	int i, frac;

	int smooth = (turn / 100) % 100;

	/* Regenerate everyone */
	for (i = 1; i < mon_max; i++)
	{
		/* Check the i'th monster */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/*
		 * Hack -- in order to avoid a monster of 200 hitpoints having twice
		 * the regeneration of one with 199, and because we shouldn't randomize
		 * things (since we don't randomize character regeneration), we use
		 * current turn to smooth things out.
		 */

		/* Regenerate mana, if needed */
		if (m_ptr->mana != r_ptr->mana)
		{
			frac = (r_ptr->mana + smooth) / 100;

			/* Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Regenerate */
			m_ptr->mana += frac;

			/* Do not over-regenerate */
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;

			/* Fully healed -> flag minimum range for recalculation */
			if (m_ptr->mana == r_ptr->mana) m_ptr->min_range = 0;

		}

		/* Allow hp regeneration, if needed. */
		if (m_ptr->hp != m_ptr->maxhp)
		{
			frac = (m_ptr->maxhp + smooth) / 100;

			/* Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;

			/* Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Fully healed -> flag minimum range for recalculation */
			if (m_ptr->hp == m_ptr->maxhp) m_ptr->min_range = 0;
		}

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
	if (!o_ptr->obj_note) return;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->obj_note), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Describe (briefly) */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

			/*Disturb the player*/
			if (disturb_minor) disturb(0, 0);

			/* Notify the player */
			if (o_ptr->number > 1)
				msg_format("Your %s are all recharged.", o_name);

			/*artifacts*/
			else if (o_ptr->art_num)
			{
				msg_format("The %s has recharged.", o_name);
			}

			/*single, non-artifact items*/
			else msg_format("Your %s has recharged.", o_name);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = strchr(s + 1, '!');
	}
}


static void play_ambient_sound(void)
{
	/* Town sound */
	if (p_ptr->depth == 0)
	{
		/* Hack - is it daytime or nighttime? */
		if (turn % (10L * TOWN_DAWN) < TOWN_DAWN / 2)
		{
			/* It's day. */
			sound(MSG_AMBIENT_DAY);
		}
		else
		{
			/* It's night. */
			sound(MSG_AMBIENT_NITE);
		}

	}

	/* Dungeon level 1-20 */
	else if (p_ptr->depth <= 20)
	{
		sound(MSG_AMBIENT_DNG1);
	}

	/* Dungeon level 21-40 */
	else if (p_ptr->depth <= 40)
	{
		sound(MSG_AMBIENT_DNG2);
	}

	/* Dungeon level 41-60 */
	else if (p_ptr->depth <= 60)
	{
		sound(MSG_AMBIENT_DNG3);
	}

	/* Dungeon level 61-80 */
	else if (p_ptr->depth <= 80)
	{
		sound(MSG_AMBIENT_DNG4);
	}

	/* Dungeon level 80- */
	else
	{
		sound(MSG_AMBIENT_DNG5);
	}
}

/*
 * This function randomly extinguish fires near the player location
 */
static void put_out_fires(void)
{
	u16b feat;
	int y1, y2;
	int x1, x2;

	/* Get the bottom-right corner of a rectangle centered on the player */
	y2 = MIN(p_ptr->cur_map_hgt - 2, p_ptr->py + MAX_SIGHT);
	x2 = MIN(p_ptr->cur_map_wid - 2, p_ptr->px + MAX_SIGHT);

	/* Traverse the rectangle */
	for (y1 = MAX(1, p_ptr->py - MAX_SIGHT); y1 <= y2; y1++)
	{
		for (x1 = MAX(1, p_ptr->px - MAX_SIGHT); x1 <= x2; x1++)
		{
			/* Get the feature */
			feat = cave_feat[y1][x1];

			/* Must be in the line of fire (to avoid abuses) */
			if (!player_can_fire_bold(y1, x1)) continue;

			/* Must be a fire */
			if (!feat_ff3_match(feat, ELEMENT_FIRE)) continue;

			/* Must be sensitive to cold  */
			if (!feat_ff2_match(feat, FF2_HURT_COLD)) continue;

			/* Get the new feature */
			feat = feat_state(feat, FS_HURT_COLD);

			/* The fire is burning oil, ignore */
			if (feat_ff3_match(feat, ELEMENT_OIL)) continue;

			/* Randomness */
			if (!one_in_(20)) continue;

			/* Extinguish the fire */
			cave_set_feat(y1, x1, feat);
		}
	}
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i, j, temp;

	int regen_amount;

	int feat;

	object_type *o_ptr;

	object_kind *k_ptr;

	bool was_ghost = FALSE;

	store_type *st_ptr = &store[STORE_HOME];

	/* We decrease noise slightly every game turn */
	total_wakeup_chance -= 400;

	/* But the character always makes some noise */
	if (total_wakeup_chance < p_ptr->base_wakeup_chance)
	    total_wakeup_chance = p_ptr->base_wakeup_chance;

	/* Every 10 game turns */
	if (turn % 10) return;

	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{
		/* Check time and load */
		if (0 != check_time())
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

	/*** Update quests ***/
	if ((p_ptr->cur_quest) && !(turn % QUEST_TURNS))
	{

		quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

		/* Check for failure */
		if ((p_ptr->cur_quest != p_ptr->depth) && (one_in_(20)))
		{
			/* Check if quest is in progress */
			if (q_ptr->started && q_ptr->active_level &&
				((q_ptr->type == QUEST_MONSTER) || (q_ptr->type == QUEST_UNIQUE)))
				quest_fail();
		}
	}

	/* Play an ambient sound at regular intervals. */
	if (!(turn % ((10L * TOWN_DAWN) / 4)))
	{
		play_ambient_sound();
	}

	/*** Handle the "town" (stores and sunshine) ***/


	/*** Process the monsters ***/

	/* Hack - see if there is already a player ghost on the level */
	if (bones_selector) was_ghost=TRUE;

	/* Check for creature generation */
	if (one_in_(MAX_M_ALLOC_CHANCE))
	{
		/* Make a new monster, but not on themed levels */
		if (feeling < LEV_THEME_HEAD) (void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack - if there is a ghost now, and there was not before,
	 * give a challenge */
	if ((bones_selector) && (!(was_ghost))) ghost_challenge();

	/* Put out fire if necessary */
	if ((element_counter.fire > 0) && !(turn % 1000)) put_out_fires();

	/* Hack -- Check for terrain damage */
	monster_terrain_damage();

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();

	/* Process effects */
	process_effects();

	/*boundry control*/
	if (recent_failed_thefts > 50) recent_failed_thefts = 50;

	/* Hack -- Dungeon Slowly Calms down from burglars
	 * but only if you are within 10 levels of your max depth
	 * and not resting in the town.
	 */
	if ((recent_failed_thefts > 0) && (p_ptr->depth) && (!(turn % 5000))
		&& ((p_ptr->depth + 10) >= (p_ptr->max_depth)))
	{
		recent_failed_thefts --;

		/*notify player that created monsters will no longer be aggravated*/
		if (recent_failed_thefts == 29) msg_print("You sense The Pits of Angband have calmed down a little.");

	}

	/* Process dynamic dungeon grids */
	process_dynamic_terrain();

	/* Show stacked monster messages */
	if (size_mon_msg > 0) flush_monster_messages();

	/*** Damage over Time ***/

	/* Get the feature */
	feat = cave_feat[p_ptr->py][p_ptr->px];

#if 0
	/* Use covered or bridged if necessary */
	if (_feat_ff2_match(f_ptr, FF2_COVERED | FF2_BRIDGED))
	{
		f_ptr = &(f_info[f_ptr->f_mimic]);
	}
#endif

	/* If paralyzed, we drown in shallow or deep */
	if ((p_ptr->paralyzed || (p_ptr->stun >= 100)) &&
		feat_ff2_match(feat, FF2_DEEP | FF2_SHALLOW))
	{
		char name[80];

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		msg_format("You are drowning in %s!", name);

		/* Apply the blow */
		take_hit(damroll(4, 6), "drowning");
	}

	/* Take damage from poison */
	if (p_ptr->poisoned)
	{
		/* Take damage */
		if(!(p_ptr->immune_pois))take_hit((p_ptr->poisoned>=10) ? p_ptr->poisoned/10 : 1, "poison");
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

	/* Regenerate the mana 
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(regen_amount);
	}
	*/

	/* Various things interfere with healing */
	if (p_ptr->paralyzed) regen_amount = 0;
	if ((p_ptr->poisoned) && (!(p_ptr->immune_pois))) regen_amount = 0;
	if (p_ptr->stun) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}

	if (p_ptr->n_woken >= 7 && disturb_wakeup){
		msg_print("You hear lots of monsters waking up!");
		disturb(1, 0);
	} else if (p_ptr->n_woken >= 3 && one_in_(2) && disturb_wakeup){
		msg_print("You hear some monsters waking up!");
		disturb(1, 0);
	} 
	p_ptr->n_woken = 0;

	/*** Timeout Various Things ***/

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		if (randint(100) <= adj_int_escape[p_ptr->stat_ind[A_PER]]/2){
			(void)set_blind(0);
		} else {
			(void)set_blind(p_ptr->blind - 1);
		}
	}

	/* Times see-invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		if (randint(100) <= adj_agi_escape[p_ptr->stat_ind[A_AGI]]){
			(void)set_paralyzed(0);
		} else {
			(void)set_paralyzed(p_ptr->paralyzed - 1);
		}
		
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		if (randint(100) <= adj_int_escape[p_ptr->stat_ind[A_INT]]){
			(void)set_confused(0);
		} else {
			(void)set_confused(p_ptr->confused - 1);
		}
	}

	if (p_ptr->temp_aggravate > 0){
		p_ptr->temp_aggravate = p_ptr->temp_aggravate - 1;
	} else if (adj_ste_noisy_chance[p_ptr->stat_ind[A_STE]] > 0){
		if (one_in_(adj_ste_noisy_chance[p_ptr->stat_ind[A_STE]])){
			msg_print("Your steps are noisy!");
			p_ptr->temp_aggravate = 2;
		}
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
	if (p_ptr->flying)
	{
		(void)set_flying(p_ptr->flying - 1, TRUE);

		if ((p_ptr->flying <= 3) && (p_ptr->flying > 0) &&
            (!p_ptr->ffall || ((f_info[cave_feat[p_ptr->py][p_ptr->px]].dam_non_native > 0) &&
             !is_player_native(p_ptr->py, p_ptr->px))))
		{
			msg_c_format(MSG_LOSING_FLYING, "You are about to stop flying.");

			message_flush();
			disturb(0, 0);

		}
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void)set_shield(p_ptr->shield - 1);
	}

	/* Shield */
	if (p_ptr->megashield)
	{
		(void)set_megashield(p_ptr->megashield - 1);
	}

	/* Shield */
	if (p_ptr->slay_elements)
	{
		(void)set_slay_elements(p_ptr->slay_elements - 1);
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

	/* Oppose Poison */
	if (p_ptr->oppose_conf)
	{
		(void)set_oppose_conf(p_ptr->oppose_conf - 1);
	}

	/*Temporary Native Flags*/

	/* Native to Lava */
	if (p_ptr->temp_native_lava)
	{
		(void)set_temp_native_lava(p_ptr->temp_native_lava - 1);

		if ((p_ptr->temp_native_lava) && (p_ptr->temp_native_lava < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_LAVA))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to lava.");

				message_flush();
				disturb(0, 0);

			}
		}

	}

	/* Native to Oil */
	if (p_ptr->temp_native_oil)
	{
		(void)set_temp_native_oil(p_ptr->temp_native_oil - 1);

		if ((p_ptr->temp_native_oil) && (p_ptr->temp_native_oil < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_OIL))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to oil.");

				message_flush();
				disturb(0, 0);

			}
		}
	}

	/* Native to Sand */
	if (p_ptr->temp_native_sand)
	{
		(void)set_temp_native_sand(p_ptr->temp_native_sand - 1);

		if ((p_ptr->temp_native_sand) && (p_ptr->temp_native_sand < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_SAND))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to sand.");

				message_flush();
				disturb(0, 0);
			}
		}
	}

	/* Native to Forest */
	if (p_ptr->temp_native_forest)
	{
		(void)set_temp_native_forest(p_ptr->temp_native_forest - 1);

		if ((p_ptr->temp_native_forest) && (p_ptr->temp_native_forest < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_FOREST))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to forest.");

				message_flush();
				disturb(0, 0);
			}
		}
	}

	/* Native to Water */
	if (p_ptr->temp_native_water)
	{
		(void)set_temp_native_water(p_ptr->temp_native_water - 1);

		if ((p_ptr->temp_native_water) && (p_ptr->temp_native_water < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_WATER))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to water.");

				message_flush();
				disturb(0, 0);
			}
		}

	}

	/* Native to Mud */
	if (p_ptr->temp_native_mud)
	{
		(void)set_temp_native_mud(p_ptr->temp_native_mud - 1);

		if ((p_ptr->temp_native_mud) && (p_ptr->temp_native_mud < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_MUD))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to mud.");

				message_flush();
				disturb(0, 0);
			}
		}
	}



	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - 2*(((p_ptr->poisoned>=10) ? (p_ptr->poisoned/10-1) : 0) + adjust));
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
		if (!artifact_p(o_ptr) && (o_ptr->timeout > 0) && o_ptr->sval!=SV_LITE_GLOW1 && o_ptr->sval!=SV_LITE_GLOW2 && o_ptr->sval!=SV_LITE_GLOW3)
		{
			/* Decrease life-span */
			o_ptr->timeout--;

			/* Hack -- notice interesting fuel steps */
			if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
			{
				/* Window stuff */
				p_ptr->window |= (PW_EQUIP);
			}

			/* Hack -- Special treatment when blind */
			if (p_ptr->blind)
			{
				/* Hack -- save some light for later */
				if (o_ptr->timeout == 0) o_ptr->timeout++;
			}

			/* The light is now out */
			else if (o_ptr->timeout == 0)
			{
				disturb(0, 0);
				msg_print("Your light has gone out!");
			}

			/* The light is getting dim */
			else if ((o_ptr->timeout < 100) && (!(o_ptr->timeout % 10)))
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
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Process equipment */
	for (j = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0 && !fuelable_lite_p(o_ptr))
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				/* Update window */
		    	j++;

				/* Message if item is recharged, if inscribed !! */
				if (!(o_ptr->timeout)) recharged_notice(o_ptr);
			}
		}

	}

	/* Notice changes */
	if (j)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/*re-charge artifacts in the home*/

	for (i = 0; i < MAX_INVENTORY_HOME	; ++i)
	{
		/* Object */
		o_ptr = &st_ptr->stock[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		if ((o_ptr->art_num) && (o_ptr->timeout))
		{
			/* Decrease timeout by that number. */
			o_ptr->timeout--;

			/* Boundary control, paranoia. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
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

	/* Feel the inventory */
	sense_inventory();



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

			/* Sound */
			sound(MSG_TPLEVEL);

			/* Determine the level */
			if (p_ptr->depth)
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = p_ptr->recall_depth;
				if (p_ptr->depth < 1) p_ptr->depth = 1;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/* Delayed teleport */
	if (p_ptr->teleport_delay)
	{
		/* Count down towards recall */
		p_ptr->teleport_delay--;

		/* Activate the recall */
		if (!p_ptr->teleport_delay)
		{
			msg_print("Space warps around you...");
			teleport_player(p_ptr->teleport_range);
		}
	}

	/* Delayed level feelings */
	if ((p_ptr->depth) && (!p_ptr->leaving) && (!do_feeling) && (!(turn % 100)))
	{

		int chance;

		/*players notice strongholds sooner*/
		if (feeling < LEV_THEME_HEAD) chance = 80;
		else chance = 40;

		/* After sufficient time, can learn about the level */
		if ((rand_int(80) < p_ptr->skill_srh) &&
			(rand_int(80) < p_ptr->skill_srh))
		{
			/* Now have a feeling */
			do_feeling = TRUE;

			/* Announce feeling */
			do_cmd_feeling();

			/* Update the level indicator */
			p_ptr->redraw |= (PR_DEPTH);

			/* Disturb */
			disturb(0, 0);
		}
	}

}

/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time - unless resurrecting a dead character */
	if (verify_special && !(p_ptr->noscore & 0x0002) && !(p_ptr->is_dead))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the very first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0002;

	/* Success */
	return (TRUE);
}



#ifdef ALLOW_DEBUG

/*
 * Verify use of "debug" mode
 */
static bool verify_debug_mode(void)
{
	/* Ask first time */
	if (verify_special && !(p_ptr->noscore & 0x0008))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, debug commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the debug commands? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0008;

	/* Okay */
	return (TRUE);
}

#endif /* ALLOW_DEBUG */



#ifdef ALLOW_BORG

/*
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
	/* Ask first time */
	if (verify_special && !(p_ptr->noscore & 0x0010))
	{
		/* Mention effects */
		msg_print("You are about to use the dangerous, unsupported, borg commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the borg commands? "))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= 0x0010;

	/* Okay */
	return (TRUE);
}

#endif /* ALLOW_BORG */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{



	/* Handle repeating the last command */
	repeat_check();



	/* Parse the command */
	switch (p_ptr->command_cmd)
	{

		/* Ignore */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
		case '\a':
		{
			break;
		}


		/*** Cheating Commands ***/

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


#ifdef ALLOW_DEBUG

		/* Special "debug" commands */
		case KTRL('A'):
		{
			if (verify_debug_mode()) do_cmd_debug();
			break;
		}

#endif


#ifdef ALLOW_BORG

		/* Special "borg" commands */
		case KTRL('Z'):
		{
			if (verify_borg_mode()) do_cmd_borg();
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

		/* Get items off the floor */
		case (']'):
		{
			do_cmd_pickup_from_pile();
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
		case KTRL('E'):
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

		/* Alter a grid */
		case 'O':
		{
			do_cmd_make_trap();
			break;
		}

		/* Alter a grid */
		case 'P':
		{
			do_cmd_steal();
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
			msg_print("You don't need to 'G'ain spells in Ironband.");
			msg_print("Just press 'm' to cast a spell from a book, or 'a' from a wand.");
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
			do_cmd_cast(FALSE);
			break;
		}

		/* Pray a prayer */
		case 'p':
		{
			do_cmd_cast(FALSE);
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

		/* Zap, also now a wand */
		case 'z':
		{
			do_cmd_aim_wand();
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

		/* Use, now also a wand */
		case 'u':
		{
			do_cmd_aim_wand();
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
			do_cmd_redraw();
			break;
		}


		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note("",  p_ptr->depth);
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

		/* Show quest */
		case KTRL('Q'):
		{
			do_cmd_quest();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

#ifdef ALLOW_DATA_DUMP

		/* Dump all the monster, ego_item, and artifact info */

		case KTRL('D'):
		{
			write_r_info_txt();
			write_o_info_txt();
			write_e_info_txt();
			write_f_info_txt();
			write_a_info_txt();
			break;
		}
#endif	/* ALLOW_DATA_DUMP	*/

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

		/* Save and quit */
		case KTRL('X'):
		{
			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Leaving */
			p_ptr->leaving = TRUE;

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
	int i;
	bool changed = FALSE;

	static int old_monster_race_idx = 0;

	static u32b	old_flags1 = 0L;
	static u32b	old_flags2 = 0L;
	static u32b	old_flags3 = 0L;
	static u32b	old_flags4 = 0L;
	static u32b	old_flags5 = 0L;
	static u32b	old_flags6 = 0L;
	static u32b	old_flags7 = 0L;

	static byte old_blows[MONSTER_BLOW_MAX];

	static byte	old_ranged = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

		for (i = 0; i < MONSTER_BLOW_MAX; i++)
		{
			if (old_blows[i] != l_ptr->blows[i])
			{
				changed = TRUE;
				break;
			}
		}

		/* Check for change of any kind */
		if (changed ||
		    (old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (old_flags1 != l_ptr->r_l_flags1) ||
		    (old_flags2 != l_ptr->r_l_flags2) ||
		    (old_flags3 != l_ptr->r_l_flags3) ||
		    (old_flags4 != l_ptr->r_l_flags4) ||
		    (old_flags5 != l_ptr->r_l_flags5) ||
		    (old_flags6 != l_ptr->r_l_flags6) ||
			(old_flags7 != l_ptr->r_l_flags7) ||
		    (old_ranged != l_ptr->ranged))

		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_flags1 = l_ptr->r_l_flags1;
			old_flags2 = l_ptr->r_l_flags2;
			old_flags3 = l_ptr->r_l_flags3;
			old_flags4 = l_ptr->r_l_flags4;
			old_flags5 = l_ptr->r_l_flags5;
			old_flags6 = l_ptr->r_l_flags6;
			old_flags7 = l_ptr->r_l_flags7;

			/* Memorize blows */
			for (i = 0; i < MONSTER_BLOW_MAX; i++)
				old_blows[i] = l_ptr->blows[i];

			/* Memorize castings */
			old_ranged = l_ptr->ranged;

			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);

			/* Window stuff */
			window_stuff();
		}
	}
}

/*
 * Process the player terrain damage
 * This function can kill the player, so all calls to this function should be able to handle this.
 */
void process_player_terrain_damage(void)
{
	/* No damage to take terrain */
	if (p_ptr->cumulative_terrain_damage > 0)
	{
		/*
		 * IMPORTANT: we divide cumulative damage by 10
		 * to get a value nearly equal to "dam_non_native" at
		 * normal speed (1 player turn every 10 game turns)
		 */
		int dam = p_ptr->cumulative_terrain_damage / 10;

		char name[80];

		cptr kb_str;

		/* Get the feature */
		int feat = cave_feat[p_ptr->py][p_ptr->px];

		/* Uncomment this if you want a damage cap */
		/*dam = MIN(dam, f_info[feat].dam_non_native);*/

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Format the killer string */
		kb_str = format("standing in %s", name);

		/* Take the hit */
		take_terrain_hit(dam, feat, kb_str);

		/* Reset terrain damage */
		p_ptr->cumulative_terrain_damage = 0;

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
 *
 * Note that the code to check for user abort during repeated commands
 * and running and resting can be disabled entirely with an option, and
 * even if not disabled, it will only check during every 128th game turn
 * while resting, for efficiency.
 */
void process_player(void)
{
	int i, item;

	/* One more player turn */
	p_ptr->p_turn++;

	/* Show stacked monster messages */
	if (size_mon_msg > 0) flush_monster_messages();

	/* Take damage from terrain */
	process_player_terrain_damage();

	/* Dead player? */
	if (p_ptr->is_dead) return;

	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{

		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) /* &&
			    (p_ptr->csp == p_ptr->msp) */ )
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    /*(p_ptr->csp == p_ptr->msp) &&*/
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall &&
				!p_ptr->teleport_delay)
			{
				disturb(0, 0);
			}
		}

		/* Rest hit points */
		else if (p_ptr->resting == -3)
		{
			if (p_ptr->chp == p_ptr->mhp)
			{
				disturb(0, 0);
			}
		}

		/* Rest spell points */
		else if (p_ptr->resting == -4)
		{
		/*	if (p_ptr->csp == p_ptr->msp)
			{ */
				disturb(0, 0);
		/*	} */
		}
	}

	/* Handle "abort" */
	if (!avoid_abort)
	{
		/* Check for "player abort" */
		if (p_ptr->running ||
		    p_ptr->command_rep ||
		    (p_ptr->resting && !(turn & 0x7F)))
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
				msg_print("Cancelled.");
			}
		}
	}

	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	do
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window) window_stuff();

		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Refresh (optional) */
		if (fresh_before) (void)Term_fresh();

		/* Hack -- Pack Overflow */
		for (item = INVEN_PACK;
			item >= INVEN_PACK - p_ptr->pack_size_reduce; item--)
		{
			char o_name[80];

			object_type *o_ptr;

			/* Get the slot to be dropped */
			o_ptr = &inventory[item];

			/* Ignore empty objects */
			if (!o_ptr->k_idx) continue;

			/* Disturbing */
			disturb(0, 0);

			/* Warning */
			msg_print("Your pack overflows!");

			/* Describe */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Message */
			msg_format("You drop %s (%c).", o_name, index_to_label(item));

			/* Drop it (carefully) near the player */
			drop_near(o_ptr, 0, p_ptr->py, p_ptr->px);

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

			/* Window stuff (if needed) */
			if (p_ptr->window) window_stuff();
		}

		/* Hack -- cancel "lurking browse mode" */
		if (!p_ptr->command_new) p_ptr->command_see = FALSE;

		/* Assume free turn */
		p_ptr->p_energy_use = 0;

		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Take a turn */
			p_ptr->p_energy_use = BASE_ENERGY_MOVE;
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
			p_ptr->p_energy_use = BASE_ENERGY_MOVE;
		}

		/* Running */
		else if (p_ptr->running)
		{
			/* Take a step */
			run_step(0);
		}

		#ifdef ALLOW_BORG

 		/* Using the borg. */
		else if (count_stop) do_cmd_borg();

		#endif /* ALLOW_BORG */

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

		/*hack - check for secret squares*/
		if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_MARKED))
		{
			/*increase chance of altered inventory for around 100 turns*/
			allow_altered_inventory += 15;

			/*most likely unnecessary boundry control*/
			if (allow_altered_inventory > 100) allow_altered_inventory = 100;

			/*unmark the square*/
			cave_info[p_ptr->py][p_ptr->px] &= ~(CAVE_MARKED);
		}

		/* Check for greater vault squares */
		if ((cave_info[p_ptr->py][p_ptr->px] & (CAVE_G_VAULT)) &&
		    (g_vault_name[0] != '\0'))
		{
			if (adult_take_notes)
			{
		    	char note[120];
		    	char *fmt = "You enter the %s";

		    	strnfmt(note, sizeof(note), fmt, g_vault_name);

		    	do_cmd_note(note, p_ptr->depth);
		  	}

		  	g_vault_name[0] = '\0';
		}

		/* Significant */
		if (p_ptr->p_energy_use)
		{
			/* Use some energy */
			p_ptr->p_energy -= p_ptr->p_energy_use;


			/* Hack -- constant hallucination */
			if (p_ptr->image)
			{
				p_ptr->redraw |= (PR_MAP);
				p_ptr->window |= (PW_MAP);
			}

			/* Hack -- Redraw depth if the temporary quest notification ends */
			if ((quest_indicator_timer > 0) && (--quest_indicator_timer == 0) &&
				!(character_icky))
			{
				p_ptr->redraw |= (PR_DEPTH);
			}

			/* Shimmer monsters if needed */
			if (!avoid_other && shimmer_monsters)
			{
				/* Clear the flag */
				shimmer_monsters = FALSE;

				/* Shimmer multi-hued monsters */
				for (i = 1; i < mon_max; i++)
				{
					monster_type *m_ptr;
					monster_race *r_ptr;

					/* Get the monster */
					m_ptr = &mon_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Get the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
				}
			}

			/* Redraw some shimmering effects */
			if (!avoid_other)
			{
				effect_type *x_ptr;

				/* Traverse effect array */
				for (x_ptr = x_list; x_ptr < x_list + z_info->x_max; x_ptr++)
				{
					/* Ignore invisible effects */
					if (x_ptr->x_flags & (EF1_HIDDEN)) continue;

					/* Only certain effects are allowed */
					if ((x_ptr->x_type == EFFECT_TRAP_SMART) ||
						(x_ptr->x_type == EFFECT_GLACIER))
					{
						/* Redraw */
						lite_spot(x_ptr->x_cur_y, x_ptr->x_cur_x);
					}
				}
			}

			/* Redraw visual indicator of temporary element brand */
			if (p_ptr->slay_elements && !avoid_other) p_ptr->redraw |= (PR_RESIST);

			/* Repair "mark" flags */
			if (repair_mflag_mark)
			{
				/* Reset the flag */
				repair_mflag_mark = FALSE;

				/* Process the monsters */
				for (i = 1; i < mon_max; i++)
				{
					monster_type *m_ptr;

					/* Get the monster */
					m_ptr = &mon_list[i];

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

						/* Hack -- Force redraw of hidden monsters */
						if ((m_ptr->mflag & (MFLAG_HIDE)) && m_ptr->ml)
						{
							/* Redraw */
							lite_spot(m_ptr->fy, m_ptr->fx);
						}
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
			for (i = 1; i < mon_max; i++)
			{
				monster_type *m_ptr;

				/* Get the monster */
				m_ptr = &mon_list[i];

				/* Skip dead monsters */
				/* if (!m_ptr->r_idx) continue; */

				/* Clear "show" flag */
				m_ptr->mflag &= ~(MFLAG_SHOW);
			}
		}
	}
	while (!p_ptr->p_energy_use && !p_ptr->leaving);

	/* Get base noise increase -- less when resting */
	if (p_ptr->resting)
		total_wakeup_chance += p_ptr->base_wakeup_chance / 2;
	else
		total_wakeup_chance += p_ptr->base_wakeup_chance;

	/* Increase noise if necessary */
	if (add_wakeup_chance)
	{
		total_wakeup_chance += add_wakeup_chance;
		add_wakeup_chance = 0;
	}

	/* Limit on noise (100% effective in disturbing monsters) */
	if (total_wakeup_chance > 10000)
	{
		total_wakeup_chance = 10000;
	}

	/* Update noise flow information */
	if ((p_ptr->py != p_ptr->update_center_y) || (p_ptr->px != p_ptr->update_center_x))
	{
		update_flows(FALSE);
	}

#ifdef MONSTER_SMELL
	/*update_smell();*/
#endif /*MONSTER_SMELL*/

	/*
	 * Reset character vulnerability if appropriate.  Will be calculated
	 * by the first member of an animal pack that has a use for it.
	 */
	if (p_ptr->vulnerability == 100)
	{
		/* Reset vulnerability only if the character is fully healed */
		if (p_ptr->chp >= p_ptr->mhp) p_ptr->vulnerability = 0;
	}
	else
	{
		p_ptr->vulnerability = 0;
	}

	/* Show stacked monster messages */
	if (size_mon_msg > 0) flush_monster_messages();

	/* Notice stuff (if needed) */
	if (p_ptr->notice) notice_stuff();

	/* Update stuff (if needed) */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff (if needed) */
	if (p_ptr->redraw) redraw_stuff();

	/* Redraw stuff (if needed) */
	if (p_ptr->window) window_stuff();

	/* Place the cursor on the player */
	move_cursor_relative(p_ptr->py, p_ptr->px);

	/* Refresh (optional) */
	if (fresh_before) (void)Term_fresh();
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	int i;

	/* Hack -- enforce illegal panel */
	Term->offset_y = p_ptr->cur_map_hgt;
	Term->offset_x = p_ptr->cur_map_wid;

	/* Not leaving */
	p_ptr->leaving = FALSE;

	/* Reset the "command" vars */
	p_ptr->command_cmd = 0;
	p_ptr->command_new = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_arg = 0;
	p_ptr->command_dir = 0;


	/* Cancel the target */
	target_set_monster(0);

	/* Cancel the health bar */
	health_track(0);

	/* Reset shimmer flags */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;

	/* Reset repair flags */
	repair_mflag_show = TRUE;
	repair_mflag_mark = TRUE;

	/* Reset terrain damage */
	p_ptr->cumulative_terrain_damage = 0;

	/* Disturb */
	disturb(1, 0);

	/* Track maximum player level */
	if (p_ptr->max_lev < p_ptr->lev)
	{
		p_ptr->max_lev = p_ptr->lev;
	}

	/* Track maximum dungeon level */
	if (p_ptr->max_depth < p_ptr->depth)
	{
		p_ptr->max_depth = p_ptr->depth;
	}

	/* Track recall dungeon level */
	if (p_ptr->recall_depth < p_ptr->depth)
	{
		p_ptr->recall_depth = p_ptr->depth;
	}

	/* No stairs down from fixed quests */
	if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
		(quest_check(p_ptr->depth) == QUEST_FIXED_U))
	{
		if ((p_ptr->create_stair == FEAT_MORE) ||
			(p_ptr->create_stair == FEAT_MORE_SHAFT))
	   		 p_ptr->create_stair = FALSE;
	}

	/* No stairs from town or if not allowed */
	if ((!p_ptr->depth) || (!dungeon_stair))
	{
		p_ptr->create_stair = FALSE;
	}

	/* Make a staircase */
	if (p_ptr->create_stair)
	{
		/* Place a staircase */
		if (cave_valid_bold(p_ptr->py, p_ptr->px))
		{
			/* XXX XXX XXX */
			delete_object(p_ptr->py, p_ptr->px);

			cave_set_feat(p_ptr->py, p_ptr->px, p_ptr->create_stair);

			/* Mark the stairs as known */
			cave_info[p_ptr->py][p_ptr->px] |= (CAVE_MARK);
		}

		/* Cancel the stair request */
		p_ptr->create_stair = FALSE;
	}

	/* Choose panel */
	verify_panel();

	/* Flush messages */
	message_flush();

	/* Hack -- Increase "xtra" depth */
	character_xtra++;

	/* Clear */
	Term_clear();

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_NATIVE);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* RE-do the flow */
	p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);

	/* Update stuff */
	update_stuff();

	/* Fully update the visuals (and monster distances) */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MONLIST);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_MAP);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();

	/* Hack -- Decrease "xtra" depth */
	character_xtra--;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_NATIVE);

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
	(void)Term_fresh();

	/* Handle delayed death */
	if (p_ptr->is_dead) return;

	/* Mark quest as started */
	if ((p_ptr->cur_quest > 0) && (p_ptr->cur_quest == p_ptr->depth))
	{
		/* Check quests */
		for (i = 0; i < z_info->q_max; i++)
		{
			/* Check for quest */
			if (q_info[i].base_level == p_ptr->depth)
			{
				q_info[i].started = TRUE;
				break;
			}
		}
	}

	/* Announce (or repeat) the feeling */
	if ((p_ptr->depth) && (do_feeling)) do_cmd_feeling();


	/* Announce a player ghost challenge. -LM- */
	if (bones_selector) ghost_challenge();

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = danger(p_ptr->depth);

	/* Reset the object generation level */
	object_level = danger(p_ptr->depth);

	/* Main loop */
	while (TRUE)
	{

		/* Hack -- Compact the monster list occasionally */
		if (mon_cnt + 32 > z_info->m_max) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (mon_cnt + 32 < mon_max) compact_monsters(0);

		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > z_info->o_max) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);

		/* Update terrain damage every game turn */
		if ((!is_player_native(p_ptr->py, p_ptr->px)) && (!p_ptr->flying))
		{
			p_ptr->cumulative_terrain_damage += f_info[cave_feat[p_ptr->py][p_ptr->px]].dam_non_native;
		}

		/*** Process player & monsters ***/
		process_entities();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after) (void)Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process the world */
		process_world();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff */
		if (p_ptr->window) window_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		if (fresh_after) (void)Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Count game turns */
		turn++;
	}
}



/*
 * Process some user pref files
 */
static void process_some_user_pref_files(void)
{
	char buf[1024];


	/* Process the "user.prf" file */
	(void)process_pref_file("user.prf");

	/* Process the "user.scb" autoinscriptions file */
	(void)process_pref_file("user.scb");

	/* Process the "classes.prf" file */
	(void)process_pref_file("classes.prf");

	/* Process the "races.prf" file */
	(void)process_pref_file("races.prf");

	/* Get the "PLAYER.prf" filename */
	(void)strnfmt(buf, sizeof(buf), "%s.prf", op_ptr->base_name);

	/* Process the "PLAYER.prf" file */
	(void)process_pref_file(buf);
}


/*
 * Actually play a game.
 *
 * This function is called from a variety of entry points, since both
 * the standard "main.c" file, as well as several platform-specific
 * "main-xxx.c" files, call this function to start a new game with a
 * new savefile, start a new game with an existing savefile, or resume
 * a saved game with an existing savefile.
 *
 * If the "new_game" parameter is true, and the savefile contains a
 * living character, then that character will be killed, so that the
 * player may start a new game with that savefile.  This is only used
 * by the "-n" option in "main.c".
 *
 * If the savefile does not exist, cannot be loaded, or contains a dead
 * (non-wizard-mode) character, then a new game will be started.
 *
 * Several platforms (Windows, Macintosh, Amiga) start brand new games
 * with "savefile" and "op_ptr->base_name" both empty, and initialize
 * them later based on the player name.  To prevent weirdness, we must
 * initialize "op_ptr->base_name" to "PLAYER" if it is empty.
 *
 * Note that we load the RNG state from savefiles (2.8.0 or later) and
 * so we only initialize it if we were unable to load it.  The loading
 * code marks successful loading of the RNG state using the "Rand_quick"
 * flag, which is a hack, but which optimizes loading of savefiles.
 */
void play_game(bool new_game)
{
	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* Verify main term */
	if (!term_screen)
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(term_screen);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(FALSE);

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
	if (!op_ptr->base_name[0])
	{
		strcpy(op_ptr->base_name, "PLAYER");
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

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

		/* Hack -- seed for random artifacts */
		seed_randart = rand_int(0x10000000);

		/* Roll up a new character */
		player_birth();

		/* Randomize the artifacts */
		if (adult_rand_artifacts)
		{
			do_randart(seed_randart, TRUE);
		}

		/* Hack -- enter the world */
		turn = 1;

		p_ptr->p_turn = 0;

		quest_indicator_timer = 0;
	}

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

	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	(void)Term_fresh();

	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;

	/* Flavor the objects */
	flavor_init();

	/* Reset visuals */
	reset_visuals(TRUE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_MESSAGE);

	/* Window stuff */
	window_stuff();

	/* Process some user pref files */
	process_some_user_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	if (new_game){
		/* Start in the dungeon */
		p_ptr->depth = 1;
		p_ptr->max_depth = 1;
	}
		
	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;

	/* Start with normal object generation mode */
	object_generation_mode = OB_GEN_MODE_NORMAL;

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

	/* Process */
	while (TRUE)
	{
		/* Play ambient sound on change of level. */
		play_ambient_sound();

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
		target_set_monster(0);

		/* Cancel the health bar */
		health_track(0);

		/* Forget the view */
		forget_view();

		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;

		/* Handle lost greater vaults */
		if (g_vault_name[0] != '\0')
		{
			if (adult_take_notes)
			{
		    	char note[120];
				char *fmt = "Left the level without entering the %s";

		   		strnfmt(note, sizeof(note), fmt, g_vault_name);
				do_cmd_note(note, p_ptr->depth);
		  	}

		  	g_vault_name[0] = '\0';
		}

		/* Erase the old cave */
		wipe_o_list();
		wipe_mon_list();
		wipe_x_list();
		count_feat_everseen();

		/* Delete any pending monster message */
		size_mon_msg = 0;

		/* Check for quest_failure */
		if (p_ptr->cur_quest)
		{
			byte quest_info = quest_check(p_ptr->cur_quest);

			quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

			if (quest_info == QUEST_VAULT)
			{
				/* Check if had already been on quest level */
				if (q_ptr->started)
				{
					if (quest_item_slot() == -1) quest_fail();
				}
			}

			else if ((quest_info == QUEST_MONSTER) || (quest_info == QUEST_UNIQUE))
			{
				/* Check that the quest is in process */
				if ((p_ptr->cur_quest != p_ptr->depth) && (q_ptr->started)
				      && (q_ptr->active_level))
				{
					/* Have a chance to fail if the quest is in progress */
					if (one_in_(10)) quest_fail();
				}
			}
			else if ((quest_info == QUEST_NEST) || (quest_info == QUEST_PIT) ||
					 (quest_info == QUEST_THEMED_LEVEL))
			{
				/*You can't leave this level*/
				if ((q_ptr->started) && (q_ptr->active_level)) quest_fail();
			}
		}

		/* XXX XXX XXX */
		message_flush();

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Make a new level */
		generate_cave();

	}

	/* Close stuff */
	close_game();
}

int danger(int actual){
	if (actual < 2) return actual;
	if (actual == 2) return actual+1;
	if (actual <= 4) return actual+2;
	if (actual <= 6) return actual+3;
	if (actual <= 8) return actual+4;
	if (actual <= 10) return actual+5;
	if (actual <= 20) return actual+6;
	if (actual <= 22) return actual+7;
	if (actual <= 24) return actual+8;
	if (actual <= 26) return actual+9;
	if (actual <= 28) return actual+10;
	if (actual <= 38) return actual+11;
	if (actual <= 40) return actual+12;
	if (actual <= 42) return actual+13;
	if (actual <= 44) return actual+14;
	if (actual <= 46) return actual+15;
	return actual+16;
}