/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "game-event.h"

/*
 * Change dungeon level.
 * Aside from setting the player depth at the beginning of the game,
 * this should be the only place where a player depth is actually
 * changed.
 */
void dungeon_change_level(int dlev)
{
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

	/* New depth */
	p_ptr->depth = dlev;

	/* Leaving */
	p_ptr->leaving = TRUE;

	/* Save the game when we arrive on the new level. */
	p_ptr->autosave = TRUE;

	p_ptr->redraw |= (PR_DEPTH | PR_QUEST_ST | PR_FEELING);

}



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

		/*
		 * Mega-hack - Allow fireproof books if store services
		 * are disabled
		 */
		if ((table[i].index == EGO_FIREPROOF) &&
			adult_no_store_services) continue;

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

			/*
			 * Hack - don't let the player cast the spells from
			 * an ironman_only book.  Note this can be a quest item, so it can be tried.
			 */
			if ((k_ptr->tval == cp_ptr->spell_book) && (k_ptr->tried == FALSE))
			{
				byte realm, j;

				/*Get the player spell realm*/
				realm =  get_player_spell_realm();

				/* Extract spells */
				for (j = 0; j < SPELLS_PER_BOOK; j++)
				{

					byte sval = k_ptr->sval;

					s16b spell = spell_list[realm][sval][j];

					/*skip blank spell slots*/
					if (spell == -1) continue;

					/* Don't count Ironman Spells. */
					p_ptr->spell_flags[spell] |= PY_SPELL_IRONMAN;

				}
			}
		}
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
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
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
		p_ptr->csp_frac = (u16b)new_mana_frac;
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
		u16b feat = cave_feat[m_ptr->fy][m_ptr->fx];
		feature_type *f_ptr = &f_info[feat];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Monsters in non-native terrain take damage, and isn't flying */
		if ((f_ptr->dam_non_native > 0) &&
			!is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr) &&
                       !MONSTER_CAN_FLY(m_ptr, feat))
 		{
			int gf_type = 0;

			u16b tmd_flag = (MON_TMD_FLG_NOTIFY);

			if (m_ptr->ml) tmd_flag |= MON_TMD_FLG_SEEN;

			/* Wake it up */
			mon_clear_timed(i, MON_TMD_SLEEP, tmd_flag);

			/*If we saw this, count this in the lore*/
			if (m_ptr->ml)
			{
				feature_lore *f_l_ptr = &f_l_list[feat];

				/*Count the number of times this damage has been felt*/
				if (f_l_ptr->f_l_dam_non_native < MAX_UCHAR) f_l_ptr->f_l_dam_non_native++;
			}

			get_spell_type_from_feature(feat, &gf_type, NULL);

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
 * "all" must be FALSE when only one object of a stack is recharged.
 */
static void recharged_notice(object_type *o_ptr, bool all)
{
	char o_name[120];

	cptr s;

	bool notify = FALSE;

	if (notify_recharge)
	{
		notify = TRUE;
	}

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
			object_desc(o_name, sizeof(o_name), o_ptr, ODESC_BASE);

			/*Disturb the player*/
			disturb(0, 0);

			/* Notify the player */
			if (o_ptr->number > 1)
			{
				if (all)
				{
					msg_format("Your %s are recharged.", o_name);
				}
				else
				{
					msg_format("One of your %s has recharged.", o_name);
				}
			}

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


/*
 * Recharge activatable objects in the player's equipment
 * and rods in the inventory and on the ground.
 */
static void recharge_objects(void)
{
	int i;
	int j = 0;

	object_type *o_ptr;
	object_kind *k_ptr;

	/*** Recharge equipment ***/
	for (i = INVEN_WIELD; i < ALL_INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0 && !fuelable_light_p(o_ptr))
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				/* Update window */
		    	j++;

				/* Message if item is recharged, if inscribed !! */
				if (!(o_ptr->timeout)) recharged_notice(o_ptr, TRUE);
			}
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Redraw stuff */
		p_ptr->redraw |= (PR_EQUIP);
	}

	/* Recharge rods */
	for (j = 0, i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{

			/* Determine how many rods are charging. */
			s16b temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Update if any rods are recharged */
			if (temp > (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval)
			{
				/* Update window */
				j++;

			/* Message if whole stack is recharged, if inscribed !! */
			if (!(o_ptr->timeout)) recharged_notice(o_ptr, TRUE);

			/* Message if first rod in the stack is recharged, if inscribed !! */
			else if (temp == o_ptr->number) recharged_notice(o_ptr, FALSE);

			}

		}
	}

	/* Notice changes */
	if (j)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_ITEMLIST);
	}

	/*** Recharge the ground ***/
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			s16b temp;

			k_ptr = &k_info[o_ptr->k_idx];

			/* Determine how many rods are charging. */
			temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

		}
	}

	/*re-charge rods and artifacts in the home*/
	for (i = 0; i < MAX_INVENTORY_HOME	; ++i)
	{
		store_type *st_ptr = &store[STORE_HOME];

		/* Object */
		o_ptr = &st_ptr->stock[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Determine how many rods are charging. */
			s16b temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;


		}
		else if ((o_ptr->art_num) && (o_ptr->timeout))
		{
			/* Decrease timeout by that number. */
			o_ptr->timeout--;

			/* Boundary control, paranoia. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
		}
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
	else if (effective_depth(p_ptr->depth) <= 20)
	{
		sound(MSG_AMBIENT_DNG1);
	}

	/* Dungeon level 21-40 */
	else if (effective_depth(p_ptr->depth) <= 40)
	{
		sound(MSG_AMBIENT_DNG2);
	}

	/* Dungeon level 41-60 */
	else if (effective_depth(p_ptr->depth) <= 60)
	{
		sound(MSG_AMBIENT_DNG3);
	}

	/* Dungeon level 61-80 */
	else if (effective_depth(p_ptr->depth) <= 80)
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

	/* Debug message */
	if (cheat_room)
	{
		msg_c_format(MSG_NOTICE, "Putting out fires.");
		disturb(0, 0);
	}

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

	/* Rescan the element flags of the level */
	update_level_flag();
}

/*
 * Helper for process_world -- decrement p_ptr->timed[] fields.
 */
static void decrease_timeouts(void)
{
	int adjust = (adj_con_fix[p_ptr->state.stat_ind[A_CON]] + 1);
	int i;

	/* Decrement all effects that can be done simply */
	for (i = 0; i < TMD_MAX; i++)
	{
		int decr = 1;
		if (!p_ptr->timed[i])
			continue;

		switch (i)
		{
			case TMD_CUT:
			{
				/* Hack -- check for truly "mortal" wound */
				decr = (p_ptr->timed[i] > 1000) ? 0 : adjust;
				break;
			}

			case TMD_POISONED:
			case TMD_STUN:
			{
				decr = adjust;
				break;
			}
		}
		/* Decrement the effect */
		dec_timed(i, decr, FALSE);
	}

	return;
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i;

	int regen_amount;

	int feat;

	object_type *o_ptr;

	bool was_ghost = FALSE;

	/* We decrease noise slightly every game turn */
	total_wakeup_chance -= 400;

	/* But the character always makes some noise */
	if (total_wakeup_chance < p_ptr->base_wakeup_chance)
	    total_wakeup_chance = p_ptr->base_wakeup_chance;

	/* Every 10 game turns */
	if (turn % 10) return;

	/*** Update quests ***/
	if ((p_ptr->cur_quest) && !(turn % QUEST_TURNS))
	{

		quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

		/* Check for failure */
		if ((p_ptr->cur_quest != p_ptr->depth) && (one_in_(20)))
		{
			/* Check if quest is in progress */
			if ((q_ptr->q_flags & (QFLAG_STARTED)) && q_ptr->active_level &&
				((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_UNIQUE)))
				quest_fail();
		}

		/* hack - make sure there are enough monsters */
		if ((q_ptr->q_type == QUEST_THEMED_LEVEL) ||
			(q_ptr->q_type == PIT_NEST_QUEST_BOOST))
		{
			if ((q_ptr->max_num - q_ptr->cur_num) > mon_cnt)
			{
				int old_feeling = feeling;
				int i, y, x;
				int best_r_idx, r_idx;
				int attempts_left = 10000;

				if (q_ptr->q_type == QUEST_THEMED_LEVEL)
				{
					monster_level = effective_depth(p_ptr->depth) + THEMED_LEVEL_QUEST_BOOST;
				}
				else  monster_level = effective_depth(p_ptr->depth) + PIT_NEST_QUEST_BOOST;

				/* make a monster */
				get_mon_hook(q_ptr->theme);

				/* Prepare allocation table */
				get_mon_num_prep();

				/* mega-hack - undo feeling so monster can be generated */
				feeling = 0;

				best_r_idx = 0;

				/* Find a legal, distant, unoccupied, space */
				while (attempts_left)
				{
					--attempts_left;

					/* Pick a location */
					y = rand_int(p_ptr->cur_map_hgt);
					x = rand_int(p_ptr->cur_map_wid);

					/* Require a grid that all monsters can exist in. */
					if (!cave_empty_bold(y, x)) continue;

					/* Accept far away grids */
					if (distance(y, x, p_ptr->py, p_ptr->px) >  MAX_SIGHT + 5) break;
				}

				if (attempts_left)
				{
					monster_race *r_ptr;
					monster_race *r2_ptr = &r_info[best_r_idx];

					/* 10 chances to get the strongest monster possible */
					for (i = 0; i < 10; i++)
					{
						r_idx = get_mon_num(monster_level, y, x);

						if (!best_r_idx)
						{
							best_r_idx = r_idx;
							r2_ptr = &r_info[best_r_idx];
							continue;
						}

						r_ptr = &r_info[r_idx];

						/* Don't use a unique as a replacement */
						if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

						/* Weaker monster.  Don't use it */
						if (r_ptr->mon_power < r2_ptr->mon_power) continue;

						best_r_idx = r_idx;
						r2_ptr = &r_info[best_r_idx];

					}

					if (place_monster_aux(y, x, best_r_idx, TRUE, FALSE))
					{
						/* Scan the monster list */
						for (i = 1; i < mon_max; i++)
						{
							monster_type *m_ptr = &mon_list[i];

							/* Ignore dead monsters */
							if (!m_ptr->r_idx) continue;

							/*mark it as a quest monster*/
							m_ptr->mflag |= (MFLAG_QUEST);
						}
					}
				}

				feeling = old_feeling;

				monster_level = effective_depth(p_ptr->depth);

				/* Remove restriction */
				get_mon_num_hook = NULL;

				/* Prepare allocation table */
				get_mon_num_prep();

			}
		}
	}

	/* Play an ambient sound at regular intervals. */
	if (!(turn % ((10L * TOWN_DAWN) / 4)))
	{
		play_ambient_sound();
	}

	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
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
				msg_print("The sun has risen.");
			}

			/* Night falls */
			else
			{
				/* Message */
				msg_print("The sun has fallen.");
			}

			/* Illuminate */
			town_illuminate(dawn);
		}
	}

	/* While in the dungeon */
	else
	{
		/*** Update the Stores ***/

		/* Update each store once a day (while in dungeon) */
		if (!(turn % (10L * STORE_TURNS)))
		{

			int n;

			/* Message */
			if (cheat_xtra) msg_print("Updating Shops...");

			/* Maintain each shop (except home and guild) */
			for (n = 0; n < MAX_STORES; n++)
			{
				/* Skip the home */
				if (n == STORE_HOME) continue;
				if (n == STORE_GUILD)  continue;

				/* Maintain */
				store_maint(n);
			}

			/* Sometimes, shuffle the shop-keepers */
			if (one_in_(STORE_SHUFFLE))
			{

				/* Message */
				if (cheat_xtra) msg_print("Shuffling a Shopkeeper...");

				/* Pick a random shop (except home and guild) */
				while (1)
				{
					n = rand_int(MAX_STORES);
					if ((n != STORE_HOME) && (n != STORE_GUILD)) break;
				}

				/* Shuffle it */
				store_shuffle(n);
			}

			/* Message */
			if (cheat_xtra) msg_print("Done.");
		}
	}

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
	if ((level_flag & (LF1_FIRE)) && !(turn % 1000)) put_out_fires();

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
	notice_stuff();

	/*** Damage over Time ***/

	/* Get the feature */
	feat = cave_feat[p_ptr->py][p_ptr->px];

	/* If paralyzed, we drown in deep */
	if ((p_ptr->timed[TMD_PARALYZED] || (p_ptr->timed[TMD_STUN] >= 100)) &&
		feat_ff2_match(feat, FF2_DEEP))
	{
		/* Calculate damage */
		int dam = damroll(4, 6);

		/* Don't kill the player, just hurt him/her */
		if (dam <= p_ptr->chp)
		{
			char name[80];

			/* Get the feature name */
			feature_desc(name, sizeof(name), feat, TRUE, TRUE);

			msg_format("You are drowning in %s!", name);

			/* Apply the blow */
			take_hit(dam, "drowning");
		}
	}

	/* Take damage from poison */
	if (p_ptr->timed[TMD_POISONED])
	{
		/* Take damage */
		if(!(p_ptr->state.immune_pois))take_hit(1, "poison");
	}

	/* Take damage from cuts */
	if (p_ptr->timed[TMD_CUT])
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->timed[TMD_CUT] > 200)
		{
			i = 3;
		}

		/* Severe cut */
		else if (p_ptr->timed[TMD_CUT] > 100)
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
			i = extract_energy[p_ptr->state.p_speed] * 2;

			/* Regeneration takes more food */
			if (p_ptr->state.regenerate) i += 30;

			/* Slow digestion takes less food */
			if (p_ptr->state.slow_digest) i -= 10;

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
			if (!p_ptr->timed[TMD_PARALYZED] && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from the lack of food.");
				disturb(1, 0);

				/* Hack -- faint (bypass free action) */
				(void)inc_timed(TMD_PARALYZED, 1 + rand_int(5), TRUE);
			}
		}
	}

	/* Regeneration ability */
	if (p_ptr->state.regenerate)
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
	if (p_ptr->timed[TMD_PARALYZED]) regen_amount = 0;
	if ((p_ptr->timed[TMD_POISONED]) && (!(p_ptr->state.immune_pois))) regen_amount = 0;
	if (p_ptr->timed[TMD_STUN]) regen_amount = 0;
	if (p_ptr->timed[TMD_CUT]) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}

	/*** Timeout Various Things ***/

	decrease_timeouts();

	/* Warn about flying */
	if (p_ptr->timed[TMD_FLYING])
	{
		if ((p_ptr->timed[TMD_FLYING] <= 3) && (p_ptr->timed[TMD_FLYING] > 0) &&
            (!p_ptr->state.ffall || ((f_info[cave_feat[p_ptr->py][p_ptr->px]].dam_non_native > 0) &&
             !is_player_native(p_ptr->py, p_ptr->px))))
		{
			msg_c_format(MSG_LOSING_FLYING, "You are about to stop flying.");

			disturb(0, 0);

		}
	}

	/*Temporary Native Flags*/

	/* Native to Lava */
	if (p_ptr->timed[TMD_NAT_LAVA])
	{
		if ((p_ptr->timed[TMD_NAT_LAVA]) && (p_ptr->timed[TMD_NAT_LAVA] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_LAVA))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to lava.");

				disturb(0, 0);

			}
		}

	}

	/* Native to Oil */
	if (p_ptr->timed[TMD_NAT_OIL])
	{
		if ((p_ptr->timed[TMD_NAT_OIL]) && (p_ptr->timed[TMD_NAT_OIL] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_OIL))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to oil.");

				disturb(0, 0);

			}
		}
	}

	/* Native to Sand */
	if (p_ptr->timed[TMD_NAT_SAND])
	{
		if ((p_ptr->timed[TMD_NAT_SAND]) && (p_ptr->timed[TMD_NAT_SAND] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_SAND))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to sand.");

				disturb(0, 0);
			}
		}
	}

	/* Native to Forest */
	if (p_ptr->timed[TMD_NAT_TREE])
	{
		if ((p_ptr->timed[TMD_NAT_TREE]) && (p_ptr->timed[TMD_NAT_TREE] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_FOREST))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to forest.");

				disturb(0, 0);
			}
		}
	}

	/* Native to Water */
	if (p_ptr->timed[TMD_NAT_WATER])
	{
		if ((p_ptr->timed[TMD_NAT_WATER]) && (p_ptr->timed[TMD_NAT_WATER] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_WATER))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to water.");

				disturb(0, 0);
			}
		}

	}

	/* Native to Mud */
	if (p_ptr->timed[TMD_NAT_MUD])
	{
		if ((p_ptr->timed[TMD_NAT_MUD]) && (p_ptr->timed[TMD_NAT_MUD] < 5))
		{
			if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_MUD))
			{
				msg_c_format(MSG_LOSING_NATIVITY, "You are about to lose nativity to mud.");

				disturb(0, 0);
			}
		}
	}

	/* Animate trees if necessary */
	if (p_ptr->timed[TMD_CALL_HOURNS] > 0) call_huorns();

	/*** Process Light ***/

	/* Check for light being wielded */
	o_ptr = &inventory[INVEN_LIGHT];

	/* Burn some fuel in the current lite */
	if (o_ptr->tval == TV_LIGHT)
	{
		/* Hack -- Use some fuel (except on artifacts) */
		if (!artifact_p(o_ptr) && (o_ptr->timeout > 0) &&
			!(cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW | CAVE_HALO)))
		{
			/* Decrease life-span */
			o_ptr->timeout--;

			/* Hack -- notice interesting fuel steps */
			if ((o_ptr->timeout < 100) || (!(o_ptr->timeout % 100)))
			{
				/* Redraw stuff */
				p_ptr->redraw |= (PR_EQUIP);
			}

			/* Hack -- Special treatment when blind */
			if (p_ptr->timed[TMD_BLIND])
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
				disturb(0, 0);
				msg_print("Your light is growing faint.");
			}
		}
	}


	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/*** Process Inventory ***/

	/* Handle experience draining */
	if (p_ptr->state.exp_drain)
	{
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
		{
			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Recharge activatable objects and rods */
	recharge_objects();

	/* Feel the inventory */
	sense_inventory();


	/*** Process Objects ***/

	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->state.teleport) && (rand_int(100) < 1))
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

				/* Go to the town. */
				dungeon_change_level(0);
			}
			else
			{
				/* New depth */
				int new_depth = p_ptr->recall_depth;
				if (new_depth < 1) new_depth = 1;

				msg_print("You feel yourself yanked downwards!");

				dungeon_change_level(new_depth);
			}
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
		if ((rand_int(80) < p_ptr->state.skills[SKILL_SEARCH]) &&
			(rand_int(80) < p_ptr->state.skills[SKILL_SEARCH]))
		{
			/* Now have a feeling */
			do_feeling = TRUE;

			/* Announce feeling */
			do_cmd_feeling();

			/* Update the level indicator */
			p_ptr->redraw |= (PR_DEPTH | PR_FEELING);

			/* Disturb */
			disturb(0, 0);
		}
	}

	/* Notice stuff */
	notice_stuff();

}

/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time - unless resurrecting a dead character */
	if (!(p_ptr->noscore & 0x0002) && !(p_ptr->is_dead))
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
			p_ptr->redraw |= (PR_MONSTER);

			/* Window stuff */
			handle_stuff();
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
	int i;

	/* One more player turn */
	p_ptr->p_turn++;

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
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    !p_ptr->timed[TMD_BLIND] && !p_ptr->timed[TMD_CONFUSED] &&
			    !p_ptr->timed[TMD_POISONED] && !p_ptr->timed[TMD_AFRAID] &&
			    !p_ptr->timed[TMD_STUN] && !p_ptr->timed[TMD_CUT] &&
			    !p_ptr->timed[TMD_SLOW] && !p_ptr->timed[TMD_PARALYZED] &&
			    !p_ptr->timed[TMD_IMAGE] && !p_ptr->word_recall &&
			    (p_ptr->food < PY_FOOD_UPPER))
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
			if (p_ptr->csp == p_ptr->msp)
			{
				disturb(0, 0);
			}
		}
	}

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

		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Refresh (optional) */
		(void)Term_fresh();

		/* Hack -- Pack Overflow */
		pack_overflow();

		/* Hack -- reset to inventory display */
		if (!p_ptr->command_new) p_ptr->command_wrk = USE_INVEN;

		/* Assume free turn */
		p_ptr->p_energy_use = 0;

		/* Paralyzed or Knocked Out */
		if ((p_ptr->timed[TMD_PARALYZED]) || (p_ptr->timed[TMD_STUN] >= 100))
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

		/* Repeated command */
		else if (p_ptr->command_rep)
		{
			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command(CMD_GAME, TRUE);

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

			/* Get and process a command */
			process_command(CMD_GAME, FALSE);

			py_pickup_gold();
		}

		/*** Clean up ***/

		/*hack - check for secret squares*/
		if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_MARKED))
		{
			/*increase chance of altered inventory for around 100 turns*/
			altered_inventory_counter += 1;

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
			effect_type *x_ptr;

			/* Use some energy */
			p_ptr->p_energy -= p_ptr->p_energy_use;


			/* Hack -- constant hallucination */
			if (p_ptr->timed[TMD_IMAGE])
			{
				p_ptr->redraw |= (PR_MAP);
			}

			/* Hack -- Redraw depth if the temporary quest notification ends */
			if ((quest_indicator_timer > 0) && (--quest_indicator_timer == 0) &&
				!(character_icky))
			{
				quest_indicator_complete = FALSE;
				p_ptr->redraw |= (PR_QUEST_ST);
			}

			/* Shimmer monsters if needed */
			if (shimmer_monsters)
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
					light_spot(m_ptr->fy, m_ptr->fx);
				}
			}

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
					light_spot(x_ptr->x_cur_y, x_ptr->x_cur_x);
				}
			}

			/* Redraw visual indicator of temporary element brand */
			if (p_ptr->timed[TMD_SLAY_ELEM]) p_ptr->redraw |= (PR_RESIST);

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
							light_spot(m_ptr->fy, m_ptr->fx);
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

	/* Notice stuff (if needed) */
	if (p_ptr->notice) notice_stuff();

	/* Update stuff (if needed) */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff (if needed) */
	if (p_ptr->redraw) redraw_stuff();

	/* Place the cursor on the player */
	move_cursor_relative(p_ptr->py, p_ptr->px);

	/* Refresh */
	(void)Term_fresh();

}

/*
 * Checks if multi-color monsters onscreen.
 */
void do_animation(void)
{
	int i;

	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		if (!m_ptr->r_idx) continue;
		if (!m_ptr->ml) continue;
		if (!(r_ptr->flags1 & (RF1_ATTR_MULTI))) continue;

		m_ptr->m_attr = multi_hued_attr(r_ptr);

		p_ptr->redraw |= (PR_MAP | PR_MONLIST);
	}
}


/*
 * This is used when the user is idle to allow for simple animations.
 * Currently the only thing it really does is animate shimmering monsters.
 */
void idle_update(void)
{
	if (!character_dungeon) return;

	if (!animate_flicker) return;

	/* Animate and redraw if necessary */
	do_animation();
	redraw_stuff();

	/* Refresh the main screen */
	Term_fresh();
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

	/* Track maximum quest level */
	if ((p_ptr->max_depth > 1) &&
		(p_ptr->max_depth > p_ptr->quest_depth))
	{
		p_ptr->quest_depth = p_ptr->max_depth;
	}

	/* Track recall dungeon level */
	if (p_ptr->recall_depth < p_ptr->depth)
	{
		p_ptr->recall_depth = p_ptr->depth;
	}

	/* If autosave is pending, do it now. */
	if (p_ptr->autosave)
	{
		save_game();
		p_ptr->autosave = FALSE;
	}

	/* No stairs down from fixed quests */
	if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
		(quest_check(p_ptr->depth) == QUEST_FIXED_U) ||
		(quest_check(p_ptr->depth) == QUEST_FIXED_MON))
	{
		if ((p_ptr->create_stair == FEAT_MORE) ||
			(p_ptr->create_stair == FEAT_MORE_SHAFT))
	   		 p_ptr->create_stair = FALSE;
	}

	/* No stairs from town or if not allowed */
	if (!p_ptr->depth)
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

	/* Redraw "statusy" things */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_MONSTER | PR_MONLIST | PR_ITEMLIST);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Hack -- Decrease "xtra" depth */
	character_xtra--;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_NATIVE);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

	/* Make basic mouse buttons */
	(void) button_add("[ESC]", ESCAPE);
	(void) button_add("[Ret]", '\r');
	(void) button_add("[Spc]", ' ');
	(void) button_add("[Rpt]", 'n');
	(void) button_add("[Std]", ',');

	/* Redraw buttons */
	p_ptr->redraw |= (PR_BUTTONS);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

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
				q_info[i].q_flags |= (QFLAG_STARTED);
				break;
			}
		}
	}

	/* Announce (or repeat) the feeling */
	if ((effective_depth(p_ptr->depth)) && (do_feeling)) do_cmd_feeling();


	/* Announce a player ghost challenge. -LM- */
	if (bones_selector) ghost_challenge();

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = effective_depth(p_ptr->depth);

	/* Reset the object generation level */
	object_level = effective_depth(p_ptr->depth);

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

		/* Do any necessary animations */
		do_animation();

		/* Update terrain damage every game turn */
		if ((!is_player_native(p_ptr->py, p_ptr->px)) && (!p_ptr->timed[TMD_FLYING]))
		{
			p_ptr->cumulative_terrain_damage += f_info[cave_feat[p_ptr->py][p_ptr->px]].dam_non_native;
		}

		/*** Process player & monsters ***/
		process_entities();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		(void)Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process the world */
		process_world();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Hack -- Hilite the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Optional fresh */
		(void)Term_fresh();

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
void play_game(void)
{
	/* Initialize */
	bool new_game = init_angband();

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

	do_playtesting = FALSE;

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
		my_strcpy(op_ptr->base_name, "PLAYER", sizeof(op_ptr->base_name));
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

		/* Start in town */
		p_ptr->depth = 0;

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

		/* Hack -- seed for random artifacts */
		seed_randart = rand_int(0x10000000);

		/* Roll up a new character. Quickstart is allowed if ht_birth is set */
		player_birth(p_ptr->ht_birth ? TRUE : FALSE);

		/* Randomize the artifacts */
		if (adult_rand_artifacts)
		{
			do_randart(seed_randart, TRUE);
		}

		/* Hack -- enter the world */
		turn = 1;

		p_ptr->p_turn = 0;

		quest_indicator_timer = 0;
		quest_indicator_complete = FALSE;
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

	/* Tell the UI we've started. */
	event_signal(EVENT_ENTER_GAME);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_INVEN | PR_EQUIP | PR_MONSTER | PR_MESSAGE);

	/* Window stuff */
	handle_stuff();

	/* Process some user pref files */
	process_some_user_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Remove ironman ego-items if needed */
	if (!adult_ironman && !adult_no_stores)
	{
		remove_ironman_items();
		remove_ironman_ego_items();
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

	/* Save not required yet. */
	p_ptr->autosave = FALSE;

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

		/* Cancel the target */
		target_set_monster(0);

		/* Cancel the health bar */
		health_track(0);

		/* Forget the view */
		forget_view();

		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;

		/* Erase the old cave */
		wipe_o_list();
		wipe_mon_list();
		wipe_x_list();
		count_feat_everseen();

		/* Delete any pending monster message */
		size_mon_msg = 0;
		size_mon_hist = 0;

		/* Check for quest_failure */
		if (p_ptr->cur_quest)
		{
			byte quest_info = quest_check(p_ptr->cur_quest);

			quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

			if (quest_info == QUEST_VAULT)
			{
				/* Check if had already been on quest level */
				if (q_ptr->q_flags & (QFLAG_STARTED))
				{
					if (quest_item_slot() == -1) quest_fail();

				}

			}

			else if ((quest_info == QUEST_MONSTER) || (quest_info == QUEST_UNIQUE))
			{
				/* Check that the quest is in process */
				if ((p_ptr->cur_quest != p_ptr->depth) && (q_ptr->q_flags & (QFLAG_STARTED))
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
				if ((q_ptr->q_flags & (QFLAG_STARTED)) && (q_ptr->active_level)) quest_fail();
			}
		}

		/* XXX XXX XXX */
		message_flush();

		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			/* Mega-Hack -- Allow player to cheat death */
			if ((p_ptr->wizard || cheat_live) && !get_check("Die? "))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				p_ptr->noscore |= 0x0001;

				/* Message */
				msg_print("You invoke wizard mode and cheat death.");
				message_flush();

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void)(clear_timed(TMD_BLIND, TRUE));
				(void)clear_timed(TMD_CONFUSED, TRUE);
				(void)clear_timed(TMD_POISONED, TRUE);
				(void)clear_timed(TMD_AFRAID, TRUE);
				(void)clear_timed(TMD_PARALYZED, TRUE);
				(void)clear_timed(TMD_IMAGE, TRUE);
				(void)set_stun(0);
				(void)set_cut(0);

				/* Hack -- Prevent starvation */
				(void)set_food(PY_FOOD_MAX - 1);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall)
				{
					/* Message */
					msg_print("A tension leaves the air around you...");
					message_flush();

					/* Hack -- Prevent recall */
					p_ptr->word_recall = 0;
				}

				/* Note cause of death XXX XXX XXX */
				my_strcpy(p_ptr->died_from, "Cheating death", sizeof(p_ptr->died_from));

				/* New depth */
				dungeon_change_level(0);

			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Make a new level */
		generate_cave();

	}

	/* Tell the UI we're done with the game state */
	event_signal(EVENT_LEAVE_GAME);

	/* Close stuff */
	close_game();
}

