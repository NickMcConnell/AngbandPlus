/* File: dungeon.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
int value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);

		/* Superb */
		if ((variant_great_id) && (o_ptr->xtra1)) return (INSCRIP_SUPERB);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Great "armor" bonus */
	if ((variant_great_id) && (o_ptr->to_a > 8)) return (INSCRIP_GREAT);

	/* Great "weapon" bonus */
	if ((variant_great_id) && (o_ptr->to_h + o_ptr->to_d > 14)) return (INSCRIP_GREAT);

	/* Very good "armor" bonus */
	if ((variant_great_id) && (o_ptr->to_a > 4)) return (INSCRIP_VERY_GOOD);

	/* Good "weapon" bonus */
	if ((variant_great_id) && (o_ptr->to_h + o_ptr->to_d > 7)) return (INSCRIP_VERY_GOOD);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* Default to "average" */
	return (0);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static int value_check_aux2(object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr)) return (INSCRIP_UNCURSED);

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return (INSCRIP_UNCURSED);

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_UNCURSED);

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_UNCURSED);

	/* No feeling */
	return (0);
}



/*
 * Sense the inventory
 *
 * Class abilities defined in c_info.txt
 */
static void sense_inventory(void)
{
	int i;

	int plev = p_ptr->lev;

	int feel;

	u32b f1=0x0L;
	u32b f2=0x0L;
	u32b f3=0x0L;

	u32b af1=0x0L;
	u32b af2=0x0L;
	u32b af3=0x0L;

	object_type *o_ptr;

	char o_name[80];

	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* No sensing when paralyzed */
	if (p_ptr->paralyzed) return;

	/* No sensing when knocked out */
	if (p_ptr->stun > 100) return;

	if (cp_ptr->sense_squared)
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev * plev + cp_ptr->sense_div)))
			return;
	}
	else
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev + cp_ptr->sense_div)))
			return;
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL+1; i++)
	{
		bool okay = FALSE;

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Always sense flags to see if we have ability */
		if ((i >= INVEN_WIELD) && (i != INVEN_BELT))
		{
			u32b if1,if2,if3;

			object_flags(o_ptr,&if1,&if2,&if3);

			af1 |= if1;
			af2 |= if2;
			af3 |= if3;

		}

		/* Sometimes sense flags to see if we gain ability */
		if (!(o_ptr->ident & (IDENT_MENTAL)) && (i >= INVEN_WIELD) && (i != INVEN_BELT) && (rand_int(100)<30))
		{
			u32b if1,if2,if3;

			object_flags(o_ptr,&if1,&if2,&if3);

			f1 |= (if1 & ~(o_ptr->may_flags1)) & ~(o_ptr->can_flags1);
			f2 |= (if2 & ~(o_ptr->may_flags2)) & ~(o_ptr->can_flags2);
			f3 |= (if3 & ~(o_ptr->may_flags3)) & ~(o_ptr->can_flags3);
		}

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
			case TV_INSTRUMENT:
			case TV_STAFF:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip non-sense machines */
		if (!okay) continue;

		/* It already has a discount or special inscription */
		if (o_ptr->discount > 0) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* Check for a feeling */
		feel = (cp_ptr->sense_heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Message (equipment) */
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
			msg_format("You feel the %s (%c) in your pack %s %s...",
				   o_name, index_to_label(i),
				   ((o_ptr->number == 1) ? "is" : "are"),
				   inscrip_text[feel - INSCRIP_NULL]);
		}

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

	/* Hack --- silently notice stuff */
	if ((f1 & (TR1_STR)) && (rand_int(100)<30)) equip_can_flags(TR1_STR,0x0L,0x0L);
	else if (!(af1 & (TR1_STR))) equip_not_flags(TR1_STR,0x0L,0x0L);

	if ((f1 & (TR1_INT)) && (rand_int(100)<30)) equip_can_flags(TR1_INT,0x0L,0x0L);
	else if (!(af1 & (TR1_INT))) equip_not_flags(TR1_INT,0x0L,0x0L);

	if ((f1 & (TR1_WIS)) && (rand_int(100)<30)) equip_can_flags(TR1_WIS,0x0L,0x0L);
	else if (!(af1 & (TR1_WIS)) ) equip_not_flags(TR1_WIS,0x0L,0x0L);

	if ((f1 & (TR1_DEX)) && (rand_int(100)<30)) equip_can_flags(TR1_DEX,0x0L,0x0L);
	else if (!(af1 & (TR1_DEX)) ) equip_not_flags(TR1_DEX,0x0L,0x0L);

	if ((f1 & (TR1_CON)) && (rand_int(100)<30)) equip_can_flags(TR1_CON,0x0L,0x0L);
	else if (!(af1 & (TR1_CON)) ) equip_not_flags(TR1_CON,0x0L,0x0L);

	if ((f1 & (TR1_CHR)) && (rand_int(100)<30)) equip_can_flags(TR1_CHR,0x0L,0x0L);
	else if (!(af1 & (TR1_CHR)) ) equip_not_flags(TR1_CHR,0x0L,0x0L);

	if ((f1 & (TR1_STEALTH)) && (rand_int(100)<30)) equip_can_flags(TR1_STEALTH,0x0L,0x0L);
	else if (!(af1 & (TR1_STEALTH)) ) equip_not_flags(TR1_STEALTH,0x0L,0x0L);

	if ((f1 & (TR1_SEARCH)) && (rand_int(100)<30)) equip_can_flags(TR1_SEARCH,0x0L,0x0L);
	else if (!(af1 & (TR1_SEARCH)) ) equip_not_flags(TR1_SEARCH,0x0L,0x0L);

	if ((f1 & (TR1_SPEED)) && (rand_int(100)<30)) equip_can_flags(TR1_SPEED,0x0L,0x0L);
	else if (!(af1 & (TR1_SPEED))) equip_not_flags(TR1_SPEED,0x0L,0x0L);

	if ((f3 & (TR3_SLOW_DIGEST)) && (rand_int(100)<30)) equip_can_flags(0x0L,0x0L,TR3_SLOW_DIGEST);
	else if (!(af3 & (TR3_SLOW_DIGEST))) equip_not_flags(0x0L,0x0L,TR3_SLOW_DIGEST);

	if ((f3 & (TR3_REGEN)) && (rand_int(100)<30)) equip_can_flags(0x0L,0x0L,TR3_REGEN);
	else if (!(af3 & (TR3_REGEN))) equip_not_flags(0x0L,0x0L,TR3_REGEN);
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

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
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

			/* Uniques recover faster */
			if (r_ptr->flags1 & (RF1_UNIQUE)) frac *= 2;

			/* Hack -- Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
		}
	}
}

/*
 * Monster hook to use for 'wandering' monsters.
 */
bool dun_level_mon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* If no restriction, accept anything */
	if (!(t_info[p_ptr->dungeon].r_char) && !(t_info[p_ptr->dungeon].r_flag)) return (TRUE);

	/* Hack -- Accept monsters with graphic */
	if ((t_info[p_ptr->dungeon].r_char) && (r_ptr->d_char == t_info[p_ptr->dungeon].r_char)) return (TRUE);

	/* Hack -- Accept monsters with flag */
	if (t_info[p_ptr->dungeon].r_flag)
	{
		int mon_flag = t_info[p_ptr->dungeon].r_flag-1;

		if ((mon_flag < 32) && 
			(r_ptr->flags1 & (1L << mon_flag))) return (TRUE);

		if ((mon_flag >= 32) && 
			(mon_flag < 64) && 
			(r_ptr->flags2 & (1L << (mon_flag -32)))) return (TRUE);

		if ((mon_flag >= 64) && 
			(mon_flag < 96) && 
			(r_ptr->flags3 & (1L << (mon_flag -64)))) return (TRUE);

		if ((mon_flag >= 96) && 
			(mon_flag < 128) && 
			(r_ptr->flags4 & (1L << (mon_flag -96)))) return (TRUE);
	}

	return (FALSE);

}


/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i, j, k;

	feature_type *f_ptr;

	room_info_type *d_ptr;

	int mimic;

	int regen_amount;

	object_type *o_ptr;

	cptr name;

	int by,bx;

	bool room;

	/* Every 10 game turns */
	if (turn % 10) return;

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

	/* While in town */
	if (p_ptr->depth == min_depth(p_ptr->dungeon))
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

			/* Update runes */
			p_ptr->update |= (PU_RUNES);
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

			/* Maintain each shop (except home) */
			for (n = 0; n < MAX_STORES; n++)
			{
				/* Maintain */
				store_maint(n);
			}

			/* Sometimes, shuffle the shop-keepers */
			if (rand_int(STORE_SHUFFLE) == 0)
			{
				/* Message */
				if (cheat_xtra) msg_print("Shuffling a Shopkeeper...");

				/* Pick a random shop (except home) */
				while (1)
				{
					n = rand_int(MAX_STORES);
					if (n != STORE_HOME) break;
				}

				/* Shuffle it */
				store_shuffle(n);
			}

			/* Message */
			if (cheat_xtra) msg_print("Done.");
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation */
	if (rand_int(MAX_M_ALLOC_CHANCE) == 0)
	{
		/* Ensure wandering monsters suit the dungeon level */
		get_mon_num_hook = dun_level_mon;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE);

		/* Ensure wandering monsters suit the dungeon level */
		get_mon_num_hook = NULL;

		/* Prepare allocation table */
		get_mon_num_prep();
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Get the feature */
	f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

	/* Get the mimiced feature */
	mimic = f_ptr->mimic;

	/* Use covered or bridged if necessary */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		f_ptr = &(f_info[f_ptr->mimic]);
	}

	/* Take damage from features */
	if (!(f_ptr->flags1 & (FF1_HIT_TRAP)) &&
	    ((f_ptr->blow.method) || (f_ptr->spell)))
	{

		/* Damage from terrain */
		hit_trap(p_ptr->py,p_ptr->px);

	}

	/* If paralyzed, we drown in shallow, deep or filled */
	if ((p_ptr->paralyzed || (p_ptr->stun >=100)) &&
		(f_ptr->flags2 & (FF2_DEEP | FF2_SHALLOW | FF2_FILLED)))
	{

		/* Get the mimiced feature */
		mimic = f_ptr->mimic;

		/* Get the feature name */
		name = (f_name + f_info[mimic].name);

		msg_format("You are drowning %s%s!",(f_ptr->flags2 & (FF2_FILLED)?"":"in the "),name);

		/* Apply the blow */
		project_p(0, 0, p_ptr->py, p_ptr->px, damroll(4,6), GF_WATER);
	}

	/* Take damage from poison */
	if (p_ptr->poisoned)
	{
		/* Take damage */
		take_hit(1, "poison");
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


	/*** Check the Food, Rest, and Regenerate ***/

	/* Special rooms affect some of this */
	by = p_ptr->py/BLOCK_HGT;
	bx = p_ptr->px/BLOCK_HGT;

	/* In a room */
	room = (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM)) ? TRUE : FALSE;

	/* Get the room */
	d_ptr = &room_info[dun_room[by][bx]];

	/* Tire normally */
	/* XXX We exclude situations where we adjust this counter elsewhere */
	if (variant_fast_moves)
	{
		if (!(p_ptr->resting || p_ptr->searching || p_ptr->running || p_ptr->paralyzed || (p_ptr->stun >= 100)) ||
			(f_ptr->flags2 & (FF2_FILLED)))
		{
			(void)set_rest(p_ptr->rest - p_ptr->tiring);
		}
	}

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
	if ((p_ptr->food < PY_FOOD_WEAK) || (p_ptr->rest < PY_REST_WEAK))
	{
		/* Lower regeneration */
		if (p_ptr->food < PY_FOOD_STARVE)
		{
			regen_amount = 0;
		}
		else if ((p_ptr->food < PY_FOOD_FAINT) || (p_ptr->rest < PY_REST_FAINT))
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

		/* Getting Faint - lack of rest */
		if (p_ptr->rest < PY_REST_FAINT) 
		{
			/* Faint occasionally */
			if (!p_ptr->paralyzed && (rand_int(100) < 10))
			{
				/* Message */
				msg_print("You faint from exhaustion.");
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

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(regen_amount);
	}

	/* Various things interfere with healing */
	if (p_ptr->paralyzed) regen_amount = 0;
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->stun) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;
	if ((room) && (d_ptr->flags & (ROOM_BLOODY))) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
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

		/* Some rooms make wounds magically worse */
		if ((room) && (d_ptr->flags & (ROOM_BLOODY))) adjust = -1;

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

		/* Some rooms make wounds magically worse */
		if ((room) && (d_ptr->flags & (ROOM_BLOODY))) adjust = -1;
		
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
		if (!artifact_p(o_ptr) && (o_ptr->pval > 0))
		{
			/* Decrease life-span */
			o_ptr->pval--;

			/* Hack -- notice interesting fuel steps */
			if ((o_ptr->pval < 100) || (!(o_ptr->pval % 10)))
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
		if ((rand_int(100) < 10) && (p_ptr->exp > 0))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,TR3_DRAIN_EXP);

			p_ptr->exp--;
			p_ptr->max_exp--;
			check_experience();
		}
	}

	/* Handle hit point draining */
	if (p_ptr->hp_drain)
	{
		if ((rand_int(100) < 10) && (p_ptr->chp > 0))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,TR3_DRAIN_HP);

			p_ptr->chp--;
			p_ptr->chp_frac = 0;

			/* Redraw */
			p_ptr->redraw |= (PR_HP);

		}
	}

	/* Handle mana draining */
	if (p_ptr->mana_drain)
	{
		if ((rand_int(100) < 10) && (p_ptr->csp > 0))
		{
			/* Always notice */
			equip_can_flags(0x0L,0x0L,TR3_DRAIN_MANA);

			p_ptr->csp--;
			p_ptr->csp_frac = 0;

			/* Redraw */
			p_ptr->redraw |= (PR_MANA);		
		}
	}

	/* Process timeouts */
	for (k = 0, j = 0, i = 0; i < INVEN_TOTAL+1; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activateable objects/rods */
		/* Also check for mimics/regenerating bodies */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				char o_name[80];

				u32b f1, f2, f3;

				/* Get the flags */
				object_flags(o_ptr,&f1, &f2, &f3);

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				/* Spells run out */
				if (o_ptr->tval == TV_SPELL)
				{
					/* Notice things */
					if (i < INVEN_PACK) j++;
					else k++;

					/* Message */
					msg_format("Your %s %s has run out.", o_name,
					   ((o_ptr->stackc == 1) ? "has" :
					   ((!(o_ptr->stackc) && (o_ptr->number == 1)) ?
					   "has" : "have")));

					/* Destroy a spell if discharged */
					if (o_ptr->stackc) inven_item_increase(i, -(o_ptr->stackc));
					else inven_item_increase(i,-(o_ptr->number));

					inven_item_optimize(i);

				}

				/* Rods and activatible items charge */
				else if ((o_ptr->tval == TV_ROD) || (f3 & (TR3_ACTIVATE)))
				{
					/* Notice things */
					if (i < INVEN_PACK) j++;
					else k++;
	
					/* Reset stack */
					o_ptr->stackc = 0;
	
					/* Message */
					msg_format("Your %s %s charged.", o_name,
					   ((o_ptr->stackc == 1) ? "has" :
					   ((!(o_ptr->stackc) && (o_ptr->number == 1)) ?
					   "has" : "have")));
				}

				/* Bodies/mimics become a monster */
				else
				{
					/* Notice things */
					if (animate_object(i))
					{
						if (i < INVEN_PACK) j++;
						else k++;
					}
				}
			}

			/* The spell is running low */
			else if ((o_ptr->timeout < 50) && (!(o_ptr->timeout % 10)) && (o_ptr->tval == TV_SPELL))
			{
				char o_name[80];

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				if (disturb_minor) disturb(0, 0);

				msg_format("Your %s is running out.", o_name);
			}

		}
	}

	/* Notice changes -- equip */
	if (k)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Disturb */
		if (disturb_minor) disturb(1, 0);
	}

	/* Notice changes - inventory */
	if (j)
	{
		/* Combine pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

		/* Disturb */
		if (disturb_minor) disturb(1,0);
	}

	/* Feel the inventory */
	sense_inventory();


	/*** Process Objects ***/

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Timing out? */
		if (o_ptr->timeout)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout))
			{
				u32b f1, f2, f3;

				/* Get the flags */
				object_flags(o_ptr,&f1, &f2, &f3);

				/* Spells run out */
				if (o_ptr->tval == TV_SPELL)
				{
					/* Destroy a spell if discharged */
					if (o_ptr->stackc) floor_item_increase(i, -(o_ptr->stackc));
					else floor_item_increase(i,-(o_ptr->number));

					floor_item_optimize(i);
				}
		
				/* Rods and activatible items charge */
				else if ((o_ptr->tval == TV_ROD) || (f3 & (TR3_ACTIVATE)))
				{
					/* Reset stack */
					o_ptr->stackc = 0;
		
				}

				/* Bodies/mimics become a monster */
				else
				{
					/* Notice count */
					j++;
		
		
				}
			}
		}

	}


	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && (rand_int(100) < 1))
	{
		/* Teleport player */
		teleport_player(40);

		/* Always notice */
		equip_can_flags(0x0L,0x0L,TR3_TELEPORT);

	}
	/* Mega-Hack -- Portal room */
	else if ((room) && (d_ptr->flags & (ROOM_PORTAL)) && (rand_int(100)<1))
	{
		/* Warn the player */
		msg_print("There is a brilliant flash of light.");

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
			if (p_ptr->depth > min_depth(p_ptr->dungeon))
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = min_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else if (min_depth(p_ptr->dungeon) < max_depth(p_ptr->dungeon))
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth <= min_depth(p_ptr->dungeon)) p_ptr->depth = min_depth(p_ptr->dungeon)+1;
				if (p_ptr->depth > max_depth(p_ptr->dungeon)) p_ptr->depth = max_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else 
			{
				msg_print("A tension leaves the air around you...");
			}
		}
	}
}


/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (verify_special || !(p_ptr->noscore & 0x0002))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the very first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		msg_print(NULL);

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
		msg_print(NULL);

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


/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif



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
		msg_print(NULL);

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


/*
 * Hack -- Declare the Borg Routines
 */
extern void do_cmd_borg(void);

#endif



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{

#ifdef ALLOW_REPEAT

	/* Handle repeating the last command */
	repeat_check();

#endif /* ALLOW_REPEAT */

	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
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
			do_cmd_set_trap_or_spike();
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
			do_cmd_study();
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
			do_cmd_cast();
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

		/* Apply a rune */
		case 'y':
		{
			do_cmd_apply_rune();
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

		/* Save "html screen dump" */
		case ']':
		{
			do_cmd_save_screen_html();
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

	static u32b     old_flags1 = 0L;
	static u32b     old_flags2 = 0L;
	static u32b     old_flags3 = 0L;
	static u32b     old_flags4 = 0L;
	static u32b     old_flags5 = 0L;
	static u32b     old_flags6 = 0L;

	static byte	old_r_blows0 = 0;
	static byte	old_r_blows1 = 0;
	static byte	old_r_blows2 = 0;
	static byte	old_r_blows3 = 0;

	static byte     old_r_cast_innate = 0;
	static byte	old_r_cast_spell = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

		/* Check for change of any kind */
		if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (old_flags1 != l_ptr->flags1) ||
		    (old_flags2 != l_ptr->flags2) ||
		    (old_flags3 != l_ptr->flags3) ||
		    (old_flags4 != l_ptr->flags4) ||
		    (old_flags5 != l_ptr->flags5) ||
		    (old_flags6 != l_ptr->flags6) ||
		    (old_r_blows0 != l_ptr->blows[0]) ||
		    (old_r_blows1 != l_ptr->blows[1]) ||
		    (old_r_blows2 != l_ptr->blows[2]) ||
		    (old_r_blows3 != l_ptr->blows[3]) ||
		    (old_r_cast_innate != l_ptr->cast_innate) ||
		    (old_r_cast_spell != l_ptr->cast_spell))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_flags1 = l_ptr->flags1;
			old_flags2 = l_ptr->flags2;
			old_flags3 = l_ptr->flags3;
			old_flags4 = l_ptr->flags4;
			old_flags5 = l_ptr->flags5;
			old_flags6 = l_ptr->flags6;

			/* Memorize blows */
			old_r_blows0 = l_ptr->blows[0];
			old_r_blows1 = l_ptr->blows[1];
			old_r_blows2 = l_ptr->blows[2];
			old_r_blows3 = l_ptr->blows[3];

			/* Memorize castings */
			old_r_cast_innate = l_ptr->cast_innate;
			old_r_cast_spell = l_ptr->cast_spell;

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
 *
 * Note that the code to check for user abort during repeated commands
 * and running and resting can be disabled entirely with an option, and
 * even if not disabled, it will only check during every 128th game turn
 * while resting, for efficiency.
 */
static void process_player(void)
{
	int i;


	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Check to see if on damaging terrain */
		/* XXX This may cause a big slow down, but is needed for 'correctness' */

		/*Get the feature */
		feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

		/* Use covered or bridged if necessary */
		if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
		{
			f_ptr = &(f_info[f_ptr->mimic]);
		}

		/* Stop resting if would take damage */
		if (!(f_ptr->flags1 & (FF1_HIT_TRAP)) &&
		    ((f_ptr->blow.method) || (f_ptr->spell)))
		{

			disturb(0, 0);
		}

		/* Stop resting if tiring too fast */
		if ((p_ptr->tiring + 10) > PY_REST_RATE)
		{
			disturb(0, 0);
		}


		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    (p_ptr->rest > PY_REST_FULL))
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
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned && !p_ptr->afraid &&
			    !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall &&
			    (p_ptr->rest > PY_REST_FULL))
			{
				disturb(0, 0);
			}
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

	/*** Start searching ***/
	if ((easy_search) && (!p_ptr->searching) && (p_ptr->last_disturb < (turn-20)))
	{
		/* Start searching */
		p_ptr->searching = TRUE;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Redraw the state */
		p_ptr->redraw |= (PR_STATE);
	
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
		if (fresh_before) Term_fresh();


		/* Hack -- Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			char o_name[80];

			object_type *o_ptr;

			/* Get the slot to be dropped */
			o_ptr = &inventory[item];

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
		p_ptr->energy_use = 0;

		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

			/* Take a turn */
			p_ptr->energy_use = 100;

			/* Catch breath */
			if (!(f_ptr->flags2 & (FF2_FILLED)))
			{
				/* Rest the player */
				set_rest(p_ptr->rest + PY_REST_RATE - p_ptr->tiring);
			}
		}

		/* Resting */
		else if (p_ptr->resting)
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

			/* Timed rest */
			if (p_ptr->resting > 0)
			{
				/* Reduce rest count */
				p_ptr->resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Catch breath */
			if (!(f_ptr->flags2 & (FF2_FILLED)))
			{
				/* Rest the player */
				set_rest(p_ptr->rest + PY_REST_RATE * 2 - p_ptr->tiring);
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

			/* Hack -- sing song */
			if (p_ptr->held_song)
			{

				/* Cast the spell */
			    	do_cmd_cast_aux(p_ptr->held_song, spell_power(p_ptr->held_song), ((p_ptr->pstyle == WS_INSTRUMENT)?"play":"sing"), "song");

				/* Hack -- Always use a full turn */
				p_ptr->energy_use = 100;

			}

			/* Use some energy */
			p_ptr->energy -= p_ptr->energy_use;

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

					/* Get the monster */
					m_ptr = &m_list[i];

					/* Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Get the monster race */
					r_ptr = &r_info[m_ptr->r_idx];

					/* Skip non-multi-hued monsters */
					if (!(r_ptr->flags1 & (RF1_ATTR_MULTI)) && !(r_ptr->flags1 & (RF1_ATTR_METAL))) continue;

					/* Reset the flag */
					shimmer_monsters = TRUE;

					/* Redraw regardless */
					lite_spot(m_ptr->fy, m_ptr->fx);
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

					/* Get the monster */
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

				/* Get the monster */
				m_ptr = &m_list[i];

				/* Skip dead monsters */
				/* if (!m_ptr->r_idx) continue; */

				/* Clear "show" flag */
				m_ptr->mflag &= ~(MFLAG_SHOW);
			}
		}
	}
	while (!p_ptr->energy_use && !p_ptr->leaving);

	/* Update dynamic terrain */
	update_dyna();

	/* Update noise flow information */
	update_noise();

	/* Update scent trail */
	update_smell();


	/* 
	 * Reset character vulnerability.  Will be calculated by 
	 * the first member of an animal pack that has a use for it.
	 */
	p_ptr->vulnerability = 0;

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
	target_set_monster(0);

	/* Cancel the health bar */
	health_track(0);


	/* Reset shimmer flags */
	shimmer_monsters = TRUE;
	shimmer_objects = TRUE;

	/* Reset repair flags */
	repair_mflag_show = TRUE;
	repair_mflag_mark = TRUE;


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


	/* No stairs down from Quest */
	if (is_quest(p_ptr->depth))
	{
		p_ptr->create_down_stair = FALSE;
	}

	/* No stairs down from deepest level */
	if (p_ptr->depth==max_depth(p_ptr->dungeon))
	{
		p_ptr->create_down_stair = FALSE;
	}


	/* No stairs from town or if not allowed */
	if ((p_ptr->depth == min_depth(p_ptr->dungeon)) || !dungeon_stair)
	{
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}

	/* Make a staircase */
	if (p_ptr->create_down_stair || p_ptr->create_up_stair)
	{
		/* Place a staircase */
		if (cave_valid_bold(py, px))
		{
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if (p_ptr->create_down_stair)
			{
				cave_set_feat(py, px, FEAT_MORE);
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


	/* Hack -- Increase "xtra" depth */
	character_xtra++;


	/* Clear */
	Term_clear();


	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/* Update stuff */
	update_stuff();

	/* Fully update the visuals (and monster distances) */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();


	/* Hack -- Decrease "xtra" depth */
	character_xtra--;

	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_ROOM_INFO);

	/* Combine / Reorder the pack */
/*	p_ptr->notice |= (PN_COMBINE | PN_REORDER);*/

	/* Window stuff */
/*	p_ptr->window |= (PW_ROOM_INFO);*/

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
	if (p_ptr->is_dead) return;

	/* Announce (or repeat) the feeling */
	if (p_ptr->depth>min_depth(p_ptr->dungeon)) do_cmd_feeling();

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

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


		/*** Apply energy ***/

		/* Give the player some energy */
		p_ptr->energy += extract_energy[p_ptr->pspeed];

		/* Can the player move? */
		while ((p_ptr->energy >= 100) && !p_ptr->leaving)
		{

			/* process monster with even more energy first */
			process_monsters((byte)(p_ptr->energy + 1));

			/* if still alive */
			if (!p_ptr->leaving)
			{
				/* Process the player */
				process_player();
			}
		}

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
		if (fresh_after) Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Process monsters */
		process_monsters(0);

		/* Reset Monsters */
		reset_monsters();

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
		if (fresh_after) Term_fresh();

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
		if (fresh_after) Term_fresh();

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Count game turns */
		turn++;
	}
}



/*
 * Process some user pref files
 *
 * Hack -- Allow players on UNIX systems to keep a ".angband.prf" user
 * pref file in their home directory.  Perhaps it should be loaded with
 * the "basic" user pref files instead of here.  This may allow bypassing
 * of some of the "security" compilation options.  XXX XXX XXX XXX XXX
 */
static void process_some_user_pref_files(void)
{
	char buf[1024];

#ifdef ALLOW_PREF_IN_HOME
#ifdef SET_UID

	char *homedir;

#endif /* SET_UID */
#endif /* ALLOW_PREF_IN_HOME */

	/* Process the "user.prf" file */
	(void)process_pref_file("user.prf");

	/* Get the "PLAYER.prf" filename */
	sprintf(buf, "%s.prf", op_ptr->base_name);

	/* Process the "PLAYER.prf" file */
	(void)process_pref_file(buf);

#ifdef ALLOW_PREF_IN_HOME
#ifdef SET_UID

	/* Process the "~/.angband.prf" file */
	if ((homedir = getenv("HOME")))
	{
		/* Get the ".angband.prf" filename */
		path_build(buf, 1024, homedir, ".angband.prf");

		/* Process the ".angband.prf" file */
		(void)process_pref_file(buf);
	}

#endif /* SET_UID */
#endif /* ALLOW_PREF_IN_HOME */
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

	/* Forbid resizing */
	Term->fixed_shape = TRUE;


	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(0);

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

		/* Start in town */
		p_ptr->depth = 0;

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);

#ifdef GJW_RANDART

		/* Hack -- seed for random artifacts */
		if (reseed_artifacts) seed_randart = rand_int(0x10000000);

		/* Hack -- clear artifact memory */
		if (reseed_artifacts)
		{
			int i;

			for (i = 0;i<z_info->a_max;i++)
			{
				object_lore *n_ptr = &a_list[i];

				n_ptr->can_flags1 = 0x0L;
				n_ptr->can_flags2 = 0x0L;
				n_ptr->can_flags3 = 0x0L;

				n_ptr->may_flags1 = 0x0L;
				n_ptr->may_flags2 = 0x0L;
				n_ptr->may_flags3 = 0x0L;

				n_ptr->not_flags1 = 0x0L;
				n_ptr->not_flags2 = 0x0L;
				n_ptr->not_flags3 = 0x0L;
			}

		}

#endif

		/* Roll up a new character */
		player_birth();

                /* Random artifacts */
                do_randart(seed_randart, TRUE);

		/* Hack -- enter the world */
		turn = 1;

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
	Term_fresh();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;


	/* Flavor the objects */
	flavor_init();

	/* Mark the fixed monsters as quests */
	if (adult_campaign)
	{
		int i;

		for (i = 0; i < z_info->t_max; i++)
		{
			int ii;

			for (ii = 0; ii < MAX_DUNGEON_ZONES;ii++)
			{
				int guard = t_info[i].zone[ii].guard;

				if (guard) r_info[guard].flags1 |= RF1_GUARDIAN;
			}
		}
	}

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


	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

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
		target_set_monster(0);

		/* Cancel the health bar */
		health_track(0);


		/* Forget the view */
		forget_view();


		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;


		/* Erase the old cave */
		wipe_o_list();
		wipe_m_list();


		/* XXX XXX XXX */
		msg_print(NULL);

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
				strcpy(p_ptr->died_from, "Cheating death");

				/* New depth */
				p_ptr->depth = min_depth(p_ptr->dungeon);

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Make a new level */
		generate_cave();
	}

	/* Close stuff */
	close_game();

	/* Quit */
	quit(NULL);
}
