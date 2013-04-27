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
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static cptr value_check_aux1(object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr) || o_ptr->art_name)
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
	if (broken_p(o_ptr)) return "broken";

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
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return "cursed";

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return "broken";

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr) || o_ptr->art_name) return "good";

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return "good";

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return "good";

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return "good";

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
 *   Class 4 = Ranger  --> slow but heavy  (changed!)
 *   Class 5 = Paladin --> slow but heavy
 */
static void sense_inventory(void)
{
	int		i;

	int		plev = p_ptr->level;

	bool	heavy = FALSE;

	cptr	feel;

	object_type *o_ptr;

	char o_name[80];


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* Analyze the class */
	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
		{
			/* Good sensing */
			if (0 != rand_int(9000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_MAGE: case CLASS_HIGH_MAGE:
		{
			/* Very bad (light) sensing */
			if (0 != rand_int(240000L / (plev + 5))) return;

			/* Done */
			break;
		}

		case CLASS_PRIEST:
		{
			/* Good (light) sensing */
			if (0 != rand_int(10000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case CLASS_ROGUE:
		{
			/* Okay sensing */
			if (0 != rand_int(20000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_RANGER:
		{

			/* Bad sensing */
			if (0 != rand_int(95000L / (plev * plev + 40))) return;

			/* Changed! */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_PALADIN:
		{
			/* Bad sensing */
			if (0 != rand_int(77777L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_WARRIOR_MAGE:
		{

			/* Bad sensing */
			if (0 != rand_int(75000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case CLASS_MINDCRAFTER:
		{

			/* Bad sensing */
			if (0 != rand_int(55000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}

		case CLASS_CHAOS_WARRIOR:
		{

			/* Bad sensing */
			if (0 != rand_int(80000L / (plev * plev + 40))) return;

			/* Changed! */
			heavy = TRUE;

			/* Done */
			break;
		}

		case CLASS_MONK:
		{
			/* Okay sensing */
			if (0 != rand_int(20000L / (plev * plev + 40))) return;

			/* Done */
			break;
		}
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		bool okay = FALSE;

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

		/* We know about it already, do not tell us again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Message (equipment) */
		if (i >= INVEN_WIELD)
		{
			msg_format("You feel the %s (%c) you are %s %s %s...",
			           o_name, index_to_label(i), describe_use(i),
			           ((o_ptr->number == 1) ? "is" : "are"), feel);
		}

		/* Message (inventory) */
		else
		{
			msg_format("You feel the %s (%c) in your pack %s %s...",
			           o_name, index_to_label(i),
			           ((o_ptr->number == 1) ? "is" : "are"), feel);
		}

		/* We have "felt" it */
		o_ptr->ident |= (IDENT_SENSE);

		/* Inscribe it textually */
		if (!o_ptr->note) o_ptr->note = quark_add(feel);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}

/*
 * Go to any level (ripped off from wiz_jump)
 */

static void pattern_teleport(void)
{
	/* Ask for level */
	if (get_check("Teleport level? "))
	{
		char	ppp[80];

		char	tmp_val[160];

		/* Prompt */
		sprintf(ppp, "Teleport to level (0-%d): ", 99);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->depth);

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
	if (command_arg < 0) command_arg = 0;

	/* Paranoia */
	if (command_arg > 99) command_arg = 99;

	/* Accept request */
	msg_format("You teleport to dungeon level %d.", command_arg);

	/* Change level */
	p_ptr->depth = command_arg;
	new_level_flag = TRUE;

	if (autosave_l)
	{
		is_autosave = TRUE;
		msg_print("Autosaving the game...");
		do_cmd_save_game();
		is_autosave = FALSE;
	}
}


static void wreck_the_pattern(void)
{
	int to_ruin, r_y, r_x;
	int py = p_ptr->py;
	int px = p_ptr->px;

	if (cave_feat[py][px] == FEAT_PATTERN_XTRA2)
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
		if ((cave_feat[r_y][r_x] >= FEAT_PATTERN_START)
			 && (cave_feat[r_y][r_x] < FEAT_PATTERN_XTRA2))
		{
			draw_grid_feat(r_y, r_x, FEAT_PATTERN_XTRA2);
		}
	}

	draw_grid_feat(py, px, FEAT_PATTERN_XTRA2);
}


/* Returns TRUE if we are on the Pattern... */
static bool pattern_effect(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	if ((cave_feat[py][px] < FEAT_PATTERN_START)
		 || (cave_feat[py][px] > FEAT_PATTERN_XTRA2))
	return FALSE;

	if ((p_ptr->prace == RACE_AMBERITE) && (p_ptr->cut>0) && (randint(10)==1))
	{
		wreck_the_pattern();
	}

	if (cave_feat[py][px] == FEAT_PATTERN_END)
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
		draw_grid_feat(py, px, FEAT_PATTERN_OLD);
		msg_print("This section of the Pattern looks less powerful.");
	}


	/*
	 * We could make the healing effect of the
	 * Pattern center one-time only to avoid various kinds
	 * of abuse, like luring the win monster into fighting you
	 * in the middle of the pattern...
	 */

	else if (cave_feat[py][px] == FEAT_PATTERN_OLD)
	{
		/* No effect */
	}
	else if (cave_feat[py][px] == FEAT_PATTERN_XTRA1)
	{
		pattern_teleport();
	}
	else if (cave_feat[py][px] == FEAT_PATTERN_XTRA2)
	{
		if (!(p_ptr->invuln))
		take_hit(200, "walking the corrupted Pattern");
	}

	else
	{
		if ((p_ptr->prace == RACE_AMBERITE) && (randint(2)!=1))
			return TRUE;
		else if (!(p_ptr->invuln))
			take_hit(damroll(1,3), "walking the Pattern");
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
	p_ptr->chp += new_chp >> 16;   /* div 65536 */

	/* check for overflow */
	if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
	new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;	/* mod 65536 */
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
		p_ptr->window |= (PW_PLAYER);
	}
}


/*
 * Regenerate mana points				-RAK-
 */
static void regenmana(int percent)
{
	s32b        new_mana, new_mana_frac;
	int                   old_csp;

	old_csp = p_ptr->csp;
	new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
	p_ptr->csp += new_mana >> 16;	/* div 65536 */
	/* check for overflow */
	if ((p_ptr->csp < 0) && (old_csp > 0))
	{
		p_ptr->csp = MAX_SHORT;
	}
	new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;	/* mod 65536 */
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
 * bad (Poison, Weakness etc) or 'useless' (Slime Mold Juice, etc).
 */
bool psychometry(void)
{
	int                     item;

	object_type             *o_ptr;

	char            o_name[80];
        cptr            feel;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Meditate on which item? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing appropriate.");
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

	/* It is fully known, no information needed */
	if ((object_known_p(o_ptr)) || (o_ptr->ident & IDENT_SENSE))
	{
		msg_print("You cannot find out anything more about that.");
		return TRUE;
	}

	/* Check for a feeling */
	feel = value_check_aux1(o_ptr);

	/* Get an object description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Skip non-feelings */
	if (!feel)
	{
		msg_format("You do not perceive anything unusual about the %s.", o_name);
		return TRUE;
	}

	msg_format("You feel that the %s %s %s...",
	    o_name, ((o_ptr->number == 1) ? "is" : "are"), feel);

	/* We have "felt" it */
	o_ptr->ident |= (IDENT_SENSE);

	/* Inscribe it textually */
	if (!o_ptr->note) o_ptr->note = quark_add(feel);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Something happened */
	return (TRUE);
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[80];

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
			/* Describe (briefly) */
			object_desc(o_name, o_ptr, FALSE, 0);

			/* Notify the player */
			msg_format("Your %s %s recharged.", o_name, (o_ptr->number > 1) ? "are" : "is");

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
	int x, y, i, j;

	int regen_amount;
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *o_ptr;
	u32b f1 = 0 , f2 = 0 , f3 = 0;


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
				alive = FALSE;
			}
		}
	}

	/*** Attempt timed autosave ***/
	if (autosave_t && autosave_freq)
	{
		if (!(turn % ((s32b) autosave_freq * 10 )))
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}
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
				msg_print("The sun has risen.  ");

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Assume lit */
						cave_info[y][x] |= CAVE_GLOW;

						/* Hack -- Memorize lit grids if allowed */
						if (view_perma_grids) cave_info[y][x] |= (CAVE_MARK);

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

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Darken "boring" features */
						if (cave_feat[y][x] <= FEAT_INVIS)
						{
							/* Forget the grid */
							cave_info[y][x] &= ~(CAVE_GLOW | CAVE_MARK);

							/* Hack -- Notice spot */
							note_spot(y, x);
						}
					}
				}
			}

			/* Update the monsters and view */
			p_ptr->update |= (PU_MONSTERS | PU_UN_VIEW | PU_VIEW);

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

				/* Shuffle a random shop */
				store_shuffle(rand_int(MAX_STORES));
			}

			/* Message */
			if (cheat_xtra) msg_print("Done.");
		}
	}


	/*** Process special stuff ***/
	if (crusher_n)
	{
		bool known = FALSE;

		/* Check all crushers */
		for (i = 0; i < crusher_n; i++)
		{
			/* Find crushers that need updating */
			if (turn - crusher_t[i] >= CRUSHER_TURNS)
			{
				int y = crusher_y[i];
				int x = crusher_x[i];

				int flg = PROJECT_ITEM | PROJECT_KILL | PROJECT_HIDE | PROJECT_JUMP;

				/* Note visible crushers */
				if (player_has_los_bold(y, x)) known = TRUE;

				/* Lower raised crushers */
				if (cave_feat[y][x] == FEAT_CRUSHER_UP)
				{
					/* Hurt stuff badly */
					project(0, 0, y, x, damroll(30, 6), GF_CRUSHER, flg);

					draw_grid_feat(y, x, FEAT_CRUSHER_DOWN);
				}
				/* Raise lowered crushers */
				else
				{
					draw_grid_feat(y, x, FEAT_CRUSHER_UP);
				}

				/* Forget it */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Notice it */
				note_spot(y, x);

				/* Redraw it */
				lite_spot(y, x);

				/* Note the last change */
				crusher_t[i] = turn;
			}
		}

		/* Take note */
		if (known)
		{
			/* Update some things */
			p_ptr->update |= (PU_VIEW | PU_MONSTERS);

			/* Disturbing */
			disturb(1, 0);

			/* Message */
			msg_print("Thump!");
		}
	}


	/*** Process the monsters ***/

	/* Check for creature generation */
	if (rand_int(MAX_M_ALLOC_CHANCE) == 0)
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
		if ((!p_ptr->depth)
			 && (!(p_ptr->resist_lite)) && !(p_ptr->invuln)
			 && (!((turn / ((10L * TOWN_DAWN)/2)) % 2)))
		{
			/* Take damage */
			msg_print("The sun's rays scorch your undead flesh!");
			take_hit(1, "sunlight");
			cave_no_regen = TRUE;
		}

		if (!(p_ptr->resist_lite) && !(p_ptr->invuln))
		{
			/* Loop through all wielded items */
			for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
			{
				o_ptr = &inventory[i];

				/* Skip empty slots */
				if (!o_ptr->k_idx) continue;

				/* Extract the flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Does this item glow? */
				if ((f3 & TR3_LITE) || (i == INVEN_LITE))
				{
					char o_name[80];
					char ouch[80];

					/* Get an object description */
					object_desc(o_name, o_ptr, FALSE, 0);

					msg_format("The %s scorches your undead flesh!", o_name);

					cave_no_regen = TRUE;

					/* Get an object description */
					object_desc(o_name, o_ptr, TRUE, 0);

					sprintf(ouch, "wielding %s", o_name);

					take_hit(1, ouch);
				}
			}
		}
	}

	/* Spectres -- take damage when moving through walls */
	/*
	 * Added: ANYBODY takes damage if inside through walls
	 * without wraith form -- NOTE: Spectres will never be
	 * reduced below 0 hp by being inside a stone wall; others
	 * WILL BE!
	 */

	if (!cave_floor_bold(py, px))
	{
		int dam = damroll(30, 6);

		cave_no_regen = TRUE;

		if (!(p_ptr->invuln) &&
			 !(p_ptr->wraith_form) &&
			 ((p_ptr->chp > dam) || (p_ptr->prace != RACE_SPECTRE)))
		{
			cptr dam_desc;

			if (p_ptr->prace == RACE_SPECTRE)
			{
				msg_print("You are being disrupted!");
				dam_desc = "density";
			}
			else
			{
				msg_print("You are being crushed!");
				dam_desc = "solid rock";
			}

			take_hit(dam, dam_desc);
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
		if (!(p_ptr->invuln)) take_hit(i, "starvation");
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


	/* Searching or Resting */
	if (p_ptr->searching || resting)
	{
		regen_amount = regen_amount * 2;
	}

	if (total_friends)
	{
		int upkeep_divider = 20;

		if (p_ptr->pclass == CLASS_MAGE)
			upkeep_divider = 15;
		else if (p_ptr->pclass == CLASS_HIGH_MAGE)
			upkeep_divider = 12;

		if (total_friends > 1 + (p_ptr->level / (upkeep_divider)))
		{
			upkeep_factor = (total_friend_levels);

			if (upkeep_factor > 100) upkeep_factor = 100;
			else if (upkeep_factor < 10) upkeep_factor = 10;
		}
	}

	/* Cancel resting if no longer useful */
	if ((resting < 0) && (p_ptr->chp == p_ptr->mhp) && (upkeep_factor >= 90))
	{
		disturb(0, 0);
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
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

	/* Special floor -- Pattern, in a wall -- yields no healing */
	if (cave_no_regen) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if ((p_ptr->chp < p_ptr->mhp) && !(cave_no_regen))
	{
		if ((cave_feat[py][px] < FEAT_PATTERN_END) &&
		    (cave_feat[py][px] >= FEAT_PATTERN_START))
		{
			regenhp(regen_amount / 5); /* Hmmm. this should never happen? */
		}
		else
		{
			regenhp(regen_amount);
		}
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
		(void)set_wraith_form(p_ptr->wraith_form - 1);
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


	/*** Process mutation effects ***/
	if (p_ptr->muta2)
	{
		if (p_ptr->muta2 & MUT2_BERS_RAGE)
		{
			int i = 0;

			if (!rand_int(3000)) i = randint(p_ptr->level) + 10;
			else if (!rand_int(1000)) i = randint(p_ptr->level / 10) + 1;

			if (i)
			{
				disturb(0,0);
				msg_print("RAAAAGHH!");
				msg_print("A fit of rage comes over you!");
				set_shero(p_ptr->shero + i);
			}
		}

		if ((p_ptr->muta2 & MUT2_COWARDICE) &&
			 !(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
		{
			int i = 0;

			if (!rand_int(3000)) i = rand_int(26) + 13;
			else if (!rand_int(1000)) i = randint(4);

			if (i)
			{
				disturb(0,0);
				msg_print("It's so dark... so scary!");
				p_ptr->redraw |= PR_AFRAID;
				set_afraid(p_ptr->afraid + i);
			}
		}

		if ((p_ptr->muta2 & MUT2_RTELEPORT) && !(p_ptr->resist_nexus) &&
			 !(p_ptr->muta1 & MUT1_VTELEPORT) && !(p_ptr->anti_tele))
		{
			int i = 0;

			if (!rand_int(5000)) i = 40;
			else if (!rand_int(2000)) i = 4;

			if (i)
			{
				disturb(0,0);

				/* Teleport player */
				msg_print("Your position suddenly seems very uncertain...");
				msg_print(NULL);
				teleport_player(i);
			}
		}

		if ((p_ptr->muta2 & MUT2_ALCOHOL) && (randint(6400)==321))
		{
			if (!(p_ptr->resist_chaos || p_ptr->resist_conf))
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				msg_print("A SSSCHtupor cOmESH over yOu... *HIC*!");

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

		if ((p_ptr->muta2 & MUT2_HALLU) && !(p_ptr->resist_chaos))
		{
			int i = 0;

			if (!rand_int(6400)) i = rand_int(50) + 20;
			else if (!rand_int(3000)) i = rand_int(6) + 2;

			if (i)
			{
				disturb(0,0);
				p_ptr->redraw |= PR_EXTRA;
				(void)set_image(p_ptr->image + i);
			}
		}

		if (p_ptr->muta2 & MUT2_FLATULENT)
		{
			int i = 0;
			int r = 0;

			if (!rand_int(3000))
			{
				i = p_ptr->level;
				r = 3;
			}
			else if (!rand_int(1000))
			{
				i = p_ptr->level / 10;
				r = 1;
			}

			if (i)
			{
				disturb(0,0);
				msg_print("BRRAAAP! Oops.");
				msg_print(NULL);
				fire_ball(GF_POIS, 0, i, r);
			}
		}

		if ((p_ptr->muta2 & MUT2_PROD_MANA) && !(p_ptr->anti_magic))
		{
			int i = 0;
			int r = 0;
			int dire = 0;

			if (!rand_int(9000))
			{
				i = p_ptr->level * 2;
				r = 3;
			}
			else if (!rand_int(4000))
			{
				i = p_ptr->level / 5;
				r = 1;
			}

			if (i)
			{
				disturb(0,0);
				msg_print("Magical energy flows through you! You must release it!");
				flush();
				msg_print(NULL);
				(void)get_hack_dir(&dire);
				fire_ball(GF_MANA, dire, i, r);
			}
		}

		if ((p_ptr->muta2 & MUT2_ATT_ANIMAL) &&
			!p_ptr->anti_magic && !rand_int(7000))
		{
			bool d_summon = FALSE;

			d_summon = summon_specific(py, px, p_ptr->depth + 5, SUMMON_ANIMAL, TRUE, !rand_int(3));

			if (d_summon)
			{
				msg_print("You have attracted an animal!");
				disturb(0, 0);
			}
		}

		if ((p_ptr->muta2 & MUT2_ATT_DEMON) &&
			 (!(p_ptr->anti_magic)) && (randint(6666)==666))
		{
			bool d_summon = FALSE;

			d_summon = summon_specific(py, px, p_ptr->depth + 5, SUMMON_DEMON, TRUE, !rand_int(6));

			if (d_summon)
			{
				msg_print("You have attracted a demon!");
				disturb(0,0);
			}
		}

		if ((p_ptr->muta2 & MUT2_ATT_DRAGON) &&
			!p_ptr->anti_magic && !rand_int(3000))
		{
			bool d_summon = FALSE;

			d_summon = summon_specific(py, px, p_ptr->depth + 5, SUMMON_DRAGON, TRUE, !rand_int(5));

			if (d_summon)
			{
				msg_print("You have attracted a dragon!");
				disturb(0,0);
			}
		}

		if (p_ptr->muta2 & MUT2_WOUND)
		{
			int i = 0;

			if (!rand_int(3000)) i = rand_int(20) + 10;
			else if (!rand_int(1000)) i = randint(3);

			if (i)
			{
				disturb(0,0);
				msg_print("Your skin rips open!  Ouch!");
				set_cut(p_ptr->cut + i);
			}

		}
		if (p_ptr->muta2 & MUT2_SPEED_FLUX)
		{
			int i = 0;

			if (!rand_int(6000)) i = rand_int(30) + 10;
			else if (!rand_int(3000)) i = randint(4);

			if (i)
			{
				disturb(0, 0);

				if (!rand_int(2))
				{
					msg_print("You feel less energetic.");
					if (p_ptr->fast > 0)
					{
						set_fast(0);
					}
					else
					{
						set_slow(p_ptr->slow + i);
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
						set_fast(p_ptr->fast + i);
					}
				}
			}
		}

		if (p_ptr->muta2 & MUT2_DISPEL_ALL)
		{
			int i = 0;
			int j = 0;

			if (!rand_int(9000))
			{
				i = 150;
				j = rand_int(10) + 10;
			}
			else if (!rand_int(4000))
			{
				i = 15;
				j = randint(2);
			}

			if (i)
			{
				disturb(0, 0);
				msg_print("You feel a dark power take hold of you.");
				dispel_monsters(i);
				set_stun(p_ptr->stun + j);
				if (!p_ptr->depth)
				{
					msg_print("You see one of the shopkeepers running for the hills!");
					store_shuffle(rand_int(MAX_STORES));
				}
			}
		}

		if ((p_ptr->muta2 & MUT2_EAT_LIGHT) && !rand_int(3000))
		{
			object_type *o_ptr;

			msg_print("A shadow passes over you.");

			/* Absorb light from the current possition */
			if (cave_info[py][px] & CAVE_GLOW)
			{
				hp_player(10);
			}

			o_ptr = &inventory[INVEN_LITE];

			/* Absorb some fuel in the current lite */
			if (o_ptr->tval == TV_LITE)
			{
				/* Use some fuel (except on artifacts) */
				if (!artifact_p(o_ptr) && (o_ptr->pval > 0))
				{
					/* Heal the player a bit */
					hp_player(o_ptr->pval / 20);

					/* Decrease life-span of lite */
					o_ptr->pval /= 2;

					msg_print("You absorb energy from your light!");
				}
			}

			/*
			 * Unlite the area (radius 10) around player and
			 * do 50 points damage to every affected monster
			 */
			unlite_area(50, 10);
		}

		if ((p_ptr->muta2 & MUT2_RAW_CHAOS) && !p_ptr->anti_magic)
		{
			int i = 0;
			int r = 0;

			if (!rand_int(8000))
			{
				i = p_ptr->level;
				r = 8;
			}
			else if (!rand_int(4000))
			{
				i = p_ptr->level / 10;
				r = 2;
			}

			if (i)
			{
				disturb(0, 0);
				msg_print("You feel the world warping around you!");
				msg_print(NULL);
				fire_ball(GF_CHAOS, 0, i, r);
			}
		}

		if ((p_ptr->muta2 & MUT2_WRAITH) && !p_ptr->anti_magic)
		{
			int i = 0;

			if (!rand_int(3000))
			{
				i = rand_int(p_ptr->level / 2) + (p_ptr->level / 2);
			}
			else if (!rand_int(1000))
			{
				i = rand_int(p_ptr->level / 20) + (p_ptr->level / 20);
			}

			if (i)
			{
				disturb(0,0);
				msg_print("You feel insubstantial!");
				msg_print(NULL);
				set_wraith_form(p_ptr->wraith_form + i);
			}
		}
		if ((p_ptr->muta2 & MUT2_POLY_WOUND) && !rand_int(3000))
		{
			do_poly_wounds();
		}
		if ((p_ptr->muta2 & MUT2_WASTING) && !rand_int(3000))
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
				disturb(0, 0);
				msg_print("You are wasting away!");
				msg_print(NULL);
				(void)dec_stat(which_stat, randint(6) + 6, randint(3) == 1);
			}
		}
		if ((p_ptr->muta2 & MUT2_WEIRD_MIND) && !p_ptr->anti_magic)
		{
			int i = 0;

			if (!rand_int(3000)) i = p_ptr->level;
			else if (!rand_int(1000)) i = p_ptr->level / 10;

			if (i)
			{
				if (p_ptr->tim_esp > 0)
				{
					msg_print("Your mind feels cloudy!");
					set_tim_esp(0);
				}
				else
				{
					msg_print("Your mind expands!");
					set_tim_esp(i);
				}
			}
		}
		if ((p_ptr->muta2 & MUT2_NAUSEA) && !p_ptr->slow_digest &&
			!rand_int(9000))
		{
			disturb(0,0);
			msg_print("Your stomach roils, and you lose your lunch!");
			msg_print(NULL);
			set_food(PY_FOOD_WEAK);
		}

		if ((p_ptr->muta2 & MUT2_WALK_SHAD) &&
			!p_ptr->anti_magic && !rand_int(12000))
		{
			new_level_flag = TRUE;
		}

		if ((p_ptr->muta2 & MUT2_WARNING) && !rand_int(1000))
		{
			int danger_amount = 0;
			int monster;

			for (monster = 0; monster < m_max; monster++)
			{
				monster_type    *m_ptr = &m_list[monster];
				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Paranoia -- Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				if (r_ptr->level >= p_ptr->level)
				{
					danger_amount += r_ptr->level - p_ptr->level + 1;
				}
			}

			disturb(0, 0);

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
		if ((p_ptr->muta2 & MUT2_INVULN) && !p_ptr->anti_magic)
		{
			int i = 0;

			if (!rand_int(5000)) i = rand_int(10) + 10;
			else if (!rand_int(2000)) i = randint(2);

			if (i)
			{
				disturb(0, 0);
				msg_print("You feel invincible!");
				msg_print(NULL);
				(void)set_invuln(p_ptr->invuln + i);
			}
		}
		if ((p_ptr->muta2 & MUT2_SP_TO_HP) && !rand_int(2000))
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
		if ((p_ptr->muta2 & MUT2_HP_TO_SP) && !p_ptr->anti_magic &&
			!rand_int(4000))
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
		if ((p_ptr->muta2 & MUT2_DISARM) && !rand_int(10000))
		{
			object_type *o_ptr;

			disturb(0, 0);
			msg_print("You trip over your own feet!");
			take_hit(randint(p_ptr->wt / 6), "tripping");

			msg_print(NULL);
			o_ptr = &inventory[INVEN_WIELD];
			if (o_ptr->k_idx)
			{
				msg_print("You drop your weapon!");
				inven_drop(INVEN_WIELD,1);
			}
		}
	}


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

	/* Rarely, take damage from the Jewel of Judgement */
	if ((randint(999)==1) && !(p_ptr->anti_magic))
	{
		if ((inventory[INVEN_LITE].tval) && !(p_ptr->invuln)
		    && (inventory[INVEN_LITE].sval == SV_LITE_THRAIN))
		{
			msg_print("The Jewel of Judgement drains life from you!");
			take_hit(MIN(p_ptr->level, 50), "the Jewel of Judgement");
		}
	}


	/* Process equipment */
	for (j = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		object_flags(o_ptr, &f1, &f2, &f3);

		/* TY Curse */
		if ((f3 & TR3_TY_CURSE) && !rand_int(TY_CURSE_CHANCE))
		{
			activate_ty_curse();
		}

		/* Make a chainsword noise */
		if ((o_ptr->name1 == ART_ELVAGIL) && !rand_int(CHAINSWORD_NOISE))
		{
			char buf[1024];

			if (!get_rnd_line("chainswd.txt", "", buf))
			{
				msg_print(buf);
			}
		}

		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 *can actually be useful!
		 */

		if ((f3 & TR3_TELEPORT) && !rand_int(100))
		{
			if ((o_ptr->ident & IDENT_CURSED) && !(p_ptr->anti_tele))
			{
				disturb(0,0);

				/* Teleport player */
				teleport_player(40);
			}
			else
			{
				if ((!disturb_other) ||
					 (o_ptr->note && (strchr(quark_str(o_ptr->note),'.'))))
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


		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!o_ptr->timeout)
			{
				recharged_notice(o_ptr);
				j++;
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
				recharged_notice(o_ptr);
				j++;
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


	/*** Involuntary Movement ***/

	/* Delayed Word-of-Recall */
	if (p_ptr->word_recall)
	{
		/*
		 * HACK: Autosave BEFORE resetting the recall counter (rr9)
		 * The player is yanked up/down as soon as
		 * he loads the autosaved game.
		 */
		if (autosave_l && (p_ptr->word_recall == 1))
		{
			is_autosave = TRUE;
			msg_print("Autosaving the game...");
			do_cmd_save_game();
			is_autosave = FALSE;
		}

		/* Count down towards recall */
		p_ptr->word_recall--;

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Determine the level */
			if (p_ptr->depth)
			{
				msg_print("You are yanked upwards!");

				p_ptr->depth = 0;
				new_level_flag = TRUE;
			}
			else
			{
				msg_print("You are yanked downwards!");

				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth < 1) p_ptr->depth = 1;
				new_level_flag = TRUE;
			}

			/* Sound */
			sound(SOUND_TPLEVEL);
		}
	}
}



#ifdef ALLOW_WIZARD
/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (!p_ptr->noscore)
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


/*
 * Verify use of "debug" commands
 */
static bool enter_debug_mode(void)
{
	/* Ask first time */
	if (!p_ptr->noscore)
	{
		/* Mention effects */
		msg_print("The debug commands are for debugging and experimenting.");
		msg_print("The game will not be scored if you use debug commands.");
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


#if 0
/*
 * Verify use of "god" commands
 */
static bool enter_god_mode(void)
{
	/* Ask first time */
	if (!p_ptr->noscore)
	{
		/* Mention effects */
		msg_print("These commands are for experimenting and testing.");
		msg_print("The game will not be scored afterward, for obvious reasons.");
		msg_print(NULL);

		/* Verify request */
		if (!get_check("Are you REALLY sure you want to use these commands? "))
		{
			return (FALSE);
		}
	}

	/* *Mark* savefile */
	p_ptr->noscore = 0xFFFF;

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Declare the God Routines
 */
extern void do_cmd_god(void);
#endif


#endif /* ALLOW_WIZARD */


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
		msg_print("The borg commands are for debugging and experimenting.");
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

#endif /* ALLOW_BORG */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{
	/* Parse the command */
	switch (command_cmd)
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

#ifdef ALLOW_WIZARD
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

#ifdef ALLOW_GOD
		/* Special "god" commands */
		case KTRL('\\'):
		{
			/* Enter god mode */
			if (enter_god_mode())
			{
				do_cmd_god();

				/* Redraw "title" */
				p_ptr->redraw |= (PR_TITLE);
			}
			break;
		}
#endif

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

#endif /* ALLOW_BORG */



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

		/* Issue a pet command */
		case 'p':
		{
			do_cmd_pet();
			break;
		}

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

		/* Move (usually pick up things) */
		case ';':
		{
			do_cmd_walk(always_pickup);
			break;
		}

		/* Move (usually do not pick up) */
		case '-':
		{
			do_cmd_walk(!always_pickup);
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
			if (p_ptr->anti_magic)
			{
				cptr which_power = "magic";
				if (p_ptr->pclass == CLASS_MINDCRAFTER)
					which_power = "psionic powers";
				else if (mp_ptr->spell_book == TV_LIFE_BOOK)
					which_power = "prayer";

				msg_format("An anti-magic shell disrupts your %s!", which_power);

				energy_use = 0;
			}
			else
			{
				if (p_ptr->pclass == CLASS_MINDCRAFTER)
					do_cmd_mindcraft();
				else
					do_cmd_cast();
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
			do_cmd_help("help.hlp");
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

		/* Damage info */
		case KTRL('G'):
		{
			do_cmd_knowledge_damage();
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
			is_autosave = FALSE;
			do_cmd_save_game();
			break;
		}

#endif /* VERIFY_SAVEFILE */

		case KTRL('T'):
		{
			do_cmd_time();
			break;
		}

		/*
		 * Show quest status
		 * Heino Vander Sanden
		 */
		case KTRL('Q'):
		{
			if (is_quest(p_ptr->depth))
			{
				print_quest_message();
			}
			else
			{
				msg_print("You have no quests.");
			}
			break;
		}

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

		/* Hack -- Unknown command */
		default:
		{
			char buf[1024];

			if (!rand_int(2) && !get_rnd_line("error.txt", "", buf))
			{
				msg_print(buf);
			}
			else
			{
				prt("Type '?' for help.", 0, 0);
			}
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
	int i;
	int py = p_ptr->py;
	int px = p_ptr->px;

	/*** Apply energy ***/

	if (hack_mutation)
	{
		msg_print("You feel different!");
		(void)gain_random_mutation(0);
		hack_mutation = FALSE;
	}

	/* Give the player some energy */
	p_ptr->energy += extract_energy[p_ptr->pspeed];

	/* No turn yet */
	if (p_ptr->energy < 100) return;


	/*** Check for interupts ***/

	/* Complete resting */
	if (resting < 0)
	{
		/* Basic resting */
		if (resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
                (p_ptr->csp >= p_ptr->msp))
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

	/* Repeat until out of energy */
	while (p_ptr->energy >= 100)
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
		move_cursor_relative(py, px);

		/* Refresh (optional) */
		if (fresh_before) Term_fresh();


		/* Hack -- Pack Overflow */
		if (inventory[INVEN_PACK].k_idx)
		{
			int item = INVEN_PACK;

			char o_name[80];

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

			/* Redraw stuff (if needed) */
			if (p_ptr->window) window_stuff();
		}


		/* Hack -- cancel "lurking browse mode" */
		if (!command_new) command_see = FALSE;


		/* Assume free turn */
		energy_use = 0;


		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= 100))
		{
			/* Take a turn */
			energy_use = 100;
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
			energy_use = 100;
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


			/* Hack -- constant hallucination */
			if (p_ptr->image) p_ptr->redraw |= (PR_MAP);

			if (p_ptr->image) p_ptr->window |= (PW_INVEN | PW_EQUIP);


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
	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Reset various flags */
	new_level_flag = FALSE;
	hack_mind = FALSE;

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


	/* Track maximum player level */
	if (p_ptr->max_level < p_ptr->level)
	{
		p_ptr->max_level = p_ptr->level;
	}


	/* Track maximum dungeon level */
	if (p_ptr->max_depth < p_ptr->depth)
	{
		p_ptr->max_depth = p_ptr->depth;
	}

	/* Paranoia -- No stairs down from Quest */
	if (is_quest(p_ptr->depth)) create_down_stair = FALSE;

	/* Paranoia -- no stairs from town */
	if (!p_ptr->depth) create_down_stair = create_up_stair = FALSE;

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
				draw_grid_feat(py, px, FEAT_MORE);
			}
			else
			{
				draw_grid_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		create_down_stair = create_up_stair = FALSE;
	}


	/* Verify the panel */
	verify_panel();

	/* Validate the panel */
	if (center_player)
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
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

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

	/* Redraw stuff */
	window_stuff();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_FLOW | PU_DISTANCE);

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

	/* Window stuff */
	window_stuff();

	/* Refresh */
	Term_fresh();


	/* Announce (or repeat) the feeling */
	if (p_ptr->depth) do_cmd_feeling();


	/* Hack -- notice death or departure */
	if (!alive || death || new_level_flag) return;

	/* Print quest message if appropriate */
	if (is_quest(p_ptr->depth)) quest_discovery();

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

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


		/* Process the player */
		process_player();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

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

		/* Redraw stuff */
		if (p_ptr->window) window_stuff();

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

		/* Window stuff */
		if (p_ptr->window) window_stuff();

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
 * Load some "user pref files"
 */
static void load_all_pref_files(void)
{
	char buf[1024];

	/* Access the "race" pref file */
	sprintf(buf, "%s.prf", rp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "class" pref file */
	sprintf(buf, "%s.prf", cp_ptr->title);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "realm 1" pref file */
	if (realm_names[p_ptr->realm1]!="no magic")
		if (realm_names[p_ptr->realm1]!="unknown")
		{
			sprintf(buf, "%s.prf",realm_names[p_ptr->realm1]);

			/* Process that file */
			process_pref_file(buf);
		}

		/* Access the "realm 2" pref file */
		if (realm_names[p_ptr->realm2]!="no magic")
		if (realm_names[p_ptr->realm2]!="unknown")
		{
			sprintf(buf, "%s.prf",realm_names[p_ptr->realm2]);

			/* Process that file */
			process_pref_file(buf);
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

	hack_mutation = FALSE;

	/* Hack -- Character is "icky" */
	character_icky = TRUE;


	/* Hack -- turn off the cursor */
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

	/* Process old character */
	if (!new_game)
	{
		/* Process the player name */
		process_player_name(FALSE);
	}

	rand_init();

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

		/* Roll up a new character */
		player_birth();

		/* Hack -- enter the world */
		turn = 1;
	}


	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();


#ifdef ALLOW_WIZARD
	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;
#endif

	/* Flavor the objects */
	flavor_init();

	/* Reset the visual mappings */
	reset_visuals();

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER | PW_MONSTER);

	/* Window stuff */
	window_stuff();


	/* Load the "pref" files */
	load_all_pref_files();

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

	msg_flag = FALSE;

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


		/* Forget the view */
		forget_view();


		/* Handle "quit and save" */
		if (!alive && !death) break;


		/* Erase the old cave */
		wipe_o_list();
		wipe_m_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (alive && death)
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
				(void)strcpy(died_from, "Cheating death");

				/* Teleport to town */
				new_level_flag = TRUE;

				/* Go to town */
				p_ptr->depth = 0;

				/* Do not die */
				death = FALSE;
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


