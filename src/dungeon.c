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

#define CHAINSWORD_NOISE 100

/*
 * I created this when a bug misplaced my character and the game wasn't able
 * to load it again.. very frustrating.
 * So this hack will generate a new level without calling dungeon(), and
 * then the normal behavior will apply.
 */
/* #define SAVE_HACK */
#ifdef SAVE_HACK
bool save_hack = TRUE;
#endif

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
 * Can a player be resurrected?
 */
static bool granted_resurrection(void)
{  
	return FALSE;
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

	int		plev = p_ptr->lev;

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
                case CLASS_MARKSMAN:
		{
			/* Good sensing */
			if (0 != rand_int(9000L / (plev * plev + 40))) return;

			/* Heavy sensing */
			heavy = TRUE;

			/* Done */
			break;
		}

                case CLASS_MAGE:
                case CLASS_HIGH_MAGE:
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
			case TV_AMMO:
			case TV_RANGED:
			case TV_DIGGING:
			case TV_WEAPON:
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
	if (command_arg < 0) command_arg = 0;

	/* Paranoia */
	if (command_arg > 99) command_arg = 99;

	/* Accept request */
	msg_format("You teleport to dungeon level %d.", command_arg);

	if (autosave_l)
	{
		is_autosave = TRUE;
		msg_print("Autosaving the game...");
		do_cmd_save_game();
		is_autosave = FALSE;
	}

	/* Change level */
	dun_level = command_arg;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/* Returns TRUE if we are on the Straight Road... */
static bool pattern_effect(void)
{
	if ((cave[py][px].feat < FEAT_PATTERN_START)
	    || (cave[py][px].feat > FEAT_PATTERN_XTRA2))
	return FALSE;

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
                msg_print("This section of the Straight Road looks less powerful.");
	}


	/*
	 * We could make the healing effect of the
	 * Pattern center one-time only to avoid various kinds
	 * of abuse, like luring the win monster into fighting you
	 * in the middle of the pattern...
	 */

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
                take_hit(200, "walking the corrupted Straight Road");
	}

	else
	{
                        take_hit(damroll(1,3), "walking the Straight Road");
	}

	return TRUE;
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
			if (o_ptr->number > 1) 
				msg_format("Your %s are recharged.", o_name);
			else msg_format("Your %s is recharged.", o_name);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = strchr(s + 1, '!');
	}
}



/*
 * Regenerate hit points				-RAK-
 */
static void regenhp(int percent)
{
	s32b regenamt;
	s32b old_chp;

	old_chp = p_ptr->chp;
        
        /* Regenerate by (percent)% */
	regenamt = multiply_divide(p_ptr->chp, percent, 100);
	if (regenamt < 1) regenamt = 1;
	p_ptr->chp += regenamt;

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
	s32b regenamt;
	s32b old_csp;

	old_csp = p_ptr->csp;

	/* Regenerate by (percent)% */
	regenamt = multiply_divide(p_ptr->csp, percent, 100);

        /* Wielding a rod with a high skill? */
        if (rod_has() && p_ptr->skill[17] >= 30)
        {
                regenamt *= 2;
                regenamt += 1;
        }

	if (regenamt < 1) regenamt = 1;
	p_ptr->csp += regenamt;

        /* Magic Blood high-mage ability! */
        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 1] >= 1)
        {
		p_ptr->csp += multiply_divide(p_ptr->csp, p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 1], 100);
                p_ptr->csp += p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 1] * 5;
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

	cptr q, s;


	/* Get an item */
	q = "Meditate on which item? ";
	s = "You have nothing appropriate.";
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
 * Does an object decay?
 */
bool decays(object_type *o_ptr)
{
        u32b f1, f2, f3, f4;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (f3 & TR3_DECAY) return TRUE;

	return FALSE;
}

bool is_recall = FALSE;

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
        int x, y, i, j, temp;
	int cursealloc;

	int regen_amount;
	bool cave_no_regen = FALSE;
	int upkeep_factor = 0;

	cave_type *c_ptr;

	object_type *o_ptr;
        object_kind *k_ptr;
        u32b f1 = 0 , f2 = 0 , f3 = 0, f4 = 0;


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

				/* Leaving */
				p_ptr->leaving = TRUE;
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


	/*** Handle the wilderness/town (sunshine) ***/

	/* While in town/wilderness */
	if (!dun_level)
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

				/* Hack -- Scan the town */
				for (y = 0; y < cur_hgt; y++)
				{
					for (x = 0; x < cur_wid; x++)
					{
						/* Get the cave grid */
						c_ptr = &cave[y][x];

						/* Darken "boring" features */
                                                if ((f_info[c_ptr->feat].flags1 & FF1_FLOOR) && !(f_info[c_ptr->feat].flags1 & FF1_REMEMBER))
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


	/*** Process the monsters ***/

	/* Check for creature generation. */

	/* Cursed players will meet more monsters. */
	cursealloc = p_ptr->cursed / 3;

	/* Never exceed a certain amount. */
	if (cursealloc > (d_info[dungeon_type].max_m_alloc_chance - 10)) cursealloc = (d_info[dungeon_type].max_m_alloc_chance - 10);

        if ((rand_int(d_info[dungeon_type].max_m_alloc_chance - cursealloc) == 0) && !(p_ptr->inside_quest))
	{
		/* Make a new monster */
		if (!special_flag) (void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/* Hack -- Check for creature regeneration */
	if (!(turn % 100)) regen_monsters();


	/*** Damage over Time ***/

	/* Take damage from poison */
	if ((p_ptr->poisoned))
	{
		/* Take damage */
		take_hit(1, "poison");
	}


	/* (Vampires) Take damage from sunlight */
        if ((p_ptr->prace == RACE_VAMPIRE))
	{
		if ((!dun_level)
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

	}


	if ((cave[py][px].feat == FEAT_SHAL_LAVA) &&
		!p_ptr->ffall)
	{
		int damage = p_ptr->lev;

		if (cave[py][px].feat == FEAT_SHAL_LAVA) damage = damage / 2;

		/* Take damage */
		msg_print("The lava burns you!");
		/*take_hit(damage, "shallow lava");*/
		project(-2, 0, py, px, damage, GF_FIRE, PROJECT_KILL | PROJECT_JUMP);
		cave_no_regen = TRUE;
	}

	else if ((cave[py][px].feat == FEAT_DEEP_LAVA))
	{
		int damage = p_ptr->lev * 2;
		cptr message;
		cptr hit_from;

		if (p_ptr->ffall)
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
			msg_print(message);
			/*take_hit(damage, hit_from);*/
			project(-2, 0, py, px, damage, GF_FIRE, PROJECT_KILL | PROJECT_JUMP);

			cave_no_regen = TRUE;
		}
	}

        /* Drown in deep water unless the player have levitation or water walking */
        else if ((cave[py][px].feat == FEAT_DEEP_WATER) && !p_ptr->ffall)
	{
                if (total_weight > ((max_carry() * 100) / 2))
		{
			/* Take damage */
			msg_print("You are drowning!");
			/*take_hit(randint(p_ptr->lev), "drowning");*/
			project(-2, 0, py, px, randint(p_ptr->lev), GF_WATER, PROJECT_KILL | PROJECT_JUMP);
			cave_no_regen = TRUE;
		}
	}

	else if ((cave[py][px].feat == FEAT_FIRE_FIELD) && cave[py][px].owner != 0)
	{
		project(-2, 0, py, px, cave[py][px].field_damage, GF_FIRE, PROJECT_KILL | PROJECT_JUMP);
		if (randint(100) >= 50)
		{
			cave[py][px].feat = FEAT_FLOOR;
			update_and_handle();
		}
	}

	else if ((cave[py][px].feat == FEAT_COLD_FIELD) && cave[py][px].owner != 0)
	{
		project(-2, 0, py, px, cave[py][px].field_damage, GF_COLD, PROJECT_KILL | PROJECT_JUMP);
		if (randint(100) >= 50)
		{
			cave[py][px].feat = FEAT_FLOOR;
			update_and_handle();
		}
	}

	else if ((cave[py][px].feat == FEAT_ELEC_FIELD) && cave[py][px].owner != 0)
	{
		project(-2, 0, py, px, cave[py][px].field_damage, GF_ELEC, PROJECT_KILL | PROJECT_JUMP);
		if (randint(100) >= 50)
		{
			cave[py][px].feat = FEAT_FLOOR;
			update_and_handle();
		}
	}

	else if ((cave[py][px].feat == FEAT_STORMS) && cave[py][px].owner != 0)
	{
		project(-2, 0, py, px, cave[py][px].field_damage, GF_WIND, PROJECT_KILL | PROJECT_JUMP);
		if (randint(100) >= 50)
		{
			cave[py][px].feat = FEAT_FLOOR;
			update_and_handle();
		}
	}

	else if ((cave[py][px].feat == FEAT_THORNED_VINES) && cave[py][px].owner != 0)
	{
		project(-2, 0, py, px, cave[py][px].field_damage, GF_EARTH, PROJECT_KILL | PROJECT_JUMP);
		if (randint(100) >= 50)
		{
			cave[py][px].feat = FEAT_FLOOR;
			update_and_handle();
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
		/* Player can walk through trees */
                if ((cave[py][px].feat == FEAT_TREES) &&
                    ((p_ptr->pclass==CLASS_RANGER) || (p_ptr->prace==RACE_ENT) || p_ptr->abilities[(CLASS_RANGER * 10) + 1] >= 1))
		{
			/* Do nothing */
		}

                else if (((cave[py][px].feat >= FEAT_ALTAR_HEAD) &&
                   (cave[py][px].feat <= FEAT_ALTAR_TAIL)) || cave[py][px].feat == FEAT_SPIKE_TRAP || cave[py][px].feat == FEAT_GAS_TRAP || cave[py][px].feat == FEAT_POISON_TRAP || cave[py][px].feat == FEAT_VINE_FIELD || cave[py][px].feat == FEAT_THORNED_VINES || cave[py][px].feat == FEAT_STORMS || cave[py][px].feat == FEAT_DARK_MIST)
                {
                        /* Do nothing */
                }

		/*else if (!(p_ptr->wraith_form) &&
                    (!p_ptr->fly || !(f_info[cave[py][px].feat].flags1 & FF1_CAN_FLY)) &&
                    (!p_ptr->climb || !(f_info[cave[py][px].feat].flags1 & FF1_CAN_CLIMB)) &&
                    ((p_ptr->chp > ((p_ptr->lev)/5))))
		{
			cptr dam_desc;

			cave_no_regen = TRUE;

                        msg_print("You are being crushed!");
                        dam_desc = "solid rock";

			take_hit(1 + ((p_ptr->lev)/5), dam_desc);
		}*/
	}


	/* Take damage from cuts */
	if ((p_ptr->cut))
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

	/* Default regeneration */
	regen_amount = 1;

	/* Divine Blood ability */
	if (p_ptr->abilities[(CLASS_PRIEST * 10) + 2] >= 1)
	{

		regen_amount += (p_ptr->abilities[(CLASS_PRIEST * 10) + 2] / 5);
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

	/* Regenerate the mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(regen_amount);
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
		{
			regenhp(regen_amount / 5); /* Hmmm. this should never happen? */
		}
		else
		{
			regenhp(regen_amount);
		}
	}


	/*** Timeout Various Things ***/

        /* Restore the Great Guard counter */
        if (p_ptr->guardconfuse > 0)
        {
                p_ptr->guardconfuse -= 1;
        }

	/* Eventually remove the damages counter. */
	if (damages_counter_duration > 0) damages_counter_duration -= 1;

	/* Handle temporary stat drains */
	for (i = 0; i < 6; i++)
	{
		if (p_ptr->stat_cnt[i] > 0)
		{
			p_ptr->stat_cnt[i]--;
                        if (p_ptr->stat_cnt[i] == 0)
			{
                                do_res_stat(i);
			}
		}
	}

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

        /* Timed Levitation */
        if (p_ptr->tim_ffall)
	{
                (void)set_tim_ffall(p_ptr->tim_ffall - 1);
	}

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - 1);
	}

	/* Timed invisibility */
        if (p_ptr->tim_invisible)
	{
                (void)set_invis(p_ptr->tim_invisible - 1, p_ptr->tim_inv_pow);
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
                (void)set_shield(p_ptr->shield - 1, p_ptr->shield_power);
	}


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = ((p_ptr->stat_ind[A_CON] / 10) + 1);

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = ((p_ptr->stat_ind[A_CON] / 10) + 1);

		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = ((p_ptr->stat_ind[A_CON] / 10) + 1);

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000) adjust = 0;

                /* Monsters heals MUCH faster */
                if (p_ptr->prace == RACE_MONSTER)
                {
                        adjust += 1;
                        adjust *= 5;
                }

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}

        /* The temporary stats boosts from spells! :) */
        /* Strength */
        if (p_ptr->str_boost_dur)
	{
                (void)set_str_boost(p_ptr->str_boost_dur - 1);
	}
        /* Intelligence */
        if (p_ptr->int_boost_dur)
	{
                (void)set_int_boost(p_ptr->int_boost_dur - 1);
	}
        /* Wisdom */
        if (p_ptr->wis_boost_dur)
	{
                (void)set_wis_boost(p_ptr->wis_boost_dur - 1);
	}
        /* Dexterity */
        if (p_ptr->dex_boost_dur)
	{
                (void)set_dex_boost(p_ptr->dex_boost_dur - 1);
	}
        /* Constitution */
        if (p_ptr->con_boost_dur)
	{
                (void)set_con_boost(p_ptr->con_boost_dur - 1);
	}
        /* Charisma */
        if (p_ptr->chr_boost_dur)
	{
                (void)set_chr_boost(p_ptr->chr_boost_dur - 1);
	}

        /* Temporary resistances + ac boost! */
        if (p_ptr->pres_dur)
	{
                (void)set_pres(p_ptr->pres_dur - 1);
	}
        if (p_ptr->mres_dur)
	{
                (void)set_mres(p_ptr->mres_dur - 1);
	}
        if (p_ptr->ac_boost_dur)
	{
                (void)set_ac_boost(p_ptr->ac_boost_dur - 1);
	}
        if (p_ptr->elem_shield)
	{
                (void)set_elem_shield(p_ptr->elem_shield - 1);
	}
        if (p_ptr->powerattack)
	{
                (void)set_powerattack(p_ptr->powerattack - 1);
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

                object_flags(o_ptr, &f1, &f2, &f3, &f4);

		/*
		 * Hack: Uncursed teleporting items (e.g. Trump Weapons)
		 * can actually be useful!
		 */

		if ((f3 & TR3_TELEPORT) && (rand_int(100)<1))
		{
			if ((o_ptr->ident & IDENT_CURSED))
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
                        if (!(o_ptr->timeout))
                        {
                                if (f3 & (TR3_ACTIVATE))
                                {
                                        recharged_notice(o_ptr);
                                        j++;
                                }
                                else
                                {
                                        inven_item_increase(i, -(o_ptr->number));
                                        inven_item_describe(i);
                                        inven_item_optimize(i);
                                        j++;                                        
                                }
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
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging rods or stacks of charging rods. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Determine how many rods are charging. */
			temp = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
			if (temp > o_ptr->number) temp = o_ptr->number;

			/* Decrease timeout by that number. */
			o_ptr->timeout -= temp;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;

			/* Notice changes, provide message if object is inscribed. */
			if (!(o_ptr->timeout)) 
			{
				recharged_notice(o_ptr);
				j++;
			}
		}

                /* Examine all charging random artifacts */
                if ((o_ptr->tval == TV_RANDART) && (o_ptr->timeout))
		{
			/* Charge it */
                        o_ptr->timeout--;

			/* Notice changes */
                        if (!(o_ptr->timeout)) j++;
		}

                /* Examine all charging souls */
                if ((o_ptr->tval == TV_SOUL || o_ptr->tval == TV_ESSENCE) && (o_ptr->timeout))
		{
			/* Charge it */
                        o_ptr->timeout--;

			/* Notice changes */
                        if (!(o_ptr->timeout)) j++;
		}


		/* Decay objects in pack */
                if (decays(o_ptr))
		{
			/* Decay it */
                        if (o_ptr->pval != 0)
                        {
                                if (!o_ptr->timeout)
                                {
                                        if(d_info[dungeon_type].flags1 & DF1_HOT)
                                        {
                                               o_ptr->pval -= 2;
                                        }
                                        else if((d_info[dungeon_type].flags1 & DF1_COLD) && rand_int(2))
                                        {
                                               o_ptr->pval--;
                                        }
                                        else
                                        {
                                               o_ptr->pval--;
                                        }
                                }

                                if ((o_ptr->timeout > 0) && o_ptr->timeout < o_ptr->weight) o_ptr->timeout--;

                                /* Notice changes */
                                if (o_ptr->pval <= 0)
                                {
                                        pack_decay(i);
                                        j++;
                                }
                        }
		}

                /* Hatch eggs */
                if(o_ptr->tval == TV_EGG)
                {
                        int mx,my;
			if (!o_ptr->timeout) o_ptr->pval--;
                        
			/* Notice changes */
			if (o_ptr->pval <= 0)
			{
                                monster_type *m_ptr;
                                monster_race *r_ptr;

                                r_ptr = &r_info[o_ptr->pval2];
                                if (r_ptr->level > (p_ptr->lev * 2))
                                {
                                        msg_format("Your egg cannot hatch until you reach level %d !", r_ptr->level / 2);
                                }
                                else
                                {

                                mx=px;
                                my=py+1;
                                get_pos_player(5, &my, &mx);
                                msg_print("Your egg hatchs!");
                                place_monster_aux_no_boss(my, mx, o_ptr->pval2, FALSE, FALSE, TRUE, 0);

                                m_ptr = &m_list[cave[my][mx].m_idx];
                                m_ptr->level = o_ptr->pval3;
                                apply_monster_level_hp(m_ptr);
                                r_ptr = &r_info[m_ptr->r_idx];
                                m_ptr->imprinted = TRUE;
                                set_pet(m_ptr, TRUE);

                                inven_item_increase(i, -1);
                                inven_item_describe(i);
                                inven_item_optimize(i);
                                j++;
                                }
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

		/* Recharge rods on the ground.  No messages. */
		if ((o_ptr->tval == TV_ROD) && (o_ptr->timeout))
		{
			/* Charge it */
			o_ptr->timeout -= o_ptr->number;

			/* Boundary control. */
			if (o_ptr->timeout < 0) o_ptr->timeout = 0;
		}

		/* Decay objects on the ground*/
		if (decays(o_ptr))
		{
			/* Decay it */
                        if (o_ptr->pval != 0)
                        {
                                if (!o_ptr->timeout)
                                {
                                        if(d_info[dungeon_type].flags1 & DF1_HOT)
                                        {
                                               o_ptr->pval -= 2;
                                        }
                                        else if((d_info[dungeon_type].flags1 & DF1_COLD) && rand_int(2))
                                        {
                                               o_ptr->pval--;
                                        }
                                        else
                                        {
                                               o_ptr->pval--;
                                        }
                                }

                                if ((o_ptr->timeout > 0) && o_ptr->timeout < o_ptr->weight) o_ptr->timeout--;

                                /* Turn it into a skeleton */
                                if (o_ptr->pval <= 0)
                                {
                                        floor_decay(i);
                                }
                        }
                }

                /* Hatch eggs */
                if(o_ptr->tval == TV_EGG)
                {
                        int mx,my;
			if (!o_ptr->timeout) o_ptr->pval--;
                        
			/* Notice changes */
			if (o_ptr->pval <= 0)
			{
                                mx=o_ptr->ix;
                                my=o_ptr->iy;
                                get_pos_player(5, &my, &mx);
                                msg_print("An egg hatchs!");
                                place_monster_one(my, mx, o_ptr->pval2, FALSE, FALSE, 0);
                                floor_item_increase(i, -1);
                                floor_item_describe(i);
                                floor_item_optimize(i);
			}
                }
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
                        if (p_ptr->inside_quest)
                        {
                                msg_print("Cannot recall here.");
                        }
			else if (p_ptr->wild_mode)
			{
				int ii, jj;
				msg_print("You are transported to the last town you visited!");
				p_ptr->wild_mode = 0;
				is_recall = TRUE;
				p_ptr->inside_quest = 0;
				dun_level = 0;
				/* Re-initialise the wilderness position! */
				/* jj is y, ii is x */
				for (jj = 0; jj < wild_max_y; jj++)
				{
					for (ii = 0; ii < wild_max_x; ii++)
					{
						if (wild[ii][jj].town == p_ptr->town_num)
						{
							p_ptr->wild_x = ii;
							p_ptr->wild_y = jj;
						}
					}
				}
				p_ptr->startx = get_town_startx(p_ptr->town_num);
				p_ptr->starty = get_town_starty(p_ptr->town_num);
				p_ptr->leaving = TRUE;
			}
                        else if (dun_level)
			{
				msg_print("You feel yourself yanked upwards!");

                                p_ptr->recall_dungeon = dungeon_type;
				dun_level = 0;

                                is_recall = TRUE;

				p_ptr->inside_quest = 0;
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");
				/* Cannot recall to random dungeons! */
				if (p_ptr->recall_dungeon == 200 || p_ptr->recall_dungeon == 201) p_ptr->recall_dungeon = 0;

				/* New depth */
                                dungeon_type = p_ptr->recall_dungeon;
                                dun_level = max_dlv[dungeon_type];
				if (dun_level < 1) dun_level = 1;

				/* Reset player position */
                                p_ptr->oldpx = px;
                                p_ptr->oldpy = py;

				/* Leaving */
                                is_recall = TRUE;

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
#if 0
	if (!(noscore & 0x0002))
#else
	if (!noscore)
#endif
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
                noscore |= 0x0002;
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
	if (!(noscore & 0x0008))
#else
	if (!noscore)
#endif
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
                noscore |= 0x0008;
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

#endif /* ALLOW_BORG */



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 *
 * XXX XXX XXX Make some "blocks"
 */
static void process_command(void)
{
	char error_m[80];

#ifdef ALLOW_REPEAT /* TNB */

    /* Handle repeating the last command */
    repeat_check();

#endif /* ALLOW_REPEAT -- TNB */

#ifdef USE_PYTHON
        if (perform_event(EVENT_COMMAND, Py_BuildValue("(c)", command_cmd))) return;
#endif
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
#ifdef ALLOW_QUITING
                case KTRL('L'):
		{
                        exit(0);
                        break;
                }
#endif
		/*** Wizard Commands ***/

		/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (wizard)
			{
				wizard = FALSE;
				msg_print("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				wizard = TRUE;
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
                case 'W':
                {
			int x, y;
                        msg_print("Talk to who? ");
                        if (!tgt_pt(&x,&y)) return;
                        if (distance(y,x,py,px) > 3)
                        {
                                msg_print("That is too far away.");
                        }
			else talk_to_monster(x, y);

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
                case 'Y':
		{
                        if (p_ptr->prace == RACE_MONSTER) do_cmd_evolve();
                        else do_cmd_change_class();
			break;
		}
                case 'y':
		{
                        make_gold_pile();
			break;
		}
                case '‚':
		{
                        know_body_monster();
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

                /* Use a Licialhyd */
                case 'X':
		{
                        /*do_cmd_use_licialhyd();*/
			call_lua("use_licialhyd", "", "");
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
#ifdef ALLOW_EASY_DISARM /* TNB */

			do_cmd_walk(FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

			do_cmd_walk(always_pickup);

#endif /* ALLOW_EASY_DISARM -- TNB */

			break;
		}

		/* Move (usually do not pick up) */
		case '-':
		{
#ifdef ALLOW_EASY_DISARM /* TNB */

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

		/* Toggle search mode */
		case 'S':
		{
			do_cmd_toggle_search();
			break;
		}
                /* Pickup corpses(and other non-pickable/pikable items) */
                case 'Š':
                {
                        no_pickup_corpse = FALSE;
                        py_pickup_floor(TRUE);
                        no_pickup_corpse = TRUE;
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

                /* Learn ability */
                case 'J':
		{
                        learn_ability();
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

		/* Cast a spell */
		case 'm':
		{
                        if (p_ptr->body_monster != 0)
			{
				int i;
				monster_race *r_ptr = &r_info[p_ptr->body_monster];
				for (i = 0; i < r_ptr->spells; i++)
				{
                        		use_body_power(p_ptr->body_monster, FALSE);
				}
			}
                        else if (p_ptr->magic_mode == 1)
                        {
                        	use_monster_power();
                        }
			else
                        {
                        	do_cmd_cast(FALSE);
                        }
			break;
		}

		/* Pray a prayer */
		/* Also known as "Wisdom Casting" in NewAngband 1.7.2! :) */
		case 'p':
		{
                        if (p_ptr->body_monster != 0) do_cmd_cast(FALSE);
			else
                        {
                        	do_cmd_cast(TRUE);
                        }
			break;
		}

                /* Issue commands to pets */
                case 'P':
		{
                        do_cmd_pet();
			break;
		}

		/* Monster info */
		case 'H':
		{
			if (p_ptr->prace == RACE_MONSTER) evolution_compare(0, FALSE, FALSE);
			break;
		}

		/* Toggle stuff */
		case 'K':
                {
			do_cmd_turn_on_off_misc();
                        break;
                }

                /* Steal an item form a monster */
                case 'Z':
                {
                        do_cmd_steal();
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

		/* Fuel your lantern/torch */
		case 'F':
		{
			do_cmd_refill();
			break;
		}

		/* Fire an item */
		case 'f':
                {
			if (p_ptr->prace == RACE_MONSTER && p_ptr->events[29030] == 1)
			{
				monster_race *r_ptr = &r_info[p_ptr->body_monster];
				if (r_ptr->attack[p_ptr->events[29031]].type == 3)
				{
					call_lua("monster_ranged_attacks", "(d)", "", p_ptr->events[29031]+1);
				}
				else do_cmd_fire();
			}
                        else do_cmd_fire();
			break;
		}

		/* Reload a ranged weapon */
		case 'O':
                {
                        reload_ranged();
			break;
		}

		/* Throw an item */
		case 'v':
                {
			p_ptr->events[29044] = 0;
			call_lua("ranged_throw", "", "");
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
			/*do_cmd_zap_rod();*/
			call_lua("zap_rod", "", "");
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

		/* Use racial power */
		case 'U':
		{
                        do_cmd_racial_power(0);
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
			if (!center_player) do_cmd_locate();
			else do_cmd_locate_center(py, px);
                        msg_format("x,y = %d, %d", px, py);
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

                case 'x':
                {
                        /*scan_targetting();*/
			if (p_ptr->dualwield == 0)
			{
				msg_print("Dual Wielding is turned On.");
				p_ptr->dualwield = 1;
			}
			else
			{
				msg_print("Dual Wielding is turned Off.");
				p_ptr->dualwield = 0;
			}
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
			is_autosave = FALSE;
			do_cmd_save_game();
			break;
		}

#endif /* VERIFY_SAVEFILE */

		case KTRL('T'):
		{
			do_cmd_time();
		}
		break;

		/* Save and quit */
		case KTRL('X'):
		{
			alive = FALSE;

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
		/* Use song! */
		case 'h':
		{
			call_lua("music_menu", "", "");
			break;
		}
                /* Charge weapon */
                case 'N':
		{
                        special_weapon_charge();
			break;
		}


		/* Hack -- Unknown command */
		default:
		{
                        if (randint(2)==1)
                        {
                                get_rnd_line("error.txt", error_m);
                                sound(SOUND_ILLEGAL);
                                msg_print(error_m);
                        }
                        else
                                prt("Type '?' for help.", 0, 0);
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

	/*** Apply energy ***/

	if (hack_mutation)
	{
		msg_print("You feel different!");
		(void)gain_random_mutation(0);
		hack_mutation = FALSE;
	}

	/* Give the player some energy */
        p_ptr->energy += extract_energy[(p_ptr->pspeed > 199)?199:(p_ptr->pspeed < 0)?0:p_ptr->pspeed];

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
                        if ((p_ptr->chp >= p_ptr->mhp) &&
                (p_ptr->csp >= p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (resting == -2)
		{
			bool stop = TRUE;

			/* Stop resting */
                        if (p_ptr->chp != p_ptr->mhp && !(p_ptr->chp > p_ptr->mhp)) stop = FALSE;
			if (p_ptr->csp != p_ptr->msp) stop = FALSE;
			if (p_ptr->blind || p_ptr->confused) stop = FALSE;
			if (p_ptr->poisoned || p_ptr->afraid) stop = FALSE;
			if (p_ptr->stun || p_ptr->cut) stop = FALSE;
			if (p_ptr->slow || p_ptr->paralyzed) stop = FALSE;
                        if (p_ptr->image || p_ptr->word_recall) stop = FALSE;
			for (i = 0; i < 6; i++)
				if (p_ptr->stat_cnt[i] > 0) stop = FALSE;
			
			if (stop)
			{
				disturb(0, 0);
			}
			p_ptr->redraw |= (PR_STATE);
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
                        /* Your constitution MAY save you... */
                        if (randint(100) <= p_ptr->stat_ind[A_CON])
                        {
                                msg_print("You stand up!");
                                p_ptr->paralyzed = FALSE;
                                p_ptr->stun -= 50;
                        }
                        else energy_use = 100; /* Take a turn */
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

                        /* We won, no more monsters! */
                        if (total_winner) anihilate_monsters();

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


		/* Hack -- notice death */
		if (!alive || death) break;

		/* Handle "leaving" */
                if (p_ptr->leaving) break;
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
	hack_mind = FALSE;

	/* Not leaving */
	p_ptr->leaving = FALSE;

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
	if (p_ptr->max_plv < p_ptr->lev)
	{
		p_ptr->max_plv = p_ptr->lev;
	}

	/* Track maximum dungeon level (if not in quest -KMW-) */
        if ((max_dlv[dungeon_type] < dun_level) && (!p_ptr->inside_quest))
	{
                max_dlv[dungeon_type] = dun_level;
	}

	/* No stairs down from Quest */
	if (is_quest(dun_level))
	{
		create_down_stair = FALSE;
                create_down_shaft = FALSE;
	}

	/* Paranoia -- no stairs from town or wilderness */
	if (!dun_level) create_down_stair = create_up_stair = FALSE;
        if (!dun_level) create_down_shaft = create_up_shaft = FALSE;

	/* Option -- no connected stairs */
	if (!dungeon_stair) create_down_stair = create_up_stair = FALSE;
        if (!dungeon_stair) create_down_shaft = create_up_shaft = FALSE;

	/* no connecting stairs on special levels */
	if (special_flag) create_down_stair = create_up_stair = FALSE;
        if (special_flag) create_down_shaft = create_up_shaft = FALSE;

	/* Make a stairway. */
        if ((create_up_stair || create_down_stair || create_up_shaft ||
            create_down_shaft) && !get_fbranch() && !(p_ptr->inside_quest))
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
                        else if (create_down_shaft)
                        {
                                cave_set_feat(py, px, FEAT_SHAFT_DOWN);
                        }
                        else if (create_up_shaft)
                        {
                                cave_set_feat(py, px, FEAT_SHAFT_UP);
                        }
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}
		}

		/* Cancel the stair request */
		create_down_stair = create_up_stair = FALSE;
                create_down_shaft = create_up_shaft = FALSE;
	}


	/* Choose a panel row */
	panel_row = ((py - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
	if (panel_row > max_panel_rows) panel_row = max_panel_rows;
	else if (panel_row < 0) panel_row = 0;

	/* Choose a panel col */
	panel_col = ((px - SCREEN_WID / 4) / (SCREEN_WID / 2));
	if (panel_col > max_panel_cols) panel_col = max_panel_cols;
	else if (panel_col < 0) panel_col = 0;

	/* Recalculate the boundaries */
	panel_bounds();


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
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_SANITY | PU_BODY);

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
        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_BODY);

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
	if (dun_level) do_cmd_feeling();


	/* Hack -- notice death or departure */
	if (!alive || death) return;

	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = dun_level;

	/* Reset the object generation level */
	object_level = dun_level;

	hack_mind = TRUE;

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > max_m_idx) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > max_o_idx) compact_objects(64);

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
		if (!alive || death) break;

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
		if (!alive || death) break;


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
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Count game turns */
                turn++;
	}

        /* Did we leave a dungeon ? */
        if ((dun_level < d_info[dungeon_type].mindepth) && (!is_recall))
        {
                dun_level = 0;
        }
        if (dun_level > d_info[dungeon_type].maxdepth && !(p_ptr->inside_quest))
        {
		/* There may be a quest at the end of the dungeon... */
		if (d_info[dungeon_type].quest > 0)
		{
			/* Reset the positions if we're not in a quest. */
			if (!(p_ptr->inside_quest))
			{
				p_ptr->questx = 0;
				p_ptr->questy = 0;
			}
			p_ptr->inside_quest = d_info[dungeon_type].quest;
		}
                else dun_level = 0;
        }
        is_recall = FALSE;
}




#if 0

/* Old routine to load pref files */

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
	sprintf(buf, "%s.prf", classes_def[p_ptr->pclass].name);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);
}

#else

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
	sprintf(buf, "%s.prf", classes_def[p_ptr->pclass].name);

	/* Process that file */
	process_pref_file(buf);

	/* Access the "character" pref file */
	sprintf(buf, "%s.prf", player_base);

	/* Process that file */
	process_pref_file(buf);
}


#endif


/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 */
void play_game(bool new_game)
{
        int i, tmp_dun;
        bool cheat_death=FALSE;

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

	/* Roll new character */
	if (new_game)
	{
		/* The dungeon is not ready */
		character_dungeon = FALSE;

		/* Start in town */
		dun_level = 0;
		p_ptr->inside_quest = 0;

		/* Hack -- seed for flavors */
		seed_flavor = rand_int(0x10000000);

		/* Hack -- seed for town layout */
		seed_town = rand_int(0x10000000);
#ifdef USE_PYTHON
		/* Event -- start game */
		perform_event(EVENT_START_GAME, Py_BuildValue("()"));
#endif
		/* Roll up a new character */
		player_birth();

		/* Hack -- enter the world */
                turn = 1;
	}

	/* Flash a message */
	prt("Please wait...", 0, 0);

	/* Flush the message */
	Term_fresh();

	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) wizard = TRUE;

	/* Flavor the objects */
	flavor_init();

	/* Reset the visual mappings */
	reset_visuals();

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER);

	/* Window stuff */
	window_stuff();


	/* Load the "pref" files */
	load_all_pref_files();

	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* Initialize vault info */
	if (init_v_info()) quit("Cannot initialize vaults");

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

#ifdef USE_PYTHON
        perform_event(EVENT_ENTER_QUEST, Py_BuildValue("(ii)", p_ptr->inside_quest, dun_level));
#endif

	/* Process */
	while (TRUE)
	{
                /* Save the level */
                old_dun_level = dun_level;

#ifdef SAVE_HACK
		/* Process the level */
                if (!save_hack)
                {
                        dungeon();
                }else{
                        generate_cave();
                }

                save_hack=FALSE;

                p_ptr->leaving=TRUE;
#else
		/* Process the level */
                dungeon();
#endif

                /* Save the current level if in a persistent level */
                tmp_dun = dun_level;
                dun_level = old_dun_level;
                save_dungeon();
                dun_level = tmp_dun;

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


		/* Erase the old cave */
		wipe_o_list();


		/* XXX XXX XXX */
		msg_print(NULL);

		/* Accidental Death */
		if (alive && death)
		{
                cheat_death = FALSE;
                if (p_ptr->allow_one_death>0)
    {
      cheat_death = TRUE;
      if(p_ptr->allow_one_death>1)p_ptr->allow_one_death--; else p_ptr->allow_one_death=0;
      msg_print("You have been saved by the Blood of Life!");
      msg_print(NULL);
    }
  else
    if ((wizard || cheat_live) && !get_check("Die? "))
      {
	cheat_death = TRUE;

				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				noscore |= 0x0001;
	msg_print("You invoke wizard mode and cheat death.");
	msg_print(NULL);
      }
  if (cheat_death)
    {
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

				/* Do not die */
				death = FALSE;

				/* New depth -KMW- */
				/* dun_level = 0; */
				leaving_quest = 0;
				p_ptr->inside_quest = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
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

void show_dialog(int dialognum)
{
	int i, pos;
	char fname[80];
	char sname[80];
	char c;
	char buf[1024];
	int var1, var2, var3;
	int old_town_num;
	bool dialogfound = FALSE;
	FILE *fp;

	/* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        /* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }

	/* Determine file's name */
	sprintf(fname, "d%d.txt", dialognum);

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_FILE, fname);

	/* Open the file */
	fp = my_fopen(buf, "r");

	/* Parse it */
	if (!fp)
	{
		msg_format("Cannot load file: %s", fname);
		Term_load();
		return;
	}

	/* Parse the file */
	pos = 0;
        while (0 == my_fgets(fp, buf, 1024))
	{
		/* First, look for 'N' */
		if ((buf[0] == 'N') && !(dialogfound))
		{
			/* Scan for the values */
			if (3 != sscanf(buf+2, "%d:%d:%d",
                                &var1, &var2, &var3)) return (1);
			
			if (var2 != 0)
			{
				if (p_ptr->events[var2] == var3) dialogfound = TRUE;
			}
			else dialogfound = TRUE;
		}
		continue;
	}

	/* First, let's save the town we're in. */
	old_town_num = p_ptr->town_num;	

	/* Now, process the dialog! */
	process_dialog(var1, fp);	

	/* Close it */
	my_fclose(fp);

	/* Load term. */
	Term_load();

	/* If the town has changed... */
	if (p_ptr->town_num != old_town_num)
	{
		p_ptr->inside_quest = FALSE;
		p_ptr->leaving = TRUE;
	}

}

static void object_prep_magic(object_type *o_ptr, int k_idx, int magic)
{
	int i;
	object_kind *k_ptr = &k_info[k_idx];

	/* Clear the record */
	WIPE(o_ptr, object_type);

	/* Save the kind index */
	o_ptr->k_idx = k_idx;

	/* Efficiency -- tval/sval */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Default "pval" */
	o_ptr->pval = k_ptr->pval;

        /* Default "pval3" for weapons */
        if (is_weapon(o_ptr))
        {
        o_ptr->pval3 = 20 + randint(10);
        if (o_ptr->name1 || o_ptr->name2 == 131)
        {
               o_ptr->pval3 += 50;
        }
        o_ptr->xtra1 = 0;
        }


	/* Default number */
	o_ptr->number = 1;

	/* Default weight */
	o_ptr->weight = k_ptr->weight;

	/* Default magic */
	o_ptr->to_h = k_ptr->to_h;
	o_ptr->to_d = k_ptr->to_d;
	o_ptr->to_a = k_ptr->to_a;

	/* Default power */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Default brand */
	if (k_ptr->brandtype > 0)
	{
		o_ptr->brandtype = k_ptr->brandtype;
		o_ptr->branddam = k_ptr->branddam;
		o_ptr->brandrad = k_ptr->brandrad;
	}
	else
	{
		o_ptr->brandtype = 0;
		o_ptr->branddam = 0;
		o_ptr->brandrad = 0;
	}

	/* Default resistances */
	for (i = 0; i < MAX_RESIST; i++)
	{
		o_ptr->resistances[i] = k_ptr->resistances[i];
	}

	/* Default stats bonus. */
	for (i = 0; i < 6; i++)
	{
		o_ptr->statsbonus[i] = k_ptr->statsbonus[i];
	}

	/* Default skills bonus. */
	for (i = 0; i < SKILL_MAX; i++)
	{
		o_ptr->skillsbonus[i] = k_ptr->skillsbonus[i];
	}

	/* Spells(if any) */
	for (i = 0; i < 20; i++)
	{
		strcpy(o_ptr->spell[i].name, k_ptr->spell[i].name);
                strcpy(o_ptr->spell[i].act, "");
		o_ptr->spell[i].type = k_ptr->spell[i].type;
		o_ptr->spell[i].power = k_ptr->spell[i].power;
		o_ptr->spell[i].special1 = k_ptr->spell[i].special1;
		o_ptr->spell[i].special2 = k_ptr->spell[i].special2;
		o_ptr->spell[i].special3 = k_ptr->spell[i].special3;
		o_ptr->spell[i].summchar = k_ptr->spell[i].summchar;
		o_ptr->spell[i].cost = k_ptr->spell[i].cost;
	}

	/* Other bonuses. */
	o_ptr->itemtype = k_ptr->itemtype;
	o_ptr->itemskill = k_ptr->itemskill;
	o_ptr->extrablows = k_ptr->extrablows;
	o_ptr->extrashots = k_ptr->extrashots;
	o_ptr->speedbonus = k_ptr->speedbonus;
	o_ptr->lifebonus = k_ptr->lifebonus;
	o_ptr->manabonus = k_ptr->manabonus;
	o_ptr->infravision = k_ptr->infravision;
	o_ptr->spellbonus = k_ptr->spellbonus;
	o_ptr->invisibility = k_ptr->invisibility;
	o_ptr->light = k_ptr->light;
	o_ptr->extra1 = k_ptr->extra1;
	o_ptr->extra2 = k_ptr->extra2;
	o_ptr->extra3 = k_ptr->extra3;
	o_ptr->extra4 = k_ptr->extra4;
	o_ptr->extra5 = k_ptr->extra5;
	o_ptr->reflect = k_ptr->reflect;

	/* Hack -- worthless items are always "broken" */
	if (k_ptr->cost <= 0 && o_ptr->tval != TV_LICIALHYD) o_ptr->ident |= (IDENT_BROKEN);

	/* Hack -- cursed items are always "cursed" */
	if (k_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);

        /* Hack give a basic exp/exp level to an object that needs it */
        if(k_ptr->flags4 & TR4_LEVELS)
        {
                o_ptr->level = 1;
                o_ptr->kills = 0;
		o_ptr->tweakpoints = 2;
        }

	/* Ranged weapons are fully loaded. */
	if (o_ptr->tval == TV_RANGED)
	{
		o_ptr->pval2 = k_ptr->extra3;
	}

	if (o_ptr->tval == TV_GOLD)
	{
		o_ptr->pval = magic;
	}
	else
	{
		switch (magic)
		{
			case 0:
			{
                		apply_magic(o_ptr, p_ptr->lev, TRUE, FALSE, FALSE, FALSE);
				break;
			}
			case 1:
			{
                		apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, FALSE, FALSE);
				break;
			}
			case 2:
			{
                		apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, TRUE, FALSE);
				break;
			}
			case 3:
			{
                		apply_magic(o_ptr, p_ptr->lev, TRUE, TRUE, TRUE, TRUE);
				break;
			}
		}
	}
}

static void dialog_artifact_prep(int a_idx)
{
	int i;
	object_type forge;
	object_type *q_ptr;
        object_kind *k_ptr;
        int w;
        char out_val[80];
        artifact_type *a_ptr;
        u32b f1, f2, f3, f4;                              

        a_ptr = &a_info[a_idx];

	/* Get local object */
	q_ptr = &forge;

	/* Wipe the object */
	object_wipe(q_ptr);

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

#if 0
        /* Ignore generated artifacts */
        if (a_ptr->cur_num) return;
#endif

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the artifact */
	object_prep(q_ptr, i);

	/* Save the name */
	q_ptr->name1 = a_idx;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Hack -- acquire "cursed" flag */
	if (a_ptr->flags3 & (TR3_CURSED)) q_ptr->ident |= (IDENT_CURSED);

        k_ptr = &k_info[q_ptr->k_idx];

        /* Extract some flags */
        object_flags(q_ptr, &f1, &f2, &f3, &f4);

        /* Hack give a basic exp/exp level to an object that needs it */
        if(f4 & TR4_LEVELS)
        {
                q_ptr->level = 1;
                q_ptr->kills = 0;
        }

	/* Artifact Brand! */
	if (a_ptr->brandtype > 0)
	{
		q_ptr->brandtype = a_ptr->brandtype;
		q_ptr->branddam = a_ptr->branddam;
		q_ptr->brandrad = a_ptr->brandrad;
	}
	else
	{
		q_ptr->brandtype = 0;
		q_ptr->branddam = 0;
		q_ptr->brandrad = 0;
	}

	for (w = 0; w < MAX_RESIST; w++)
	{
		q_ptr->resistances[w] = a_ptr->resistances[w];
	}

	/* Default stats bonus. */
	for (i = 0; i < 6; i++)
	{
		q_ptr->statsbonus[i] = a_ptr->statsbonus[i];
	}

	/* Default skills bonus. */
	for (i = 0; i < SKILL_MAX; i++)
	{
		q_ptr->skillsbonus[i] = a_ptr->skillsbonus[i];
	}

	/* Spells(if any) */
	for (i = 0; i < 20; i++)
	{
		strcpy(q_ptr->spell[i].name, a_ptr->spell[i].name);
                strcpy(q_ptr->spell[i].act, "");
		q_ptr->spell[i].type = a_ptr->spell[i].type;
		q_ptr->spell[i].power = a_ptr->spell[i].power;
		q_ptr->spell[i].special1 = a_ptr->spell[i].special1;
		q_ptr->spell[i].special2 = a_ptr->spell[i].special2;
		q_ptr->spell[i].special3 = a_ptr->spell[i].special3;
		q_ptr->spell[i].summchar = a_ptr->spell[i].summchar;
		q_ptr->spell[i].cost = a_ptr->spell[i].cost;
	}

	/* Hack give a basic exp/exp level to an object that needs it */
        if(a_ptr->flags4 & (TR4_LEVELS))
        {
                q_ptr->level = 1;
                q_ptr->kills = 0;
		q_ptr->tweakpoints = 2;
        }

	/* Other bonuses. */
	q_ptr->itemtype = a_ptr->itemtype;
	q_ptr->itemskill = a_ptr->itemskill;
	q_ptr->extrablows = a_ptr->extrablows;
	q_ptr->extrashots = a_ptr->extrashots;
	q_ptr->speedbonus = a_ptr->speedbonus;
	q_ptr->lifebonus = a_ptr->lifebonus;
	q_ptr->manabonus = a_ptr->manabonus;
	q_ptr->infravision = a_ptr->infravision;
	q_ptr->spellbonus = a_ptr->spellbonus;
	q_ptr->invisibility = a_ptr->invisibility;
	q_ptr->light = a_ptr->light;
	q_ptr->extra1 = a_ptr->extra1;
	q_ptr->extra2 = a_ptr->extra2;
	q_ptr->extra3 = a_ptr->extra3;
	q_ptr->extra4 = a_ptr->extra4;
	q_ptr->extra5 = a_ptr->extra5;
	q_ptr->reflect = a_ptr->reflect;
	q_ptr->cursed = a_ptr->cursed;

        a_ptr->cur_num = 1;

	/* Drop the artifact from heaven */
	(void)inven_carry(q_ptr, FALSE);
}

/* Process the dialog! */
int process_dialog(int dnum, FILE *fp)
{
	int i, pos, curline, choice, j;
	char sname[1024];
	char tmp[1024];	
	char str[1024];
	char c;
	char buf[1024];
	char query;
	s32b var1, var2, var3, var4, var5, var6, var7, var8, var9;
	bool dialogfound = FALSE;
	bool dialog_active = TRUE;
	dialog_answers answers[5];
	object_type *o_ptr;
	monster_race *r_ptr;

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

	/* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }

	/* Clear the answers */
	for (i = 0; i < 5; i++)
	{
		strcpy(answers[i].name, "");
		answers[i].ctype = 0;
		answers[i].cparam1 = 0;
		answers[i].cparam2 = 0;
		answers[i].effect = 0;
		answers[i].eparam1 = 0;
		answers[i].eparam2 = 0;
		answers[i].eparam3 = 0;
		answers[i].eparam4 = 0;
		answers[i].eparam5 = 0;
		answers[i].valid = 0;
	}

	/* Dialog 0 = end of dialog. */
	if (dnum == 0) return;

	/* Parse the file */
	curline = 0;
	rewind(fp);
	while (0 == my_fgets(fp, buf, 1024))
	{
		pos = 0;
		if (dialogfound)
		{
			/* Speaker's name should be first... */
			if (buf[0] == 'I')
			{
				pos = 2;
				c = buf[pos];
				strcpy(sname, "");
				while (c != ';')
				{
					sprintf(tmp, "%s%c", sname, c);
					strcpy(sname, tmp);
					pos = pos + 1;
					c = buf[pos];
				}
				c_put_str(TERM_WHITE, "                                                                     ", curline, 0);
				c_put_str(TERM_WHITE, sname, curline, 0);
				curline += 1;
        			c_put_str(TERM_WHITE, "--------------", curline, 0);
				curline += 2;
			}

			/* Dialog text */
			if (buf[0] == 'T')
			{
				pos = 2;
				c = buf[pos];
				strcpy(str, "");
				while (c != ';')
				{
					/* Special character */
					if (c == '<')
					{
						char tmpspec[80];
						char tmpstr[80];
						pos += 1;
						c = buf[pos];
						strcpy(tmpspec, "");
						strcpy(tmpstr, "");
						while (c != '>')
						{
							sprintf(tmpspec, "%s%c", tmpstr, c);
							strcpy(tmpstr, tmpspec);
							pos += 1;
							c = buf[pos];
						}
						if (strstr(tmpspec,"NAME"))
						{
							sprintf(tmp, "%s%s", str, player_name);
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"RACE"))
						{
							sprintf(tmp, "%s%s", str, rp_ptr->title);
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"CLASS"))
						{
							sprintf(tmp, "%s%s", str, classes_def[p_ptr->pclass].name);
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"HeShe"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "He");
							else sprintf(tmp, "%s%s", str, "She");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"heshe"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "he");
							else sprintf(tmp, "%s%s", str, "she");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"ManWoman"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Man");
							else sprintf(tmp, "%s%s", str, "Woman");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"manwoman"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "man");
							else sprintf(tmp, "%s%s", str, "woman");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"BoyGirl"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Boy");
							else sprintf(tmp, "%s%s", str, "Girl");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"boygirl"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "boy");
							else sprintf(tmp, "%s%s", str, "girl");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"SirMadam"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Sir");
							else sprintf(tmp, "%s%s", str, "Madam");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"sirmadam"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "sir");
							else sprintf(tmp, "%s%s", str, "madam");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"LadLady"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Lad");
							else sprintf(tmp, "%s%s", str, "Lady");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"ladlady"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "lad");
							else sprintf(tmp, "%s%s", str, "lady");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"HimHer"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Him");
							else sprintf(tmp, "%s%s", str, "Her");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"himher"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "him");
							else sprintf(tmp, "%s%s", str, "her");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"HisHer"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "His");
							else sprintf(tmp, "%s%s", str, "Her");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"hisher"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "his");
							else sprintf(tmp, "%s%s", str, "her");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"HeroHeroine"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Hero");
							else sprintf(tmp, "%s%s", str, "Heroine");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"heroheroine"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "hero");
							else sprintf(tmp, "%s%s", str, "heroine");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"MrMiss"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Mr");
							else sprintf(tmp, "%s%s", str, "Miss");
							strcpy(str, tmp);
						}
						if (strstr(tmpspec,"MrMrs"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", str, "Mr");
							else sprintf(tmp, "%s%s", str, "Mrs");
							strcpy(str, tmp);
						}
					}
					else
					{
						sprintf(tmp, "%s%c", str, c);
						strcpy(str, tmp);
					}
					pos = pos + 1;
					c = buf[pos];
				}
				c_put_str(TERM_WHITE, str, curline, 0);
				curline += 1;
			}

			/* A dialog answer */
			if (buf[0] == 'A')
			{
				int n1, n2;
				char aname[1024];
				char tmp[1024];

				/* Find the next empty answer */
				for (i = 0; i < 5; i++) if (!answers[i].valid) break;

				/* Oops, no more slots */
				if (i == 5) return (1);

				/* Read the answer's name */
				pos = 2;
				c = buf[pos];
				strcpy(aname, "");
				while (c != ':')
				{
					/* Special character */
					if (c == '<')
					{
						char tmpspec[80];
						char tmpstr[80];
						pos += 1;
						c = buf[pos];
						strcpy(tmpspec, "");
						strcpy(tmpstr, "");
						while (c != '>')
						{
							sprintf(tmpspec, "%s%c", tmpstr, c);
							strcpy(tmpstr, tmpspec);
							pos += 1;
							c = buf[pos];
						}
						if (strstr(tmpspec,"NAME"))
						{
							sprintf(tmp, "%s%s", aname, player_name);
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"RACE"))
						{
							sprintf(tmp, "%s%s", aname, rp_ptr->title);
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"CLASS"))
						{
							sprintf(tmp, "%s%s", aname, classes_def[p_ptr->pclass].name);
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"HeShe"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "He");
							else sprintf(tmp, "%s%s", aname, "She");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"heshe"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "he");
							else sprintf(tmp, "%s%s", aname, "she");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"ManWoman"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Man");
							else sprintf(tmp, "%s%s", aname, "Woman");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"manwoman"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "man");
							else sprintf(tmp, "%s%s", aname, "woman");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"BoyGirl"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Boy");
							else sprintf(tmp, "%s%s", aname, "Girl");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"boygirl"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "boy");
							else sprintf(tmp, "%s%s", aname, "girl");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"SirMadam"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Sir");
							else sprintf(tmp, "%s%s", aname, "Madam");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"sirmadam"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "sir");
							else sprintf(tmp, "%s%s", aname, "madam");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"LadLady"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Lad");
							else sprintf(tmp, "%s%s", aname, "Lady");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"ladlady"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "lad");
							else sprintf(tmp, "%s%s", aname, "lady");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"HimHer"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Him");
							else sprintf(tmp, "%s%s", aname, "Her");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"himher"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "him");
							else sprintf(tmp, "%s%s", aname, "her");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"HisHer"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "His");
							else sprintf(tmp, "%s%s", aname, "Her");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"hisher"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "his");
							else sprintf(tmp, "%s%s", aname, "her");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"HeroHeroine"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Hero");
							else sprintf(tmp, "%s%s", aname, "Heroine");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"heroheroine"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "hero");
							else sprintf(tmp, "%s%s", aname, "heroine");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"MrMiss"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Mr");
							else sprintf(tmp, "%s%s", aname, "Miss");
							strcpy(aname, tmp);
						}
						if (strstr(tmpspec,"MrMrs"))
						{
							if (p_ptr->psex == SEX_MALE) sprintf(tmp, "%s%s", aname, "Mr");
							else sprintf(tmp, "%s%s", aname, "Mrs");
							strcpy(aname, tmp);
						}
					}
					else
					{
						sprintf(tmp, "%s%c", aname, c);
						strcpy(aname, tmp);
					}
					/*sprintf(tmp, "%s%c", aname, c);
					strcpy(aname, tmp);*/
					pos = pos + 1;
					c = buf[pos];
				}
				pos = pos + 1;

				/* Scan for the other values */
                        	if (9 != sscanf(buf+pos, "%ld:%ld:%ld:%ld:%ld:%ld:%ld:%ld:%ld",
					&var1, &var2, &var3, &var4, &var5, &var6, &var7, &var8, &var9)) return (1);

				/* Save the values */
                        	strcpy(answers[i].name, aname);
				answers[i].ctype = var1;
				answers[i].cparam1 = var2;
				answers[i].cparam2 = var3;
				answers[i].effect = var4;
				answers[i].eparam1 = var5;
				answers[i].eparam2 = var6;
				answers[i].eparam3 = var7;
				answers[i].eparam4 = var8;
				answers[i].eparam5 = var9;

				/* Ressolve the condition */
				switch (answers[i].ctype)
				{
					case 0:
						answers[i].valid = 1;
						break;

					/* Events condition */
					case 1:
					{
						if (p_ptr->events[answers[i].cparam1] == answers[i].cparam2) answers[i].valid = 1;
						else answers[i].valid = 0;
						break;
					}

					/* Item condition */
					case 2:
					{
						int j = 0;
						answers[i].valid = 0;
        					while (j <= 52)
        					{
                					/* Get the item */
                					o_ptr = &inventory[j];

                					if ((o_ptr->tval == answers[i].cparam1) && (o_ptr->sval == answers[i].cparam2))
							{
								answers[i].valid = 1;
								j = 52;
							}

                					j++;
        					}
						break;
					}
					
					/* Monster slain condition */
					case 3:
					{
						int j = 0;
						answers[i].valid = 0;
        					while (j < max_r_idx)
        					{
							if (j == answers[i].cparam1)
							{
                						/* Get the monster */
                						r_ptr = &r_info[j];

                						if (r_ptr->r_tkills >= answers[i].cparam2)
								{
									answers[i].valid = 1;
									j = max_r_idx;
								}
							}

                					j++;
        					}
						break;
					}
					/* Alignment condition */
					case 4:
					{
						answers[i].valid = 0;
						if (answers[i].cparam1 >= 0)
						{
							if (p_ptr->alignment >= answers[i].cparam1) answers[i].valid = 1;
						}
						else
						{
							if (p_ptr->alignment <= answers[i].cparam1) answers[i].valid = 1;
						}
						break;
					}
					/* Charisma condition */
					case 5:
					{
						answers[i].valid = 0;
						if (p_ptr->stat_ind[A_CHR] + (p_ptr->abilities[(CLASS_BARD * 10) + 6] * 5) >= answers[i].cparam1) answers[i].valid = 1;
						break;
					}
				}
			}
			/* '%' is end of block. */
			if (buf[0] == '%')
			{
				break;
			}
		}
		/* First, look for the correct 'N' */
		if ((buf[0] == 'N') && !(dialogfound))
		{
			s32b racevar, sexvar, alivar;
			/* Scan for the values */
			if (6 != sscanf(buf+2, "%ld:%ld:%ld:%ld:%ld:%ld",
                                &var1, &var2, &var3, &racevar, &sexvar, &alivar)) return (1);

			if (var1 == dnum)
			{
				
				if (p_ptr->events[var2] == var3)
				{
					if ((racevar > 0) && (p_ptr->prace == racevar))
					{
						if ((sexvar > 0) && ((p_ptr->psex + 1) == sexvar)) dialogfound = TRUE;
						else if (sexvar == 0) dialogfound = TRUE;
					}
					else if (racevar == 0)
					{ 
						if ((sexvar > 0) && ((p_ptr->psex + 1) == sexvar)) dialogfound = TRUE;
						else if (sexvar == 0) dialogfound = TRUE;
					}
				}
				
			}
			/* Check the alignment here. */
			if (alivar != 0)
			{
				if (alivar >= 0 && p_ptr->alignment < alivar) dialogfound = FALSE;
				else if (alivar < 0 && p_ptr->alignment > alivar) dialogfound = FALSE;
			}
		}

		/* Move to next line. */
		continue;
	}
	
	/* We're done writing the dialog and generating possible answers! */
	/* Now, let the player decide something! */
	curline += 1;
	i = 0;
	while (answers[i].valid == 1)
	{
		sprintf(str, "[%d] %s", (i + 1), answers[i].name);
		if (answers[i].ctype == 5) c_put_str(TERM_L_GREEN, str, curline, 0);
		else if (answers[i].ctype == 4)
		{
			if (answers[i].cparam1 >= 0) c_put_str(TERM_YELLOW, str, curline, 0);
			else c_put_str(TERM_L_RED, str, curline, 0);
		}
		else c_put_str(TERM_L_BLUE, str, curline, 0);
		curline += 1;
		i += 1;
	}
	
	/* Now, let the player do something. */
	choice = -1;
	while (dialog_active)
	{
		query = inkey();
        	if ((query == '1') && (answers[0].valid == 1)) choice = 0;
		if ((query == '2') && (answers[1].valid == 1)) choice = 1;
		if ((query == '3') && (answers[2].valid == 1)) choice = 2;
		if ((query == '4') && (answers[3].valid == 1)) choice = 3;
		if ((query == '5') && (answers[4].valid == 1)) choice = 4;

		/* Process the choice. */
		if (choice >= 0)
		{
			/* No matter the effect, param 4 and 5 are events settings. */
			if (answers[choice].eparam4 > 0)
			{
				p_ptr->events[answers[choice].eparam4] = answers[choice].eparam5;
			}

			switch (answers[choice].effect)
			{
				/* Next dialog block. */
				case 1:
				{
					process_dialog(answers[choice].eparam1, fp);
					break;
				}

				/* End dialog, change town. */
				case 2:
				{
					int newx, newy;
					p_ptr->town_num = answers[choice].eparam1;
					p_ptr->startx = answers[choice].eparam2;
					p_ptr->starty = answers[choice].eparam3;
					p_ptr->inside_quest = 0;
					p_ptr->wild_mode = 0;

					newx = get_town_overworldx(answers[choice].eparam1);
					newy = get_town_overworldy(answers[choice].eparam1);

					/* Shouldn't happen, but... */
					if (newx != 0 && newy != 0)
					{
						p_ptr->wild_x = newx;
						p_ptr->wild_y = newy;
					}

					dun_level = 0;
					p_ptr->leaving = TRUE;
					
					break;
				}

				/* Give item */
				case 3:
				{
					object_type *q_ptr;
					object_type new_obj;
						
					/* Get local object */
					q_ptr = &new_obj;

					/* Create the item */
					if (answers[choice].eparam2 == 4)
					{
						dialog_artifact_prep(answers[choice].eparam1);
					}
					else
					{
						object_prep_magic(q_ptr, answers[choice].eparam1, answers[choice].eparam2);

						/* If an ammo, create many! */
						if (q_ptr->tval == TV_AMMO)
						{
							q_ptr->number = 20;
						}

        					(void)inven_carry(q_ptr, FALSE);
						update_and_handle();
					}

					process_dialog(answers[choice].eparam3, fp);
					break;
				}

				/* Random dialog block. */
				case 4:
				{
					int minanswer = answers[choice].eparam1;
					int maxanswer = answers[choice].eparam2;
					int tmpint;
					tmpint = (maxanswer - minanswer) + 1;
					process_dialog((maxanswer - (randint(tmpint) - 1)), fp);
					break;
				}

				/* Affect alignment */
				case 5:
				{
					p_ptr->alignment += answers[choice].eparam1;
					process_dialog(answers[choice].eparam2, fp);
					break;
				}

				/* Take item */
				case 6:
				{
					object_type *o_ptr;
					j = 0;
					while (j <= 52)
        				{
                				/* Get the item */
                				o_ptr = &inventory[j];

						if (o_ptr->k_idx == answers[choice].eparam1)
						{
							inven_item_increase(j, -1);
							inven_item_describe(j);
							inven_item_optimize(j);
						}

                				j++;
        				}

					update_and_handle();

					process_dialog(answers[choice].eparam2, fp);
					break;
				}
				/* Alter Town */
				case 7:
				{
					p_ptr->towns[answers[choice].eparam1] = answers[choice].eparam2;
					process_dialog(answers[choice].eparam3, fp);
					break;
				}
				/* Pay gold. */
				case 8:
				{
					if (p_ptr->au >= answers[choice].eparam1)
					{
						p_ptr->au -= answers[choice].eparam1;
						process_dialog(answers[choice].eparam2, fp);
					}
					else process_dialog(answers[choice].eparam3, fp);
					break;
				}
				/* Remove all monsters of a given kind. */
				case 9:
				{
					anihilate_monsters_specific(answers[choice].eparam1);
					process_dialog(answers[choice].eparam2, fp);
					break;
				}
				/* All monsters of a given race changes their allegiance. */
				case 10:
				{
					if (answers[choice].eparam2 == 1)
					{
						mass_change_allegiance(answers[choice].eparam1, TRUE);
					}
					else mass_change_allegiance(answers[choice].eparam1, FALSE);
					process_dialog(answers[choice].eparam3, fp);
					break;
				}
				/* Run script. */
				case 11:
				{
					call_lua("dialog_script", "(d)", "", answers[choice].eparam1);
					process_dialog(answers[choice].eparam2, fp);
					break;
				}
			}
			dialog_active = FALSE;
		}
	}
	
}