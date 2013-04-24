/* File: dungeon.c */
/* Purpose: Regen and gameflow code */

/*
 * The man came in raving at the top of his lungs.
 * "I have found it! The source of all knowledge in this arcane text! it tells
 * me how to regenerate hitpoints and mana, and handle the passage of time.  Enter
 * wizard, debug, and borg modes.  Process commands.  Handle a character's
 * turn.  Interact with the current dungeon level.  Play a game!
 * "It is all so clear to me now!"
 *
 * We shut him out and went back to drinking our mead. This was none
 * too an unusual occurance here at Arkham.
 *
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
static int value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (broken_p(o_ptr)) return (INSCRIP_SHATTERED);
		if (cursed_p(o_ptr)) return (INSCRIP_TWISTED);
		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (broken_p(o_ptr)) return (INSCRIP_WORTHLESS);
		if (cursed_p(o_ptr)) return (INSCRIP_WEIRD);
		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 * I've changed the order of the id, with cursed/broken being discovered
 * over artifacts/ego items. Originally it was reversed, but that appears
 * to be different then all the other codebases I steal from and even
 * from the original codebase I started from (!?) 2/13/05
 */
static int value_check_aux2(const object_type *o_ptr)
{
	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Artifacts (all of them) -- except cursed/broken  */
	if (artifact_p(o_ptr)) return (INSCRIP_GOOD);

	/* Ego-Items (all of them) -- except cursed/broken  */
	if (ego_item_p(o_ptr)) return (INSCRIP_GOOD);

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD);

	/* No feeling */
	return (0);
}



/*
 * Sense the inventory
 * 
 * Eyangband tested, Steam approved!
 */
static void sense_inventory(void)
{
	int i, delay1, delay2;

	int plev = p_ptr->lev;

	bool okay; 
	bool heavy = ((cp_ptr->flags & CF_PSEUDO_ID_HEAVY) ? TRUE : FALSE);

	int feel;

	object_type *o_ptr;

	char o_name[80];
	
	/* Paranoia */
	delay1 = 150000L;

	/*** Check for "sensing" ***/
	/* Should replace plev below with something skill related */

	/* No sensing when confused */
	if (p_ptr->confused) return;

	if (cp_ptr->flags & CF_PSEUDO_ID1) 
			delay1 = 100000L / (p_ptr->lev + 10);
	if (cp_ptr->flags & CF_PSEUDO_ID2) 
			delay1 = 60000L / (p_ptr->lev + 10);
	if (cp_ptr->flags & CF_PSEUDO_ID3) 
			delay1 = 80000L / ((p_ptr->lev * p_ptr->lev) + 50);
	if (cp_ptr->flags & CF_PSEUDO_ID4) 
			delay1 = 10000L / ((p_ptr->lev * p_ptr->lev) + 50);

	delay2 = delay1 / 40;

	if (delay1 < 5) delay1 = 5;
	if (delay2 < 3) delay2 = 3;

	/* Check to see if detected anything worn */
	if (!(rand_int(delay2)))
	{	
		/*** Sense your wielded slots ***/
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			okay = FALSE;
	
			o_ptr = &inventory[i];
	
			/* Skip empty slots */
			if (!o_ptr->k_idx) continue;
	
			/* Valid "tval" codes */
			switch (o_ptr->tval)
			{
				case TV_GUN:
				case TV_DIGGING:
				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				case TV_DAGGER:
				case TV_AXES:
				case TV_BLUNT:			
				case TV_BOOTS:
				case TV_GLOVES:
				case TV_HELM:
				case TV_CROWN:
				case TV_LEG:
				case TV_CLOAK:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:
				case TV_MECHA_TORSO:
				case TV_MECHA_HEAD:
				case TV_MECHA_ARMS:
				case TV_MECHA_FEET:
				{
					okay = TRUE;
					break;
				}
				case TV_RING:
				case TV_AMULET:
				case TV_LITE:
				{
					/* It has already been sensed, do not sense it again */
					if (o_ptr->ident & (IDENT_SENSE))
					{
						/* Small chance of fully learning the item's abilities */
						if ((!o_ptr->name1) && !(object_known_p(o_ptr)))
						{
							if(!rand_int(delay2)) 
							{
								object_aware(o_ptr);
								object_known(o_ptr);
													
								/* Recalculate bonuses */
								p_ptr->update |= (PU_BONUS);
	
								/* Combine / Reorder the pack (later) */
								p_ptr->notice |= (PN_COMBINE | PN_REORDER);
						
								/* Window stuff */
								p_ptr->window |= (PW_INVEN | PW_EQUIP);
								/* Description */
								object_desc(o_name, o_ptr, TRUE, 3);
							
								/* Describe - Must be wielded.*/
								msg_format("%^s: %s (%c).",
									           describe_use(i), o_name, index_to_label(i));
							}
						}
					}
					else (o_ptr->ident |= (IDENT_SENSE));				
					continue;
				}
			}
	
			/* Skip irrelevant items*/
			if (!okay) continue;
	
			/* It has already been sensed, do not sense it again */
			if (o_ptr->ident & (IDENT_SENSE)) 
			{
				/* Small chance of fully learning the item's abilities */
				if ((!o_ptr->name1) && !(object_known_p(o_ptr)))
				{
					if(!rand_int(delay2)) 
					{
						object_aware(o_ptr);
						object_known(o_ptr);
											
						/* Recalculate bonuses */
						p_ptr->update |= (PU_BONUS);
	
						/* Combine / Reorder the pack (later) */
						p_ptr->notice |= (PN_COMBINE | PN_REORDER);
				
						/* Window stuff */
						p_ptr->window |= (PW_INVEN | PW_EQUIP);
						/* Description */
						object_desc(o_name, o_ptr, TRUE, 3);
					
						/* Describe - Must be wielded.*/
						msg_format("%^s: %s (%c).",
							           describe_use(i), o_name, index_to_label(i));
					}
				}
				continue;
			}
	
			/* It already has a discount or special inscription, except "indestructible" */
			if ((o_ptr->discount > 0) && !(o_ptr->discount == INSCRIP_INDESTRUCTIBLE)) continue;
	
			/* It is fully known, no information needed */
			if (object_known_p(o_ptr)) continue;
	
			/* Indestructible objects are either excellent or terrible */
			if (o_ptr->discount == INSCRIP_INDESTRUCTIBLE)
				heavy = TRUE;
	
			/* Check for a feeling */
			feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));
	
			/* Skip non-feelings */
			if (!feel) continue;
	
			/* Get an object description */
			object_desc(o_name, o_ptr, FALSE, 0);
	
			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);
	
			/* Sense the object */
			o_ptr->discount = feel;
	
			msg_format("You feel the %s (%c) you are %s %s %s...",
				           o_name, index_to_label(i), describe_use(i),
				           ((o_ptr->number == 1) ? "is" : "are"),
				           inscrip_text[feel - INSCRIP_NULL]);
	
	
			/* Stop everything */
			if (disturb_minor) disturb(0, 0);
	
			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	
			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP);
		}
	}
	
	/* Check to see if detected anything in the pack */
	if (rand_int(delay1)) return;
	
	/* Check everything */
	for (i = 0; i < INVEN_PACK; i++)
	{
		okay = FALSE;

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Valid "tval" codes */
		switch (o_ptr->tval)
		{
			case TV_AMMO:
			case TV_BULLET:
			case TV_SHOT:
			case TV_GUN:
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_DAGGER:
			case TV_AXES:
			case TV_BLUNT:			
			case TV_BOOTS:
			case TV_GLOVES:
			case TV_HELM:
			case TV_CROWN:
			case TV_LEG:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_MECHA_TORSO:
			case TV_MECHA_HEAD:
			case TV_MECHA_ARMS:
			case TV_MECHA_FEET:
			{
				okay = TRUE;
				break;
			}
		}

		/* Skip irrelevant items */
		if (!okay) continue;

		/* It already has a discount or special inscription, except "indestructible" */
		if ((o_ptr->discount > 0) && !(o_ptr->discount == INSCRIP_INDESTRUCTIBLE)) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is fully known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* Occasional failure on inventory items */
		if (!rand_int(5)) continue;

		/* Indestructible objects are either excellent or terrible */
		if (o_ptr->discount == INSCRIP_INDESTRUCTIBLE)
			heavy = TRUE;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Get an object description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Sense the object */
		o_ptr->discount = feel;

		/* The object has been "sensed" */
		o_ptr->ident |= (IDENT_SENSE);

		msg_format("You feel the %s (%c) in your pack %s %s...",
					o_name, index_to_label(i),
					((o_ptr->number == 1) ? "is" : "are"),
					inscrip_text[feel - INSCRIP_NULL]);

		/* Stop everything */
		if (disturb_minor) disturb(0, 0);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}



/*
 * Regenerate hit points
 *
 * The basic rate of hitpoint recovery is 30/10000ths per hitpoint, plus
 * 250/10000.  Therefore, a character with 100 maximum hitpoints will
 * recover (30 * 100) + 250/ 10000 = 3250 / 10000, or about a third of a
 * hitpoint every 10 game turns under normal conditions.
 *
 * By comparison, most monsters with 100 maximum hitpoints regain 1 hit-
 * point every 100 game turns (some monsters regenerate at twice this).
 *
 * Sangband tested, Steam Approved!
 */
static void regenhp(int percent)
{
	s32b temp;
	
	/* Save the old hitpoints */
	int old_chp = p_ptr->chp;

	/* Multiply max HP by "amount", then add the base regeneration */
	temp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;
	
	/* Add stored fractional regeneration */
	temp += p_ptr->chp_frac;
	
	/* Additional HPs are 1/10000th of the result */
	p_ptr->chp += temp / 10000;
	
	/* Store any fractional regeneration */
	p_ptr->chp_frac = temp % 10000;
	
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
 * Regenerate wound points
 */
static void regenwp(int percent)
{
	s32b temp;
	
	/* Save the old hitpoints */
	int old_cwp = p_ptr->cwp;

	/* Multiply max HP by "amount", then add the base regeneration */
	temp = ((long)p_ptr->mwp) * percent + PY_REGEN_WPBASE;
	
	/* Add stored fractional regeneration */
	temp += p_ptr->cwp_frac;
	
	/* Additional HPs are 1/10000th of the result */
	p_ptr->cwp += temp / 10000;
	
	/* Store any fractional regeneration */
	p_ptr->cwp_frac = temp % 10000;
	
	/* Fully healed */
	if (p_ptr->cwp >= p_ptr->mwp)
	{
		p_ptr->cwp = p_ptr->mwp;
		p_ptr->cwp_frac = 0;
	}

	/* Notice changes */
	if (old_cwp != p_ptr->cwp)
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
	s32b temp;

	/* Save the old hitpoints */
	int old_csp = p_ptr->csp;


	/* Multiply max mana by "amount", then add the base regeneration */
	temp = (long)p_ptr->msp * percent + PY_REGEN_MNBASE;

	/* Add stored fractional regeneration */
	temp += p_ptr->csp_frac;

	/* Additional mana points are 1/10000th of the result */
	p_ptr->csp += temp / 10000;

	/* Store any fractional regeneration */
	p_ptr->csp_frac = temp % 10000;


	/* Fully healed */
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
 * This function does one of my favorite things, takes
 * a bad situation and makes it worse. Mostly it punishes
 * the player for doing things besides resting while they
 * are very injured. It happens infrequently enough to not
 * result in death, and frequently enough to cause a bit of
 * a scare to an uncautious player.
 * 
 * Now that I've added the skill pain tolerance, I'm going 
 * to increase the penalties for wounds a bit to make it worth
 * diverting some skill points too.
 *
 * The fate roll has values from 10-70 (possibly higher or lower)
 * with numbers weighted at 40, producing number 25-55 70% of the time.
 * It has the percent of wps subtracted from it, as well as half the remaining
 * hit point percentage. (if you're at 3/10 wound points, and fullhps, your 
 * fate will likely be at below 0. (40-30-50)
 *
 * Because I'm a dick (and other factors) the fate roll now has values
 * from 0-120 (possibly higher or lower) with numbers weighted at 60,
 * producing numbers 40-80 70% (66%) of the time. It still has the 
 * percents subtrated from it. The reason this change was made is
 * that it was damaging a percentage of *total* hit points, instead
 * of a percentage of *current* points. It's now likely to be less
 * total damage, but now it will happen ever slighly more often. 12/3/07
 *
 * Notes on standard deivation. Values within 1 standard deivation
 * occur 66% of the time. Values within 2 standard deivations occur
 * 95% of the time. Values within 3 standard deivations occur 99.7%
 * of the time. Any values of s16b are possible.
 * 
 */
 
static void wheel_of_pain(void)
{
	int percent, hp_percent, fate, dam;
	
	int pain_tolerance = 0;
	
	bool automata;
	
	if (p_ptr->skills[SK_PAIN_TOLERANCE].skill_max > 0)
	{
		pain_tolerance = p_ptr->skills[SK_PAIN_TOLERANCE].skill_rank;
	}

	
	/* determine living state */
	if ((p_ptr->prace == RACE_AUTOMATA) || 
		(p_ptr->prace == RACE_STEAM_MECHA))
	{
		automata = TRUE;
	}
	else automata = FALSE;

	
	/* Percent is the % of remaining wound points */
	percent = 100 * p_ptr->cwp / p_ptr->mwp;
	
	/* HP_percent is the % of remaining hit points */
	hp_percent = 100 * p_ptr->chp / p_ptr->mhp;
	
	/* Kick out of this function if the character is resting */
	if (p_ptr->resting) return;
	
	/* base 10% chance of badness (1/10) */
	if (one_in_((10 + pain_tolerance)))
	{
		/* we have a % chance based off our wounds for a bad effect */
		/* i.e. if we are 40% wounded (12/20wp) we have a 40% chance of a fate roll */
		if (randint(100) > percent)
		{
	
			/* Get a fate roll */
			fate = Rand_normal(60, 20) - percent - (hp_percent/2) - (pain_tolerance * 2);


			if (fate < 0)
			{
				/* nothing happens */
			}
			else if (fate < 10)
			{
				/* Disturb */
				disturb(1, 0);
	
				message(MSG_BELL, 0, "Your wounds ache.");
		
				/* find out the damage */
				dam = (p_ptr->chp * 10 / 100) + 1;
				
				take_hit(dam, "your wounds", FALSE);	
			}
			else if (fate < 20)
			{
				/* Disturb */
				disturb(1, 0);
	
				/* find out the damage */
				dam = (p_ptr->chp * 10 / 100) + 2;
	
				message(MSG_BELL, 0, "You start bleeding from your wounds.");
				(void)set_cut(p_ptr->cut + (dam) + 2);
			}
			else if (fate < 30)
			{
				/* Disturb */
				disturb(1, 0);

				message(MSG_BELL, 0, "Your wounds sap your strength.");

				if (automata)
				{
						automata_equipment_decay(randint(2));
				}
				else
				{
						dec_stat(A_VIG, rand_range(20, 40), FALSE);
						dec_stat(A_MUS, rand_range(10, 20), FALSE);
				}		
			}
			else if (fate < 40)
			{
				/* Disturb */
				disturb(1, 0);
	
				message(MSG_BELL, 0, "Your wounds hurt.");

				/* find out the damage */
				dam = (p_ptr->chp * 30 / 100) + 5;
				
				take_hit(dam, "your wounds", TRUE);
			}
			else if (fate < 50)
			{
				/* Disturb */
				disturb(1, 0);
				

				if (automata)
				{
						automata_equipment_decay(2 + randint(2));
				}
				else
				{
						message(MSG_BELL, 0, "Your wounds sap your strength.");
				
						dec_stat(A_VIG, rand_range(40, 80), FALSE);
						dec_stat(A_MUS, rand_range(20, 40), FALSE);
				}						
			}
			else if (fate < 70)
			{
				/* Disturb */
				disturb(1, 0);				
	
				message(MSG_BELL, 0, "Your wounds hurt!");
	
				/* find out the damage */
				dam = (p_ptr->chp * 50 / 100) + 10;
				
				take_hit(dam, "your wounds", TRUE);
			}
			else if (fate < 80)
			{
				/* Disturb */
				disturb(1, 0);
				
				
				if (automata)
				{
						automata_equipment_decay(4 + randint(4));
				}
				else
				{
						message(MSG_BELL, 0, "Your wounds sap your strength.");

						dec_stat(A_VIG, rand_range(80, 120), FALSE);
						dec_stat(A_MUS, rand_range(40, 80), FALSE);
				}						
			}
			else
			{
				/* Disturb */
				disturb(1, 0);
	
				/* find out the damage */
				dam = (p_ptr->chp * 40 / 100) + 5;
	
				message(MSG_BELL, 0, "Your wounds break open and start bleeding!");

				(void)set_cut(p_ptr->cut + (dam * 2));
			}
		}
	}
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
	int i, j;
	u32b f1, f2, f3;
	u32b p1, p2, p3;
	int k = 0;

	int regen_amount, mutiplier;
	int upkeep_factor = 0;
	int monster_creation_chance = MAX_M_ALLOC_CHANCE;
	object_type *o_ptr;

	player_flags(&p1, &p2, &p3);

	/* We decrease noise slightly every game turn */
	total_wakeup_chance -= 400;

	/* But the character always makes some noise */
	if (total_wakeup_chance < p_ptr->base_wakeup_chance)
	    total_wakeup_chance = p_ptr->base_wakeup_chance;


	/*Stop being Wonderland once in the town! -- from Gumband */
	if (p_ptr->wonderland && !p_ptr->depth)
	{
		msg_print("Ahhhh! You have finally reached the town!");
	
		p_ptr->wonderland = FALSE;
		p_ptr->was_wonderland = TRUE;
		p_ptr->max_depth = p_ptr->depth;
		msg_print("Unfortunately, that means you'll have to walk back out again...");
	}

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
				msg_print("The gates to Steamband are closing...");
				msg_print("Please finish up and/or save your game.");
			}

			/* Slam the gate */
			else
			{
				/* Message */
				msg_print("The gates to Steamband are now closed.");

				/* Stop playing */
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/*** Attempt timed autosave.  From Zangband. ***/
	if (autosave_freq)
	{
		if (!(turn % (autosave_freq * 10L)))
		{
			do_cmd_save_game(TRUE);
		}
	}

	/*** Update quests ***/
	if ((p_ptr->cur_quest) && !(turn % (10L * QUEST_TURNS)))
	{
		quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

		/* Check for failure */
		if ((p_ptr->cur_quest != p_ptr->depth) && (one_in_(2)))
		{
			/* Check if quest is in progress */
			if (q_ptr->started && q_ptr->active_level &&
				((q_ptr->type == QUEST_GUILD) || (q_ptr->type == QUEST_UNIQUE)))
				quest_fail();
		}
	}


	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
	if (!p_ptr->depth)
	{
		/* Illuminate */
		town_illuminate(TRUE);
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
				/* Skip the home */
				if (n == STORE_HOME) continue;

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
	/*** Vary Monster Intensity between three states ***/
	if (turn % 1000)
	{
			k++;
			if (k = 1) monster_creation_chance = MAX_M_ALLOC_CHANCE + 50;
			else if (k = 2) monster_creation_chance = MAX_M_ALLOC_CHANCE;
			else if (k = 3) monster_creation_chance = MAX_M_ALLOC_CHANCE - 50;
			else if (k = 4) monster_creation_chance = MAX_M_ALLOC_CHANCE + 75;
			else if (k = 5) monster_creation_chance = MAX_M_ALLOC_CHANCE;
			else 
			{
					monster_creation_chance = MAX_M_ALLOC_CHANCE - 75;
					k = 0;
			}
	}	

	/* Check for creature generation */
	if (rand_int(monster_creation_chance) == 0)
	{
		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, FALSE);
	}

	/*** Damage over Time ***/

	/* Take damage from poison */
	if (p_ptr->poisoned)
	{
		/* Take damage */
		take_hit(randint(p_ptr->poisoned / 2), "poison", TRUE);
	}

	/* if the player is in wraithform, take damage */
	if (!cave_floor_bold(p_ptr->py, p_ptr->px))
	{
		if ((!(p_ptr->tim_wraith) && ((p_ptr->chp) > ((p_ptr->lev)/10))) || 
		(p_ptr->wraith_form))
		{
			cptr dam_desc;

			if ((p_ptr->prace == RACE_GHOST))
			{
				/* All ghosts have etheric attune */
				int ghostform = p_ptr->skills[SK_ETHERIC_ATTUNE].skill_rank;
				
				/* The ghost doesn't die from wraithform 10 + (20 * 25) = 510 */
				if (p_ptr->chp > (10 + (ghostform * p_ptr->lev / 2)))
				{
					int damage = 0;
					int lev = p_ptr->lev;

					/* Ghosts very eaisly reduce damage */
					damage = (2 + ((p_ptr->lev) / 5)) - ghostform;

					/* always do at least 1 point of damage */
					if (damage < 1) damage = 1;

					msg_print("Your molecules feel disrupted!");
					dam_desc = "density";

					/* Taking damage from wraithform */
					take_hit(damage, dam_desc, FALSE);
				}
			}
			else
			{
				msg_print("You are being crushed!");
				dam_desc = "solid rock";
				/* Taking damage from wraithform */
				take_hit(2 + ((p_ptr->lev)/5), dam_desc, FALSE);
			}
		}
	}


	/* Take damage from cuts */
	if (p_ptr->cut)
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->cut > 200) i = 5;

		/* Severe cut */
		else if (p_ptr->cut > 100) i = 3;

		/* Other cuts */
		else i = 1;

		/* Take damage */
		take_hit(i, "a fatal wound", TRUE);
	}

	/* Any time we're wounded, our day can get worse */
	if (p_ptr->cwp < p_ptr->mwp) wheel_of_pain();

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
			if (p_ptr->regenerate_25) i += 20;
			if (p_ptr->regenerate_50) i += 30;
			if (p_ptr->regenerate_75) i += 40;

			/* Slow digestion takes less food */
			if (p_ptr->slow_digest) i -= 50;

			if (p_ptr->prace == RACE_GIANT) i *= 2;
			if (p_ptr->prace == RACE_OGRE) i += i / 4;
			if (p_ptr->prace == RACE_TROLL) i += i / 2;
			if (p_ptr->prace == RACE_GHOST) i = i / 3;
			if ((p_ptr->prace == RACE_GOBLIN) ||
				(p_ptr->prace == RACE_SEELIE_FAE) ||
				(p_ptr->prace == RACE_UNSEELIE_FAE))
			{
			 	i = i / 2;
			}

			/* Minimal digestion */
			if (i < 5) i = 5;
			
			/* Stay sane */
			if (i > 150) i = 150;
			
			/* What's a stomach? */
			if (p_ptr->prace == RACE_OLD_ONE) 
				(void)set_food(PY_FOOD_FULL - 1);
			
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
		take_hit(i, "starvation", TRUE);
	}

	/* Default regeneration - quicker to rest in town. */
	if (p_ptr->depth) regen_amount = PY_REGEN_NORMAL;
	else regen_amount = PY_REGEN_TOWN;
	
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

	/* Iron men - due to their inability to return to town */
	/* Automatically get a *slow* wound point regeneration rate */
	if (adult_ironman) mutiplier = 2;
	else mutiplier = 1;
	
	/* Regeneration ability */
	if (p_ptr->regenerate_25)
	{
		mutiplier += 2;
	}
	if (p_ptr->regenerate_50)
	{
		mutiplier += 4;
	}
	if (p_ptr->regenerate_75)
	{
		mutiplier += 6;
	}
	if (p_ptr->skills[SK_SPIRIT_HEALING].skill_max > 0)
	{
		mutiplier += (((p_ptr->skills[SK_SPIRIT_HEALING].skill_rank / 5) + 1));
	}
	if (p_ptr->skills[SK_WATER_LORE].skill_max > 1)
	{
		mutiplier += (((p_ptr->skills[SK_WATER_LORE].skill_rank + 4) / 6));
	}
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max > 1)
	{
		mutiplier += ((p_ptr->skills[SK_WATER_MASTERY].skill_rank / 5));
	}
	if (p_ptr->skills[SK_BATTLE_ENDURANCE].skill_max > 1)
	{
		mutiplier += ((p_ptr->skills[SK_BATTLE_ENDURANCE].skill_rank / 5));
	}
	
	/* You're going to get a number for a mutiplier between 1-20 or so */
	/* Take regen amount (currently 66) and add between 20-200% of that number */
	/* to itself, regen rate then = 66-198 or thereabouts, or the in-town regen # */
	if (mutiplier > 1) regen_amount = regen_amount + ((regen_amount * mutiplier) / 100);
	
	/* Searching or Resting */
	if (p_ptr->searching || p_ptr->resting)
	{
			regen_amount = regen_amount * 2;
	}
	
	/* Setting upkeep factor for pets */
	if (total_friends > 1 + (p_ptr->lev / cp_ptr->pet_upkeep_div))
	{
		upkeep_factor = total_friend_levels;

		if (upkeep_factor > 100) upkeep_factor = 100;
		else if (upkeep_factor < 10) upkeep_factor = 10;
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

	/* Various things interfere with healing */
	if (p_ptr->paralyzed) regen_amount = 0;
	if (p_ptr->poisoned) regen_amount = 0;
	if (p_ptr->stun) regen_amount = 0;
	if (p_ptr->cut) regen_amount = 0;

	/* Regenerate Hit Points if needed */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(regen_amount);
	}
	if (!p_ptr->depth) regenwp(regen_amount);
	
	/* This is key, regenwp isn't even called if mutiplier <= 1 */
	if (regen_amount && (mutiplier > 1)) regenwp(mutiplier);

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
	/* Timed ESP */
	if (p_ptr->tim_esp) 
	{
		(void)set_tim_esp(p_ptr->tim_esp - 1);
	}
	/* Times See-Invisible */
	if (p_ptr->tim_invis)
	{
		(void)set_tim_invis(p_ptr->tim_invis - 1);
	}
	/* Timed Light */
	if (p_ptr->tim_light)
	{
		(void)set_tim_light(p_ptr->tim_light - 1);
	}
	/* Timed Demonic Spellcasting */
	if (p_ptr->tim_demonspell)
	{
		(void)set_tim_demonspell(p_ptr->tim_demonspell - 1);
	}
	/* Timed Demonic Health */
	if (p_ptr->tim_demonhealth)
	{
		(void)set_tim_demonhealth(p_ptr->tim_demonhealth - 1);
	}
	/* Timed Wormsense */
	if (p_ptr->tim_wormsense)
	{
		(void)set_tim_wormsense(p_ptr->tim_wormsense - 1);
	}
	/* Timed Voorish Power */
	if (p_ptr->tim_voorish)
	{
		(void)set_tim_voorish(p_ptr->tim_voorish - 1);
	}
	/* Timed Stygian Power */
	if (p_ptr->tim_stygian)
	{
		(void)set_tim_stygian(p_ptr->tim_stygian - 1);
	}
	/* Timed Muscle */
	if (p_ptr->tim_muscle)
	{
		(void)set_tim_muscle(p_ptr->tim_muscle - 1);
	}
	/* Timed Vigor */
	if (p_ptr->tim_vigor)
	{
		(void)set_tim_vigor(p_ptr->tim_vigor - 1);
	}
	/* Timed No Teleport */
	if (p_ptr->tim_no_tele)
	{
		(void)set_tim_no_tele(p_ptr->tim_no_tele - 1);
	}
	/* Timed Free Action */
	if (p_ptr->tim_free_act)
	{
		(void)set_tim_free_act(p_ptr->tim_free_act - 1);
	}
	/* Timed Anti Magic */
	if (p_ptr->tim_anti_magic)
	{
		(void)set_tim_anti_magic(p_ptr->tim_anti_magic - 1);
	}
	/* Timed Infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}
	/* Steam-Mecha Armor */
	if (p_ptr->tim_harding)
	{
		(void)set_tim_harding(p_ptr->tim_harding - 1);
	}
	/* Steam-Mecha Armor */
	if (p_ptr->tim_evade)
	{
		(void)set_tim_evade(p_ptr->tim_evade - 1);
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
	if (p_ptr->tim_wraith)
	{
		(void)set_shadow(p_ptr->tim_wraith - 1);
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
	
	/* Invisiblity */
	if (p_ptr->tim_invisiblity)
	{
		(void)set_tim_invisiblity(p_ptr->tim_invisiblity - 1);
	}
	
	/* Timed Resistances */
	for (i = 0; i < RS_MAX; i++)
	{
		if (p_ptr->tim_res[i])	(void)set_tim_res(i, p_ptr->tim_res[i] - 1);
	}
	
	/*** Steamware Research Times ***/
	if (p_ptr->eyes_research)
	{
		(void)set_tim_eyes_research(p_ptr->eyes_research - 1);
	}
	if (p_ptr->reflex_research)
	{
		(void)set_tim_reflex_research(p_ptr->reflex_research - 1);
	}
	if (p_ptr->plate_research)
	{
		(void)set_tim_plate_research(p_ptr->plate_research - 1);
	}
	if (p_ptr->core_research)
	{
		(void)set_tim_core_research(p_ptr->core_research - 1);
	}
	if (p_ptr->spur_research)
	{
		(void)set_tim_spur_research(p_ptr->spur_research - 1);
	}

	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		/* @STAT@ */
		int adjust = ((p_ptr->stat_use[A_VIG] / 80) + 1);

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		/* @STAT@ */
		int adjust = ((p_ptr->stat_use[A_VIG] / 80) + 1);

		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		/* @STAT@ */
		int adjust = ((p_ptr->stat_use[A_VIG] / 80) + 1);

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut > 1000) adjust = 0;

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}



	/*** Process Lights ***/


	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Burn some fuel in the current light, unless it doesn't need fuel */
		if ((o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NO_FUEL))))
		{
			/* Hack -- Use some fuel */
			if (o_ptr->pval > 0)
			{
				/* Decrease life-span */
				o_ptr->pval--;

				/* Hack -- notice interesting fuel steps */
				if ((o_ptr->pval < 100) || (!(o_ptr->pval % 20)))
				{
					/* Window stuff */
					p_ptr->window |= (PW_EQUIP);
				}

				/* The light is now out */
				if (o_ptr->pval == 0)
				{
					/* Hack -- Special treatment when blind */
					if (p_ptr->blind)
					{
						o_ptr->pval++;
					}
					else
					{
						disturb(0, 0);
						msg_print("Your light has gone out!");

						/* Calculate torch radius */
						p_ptr->update |= (PU_TORCH);
					}
				}
				
				/* Torch dims more */
				else if ((o_ptr->sval == SV_LITE_TORCH) && ((o_ptr->pval > 0)) && (o_ptr->pval == FUEL_TORCH_ONE))
				{
					disturb(0, 0);
					msg_print("Your light dims a little.");
				}
				/* Latern dims a bit more */
				else if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval == FUEL_LAMP_TWO))
				{
					disturb(0, 0);
					msg_print("Your light dims a bit.");
				}
				/* Lantern on its way out */
				else if ((o_ptr->sval == SV_LITE_LANTERN) && ((o_ptr->pval > 0)) && (o_ptr->pval == FUEL_LAMP_ONE))
				{
					disturb(0, 0);
					msg_print("Your light dims some more.");
				}
				/* The light is getting dim */
				else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 20)))
				{
					if (disturb_minor) disturb(0, 0);
					msg_print("Your light is growing faint.");
				}
			}
		}
	}

	/* Calculate torch radius */
	p_ptr->update |= (PU_TORCH);

	/*
	 * Process the effects of mutations (in mutation.c), if you have any
	 * that might need to be checked -- Gumby
	 *
	 * heh. Doubled the number of available mutations.
	 * Doubled! -CCC
	 */
	if (p_ptr->muta3 || p_ptr->muta4)
	{
		process_mutations();
	}

	if (cp_ptr->flags & CF_MUTABLE || (p3 & (TR3_MUTABLE)))
	{
		if ((p_ptr->prace == RACE_AUTOMATA) || 
			(p_ptr->prace == RACE_STEAM_MECHA) ||
			(p_ptr->prace == RACE_GHOST))
		{
			/* nothing */
		}
		else if(!rand_int(15000))
		{
			if(randint(100) < 80) gain_random_mutation(0);
			else lose_mutation(0);
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

	/* Need to make this better - seperate function for these checks */
	/* Handle hp draining */
	if (p_ptr->hp_drain)
	{
		if (!(p_ptr->resting))
		{
			int drain = randint(p_ptr->mhp / 10);
			p_ptr->chp -= (drain < p_ptr->chp ? drain : p_ptr->chp);
			/* Redraw */
			p_ptr->redraw |= (PR_HP);

			/* Window stuff */
			 p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		}
	}


	/* Handle sp draining */
	if (p_ptr->sp_drain)
	{
		if (!(p_ptr->resting))
		{
			int drain = randint(p_ptr->msp / 10);
			p_ptr->csp -= (drain < p_ptr->csp ? drain : p_ptr->csp);

			/* Redraw */
			p_ptr->redraw |= (PR_MANA);

			/* Window stuff */
			 p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

		}
	}


	/* Handle item draining */

	/* Process equipment */
	/* This likely isn't working XCCCX */
	for (j = 0, i = 0; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Recharge */
			o_ptr->timeout--;

			/* Notice changes */
			if (!(o_ptr->timeout)) j++;
		}
	}

	/* Notice changes */
	if (j)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Recharge apparatuses */
	for (j = 0, i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Examine all charging apparatuses */
		if ((o_ptr->tval == TV_APPARATUS) && (o_ptr->pval))
		{
			/* Charge it */
			o_ptr->pval--;

			/* Notice changes */
			if (!(o_ptr->pval)) j++;
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
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge apparatuses on the ground */
		if ((o_ptr->tval == TV_APPARATUS) && (o_ptr->pval)) o_ptr->pval--;
	}


	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && (rand_int(500) < 1))
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
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth < 1) p_ptr->depth = 1;

				/* Leaving */
				p_ptr->leaving = TRUE;
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
			do_cmd_destroy(0);
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
			do_cmd_observe(NULL, FALSE);
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

		/* This is confusing - see command-misc for the explination */
		/* Go up staircase */
		case '>':
		{
			do_cmd_go_up();
			break;
		}

		/* This is confusing - see command-misc for the explination */
		/* Go down staircase */
		case '<':
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
		/* 
		 * This will be changed to 'gain a level' b/c skills will determine
		 * what spells you get from objects (books + spells)
		 */
		case 'G':
		{
			/* do_cmd_study(); */
			do_cmd_gain_level(FALSE);
			break;
		}

		/* Browse a book */
		case 'b':
		{
				do_cmd_browse();
			break;
		}

		/* Cast a spell, use a device */
		case 'm':
		{
			do_cmd_magic();
			break;
		}

		/* Use a class 'p'ower */
		case 'p':
		{
			do_cmd_mind();
			break;
		}
		
		/* Activate Pets */
		case 'P':
		{
			do_cmd_pet();
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
			use_object(TV_FOOD);
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
			do_cmd_fire(0);
			break;
		}

		/* Throw an item */
		case 'v':
		{
			do_cmd_throw();
			break;
		}

		/* Aim a ray gun */
		case 'a':
		{
			use_device(TV_RAY);
			break;
		}

		/* Zap a apparatus */
		case 'z':
		{
			use_device(TV_APPARATUS);
			break;
		}

		/* Quaff a tonic */
		case 'q':
		{
			use_object(TV_TONIC);
			break;
		}

		/* Rig a mechanism */
		case 'r':
		{
			use_object(TV_MECHANISM);
			break;
		}

		/* Use a tool */
		case 'u':
		{
			use_device(TV_TOOL);
			break;
		}

		/* Activate a racial power */
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

 #if 0
 		/* Switch monster memory format */
		case '\t':
		{
			if (p_ptr->monster_mem_fmt == FALSE)
				p_ptr->monster_mem_fmt = TRUE;
			else
				p_ptr->monster_mem_fmt = FALSE;
			p_ptr->window |= (PW_MONSTER);
			break;
		}
#endif
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

	static u32b	old_r_flags1 = 0L;
	static u32b	old_r_flags2 = 0L;
	static u32b	old_r_flags3 = 0L;
	static u32b	old_r_flags4 = 0L;
	static u32b	old_r_flags5 = 0L;
	static u32b	old_r_flags6 = 0L;
	static u32b old_r_flags7 = 0L;
	static u32b old_r_flags8 = 0L;

	static byte	old_r_blows0 = 0;
	static byte	old_r_blows1 = 0;
	static byte	old_r_blows2 = 0;
	static byte	old_r_blows3 = 0;

	static byte	old_r_ranged = 0;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

		/* Check for change of any kind */
		if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (old_r_flags1 != l_ptr->r_flags1) ||
		    (old_r_flags2 != l_ptr->r_flags2) ||
		    (old_r_flags3 != l_ptr->r_flags3) ||
		    (old_r_flags4 != l_ptr->r_flags4) ||
		    (old_r_flags5 != l_ptr->r_flags5) ||
		    (old_r_flags6 != l_ptr->r_flags6) ||
		    (old_r_flags7 != l_ptr->r_flags7) ||
		    (old_r_flags8 != l_ptr->r_flags8) ||
		    (old_r_blows0 != l_ptr->r_blows[0]) ||
		    (old_r_blows1 != l_ptr->r_blows[1]) ||
		    (old_r_blows2 != l_ptr->r_blows[2]) ||
		    (old_r_blows3 != l_ptr->r_blows[3]) ||
		    (old_r_ranged != l_ptr->r_ranged))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_r_flags1 = l_ptr->r_flags1;
			old_r_flags2 = l_ptr->r_flags2;
			old_r_flags3 = l_ptr->r_flags3;
			old_r_flags4 = l_ptr->r_flags4;
			old_r_flags5 = l_ptr->r_flags5;
			old_r_flags6 = l_ptr->r_flags6;
			old_r_flags7 = l_ptr->r_flags7;
			old_r_flags8 = l_ptr->r_flags8;

			/* Memorize blows */
			old_r_blows0 = l_ptr->r_blows[0];
			old_r_blows1 = l_ptr->r_blows[1];
			old_r_blows2 = l_ptr->r_blows[2];
			old_r_blows3 = l_ptr->r_blows[3];

			/* Memorize castings */
			old_r_ranged = l_ptr->r_ranged;

			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);

			/* Window stuff */
			window_stuff();
		}
	}
}

/*
 * Some refreshes are done before a character takes his turn.  -LM-
 *
 * We update extra monster health bars, and insure that enough quest
 * monsters exist.
 */
static void refresh_monsters_before(void)
{
	int i;

	/* Remove "show" flags from all monsters */
	if (repair_mflag_show)
	{
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr;

			/* Get the monster */
			m_ptr = &m_list[i];

			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_SHOW);
		}

		/* Done */
		repair_mflag_show = FALSE;
	}

	/* Refresh the list of visible monsters */
	p_ptr->window |= PW_VISIBLE;

#if 0
	/* Make sure that enough quest monsters exist  XXX XXX */
	if ((p_ptr->cur_quest) && (p_ptr->depth == p_ptr->cur_quest))
	{
		insure_quest_monsters();
	}
#endif
}



/*
 * We refresh monsters after every character action that uses energy.  -LM-
 *
 * All monsters detected here, or during that action, will stay visible
 * until after the character's next energy-using action.
 */
static void refresh_monsters_after(void)
{
	int i;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Note whether we need to update distances */
	bool do_dist = (p_ptr->update & (PU_DISTANCE)) ? TRUE : FALSE;


	/* We handle monster visibility and distances here */
	p_ptr->update &= ~(PU_MONSTERS | PU_DISTANCE);

	/* Refresh the list of visible monsters */
	p_ptr->window |= PW_VISIBLE;

	/* Update stuff (especially the character's field of vision) */
	handle_stuff();


	/* Remove "mark" flags from all monsters not magically detected */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];

		/* Monster has been magically detected */
		if (m_ptr->mflag & MFLAG_SHOW)
		{
			/* Repair "show" flags at the beginning of next turn */
			repair_mflag_show = TRUE;
		}
		else
		{
			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_FULL | MFLAG_MARK);
		}
	}


	/* Process the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Handle visibility changes due to telepathy, sensing, detection. */
		if (update_mon(i, do_dist, TRUE))
		{
			continue;
		}

		/* Handle color changes due to shimmering */
		else if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) &&
		         (mon_fully_visible(m_ptr)))
		{
			/* Reraw the monster */
			lite_spot(m_ptr->fy, m_ptr->fx);
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
	if (hack_mutation)
	{
		msg_print("You feel different!");
		(void)gain_random_mutation(0);
		hack_mutation = FALSE;
	}


	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if (!p_ptr->depth)
			{
				if ((p_ptr->chp == p_ptr->mhp) &&
			   		(p_ptr->csp == p_ptr->msp) &&
			    	(p_ptr->cwp == p_ptr->mwp))
			    {
			    	disturb(0, 0);
				}
			}
			
			else if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* In town wound points heal */
			if (!p_ptr->depth)
			{
				if ((p_ptr->chp == p_ptr->mhp) &&
				    (p_ptr->csp == p_ptr->msp) &&
				    (p_ptr->cwp == p_ptr->mwp) &&
				    !p_ptr->blind && !p_ptr->confused &&
				    !p_ptr->poisoned && !p_ptr->afraid &&
				    !p_ptr->stun && !p_ptr->cut &&
				    !p_ptr->slow && !p_ptr->paralyzed &&
				    !p_ptr->image && !p_ptr->word_recall)
				{
					disturb(0, 0);
				}
			}
			/* Stop resting */
			else if ((p_ptr->chp == p_ptr->mhp) &&
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

	/*** Handle certain refreshes right before player moves ***/

	/* Refresh monsters as needed */
	refresh_monsters_before();


	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	do
	{
		/* It's your turn again and counterstrike is turned off */
		p_ptr->counter = FALSE;

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
			object_desc(o_name, o_ptr, TRUE, 3);

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

		}
	}

	while (!p_ptr->energy_use && !p_ptr->leaving);

	/* Hack -- constant hallucination */
	if (p_ptr->image) p_ptr->redraw |= (PR_MAP);

	/* Refresh monsters */
	refresh_monsters_after();

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
	update_noise(FALSE);

	/* Update scent trail */
	update_smell();


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


	/* No stairs down from fixed quests */
	if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
		(quest_check(p_ptr->depth) == QUEST_FIXED_U))
	/* No stairs down from Quest or Wonderland Mode */
	if (((quest_check(p_ptr->depth) == QUEST_FIXED) ||
		(quest_check(p_ptr->depth) == QUEST_FIXED_U)) || 
		((p_ptr->wonderland) && (p_ptr->depth == 48)))
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
			/* XXX XXX XXX */
			delete_object(py, px);

			/* Make stairs */
			if ((p_ptr->create_down_stair) && ((p_ptr->wonderland) && (p_ptr->depth == 48)))
			{
				cave_set_feat(py, px, FEAT_LESS);
			}
			else if (p_ptr->create_down_stair)
			{
				cave_set_feat(py, px, FEAT_MORE);
			}
			else
			{
				cave_set_feat(py, px, FEAT_LESS);
			}

			/* Mark the stairs as known */
			cave_info[py][px] |= (CAVE_MARK);
		}

		/* Cancel the stair request */
		p_ptr->create_down_stair = p_ptr->create_up_stair = FALSE;
	}


	/* Choose panel */
	verify_panel();


	/* Flush messages */
	message_flush();


	/* Hack -- Increase "xtra" depth */
	/* WTF?!? -CCC */
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

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);

	/* Redraw dungeon */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_VISIBLE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Redraw stuff */
	window_stuff();


	/* Hack -- Decrease "xtra" depth */
	/* RE:wtf!?! -CCC */
	character_xtra--;


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


	/* Handle delayed death */
	if (p_ptr->is_dead) return;


	/* Mark quest as started */
	if (p_ptr->cur_quest == p_ptr->depth)
	{
		int i;

		/* Check quests */
		for (i = 0; i < z_info->q_max; i++)
		{
			/* Check for quest */
			if (q_info[i].active_level == p_ptr->depth)
			{
				q_info[i].started = TRUE;
				break;
			}
		}
	}

	/* Announce (or repeat) the feeling */
	/* if (p_ptr->depth) do_cmd_feeling(); */

    if (p_ptr->depth)
	{
    	do_cmd_feeling();

		/* Update the level indicator */
		p_ptr->redraw |= (PR_DEPTH);
	}

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
			/* Process monster with even more energy first */
			process_monsters((byte)(p_ptr->energy + 1));

			/* If still alive */
			if (!p_ptr->leaving)
			{
				/* Process the player */
				process_player();
			}
		}

		total_friends = 0;
		total_friend_levels = 0;

		/* Process other things that happen during a game turn */
		for (i = 0;; i++)
		{

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

			/* Monsters (any that haven't had a chance to move yet) */
			if (i == 0)
			{
				/* Process all of the monsters */
				process_monsters(0);
				
				/* Reset monsters */
				reset_monsters();
			}

			/* Special effects */
			else if (i == 1)
			{
				/* Process effects */
				process_effects();
			}
			
			/* The world */
			else if (i == 2)
			{
				/* Process the world */
				process_world();
			}

			/* Count game turns, end this turn */
			else
			{
				/* Count game turns */
				turn++;

				/* End this game turn */
				break;
			}
		}

		/* Handle "leaving" */
		if (p_ptr->leaving) break;
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

	/* Get the "PLAYER.prf" filename */
	sprintf(buf, "%s.prf", op_ptr->base_name);

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
	/* I hope I don't have to figure out what this does -ccc */
	hack_mutation = FALSE;

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

	/* Forbid resizing */
	/* Term->fixed_shape = TRUE; */


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

/* #ifdef GJW_RANDART */

		/* Hack -- seed for random artifacts */
/*		seed_randart = rand_int(0x10000000); */

/* #endif  GJW_RANDART */

		/* Roll up a new character */
		player_birth();

/* #ifdef GJW_RANDART */

		/* Randomize the artifacts */
/*		if (adult_rand_artifacts)
		{
			do_randart(seed_randart, TRUE);
		}
*/
/* #else  GJW_RANDART */

		/* Make sure random artifacts are turned off if not available */
		adult_rand_artifacts = FALSE;

/* #endif  GJW_RANDART */

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

		/* Quest items on quest levels */
		if ((p_ptr->cur_quest) && (quest_check(p_ptr->cur_quest) == QUEST_VAULT))
		{
			quest_type *q_ptr = &q_info[quest_num(p_ptr->cur_quest)];

			/* Check if had already been on quest level */
			if (q_ptr->started)
			{
				int i;
				object_type *o_ptr;
				bool fail_quest = TRUE;

				/* Check if player has a quest item in his inventory */
				for (i = 0; i < INVEN_PACK; i++)
				{
					o_ptr = &inventory[i];

					/* Found the quest item */
					if (o_ptr->ident & IDENT_QUEST)
					{
						fail_quest = FALSE;
						break;
					}
				}

				if (fail_quest) quest_fail();
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
				
				/* Restore wound points */
				p_ptr->cwp = p_ptr->mwp;
				p_ptr->cwp_frac = 0;

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
					message_flush();

					/* Hack -- Prevent recall */
					p_ptr->word_recall = 0;
				}

				/* Note cause of death XXX XXX XXX */
				strcpy(p_ptr->died_from, "Cheating death");

				/* Check wonderland status for new depth */
				if (p_ptr->wonderland)
					p_ptr->depth = 48;
				else
					p_ptr->depth = 0;


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
}
