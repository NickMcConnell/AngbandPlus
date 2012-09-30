/* File: cmd1.c */

/* Purpose: Movement commands (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
#define MAX_VAMPIRIC_DRAIN 100





/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
static bool test_hit_combat(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant hit.  Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if ((chance > 0) && (rand_int(chance) >= ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/* At the moment this function is exactly the same as the melee function */
bool test_hit_fire(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant hit.  Chance to miss removed in Oangband because
	 * of the way monster ACs now work (fewer truly easy targets).
	 */
	if (k < 5) return (TRUE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = chance / 2;

	/* Power competes against armor. */
	if ((chance > 0) && (rand_int(chance) >= ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Calculation of critical hits by the player in hand-to-hand combat. -LM-
 */
static sint critical_melee(int chance, int sleeping_bonus, char m_name[], object_type *o_ptr)
{
	int i, k;
	int mult_m_crit;

	/* Extract melee attack power. */
	i = (chance + sleeping_bonus);

	/* Test for critical hit. */
	if (randint(i + 200) <= i)
	{
		/* Encourage the player to make sneak attacks on
		 * sleeping monsters. -LM-
		 */
		if ((sleeping_bonus) && (p_ptr->pclass == CLASS_ROGUE))
			msg_print("You ruthlessly sneak attack!");


		/* Hack - Weapons that normally do little damage benefit most from
		 * critical hits (10x inflation).
		 */
		mult_m_crit = 120 / (o_ptr->dd * (o_ptr->ds + 1));
		if (mult_m_crit > 20) mult_m_crit = 20;
		if (mult_m_crit < 10) mult_m_crit = 10;


		/* Determine level of critical hit */
		k = randint(i) + randint(100);

		/* This portion of the function determines the level of critical hit,
		 * the critical mult_m_crit, and displays an appropriate combat
		 * message.  A distinction is often made between edged and blunt
		 * weapons.  Unfortunately, whips sometimes display rather odd
		 * messages...
		 */
		if (k < 100)
		{
			mult_m_crit *= 15;
			msg_format("You strike %s.", m_name);
		}
		else if (k < 160)
		{
			mult_m_crit *= 17;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				msg_format("You hack at %s.", m_name);
			else
				msg_format("You bash %s.", m_name);
		}
		else if (k < 210)
		{
			mult_m_crit *= 20;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				msg_format("You slash %s.", m_name);
		else
				msg_format("You pound %s.", m_name);
		}
		else if (k < 250)
		{
			mult_m_crit *= 23;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				msg_format("You score %s!", m_name);
			else
				msg_format("You batter %s!", m_name);
		}
		else if (k < 280)
		{
			mult_m_crit *= 27;

			if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM))
				msg_format("You gouge %s!", m_name);
			else
				msg_format("You bludgeon %s!", m_name);
		}
		else
		{
			mult_m_crit *= 32;
			msg_format("You *smite* %s!", m_name);
		}

		/*
		 * Compensate for the weak weapon bonus by deflating the critical
		 * hit multiplier.
		 */
		mult_m_crit /= 10;
	}

	/* If the blow is not a critical hit, display the default attack
	 * message and apply the standard multiplier.
	 */
	else
	{
		mult_m_crit = 10;
		msg_format("You hit %s.", m_name);
	}

	return (mult_m_crit);
}


/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 * This is used by the old Monk attack routine
 * mutuations, + a few of the ego weapons.
 */
static s16b critical_norm(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3));

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650);

		if (k < 400)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
		}
		else
		{
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}
	}

	return (dam);
}



/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x2, except Slay Animal (x1.7),
 * Slay Evil (x1.5), and Kill dragon (x3). -SF-
 */
s16b tot_dam_aux(object_type *o_ptr, int tdam, monster_type *m_ptr)
{

	/*
	* mult is scaled to be *10 so that the fractional slays can be stored
	* in an integer. -SF-
	*/
	int mult = 10;


	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Some "weapons" and "ammo" do extra damage */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Slay Animal */
			if ((f1 & TR1_SLAY_ANIMAL) &&
			    (r_ptr->flags3 & RF3_ANIMAL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ANIMAL;
				}

				if (mult < 17) mult = 17;
			}

			/* Slay Evil */
			if ((f1 & TR1_SLAY_EVIL) &&
			    (r_ptr->flags3 & RF3_EVIL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				if (mult < 15) mult = 15;
			}

			/* Slay Undead */
			if ((f1 & TR1_SLAY_UNDEAD) &&
			    (r_ptr->flags3 & RF3_UNDEAD))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_UNDEAD;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Demon */
			if ((f1 & TR1_SLAY_DEMON) &&
			    (r_ptr->flags3 & RF3_DEMON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DEMON;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Orc */
			if ((f1 & TR1_SLAY_ORC) &&
			    (r_ptr->flags3 & RF3_ORC))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_ORC;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Troll */
			if ((f1 & TR1_SLAY_TROLL) &&
			    (r_ptr->flags3 & RF3_TROLL))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_TROLL;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Giant */
			if ((f1 & TR1_SLAY_GIANT) &&
			    (r_ptr->flags3 & RF3_GIANT))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_GIANT;
				}

				if (mult < 20) mult = 20;
			}

			/* Slay Dragon */
			if ((f1 & TR1_SLAY_DRAGON) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 20) mult = 20;
			}

			/* Execute Dragon */
			if ((f1 & TR1_KILL_DRAGON) &&
			    (r_ptr->flags3 & RF3_DRAGON))
			{
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_DRAGON;
				}

				if (mult < 30) mult = 30;

				if ((o_ptr->name1 == ART_AEGLIN) &&
				    strstr(r_name + r_ptr->name, "Fafner"))
					mult *= 3;
			}

			/* Brand (Acid) */
			if (f1 & TR1_BRAND_ACID)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_ACID)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_ACID;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 20) mult = 20;
				}
			}

			/* Brand (Elec) */
			if (f1 & TR1_BRAND_ELEC)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_ELEC)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_ELEC;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 20) mult = 20;
				}
			}

			/* Brand (Fire) */
			if (f1 & TR1_BRAND_FIRE)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_FIRE)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_FIRE;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 20) mult = 20;
				}
			}

			/* Brand (Cold) */
			if (f1 & TR1_BRAND_COLD)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_COLD)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_COLD;
					}
				}
				/* Otherwise, take the damage */
				else
				{
					if (mult < 20) mult = 20;
				}
			}

			/* Brand (Poison) */
			if (f1 & TR1_BRAND_POIS)
			{
				/* Notice immunity */
				if (r_ptr->flags3 & RF3_IM_POIS)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_IM_POIS;
					}
				}

				/* Otherwise, take the damage */
				else
				{
					if (mult < 20) mult = 20;
				}
			}
			break;
		}
	}


	/* Return the total damage */
	return (tdam * mult);
}


/*
 * Search for hidden things
 */
void search(void)
{
	int y, x, chance;
	int old_count;
	
	/* 
	 * Temp variables to store x and y because the
	 * count_traps() function stomps on its inputs.
	 */
	int tx, ty;
	

	s16b this_o_idx, next_o_idx = 0;

	cave_type *c_ptr;


	/* Start with base search ability */
	chance = p_ptr->skill_srh;

	/* Penalize various conditions */
	if (p_ptr->blind || no_lite()) chance = chance / 10;
	if (p_ptr->confused || p_ptr->image) chance = chance / 10;

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* do not search outside the wilderness */
				if (!in_bounds2(y, x)) continue;

				/* Access the grid */
				c_ptr = area(y, x);

#ifdef USE_SCRIPT
				if (player_search_grid_callback(y, x))
				{
					/* Disturb */
					disturb(0, 0);

					return;
				}
#endif /* USE_SCRIPT */

				/* Save x and y into temp variables */
				tx = x;
				ty = y;

				/* Count number of visible traps next to player */
				old_count = count_traps(&ty, &tx, TRUE);

				/* Look for invisible traps */
				if (field_detect_type(c_ptr->fld_idx, FTYPE_TRAP))
				{
					/* Save x and y into temp variables */
					tx = x;
					ty = y;
										
					/* See if the number of known traps has changed */
					if (old_count != count_traps(&ty, &tx, TRUE))
					{
						/* Message */
						msg_print("You have found a trap.");

						/* Disturb */
						disturb(0, 0);
					}
				}

				/* Secret door */
				if (c_ptr->feat == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(y, x);
					
					/* Notice this */
					note_spot(y, x);
					
					/* Lite this */
					lite_spot(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}

/*
 * Determine if the object can be picked up, and has "=g" in its inscription.
 */
bool auto_pickup_okay(object_type *o_ptr)
{
	cptr s;

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	/* No inscription */
	if (!o_ptr->inscription) return (FALSE);

	/* Find a '=' */
	s = strchr(quark_str(o_ptr->inscription), '=');

	/* Process inscription */
	while (s)
	{
		/* Auto-pickup on "=g" */
		if (s[1] == 'g') return (TRUE);

		/* Find another '=' */
		s = strchr(s + 1, '=');
	}

	/* Don't auto pickup */
	return (FALSE);
}

/*
 * Helper routine for py_pickup().
 *
 * Add the given dungeon object to the character's inventory.
 *
 * Delete the object afterwards.
 */
void py_pickup_aux(int o_idx)
{
	int slot, i;

	char o_name[80];
	object_type *o_ptr;

	o_ptr = &o_list[o_idx];

	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Get the object again */
	o_ptr = &inventory[slot];

	/* Describe the object */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("You have %s (%c).", o_name, index_to_label(slot));

	/* Check if completed a quest */
	for (i = 0; i < max_quests; i++)
	{
		if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
			   (quest[i].k_idx == o_ptr->name1))
		{
			quest[i].status = QUEST_STATUS_COMPLETED;
			msg_print("You completed your quest!");
			msg_print(NULL);
		}
	}

	/* Delete the object */
	delete_object_idx(o_idx);
}

/*
 * Automatically destroy items in this grid.
 */
static void auto_destroy_items(cave_type *c_ptr)
{
	s16b this_o_idx, next_o_idx = 0;

	char o_name[80];


	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Known to be worthless? */
		if (destroy_worthless && (object_value(o_ptr) < 1))
		{
			/* Artifact? */
			if (!can_player_destroy_object(o_ptr))
			{
				/* Describe the object (with {terrible/special}) */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Message */
				msg_format("You cannot auto-destroy %s.", o_name);

				/* Done */
				return;
			}

			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Print a message */
			msg_format("Auto-destroying %s.", o_name);

			/* Destroy the item */
			delete_object_idx(this_o_idx);

			continue;
		}
	}
}


/*
 * Player "wants" to pick up an object or gold.
 * Note that we ONLY handle things that can be picked up.
 * See "move_player()" for handling of other things.
 *
 * If the easy floor option is true: 
 *		Make the player carry everything in a grid
 *
 * 		If "pickup" is FALSE then only gold will be picked up
 */
void carry(int pickup)
{
	s16b this_o_idx, next_o_idx;

	char o_name[80];
	object_type *o_ptr;

	int floor_num = 0, floor_list[23], floor_o_idx = 0;

	int can_pickup = 0;

	bool do_ask = TRUE;
	
	/* Recenter the map around the player */
	verify_panel();
	
	/* Update stuff */
	p_ptr->update |= PU_MONSTERS;
	
	/* Redraw map */
	p_ptr->redraw |= PR_MAP;
	
	/* Window stuff */
	p_ptr->window |= PW_OVERHEAD;
	
	/* Handle stuff */
	handle_stuff();
	
	/* Destroy worthless items below the player */
	auto_destroy_items(area(py, px));

	/* Scan the pile of objects */
	for (this_o_idx = area(py,px)->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Access the object */
		o_ptr = &o_list[this_o_idx];

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Access the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Pick up gold */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Message */
			msg_format("You have found %ld gold pieces worth of %s.",
				(long)o_ptr->pval, o_name);

			sound(SOUND_SELL);
			
			/* Collect the gold */
			p_ptr->au += o_ptr->pval;

			/* Redraw gold */
			p_ptr->redraw |= (PR_GOLD);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);

			/* Delete the gold */
			delete_object_idx(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* Test for auto-pickup */
		if (auto_pickup_okay(o_ptr))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Check the next object */
			continue;
		}

		/* The old pick-up routine is here */
		if (!easy_floor)
		{
			/* Describe the object */
			if (!pickup)
			{
				msg_format("You see %s.", o_name);
			}

			/* Note that the pack is too full */
			else if (!inven_carry_okay(o_ptr))
			{
				msg_format("You have no room for %s.", o_name);
			}

			/* Pick up the item (if requested and allowed) */
			else
			{
				int okay = TRUE;

				/* Hack -- query every item */
				if (carry_query_flag)
				{
					char out_val[160];
					sprintf(out_val, "Pick up %s? ", o_name);
					okay = get_check(out_val);
				}

				/* Attempt to pick up an object. */
				if (okay)
				{
					/* Pick up the object */
					py_pickup_aux(this_o_idx);
				}
			}
		
			/* Get the next object */
			continue;
		}


		/* Count non-gold objects that can be picked up. */
		if (inven_carry_okay(o_ptr))
		{
			can_pickup++;
		}

		/* Remember this object index */
		floor_list[floor_num] = this_o_idx;

		/* Count non-gold objects */
		if (floor_num != 22) floor_num++;

		/* Remember this index */
		floor_o_idx = this_o_idx;
	}

	/* There are no non-gold objects (or easy floor patch is off) */
	if (!floor_num) return;

	/* Mention the number of objects */
	if (!pickup)
	{
		/* One object */
		if (floor_num == 1)
		{
			/* Access the object */
			o_ptr = &o_list[floor_o_idx];

			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_format("You see %s.", o_name);
		}

		/* Multiple objects */
		else
		{
			/* Message */
			msg_format("You see a pile of %d items.", floor_num);
		}

		/* Done */
		return;
	}

	/* The player has no room for anything on the floor. */
	if (!can_pickup)
	{
		/* One object */
		if (floor_num == 1)
		{
			/* Access the object */
			o_ptr = &o_list[floor_o_idx];

			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_format("You have no room for %s.", o_name);
		}

		/* Multiple objects */
		else
		{
			/* Message */
			msg_print("You have no room for any of the objects on the floor.");
		}

		/* Done */
		return;
	}

	/* One object */
	if (floor_num == 1)
	{
		/* Hack -- query every object */
		if (carry_query_flag)
		{
			char out_val[160];

			/* Access the object */
			o_ptr = &o_list[floor_o_idx];

			/* Describe the object */
			object_desc(o_name, o_ptr, TRUE, 3);

			/* Build a prompt */
			(void)sprintf(out_val, "Pick up %s? ", o_name);

			/* Ask the user to confirm */
			if (!get_check(out_val))
			{
				/* Done */
				return;
			}
		}

		/* Don't ask */
		do_ask = FALSE;

		/* Remember the object to pick up */
		this_o_idx = floor_o_idx;
	}

	/* Allow the user to choose an object */
	if (do_ask)
	{
		cptr q, s;

		int item;

		/* Restrict the choices */
		item_tester_hook = inven_carry_okay;

		/* Get an object */
		q = "Get which item? ";
		s = "You see nothing there.";
		if (get_item(&item, q, s, (USE_FLOOR)))
		{
			this_o_idx = 0 - item;
		}
		else
		{
			return;
		}
	}

	/* Access the object */
	o_ptr = &o_list[this_o_idx];

	/* Pick up the object */
	py_pickup_aux(this_o_idx);
}



static void touch_zap_player(monster_type *m_ptr)
{
	int aura_damage;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags2 & RF2_AURA_FIRE)
	{
		if (!p_ptr->immune_fire)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very hot!");

			if (p_ptr->oppose_fire) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_fire) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_FIRE;
			handle_stuff();
		}
	}

	if (r_ptr->flags3 & RF3_AURA_COLD)
	{
		if (!p_ptr->immune_cold)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			msg_print("You are suddenly very cold!");

			if (p_ptr->oppose_cold) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_cold) aura_damage = (aura_damage + 2) / 3;

			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags3 |= RF3_AURA_COLD;
			handle_stuff();
		}
	}

	if (r_ptr->flags2 & RF2_AURA_ELEC)
	{
		if (!p_ptr->immune_elec)
		{
			char aura_dam[80];

			aura_damage = damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

			/* Hack -- Get the "died from" name */
			monster_desc(aura_dam, m_ptr, 0x88);

			if (p_ptr->oppose_elec) aura_damage = (aura_damage + 2) / 3;
			if (p_ptr->resist_elec) aura_damage = (aura_damage + 2) / 3;

			msg_print("You get zapped!");
			take_hit(aura_damage, aura_dam);
			r_ptr->r_flags2 |= RF2_AURA_ELEC;
			handle_stuff();
		}
	}
}


static void natural_attack(s16b m_idx, int attack, bool *fear, bool *mdeath)
{
	int             k, bonus, chance;
	int             n_weight;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];
	int             dss, ddd;
	char            *atk_desc;

	switch (attack)
	{
		case MUT2_SCOR_TAIL:
			dss = 3;
			ddd = 7;
			n_weight = 5;
			atk_desc = "tail";
			break;
		case MUT2_HORNS:
			dss = 2;
			ddd = 6;
			n_weight = 15;
			atk_desc = "horns";
			break;
		case MUT2_BEAK:
			dss = 2;
			ddd = 4;
			n_weight = 5;
			atk_desc = "beak";
			break;
		case MUT2_TRUNK:
			dss = 1;
			ddd = 4;
			n_weight = 35;
			atk_desc = "trunk";
			break;
		case MUT2_TENTACLES:
			dss = 2;
			ddd = 5;
			n_weight = 5;
			atk_desc = "tentacles";
			break;
		default:
			dss = ddd = n_weight = 1;
			atk_desc = "undefined body part";
	}

	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);


	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Test for hit */
	if ((!(r_ptr->flags2 & RF2_QUANTUM) || !rand_int(2)) &&
	    test_hit_combat(chance, r_ptr->ac, m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_HIT);

		msg_format("You hit %s with your %s.", m_name, atk_desc);

		k = damroll(ddd, dss);
		k = critical_norm(n_weight, p_ptr->to_h, k);

		/* Apply the player damage bonuses */
		k += p_ptr->to_d;

		/* No negative damage */
		if (k < 0) k = 0;

		/* Modify the damage */
		k = mon_damage_mod(m_ptr, k, 0);

		/* Complex message */
		if (wizard)
		{
			msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
		}

		/* Anger the monster */
		if (k > 0) anger_monster(m_ptr);

		/* Damage, check for fear and mdeath */
		switch (attack)
		{
			case MUT2_SCOR_TAIL:
				project(0, 0, m_ptr->fy, m_ptr->fx, k, GF_POIS, PROJECT_KILL);
				*mdeath = (m_ptr->r_idx == 0);
				break;
			case MUT2_HORNS:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_BEAK:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_TRUNK:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			case MUT2_TENTACLES:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
				break;
			default:
				*mdeath = mon_take_hit(m_idx, k, fear, NULL);
		}

		touch_zap_player(m_ptr);
	}
	/* Player misses */
	else
	{
		/* Sound */
		sound(SOUND_MISS);

		/* Message */
		msg_format("You miss %s.", m_name);
	}
}

/**** The monster bashing code. -LM- ****/
static bool monster_bash(int *blows, int sleeping_bonus, cave_type *c_ptr,
			bool *fear, char *m_name)
{
	int bash_chance, bash_quality, bash_dam;

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	/* No shield on arm, no bash. */
	if (!inventory[INVEN_ARM].k_idx)
	{
		bash_chance = 0;
	}

	/* Players do not bash if they could otherwise take advantage of special
	 * bonuses against sleeping monsters, or if the monster is low-level.
	 */
	else if ((sleeping_bonus) || (r_ptr->level < p_ptr->lev / 2))
	{
		bash_chance = 0;
	}

	/* Bashing chance depends on melee Skill, Dex, and a class level bonus. */
	else bash_chance = p_ptr->skill_thn +
	                   (adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128 +
	                   (((p_ptr->pclass == CLASS_WARRIOR) ||
	                     (p_ptr->pclass == CLASS_PALADIN) ||
	                     (p_ptr->pclass == CLASS_WARRIOR_MAGE) ||
	                     (p_ptr->pclass == CLASS_CHAOS_WARRIOR)) ? p_ptr->lev : 0);

	/* Players bash more often when they see a real need. */
	if (bash_chance)
	{
		if ((!inventory[INVEN_WIELD].k_idx) && (p_ptr->pclass != CLASS_MONK))
			bash_chance *= 3;
		else if ((inventory[INVEN_WIELD].dd * inventory[INVEN_WIELD].ds * (*blows))
			< (inventory[INVEN_ARM].dd * inventory[INVEN_ARM].ds * 3))
			bash_chance *= 2;
	}

	/* Try to get in a shield bash. */
	if (bash_chance > rand_int(240 + r_ptr->level * 9))
	{
		msg_print("You get in a shield bash!");

		/* Calculate attack quality, a mix of momentum and accuracy. */
		bash_quality = p_ptr->skill_thn + (p_ptr->wt / 8) +
			(p_ptr->total_weight / 80) + (inventory[INVEN_ARM].weight / 3);

		/* Calculate damage.  Big shields are deadly. */
		bash_dam = damroll(inventory[INVEN_ARM].dd, inventory[INVEN_ARM].ds);

		/* Multiply by quality and experience factors */
		bash_dam *= bash_quality / 20 + p_ptr->lev / 7;

		/* Strength bonus. */
		bash_dam += (adj_str_td[p_ptr->stat_ind[A_STR]] - 128);

		/* Paranoia. */
		if (bash_dam > 125) bash_dam = 125;

		/* Encourage the player to keep wearing that heavy shield. */
		if (randint(bash_dam) > 30 + randint(bash_dam / 2))
			msg_print("WHAMM!");

		/* Complex message */
		if (wizard)
		{
			msg_format("You do %d (out of %d) damage.", bash_dam, m_ptr->hp);
		}

		/* Damage, check for fear and death. */
		if (mon_take_hit(c_ptr->m_idx, bash_dam, fear, NULL))
		{
			/* Fight's over. */
			return (TRUE);
		}

		/* Stunning. */
		if (bash_quality + p_ptr->lev > randint(200 + r_ptr->level * 8))
		{
			msg_format("%^s is stunned.", m_name);

			m_ptr->stunned += rand_int(p_ptr->lev / 5) + 4;
			if (m_ptr->stunned > 24) m_ptr->stunned = 24;
		}

		/* Confusion. */
		if (bash_quality + p_ptr->lev > randint(300 + r_ptr->level * 6) &&
			(!r_ptr->flags3 & (RF3_NO_CONF)))
		{
			msg_format("%^s appears confused.", m_name);

			m_ptr->confused += rand_int(p_ptr->lev / 5) + 4;
		}

		/* The player will sometimes stumble. */
		if ((30 + adj_dex_th[p_ptr->stat_ind[A_DEX]] - 128) < randint(60))
			*blows -= randint(*blows);
	}

	/* Monster is not dead */
	return (FALSE);
}

/*
 * The monk special attacks and effects.
 */

static void monk_attack(monster_type *m_ptr, long *k, char *m_name)
{
	int special_effect = 0, stun_effect = 0, times = 0;
	martial_arts *ma_ptr = &ma_blows[0], *old_ptr = &ma_blows[0];
	int resist_stun = 0;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags1 & RF1_UNIQUE) resist_stun += 88;
	if (r_ptr->flags3 & RF3_NO_CONF) resist_stun += 44;
	if (r_ptr->flags3 & RF3_NO_SLEEP) resist_stun += 44;
	if ((r_ptr->flags3 & RF3_UNDEAD) || (r_ptr->flags3 & RF3_NONLIVING))
		resist_stun += 88;

	/* Attempt 'times' */
	for (times = 0; times < (p_ptr->lev < 7 ? 1 : p_ptr->lev / 7); times++)
	{
		do
		{
			ma_ptr = &ma_blows[rand_int(MAX_MA)];
		}
		while ((ma_ptr->min_level > p_ptr->lev) ||
			(randint(p_ptr->lev) < ma_ptr->chance));

		/* keep the highest level attack available we found */
		if ((ma_ptr->min_level > old_ptr->min_level) &&
			!p_ptr->stun && !p_ptr->confused)
		{
			old_ptr = ma_ptr;

			if (wizard && cheat_xtra)
			{
				msg_print("Attack re-selected.");
			}
		}
		else
		{
			ma_ptr = old_ptr;
		}
	}

	*k = damroll(ma_ptr->dd, ma_ptr->ds);

	if (ma_ptr->effect == MA_KNEE)
	{
		if (r_ptr->flags1 & RF1_MALE)
		{
			msg_format("You hit %s in the groin with your knee!", m_name);
			sound(SOUND_PAIN);
			special_effect = MA_KNEE;
		}
		else
			msg_format(ma_ptr->desc, m_name);
	}

	else if (ma_ptr->effect == MA_SLOW)
	{
		if (!((r_ptr->flags1 & RF1_NEVER_MOVE) ||
			strchr("~#{}.UjmeEv$,DdsbBFIJQSXclnw!=?", r_ptr->d_char)))
		{
			msg_format("You kick %s in the ankle.", m_name);
			special_effect = MA_SLOW;
		}
		else msg_format(ma_ptr->desc, m_name);
	}
	else
	{
		if (ma_ptr->effect)
		{
			stun_effect = (ma_ptr->effect / 2) + randint(ma_ptr->effect / 2);
		}

		msg_format(ma_ptr->desc, m_name);
	}

	*k = critical_norm(p_ptr->lev * randint(10), ma_ptr->min_level, *k);

	if ((special_effect == MA_KNEE) && ((*k + p_ptr->to_d) < m_ptr->hp))
	{
		msg_format("%^s moans in agony!", m_name);
		stun_effect = 7 + randint(13);
		resist_stun /= 3;
	}

	else if ((special_effect == MA_SLOW) && ((*k + p_ptr->to_d) < m_ptr->hp))
	{
		if (!(r_ptr->flags1 & RF1_UNIQUE) &&
			(randint(p_ptr->lev) > r_ptr->level) &&
			m_ptr->mspeed > 60)
		{
			msg_format("%^s starts limping slower.", m_name);
			m_ptr->mspeed -= 10;
		}
	}

	if (stun_effect && ((*k + p_ptr->to_d) < m_ptr->hp))
	{
		if (p_ptr->lev > randint(r_ptr->level + resist_stun + 10))
		{
			if (m_ptr->stunned)
				msg_format("%^s is more stunned.", m_name);
			else
				msg_format("%^s is stunned.", m_name);

			m_ptr->stunned += stun_effect;
		}
	}
}


/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	/* Number of dice, also total damage. */
	long k;

	/* The whole and fractional damage dice and their resulting damage. */
	int k_remainder, k_whole;

	/* blow count */
	int num = 0;

	/* Bonus to attack if monster is sleeping, for certain classes. */
	int sleeping_bonus = 0;

	/* Bonus to effective monster ac if it can take cover in terrain. */
	int terrain_bonus = 0;

	int bonus, chance, total_deadliness;

	int blows;

	cave_type       *c_ptr = area(y,x);

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type     *o_ptr;

	char            m_name[80];


	bool            fear = FALSE;
	bool            mdeath = FALSE;

	bool            vorpal_cut = FALSE;
	int             chaos_effect = 0;
	bool            do_quake = FALSE;
	bool            drain_msg = TRUE;
	int             drain_result = 0, drain_heal = 0;
	int             drain_left = MAX_VAMPIRIC_DRAIN;
	u32b            f1, f2, f3; /* A massive hack -- life-draining weapons */
	bool            no_extra = FALSE;


	/* Disturb the player */
	disturb(0, 0);

	/* Initial blows available. */
	blows = p_ptr->num_blow;

	if (m_ptr->csleep) /* It is not honorable etc to attack helpless victims */
	{
		chg_virtue(V_COMPASSION, -1);
		if (!(p_ptr->pclass == CLASS_ROGUE)) chg_virtue(V_HONOUR, -1);
	}

	if (p_ptr->pclass == CLASS_ROGUE)
	{
		if (m_ptr->csleep && m_ptr->ml)
		{
			/* Can't backstab creatures that we can't see, right? */
			sleeping_bonus = 10 + 2 * p_ptr->lev / 5;
		}
		else if (m_ptr->monfear && m_ptr->ml)
		{
			sleeping_bonus = 5 + p_ptr->lev / 5;
		}
	}

	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Extract monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);

	/* Stop if friendly */
	if (!is_hostile(m_ptr) &&
	    !(p_ptr->stun || p_ptr->confused || p_ptr->image ||
	    ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero) ||
	    !m_ptr->ml))
	{
		if (!inventory[INVEN_WIELD].art_name)
		{
			msg_format("You stop to avoid hitting %s.", m_name);
			return;
		}

		if (!(streq(quark_str(inventory[INVEN_WIELD].art_name), "'Stormbringer'")))
		{
			msg_format("You stop to avoid hitting %s.", m_name);
			return;
		}

		msg_format("Your black blade greedily attacks %s!", m_name);

		chg_virtue(V_INDIVIDUALISM, 1);
		chg_virtue(V_HONOUR, -1);
		chg_virtue(V_JUSTICE, -1);
		chg_virtue(V_COMPASSION, -1);
	}


	/* Handle player fear */
	if (p_ptr->afraid)
	{
		/* Message */
		if (m_ptr->ml)
			msg_format("You are too afraid to attack %s!", m_name);
		else
			msg_format ("There is something scary in your way!");

		/* Done */
		return;
	}

	/* Monsters in rubble can take advantage of cover. -LM- */
	if (c_ptr->feat == FEAT_RUBBLE)
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/*
	 * Monsters in trees can take advantage of cover,
	 * except from rangers.
	 */
	else if ((c_ptr->feat == FEAT_TREES) &&
	         (p_ptr->pclass == CLASS_RANGER))
	{
		terrain_bonus = r_ptr->ac / 7 + 5;
	}
	/* Monsters in water are vulnerable. -LM- */
	else if (c_ptr->feat == FEAT_DEEP_WATER)
	{
		terrain_bonus -= r_ptr->ac / 5;
	}

	/* Attempt to shield bash the monster */
	if (monster_bash(&blows, sleeping_bonus, c_ptr, &fear, m_name)) return;


	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Initialize. */
	total_deadliness = p_ptr->to_d + o_ptr->to_d;

	/* Paranoia.  Ensure legal table access. */
	if (total_deadliness > 150) total_deadliness = 150;

	/* Calculate the "attack quality".  As BTH_PLUS_ADJ has been reduced
	 * to 1, base skill and modifiers to skill are given equal weight. -LM-
	 */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Attack once for each legal blow */
	while (num++ < blows)
	{
		/* Test for hit */
		if (test_hit_combat(chance + sleeping_bonus,
		                    r_ptr->ac + terrain_bonus, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Hack -- bare hands do one damage */
			k = 1;

			object_flags(o_ptr, &f1, &f2, &f3);

			/* Select a chaotic effect (50% chance) */
			if ((f1 & TR1_CHAOTIC) && (randint(2) == 1))
			{
				if (randint(10) == 1)
					chg_virtue(V_CHANCE, 1);

				if (randint(5) < 3)
				{
					/* Vampiric (20%) */
					chaos_effect = 1;
				}
				else if (randint(250) == 1)
				{
					/* Quake (0.12%) */
					chaos_effect = 2;
				}
				else if (randint(10) != 1)
				{
					/* Confusion (26.892%) */
					chaos_effect = 3;
				}
				else if (randint(2) == 1)
				{
					/* Teleport away (1.494%) */
					chaos_effect = 4;
				}
				else
				{
					/* Polymorph (1.494%) */
					chaos_effect = 5;
				}
			}

			/* Vampiric drain */
			if ((f1 & TR1_VAMPIRIC) || (chaos_effect == 1))
			{
				/* Only drain "living" monsters */
				if (monster_living(r_ptr))
					drain_result = m_ptr->hp;
				else
					drain_result = 0;
			}

			if ((f1 & TR1_VORPAL) && (randint((o_ptr->name1 == ART_VORPAL_BLADE) ? 3 : 6) == 1))
				vorpal_cut = TRUE;
			else vorpal_cut = FALSE;

			if ((p_ptr->pclass == CLASS_MONK) && monk_empty_hands())
			{
				/* Make a special monk attack */
				monk_attack(m_ptr, &k, m_name);
			}
			
			/* Handle normal weapon */
			else if (o_ptr->k_idx)
			{
				/* base damage dice. */
				k = o_ptr->dd;

				/* multiply by slays or brands. (10x inflation) */
				k = tot_dam_aux(o_ptr, k, m_ptr);

				/* multiply by critical hit. (10x inflation) */
				k *= critical_melee(chance, sleeping_bonus, m_name, o_ptr);

				/* Convert total Deadliness into a percentage, and apply
				 * it as a bonus or penalty. (100x inflation)
				 */
				if (total_deadliness > 0)
					k *= (100 + deadliness_conversion[total_deadliness]);
				else if (total_deadliness > -31)
					k *= (100 -
					deadliness_conversion[ABS(total_deadliness)]);
				else
					k = 0;

				/* Get the whole number of dice by deflating the result. */
				k_whole = k / 10000;

				/* Calculate the remainder (the fractional die, x10000). */
				k_remainder = k % 10000;


				/* Calculate and combine the damages of the whole and
				 * fractional dice.
				 */
				k = damroll(k_whole, o_ptr->ds) +
					(k_remainder * damroll(1, o_ptr->ds) / 10000);

				/* hack -- check for earthquake. */
				if (p_ptr->impact && (k > 49)) do_quake = TRUE;



				if ((p_ptr->impact && ((k > 50) || randint(7) == 1)) ||
					 (chaos_effect == 2))
				{
					do_quake = TRUE;
				}

				if (vorpal_cut)
				{
					/*
					 * The vorpal blade does average:
					 *	(e+2)/3 x normal damage.
					 * A normal weapon with the vorpal flag does average:
					 *   e-3/2 x normal damage.
					 * Note: this has changed from before - the vorpal blade
					 *  has been toned down because of the oangband based
					 *  combat.
					 */
					int mult = 2;

					int inc_chance = (o_ptr->name1 == ART_VORPAL_BLADE) ? 2 : 4;

					if ((o_ptr->name1 == ART_CHAINSWORD) && (randint(2) != 1))
					{
						char chainsword_noise[1024];
						if (!get_rnd_line("chainswd.txt", 0, chainsword_noise))
						{
							msg_print(chainsword_noise);
						}
					}

					if (o_ptr->name1 == ART_VORPAL_BLADE)
					{
						msg_print("Your Vorpal Blade goes snicker-snack!");
					}
					else
					{
						msg_format("Your weapon cuts deep into %s!", m_name);
					}

					/* Try to increase the damage */
					while (one_in_(inc_chance))
					{
						mult++;
						inc_chance++;
					}

					k *= mult;

					/* Ouch! */
					if (k > m_ptr->hp)
					{
						msg_format("You cut %s in half!", m_name);
					}
					else
					{
						switch(mult)
						{
							case 2: msg_format("You gouge %s!", m_name); break;
							case 3: msg_format("You maim %s!", m_name); break;
							case 4: msg_format("You carve %s!", m_name); break;
							case 5: msg_format("You cleave %s!", m_name); break;
							case 6: msg_format("You smite %s!", m_name); break;
							case 7: msg_format("You eviscerate %s!", m_name); break;
							default: msg_format("You shred %s!", m_name); break;
						}
					}
				}
			}
			
			/* Bare hands and not a monk */
			else
			{
				msg_format("You punch %s", m_name); break;			
			}

			/* No negative damage */
			if (k < 0) k = 0;

			/* Modify the damage */
			k = mon_damage_mod(m_ptr, k, 0);

			/* Complex message */
			if (wizard)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(c_ptr->m_idx, k, &fear, NULL))
			{
				/* Hack -- High-level warriors can spread their attacks out
				 * among weaker foes. -LM-
				 */
				if (((p_ptr->pclass == CLASS_WARRIOR) ||
				     (p_ptr->pclass == CLASS_CHAOS_WARRIOR)) &&
				     (p_ptr->lev > 39) && (num < p_ptr->num_blow) &&
					(energy_use))
				{
					energy_use = energy_use * num / p_ptr->num_blow;
				}

				mdeath = TRUE;
				break;
			}

			/* Anger the monster */
			if (k > 0) anger_monster(m_ptr);

			touch_zap_player(m_ptr);

			/* Are we draining it?  A little note: If the monster is
			dead, the drain does not work... */

			if (drain_result)
			{
				drain_result -= m_ptr->hp;  /* Calculate the difference */

				if (drain_result > 0) /* Did we really hurt it? */
				{
					drain_heal = damroll(4, drain_result / 6);

					if (cheat_xtra)
					{
						msg_format("Draining left: %d", drain_left);
					}

					if (drain_left)
					{
						if (drain_heal < drain_left)
						{
							drain_left -= drain_heal;
						}
						else
						{
							drain_heal = drain_left;
							drain_left = 0;
						}

						if (drain_msg)
						{
							msg_format("Your weapon drains life from %s!", m_name);
							drain_msg = FALSE;
						}

						drain_heal = (drain_heal * mutant_regenerate_mod) / 100;

						hp_player(drain_heal);
						/* We get to keep some of it! */
					}
				}
			}

			/* Confusion attack */
			if (p_ptr->confusing || (chaos_effect == 3))
			{
				/* Cancel glowing hands */
				if (p_ptr->confusing)
				{
					p_ptr->confusing = FALSE;
					msg_print("Your hands stop glowing.");
					p_ptr->redraw |= (PR_STATUS);
				}

				/* Confuse the monster */
				if (r_ptr->flags3 & RF3_NO_CONF)
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= RF3_NO_CONF;
					}

					msg_format("%^s is unaffected.", m_name);
				}
				else if (rand_int(100) < r_ptr->level)
				{
					msg_format("%^s is unaffected.", m_name);
				}
				else
				{
					msg_format("%^s appears confused.", m_name);
					m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
				}
			}

			else if (chaos_effect == 4)
			{
				bool resists_tele = FALSE;

				if (r_ptr->flags3 & RF3_RES_TELE)
				{
					if (r_ptr->flags1 & RF1_UNIQUE)
					{
						if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
						msg_format("%^s is unaffected!", m_name);
						resists_tele = TRUE;
					}
					else if (r_ptr->level > randint(100))
					{
						if (m_ptr->ml) r_ptr->r_flags3 |= RF3_RES_TELE;
						msg_format("%^s resists!", m_name);
						resists_tele = TRUE;
					}
				}

				if (!resists_tele)
				{
					msg_format("%^s disappears!", m_name);
					teleport_away(c_ptr->m_idx, 50);
					num = p_ptr->num_blow + 1; /* Can't hit it anymore! */
					no_extra = TRUE;
				}
			}

			else if ((chaos_effect == 5) && cave_floor_grid(c_ptr) &&
			         (randint(90) > r_ptr->level))
			{
				if (!(r_ptr->flags1 & RF1_UNIQUE) &&
				    !(r_ptr->flags4 & RF4_BR_CHAO) &&
				    !(r_ptr->flags1 & RF1_QUESTOR))
				{
					if (polymorph_monster(y, x))
					{
						msg_format("%^s changes!", m_name);

						/* Hack -- Get new monster */
						m_ptr = &m_list[c_ptr->m_idx];

						/* Oops, we need a different name... */
						monster_desc(m_name, m_ptr, 0);

						/* Hack -- Get new race */
						r_ptr = &r_info[m_ptr->r_idx];

						fear = FALSE;
					}
					else
					{
						msg_format("%^s is unaffected.", m_name);
					}
				}
			}
		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			/* Message */
			msg_format("You miss %s.", m_name);
		}
	}


	/* Mutations which yield extra 'natural' attacks */
	if (!no_extra)
	{
		if ((p_ptr->muta2 & MUT2_HORNS) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_HORNS, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_BEAK) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_BEAK, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_SCOR_TAIL) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_SCOR_TAIL, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_TRUNK) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TRUNK, &fear, &mdeath);
		if ((p_ptr->muta2 & MUT2_TENTACLES) && !mdeath)
			natural_attack(c_ptr->m_idx, MUT2_TENTACLES, &fear, &mdeath);
	}

	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}

	if (drain_left != MAX_VAMPIRIC_DRAIN)
	{
		if (randint(4) == 1)
			chg_virtue(V_VITALITY, 1);
	}

	/* Mega-Hack -- apply earthquake brand */
	if (do_quake)
	{
		earthquake(py, px, 10);
	}
}


static void summon_pattern_vortex(int y, int x)
{
	int i;


	/* Find the pattern vortex */
	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Summon it */
		if (strstr(r_name + r_ptr->name, "Pattern") && (r_ptr->d_char == 'v'))
		{
			if (summon_named_creature(y, x, i, FALSE, FALSE, FALSE))
			{
				msg_print("You hear a bell chime.");
			}

			break;
		}
	}
}


static bool pattern_tile(int y, int x)
{
	return ((area(y,x)->feat <= FEAT_PATTERN_XTRA2) &&
	        (area(y,x)->feat >= FEAT_PATTERN_START));
}


static bool pattern_seq(int c_y, int c_x, int n_y, int n_x)
{
	if (!pattern_tile(c_y, c_x) && !pattern_tile(n_y, n_x))
		return TRUE;

	if (area(n_y,n_x)->feat == FEAT_PATTERN_START)
	{
		if (!pattern_tile(c_y, c_x) &&
			 !p_ptr->confused && !p_ptr->stun && !p_ptr->image)
		{
			if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
				return TRUE;
			else
				return FALSE;
		}
		else
			return TRUE;
	}
	else if ((area(n_y, n_x)->feat == FEAT_PATTERN_OLD) ||
				(area(n_y, n_x)->feat == FEAT_PATTERN_END) ||
				(area(n_y, n_x)->feat == FEAT_PATTERN_XTRA2))
	{
		if (pattern_tile(c_y, c_x))
		{
			return TRUE;
		}
		else
		{
			if (get_check("Really step onto the Pattern here? "))
			{
				take_hit(100, "Stepping onto the Pattern");

				if (one_in_(3)) summon_pattern_vortex(n_y, n_x);

				return TRUE;
			}
			else
			{
				return FALSE;
			}
		}
	}
	else if ((area(n_y, n_x)->feat == FEAT_PATTERN_XTRA1) ||
				(area(c_y, c_x)->feat == FEAT_PATTERN_XTRA1))
	{
		return TRUE;
	}
	else if (area(c_y, c_x)->feat == FEAT_PATTERN_START)
	{
		if (pattern_tile(n_y, n_x))
			return TRUE;
		else
		{
			if (get_check("Really step off of the Pattern? "))
			{
				take_hit(10, "Stepping off of the Pattern");

				if (one_in_(6)) summon_pattern_vortex(n_y, n_x);

				return TRUE;
			}

			return FALSE;
		}
	}
	else if ((area(c_y,c_x)->feat == FEAT_PATTERN_OLD) ||
				(area(c_y,c_x)->feat == FEAT_PATTERN_END) ||
				(area(c_y,c_x)->feat == FEAT_PATTERN_XTRA2))
	{
		if (!pattern_tile(n_y, n_x))
		{
			if (get_check("Really step off of the Pattern? "))
			{
				take_hit(100, "Stepping off of the Pattern");

				if (one_in_(2)) summon_pattern_vortex(n_y, n_x);

				return TRUE;
			}
			else
			{
				return FALSE;
			}
		}
		else
		{
			return TRUE;
		}
	}
	else
	{
		if (!pattern_tile(c_y, c_x))
		{
			if (get_check("Really step onto the Pattern here? "))
			{
				take_hit(25, "Stepping onto the Pattern");

				if (one_in_(6)) summon_pattern_vortex(n_y, n_x);

				return TRUE;
			}
			else
			{
				return FALSE;
			}
		}
		else
		{
			byte ok_move = FEAT_PATTERN_START;

			switch (area(c_y,c_x)->feat)
			{
				case FEAT_PATTERN_1:
					ok_move = FEAT_PATTERN_2;
					break;
				case FEAT_PATTERN_2:
					ok_move = FEAT_PATTERN_3;
					break;
				case FEAT_PATTERN_3:
					ok_move = FEAT_PATTERN_4;
					break;
				case FEAT_PATTERN_4:
					ok_move = FEAT_PATTERN_1;
					break;
				default:
					if (wizard)
						msg_format("Funny Pattern walking, %d.", *area(c_y,c_x));
					return TRUE; /* Goof-up */
			}

			if ((area(n_y,n_x)->feat == ok_move) ||
				 (area(n_y,n_x)->feat == area(c_y,c_x)->feat))
				return TRUE;

			else
			{
				if (!pattern_tile(n_y, n_x) && get_check("Really step off of the Pattern? "))
				{
					take_hit(50, "Stepping off of the Pattern");

					if (one_in_(3)) summon_pattern_vortex(n_y, n_x);

					return TRUE;
				}

				else if (pattern_tile(n_y, n_x) && get_check("Really stray from the proper path? "))
				{
					take_hit(25, "Walking backwards along the Pattern");

					if (one_in_(5)) summon_pattern_vortex(n_y, n_x);

					return TRUE;
				}

				return FALSE;
			}
		}
	}
}



/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid.  Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir, int do_pickup)
{
	int y, x;

	cave_type *c_ptr;
	monster_type *m_ptr;

	char m_name[80];

	bool p_can_pass_walls = FALSE;
	bool stormbringer = FALSE;
	bool p_can_pass_fields = TRUE;

	bool oktomove = TRUE;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];

	/* Do not exit the wilderness-area */
	if (!dun_level)
	{
		if (!in_bounds2(y, x))
		{
			/* Do not leave the wilderness */
			msg_print("You can not leave the wilderness.");
			energy_use = 0;
			return;
		}
	}

	/* Examine the destination */
	c_ptr = area(y,x);

	/* Get the monster */
	m_ptr = &m_list[c_ptr->m_idx];

	if (inventory[INVEN_WIELD].art_name)
	{
		if (streq(quark_str(inventory[INVEN_WIELD].art_name), "'Stormbringer'"))
			stormbringer = TRUE;
	}

	/* Player can not walk through "walls"... */
	/* unless in Shadow Form */
	if (p_ptr->wraith_form || p_ptr->pass_wall)
		p_can_pass_walls = TRUE;
	if ((c_ptr->feat >= FEAT_PERM_EXTRA) &&
	    (c_ptr->feat <= FEAT_PERM_SOLID))
	{
		p_can_pass_walls = FALSE;
	}
	
	/* Get passability of field(s) if there */
	p_can_pass_fields = !(fields_have_flags(c_ptr->fld_idx,
		 FIELD_INFO_NO_ENTER));

	/* Hack -- attack monsters */
	if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr) || p_can_pass_walls))
	{
		/* Attack -- only if we can see it OR it is not in a wall */
		if (!is_hostile(m_ptr) &&
		    !(p_ptr->confused || p_ptr->image || !m_ptr->ml || p_ptr->stun ||
		    ((p_ptr->muta2 & MUT2_BERS_RAGE) && p_ptr->shero)) &&
		    (pattern_seq(py, px, y, x)) &&
		    ((cave_floor_grid(c_ptr)) || p_can_pass_walls))
		{
			m_ptr->csleep = 0;

			/* Extract monster name (or "it") */
			monster_desc(m_name, m_ptr, 0);

			/* Auto-Recall if possible and visible */
			if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

			/* Track a new monster */
			if (m_ptr->ml) health_track(c_ptr->m_idx);

			/* displace? */
			if (stormbringer && (randint(1000) > 666))
			{
				py_attack(y, x);
			}
			else if (cave_floor_grid(area(py, px)) ||
			    (r_info[m_ptr->r_idx].flags2 & RF2_PASS_WALL))
			{
				msg_format("You push past %s.", m_name);
				m_ptr->fy = py;
				m_ptr->fx = px;
				area(py,px)->m_idx = c_ptr->m_idx;
				c_ptr->m_idx = 0;
				update_mon(area(py,px)->m_idx, TRUE);
			}
			else
			{
				msg_format("%^s is in your way!", m_name);
				energy_use = 0;
				oktomove = FALSE;
			}

			/* now continue on to 'movement' */
		}
		else
		{
			py_attack(y, x);
			oktomove = FALSE;
		}
	}
	
	/* Fields can block movement */
	else if (!(p_can_pass_walls || p_can_pass_fields))
	{
		msg_print("You can't cross that!");
		running = 0;
		oktomove = FALSE;
	}

	/*
	 * Player can move through trees and
	 * has effective -10 speed
	 * Rangers can move without penality
	 */
	else if ((c_ptr->feat == FEAT_TREES) ||
		(c_ptr->feat == FEAT_PINE_TREE) ||
		(c_ptr->feat == FEAT_SNOW_TREE))
	{
		oktomove = TRUE;
		if (p_ptr->pclass != CLASS_RANGER) energy_use += 10;
	}

	/* Some terrains are hard to move through */

	else if ((c_ptr->feat == FEAT_MOUNTAIN) ||
		(c_ptr->feat == FEAT_SNOW_MOUNTAIN) ||
		(c_ptr->feat == FEAT_OBELISK) ||
		(c_ptr->feat == FEAT_PILLAR) ||
		(c_ptr->feat == FEAT_BOULDER))
	{
		oktomove = TRUE;
		energy_use += 10;
	}

	else if ((c_ptr->feat >= FEAT_SAND) && (c_ptr->feat <= FEAT_SOLID_LAVA))
	{
		oktomove = TRUE;
	}
	else if ((c_ptr->feat >= FEAT_BUSH) && (c_ptr->feat <= FEAT_SNOW))
	{
		oktomove = TRUE;
	}

	/* Closed door */
	else if (c_ptr->feat == FEAT_CLOSED)
	{
		/* Pass through the door? */
		if (p_can_pass_walls)
		{
			/* Automatically open the door? */
			if (easy_open && do_cmd_open_aux(y, x))
			{
				oktomove = FALSE;

				/* Disturb the player */
				disturb(0, 0);
			}
		}
		else
		{
			oktomove = FALSE;

			/* Disturb the player */
			disturb(0, 0);

			/* Notice things in the dark */
			if ((!(c_ptr->info & CAVE_MARK)) &&
				(p_ptr->blind || !(c_ptr->info & CAVE_LITE)))
			{
				msg_print("You feel a closed door blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Notice things */
			else
			{
				/* Try to open a door if is there */
				if (easy_open)
				{
					(void) do_cmd_open_aux(y, x);
					return;
				} 

				msg_print("There is a closed door blocking your way.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Sound */
			sound(SOUND_HITWALL);
		}
	}

	/* Disarm a visible trap */
	else if ((do_pickup != easy_disarm) && is_visible_trap(c_ptr))
	{
		(void)do_cmd_disarm_aux(c_ptr, dir);
		return;
	}

	/* Player can not walk through "walls" unless in wraith form... */
	else if (!cave_floor_grid(c_ptr) && !p_can_pass_walls)
	{
		oktomove = FALSE;

		/* Disturb the player */
		disturb(0, 0);

		/* Notice things in the dark */
		if ((!(c_ptr->info & (CAVE_MARK))) &&
		    (p_ptr->blind || !(c_ptr->info & (CAVE_LITE))))
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("You feel some rubble blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
			/* Wall (or secret door) */
			else
			{
				msg_print("You feel a wall blocking your way.");
				c_ptr->info |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}
		/* Notice things */
		else
		{
			/* Rubble */
			if (c_ptr->feat == FEAT_RUBBLE)
			{
				msg_print("There is rubble blocking your way.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;

				/*
				 * Well, it makes sense that you lose time bumping into
				 * a wall _if_ you are confused, stunned or blind; but
				 * typing mistakes should not cost you a turn...
				 */
			}

			/* Jungle */
			else if (c_ptr->feat == FEAT_JUNGLE)
			{
				msg_print("The jungle is impassable.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}

			/* Wall (or secret door) */
			else
			{
				msg_print("There is a wall blocking your way.");

				if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
					energy_use = 0;
			}
		}

		/* Sound */
		sound(SOUND_HITWALL);
	}

	/* Normal movement */
	if (!pattern_seq(py, px, y, x))
	{
		if (!(p_ptr->confused || p_ptr->stun || p_ptr->image))
		{
			energy_use = 0;
		}

		/* To avoid a loop with running */
		disturb(0, 0);

		oktomove = FALSE;
	}

	/* Normal movement */
	if (oktomove)
	{
		int oy, ox;

#ifdef USE_SCRIPT
		if (player_enter_grid_callback(y, x)) return;

		/* Player movement callback */
		if (player_move_callback(y, x)) return;
#endif /* USE_SCRIPT */

		/* Save old location */
		oy = py;
		ox = px;

		/* Process fields under the player. */
		field_hook(&area(py, px)->fld_idx,
			 FIELD_ACT_PLAYER_LEAVE, NULL);

		/* Move the player */
		py = y;
		px = x;
		
		if (!dun_level)
		{
			/* Scroll wilderness */
			p_ptr->wilderness_x = px;
			p_ptr->wilderness_y = py;
			move_wild();
		}
		
		/* Process fields under the player. */
		field_hook(&area(py, px)->fld_idx,
			 FIELD_ACT_PLAYER_ENTER, NULL);
		
		/* Redraw new spot */
		lite_spot(py, px);		

		/* Redraw old spot */
		lite_spot(oy, ox);

		/* Sound */
		/* sound(SOUND_WALK); */

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
		
		/* Warn about traps */
		
		/* 
		 * Is the disturb_traps option set +
		 * out of detection range +
		 * we are running?
		 */
		if (disturb_traps && p_ptr->detected && running && 
			(distance(py, px, p_ptr->detecty, p_ptr->detectx) >= MAX_DETECT))
		{
			/* We are out of range */
				
			/* Disturb */
			disturb(0, 0);
				
			/* Reset the detection flag */
			p_ptr->detected = FALSE;
		}

		/* Spontaneous Searching */
		if ((p_ptr->skill_fos >= 50) ||
		    (0 == rand_int(50 - p_ptr->skill_fos)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}

		/* Handle "objects" */
		carry(do_pickup != always_pickup);

		/* Handle "store doors" */
		if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
			 (c_ptr->feat <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			command_new = '_';
		}
#if 0
		/* Handle "building doors" -KMW- */
		else if ((c_ptr->feat >= FEAT_BLDG_HEAD) &&
		    (c_ptr->feat <= FEAT_BLDG_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter building */
			command_new = ']';
		}
#endif
#if 0

		/* Handle quest areas -KMW- */
		else if (area(y,x)->feat == FEAT_QUEST_ENTER)
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter quest level */
			command_new = '[';
		}

		else if (area(y,x)->feat == FEAT_QUEST_EXIT)
		{
			if (quest[p_ptr->inside_quest].type == QUEST_TYPE_FIND_EXIT)
			{
				quest[p_ptr->inside_quest].status = QUEST_STATUS_COMPLETED;
				msg_print("You accomplished your quest!");
				msg_print(NULL);
			}

			leaving_quest = p_ptr->inside_quest;

			/* Leaving an 'only once' quest marks it as failed */
			if (leaving_quest &&
				(quest[leaving_quest].flags & QUEST_FLAG_ONCE) &&
				(quest[leaving_quest].status == QUEST_STATUS_TAKEN))
			{
				quest[leaving_quest].status = QUEST_STATUS_FAILED;
			}

			p_ptr->inside_quest = area(y,x)->special;
			dun_level = 0;
			p_ptr->oldpx = 0;
			p_ptr->oldpy = 0;
			p_ptr->leaving = TRUE;
		}
#endif

	}
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	cave_type *c_ptr;

	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are "walls" */
	if (!in_bounds2(y, x)) return (TRUE);

	c_ptr = area(y, x);

	/* Non-wall grids are not known walls */
	if (c_ptr->feat < FEAT_SECRET) return (FALSE);

	if ((c_ptr->feat >= FEAT_DEEP_WATER) &&
	    (c_ptr->feat <= FEAT_GRASS)) return (FALSE);

	if ((c_ptr->feat >= FEAT_SHOP_HEAD) &&
	    (c_ptr->feat <= FEAT_SHOP_TAIL)) return (FALSE);

	/* Semi - transparent terrains */
	if ((c_ptr->feat & 0x60) == 0x60) return (FALSE);

	if ((c_ptr->feat == FEAT_GRASS) ||
		(c_ptr->feat == FEAT_DIRT) ||
		(c_ptr->feat == FEAT_TREE_WATER)) return (FALSE);

	if ((c_ptr->feat >= FEAT_BUSH) &&
	    (c_ptr->feat <= FEAT_SNOW)) return (FALSE);

	/* Must be known to the player */
	if (!(c_ptr->info & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	cave_type *c_ptr;

	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown */
	if (!in_bounds2(y, x)) return (FALSE);

	c_ptr = area(y, x);

	/* Memorized grids are always known */
	if (c_ptr->info & (CAVE_MARK)) return (FALSE);

	/* Non-floor grids are unknown */
	if (!cave_floor_grid(c_ptr)) return (TRUE);

	/* Viewable door/wall grids are known */
	if (player_can_see_bold(y, x)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea         You are in the open on at least one
 * side.
 * find_breakleft        You have a wall on the left, and will
 * stop if it opens
 * find_breakright       You have a wall on the right, and will
 * stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * #@x    1
 * ########### ######
 * 2        #
 * #############
 * #
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 * ...!   ...
 * .o@!   .o.!
 * ...!   ..@!
 * !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * .s
 * @x?
 * #?
 *
 * If they are both seen to be closed, then it is seen that no
 * benefit is gained from moving straight. It is a known corner.
 * To cut the corner, go diagonally, otherwise go straight, but
 * pretend you stepped diagonally into that next location for a
 * full view next time. Conversely, if one of the ? squares is
 * not seen to be closed, then there is a potential choice. We check
 * to see whether it is a potential corner or an intersection/room entrance.
 * If the square two spaces straight ahead, and the space marked with 's'
 * are both blank, then it is a potential corner and enter if find_examine
 * is set, otherwise must stop because it is not a corner.
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int             row, col, deepleft, deepright;
	int             i, shortleft, shortright;


	if (!dun_level)
	{
		/* If in the wilderness - run max 32 squares at a time */
		running = 32;
	}
	else
	{
		/* Large number to prevent infinite loops */
		running = 1000;
	}

	/* Save the direction */
	find_current = dir;

	/* Assume running straight */
	find_prevdir = dir;

	/* Assume looking for open area */
	find_openarea = TRUE;

	/* Assume not looking for breaks */
	find_breakright = find_breakleft = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for walls */
	if (see_wall(cycle[i+1], py, px))
	{
		find_breakleft = TRUE;
		shortleft = TRUE;
	}
	else if (see_wall(cycle[i+1], row, col))
	{
		find_breakleft = TRUE;
		deepleft = TRUE;
	}

	/* Check for walls */
	if (see_wall(cycle[i-1], py, px))
	{
		find_breakright = TRUE;
		shortright = TRUE;
	}
	else if (see_wall(cycle[i-1], row, col))
	{
		find_breakright = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (find_breakleft && find_breakright)
	{
		/* Not looking for open area */
		find_openarea = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				find_prevdir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				find_prevdir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				find_prevdir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				find_prevdir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int         prev_dir, new_dir, check_dir = 0;
	int         row, col;
	int         i, max, inv;
	int         option = 0, option2 = 0;
	cave_type   *c_ptr;

	/* Hack - do not run next to edge of wilderness */
	if (!in_bounds(py, px)) return TRUE;

	/* Where we came from */
	prev_dir = find_prevdir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];

		if (!in_bounds2(row, col)) continue;

		/* Access grid */
		c_ptr = area(row, col);


		/* Visible monsters abort running */
		if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			object_type *o_ptr;

			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Visible object */
			if (o_ptr->marked) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (c_ptr->info & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (c_ptr->feat)
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				/* dirt, grass, trees, ... */
				case FEAT_SAND:
				case FEAT_SALT:
				case FEAT_WET_MUD:
				case FEAT_DRY_MUD:
				case FEAT_FLOOR_TILE:
				case FEAT_FLOOR_WOOD:
				case FEAT_PEBBLES:
				case FEAT_SOLID_LAVA:
				case FEAT_SHAL_WATER:
				case FEAT_DIRT:
				case FEAT_GRASS:
				case FEAT_TREE_WATER:

				case FEAT_TREES:
				case FEAT_MOUNTAIN:
				case FEAT_SNOW_MOUNTAIN:
				case FEAT_PINE_TREE:
				case FEAT_SNOW_TREE:
				case FEAT_OBELISK:
				case FEAT_PILLAR:

				case FEAT_BUSH:
				case FEAT_DEAD_BUSH:
				case FEAT_GRASS_LONG:
				case FEAT_ROCK_GEN:
				case FEAT_ROCK_SNOW:
				case FEAT_TREE_GEN:
				case FEAT_TREE_SNOW:
				case FEAT_SNOW:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}
#if 0
				/* quest features */
				case FEAT_QUEST_ENTER:
				case FEAT_QUEST_EXIT:
				{
					/* Notice */
					notice = TRUE;

					/* Done */
					break;
				}
#endif
				case FEAT_DEEP_LAVA:
				case FEAT_SHAL_LAVA:
				{
					/* Ignore */
					if (p_ptr->invuln || p_ptr->immune_fire) notice = FALSE;

					/* Done */
					break;
				}

				case FEAT_DEEP_ACID:
				case FEAT_SHAL_ACID:
				{
					/* Ignore */
					if (p_ptr->invuln || p_ptr->immune_acid) notice = FALSE;

					/* Done */
					break;
				}

				case FEAT_DEEP_SWAMP:
				case FEAT_SHAL_SWAMP:
				{
					/* Ignore */
					if (p_ptr->invuln) notice = FALSE;

					/* Done */
					break;
				}


				case FEAT_DEEP_WATER:
				case FEAT_OCEAN_WATER:
				{
					/* Ignore */
					if (p_ptr->ffall) notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (find_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (find_ignore_stairs) notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_grid(c_ptr) ||
		    ((c_ptr->feat & 0x60) == 0x60))
		{
			/* Looking for open area */
			if (find_openarea)
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (find_openarea)
			{
				if (i < 0)
				{
					/* Break to the right */
					find_breakright = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					find_breakleft = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (find_openarea)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			if (!in_bounds2(row, col)) continue;

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!see_wall(new_dir, py, px))

			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			if (!in_bounds2(row, col)) continue;

			/* Access grid */
			c_ptr = area(row, col);

			/* Unknown grid or non-wall XXX XXX XXX cave_floor_grid(c_ptr)) */
			if (!see_wall(new_dir, py, px))

			{
				/* Looking to break left */
				if (find_breakleft)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (find_breakright)
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			find_current = option;

			/* No other options */
			find_prevdir = option;
		}

		/* Two options, examining corners */
		else if (find_examine && !find_cut)
		{
			/* Primary option */
			find_current = option;

			/* Hack -- allow curving */
			find_prevdir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = py + ddy[option];
			col = px + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (find_examine &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					find_current = option;
					find_prevdir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (find_cut)
			{
				find_current = option2;
				find_prevdir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				find_current = option;
				find_prevdir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(find_current, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}


/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
	/* Start running */
	if (dir)
	{
		/* Hack -- do not start silly run */
		if (see_wall(dir, py, px))
		{
			/* Message */
			msg_print("You cannot run in that direction.");

			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);

		/* Initialize */
		run_init(dir);
	}

	/* Keep running */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Disturb */
			disturb(0, 0);

			/* Done */
			return;
		}
	}

	/* Decrease the run counter */
	if (--running <= 0)
	{
		running = 0;
		return;
	}

	/* Take time */
	energy_use = 100;

	/* Move the player, using the "pickup" flag */
	move_player(find_current, FALSE);
}
