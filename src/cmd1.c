/* File: cmd1.c */

/* Searching, pickup, effects of traps, move one square (including special
 * terrain effects), and the running algorithm.
 *
 * Tim Baker's easy patch installed.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;

	s16b this_o_idx, next_o_idx = 0;


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
				/* Invisible trap */
				if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* Scan all objects in the grid */
				for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
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
 * Return TRUE if the given object is inscribed with "=g".
 *
 * Alternatively, also return TRUE if any similar item in the
 * backpack is marked "=g".
 */
static bool auto_pickup_check(object_type *o_ptr, bool check_pack)
{
	cptr s;

	/* Check inscription */
	if (o_ptr->note)
	{
		/* Find a '=' */
		s = strchr(quark_str(o_ptr->note), '=');

		/* Process preventions */
		while (s)
		{
			/* =g ('g'et) means auto pickup */
			if (s[1] == 'g') return (TRUE);

			/* Find another '=' */
			s = strchr(s + 1, '=');
		}
	}

	/* Optionally, check the backpack */
	if (check_pack)
	{
		int j;

		/* Look for similar and inscribed */
		for (j = 0; j < INVEN_PACK - p_ptr->pack_size_reduce; j++)
		{
			object_type *j_ptr = &inventory[j];

			/* Skip non-objects */
			if (!j_ptr->k_idx) continue;

			/* The two items must be able to combine */
			if (!object_similar(j_ptr, o_ptr)) continue;

			/* The backpack item must be inscribed */
			if (!j_ptr->note) continue;

			/* Find a '=' */
			s = strchr(quark_str(j_ptr->note), '=');

			/* Process preventions */
			while (s)
			{
				/* =g ('g'et) means auto pickup */
				if (s[1] == 'g') return (TRUE);

				/* Find another '=' */
				s = strchr(s + 1, '=');
			}
		}
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Automatically carry ammunition and throwing weapons in the quiver,
 * if it is inscribed with "=g", or it matches something already in
 * the quiver.
 */
bool quiver_carry(object_type *o_ptr, int o_idx)
{
	int i;
	u32b f1, f2, f3;

	bool throwing;

	int ammo_num, added_ammo_num;
	int attempted_quiver_slots;

	bool blind = ((p_ptr->blind) || (no_lite()));
	bool autop;
	int old_num;

	object_type *i_ptr;


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Must be ammo or throwing weapon. */
	if ((!is_missile(o_ptr)) && (!(f1 & (TR1_THROWING))))
	{
		return (FALSE);
	}

	/* Throwing weapon? */
	throwing = (((f1 & (TR1_THROWING)) && (!is_missile(o_ptr))) ? TRUE : FALSE);

	/* Count number of missiles in the quiver slots. */
	ammo_num = quiver_count();

	/* Check for autopickup */
	autop = auto_pickup_check(o_ptr, FALSE);

	/* No missiles to combine with and no autopickup. */
	if (!ammo_num && !autop) return (FALSE);

	/* Effective added count */
	added_ammo_num = (throwing ?
		o_ptr->number * THROWER_AMMO_FACTOR : o_ptr->number);

	/* How many quiver slots would be needed */
	attempted_quiver_slots = ((ammo_num + added_ammo_num + 98) / 99);

	/* Is there room, given normal inventory? */
	if (attempted_quiver_slots + p_ptr->inven_cnt > INVEN_PACK)
	{
		return (FALSE);
	}


	/* Check quiver for similar objects or empty space. */
	for (i = INVEN_Q0; i <= INVEN_Q9; i++)
	{
		/* Assume no carry */
		bool flag = FALSE;

		/* Get object in that slot. */
		i_ptr = &inventory[i];

		/* Allow auto-pickup to empty slots */
		if ((!i_ptr->k_idx) && (autop))
		{
			/* Nothing there */
			old_num = 0;

			/* Wield it */
			object_copy(i_ptr, o_ptr);

			flag = TRUE;
		}

		/* Look for similar */
		else if (object_similar(i_ptr, o_ptr))
		{
			/* How many did we have before? */
			old_num = i_ptr->number;

			/* Don't absorb unless there is space for all of it */
			if ((old_num + o_ptr->number) > 99) return (FALSE);

			/* Absorb floor object. */
			object_absorb(i_ptr, o_ptr);

			flag = TRUE;
		}

		/* We want to carry it */
		if (flag)
		{
			char o_name[120];

			/* Increase carried weight */
			p_ptr->total_weight += i_ptr->weight * (i_ptr->number - old_num);

			/* Get the object again */
			o_ptr = &inventory[i];

			/* Describe the object */
			if (blind) object_desc(o_name, o_ptr, TRUE, 0);
			else       object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_format("You have %s (%c).", o_name, index_to_label(i));

			/* Delete the object */
			delete_object_idx(o_idx);

			/* Recalculate quiver size */
			find_quiver_size();

			/* Recalculate bonuses */
			p_ptr->update |= (PU_BONUS);

			/* Reorder the quiver */
			p_ptr->notice |= (PN_COMBINE);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

			/* Redraw equippy chars */
			p_ptr->redraw |= (PR_EQUIPPY);

			return (TRUE);
		}
	}

	/* Didn't find a slot with similar objects, or an empty slot. */
	return (FALSE);
}


/*
 * Carry an object and delete it.
 */
static void py_pickup_aux(int o_idx)
{
	int slot;

	char o_name[120];
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

	/* Delete the object */
	delete_object_idx(o_idx);
}


/*
 * Pick up objects and treasure on the floor.  -LM-
 *
 * Called with pickup:
 * 0 to grab gold and describe non-gold objects.
 * 1 to pick up objects either with or without displaying a menu.
 * 2 to pick up objects, allowing cancel and quick pickup of single objects.
 * 3 to pick up objects, forcing a menu for any number of objects.
 *
 * Use the "p_ptr->auto_pickup_okay" variable to allow or dis-allow
 * automatically picking things up that take time.
 *
 * Scan the list of objects in that floor grid.   Pick up gold automatically.
 * Pick up objects automatically until pile or backpack space is full if
 * auto-pickup option is on, carry_query_floor option is not, and menus are
 * not forced (which the "get" command does). Otherwise, store objects on
 * floor in an array, and tally both how many there are and can be picked up.
 *
 * If not picking up anything, indicate objects on the floor.  Show more
 * details if the "query_floor" option is set.  Do the same thing if we
 * don't have room for anything.
 *
 * If we are picking up objects automatically, and have room for at least
 * one, allow the "query_floor" option to display information about objects
 * and prompt the player.  Otherwise, automatically pick up a single object
 * or use a menu for more than one (this "blind" autopickup option is
 * deprecated).
 *
 * Pick up multiple objects using Tim Baker's menu system.   Recursively
 * call this function (forcing menus for any number of objects) until
 * objects are gone, backpack is full, or player is satisfied.
 *
 * We keep track of number of objects picked up to calculate time spent.
 * This tally is incremented even for automatic pickup, so we are careful
 * (in "dungeon.c" and elsewhere) to handle pickup as either a separate
 * automated move or a no-cost part of the stay still or 'g'et command.
 *
 * Note the lack of chance for the character to be disturbed by unmarked
 * objects.  They are truly "unknown".
 */
byte py_pickup(int pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char o_name[120];
	char ch;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	/* Objects picked up.  Used to determine time cost of command. */
	byte objs_picked_up = 0;

	int floor_num = 0, floor_list[24], floor_o_idx = 0;

	int can_pickup = 0;
	bool call_function_again = FALSE;

	bool blind = ((p_ptr->blind) || (no_lite()));

	bool force_display_list = FALSE;

	s32b total_gold = 0L;
	byte *treasure;


	/* Nothing to pick up -- return */
	if (!cave_o_idx[py][px]) return (0);



	/* Allocate and wipe an array of ordinary gold objects */
	C_MAKE(treasure, MONEY_TYPES, byte);
	C_WIPE(treasure, MONEY_TYPES, byte);

	/* Pick up all the ordinary gold objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Ignore all hidden objects */
		if (!o_ptr->marked) continue;

		/* Ignore if not ordinary treasure */
		if ((o_ptr->tval != TV_GOLD) ||
		    (o_ptr->sval >= MONEY_TYPES)) continue;

		/* Note that we have this kind of treasure */
		treasure[o_ptr->sval]++;

		/* Increment total value */
		total_gold += (s32b)o_ptr->pval;

		/* Delete the gold */
		delete_object_idx(this_o_idx);
	}

	/* Pick up the gold, if present */
	if (total_gold)
	{
		char buf[240];
		char tmp[80];
		int i, count, total, k_idx;

		/* Build a message */
		sprintf(buf, "You have found %ld gold pieces worth of ", total_gold);

		/* Count the types of treasure present */
		for (total = 0, i = 0; i < MONEY_TYPES; i++)
		{
			if (treasure[i]) total++;
		}

		/* List the treasure types */
		for (count = 0, i = 0; i < MONEY_TYPES; i++)
		{
			/* Skip if no treasure of this type */
			if (!treasure[i]) continue;

			/* Get this object index */
			k_idx = lookup_kind(TV_GOLD, i);

			/* Skip past errors  XXX */
			if (k_idx <= 0) continue;

			/* Get the object name */
			strip_name(tmp, k_idx);

			/* Build up the pickup string */
			strcat(buf, tmp);

			/* Added another kind of treasure */
			count++;

			/* Add a comma if necessary */
			if ((total > 2) && (count < total)) strcat(buf, ",");

			/* Add an "and" if necessary */
			if ((total >= 2) && (count == total-1)) strcat(buf, " and");

			/* Add a space or period if necessary */
			if (count < total) strcat(buf, " ");
			else               strcat(buf, ".");
		}

		/* Display the message */
		msg_format("%s", buf);

		/* Add gold to purse */
		p_ptr->au += total_gold;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Free the gold array */
	C_FREE(treasure, MONEY_TYPES, byte);


	/* Scan the remaining objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Ignore all hidden objects */
		if (!o_ptr->marked) continue;

		/* Paranoia -- ignore all dead objects  XXX */
		if (!o_ptr->k_idx) continue;

		/* Hack -- disturb */
		disturb(0, 0);

		/* Automatically pick up objects into the quiver */
		if ((p_ptr->auto_pickup_okay) && (quiver_carry(o_ptr, this_o_idx)))
		{
			objs_picked_up++;
			continue;
		}

		/* Automatically pick up some items into the backpack */
		if ((p_ptr->auto_pickup_okay) && (inven_carry_okay(o_ptr)) &&
		    (auto_pickup_check(o_ptr, TRUE)))
		{
			/* Pick up the object */
			py_pickup_aux(this_o_idx);

			/* Take a small amount of time */
			objs_picked_up++;
			continue;
		}

		/* Tally objects and store them in an array. */

		/* Remember this object index */
		floor_list[floor_num] = this_o_idx;

		/* Count non-gold objects that remain on the floor. */
		floor_num++;

		/* Remember this index */
		floor_o_idx = this_o_idx;

		/* Tally objects that can be picked up.*/
		if (inven_carry_okay(o_ptr))
		{
			can_pickup++;
		}

		/* XXX Hack -- Enforce limit */
		if (floor_num == 23) break;
	}

	/* There are no objects left */
	if (!floor_num) return (objs_picked_up);


	/* Mention the objects if player is not picking them up. */
	if (!pickup)
	{
		/* Optionally, display more information about floor items */
		if (query_floor)
		{
			/* Scan all marked objects in the grid */
			(void)scan_floor(floor_list, &floor_num, py, px, 0x03);

			/* Save screen */
			screen_save();

			/* Display objects on the floor */
			show_floor(floor_list, floor_num, FALSE);

			/* Display prompt */
			prt(format("You %s: ",
			    (blind ? "feel something on the floor" : "see")), 0, 0);
			(void)Term_fresh();

			/* Wait for it.  Use key as next command. */
			p_ptr->command_new = inkey();

			/* Restore screen */
			screen_load();
		}

		/* Display compact information */
		else
		{
			/* One object */
			if (floor_num == 1)
			{
				/* Get the object */
				o_ptr = &o_list[floor_o_idx];

				/* Describe the object.  Less detail if blind. */
				if (blind) object_desc(o_name, o_ptr, TRUE, 0);
				else       object_desc(o_name, o_ptr, TRUE, 3);

				msg_print(NULL);

				/* Message */
				msg_format("You %s %s.", (blind ? "feel" : "see"),
					o_name);
			}

			/* Several objects */
			else
			{
				msg_print(NULL);

				/* Message */
				msg_format("You %s a pile of %d items.",
					(blind ? "feel" : "see"), floor_num);
			}
		}

		/* Done */
		return (objs_picked_up);
	}


	/* The player has no room for anything on the floor. */
	if (!can_pickup)
	{
		/*
		 * One object -- Always display compact information.  This
		 * should change if more information would actually be helpful.
		 */
		if (floor_num == 1)
		{
			/* Get the object */
			o_ptr = &o_list[floor_o_idx];

			/* Describe the object.  Less detail if blind. */
			if (blind) object_desc(o_name, o_ptr, TRUE, 0);
			else       object_desc(o_name, o_ptr, TRUE, 3);

			/* Message */
			msg_print(NULL);
			msg_format("You have no room for %s.", o_name);
		}

		/* Several items */
		else
		{
			/* Optionally, display more information about floor items */
			if ((query_floor) || (force_display_list))
			{
				/* Scan all marked objects in the grid */
				(void)scan_floor(floor_list, &floor_num, py, px, 0x03);

				/* Save screen */
				screen_save();

				/* Display objects on the floor */
				show_floor(floor_list, floor_num, FALSE);

				/* Display prompt */
				prt("You have no room for the following objects: ", 0, 0);
				(void)Term_fresh();

				/* Wait for it.  Use key as next command. */
				p_ptr->command_new = inkey();

				/* Restore screen */
				screen_load();
			}

			/* Display compact information */
			else
			{
				/* Message -- not very informative */
				msg_print(NULL);
				msg_print("You have no room for any of the items on the floor.");
			}
		}

		/* Done */
		return (objs_picked_up);
	}


	/* We can pick up objects.  Menus are not requested (yet). */
	if (pickup != 3)
	{
		/* Scan all marked objects in the grid (again) */
		(void)scan_floor(floor_list, &floor_num, py, px, 0x03);

		/*
		 * If not deliberately picking up objects, and if requested or
		 * potentially unsafe, ask the player to confirm all pickups.
		 */
		if (((query_floor) || (!p_ptr->auto_pickup_okay)) && (pickup <= 1))
		{
			/* Save screen */
			screen_save();

			/* Display objects on the floor */
			show_floor(floor_list, floor_num, FALSE);

			/* Display prompt */
			if (floor_num == 1)
			{
				prt("Press Return to pick up this object: ", 0, 0);
			}
			else
			{
				prt("Press Return to pick up any of the following objects: ",
					0, 0);
			}
			(void)Term_fresh();

			/* Get response */
			ch = inkey();

			/* Restore screen */
			screen_load();

			/* We don't want to pick up this item */
			if ((ch != '\r') && (ch != '\n') && (ch != 'g'))
			{
				/* Attempt to turn this command into a direction */
				int dir = target_dir(ch);

				/* We used a movement command */
				if (dir)
				{
					/* Save as a new command; move later */
					p_ptr->command_new = ch;
				}

				/* Done */
				return (objs_picked_up);
			}
		}

		/* Use a menu interface for multiple objects */
		if (floor_num > 1)
		{
			pickup = 3;
		}

		/* Automatically pick up a single object */
		else
		{
			/* Remember the object to pick up */
			this_o_idx = floor_o_idx;
		}
	}

	/* Display a list if requested. */
	if (pickup == 3)
	{
		cptr q, s;

		int item;

		/* Restrict the choices */
		item_tester_hook = inven_carry_okay;

		/* Get an object or exit. */
		q = "Get which item?";
		s = "You see nothing there.";
		if (get_item(&item, q, s, (USE_FLOOR)))
		{
			this_o_idx = 0 - item;
			call_function_again = TRUE;
		}
		else
		{
			return (objs_picked_up);
		}
	}

	/* Pick up object, if legal */
	if (this_o_idx)
	{
		/* Pick up the object */
		py_pickup_aux(this_o_idx);

		/* Indicate an object picked up. */
		objs_picked_up = 1;
	}

	/*
	 * If requested, call this function recursively.  Count objects picked
	 * up.  Force the display of a menu in all cases.
	 */
	if (call_function_again) objs_picked_up += py_pickup(3);

	/* Indicate how many objects have been picked up. */
	return (objs_picked_up);
}


/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int check_trap_hit(int power)
{
	int k, ac;
	bool hit = FALSE;

	/* This function is called from the trap attack code, which generally
	 * uses the simple RNG.   We temporarily switch over to the complex
	 * RNG for true randomness. - LM-
	 */
	Rand_quick = FALSE;

	/* Percentile dice */
	k = rand_int(100);

	/* 5% minimum chance to hit, 5% minimum chance to miss */
	if (k < 10) hit = (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power competes against Armor */
	if ((power > 0) && (randint(power) >= (ac * 3 / 4))) hit = TRUE;

	/* Resume use of the simple RNG. */
	Rand_quick = TRUE;

	/* Return hit or miss. */
	return (hit);

}



/*
 * Handle player hitting a real trap.  Rewritten in Oangband to allow a
 * greater variety of traps, with effects controlled by dungeon level.
 * To allow a trap to choose one of a variety of effects consistantly,
 * the quick RNG is often used, and xy coordinates input as a seed value.
 */
void hit_trap(int y, int x)
{
	int i, j, k, num;
	int dam = 0;

	int nastyness, selection;

	cptr name = "a trap";

	/* Use the "simple" RNG to insure that traps are consistant. */
	Rand_quick = TRUE;

	/* Use the coordinates of the trap to seed the RNG. */
	Rand_value = y * x;

	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		/* trap door. */
		case FEAT_TRAP_HEAD + 0x00:
		{
			Rand_quick = FALSE;


			msg_print("You fall through a trap door!");
			if (p_ptr->ffall)
			{
				msg_print("You float gently down to the next level.");
			}
			else
			{
				dam = damroll(2, 8);
				take_hit(dam, name);
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			Rand_quick = TRUE;

			break;
		}

		/* pits. */
		case FEAT_TRAP_HEAD + 0x01:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(20) == 1) nastyness += 20;
			else if (randint(5) == 1) nastyness += 10;

			/* Player is now in pit. */
			monster_swap(p_ptr->py, p_ptr->px, y, x);

			/* Center on player. */
			y = p_ptr->py;
			x = p_ptr->px;

			/* pit of daggers. */
			if ((nastyness > 80) && (randint(3) != 3))
			{
				msg_print("You fall into a pit of daggers!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid setting off the daggers.");
				}

				else
				{
					/* a trap of morgul. */
					if (randint(6) == 1)
					{
						Rand_quick = FALSE;


						msg_print("A single coldly gleaming dagger pierces you deeply!");
						msg_print("You feel a deadly chill slowly withering your soul.");

						/* activate the Black Breath. */
						p_ptr->black_breath = TRUE;

						/* lots of damage. */
						dam = damroll(20,15);

						/* undead may be attracted. */
						if (randint(2) == 1)
						{
							msg_print("Undead suddenly appear and call you to them!");

							k = randint(3) + 2;
							(void) summon_specific(y, x, FALSE, p_ptr->depth, SUMMON_UNDEAD, k);

						}

						/* morgul-traps are one-time only. */
						cave_info[y][x] &= ~(CAVE_MARK);
						cave_set_feat(y, x, FEAT_FLOOR);

						Rand_quick = TRUE;
					}

					else
					{
						Rand_quick = FALSE;

						/* activate the ordinary daggers. */
						msg_print("Daggers pierce you everywhere!");

						k = randint(10) + 5;
						for (i = 0; i < k; i++)
						{
							dam += damroll(3, 4);
						}

						Rand_quick = TRUE;
					}

					/* cut the player. */
					(void)set_cut(p_ptr->cut + randint(dam));

					/* Take the damage. */
					take_hit(dam, name);
				}
			}

			/* poisoned spiked pit. */
			else if ((nastyness > 55) && (randint(3) != 3))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(2, 6);

					/* Extra spike damage */
					if (rand_int(100) < 85)
					{
						bool was_poisoned;

						msg_print("You are impaled on poisonous spikes!");

						dam = dam * (randint(6) + 3);
						(void)set_cut(p_ptr->cut + randint(dam));

						was_poisoned = pois_hit(dam);

						if (!was_poisoned) msg_print("The poison does not affect you!");
					}

					/* Take the damage */
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			/* spiked pit. */
			else if ((nastyness > 30) && (randint(3) != 3))
			{
				msg_print("You fall into a spiked pit!");

				if (p_ptr->ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				else
				{
					Rand_quick = FALSE;

					/* Base damage */
					dam = damroll(2, 6);

					/* Extra spike damage */
					if (rand_int(100) < 85)
					{
						msg_print("You are impaled!");

						dam = dam * (2 + randint(4));
						(void)set_cut(p_ptr->cut + randint(dam));
					}

					/* Take the damage */
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			/* ordinary pit in all other cases. */
			else
			{
				msg_print("You fall into a pit!");
				if (p_ptr->ffall)
				{
					msg_print("You float gently to the bottom of the pit.");
				}
				else
				{
					Rand_quick = FALSE;

					dam = damroll(2, 6);
					take_hit(dam, name);

					Rand_quick = TRUE;
				}
			}

			break;
		}

		/* stat-reducing dart traps. */
		case FEAT_TRAP_HEAD + 0x02:
		{
			/* decide if the dart hits. */
			if (check_trap_hit(50 + p_ptr->depth))
			{
				/* select a stat to drain. */
				selection = rand_int(6);

				Rand_quick = FALSE;

				msg_print("A small dart hits you!");
				dam = damroll(1, 4);
				take_hit(dam, name);

				/* Determine how dangerous the trap is allowed to be. */
				nastyness = randint(p_ptr->depth);

				/* decide how much to drain the stat by. */
				if ((nastyness > 50) && (randint(3) == 1))
				{
					num = randint(4);
				}
				else num = 1;

				/* drain the stat. */
				for (i = 0; i < num; i++)
				{
					(void)do_dec_stat(selection);
				}

				Rand_quick = TRUE;
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		/* discolored spots. */
		case FEAT_TRAP_HEAD + 0x03:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(5) == 1) nastyness += 10;

			/* pick a elemental attack type. */
			selection = randint(4);


			/* electicity trap. */
			if (selection == 1)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are struck by lightning!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You get zapped!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				elec_dam(dam, "an electricity trap");
				Rand_quick = TRUE;

			}

			/* frost trap. */
			if (selection == 2)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are lost within a blizzard!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are coated in frost!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				cold_dam(dam, "a frost trap");
				Rand_quick = TRUE;
			}

			/* fire trap. */
			if (selection == 3)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("You are enveloped in a column of fire!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are surrounded by flames!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				fire_dam(dam, "a fire trap");
				Rand_quick = TRUE;
			}

			/* acid trap. */
			if (selection == 4)
			{
				if ((nastyness >= 50) && (randint(2) == 1))
				{
					Rand_quick = FALSE;

					msg_print("A cauldron of acid is tipped over your head!");
					dam = damroll(6, 30);

					Rand_quick = TRUE;
				}
				else
				{
					Rand_quick = FALSE;

					msg_print("You are splashed with acid!");
					dam = damroll(4, 8);

					Rand_quick = TRUE;
				}
				Rand_quick = FALSE;
				acid_dam(dam, "an acid trap");
				Rand_quick = TRUE;
			}

			break;
		}

		/* gas traps. */
		case FEAT_TRAP_HEAD + 0x04:
		{
			selection = randint(4);

			/* blinding trap. */
			if (selection == 1)
			{
				msg_print("You are surrounded by a black gas!");
				if (!p_ptr->no_blind)
				{
					Rand_quick = FALSE;

					(void)set_blind(p_ptr->blind + rand_int(30) + 15);

					Rand_quick = TRUE;
				}
			}

			/* confusing trap. */
			if (selection == 2)
			{
				msg_print("You are surrounded by a gas of scintillating colors!");
				if (!p_resist_pos(P_RES_CONFU))
				{
					Rand_quick = FALSE;

					(void)set_confused(p_ptr->confused + rand_int(20) + 10);

					Rand_quick = TRUE;
				}
			}

			/* poisoning trap. */
			if (selection == 3)
			{
				msg_print("You are surrounded by a pungent green gas!");

				Rand_quick = FALSE;

				pois_hit(25);

				Rand_quick = TRUE;
			}

			/* sleeping trap. */
			if (selection == 4)
			{
				msg_print("You are surrounded by a strange white mist!");
				if (!p_ptr->free_act)
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 5);
				}
			}

			break;
		}

		/* summoning traps. */
		case FEAT_TRAP_HEAD + 0x05:
		{
			/* sometimes summon thieves. */
			if ((p_ptr->depth > 8) && (randint(5) == 1))
			{
				msg_print("You have aroused a den of thieves!");

				Rand_quick = FALSE;

				num = 2 + randint(3);
				(void)summon_specific(y, x, FALSE, p_ptr->depth, SUMMON_THIEF, num);

				Rand_quick = TRUE;
			}

			/* sometimes summon a nasty unique. */
			else if (randint(8) == 1)
			{
				msg_print("You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				(void)summon_specific(y, x, FALSE, p_ptr->depth + 5, SUMMON_UNIQUE, 1);

				Rand_quick = TRUE;
			}

			/* otherwise, the ordinary summon monsters. */
			else
			{
				msg_print("You are enveloped in a cloud of smoke!");

				Rand_quick = FALSE;

				num = 2 + randint(3);
				(void)summon_specific(y, x, FALSE, p_ptr->depth, 0, num);

				Rand_quick = TRUE;
			}

			/* these are all one-time traps. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			break;
		}

		/* dungeon alteration traps. */
		case FEAT_TRAP_HEAD + 0x06:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = randint(p_ptr->depth);
			if (randint(5) == 1) nastyness += 10;

			/* make room for alterations. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Everything truely random from here on. */
			Rand_quick = FALSE;

			/* dungeon destruction trap. */
			if ((nastyness > 60) && (randint(12) == 1))
			{
				msg_print("A ear-splitting howl shatters your mind as the dungeon is smashed by hammer blows!");

				(void)destroy_level(FALSE);

				/* the player is hard-hit. */
				(void)set_confused(p_ptr->confused + rand_int(20) + 10);
				(void)set_blind(p_ptr->blind + rand_int(30) + 15);
				(void)set_stun(p_ptr->stun + randint(50) + 50);
				dam = damroll(15,15);
				take_hit(dam, name);
			}

			/* earthquake trap. */
			else if ((nastyness > 20) && (randint(4) == 1))
			{
				msg_print("A tremor shakes the dungeon around you");
				earthquake(y, x, 10, FALSE);
			}

			/* falling rock trap. */
			else if ((nastyness > 4) && (randint(2) == 1))
			{
				msg_print("A rock falls on your head.");
				dam = damroll(2,10);
				take_hit(dam, name);

				(void)set_stun(p_ptr->stun + randint(10) + 10);
			}

			/* a few pebbles. */
			else
			{
				msg_print("A bunch of pebbles rain down on you.");
				dam = damroll(1,8);
				take_hit(dam, name);
			}

			Rand_quick = TRUE;

			break;
		}

		/* various char and equipment-alteration traps, lumped together
		 * to avoid any one effect being too common (some of them can be
		 * rather nasty).
		 */
		case FEAT_TRAP_HEAD + 0x07:
		{
			/* determine how dangerous the trap is allowed to be. */
			nastyness = rand_int(100);

			/* these are all one-time traps. */
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);

			/* Everything truely random from here on. */
			Rand_quick = FALSE;

			/* trap of drain wands. */
			if (nastyness < 15)
			{
				/* Hold the object information. */
				object_type *o_ptr;

				/* Find an item */
				for (i = 0; i < 20; i++)
				{
					/* Pick an item */
					i = rand_int(INVEN_PACK - p_ptr->pack_size_reduce);

					/* Obtain the item */
					o_ptr = &inventory[i];

					/* use "num" to decide if a item can be
					 * uncharged.  By default, assume it can't. */
					num = 0;

					/* Skip non-objects */
					if (!o_ptr->k_idx) continue;

					/* Drain charged wands/staffs/rods */
					if ((o_ptr->tval == TV_STAFF) ||
						(o_ptr->tval == TV_WAND) ||
						(o_ptr->tval == TV_ROD))
					{
						/* case of charged wands/staffs. */
						if (((o_ptr->tval == TV_STAFF) ||
							(o_ptr->tval == TV_WAND)) &&
							(o_ptr->pval)) num = 1;

						/* case of charged rods. */
						if ((o_ptr->tval == TV_ROD) &&
						    (o_ptr->timeout < (o_ptr->pval * o_ptr->number))) num = 1;


						if (num == 1)
						{
							/* Message */
							msg_print("Energy drains from your pack!");

							/* Uncharge */
							if ((o_ptr->tval == TV_STAFF) ||
								(o_ptr->tval == TV_WAND))
								o_ptr->pval = 0;

							if (o_ptr->tval == TV_ROD)
								o_ptr->timeout = o_ptr->pval * o_ptr->number * 2;


							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE |
							PN_REORDER);

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

							/* not more than one inventory
							 * slot effected. */
							break;
						}
						else continue;
					}
				}
			}

			/* trap of forgetting. */
			else if (nastyness < 35)
			{
				if (check_save(100))
				{
					msg_print("You hang on to your memories!");
				}
				else if (lose_all_info())
				{
					msg_print("Your memories fade away.");
				}
			}

			/* trap of alter reality. */
			else if (nastyness < 50)
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;
			}

			/* trap of remold player. */
			else if (nastyness < 75)
			{
				int max1, cur1, max2, cur2, ii, jj;

				msg_print("You feel yourself being twisted by wild magic!");

				if (check_save(100))
				{
					msg_print("You resist the effects!");
				}
				else
				{
					msg_print("Your body starts to scramble...");

					/* Pick a pair of stats */
					ii = rand_int(6);
					for (jj = ii; jj == ii; jj = rand_int(6)) /* loop */;

					max1 = p_ptr->stat_max[ii];
					cur1 = p_ptr->stat_cur[ii];
					max2 = p_ptr->stat_max[jj];
					cur2 = p_ptr->stat_cur[jj];

					p_ptr->stat_max[ii] = max2;
					p_ptr->stat_cur[ii] = cur2;
					p_ptr->stat_max[jj] = max1;
					p_ptr->stat_cur[jj] = cur1;

					p_ptr->update |= (PU_BONUS);
				}
			}

			/* time ball trap. */
			else if (nastyness < 90)
			{
				msg_print("You feel time itself assault you!");

				/* Target the player with a radius 0 ball attack. */
				fire_meteor(0, GF_TIME, p_ptr->py, p_ptr->px,
					75, 0, TRUE);
			}

			/* trap of software bugs gone berserk. */
			else
			{
				/* explain what the dickens is going on. */
				msg_print("GRUESOME Software Bugs leap out at you!");

				if (!p_resist_pos(P_RES_CONFU))
				{
					(void)set_confused(p_ptr->confused + rand_int(20) + 10);
				}
				if (!p_resist_pos(P_RES_CHAOS))
				{
					(void)set_image(p_ptr->image + randint(40));
				}

				/* XXX (hard coded) summon 3-6 software bugs. */
				k = randint(4) + 2;
				for (i = 0; i < k; ++i)
				{
					/* Look for a location */
					for (j = 0; j < 20; ++j)
					{
						/* Pick a (scattered) distance. */
						int d = (j / 10) + randint(3);

						/* Pick a location */
						scatter(&y, &x, y, x, d, 0);

						/* Require passable terrain */
						if (!cave_passable_bold(y, x)) continue;

						/* Hack -- no summon on glyph of warding */
						if (cave_feat[y][x] == FEAT_GLYPH) continue;

						/* Okay */
						break;
					}

					/* Attempt to place the awake software bug */
					place_monster_aux(y, x, 453, FALSE, TRUE);
				}

				/* herald the arrival of Software Bugs. */
				msg_print("AAAAAAAHHHH! THEY'RE EVERYWHERE!");
			}

			Rand_quick = TRUE;

			break;
		}

		/* teleport trap */
		case FEAT_TRAP_HEAD + 0x08:
		{
			msg_print("You teleport across the dungeon.");

			Rand_quick = FALSE;

			teleport_player(250, FALSE);

			Rand_quick = TRUE;

			break;
		}

		/* murder holes. */
		case FEAT_TRAP_HEAD + 0x09:
		{
			/* hold the object info. */
			object_type *o_ptr;
			object_type object_type_body;

			/* hold the missile type and name. */
			int sval = 0;
			int tval = 0;
			cptr missile_name = "";


			/* Determine the missile type and base damage. */
			if (randint(3) == 1)
			{
				if (p_ptr->depth < 40)
				{
					missile_name = "shot";
					dam = damroll(2,3);
					tval = TV_SHOT;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker shot";
					dam = damroll(3,7);
					tval = TV_SHOT;
					sval = SV_AMMO_HEAVY;
				}
			}

			else if (randint(2) == 1)
			{
				if (p_ptr->depth < 55)
				{
					missile_name = "arrow";
					dam = damroll(2,4);
					tval = TV_ARROW;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker arrow";
					dam = damroll(3,9);
					tval = TV_ARROW;
					sval = SV_AMMO_HEAVY;
				}
			}

			else
			{
				if (p_ptr->depth < 65)
				{
					missile_name = "bolt";
					dam = damroll(2,5);
					tval = TV_BOLT;
					sval = SV_AMMO_NORMAL;
				}
				else
				{
					missile_name = "seeker bolt";
					dam = damroll(3,11);
					tval = TV_BOLT;
					sval = SV_AMMO_HEAVY;
				}
			}

			/* determine if the missile hits. */
			if (check_trap_hit(75 + p_ptr->depth))
			{
				msg_format("A %s hits you from above.", missile_name);

				Rand_quick = FALSE;

				/* critical hits. */
				if (randint(2) == 1)
				{
					msg_print("It was well-aimed!");
					dam *= 1 + randint(2);
				}
				if (randint(2) == 1)
				{
					msg_print("It gouges you!");
					dam = 3 * dam / 2;

					/* cut the player. */
					(void)set_cut(p_ptr->cut + randint(dam));
				}

				Rand_quick = TRUE;

				take_hit(dam, name);
			}

			/* Explain what just happened. */
			else msg_format("A %s wizzes by your head.", missile_name);

			/* Get local object */
			o_ptr = &object_type_body;

			/* Make a missile, identify it, and drop it near the player. */
			object_prep(o_ptr, lookup_kind(tval, sval));
			object_aware(o_ptr);
			object_known(o_ptr);
			drop_near(o_ptr, -1, y, x);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0A:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0B:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0E:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

		/* undefined trap. */
		case FEAT_TRAP_HEAD + 0x0F:
		{
			msg_print("A dagger is thrown at you from the shadows!");
			dam = damroll(3,4);
			take_hit(dam, name);

			break;
		}

	}

	/* Revert to usage of the complex RNG. */
	Rand_quick = FALSE;
}



/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/etc.
 */
void move_player(int dir, int do_pickup)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	byte str_escape, dex_escape;

	/* Permit the player to move? */
	bool can_move = FALSE;

	int temp;
	int y, x;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		/* Attack */
		py_attack(y, x);
	}

	else
	{
		/* It takes some dexterity, or failing that, strength, to get out of
		 * pits.
		 */
		if (cave_feat[p_ptr->py][p_ptr->px] == (FEAT_TRAP_HEAD + 0x01))
		{
			str_escape = adj_dex_dis[p_ptr->stat_ind[A_STR]];
			dex_escape = adj_dex_dis[p_ptr->stat_ind[A_DEX]];

			/* First attempt to leap out of the pit, */
			 if ((dex_escape + 1) * 2 < randint(16))
			{
				/* then attempt to climb out of the pit. */
				if (str_escape + 3 < randint(16))
				{
					/* Failure costs a turn. */
					msg_print("You remain stuck in the pit.");
					return;
				}
				else msg_print("You clamber out of the pit.");
			}
			else msg_print("You leap out of the pit.");
		}


		/* Option to disarm a visible trap. -TNB- */
		/* Hack - Rogues can walk over their own trap - BR */
		if ((easy_disarm) &&
			(cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
			(cave_feat[y][x] <= FEAT_TRAP_TAIL))
		{
			/* Optional auto-repeat. */
			if (always_repeat && (p_ptr->command_arg <= 0))
			{
				/* Repeat 99 times */
				p_ptr->command_arg = 99;
			}

			do_cmd_disarm();
			return;
		}

		/* Some terrain is impassable for the player, such as stone walls. */
		else if (!cave_passable_bold(y, x))
		{
			/* Disturb the player */
			disturb(0, 0);

			/* Notice unknown obstacles */
			if (!(cave_info[y][x] & (CAVE_MARK)))
			{
				/* Closed door */
				if (cave_feat[y][x] < FEAT_SECRET)
				{
					message(MSG_HITWALL, 0, "You feel a door blocking your way.");
					cave_info[y][x] |= (CAVE_MARK);
					lite_spot(y, x);
				}

				/* Wall (or secret door) */
				else
				{
					message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
					cave_info[y][x] |= (CAVE_MARK);
					lite_spot(y, x);
				}
			}

			/* Mention known obstacles */
			else
			{
				/* Closed door */
				if (cave_feat[y][x] < FEAT_SECRET)
				{
					/* Option to automatically open doors. -TNB- */
					if (easy_open)
					{
						/* Optional auto-repeat. */
						if (always_repeat && (p_ptr->command_arg <= 0))
						{
							/* Repeat 99 times */
							p_ptr->command_arg = 99;
						}

						do_cmd_open();
						return;
					}

					/* Otherwise, a message. */
					message(MSG_HITWALL, 0, "There is a door blocking your way.");
				}

				/* Wall (or secret door) */
				else
				{
					message(MSG_HITWALL, 0, "There is a wall blocking your way.");
				}
			}

			/* Sound */
			sound(SOUND_HITWALL);
		}

		/* Normal movement */
		else
		{
			/* Sound XXX XXX XXX */
			/* sound(SOUND_WALK); */

			/*** Handle traversable terrain.  ***/
			switch(cave_feat[y][x])
			{
				case FEAT_RUBBLE:
				{
					if (player_is_crossing == dir)
						can_move = TRUE;
					else
					{
						player_is_crossing = dir;

						/* Automate 2nd movement command, if not disturbed. */
						p_ptr->command_cmd = 59;
						p_ptr->command_rep = 1;
						p_ptr->command_dir = dir;
					}

					break;
				}
				case FEAT_TREE:
				{
					/* Druids and rangers slip easily under trees. */
					if ((check_ability(SP_WOODSMAN))) can_move = TRUE;

					/* Allow movement only if partway through already. */
					else if (player_is_crossing == dir)
						can_move = TRUE;
					else
					{
						player_is_crossing = dir;

						/* Automate 2nd movement command, if not disturbed. */
						p_ptr->command_cmd = 59;
						p_ptr->command_rep = 1;
						p_ptr->command_dir = dir;
					}

					break;
				}
				case FEAT_WATER:
				{
					/* Cannot cross with an over-heavy burden. */
					if (p_ptr->total_weight <
						adj_str_wgt[p_ptr->stat_ind[A_STR]] * 50)
						can_move = TRUE;
					else can_move = FALSE;

					/* Stop any run. */
					disturb(0,0);

					/* Explain why the horse won't cross the water. */
					if (can_move == FALSE)
						msg_print("You dare not cross carrying so much weight.");

					break;
				}
				case FEAT_LAVA:
				{
					/* Assume player will continue. */
					temp = TRUE;

					/* Smart enough to stop running. */
					if (p_ptr->running)
					{
						if (!get_check("Lava blocks your path.  Step into it? "))
						{
							temp = FALSE;
							p_ptr->running = 0;
						}
					}

					/* Smart enough to sense trouble. */
					else if ((!p_resist_pos(P_RES_FIRE)) ||
						 (!p_resist_strong(P_RES_FIRE) && (p_ptr->chp <= 100)) ||
						 (!p_immune(P_RES_FIRE) && (p_ptr->chp <= 30)))
					{
						if (!get_check("The heat of the lava scalds you!  Really enter? "))
						{
							temp = FALSE;
						}
					}

					/* Enter if OK or confirmed. */
					if (temp)
					{
						/* Can always cross. */
						can_move = TRUE;

						/* Feather fall makes one lightfooted. */
						if (p_ptr->ffall) temp = 49 + randint(51);
						else temp = 124 + randint(126);

						/* Will take serious fire damage. */
						fire_dam(temp, "burnt to a cinder in molten lava");
					}
					break;
				}
				default:
				{
					/* All other terrain can be traversed normally. */
					can_move = TRUE;
				}
			}

			/* If the player can move, handle various things. */
			if (can_move)
			{
				/* Move player */
				monster_swap(py, px, y, x);

				/* New location */
				y = py = p_ptr->py;
				x = px = p_ptr->px;

				/* No longer traversing. */
				player_is_crossing = 0;

				/* Spontaneous Searching */
				if ((p_ptr->skill_fos > 49) ||
				    (0 == rand_int(50 - p_ptr->skill_fos)))
				{
					search();
				}

				/* Continuous Searching */
				if (p_ptr->searching)
				{
					search();
				}

				/* Handle "objects".  Do not use extra energy for
				 * objects picked up.
				 */
				if (do_pickup) p_ptr->notice |= (PN_PICKUP1);
				else p_ptr->notice |= (PN_PICKUP0);

				/* Handle "store doors" */
				if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
				   (cave_feat[y][x] <= FEAT_SHOP_TAIL))
				{
					/* Disturb */
					disturb(0, 0);

					/* Hack -- Enter store */
					p_ptr->command_new = '_';
				}

				/* Discover invisible traps */
				else if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Disturb */
					disturb(0, 0);

					/* Message */
					msg_print("You stumble upon a trap!");

					/* Pick a trap */
					pick_trap(y, x);

					/* Hit the floor trap. */
					hit_trap(y, x);
				}

				/* Set off an visible trap */
				else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
				  (cave_feat[y][x] <= FEAT_TRAP_TAIL))
				{
					/* Disturb */
					disturb(0, 0);

					/* Hit the floor trap. */
					hit_trap(y, x);
				}

				/* Walk on a monster trap */
				else if ((cave_feat[y][x] >= FEAT_MTRAP_HEAD) &&
				  (cave_feat[y][x] <= FEAT_MTRAP_TAIL))
				{
					msg_print("You inspect your cunning trap.");
				}
			}
		}
	}
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls XXX XXX XXX */
	if (!in_bounds(y, x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave_feat[y][x] < FEAT_SECRET) return (FALSE);

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are unknown XXX XXX XXX */
	if (!in_bounds(y, x)) return (TRUE);

	/* Memorized grids are always known */
	if (cave_info[y][x] & (CAVE_MARK)) return (FALSE);

	/* Default */
	return (TRUE);
}





/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.   In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.   This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".   Both sides enclosed yields a hallway.
 *
 *    LL		     @L
 *    @x      (normal)	     RxL   (diagonal)
 *    RR      (east)	      R	   (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x	     1
 * ########### ######
 * #2	       #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!	       ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *			!!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###	      o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:		 o##
 *    o@x#   (normal)	 #@n	(maybe?)
 *    ##n#   (east)	 ##x#
 *			 ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
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
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor	Blunt Corridor (?)
 *	 # #		      #
 *	 #x#		     @x#
 *	 @p.		      p
 */
static void run_init(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;


	/* Save the direction */
	p_ptr->run_cur_dir = dir;

	/* Assume running straight */
	p_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	p_ptr->run_open_area = TRUE;

	/* Assume not looking for breaks */
	p_ptr->run_break_right = FALSE;
	p_ptr->run_break_left = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		p_ptr->run_break_right = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (p_ptr->run_break_left && p_ptr->run_break_right)
	{
		/* Not looking for open area */
		p_ptr->run_open_area = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				p_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				p_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				p_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				p_ptr->run_old_dir = cycle[i + 2];
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max, inv;
	int option, option2;


	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = p_ptr->run_old_dir;

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


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &m_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (this_o_idx = cave_o_idx[row][col]; this_o_idx; this_o_idx = next_o_idx)
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
		if (cave_info[row][col] & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[row][col])
			{
				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Special passable terrain. */
				case FEAT_LAVA:
				case FEAT_WATER:
				case FEAT_TREE:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}

				/* Open doors */
				case FEAT_OPEN:
				case FEAT_BROKEN:
				{
					/* Option -- ignore */
					if (run_ignore_doors) notice = FALSE;

					/* Done */
					break;
				}

				/* Stairs */
				case FEAT_LESS:
				case FEAT_MORE:
				{
					/* Option -- ignore */
					if (run_ignore_stairs) notice = FALSE;

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
		if (inv || cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (p_ptr->run_open_area)
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
			if (p_ptr->run_open_area)
			{
				if (i < 0)
				{
					/* Break to the right */
					p_ptr->run_break_right = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					p_ptr->run_break_left = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (p_ptr->run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
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

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
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
			p_ptr->run_cur_dir = option;

			/* No other options */
			p_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			p_ptr->run_old_dir = option2;
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
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					p_ptr->run_cur_dir = option;
					p_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				p_ptr->run_cur_dir = option2;
				p_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				p_ptr->run_cur_dir = option;
				p_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(p_ptr->run_cur_dir, py, px))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);

		/* Hack -- Set the run counter */
		p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Continue run */
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

	/* Decrease counter */
	p_ptr->running--;

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player, using the "pickup" flag */
	move_player(p_ptr->run_cur_dir, always_pickup);
}
